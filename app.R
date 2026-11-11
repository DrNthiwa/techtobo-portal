# Techtobo General Limited: public website and directors' portal
# Files in R/ (config.R, storage.R, pdf.R) are loaded automatically by Shiny.
library(shiny)
library(bslib)
library(DT)

db_init()

theme <- bs_theme(version = 5, primary = "#0B2545", secondary = "#5C6B7A", warning = "#F5A300",
                  "border-radius" = "0.35rem")

ui <- page_fluid(
  theme = theme, title = "Techtobo General Limited",
  tags$head(tags$link(rel = "stylesheet", href = "styles.css"), tags$link(rel = "icon", href = "icon.png")),
  uiOutput("root")
)

sel_in <- function(...) selectInput(..., selectize = FALSE)
named <- function(values, labels) if (length(values)) setNames(values, labels) else character(0)
safe_date <- function(x) tryCatch(as.Date(x), error = function(e) as.Date(NA))

# ---------------------------------------------------------------- UI builders
public_ui <- function() {
  svc <- lapply(names(SERVICES), function(s) div(class = "col-md-4 col-sm-6",
    div(class = "svc", h5(s), p(SERVICES[[s]]))))
  tagList(
    div(class = "topbar", img(src = "logo.png", height = 52),
        div(a(href = "#services", "Services"), a(href = "#about", "About"), a(href = "#contact", "Contact"),
            actionButton("show_login", "Portal login", class = "btn-primary btn-sm"))),
    div(class = "hero", h1("One team for every technical job."), p(class = "lead", COMPANY$tagline),
        p("Repairs, maintenance and installation for commercial, industrial and residential premises in Kenya."),
        a(href = "#contact", class = "btn btn-warning", "Request a quotation")),
    div(class = "section", id = "services", h2("Our services"), div(class = "row g-3", svc)),
    div(class = "section alt", id = "about", h2("About us"),
        p("Techtobo General Limited is a Kenyan private limited company providing technical, maintenance, repair and installation services. ",
          "We give clients one accountable partner for electrical, plumbing, mechanical and general building maintenance."),
        div(class = "row g-3",
            div(class = "col-md-4", div(class = "svc", h5("Clear documentation"), p("Every job is recorded from request to invoice: quotation, work order, completion report and invoice."))),
            div(class = "col-md-4", div(class = "svc", h5("Transparent pricing"), p("Itemised quotations and invoices that show labour and materials separately."))),
            div(class = "col-md-4", div(class = "svc", h5("Quality and safety"), p("Work is done to specification, tested, and signed off by the client before invoicing."))))),
    div(class = "section", id = "contact", h2("Contact"),
        p(strong("Telephone: "), COMPANY$tel), p(strong("Email: "), a(href = paste0("mailto:", COMPANY$email), COMPANY$email)),
        p(strong("Office: "), COMPANY$address), p(strong("Post: "), COMPANY$box)),
    div(class = "foot", paste0(COMPANY$name, "  |  Company No. ", COMPANY$number))
  )
}

line_editor <- function() tagList(
  h6("Line items"),
  layout_columns(col_widths = c(4, 2, 2, 1, 1, 2),
    textInput("ln_desc", "Description"), textInput("ln_detail", "Date of work / specification"),
    sel_in("ln_cat", "Category", CATEGORIES), numericInput("ln_qty", "Qty / Hrs", 1, min = 0),
    textInput("ln_unit", "Unit", "pc"), numericInput("ln_price", "Unit price (KES)", 0, min = 0)),
  actionButton("ln_add", "Add line", class = "btn-primary btn-sm"),
  actionButton("ln_del", "Remove selected line", class = "btn-outline-secondary btn-sm"),
  div(class = "mt-2", DTOutput("ln_tbl")), uiOutput("ln_totals")
)

portal_ui <- function(u) {
  director <- u$role == "director"
  tabs <- list(
    if (director) nav_panel("Dashboard", uiOutput("dash")),
    nav_panel("Jobs",
      if (director) div(class = "mb-2", actionButton("job_add", "Add job", class = "btn-primary btn-sm"),
                        actionButton("job_edit", "Edit selected job", class = "btn-outline-primary btn-sm")),
      DTOutput("jobs_tbl")),
    if (director) nav_panel("New quotation / invoice",
      layout_columns(col_widths = c(2, 3, 3, 2, 2),
        radioButtons("d_type", "Document", c("Quotation" = "quotation", "Invoice" = "invoice")),
        uiOutput("d_client_ui"), uiOutput("d_job_ui"), dateInput("d_date", "Date", Sys.Date()),
        numericInput("d_days", "Validity / payment terms (days)", 30, min = 0)),
      conditionalPanel("input.d_type == 'quotation'",
        textAreaInput("d_scope", "Scope of work", width = "100%", rows = 2),
        layout_columns(col_widths = c(4, 8), textInput("d_completion", "Estimated completion time", placeholder = "e.g. 3 working days"),
                       textInput("d_payterms", "Payment terms", placeholder = "e.g. 50% deposit, balance on completion"))),
      conditionalPanel("input.d_type == 'invoice'",
        layout_columns(col_widths = c(4, 3, 5), uiOutput("d_copy_ui"),
                       div(style = "margin-top:32px", actionButton("d_copy", "Copy lines from quotation", class = "btn-outline-primary btn-sm")), div())),
      hr(), line_editor(), hr(),
      actionButton("d_save", "Save document", class = "btn-warning"), span(class = "ms-3 text-muted", textOutput("d_msg", inline = TRUE))),
    nav_panel(if (director) "Documents" else "Quotations and invoices",
      DTOutput("docs_tbl"),
      div(class = "mt-2", downloadButton("doc_pdf_dl", "Download PDF of selected", class = "btn-primary btn-sm")),
      if (director) tagList(hr(), h6("Update selected document"),
        layout_columns(col_widths = c(4, 3, 5), textInput("doc_cu", "eTIMS CU invoice No. (invoices)"),
          sel_in("doc_status", "Status", c(QTN_STATUS, INV_STATUS)),
          div(style = "margin-top:32px", actionButton("doc_update", "Save changes", class = "btn-outline-primary btn-sm"))),
        p(class = "text-muted small", "Issue the tax invoice in KRA eTIMS, then record its CU invoice number here. The portal PDF is a branded invoice, not an eTIMS tax invoice."))),
    if (director) nav_panel("Receipts",
      layout_columns(col_widths = c(3, 2, 2, 2, 3), uiOutput("rc_inv_ui"), dateInput("rc_date", "Date received", Sys.Date()),
        numericInput("rc_amount", "Amount (KES)", 0, min = 0), sel_in("rc_method", "Method", PAY_METHODS),
        textInput("rc_ref", "Reference (e.g. M-Pesa code)")),
      actionButton("rc_save", "Record payment", class = "btn-warning btn-sm"), span(class = "ms-3 text-muted", textOutput("rc_msg", inline = TRUE)),
      hr(), DTOutput("rc_tbl"), div(class = "mt-2", downloadButton("rc_pdf_dl", "Download receipt PDF of selected", class = "btn-primary btn-sm"))),
    if (director) nav_panel("Clients",
      div(class = "mb-2", actionButton("cl_add", "Add client", class = "btn-primary btn-sm"),
          actionButton("cl_edit", "Edit selected client", class = "btn-outline-primary btn-sm")), DTOutput("cl_tbl")),
    if (director) nav_panel("Settings",
      layout_columns(col_widths = c(6, 6),
        card(card_header("Company and tax"), uiOutput("set_ui"), actionButton("set_save", "Save settings", class = "btn-primary btn-sm"),
             textOutput("set_msg")),
        card(card_header("Portal users"), DTOutput("us_tbl"),
          layout_columns(col_widths = c(6, 6), textInput("us_user", "Username"), textInput("us_name", "Full name")),
          layout_columns(col_widths = c(4, 4, 4), sel_in("us_role", "Role", c("Director" = "director", "Client (read-only)" = "client")),
                         uiOutput("us_client_ui"), passwordInput("us_pw", "Password (min 8)")),
          div(actionButton("us_add", "Add user / reset password", class = "btn-primary btn-sm"),
              actionButton("us_toggle", "Activate / deactivate selected", class = "btn-outline-secondary btn-sm")),
          textOutput("us_msg"))))
  )
  tagList(
    div(class = "topbar dark", img(src = "logo_reversed.png", height = 46),
        div(span(class = "me-3", paste0(u$name, " (", if (director) "Director" else u$client, ")")),
            actionButton("refresh", "Refresh data", class = "btn-outline-light btn-sm"),
            actionButton("logout", "Log out", class = "btn-warning btn-sm"))),
    div(class = "portal", do.call(navset_underline, c(Filter(Negate(is.null), tabs), list(id = "tabs"))))
  )
}

# ---------------------------------------------------------------- server
server <- function(input, output, session) {
  user <- reactiveVal(NULL)
  rv <- reactiveValues()
  load_all <- function() for (t in names(SCHEMA)) rv[[t]] <- db_read(t)
  load_all()
  change <- function(tbl, fn) rv[[tbl]] <- db_change(tbl, fn)
  st <- reactive(setNames(rv$settings$value, rv$settings$key))
  is_dir <- reactive(!is.null(user()) && user()$role == "director")
  need_dir <- function() if (!is_dir()) { showNotification("Directors only.", type = "error"); FALSE } else TRUE
  # Rows visible to the logged-in user (clients see only their own records)
  vis <- function(df) { u <- user(); if (is.null(u)) return(df[0, ]); if (u$role == "director") df else df[df$client == u$client, , drop = FALSE] }
  dt <- function(df, ...) datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE), ...)
  sel <- function(id, df) { i <- input[[paste0(id, "_rows_selected")]]; if (length(i) == 1 && i <= nrow(df)) df[i, , drop = FALSE] else NULL }

  output$root <- renderUI(if (is.null(user())) public_ui() else portal_ui(user()))

  # ---- login / first account
  observeEvent(input$show_login, {
    first <- nrow(rv$users) == 0
    can_setup <- BACKEND == "local" || nzchar(Sys.getenv("TECHTOBO_SETUP_KEY"))
    showModal(modalDialog(title = if (first) "Create the first director account" else "Portal login", easyClose = TRUE,
      if (first && !can_setup) p("No accounts exist yet. Set the TECHTOBO_SETUP_KEY secret on the server to enable first-time setup (see README).")
      else tagList(
        if (first) textInput("lg_name", "Full name"),
        textInput("lg_user", "Username"), passwordInput("lg_pw", if (first) "Password (min 8 characters)" else "Password"),
        if (first && BACKEND != "local") passwordInput("lg_key", "Setup key"),
        div(class = "text-danger", textOutput("lg_msg"))),
      footer = tagList(modalButton("Cancel"), actionButton("lg_go", if (first) "Create account" else "Log in", class = "btn-primary"))))
  })
  observeEvent(input$lg_go, {
    un <- tolower(trimws(input$lg_user %||% "")); pw <- input$lg_pw %||% ""
    users <- db_read("users"); rv$users <- users
    if (nrow(users) == 0) {
      if (BACKEND != "local" && !identical(input$lg_key, Sys.getenv("TECHTOBO_SETUP_KEY"))) { output$lg_msg <- renderText("Wrong setup key."); return() }
      if (!nzchar(un) || nchar(pw) < 8) { output$lg_msg <- renderText("Enter a username and a password of at least 8 characters."); return() }
      change("users", function(d) rbind(d, row_df("users", username = un, name = input$lg_name %||% un, role = "director", client = "",
                                                   password_hash = sodium::password_store(pw), active = "Yes")))
      users <- rv$users
    }
    u <- users[users$username == un & users$active == "Yes", , drop = FALSE]
    ok <- nrow(u) == 1 && isTRUE(tryCatch(sodium::password_verify(u$password_hash, pw), error = function(e) FALSE))
    if (!ok) { Sys.sleep(1); output$lg_msg <- renderText("Incorrect username or password."); return() }
    removeModal(); load_all(); user(as.list(u[1, c("username", "name", "role", "client")]))
  })
  observeEvent(input$logout, user(NULL))
  observeEvent(input$refresh, { load_all(); showNotification("Data refreshed.") })

  # ---- dashboard
  output$dash <- renderUI({
    req(is_dir()); j <- rv$jobs; d <- rv$documents; inv <- d[d$type == "invoice" & d$status != "Cancelled", ]
    paid <- sum(num(rv$receipts$amount)); billed <- sum(num(inv$total))
    box <- function(t, v, cls = "") div(class = "col-md-3 col-6", div(class = paste("kpi", cls), div(class = "kpi-v", v), div(class = "kpi-t", t)))
    tagList(div(class = "row g-3 mb-3",
      box("Open jobs", sum(!j$status %in% c("Paid", "Cancelled"))),
      box("Completed, not yet invoiced", sum(j$status %in% c("Completed", "Client signed off")), "amber"),
      box("Total invoiced (KES)", kes(billed)), box("Outstanding (KES)", kes(billed - paid), "amber")),
      h6("Jobs by status"), renderTable(as.data.frame(table(Status = factor(j$status, JOB_STATUS)), responseName = "Jobs"), striped = TRUE),
      if (st()[["vat_registered"]] != "Yes") p(class = "text-muted small", "VAT is switched off (company marked as not VAT-registered in Settings)."))
  })

  # ---- jobs
  jobs_view <- reactive({ j <- vis(rv$jobs); if (!is_dir()) j <- j[, setdiff(names(j), c("labour_cost", "materials_cost", "other_costs", "remarks")), drop = FALSE]; j })
  output$jobs_tbl <- renderDT(dt(jobs_view()))
  job_modal <- function(j = NULL) {
    g <- function(f, d = "") if (is.null(j)) d else j[[f]]
    dd <- function(f, d = NA) { v <- safe_date(g(f, NA)); if (is.na(v)) d else v }
    showModal(modalDialog(title = if (is.null(j)) "Add job" else paste("Edit", j$job_no), size = "l", easyClose = FALSE,
      layout_columns(col_widths = c(4, 4, 4), dateInput("j_req", "Date requested", dd("date_requested", Sys.Date())),
        sel_in("j_client", "Client", rv$clients$name, g("client", rv$clients$name[1])), textInput("j_by", "Requested by", g("requested_by"))),
      layout_columns(col_widths = c(4, 4, 4), textInput("j_site", "Site / location", g("site")),
        sel_in("j_service", "Service category", names(SERVICES), g("service", names(SERVICES)[1])),
        sel_in("j_status", "Status", JOB_STATUS, g("status", "Requested"))),
      textAreaInput("j_desc", "Job description", g("description"), width = "100%", rows = 2),
      layout_columns(col_widths = c(6, 6), dateInput("j_start", "Start date", dd("start_date")), dateInput("j_end", "Completion date", dd("completion_date"))),
      layout_columns(col_widths = c(4, 4, 4), numericInput("j_lab", "Labour cost (KES)", num(g("labour_cost", 0)), min = 0),
        numericInput("j_mat", "Materials cost (KES)", num(g("materials_cost", 0)), min = 0), numericInput("j_oth", "Other costs (KES)", num(g("other_costs", 0)), min = 0)),
      textInput("j_rem", "Remarks", g("remarks"), width = "100%"),
      footer = tagList(modalButton("Cancel"), actionButton("j_save", "Save job", class = "btn-primary"))))
  }
  editing_job <- reactiveVal(NULL)
  observeEvent(input$job_add, { req(need_dir()); editing_job(NULL); job_modal() })
  observeEvent(input$job_edit, { req(need_dir()); j <- sel("jobs_tbl", jobs_view())
    if (is.null(j)) return(showNotification("Select a job first.", type = "warning"))
    full <- rv$jobs[rv$jobs$job_no == j$job_no, ]; editing_job(full$job_no); job_modal(as.list(full)) })
  observeEvent(input$j_save, {
    req(need_dir())
    change("jobs", function(d) {
      no <- editing_job() %||% next_no("JOB", d$job_no, input$j_req %||% Sys.Date())
      r <- row_df("jobs", job_no = no, date_requested = dstr(input$j_req), client = input$j_client, requested_by = input$j_by, site = input$j_site,
                  service = input$j_service, description = input$j_desc, status = input$j_status, start_date = dstr(input$j_start),
                  completion_date = dstr(input$j_end), labour_cost = num(input$j_lab), materials_cost = num(input$j_mat),
                  other_costs = num(input$j_oth), remarks = input$j_rem)
      if (no %in% d$job_no) { d[d$job_no == no, ] <- r; d } else rbind(d, r)
    })
    removeModal(); showNotification("Job saved.")
  })

  # ---- new quotation / invoice
  lines <- reactiveVal(empty_tbl("lines"))
  output$d_client_ui <- renderUI(sel_in("d_client", "Client", rv$clients$name))
  output$d_job_ui <- renderUI({ j <- rv$jobs[rv$jobs$client == (input$d_client %||% ""), ]
    sel_in("d_job", "Job", c("(none)" = "", named(j$job_no, paste(j$job_no, "-", substr(j$description, 1, 40))))) })
  output$d_copy_ui <- renderUI({ q <- rv$documents[rv$documents$type == "quotation" & rv$documents$client == (input$d_client %||% ""), ]
    sel_in("d_from", "Quotation to copy from", c("(none)" = "", q$doc_no)) })
  observeEvent(input$d_type, updateNumericInput(session, "d_days",
    value = num(st()[[if (input$d_type == "invoice") "payment_terms_days" else "quote_validity_days"]])))
  observeEvent(input$d_copy, { req(nzchar(input$d_from %||% "")); l <- rv$lines[rv$lines$doc_no == input$d_from, ]; l$doc_no <- ""; lines(l) })
  observeEvent(input$ln_add, {
    if (!nzchar(trimws(input$ln_desc %||% ""))) return(showNotification("Enter a description.", type = "warning"))
    amt <- round(num(input$ln_qty) * num(input$ln_price), 2)
    lines(rbind(lines(), row_df("lines", doc_no = "", line = nrow(lines()) + 1, description = trimws(input$ln_desc), detail = input$ln_detail,
                                category = input$ln_cat, qty = num(input$ln_qty), unit = input$ln_unit, unit_price = num(input$ln_price), amount = amt)))
    updateTextInput(session, "ln_desc", value = ""); updateTextInput(session, "ln_detail", value = "")
  })
  observeEvent(input$ln_del, { i <- input$ln_tbl_rows_selected; if (length(i)) { l <- lines()[-i, , drop = FALSE]; l$line <- as.character(seq_len(nrow(l))); lines(l) } })
  output$ln_tbl <- renderDT({ l <- lines()[, c("line", "description", "detail", "category", "qty", "unit", "unit_price", "amount")]
    l$unit_price <- kes(l$unit_price); l$amount <- kes(l$amount)
    datatable(l, selection = "single", rownames = FALSE, options = list(dom = "t", pageLength = 100, ordering = FALSE)) })
  totals <- reactive({ sub <- round(sum(num(lines()$amount)), 2)
    rate <- if (st()[["vat_registered"]] == "Yes") num(st()[["vat_rate"]]) else 0
    vat <- round(sub * rate / 100, 2); list(sub = sub, rate = rate, vat = vat, total = round(sub + vat, 2)) })
  output$ln_totals <- renderUI({ t <- totals(); div(class = "totals",
    span("Subtotal: ", strong(kes(t$sub))), span(if (t$rate > 0) paste0("VAT @ ", t$rate, "%: ") else "VAT: ", strong(if (t$rate > 0) kes(t$vat) else "not applicable")),
    span(class = "grand", "Total (KES): ", strong(kes(t$total)))) })
  observeEvent(input$d_save, {
    req(need_dir())
    if (nrow(lines()) == 0) return(showNotification("Add at least one line.", type = "warning"))
    inv <- input$d_type == "invoice"; t <- totals(); no <- NULL
    change("documents", function(d) {
      no <<- next_no(if (inv) "INV" else "QTN", d$doc_no, input$d_date)
      rbind(d, row_df("documents", doc_no = no, type = input$d_type, date = dstr(input$d_date), client = input$d_client, job_no = input$d_job %||% "",
        scope = if (inv) "" else input$d_scope, days = num(input$d_days), completion_time = if (inv) "" else input$d_completion,
        payment_terms = if (inv) "" else input$d_payterms, vat_rate = t$rate, subtotal = t$sub, vat = t$vat, total = t$total,
        etims_cu_no = "", status = if (inv) "Unpaid" else "Draft", created_by = user()$username, created_at = format(Sys.time(), "%Y-%m-%d %H:%M")))
    })
    l <- lines(); l$doc_no <- no; change("lines", function(d) rbind(d, l))
    if (nzchar(input$d_job %||% "")) change("jobs", function(d) { i <- d$job_no == input$d_job
      if (inv) d$status[i] <- "Invoiced" else d$status[i & d$status == "Requested"] <- "Quoted"; d })
    lines(empty_tbl("lines")); output$d_msg <- renderText(paste0("Saved as ", no, ". Download the PDF from the Documents tab."))
    showNotification(paste("Saved", no))
  })

  # ---- documents list, PDF, update
  docs_view <- reactive({ d <- vis(rv$documents); if (!is_dir()) d <- d[d$status != "Draft", ]
    d <- d[order(d$doc_no, decreasing = TRUE), c("doc_no", "type", "date", "client", "job_no", "subtotal", "vat", "total", "etims_cu_no", "status")]
    d })
  output$docs_tbl <- renderDT({ d <- docs_view(); for (c in c("subtotal", "vat", "total")) d[[c]] <- kes(d[[c]]); dt(d) })
  observeEvent(input$docs_tbl_rows_selected, { d <- sel("docs_tbl", docs_view()); req(d, is_dir())
    updateTextInput(session, "doc_cu", value = d$etims_cu_no)
    updateSelectInput(session, "doc_status", choices = if (d$type == "invoice") INV_STATUS else QTN_STATUS, selected = d$status) })
  observeEvent(input$doc_update, { req(need_dir()); d <- sel("docs_tbl", docs_view())
    if (is.null(d)) return(showNotification("Select a document first.", type = "warning"))
    change("documents", function(x) { i <- x$doc_no == d$doc_no; x$status[i] <- input$doc_status
      if (d$type == "invoice") x$etims_cu_no[i] <- trimws(input$doc_cu); x })
    showNotification("Document updated.") })
  output$doc_pdf_dl <- downloadHandler(
    filename = function() { d <- sel("docs_tbl", docs_view()); paste0(if (is.null(d)) "document" else d$doc_no, ".pdf") },
    content = function(file) { d <- sel("docs_tbl", docs_view()); validate(need(!is.null(d), "Select a document first."))
      full <- rv$documents[rv$documents$doc_no == d$doc_no, ]; cl <- rv$clients[rv$clients$name == full$client, ]
      doc_pdf(file, full$type, as.list(full), rv$lines[rv$lines$doc_no == d$doc_no, ], if (nrow(cl)) as.list(cl[1, ]) else list(name = full$client), st()) })

  # ---- receipts
  balances <- reactive({ inv <- rv$documents[rv$documents$type == "invoice" & rv$documents$status != "Cancelled", ]
    paid <- vapply(inv$doc_no, function(n) sum(num(rv$receipts$amount[rv$receipts$invoice_no == n])), 0)
    data.frame(doc_no = inv$doc_no, client = inv$client, job_no = inv$job_no, total = num(inv$total), paid = paid, balance = round(num(inv$total) - paid, 2), stringsAsFactors = FALSE) })
  output$rc_inv_ui <- renderUI({ b <- balances(); b <- b[b$balance > 0, ]
    sel_in("rc_inv", "Invoice", c("(select)" = "", named(b$doc_no, paste0(b$doc_no, " - balance ", kes(b$balance))))) })
  observeEvent(input$rc_inv, { b <- balances(); i <- match(input$rc_inv, b$doc_no); if (!is.na(i)) updateNumericInput(session, "rc_amount", value = b$balance[i]) })
  observeEvent(input$rc_save, {
    req(need_dir()); b <- balances(); i <- match(input$rc_inv %||% "", b$doc_no); amt <- num(input$rc_amount)
    if (is.na(i) || amt <= 0) return(showNotification("Choose an invoice and enter an amount.", type = "warning"))
    if (amt > b$balance[i] + 0.005) return(showNotification("Amount is more than the invoice balance.", type = "error"))
    no <- NULL
    change("receipts", function(d) { no <<- next_no("RCT", d$receipt_no, input$rc_date)
      rbind(d, row_df("receipts", receipt_no = no, date = dstr(input$rc_date), invoice_no = b$doc_no[i], client = b$client[i], amount = amt,
                      method = input$rc_method, reference = input$rc_ref, received_by = user()$name, created_at = format(Sys.time(), "%Y-%m-%d %H:%M"))) })
    full <- amt >= b$balance[i] - 0.005
    change("documents", function(d) { d$status[d$doc_no == b$doc_no[i]] <- if (full) "Paid" else "Part paid"; d })
    if (full && nzchar(b$job_no[i])) change("jobs", function(d) { d$status[d$job_no == b$job_no[i]] <- "Paid"; d })
    output$rc_msg <- renderText(paste("Recorded", no)); showNotification(paste("Recorded", no))
  })
  rc_view <- reactive(rv$receipts[order(rv$receipts$receipt_no, decreasing = TRUE), c("receipt_no", "date", "invoice_no", "client", "amount", "method", "reference", "received_by")])
  output$rc_tbl <- renderDT({ req(is_dir()); d <- rc_view(); d$amount <- kes(d$amount); dt(d) })
  output$rc_pdf_dl <- downloadHandler(
    filename = function() { r <- sel("rc_tbl", rc_view()); paste0(if (is.null(r)) "receipt" else r$receipt_no, ".pdf") },
    content = function(file) { req(is_dir()); r <- sel("rc_tbl", rc_view()); validate(need(!is.null(r), "Select a receipt first."))
      tot <- num(rv$documents$total[rv$documents$doc_no == r$invoice_no])
      upto <- rv$receipts[rv$receipts$invoice_no == r$invoice_no & rv$receipts$receipt_no <= r$receipt_no, ]
      receipt_pdf(file, as.list(r), tot, sum(num(upto$amount)), st()) })

  # ---- clients
  output$cl_tbl <- renderDT({ req(is_dir()); dt(rv$clients) })
  editing_cl <- reactiveVal(NULL)
  cl_modal <- function(c = NULL) { g <- function(f) if (is.null(c)) "" else c[[f]]
    showModal(modalDialog(title = if (is.null(c)) "Add client" else paste("Edit", c$name),
      textInput("c_name", "Registered name", g("name")), textInput("c_addr", "Address", g("address")), textInput("c_pin", "KRA PIN", g("kra_pin")),
      textInput("c_contact", "Contact person", g("contact_person")), textInput("c_phone", "Phone", g("phone")), textInput("c_email", "Email", g("email")),
      footer = tagList(modalButton("Cancel"), actionButton("c_save", "Save client", class = "btn-primary")))) }
  observeEvent(input$cl_add, { req(need_dir()); editing_cl(NULL); cl_modal() })
  observeEvent(input$cl_edit, { req(need_dir()); c <- sel("cl_tbl", rv$clients); if (is.null(c)) return(showNotification("Select a client first.", type = "warning"))
    editing_cl(c$name); cl_modal(as.list(c)) })
  observeEvent(input$c_save, { req(need_dir()); nm <- trimws(input$c_name); if (!nzchar(nm)) return(showNotification("Enter a name.", type = "warning"))
    old <- editing_cl()
    if (!is.null(old) && old != nm && any(c(rv$jobs$client, rv$documents$client) == old))
      return(showNotification("This client already has jobs or documents, so the name cannot be changed.", type = "error"))
    r <- row_df("clients", name = nm, address = input$c_addr, kra_pin = input$c_pin, contact_person = input$c_contact, phone = input$c_phone, email = input$c_email)
    change("clients", function(d) { key <- old %||% nm; if (key %in% d$name) { d[d$name == key, ] <- r; d } else rbind(d, r) })
    removeModal(); showNotification("Client saved.") })

  # ---- settings and users
  SET_LABELS <- c(kra_pin = "Company KRA PIN", vat_registered = "VAT-registered? (Yes / No)", vat_rate = "VAT rate (%)", bank_name = "Bank",
    bank_branch = "Branch", bank_account_name = "Account name", bank_account_no = "Account number", mpesa = "M-Pesa Paybill / Till",
    payment_terms_days = "Invoice payment terms (days)", quote_validity_days = "Quotation validity (days)",
    signatory_name = "Signatory name", signatory_title = "Signatory title")
  output$set_ui <- renderUI({ req(is_dir()); s <- isolate(st())
    lapply(names(SET_LABELS), function(k) if (k == "vat_registered") sel_in("set_vat_registered", SET_LABELS[[k]], c("No", "Yes"), s[[k]])
                                           else textInput(paste0("set_", k), SET_LABELS[[k]], s[[k]])) })
  observeEvent(input$set_save, { req(need_dir())
    change("settings", function(d) { for (k in names(SET_LABELS)) { v <- input[[paste0("set_", k)]]; if (!is.null(v)) d$value[d$key == k] <- trimws(v) }; d })
    output$set_msg <- renderText("Settings saved. New documents will use them.") })
  users_view <- reactive(rv$users[, c("username", "name", "role", "client", "active")])
  output$us_tbl <- renderDT({ req(is_dir()); datatable(users_view(), selection = "single", rownames = FALSE, options = list(dom = "t")) })
  output$us_client_ui <- renderUI(sel_in("us_client", "Client (for client role)", c("", rv$clients$name)))
  observeEvent(input$us_add, { req(need_dir()); un <- tolower(trimws(input$us_user)); pw <- input$us_pw
    if (!nzchar(un) || nchar(pw) < 8) return(output$us_msg <- renderText("Enter a username and a password of at least 8 characters."))
    if (input$us_role == "client" && !nzchar(input$us_client %||% "")) return(output$us_msg <- renderText("Choose which client this login belongs to."))
    r <- row_df("users", username = un, name = input$us_name %||% un, role = input$us_role, client = if (input$us_role == "client") input$us_client else "",
                password_hash = sodium::password_store(pw), active = "Yes")
    change("users", function(d) if (un %in% d$username) { d[d$username == un, ] <- r; d } else rbind(d, r))
    updateTextInput(session, "us_pw", value = ""); output$us_msg <- renderText(paste("Saved user", un)) })
  observeEvent(input$us_toggle, { req(need_dir()); u <- sel("us_tbl", users_view()); if (is.null(u)) return()
    if (u$username == user()$username) return(output$us_msg <- renderText("You cannot deactivate your own account."))
    change("users", function(d) { i <- d$username == u$username; d$active[i] <- if (d$active[i] == "Yes") "No" else "Yes"; d }) })
}

shinyApp(ui, server)
