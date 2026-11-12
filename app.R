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
  clients <- lapply(PUBLIC_CLIENTS, function(cl) div(class = "col-md-3 col-sm-6",
    div(class = "client-card",
        if (!is.null(cl$logo) && file.exists(file.path("www", cl$logo))) img(src = cl$logo, alt = cl$name, class = "client-logo")
        else div(class = "client-mark", toupper(substr(cl$name, 1, 1))),
        div(class = "client-name", cl$name))))
  tagList(
    div(class = "topbar", img(src = "logo.png", height = 52),
        div(a(href = "#services", "Services"), a(href = "#about", "About"), a(href = "#clients", "Clients"), a(href = "#contact", "Contact"),
            actionButton("show_login", "Log in", class = "btn-primary btn-sm"),
            actionButton("show_signup", "Sign up", class = "btn-outline-primary btn-sm"))),
    div(class = "hero", h1("One team for every technical job"), p(class = "lead", sub("\\.$", "", COMPANY$tagline)),
        p("Repairs, maintenance and installation for commercial, industrial and residential premises in Kenya."),
        actionButton("show_quote", "Request a quotation", class = "btn-warning")),
    div(class = "section", id = "services", h2("Our services"), div(class = "row g-3", svc)),
    div(class = "section alt", id = "about", h2("About us"),
        p("Techtobo General Limited is a Kenyan private limited company providing technical, maintenance, repair and installation services. ",
          "We give clients one accountable partner for electrical, plumbing, mechanical and general building maintenance."),
        div(class = "row g-3",
            div(class = "col-md-4", div(class = "svc", h5("Clear documentation"), p("Every job is recorded from request to invoice: quotation, work order, completion report and invoice."))),
            div(class = "col-md-4", div(class = "svc", h5("Transparent pricing"), p("Itemised quotations and invoices that show labour and materials separately."))),
            div(class = "col-md-4", div(class = "svc", h5("Quality and safety"), p("Work is done to specification, tested, and signed off by the client before invoicing."))))),
    div(class = "section", id = "clients", h2("Our Clients"), div(class = "row g-3", clients)),
    div(class = "section alt", id = "contact", h2("Contact"),
        p(strong("Telephone: "), COMPANY$tel), p(strong("Email: "), a(href = paste0("mailto:", COMPANY$email), COMPANY$email)),
        p(strong("Office: "), COMPANY$address), p(strong("Post: "), COMPANY$box)),
    div(class = "foot", COMPANY$name, span(class = "foot-sep", "|"), actionLink("show_admin_login", "Administrator login"))
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
  director <- u$role == "director"; client <- u$role == "client"
  tabs <- list(
    if (director) nav_panel("Dashboard", uiOutput("dash")),
    if (director) nav_panel("Quotation requests",
      p(class = "text-muted small", "Requests submitted through the website. Select a row to read it in full and update its status."),
      DTOutput("rq_tbl"), uiOutput("rq_detail"),
      layout_columns(col_widths = c(3, 6, 3), sel_in("rq_status", "Status", REQ_STATUS), textInput("rq_notes", "Internal notes (not shown to the requester)", width = "100%"),
        div(style = "margin-top:32px", actionButton("rq_update", "Save changes", class = "btn-outline-primary btn-sm")))),
    if (!director && !client) nav_panel("My requests",
      div(class = "mb-2", actionButton("show_quote", "Request a quotation", class = "btn-warning btn-sm")), DTOutput("myrq_tbl")),
    if (director || client) nav_panel("Jobs",
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
    if (director || client) nav_panel(if (director) "Documents" else "Quotations and invoices",
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
    if (client) nav_panel("My requests",
      div(class = "mb-2", actionButton("show_quote", "Request a quotation", class = "btn-warning btn-sm")), DTOutput("myrq_tbl")),
    if (director) nav_panel("Clients",
      div(class = "mb-2", actionButton("cl_add", "Add client", class = "btn-primary btn-sm"),
          actionButton("cl_edit", "Edit selected client", class = "btn-outline-primary btn-sm")), DTOutput("cl_tbl")),
    if (director) nav_panel("Property Management", pm_ui()),
    if (director) nav_panel("Settings",
      layout_columns(col_widths = c(6, 6),
        card(card_header("Company and tax"), uiOutput("set_ui"), actionButton("set_save", "Save settings", class = "btn-primary btn-sm"),
             textOutput("set_msg")),
        card(card_header("Portal users"), DTOutput("us_tbl"),
          layout_columns(col_widths = c(6, 6), textInput("us_user", "Username"), textInput("us_name", "Full name")),
          layout_columns(col_widths = c(4, 4, 4), sel_in("us_role", "Role", c("Administrator" = "director", "Client (read-only)" = "client", "User" = "user")),
                         uiOutput("us_client_ui"), passwordInput("us_pw", "Password (min 8; blank keeps current)")),
          div(actionButton("us_add", "Add or update user", class = "btn-primary btn-sm"),
              actionButton("us_toggle", "Activate / deactivate selected", class = "btn-outline-secondary btn-sm")),
          textOutput("us_msg"))))
  )
  tagList(
    div(class = "topbar dark", img(src = "logo_reversed.png", height = 46),
        div(span(class = "me-3", paste0(u$name, " (", if (client) u$client else ROLE_LABELS[[u$role]], ")")),
            actionButton("refresh", "Refresh data", class = "btn-outline-light btn-sm"),
            actionButton("logout", "Log out", class = "btn-warning btn-sm"))),
    div(class = "portal", do.call(navset_underline, c(Filter(Negate(is.null), tabs), list(id = "tabs"))))
  )
}

# ---------------------------------------------------------------- server
server <- function(input, output, session) {
  user <- reactiveVal(NULL)
  rv <- reactiveValues()
  load_all <- function() for (t in setdiff(names(SCHEMA), names(PM_SCHEMA))) rv[[t]] <- db_read(t)   # property tables load for administrators only
  load_all()
  change <- function(tbl, fn) rv[[tbl]] <- db_change(tbl, fn)
  st <- reactive(setNames(rv$settings$value, rv$settings$key))
  is_dir <- reactive(!is.null(user()) && user()$role == "director")
  need_dir <- function() if (!is_dir()) { showNotification("Administrators only.", type = "error"); FALSE } else TRUE
  # Rows visible to the logged-in user (clients see only their own records)
  vis <- function(df) { u <- user(); if (is.null(u)) return(df[0, ])
    if (u$role == "director") df else if (u$role == "client" && nzchar(u$client)) df[df$client == u$client, , drop = FALSE] else df[0, , drop = FALSE] }
  dt <- function(df, ...) datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE), ...)
  sel <- function(id, df) { i <- input[[paste0(id, "_rows_selected")]]; if (length(i) == 1 && i <= nrow(df)) df[i, , drop = FALSE] else NULL }

  output$root <- renderUI(if (is.null(user())) public_ui() else portal_ui(user()))

  # ---- login (general and administrator), first administrator account
  login_mode <- reactiveVal("user"); fails <- reactiveVal(0)
  no_admin <- function(users) !any(users$role == "director" & users$active == "Yes")
  login_modal <- function(admin) {
    login_mode(if (admin) "admin" else "user"); output$lg_msg <- renderText("")
    first <- no_admin(rv$users)
    can_setup <- BACKEND == "local" || nzchar(Sys.getenv("TECHTOBO_SETUP_KEY"))
    showModal(modalDialog(title = if (first) "Create the first administrator account" else if (admin) "Administrator login" else "Log in", easyClose = TRUE,
      if (first && !can_setup) p("No administrator account exists yet. Set the TECHTOBO_SETUP_KEY secret on the server to enable first-time setup (see README).")
      else tagList(
        if (first) textInput("lg_name", "Full name", width = "100%"),
        textInput("lg_user", if (first) "Username" else "Username or email", width = "100%"),
        passwordInput("lg_pw", if (first) "Password (min 8 characters)" else "Password", width = "100%"),
        if (first && BACKEND != "local") passwordInput("lg_key", "Setup key", width = "100%"),
        div(class = "text-danger", textOutput("lg_msg")),
        if (!first && !admin) p(class = "small mt-2 mb-0", "New here? ", actionLink("lg_to_signup", "Create an account"))),
      footer = tagList(modalButton("Cancel"), actionButton("lg_go", if (first) "Create account" else "Log in", class = "btn-primary"))))
  }
  observeEvent(input$show_login, login_modal(FALSE))
  observeEvent(input$show_admin_login, login_modal(TRUE))
  sign_in <- function(u) { removeModal(); load_all(); fails(0)
    user(as.list(u[1, c("username", "name", "role", "client", "email", "phone", "company")])) }
  observeEvent(input$lg_go, {
    if (fails() >= 5) { output$lg_msg <- renderText("Too many failed attempts. Reload the page and try again later."); return() }
    un <- tolower(trimws(input$lg_user %||% "")); pw <- input$lg_pw %||% ""
    users <- db_read("users"); rv$users <- users
    if (no_admin(users)) {
      if (BACKEND != "local" && !identical(input$lg_key, Sys.getenv("TECHTOBO_SETUP_KEY"))) { output$lg_msg <- renderText("Wrong setup key."); return() }
      if (!nzchar(un) || nchar(pw) < 8) { output$lg_msg <- renderText("Enter a username and a password of at least 8 characters."); return() }
      if (un %in% users$username) { output$lg_msg <- renderText("That username is already taken."); return() }
      change("users", function(d) rbind(d, row_df("users", username = un, name = clip(input$lg_name, 80) %||% un, role = "director", client = "",
                                                   password_hash = sodium::password_store(pw), active = "Yes", created_at = now_str())))
      users <- rv$users
    }
    u <- users[(users$username == un | (nzchar(users$email) & tolower(users$email) == un)) & users$active == "Yes", , drop = FALSE]
    ok <- nzchar(un) && nrow(u) == 1 && isTRUE(tryCatch(sodium::password_verify(u$password_hash, pw), error = function(e) FALSE))
    if (ok && login_mode() == "admin" && u$role != "director") ok <- FALSE   # administrator login accepts administrators only
    if (!ok) { Sys.sleep(1); fails(fails() + 1); output$lg_msg <- renderText("Incorrect username or password."); return() }
    sign_in(u)
  })

  # ---- self sign-up for general users
  signups <- reactiveVal(0)
  signup_modal <- function() { output$su_msg <- renderText("")
    showModal(modalDialog(title = "Create an account", easyClose = TRUE,
      p(class = "text-muted small", "An account lets you request quotations and follow their progress."),
      textInput("su_name", "Full name", width = "100%"), textInput("su_company", "Company / organisation (optional)", width = "100%"),
      textInput("su_email", "Email address (this is your login)", width = "100%"), textInput("su_phone", "Telephone number", width = "100%"),
      passwordInput("su_pw", "Password (min 8 characters)", width = "100%"), passwordInput("su_pw2", "Confirm password", width = "100%"),
      div(class = "text-danger", textOutput("su_msg")),
      footer = tagList(modalButton("Cancel"), actionButton("su_go", "Create account", class = "btn-primary")))) }
  observeEvent(input$show_signup, signup_modal())
  observeEvent(input$lg_to_signup, signup_modal())
  observeEvent(input$su_go, {
    bad <- function(m) { output$su_msg <- renderText(m); NULL }
    nm <- clip(input$su_name, 80); em <- tolower(clip(input$su_email, 120)); ph <- clip(input$su_phone, 30); pw <- input$su_pw %||% ""
    if (signups() >= 2) return(bad("An account was already created in this session. Please log in."))
    if (!nzchar(nm)) return(bad("Enter your full name."))
    if (!valid_email(em)) return(bad("Enter a valid email address."))
    if (nchar(gsub("[^0-9]", "", ph)) < 7) return(bad("Enter a valid telephone number."))
    if (nchar(pw) < 8) return(bad("The password must be at least 8 characters."))
    if (!identical(pw, input$su_pw2 %||% "")) return(bad("The two passwords do not match."))
    users <- db_read("users")
    if (em %in% users$username || em %in% tolower(users$email)) return(bad("An account with this email already exists. Please log in instead."))
    change("users", function(d) rbind(d, row_df("users", username = em, name = nm, role = "user", client = "", password_hash = sodium::password_store(pw),
                                                 active = "Yes", email = em, phone = ph, company = clip(input$su_company, 120), created_at = now_str())))
    signups(signups() + 1)
    sign_in(rv$users[rv$users$username == em, , drop = FALSE])
    showNotification("Account created. You are now logged in.")
  })

  # ---- request a quotation (public visitors and logged-in users)
  rq_count <- reactiveVal(0)
  observeEvent(input$show_quote, { output$rq_msg <- renderText("")
    u <- user(); g <- function(f) if (is.null(u)) "" else (u[[f]] %||% "")
    showModal(modalDialog(title = "Request a quotation", size = "l", easyClose = FALSE,
      layout_columns(col_widths = c(6, 6), textInput("rq_name", "Your name", g("name"), width = "100%"),
        textInput("rq_company", "Company / organisation", if (!is.null(u) && u$role == "client") u$client else g("company"), width = "100%")),
      layout_columns(col_widths = c(6, 6), textInput("rq_email", "Email address", g("email"), width = "100%"), textInput("rq_phone", "Telephone number", g("phone"), width = "100%")),
      layout_columns(col_widths = c(6, 6), sel_in("rq_service", "Service required", c(names(SERVICES), "Other / not sure"), width = "100%"),
        textInput("rq_time", "Preferred date or timeframe", placeholder = "e.g. 20 November, or within two weeks", width = "100%")),
      textAreaInput("rq_desc", "Describe the work you need done", rows = 4, width = "100%"),
      div(class = "hp", `aria-hidden` = "true", textInput("rq_website", "Leave this field empty")),
      div(class = "text-danger", textOutput("rq_msg")),
      footer = tagList(modalButton("Cancel"), actionButton("rq_go", "Submit request", class = "btn-warning"))))
  })
  observeEvent(input$rq_go, {
    bad <- function(m) { output$rq_msg <- renderText(m); NULL }
    nm <- clip(input$rq_name, 80); em <- tolower(clip(input$rq_email, 120)); ph <- clip(input$rq_phone, 30); ds <- clip(input$rq_desc, 2000)
    if (!nzchar(nm)) return(bad("Enter your name."))
    if (!valid_email(em)) return(bad("Enter a valid email address."))
    if (nchar(gsub("[^0-9]", "", ph)) < 7) return(bad("Enter a valid telephone number."))
    if (nchar(ds) < 10) return(bad("Describe the work you need done (at least a short sentence)."))
    if (rq_count() >= 3) return(bad("You have submitted several requests in this visit. Please contact us directly for anything further."))
    no <- NULL; at <- now_str()
    if (!nzchar(input$rq_website %||% "")) {   # the hidden field is only ever filled in by automated spam
      change("requests", function(d) { no <<- next_no("REQ", d$request_no)
        rbind(d, row_df("requests", request_no = no, submitted_at = at, name = nm, company = clip(input$rq_company, 120), email = em, phone = ph,
                        service = input$rq_service %||% "", description = ds, timeframe = clip(input$rq_time, 120), status = "New",
                        username = if (is.null(user())) "" else user()$username, admin_notes = "")) })
    }
    rq_count(rq_count() + 1)
    showModal(modalDialog(title = "Request received", easyClose = TRUE,
      p("Thank you, ", nm, ". Your quotation request has been received", if (!is.null(no)) tagList(" under reference ", strong(no)), "."),
      p("Submitted on ", nice_date(substr(at, 1, 10)), " at ", substr(at, 12, 16), " (East Africa Time). We will contact you on ", em, " or ", ph, "."),
      footer = modalButton("Close")))
  })

  # ---- quotation requests: administrator view
  rq_view <- reactive(rv$requests[order(rv$requests$request_no, decreasing = TRUE),
    c("request_no", "submitted_at", "name", "company", "email", "phone", "service", "timeframe", "status", "description", "admin_notes")])
  output$rq_tbl <- renderDT({ req(is_dir()); d <- rq_view(); d$description <- ifelse(nchar(d$description) > 60, paste0(substr(d$description, 1, 60), "..."), d$description); dt(d) })
  output$rq_detail <- renderUI({ req(is_dir()); r <- sel("rq_tbl", rq_view()); if (is.null(r)) return(NULL)
    div(class = "rq-detail", h6(paste0(r$request_no, ": ", r$service)),
        p(strong(r$name), if (nzchar(r$company)) paste0(", ", r$company), " | ", r$email, " | ", r$phone),
        p(strong("Preferred timeframe: "), if (nzchar(r$timeframe)) r$timeframe else "not stated", " | ", strong("Submitted: "), r$submitted_at),
        p(r$description)) })
  observeEvent(input$rq_tbl_rows_selected, { r <- sel("rq_tbl", rq_view()); req(r, is_dir())
    updateSelectInput(session, "rq_status", selected = r$status); updateTextInput(session, "rq_notes", value = r$admin_notes) })
  observeEvent(input$rq_update, { req(need_dir()); r <- sel("rq_tbl", rq_view())
    if (is.null(r)) return(showNotification("Select a request first.", type = "warning"))
    change("requests", function(d) { i <- d$request_no == r$request_no; d$status[i] <- input$rq_status; d$admin_notes[i] <- clip(input$rq_notes, 500); d })
    showNotification("Request updated.") })

  # ---- quotation requests: the logged-in user's own
  output$myrq_tbl <- renderDT({ u <- user(); req(u); d <- rv$requests[rv$requests$username == u$username, , drop = FALSE]
    dt(d[order(d$request_no, decreasing = TRUE), c("request_no", "submitted_at", "service", "description", "timeframe", "status")]) })

  observeEvent(input$logout, user(NULL))
  observeEvent(input$refresh, { load_all(); showNotification("Data refreshed.") })

  # ---- dashboard
  output$dash <- renderUI({
    req(is_dir()); j <- rv$jobs; d <- rv$documents; inv <- d[d$type == "invoice" & d$status != "Cancelled", ]
    paid <- sum(num(rv$receipts$amount)); billed <- sum(num(inv$total))
    box <- function(t, v, cls = "") div(class = "col-lg col-md-4 col-6", div(class = paste("kpi", cls), div(class = "kpi-v", v), div(class = "kpi-t", t)))
    tagList(div(class = "row g-3 mb-3",
      box("New quotation requests", sum(rv$requests$status == "New"), "amber"),
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
  users_view <- reactive({ u <- rv$users[, c("username", "name", "role", "client", "email", "phone", "company", "active")]   # never the password hash
    u$role <- ifelse(u$role %in% names(ROLE_LABELS), unname(ROLE_LABELS[u$role]), u$role); u })
  output$us_tbl <- renderDT({ req(is_dir()); datatable(users_view(), selection = "single", rownames = FALSE, options = list(dom = "tp", pageLength = 8, scrollX = TRUE)) })
  output$us_client_ui <- renderUI(sel_in("us_client", "Client (for client role)", c("", rv$clients$name)))
  observeEvent(input$us_tbl_rows_selected, { req(is_dir()); v <- sel("us_tbl", users_view()); req(v); u <- rv$users[rv$users$username == v$username, ]
    updateTextInput(session, "us_user", value = u$username); updateTextInput(session, "us_name", value = u$name)
    updateSelectInput(session, "us_role", selected = u$role); updateSelectInput(session, "us_client", selected = u$client) })
  observeEvent(input$us_add, { req(need_dir()); un <- tolower(trimws(input$us_user)); pw <- input$us_pw %||% ""; role <- input$us_role
    msg <- function(m) { output$us_msg <- renderText(m); NULL }
    exists <- un %in% db_read("users")$username
    if (!nzchar(un)) return(msg("Enter a username."))
    if (!exists && nchar(pw) < 8) return(msg("A new user needs a password of at least 8 characters."))
    if (nzchar(pw) && nchar(pw) < 8) return(msg("The password must be at least 8 characters."))
    if (!role %in% names(ROLE_LABELS)) return(msg("Choose a role."))
    if (role == "client" && !nzchar(input$us_client %||% "")) return(msg("Choose which client this login belongs to."))
    if (un == user()$username && role != "director") return(msg("You cannot remove your own administrator role."))
    cl <- if (role == "client") input$us_client else ""; nm <- clip(input$us_name, 80)
    change("users", function(d) {
      if (un %in% d$username) { i <- d$username == un; d$role[i] <- role; d$client[i] <- cl
        if (nzchar(nm)) d$name[i] <- nm; if (nzchar(pw)) d$password_hash[i] <- sodium::password_store(pw); d }
      else rbind(d, row_df("users", username = un, name = nm %||% un, role = role, client = cl, password_hash = sodium::password_store(pw),
                           active = "Yes", created_at = now_str())) })
    updateTextInput(session, "us_pw", value = ""); msg(paste(if (exists) "Updated user" else "Added user", un)) })
  observeEvent(input$us_toggle, { req(need_dir()); u <- sel("us_tbl", users_view()); if (is.null(u)) return()
    if (u$username == user()$username) return(output$us_msg <- renderText("You cannot deactivate your own account."))
    change("users", function(d) { i <- d$username == u$username; d$active[i] <- if (d$active[i] == "Yes") "No" else "Yes"; d }) })

  # ---- property management module (R/property.R); administrators only
  pm_server(input, output, session, rv, user, is_dir, need_dir, change, st)
}

shinyApp(ui, server)
