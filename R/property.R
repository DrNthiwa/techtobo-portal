# Property Management module: properties, units, tenants, monthly rent, electricity and water
# charges, consolidated monthly invoices, payments, receipts, history, reports and audit trail.
# Visible to and usable by administrators (role "director") only; every server action re-checks the role.

PM_SCHEMA <- list(
  pm_properties = c("property_id", "name", "location", "address", "type", "units_count", "owner", "start_date", "status", "created_at"),
  pm_units      = c("unit_id", "property_id", "unit_name", "status"),
  pm_tenants    = c("tenant_id", "name", "phone", "email", "id_no", "property_id", "unit_id", "start_date", "end_date",
                    "monthly_rent", "status", "created_at"),
  pm_charges    = c("charge_id", "type", "month", "property_id", "unit_id", "tenant_id", "token_value", "unit_price",
                    "prev_reading", "curr_reading", "units", "basis", "amount", "invoice_no", "updated_by", "updated_at"),
  pm_invoices   = c("invoice_no", "date", "due_date", "month", "tenant_id", "tenant_name", "property_id", "unit_id", "rent",
                    "electricity", "electricity_detail", "water", "water_detail", "prev_balance", "total", "status",
                    "cancel_reason", "created_by", "created_at"),
  pm_payments   = c("payment_ref", "receipt_no", "invoice_no", "month", "tenant_id", "tenant_name", "property_id", "unit_id",
                    "amount", "date", "method", "txn_code", "payment_for", "remarks", "balance_after", "status",
                    "reversal_reason", "recorded_by", "created_at"),
  pm_audit      = c("at", "user", "action", "record", "details")
)
SCHEMA[names(PM_SCHEMA)] <- PM_SCHEMA

PM_TYPES   <- c("Residential", "Commercial", "Mixed use", "Other")
PM_METHODS <- c("M-Pesa", "Bank transfer", "Cash", "Other")
PM_FOR     <- c("All charges", "Rent", "Electricity", "Water")

# ---------------------------------------------------------------- pure calculations (no Shiny)
month_label <- function(m) vapply(as.character(m), function(x) if (!is.na(x) && nzchar(x)) format(as.Date(paste0(x, "-01")), "%B %Y") else "", "", USE.NAMES = FALSE)
pm_months <- function(back = 24, fwd = 2) {
  first <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  d <- c(rev(seq(first, by = "1 month", length.out = fwd + 1)), seq(first, by = "-1 month", length.out = back + 1)[-1])
  m <- format(d, "%Y-%m"); setNames(m, month_label(m))
}
fmtn <- function(x) format(num(x), big.mark = ",", trim = TRUE, scientific = FALSE)
elec_total  <- function(token, price) round(num(token) * num(price), 2)
water_units <- function(prev, curr) round(num(curr) - num(prev), 3)
water_total <- function(prev, curr, price) round(water_units(prev, curr) * num(price), 2)

pm_paid <- function(pay, inv_no) sum(num(pay$amount[pay$invoice_no == inv_no & pay$status == "Confirmed"]))
# A tenant's live invoices, oldest first
pm_tenant_invoices <- function(inv, tid) {
  x <- inv[inv$tenant_id == tid & inv$status != "Cancelled", , drop = FALSE]
  x[order(x$month, x$invoice_no), , drop = FALSE]
}
# What the tenant owes now = unpaid part of their latest invoice (each invoice carries the previous balance forward)
pm_tenant_balance <- function(inv, pay, tid) {
  x <- pm_tenant_invoices(inv, tid); if (!nrow(x)) return(0)
  round(num(x$total[nrow(x)]) - pm_paid(pay, x$invoice_no[nrow(x)]), 2)
}
# Recalculate the status of every live invoice of one tenant from the payments on record
pm_restatus <- function(inv, pay, tid) {
  x <- pm_tenant_invoices(inv, tid); n <- nrow(x); if (!n) return(inv)
  bal <- vapply(seq_len(n), function(i) round(num(x$total[i]) - pm_paid(pay, x$invoice_no[i]), 2), 0)
  for (i in seq_len(n)) {
    s <- if (i == n) { if (bal[i] <= 0.005) "Paid" else if (num(x$total[i]) - bal[i] > 0.005) "Partially Paid" else "Unpaid" }
         else if (bal[i] <= 0.005) "Paid" else if (bal[n] <= 0.005) "Settled" else "Carried Forward"
    inv$status[inv$invoice_no == x$invoice_no[i]] <- s
  }
  inv
}
# Split the payments on one invoice across its components. A payment marked for Rent, Electricity or Water goes
# there first; anything else is applied oldest debt first: previous balance, rent, electricity, water.
pm_alloc <- function(invrow, pay) {
  comp <- c(Previous = num(invrow$prev_balance), Rent = num(invrow$rent), Electricity = num(invrow$electricity), Water = num(invrow$water))
  paid <- comp * 0; pool <- 0
  p <- pay[pay$invoice_no == invrow$invoice_no & pay$status == "Confirmed", , drop = FALSE]
  for (k in seq_len(nrow(p))) {
    a <- num(p$amount[k]); f <- p$payment_for[k]
    if (f %in% names(comp)) { use <- min(a, comp[[f]] - paid[[f]]); paid[[f]] <- paid[[f]] + use; a <- a - use }
    pool <- pool + a
  }
  for (f in names(comp)) { use <- min(pool, comp[[f]] - paid[[f]]); paid[[f]] <- paid[[f]] + use; pool <- pool - use }
  list(charged = comp, paid = round(paid, 2))
}
pm_comp_status <- function(invrow, pay, comp) {
  if (is.null(invrow) || !nrow(invrow)) return(list(paid = 0, status = "Not invoiced"))
  a <- pm_alloc(invrow, pay); ch <- a$charged[[comp]]; pd <- a$paid[[comp]]
  s <- if (pd >= ch - 0.005) "Paid" else if (invrow$status %in% c("Carried Forward", "Settled")) invrow$status else if (pd > 0.005) "Partially Paid" else "Unpaid"
  list(paid = pd, status = s)
}
# Work out a tenant's invoice for a month from the charges on record (nothing is saved here)
pm_draft <- function(t, month, charges, inv, pay) {
  ch <- charges[charges$month == month & charges$tenant_id == t$tenant_id, , drop = FALSE]
  g <- function(type) ch[ch$type == type, , drop = FALSE]
  r <- g("rent"); e <- g("electricity"); w <- g("water")
  rent <- if (nrow(r)) num(r$amount[1]) else num(t$monthly_rent)
  elec <- if (nrow(e)) num(e$amount[1]) else 0
  water <- if (nrow(w)) num(w$amount[1]) else 0
  ed <- if (nrow(e)) sprintf("Electricity (%s units x KES %s)", fmtn(e$token_value[1]), fmtn(e$unit_price[1])) else "Electricity"
  wd <- if (!nrow(w)) "Water Charges" else if (w$basis[1] == "Fixed") "Water Charges (fixed monthly charge)"
        else sprintf("Water Charges (%s units x KES %s; meter %s to %s)", fmtn(w$units[1]), fmtn(w$unit_price[1]), fmtn(w$prev_reading[1]), fmtn(w$curr_reading[1]))
  prev <- pm_tenant_balance(inv, pay, t$tenant_id)
  list(tenant_id = t$tenant_id, tenant_name = t$name, property_id = t$property_id, unit_id = t$unit_id, month = month,
       rent = rent, electricity = elec, electricity_detail = ed, water = water, water_detail = wd, prev_balance = prev,
       total = round(rent + elec + water + prev, 2))
}
# One row per invoice with its payments: the monthly billing and payment history
pm_history <- function(inv, pay, props, units, property = "", tenant = "", month = "") {
  x <- inv[inv$status != "Cancelled", , drop = FALSE]
  if (nzchar(property)) x <- x[x$property_id == property, , drop = FALSE]
  if (nzchar(tenant))   x <- x[x$tenant_id == tenant, , drop = FALSE]
  if (nzchar(month))    x <- x[x$month == month, , drop = FALSE]
  x <- x[order(x$month, x$invoice_no, decreasing = TRUE), , drop = FALSE]
  pv <- function(no, col) { p <- pay[pay$invoice_no == no & pay$status == "Confirmed", , drop = FALSE]; paste(p[[col]][nzchar(p[[col]])], collapse = "; ") }
  paid <- vapply(x$invoice_no, function(n) pm_paid(pay, n), 0)
  data.frame(
    `Billing month` = month_label(x$month), Property = props$name[match(x$property_id, props$property_id)],
    Unit = units$unit_name[match(x$unit_id, units$unit_id)], Tenant = x$tenant_name, `Invoice No.` = x$invoice_no,
    Rent = num(x$rent), Electricity = num(x$electricity), Water = num(x$water), `Previous balance` = num(x$prev_balance),
    `Total invoice` = num(x$total), `Amount paid` = unname(paid),
    `Payment date` = vapply(x$invoice_no, pv, "", col = "date"), `Transaction code` = vapply(x$invoice_no, pv, "", col = "txn_code"),
    `Receipt No.` = vapply(x$invoice_no, pv, "", col = "receipt_no"),
    `Remaining balance` = round(num(x$total) - unname(paid), 2), Status = x$status,
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
}
pm_badge <- function(s) {
  cls <- c(Paid = "ok", Settled = "ok", Confirmed = "ok", Active = "ok", `Partially Paid` = "part", Unpaid = "bad",
           `Carried Forward` = "warn", Cancelled = "mute", Reversed = "mute", Inactive = "mute", `Not invoiced` = "mute")[s]
  cls[is.na(cls)] <- "mute"
  sprintf('<span class="pm-badge %s">%s</span>', cls, htmltools::htmlEscape(s))
}

# ---------------------------------------------------------------- UI
pm_ui <- function() {
  mon <- function(id, label = "Billing month") sel_in(id, label, pm_months(), selected = format(Sys.Date(), "%Y-%m"))
  navset_pill(id = "pm_tabs",
    nav_panel("Overview",
      layout_columns(col_widths = c(3, 9), sel_in("pm_dash_month", "Period", c("All months" = "", pm_months())), div()),
      uiOutput("pm_dash")),
    nav_panel("Properties and tenants",
      h6("Properties"),
      div(class = "mb-2", actionButton("pm_prop_add", "Register property", class = "btn-primary btn-sm"),
          actionButton("pm_prop_edit", "Edit selected property", class = "btn-outline-primary btn-sm")),
      DTOutput("pm_prop_tbl"), hr(),
      h6("Rental units"),
      layout_columns(col_widths = c(4, 3, 5), uiOutput("pm_unit_prop_ui"), textInput("pm_unit_name", "Unit name / number", placeholder = "e.g. A01", width = "100%"),
        div(style = "margin-top:32px", actionButton("pm_unit_add", "Add unit", class = "btn-primary btn-sm"),
            actionButton("pm_unit_toggle", "Activate / deactivate selected unit", class = "btn-outline-secondary btn-sm"))),
      DTOutput("pm_unit_tbl"), hr(),
      h6("Tenants"),
      div(class = "mb-2", actionButton("pm_ten_add", "Register tenant", class = "btn-primary btn-sm"),
          actionButton("pm_ten_edit", "Edit selected tenant", class = "btn-outline-primary btn-sm")),
      DTOutput("pm_ten_tbl")),
    nav_panel("Rent Management",
      layout_columns(col_widths = c(3, 9), mon("pm_rent_month"),
        div(style = "margin-top:32px", actionButton("pm_rent_gen", "Generate rent charges for this month", class = "btn-primary btn-sm"),
            actionButton("pm_rent_pay", "Record rent payment for selected", class = "btn-warning btn-sm"))),
      DTOutput("pm_rent_tbl"),
      layout_columns(col_widths = c(3, 9), numericInput("pm_rent_amt", "Rent for this month, selected tenant (KES)", NA, min = 0),
        div(style = "margin-top:32px", actionButton("pm_rent_save", "Save rent for this month", class = "btn-outline-primary btn-sm")))),
    nav_panel("Electricity Management",
      layout_columns(col_widths = c(3, 4, 2, 3), mon("pm_el_month"), uiOutput("pm_el_unit_ui"),
        numericInput("pm_el_token", "Token value (units)", NA, min = 0), numericInput("pm_el_price", "Unit price (KES)", NA, min = 0)),
      div(class = "pm-calc", textOutput("pm_el_calc")),
      div(class = "mb-3", actionButton("pm_el_save", "Save electricity charge", class = "btn-primary btn-sm"),
          actionButton("pm_el_pay", "Record electricity payment for selected", class = "btn-warning btn-sm")),
      DTOutput("pm_el_tbl")),
    nav_panel("Water Management",
      layout_columns(col_widths = c(3, 4, 5), mon("pm_wa_month"), uiOutput("pm_wa_unit_ui"),
        radioButtons("pm_wa_basis", "Billing basis", c("Meter readings" = "Metered", "Fixed monthly charge" = "Fixed"), inline = TRUE)),
      conditionalPanel("input.pm_wa_basis == 'Metered'",
        layout_columns(col_widths = c(4, 4, 4), numericInput("pm_wa_prev", "Previous meter reading", NA, min = 0),
          numericInput("pm_wa_curr", "Current meter reading", NA, min = 0), numericInput("pm_wa_price", "Price per unit (KES)", NA, min = 0))),
      conditionalPanel("input.pm_wa_basis == 'Fixed'", numericInput("pm_wa_fixed", "Fixed water charge (KES)", NA, min = 0)),
      div(class = "pm-calc", textOutput("pm_wa_calc")),
      div(class = "mb-3", actionButton("pm_wa_save", "Save water charge", class = "btn-primary btn-sm"),
          actionButton("pm_wa_pay", "Record water payment for selected", class = "btn-warning btn-sm")),
      DTOutput("pm_wa_tbl")),
    nav_panel("Invoices",
      h6("Generate monthly invoices"),
      layout_columns(col_widths = c(3, 3, 2, 2, 2), mon("pm_inv_month"), uiOutput("pm_inv_tenant_ui"),
        dateInput("pm_inv_date", "Invoice date", Sys.Date()), dateInput("pm_inv_due", "Payment due date", Sys.Date() + 7),
        div(style = "margin-top:32px", actionButton("pm_inv_preview", "Preview", class = "btn-primary btn-sm"))),
      uiOutput("pm_prev_ui"), hr(),
      h6("Issued invoices"),
      layout_columns(col_widths = c(3, 9), sel_in("pm_invf_month", "Show", c("All months" = "", pm_months())), div()),
      DTOutput("pm_inv_tbl"),
      div(class = "mt-2", downloadButton("pm_inv_pdf", "Download PDF of selected", class = "btn-primary btn-sm"),
          actionButton("pm_inv_pay", "Record payment for selected", class = "btn-warning btn-sm")),
      layout_columns(col_widths = c(6, 6), textInput("pm_inv_reason", "Reason for cancelling the selected invoice", width = "100%"),
        div(style = "margin-top:32px", actionButton("pm_inv_cancel", "Cancel selected invoice", class = "btn-outline-secondary btn-sm"))),
      p(class = "text-muted small", "An issued invoice cannot be edited. An invoice with no payments can be cancelled and reissued; it stays on record as Cancelled.")),
    nav_panel("Payments and receipts",
      layout_columns(col_widths = c(4, 8), uiOutput("pm_payf_tenant_ui"),
        div(style = "margin-top:32px", actionButton("pm_pay_new", "Record a payment", class = "btn-warning btn-sm"))),
      DTOutput("pm_pay_tbl"),
      div(class = "mt-2", downloadButton("pm_rcpt_pdf", "Download receipt PDF of selected", class = "btn-primary btn-sm")),
      layout_columns(col_widths = c(6, 6), textInput("pm_rev_reason", "Reason for reversing the selected payment", width = "100%"),
        div(style = "margin-top:32px", actionButton("pm_pay_reverse", "Reverse selected payment", class = "btn-outline-secondary btn-sm"))),
      p(class = "text-muted small", "Payments are never deleted. A payment recorded in error is reversed, with a reason, and stays on record.")),
    nav_panel("History and reports",
      layout_columns(col_widths = c(3, 3, 3, 3), uiOutput("pm_hf_prop_ui"), uiOutput("pm_hf_tenant_ui"),
        sel_in("pm_hf_month", "Billing month", c("All months" = "", pm_months())),
        div(style = "margin-top:32px", downloadButton("pm_rep_xlsx", "Excel", class = "btn-primary btn-sm"),
            downloadButton("pm_rep_pdf", "PDF", class = "btn-primary btn-sm"))),
      DTOutput("pm_hist_tbl")),
    nav_panel("Audit trail", p(class = "text-muted small", "Every change to properties, tenants, charges, invoices and payments, newest first."),
      DTOutput("pm_audit_tbl"))
  )
}

# ---------------------------------------------------------------- server
pm_server <- function(input, output, session, rv, user, is_dir, need_dir, change, st) {
  load_pm <- function() for (t in names(PM_SCHEMA)) rv[[t]] <- db_read(t)
  observeEvent(user(), { if (is_dir()) load_pm() }, ignoreNULL = TRUE)
  observeEvent(input$refresh, { if (is_dir()) load_pm() })
  P <- function(t) { req(is_dir()); x <- rv[[t]]; req(!is.null(x)); x }           # table access, administrators only
  warn <- function(m, type = "warning") { showNotification(m, type = type, duration = 8); NULL }
  audit <- function(action, record, details = "") change("pm_audit", function(d)
    rbind(d, row_df("pm_audit", at = now_str(), user = user()$username, action = action, record = record, details = details)))
  pdt <- function(df, multi = FALSE, selected = NULL, len = 10) {
    b <- which(names(df) == "Status"); if (length(b)) df[[b]] <- pm_badge(df[[b]])
    datatable(df, rownames = FALSE, escape = if (length(b)) -b else TRUE,
              selection = if (multi) list(mode = "multiple", selected = selected) else "single",
              options = list(pageLength = len, scrollX = TRUE))
  }
  sel <- function(id, df) { i <- input[[paste0(id, "_rows_selected")]]; if (length(i) == 1 && i <= nrow(df)) df[i, , drop = FALSE] else NULL }
  pname <- function(id) { p <- rv$pm_properties; p$name[match(id, p$property_id)] }
  uname <- function(id) { u <- rv$pm_units; u$unit_name[match(id, u$unit_id)] }
  ulabel <- function(id) paste0(pname(rv$pm_units$property_id[match(id, rv$pm_units$unit_id)]), " / ", uname(id))
  active_tenant <- function(unit_id, tenants = rv$pm_tenants) tenants[tenants$unit_id == unit_id & tenants$status == "Active", , drop = FALSE]
  month_invoice <- function(tid, month, inv = rv$pm_invoices) inv[inv$tenant_id == tid & inv$month == month & inv$status != "Cancelled", , drop = FALSE]

  # ================= properties, units, tenants
  props_view <- reactive({ p <- P("pm_properties")
    data.frame(Reference = p$property_id, Property = p$name, Location = p$location, `Physical address` = p$address, Type = p$type,
               Units = p$units_count, Owner = p$owner, `Managed since` = p$start_date, Status = p$status, check.names = FALSE, stringsAsFactors = FALSE) })
  output$pm_prop_tbl <- renderDT(pdt(props_view(), len = 5))
  editing_prop <- reactiveVal(NULL)
  prop_modal <- function(p = NULL) { g <- function(f, d = "") if (is.null(p)) d else p[[f]]
    nxt <- sprintf("PROP-%03d", max(c(0, num(sub("^PROP-", "", rv$pm_properties$property_id)))) + 1)
    showModal(modalDialog(title = if (is.null(p)) "Register property" else paste("Edit property", p$property_id), size = "l",
      layout_columns(col_widths = c(6, 6), textInput("pp_name", "Property name", g("name"), width = "100%"),
        if (is.null(p)) textInput("pp_id", "Property reference (unique)", nxt, width = "100%") else div(strong("Reference: "), p$property_id)),
      layout_columns(col_widths = c(6, 6), textInput("pp_loc", "Location (town / area)", g("location"), width = "100%"),
        textInput("pp_addr", "Physical address", g("address"), width = "100%")),
      layout_columns(col_widths = c(4, 4, 4), sel_in("pp_type", "Property type", PM_TYPES, g("type", PM_TYPES[1])),
        numericInput("pp_units", "Number of rental units", num(g("units_count", 1)), min = 1, step = 1), textInput("pp_owner", "Property owner", g("owner"), width = "100%")),
      layout_columns(col_widths = c(6, 6), dateInput("pp_start", "Date management commenced", safe_date(g("start_date", format(Sys.Date())))),
        sel_in("pp_status", "Status", c("Active", "Inactive"), g("status", "Active"))),
      footer = tagList(modalButton("Cancel"), actionButton("pp_save", "Save property", class = "btn-primary")))) }
  observeEvent(input$pm_prop_add, { req(need_dir()); editing_prop(NULL); prop_modal() })
  observeEvent(input$pm_prop_edit, { req(need_dir()); v <- sel("pm_prop_tbl", props_view()); if (is.null(v)) return(warn("Select a property first."))
    editing_prop(v$Reference); prop_modal(as.list(rv$pm_properties[rv$pm_properties$property_id == v$Reference, ])) })
  observeEvent(input$pp_save, { req(need_dir())
    old <- editing_prop(); id <- if (is.null(old)) toupper(clip(input$pp_id, 20)) else old; nm <- clip(input$pp_name, 100)
    if (!nzchar(nm)) return(warn("Enter the property name."))
    if (!grepl("^[A-Z0-9][A-Z0-9-]*$", id)) return(warn("The reference may contain letters, numbers and hyphens only."))
    if (is.null(old) && id %in% db_read("pm_properties")$property_id) return(warn("That property reference is already in use.", "error"))
    n <- max(1, round(num(input$pp_units)))
    r <- row_df("pm_properties", property_id = id, name = nm, location = clip(input$pp_loc, 100), address = clip(input$pp_addr, 200), type = input$pp_type,
                units_count = n, owner = clip(input$pp_owner, 100), start_date = dstr(input$pp_start), status = input$pp_status, created_at = now_str())
    change("pm_properties", function(d) if (id %in% d$property_id) { r$created_at <- d$created_at[d$property_id == id]; d[d$property_id == id, ] <- r; d } else rbind(d, r))
    if (is.null(old) && n == 1)   # a single-unit property gets its one unit automatically
      change("pm_units", function(d) rbind(d, row_df("pm_units", unit_id = paste0(id, "/MAIN"), property_id = id, unit_name = "Main", status = "Active")))
    audit(if (is.null(old)) "Property registered" else "Property updated", id, paste0(nm, "; ", input$pp_type, "; units ", n, "; ", input$pp_status))
    removeModal(); showNotification("Property saved.") })

  units_view <- reactive({ u <- P("pm_units"); t <- P("pm_tenants")
    occ <- vapply(u$unit_id, function(i) paste(t$name[t$unit_id == i & t$status == "Active"], collapse = ", "), "")
    data.frame(`Unit reference` = u$unit_id, Property = pname(u$property_id), Unit = u$unit_name, `Current tenant` = ifelse(nzchar(occ), occ, "Vacant"),
               Status = u$status, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_unit_tbl <- renderDT(pdt(units_view(), len = 5))
  output$pm_unit_prop_ui <- renderUI({ p <- P("pm_properties"); sel_in("pm_unit_prop", "Property", named(p$property_id, paste0(p$name, " (", p$property_id, ")"))) })
  observeEvent(input$pm_unit_add, { req(need_dir()); pid <- input$pm_unit_prop %||% ""; un <- clip(input$pm_unit_name, 30)
    if (!nzchar(pid) || !nzchar(un)) return(warn("Choose a property and enter the unit name."))
    id <- paste0(pid, "/", toupper(gsub("[^A-Za-z0-9-]", "", un)))
    if (id %in% db_read("pm_units")$unit_id) return(warn("That unit already exists for this property.", "error"))
    change("pm_units", function(d) rbind(d, row_df("pm_units", unit_id = id, property_id = pid, unit_name = un, status = "Active")))
    audit("Unit registered", id); updateTextInput(session, "pm_unit_name", value = ""); showNotification("Unit added.") })
  observeEvent(input$pm_unit_toggle, { req(need_dir()); v <- sel("pm_unit_tbl", units_view()); if (is.null(v)) return(warn("Select a unit first."))
    id <- v$`Unit reference`
    if (v$Status == "Active" && nrow(active_tenant(id))) return(warn("This unit has an active tenant. Mark the tenancy inactive first.", "error"))
    change("pm_units", function(d) { i <- d$unit_id == id; d$status[i] <- if (d$status[i] == "Active") "Inactive" else "Active"; d })
    audit("Unit status changed", id, rv$pm_units$status[rv$pm_units$unit_id == id]) })

  tenants_view <- reactive({ t <- P("pm_tenants"); t <- t[order(t$status, t$name), , drop = FALSE]
    data.frame(`Tenant ID` = t$tenant_id, Tenant = t$name, Telephone = t$phone, Email = t$email, `ID / reference No.` = t$id_no, Property = pname(t$property_id),
               Unit = uname(t$unit_id), `Tenancy start` = t$start_date, `Tenancy end` = t$end_date, `Monthly rent (KES)` = kes(t$monthly_rent),
               Status = t$status, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_ten_tbl <- renderDT(pdt(tenants_view()))
  editing_ten <- reactiveVal(NULL)
  ten_modal <- function(t = NULL) { g <- function(f, d = "") if (is.null(t)) d else t[[f]]
    u <- rv$pm_units[rv$pm_units$status == "Active" | rv$pm_units$unit_id == g("unit_id"), , drop = FALSE]
    showModal(modalDialog(title = if (is.null(t)) "Register tenant" else paste("Edit tenant", t$tenant_id), size = "l",
      layout_columns(col_widths = c(6, 6), textInput("pt_name", "Tenant's full name", g("name"), width = "100%"), textInput("pt_phone", "Telephone number", g("phone"), width = "100%")),
      layout_columns(col_widths = c(6, 6), textInput("pt_email", "Email address", g("email"), width = "100%"), textInput("pt_idno", "Identification / reference number (optional)", g("id_no"), width = "100%")),
      layout_columns(col_widths = c(6, 6), sel_in("pt_unit", "Property and unit occupied", named(u$unit_id, vapply(u$unit_id, ulabel, "")), g("unit_id", NULL)),
        numericInput("pt_rent", "Monthly rent (KES)", num(g("monthly_rent", 0)), min = 0)),
      layout_columns(col_widths = c(4, 4, 4), dateInput("pt_start", "Tenancy commencement date", safe_date(g("start_date", format(Sys.Date())))),
        sel_in("pt_status", "Tenancy status", c("Active", "Inactive"), g("status", "Active")),
        dateInput("pt_end", "Tenancy end date (if inactive)", { e <- safe_date(g("end_date", NA)); if (is.na(e)) NA else e })),
      if (!is.null(t)) p(class = "text-muted small", "When a tenant moves out, set the status to Inactive. Their invoices, payments and receipts are kept."),
      footer = tagList(modalButton("Cancel"), actionButton("pt_save", "Save tenant", class = "btn-primary")))) }
  observeEvent(input$pm_ten_add, { req(need_dir()); if (!nrow(rv$pm_units)) return(warn("Register a property and its units first.")); editing_ten(NULL); ten_modal() })
  observeEvent(input$pm_ten_edit, { req(need_dir()); v <- sel("pm_ten_tbl", tenants_view()); if (is.null(v)) return(warn("Select a tenant first."))
    editing_ten(v$`Tenant ID`); ten_modal(as.list(rv$pm_tenants[rv$pm_tenants$tenant_id == v$`Tenant ID`, ])) })
  observeEvent(input$pt_save, { req(need_dir())
    old <- editing_ten(); nm <- clip(input$pt_name, 100); unit <- input$pt_unit %||% ""; em <- tolower(clip(input$pt_email, 120)); rent <- num(input$pt_rent)
    if (!nzchar(nm)) return(warn("Enter the tenant's full name."))
    if (!nzchar(unit)) return(warn("Choose the property and unit."))
    if (nzchar(em) && !valid_email(em)) return(warn("Enter a valid email address, or leave it empty."))
    if (rent < 0) return(warn("Monthly rent cannot be negative."))
    cur <- db_read("pm_tenants")
    if (input$pt_status == "Active" && any(cur$unit_id == unit & cur$status == "Active" & cur$tenant_id != (old %||% "")))
      return(warn("That unit already has an active tenant. Mark the current tenancy inactive first.", "error"))
    id <- NULL; before <- if (!is.null(old)) cur[cur$tenant_id == old, ] else NULL
    change("pm_tenants", function(d) {
      id <<- old %||% sprintf("TEN-%04d", max(c(0, num(sub("^TEN-", "", d$tenant_id)))) + 1)
      r <- row_df("pm_tenants", tenant_id = id, name = nm, phone = clip(input$pt_phone, 30), email = em, id_no = clip(input$pt_idno, 40),
                  property_id = rv$pm_units$property_id[match(unit, rv$pm_units$unit_id)], unit_id = unit, start_date = dstr(input$pt_start),
                  end_date = if (input$pt_status == "Inactive") (dstr(input$pt_end) %||% format(Sys.Date())) else "", monthly_rent = rent,
                  status = input$pt_status, created_at = now_str())
      if (id %in% d$tenant_id) { r$created_at <- d$created_at[d$tenant_id == id]; d[d$tenant_id == id, ] <- r; d } else rbind(d, r) })
    det <- paste0(nm, "; ", unit, "; rent ", kes(rent), "; ", input$pt_status)
    if (!is.null(before)) det <- paste0(det, " (was: ", before$unit_id, "; rent ", kes(before$monthly_rent), "; ", before$status, ")")
    audit(if (is.null(old)) "Tenant registered" else "Tenant updated", id, det)
    removeModal(); showNotification("Tenant saved.") })

  # ================= charges (shared helpers)
  # Insert or update one charge. Refused once the charge is on an issued invoice.
  save_charge <- function(type, month, tenant, fields) {
    key <- paste(type, month, tenant$unit_id, tenant$tenant_id, sep = "|"); locked <- FALSE; was <- NULL
    change("pm_charges", function(d) {
      i <- d$charge_id == key
      if (any(i) && nzchar(d$invoice_no[i][1])) { locked <<- TRUE; return(d) }
      r <- do.call(row_df, c(list("pm_charges", charge_id = key, type = type, month = month, property_id = tenant$property_id, unit_id = tenant$unit_id,
                                  tenant_id = tenant$tenant_id, invoice_no = "", updated_by = user()$username, updated_at = now_str()), fields))
      if (any(i)) { was <<- d$amount[i][1]; d[i, ] <- r; d } else rbind(d, r) })
    if (locked) return(warn("This charge is already on an issued invoice and cannot be changed. Cancel the invoice first if it has no payments.", "error"))
    audit(paste0(tools::toTitleCase(type), " charge ", if (is.null(was)) "entered" else "updated"), key,
          paste0("KES ", kes(fields$amount), if (!is.null(was)) paste0(" (was ", kes(was), ")") else ""))
    TRUE
  }
  unit_choices <- function() { t <- rv$pm_tenants[rv$pm_tenants$status == "Active", , drop = FALSE]
    named(t$unit_id, paste0(vapply(t$unit_id, ulabel, ""), " - ", t$name)) }
  # Rows for the electricity / water tables of one month
  charge_rows <- function(type, month, comp) {
    ch <- P("pm_charges"); ch <- ch[ch$type == type & ch$month == month, , drop = FALSE]; t <- P("pm_tenants"); inv <- P("pm_invoices"); pay <- P("pm_payments")
    cs <- lapply(seq_len(nrow(ch)), function(k) pm_comp_status(month_invoice(ch$tenant_id[k], month, inv), pay, comp))
    paid <- vapply(cs, function(x) x$paid, 0)
    list(ch = ch, base = data.frame(Property = pname(ch$property_id), `Unit reference` = ch$unit_id, Tenant = t$name[match(ch$tenant_id, t$tenant_id)],
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL),
      tail = data.frame(`Invoice No.` = ch$invoice_no, `Paid (KES)` = kes(paid), `Balance (KES)` = kes(num(ch$amount) - paid),
                        Status = vapply(cs, function(x) x$status, ""), check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL))
  }

  # ================= rent
  rent_view <- reactive({ m <- input$pm_rent_month; req(m); t <- P("pm_tenants"); ch <- P("pm_charges"); inv <- P("pm_invoices"); pay <- P("pm_payments")
    rc <- ch[ch$type == "rent" & ch$month == m, , drop = FALSE]
    t <- t[t$status == "Active" | t$tenant_id %in% rc$tenant_id | t$tenant_id %in% inv$tenant_id[inv$month == m & inv$status != "Cancelled"], , drop = FALSE]
    charged <- rc$amount[match(t$tenant_id, rc$tenant_id)]
    cs <- lapply(t$tenant_id, function(i) pm_comp_status(month_invoice(i, m, inv), pay, "Rent"))
    paid <- vapply(cs, function(x) x$paid, 0); invno <- vapply(t$tenant_id, function(i) paste(month_invoice(i, m, inv)$invoice_no, collapse = ""), "")
    data.frame(`Tenant ID` = t$tenant_id, Property = pname(t$property_id), Unit = uname(t$unit_id), Tenant = t$name, `Monthly rent (KES)` = kes(t$monthly_rent),
               `Rent charged this month (KES)` = ifelse(is.na(charged), "not generated", kes(charged)), `Invoice No.` = invno, `Rent paid (KES)` = kes(paid),
               `Rent balance (KES)` = ifelse(is.na(charged), "", kes(num(charged) - paid)), Status = vapply(cs, function(x) x$status, ""),
               check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_rent_tbl <- renderDT(pdt(rent_view()))
  observeEvent(input$pm_rent_gen, { req(need_dir()); m <- input$pm_rent_month; t <- rv$pm_tenants[rv$pm_tenants$status == "Active", , drop = FALSE]; n <- 0
    if (!nrow(t)) return(warn("There are no active tenants."))
    change("pm_charges", function(d) { new <- t[!paste("rent", m, t$unit_id, t$tenant_id, sep = "|") %in% d$charge_id, , drop = FALSE]; n <<- nrow(new)
      if (!n) return(d)
      rbind(d, row_df("pm_charges", charge_id = paste("rent", m, new$unit_id, new$tenant_id, sep = "|"), type = "rent", month = m, property_id = new$property_id,
                      unit_id = new$unit_id, tenant_id = new$tenant_id, basis = "Monthly rent", amount = num(new$monthly_rent), invoice_no = "",
                      updated_by = user()$username, updated_at = now_str())) })
    if (n) audit("Rent charges generated", m, paste(n, "tenant(s)"))
    showNotification(if (n) paste("Rent charges generated for", n, "tenant(s).") else "Rent charges already exist for every active tenant this month.") })
  observeEvent(input$pm_rent_tbl_rows_selected, { v <- sel("pm_rent_tbl", rent_view()); req(v, is_dir())
    x <- v$`Rent charged this month (KES)`; updateNumericInput(session, "pm_rent_amt", value = if (x == "not generated") num(gsub(",", "", v$`Monthly rent (KES)`)) else num(gsub(",", "", x))) })
  observeEvent(input$pm_rent_save, { req(need_dir()); v <- sel("pm_rent_tbl", rent_view()); if (is.null(v)) return(warn("Select a tenant first."))
    if (is.na(input$pm_rent_amt) || input$pm_rent_amt < 0) return(warn("Enter the rent amount."))
    t <- rv$pm_tenants[rv$pm_tenants$tenant_id == v$`Tenant ID`, ]
    if (isTRUE(save_charge("rent", input$pm_rent_month, t, list(basis = "Monthly rent", amount = round(input$pm_rent_amt, 2))))) showNotification("Rent for the month saved.") })

  # ================= electricity
  output$pm_el_unit_ui <- renderUI({ P("pm_tenants"); sel_in("pm_el_unit", "Property / unit and tenant", unit_choices()) })
  output$pm_el_calc <- renderText(paste0("Total electricity charge = token value x unit price = KES ", kes(elec_total(input$pm_el_token, input$pm_el_price))))
  el_rows <- reactive({ m <- input$pm_el_month; req(m); charge_rows("electricity", m, "Electricity") })
  el_view <- reactive({ r <- el_rows(); cbind(r$base, data.frame(`Billing month` = month_label(r$ch$month), `Token value (units)` = fmtn(r$ch$token_value),
    `Unit price (KES)` = kes(r$ch$unit_price), `Total charge (KES)` = kes(r$ch$amount), check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL), r$tail) })
  output$pm_el_tbl <- renderDT(pdt(el_view()))
  observeEvent(input$pm_el_tbl_rows_selected, { i <- input$pm_el_tbl_rows_selected; ch <- el_rows()$ch; req(is_dir(), length(i) == 1, i <= nrow(ch))
    updateSelectInput(session, "pm_el_unit", selected = ch$unit_id[i]); updateNumericInput(session, "pm_el_token", value = num(ch$token_value[i]))
    updateNumericInput(session, "pm_el_price", value = num(ch$unit_price[i])) })
  observeEvent(input$pm_el_save, { req(need_dir()); t <- active_tenant(input$pm_el_unit %||% "")
    if (!nrow(t)) return(warn("Choose a unit with an active tenant."))
    if (is.na(input$pm_el_token) || is.na(input$pm_el_price) || input$pm_el_token < 0 || input$pm_el_price < 0) return(warn("Enter the token value and the unit price."))
    if (isTRUE(save_charge("electricity", input$pm_el_month, t[1, ], list(token_value = input$pm_el_token, unit_price = input$pm_el_price, units = input$pm_el_token,
        basis = "Token", amount = elec_total(input$pm_el_token, input$pm_el_price))))) showNotification("Electricity charge saved.") })

  # ================= water
  output$pm_wa_unit_ui <- renderUI({ P("pm_tenants"); sel_in("pm_wa_unit", "Property / unit and tenant", unit_choices()) })
  observeEvent(list(input$pm_wa_unit, input$pm_wa_month), { req(is_dir(), input$pm_wa_unit, input$pm_wa_month)   # suggest last month's closing reading
    ch <- rv$pm_charges; ch <- ch[ch$type == "water" & ch$unit_id == input$pm_wa_unit & ch$basis == "Metered" & ch$month < input$pm_wa_month, , drop = FALSE]
    if (nrow(ch)) updateNumericInput(session, "pm_wa_prev", value = num(ch$curr_reading[which.max(rank(ch$month, ties.method = "first"))])) })
  wa_amount <- reactive(if (identical(input$pm_wa_basis, "Fixed")) round(num(input$pm_wa_fixed), 2) else water_total(input$pm_wa_prev, input$pm_wa_curr, input$pm_wa_price))
  output$pm_wa_calc <- renderText(if (identical(input$pm_wa_basis, "Fixed")) paste0("Total water charge (fixed) = KES ", kes(wa_amount()))
    else paste0("Units consumed = current - previous = ", fmtn(water_units(input$pm_wa_prev, input$pm_wa_curr)), "; total water charge = units x price = KES ", kes(wa_amount())))
  wa_rows <- reactive({ m <- input$pm_wa_month; req(m); charge_rows("water", m, "Water") })
  wa_view <- reactive({ r <- wa_rows(); f <- r$ch$basis == "Fixed"
    cbind(r$base, data.frame(`Billing month` = month_label(r$ch$month), Basis = r$ch$basis, `Previous reading` = ifelse(f, "", fmtn(r$ch$prev_reading)),
      `Current reading` = ifelse(f, "", fmtn(r$ch$curr_reading)), `Units consumed` = ifelse(f, "", fmtn(r$ch$units)), `Price per unit (KES)` = ifelse(f, "", kes(r$ch$unit_price)),
      `Total charge (KES)` = kes(r$ch$amount), check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL), r$tail) })
  output$pm_wa_tbl <- renderDT(pdt(wa_view()))
  observeEvent(input$pm_wa_tbl_rows_selected, { i <- input$pm_wa_tbl_rows_selected; ch <- wa_rows()$ch; req(is_dir(), length(i) == 1, i <= nrow(ch))
    updateSelectInput(session, "pm_wa_unit", selected = ch$unit_id[i]); updateRadioButtons(session, "pm_wa_basis", selected = ch$basis[i])
    if (ch$basis[i] == "Fixed") updateNumericInput(session, "pm_wa_fixed", value = num(ch$amount[i]))
    else { updateNumericInput(session, "pm_wa_prev", value = num(ch$prev_reading[i])); updateNumericInput(session, "pm_wa_curr", value = num(ch$curr_reading[i]))
           updateNumericInput(session, "pm_wa_price", value = num(ch$unit_price[i])) } })
  observeEvent(input$pm_wa_save, { req(need_dir()); t <- active_tenant(input$pm_wa_unit %||% "")
    if (!nrow(t)) return(warn("Choose a unit with an active tenant."))
    if (input$pm_wa_basis == "Fixed") {
      if (is.na(input$pm_wa_fixed) || input$pm_wa_fixed < 0) return(warn("Enter the fixed water charge."))
      f <- list(basis = "Fixed", amount = round(input$pm_wa_fixed, 2))
    } else {
      if (anyNA(c(input$pm_wa_prev, input$pm_wa_curr, input$pm_wa_price)) || input$pm_wa_price < 0) return(warn("Enter both meter readings and the price per unit."))
      if (input$pm_wa_curr < input$pm_wa_prev) return(warn("The current reading cannot be lower than the previous reading.", "error"))
      f <- list(basis = "Metered", prev_reading = input$pm_wa_prev, curr_reading = input$pm_wa_curr, units = water_units(input$pm_wa_prev, input$pm_wa_curr),
                unit_price = input$pm_wa_price, amount = water_total(input$pm_wa_prev, input$pm_wa_curr, input$pm_wa_price))
    }
    if (isTRUE(save_charge("water", input$pm_wa_month, t[1, ], f))) showNotification("Water charge saved.") })

  # ================= invoices: preview, issue, list, cancel
  output$pm_inv_tenant_ui <- renderUI({ t <- P("pm_tenants"); t <- t[t$status == "Active", , drop = FALSE]
    sel_in("pm_inv_tenant", "Tenant", c("All active tenants" = "", named(t$tenant_id, paste0(t$name, " - ", vapply(t$unit_id, ulabel, ""))))) })
  drafts <- reactiveVal(NULL); skipped <- reactiveVal(character(0))
  build_drafts <- function(m, tid, inv, pay, charges, tenants) {
    t <- tenants[tenants$status == "Active", , drop = FALSE]; if (nzchar(tid)) t <- t[t$tenant_id == tid, , drop = FALSE]
    out <- list(); skip <- character(0)
    for (k in seq_len(nrow(t))) { tk <- t[k, ]; live <- pm_tenant_invoices(inv, tk$tenant_id)
      if (any(live$month == m)) { skip <- c(skip, paste0(tk$name, " (already invoiced for ", month_label(m), ")")); next }
      if (any(live$month > m)) { skip <- c(skip, paste0(tk$name, " (has a later invoice)")); next }
      out[[length(out) + 1]] <- as.data.frame(pm_draft(tk, m, charges, inv, pay), stringsAsFactors = FALSE) }
    list(d = if (length(out)) do.call(rbind, out) else NULL, skip = skip)
  }
  observeEvent(input$pm_inv_preview, { req(need_dir()); load_pm()
    b <- build_drafts(input$pm_inv_month, input$pm_inv_tenant %||% "", rv$pm_invoices, rv$pm_payments, rv$pm_charges, rv$pm_tenants)
    drafts(b$d); skipped(b$skip); if (is.null(b$d)) warn("There is nothing to invoice for this selection.") })
  drafts_view <- reactive({ d <- drafts(); req(d)
    data.frame(Tenant = d$tenant_name, Property = pname(d$property_id), Unit = uname(d$unit_id), `Billing month` = month_label(d$month), `Rent (KES)` = kes(d$rent),
               `Electricity (KES)` = kes(d$electricity), `Water (KES)` = kes(d$water), `Previous balance (KES)` = kes(d$prev_balance), `Total due (KES)` = kes(d$total),
               check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_prev_tbl <- renderDT({ req(is_dir()); v <- drafts_view(); pdt(v, multi = TRUE, selected = seq_len(nrow(v)), len = 25) })
  output$pm_prev_ui <- renderUI({ req(is_dir())
    tagList(if (length(skipped())) p(class = "text-muted small", "Not included: ", paste(skipped(), collapse = "; "), "."),
      if (!is.null(drafts())) div(class = "pm-preview", p(strong("Preview (not yet issued). "), "Check the amounts. Untick any tenant you do not want to invoice now by clicking the row."),
        DTOutput("pm_prev_tbl"),
        div(class = "mt-2", actionButton("pm_inv_issue", "Issue selected invoices", class = "btn-warning btn-sm"),
            downloadButton("pm_prev_pdf", "Download draft PDF of first selected", class = "btn-outline-primary btn-sm"),
            actionButton("pm_prev_clear", "Discard preview", class = "btn-outline-secondary btn-sm")))) })
  observeEvent(input$pm_prev_clear, { drafts(NULL); skipped(character(0)) })
  inv_pdf_file <- function(file, i, draft = FALSE) {
    t <- rv$pm_tenants[rv$pm_tenants$tenant_id == i$tenant_id, ]; p <- rv$pm_properties[rv$pm_properties$property_id == i$property_id, ]
    pm_invoice_pdf(file, i, if (nrow(t)) as.list(t[1, ]) else list(name = i$tenant_name), if (nrow(p)) as.list(p[1, ]) else list(name = ""), uname(i$unit_id), st(), draft)
  }
  output$pm_prev_pdf <- downloadHandler(filename = function() "DRAFT-property-invoice.pdf", content = function(file) { req(is_dir()); d <- drafts(); i <- input$pm_prev_tbl_rows_selected
    validate(need(!is.null(d) && length(i) >= 1, "Select a row in the preview first."))
    x <- as.list(d[min(i), ]); x$invoice_no <- "DRAFT"; x$date <- dstr(input$pm_inv_date); x$due_date <- dstr(input$pm_inv_due); inv_pdf_file(file, x, TRUE) })
  observeEvent(input$pm_inv_issue, { req(need_dir()); d0 <- drafts(); i <- input$pm_prev_tbl_rows_selected
    if (is.null(d0) || !length(i)) return(warn("Select at least one tenant in the preview."))
    if (is.na(input$pm_inv_date) || is.na(input$pm_inv_due) || input$pm_inv_due < input$pm_inv_date) return(warn("Check the invoice date and the due date."))
    want <- d0$tenant_id[i]; m <- d0$month[1]; issued <- NULL
    pay <- db_read("pm_payments"); charges <- db_read("pm_charges"); tenants <- db_read("pm_tenants")
    change("pm_invoices", function(d) {                                    # recomputed from the stored data at the moment of issue
      b <- build_drafts(m, "", d, pay, charges, tenants)$d; b <- b[b$tenant_id %in% want, , drop = FALSE]
      if (is.null(b) || !nrow(b)) return(d)
      for (k in seq_len(nrow(b))) { no <- next_no("PINV", d$invoice_no, input$pm_inv_date)
        d <- rbind(d, row_df("pm_invoices", invoice_no = no, date = dstr(input$pm_inv_date), due_date = dstr(input$pm_inv_due), month = m, tenant_id = b$tenant_id[k],
          tenant_name = b$tenant_name[k], property_id = b$property_id[k], unit_id = b$unit_id[k], rent = b$rent[k], electricity = b$electricity[k],
          electricity_detail = b$electricity_detail[k], water = b$water[k], water_detail = b$water_detail[k], prev_balance = b$prev_balance[k], total = b$total[k],
          status = "Unpaid", cancel_reason = "", created_by = user()$username, created_at = now_str()))
        d <- pm_restatus(d, pay, b$tenant_id[k]); b$invoice_no[k] <- no }
      issued <<- b; d })
    if (is.null(issued)) return(warn("Nothing was issued: these tenants have been invoiced already."))
    change("pm_charges", function(d) {                                      # lock the month's charges to their invoice; add the rent line if it was not generated
      for (k in seq_len(nrow(issued))) { key <- paste("rent", m, issued$unit_id[k], issued$tenant_id[k], sep = "|")
        if (!key %in% d$charge_id) d <- rbind(d, row_df("pm_charges", charge_id = key, type = "rent", month = m, property_id = issued$property_id[k], unit_id = issued$unit_id[k],
          tenant_id = issued$tenant_id[k], basis = "Monthly rent", amount = issued$rent[k], invoice_no = "", updated_by = user()$username, updated_at = now_str()))
        d$invoice_no[d$month == m & d$tenant_id == issued$tenant_id[k]] <- issued$invoice_no[k] }
      d })
    audit("Invoice issued", issued$invoice_no, paste0(issued$tenant_name, "; ", month_label(m), "; total KES ", kes(issued$total)))
    drafts(NULL); skipped(character(0)); showNotification(paste(nrow(issued), "invoice(s) issued:", paste(issued$invoice_no, collapse = ", ")), duration = 8) })

  inv_view <- reactive({ x <- P("pm_invoices"); pay <- P("pm_payments"); if (nzchar(input$pm_invf_month %||% "")) x <- x[x$month == input$pm_invf_month, , drop = FALSE]
    x <- x[order(x$invoice_no, decreasing = TRUE), , drop = FALSE]; paid <- vapply(x$invoice_no, function(n) pm_paid(pay, n), 0)
    data.frame(`Invoice No.` = x$invoice_no, `Invoice date` = x$date, `Billing month` = month_label(x$month), Tenant = x$tenant_name, Property = pname(x$property_id),
               Unit = uname(x$unit_id), `Total due (KES)` = kes(x$total), `Paid (KES)` = kes(paid), `Balance (KES)` = kes(ifelse(x$status == "Cancelled", 0, num(x$total) - paid)),
               `Due date` = x$due_date, Status = x$status, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_inv_tbl <- renderDT(pdt(inv_view()))
  sel_invoice <- function() { v <- sel("pm_inv_tbl", inv_view()); if (is.null(v)) NULL else rv$pm_invoices[rv$pm_invoices$invoice_no == v$`Invoice No.`, ] }
  output$pm_inv_pdf <- downloadHandler(filename = function() { i <- sel_invoice(); paste0(if (is.null(i)) "invoice" else i$invoice_no, ".pdf") },
    content = function(file) { req(is_dir()); i <- sel_invoice(); validate(need(!is.null(i), "Select an invoice first.")); inv_pdf_file(file, as.list(i), i$status == "Cancelled") })
  observeEvent(input$pm_inv_cancel, { req(need_dir()); i <- sel_invoice(); if (is.null(i)) return(warn("Select an invoice first."))
    why <- clip(input$pm_inv_reason, 200); no <- i$invoice_no; pay <- db_read("pm_payments"); inv <- db_read("pm_invoices"); live <- pm_tenant_invoices(inv, i$tenant_id)
    if (i$status == "Cancelled") return(warn("This invoice is already cancelled."))
    if (any(pay$invoice_no == no & pay$status == "Confirmed")) return(warn("This invoice has payments recorded and cannot be cancelled. Reverse the payments first if they were recorded in error.", "error"))
    if (!nrow(live) || live$invoice_no[nrow(live)] != no) return(warn("Only the tenant's most recent invoice can be cancelled, because later invoices carry its balance.", "error"))
    if (nchar(why) < 5) return(warn("Give a reason for cancelling."))
    change("pm_invoices", function(d) { d$status[d$invoice_no == no] <- "Cancelled"; d$cancel_reason[d$invoice_no == no] <- why; pm_restatus(d, pay, i$tenant_id) })
    change("pm_charges", function(d) { d$invoice_no[d$invoice_no == no] <- ""; d })          # charges become editable again
    audit("Invoice cancelled", no, why); updateTextInput(session, "pm_inv_reason", value = ""); showNotification("Invoice cancelled. It stays on record.") })

  # ================= payments and receipts
  open_invoices <- function(inv = rv$pm_invoices, pay = rv$pm_payments) { x <- inv[inv$status %in% c("Unpaid", "Partially Paid"), , drop = FALSE]
    x$balance <- vapply(x$invoice_no, function(n) round(num(x$total[x$invoice_no == n]) - pm_paid(pay, n), 2), 0); x[order(x$tenant_name), , drop = FALSE] }
  pay_modal <- function(invoice_no = NULL, payment_for = "All charges") { oi <- open_invoices()
    if (!nrow(oi)) return(warn("There are no invoices with a balance to pay."))
    if (!is.null(invoice_no) && !invoice_no %in% oi$invoice_no) return(warn("That invoice has no balance to pay, or its balance was carried forward to a later invoice. Record the payment against the tenant's latest invoice."))
    output$pp_msg <- renderText("")
    showModal(modalDialog(title = "Record a payment", size = "l", easyClose = FALSE,
      sel_in("pmt_inv", "Invoice", named(oi$invoice_no, paste0(oi$invoice_no, " | ", oi$tenant_name, " | ", vapply(oi$unit_id, ulabel, ""), " | ", month_label(oi$month),
        " | balance KES ", kes(oi$balance))), invoice_no, width = "100%"),
      layout_columns(col_widths = c(4, 4, 4), numericInput("pmt_amount", "Amount paid (KES)", NA, min = 0), dateInput("pmt_date", "Payment date", Sys.Date()),
        sel_in("pmt_method", "Payment method", PM_METHODS)),
      layout_columns(col_widths = c(4, 4, 4), textInput("pmt_code", "M-Pesa code / bank reference", width = "100%"), sel_in("pmt_for", "Payment for", PM_FOR, payment_for),
        textInput("pmt_remarks", "Remarks (optional)", width = "100%")),
      checkboxInput("pmt_dup", "This transaction code has been used before; record it anyway", FALSE),
      div(class = "text-danger", textOutput("pp_msg")),
      footer = tagList(modalButton("Cancel"), actionButton("pmt_save", "Confirm and record payment", class = "btn-warning")))) }
  observeEvent(input$pmt_inv, { req(is_dir()); oi <- open_invoices(); k <- match(input$pmt_inv, oi$invoice_no); if (!is.na(k)) updateNumericInput(session, "pmt_amount", value = oi$balance[k]) })
  observeEvent(input$pm_pay_new, { req(need_dir()); pay_modal() })
  observeEvent(input$pm_inv_pay, { req(need_dir()); i <- sel_invoice(); if (is.null(i)) return(warn("Select an invoice first.")); pay_modal(i$invoice_no) })
  pay_for_row <- function(tid, month, what) { if (is.null(tid)) return(warn("Select a row first.")); i <- month_invoice(tid, month)
    if (!nrow(i)) return(warn("Generate and issue the invoice for this tenant and month first; payments are recorded against invoices.")); pay_modal(i$invoice_no[1], what) }
  observeEvent(input$pm_rent_pay, { req(need_dir()); v <- sel("pm_rent_tbl", rent_view()); pay_for_row(v$`Tenant ID`, input$pm_rent_month, "Rent") })
  observeEvent(input$pm_el_pay, { req(need_dir()); i <- input$pm_el_tbl_rows_selected; ch <- el_rows()$ch; pay_for_row(if (length(i) == 1) ch$tenant_id[i] else NULL, input$pm_el_month, "Electricity") })
  observeEvent(input$pm_wa_pay, { req(need_dir()); i <- input$pm_wa_tbl_rows_selected; ch <- wa_rows()$ch; pay_for_row(if (length(i) == 1) ch$tenant_id[i] else NULL, input$pm_wa_month, "Water") })
  last_receipt <- reactiveVal(NULL)
  observeEvent(input$pmt_save, { req(need_dir()); bad <- function(m) { output$pp_msg <- renderText(m); NULL }
    amt <- round(num(input$pmt_amount), 2); code <- toupper(clip(input$pmt_code, 40)); no <- input$pmt_inv %||% ""
    inv <- db_read("pm_invoices"); i <- inv[inv$invoice_no == no, , drop = FALSE]
    if (!nrow(i) || !i$status %in% c("Unpaid", "Partially Paid")) return(bad("Choose an invoice that has a balance."))
    if (is.na(input$pmt_date) || input$pmt_date > Sys.Date() + 1) return(bad("Check the payment date."))
    if (input$pmt_method %in% c("M-Pesa", "Bank transfer") && !nzchar(code)) return(bad("Enter the M-Pesa transaction code or bank reference."))
    ref <- NULL; rcpt <- NULL; err <- NULL; bal_after <- NULL
    change("pm_payments", function(d) {                                    # checked against the stored data at the moment of saving
      bal <- round(num(i$total) - pm_paid(d, no), 2)
      if (amt <= 0) { err <<- "Enter the amount paid."; return(d) }
      if (amt > bal + 0.005) { err <<- paste0("The amount is more than the invoice balance of KES ", kes(bal), "."); return(d) }
      if (nzchar(code) && any(toupper(d$txn_code) == code & d$status == "Confirmed") && !isTRUE(input$pmt_dup)) {
        dup <- d[toupper(d$txn_code) == code & d$status == "Confirmed", ][1, ]
        err <<- paste0("Transaction code ", code, " is already recorded on ", dup$payment_ref, " (", dup$tenant_name, ", KES ", kes(dup$amount),
                       "). If this really is a different payment, tick the box above and confirm again."); return(d) }
      ref <<- next_no("PAY", d$payment_ref, input$pmt_date); rcpt <<- next_no("RCPT", d$receipt_no, input$pmt_date); bal_after <<- round(bal - amt, 2)
      rbind(d, row_df("pm_payments", payment_ref = ref, receipt_no = rcpt, invoice_no = no, month = i$month, tenant_id = i$tenant_id, tenant_name = i$tenant_name,
        property_id = i$property_id, unit_id = i$unit_id, amount = amt, date = dstr(input$pmt_date), method = input$pmt_method, txn_code = code,
        payment_for = input$pmt_for, remarks = clip(input$pmt_remarks, 200), balance_after = bal_after, status = "Confirmed", reversal_reason = "",
        recorded_by = user()$name, created_at = now_str())) })
    if (!is.null(err)) return(bad(err))
    change("pm_invoices", function(d) pm_restatus(d, rv$pm_payments, i$tenant_id))
    audit("Payment recorded", ref, paste0(rcpt, "; ", no, "; ", i$tenant_name, "; KES ", kes(amt), "; ", input$pmt_method, " ", code, "; balance after KES ", kes(bal_after)))
    last_receipt(ref)
    showModal(modalDialog(title = "Payment recorded", easyClose = TRUE,
      p("Payment ", strong(ref), " of KES ", strong(kes(amt)), " has been recorded against invoice ", no, "."),
      p("Receipt ", strong(rcpt), " has been generated. Balance remaining on the invoice: KES ", kes(bal_after), "."),
      footer = tagList(downloadButton("pm_rcpt_now", "Download receipt PDF", class = "btn-primary"), modalButton("Close")))) })
  rcpt_file <- function(file, ref) { p <- rv$pm_payments[rv$pm_payments$payment_ref == ref, ]; validate(need(nrow(p) == 1, "Select a payment first."))
    i <- rv$pm_invoices[rv$pm_invoices$invoice_no == p$invoice_no, ]; pm_receipt_pdf(file, as.list(p), as.list(i[1, ]), pname(p$property_id), uname(p$unit_id), st()) }
  output$pm_rcpt_now <- downloadHandler(filename = function() paste0(rv$pm_payments$receipt_no[match(last_receipt(), rv$pm_payments$payment_ref)], ".pdf"),
    content = function(file) { req(is_dir()); rcpt_file(file, last_receipt()) })
  output$pm_payf_tenant_ui <- renderUI({ t <- P("pm_tenants"); sel_in("pm_payf_tenant", "Tenant", c("All tenants" = "", named(t$tenant_id, paste0(t$name, " (", t$status, ")")))) })
  pay_view <- reactive({ x <- P("pm_payments"); if (nzchar(input$pm_payf_tenant %||% "")) x <- x[x$tenant_id == input$pm_payf_tenant, , drop = FALSE]
    x <- x[order(x$payment_ref, decreasing = TRUE), , drop = FALSE]
    data.frame(`Payment ref.` = x$payment_ref, `Receipt No.` = x$receipt_no, `Payment date` = x$date, Tenant = x$tenant_name, Property = pname(x$property_id),
               Unit = uname(x$unit_id), `Invoice No.` = x$invoice_no, `Billing month` = month_label(x$month), `Amount (KES)` = kes(x$amount), Method = x$method,
               `Transaction code` = x$txn_code, `Payment for` = x$payment_for, `Balance after (KES)` = kes(x$balance_after), Remarks = x$remarks,
               Status = x$status, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL) })
  output$pm_pay_tbl <- renderDT(pdt(pay_view()))
  output$pm_rcpt_pdf <- downloadHandler(filename = function() { v <- sel("pm_pay_tbl", pay_view()); paste0(if (is.null(v)) "receipt" else v$`Receipt No.`, ".pdf") },
    content = function(file) { req(is_dir()); v <- sel("pm_pay_tbl", pay_view()); validate(need(!is.null(v), "Select a payment first.")); rcpt_file(file, v$`Payment ref.`) })
  observeEvent(input$pm_pay_reverse, { req(need_dir()); v <- sel("pm_pay_tbl", pay_view()); if (is.null(v)) return(warn("Select a payment first."))
    why <- clip(input$pm_rev_reason, 200); ref <- v$`Payment ref.`; pay <- db_read("pm_payments"); p <- pay[pay$payment_ref == ref, ]
    if (p$status != "Confirmed") return(warn("This payment has already been reversed."))
    live <- pm_tenant_invoices(db_read("pm_invoices"), p$tenant_id)
    if (!nrow(live) || live$invoice_no[nrow(live)] != p$invoice_no) return(warn("This payment is on an invoice whose balance has been carried forward, so it cannot be reversed here.", "error"))
    if (nchar(why) < 5) return(warn("Give a reason for reversing the payment."))
    change("pm_payments", function(d) { k <- d$payment_ref == ref; d$status[k] <- "Reversed"; d$reversal_reason[k] <- why; d })
    change("pm_invoices", function(d) pm_restatus(d, rv$pm_payments, p$tenant_id))
    audit("Payment reversed", ref, paste0(p$receipt_no, "; ", p$invoice_no, "; KES ", kes(p$amount), "; reason: ", why))
    updateTextInput(session, "pm_rev_reason", value = ""); showNotification("Payment reversed. It stays on record.") })

  # ================= history and reports
  output$pm_hf_prop_ui <- renderUI({ p <- P("pm_properties"); sel_in("pm_hf_prop", "Property", c("All properties" = "", named(p$property_id, p$name))) })
  output$pm_hf_tenant_ui <- renderUI({ t <- P("pm_tenants"); if (nzchar(input$pm_hf_prop %||% "")) t <- t[t$property_id == input$pm_hf_prop, , drop = FALSE]
    sel_in("pm_hf_tenant", "Tenant", c("All tenants" = "", named(t$tenant_id, paste0(t$name, " (", t$status, ")")))) })
  hist <- reactive(pm_history(P("pm_invoices"), P("pm_payments"), P("pm_properties"), P("pm_units"), input$pm_hf_prop %||% "", input$pm_hf_tenant %||% "", input$pm_hf_month %||% ""))
  output$pm_hist_tbl <- renderDT({ h <- hist(); for (c in c("Rent", "Electricity", "Water", "Previous balance", "Total invoice", "Amount paid", "Remaining balance")) h[[c]] <- kes(h[[c]]); pdt(h, len = 25) })
  rep_sub <- function() paste0("Property: ", if (nzchar(input$pm_hf_prop %||% "")) pname(input$pm_hf_prop) else "All", "  |  Tenant: ",
    if (nzchar(input$pm_hf_tenant %||% "")) rv$pm_tenants$name[match(input$pm_hf_tenant, rv$pm_tenants$tenant_id)] else "All", "  |  Billing month: ",
    if (nzchar(input$pm_hf_month %||% "")) month_label(input$pm_hf_month) else "All")
  output$pm_rep_xlsx <- downloadHandler(
    filename = function() paste0("Techtobo_property_report_", format(Sys.Date()), if (requireNamespace("writexl", quietly = TRUE)) ".xlsx" else ".csv"),
    content = function(file) { req(is_dir()); h <- hist()
      if (requireNamespace("writexl", quietly = TRUE)) writexl::write_xlsx(list(`Billing and payments` = h), file) else utils::write.csv(h, file, row.names = FALSE) })
  output$pm_rep_pdf <- downloadHandler(filename = function() paste0("Techtobo_property_report_", format(Sys.Date()), ".pdf"),
    content = function(file) { req(is_dir()); pm_report_pdf(file, hist(), rep_sub(), st()) })

  # ================= overview
  output$pm_dash <- renderUI({ m <- input$pm_dash_month %||% ""; p <- P("pm_properties"); t <- P("pm_tenants"); inv <- P("pm_invoices"); pay <- P("pm_payments"); ch <- P("pm_charges")
    live <- inv[inv$status != "Cancelled", , drop = FALSE]; li <- if (nzchar(m)) live[live$month == m, , drop = FALSE] else live
    cm <- if (nzchar(m)) ch[ch$month == m, , drop = FALSE] else ch
    cp <- pay[pay$status == "Confirmed" & pay$invoice_no %in% li$invoice_no, , drop = FALSE]
    rent_paid <- sum(vapply(seq_len(nrow(li)), function(k) pm_alloc(li[k, ], pay)$paid[["Rent"]], 0))
    act <- t[t$status == "Active", , drop = FALSE]; outstanding <- sum(vapply(unique(live$tenant_id), function(i) pm_tenant_balance(inv, pay, i), 0))
    box <- function(lbl, v, cls = "") div(class = "col-lg-4 col-md-4 col-6", div(class = paste("kpi", cls), div(class = "kpi-v", v), div(class = "kpi-t", lbl)))
    tagList(div(class = "row g-3 mb-3",
      box("Properties managed (active)", sum(p$status == "Active")), box("Active tenants", nrow(act)), box("Monthly rent expected (KES)", kes(sum(num(act$monthly_rent)))),
      box("Rent collected (KES)", kes(rent_paid)), box("Electricity charges (KES)", kes(sum(num(cm$amount[cm$type == "electricity"])))),
      box("Water charges (KES)", kes(sum(num(cm$amount[cm$type == "water"])))),
      box("Total invoiced (KES)", kes(sum(num(li$rent) + num(li$electricity) + num(li$water)))), box("Payments received (KES)", kes(sum(num(cp$amount)))),
      box("Outstanding balances, all tenants (KES)", kes(outstanding), "amber")),
      p(class = "text-muted small", "Rent collected, charges, invoiced and payments follow the period chosen above. Outstanding balances are the current position across all months. ",
        "Total invoiced counts each month's own charges once, without balances brought forward.")) })

  output$pm_audit_tbl <- renderDT({ a <- P("pm_audit"); a <- a[rev(seq_len(nrow(a))), , drop = FALSE]
    pdt(data.frame(`Date and time` = a$at, User = a$user, Action = a$action, Record = a$record, Details = a$details, check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL), len = 25) })
}
