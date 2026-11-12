# PDFs for the Property Management module. Uses the drawing helpers in pdf.R, which is left unchanged.

pm_invoice_pdf <- function(file, i, tenant, prop, unit_name, st, draft = FALSE) {
  open_pdf(file); on.exit(grDevices::dev.off())
  pdf_header(st)
  tx(MG, 1.62, "MONTHLY PROPERTY INVOICE", 20, TRUE, NAVY)
  if (draft) tx(PW - MG, 1.62, if (identical(i$status, "Cancelled")) "CANCELLED" else "DRAFT - NOT ISSUED", 12, TRUE, "#C00000", "right")

  tx(MG, 2.05, "BILL TO", 7.5, TRUE, NAVY)
  tx(MG, 2.24, tenant$name %||% i$tenant_name, 11, TRUE)
  y <- 2.42
  for (s in c(if (nzchar(tenant$phone %||% "")) paste0("Tel: ", tenant$phone), if (nzchar(tenant$email %||% "")) paste0("Email: ", tenant$email),
              paste0("Property: ", prop$name %||% "", " (", i$property_id, ")"), paste0("Unit: ", unit_name %||% ""),
              if (nzchar(prop$address %||% "")) prop$address, if (nzchar(prop$location %||% "")) prop$location)) { tx(MG, y, s, 8.5); y <- y + 0.15 }

  meta <- list(c("Invoice No.", i$invoice_no), c("Invoice date", nice_date(i$date)), c("Billing period", month_label(i$month)), c("Payment due date", nice_date(i$due_date)))
  mx <- PW - MG - 3.0; my <- 1.98
  for (m in meta) { bx(mx, my, 1.4, 0.21, MIST); bx(mx + 1.4, my, 1.6, 0.21)
    tx(mx + 0.05, my + 0.105, m[1], 7.5, TRUE); tx(PW - MG - 0.05, my + 0.105, m[2] %||% "", 8.5, m[1] == "Invoice No.", just = "right"); my <- my + 0.21 }

  y <- max(y, my) + 0.3; aw <- 1.7; dw <- CWD - aw
  bx(MG, y, dw, 0.28, NAVY, "white"); bx(MG + dw, y, aw, 0.28, NAVY, "white")
  tx(MG + 0.08, y + 0.14, "Description", 8.5, TRUE, "white"); tx(PW - MG - 0.08, y + 0.14, "Amount (KES)", 8.5, TRUE, "white", "right"); y <- y + 0.28
  rows <- list(c("Monthly Rent", i$rent), c(i$electricity_detail %||% "Electricity", i$electricity), c(i$water_detail %||% "Water Charges", i$water),
               c("Previous Outstanding Balance", i$prev_balance))
  for (r in rows) { bx(MG, y, dw, 0.3); bx(MG + dw, y, aw, 0.3, MIST)
    tx(MG + 0.08, y + 0.15, r[1], 9.5); tx(PW - MG - 0.08, y + 0.15, kes(r[2]), 9.5, just = "right"); y <- y + 0.3 }
  bx(MG, y, CWD, 0.38, NAVY, NAVY)
  tx(MG + 0.08, y + 0.19, "TOTAL AMOUNT DUE", 11, TRUE, "white"); tx(PW - MG - 0.08, y + 0.19, kes(i$total), 12, TRUE, "white", "right"); y <- y + 0.38 + 0.35

  band(MG, y, CWD, "PAYMENT INSTRUCTIONS"); y <- y + 0.34
  for (s in c(paste0("Please pay KES ", kes(i$total), " by ", nice_date(i$due_date), "."),
              paste0("Bank: ", st[["bank_name"]], ", ", st[["bank_branch"]], "   |   Account name: ", st[["bank_account_name"]], "   |   Account No.: ", st[["bank_account_no"]]),
              paste0("M-Pesa Paybill / Till: ", st[["mpesa"]]),
              paste0("Quote the invoice number ", i$invoice_no, " as the payment reference. A receipt is issued for every payment."))) { tx(MG + 0.03, y, s, 8.5); y <- y + 0.18 }
  y <- y + 0.3
  tx(MG, y, paste0("Issued by: ", st[["signatory_name"]], ", ", st[["signatory_title"]]), 8.5, col = STEEL)
  pdf_footer(1)
  invisible(file)
}

pm_receipt_pdf <- function(file, p, i, prop_name, unit_name, st) {
  open_pdf(file); on.exit(grDevices::dev.off())
  pdf_header(st)
  tx(MG, 1.62, "PAYMENT RECEIPT", 22, TRUE, NAVY)
  if (identical(p$status, "Reversed")) tx(PW - MG, 1.62, "REVERSED", 12, TRUE, "#C00000", "right")
  what <- if (p$payment_for %in% c("Rent", "Electricity", "Water")) tolower(p$payment_for) else "rent and utility charges"
  desc <- paste0(if (num(p$balance_after) > 0.005) "Part payment" else "Payment", " of ", what, " for ", month_label(p$month),
                 if (nzchar(p$remarks %||% "")) paste0(". ", p$remarks) else "")
  rows <- list(c("Receipt No.", p$receipt_no), c("Receipt date", nice_date(p$date)), c("Received from", p$tenant_name),
               c("Property / unit", paste0(prop_name %||% "", " / ", unit_name %||% "", " (", p$unit_id, ")")), c("Invoice No.", p$invoice_no),
               c("Billing period", month_label(p$month)), c("Payment method", p$method), c("Transaction code / reference", p$txn_code),
               c("Description of payment", desc), c("Invoice total (KES)", kes(i$total)), c("Outstanding balance after this payment (KES)", kes(p$balance_after)))
  y <- 2.1
  for (r in rows) { w <- wrap(r[2] %||% "", 78); h <- 0.16 + 0.15 * length(w)
    bx(MG, y, 2.7, h, MIST); bx(MG + 2.7, y, CWD - 2.7, h); tx(MG + 0.08, y + h / 2, r[1], 8.5, TRUE)
    for (k in seq_along(w)) tx(MG + 2.8, y + 0.155 + 0.15 * (k - 1), w[k], 9.5)
    y <- y + h }
  y <- y + 0.25
  bx(MG, y, CWD, 0.5, NAVY, NAVY)
  tx(MG + 0.15, y + 0.25, "AMOUNT RECEIVED (KES)", 12, TRUE, "white"); tx(PW - MG - 0.15, y + 0.25, kes(p$amount), 16, TRUE, "white", "right")
  y <- y + 1.0
  tx(MG, y, paste0("Authorised by: ", st[["signatory_name"]], ", ", st[["signatory_title"]]), 9.5)
  tx(MG, y + 0.25, paste0("Recorded by: ", p$recorded_by %||% "", " on ", p$created_at %||% ""), 8.5, col = STEEL)
  tx(MG, y + 0.75, "Signature: ______________________________", 9.5)
  tx(MG, y + 1.15, "With thanks.", 9, col = STEEL, italic = TRUE)
  pdf_footer(1)
  invisible(file)
}

# Landscape report of the billing and payment history
pm_report_pdf <- function(file, h, subtitle, st) {
  W <- 11.69; H <- 8.27; M <- 0.4
  if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf(file, width = W, height = H, onefile = TRUE) else grDevices::pdf(file, width = W, height = H)
  on.exit(grDevices::dev.off())
  money <- c("Rent", "Electricity", "Water", "Previous balance", "Total invoice", "Amount paid", "Remaining balance")
  cols <- c("Billing month", "Property", "Unit", "Tenant", "Invoice No.", money[1:6], "Payment date", "Transaction code", "Receipt No.", "Remaining balance", "Status")
  cw <- c(0.8, 1.0, 0.45, 1.1, 0.85, 0.62, 0.62, 0.55, 0.62, 0.68, 0.68, 0.7, 0.8, 0.85, 0.68, 0.79); cw <- cw * (W - 2 * M) / sum(cw); cx <- M + c(0, cumsum(cw))
  page <- 0; y <- 0
  newpage <- function() { if (page > 0) grid.newpage(); page <<- page + 1
    pushViewport(viewport(xscale = c(0, W), yscale = c(H, 0)))
    T <- function(x, yy, s, size = 8, bold = FALSE, col = "#222222", just = "left") grid.text(s, unit(x, "native"), unit(yy, "native"), just = c(just, "centre"),
      gp = gpar(fontsize = size, fontface = if (bold) "bold" else "plain", col = col))
    T(M, 0.45, COMPANY$name, 12, TRUE, NAVY); T(M, 0.68, "Property management: billing and payment report", 10, TRUE, NAVY)
    T(M, 0.88, subtitle, 7.5, col = STEEL); T(W - M, 0.45, paste0("Generated ", now_str(), "  |  Page ", page), 7, col = STEEL, just = "right")
    grid.lines(unit(c(M, W - M), "native"), unit(c(1.0, 1.0), "native"), gp = gpar(col = AMBER, lwd = 2))
    y <<- 1.12
    for (k in seq_along(cols)) { grid.rect(unit(cx[k], "native"), unit(y, "native"), unit(cw[k], "inches"), unit(0.3, "inches"), just = c("left", "top"), gp = gpar(fill = NAVY, col = "white", lwd = 0.5))
      lab <- strwrap(cols[k], 13); for (j in seq_along(lab)) T(cx[k] + cw[k] / 2, y + 0.15 + (j - (length(lab) + 1) / 2) * 0.1, lab[j], 6, TRUE, "white", "centre") }
    y <<- y + 0.3; T }
  T <- newpage()
  cell <- function(k, s, bold = FALSE, fill = NA, h = 0.2) { grid.rect(unit(cx[k], "native"), unit(y, "native"), unit(cw[k], "inches"), unit(h, "inches"), just = c("left", "top"), gp = gpar(fill = fill, col = MID, lwd = 0.4))
    num_col <- cols[k] %in% money
    T(if (num_col) cx[k + 1] - 0.04 else cx[k] + 0.04, y + h / 2, s, 6, bold, just = if (num_col) "right" else "left") }
  short <- function(s, n) ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "..."), s)
  for (r in seq_len(nrow(h))) {
    if (y > H - 0.7) T <- newpage()
    for (k in seq_along(cols)) { v <- h[[cols[k]]][r]; cell(k, if (cols[k] %in% money) kes(v) else short(as.character(v), round(cw[k] * 17))) }
    y <- y + 0.2
  }
  if (nrow(h)) {      # totals of the month's own charges and of payments; balances are not summed because later invoices carry earlier ones
    if (y > H - 0.7) T <- newpage()
    for (k in seq_along(cols)) cell(k, if (cols[k] %in% c("Rent", "Electricity", "Water", "Amount paid")) kes(sum(num(h[[cols[k]]]))) else if (k == 1) "TOTAL" else "", TRUE, MIST, 0.22)
  } else T(M, y + 0.2, "No records match this selection.", 8, col = STEEL)
  invisible(file)
}
