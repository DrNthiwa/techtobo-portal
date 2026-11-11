# Branded PDF documents drawn with base R 'grid' (no LaTeX or browser needed on the server).
library(grid)

NAVY <- "#0B2545"; STEEL <- "#5C6B7A"; AMBER <- "#F5A300"; MIST <- "#EEF1F4"; MID <- "#C9D1DA"
PW <- 8.27; PH <- 11.69; MG <- 0.55; CWD <- PW - 2 * MG

open_pdf <- function(file) {
  if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf(file, width = PW, height = PH, onefile = TRUE)
  else grDevices::pdf(file, width = PW, height = PH, paper = "a4")
  pushViewport(viewport(xscale = c(0, PW), yscale = c(PH, 0)))   # inches, y measured from the top
}
tx <- function(x, y, s, size = 9, bold = FALSE, col = "#222222", just = "left", italic = FALSE) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(invisible())
  face <- if (bold && italic) "bold.italic" else if (bold) "bold" else if (italic) "italic" else "plain"
  grid.text(s, unit(x, "native"), unit(y, "native"), just = c(just, "centre"),
            gp = gpar(fontsize = size, fontface = face, col = col))
}
bx <- function(x, y, w, h, fill = NA, border = MID, lwd = 0.6) {
  grid.rect(unit(x, "native"), unit(y, "native"), unit(w, "inches"), unit(h, "inches"),
            just = c("left", "top"), gp = gpar(fill = fill, col = border, lwd = lwd))
}
hline <- function(y, col = AMBER, lwd = 2, x0 = MG, x1 = PW - MG) {
  grid.lines(unit(c(x0, x1), "native"), unit(c(y, y), "native"), gp = gpar(col = col, lwd = lwd))
}
wrap <- function(s, width) { w <- strwrap(s %||% "", width = width); if (!length(w)) "" else w }

pdf_header <- function(st, logo_path = "www/logo.png") {
  if (file.exists(logo_path)) {
    img <- png::readPNG(logo_path)
    grid.raster(img, unit(MG, "native"), unit(0.45, "native"), width = unit(2.2, "inches"),
                height = unit(2.2 * nrow(img) / ncol(img), "inches"), just = c("left", "top"))
  }
  x <- PW - MG
  tx(x, 0.52, COMPANY$name, 10, TRUE, NAVY, "right")
  tx(x, 0.69, COMPANY$address, 7.5, col = STEEL, just = "right")
  tx(x, 0.83, COMPANY$box, 7.5, col = STEEL, just = "right")
  tx(x, 0.97, paste0("Tel: ", COMPANY$tel, "  |  Email: ", COMPANY$email), 7.5, col = STEEL, just = "right")
  tx(x, 1.11, paste0("Company No. ", COMPANY$number, "  |  KRA PIN: ", st[["kra_pin"]]), 7.5, col = STEEL, just = "right")
  hline(1.25)
}
pdf_footer <- function(page) {
  hline(PH - 0.62, NAVY, 0.8)
  s <- names(SERVICES)
  tx(PW / 2, PH - 0.50, paste(s[1:5], collapse = "  |  "), 6, TRUE, NAVY, "centre")
  tx(PW / 2, PH - 0.39, paste(s[6:9], collapse = "  |  "), 6, TRUE, NAVY, "centre")
  tx(PW / 2, PH - 0.26, paste0(COMPANY$name, "  |  Company No. ", COMPANY$number, "  |  Page ", page), 6, col = STEEL, just = "centre")
}
band <- function(x, y, w, label) { bx(x, y, w, 0.2, NAVY, NAVY); tx(x + 0.06, y + 0.1, label, 7.5, TRUE, "white") }

# type: "quotation" or "invoice". doc: one-row data frame / list. lines: data frame. client: list. st: named settings vector.
doc_pdf <- function(file, type, doc, lines, client, st) {
  inv <- type == "invoice"
  open_pdf(file); on.exit(grDevices::dev.off())
  page <- 1; pdf_header(st)
  tx(MG, 1.62, if (inv) "INVOICE" else "QUOTATION", 22, TRUE, NAVY)

  # Client block
  tx(MG, 2.05, if (inv) "BILL TO" else "PREPARED FOR", 7.5, TRUE, NAVY)
  tx(MG, 2.24, client$name %||% doc$client, 11, TRUE)
  tx(MG, 2.42, client$address %||% "", 8.5)
  tx(MG, 2.57, paste0("KRA PIN: ", client$kra_pin %||% ""), 8.5)
  tx(MG, 2.72, paste0("Attention: ", client$contact_person %||% ""), 8.5)

  # Meta block
  due <- tryCatch(as.Date(doc$date) + num(doc$days), error = function(e) as.Date(NA))
  meta <- if (inv) list(c("Invoice No.", doc$doc_no), c("Invoice date", nice_date(doc$date)),
                        c("Payment terms", paste(doc$days, "days")), c("Due date", nice_date(format(due))),
                        c("Job No.", doc$job_no), c("eTIMS CU invoice No.", doc$etims_cu_no))
          else list(c("Quotation No.", doc$doc_no), c("Quotation date", nice_date(doc$date)),
                    c("Valid until", nice_date(format(due))), c("Est. completion", doc$completion_time),
                    c("Job No.", doc$job_no))
  mx <- PW - MG - 3.0; y <- 1.98
  for (m in meta) {
    bx(mx, y, 1.4, 0.19, MIST); bx(mx + 1.4, y, 1.6, 0.19)
    tx(mx + 0.05, y + 0.095, m[1], 7.5, TRUE); tx(PW - MG - 0.05, y + 0.095, m[2] %||% "", 8.5, m[1] %in% c("Invoice No.", "Quotation No."), just = "right")
    y <- y + 0.19
  }
  y <- 3.3

  # Scope of work (quotation)
  if (!inv && nzchar(doc$scope %||% "")) {
    band(MG, y, CWD, "SCOPE OF WORK"); y <- y + 0.2
    sw <- wrap(doc$scope, 120); h <- 0.1 + 0.15 * length(sw)
    bx(MG, y, CWD, h)
    for (i in seq_along(sw)) tx(MG + 0.06, y + 0.13 + 0.15 * (i - 1), sw[i], 8.5)
    y <- y + h + 0.15
  }

  # Lines table
  cw <- c(0.3, 2.45, 1.05, 0.8, 0.5, 0.47, 0.75, 0.85); cx <- MG + c(0, cumsum(cw))
  heads <- c("#", "Description", if (inv) "Date of work" else "Specification", "Category", "Qty / Hrs", "Unit", "Unit price", "Amount (KES)")
  thead <- function() {
    for (i in seq_along(cw)) { bx(cx[i], y, cw[i], 0.26, NAVY, "white"); tx(cx[i] + cw[i] / 2, y + 0.13, heads[i], 7.5, TRUE, "white", "centre") }
    y <<- y + 0.26
  }
  thead()
  for (k in seq_len(nrow(lines))) {
    L <- lines[k, ]
    d <- wrap(L$description, 46); e <- wrap(if (inv) nice_date(L$detail) %||% L$detail else L$detail, 19)
    n <- max(length(d), length(e)); h <- 0.1 + 0.14 * n
    if (y + h > PH - 0.9) { pdf_footer(page); grid.newpage(); pushViewport(viewport(xscale = c(0, PW), yscale = c(PH, 0)))
      page <- page + 1; pdf_header(st); y <- 1.45; thead() }
    for (i in seq_along(cw)) bx(cx[i], y, cw[i], h, if (i == 8) MIST else NA)
    mid <- y + h / 2
    tx(cx[1] + cw[1] / 2, mid, as.character(k), 7.5, col = STEEL, just = "centre")
    for (i in seq_along(d)) tx(cx[2] + 0.05, y + 0.12 + 0.14 * (i - 1), d[i], 8.5)
    for (i in seq_along(e)) tx(cx[3] + cw[3] / 2, y + 0.12 + 0.14 * (i - 1), e[i], 8, just = "centre")
    tx(cx[4] + cw[4] / 2, mid, L$category, 8, just = "centre")
    tx(cx[5] + cw[5] / 2, mid, format(num(L$qty), big.mark = ","), 8.5, just = "centre")
    tx(cx[6] + cw[6] / 2, mid, L$unit, 8, just = "centre")
    tx(cx[8] - 0.05, mid, kes(L$unit_price), 8.5, just = "right")
    tx(cx[9] - 0.05, mid, kes(L$amount), 8.5, just = "right")
    y <- y + h
  }
  if (y > PH - 4.2) { pdf_footer(page); grid.newpage(); pushViewport(viewport(xscale = c(0, PW), yscale = c(PH, 0)))
    page <- page + 1; pdf_header(st); y <- 1.45 }
  y <- y + 0.2; y0 <- y

  # Totals (right)
  amt <- num(lines$amount); cat <- lines$category
  lab <- sum(amt[cat == "Labour"]); mat <- sum(amt[cat == "Materials"]); sub <- num(doc$subtotal)
  rate <- num(doc$vat_rate)
  rows <- list(c("Labour charges", kes(lab)), c("Materials and parts", kes(mat)), c("Equipment and other", kes(sub - lab - mat)),
               c("Subtotal", kes(sub)), c(if (rate > 0) paste0("VAT @ ", rate, "%") else "VAT (not applicable)", if (rate > 0) kes(doc$vat) else "-"))
  txx <- PW - MG - 2.7
  for (r in rows) {
    b <- r[1] == "Subtotal"
    bx(txx, y, 1.6, 0.2); bx(txx + 1.6, y, 1.1, 0.2, MIST)
    tx(txx + 1.55, y + 0.1, r[1], 8.5, b, just = "right"); tx(PW - MG - 0.05, y + 0.1, r[2], 8.5, b, just = "right")
    y <- y + 0.2
  }
  bx(txx, y, 2.7, 0.3, NAVY, NAVY)
  tx(txx + 1.55, y + 0.15, if (inv) "TOTAL PAYABLE (KES)" else "TOTAL QUOTED (KES)", 9.5, TRUE, "white", "right")
  tx(PW - MG - 0.05, y + 0.15, kes(doc$total), 10.5, TRUE, "white", "right")
  yt <- y + 0.3

  # Left panel: payment details (invoice) or terms (quotation)
  lw <- CWD - 2.9; y <- y0
  if (inv) {
    band(MG, y, lw, "PAYMENT DETAILS"); y <- y + 0.32
    for (s in c(paste0("Bank: ", st[["bank_name"]], ", ", st[["bank_branch"]]), paste0("Account name: ", st[["bank_account_name"]]),
                paste0("Account No.: ", st[["bank_account_no"]]), paste0("M-Pesa Paybill / Till: ", st[["mpesa"]]),
                paste0("Payment is due within ", doc$days, " days of the invoice date."))) { tx(MG + 0.03, y, s, 8.5); y <- y + 0.17 }
  } else {
    band(MG, y, lw, "TERMS"); y <- y + 0.32
    for (s in c(paste0("This quotation is valid for ", doc$days, " days from the quotation date."),
                paste0("Payment terms: ", doc$payment_terms %||% "[to be agreed]"),
                "Prices are in Kenya Shillings (KES).", "Work outside the stated scope will be quoted separately.")) {
      for (w in wrap(s, 70)) { tx(MG + 0.03, y, w, 8.5); y <- y + 0.17 } }
  }
  y <- max(y, yt) + 0.3

  # Sign-off
  half <- CWD / 2
  band(MG, y, half, if (inv) "PREPARED BY" else "PREPARED BY (TECHTOBO)")
  band(MG + half, y, half, if (inv) "AUTHORISED BY" else "ACCEPTED AND APPROVED BY (CLIENT)"); y <- y + 0.2
  for (lab_ in c("Name", "Designation", "Signature", "Date")) {
    h <- if (lab_ == "Signature") 0.36 else 0.22
    for (side in 0:1) {
      x <- MG + side * half
      bx(x, y, 1.0, h, MIST); bx(x + 1.0, y, half - 1.0, h); tx(x + 0.05, y + h / 2, lab_, 7.5, TRUE)
      v <- if (side == 0 || inv) switch(lab_, Name = st[["signatory_name"]], Designation = st[["signatory_title"]], "") else ""
      tx(x + 1.06, y + h / 2, v, 8.5)
    }
    y <- y + h
  }
  y <- y + 0.18
  note <- if (inv) c("This document is a billing statement and supporting breakdown. The official tax invoice for this transaction is the KRA eTIMS",
                     "invoice bearing a QR code and the CU invoice number quoted above.")
          else "Work will begin once this quotation is signed or a purchase order is received."
  for (s in note) { tx(MG, y, s, 7, col = STEEL, italic = TRUE); y <- y + 0.13 }
  pdf_footer(page)
  invisible(file)
}

receipt_pdf <- function(file, rc, inv_total, paid_to_date, st) {
  open_pdf(file); on.exit(grDevices::dev.off())
  pdf_header(st)
  tx(MG, 1.62, "PAYMENT RECEIPT", 22, TRUE, NAVY)
  rows <- list(c("Receipt No.", rc$receipt_no), c("Date received", nice_date(rc$date)), c("Received from", rc$client),
               c("In payment of invoice", rc$invoice_no), c("Payment method", rc$method), c("Payment reference", rc$reference),
               c("Invoice total (KES)", kes(inv_total)), c("Total paid to date (KES)", kes(paid_to_date)),
               c("Balance outstanding (KES)", kes(num(inv_total) - num(paid_to_date))))
  y <- 2.1
  for (r in rows) {
    bx(MG, y, 2.4, 0.3, MIST); bx(MG + 2.4, y, CWD - 2.4, 0.3)
    tx(MG + 0.08, y + 0.15, r[1], 9, TRUE); tx(MG + 2.5, y + 0.15, r[2] %||% "", 10)
    y <- y + 0.3
  }
  y <- y + 0.25
  bx(MG, y, CWD, 0.5, NAVY, NAVY)
  tx(MG + 0.15, y + 0.25, "AMOUNT RECEIVED (KES)", 12, TRUE, "white"); tx(PW - MG - 0.15, y + 0.25, kes(rc$amount), 16, TRUE, "white", "right")
  y <- y + 1.0
  tx(MG, y, paste0("Received by: ", rc$received_by %||% ""), 9.5)
  tx(MG, y + 0.6, "Signature: ______________________________", 9.5)
  tx(MG, y + 1.0, "With thanks.", 9, col = STEEL, italic = TRUE)
  pdf_footer(1)
  invisible(file)
}
