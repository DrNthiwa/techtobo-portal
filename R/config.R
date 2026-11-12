# Company constants, lists and data schema for the Techtobo portal

COMPANY <- list(
  name    = "TECHTOBO GENERAL LIMITED",
  short   = "Techtobo General Limited",
  number  = "PVT-GYUYA39",
  address = "Jobstog Complex, North Airport Road, Embakasi, Nairobi",
  box     = "P.O. Box 52198 - 00100, G.P.O Nairobi",
  tel     = "+254 700 444 644",
  email   = "techtobgeneral@gmail.com",
  tagline = "Reliable Technical Solutions, Done Right."
)

SERVICES <- c(
  "General repairs and maintenance"         = "Planned and reactive maintenance of buildings, fittings and equipment.",
  "Electrical installation and repair"      = "Wiring, distribution boards, lighting, sockets, fault-finding and repairs.",
  "Generator servicing and maintenance"     = "Routine servicing, inspection, fault diagnosis and repair of standby generators.",
  "Plumbing services"                       = "Installation and repair of water supply, drainage and sanitary fittings.",
  "Piping installation and maintenance"     = "Installation, replacement, leak repair and maintenance of pipework.",
  "Welding and fabrication"                 = "Fabrication, repair and installation of metal structures, gates, grilles and frames.",
  "Handyman services"                       = "Minor repairs, fittings, fixtures and general upkeep tasks.",
  "CCTV installation and maintenance"       = "Supply, installation, configuration and maintenance of CCTV systems.",
  "Water meter installation and maintenance" = "Installation, replacement and maintenance of water meters and fittings."
)

JOB_STATUS <- c("Requested", "Quoted", "Approved", "In progress", "Completed",
                "Client signed off", "Invoiced", "Paid", "Cancelled")
QTN_STATUS <- c("Draft", "Sent", "Accepted", "Declined")
INV_STATUS <- c("Unpaid", "Part paid", "Paid", "Cancelled")
CATEGORIES <- c("Labour", "Materials", "Equipment", "Other")
PAY_METHODS <- c("Bank transfer", "M-Pesa", "Cheque", "Cash")
REQ_STATUS <- c("New", "Contacted", "Quoted", "Closed")

# Clients shown in the "Our Clients" section of the public site.
# To add one, add a line. If a logo file exists in www/ at the given path it is shown;
# otherwise the client's name is shown as text.
PUBLIC_CLIENTS <- list(
  list(name = "Jalisa Limited", logo = "clients/jalisa-limited.png")
)

ROLE_LABELS <- c(director = "Administrator", client = "Client", user = "User")

# One tab (Google Sheet) or one CSV file (local) per table
SCHEMA <- list(
  settings  = c("key", "value"),
  users     = c("username", "name", "role", "client", "password_hash", "active",
                "email", "phone", "company", "created_at"),
  clients   = c("name", "address", "kra_pin", "contact_person", "phone", "email"),
  jobs      = c("job_no", "date_requested", "client", "requested_by", "site", "service",
                "description", "status", "start_date", "completion_date", "labour_cost",
                "materials_cost", "other_costs", "remarks"),
  documents = c("doc_no", "type", "date", "client", "job_no", "scope", "days",
                "completion_time", "payment_terms", "vat_rate", "subtotal", "vat", "total",
                "etims_cu_no", "status", "created_by", "created_at"),
  lines     = c("doc_no", "line", "description", "detail", "category", "qty", "unit",
                "unit_price", "amount"),
  receipts  = c("receipt_no", "date", "invoice_no", "client", "amount", "method",
                "reference", "received_by", "created_at"),
  requests  = c("request_no", "submitted_at", "name", "company", "email", "phone", "service",
                "description", "timeframe", "status", "username", "admin_notes")
)

DEFAULT_SETTINGS <- c(
  kra_pin = "[KRA PIN]", vat_registered = "No", vat_rate = "16",
  bank_name = "[Bank name]", bank_branch = "[Branch]",
  bank_account_name = "Techtobo General Limited", bank_account_no = "[Account number]",
  mpesa = "[Paybill / Till number]", payment_terms_days = "30", quote_validity_days = "30",
  signatory_name = "Johnson Brian Ondari", signatory_title = "Director"
)

num  <- function(x) { v <- suppressWarnings(as.numeric(x)); v[is.na(v)] <- 0; v }
kes  <- function(x) formatC(num(x), format = "f", digits = 2, big.mark = ",")
dstr <- function(d) if (length(d) == 0 || is.na(d)) "" else format(as.Date(d), "%Y-%m-%d")
nice_date <- function(s) {
  d <- tryCatch(as.Date(s), error = function(e) as.Date(NA)); ifelse(is.na(d), "", format(d, "%d %b %Y"))
}
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || (is.character(a) && a == ""))) return(b)
  a
}

valid_email <- function(x) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]{2,}$", x)
now_str <- function() format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Africa/Nairobi")
clip <- function(x, n) substr(trimws(x %||% ""), 1, n)

# Next number in a series, e.g. INV-2026-003
next_no <- function(prefix, existing, date = Sys.Date()) {
  yr <- format(as.Date(date), "%Y")
  stem <- paste0(prefix, "-", yr, "-")
  used <- existing[startsWith(existing, stem)]
  n <- if (length(used)) max(num(sub(stem, "", used, fixed = TRUE))) else 0
  sprintf("%s%03d", stem, n + 1)
}
