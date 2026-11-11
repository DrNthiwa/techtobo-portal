# Storage backend.
# - If the environment variable TECHTOBO_SHEET_ID is set, records live in that Google Sheet
#   (one tab per table), accessed with a Google service account.
# - Otherwise records are kept as CSV files in the local data/ folder (for testing on a laptop).

# Load settings from a .Renviron file in the app folder, if there is one (local use)
if (file.exists(".Renviron")) readRenviron(".Renviron")

BACKEND <- if (nzchar(Sys.getenv("TECHTOBO_SHEET_ID"))) "google" else "local"
DATA_DIR <- Sys.getenv("TECHTOBO_DATA_DIR", "data")

empty_tbl <- function(tbl) {
  as.data.frame(setNames(replicate(length(SCHEMA[[tbl]]), character(0), simplify = FALSE), SCHEMA[[tbl]]),
                stringsAsFactors = FALSE)
}

# Force a data frame into the schema: all columns present, in order, as text, no NA
conform <- function(tbl, df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (col in SCHEMA[[tbl]]) if (is.null(df[[col]])) df[[col]] <- rep("", nrow(df))
  df <- df[, SCHEMA[[tbl]], drop = FALSE]
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  df
}

gs_connect <- function() {
  json <- Sys.getenv("GS_SERVICE_ACCOUNT_JSON")
  if (nzchar(json)) {
    f <- tempfile(fileext = ".json"); writeLines(json, f)
    googlesheets4::gs4_auth(path = f)
  } else if (nzchar(Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))) {
    googlesheets4::gs4_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))
  } else stop("Google Sheet storage is selected but no service account key was provided. ",
              "Set GS_SERVICE_ACCOUNT_JSON (see README).")
  Sys.getenv("TECHTOBO_SHEET_ID")
}

db_init <- function() {
  if (BACKEND == "google") {
    ss <- gs_connect()
    have <- googlesheets4::sheet_names(ss)
    for (tbl in setdiff(names(SCHEMA), have)) googlesheets4::sheet_write(empty_tbl(tbl), ss = ss, sheet = tbl)
  } else {
    dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
    for (tbl in names(SCHEMA)) {
      f <- file.path(DATA_DIR, paste0(tbl, ".csv"))
      if (!file.exists(f)) utils::write.csv(empty_tbl(tbl), f, row.names = FALSE)
    }
  }
  # Seed defaults on first run
  st <- db_read("settings")
  miss <- setdiff(names(DEFAULT_SETTINGS), st$key)
  if (length(miss)) db_write("settings", rbind(st, data.frame(key = miss, value = unname(DEFAULT_SETTINGS[miss]), stringsAsFactors = FALSE)))
  if (nrow(db_read("clients")) == 0)
    db_write("clients", conform("clients", data.frame(name = "Jalisa Limited", address = "[Client address]", kra_pin = "[Client PIN]",
                                                       contact_person = "[Contact person]", stringsAsFactors = FALSE)))
  invisible(TRUE)
}

db_read <- function(tbl) {
  if (BACKEND == "google") {
    df <- googlesheets4::read_sheet(Sys.getenv("TECHTOBO_SHEET_ID"), sheet = tbl, col_types = "c")
  } else {
    df <- utils::read.csv(file.path(DATA_DIR, paste0(tbl, ".csv")), colClasses = "character",
                          na.strings = character(0), check.names = FALSE)
  }
  conform(tbl, df)
}

db_write <- function(tbl, df) {
  df <- conform(tbl, df)
  if (BACKEND == "google") {
    googlesheets4::sheet_write(df, ss = Sys.getenv("TECHTOBO_SHEET_ID"), sheet = tbl)
  } else {
    utils::write.csv(df, file.path(DATA_DIR, paste0(tbl, ".csv")), row.names = FALSE)
  }
  invisible(df)
}

# Re-read the table, apply a change, write it back. Returns the new table.
db_change <- function(tbl, fn) db_write(tbl, fn(db_read(tbl)))

row_df <- function(tbl, ...) conform(tbl, data.frame(..., stringsAsFactors = FALSE))
