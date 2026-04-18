# tools/auto_repair_surroNMA.R
# Run from the package root (where DESCRIPTION is)

message("=== surroNMA: iterative syntax repair ===")
stopifnot(file.exists("DESCRIPTION"))
stopifnot(dir.exists("R"))
stopifnot(file.exists("R/surroNMA.R"))

stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
bak   <- file.path("R", paste0("surroNMA.R.autobak-", stamp))
file.copy("R/surroNMA.R", bak, overwrite = TRUE)
message("Backup written: ", bak)

read_lines <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

# Normalize file: LF line endings, trim trailing space, final newline
normalize_file <- function() {
  txt <- readLines("R/surroNMA.R", warn = FALSE)
  txt <- sub("[ \t]+$", "", txt, perl = TRUE)
  write_lines(c(txt, ""))
}
normalize_file()

try_parse <- function() {
  out <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) {
    msg <- conditionMessage(attr(out, "condition"))
    list(ok = FALSE, msg = msg)
  } else {
    list(ok = TRUE, msg = "OK")
  }
}

count_braces <- function(s) {
  # crude count outside quotes
  s2 <- gsub('"[^"\\\\]*(\\\\.[^"\\\\]*)*"', '""', s, perl = TRUE)
  s2 <- gsub("'[^'\\\\]*(\\\\.[^'\\\\]*)*'", "''", s2, perl = TRUE)
  opens  <- gregexpr("\\{", s2, perl = TRUE); opens  <- if (opens[[1]][1] == -1) 0L else length(opens[[1]])
  closes <- gregexpr("\\}", s2, perl = TRUE); closes <- if (closes[[1]][1] == -1) 0L else length(closes[[1]])
  c(open = opens, close = closes)
}

remove_nearest_stray_brace <- function(lines, err_line) {
  n <- length(lines)
  window <- max(1, err_line - 10):min(n, err_line + 2)

  # preference 1: a line that's only "}" possibly with spaces/comments
  only_close <- function(x) grepl("^\\s*\\}\\s*(#.*)?$", x)
  idx <- which(only_close(lines[window]))
  if (length(idx)) {
    del <- window[min(idx)]
    lines[del] <- paste0("# AUTO-FIX removed stray '}' formerly here")
    attr(lines, "change") <- sprintf("Removed a stray '}' at line %d", del)
    return(lines)
  }

  # preference 2: a line ending with "}" where that line has more closes than opens
  ends_close <- function(x) grepl("\\}\\s*(#.*)?\\s*$", x)
  idx <- which(ends_close(lines[window]))
  if (length(idx)) {
    # remove ONE '}' at end of that line
    del <- window[min(idx)]
    bc <- count_braces(lines[del])
    if (bc["close"] > bc["open"]) {
      # remove last '}' only
      lines[del] <- sub("\\}(\\s*(#.*)?\\s*)$", "\\1 # AUTO-FIX: removed one '}'", lines[del])
      attr(lines, "change") <- sprintf("Removed one trailing '}' at line %d", del)
      return(lines)
    }
  }

  # fallback: scan upward for a plausible closing brace
  up <- seq(err_line, max(1, err_line - 50), by = -1L)
  for (del in up) {
    bc <- count_braces(lines[del])
    if (grepl("\\}", lines[del]) && bc["close"] > bc["open"]) {
      lines[del] <- sub("\\}", "", lines[del])
      lines[del] <- paste0(lines[del], " # AUTO-FIX: removed one '}'")
      attr(lines, "change") <- sprintf("Removed one '}' at line %d (fallback)", del)
      return(lines)
    }
  }

  attr(lines, "change") <- "No removable brace found near error line."
  lines
}

add_one_closing_brace <- function(lines) {
  lines <- c(lines, "}", "# AUTO-FIX: added one closing brace at EOF")
  attr(lines, "change") <- "Added one '}' at EOF."
  lines
}

repair_once <- function() {
  p <- try_parse()
  if (p$ok) return(list(done = TRUE, note = "Already parses."))
  msg <- p$msg

  # Detect specific patterns
  m_close <- regexec("unexpected '\\}' in \".*?\" at line ([0-9]+)", msg)
  rm      <- regmatches(msg, m_close)[[1]]

  if (length(rm) >= 2) {
    err_line <- as.integer(rm[2])
    lines <- read_lines()
    lines2 <- remove_nearest_stray_brace(lines, err_line)
    note <- attr(lines2, "change", exact = TRUE)
    write_lines(lines2)
    normalize_file()
    return(list(done = FALSE, note = paste("Brace removal:", note)))
  }

  if (grepl("unexpected end of input", msg, fixed = TRUE)) {
    lines <- read_lines()
    lines2 <- add_one_closing_brace(lines)
    note <- attr(lines2, "change", exact = TRUE)
    write_lines(lines2)
    normalize_file()
    return(list(done = FALSE, note = paste("Brace addition:", note)))
  }

  # Also handle unexpected ')'
  m_paren <- regexec("unexpected '\\)' in \".*?\" at line ([0-9]+)", msg)
  rp      <- regmatches(msg, m_paren)[[1]]
  if (length(rp) >= 2) {
    err_line <- as.integer(rp[2])
    lines <- read_lines()
    # remove a stray ')' on or just above err_line
    rng <- max(1, err_line - 2):err_line
    for (i in rev(rng)) {
      if (grepl("\\)", lines[i])) {
        lines[i] <- sub("\\)", "", lines[i])
        lines[i] <- paste0(lines[i], " # AUTO-FIX: removed one ')'")
        write_lines(lines)
        normalize_file()
        return(list(done = FALSE, note = sprintf("Removed one ')' at line %d", i)))
      }
    }
    return(list(done = FALSE, note = "Could not locate a removable ')' near error line."))
  }

  list(done = FALSE, note = paste("Unhandled parse error:", msg))
}

# Iterate repairs up to a safe limit
limit <- 40L
notes <- character()
for (i in seq_len(limit)) {
  step <- repair_once()
  notes <- c(notes, sprintf("[%02d] %s", i, step$note))
  message(notes[length(notes)])
  if (identical(step$done, TRUE)) break
}

# Final parse check
final <- try_parse()
if (!final$ok) {
  message("\n✗ Still not parseable after ", length(notes), " steps.")
  message("Last error:\n", final$msg)
  message("See backup: ", bak)
  quit(save = "no", status = 1)
}

message("\n✓ File parses after ", length(notes), " steps.")
if (length(notes)) {
  cat("Repair log:\n", paste0("  - ", notes, collapse = "\n"), "\n")
}

# Optional: regenerate docs/namespace
if (requireNamespace("devtools", quietly = TRUE) &&
    requireNamespace("roxygen2", quietly = TRUE)) {
  message("\nRegenerating NAMESPACE + Rd with roxygen2…")
  suppressPackageStartupMessages(devtools::document(roclets = c("rd", "namespace")))
} else {
  message("\n(roxygen2/devtools not both available) — skipping document().")
  # Fallback: create a simple NAMESPACE exporting non-dot functions from this file
  lines <- read_lines()
  fun_names <- sub("^\\s*([A-Za-z0-9_.]+)\\s*<-.*$", "\\1",
                   grep("^[[:space:]]*[A-Za-z0-9_.]+[[:space:]]*<-[[:space:]]*function\\s*\\(",
                        lines, value = TRUE))
  fun_names <- unique(fun_names)
  exports <- sort(fun_names[!grepl("^\\.", fun_names)])
  ns_txt <- c("exportPattern(\"^[^\\\\.]\")",
              sprintf("# explicit exports (%d)", length(exports)),
              sprintf("export(%s)", paste0("\"", exports, "\"", collapse = ", ")))
  writeLines(ns_txt, "NAMESPACE", useBytes = TRUE)
  message("Fallback NAMESPACE written with ", length(exports), " exports.")
}

# Smoke test load
if (requireNamespace("devtools", quietly = TRUE)) {
  message("\nLoading package with devtools::load_all()…")
  suppressMessages(devtools::load_all(quiet = TRUE))
  needed <- c("simulate_surro_data", "surro_network", "surro_nma")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(missing)) {
    message("⚠ Missing in namespace: ", paste(missing, collapse = ", "))
  } else {
    message("✓ Core functions present: ", paste(needed, collapse = ", "))
  }
}

message("\n=== Done. Now try: devtools::check() ===")

