# tools/repair_safe.R
# Safe, RStudio-friendly iterative syntax repair for R/surroNMA.R

message("=== surroNMA: SAFE syntax repair ===")
if (!file.exists("DESCRIPTION")) stop("Run from package root (where DESCRIPTION lives).")
if (!dir.exists("R")) stop("No R/ directory found.")
if (!file.exists("R/surroNMA.R")) stop("R/surroNMA.R not found.")

# --- configuration (conservative) ---
max_steps <- 10L               # small, bounded
window_up <- 10L               # scan 10 lines above error line
window_dn <- 2L                # scan 2 lines below error line
log_path  <- "tools/repair_log.txt"

# --- helpers ---
read_lines  <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

append_log <- function(...) {
  dir.create("tools", showWarnings = FALSE, recursive = TRUE)
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", file = log_path, append = TRUE)
}

normalize_file <- function() {
  x <- readLines("R/surroNMA.R", warn = FALSE)
  # trim trailing spaces only (avoid large mutations)
  x <- sub("[ \t]+$", "", x, perl = TRUE)
  # ensure single trailing newline
  if (length(x) == 0L || nzchar(x[length(x)])) x <- c(x, "")
  write_lines(x)
}

try_parse <- function() {
  out <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) {
    list(ok = FALSE, msg = conditionMessage(attr(out, "condition")))
  } else list(ok = TRUE, msg = "OK")
}

count_braces <- function(s) {
  # crude count outside quotes to guide heuristics
  s2 <- gsub('"[^"\\\\]*(\\\\.[^"\\\\]*)*"', '""', s, perl = TRUE)
  s2 <- gsub("'[^'\\\\]*(\\\\.[^'\\\\]*)*'", "''", s2, perl = TRUE)
  opens  <- gregexpr("\\{", s2, perl = TRUE); opens  <- if (opens[[1]][1] == -1) 0L else length(opens[[1]])
  closes <- gregexpr("\\}", s2, perl = TRUE); closes <- if (closes[[1]][1] == -1) 0L else length(closes[[1]])
  c(open = opens, close = closes)
}

remove_nearby_closing_brace <- function(lines, err_line) {
  n <- length(lines)
  rng <- max(1, err_line - window_up):min(n, err_line + window_dn)

  # 1) Prefer standalone '}'
  only_close <- function(x) grepl("^\\s*\\}\\s*(#.*)?$", x)
  idx <- which(only_close(lines[rng]))
  if (length(idx)) {
    del <- rng[min(idx)]
    old <- lines[del]
    lines[del] <- paste0("# AUTO-FIX removed stray '}' that was here")
    attr(lines, "note") <- sprintf("Removed standalone '}' at line %d: %s", del, old)
    return(lines)
  }

  # 2) Next, remove one trailing '}' where line has more closes than opens
  ends_close <- function(x) grepl("\\}\\s*(#.*)?\\s*$", x)
  idx <- which(ends_close(lines[rng]))
  if (length(idx)) {
    del <- rng[min(idx)]
    bc <- count_braces(lines[del])
    if (bc["close"] > bc["open"]) {
      old <- lines[del]
      lines[del] <- sub("\\}(\\s*(#.*)?\\s*)$", "\\1 # AUTO-FIX removed one '}'", lines[del])
      attr(lines, "note") <- sprintf("Removed trailing '}' at line %d: %s", del, old)
      return(lines)
    }
  }

  # 3) Fallback: scan upwards for a line with surplus closes
  for (del in seq(err_line, max(1, err_line - 50L), by = -1L)) {
    if (grepl("\\}", lines[del])) {
      bc <- count_braces(lines[del])
      if (bc["close"] > bc["open"]) {
        old <- lines[del]
        # remove first rightmost '}' only
        rightmost <- regexpr("\\}([^}]*)$", lines[del], perl = TRUE)
        if (rightmost > 0) {
          lines[del] <- paste0(
            substr(lines[del], 1, rightmost - 1),
            sub("^\\}", "", substr(lines[del], rightmost, nchar(lines[del]))),
            " # AUTO-FIX removed one '}'"
          )
          attr(lines, "note") <- sprintf("Removed one '}' at line %d (fallback): %s", del, old)
          return(lines)
        }
      }
    }
  }

  attr(lines, "note") <- "No removable '}' found near error line."
  lines
}

add_one_closing_brace_eof <- function(lines) {
  lines <- c(lines, "}", "# AUTO-FIX added one '}' at EOF")
  attr(lines, "note") <- "Added one '}' at EOF."
  lines
}

# --- backup current file ---
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
bak   <- file.path("R", paste0("surroNMA.R.bak-", stamp))
file.copy("R/surroNMA.R", bak, overwrite = TRUE)
append_log("Backup:", bak)
message("Backup written: ", bak)

# --- normalize once ---
normalize_file()

# --- iterative repair (bounded) ---
notes <- character()
for (i in seq_len(max_steps)) {
  p <- try_parse()
  if (p$ok) { notes <- c(notes, sprintf("[%02d] OK (no change)", i)); break }

  msg <- p$msg
  append_log(sprintf("Step %02d parse error: %s", i, msg))

  # unexpected '}' at line N
  m_close <- regexec("unexpected '\\}' in \".*?\" at line ([0-9]+)", msg)
  rm <- regmatches(msg, m_close)[[1]]
  if (length(rm) >= 2) {
    err_line <- as.integer(rm[2])
    lines <- read_lines()
    lines2 <- remove_nearby_closing_brace(lines, err_line)
    note <- attr(lines2, "note", exact = TRUE)
    write_lines(lines2)
    normalize_file()
    notes <- c(notes, sprintf("[%02d] %s", i, note))
    next
  }

  # unexpected end of input -> add one }
  if (grepl("unexpected end of input", msg, fixed = TRUE)) {
    lines <- read_lines()
    lines2 <- add_one_closing_brace_eof(lines)
    note <- attr(lines2, "note", exact = TRUE)
    write_lines(lines2)
    normalize_file()
    notes <- c(notes, sprintf("[%02d] %s", i, note))
    next
  }

  # unexpected ')' at line N -> remove one ')' nearby (very conservative)
  m_paren <- regexec("unexpected '\\)' in \".*?\" at line ([0-9]+)", msg)
  rp <- regmatches(msg, m_paren)[[1]]
  if (length(rp) >= 2) {
    err_line <- as.integer(rp[2])
    lines <- read_lines()
    rng <- max(1, err_line - 2L):err_line
    changed <- FALSE
    for (j in rev(rng)) {
      if (grepl("\\)", lines[j])) {
        old <- lines[j]
        lines[j] <- sub("\\)", "", lines[j])
        lines[j] <- paste0(lines[j], " # AUTO-FIX removed one ')'")
        write_lines(lines)
        normalize_file()
        notes <- c(notes, sprintf("[%02d] Removed one ')' at line %d: %s", i, j, old))
        changed <- TRUE
        break
      }
    }
    if (changed) next
  }

  # Unhandled error: leave file as-is and stop safely
  notes <- c(notes, sprintf("[%02d] Unhandled parse error; stopped to avoid disruptive edits.", i))
  break
}

# Final parse check & summary
final <- try_parse()
if (final$ok) {
  message("\n✓ R/surroNMA.R parses successfully.")
  append_log("FINAL: parses successfully.")
} else {
  message("\nℹ Still does not parse. See:", log_path)
  append_log("FINAL: still fails to parse: ", final$msg)
}

if (length(notes)) {
  cat("\nRepair notes:\n", paste0("  - ", notes, collapse = "\n"), "\n", sep = "")
  append_log("Notes:\n", paste0("  - ", notes, collapse = "\n"))
}

message("\nSafe repair complete. Review changes, then run devtools::load_all()/check() manually.")
