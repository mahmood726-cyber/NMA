# tools/diagnose_and_patch.R
# Safe diagnostics (default: read-only). Run from package root.

DO_PATCH <- FALSE   # <<< set TRUE to apply a minimal, surgical fix automatically

stopifnot(file.exists("DESCRIPTION"))
stopifnot(dir.exists("R"))
stopifnot(file.exists("R/surroNMA.R"))

read_lines  <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

# Remove strings (both "..." and '...') and then comments, line by line.
strip_strings_comments <- function(x) {
  # Remove double-quoted strings: "(any non-quote or escaped char)*"
  x <- gsub('"(?:[^"\\\\]|\\\\.)*"', '""', x, perl = TRUE)
  # Remove single-quoted strings: '(any non-quote or escaped char)*'
  x <- gsub("'(?:[^'\\\\]|\\\\.)*'", "''", x, perl = TRUE)
  # Remove comments unless the line starts with shebang '#!'
  is_shebang <- grepl("^#!", x)
  x[!is_shebang] <- sub("#.*$", "", x[!is_shebang], perl = TRUE)
  x
}

try_parse <- function() {
  out <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) {
    list(ok = FALSE, msg = conditionMessage(attr(out, "condition")))
  } else list(ok = TRUE, msg = "OK")
}

context_print <- function(lines, at, span = 20) {
  n <- length(lines); a <- max(1, at - span); b <- min(n, at + span)
  cat(sprintf("\n--- Context (lines %d..%d) ---\n", a, b))
  for (i in a:b) cat(sprintf("%5d | %s\n", i, lines[i]))
  cat("--- End context ---\n\n")
}

scan_balance <- function(lines) {
  s <- strip_strings_comments(lines)
  count_chr <- function(vec, pat) {
    vapply(gregexpr(pat, vec, perl = TRUE), function(m)
      ifelse(m[1] == -1, 0L, length(m)), 0L)
  }
  data.frame(
    line = seq_along(s),
    open_curly  = count_chr(s, "\\{"),
    close_curly = count_chr(s, "\\}"),
    open_paren  = count_chr(s, "\\("),
    close_paren = count_chr(s, "\\)"),
    open_brack  = count_chr(s, "\\["),
    close_brack = count_chr(s, "\\]"),
    text = lines,
    stringsAsFactors = FALSE
  )
}

suggest_fix <- function(bal) {
  bal$curly_cum <- cumsum(bal$open_curly - bal$close_curly)
  bal$paren_cum <- cumsum(bal$open_paren - bal$close_paren)
  bal$brack_cum <- cumsum(bal$open_brack - bal$close_brack)
  list(
    first_neg_curly = which(bal$curly_cum < 0)[1],
    first_neg_paren = which(bal$paren_cum < 0)[1],
    first_neg_brack = which(bal$brack_cum < 0)[1],
    end_curly = tail(bal$curly_cum, 1),
    end_paren = tail(bal$paren_cum, 1),
    end_brack = tail(bal$brack_cum, 1)
  )
}

remove_stray_closer_at <- function(lines, L) {
  if (L <= 0 || L > length(lines)) return(lines)
  # Prefer removing a line that's just "}"
  if (grepl("^\\s*\\}\\s*(#.*)?$", lines[L])) {
    lines[L] <- "# AUTO-FIX removed stray '}' that was here"
    return(lines)
  }
  # Otherwise remove one trailing '}' at end of line
  if (grepl("\\}\\s*(#.*)?\\s*$", lines[L])) {
    lines[L] <- sub("\\}(\\s*(#.*)?\\s*)$", "\\1 # AUTO-FIX removed one '}'", lines[L])
    return(lines)
  }
  # Fallback: remove the first '}' occurrence
  if (grepl("\\}", lines[L])) {
    lines[L] <- sub("\\}", " # AUTO-FIX removed one '}' ", lines[L])
  }
  lines
}

add_closers_eof <- function(lines, n) {
  n <- as.integer(n)
  if (is.na(n) || n <= 0) return(lines)
  c(lines, rep("}", n), sprintf("# AUTO-FIX: added %d closing '}' at EOF", n))
}

# --- MAIN ---
lines <- read_lines()
p <- try_parse()
cat("Parse status:", if (p$ok) "OK\n" else paste("ERROR\n", p$msg, "\n"))

err_line <- NA_integer_
if (!p$ok) {
  m <- regexec(" at line ([0-9]+)", p$msg)
  r <- regmatches(p$msg, m)[[1]]
  if (length(r) >= 2) err_line <- as.integer(r[2])
}

if (!is.na(err_line)) context_print(lines, err_line, span = 20)

bal <- scan_balance(lines)
sx  <- suggest_fix(bal)

cat("Balance summary:\n")
cat(sprintf("  Curly  cumulative end: %d\n", sx$end_curly))
cat(sprintf("  Paren  cumulative end: %d\n", sx$end_paren))
cat(sprintf("  Brack  cumulative end: %d\n", sx$end_brack))
if (!is.na(sx$first_neg_curly)) cat(sprintf("  First negative curly balance at line: %d\n", sx$first_neg_curly))
if (!is.na(sx$first_neg_paren)) cat(sprintf("  First negative paren balance at line: %d\n", sx$first_neg_paren))
if (!is.na(sx$first_neg_brack)) cat(sprintf("  First negative brack balance at line: %d\n", sx$first_neg_brack))

if (!p$ok) {
  if (!is.na(sx$first_neg_curly)) {
    cat(sprintf("\nSuggestion: remove one '}' on or above line %d.\n", sx$first_neg_curly))
  } else if (grepl("unexpected end of input", p$msg, fixed = TRUE) && sx$end_curly > 0) {
    cat(sprintf("\nSuggestion: add %d closing '}' at EOF.\n", sx$end_curly))
  } else if (!is.na(sx$first_neg_paren)) {
    cat(sprintf("\nSuggestion: remove one ')' on or above line %d.\n", sx$first_neg_paren))
  } else if (!is.na(sx$first_neg_brack)) {
    cat(sprintf("\nSuggestion: remove one ']' on or above line %d.\n", sx$first_neg_brack))
  }
}

# Optional surgical patch (applies ONE change) – only if DO_PATCH is TRUE
if (DO_PATCH) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  bak   <- file.path("R", paste0("surroNMA.R.diagbak-", stamp))
  file.copy("R/surroNMA.R", bak, overwrite = TRUE)
  cat("Backup written:", bak, "\n")

  if (!is.na(sx$first_neg_curly)) {
    L <- sx$first_neg_curly
    lines2 <- remove_stray_closer_at(lines, L)
    write_lines(lines2)
    cat("Applied: removed one stray '}' near line", L, "\n")
  } else if (grepl("unexpected end of input", p$msg, fixed = TRUE) && sx$end_curly > 0) {
    lines2 <- add_closers_eof(lines, sx$end_curly)
    write_lines(lines2)
    cat("Applied: added", sx$end_curly, "closing '}' at EOF\n")
  } else if (!is.na(sx$first_neg_paren)) {
    L <- sx$first_neg_paren
    # remove a nearby ')' (conservatively, only if the line ends with ')')
    if (grepl("\\)\\s*(#.*)?\\s*$", lines[L])) {
      old <- lines[L]
      lines[L] <- sub("\\)\\s*(#.*)?\\s*$", " # AUTO-FIX removed one ')' \\1", lines[L])
      write_lines(lines)
      cat("Applied: removed one ')' near line", L, "\n")
    } else {
      cat("No safe single-line ')' removal found; no changes made.\n")
    }
  } else {
    cat("No safe automatic patch rule matched. No changes made.\n")
  }

  # Re-test
  p2 <- try_parse()
  cat("\nAfter patch, parse status:", if (p2$ok) "OK\n" else paste("ERROR\n", p2$msg, "\n"))
}

cat("\nDone. (No edits unless DO_PATCH=TRUE.)\n")
