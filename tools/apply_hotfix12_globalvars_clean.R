# == surroNMA hotfix12: clean stray commas + reset globalVariables ==
message("== surroNMA hotfix12 ==")
rfile <- file.path(getwd(), "R", "surroNMA.R")
stopifnot(file.exists(rfile))
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

# Read whole file text (for multi-line regex) and also as lines
txt   <- readChar(rfile, file.info(rfile)$size, useBytes = TRUE)
lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

# 0) Remove any top-of-file lines that are only commas (causes "unexpected ','")
#    Limit to the first ~50 lines to be conservative.
top_n <- min(50L, length(lines))
keep  <- !grepl("^\\s*,\\s*$", lines[seq_len(top_n)])
lines[seq_len(top_n)] <- lines[seq_len(top_n)][keep]

# Rebuild txt after comma cleanup
txt <- paste(lines, collapse = "\n")

# 1) Remove ANY existing utils::globalVariables(...) block (handles multiline)
txt <- gsub("utils::globalVariables\\s*\\((?s).*?\\)\\s*;?", "", txt, perl = TRUE)

# 2) Prepare canonical globalVariables call
canon <- paste0(
  "utils::globalVariables(c(",
  paste(shQuote(c(
    "S_eff","S_se","S_multi","S_multi_se",
    "T_eff","T_se","corr_ST","class",
    "baseline_risk","rob"
  )), collapse = ", "),
  "))"
)

# 3) Insert canonical call once, right after roxygen header (lines starting with #')
lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
non_rog <- which(!grepl("^#'", lines))
insert_after <- if (length(non_rog)) non_rog[1] - 1 else length(lines)

# Avoid duplicates if already present
if (!any(grepl("\\butils::globalVariables\\s*\\(", lines))) {
  # ensure a blank line before/after for readability
  ins <- c("", canon, "")
  lines <- append(lines, ins, after = insert_after)
}

# 4) Write back
writeLines(lines, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# 5) Rebuild docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(getwd(), "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
