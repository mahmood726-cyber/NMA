# == surroNMA hotfix11: nuke and replace utils::globalVariables() ==
message("== surroNMA hotfix11 ==")
rfile <- file.path(getwd(), "R", "surroNMA.R")
stopifnot(file.exists(rfile))
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

# Read whole file as a single string so we can match across lines
txt <- readChar(rfile, file.info(rfile)$size, useBytes = TRUE)

# 1) Remove ANY existing utils::globalVariables(...) call (handles multiline)
txt <- gsub("utils::globalVariables\\s*\\((?s).*?\\)\\s*;?",
            "", txt, perl = TRUE)

# 2) Build a single canonical call
canon <- paste0(
  "utils::globalVariables(c(",
  paste(shQuote(c(
    "S_eff","S_se","S_multi","S_multi_se",
    "T_eff","T_se","corr_ST","class",
    "baseline_risk","rob"
  )), collapse = ", "),
  "))"
)

# 3) Insert the canonical call once, right after any roxygen header
lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

# Find first non-roxygen line; if all roxygen, insert after header
non_rog <- which(!grepl("^#'", lines))
ins_at <- if (length(non_rog)) non_rog[1] else length(lines) + 1

# Avoid duplicate if it already exists (from earlier runs)
if (!any(grepl("\\butils::globalVariables\\s*\\(", lines))) {
  lines <- append(lines, values = canon, after = ins_at - 1)
}

writeLines(lines, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# Rebuild docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(getwd(), "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
