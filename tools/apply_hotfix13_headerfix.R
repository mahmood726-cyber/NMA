# == surroNMA hotfix13: header cleanup + globalVariables ==
message("== surroNMA hotfix13 ==")
rfile <- file.path(getwd(), "R", "surroNMA.R")
stopifnot(file.exists(rfile))
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

lines <- readLines(rfile, warn = FALSE)

# 1) Remove any lone ')' lines in the first 60 lines (causes "unexpected ')'")
cap <- min(60L, length(lines))
mask_top <- !grepl("^\\s*\\)\\s*$", lines[seq_len(cap)])
lines[seq_len(cap)] <- lines[seq_len(cap)][mask_top]

# 2) Ensure we have the canonical package doc block: "_PACKAGE" then NULL
#    (Keep your existing roxygen comments as-is.)
has_pkg_line  <- any(grepl('^"_PACKAGE"\\s*$', lines))
has_null_line <- any(grepl("^NULL\\s*$", lines))

if (!has_pkg_line) {
  # Insert "_PACKAGE" after the last roxygen line block at top
  first_non_rog <- which(!grepl("^#'", lines))[1]; if (is.na(first_non_rog)) first_non_rog <- 0
  insert_after <- max(0, first_non_rog - 1L)
  lines <- append(lines, '"_PACKAGE"', after = insert_after)
}
if (!has_null_line) {
  # Put NULL immediately after the "_PACKAGE" line
  pkg_idx <- which(grepl('^"_PACKAGE"\\s*$', lines))[1]
  lines <- append(lines, "NULL", after = pkg_idx)
}

# 3) Remove any existing utils::globalVariables(...) then add a clean one after NULL
lines <- lines[!grepl("^\\s*utils::globalVariables\\s*\\(", lines)]

gv_call <- paste0(
  'utils::globalVariables(c(',
  paste(shQuote(c(
    "S_eff","S_se","S_multi","S_multi_se",
    "T_eff","T_se","corr_ST","class",
    "baseline_risk","rob"
  )), collapse = ", "),
  '))'
)

# Insert after NULL (first occurrence)
null_idx <- which(grepl("^NULL\\s*$", lines))[1]
if (!any(grepl("\\butils::globalVariables\\s*\\(", lines))) {
  lines <- append(lines, c("", gv_call, ""), after = null_idx)
}

# 4) Write back
writeLines(lines, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# 5) Regenerate NAMESPACE/docs
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(getwd(), "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
