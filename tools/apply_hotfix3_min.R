# == surroNMA hotfix3 (minimal) ==
message("== surroNMA hotfix3 (minimal) ==")
root <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

# backup
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# 1) Remove accidental trailing " or" after a stop(...) line
x <- sub('\\)\\s*or\\s*$', ')', x, perl = TRUE)

# 2) Fix the guard: detect pathway by missing(args), and fix Se -> S_se typo
# Replace any 'if (is.null(Se) && is.null(S_multi))' with missing()-based check
x <- gsub('if\\s*\\(\\s*is\\.null\\(Se\\)\\s*&&\\s*is\\.null\\(S_multi\\)\\s*\\)',
          'if (missing(S_eff) && missing(S_multi))',
          x, perl = TRUE)

# If the package header lacks the import we need for set_visible(), add it once
has_pkg_header <- any(grepl("^#'.*_PACKAGE", x)) || any(grepl('^#\'\\s*@docType\\s+"package"', x))
has_import_tag <- any(grepl("^#'\\s*@importFrom\\s+utils\\s+getFromNamespace\\b", x))
if (has_pkg_header && !has_import_tag) {
  hdr_end <- max(grep("^#'", x))  # coarse but safe: last contiguous roxygen line near top
  # insert right after header block
  x <- append(x, "#' @importFrom utils getFromNamespace", after = hdr_end)
}

writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# Rebuild docs
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
