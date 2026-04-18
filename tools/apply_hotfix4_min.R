# == surroNMA hotfix4 (minimal) ==
message("== surroNMA hotfix4 (minimal) ==")
root <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

# backup
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# 1) Remove any trailing " or" (with optional semicolon) after a stop(...) line
#    e.g. stop("..."); or   or   stop("...") or
x <- gsub('\\)\\s*;?\\s*or(\\s*#.*)?\\s*$',
          ')\\1',
          x, perl = TRUE)

# 2) Fix the guard near the top of surro_network():
#    We want: if (missing(S_multi) && (missing(S_eff) || missing(S_se))) stop(...)
pat_old1 <- 'if\\s*\\(\\s*is\\.null\\(Se\\)\\s*&&\\s*is\\.null\\(S_multi\\)\\s*\\)'
pat_old2 <- 'if\\s*\\(\\s*missing\\(S_eff\\)\\s*&&\\s*missing\\(S_multi\\)\\s*\\)'
repl_guard <- 'if (missing(S_multi) && (missing(S_eff) || missing(S_se)))'

x <- gsub(pat_old1, repl_guard, x, perl = TRUE)
x <- gsub(pat_old2, repl_guard, x, perl = TRUE)

# (Optional) ensure we can import getFromNamespace for set_visible() note
# only add the roxygen tag once if there’s a package header
has_pkg_header <- any(grepl("^#'.*_PACKAGE", x)) || any(grepl('^#\'\\s*@docType\\s+"package"', x))
needs_utils <- !any(grepl("^#'\\s*@importFrom\\s+utils\\s+getFromNamespace\\b", x))
if (has_pkg_header && needs_utils) {
  hdr_end <- max(grep("^#'", x))
  x <- append(x, "#' @importFrom utils getFromNamespace", after = hdr_end)
}

writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# Regenerate docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
