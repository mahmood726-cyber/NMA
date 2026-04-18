# == surroNMA hotfix5 ==
message("== surroNMA hotfix5 ==")
root  <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

# backup
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# 1) Strip any trailing "or" that follows a stop(...) on the SAME line
#    Handles: stop("...") or
#             stop("..."); or
pat_end <- '(stop\\s*\\((?:[^()"\\\\]|\\\\.|"[^"\\\\]*(?:\\\\.[^"\\\\]*)*")*\\))\\s*;?\\s*or\\s*$'
x <- sub(pat_end, '\\1', x, perl = TRUE)

# 2) Also strip inline "... ) or ..." just in case it's followed by code/comments
pat_inline <- '(stop\\s*\\((?:[^()"\\\\]|\\\\.|"[^"\\\\]*(?:\\\\.[^"\\\\]*)*")*\\))\\s*;?\\s*or\\b'
x <- gsub(pat_inline, '\\1', x, perl = TRUE)

# 3) Guard: ensure the surrogate-args check is correct
guard_good <- 'if (missing(S_multi) && (missing(S_eff) || missing(S_se)))'
x <- gsub('if\\s*\\(\\s*missing\\(S_multi\\)\\s*&&\\s*\\(\\s*missing\\(S_eff\\)\\s*\\|\\|\\s*missing\\(S_se\\)\\s*\\)\\s*\\)',
          guard_good, x, perl = TRUE)
x <- gsub('if\\s*\\(\\s*is\\.null\\(Se\\)\\s*&&\\s*is\\.null\\(S_multi\\)\\s*\\)', guard_good, x, perl = TRUE)
x <- gsub('if\\s*\\(\\s*missing\\(S_eff\\)\\s*&&\\s*missing\\(S_multi\\)\\s*\\)', guard_good, x, perl = TRUE)

writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# re-gen docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
