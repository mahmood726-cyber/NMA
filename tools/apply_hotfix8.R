# == surroNMA hotfix8 ==
message("== surroNMA hotfix8 ==")
root  <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# Helper: fix lone '\(' or '\)' ONLY when they appear inside a quoted string on a line.
fix_line <- function(s) {
  # find quoted segments (single or double quotes, naive but works well here)
  m <- gregexpr("([\"'])(?:\\\\.|(?!\\1).)*\\1", s, perl = TRUE)[[1]]
  if (m[1] == -1) return(s)
  starts <- as.integer(m)
  lens   <- attr(m, "match.length")
  for (i in seq_along(starts)) {
    a <- starts[i]; b <- a + lens[i] - 1
    seg <- substring(s, a, b)
    # replace single backslash-paren inside the quoted segment
    seg <- gsub("\\\\(?=\\()", "(", seg, perl = TRUE)  #  '\(' -> '('
    seg <- gsub("\\\\(?=\\))", ")", seg, perl = TRUE)  #  '\)' -> ')'
    # (intentionally NOT touching '\\(' or '\\)' which are valid)
    s <- paste0(substr(s, 1, a - 1), seg, substr(s, b + 1, nchar(s)))
  }
  s
}

x <- vapply(x, fix_line, character(1L), USE.NAMES = FALSE)
writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# Re-gen docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}

message("== Done. Now run devtools::check() ==")
