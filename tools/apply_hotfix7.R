# == surroNMA hotfix7 ==
message("== surroNMA hotfix7 ==")
root  <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

# backup
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# 1) Fix the specific broken 'stop("study' line by normalizing its message.
x <- sub(
  '^\\s*stop\\("study.*$',
  '    stop("Required columns are missing (study/trt/comp): A, B or study_id is NULL.")',
  x, perl = TRUE
)

# 2) Trim any junk after a properly closed stop(...) on the SAME line.
x <- sub('^(\\s*stop\\([^)]*\\)).*$', '\\1', x, perl = TRUE)

# 3) For any stop("... line that never closes on this line, hard-close it.
needs_close <- grepl('^\\s*stop\\("[^"]*$', x)
x[needs_close] <- paste0(x[needs_close], '")')

# 4) Make sure the “either S_eff/S_se or S_multi” guard (if present) is cleanly formed.
guard_msg <- 'Provide either S_eff/S_se (univariate) or S_multi (multivariate surrogates).'
guard_if_pat <- '^\\s*if\\s*\\(.*S_multi.*\\)\\s*$'
guard_stop_pat <- '^\\s*stop\\("Provide either S_eff/S_se \\(univariate\\) or S_multi \\(multivariate surrogates\\)\\."\\)\\s*$'

for (i in seq_along(x)) {
  if (grepl(guard_if_pat, x[i], perl = TRUE)) {
    # normalize the if-line
    x[i] <- '  if (missing(S_multi) && (missing(S_eff) || missing(S_se)))'
    # normalize the stop-line directly after
    if (i + 1 <= length(x)) {
      x[i + 1] <- paste0('    stop("', guard_msg, '")')
    } else {
      x <- append(x, values = paste0('    stop("', guard_msg, '")'), after = i)
    }
    break
  }
}

writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# 5) Re-generate docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}

# 6) If anything still looks suspicious, show it so you can paste here.
sus <- grep('^\\s*stop\\([^)]*\\).+\\S$', readLines(rfile, warn = FALSE), perl = TRUE)
if (length(sus)) {
  message("Lines with trailing junk after stop(...): ", paste(sus, collapse = ", "))
} else {
  message("No stop(...) lines with trailing junk.")
}

message("== Done. Now run devtools::check() ==")
