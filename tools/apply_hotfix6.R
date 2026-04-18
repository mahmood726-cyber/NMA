# == surroNMA hotfix6 ==
message("== surroNMA hotfix6 ==")
root  <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

# backup
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

# --- 1) Normalize the “either S_eff/S_se or S_multi” guard into two clean lines ---
guard_pat <- '^\\s*if\\s*\\(.*(S_multi).*\\)\\s*$'
stop_pat  <- 'Provide either S_eff/S_se \\(univariate\\) or S_multi \\(multivariate surrogates\\)\\.'

for (i in seq_along(x)) {
  if (grepl(guard_pat, x[i], perl = TRUE)) {
    # Force the exact guard we expect
    x[i] <- '  if (missing(S_multi) && (missing(S_eff) || missing(S_se)))'
    # The next line should be the stop. If it contains extra junk, replace it.
    if (i + 1 <= length(x)) {
      if (!grepl('^\\s*stop\\(', x[i + 1])) {
        x[i + 1] <- paste0('    stop("', stop_pat, '")')
      } else {
        # Strip any trailing tokens after the closing parenthesis on the stop line
        x[i + 1] <- sub('^(\\s*stop\\([^)]*\\)).*$', '\\1', x[i + 1], perl = TRUE)
        # Ensure the message text is correct (optional, but makes diffs tidy)
        x[i + 1] <- sub('^\\s*stop\\([^"]*"[^"]*"\\)',
                        paste0('    stop("', stop_pat, '")'),
                        x[i + 1])
      }
    } else {
      # File ended right after the if; insert the stop
      x <- append(x, values = paste0('    stop("', stop_pat, '")'), after = i)
    }
  }
}

# --- 2) Global safety: if ANY stop(...) line has trailing junk, trim it ---
x <- sub('^(\\s*stop\\([^)]*\\)).*$', '\\1', x, perl = TRUE)

writeLines(x, rfile, useBytes = TRUE)
message("Patched: ", rfile)

# Re-generate docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
