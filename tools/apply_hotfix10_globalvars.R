# == surroNMA hotfix10: fix utils::globalVariables() call ==
message("== surroNMA hotfix10 ==")
rfile <- file.path(getwd(), "R", "surroNMA.R")
stopifnot(file.exists(rfile))
bak <- sprintf("%s.bak-%s", rfile, format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy(rfile, bak, overwrite = TRUE); message("Backup: ", bak)

x <- readLines(rfile, warn = FALSE)

ix <- grep("\\butils::globalVariables\\s*\\(", x)
if (length(ix)) {
  for (i in ix) {
    # If it's already utils::globalVariables(c(...)) leave it
    if (!grepl("globalVariables\\s*\\(\\s*c\\s*\\(", x[i])) {
      # Extract what's inside the parentheses
      inner <- sub(".*globalVariables\\s*\\((.*)\\)\\s*$", "\\1", x[i])
      # Normalize quotes and commas, then wrap with c(...)
      inner <- gsub("^\\s+|\\s+$", "", inner)
      x[i] <- sprintf("utils::globalVariables(c(%s))", inner)
    }
  }
  writeLines(x, rfile, useBytes = TRUE)
  message("Patched: ", rfile)
} else {
  message("No utils::globalVariables() call found; nothing to change.")
}

# Rebuild docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(getwd(), "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Done. Now run devtools::check() ==")
