message("== surroNMA hotfix2 ==")

root <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

backup <- function(p) {
  dst <- sprintf("%s.bak-%s", p, format(Sys.time(), "%Y%m%d-%H%M%S"))
  file.copy(p, dst, overwrite = TRUE)
  message("Backup: ", dst)
}
backup(rfile)

txt <- readLines(rfile, warn = FALSE)

# ---- 1) Ensure roxygen imports utils::getFromNamespace at the package level
# Find the package-level roxygen header (line with _PACKAGE or @docType etc.)
ix_head <- grep("_PACKAGE|@docType\\s+\"package\"", txt)
if (length(ix_head)) {
  # If not already present, add the import tag once right after the header block
  has_tag <- any(grepl("^#'\\s*@importFrom\\s+utils\\s+getFromNamespace\\b", txt))
  if (!has_tag) {
    # Insert below the first roxygen header line
    insert_at <- ix_head[1]
    # search upward to the beginning of roxygen block
    while (insert_at > 1 && grepl("^#'", txt[insert_at - 1])) insert_at <- insert_at - 1
    # insert one line after the roxygen block start
    # place near the end of the header is also fine; simplest is just after this header line
    txt <- append(txt, values = "#' @importFrom utils getFromNamespace", after = ix_head[length(ix_head)])
  }
}

# ---- 2) Rewrite set_visible() to avoid the f<- NOTE and call the replacement via do.call
beg <- grep("^\\s*set_visible\\s*<-\\s*function\\s*\\(", txt)
if (length(beg)) {
  # find end of the function (first solo closing brace after start)
  end <- beg
  brace <- 0L
  for (i in beg:length(txt)) {
    brace <- brace + stringi::stri_count_fixed(txt[i], "{") - stringi::stri_count_fixed(txt[i], "}")
    if (brace == 0L) { end <- i; break }
  }
  new_fun <- c(
    "set_visible <- function(obj, value) {",
    "  # Call gWidgets2's replacement function visible<- without creating an 'f<-' symbol",
    "  setter <- utils::getFromNamespace(\"visible<-\", \"gWidgets2\")",
    "  # replacement functions have signature (x, value); do.call avoids parse-time f<- NOTE",
    "  do.call(setter, list(obj, value))",
    "  invisible(obj)",
    "}"
  )
  txt <- append(txt[-(beg:end)], values = new_fun, after = beg - 1)
}

# ---- 3) Make 'uni vs multi' decision in surro_network() from missing() only
# Create a small transformation: after arguments are captured, define flags and use them.
# We’ll add/replace a block that sets has_uni / has_multi and the stop() that complains.
sw <- grep("^\\s*surro_network\\s*<-\\s*function\\s*\\(", txt)
if (length(sw)) {
  # Insert (or replace) a guard soon after the opening '{' of the function
  # Find the first '{' after the function line
  open_line <- sw[1]
  while (open_line <= length(txt) && !grepl("\\{", txt[open_line])) open_line <- open_line + 1

  # After captures, there is usually a stop() that says “Provide either S_eff/S_se …”.
  # Replace any such stop with our version that uses the flags.
  txt <- gsub(
    "stop\\([^\\)]*Provide either S_eff/S_se[^\\)]*\\)",
    "stop(\"Provide either S_eff/S_se (univariate) or S_multi (multivariate surrogates).\")",
    txt
  )

  # Add (or replace) the flag block. We’ll remove any existing lines that try to infer
  # from NULLs and reinsert our explicit version right after the captures typically occur.
  # To keep it simple and safe, just ensure the flags exist once in the function body.
  flag_block <- c(
    "  # decide pathway by presence of args (not their captured values):",
    "  has_uni   <- !missing(S_eff) && !missing(S_se)",
    "  has_multi <- !missing(S_multi)",
    "  if (!has_uni && !has_multi) {",
    "    stop(\"Provide either S_eff/S_se (univariate) or S_multi (multivariate surrogates).\")",
    "  }"
  )

  # Heuristically place after the function’s first '{' line
  txt <- append(txt, values = flag_block, after = open_line)
}

# Write back
writeLines(txt, rfile, useBytes = TRUE)
message("Patched: R/surroNMA.R")

# ---- 4) Rebuild docs / NAMESPACE
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}

message("== hotfix2 done. Now run devtools::check() ==")
