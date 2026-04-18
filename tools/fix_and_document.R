# tools/fix_and_document.R
# Run from package root (where DESCRIPTION lives)

message("=== surroNMA: package fixer for CRAN prep ===")

stopifnot(file.exists("DESCRIPTION"))

# 1) Ensure R/ exists
if (!dir.exists("R")) dir.create("R")

# 2) Pick best source file to restore R/surroNMA.R
pick_source <- function() {
  candidates <- c(
    "R/surroNMA.R",
    "surroNMA.R",
    "surroNMA.R.backup",
    "surroNMA_clean.R"
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("No source file found. Expected one of: ", paste(candidates, collapse=", "))
  }
  # Prefer the one already in R/, otherwise largest by size
  if ("R/surroNMA.R" %in% existing) return("R/surroNMA.R")
  sizes <- file.info(existing)$size
  existing[order(-sizes)][1]
}

src <- pick_source()
dst <- "R/surroNMA.R"
if (normalizePath(src, mustWork = TRUE) != normalizePath(dst, mustWork = FALSE)) {
  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!ok) stop("Failed to copy ", src, " -> ", dst)
  message("Placed source at ", dst, " (from ", src, ")")
} else {
  message(dst, " already present.")
}

# 3) Normalize file (encoding, EOLs, trailing spaces, final newline)
normalize_r_file <- function(path) {
  x <- readBin(path, what = "raw", n = file.info(path)$size)
  # convert CRLF/CR to LF
  raw_to_text <- function(r) rawToChar(r, multiple = FALSE)
  txt <- raw_to_text(x)
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  # trim trailing spaces, preserve indentation
  lines <- sub("[ \t]+$", "", lines, perl = TRUE)
  # ensure UTF-8
  lines <- enc2utf8(lines)
  # ensure single trailing newline
  txt2 <- paste0(paste(lines, collapse = "\n"), "\n")
  writeLines(txt2, path, useBytes = TRUE)
}

normalize_r_file(dst)
message("Normalized ", dst)

# 4) Auto-fix unbalanced braces in functions (only if detected)
fix_braces_in_functions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  L <- length(lines)

  # Locate top-level function starts: name <- function(
    fun_starts <- grep("^[[:space:]]*[A-Za-z0-9_.]+[[:space:]]*<-[[:space:]]*function\\s*\\(", lines, perl = TRUE)
    if (length(fun_starts) == 0) {
      message("No function definitions detected; skipping brace fix.")
      return(invisible(FALSE))
    }

    # Helper to count { and } on a line (ignoring those inside quoted strings)
    count_braces <- function(s) {
      # Quick-and-dirty: strip quoted substrings
      s <- gsub('"[^"\\\\]*(\\\\.[^"\\\\]*)*"', '""', s, perl = TRUE)
      s <- gsub("'[^'\\\\]*(\\\\.[^'\\\\]*)*'", "''", s, perl = TRUE)
      opens  <- gregexpr("\\{", s, perl = TRUE); opens  <- if (opens[[1]][1] == -1) 0 else length(opens[[1]])
      closes <- gregexpr("\\}", s, perl = TRUE); closes <- if (closes[[1]][1] == -1) 0 else length(closes[[1]])
      c(opens = opens, closes = closes)
    }

    insertions <- list()
    starts <- c(fun_starts, L + 1)  # sentinel to handle last function
    for (i in seq_along(fun_starts)) {
      s <- starts[i]
      e <- starts[i + 1] - 1
      seg <- lines[s:e]
      balance <- 0L
      for (ln in seg) {
        bc <- count_braces(ln)
        balance <- balance + bc["opens"] - bc["closes"]
      }
      if (balance > 0L) {
        # Insert 'balance' number of '}' before next function start (i.e., at line e)
        ins <- paste(rep("}", balance), collapse = "\n")
        insertions[[length(insertions) + 1]] <- list(pos = e, text = ins)
        message(sprintf("Unbalanced braces detected in function starting at line %d; adding %d closing brace(s).", s, balance))
      }
    }

    if (length(insertions) > 0) {
      # Apply from bottom to top to keep indices correct
      for (ins in rev(insertions)) {
        lines <- append(lines, values = ins$text, after = ins$pos)
      }
      writeLines(lines, path, useBytes = TRUE)
      return(invisible(TRUE))
    }
    invisible(FALSE)
}

did_fix <- fix_braces_in_functions(dst)
if (did_fix) {
  message("Applied brace fixes; re-normalizing file.")
  normalize_r_file(dst)
}

# 5) Parse check
p_ok <- tryCatch({ parse(dst); TRUE }, error = function(e) { message("Parse error: ", e$message); FALSE })
if (!p_ok) stop("R/surroNMA.R still does not parse. Inspect around the last reported error position.")

# 6) NAMESPACE generation
# Prefer roxygen2; otherwise fall back to a generated NAMESPACE exporting all top-level functions.
make_namespace_fallback <- function(r_file, namespace_path = "NAMESPACE") {
  lines <- readLines(r_file, warn = FALSE)
  fun_names <- sub("^\\s*([A-Za-z0-9_.]+)\\s*<-.*$", "\\1",
                   grep("^[[:space:]]*[A-Za-z0-9_.]+[[:space:]]*<-[[:space:]]*function\\s*\\(", lines, value = TRUE))
  fun_names <- unique(fun_names)
  if (length(fun_names) == 0) stop("No functions found to export for fallback NAMESPACE.")
  # Keep common non-API internals private if desired (prefix with .)
  exports <- fun_names[!grepl("^\\.", fun_names)]
  ns_txt <- c("exportPattern(\"^[^\\\\.]\")", sprintf("# explicit exports (%d)", length(exports)),
              sprintf("export(%s)", paste0("\"", exports, "\"", collapse = ", ")))
  writeLines(ns_txt, namespace_path, useBytes = TRUE)
  message("Wrote fallback NAMESPACE exporting ", length(exports), " functions (non-dot).")
}

use_roxygen <- requireNamespace("roxygen2", quietly = TRUE) && requireNamespace("devtools", quietly = TRUE)

if (use_roxygen) {
  message("Regenerating Rd + NAMESPACE via roxygen2…")
  suppressPackageStartupMessages({
    devtools::document(roclets = c("rd", "namespace"))
  })
} else {
  message("roxygen2/devtools not available; writing fallback NAMESPACE.")
  make_namespace_fallback(dst, "NAMESPACE")
}

# 7) Load & verify symbols
have_devtools <- requireNamespace("devtools", quietly = TRUE)
if (!have_devtools) {
  message("devtools not installed; skipping load_all() smoke test. Install devtools to verify.")
  quit(save = "no")
}

message("Running devtools::load_all()…")
suppressMessages(devtools::load_all(quiet = TRUE))

needed <- c("simulate_surro_data", "surro_network", "surro_nma")
missing <- needed[!nzchar(sapply(needed, function(x) {
  if (exists(x, mode = "function")) x else ""
}))]

if (length(missing)) {
  message("⚠ The following functions are still missing in the namespace: ", paste(missing, collapse = ", "))
  message("If you intended to export them, ensure they are defined in R/surroNMA.R and not wrapped in `if (FALSE) {}`.")
} else {
  message("✓ Core functions present: ", paste(needed, collapse = ", "))
  # tiny smoke test (non-Bayesian to avoid toolchain)
  ok_test <- FALSE
  try({
    if (is.function(simulate_surro_data) && is.function(surro_network) && is.function(surro_nma)) {
      df <- simulate_surro_data(K = 3, J = 5, seed = 1)
      net <- surro_network(df, study = study, trt = trt, comp = comp,
                           S_eff = logHR_S, S_se = se_S, T_eff = logHR_T, T_se = se_T)
      fit <- surro_nma(net, engine = "freq", B = 10)
      ok_test <- TRUE
    }
  }, silent = TRUE)
  if (ok_test) message("✅ Smoke test ran (freq engine).")
  else message("ℹ Core functions found, but smoke test skipped/failed (check data & args).")
}

message("=== Done. Try: devtools::check() and then build for CRAN. ===")
