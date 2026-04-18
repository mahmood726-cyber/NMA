# tools/apply_fixes.R
# One-shot patcher for surroNMA package tree under C:/NMA
# Safe: always writes a timestamped backup before modifying any file.

message("== surroNMA patcher starting ==")

pkg_root <- getwd()
stopifnot(dir.exists(pkg_root))
stopifnot(file.exists(file.path(pkg_root, "DESCRIPTION")))
stopifnot(dir.exists(file.path(pkg_root, "R")))
rfile <- file.path(pkg_root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

read_lines  <- function(p) readLines(p, warn = FALSE)
write_lines <- function(x, p) writeLines(x, p, useBytes = TRUE)

# ---- helper: file backup ------------------------------------------------------
backup_file <- function(p) {
  dest <- sprintf("%s.bak-%s", p, timestamp)
  ok <- file.copy(p, dest, overwrite = TRUE)
  if (ok) message("Backed up: ", p, " -> ", dest) else stop("Backup failed for ", p)
}

# ---- 1) Patch R/surroNMA.R ----------------------------------------------------
backup_file(rfile)
x <- read_lines(rfile)

# A. Remove deprecated @docType and make a proper package header with imports
#    We'll replace the first roxygen block up to the first NULL sentinel.
ix_null <- which(trimws(x) == "NULL")[1]
if (!is.na(ix_null)) {
  # Find where roxygen header starts (first line with "#'")
  h_start <- which(grepl("^#'", x))[1]
  if (!is.na(h_start) && h_start < ix_null) {
    header <- x[h_start:ix_null]
    # Kill any @docType lines
    header <- header[!grepl("@docType", header)]
    # Ensure keywords/name remain; then append the _PACKAGE + imports if not present
    needed <- c(
      "#' @keywords meta-analysis, network meta-analysis, surrogate",
      "#' @name surroNMA",
      "#' @importFrom stats ave coef na.omit setNames var cov quantile rnorm rchisq pnorm",
      "#' @importFrom utils flush.console write.csv",
      "\"_PACKAGE\""
    )
    # Make sure last line before NULL is "_PACKAGE"
    # Replace whole header region cleanly:
    x[h_start:ix_null] <- c(header[grepl("^#'", header)], needed, "NULL")
  }
}

# B. Ensure utils::globalVariables() exists (add right after NULL or near top)
gv <- "utils::globalVariables(c(\"S\",\"T\",\"ste\",\"pred\",\"obs\",\"prob\",\"treatment\",\"type\",\"SUCRA\",\"scenario\"))"
if (!any(grepl("^utils::globalVariables\\(", x))) {
  insert_at <- which(trimws(x) == "NULL")[1]
  if (is.na(insert_at)) insert_at <- 1
  x <- append(x, values = c("", gv, ""), after = insert_at)
}

# C. Insert .get_col() helper after the globalVariables line if not present
if (!any(grepl("^\\.get_col\\s*<-\\s*function", x))) {
  anchor <- which(grepl("globalVariables\\(", x))[1]
  if (is.na(anchor)) anchor <- 1
  col_helper <- c(
    "",
    "# internal: resolve a column from data even if the user passed \"col\" as a string literal",
    ".get_col <- function(df, expr) {",
    "  nm <- deparse(substitute(expr))",
    "  nm <- sub('^\"(.*)\"$', '\\\\1', nm)",
    "  nm <- sub(\"^'(.*)'$\", \"\\\\1\", nm)",
    "  df[[nm]]",
    "}",
    ""
  )
  x <- append(x, values = col_helper, after = anchor)
}

# D. Patch surro_network() column capture to use .get_col()
sn_start <- grep("^\\s*surro_network\\s*<-\\s*function\\s*\\(", x)
if (length(sn_start)) {
  # Find the brace block roughly; we only need to replace assignments near top.
  rng <- sn_start[1]:min(length(x), sn_start[1] + 250)
  repl_map <- list(
    "study_id <- df\\[\\[deparse\\(substitute\\(study\\)\\)\\]\\]" = "study_id <- .get_col(df, study)",
    "A <- df\\[\\[deparse\\(substitute\\(trt\\)\\)\\]\\]" = "A <- .get_col(df, trt)",
    "B <- df\\[\\[deparse\\(substitute\\(comp\\)\\)\\]\\]" = "B <- .get_col(df, comp)",
    "Se <- if \\(!is.null\\(S_eff\\)\\) df\\[\\[deparse\\(substitute\\(S_eff\\)\\)\\]\\] else NULL" = "Se <- if (!is.null(S_eff)) .get_col(df, S_eff) else NULL",
    "Ss <- if \\(!is.null\\(S_se\\)\\)  df\\[\\[deparse\\(substitute\\(S_se\\)\\)\\]\\]  else NULL" = "Ss <- if (!is.null(S_se))  .get_col(df, S_se)  else NULL",
    "Te <- if \\(!is.null\\(T_eff\\)\\) df\\[\\[deparse\\(substitute\\(T_eff\\)\\)\\]\\] else NULL" = "Te <- if (!is.null(T_eff)) .get_col(df, T_eff) else NULL",
    "Ts <- if \\(!is.null\\(T_se\\)\\)  df\\[\\[deparse\\(substitute\\(T_se\\)\\)\\]\\]  else NULL" = "Ts <- if (!is.null(T_se))  .get_col(df, T_se)  else NULL",
    "Corr <- if \\(!is.null\\(corr_ST\\)\\) df\\[\\[deparse\\(substitute\\(corr_ST\\)\\)\\]\\] else NULL" = "Corr <- if (!is.null(corr_ST)) .get_col(df, corr_ST) else NULL",
    "ClsRow <- if \\(!is.null\\(class\\)\\) df\\[\\[deparse\\(substitute\\(class\\)\\)\\]\\] else NULL" = "ClsRow <- if (!is.null(class)) .get_col(df, class) else NULL",
    "BaseRisk <- if \\(!is.null\\(baseline_risk\\)\\) df\\[\\[deparse\\(substitute\\(baseline_risk\\)\\)\\]\\] else NULL" = "BaseRisk <- if (!is.null(baseline_risk)) .get_col(df, baseline_risk) else NULL",
    "RoB <- if \\(!is.null\\(rob\\)\\) df\\[\\[deparse\\(substitute\\(rob\\)\\)\\]\\] else NULL" = "RoB <- if (!is.null(rob)) .get_col(df, rob) else NULL"
  )
  for (pat in names(repl_map)) {
    idx <- grep(pat, x[rng])
    if (length(idx)) {
      x[rng[idx]] <- sub(pat, repl_map[[pat]], x[rng[idx]])
    }
  }
}

# E. Remove any "library(sl3)" or "library(gWidgets2)" lines in package code
x <- x[!grepl("^\\s*library\\((sl3|gWidgets2)\\)\\s*$", x)]

# F. Fully-qualify sl3 learners in surro_index_train()
sit_start <- grep("^\\s*surro_index_train\\s*<-\\s*function\\s*\\(", x)
if (length(sit_start)) {
  rng <- sit_start[1]:min(length(x), sit_start[1] + 200)
  x[rng] <- gsub("\\bLrnr_glm_fast\\b", "sl3::Lrnr_glm_fast", x[rng])
  x[rng] <- gsub("\\bLrnr_sl\\b",       "sl3::Lrnr_sl",       x[rng])
  x[rng] <- gsub("\\bLrnr_nnls\\b",     "sl3::Lrnr_nnls",     x[rng])
}

# G. Replace posterior::as_draws_vec usage with as_draws_matrix -> as.numeric
sd_start <- grep("^\\s*surrogacy_diagnostics\\s*<-\\s*function\\s*\\(", x)
if (length(sd_start)) {
  rng <- sd_start[1]:min(length(x), sd_start[1] + 200)
  # Replace the two-line pattern if present
  patt1 <- "alpha <- posterior::as_draws_vec\\(a0\\); beta <- posterior::as_draws_vec\\(b0\\)"
  if (any(grepl(patt1, x[rng], fixed = TRUE))) {
    x[rng] <- gsub(patt1,
                   "alpha <- as.numeric(posterior::as_draws_matrix(a0)); beta <- as.numeric(posterior::as_draws_matrix(b0))",
                   x[rng], fixed = TRUE)
  } else {
    # Or replace individually
    x[rng] <- gsub("posterior::as_draws_vec\\(a0\\)",
                   "as.numeric(posterior::as_draws_matrix(a0))", x[rng])
    x[rng] <- gsub("posterior::as_draws_vec\\(b0\\)",
                   "as.numeric(posterior::as_draws_matrix(b0))", x[rng])
  }
}

# H. Remove stray, duplicate GUI block outside the function
#    Detect a block that starts with bare gwindow(...) and ends at invisible(w)
blk_start <- grep("^\\s*w\\s*<-\\s*gwindow\\(", x)
if (length(blk_start)) {
  # Heuristic: keep only blocks NOT inside 'surroNMA_gui_gw <- function'
  # We'll nuke from the first bare gwindow(...) until the next closing brace that follows an "invisible(w)"
  s <- blk_start[1]
  e_inv <- grep("^\\s*invisible\\(w\\)\\s*$", x)
  e_inv <- e_inv[e_inv >= s]
  if (length(e_inv)) {
    e <- e_inv[1]
    x <- x[-seq(s, e)]
    message("Removed stray GUI block lines ", s, ":", e)
  }
}

# I. Ensure GUI calls are qualified (best-effort)
qualify_gui <- function(vec) {
  gui_syms <- c("gwindow","gnotebook","ggroup","gframe","gfilebrowse","gformlayout",
                "gedit","gbutton","gcombobox","gcheckbox","gtext","gtable",
                "addHandlerChanged","addHandlerClicked","gmessage","gfile","visible\\s*<-")
  for (sym in gui_syms) {
    # skip already qualified
    vec <- gsub(paste0("(?<![:alnum:]:)", sym, "\\b"),
                paste0("gWidgets2::", sub("\\\\s\\*<-","<-", sym)),
                vec, perl = TRUE)
  }
  vec
}
x <- qualify_gui(x)

# J. Write back the patched R file
write_lines(x, rfile)
message("Patched: ", rfile)

# ---- 2) Clean up backup junk under R/ -----------------------------------------
junk_patterns <- c("surroNMA.R.autobak-", "surroNMA.R.backup-", "surroNMA.R.bak-", "surroNMA.R.pre-bracefix-")
rfiles <- list.files(file.path(pkg_root, "R"), full.names = TRUE)
for (p in rfiles) {
  if (any(startsWith(basename(p), junk_patterns))) {
    unlink(p, force = TRUE)
  }
}

# ---- 3) .Rbuildignore updates -------------------------------------------------
rb <- file.path(pkg_root, ".Rbuildignore")
if (!file.exists(rb)) file.create(rb)
b <- read_lines(rb)
need <- c(
  "^tools$",
  "^.*[.]bak",
  "^.*backup",
  "^.*autobak",
  "^.*pre-bracefix",
  "^NAMESPACE[.]backup$",
  "^.*[.]Rproj$",
  "^\\.Rhistory$",
  "^\\.Rprofile$",
  "^\\.DS_Store$",
  "^cran-comments[.]md$"
)
add <- setdiff(need, b)
if (length(add)) {
  write_lines(c(b, add), rb)
  message("Updated .Rbuildignore")
}

# ---- 4) DESCRIPTION: ensure proper License lines ------------------------------
desc_path <- file.path(pkg_root, "DESCRIPTION")
backup_file(desc_path)
d <- read_lines(desc_path)

set_field <- function(lines, key, value) {
  pat <- paste0("^", key, ":")
  if (any(grepl(pat, lines))) {
    lines[grepl(pat, lines)] <- paste0(key, ": ", value)
  } else {
    lines <- c(lines, paste0(key, ": ", value))
  }
  lines
}
d <- set_field(d, "License", "MIT + file LICENSE")
if (!any(grepl("^VignetteBuilder:", d))) {
  d <- c(d, "VignetteBuilder: knitr")
}
write_lines(d, desc_path)
message("Patched DESCRIPTION")

# ---- 5) LICENSE file (CRAN two-liner) -----------------------------------------
lic_path <- file.path(pkg_root, "LICENSE")
if (!file.exists(lic_path)) {
  write_lines(c("YEAR: 2025", "COPYRIGHT HOLDER: Your Name"), lic_path)
  message("Created LICENSE (CRAN two-liner). Edit the name/year as appropriate.")
} else {
  # Ensure it has the two required keys
  L <- read_lines(lic_path)
  if (!any(grepl("^YEAR:", L))) L <- c("YEAR: 2025", L)
  if (!any(grepl("^COPYRIGHT HOLDER:", L))) L <- c(L, "COPYRIGHT HOLDER: Your Name")
  write_lines(L, lic_path)
  message("Ensured LICENSE has CRAN two-liner keys (edit values as needed).")
}

# ---- 6) Re-document to regenerate NAMESPACE -----------------------------------
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(pkg_root, "NAMESPACE")), silent = TRUE)
  try(devtools::document(roclets = c("rd","namespace")), silent = FALSE)
} else {
  message("NOTE: devtools not installed; skip document(). Install devtools and run devtools::document() manually.")
}

message("== Done. Now run: devtools::check() ==")
