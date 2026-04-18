# tools/apply_fixes2.R
message("== surroNMA patcher v2 starting ==")

pkg_root <- getwd()
stopifnot(dir.exists(file.path(pkg_root, "R")))
rfile <- file.path(pkg_root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

read_lines  <- function(p) readLines(p, warn = FALSE)
write_lines <- function(x, p) writeLines(x, p, useBytes = TRUE)
backup_file <- function(p) {
  dest <- sprintf("%s.bak-%s", p, timestamp)
  if (!file.copy(p, dest, overwrite = TRUE)) stop("Backup failed for ", p)
  message("Backed up: ", p, " -> ", dest)
}

backup_file(rfile)
x <- read_lines(rfile)

# ---------- A) Ensure proper package header (no @docType) ----------
has_pkg_block <- any(grepl('^"_PACKAGE"\\s*$', x))
if (!has_pkg_block) {
  header <- c(
    "#' @keywords meta-analysis, network meta-analysis, surrogate",
    "#' @name surroNMA",
    "#' @importFrom stats ave coef na.omit setNames var cov quantile rnorm rchisq pnorm",
    "#' @importFrom utils flush.console write.csv",
    "\"_PACKAGE\"",
    "NULL",
    ""
  )
  x <- c(header, x)
  message("Inserted roxygen package header with _PACKAGE.")
} else {
  # strip any @docType lines if they exist
  x <- x[!grepl("^#'\\s*@docType", x)]
}

# ---------- B) Add globalVariables for NSE plots ----------
if (!any(grepl("^utils::globalVariables\\(", x))) {
  gv <- "utils::globalVariables(c(\"S\",\"T\",\"ste\",\"pred\",\"obs\",\"prob\",\"treatment\",\"type\",\"SUCRA\",\"scenario\"))"
  # place right after first NULL (the package header NULL)
  after <- which(trimws(x) == "NULL")
  ins_at <- if (length(after)) after[1] else 1
  x <- append(x, values = c("", gv, ""), after = ins_at)
  message("Added utils::globalVariables(...)")
}

# ---------- C) Add .get_col() helper ----------
if (!any(grepl("^\\s*\\.get_col\\s*<-\\s*function", x))) {
  anchor <- which(grepl("globalVariables\\(", x))
  ins_at <- if (length(anchor)) anchor[1] else 1
  col_helper <- c(
    "",
    "# internal: resolve a column from data even if the user passed \"col\" as a string",
    ".get_col <- function(df, expr) {",
    "  nm <- deparse(substitute(expr))",
    "  nm <- sub('^\"(.*)\"$', '\\\\1', nm)",
    "  nm <- sub(\"^'(.*)'$\", \"\\\\1\", nm)",
    "  df[[nm]]",
    "}",
    ""
  )
  x <- append(x, values = col_helper, after = ins_at)
  message("Inserted .get_col() helper.")
}

# ---------- D) Switch surro_network() column capture to .get_col ----------
sn_start <- grep("^\\s*surro_network\\s*<-\\s*function\\s*\\(", x)
if (length(sn_start)) {
  s <- sn_start[1]
  e <- s + 250L
  e <- min(e, length(x))
  repls <- list(
    "study_id <- df\\[\\[deparse\\(substitute\\(study\\)\\)\\]\\]" = "study_id <- .get_col(df, study)",
    "A <- df\\[\\[deparse\\(substitute\\(trt\\)\\)\\]\\]" = "A <- .get_col(df, trt)",
    "B <- df\\[\\[deparse\\(substitute\\(comp\\)\\)\\]\\]" = "B <- .get_col(df, comp)",
    "Se <- if \\(!is.null\\(S_eff\\)\\) df\\[\\[deparse\\(substitute\\(S_eff\\)\\)\\]\\] else NULL" =
      "Se <- if (!is.null(S_eff)) .get_col(df, S_eff) else NULL",
    "Ss <- if \\(!is.null\\(S_se\\)\\)  df\\[\\[deparse\\(substitute\\(S_se\\)\\)\\]\\]  else NULL" =
      "Ss <- if (!is.null(S_se))  .get_col(df, S_se)  else NULL",
    "Te <- if \\(!is.null\\(T_eff\\)\\) df\\[\\[deparse\\(substitute\\(T_eff\\)\\)\\]\\] else NULL" =
      "Te <- if (!is.null(T_eff)) .get_col(df, T_eff) else NULL",
    "Ts <- if \\(!is.null\\(T_se\\)\\)  df\\[\\[deparse\\(substitute\\(T_se\\)\\)\\]\\]  else NULL" =
      "Ts <- if (!is.null(T_se))  .get_col(df, T_se)  else NULL",
    "Corr <- if \\(!is.null\\(corr_ST\\)\\) df\\[\\[deparse\\(substitute\\(corr_ST\\)\\)\\]\\] else NULL" =
      "Corr <- if (!is.null(corr_ST)) .get_col(df, corr_ST) else NULL",
    "ClsRow <- if \\(!is.null\\(class\\)\\) df\\[\\[deparse\\(substitute\\(class\\)\\)\\]\\] else NULL" =
      "ClsRow <- if (!is.null(class)) .get_col(df, class) else NULL",
    "BaseRisk <- if \\(!is.null\\(baseline_risk\\)\\) df\\[\\[deparse\\(substitute\\(baseline_risk\\)\\)\\]\\] else NULL" =
      "BaseRisk <- if (!is.null(baseline_risk)) .get_col(df, baseline_risk) else NULL",
    "RoB <- if \\(!is.null\\(rob\\)\\) df\\[\\[deparse\\(substitute\\(rob\\)\\)\\]\\] else NULL" =
      "RoB <- if (!is.null(rob)) .get_col(df, rob) else NULL"
  )
  for (pat in names(repls)) {
    local <- x[s:e]
    hit <- grep(pat, local)
    if (length(hit)) x[s + hit[1L] - 1L] <- sub(pat, repls[[pat]], local[hit[1L]])
  }
  message("Patched surro_network() column capture.")
}

# ---------- E) Remove library() calls to suggested packages ----------
x <- x[!grepl("^\\s*library\\((sl3|gWidgets2)\\)\\s*$", x)]

# ---------- F) Qualify sl3 learners ----------
sit_start <- grep("^\\s*surro_index_train\\s*<-\\s*function\\s*\\(", x)
if (length(sit_start)) {
  s <- sit_start[1]; e <- min(s + 200L, length(x))
  x[s:e] <- gsub("\\bLrnr_glm_fast\\b", "sl3::Lrnr_glm_fast", x[s:e])
  x[s:e] <- gsub("\\bLrnr_sl\\b",       "sl3::Lrnr_sl",       x[s:e])
  x[s:e] <- gsub("\\bLrnr_nnls\\b",     "sl3::Lrnr_nnls",     x[s:e])
  message("Qualified sl3 learners in surro_index_train().")
}

# ---------- G) Replace posterior::as_draws_vec usage ----------
sd_start <- grep("^\\s*surrogacy_diagnostics\\s*<-\\s*function\\s*\\(", x)
if (length(sd_start)) {
  s <- sd_start[1]; e <- min(s + 200L, length(x))
  x[s:e] <- gsub("posterior::as_draws_vec\\(a0\\)",
                 "as.numeric(posterior::as_draws_matrix(a0))", x[s:e])
  x[s:e] <- gsub("posterior::as_draws_vec\\(b0\\)",
                 "as.numeric(posterior::as_draws_matrix(b0))", x[s:e])
  message("Replaced as_draws_vec() calls.")
}

# ---------- H) Remove stray GUI block outside function (heuristic) ----------
bare_gw <- grep("^\\s*w\\s*<-\\s*gwindow\\(", x)
if (length(bare_gw)) {
  s <- bare_gw[1]
  # Only remove if NOT inside the surroNMA_gui_gw function body (very rough scan)
  fn_start <- grep("^\\s*surroNMA_gui_gw\\s*<-\\s*function\\s*\\(", x)
  fn_end   <- if (length(fn_start)) {
    # naive: find next top-level closing brace after fn_start
    # fallback: cut nothing if uncertain
    cand <- grep("^\\}\\s*$", x)
    cand[cand > fn_start[1]][1]
  } else NA_integer_
  remove_block <- TRUE
  if (!is.na(fn_start) && !is.na(fn_end) && s > fn_start && s < fn_end) remove_block <- FALSE
  if (remove_block) {
    e <- grep("^\\s*invisible\\(w\\)\\s*$", x)
    e <- e[e >= s]
    if (length(e)) {
      x <- x[-seq(s, e[1])]
      message("Removed stray GUI block lines ", s, ":", e[1])
    }
  }
}

# ---------- I) Qualify common gWidgets2 calls (no look-behind) ----------
qualify_gui <- function(vec) {
  syms <- c("gwindow","gnotebook","ggroup","gframe","gfilebrowse","gformlayout",
            "gedit","gbutton","gcombobox","gcheckbox","gtext","gtable",
            "addHandlerChanged","addHandlerClicked","gmessage","gfile")
  for (sym in syms) {
    # Replace occurrences of 'sym' not already qualified with '::'
    # (^|[^:])\bSYM\b  ->  \1gWidgets2::SYM
    pat <- paste0("(^|[^:])\\b", sym, "\\b")
    repl <- paste0("\\1gWidgets2::", sym)
    vec <- gsub(pat, repl, vec, perl = TRUE)
  }
  vec
}
x <- qualify_gui(x)

write_lines(x, rfile)
message("Patched: ", rfile)

# ---------- J) Clean junk backups under R/ ----------
junk_patterns <- c("surroNMA.R.autobak-","surroNMA.R.backup-","surroNMA.R.bak-","surroNMA.R.pre-bracefix-")
rfiles <- list.files(file.path(pkg_root, "R"), full.names = TRUE)
for (p in rfiles) {
  b <- basename(p)
  if (any(startsWith(b, junk_patterns))) unlink(p, force = TRUE)
}

# ---------- K) .Rbuildignore ----------
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
  message("Updated .Rbuildignore.")
}

# ---------- L) DESCRIPTION license + vignette builder ----------
desc_path <- file.path(pkg_root, "DESCRIPTION")
backup_file(desc_path)
d <- read_lines(desc_path)
set_field <- function(lines, key, value) {
  pat <- paste0("^", key, ":\\s*")
  if (any(grepl(pat, lines))) {
    lines[grepl(pat, lines)] <- paste0(key, ": ", value)
  } else lines <- c(lines, paste0(key, ": ", value))
  lines
}
d <- set_field(d, "License", "MIT + file LICENSE")
if (!any(grepl("^VignetteBuilder:", d))) d <- c(d, "VignetteBuilder: knitr")
write_lines(d, desc_path)
message("Patched DESCRIPTION.")

# ---------- M) LICENSE two-liner ----------
lic_path <- file.path(pkg_root, "LICENSE")
if (!file.exists(lic_path)) {
  write_lines(c("YEAR: 2025", "COPYRIGHT HOLDER: Your Name"), lic_path)
  message("Created LICENSE (CRAN two-liner).")
} else {
  L <- read_lines(lic_path)
  if (!any(grepl("^YEAR:", L))) L <- c("YEAR: 2025", L)
  if (!any(grepl("^COPYRIGHT HOLDER:", L))) L <- c(L, "COPYRIGHT HOLDER: Your Name")
  write_lines(L, lic_path)
  message("Ensured LICENSE has required keys (edit values as needed).")
}

# ---------- N) Re-document ----------
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(pkg_root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
} else {
  message("NOTE: devtools not installed; skipping document().")
}

message("== Done. Now run: devtools::check() ==")
