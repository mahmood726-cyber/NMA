# tools/apply_fixes3.R
message("== surroNMA patcher v3 ==")

pkg_root <- getwd()
rfile <- file.path(pkg_root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
rd <- function(p) readLines(p, warn = FALSE)
wr <- function(x,p) writeLines(x, p, useBytes = TRUE)
bk <- function(p){ dest <- sprintf("%s.bak-%s", p, timestamp); file.copy(p, dest, TRUE); message("Backup: ", dest) }

bk(rfile)
x <- rd(rfile)

# 1) Ensure proper roxygen package header with imports used
if (!any(grepl('^"_PACKAGE"$', x))) {
  header <- c(
    "#' @keywords meta-analysis, network meta-analysis, surrogate",
    "#' @name surroNMA",
    "#' @importFrom stats ave coef na.omit setNames var cov quantile rnorm rchisq pnorm",
    "#' @importFrom utils flush.console write.csv globalVariables",
    "#' @import graphics",
    "#' @import methods",
    "\"_PACKAGE\"",
    "NULL",
    ""
  )
  x <- c(header, x)
  message("Inserted roxygen package header with imports.")
} else {
  # remove deprecated @docType if present
  x <- x[!grepl("^#'\\s*@docType", x)]
}

# 2) Global variables (tidy eval / ggplot vars)
if (!any(grepl("^utils::globalVariables\\(", x))) {
  x <- append(x,
              'utils::globalVariables(c("S","T","ste","pred","obs","prob","treatment","type","SUCRA","scenario","svalue","visible<-"))',
              after = which(trimws(x) == "NULL")[1])
  message("Added utils::globalVariables().")
}

# 3) Robust column getter (+ name normalizer)
if (!any(grepl("^\\s*\\.norm_colname\\s*<-\\s*function", x))) {
  ins_at <- which(grepl("globalVariables\\(", x)); ins_at <- if (length(ins_at)) ins_at[1] else 1
  add <- c(
    "",
    ".norm_colname <- function(expr) {",
    "  # accept bare symbol, name, or quoted string",
    "  if (is.character(expr) && length(expr)==1) return(expr)",
    "  nm <- tryCatch(deparse(substitute(expr)), error = function(e) NA_character_)",
    "  if (!is.na(nm)) {",
    "    nm <- sub('^\"(.*)\"$', '\\\\1', nm)",
    "    nm <- sub(\"^'(.*)'$\", \"\\\\1\", nm)",
    "    return(nm)",
    "  }",
    "  stop(\"Could not resolve column name from argument\")",
    "}",
    ".get_col <- function(df, expr, allow_null = FALSE) {",
    "  if (is.null(expr)) { if (allow_null) return(NULL) else stop(\"Required column not specified\") }",
    "  nm <- .norm_colname(expr)",
    "  if (!nzchar(nm)) stop(\"Empty column name\")",
    "  if (!nm %in% names(df)) stop(\"Column not found in data: \", nm)",
    "  df[[nm]]",
    "}",
    ""
  )
  x <- append(x, add, after = ins_at)
  message("Inserted .norm_colname() / .get_col().")
}

# 4) Patch surro_network to use .get_col everywhere
sn_start <- grep("^\\s*surro_network\\s*<-\\s*function\\s*\\(", x)
if (length(sn_start)) {
  s <- sn_start[1]; e <- min(s + 400L, length(x))
  block <- x[s:e]
  # Replace the deparse(substitute(...)) captures with .get_col()
  repls <- list(
    "study_id <- df\\[\\[deparse\\(substitute\\(study\\)\\)\\]\\]" = "study_id <- .get_col(df, study)",
    "A <- df\\[\\[deparse\\(substitute\\(trt\\)\\)\\]\\]"         = "A <- .get_col(df, trt)",
    "B <- df\\[\\[deparse\\(substitute\\(comp\\)\\)\\]\\]"        = "B <- .get_col(df, comp)",
    "Se <- if \\(!is.null\\(S_eff\\)\\) df\\[\\[deparse\\(substitute\\(S_eff\\)\\)\\]\\] else NULL" =
      "Se <- if (!is.null(S_eff)) .get_col(df, S_eff) else NULL",
    "Ss <- if \\(!is.null\\(S_se\\)\\)\\s*df\\[\\[deparse\\(substitute\\(S_se\\)\\)\\]\\]\\s*else NULL" =
      "Ss <- if (!is.null(S_se)) .get_col(df, S_se) else NULL",
    "Te <- if \\(!is.null\\(T_eff\\)\\) df\\[\\[deparse\\(substitute\\(T_eff\\)\\)\\]\\] else NULL" =
      "Te <- if (!is.null(T_eff)) .get_col(df, T_eff) else NULL",
    "Ts <- if \\(!is.null\\(T_se\\)\\)\\s*df\\[\\[deparse\\(substitute\\(T_se\\)\\)\\]\\]\\s*else NULL" =
      "Ts <- if (!is.null(T_se)) .get_col(df, T_se) else NULL",
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
    block <- sub(pat, repls[[pat]], block, perl = TRUE)
  }
  x[s:e] <- block
  message("Rewrote column capture in surro_network().")
}

# 5) Remove any library(sl3) / library(gWidgets2) in package code
x <- x[!grepl("^\\s*library\\((sl3|gWidgets2)\\)\\s*$", x)]

# 6) Qualify any plain svalue(...) calls; and fix visible(w) <- TRUE
x <- gsub("(?<![:\\w])svalue\\(", "gWidgets2::svalue(", x, perl = TRUE)
# Replace assignments like: visible(w) <- TRUE  ==> set_visible(w, TRUE)
if (!any(grepl("^\\s*set_visible\\s*<-\\s*function\\s*\\(", x))) {
  add <- c(
    "",
    "set_visible <- function(obj, value) {",
    "  f <- getFromNamespace(\"visible<-\", \"gWidgets2\")",
    "  f(obj) <- value",
    "  invisible(obj)",
    "}",
    ""
  )
  x <- c(x, add)
}
x <- gsub("^\\s*visible\\(([^\\)]*)\\)\\s*<-\\s*(TRUE|FALSE)\\s*$",
          "set_visible(\\1, \\2)", x, perl = TRUE)

# 7) Prefix sl3 learner class names (safety)
x <- gsub("\\bLrnr_glm_fast\\b", "sl3::Lrnr_glm_fast", x)
x <- gsub("\\bLrnr_sl\\b",       "sl3::Lrnr_sl",       x)
x <- gsub("\\bLrnr_nnls\\b",     "sl3::Lrnr_nnls",     x)

# 8) Replace posterior::as_draws_vec(...) (if any remain)
x <- gsub("posterior::as_draws_vec\\(([^)]+)\\)",
          "as.numeric(posterior::as_draws_matrix(\\1))", x)

# 9) Write file
wr(x, rfile)
message("Wrote patched R/surroNMA.R")

# 10) Ensure DESCRIPTION has proper License + VignetteBuilder
desc <- file.path(pkg_root, "DESCRIPTION"); bk(desc); d <- rd(desc)
set_field <- function(lines, key, value){
  pat <- paste0("^", key, ":\\s*")
  if (any(grepl(pat, lines))) { lines[grepl(pat, lines)] <- paste0(key, ": ", value)
  } else lines <- c(lines, paste0(key, ": ", value))
  lines
}
d <- set_field(d, "License", "MIT + file LICENSE")
if (!any(grepl("^VignetteBuilder:", d))) d <- c(d, "VignetteBuilder: knitr")
wr(d, desc)
lic <- file.path(pkg_root, "LICENSE")
if (!file.exists(lic)) writeLines(c("YEAR: 2025","COPYRIGHT HOLDER: Your Name"), lic)
message("Patched DESCRIPTION / LICENSE.")

# 11) .Rbuildignore tidy
rb <- file.path(pkg_root, ".Rbuildignore")
if (!file.exists(rb)) file.create(rb)
b <- rd(rb)
need <- c("^tools$","^.*[.]bak","^.*backup","^.*autobak","^.*pre-bracefix","^.*[.]Rproj$",
          "^\\.Rhistory$","^\\.Rprofile$","^\\.DS_Store$","^cran-comments[.]md$")
add <- setdiff(need, b)
if (length(add)) writeLines(c(b, add), rb)
message("Updated .Rbuildignore.")

# 12) Re-document
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(pkg_root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}

message("== Patch completed. Run devtools::check() next. ==")
