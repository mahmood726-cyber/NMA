message("== surroNMA hotfix ==")

root <- getwd()
rfile <- file.path(root, "R", "surroNMA.R")
stopifnot(file.exists(rfile))

backup <- function(p) {
  dst <- sprintf("%s.bak-%s", p, format(Sys.time(), "%Y%m%d-%H%M%S"))
  file.copy(p, dst, overwrite = TRUE)
  message("Backup: ", dst)
}
txt <- readLines(rfile, warn = FALSE)
backup(rfile)

# 1) Stop evaluating bare symbols in surro_network() --------------------
# Replace any logic like: if (!is.null(S_eff)) ...  with  if (!missing(S_eff)) ...
swap_missing <- function(x, nm) {
  x <- gsub(sprintf("if\\s*\\(!is\\.null\\(%s\\)\\)", nm),
            sprintf("if (!missing(%s))", nm),
            x, perl = TRUE)
  # also common variants that used is.null(...) in a ternary/ifelse style
  x <- gsub(sprintf("!is\\.null\\(%s\\)", nm),
            sprintf("!missing(%s)", nm),
            x, perl = TRUE)
  x
}

for (nm in c("S_eff","S_se","T_eff","T_se","S_multi","S_multi_se",
             "corr_ST","class","baseline_risk","rob","treatment_info")) {
  txt <- swap_missing(txt, nm)
}

# And ensure the actual captures DON’T evaluate; use .get_col only when not missing()
txt <- gsub(
  "Se\\s*<-\\s*if\\s*\\(!missing\\(S_eff\\)\\)\\s*\\.get_col\\(df,\\s*S_eff\\)\\s*else\\s*NULL|Se\\s*<-.*deparse\\(substitute\\(S_eff\\)\\).*",
  "Se <- if (!missing(S_eff)) .get_col(df, S_eff) else NULL", txt, perl = TRUE)
txt <- gsub(
  "Ss\\s*<-\\s*if\\s*\\(!missing\\(S_se\\)\\)\\s*\\.get_col\\(df,\\s*S_se\\)\\s*else\\s*NULL|Ss\\s*<-.*deparse\\(substitute\\(S_se\\)\\).*",
  "Ss <- if (!missing(S_se))  .get_col(df, S_se)  else NULL", txt, perl = TRUE)
txt <- gsub(
  "Te\\s*<-\\s*if\\s*\\(!missing\\(T_eff\\)\\)\\s*\\.get_col\\(df,\\s*T_eff\\)\\s*else\\s*NULL|Te\\s*<-.*deparse\\(substitute\\(T_eff\\)\\).*",
  "Te <- if (!missing(T_eff)) .get_col(df, T_eff) else NULL", txt, perl = TRUE)
txt <- gsub(
  "Ts\\s*<-\\s*if\\s*\\(!missing\\(T_se\\)\\)\\s*\\.get_col\\(df,\\s*T_se\\)\\s*else\\s*NULL|Ts\\s*<-.*deparse\\(substitute\\(T_se\\)\\).*",
  "Ts <- if (!missing(T_se))  .get_col(df, T_se)  else NULL", txt, perl = TRUE)

txt <- gsub(
  "Corr\\s*<-\\s*if\\s*\\(!missing\\(corr_ST\\)\\)\\s*\\.get_col\\(df,\\s*corr_ST\\)\\s*else\\s*NULL|Corr\\s*<-.*deparse\\(substitute\\(corr_ST\\)\\).*",
  "Corr <- if (!missing(corr_ST)) .get_col(df, corr_ST) else NULL", txt, perl = TRUE)
txt <- gsub(
  "ClsRow\\s*<-\\s*if\\s*\\(!missing\\(class\\)\\)\\s*\\.get_col\\(df,\\s*class\\)\\s*else\\s*NULL|ClsRow\\s*<-.*deparse\\(substitute\\(class\\)\\).*",
  "ClsRow <- if (!missing(class)) .get_col(df, class) else NULL", txt, perl = TRUE)
txt <- gsub(
  "BaseRisk\\s*<-\\s*if\\s*\\(!missing\\(baseline_risk\\)\\)\\s*\\.get_col\\(df,\\s*baseline_risk\\)\\s*else\\s*NULL|BaseRisk\\s*<-.*deparse\\(substitute\\(baseline_risk\\)\\).*",
  "BaseRisk <- if (!missing(baseline_risk)) .get_col(df, baseline_risk) else NULL", txt, perl = TRUE)
txt <- gsub(
  "RoB\\s*<-\\s*if\\s*\\(!missing\\(rob\\)\\)\\s*\\.get_col\\(df,\\s*rob\\)\\s*else\\s*NULL|RoB\\s*<-.*deparse\\(substitute\\(rob\\)\\).*",
  "RoB <- if (!missing(rob)) .get_col(df, rob) else NULL", txt, perl = TRUE)

# Also for the core IDs:
txt <- gsub(
  "study_id\\s*<-\\s*df\\[\\[deparse\\(substitute\\(study\\)\\)\\]\\]",
  "study_id <- .get_col(df, study)", txt, perl = TRUE)
txt <- gsub(
  "A\\s*<-\\s*df\\[\\[deparse\\(substitute\\(trt\\)\\)\\]\\]",
  "A <- .get_col(df, trt)", txt, perl = TRUE)
txt <- gsub(
  "B\\s*<-\\s*df\\[\\[deparse\\(substitute\\(comp\\)\\)\\]\\]",
  "B <- .get_col(df, comp)", txt, perl = TRUE)

# 2) Fix accidental 'sl3::sl3::' introduced earlier --------------------
txt <- gsub("\\bsl3::sl3::", "sl3::", txt, perl = TRUE)

# 3) Namespace gWidgets2 GUI helpers to silence NOTE -------------------
gws <- c("gwindow","gnotebook","ggroup","gframe","gfilebrowse","gformlayout",
         "gedit","gbutton","gcombobox","gcheckbox","gtext","gtable",
         "addHandlerChanged","addHandlerClicked","gfile","gmessage")
for (sym in gws) {
  # only prefix when not already namespaced
  txt <- gsub(paste0("(?<![:\\w])", sym, "\\b"), paste0("gWidgets2::", sym), txt, perl = TRUE)
}

# svalue() and visible<- were handled previously, but ensure again:
txt <- gsub("(?<![:\\w])svalue\\(", "gWidgets2::svalue(", txt, perl = TRUE)
# replace 'visible(w) <- TRUE/FALSE' with helper using exported setter
if (!any(grepl("^\\s*set_visible\\s*<-\\s*function\\s*\\(", txt))) {
  txt <- c(txt,
           "",
           "set_visible <- function(obj, value) {",
           "  f <- getFromNamespace(\"visible<-\", \"gWidgets2\")",
           "  f(obj) <- value",
           "  invisible(obj)",
           "}",
           "")
}
txt <- gsub("^\\s*visible\\(([^\\)]*)\\)\\s*<-\\s*(TRUE|FALSE)\\s*$",
            "set_visible(\\1, \\2)", txt, perl = TRUE)

# Write file
writeLines(txt, rfile, useBytes = TRUE)
message("Patched: R/surroNMA.R")

# Re-gen docs/namespace
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Running devtools::document() ...")
  try(unlink(file.path(root, "NAMESPACE")), silent = TRUE)
  devtools::document(roclets = c("rd","namespace"))
}
message("== Hotfix done. Now run devtools::check() ==")
