# tools/patch_surroNMA_core.R
# Run from package root. This script surgically replaces several malformed
# regions in R/surroNMA.R so the file parses and the package can build.

message("=== surroNMA core patch ===")
stopifnot(file.exists("DESCRIPTION"))
stopifnot(dir.exists("R"))
stopifnot(file.exists("R/surroNMA.R"))

read_lines  <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

backup <- sprintf("R/surroNMA.R.bak-%s", format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy("R/surroNMA.R", backup, overwrite = TRUE)
message("Backup:", backup)

find_block <- function(lines, start_pat, end_pat = NULL, end_after_marker = NULL) {
  s <- grep(start_pat, lines, perl = TRUE)
  if (!length(s)) return(NULL)
  s <- s[1]

  if (!is.null(end_after_marker)) {
    e <- grep(end_after_marker, lines, perl = TRUE)
    e <- e[e > s]
    if (!length(e)) e <- length(lines) else e <- e[1] - 1L
    return(list(start = s, end = e))
  }

  if (!is.null(end_pat)) {
    e <- grep(end_pat, lines, perl = TRUE)
    e <- e[e > s]
    if (!length(e)) e <- length(lines) else e <- e[1] - 1L
    return(list(start = s, end = e))
  }

  # Fallback: scan brace balance to find function end
  bal <- 0L
  i <- s
  open_fun <- grepl("\\{", lines[s])
  if (!open_fun) {
    # find the opening brace line
    j <- s
    while (j <= length(lines) && !grepl("\\{", lines[j])) j <- j + 1L
    if (j > length(lines)) return(NULL)
    s <- j; i <- j
  }
  while (i <= length(lines)) {
    # ignore braces inside strings/comments lightly
    li <- sub("#.*$", "", lines[i])
    li <- gsub('"(?:[^"\\\\]|\\\\.)*"', '""', li, perl = TRUE)
    li <- gsub("'(?:[^'\\\\]|\\\\.)*'", "''", li, perl = TRUE)
    bal <- bal + lengths(regmatches(li, gregexpr("\\{", li, perl = TRUE)))
    bal <- bal - lengths(regmatches(li, gregexpr("\\}", li, perl = TRUE)))
    if (bal <= 0L && i > s) break
    i <- i + 1L
  }
  list(start = s, end = min(i, length(lines)))
}

replace_block <- function(lines, start_pat, new_text, end_pat = NULL, end_after_marker = NULL) {
  blk <- find_block(lines, start_pat, end_pat = end_pat, end_after_marker = end_after_marker)
  if (is.null(blk)) return(NULL)
  pre  <- if (blk$start > 1) lines[1:(blk$start - 1)] else character()
  post <- if (blk$end   < length(lines)) lines[(blk$end + 1):length(lines)] else character()
  c(pre, new_text, post)
}

# ---------- Clean function bodies ----------
poth_clean <- c(
  "#' @export",
  "poth <- function(ranks) {",
  "  K <- ncol(ranks);",
  "  modal_order <- order(colMeans(ranks, na.rm = TRUE));",
  "  kendall_dist <- function(order1, order2) {",
  "    K <- length(order1);",
  "    o1 <- order(order1); o2 <- order(order2);",
  "    d <- 0L;",
  "    for (i in 1:(K-1)) for (j in (i+1):K) {",
  "      d <- d + as.integer((o1[i]-o1[j])*(o2[i]-o2[j]) < 0)",
  "    }",
  "    d",
  "  }",
  "  dmax <- K*(K-1)/2",
  "  dbar <- mean(apply(ranks, 1, function(r) kendall_dist(order(r), modal_order)))",
  "  1 - dbar/dmax",
  "}"
)

stan_code_biv_clean <- c(
  ".stan_code_biv <- function(second_order = FALSE, use_t = FALSE, class_specific = TRUE,",
  "                           inconsistency = c('none','random'),",
  "                           inc_on = c('T','S','both'),",
  "                           global_surrogacy = FALSE,",
  "                           survival_mode = c('ph','nph_msplines')) {",
  "  inconsistency <- match.arg(inconsistency)",
  "  inc_on <- match.arg(inc_on)",
  "  survival_mode <- match.arg(survival_mode)",
  "  append_if <- function(v, cond, s) if (cond) c(v, s) else v",
  "  code <- c(",
  "    'data{',",
  "    'int<lower=2> K;',",
  "    'int<lower=1> N;',",
  "    'int<lower=1> J;',",
  "    'int<lower=1, upper=J> study_id[N];',",
  "    'int<lower=1, upper=K> a[N];',",
  "    'int<lower=1, upper=K> b[N];',",
  "    'vector[N] S_eff;',",
  "    'vector<lower=0>[N] S_se;',",
  "    'vector[N] T_eff;',",
  "    'vector<lower=0>[N] T_se;',",
  "    'vector[N] rho_ST;',",
  "    'int<lower=0,upper=1> has_T[N];',",
  "    'int<lower=1> G;',",
  "    'int<lower=1,upper=G> trt_class[K];',",
  "    'int<lower=0,upper=1> class_specific;',",
  "    'int<lower=0,upper=1> global_surrogacy;',",
  "    'real<lower=0> prior_tauS_scale;',",
  "    'real<lower=0> prior_tauT_scale;',",
  "    'real<lower=0> prior_tauI_scale;',",
  "    'real<lower=0> prior_sigma_d_scale;',",
  "    '}',",
  "    'parameters{',",
  "    'vector[K-1] dS_raw;',",
  "    'vector[K-1] dT_raw;',",
  "    'real alpha0;',",
  "    'real beta0;',",
  "    'vector[G] alpha_g_raw;',",
  "    'vector[G] beta_g_raw;',",
  "    'real<lower=0> tauS;',",
  "    'real<lower=0> tauT;',",
  "    'real<lower=0> sigma_d;',",
  "  )",
  "  code <- append_if(code, inconsistency != 'none', 'real<lower=0> tauI_S; real<lower=0> tauI_T;')",
  "  code <- c(code,",
  "    'vector[J] uS;',",
  "    'vector[J] uT;'",
  "  )",
  "  code <- append_if(code, isTRUE(use_t), 'real<lower=2> nu;')",
  "  code <- c(code,",
  "    '}',",
  "    'transformed parameters{',",
  "    'vector[K] dS; vector[K] dT;',",
  "    'dS[1] = 0; dT[1] = 0;',",
  "    'for (k in 2:K){ dS[k] = dS_raw[k-1]; dT[k] = dT_raw[k-1]; }',",
  "    'vector[G] alpha_g; vector[G] beta_g;',",
  "    'for (g in 1:G){ alpha_g[g] = alpha0 + alpha_g_raw[g]; beta_g[g] = beta0 + beta_g_raw[g]; }',",
  "    '}',",
  "    'model{',",
  "    'dS_raw ~ normal(0, 1.0);',",
  "    'dT_raw ~ normal(0, 1.0);',",
  "    'alpha0 ~ normal(0, 2);',",
  "    'beta0  ~ normal(0, 1);',",
  "    'alpha_g_raw ~ normal(0, 1);',",
  "    'beta_g_raw  ~ normal(0, 0.5);',",
  "    'tauS ~ normal(0, prior_tauS_scale);',",
  "    'tauT ~ normal(0, prior_tauT_scale);',",
  "    'sigma_d ~ normal(0, prior_sigma_d_scale);'",
  "  )",
  "  code <- append_if(code, inconsistency != 'none', 'tauI_S ~ normal(0, prior_tauI_scale); tauI_T ~ normal(0, prior_tauI_scale);')",
  "  code <- c(code,",
  "    'uS ~ normal(0, tauS);',",
  "    'uT ~ normal(0, tauT);',",
  "    'for (i in 1:N){',",
  "    '  int j = study_id[i];',",
  "    '  real muS = dS[a[i]] - dS[b[i]] + uS[j];'",
  "  )",
  "  code <- append_if(code, inconsistency != 'none' && inc_on %in% c('S','both'), '  muS = muS + normal_rng(0, tauI_S);')",
  "  code <- c(code,",
  "    '  target += normal_lpdf(S_eff[i] | muS, S_se[i]);',",
  "    '  if (has_T[i]==1){',",
  "    '    real muT = dT[a[i]] - dT[b[i]] + uT[j];',",
  "    '    real alpha = (global_surrogacy==1) ? alpha0 : alpha_g[ trt_class[a[i]] ];',",
  "    '    real beta  = (global_surrogacy==1) ? beta0  : beta_g[ trt_class[a[i]] ];',",
  "    '    target += normal_lpdf(T_eff[i] | muT, T_se[i]);',",
  "    '    target += normal_lpdf( ( (dT[a[i]]-dT[b[i]]) - (alpha + beta * (dS[a[i]]-dS[b[i]]) ) ) | 0, sigma_d );'",
  "  )",
  "  code <- append_if(code, inconsistency != 'none' && inc_on %in% c('T','both'),",
  "                    '    target += normal_lpdf( (T_eff[i] - (dT[a[i]]-dT[b[i]] + uT[j])) | 0, tauI_T );')",
  "  code <- c(code,",
  "    '  }',",
  "    '}',",
  "    '}',",
  "    'generated quantities{',",
  "    'vector[N] muT_pred;',",
  "    'for (i in 1:N){',",
  "    '  real alpha = (global_surrogacy==1) ? alpha0 : alpha_g[trt_class[a[i]]];',",
  "    '  real beta  = (global_surrogacy==1) ? beta0  : beta_g[trt_class[a[i]]];',",
  "    '  muT_pred[i] = alpha + beta * (dS[a[i]] - dS[b[i]]);',",
  "    '}',",
  "    '}'",
  "  )",
  "  paste(code, collapse = '\\n')",
  "}"
)

surro_nma_freq_clean <- c(
  "#' @export",
  "surro_nma_freq <- function(net, B = 400, boot = c('normal','student'), df = 5, seed = 1,",
  "                           multiarm_adj = TRUE, baseline_risk_mod = !is.null(net$baseline_risk),",
  "                           rob_weights = !is.null(net$rob), mid = NULL) {",
  "  boot <- match.arg(boot)",
  "  set.seed(seed)",
  "  dfreq <- net$data",
  "  K <- net$K; N <- nrow(dfreq)",
  "  ref <- 1L",
  "  X <- matrix(0, nrow=N, ncol=K-1)",
  "  for (i in 1:N) {",
  "    if (net$trt[i] != ref) X[i, net$trt[i]-1] <-  1",
  "    if (net$comp[i]!= ref) X[i, net$comp[i]-1] <- -1",
  "  }",
  "  w_rob <- rep(1, N)",
  "  if (rob_weights) {",
  "    r <- net$rob",
  "    r <- ifelse(is.finite(r), r, 0)",
  "    r <- (r - min(r,na.rm=TRUE)) / max(1e-8, diff(range(r,na.rm=TRUE)))",
  "    w_rob <- 1 - 0.5*r",
  "  }",
  "  if (multiarm_adj) {",
  "    m_con <- ave(seq_len(N), net$study, FUN = length)",
  "    w_ma <- 1/pmax(m_con - 1, 1)",
  "  } else w_ma <- 1",
  "  yS <- net$S_eff; vS <- net$S_se^2",
  "  W_S <- 1/(vS) * w_ma * w_rob",
  "  betaS <- try(solve(t(X)%*%(W_S*X), t(X)%*%(W_S*yS)), silent=TRUE)",
  "  if (inherits(betaS, 'try-error')) betaS <- MASS::ginv(t(X)%*%(W_S*X)) %*% t(X)%*%(W_S*yS)",
  "  dS_hat <- c(0, as.numeric(betaS))",
  "  obsT <- is.finite(net$T_eff)",
  "  XT <- X[obsT,,drop=FALSE]; yT <- net$T_eff[obsT]; vT <- net$T_se[obsT]^2",
  "  W_T <- 1/(vT) * w_ma[obsT] * w_rob[obsT]",
  "  if (nrow(XT) >= (K-1)) {",
  "    betaT <- try(solve(t(XT)%*%(W_T*XT), t(XT)%*%(W_T*yT)), silent=TRUE)",
  "    if (inherits(betaT,'try-error')) betaT <- MASS::ginv(t(XT)%*%(W_T*XT)) %*% t(XT)%*%(W_T*yT)",
  "  } else {",
  "    betaT <- rep(0, K-1)",
  "  }",
  "  dT_hat <- c(0, as.numeric(betaT))",
  "  sX  <-  X %*% betaS",
  "  sY  <- ifelse(obsT, (XT %*% betaT), NA_real_)",
  "  idx <- which(obsT)",
  "  if (length(idx) < 3) warning('Few T-observed rows; frequentist surrogacy uncertain.')",
  "  ratio <- mean(vT[is.finite(vT)], na.rm=TRUE) / mean(vS[is.finite(vS)], na.rm=TRUE)",
  "  deming <- function(x,y, lambda){",
  "    xbar <- mean(x); ybar <- mean(y)",
  "    sxx <- stats::var(x); syy <- stats::var(y); sxy <- stats::cov(x,y)",
  "    beta <- (syy - lambda*sxx + sqrt((syy - lambda*sxx)^2 + 4*lambda*sxy^2))/(2*sxy)",
  "    alpha <- ybar - beta*xbar",
  "    c(alpha=alpha, beta=beta)",
  "  }",
  "  ab <- if (sum(is.finite(sX[idx]) & is.finite(sY)) >= 3) deming(sX[idx], sY, ratio) else c(alpha=0,beta=1)",
  "  B <- as.integer(B)",
  "  draws_T <- matrix(NA_real_, B, K)",
  "  for (b in 1:B) {",
  "    eS <- stats::rnorm(N, 0, sqrt(vS))",
  "    eT <- stats::rnorm(sum(obsT), 0, sqrt(vT))",
  "    if (boot=='student') {",
  "      eS <- eS * sqrt(df / stats::rchisq(N, df))",
  "      eT <- eT * sqrt(df / stats::rchisq(sum(obsT), df))",
  "    }",
  "    yS_b <- yS + eS",
  "    yT_b <- net$T_eff[obsT] + eT",
  "    betaS_b <- try(solve(t(X)%*%(W_S*X), t(X)%*%(W_S*yS_b)), silent=TRUE)",
  "    if (inherits(betaS_b,'try-error')) betaS_b <- MASS::ginv(t(X)%*%(W_S*X)) %*% t(X)%*%(W_S*yS_b)",
  "    dS_b <- c(0, as.numeric(betaS_b))",
  "    XT <- X[obsT,,drop=FALSE]",
  "    betaT_b <- try(solve(t(XT)%*%(W_T*XT), t(XT)%*%(W_T*yT_b)), silent=TRUE)",
  "    if (inherits(betaT_b,'try-error')) betaT_b <- MASS::ginv(t(XT)%*%(W_T*XT)) %*% t(XT)%*%(W_T*yT_b)",
  "    dT_b <- c(0, as.numeric(betaT_b))",
  "    dT_sur <- (ab['alpha'] + ab['beta'] * dS_b)",
  "    draws_T[b, ] <- dT_sur",
  "  }",
  "  ranks <- rank_from_draws(draws_T)",
  "  out <- list(",
  "    engine = 'freq',",
  "    net = net,",
  "    dS = dS_hat, dT = dT_hat,",
  "    deming = ab,",
  "    draws_T = draws_T,",
  "    ranks = ranks,",
  "    sucra = sucra(ranks),",
  "    poth = poth(ranks),",
  "    mid = if (!is.null(mid)) mid else NA_real_",
  "  )",
  "  class(out) <- 'surro_fit'",
  "  out",
  "}"
)

gui_clean <- c(
  "#' @export",
  "surroNMA_gui_gw <- function() {",
  "  if (!.suro_require(c('gWidgets2','gWidgets2RGtk2'))) {",
  "    message('gWidgets2/RGtk2 not found; falling back to Tcl/Tk basic GUI.')",
  "    if (exists('surroNMA_gui_tcltk', mode = 'function')) return(surroNMA_gui_tcltk())",
  "    message('Basic Tcl/Tk GUI not available.'); return(invisible(NULL))",
  "  }",
  "  options(guiToolkit = 'RGtk2')",
  "  library(gWidgets2)",
  "  w <- gwindow('surroNMA v1.0', visible = TRUE)",
  "  nb <- gnotebook(container = w)",
  "  pg_data <- ggroup(container = nb, label = 'Data')",
  "  fl <- gframe('Load CSV', container = pg_data)",
  "  f_file <- gfilebrowse(text = 'Choose CSV', type = 'open', container = fl)",
  "  gl <- gformlayout(container = pg_data)",
  "  e_study <- gedit('', label='study', container = gl)",
  "  e_trt   <- gedit('', label='trt',   container = gl)",
  "  e_comp  <- gedit('', label='comp',  container = gl)",
  "  e_S     <- gedit('', label='S_eff (or leave blank)', container = gl)",
  "  e_Sse   <- gedit('', label='S_se', container = gl)",
  "  e_T     <- gedit('', label='T_eff', container = gl)",
  "  e_Tse   <- gedit('', label='T_se', container = gl)",
  "  e_Smulti<- gedit('', label='S_multi (comma-separated)', container = gl)",
  "  e_class <- gedit('', label='class (optional)', container = gl)",
  "  e_corr  <- gedit('', label='corr_ST (optional)', container = gl)",
  "  e_base  <- gedit('', label='baseline_risk (optional)', container = gl)",
  "  e_rob   <- gedit('', label='rob (optional)', container = gl)",
  "  btn_load <- gbutton('Build network', container = pg_data)",
  "  net_env <- new.env(parent=emptyenv())",
  "  col_val <- function(x) { z <- svalue(x); if (nzchar(z)) z else NULL }",
  "  addHandlerChanged(btn_load, handler = function(h,...) {",
  "    fp <- svalue(f_file); if (!nzchar(fp)) { gmessage('Choose a CSV file'); return() }",
  "    df <- try(utils::read.csv(fp, check.names=FALSE), silent=TRUE)",
  "    if (inherits(df,'try-error')) { gmessage('Could not read CSV'); return() }",
  "    Sm <- col_val(e_Smulti)",
  "    if (!is.null(Sm)) Sm <- trimws(strsplit(Sm, ',', fixed = TRUE)[[1]])",
  "    net <- try(surro_network(",
  "      data=df,",
  "      study = as.name(col_val(e_study)), trt=as.name(col_val(e_trt)), comp=as.name(col_val(e_comp)),",
  "      S_eff = if (!is.null(col_val(e_S)))   as.name(col_val(e_S))   else NULL,",
  "      S_se  = if (!is.null(col_val(e_Sse))) as.name(col_val(e_Sse)) else NULL,",
  "      T_eff = if (!is.null(col_val(e_T)))   as.name(col_val(e_T))   else NULL,",
  "      T_se  = if (!is.null(col_val(e_Tse))) as.name(col_val(e_Tse)) else NULL,",
  "      S_multi = Sm,",
  "      class = if (!is.null(col_val(e_class))) as.name(col_val(e_class)) else NULL,",
  "      corr_ST = if (!is.null(col_val(e_corr)))  as.name(col_val(e_corr))  else NULL,",
  "      baseline_risk = if (!is.null(col_val(e_base))) as.name(col_val(e_base)) else NULL,",
  "      rob = if (!is.null(col_val(e_rob))) as.name(col_val(e_rob)) else NULL",
  "    ), silent=TRUE)",
  "    if (inherits(net,'try-error')) { gmessage(paste('Network build error:', as.character(net))); return() }",
  "    net_env$net <- net",
  "    gmessage(paste0('Network built with treatments: ', paste(net$trt_levels, collapse=', ')))",
  "  })",
  "  invisible(list(window=w, env=net_env))",
  "}"
)

# ---------- Apply replacements ----------
x <- read_lines()

# 1) poth()
x2 <- replace_block(x,
                    start_pat = "^\\s*poth\\s*<-\\s*function\\s*\\(",
                    new_text  = poth_clean,
                    end_pat   = "^\\s*#'\\s*@export\\b|^\\s*mid_adjusted_preference\\s*<-\\s*function\\s*\\("
)
if (!is.null(x2)) { x <- x2; message("Patched: poth()") }

# 2) .stan_code_biv() (replace up to the Stan data section marker)
x2 <- replace_block(x,
                    start_pat = "^\\s*\\.stan_code_biv\\s*<-\\s*function\\s*\\(",
                    new_text  = stan_code_biv_clean,
                    end_after_marker = "^\\s*#\\s*-+\\s*Stan data\\s*-+\\s*$"
)
if (!is.null(x2)) { x <- x2; message("Patched: .stan_code_biv()") }

# 3) surro_nma_freq() (replace up to Unified front-end marker)
x2 <- replace_block(x,
                    start_pat = "^\\s*surro_nma_freq\\s*<-\\s*function\\s*\\(",
                    new_text  = surro_nma_freq_clean,
                    end_after_marker = "^\\s*#\\s*-+\\s*Unified front-end\\s*-+\\s*$"
)
if (!is.null(x2)) { x <- x2; message("Patched: surro_nma_freq()") }

# 4) surroNMA_gui_gw() (replace to end-of-file or next dashed section)
x2 <- replace_block(x,
                    start_pat = "^\\s*surroNMA_gui_gw\\s*<-\\s*function\\s*\\(",
                    new_text  = gui_clean,
                    end_pat   = "^\\s*#\\s*-{3,,}\\s*|^\\s*$"  # stop at next big dashed comment or EOF
)
if (!is.null(x2)) { x <- x2; message("Patched: surroNMA_gui_gw()") }

# 5) Remove noisy AUTO-FIX comment artifacts (safe)
x <- x[!grepl("^\\s*#\\s*AUTO-FIX", x)]

# 6) Normalize whitespace & ensure final newline
x <- sub("[ \t]+$", "", x, perl = TRUE)
x <- c(x, "")

write_lines(x)

# 7) Parse check
pe <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
if (inherits(pe, "try-error")) {
  msg <- conditionMessage(attr(pe, "condition"))
  message("✗ Still does not parse:\n", msg)
  message("Backup is at: ", backup)
  quit(save = "no", status = 1)
} else {
  message("✓ R/surroNMA.R parses after patches.")
}

# 8) (Optional) Regenerate docs/namespace
if (requireNamespace("devtools", quietly = TRUE) &&
    requireNamespace("roxygen2", quietly = TRUE)) {
  message("Regenerating NAMESPACE & Rd via roxygen2…")
  suppressPackageStartupMessages(devtools::document(roclets = c("rd", "namespace")))
} else {
  message("(devtools/roxygen2 not both available) — skipping document().")
}

# 9) Quick smoke-load (optional)
if (requireNamespace("devtools", quietly = TRUE)) {
  message("Loading package with devtools::load_all()…")
  suppressMessages(devtools::load_all(quiet = TRUE))
  needed <- c("surro_network","surro_nma","as_draws_T","summarize_treatments")
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(missing)) {
    message("⚠ Missing in namespace: ", paste(missing, collapse = ", "))
  } else {
    message("✓ Core functions present: ", paste(needed, collapse = ", "))
  }
}

message("=== Patch complete. You can now run devtools::check(). ===")
