# Baseline probe for NMA (surroNMA package).
#
# Deterministic Network Meta-Analysis signals on a fixed simulated network.
# Outputs a JSON object of stable values for Overmind's NumericalWitness.
#
# Run: Rscript probe.R

suppressPackageStartupMessages({
  if (!"surroNMA" %in% loadedNamespaces()) {
    pkgload::load_all(rprojroot::find_package_root_file(), quiet = TRUE)
  }
})

# Fixed deterministic input via simulate_surro_data with pinned seed.
df <- simulate_surro_data(K = 4, J = 6, per_study = 1, seed = 42)
net <- surro_network(df, study = "study", trt = "trt", comp = "comp",
                     S_eff = "logHR_S", S_se = "se_S",
                     T_eff = "logHR_T", T_se = "se_T", class = "class")

# Extract stable structural and pooled-effect signals on S (fully populated).
# T outcomes can be NA in this simulator, so we summarise the surrogate only.
out <- list(
  K = net$K,
  J = nrow(df),
  n_treatments = length(unique(c(df$trt, df$comp))),
  S_mean = round(mean(df$logHR_S), 6),
  S_sd   = round(sd(df$logHR_S),   6),
  S_min  = round(min(df$logHR_S),  6),
  S_max  = round(max(df$logHR_S),  6),
  se_S_mean = round(mean(df$se_S), 6),
  T_observed = sum(!is.na(df$logHR_T))
)

cat(jsonlite::toJSON(out, auto_unbox = TRUE, digits = 6))
cat("\n")
