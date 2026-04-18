# surroNMA

Advanced surrogate-assisted network meta-analysis, packaged from a single-file script.

## Install (local)

```r
# From the zip you downloaded/unpacked:
install.packages("surroNMA", repos = NULL, type = "source")
```

## Quick start

```r
library(surroNMA)
set.seed(1)
df <- simulate_surro_data()
net <- surro_network(df, study=study, trt=trt, comp=comp,
                     S_eff=logHR_S, S_se=se_S, T_eff=logHR_T, T_se=se_T, class=class)
fit <- surro_nma(net, engine="freq", B=50)
print(explain(fit))
```

### CmdStan (optional, for Bayesian)

```r
help_cmdstan_setup()
```
