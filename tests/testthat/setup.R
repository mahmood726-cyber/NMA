# testthat auto-runs setup*.R before any test_that() block.
# Use pkgload::load_all() so the package's R/ files are visible without
# requiring R CMD INSTALL — keeps the test command portable to plain
# `Rscript -e "testthat::test_dir('tests/testthat')"` invocations
# (e.g. Overmind's nightly verifier) and `Rscript tests/testthat.R`
# (the standard R-package entry point) alike.
if (!"surroNMA" %in% loadedNamespaces()) {
  pkgload::load_all(rprojroot::find_package_root_file(), quiet = TRUE)
}
