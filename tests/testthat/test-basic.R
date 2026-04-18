test_that('simulate_surro_data produces expected structure', {
  df <- simulate_surro_data(K=4, J=6, per_study=1, seed=1)
  expect_true(all(c('study','trt','comp','logHR_S','se_S','class') %in% names(df)))
})

test_that('frequentist pipeline runs', {
  df <- simulate_surro_data(K=5, J=10, per_study=1, seed=2)
  net <- surro_network(df, study="study", trt="trt", comp="comp",
                       S_eff="logHR_S", S_se="se_S",
                       T_eff="logHR_T", T_se="se_T", class="class")
  expect_type(net, "list")
  expect_equal(net$K, 5)
})
