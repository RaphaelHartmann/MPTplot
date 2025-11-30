test_that("splitting trees in categories", {
  lines <- c("(1-K)*F+(1-K)*(1-F)*g", "K", "(1-K)*(1-F)*(1-g)", "", "(1-K)*(1-F)*g", "K+(1-K)*F", "(1-K)*(1-F)*(1-g)")
  tree_list <- list(categories = list(c("(1-K)*F+(1-K)*(1-F)*g", "K", "(1-K)*(1-F)*(1-g)"),
                                      c("(1-K)*(1-F)*g", "K+(1-K)*F", "(1-K)*(1-F)*(1-g)")))
  expect_identical(get_categories(lines), tree_list)
})
