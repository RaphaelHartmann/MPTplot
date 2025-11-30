test_that("read_lines gives lines of mdl file back", {
  mdl <- system.file("extdata", "mdl", package = "MPTplot")
  expect_identical(read_lines(mdl), c("(1-K)*F+(1-K)*(1-F)*g", "K",
                                      "(1-K)*(1-F)*(1-g)", "", "(1-K)*(1-F)*g",
                                      "K+(1-K)*F", "(1-K)*(1-F)*(1-g)"))
})

test_that("read_lines gives lines of mdl equation character back", {
  mdl <- mdl_object
  expect_identical(read_lines(mdl), c("(1-K)*F+(1-K)*(1-F)*g", "K",
                                      "(1-K)*(1-F)*(1-g)", "", "(1-K)*(1-F)*g",
                                      "K+(1-K)*F", "(1-K)*(1-F)*(1-g)"))
})
