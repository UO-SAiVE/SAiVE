test_that("ranger model and default params returns no error", {
  expect_no_error(modelMatch("ranger"))
})

test_that("ranger model and default params returns expected columns", {
  expect_named(modelMatch("ranger"), c("Similarity to Random Forest (ranger)", "Model Abbreviation"))
})

test_that("ranger model and default params returns 11 rows", {
 expect_length(modelMatch("ranger")$`Model Abbreviation`, 11)
})
