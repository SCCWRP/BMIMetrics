context("metadata")

test_that("Metadata loaded properly", {
  expect_that(is.null(metadata), is_false())
})

test_that("Metadata has proper dimensions", {
  expect_that(nrow(metadata), equals(8627))
  expect_that(ncol(metadata), equals(34))
})