context("processing")

setwd("../..")

test_that("test data loaded in", {
  expect_that(is.null(bugdata), is_false())
})

bugdata1 <- bugdata[, -4]
bugdata2 <- bugdata[, -5]
bugdata3 <- bugdata[, -6]
bugdata4 <- bugdata[, -7]
bugdata5 <- bugdata[, -8]

bugdata6 <- bugdata
bugdata6$FinalID <- as.character(bugdata$FinalID)
bugdata6[5, "FinalID"] <- "asdf"

test_that("BMI error checking", {
  expect_that(BMI(bugdata1), throws_error())
  expect_that(BMI(bugdata2), throws_error())
  expect_that(BMI(bugdata3), throws_error())
  expect_that(BMI(bugdata4), throws_error())
  expect_that(nrow(BMI(bugdata5))>0, is_true())
  expect_that(BMI(bugdata1), throws_error())
})

bugdata.BMI <- BMI(bugdata)
bugdata.BMI1 <- bugdata.BMI
bugdata.BMI1$DistinctCode <- "Yes"


test_that("BMI aggregate", {
  expect_that(nrow(aggregate(bugdata.BMI))>0, is_true())
  expect_that(class(aggregate(bugdata.BMI))[1] =="BMIagg", is_true())
  expect_that(nrow(aggregate(bugdata.BMI1))>0, is_true())
})