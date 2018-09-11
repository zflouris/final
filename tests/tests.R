context("Testing the functions")


test_that("is a data frame", {
  expect_is(eq_clean_data(data), "data.frame")})



test_that("coordinates are numeric", {
  expect_is(eq_clean_data(data)$LATITUDE, "numeric")
  expect_is(eq_clean_data(data)$LONGITUDE, "numeric")})

test_that("eq_location_clean returns a data frame", {
  expect_is(eq_location_clean(data), "data.frame")
})

test_that("eq_create_label returns character vector", {
  expect_is(eq_create_label(data), "character")
})

test_that("eq_create_label has the same length as input df", {
  expect_equal(length(eq_create_label(data)), dim(data)[1])
})
