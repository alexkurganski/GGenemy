test_that("sum_stats only allows correct inputs for n_stats and n_quantiles", {
  utils::data("iris")

  expect_error(
    sum_stats(iris, "Sepal.Length", stats = 4, n_quantiles = 15),
    "You can only partition your given_var into one to ten quantiles."
  )
})


test_that("sum_stats automatically removes factors and logicals from the dataset", {
  utils::data("iris")

  expect_message(
    sum_stats(iris, "Sepal.Length", stats = 4, n_quantiles = 5),
    "Factors have been removed."
  )

  iris <- iris[, -5]
  iris$logic <- rep(TRUE, 150)
  expect_message(
    sum_stats(iris, "Sepal.Length", stats = 4, n_quantiles = 5),
    "Logicals have been removed."
  )
})


test_that("sum_stats calculated the unconditional moments if n_quantiles = 1", {
  utils::data("iris")
  iris <- iris[, -5]

  expect_equal(
    sum_stats(iris, "Sepal.Length", stats = 1, n_quantiles = 1)[[1]][1],
    mean(iris$Sepal.Length)
  )
  expect_equal(
    sum_stats(iris, "Sepal.Length", stats = 1, n_quantiles = 1)[[1]][2],
    mean(iris$Sepal.Width)
  )
  expect_equal(
    sum_stats(iris, "Sepal.Length", stats = 2, n_quantiles = 1)[[1]][1],
    var(iris$Sepal.Length)
  )
  expect_equal(
    sum_stats(iris, "Sepal.Length", stats = 3, n_quantiles = 1)[[1]][1],
    moments::skewness(iris$Sepal.Length)
  )
  expect_equal(
    sum_stats(iris, "Sepal.Length", stats = 4, n_quantiles = 1)[[1]][1],
    moments::kurtosis(iris$Sepal.Length)
  )
})
