#test_that("describe needs a valid input", {
  #expect_error(describe( ,num.desc = c("mean", "median", "var", "sd", "valid.n"),
                        #xname = NA),
               #"Usage: describe(x,...) where x is a vector, data frame or matrix")
#})

test_that("describe automatically creates varnames if none are given", {
  data(iris)
  names(iris) <- NULL
  desc <- describe(iris)
  expect_identical(names(desc[[1]]), c("V1", "V2", "V3", "V4"))
})


test_that("describe correctly separates numerical variables and factors", {
  data(iris)
  desc <- describe(iris)
  
  for(i in 1:length(desc[[1]])){
    expect_true(is.numeric(iris[, names(desc[[1]][i])]))
  }
  
  for(i in 1:length(desc[[2]])){
    expect_true(is.factor(iris[, names(desc[[2]][i])]))
  }
  
})


#test_that("describe calculates the correct summary statistics", {
  
#})
