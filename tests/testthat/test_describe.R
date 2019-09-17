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
    expect_true(is.numeric(iris[, names(desc$Numeric[i])]))
  }
  
  for(i in 1:length(desc[[2]])){
    expect_true(is.factor(iris[, names(desc$Factor[i])]))
  }
  
})


test_that("describe calculates the correct summary statistics for numeric
          variables", {
  data(iris)
  desc <- describe(iris)
  
  for(i in 1:length(desc[[1]])){
    expect_equal(desc[[1]][[i]][[1]], mean(iris[, i]))
  }

  for(i in 1:length(desc[[1]])){
    expect_equal(desc[[1]][[i]][[2]], median(iris[, i]))
  }
  
  for(i in 1:length(desc[[1]])){
    expect_equal(desc[[1]][[i]][[3]], var(iris[, i]))
  }

  for(i in 1:length(desc[[1]])){
    expect_equal(desc[[1]][[i]][[4]], sd(iris[, i]))
  }

  for(i in 1:length(desc[[1]])){
    expect_equal(desc[[1]][[i]][[5]], valid.n(iris[, i]))
  }
  
})

test_that("describe calculates the correct statistics for factors", {
  data(iris)
  desc <- describe(iris)
  ind1 <- c(1,3,5)
  ind2 <- c(2,4,6)
  
  for(i in ind1){
    expect_equal(desc[[2]][[1]][[i]], 50)
  }
  
  for(i in ind2){
    expect_equal(desc[[2]][[1]][[i]], (50/150)*100)
  }
})
