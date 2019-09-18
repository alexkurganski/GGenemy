test_that("the output of 'plot_GGenemy() is a ggplot object", {
   data("iris")
   obj <- plot_GGenemy(iris, "Sepal.Length", "Sepal.Width")

   
   expect_is(obj, "gg")
   expect_is(obj, "ggplot")
})


test_that("plot_GGenemy() creates the correct types of plots for a given numeric
          variable", {
  data("iris")
  iris$Place <- as.factor(sample(1:5, 150, replace = TRUE))
  obj <- plot_GGenemy(iris, "Sepal.Length", c("Sepal.Width","Species"))
  
  expect_is(obj[[1]]$layers[[1]]$geom, "GeomDensity")
  expect_is(obj[[2]]$layers[[1]]$geom, "GeomBoxplot")
})

test_that("plot_GGenemy() creates the correct types of plots for a given factor", {
  data("iris")
  iris$Place <- as.factor(sample(1:5, 150, replace = TRUE))
  obj <- plot_GGenemy(iris, "Species", c("Sepal.Length","Place"))
  
  expect_is(obj[[1]]$layers[[1]]$geom, "GeomDensity")
  expect_is(obj[[2]]$layers[[1]]$geom, "GeomBar")
})

test_that("plot_GGenemy() plots the data correctly", {
  data("iris")
  iris$Place <- as.factor(sample(1:5, 150, replace = TRUE))
  obj <- plot_GGenemy(iris, "Species", "Place")
  tab <- table(iris$Species, iris$Place)
  tab2 <- rbind(tab[3,], tab[2,], tab[1,])
  tab3 <- c(tab2[,1], tab2[,2],tab2[,3],tab2[,4],tab2[,5])
  
  expect_equal(ggplot2::ggplot_build(obj)$data[[1]]$count, 
               tab3)
})

# build <- ggplot2::ggplot_build(obj)
# build
# tab <- table(iris$Species, iris$Place)
# tab2 <- rbind(tab[3,], tab[2,], tab[1,])
# tab3 <- c(tab2[,1], tab2[,2],tab2[,3],tab2[,4],tab2[,5])

test_that("plot_sum_stats() returns a list of class arrangelist for every chosen
          summary statistic", {
  data("iris")
  obj <- plot_sum_stats(iris, "Sepal.Length", c(1,2))
  
  expect_is(obj, "list")
  expect_is(obj[[1]], "arrangelist")
  expect_is(obj[[2]], "arrangelist")
})
