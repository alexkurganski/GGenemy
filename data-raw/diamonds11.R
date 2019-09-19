set.seed(42)

library(ggplot2)
data(diamonds)
View(diamonds)

diamonds$area <- sample(1:5, nrow(diamonds), replace = TRUE)
str(diamonds)

diamonds11 <- diamonds

usethis::use_data(diamonds11)

