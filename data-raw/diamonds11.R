set.seed(42)

library(ggplot2)
data(diamonds)
View(diamonds)

diamonds$area <- sample(1:5, nrow(diamonds), replace = TRUE)

diamonds11 <- diamonds
diamonds11$cut <- factor(diamonds11$cut, ordered = TRUE)
diamonds11$color <- factor(diamonds11$color, ordered = TRUE)
diamonds11$clarity <- factor(diamonds11$clarity, ordered = TRUE)

usethis::use_data(diamonds11)

