car_data <- 
  mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  print


## https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

## https://dplyr.tidyverse.org/ 

## https://ggplot2.tidyverse.org/

