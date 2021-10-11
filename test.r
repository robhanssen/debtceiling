library(tidyverse)
library(broom)

t <- tibble(date = as_datetime(c("2021-01-01", "2021-12-31")), x = c(0,1))

lmmod <- 
    lm(x ~ date, data = t)

summary(lmmod)

lmmod %>% tidy() %>% mutate(est = estimate * 365*24*3600)