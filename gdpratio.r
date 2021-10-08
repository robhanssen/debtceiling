library(tidyverse)
library(lubridate)

inaugdates = sort(as_datetime(as.Date(c("1900-01-01","2100-01-01","2009-01-20","2017-01-21","2001-01-21","1993-01-21", "1989-01-21", "1981-01-21"))))
presidents = c("Before 1980", "Reagan", "Bush Sr", "Clinton", "Bush Jr", "Obama", "Trump" )


fedrate <- read_csv("fedrate.csv") %>%
        janitor::clean_names() %>%
        mutate(date = as_datetime(date)) %>%
        rename(floordate = "date")

gdp <- read_csv("gdp.csv") %>%
        janitor::clean_names() %>%
        mutate(date = as_datetime(date)) %>%
        rename(floordate = "date")

debt <- read_csv("debtceiling.csv") %>%
        janitor::clean_names() %>%
        select(date, debt_ceiling) %>%
        mutate(date = parse_date(.$date, "%B %d, %Y")) %>%
        mutate(floordate = floor_date(date, unit = "quarter")) %>%
        mutate(date = as_datetime(date),
                floordate = as_datetime(floordate)) %>%
        filter(!is.na(debt_ceiling)) %>%
        mutate(president = cut(date, inaugdates, presidents)) %>%
        inner_join(gdp) %>% 
        mutate(gdpratio = debt_ceiling / gdp)


debt %>% 
    filter(date > as_datetime("1979-12-31")) %>%
    ggplot() + 
    aes(date, gdpratio) +
    geom_point() +
    scale_y_continuous(breaks = .2 * 0:100, limits = c(0, NA), labels = scales::percent_format()) + 
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() + 
    labs(x = "Date", y = "ratio Debt Ceiling / GDP", caption = "Inauguration dates", 
         title = "Ratio of Debt ceiling to GDP since 1980") 

ggsave("debtceiling-gdp-ratio-since-1980.png", width = 6, height = 6)