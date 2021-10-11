source("std/stdpackages.r")
source("std/stdfunctions.r")

fedrate <- read_csv("sources/fedrate.csv") %>%
        janitor::clean_names() %>%
        mutate(date = as_datetime(date)) %>%
        rename(floordate = "date")

gdp <- read_csv("sources/gdp.csv") %>%
        janitor::clean_names() %>%
        mutate(date = as_datetime(date)) %>%
        rename(floordate = "date")

gdp %>%
        filter(floordate > as_datetime("1979-12-31")) %>%
        ggplot() +
        aes(floordate, gdp) +
        scale_y_log10(labels = scales::dollar_format(scale = 1 / 1000, suffix = "T"),
                        breaks = rep(c(1, 2, 5), times = 3) * rep(10 ^ (3:5), each = 3)) +
        geom_line() +
        theme_light() +
        geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
        labs(x = "Date", y = "US GDP in ($T)", title = "US GDP since 1980")

ggsave("graphs/us-gsp-since-1980.png", width = 6, height = 6)

# small fudge to cram in the last date point. Assumption is GDP growth of 500 B
gdpadd <- tibble(floordate = as_datetime("2021-07-01"),
                 gdp = last(gdp$gdp) + 500)

gdp <- bind_rows(gdp, gdpadd)

debt <-
    debt %>%
        mutate(floordate = floor_date(date, unit = "quarter")) %>%
        mutate(date = as_datetime(date),
                floordate = as_datetime(floordate)) %>%
        filter(!is.na(debt_ceiling)) %>%
        inner_join(gdp) %>%
        mutate(gdpratio = debt_ceiling / gdp)

debt %>%
    filter(date > as_datetime("1979-12-31")) %>%
    ggplot() +
    aes(date, gdpratio) +
    geom_point() +
    scale_y_continuous(breaks = .2 * 0:100,
                        limits = c(0, NA),
                        labels = scales::percent_format()) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(x = "Date",
         y = "ratio Debt Ceiling / GDP",
         caption = "Inauguration dates",
         title = "Ratio of Debt ceiling to GDP since 1980")

ggsave("graphs/debtceiling-gdp-ratio-since-1980.png", width = 6, height = 6)