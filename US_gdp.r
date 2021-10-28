source("std/stdpackages.r")
source("std/stdfunctions.r")

gdp <- read_csv("sources/gdp.csv") %>%
        janitor::clean_names() %>%
        mutate(date = as_datetime(date)) %>%
        rename(floordate = "date")

presidentinfo <-
        presidentinfo %>% 
        mutate(date = as_datetime(inaugdate)) %>%
        mutate(date = ceiling_date(date, "quarter")) %>%
        rename(floordate = "date")


gdp_plot <-
    gdp %>%
        mutate(floordate = floor_date(floordate, unit = "quarter")) %>%
        # mutate(date = as_datetime(date),
        #         floordate = as_datetime(floordate)) %>%
        # filter(!is.na(debt)) %>%
        # inner_join(gdp) %>%
        # mutate(gdpratio = gfdebtn / (1000*gdp)) %>%
        left_join(presidentinfo) %>% 
        fill(party, .direction = "down")

gdp_plot %>%
    filter(floordate > as_datetime("1979-12-31")) %>%
    ggplot() +
    aes(floordate, gdp, color = party, group = FALSE) +
    geom_line() +
    scale_y_log10(
                        limits = c(3000, NA),
                        labels = scales::dollar_format()) +
    geom_vline(xintercept = inaugdates, lty = 2, alpha = .3) +
    theme_light() +
    labs(x = "Date",
         y = "ratio Public Debt / GDP",
         caption = "Dashed vertical line indicate inauguration dates",
         title = "Ratio of Public Debt to GDP since 1980") +
    scale_color_manual(values = partycolor) + 
    theme(legend.position = "none")

# ggsave("graphs/debt-to-gdp-ratio-since-1980.png", width = 6, height = 6)