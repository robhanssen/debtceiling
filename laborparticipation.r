library(tidyverse)
library(lubridate)
theme_set(theme_light())

source <- "https://fred.stlouisfed.org/series/CIVPART"

cutoff_date <- as.Date("2000-01-01")

presidents <-
    read_csv("sources/presidents.csv") %>%
    mutate(date = lubridate::floor_date(inaugdate, unit = "month"))

labor <-
    read_csv("sources/civpart.csv") %>%
    janitor::clean_names() %>%
    left_join(presidents) %>%
    fill(c(party, president), .direction = "down") %>%
    select(-inaugdate)

lastdate <- format(max(labor$date), format = "%b %d, %Y")

labor %>%
    filter(date >= cutoff_date) %>%
    ggplot() +
    aes(x = date, y = civpart, color = party, group = TRUE) +
    geom_line() +
    labs(
        x = "Date",
        y = "Labor participartion rate (in %)",
        color = "President affiliation",
        title = "Labor participation",
        caption = paste0("Source: ",
                          source,
                          "(until ",
                         lastdate,
                         ")")
    ) +
    scale_color_manual(values = c("D" = "blue", "R" = "red")) +
    geom_vline(xintercept = presidents$inaugdate, lty = 3, color = "gray50")

ggsave("graphs/laborparticipation.png", width = 8, height = 6)