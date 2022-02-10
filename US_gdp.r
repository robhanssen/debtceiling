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
         y = "US GDP",
         caption = "Dashed vertical line indicate inauguration dates",
         title = "US GDP since 1980") +
    scale_color_manual(values = partycolor) + 
    theme(legend.position = "none")

 ggsave("graphs/us-gdp-since-1980.png", width = 6, height = 6)

 #
 # Obama to Trump transition
 #
cutoff_date_lo <- as_datetime("2009-09-01")
cutoff_date_hi <- as_datetime("2017-01-21")

gdpmodel <-
        gdp %>% 
        ungroup() %>%
        rename(date = "floordate") %>%
        filter(date > cutoff_date_lo, date < cutoff_date_hi) %>%
        nest() %>%
        mutate(gdpmodel = map(data, function(tbl) lm(log10(gdp) ~ date, data = tbl)))

gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% glance())) %>% unnest(modelq)

ten_years = 86400 * 365 * 10

gdpmodel %>%
        mutate(modelq = map(gdpmodel,
                           function(x) x %>% augment(newdata = tibble(date = seq(cutoff_date_lo, cutoff_date_lo + ten_years, ten_years/100))))) %>%
        unnest(modelq) %>%
        #mutate(date = as_datetime(date)) %>%
        ggplot + 
        aes(x = date, y = 10^.fitted) + 
        geom_line(lty = 2) + 
        geom_point(data = gdp %>% filter(floordate > cutoff_date_lo), aes(x = floordate, y = gdp)) + 
        #scale_x_date(limits = c(cutoff_date_lo, NA)) + 
        scale_y_log10(limit = c(14000, NA)) + 
        geom_vline(xintercept = as_datetime("2017-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2021-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2018-04-01"), lty = 2, color = "red") + 
        labs(x = "Date", 
             y = "GDP in ($)",
             caption = "Model trained between 2009 and 2017 and predicted into 2020")


