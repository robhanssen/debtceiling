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

modelconstant <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% tidy())) %>% unnest(modelq) %>%
        filter(term == "date") %>%
        pull(estimate)

pct_increase <- scales::percent(10 ^ (modelconstant * 86400 *365) - 1, accuracy = .1)

ten_years = 86400 * 365 * 10

predictions <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel,
                           function(x) x %>% augment(interval = "confidence", newdata = tibble(date = seq(cutoff_date_lo, cutoff_date_hi + .4 * ten_years, ten_years/100))))) %>%
        unnest(modelq) %>%
        mutate(across(.cols = .fitted:.upper, .fns =~10^.x))

gdp_max_pred <- predictions %>% filter(date < as_datetime("2019-11-10")) %>% slice_max(date, n = 1) %>% select(.fitted) %>% pull(.fitted)

gdp_max_real <- gdp %>% filter(floordate < as_datetime("2019-11-05")) %>% slice_max(floordate, n = 1) %>% pull(gdp)

real_growth <- scales::dollar(-gdp_max_pred + gdp_max_real, accuracy = 1, suffix = "B")
real_growth_per <- scales::percent((-gdp_max_pred + gdp_max_real)/gdp_max_real, accuracy = .01)

comment <- paste0("Difference between Obama model\nand outcome : ",
                  real_growth,
                  " (", real_growth_per,") ",
                  "over 3 years")

predictions %>%
        ggplot + 
        aes(x = date, y = .fitted) + 
        geom_line(lty = 2) + 
        geom_point(data = gdp %>% filter(floordate > cutoff_date_lo), aes(x = floordate, y = gdp)) + 
        #scale_x_date(limits = c(cutoff_date_lo, NA)) + 
        scale_y_continuous(limit = c(14000, NA)) +
        geom_line(aes(y = .lower), color = "gray50", lty = 1) + 
        geom_line(aes(y = .upper), color = "gray50", lty = 1) +
        geom_vline(xintercept = as_datetime("2017-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2021-01-21"), lty = 2, color = "gray50") + 
        #geom_vline(xintercept = as_datetime("2019-10-05"), lty = 2, color = "red") + 
        geom_segment(aes(x = as_datetime("2019-10-05"), xend = as_datetime("2019-10-05"), y = 23000, yend = 21000), color = "red", lty = 2)+
        geom_segment(aes(x = cutoff_date_lo, xend = cutoff_date_hi, y = 14001, yend = 14001), color = "red") +
        geom_segment(aes(x = cutoff_date_hi, xend = cutoff_date_hi + .31 * ten_years, y = 14001, yend = 14001), color = "darkgreen") +
        annotate("label", x = as_datetime("2018-06-01"), y = 23000, label = comment) +
        annotate("label", x = as_datetime("2014-11-01"), y = 18000, label = pct_increase) +
        labs(x = "Date", 
             y = "GDP in (billions of $)",
             caption = "Model trained between 2009 and 2017 and predicted into 2020")

ggsave("graphs/real-growth.png", width = 6, height = 6)