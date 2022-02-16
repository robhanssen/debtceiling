library(patchwork)
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

gdp_allyears <-
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

 ggsave("graphs/us-gdp-since-1980.png", width = 6, height = 6, plot = gdp_allyears)

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

rsq <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% glance())) %>% unnest(modelq) %>% pull(r.squared)

modelconstant <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% tidy())) %>% unnest(modelq) %>%
        filter(term == "date") %>%
        pull(estimate)

pct_increase <- scales::percent(10 ^ (modelconstant * 86400 *365) - 1, accuracy = .1)

ten_years = 86400 * 365 * 10

gdp_predict_range <-
        gdp %>% 
        ungroup() %>%
        rename(date = "floordate") %>%
        filter(date > cutoff_date_lo) %>%
        select(date)



predictions <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel,
                           function(x) x %>% augment(interval = "confidence", newdata = gdp_predict_range))) %>%
        unnest(modelq) %>%
        mutate(across(.cols = .fitted:.upper, .fns =~10^.x))

gdp_max_pred <- predictions %>% filter(date < as_datetime("2019-11-10")) %>% slice_max(date, n = 1) %>% select(.fitted) %>% pull(.fitted)

gdp_max_real <- gdp %>% filter(floordate < as_datetime("2019-11-05")) %>% slice_max(floordate, n = 1) %>% pull(gdp)

real_growth <- scales::dollar(-gdp_max_pred + gdp_max_real, accuracy = 1, suffix = "B")
real_growth_per <- scales::percent((-gdp_max_pred + gdp_max_real)/gdp_max_real, accuracy = .01)

max_date <- max(gdp$floordate)
comment <- paste0("Difference between Obama model\nand outcome : ",
                  real_growth,
                  " (", real_growth_per,")")

gdp_full_scale <-
        predictions %>%
        ggplot + 
        aes(x = date, y = .fitted) + 
        geom_line(lty = 2) + 
        geom_point(data = gdp %>% filter(floordate > cutoff_date_lo), aes(x = floordate, y = gdp)) + 
        #scale_x_date(limits = c(cutoff_date_lo, NA)) + 
        scale_x_datetime(breaks = "2 year", date_label = "%Y") + 
        scale_y_continuous(limit = c(14000, NA)) +
        geom_line(aes(y = .lower), color = "gray50", lty = 1) + 
        geom_line(aes(y = .upper), color = "gray50", lty = 1) +
        geom_vline(xintercept = as_datetime("2017-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2021-01-21"), lty = 2, color = "gray50") + 
        geom_segment(aes(x = cutoff_date_lo, xend = cutoff_date_hi, y = 14001, yend = 14001), color = "red") +
        geom_segment(aes(x = cutoff_date_hi, xend = max_date, y = 14001, yend = 14001), color = "darkgreen") +
        annotate("label", x = as_datetime("2014-06-01"), y = 14000, color = "red", label = "training set") +
        annotate("label", x = as_datetime("2018-09-01"), y = 14000, color = "darkgreen", label = "prediction") +
        # annotate("label", x = as_datetime("2018-06-01"), y = 23000, label = comment) +
        annotate("label", x = as_datetime("2014-11-01"), y = 18000, label = paste0(pct_increase, "\nr.sq = ", scales::pvalue(rsq))) +
        labs(x = "Date", 
             y = "GDP in (billions of $)",
             caption = "Model trained between 2009 and 2017 and predicted into 2021")



#
# difference plot
#

gdp_predict_range <-
        gdp %>% 
        ungroup() %>%
        rename(date = "floordate") %>%
        filter(date > cutoff_date_lo) %>%
        select(date)


predictions_diff <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel,
                           function(x) x %>% augment(interval = "confidence", newdata = gdp_predict_range))) %>%
        unnest(modelq) %>%
        mutate(across(.cols = .fitted:.upper, .fns =~10^.x)) %>%
        inner_join(gdp, by = c("date" = "floordate")) %>%
        mutate(real_growth = gdp - .fitted,
               real_lo = gdp - .lower,
               real_hi = gdp - .upper)

max_date <- max(gdp$floordate)

max_dip <- min(predictions_diff$real_growth)
dip_comment <- paste0("COVID-19 GDP dip\n", scales::dollar(-max_dip, accuracy = 1, suffix = "B"), " below Obama model")

gdp_diff_scale <-
        predictions_diff %>%
        ggplot + 
        aes(x = date, y = real_growth) + 
        geom_point() + 
        # geom_line(lty = 1) + 
        scale_x_datetime(breaks = "2 year", date_label = "%Y") + 
        scale_y_continuous(limits = c(-3000, 1500)) +
        geom_line(aes(y = real_lo), color = "gray70", lty = 1) + 
        geom_line(aes(y = real_hi), color = "gray70", lty = 1) +
        geom_vline(xintercept = as_datetime("2009-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2017-01-21"), lty = 2, color = "gray50") + 
        geom_vline(xintercept = as_datetime("2021-01-21"), lty = 2, color = "gray50") + 
        geom_segment(aes(x = as_datetime("2019-10-05"), xend = as_datetime("2019-10-05"), y = 23000, yend = 21000), color = "red", lty = 2)+
        geom_segment(aes(x = cutoff_date_lo, xend = cutoff_date_hi, y = 0, yend = 0), color = "red") +
        geom_segment(aes(x = cutoff_date_hi, xend = max_date, y = 0, yend = 0), color = "darkgreen") +
        annotate("label", x = as_datetime("2014-06-01"), y = -500, color = "red", label = "training set") +
        annotate("label", x = as_datetime("2018-09-01"), y = -500, color = "darkgreen", label = "prediction") +
        annotate("label", x = as_datetime("2018-01-01"), y = 900, label = comment) +
        annotate("label", x = as_datetime("2019-09-01"), y = -2500, label = dip_comment) +
        labs(x = "Date", 
             y = "GDP growth above Obama model (in $B)",
             caption = "Model trained between 2009 and 2017 and predicted into 2021")

p <- gdp_full_scale + gdp_diff_scale


ggsave("graphs/real-growth.png", width = 12, height = 6, plot = p)


#
#
#
#

cutoff_date_lo <- as_datetime("2020-09-01")
cutoff_date_hi <- as_datetime(today())

gdpmodel <-
        gdp %>% 
        ungroup() %>%
        rename(date = "floordate") %>%
        filter(date > cutoff_date_lo, date < cutoff_date_hi) %>%
        nest() %>%
        mutate(gdpmodel = map(data, function(tbl) lm(log10(gdp) ~ date, data = tbl)))

rsq <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% glance())) %>% unnest(modelq) %>% pull(r.squared)


model_constant_pre2020 <- modelconstant

modelconstant <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel, function(x) x %>% tidy())) %>% unnest(modelq) %>%
        filter(term == "date") %>%
        pull(estimate)

pct_increase <- scales::percent(10 ^ (modelconstant * 86400 *365) - 1, accuracy = .1)

predict_post_2020 <-
        gdpmodel %>%
        mutate(modelq = map(gdpmodel,
                           function(x) x %>% augment(interval = "confidence"))) %>%
        unnest(modelq) %>%
        mutate(across(.cols = .fitted:.upper, .fns =~10^.x)) 

model_contant_inc <- 10^(86400*365*(modelconstant - model_constant_pre2020)) - 1
model_contant_comment <- paste0("Estimate inflation: ", scales::percent(model_contant_inc, accuracy = .1),
                                " over Obama model")


gdp_full_scale +
        geom_line(data = predict_post_2020, aes(y = .fitted), lty = 2, color = "gray50") +
        annotate("label", x = as_datetime("2020-01-01"), y = 24000, label = paste0(pct_increase, "\nr.sq = ", scales::pvalue(rsq)))  +
        labs(subtitle = model_contant_comment)

ggsave("graphs/estimating-inflation.png", width = 6, height = 6)        