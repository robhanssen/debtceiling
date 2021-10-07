library(tidyverse)
library(broom)
library(lubridate)
theme_set(theme_light())

inaugdates = sort(as_datetime(as.Date(c("1900-01-01","2100-01-01","2009-01-20","2017-01-21","2001-01-21","1993-01-21", "1989-01-21", "1981-01-21"))))
presidents = c("Before 1980", "Reagan", "Bush Sr", "Clinton", "Bush Jr", "Obama", "Trump" )

debt <- read_csv("debtceiling.csv") %>%
        janitor::clean_names() %>%
        select(date, debt_ceiling) %>%
        mutate(date = parse_date(.$date, "%B %d, %Y")) %>%
        mutate(date = as_datetime(date)) %>%
        filter(!is.na(debt_ceiling)) %>%
        mutate(president = cut(date, inaugdates, presidents))

debt %>% 
    ggplot() + 
    aes(date, debt_ceiling) + 
    geom_line() + 
    scale_y_log10(labels = scales::dollar_format(scale = 1/1000, suffix = "T")) +
    expand_limits(y = 30000)


debtmodel <- function(tbl) {
    lm(log10(debt_ceiling) ~ date, data = tbl)
}

altdebtmodel <- function(tbl) {
    lm(log10(debt_ceiling) ~ date + pres2, data = tbl)
} 

debtmodels <-
    debt %>% 
    group_by(president) %>%
    nest() %>%
    mutate(model = map(data, debtmodel)) %>% ungroup()

debtdata <-
    debtmodels %>%
    mutate(debtdata = map(model, augment)) %>%
    unnest(debtdata) %>%
    mutate(debt_ceiling = 10^.fitted)

debtmodelinfo <-
    debtmodels %>%
    mutate(info = map(model, tidy)) %>%
    unnest(info) %>% 
    filter(term == "date") %>%
    mutate(growth = estimate * 365 * 24 * 60 * 60)


# alternate model
altmodel <-
    debt %>%
    filter(date > as_datetime("1981-12-31")) %>%
    mutate(pres2 = ifelse(date < as_datetime("1991-12-31"), "Group 1", "Group 2")) %>%
    group_by(pres2) %>%
    nest() %>%
    mutate(model2 = map(data, debtmodel)) %>%
    mutate(debtdata = map(model2, augment)) %>%
    unnest(debtdata) %>% 
    mutate(debt_ceiling = 10^.fitted)

altmodel2 <-
    debt %>%
    filter(date > as_datetime("1981-12-31")) %>%
    mutate(pres2 = ifelse(date < as_datetime("1991-12-31"), "Group 1", "Group 2")) %>%
    mutate(pres2 = factor(pres2)) %>%
    lm(log10(debt_ceiling) ~ date * pres2, data = .) %>% 
    augment() %>%
    mutate(debt_ceiling = 10^.fitted)
    
debtdata %>%
    filter(president != "Before 1980") %>%
    group_by(president) %>%
    ggplot() + 
    aes(date, debt_ceiling, color = president) + 
    geom_line() + 
    scale_y_log10(labels = scales::dollar_format(scale = 1/1000, suffix = "T")) + 
    geom_vline(xintercept =  inaugdates, lty = 2, alpha = .4) + 
    geom_point(data = debt %>% filter(president != "Before 1980")) + 
    labs(x = "Date", y = "Debt ceiling (in $T)", color = "President",
         title = "Increase in the debt ceiling since 1980", 
         caption = "Dotted vertical lines are presidential inauguration dates") + 
    #geom_line(data = altmodel, aes(color = pres2), lty = 2) +
    theme(legend.position = "none") +
    geom_line(data = altmodel2, aes(color = NA), color = "black", lty = 2)

    ggsave("debtceiling-since-1980.png", width = 6, height = 6)
