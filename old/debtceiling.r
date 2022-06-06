source("std/stdpackages.r")
source("std/stdfunctions.r")


debt %>%
    ggplot() +
    aes(date, debt_ceiling) +
    geom_line() +
    scale_y_log10(labels = scales::dollar_format(scale = 1 / 1000, suffix = "T")) +
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
    mutate(model = map(data, debtmodel)) %>%
    ungroup()

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
    mutate(growth = estimate * secondsperyear) %>%
    inner_join(presidentinfo, by = "president")


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
    filter(date > as_datetime("1980-12-31")) %>%
    mutate(pres2 = ifelse(president == "Reagan" | president == "GHW Bush", "Group 1", "Group 2")) %>%
    mutate(pres2 = factor(pres2)) %>%
    lm(log10(debt_ceiling) ~ date * pres2, data = .) %>%
    augment() %>%
    mutate(debt_ceiling = 10^.fitted)
    
debtdata %>%
    inner_join(presidentinfo, by = "president") %>%
    filter(president != "") %>%
    group_by(president) %>%
    ggplot() +
    aes(date, debt_ceiling, color = president) +
    geom_line() +
    scale_y_log10(labels = scales::dollar_format(scale = 1 / 1000, suffix = "T")) +
    geom_vline(xintercept =  inaugdates, lty = 2, alpha = .4) +
    geom_point(data = debt %>% filter(president != "")) +
    labs(x = "Date", y = "Debt ceiling (in $T)", color = "President",
         title = "Increase in the debt ceiling since the 1960s",
         caption = "Dotted vertical lines are presidential inauguration dates") +
    #geom_line(data = altmodel, aes(color = pres2), lty = 2) +
    theme(legend.position = "none") +
    geom_line(data = altmodel2, aes(color = NA), color = "black", lty = 2, size = 1.2)

ggsave("graphs/debtceiling-since-1980.png", width = 6, height = 6)

debtmodelinfo %>%
    ggplot +
    aes(x = fct_reorder(president, desc(inaugdate)), y = growth, fill = party) +
    geom_col() +
    scale_y_continuous(labels =  scales::percent_format(accuracy = .1)) +
    scale_fill_manual(values = partycolor) +
    labs(x = "President",
         y = "Debt ceiling CAGR (in %)",
         fill = "Party",
         title = "US debt ceiling growth since the 1960s") +
    coord_flip()

ggsave("graphs/debtceiling-growth-by-president.png", width = 6, height = 8)
