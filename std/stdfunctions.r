# set standard graphics
theme_set(theme_light())

# constants
secondsperyear <- 365 * 24 * 3600
partycolor <- c("R" = "red", "D" = "blue")

# read president info file
presidentinfo <-
    read_csv("sources/presidents.csv") %>%
    mutate(inaugdate = as_datetime(inaugdate),
           party = factor(party, levels = c("R", "D"))
           )

inaugdates <- c(as_datetime("1776-03-31"),
                presidentinfo$inaugdate,
                as_datetime("9999-12-31"))

presidents <- c("", presidentinfo$president)

# read debt ceiling file
debt <- read_csv("sources/debtceiling.csv") %>%
        janitor::clean_names() %>%
        select(date, debt_ceiling) %>%
        mutate(date = parse_date(.$date, "%B %d, %Y")) %>%
        mutate(date = as_datetime(date)) %>%
        filter(!is.na(debt_ceiling)) %>%
        mutate(president = cut(date, inaugdates, presidents))
