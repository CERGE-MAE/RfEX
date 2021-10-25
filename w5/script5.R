library(magrittr)
library(tidyverse)


# PREPARE THE DATA --------------------------------------------------------
# https://toolbox.google.com/datasetsearch/search?query=Japan%20Trade%20Statistics&docid=wssrt0mVG%2F6fuGBBAAAAAA%3D%3D

list.files("w5/data/")

zipped = unzip("w5/data/ym_2018.csv.zip", list = TRUE)
zipped

japan <- read_csv(unz("w5/data/ym_2018.csv.zip", zipped$Name),
                  col_types = paste0(rep("c", 13), collapse = ""))

summary(japan)
head(japan)

# standardise colnames
colnames(japan) %>% tolower() %>% unique() %>% length() == ncol(japan)
colnames(japan) %<>% tolower()

# numeric values
japan$value %<>% as.numeric()
japan$value %>% summary()

# check import definition
japan %>%
    pull(exp_imp) %>% 
    unique()

# because builtin functions
na_dif = function(x, y){
    x[is.na(x)] = 0
    y[is.na(y)] = 0
    return(x - y)
}

# OVERVIEWS ---------------------------------------------------------------
# what is the export/import and the trade deficit per commodity type and country?
EI = 
    japan %>% 
    select(exp_imp, country, hs2, value) %>% 
    group_by(country, hs2, exp_imp) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(exp_imp = ifelse(exp_imp == 1, "export", "import")) %>% 
    spread(exp_imp, value) %>% 
    mutate(dif_na = export - import,
           dif = na_dif(export, import))
EI

# for each country, find the import & export shares of each commodity
# e.g. commodity 1 makes 20 % of total exports of country 1
EI_comp = 
    EI %>% 
    select(-dif_na, -dif) %>% 
    gather(EI, value, export:import) %>% 
    group_by(country, EI) %>% 
    mutate(importance = 100 * value / sum(value, na.rm = T)) %>% 
    select(- value) %>% 
    spread(EI, importance)

list.files("w5/data")

# read the information about the countries and commodities
countries = read_csv("w5/data/country_eng.csv", col_types = "ccc")
head(countries)
colnames(countries) %<>% tolower()

h2 = read_csv("w5/data/hs2_eng.csv")
head(h2)

# add country identifiers
EI_comp %>% 
    full_join(countries, by = c("country" = "country"))

EI_comp %>% 
    left_join(countries) %>% View()

# & add commodity description
EI_comp %>% 
    left_join(countries, by = "country") %>% 
    inner_join(h2, by = "hs2") %>% 
    View()
