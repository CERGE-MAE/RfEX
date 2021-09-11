library(tidyverse)

econ = readxl::read_xlsx("w4/data/econmap.xlsx", 1)

# HISTOGRAM -------------------------------------------------------------------
econ %>%
    filter(name == "Azerbaijan") %>% 
    pull(population) %>%
    hist()

econ %>%
    filter(name == "Azerbaijan") %>% 
    pull(gdp_cap) %>% 
    hist(breaks = 25)

# BOXPLOT ---------------------------------------------------------------------
econ %>%
    filter(name == "Azerbaijan") %>% 
    pull(population) %>% 
    boxplot()

# a little fun with vectorisation
# prepare the plotting function
pop_box = function(x) {
    boxplot(x$population)
    title(unique(x$name))
}

# number of plots and organisation
length(unique(econ$name))
par(mfrow = c(3,4))

# vectorise
econ %>% 
    select(name, population) %>% 
    split(.$name) %>% 
    walk(.,pop_box)

par(mfrow = c(1,1))

# SCATTER PLOT ----------------------------------------------------------------
econ %>% 
    filter(name == "Bangladesh") %>% 
    {plot(x = .$year, y = .$gdp_05)}

econ %>% 
    filter(name == "Bangladesh") %>% 
    {plot(x = .$year, y = .$gdp_05, type = "l")}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########### GGPLOT2 ########### -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)

econ %>% 
    ggplot(aes(x = year, y = population, color = name)) +
    geom_line() +
    geom_point(size = 1)

# COMPARING TRENDS ------------------------------------------------------------
## UGLY
econ %>% 
    filter(name == "Afghanistan", year < 2018) %>% 
    
    ggplot(aes(year, population, col = "population")) + 
    geom_line() +
    geom_line(aes(y = gdp_05, col = "gdp"))

## BAD
econ %>% 
    filter(name == "Afghanistan", year < 2018) %>% 
    select(name, year, population, gdp_05) %>% 
    gather("metric", "value", population:gdp_05) %>% 
    
    ggplot(aes(year, value, color = metric)) + 
    geom_line()


## BUT !!!
econ %>% 
    filter(name == "Afghanistan", year < 2018) %>% 
    
    ggplot(aes(year, gdp_05)) + 
    geom_line()

## GOOD
econ %>% 
    filter(name == "Afghanistan", year < 2018) %>% 
    select(name, year, population, gdp_05) %>% 
    gather("metric", "value", population:gdp_05) %>% 
    group_by(metric) %>% 
    mutate(value = value/mean(value)) %>% 
    
    ggplot(aes(year, value, color = metric)) + 
    geom_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########### ECONOMETRICS  ########### -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
econ %>% 
    select(year, labor_force, savings_rate, population, gdp_cap) %>% 
    GGally::ggpairs()

## linear model
solow = 
    econ %>% 
    drop_na() %>% 
    {lm(gdp_cap ~ year + labor_force + savings_rate + population, .)}
summary(solow)

    ### or if you feel like it
X = econ %>% 
    drop_na() %>% 
    select(year, labor_force, savings_rate, population) %>% 
    mutate(int = 1) %>% 
    as.matrix()

Y = econ %>% 
    drop_na() %>%
    select(gdp_cap) %>% 
    as.matrix()

beta = solve(t(X) %*% X) %*% t(X) %*% Y

## check your residuals
lm(disp ~ ., mtcars) %>% plot()
plot(solow)

## test your model
    ### F-test
var.test(solow, lm(gdp_cap ~ year + labor_force, econ))

    ### RESET
library(lmtest)
reset(solow)

    ### Breusch-Pagan
bptest(solow)
