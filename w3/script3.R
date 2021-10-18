library(magrittr)
# install.packages("magrittr")

# 1.) FUN WITH FACTORS ----------------------------------------------------
sample(c("3","1"), 20, replace = T)

# CREATE CATEGORICAL VECTOR
fac = 
    sample(c("3","1"), 20, replace = T) %>% 
    as.factor()
fac

# WHY BOTHER?
# RAM!
sample(c("3","1"), 20000, replace = T) %>% object.size()
sample(c("3","1"), 20000, replace = T) %>% as.factor() %>%  object.size()

# NEVER CAST TO NUMERIC!
as.numeric(fac)
fac

# FIRST CAST TO CHARACTER, THEN TO NUMERIC
fac %>% as.character() %>% as.numeric()

# LEVELS + ORDERED
levels(fac)
levels(fac) = c("M", "F", "U")
fac

ordered(fac, c("U", "M", "F"))

# 2.) FUN WITH DATETIMES --------------------------------------------------
now = Sys.time() # value
now %<>% as.POSIXlt() # list?
now = now %>% as.POSIXlt() # equivalent to previous line
now

attributes(now) # special format

now$hour
now$min
now$wday
now$yday

# 3.) FUN WITH CASTING ----------------------------------------------------
c(T, NA) # boolean
c(T, NA, 1) # numeric
c(F, 258) # numeric

sum(c(T, T, T, F, T)) # casted into numeric
T * 25 + F * 10 # casted into numeric

Sys.Date() + 5 # simple adding of days (lowest unit of class Date)
Sys.time() + 120 # simple adding of seconds (lowest unit of class POSIX)

Sys.time() - as.numeric(Sys.time()) # 1970-01-01 00:00:00 UTC

c(factor(c("A","B","C")),"B") # brainfuck

# 4.) DATA.FRAMES BASICS ------------------------------------------------------
# PACKAGES
library(tidyverse)
install.packages('tidyverse')

# DATA LOADING
stocks =
    datasets::EuStockMarkets %>% 
    as.data.frame()

# OVERVIEW
head(stocks)
str(stocks)
dim(stocks)
nrow(stocks)
ncol(stocks)
summary(stocks)

# RANGE OF DAX CLOSING VALUES
range(stocks$DAX)

diff(c(1,5,11))
diff(range(stocks$DAX))

# ADDING NEW (TIME) COLUMNS
    ## Which values to add?
nrow(stocks) / 8

    ## Prepare the vectors
days = rep(c(seq_len(232), seq_len(233)), 4)

years = rep(1991:1998, each = 232)
years = c(years, seq(1992, 1998, by = 2))
years = sort(years)

    ## Assign to the table
stocks$day = days # if the column exists, it will get overwritten
stocks$year = years # if it doesn't exists, it will be created

# SHOW ONLY DAX & FTSE FROM THE FIRST 10 DAYS OF EACH YEAR
    ## The base way
stocks[stocks$day < 11,
       c("day", "year", "DAX", "FTSE")]

    ## The tidyverse way
stocks %>% 
    filter(day <= 10) %>% 
    select(day, year, DAX, FTSE)

# SHOW ONLY SMI & CAC FOR FOLLOWING TIMES (IN ONE TABLE):
#        first 5 days of even years
#        first 10 days of odd years
stocks %>% 
    select(day, year, SMI, CAC) %>%
    filter((year %% 2 == 0 & day <= 5) | (year %% 2 == 1 & day <= 10))

# OTHER WAYS OF SELECT SPECIFICATION
    ## A range of columns
colnames(stocks)
stocks %>% 
    select(DAX:CAC) %>% 
    head()

    ## Exclude column
stocks %>% 
    select(-year) %>% 
    head()

    ## Exclude a range of columns
stocks %>% 
    select(-SMI:-year) %>% 
    head()
