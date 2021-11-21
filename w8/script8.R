library(magrittr)
library(tidyverse)

# TEXT DATA (SHAKESPEAR)---------------------------------------------------------------
# read and remove empty lines
romeo = readLines("http://www.gutenberg.org/cache/epub/1112/pg1112.txt", encoding = "UTF8")
romeo %<>% {.[. != ""]}
# romeo = romeo[romeo != ""]

# split into header nad corpus
first_line = which(romeo == "1595")
romeo = romeo[-1:-first_line]
header = romeo[4:28]
corpus = romeo[-1:-32]
rm(first_line, romeo)

# FIND THE LINES OF EVERY PERSON IN THE BOOK ----------------------------------

na_rem = function(x){
    return(x[!is.na(x)])
}

# define person names
persons = 
    header %>% 
    str_extract("[ [:alpha:]]+,") %>% 
    str_remove(",") %>%
    str_squish() %>% 
    na_rem()
rm(header)

# try to find them in the text
corpus %>% 
    str_subset(paste(persons, collapse = "|"))

corpus[20:30]

test_person =
    corpus[20:30] %>% 
    str_extract("^\\s{2}[:alpha:]+\\.") %>%
    str_squish() %>% 
    str_remove("\\.") %>% 
    na_rem() %>% 
    unique()
rm(test_person)

connect_person = map(corpus_person, ~str_subset(persons, .x))
names(connect_person) = corpus_person
connect_person %<>% unlist()
rm(corpus_person)
rm(persons)

names(connect_person) %<>%
    str_remove("[0-9]+") 

connect_person =
    names(connect_person) %>% 
    {.[duplicated(.)]} %>% 
    unique() %>% 
    {which(names(connect_person) %in% .)} %>% 
    {connect_person[multiply_by(.,-1)]}

lines_person = 
    names(connect_person) %>% 
    map(~str_subset(corpus, paste0("^\\s{2}", .x)))

names(lines_person) = connect_person
map_dbl(lines_person, length)

rm(na_rem, lines_person, connect_person, corpus)

# SALES DATA --------------------------------------------------------------------
# original at:
# https://toolbox.google.com/datasetsearch/search?query=sales&docid=5kapgBB5IYEGaNZVAAAAAA%3D%3D
# but I did some preprocessing

list.files("w8/data")
zipped = unzip("w8/data/liquor_sales.zip", list = TRUE)

liquor = read_csv(unz("w8/data/liquor_sales.zip", zipped$Name),
                  col_types = paste0(rep("c", 24), collapse = "")) # safer way

summary(liquor)
head(liquor, 100) %>% View()

index = 
    colnames(liquor) %>% 
    str_detect("Item Description") %>% 
    which() 
char_names = colnames(liquor)[1:index]
num_names = colnames(liquor)[(index + 1):ncol(liquor)]
rm(index)

liquor %<>% 
    mutate_at(char_names, as.character)

to_num = function(x) {
    x %<>% 
        str_remove("\\s") %>% 
        str_remove("\\$") %>% 
        str_replace("\\,",".") %>% 
        as.numeric()
    return(x)
}

liquor %<>% 
    mutate_at(num_names, to_num)

rm(char_names, num_names, to_num)

map_chr(liquor, class)

liquor %<>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

summary(liquor)

# do the sales make sense?
liquor %>% 
    mutate(sales_test = `State Bottle Retail`*`Bottles Sold` - `Sale (Dollars)`) %>% 
    pull(sales_test) %>% 
    summary()

# lets check GPS
liquor %<>% 
    mutate(gps = str_extract(`Store Location`,
                             "[0-9]{2}\\.[0-9]{1,6}, \\-[0-9]{2,3}\\.[0-9]{1,6}"))

# where GPS wasnt extracted?
liquor %>% 
    pull(gps) %>% 
    is.na() %>% 
    filter(liquor, .) %>% 
    pull(`Store Location`) %>% 
    unique()

splitter = function(x) {
    if (is.na(x)) {
        txt = c(NA,NA)
    } else {
        txt =
            strsplit(x, split = ", ") %>% 
            unlist()
    }
    names(txt) = c("lat","lon")
    return(txt)
}

liquor =
    liquor %>% 
    mutate(gps_c = map(gps, splitter)) %>% 
    unnest_wider(gps_c)

liquor %>% 
    mutate_at(c("lat", "lon"), .funs = as.numeric) %>% 
    View()
