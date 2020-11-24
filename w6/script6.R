library(magrittr)
library(tidyverse)

# DATA --------------------------------------------------------
# https://toolbox.google.com/datasetsearch/search?query=sales&docid=mEb0PRlWa0KIiHwzAAAAAA%3D%3D

# READ BY YEAR ------------------------------------------------------------

files = list.files("w6/data/yearly", full.names = T)

# the for loop way
for (i in files) {
    if (exists("games")) {
        games = rbind(games, read_csv(i))
    } else {
        games = read_csv(i)
    }
}
games

# vectorised
games = map_df(files, read_csv)
games = map_df(files,
               ~read_csv(.x, col_types = cols(.default = "c")))


# PARSE NUMERIC FEATURES --------------------------------------------------

# R is functional programming language
# prepare a function that will check, whether the columns can be changed 
# to numeric -> if yes, change it
check_numeric = function(col) {
    nas =
        col[!is.na(col)] %>% 
        as.numeric() %>% 
        is.na() %>%
        sum()
    
    if (nas == 0) {
        col = as.numeric(col)
    }
    
    return(col)
}

# loopy way
for (i in colnames(games)) {
    games[,i] = check_numeric(pull(games[,i]))
}

# vectorised
games = map_df(games, check_numeric)
    

# READ BY YEAR FUNCTION ---------------------------------------------------

read_by_year = function(path, years) {
    files = list.files(path)
    
    if (is.numeric(years) | is.character(years)) {
        pattern = paste0("game_sales", years, ".csv")
        files = files[files %in% pattern]
        
        writeLines(paste0("Reading files:\n",
                     paste(files, collapse = "\n")
                     )
              )

    } else if (year != "all") {
    } else {
        stop("Please specify years as numeric or character vector, or use 'all' to use all files")
    } # WHAT IS WRONG WITH THIS?
    
    input =
        file.path(path, files) %>% 
        map_df(~read_csv(.x, col_types = cols(.default = "c"))) %>% 
        map_df(check_numeric)
    
    return(input)
}

games_2k = read_by_year("w6/data/yearly", 2000:2030)


# LINEAR RELATIONSHIP? ----------------------------------------------------

games_lm = 
    games %>% 
    select(Global_Sales, Year_of_Release, Critic_Score, User_Score) %>% 
    na.omit() %>% 
    split(.$Year_of_Release) %>% 
    map(~lm(Global_Sales ~ Critic_Score + User_Score, data = .)) %>% 
    map(summary) %>% 
    map("coefficients") %>% 
    do.call(rbind.data.frame, .) %>%
    mutate(desc = rownames(.))

games_lm$year = 
    games_lm %>% 
    pull(desc) %>% 
    map_chr(~strsplit(.x, "\\.")[[1]][1])

games_lm$coef =
    games_lm %>% 
    pull(desc) %>% 
    map_chr(~strsplit(.x, "\\.")[[1]][2])

games_lm %>% 
    select(year, coef, Estimate) %>% 
    filter(year > 1994) %>% 
    spread(coef, Estimate) %>% View()

games %>% 
    drop_na() %>% 
    filter(Year_of_Release > 1994) %>% 
    group_by(Year_of_Release) %>% 
    summarise(critics = min(Critic_Score, na.rm = T),
              users = min(User_Score, na.rm = T)) %>% View()

# SAVE BY YEAR ------------------------------------------------------------
# dir.create("w6/data/yearly")

games %>% 
    split(.$Year_of_Release) %>% 
    map(., ~write_csv(.x,
                       paste0("w6/data/yearly/game_sales",
                              unique(.x$Year_of_Release),
                              ".csv"
                              )
                       )
         )

list_df = 
    games %>% 
    split(.$Year_of_Release) 

for (i in list_df) {
    write_csv(i,
              paste0("w6/data/yearly/game_sales",
                     unique(i$Year_of_Release)
              )
    )
    
}
