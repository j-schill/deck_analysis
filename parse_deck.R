#load packages
library(readxl)
library(tidyverse)

#paste a decklist into excel and import
#(in the future set up an import straight from clipboard)
deck = read_xlsx( file.choose(), col_names = F)

#regex format
regexp = ' (?=[^ ]+$)'

split1 <- do.call(rbind, strsplit(deck[[1]], regexp, perl=TRUE))

deck$coll_id <- split1[,2]
deck$...1 <- split1[,1]
split2 <- do.call(rbind, strsplit(deck[[1]], regexp, perl=TRUE))
deck$set <- split2[,2]
deck$...1 <- split2[,1]

regexp <- "^(\\w+)\\s?(.*)$"

deck$count <- sub(regexp,"\\1",deck$...1)
deck$mons = sub(regexp,"\\2",deck$...1)

clean_deck <- deck %>% filter(mons != "") %>%
  transmute(count = as.double(count), mons, set, coll_id)

count <- clean_deck$count %>% sum()


clean_deck %>% mutate(draw_prob = count/60)

