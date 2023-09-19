#deck optimizer

library(readxl)
library(tidyverse)

deck = read_xlsx( "C:\\Users\\xjxs528\\OneDrive - W.W Grainger, inc\\Desktop\\lugia_ss.xlsx", col_names = F)
deck
regexp = ' (?=[^ ]+$)'

split1 <- do.call(rbind, strsplit(deck[[1]], regexp, perl=TRUE))

deck$coll_id <- split1[,2]
deck$...1 <- split1[,1]
split2 <- do.call(rbind, strsplit(deck[[1]], regexp, perl=TRUE))
deck$set <- split2[,2]
deck$...1 <- split2[,1]

rexp <- "^(\\w+)\\s?(.*)$"

deck$count <- sub(rexp,"\\1",deck$...1)
deck$mons = sub(rexp,"\\2",deck$...1)

clean_deck <- deck %>% filter(mons != "") %>%
  transmute(count = as.double(count), mons, set, coll_id)

count <- clean_deck$count %>% sum()


#function to calculate odds of drawing card X off the top
draw = function(deck, card){
  totalx = filter(deck, mons == card) %>% pull(count) %>% sum()

  totalx/(clean_deck$count %>% sum())

}
clean_deck %>% mutate(draw_prob = count/60)

