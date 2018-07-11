library(here)
library(readr)
library(cluster)
library(dplyr)

raw_cards <-read_csv(here("data/cardlist.csv"))
typeCodes <- data.frame(Type=c("Summoner","Monster"),typeCode=c(100,10))
rarityCodes <- data.frame(Rarity=c("Common","Rare","Epic","Legendary"),
                          rarityCode=c(1,4.4,28,66.6))

cards <- raw_cards %>% mutate(Splinter=factor(Splinter)) %>%
                       inner_join(typeCodes) %>% 
                       inner_join(rarityCodes) %>% 
                       select(Name, rarityCode, typeCode, Splinter)  


gower_dist <- daisy(cards[, -1],
                    metric = "gower",
                    type = list(nominal=3),
                    weights = c(1,1,0.1))

gower_mat <- as.matrix(gower_dist)





