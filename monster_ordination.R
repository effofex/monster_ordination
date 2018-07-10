library(here)
library(readr)
library(cluster)
library(dplyr)

raw_cards <-read_csv(here("data/cardlist.csv"))

typeCode <- c("Summoner"=10,"Monster"=100)
rarityCode <- c("Common"=1,"Rare"=2,"Epic"=4,"Legendary"=8)
#TODO  we can make this work by doing joins on df
cards <- raw_cards %>% mutate_if(is.character, factor)
cards$codedRarity = rarityCode[cards$Rarity]
cards2 <-  cards %>% rowwise() %>% mutate(codedType=typeCode[Type],
                            codedRarity=rarityCode[Rarity]) %>%
                            select(Name, codedRarity, codedType, Splinter)
gower_dist <- daisy(cards2[, -1],
                    metric = "gower",
                    type = list(ordinal = 1,logratio=2, nominal=3))

gower_mat <- as.matrix(gower_dist)





