library(here)     # handle paths to data and output
library(readr)    # read CSV file
library(dplyr)    # easy data manipulation
library(cluster)  # for Gower distance and daisy 
library(ape)      # ordination
library(ggplot2)  # plotting
library(Cairo)    # nicer looking plots on Win

# get our list of non-foil cards
raw_cards <-read_csv(here("data/cardlist.csv"))
# Translate card type to a numeric code
typeCodes <- data.frame(Type=c("Summoner","Monster"),typeCode=c(0.25,1))
# Translate rarity to a numeric code
rarityCodes <- data.frame(Rarity=c("Common","Rare","Epic","Legendary"),
                          rarityCode=c(1,4.4,28,66.6))

# Creates a data frame with all the info we need to ordinate
cards <- raw_cards %>% mutate(Splinter=factor(Splinter)) %>%
                       inner_join(typeCodes) %>% 
                       inner_join(rarityCodes) %>% 
                       dplyr::select(Name, rarityCode, typeCode, Splinter)  

# Compute a pairwise gower distances for card pairs, using Gower distance
# and adjusting weights so Splinter isn't overwhelming
gower_dist <- daisy(cards[, -1],
                    metric = "gower",
                    type = list(nominal=3),
                    weights = c(1,1,0.1))

gower_mat <- as.matrix(gower_dist)

# Create a pcoa ordination using the distance matrix
cord <- pcoa(gower_dist, correction="none", rn=NULL)

# create a data frame consisting of card names and their locations on the
# first two ordination axes, for plotting
orded<-data.frame(x=cord$vectors[,1],y=cord$vectors[,2])
neword<-cbind(cards,orded)

p <- ggplot(subset(neword),aes(x=x,y=y,fill=factor(Splinter),color=factor(typeCode),shape=factor(rarityCode)))
p <- p+theme_bw()
p <- p+geom_jitter(size=4,width=0.02,height=0.02,stroke=1)
p <- p + scale_shape_manual(values = c(21, 22, 23, 24))
p <- p + scale_color_manual(values = c("Red", "Black"))
#p <- p + geom_jitter(data=(subset(neword,typeCode==0.25)),color="Black")
p
CairoWin()
p<-ggplot(subset(neword,Splinter=="Purple"),aes(x=x,y=y,shape=factor(typeCode),color=factor(rarityCode)))
p <- p+geom_point()
p
p<-ggplot(neword,aes(x=x,y=y,shape=factor(typeCode),color=factor(rarityCode)))
p <- p+geom_point()
p

skills <- data.frame(skill1=rbinom(59, 1, 0.1),
           skill2=rbinom(59, 1, 0.2),
           skill3=rbinom(59, 1, 0.1),
           skill4=rbinom(59, 1, 0.1),
           skill5=rbinom(59, 1, 0.05))
cskill <- cbind(cards,skills)

gower_dist_skills<- daisy(cskill[, -1],
                    metric = "gower",
                    type = list(nominal=3),
                    weights = c(1,1,0.1,1,1,1,1,3))

cordskill <- pcoa(gower_dist_skills, correction="none", rn=NULL)
biplot(cordskill)
x=cordskill$vectors[,1]
y=cordskill$vectors[,2]
