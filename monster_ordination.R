library(here)     # handle paths to data and output
library(readr)    # read CSV file
library(dplyr)    # easy data manipulation
library(cluster)  # for Gower distance and daisy 
library(ape)      # ordination
library(ggplot2)  # plotting
library(ggrepel)  # labelling points
library(Cairo)    # nicer looking plots on Win

# get our list of non-foil cards
raw_cards <-read_csv(here("data/cardlist.csv"))
# Translate card type to a numeric code
typeCodes <- data.frame(Type=c("Summoner","Monster"),typeCode=c(0.25,5))
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
                    weights = c(1,1,0.5))

gower_mat <- as.matrix(gower_dist)

# Create a pcoa ordination using the distance matrix
cord <- pcoa(gower_dist, correction="none", rn=NULL)

# create a data frame consisting of card names and their locations on the
# first two ordination axes, for plotting
orded<-data.frame(x=cord$vectors[,1],y=cord$vectors[,2])
neword<-cbind(cards,orded)

p <- ggplot(subset(neword),aes(x=x,y=y,
                               fill=factor(Splinter),
                               color=factor(typeCode),
                               shape=factor(rarityCode))) +
  theme_bw() +
  theme(text = element_text(size=20,color="Black"),
        axis.text.x  = element_text(size=20,color="Black"),
        axis.text.y  = element_text(size=20,color="Black"),
        axis.title.x  = element_text(size=20,color="Black"),
        axis.title.y  = element_text(size=20,color="Black")) +
  xlab(sprintf(fmt="PC1: %.2f%% variance explained",
               cord$values$Relative_eig[1]*100) ) +
  ylab(sprintf(fmt="PC2: %.2f%% variance explained",
               cord$values$Relative_eig[2]*100) ) +
  geom_jitter(size=4,width=0.04,height=0.02,stroke=1,alpha=0.7) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                            breaks = c(1,4.4,28,66.6),
                            labels = c("Common","Rare","Epic","Legendary")) +
  scale_fill_manual(values = c("Grey", "Skyblue2", "palegreen4", "orchid3", 
                               "Tomato2","lightgoldenrod2"),
                               breaks = c( "Black", "Blue","Green","Purple",
                                           "Red","White"),
                               labels = c("Death","Water","Nature","Dragon",
                                          "Fire","Life")) +
  guides(fill=guide_legend(title="Splinter",
                           override.aes=list(alpha=1,
                           colour=c(Death="Grey",Water="Skyblue2",
                                   Nature="palegreen4",
                                   Dragon="orchid3",
                                   Fire="Tomato2",
                                   Life="lightgoldenrod2")))) +

  scale_color_manual(values = c("Mediumorchid4", "Black"),
                            breaks = c(0.25,5),
                            labels = c("Summoner","Monster")) +
  guides(shape=guide_legend(title="Rarity")) +
  guides(color=guide_legend(title="Type",
                            override.aes=list(
                              fill=c(Summoner="White",Monster="White"),
                              shape=21,label=""))) +
#  geom_text_repel(data=subset(neword,typeCode==0.25),
#                          aes(label=Name),
#                          xlim  = c(-0.25,0.0),
#                          ylim = c(-0.25,-1.0)) +
  geom_text(data=subset(neword,typeCode==0.25), aes(label=Name))

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
