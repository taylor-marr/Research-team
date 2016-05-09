
setwd("~/Google Drive/Personal/R/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-02 Misc/Flips Update/")

flipping_listings <- read.csv("./The_Flipping_Data.csv")

All_flipping_listings <- flipping_listings[flipping_listings$metro=="Combined Markets",]
str(All_flipping_listings)

flips <- All_flipping_listings[All_flipping_listings$Year %in% c(2014,2015),]
summary(flips[,3])

ggplot(All_flipping_listings, aes(x=factor(All_flipping_listings$Year), y=Flipped)) +
    geom_bar(stat="identity", show.legend = TRUE) +
    theme_redfin() + scale_fill_redfin()

head(flipping_listings)

metro_flips <- flipping_listings[,1:3]
head(metro_flips)

library(reshape2)
metro_flips <- dcast(metro_flips,metro ~ Year)
head(metro_flips)

metro_flips <- metro_flips[,c(1,8,9)]
metro_flips$yoy_growth <- (metro_flips[,3]-metro_flips[,2])/metro_flips[,3]
metro_flips$yoy_growth <- metro_flips$yoy_growth*100

metro_flips[metro_flips$metro=="Combined Markets",]

write.csv(metro_flips, "./flips_yoy_by_metro.csv")
