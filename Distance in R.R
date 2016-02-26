# Distance from city center
setwd("~/Google Drive/Personal/R/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-02 Misc/")
if(!file.exists("./Distance from Center")){dir.create("./Distance from Center")}
setwd("./Distance from Center/")

# dbExistsTable(rscon, "listings")

properties <- read.csv("properties.csv")

city_centers <- read.csv("city-centers-by-metro.csv")

# Calculates the geodesic distance between 
# two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
    R <- 6378 # Earth mean radius [km]
    long1 <- long1*pi/180
    lat1 <- lat1*pi/180
    long2 <- long2*pi/180
    lat2 <- lat2*pi/180
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(d) # Distance in km
}

metros <- city_centers$redfin_metro

# Calculate the distance to city-center of every property
df = data.frame()
df2 = data.frame()
for (i in 1:length(metros)) {
    df <- NULL
    df <- properties[properties$redfin_metro == metros[[i]],]
    df$dist <- gcd.slc(df$longitude, df$latitude, city_centers[i,7], city_centers[i,8])
    df2 <- rbind(df2,df)
}
rm(df)

# Clean the data
str(df2)

df2$sale_date <- as.Date(df2$sale_date)

# missing values?
sum(is.na(df2$dist))

dist_metro <- data.frame()
df3 <- data.frame()
metros <- as.character(metros)

#View distribution of distance for Seattle
seattle_2015_df2 <- df2[df2$redfin_metro=="Seattle, WA" & year(df2$sale_date) %in% c(2011,2013,2015),]
head(seattle_2015_df2)
seattle_2015_df2$sale_date <- as.factor(year(seattle_2015_df2$sale_date))
summary(seattle_2015_df2)
ggplot(seattle_2015_df2,aes(x=dist,y=ppsf,col=seattle_2015_df2$sale_date)) + 
    geom_smooth(alpha = .6) + 
    theme_redfin() + scale_colour_redfin()


# Could use the aggregate function instead of rbinding subsets
for (i in 1:length(metros)) {
    dist_metro <- NULL
    dist_metro <- df2[df2$redfin_metro == metros[[i]],]
    avg_dist <- tapply(dist_metro$dist,year(dist_metro$sale_date),median,na.rm=TRUE)
    avg_dist <- cbind(round(avg_dist,2),names(avg_dist), metros[i])
    df3 <- rbind(df3,avg_dist)
}
img <- readPNG("~/Google Drive/Personal/R/Redfin-Logo-Web.png", FALSE)
my_g <- grobTree(rasterGrob(img, interpolate=TRUE, x=1, hjust=1))

df3 <- df3[df3$V2 != 2010,]

df2$year <- as.factor(year(df2$sale_date))
str(df2)

df3$Growth <- NULL
setNames(df3,c("med_dist","redfin_metro","year"))
merged_data <- merge(x=df2,y=df3, all.x=TRUE)

head(merged_data)
rm(merged_data$V2)
rm(merged_data$V3)

df2$urban_core <- df2$dist <= 5 #km radius for urban core

write.csv(df3,file="distance_by_metro.csv")
#calculate the median sale price for urban cores, by year

med_prices <- aggregate(sale_price ~ redfin_metro + year(sale_date) + urban_core, data = df2, FUN = median, na.rm=TRUE)
med_prices$Growth <- with(med_prices, ave(sale_price, redfin_metro, urban_core, 
                            FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
med_prices$Growth <- med_prices$Growth*100

med_prices_all <- aggregate(Growth ~ med_prices[,2] + urban_core, data = med_prices, FUN = mean)

# Overall average growth by year by urban core
med_prices_all <- med_prices_all[med_prices_all$`med_prices[, 2]` != 2016,]

# Overall average growth by urban core or not
aggregate(Growth ~ urban_core, data = med_prices_all, FUN = mean)

getwd()
setwd("./PNGs")
if(!file.exists("./Price Growth by Distance")){dir.create("./Price Growth by Distance")}
setwd("./Price Growth by Distance")
# Make and save graphs for each metro
for (i in 1:length(metros)) {
    # View Sale Growth of Austin
    metro <- med_prices[med_prices$redfin_metro == metros[i] & med_prices$`year(sale_date)` != 2016,]
    g <- ggplot(metro, aes(x=metro[,2], y=metro[,5], fill = urban_core)) + 
        geom_bar(stat="identity", show.legend = TRUE, position="dodge") +
        # Add Redfin Colors
        ggtitle(paste0("Median Price Growth\n(",metro[1,1],")")) +
        scale_fill_redfin(limits=c("TRUE", "FALSE")) +
        xlab("Year") + ylab("Median Price Growth in %") + #remove labels of axis and legends
        theme_redfin() + labs(fill="Urban Core")
    
    filename <- paste(metro[1,1],"'s Med Price Growth by Distance from City Center.png")
    print(g)
    png(file = filename, width = 675, bg = "white")
    grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))
    dev.off()
}

#calculate the median price per square foot for urban cores, by year
med_ppsf <- aggregate(ppsf ~ redfin_metro + year(sale_date) + urban_core, data = df2, FUN = median, na.rm=TRUE)
med_ppsf$Growth <- with(med_ppsf, ave(ppsf, redfin_metro, urban_core, 
                                          FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
med_ppsf$Growth <- med_ppsf$Growth*100
med_ppsf$Growth

med_ppsf_all <- aggregate(Growth ~ med_ppsf[,2] + urban_core, data = med_ppsf, FUN = mean)

# Overall average growth by year by urban core
med_ppsf_all <- med_ppsf_all[med_ppsf_all$`med_ppsf[, 2]` != 2016,]

# Overall average growth by urban core or not
aggregate(Growth ~ urban_core, data = med_ppsf_all, FUN = mean)

getwd()
setwd("./../")
if(!file.exists("./PPSF Growth by Distance")){dir.create("./PPSF Growth by Distance")}
setwd("./PPSF Growth by Distance")
# Make and save graphs for each metro
for (i in 1:length(metros)) {
    # View Sale Growth of Austin
    metro <- med_ppsf[med_ppsf$redfin_metro == metros[i] & med_ppsf$`year(sale_date)` != 2016,]
    g <- ggplot(metro, aes(x=metro[,2], y=metro[,5], fill = urban_core)) + 
        geom_bar(stat="identity", show.legend = TRUE, position="dodge") +
        # Add Redfin Colors
        ggtitle(paste0("Median $/sqft Growth\n(",metro[1,1],")")) +
        scale_fill_redfin(limits=c("TRUE", "FALSE")) +
        xlab("Year") + ylab("Median $/sqft Growth in %") + #remove labels of axis and legends
        theme_redfin() + labs(fill="Urban Core")
    
    filename <- paste(metro[1,1],"'s Med PPSF Growth by Distance from City Center.png")
    print(g)
    png(file = filename, width = 675, bg = "white")
    grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))
    dev.off()
}

getwd()
setwd("./../")
if(!file.exists("./Median Distance Trend")){dir.create("./Median Distance Trend")}
setwd("./Median Distance Trend")
# Make and save graphs for each metro
for (i in 1:length(metros)) {
df4 <- df3[df3$red == metros[i] & df3$V2 != 2016,]
g <- ggplot(df4, aes(x=V2, y=V1)) + 
    geom_bar(stat="identity", fill = redfin_color_pal()(1), show.legend = FALSE) +
    # Add Redfin Colors
    ggtitle(paste0("Average Distance from City Center\n(",df4[1,3]," Metro Region)")) +
    # scale_fill_redfin() +
    xlab("Year") + ylab("Median Distance in km") + #remove labels of axis and legends
    theme_redfin()

filename <- paste(df4[1,3],"'s Average Distance from City Center.png")
print(g)
png(file = filename, width = 675, bg = "white")
grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))
dev.off()
}

write.csv(df3,"./distances_by_metro.csv", row.names = FALSE)
df4 <- read.csv("./distances_by_metro.csv")

urban_prices <- med_prices[med_prices$urban_core == TRUE,]
non_urban_prices <- med_prices[med_prices$urban_core == FALSE,]

urban_prices$Urban_markup <- urban_prices[,4]/non_urban_prices[,4]


Urban_markup_avg <- aggregate(Urban_markup ~ redfin_metro, data = urban_prices[urban_prices$`year(sale_date)`==2015,], FUN = mean)

g <- ggplot(Urban_markup_avg, aes(x=reorder(Urban_markup_avg$redfin_metro, Urban_markup_avg$Urban_markup), y=Urban_markup)) + 
    geom_bar(stat="identity", fill = redfin_color_pal()(1), show.legend = FALSE) +
    # Add Redfin Colors
    ggtitle(paste0("Average Markup for City Center in 2015")) +
    # scale_fill_redfin() +
    xlab("Metro") + ylab("") + #remove labels of axis and legends
    theme_redfin() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    geom_hline(yintercept = 1,  lty=2)
g

urban_ppsf <- med_ppsf[med_ppsf$urban_core == TRUE,]
non_urban_ppsf <- med_ppsf[med_ppsf$urban_core == FALSE,]

urban_ppsf$Urban_markup <- urban_ppsf[,4]/non_urban_ppsf[,4]


Urban_ppsf_markup_avg <- aggregate(Urban_markup ~ redfin_metro, data = urban_ppsf[urban_ppsf$`year(sale_date)`==2015,], FUN = mean)

med_ppsf <- med_ppsf[med_ppsf$`year(sale_date)`==2015,]
g <- ggplot(med_ppsf, aes(x=ppsf, y=reorder(med_ppsf$redfin_metro, med_ppsf$ppsf))) + 
    geom_segment(aes(yend=redfin_metro,xend=redfin_metro),xend=0, colour="grey50", show.legend = FALSE, lineend = "butt") +
    geom_point(aes(colour = med_ppsf$urban_core), size = 3, show.legend = FALSE) +
    theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
          legend.position=c(1, 0.55), # Put legend inside plot area 
          legend.justification=c(1, 0.5)) +
    # Add Redfin Colors
    ggtitle(paste0("Average Price/SQFT in 2015")) +
    scale_colour_redfin() +
    xlab("Price per Square Foot") + ylab("") + #remove labels of axis and legends
    theme_redfin() +labs("")
g

png(file = "Average Markup for City Center in 2015.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))
dev.off()
