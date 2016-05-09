############################################
# Explanation of analysis for UW Runstad Center 
# due April 08, 2016 by Taylor Marr
############################################

# -Supply
#   # of new condos sold in Seattle going back 30 years, or as many years as possible
#   # of new condos sold for the last 5 years in each of the following cities:
# ·         Portland, OR
# ·         San Francisco, CA
# ·         Los Angeles, CA
# ·         San Diego, CA
# ·         Phoenix, AZ
# ·         Las Vegas, NV
# ·         Chicago, IL

#   Population of each city mentioned above, including Seattle
#   New single-family homes sold in each city mentioned above, including Seattle, going back 5 years

# -Affordability
#   Current median income of each city mentioned above, including Seattle
#   Avg. new condo sale price for each city mentioned above, including Seattle, going back 5 years

# Add column that shows condo units/1000 people by city
# Add column with New/existing sale price ratio by city and historical

# --HYPOTHESIS IS THAT NEW CONDOS ARE NOT BEING BUILT TO MEET MARKET DEMAND OR AT PRICE POINT NEEDED.
# -WA ST CONDO LAW imposing liability concerns on builders and adding more incentive to build apartments 
# (along with a hundred other incentives and lower risks)

# Lack of investment in construction since 2008 - 2013
# Almost all money has gone into apartments not Condos

# Pugent sound business journal
# http://www.bizjournals.com/seattle/news/2016/04/06/uw-study-aims-to-find-out-why-so-few-condos-are.html
# https://www.redfin.com/blog/2016/02/is-seattle-getting-richer-or-poorer.html

# Include footnotes on:
#     back to 1990 only,
#     Date ranges for the median income for 2014
#     about missing 2015 from historical set
#     Explain issues of missing sale price data in MLS and Historical
#     population is 5-yr census estimate for 2014
#     median household income is 5-yr census estimate for all or 2014
#     Same as income distribution
    

# Share City-center vs periphery for existing condo market:
#     https://public.tableau.com/profile/redfin#!/vizhome/SeattleCondos/CondoTrends

# Hope to set up meeting when Nela is in town next weekday
# 
# Be precise about any data issues and ensure no doubt is cast
# Paper will publish, then we can add a version on our Blog


############################################
# Load Packages & Setup
############################################

setwd("~/Google Drive/PRTeam Analysis/R Scripts/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-03 Misc/")
if(!file.exists("./UW Runstad Report")){dir.create("./UW Runstad Report")}
setwd("./UW Runstad Report/")

# Checks if connection is succesful
dbExistsTable(con, "listings")
library(WriteXLS)
library(plotly)
library(reshape2)

############################################
# Getting Data #
############################################

# data_frame <- dbGetQuery(con, statement =
#                               "
# with a as (
#     select l.city || ', ' || l.state_code as place,
#      date_trunc('year', l.sale_date) as date_sold,
#      pt.name as property_type,
#      l.is_new_construction,
#      count(distinct l.property_id) as home_sales,
#      median(l.sale_price) as median_sale_price,
#      (count(distinct l.property_id)::float/pp.total_population::float)*1000 as home_sales_per_1000,
#      pp.total_population,
#      i.median_family_income_2014
#      from listings l
#      join property_types pt on pt.property_type_id=l.property_type_id
#      left join places_population pp on l.city = pp.place and l.state_code = pp.state_code
#      left join __place_median_income_verbose i on l.city = replace(i.namelsad,' city','') and i.namelsad in ('Seattle city', 'Portland city', 'San Francisco city', 'Los Angeles city', 'San Diego city', 'Phoenix city', 'Las Vegas city', 'Chicago city')
#      and i.name != 'Portland city, Maine'
#      where l.property_type_id in (3,6,13)
#      and l.listing_type_id = 1
#      and l.city || ', ' || l.state_code in
#      ('Seattle, WA', 'Portland, OR', 'San Francisco, CA', 'Los Angeles, CA', 'San Diego, CA', 'Phoenix, AZ', 'Las Vegas, NV', 'Chicago, IL')
#      and date_trunc('year',l.sale_date) >= '2010-01-01'
#      and date_trunc('year',l.sale_date) < '2016-01-01'
#      group by 1,2,3,4,8,9
#      order by 1,2,3)
#  select a.*, 
#  (case when a.is_new_construction = TRUE then a.median_sale_price::float/a2.median_sale_price::float end) as new_to_existing_price_ratio
#  from a
#  left join a a2 on a.place = a2.place and a.date_sold = a2.date_sold and a.property_type = a2.property_type and a2.is_new_construction = FALSE
#  
# ")
# 
# write.csv(data_frame,file = "data_frame.csv", row.names = FALSE)

# data_frame_raw <- dbGetQuery(con, statement =
#                               "
# select l.city || ', ' || l.state_code as place,
# date_trunc('year', l.sale_date) as date_sold,
# pt.name as property_type,
# l.is_new_construction,
# l.property_id,
# l.sale_price,
# l.list_price,
# pp.total_population,
# i.median_family_income_2014
# from listings l
# join property_types pt on pt.property_type_id=l.property_type_id
# left join places_population pp on l.city = pp.place and l.state_code = pp.state_code
# left join __place_median_income_verbose i on l.city = replace(i.namelsad,' city','') and i.namelsad in ('Seattle city', 'Portland city', 'San Francisco city', 'Los Angeles city', 'San Diego city', 'Phoenix city', 'Las Vegas city', 'Chicago city')
# and i.name != 'Portland city, Maine'
# where l.property_type_id in (3,6,13)
# and l.listing_type_id = 1
# and l.city || ', ' || l.state_code in
# ('Seattle, WA', 'Portland, OR', 'San Francisco, CA', 'Los Angeles, CA', 'San Diego, CA', 'Phoenix, AZ', 'Las Vegas, NV', 'Chicago, IL')
# and date_trunc('year',l.sale_date) >= '2010-01-01'
# and date_trunc('year',l.sale_date) < '2016-01-01'
# order by 1,2,3
# ")
# 
# write.csv(data_frame_raw,file = "data_frame_raw.csv", row.names = FALSE)


# Seattle Historical Sales Data

# seattle_historical <- dbGetQuery(con, statement =
#                              "
# with a as (
#     select p.city || ', ' || p.state_code as place,
#      date_trunc('year', t.sale_date) as date_sold,
#      pt.name as property_type,
#      (CASE WHEN to_char(t.sale_date, 'yyyy')::INT - year_built::INT between -1 and 2 THEN TRUE ELSE FALSE END) as is_new_construction,
#      count(t.property_id) as home_sales,
#      median(t.sale_price) as median_sale_price
#      from property_transactions t
#      join properties p on t.property_id = p.property_id
#      join property_types pt on pt.property_type_id = p.property_type_id
#      where p.property_type_id in (3,6,13)
#      and p.city || ', ' || p.state_code = 'Seattle, WA'
#      and date_trunc('year',t.sale_date) >= '1990-01-01'
#      and date_trunc('year',t.sale_date) < '2015-01-01'
#      group by 1,2,3,4
#      order by 1,4,2,3
# )
# select a.*,
# (case when a.is_new_construction = TRUE then a.median_sale_price::float/a2.median_sale_price::float end) as new_to_existing_price_ratio
# from a
# left join a a2 on a.place = a2.place and a.date_sold = a2.date_sold and a.property_type = a2.property_type and a2.is_new_construction = FALSE
# 
# ")
# 
# write.csv(seattle_historical,file = "seattle_historical.csv", row.names = FALSE)

# Read in raw saved data
data_frame_raw <- read.csv("./data_frame_raw.csv",stringsAsFactors = FALSE)

# Read in saved historical data for Seattle
seattle_historical <- read.csv("./seattle_historical.csv",stringsAsFactors = FALSE)

# Read in saved aggregated data
data_frame <- read.csv("./data_frame.csv",stringsAsFactors = FALSE)

# Read in Census data
acs_2014_data <- read.csv("./ACS_14_5YR_DP03.csv",stringsAsFactors = FALSE,skip = 1)

############################################
# Data checking and cleaning #
############################################

dim(data_frame_raw)
head(data_frame_raw)
tail(data_frame_raw)
str(data_frame_raw)

# Clean up dates to remove timestamp and set to monthly
head(data_frame_raw[,2])
data_frame_raw[,2] <- as.Date(data_frame_raw[,2])
data_frame_raw[,1] <- as.factor(data_frame_raw[,1]) # City to factor

# View results
dim(data_frame)
head(data_frame)
tail(data_frame)
str(data_frame)


# Clean up dates to remove timestamp and set to monthly
head(data_frame[,2])
data_frame[,2] <- as.Date(data_frame[,2])
data_frame[,1] <- as.factor(data_frame[,1]) # City to factor



# View results
dim(seattle_historical)
head(seattle_historical)
tail(seattle_historical)
str(seattle_historical)
summary(seattle_historical)

# Clean up dates to remove timestamp and set to monthly
head(seattle_historical[,2])
seattle_historical[,2] <- as.Date(seattle_historical[,2])
seattle_historical[,1] <- as.factor(seattle_historical[,1]) # City to factor

dim(acs_2014_data)
str(acs_2014_data)
View(acs_2014_data)

acs_2014_data_long <- melt(acs_2014_data,id=c("Id","Id2","Geography"))

# function to title case the column names
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- colnames(data_frame)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(data_frame) <- cn
colnames(data_frame)

cn <- colnames(data_frame_raw)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(data_frame_raw) <- cn
colnames(data_frame_raw)

cn <- colnames(seattle_historical)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(seattle_historical) <- cn
colnames(seattle_historical)


# Change the order of levels
data_frame$Place <- factor(data_frame$Place, levels = c("Seattle, WA", "Chicago, IL", "Las Vegas, NV", "Los Angeles, CA", "Phoenix, AZ", "Portland, OR", "San Diego, CA", "San Francisco, CA"))
unique(data_frame$Place)

data_frame_raw$Place <- factor(data_frame_raw$Place, levels = c("Seattle, WA", "Chicago, IL", "Las Vegas, NV", "Los Angeles, CA", "Phoenix, AZ", "Portland, OR", "San Diego, CA", "San Francisco, CA"))
unique(data_frame_raw$Place)

data_frame_raw$`Is New Construction` <- factor(data_frame_raw$`Is New Construction`, levels = c("t","f"))
unique(data_frame_raw$`Is New Construction`)

acs_2014_data_long$Geography <- factor(acs_2014_data_long$Geography, levels = c("Seattle, WA", "Chicago, IL", "Las Vegas, NV", "Los Angeles, CA", "Phoenix, AZ", "Portland, OR", "San Diego, CA", "San Francisco, CA"))
unique(data_frame$Place)


############################################
# Data exploration #
# Visual Analysis # 
############################################

# Read in Redfin logo and formater
img <- readPNG("~/Google Drive/PRTeam Analysis/R Scripts/Redfin-Logo-Web.png", FALSE)
my_g <- grobTree(rasterGrob(img, interpolate=TRUE, x=1, hjust=1))
g <- arrangeGrob(gs, sub = textGrob("Footnote", x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 18)))

# While these cities appear similar for new construction of SFRs, Seattle becomes an outlier among new condo sales in 2015 and 2016 YTD (likely a lot of Insignia sales + Vik Ballard and Luma). Seattle also outpaces in new construction of townhomes.
ggplotly(
    gs <- ggplot(data_frame[data_frame$`Property Type` %in% c(
        'Single Family Residential'
        # 'Condo/Co-op'
        # 'Townhouse'
        ) 
                & data_frame$`Is New Construction` == 't' 
                &  data_frame$Place %in% c("Seattle, WA", "Las Vegas, NV","Phoenix, AZ", "Portland, OR")
        ,], 
            aes(x=`Date Sold`, y=`Home Sales`, fill=`Place`)) + 
    geom_bar(stat="identity", position = "dodge") +
    ggtitle("New Single Family Residential Sales") +
    xlab("") + ylab("") + labs(fill="") +#remove labels of axis and legends
    theme_redfin() + scale_fill_redfin()
    )


ggplotly(
    gt <- ggplot(data_frame[data_frame$`Property Type` %in% c(
        # 'Single Family Residential'
        # 'Condo/Co-op'
        'Townhouse'
    ) 
    & data_frame$`Is New Construction` == 't' 
    &  data_frame$Place %in% c("Seattle, WA", "Las Vegas, NV","Phoenix, AZ", "Portland, OR")
    ,], 
    aes(x=`Date Sold`, y=`Home Sales`, fill=`Place`)) + 
        geom_bar(stat="identity", position = "dodge") +
        ggtitle("New Townhome Sales") +
        xlab("") + ylab("") + labs(fill="") +
        theme_redfin() + scale_fill_redfin()
)

ggplotly(
    gc <- ggplot(data_frame[data_frame$`Property Type` %in% c(
        # 'Single Family Residential'
        'Condo/Co-op'
        # 'Townhouse'
    ) 
    & data_frame$`Is New Construction` == 't' 
    & data_frame$Place %in% c("Seattle, WA", "Las Vegas, NV","Phoenix, AZ", "Portland, OR", "San Francisco, CA")
    ,], 
    aes(x=`Date Sold`, y=`Home Sales`, fill=`Place`)) + 
        geom_bar(stat="identity", position = "dodge") +
        ggtitle("New Condo Sales") +
        xlab("") + ylab("") + labs(fill="") +
        theme_redfin() + scale_fill_redfin()
)


ggplotly(g1 <- ggplot(seattle_historical[seattle_historical$`Is New Construction` == 't',], 
            aes(x=`Date Sold`, y=`Home Sales`, fill=`Property Type`)) + 
    geom_bar(stat="identity") +
    ggtitle("New Home Sales in Seattle") +
    xlab("") + ylab("") + labs(fill="") +#remove labels of axis and legends
    theme_redfin() + scale_fill_redfin())


# The price gap between newly constructed condos and existing has been widening over time
ggplotly(
    g2 <- ggplot(seattle_historical[seattle_historical$`Property Type` == 'Condo/Co-op',], 
                     aes(x=`Date Sold`, y=`Median Sale Price`, fill=`Is New Construction`)) + 
             geom_bar(stat="identity", position = "dodge") +
             ggtitle("Median New Home Price in Seattle") +
             scale_y_continuous(labels = scales::dollar, lim= c(0,650000)) +
             xlab("") + ylab("") + labs(fill="") +
             theme_redfin() + 
             scale_fill_redfin(limits=c("t","f"),labels=c("New Construction","Existing Condos"))
         )

ggplotly(
(inc <- ggplot(acs_2014_data_long[grepl("^Percent..INCOME.AND.BENEFITS", acs_2014_data_long$variable) 
                & acs_2014_data_long$value < 100,],
       aes(x=variable,y=value/100,fill=Geography)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Household Income Distribution") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("< $10k", "$10k-$15k","$15k-$25k","$25k-$35k","$35k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k-$200k","> $200k")) +
    xlab("") + ylab("") + labs(fill="") +
    theme_redfin() + scale_fill_redfin() +
    theme(axis.text.x = element_text(angle=20, hjust=1))
)
)

(inc_sea <- ggplot(acs_2014_data_long[grepl("^Percent..INCOME.AND.BENEFITS", acs_2014_data_long$variable) 
                                  & acs_2014_data_long$Geography == 'Seattle, WA'
                                  & acs_2014_data_long$value < 100,],
               aes(x=variable,y=value/100,fill=Geography)) + 
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Household Income Distribution of Seattle") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("< $10k", "$10k-$15k","$15k-$25k","$25k-$35k","$35k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k-$200k","> $200k")) +
    xlab("") + ylab("") + labs(fill="") +
    theme_redfin() + scale_fill_redfin(guide=FALSE) +
    theme(axis.text.x = element_text(angle=20, hjust=1)))



(price_dist <- ggplot(data_frame_raw[data_frame_raw$`Property Type` == 'Condo/Co-op' 
                                    & data_frame_raw$Place == 'Seattle, WA',]
                     , aes(x=interaction(`Is New Construction`,format(`Date Sold`,"%Y")), 
                           y=`Sale Price`, 
                           group=interaction(`Date Sold`,`Is New Construction`), 
                           col=`Is New Construction`)) +
    geom_boxplot(outlier.colour = NA) + 
    ggtitle("Sale Price Distribution of Condos in Seattle") +
    xlab("") + ylab("") + labs(col="") +
    scale_y_continuous(labels = scales::dollar,limits = c(0,2000000)) +
    theme_redfin() + 
    scale_x_discrete(labels = c("2010","'10","'11","'11","'12","'12","'13","'13","'14","'14","'15","'15","'16","2016")) +
    scale_colour_redfin(limits=c("t","f"),
                        labels=c("New Construction","Existing Condos")))


(price_dist_2015 <- ggplot(data_frame_raw[data_frame_raw$`Property Type` == 'Condo/Co-op' 
                                     & data_frame_raw$`Date Sold` == '2015-01-01'
                                     & data_frame_raw$Place == 'Seattle, WA',]
                      , aes(x=`Sale Price`/1000,fill = `Is New Construction`)) +
    geom_density(alpha=.8) + 
    ggtitle("Sale Price Distribution of Condos in Seattle in 2015") +
    xlab("") + ylab("") + labs(fill="") +
    scale_x_continuous(labels = scales::dollar_format(suffix = "k"),breaks = c(0,250,500,750,1000,1500,2000),lim=c(0,2500)) +
    scale_y_continuous(labels = c("","",""),breaks = c(0,.001,.002))+
    theme_redfin() + 
    theme(panel.grid = element_blank(),
          legend.position=c(1,0.5), legend.justification=c(1,0)) +
    scale_colour_redfin() + 
    scale_fill_redfin(limits=c("t","f"),
                      labels=c("New Construction","Existing Condos")))



(price_dist_2015_SF <- ggplot(data_frame_raw[data_frame_raw$`Property Type` == 'Condo/Co-op' 
                                          & data_frame_raw$`Date Sold` == '2015-01-01'
                                          & data_frame_raw$Place == 'San Francisco, CA',]
                           , aes(x=`Sale Price`/1000,fill = `Is New Construction`)) +
    geom_density(alpha=.8) + 
    ggtitle("Sale Price Distribution of Condos in San Francisco in 2015") +
    xlab("") + ylab("") + labs(fill="") +
    scale_x_continuous(labels = scales::dollar_format(suffix = "k"),breaks = c(0,1000,2000,3000,4000,5000),lim=c(0,6000)) +
    scale_y_continuous(labels = c("","",""),breaks = c(0,.001,.002))+
    theme_redfin() + 
    theme(panel.grid = element_blank(),
          legend.position=c(1,0.5), legend.justification=c(1,0)) +
    scale_colour_redfin() + 
    scale_fill_redfin(limits=c("t","f"),
                      labels=c("New Construction","Existing Condos")))


(price_dist_2015_OR <- ggplot(data_frame_raw[data_frame_raw$`Property Type` == 'Condo/Co-op' 
                                             & data_frame_raw$`Date Sold` == '2015-01-01'
                                             & data_frame_raw$Place == 'Portland, OR',]
                              , aes(x=`Sale Price`/1000,fill = `Is New Construction`)) +
    geom_density(alpha=.8) + 
    ggtitle("Sale Price Distribution of Condos in Portland, OR in 2015") +
    xlab("") + ylab("") + labs(fill="") +
    scale_x_continuous(labels = scales::dollar_format(suffix = "k"),breaks =  c(0,250,500,750,1000,1500,2000),lim=c(0,2000)) +
    scale_y_continuous(labels = c("","",""),breaks = c(0,.001,.002))+
    theme_redfin() + 
    theme(panel.grid = element_blank(),
          legend.position=c(1,0.5), legend.justification=c(1,0)) +
    scale_colour_redfin() + 
    scale_fill_redfin(limits=c("t","f"),
                      labels=c("New Construction","Existing Condos")))

(price_dist_pt <- ggplot(data_frame_raw[data_frame_raw$`Is New Construction` == 't'
                                     & data_frame_raw$Place == 'Seattle, WA',]
                      , aes(x=interaction(`Property Type`,format(`Date Sold`,"%Y")), 
                            y=`Sale Price`/1000, 
                            group=interaction(format(`Date Sold`,"%Y"),`Property Type`), 
                            col=`Property Type`)) +
    geom_boxplot(outlier.colour = NA) + 
    ggtitle("Sale Price Distribution of New Properties in Seattle") +
    xlab("") + ylab("") + labs(col="") +
    scale_y_continuous(labels = scales::dollar_format(suffix = "k"),limits = c(0,2000)) +
    scale_x_discrete(labels = c("","2010","","","'11","","","'12","","","'13","","","'14","","","'15","","","2016","")) +
    theme_redfin() + 
    scale_colour_redfin())


############################################
# Data and analysis exportation #
############################################

png(file = "New Single-Family Sales by City.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(gs), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "New Townhome Sales by City.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(gt), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "New Condo Sales by City.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(gc), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "New Home Sales in Seattle.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(g1), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Median New Home Sales Price in Seattle.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(g2), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Household Income Distribution by City.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(inc), sub = textGrob("Source: Census",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Household Income Distribution in Seattle.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(inc_sea), sub = textGrob("Source: Census",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Sale Price Distribution of Condos in Seattle.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(price_dist), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Sale Price Distribution of Condos in Seattle in 2015.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(price_dist_2015), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Sale Price Distribution of Condos in Portland in 2015.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(price_dist_2015_OR), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Sale Price Distribution of Condos in SF in 2015.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(price_dist_2015_SF), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

png(file = "Sale Price Distribution of New Properties in Seattle.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(price_dist_pt), sub = textGrob("Source: Redfin",x=0.1, gp = gpar(fontsize = 11, col = grey(.4))),my_g, heights=c(9,.35,.35))
dev.off()

# Reformat Year and Is New Construction columns
data_frame[,2] <- format(data_frame[,2],"%Y")
seattle_historical[,2] <- format(seattle_historical[,2],"%Y")

data_frame[data_frame$`Is New Construction`=="t",4] <- "TRUE"
data_frame[data_frame$`Is New Construction`=="f",4] <- "FALSE"
seattle_historical[seattle_historical$`Is New Construction`=="t",4] <- "TRUE"
seattle_historical[seattle_historical$`Is New Construction`=="f",4] <- "FALSE"

acs_2014_data_wide <- 
    dcast(acs_2014_data_long[grepl("^Percent..INCOME.AND.BENEFITS", acs_2014_data_long$variable)
                             & acs_2014_data_long$value < 100,3:5], Geography ~ variable)

acs_2014_data_wide <- setNames(acs_2014_data_wide, c("Geography", "< $10k", "$10k-$15k","$15k-$25k","$25k-$35k","$35k-$50k","$50k-$75k","$75k-$100k","$100k-$150k","$150k-$200k","> $200k"))

# Add in footnotes for excel file
comment(data_frame$`Median Family Income 2014`) <- "5-yr census (ACS) estimate for 2014"
comment(data_frame$`Total Population`) <- "5-yr census (ACS) estimate for 2014"
comment(seattle_historical$`Median Sale Price`) <- "About 30% of county sale records are missing sale price data"
comment(seattle_historical$`Date Sold`) <- "1990 - 2014; Data for 2015 is incomplete from County Records"
comment(acs_2014_data_wide$`< $10k`) <- "As Percent of Total Households"
comment(acs_2014_data_wide$Geography) <- "5-yr census (ACS) estimate for 2014"

# Save the cleaned output to an excel file
WriteXLS(c("data_frame","seattle_historical","acs_2014_data_wide"),
         ExcelFileName = "./UW-New-Condo-Analysis.xlsx",
         SheetNames = c("City-Level-Data","Seattle-Historical-Data","Income-Distributions"),
         row.names = FALSE,
         AdjWidth = TRUE,
         AutoFilter = FALSE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE
)
