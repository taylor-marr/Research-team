#So Cal Data for LA Times

# LA Metro, OC Metro, IE Metro and San DIego Metro is best since her has begun covering "So Cal". Here s his note:

# "I want to do a story looking at why we’ve had such tight inventory for so long, though I’d like to get a better sense of how much listings are increasing at the moment just in case they are surging and maybe the tight inventory is ending and this isn’t a big deal.
 
# I would expect March listing data from you guys won’t be out for a while, but is there any way to get a sense what is going on now?"


#Load Packages
setwd("~/Google Drive/Personal/R/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-03 Misc/")

dbExistsTable(con, "listings")
library(WriteXLS)

# Example Query
so_cal_data <- read.csv("so-cal-data.csv")
# so_cal_data <- dbGetQuery(con, statement = 
#                      "
# with a as (
# select redfin_metro, period, months_of_supply, new_listings, inventory, homes_sold, median_list_price, avg_sale_to_list, avg_inventory_age_days from market_data_metro_current
# where redfin_metro in ('Los Angeles, CA', 'San Diego, CA', 'Orange County, CA', 'Riverside-San Bernardino, CA')
# and period::date <= '2016-03-01'
# and period::date >= '2010-01-01'
# order by 1,2
# ), b as (
# select
#    date_trunc('month', h.min_date::date) as Month
#    , m.redfin_metro
#    , count(distinct l.property_id) as new_listings_2
# from months h
# join listings l on coalesce(listing_date, listing_added_date) >= h.min_date and coalesce(listing_date, listing_added_date) <= h.max_date
# join metro m on m.county_id = l.county_id
# where h.min_date >= '2010-01-01' and h.max_date < date_trunc('month', now())
# and l.listing_type_id = 1
# and l.property_type_id in (3,6,13)
# and m.redfin_metro in ('Los Angeles, CA', 'San Diego, CA', 'Orange County, CA', 'Riverside-San Bernardino, CA')
# group by 1, 2)
# select a.*, b.new_listings_2
# from a 
# join b on a.redfin_metro = b.redfin_metro and a.period::date = b.Month::date
#     ")

# View results
dim(so_cal_data)
head(so_cal_data)
tail(so_cal_data)
str(so_cal_data)

so_cal_data[so_cal_data$period=='2016-03-01',3:9] <- NA

# Clean up dates to remove timestamp and set to monthly
head(so_cal_data[,2])
so_cal_data[,2] <- format(as.Date(so_cal_data[,2]),"%b-%Y")

so_cal_data$new_listings <- so_cal_data$new_listings_2
so_cal_data$new_listings_2 <- NULL
head(so_cal_data)

# function to title case the column names
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- colnames(so_cal_data)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(so_cal_data) <- cn
colnames(so_cal_data)

#Load in logo for graphs
img <- readPNG("~/Google Drive/Personal/R/Redfin-Logo-Web.png", FALSE)
my_g <- grobTree(rasterGrob(img, interpolate=TRUE, x=1, hjust=1))

#Visual Analysis for Feb Year over Year comparison
df_mar <- so_cal_data[grepl("^Mar", so_cal_data$Period),]
df_mar$`New Listings YoY` <- with(df_mar, ave(as.numeric(`New Listings`), `Redfin Metro`, FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))*100


g <- ggplot(df_mar[df_mar$Period!='Mar-2010',], aes(x=`Period`, y=`New Listings YoY`, fill=`Redfin Metro`)) + 
    geom_bar(stat="identity", position = "dodge") +
    ggtitle("New Listings, Annual Percent Change") +
    xlab("") + ylab("") + labs(fill="") +#remove labels of axis and legends
    theme_redfin() + scale_fill_redfin()

grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))

png(file = "YoY Change in New Listings by Metro.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(g),my_g, heights=c(9,.35))
dev.off()


# Save the cleaned output to an excel file
WriteXLS(so_cal_data,
         ExcelFileName = "./so-cal-data.xlsx",
         SheetNames = "Data",
         row.names = FALSE,
         AdjWidth = TRUE,
         AutoFilter = TRUE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE
)
