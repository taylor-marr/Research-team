# Market Data for Blog Post About New Builder Services Project


# The area has an ok Walk Score (67) and the transit score is pretty high. I think Eric already has home prices and Walk Score data by neighborhood. I’d like to say something like, “There are just x affordable neighborhoods with transit scores above 50”

# 500-meter grid, 74% of Seattle's affordable areas have a Walk Score of 50 or higher
# using the 500-meter grid, 57% of Seattle's affordable areas have a Transit Score of 50 or higher

getwd()
setwd("/Users/taylor.marr/Google Drive/PRTeam Analysis/Misc/2016-01 Misc/")

library(readr)
library(data.table)

files <- list.files("./")
print(files)    

inventory <- read_tsv(file = "neighborhood_monthly_inventory.txt")
listings <- read_tsv(file = "neighborhood_monthly_listings.txt")
prop_trans <- read_tsv(file = "neighborhood_monthly_property_transactions.txt")
sales <- read_tsv(file = "neighborhood_monthly_sales.txt")

# Market Tracker data for Rainier Beach, plus average days on market, demand, etc. Can we have the data for each month in 2015?

# Rainier Beach neighborhoo_id = 2245
rb_inv <- inventory[inventory$neighborhood_id == 2245,]
rb_listings <- listings[listings$neighborhood_id == 2245,]
rb_prop_trans <- prop_trans[prop_trans$neighborhood_id == 2245,]
rb_sales <- sales[sales$neighborhood_id == 2245,]

mergedData = merge(rb_inv,rb_sales,all = TRUE)
mergedData2 = merge(rb_listings,rb_prop_trans,all = TRUE)
rb_merged = merge(mergedData,mergedData2,all = TRUE)

rb_merged <- rb_merged[rb_merged$period >= "2015-01-01",]

#remove the colums on condo data as it is largely null
result <- grep("condo", names(rb_merged))
rb_merged <- rb_merged[,-result]
result <- grep("_sfr", names(rb_merged))
rb_merged <- rb_merged[,-result]

write_csv(rb_merged, "./rainier_beach_data.csv")

# City-wide Inventory of homes for sale and homes sold in the $300Ks. I’d like to go back a few years. Ideally, the data lets us say something like, “Across Seattle last year, only xxx homes in the $300K range were on the market. And they only stayed on the market for an average of x days”

# ran sql query and saved output to wd
seattle_inv <- read.csv(file = "seattle_300k_inventory.csv")

homes_sold <- seattle_inv[4, 6]
days <- seattle_inv[4, 7]
names(sales)
sea_ttl_homes_sold <- sum(sales[sales$period >= "2015-01-01" & sales$redfin_metro == "Seattle, WA",18])
homes_sold
homes_sold_aspercent <- round(homes_sold/sea_ttl_homes_sold,4)*100

days

# Can we look at the housing stock. These are brand new townhomes with pretty high-end features. Is the inventory in Rainier Beach older single family homes? Is there a lack of townhomes and condos? Are all the houses old? I’d like to say something about these being one of a few and/or new townhomes available in the area.

rb_years <- read.csv(file = "rb_year_built.csv")
str(rb_years)
rb_years
homes <- sum(rb_years$count)
rb_years$oftotal <- round(rb_years$count/homes,3)*100

write.csv(rb_years, "./rb_year_built.csv")

rb_prices <- read.csv(file = "rb_price_bins.csv")
str(rb_prices)
rb_prices
homes_p <- sum(rb_prices$count)
rb_prices$oftotal <- round(rb_prices$count/homes_p,3)*100

write.csv(rb_prices, "./rb_year_built.csv")
