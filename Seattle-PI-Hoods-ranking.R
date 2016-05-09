# Neighborhood Data Pull

source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")

require(ggplot2)
require(ggthemes)
require(grid)
require(gridExtra)
require(png)


img <- readPNG("./Redfin-Logo-Web.png", FALSE)
my_g <- grobTree(rasterGrob(img, interpolate=TRUE, x=1, hjust=1))

setwd("./../../PRTeam Analysis/Misc/2016-02 Misc/Seattle-PI-Neighborhoods/")

q_transit <- "select neighborhood_id,
transit_score 
from neighborhoods 
left join region_walkscores on region_type_id=1 and neighborhood_id = region_table_id
where market_id = 1 and maponics_ncs_code NOT ILIKE '50%'
and transit_score is not null"

transit_scores <- dbGetQuery(srcon, statement = q_transit)
# legit_hoods <- dbGetQuery(con, statement = "select neighborhood_id from places_and_neighborhoods where place_id = 16163")

hoods <- read.csv("./Inv_Prices_by_hood.csv")
# Remove hoods that are above Seattle's median price range to get affordable hoods

hoods <- hoods[hoods$median_sale_price <= 530000,]
hoods <- hoods[hoods$neighborhood_id != 185308,] # Portage Bay (low med price)
hoods <- hoods[hoods$neighborhood_id != 184492,] # Westlake (small)
hoods <- hoods[hoods$neighborhood_id != 185480,] # brighton
hoods <- hoods[hoods$neighborhood_id != 2558,] # south park
hoods <- merge(hoods, transit_scores)
hoods$rank_transit <- rank(-hoods$transit_score)
hoods$rank_inventory <- rank(-hoods$inventory)
hoods$rank_prices <- rank(hoods$median_sale_price)

hoods$final_rank <- rank(hoods$rank_transit + hoods$rank_inventory + 2*hoods$rank_prices)

hoods <- hoods[order(hoods$final_rank),]

head(hoods)




# function to title case the column names
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- colnames(hoods)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(hoods) <- cn

WriteXLS(hoods,
         ExcelFileName = "./neighborhoods-ranked.xlsx",
         SheetNames = "neighborhoods",
         row.names = FALSE,
         AutoFilter = TRUE,
         AdjWidth = TRUE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         FreezeCol = 4,
         verbose = TRUE)


