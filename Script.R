getwd()
setwd("~/Google Drive/PRTeam Analysis/R Scripts/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-04 Misc/Work From Home/")
list.files()

library(dplyr); library(tidyr); library(stringr); library(WriteXLS)
dbExistsTable(con, "listings")

metro_map <- dbGetQuery(con, "select * from metro")

Income <- read.csv("Data USA - Map of Yearly Income by County.csv", stringsAsFactors = FALSE)
# Population <- read.csv("Data USA - Map of Population by County.csv", stringsAsFactors = FALSE)
Commute <- read.csv("Data USA - Map of Average Travel Time by County.csv", stringsAsFactors = FALSE)
home_prices <- read.csv("Market-Tracker-Metro-March-2016-for-Download.csv", stringsAsFactors = FALSE)

full_data <- full_join(Income, Commute)

full_data <- separate(full_data, geo, into = c("blank", "fips"), sep = "05000US", remove = FALSE, fill = "right")
full_data$blank <- NULL

full_data <- inner_join(full_data, metro_map)

full_data <- arrange(full_data, redfin_metro, population)

full_data <- tbl_df(full_data)
glimpse(full_data)

nrow(full_data[is.na(full_data$population),])

metro_pop <- full_data %>%
    group_by(redfin_metro) %>%
    summarise(metro_pop = sum(population))


full_data <- full_join(full_data, metro_pop)
rm(metro_pop)

avg_commute_metro <- full_data %>%
    mutate(pop_portion = population / metro_pop, metro_commute = mean_commute_minutes * pop_portion) %>%
    group_by(redfin_metro) %>%
    summarise(metro_commute = sum(metro_commute), min_commute_metro = min(mean_commute_minutes), max_commute_metro = max(mean_commute_minutes))

full_data <- full_join(full_data, avg_commute_metro)
rm(avg_commute_metro)

avg_income_metro <- full_data %>%
    mutate(pop_portion = population / metro_pop, metro_income = income * pop_portion) %>%
    group_by(redfin_metro) %>%
    summarise(metro_income = sum(metro_income))

full_data <- full_join(full_data, avg_income_metro)
rm(avg_income_metro)


full_data %>%
    select(geo_name, mean_commute_minutes, redfin_metro, metro_commute, min_commute_metro, max_commute_metro)

metro_summaries <- unique(full_data %>%
           select(redfin_metro, metro_income, metro_pop, metro_commute, min_commute_metro, max_commute_metro) %>%
            arrange(desc(metro_commute)))



    
metro_summaries <- inner_join(home_prices[,2:3], metro_summaries, by = c("Metro" = "redfin_metro"))

metro_summaries$Median.sale.price <- str_replace(metro_summaries$Median.sale.price, fixed("$"), "")
metro_summaries$Median.sale.price <- str_replace_all(metro_summaries$Median.sale.price, ",", "")
metro_summaries$Median.sale.price <- as.numeric(metro_summaries$Median.sale.price)
metro_summaries <- arrange(metro_summaries, desc(Median.sale.price))

# metro_summaries$Median.sale.price <- scales::dollar(metro_summaries$Median.sale.price)
# metro_summaries$metro_income <- scales::dollar(metro_summaries$metro_income)

# summarise(metro_summaries, median_commute = median(metro_commute))
metro_summaries$below_avg_commute <- FALSE
metro_summaries$below_avg_commute[metro_summaries$metro_commute <= 24] <- TRUE

write.csv(full_data, file = "./full_data.csv")

WriteXLS("metro_summaries",
         ExcelFileName = "./metro_summaries.xlsx",
         row.names = FALSE,
         AdjWidth = TRUE,
         AutoFilter = FALSE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE
)
