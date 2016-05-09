getwd()
setwd("~/Google Drive/PRTeam Analysis/R Scripts/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-04 Misc/Tendril Energy Data")

library(dplyr); library(tidyr); library(stringr); library(WriteXLS)
dbExistsTable(con, "listings")

list.files()
Income <- read.csv("Data USA - Map of Yearly Income by County.csv", stringsAsFactors = FALSE)
metro_map <- dbGetQuery(con, "select * from metro")

full_data <- separate(Income, geo, into = c("blank", "fips"), sep = "05000US", remove = FALSE, fill = "right")
full_data$blank <- NULL
full_data <- inner_join(full_data, metro_map)

metro_pop <- full_data %>%
    group_by(redfin_metro) %>%
    summarise(metro_pop = sum(population))

full_data <- full_join(full_data, metro_pop)
rm(metro_pop)

avg_income_metro <- full_data %>%
    mutate(pop_portion = population / metro_pop, metro_income = income * pop_portion) %>%
    group_by(redfin_metro) %>%
    summarise(metro_income = sum(metro_income))

full_data <- full_join(full_data, avg_income_metro)
rm(avg_income_metro)

metro_income <- unique(full_data %>%
                              select(redfin_metro, metro_income, metro_pop) %>%
                              arrange(desc(metro_income)))

Energy <- read.csv("List of Redfin Metros_EnergyCosts.csv", stringsAsFactors = FALSE)
home_prices <- dbGetQuery(con, statement = "select redfin_metro, median_sale_price from market_data_metro_current where period = '2016-04-01' and median_sale_price is not null")

metro_data <- inner_join(home_prices, Energy, by = c("redfin_metro" = "redfin_metro_alias"))

glimpse(metro_data)

# Transform population to numeric
metro_data$metro_population <- str_replace_all(metro_data$metro_population, ",", "")
metro_data$metro_population <- as.numeric(metro_data$metro_population)

# Transform Energy Cost to numeric
metro_data$Annual.Energy.Costs <- str_replace(metro_data$Annual.Energy.Costs, fixed("$"), "")
metro_data$Annual.Energy.Costs <- str_replace_all(metro_data$Annual.Energy.Costs, ",", "")
metro_data$Annual.Energy.Costs <- as.numeric(metro_data$Annual.Energy.Costs)

glimpse(metro_data)

metro_data <- inner_join(metro_income[,1:2], metro_data)

# Expected Mortgage with 20% down at 4% interest for a 30-year fixed
metro_data <- metro_data %>%
    mutate(loan_amount = median_sale_price - median_sale_price*.2,
           monthly_mortgage = loan_amount*((0.04/12)+(0.04/12)/((1+(0.04/12))^360 - 1)), 
           annual_mortage_cost = monthly_mortgage*12,
           energy_over_mortgage = Annual.Energy.Costs/annual_mortage_cost)

metro_data <- metro_data %>%
    mutate(energy_to_income_ratio = Annual.Energy.Costs/metro_income) %>%
    arrange(energy_to_income_ratio)

head(metro_data, 10)
tail(metro_data, 10)

write.csv(metro_data, "metro_energy_mortgage_cost.csv", row.names = FALSE)

metro_data <- read.csv("metro_energy_mortgage_cost.csv", stringsAsFactors = FALSE)

glimpse(metro_data)
metro_data <- arrange(metro_data, energy_over_mortgage)

metro_data$metro_income <- scales::dollar(metro_data$metro_income)
metro_data$median_sale_price <- scales::dollar(metro_data$median_sale_price)
metro_data$metro_population <- scales::comma(metro_data$metro_population)
metro_data$Annual.Energy.Costs <- scales::dollar(metro_data$Annual.Energy.Costs)
metro_data$loan_amount <- scales::dollar(metro_data$loan_amount)
metro_data$monthly_mortgage <- scales::dollar(metro_data$monthly_mortgage)
metro_data$annual_mortage_cost <- scales::dollar(metro_data$annual_mortage_cost)
metro_data$energy_over_mortgage <- scales::percent(round(metro_data$energy_over_mortgage,5))
metro_data$energy_to_income_ratio <- scales::percent(round(metro_data$energy_to_income_ratio,4))

metro_data$Annual_Energy_Costs <- metro_data$Annual.Energy.Costs
metro_data$Annual.Energy.Costs <- NULL

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- colnames(metro_data)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(metro_data) <- cn
colnames(metro_data)

glimpse(metro_data)
WriteXLS("metro_data",
         ExcelFileName = "./metro_energy_mortgage_cost.xlsx",
         row.names = FALSE,
         AdjWidth = TRUE,
         AutoFilter = FALSE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE
)
