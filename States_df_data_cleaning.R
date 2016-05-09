getwd()
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-04 Misc/Value of Owner-Occupied Housing in Swing Districts/State-Place-Data")

library(dplyr); library(tidyr)

NY <- read.csv("NY aggregate value dat.csv", stringsAsFactors = FALSE)
TX <- read.csv("Texas places aggregates.csv", stringsAsFactors = FALSE)
VT <- read.csv("Vermont aggregate.csv", stringsAsFactors = FALSE)
OH <- read.csv("ohio places aggregate.csv", stringsAsFactors = FALSE)
IN <- read.csv("indiana places aggregate.csv", stringsAsFactors = FALSE)

states <- list(NY, TX, VT, OH, IN)

RemoveTail <- function(x) {
    x[1:(nrow(x)-1),]
}

# Clean each of the spreadsheets
states_df <- data.frame()
for(i in states) {
    i <- i[,1:4]
    i <- RemoveTail(i)
    names(i) <- c("Id", "Id2", "Geography", "Housing_Value")
    states_df <- rbind(states_df, i)
}

dim(states_df)
glimpse(states_df)

states_df <- separate(states_df, Geography, into = c("City", "State"), sep = ", ", extra = "merge", fill = "left")

states_df$City_Clean <- str_replace(states_df$City, " village| CDP| CCD| city| town", "")

# Need to remove county names in parentheses
# states_df$City_Clean <- str_replace(str_detect(states_df$City, " ("), "")

states_df[str_detect(states_df$State, "County"), 4] <- "Texas"

states_df$Housing_Value <- str_replace(states_df$Housing_Value, fixed("$"), "")
states_df$Housing_Value <- str_replace_all(states_df$Housing_Value, ",", "")
states_df$Housing_Value <- as.numeric(states_df$Housing_Value)

states_df_NA <- states_df
states_df <- states_df[complete.cases(states_df),]

dim(states_df)
glimpse(states_df)
tail(states_df)
head(states_df)




states_df$Trump[states_df$State == "New York"] <- 0
states_df$Clinton[states_df$State == "New York"] <- 0
states_df$Sanders[states_df$State == "Vermont"] <- 0
states_df$Kasich[states_df$State == "Ohio"] <- 0
states_df$Cruz[states_df$State == "Texas"] <- 0

states_df$Trump[states_df$Housing_Value <= 4500000000 & states_df$State == "New York"] <- 1
states_df$Clinton[states_df$Housing_Value <= 256400000 & states_df$State == "New York"] <- 1
states_df$Sanders[states_df$Housing_Value <= 180000000 & states_df$State == "Vermont"] <- 1
states_df$Kasich[states_df$Housing_Value <= 29000000 & states_df$State == "Ohio"] <- 1
states_df$Cruz[states_df$Housing_Value <= 142000000 & states_df$State == "Texas"] <- 1

states_df$Trump[states_df$State == "Indiana"] <- 0
states_df$Clinton[states_df$State == "Indiana"] <- 0
states_df$Sanders[states_df$State == "Indiana"] <- 0
states_df$Kasich[states_df$State == "Indiana"] <- 0
states_df$Cruz[states_df$State == "Indiana"] <- 0

states_df$Trump[states_df$Housing_Value <= 4500000000 & states_df$State == "Indiana"] <- 1
states_df$Clinton[states_df$Housing_Value <= 256400000 & states_df$State == "Indiana"] <- 1
states_df$Sanders[states_df$Housing_Value <= 180000000 & states_df$State == "Indiana"] <- 1
states_df$Kasich[states_df$Housing_Value <= 29000000 & states_df$State == "Indiana"] <- 1
states_df$Cruz[states_df$Housing_Value <= 142000000 & states_df$State == "Indiana"] <- 1

colSums(select(states_df, Trump:Cruz), na.rm = TRUE)

write.csv(states_df, "./states_aggregated.csv")

library(WriteXLS)

WriteXLS("states_df",
         ExcelFileName = "./states_aggregated.xlsx",
         row.names = FALSE,
         AdjWidth = TRUE,
         AutoFilter = FALSE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE
)
