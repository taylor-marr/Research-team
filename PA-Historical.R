# Historical Data for PA cities

setwd("~/Google Drive/Personal/R/")
source("./RedfinTheme.R")
source("./Connecting to Redfin db in R.R")

setwd("~/Google Drive/PRTeam Analysis/Misc/2016-02 Misc/")
if(!file.exists("./PA-Historical-data/")){dir.create("./PA-Historical-data/")}
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-02 Misc/PA-Historical-data/")


city_data <- dbGetQuery(con,statement = "-- normalization ratios
with t as (
select count(t.property_id) as sales, median(sale_price) as price, date_trunc('year',t.sale_date) as period, p.city || p.state_code as place
from listings t join properties p on p.property_id = t.property_id
where p.city in ('Yeadon','Levittown' )
and p.state_code = 'PA'
and p.property_type_id in (3,4,6,13)
and listing_type_id = 1
and sale_date >= '2014-01-01'
group by 3,4
order by 4,3)
, h as (
-- Historical Data
select count(t.property_id) as sales, median(sale_price) as price, date_trunc('year',t.sale_date) as period, p.city || p.state_code as place
from property_transactions t join properties p on p.property_id = t.property_id
where p.city in ('Yeadon','Levittown')
and p.state_code = 'PA'
and transaction_type_code = 'S'
and property_type_id in (3,4,6,13)
and sale_date >= '2000-01-01'
group by 3,4
order by 4,3
)
, s as (
select h.*
, round(t.sales::numeric / t2.sales - 1, 3) as homes_sold_yoy
from h
left join t on  h.period = t.period and t.place = h.place
left join t t2 on t2.period = t.period - interval '1 year' and t.place = t2.place
order by 2,1)

select *
from s
order by place, period")


head(city_data)
city_data$period <- as.Date(city_data$period)
city_data[city_data$period=="2015-01-01",1] <- round(city_data[city_data$period=="2014-01-01",1]*city_data[!is.na(city_data$homes_sold_yoy),5]+city_data[city_data$period=="2014-01-01",1])

city_data

city_data <- city_data[,1:4]
city_data[city_data$place=="LevittownPA",4] <- "Levittown, PA"
city_data[city_data$place=="YeadonPA",4] <- "Yeadon, PA"

Sales_chart <- ggplot(city_data,aes(x=period,y=sales)) +
        geom_bar(stat="identity",position="dodge",aes(fill=city_data$place),size=3,alpha=1) + 
        theme_redfin() + 
        scale_fill_redfin() +
        labs(x="Year",y="",fill="") +
        ggtitle("Annual Home Sales")

Sales_chart

Prices_chart <- ggplot(city_data,aes(x=period,y=price/1000)) +
    geom_bar(stat="identity",position="dodge",aes(fill=city_data$place),size=3,alpha=1) + 
    theme_redfin() + 
    scale_fill_redfin() +
    labs(x="Year",y="",fill="") +
    ggtitle("Annual Median Sale Price\n(in thousands)")

Prices_chart

logo <- readPNG("~/Google Drive/Personal/R/Redfin-Logo-Web.png", FALSE)
my_grid <- grobTree(rasterGrob(logo, interpolate=TRUE, x=1, hjust=1))

png(file = "Annual Home Sales.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(Sales_chart),my_grid, heights=c(9,.35))
dev.off()

png(file = "Annual Median Sale Price.png", width = 675, bg = "white")
grid.arrange(arrangeGrob(Prices_chart),my_grid, heights=c(9,.35))
dev.off()

# function to title case the column names
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- colnames(city_data)
cn <- gsub("_", " ", cn)
cn <- sapply(cn, simpleCap)
colnames(city_data) <- cn
colnames(city_data)

library(WriteXLS)
Yeadon<-city_data[city_data$Place=="Yeadon, PA",]
Levittown<-city_data[city_data$Place=="Levittown, PA",]

WriteXLS(c("Levittown","Yeadon"),
         ExcelFileName = "./Historical-PA-Data.xlsx",
         SheetNames = c("Levittown","Yeadon"),
         row.names = FALSE,
         AdjWidth = TRUE,
         BoldHeaderRow = TRUE,
         FreezeRow = 1,
         verbose = TRUE)
