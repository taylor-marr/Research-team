## Connecting to Redfin Database in R

# create a PostgreSQL instance and create one connection.

# install.packages("RJDBC")
require(RPostgreSQL)
require(RJDBC)

# Set up credentials to be used for Corp-Query-5
usr <- "taylor.marr"
pwd <- {
    readChar("~/Google Drive/Personal/R/.pw",11)
}

drv <- JDBC("org.postgresql.Driver",
            "/users/taylor.marr/downloads/postgresql-9.4-1204.jdbc4.jar", "`")

url <- paste0("jdbc:postgresql://corp-query-5.redfintest.com:5432/pr?ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory","&user=",usr,"&password=",pwd)
# open the connection using user, passsword, etc., as
con <- dbConnect(drv, url)

rm(pwd) # removes the password
rm(url)
rm(usr)

# dbExistsTable(con, "listings") # Check if a specific table exists
# # dbListTables(con) # View all tables
# 
# # Example Query
# df <- dbGetQuery(con, statement = 
#     "
#     SELECT *
#     FROM listings
#     LIMIT 10
#     ")

# Set up credentials to be used for Redshift
usr <- "taylor.marr"
rspwd <- {
    readChar("~/Google Drive/Personal/R/.redshiftpw",10)
}

drv <- JDBC("org.postgresql.Driver",
            "/users/taylor.marr/downloads/postgresql-9.4-1204.jdbc4.jar", "`")

rsurl <- paste0("jdbc:postgresql://10.0.7.23:5439/rmus_prod?tcpKeepAlive=true&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory","&user=",usr,"&password=",rspwd)

# open the connection using user, passsword, etc., as
rscon <- dbConnect(drv, rsurl)

rm(rspwd) # removes the password
rm(rsurl)
rm(usr)

dbExistsTable(rscon, "listings") # Check if a specific table exists

# Set up credentials to be used for Stingray
usr <- "taylor.marr"
srpwd <- {
    readChar("~/Google Drive/Personal/R/.pw",11)
}

drv <- JDBC("org.postgresql.Driver",
            "/users/taylor.marr/downloads/postgresql-9.4-1204.jdbc4.jar", "`")

stingrayurl <- paste0("jdbc:postgresql://10.0.7.22:5432/stingray_prod?tcpKeepAlive=true&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory","&user=",usr,"&password=",srpwd)

# open the connection using user, passsword, etc., as
srcon <- dbConnect(drv, stingrayurl)
rm(srpwd) # removes the password
rm(stingrayurl)
rm(usr)

dbExistsTable(srcon, "region_walkscores") # Check if a specific table exists



