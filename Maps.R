#  Working with Maps
library("maps")
library("mapproj")
us <- fortify(map_data("state"), region = "region")

ggplot() +
    geom_map(data  =  us, map = us,
             aes(x = long, y = lat, map_id = region, group = group),
             fill = "white", color = "black", size = 0.25) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme_map()

# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
if(require(mapproj)) {	# mapproj is used for  projection="polyconic"
    # color US county map by 2009 unemployment rate
    # match counties to map using FIPS county codes
    # Based on J's solution to the "Choropleth Challenge"
    # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
    
    # load data
    # unemp includes data for some counties not on the "lower 48 states" county
    # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
    #  cities
    data(unemp)
    data(county.fips)
    
    # define color buckets
    colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
    unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
    leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
    
    # align data with map definitions by (partial) matching state,county
    # names, which include multiple polygons for some counties
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                        county.fips$polyname)]
    colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
    
    # draw map
    map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
        lty = 0, projection = "polyconic")
    map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title("unemployment by county, 2009")
    legend("topright", leg.txt, horiz = TRUE, fill = colors)
    
    # Choropleth Challenge example, based on J's solution, see:
    # http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
    # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}

data(us.cities)
map.cities(us.cities)

# Mapping Crimes
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
if (require(maps)) {
    states_map <- map_data("state")
    ggplot(crimes, aes(map_id = state)) +
        geom_map(aes(fill = Murder), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat)
    
    last_plot() + coord_map()
    ggplot(crimesm, aes(map_id = state)) +
        geom_map(aes(fill = value), map = states_map) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        facet_wrap( ~ variable)
}