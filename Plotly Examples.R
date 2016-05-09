# Plotly Testing

source("./RedfinTheme.R")

setwd("~/Google Drive/Personal/R/")
install.packages("plotly")
library(plotly)
install.packages("dplyr")
library(dplyr)

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

# Use ggplotly for ggplot2 styled graphs:
p <- ggplot(data = d, aes(x = carat, y = price)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 2) +
    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut) +
    theme_redfin() +
    scale_fill_redfin() + 
    scale_colour_redfin()

(gg <- ggplotly(p))

str(p <- plot_ly(economics, x = date, y = unemploy))

# Annotating with trend line
p %>%
    add_trace(y = fitted(loess(unemploy ~ as.numeric(date)))) %>%
    layout(title = "Median duration of unemployment (in weeks)",
           showlegend = FALSE) %>%
    dplyr::filter(unemploy == max(unemploy)) %>%
    layout(annotations = list(x = date, y = unemploy, text = "Peak", showarrow = T))

