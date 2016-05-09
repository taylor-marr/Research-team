# Redfin Branded theme and colors for marking graphs in R
require(ggplot2)
require(ggthemes)
require(grid)
require(gridExtra)
require(png)

# Functions for redfin colors and themes:
redfin_color_pal <- function(fill=TRUE) {
    redfin_colors <- c(
        redfin_red = "#A02021", 
        dark_red = "#710F11", 
        redfin_gray = "#808285", 
        white = "#FFFFFF", 
        darkest_gray = "#333333", 
        dark_gray = "#585858", 
        mid_gray = "#999999", 
        light_gray = "#E2E2E2", 
        lightest_gray = "#F5F5F5", 
        black = "#000000", 
        blue = "#1876a6", 
        light_orange = "#e96727", 
        orange = "#f15a22", 
        yellow = "#fcaf2a", 
        lime_green = "#bbcc39"
    )
    if (fill) {
        function(n) {
                if (n == 1) {
                i <- "redfin_red"
            } else if (n == 2) {
                i <- c("redfin_red", "redfin_gray")
            } else if (n == 3) {
                i <- c("redfin_red", "redfin_gray", "blue")
            } else if (n == 4) {
                i <- c("redfin_red", "blue", "orange", "lime_green")
            } else if (n %in% 5:6) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "redfin_gray")
            } else if (n == 7) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "light_orange", "redfin_gray")
            } else if (n >= 8) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "light_orange", "redfin_gray", "dark_red")
            }
            unname(redfin_colors[i][seq_len(n)])
        }
    } else {
        function(n) {
            if (n == 1) {
                i <- "redfin_red"
            } else if (n == 2) {
                i <- c("redfin_red", "redfin_gray")
            } else if (n == 3) {
                i <- c("redfin_red", "blue", "lime_green")
            } else if (n == 4) {
                i <- c("redfin_red", "blue", "orange", "lime_green")
            } else if (n %in% 5:6) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "redfin_gray")
            } else if (n == 7) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "light_orange", "redfin_gray")
            } else if (n >= 8) {
                i <- c("redfin_red", "blue", "orange", "lime_green", "yellow", "light_orange", "redfin_gray", "dark_red")
            }
            unname(redfin_colors[i][seq_len(n)])
        }
    }
}

scale_colour_redfin <- function(...) {
    discrete_scale("colour", "economist", redfin_color_pal(), ...)
}

scale_fill_redfin <- function(...) {
    discrete_scale("fill", "economist", redfin_color_pal(), ...)
}

theme_redfin <- function(base_size = 10, base_family = "sans") {
    redfin <- theme(
        line = element_line(colour = "black", size = 0.5,
                            linetype = 1, lineend = "butt"),
        text = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size, hjust = 0.5, vjust = 0.5,
                            angle = 0, lineheight = 1, color = "black",
                            margin = unit(c(.25, .1, .10,.1), "cm")
                            , debug = FALSE),
        
        ## Axis
        axis.line = element_line(size = rel(0.8)),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 0),
        axis.ticks = element_line(colour="gray90"),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(angle = 90),
        axis.ticks.length = unit(-base_size * 0.5 , "points"),
        
        # Legend
        legend.background = element_rect(linetype=0),
        legend.margin = unit(base_size * 1.5, "points"),
        legend.key = element_rect(linetype=0),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(1.25)),
        legend.text.align = NULL,
        legend.title = element_text(size = rel(1),  hjust = 0),
        legend.title.align = NULL,
        legend.position = "bottom",
        legend.direction = NULL,
        legend.justification = "left",
        
        ## Panel
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.grid.minor.y = element_line(colour = "gray90"),
        panel.background = element_blank(), 
        
        panel.margin = unit(0.25, "lines"),
        
        strip.text = element_text(size = rel(1.25)),
        strip.text.y = element_text(angle = -90),
        
        plot.title = element_text(size = rel(1.5), hjust=.5, face="plain"),
        aspect.ratio = 0.6,
        plot.margin = unit(c(.25, .1, .10,.1), "cm"),
        complete = TRUE)
    redfin
}

