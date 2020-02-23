#!/usr/bin/env Rscript

# Author: Christian Brueffer <christian@brueffer.io>
# License: MIT

# Plots the SCAN-B site map
#
# Inspiration:
# https://github.com/reinholdsson/swemaps
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(dplyr)
library(magrittr)
library(mapdata)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(swemaps)  # devtools::install_github("reinholdsson/swemaps")


# Latitude/longitude data from https://latitudelongitude.org/se
sites = as_tibble(rbind(
    c("Lund", 55.70584, 13.19321, 55.70584, 13.9),
    c("Malmö", 55.60587, 13.00073, 55.60587, 12.15),
    c("Kristianstad", 56.03129, 14.15242, 55.96, 15.57),
    c("Helsingborg", 56.04673, 12.69437, 56.27, 12),
    c("Karlskrona", 56.16156, 15.58661, 56.37, 16.65),
    c("Halmstad", 56.67446, 12.85676, 56.9, 12.85676),
    c("Växjö", 56.87767, 14.80906, 57, 15.53),
    c("Jönköping (Jul 2015)", 57.78145, 14.15618, 58.05, 14.15),
    c("Uppsala (Oct 2013)", 59.85882, 17.63889, 60.1, 17.63)
))
colnames(sites) = c("label", "lat", "lon", "label.lat", "label.lon")
sites$lat <- as.numeric(sites$lat)
sites$lon <- as.numeric(sites$lon)
sites$label.lat <- as.numeric(sites$label.lat)
sites$label.lon <- as.numeric(sites$label.lon)

detail_counties = map_ln %>%
    dplyr::select(c(lnkod, lnnamn)) %>%
    unique() %>%
    dplyr::filter(lnnamn %in% c(
        "Skåne län",
        "Blekinge län",
        "Kalmar län",
        "Kronobergs län",
        "Jönköpings län",
        "Östergötlands län",
        "Södermanlands län",
        "Västra Götalands län",
        "Värmlands län",
        "Västmanlands län",
        "Örebro län",
        "Gotlands län",
        "Hallands län",
        "Uppsala län",
        "Stockholms län"
    ))


sweden.blue = "#006aa8"
sweden.yellow = "#fecd00"

color.landmass = sweden.yellow
color.text = sweden.blue


#####################################################
#
# Map of whole Sweden
#
#####################################################

x <- map_ln
outline_sweden = map_data("worldHires", "sweden")

# Outline of Sweden
map_sweden = ggplot() +
    coord_fixed(1.7) +
    theme_nothing()

# Highlight the selected counties
for (ln in detail_counties$lnkod) {
    i <- x[x$lnkod == ln, ]
    map_sweden <- map_sweden +
        geom_polygon(data = i, aes(x=leaflet_long, y=leaflet_lat), fill = alpha(color.landmass, 0.4), color = NA)
}

map_sweden = map_sweden +
    geom_polygon(data = outline_sweden, aes(x=long, y = lat, group = group), fill=NA, color = alpha("black", 0.7)) +
    geom_point(data = sites, aes(x = lon, y = lat), color = color.text, size = 3) +
    geom_rect(aes(xmin = 10.3, xmax = 20.1, ymin = 55, ymax = 61.3),
              fill = NA, colour = color.text, size = 1.5)


#####################################################
#
# Map of selected counties and SCAN-B sites
#
#####################################################

map_selected = ggplot() +
    coord_fixed(1.7) +
    theme_nothing()

# Highlight the selected counties
for (ln in detail_counties$lnkod) {
    i <- x[x$lnkod == ln, ]
    map_selected <- map_selected +
        geom_polygon(data = i, aes(x=leaflet_long, y=leaflet_lat), fill = alpha(color.landmass, 0.4), color = alpha("black", 0.1))
}

map_selected = map_selected +
    # Plot Gotska Sandon seperately since it's not in the county data.
    geom_polygon(data = outline_sweden[outline_sweden$subregion == "Gotska Sandon", ], aes(x=long, y=lat), fill = alpha(color.landmass, 0.4), color = alpha("black", 0.1)) +
    geom_point(data = sites, aes(x = lon, y = lat), color = color.text, size = 4) +
    geom_text(data = sites, aes(x = label.lon, y = label.lat, label = label), fontface = "bold", size = 6, color=color.text)


#####################################################
#
# Combined inset map
#
#####################################################

# Join the two maps
gg_inset_map = ggdraw() +
    draw_plot(map_selected, x = 0.09, y = 0) +
    draw_plot(map_sweden, x = 0, y = 0.38, width = 0.27, height = 0.7)

gg_inset_map

cowplot::ggsave2(file = "scanb_map.pdf", plot = gg_inset_map)
