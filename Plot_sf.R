####building the query
OSMlines_route <- getbb(AOI) %>%
  opq() %>%
  add_osm_feature("route")


OSMlines_highway <- getbb(AOI) %>%
  opq() %>%
  add_osm_feature("highway")


OSMpoints <- getbb(AOI) %>%
  opq() %>%
  add_osm_feature("natural", "spring")

####query structure

str(OSMlines_route)
str(OSMlines_highway)
str(OSMpoints)

route <- osmdata_sf(OSMlines_route)
highway <- osmdata_sf(OSMlines_highway)
spring <- osmdata_sf(OSMpoints)

###Plot OSM Data
#our background map
mad_map <- get_map(getbb(AOI), maptype = "toner-background")

#final map

ggmap(mad_map) +
  geom_sf(
    data = spring,
    inherit.aes = FALSE,
    colour = "green",
    fill = "#004529",
    alpha = .5,
    size = 4,
    shape = 21
  ) +
  labs(x = "", y = "") +
  geom_sf(
    data = route,
    inherit.aes = FALSE,
    colour = "red",
    fill = "#004529",
    alpha = .5,
    size = 2,
    shape = 21
  ) +
  labs(x = "", y = "") +
  geom_sf(
    data = highway,
    inherit.aes = FALSE,
    colour = "yellow",
    fill = "#004529",
    alpha = .5,
    size = 1,
    shape = 21
  ) +
  labs(x = "", y = "")



assign(paste(Features[1]))

eval(parse(Features))
