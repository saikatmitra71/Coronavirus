library(dplyr)
library(sf)
library(rmapshaper)
library(rvest)
library(readr)
library(tmap)

# shape file and data
world_sf <- st_read("World_Countries/World_Countries.shp") # downloded shape file
simp_sf <- ms_simplify(world_sf, keep = 0.01, keep_shapes = TRUE)

h <- read_html("https://www.worldometers.info/coronavirus")
tab <- h %>% html_nodes("table")
corona_data <- tab[[1]] %>% html_table()

# some necessary wranglings
corona_combined <- corona_data %>% 
  mutate(TotalCases=parse_number(TotalCases)) %>% 
  mutate(Country=recode(`Country,Other`, 
                        "S. Korea"="South Korea",
                        "USA"="United States",
                        "Diamond Princess"="Japan",
                        "U.K."="United Kingdom",
                        "U.A.E."="United Arab Emirates",
                        "North Macedonia"="Macedonia",
                        "Hong Kong"="China",
                        "Macao"="China")) %>% 
  group_by(Country) %>% 
  summarise(c=sum(TotalCases)) %>% 
  ungroup() %>% 
  right_join(simp_sf, by=c("Country"="COUNTRY")) %>% 
  st_as_sf()

# The Map
corona_combined %>% 
  tm_shape() +
  tm_fill(style = "log10_pretty", col = "c", title = "Total Cases", palette = "Reds") +
  tm_borders() +
  tm_style("cobalt") +
  tm_layout(
    main.title = "Corona Outbreak",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("left", "bottom"),
    legend.frame = TRUE
  ) +
  tm_credits("Data: https://www.worldometers.info/coronavirus", position = c("center", "bottom"))
