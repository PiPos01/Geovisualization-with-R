install.packages(c(
  "leaflet", "sf", "sp", "maps", "htmlwidgets"
))
library(leaflet)
library(sf)
library(sp)
library(maps)
library(htmlwidgets)
#install.packages("RColorBrewer")
library(RColorBrewer)


zabytki <- st_read('zabytki.geojson')
zabytki <- st_transform(zabytki, crs = 4326)
st_crs(zabytki)
str(zabytki)

#zmieniamy typ zabytkow (z 55) na kilka uniklanych
zabytki$type <- 
  ifelse(zabytki$type %in% c("Church", "Basilica", "Temple", "Chapel", "Sanctuary", "Catacombs", "Crypt"), "Zabytki sakralne",
  ifelse(zabytki$type %in% c("Archaeological Site", "Archaeological Park", "Historic Site", "Ancient Monument", "Historic Baths", "Ancient Theater", "World Heritage Site"), "Antyk i Archeologia",
  ifelse(zabytki$type %in% c("Museum", "Art Museum", "Museum Area"), "Muzea i Galerie",
  ifelse(zabytki$type %in% c("Palace", "Castle"), "Pałace i Zamki",
  ifelse(zabytki$type %in% c("Fountain"), "Fontanny",
  ifelse(zabytki$type %in% c("Park", "Garden", "Renaissance Garden", "Viewpoint", "Historic Hill", "Island", "Park/Museum"), "Parki i Punkty widokowe",
  ifelse(zabytki$type %in% c("Square", "Market Square", "Street", "Historic Street", "Neighborhood", "District"), "Place, Ulice i Dzielnice",
  ifelse(zabytki$type %in% c("Monument", "Bridge", "Gate", "Tower", "Historic Wall", "Stairs"), "Pomniki i Budowle",
  "Inne"))))))))

# #pobieranie wartosci unikalnych dla kolumny type
# unikat_type <- unique(zabytki$type)
# 
# kolory_typow <- RColorBrewer::brewer.pal(length(unikat_type), name="Paired")
# pal_type <- colorFactor(kolory_typow, domain = unikat_type)

kolory_dedykowane <- c(
  "Zabytki sakralne" = "#8E44AD",         
  "Antyk i Archeologia" = "#8B4513",      
  "Muzea i Galerie" = "#D68910",         
  "Pałace i Zamki" = "#C0392B",          
  "Fontanny" = "#2980B9",                 
  "Parki i Punkty widokowe" = "#117A65", 
  "Place, Ulice i Dzielnice" = "#D35400", 
  "Pomniki i Budowle" = "#A67B5B",       
  "Inne" = "#E84393"                     
)

pal_type <- colorFactor(
  palette = unname(kolory_dedykowane), 
  domain = names(kolory_dedykowane)
)

zabytki_rzym <- leaflet(zabytki) %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group= "Mapa topograficzna") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  setView(lng = 12.50, lat = 41.9, zoom = 12) %>%
  addCircleMarkers(
    data = zabytki,
    radius = 6,
    stroke = TRUE,
    color = "#2c3e50",
    fillColor = ~pal_type(type),
    fillOpacity = 0.9,
    opacity = 0.9,
    weight = 1,             
    #opacity = 0.1,
    clusterOptions = markerClusterOptions(
      disableClusteringAtZoom = 15,
      spiderfyOnMaxZoom = FALSE,   # Wyłącza pajęczynę przy maksymalnym przybliżeniu
      zoomToBoundsOnClick = TRUE
    ),
    label = ~name,
    popup = ~paste0(
      "<b>", name, "</b><br>", 
      "<b>Typ: </b>", type, "<br>",
      "<b>Opis: </b>", description),
    group = "POI") %>%
  addLegend(
    position = "bottomright",
    pal = pal_type,
    values = names(kolory_dedykowane),
    title = "Typ Obiektu",
    opacity = 1
  ) %>%
  addLayersControl(
    baseGroups = c("Mapa topograficzna", "World Imagery"),
    overlayGroups = c("POI"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addScaleBar(position = "bottomleft") |>
  addMiniMap() |>
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479"
  ) 
  
zabytki_rzym

saveWidget(zabytki_rzym, file = "mapa_rzymu.html", selfcontained = TRUE)

#https://leaflet-extras.github.io/leaflet-providers/preview/
