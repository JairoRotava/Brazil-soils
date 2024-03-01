## Carrega mapa do Brasil para plotar os pontos de dados

library(geobr)
library(dplyr)
library(sf)
library(ggplot2)

# carrega shape dos estados do brasil
states <- read_state()


# gerando mapa
ggplot() + geom_sf(data=states)

# Plota pontos
#(sites <- data.frame(longitude = c(-50.144005, -50.109), latitude = c(-26.479005, 
#                                                                      -16.83)))
(sites <- data.frame(longitude = -lon[2,], latitude = -lat[2,]))



ggplot() +
  geom_sf(data=states) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size=1)


########## PEsquisa


# Read data.frame with life expectancy data
df <- utils::read.csv(system.file("extdata/br_states_lifexpect2017.csv", package = "geobr"), encoding = "UTF-8")

states$name_state <- tolower(states$name_state)
df$uf <- tolower(df$uf)

# join the databases
states <- dplyr::left_join(states, df, by = c("name_state" = "uf"))

ggplot() +
  geom_sf(data=states, color= NA, size=.15) +
  labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
  scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
  theme_minimal()


########## PEsquisa


#mun <- left_joint(mun, Rais_Coop_Estb_2016mapamun, by= c("code_muni" = "Município"))

#ggplot() + geom_sf(data=mun, aes(fill= Score))

library(ggplot2)

shpMun <- rgdal::readOGR('./shapefiles', 'municipios')
mapaMun <- fortify(shpMun, region = 'GEOCODE')

shpUFs <- rgdal::readOGR('./shapefiles', 'ufs')

# Simulando as informações de cooperativas
dadosCoop <- data.frame(
  GEOCODE = shpMun@data$GEOCODE,
  cooperativas = sample(1:20, length(shpMun@data$GEOCODE), replace = TRUE) )
#

ggplot(dadosCoop) +
  geom_map(
    map = mapaMun,
    color = 'gray60', size = .1,
    aes(map_id = GEOCODE, fill = cooperativas)
  ) +
  expand_limits(
    x = mapaMun$long,
    y = mapaMun$lat
  ) +
  scale_fill_gradient(
    low = 'lightyellow',
    high = 'darkred'
  ) +
  geom_path(
    data = shpUFs,
    size = .2,
    aes(long, lat, group = group)
  ) +
  coord_map() +
  theme_void() +
  theme(legend.position = c(0.2,0.3))

ggsave('mapa.png', width = 8, height = 8, dpi = 100)



library(rgdal)

# Get the world polygon and extract UK
library(ggplot2)
library(dplyr)
library(maps)
#brasil <- map_data("world") %>% filter(region=="UK")
brasil <- map_data("world")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc=="UK")

# Left chart
ggplot() +
  geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() +  coord_map() 





#install.packages("tidyverse")

# Read this shape file with the rgdal library. 
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/Data/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)


# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )


# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
library(maptools)
#spdf_fortified <- tidy(my_spdf, region = "NAME")
spdf_fortified <- fortify(my_spdf)


# Plot it
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 


# Libraries
library(ggplot2)
library(dplyr)

# Get the world polygon and extract UK
library(maps)
UK <- map_data("world") %>% filter(region=="UK")
brasil <- map_data("world") %>% filter(region=="BRAZIL")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc=="UK")

# Left chart
ggplot() +
  geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() + ylim(50,59) + coord_map() 

# Second graphic with names of the 10 biggest cities
library(ggrepel)
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")

# virids package for the color palette
library(viridis)

# Left: use size and color
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() 

# Center: reorder your dataset first! Big cities appear later = on top
data %>%
  arrange(pop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# Right: just use arrange(desc(pop)) instead
data %>%
  arrange(desc(pop)) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# Create breaks for the color scale
mybreaks <- c(0.02, 0.04, 0.08, 1, 7)

# Reorder data to show biggest cities on top
data <- data %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) 

# Build the map
map %>%
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in M)" ) +
  theme_void() + ylim(50,59) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in the UK") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
