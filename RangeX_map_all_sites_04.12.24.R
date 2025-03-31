

# Map of sites ------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)


# Example coordinates (replace with your actual coordinates)
locations <- data.frame(
  site = c("Norway", "Switzerland", "South Africa", "China"),
  lat = c(60.5403, 46.888, -28.7550, 35.9),
  lon = c(6.5676, 9.489, 28.8670, 104.2)
)

# Convert locations to a simple features (sf) object
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

# Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the map
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +  # World map
  geom_sf(data = locations_sf, aes(color = site), size = 3) +  # Add points for locations
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  # Custom colors
  labs(
    title = "Global Locations of RangeX Sites",
    subtitle = "Norway, Switzerland, South Africa, and China",
    x = "Longitude",
    y = "Latitude",
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )


# map a bit more fancy
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +  # Base map
  geom_sf(data = locations_sf, aes(color = site), size = 4, shape = 21, fill = "white") +  # Add points
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  
  geom_label_repel(
    data = locations, 
    aes(x = lon, y = lat, label = site), 
    size = 3, nudge_y = 5, segment.color = "gray50"
  ) +  # Add labels with repel
  annotation_scale(location = "bl", width_hint = 0.3) +  # Add scale bar
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +  # Add north arrow
  labs(
    title = "Global Locations of RangeX Sites",
    subtitle = "Norway, Switzerland, South Africa, and China",
    x = "Longitude",
    y = "Latitude",
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "aliceblue")  # Light blue background for ocean
  )



# map without America -----------------------------------------------------

# Define longitude and latitude limits to exclude America
lon_limits <- c(-25, 120)  # From Western Europe to East Asia
lat_limits <- c(-40, 70)   # From Southern Africa to Northern Europe

# Create the map
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +  # World map
  geom_sf(data = locations_sf, aes(color = site), size = 3) +  # Add points for locations
  scale_color_manual(values = c("red", "blue", "darkgreen", "turquoise")) +  # Custom colors
  labs(
    title = "Global locations of RangeX sites",
    # subtitle = "Norway, Switzerland, South Africa, and China",
    x = "Longitude",
    y = "Latitude",
    color = "Site"
  ) +
  coord_sf(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +  # Set map limits
  geom_label_repel(
    data = locations, 
    aes(x = lon, y = lat, label = site, color = site), 
    size = 4, box.padding = 0.5, max.overlaps = 10
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )


# globe -------------------------------------------------------------------

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert to sf and match the projection
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
locations_sf <- st_transform(locations_sf, crs = "+proj=ortho +lat_0=30 +lon_0=20")


globe <- ggplot(data = world) +
  geom_sf(fill = "gray", color = "grey1") +  # World map
  geom_sf(data = locations_sf, aes(color = site), size = 9) +  # Points
  scale_color_manual(values = c("grey2", "turquoise4", "grey2", "orange3")) +  # Colors
  coord_sf(crs = "+proj=ortho +lat_0=30 +lon_0=20") +  # Orthographic projection
  geom_label_repel(data = locations_sf,  
                   aes(x = st_coordinates(locations_sf)[,1], 
                       y = st_coordinates(locations_sf)[,2], 
                       label = site, color = site), 
                   size = 20, box.padding = 1, max.overlaps = 10) +  
  theme_bw() +
  theme(
    axis.title = element_blank(),  
    axis.text = element_blank(),   
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    
    # Make graticule (grid) lines darker and more visible
    panel.grid.major = element_line(color = "gray40", size = 0.6),
    panel.grid.minor = element_line(color = "gray40", size = 0.6)
  )
globe

ggsave(filename = "RangeX_map_globe_all_sites.png", 
       plot = globe, 
       path = "Graphs", 
       width = 15, height = 15)


