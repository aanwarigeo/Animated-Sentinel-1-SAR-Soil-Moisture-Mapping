################################################################################
# Sentinel-1 SAR Soil Moisture Visualization - Animated GIF Map Generation
################################################################################

# This R script generates a professional static map (can be adapted for animation)
# visualizing soil moisture estimates derived from the following complete workflow 
# using high resolution Sentinel-1 GRD SAR data.

# check it workflow here: https://github.com/aanwarigeo/sentinel-1-soil-moisture

# Author: Anwarullah Anwari
# Date: May 28, 2025

################################################################################

# --------------------------
# 1. Load Required Libraries
# --------------------------
library(raster)
library(sf)
library(ggplot2)


# --------------------------
# 2. Set Input Paths
# --------------------------

raster_path <- "/Users/enter_your_raster_input_path"
experimental_area_path <- "/Users/enter_your_study_area_input_path"


# --------------------------
# 3. Load and Preprocess Data
# --------------------------

# Load raster and vector data
soil_moisture <- raster(raster_path)
experimental_area <- st_read(experimental_area_path, quiet = TRUE)

# Reproject to WGS84 (EPSG:4326)
soil_moisture_wgs84 <- projectRaster(soil_moisture, crs = st_crs(4326)$wkt)
experimental_area_wgs84 <- st_transform(experimental_area, 4326)

# Mask soil moisture raster with the experimental area
masked_soil_moisture <- mask(soil_moisture_wgs84, experimental_area_wgs84)
soil_moisture_df <- as.data.frame(masked_soil_moisture, xy = TRUE)
names(soil_moisture_df)[3] <- "moisture"

# Define in-situ measuring station
avocado_coords <- c(-7.0234244, 37.3849789)
avocado_point_sf <- st_sfc(st_point(avocado_coords), crs = 4326)
avocado_farm <- data.frame(
  x = avocado_coords[1],
  y = avocado_coords[2],
  label = "In-situ measuring station"
)

# Define plot bounding box with buffer
bbox <- st_bbox(experimental_area_wgs84)
buffer_factor <- 0.05
x_buffer <- (bbox["xmax"] - bbox["xmin"]) * buffer_factor
y_buffer <- (bbox["ymax"] - bbox["ymin"]) * buffer_factor
top_extra_buffer <- y_buffer * 0.5

xlim <- c(bbox["xmin"] - x_buffer, bbox["xmax"] + x_buffer)
ylim <- c(bbox["ymin"] - y_buffer, bbox["ymax"] + y_buffer + top_extra_buffer)

# Define color palette
color_palette <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb',
                   '#74a9cf','#3690c0','#0570b0','#045a8d','#023858')

# Extract date from raster file name
acquisition_date <- sub(
  ".*absolute_soil_moisture_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*.tif",
  "\\1",
  raster_path
)

# Annotation offsets
offset_x <- 0.07 * diff(xlim)
offset_y <- 0.07 * diff(ylim)

# --------------------------
# 4. Create Soil Moisture Map
# --------------------------
p <- ggplot() +
  geom_raster(data = soil_moisture_df, aes(x = x, y = y, fill = moisture)) +
  geom_point(data = avocado_farm, aes(x = x, y = y, shape = label),
             size = 3, color = "red", fill = NA, stroke = 1) +
  scale_fill_gradientn(
    colors = color_palette,
    na.value = "transparent",
    guide = guide_colourbar(
      direction = "horizontal", barwidth = 20, barheight = 0.1,
      ticks.colour = "white", frame.colour = "white",
      title.position = "top", title.hjust = 0.5
    )
  ) +
  scale_shape_manual(
    values = c("In-situ measuring station" = 1),
    guide = guide_legend(direction = "horizontal",
                         keywidth = unit(0.3, "cm"), keyheight = unit(0.3, "cm"))
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = 4326) +
  labs(fill = expression("Soil Moisture (m"^3*" m"^-3*")")) +
  annotate("text", x = xlim[2] - offset_x, y = ylim[2] - offset_y,
           label = "Sentinel-1 SAR Soil Moisture", hjust = 0.9, vjust = 1,
           color = "white", size = 7) +
  annotate("text", x = xlim[2] - offset_x, y = ylim[2] - 4 * offset_y,
           label = acquisition_date, hjust = 1.5, vjust = -7,
           color = "skyblue", size = 6) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.border = element_rect(color = "white", fill = NA, linewidth = 1),
    plot.margin = margin(20, 10, 10, 10, unit = "pt"),
    axis.text = element_text(color = "white", size = 10),
    axis.text.x = element_text(margin = margin(t = 8)),
    axis.text.y = element_text(margin = margin(r = 8)),
    axis.ticks = element_line(color = "white", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = c(0.97, 0.6),
    legend.justification = c(1, 0.1),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.margin = margin(0),
    legend.spacing.x = unit(0.5, "cm"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 10),
    legend.title = element_text(color = "white", size = 10, hjust = -0.0001),
    legend.key = element_rect(fill = "black")
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%.4f°W", abs(x)),
    breaks = seq(round(xlim[1], 4), round(xlim[2], 4), length.out = 5)
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%.4f°N", x),
    breaks = seq(round(ylim[1], 4), round(ylim[2], 4), length.out = 5)
  ) +
  guides(shape = guide_legend(order = 1), fill = guide_colourbar(order = 2))

# --------------------------
# 5. Export the Map 
# --------------------------
ggsave("Sentinel1_Soil_Moisture_Map.png", p,
       width = 11.69, height = 8.5, units = "in",
       dpi = 300, bg = "black")


################################################################################
# End of Script
################################################################################
