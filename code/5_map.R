library(readxl)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(viridis)

options(warn = -1)

# 1. Load Data
data <- read_excel("D:/fe_R/newcode/results/4_PAF_SSP/Attri_Province_Formula11_12.xlsx", sheet = 1)
shapefile <- st_read("D:/fe_R/map_shp/flood_41countries.shp", quiet = TRUE)

# 1.1 Load Basemaps
adm1 <- st_read("D:/fe_R/map_shp/ADM_1.shp", quiet = TRUE)
china <- st_read("D:/fe_R/map_shp/china.shp", quiet = TRUE)

tryCatch({
  
  # 2. CRS Transformation - Equal Earth / Equidistant Cylindrical
  target_crs <- "+proj=eqc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  shapefile_proj <- st_transform(shapefile, crs = target_crs)
  
  # 2.1 Transform Basemaps
  adm1_proj <- st_transform(adm1, crs = target_crs)
  china_proj <- st_transform(china, crs = target_crs)
  
  # 3. Get Bounding Box
  combined_bbox <- st_bbox(c(st_bbox(adm1_proj), st_bbox(shapefile_proj)))
  x_range <- c(combined_bbox["xmin"], combined_bbox["xmax"])
  y_range <- c(combined_bbox["ymin"], combined_bbox["ymax"])
  
  x_range_expanded <- c(x_range[1] - diff(x_range) * 0.08, x_range[2] + diff(x_range) * 0.08)
  y_range_expanded <- c(y_range[1] - diff(y_range) * 0.08, y_range[2] + diff(y_range) * 0.08)
  
  # 4. Create Graticule Grid
  lon_breaks <- seq(-180, 180, by = 30)
  lat_breaks <- seq(-90, 90, by = 30)
  
  meridians_list <- list()
  for(lon in lon_breaks) {
    points_df <- data.frame(
      lon = rep(lon, 181),
      lat = seq(-90, 90, by = 1)
    )
    points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
    points_proj <- st_transform(points_sf, crs = target_crs)
    coords <- st_coordinates(points_proj)
    meridians_list[[length(meridians_list) + 1]] <- data.frame(x = coords[,1], y = coords[,2], group = lon)
  }
  meridians_df <- do.call(rbind, meridians_list)
  
  parallels_list <- list()
  for(lat in lat_breaks) {
    points_df <- data.frame(
      lon = seq(-180, 180, by = 1),
      lat = rep(lat, 361)
    )
    points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
    points_proj <- st_transform(points_sf, crs = target_crs)
    coords <- st_coordinates(points_proj)
    parallels_list[[length(parallels_list) + 1]] <- data.frame(x = coords[,1], y = coords[,2], group = lat)
  }
  parallels_df <- do.call(rbind, parallels_list)
  
  # 5. Create Graticule Labels
  lon_labels <- data.frame()
  for(lon in lon_breaks) {
    point_sf <- st_as_sf(data.frame(lon = lon, lat = -85), coords = c("lon", "lat"), crs = 4326)
    point_proj <- st_transform(point_sf, crs = target_crs)
    coords <- st_coordinates(point_proj)
    lon_labels <- rbind(lon_labels, data.frame(
      x = coords[1],
      y = y_range[1] - diff(y_range) * 0.05,
      label = paste0(lon, "°")
    ))
  }
  
  lat_labels <- data.frame()
  for(lat in lat_breaks) {
    point_sf <- st_as_sf(data.frame(lon = -175, lat = lat), coords = c("lon", "lat"), crs = 4326)
    point_proj <- st_transform(point_sf, crs = target_crs)
    coords <- st_coordinates(point_proj)
    lat_labels <- rbind(lat_labels, data.frame(
      x = x_range[1] - diff(x_range) * 0.05,
      y = coords[2],
      label = paste0(abs(lat), "°", ifelse(lat >= 0, "N", "S"))
    ))
  }
  
  # 6. Join Data
  map_with_data <- shapefile_proj %>%
    left_join(data, by = c("NAME_1" = "Province",
                           "COUNTRY" = "Country"))
  
  # 7. Plotting
  paf_base_map <- ggplot() +
    geom_line(data = meridians_df, aes(x = x, y = y, group = group), 
              color = "grey70", size = 0.3, linetype = "dashed") +
    geom_line(data = parallels_df, aes(x = x, y = y, group = group), 
              color = "grey70", size = 0.3, linetype = "dashed") +
    geom_sf(data = adm1_proj, 
            aes(fill = NULL), 
            fill = "grey96",    
            color = "grey75",   
            size = 0.2) +
    geom_sf(data = china_proj, 
            aes(fill = NULL), 
            fill = "grey96",    
            color = "grey75",   
            size = 0.2) +
    # prov_mean_dropout_rate       PAF_r_est
    # sum_flood_count              AN_r_est
    geom_sf(data = map_with_data, aes(fill = AN_r_est), color = "white", size = 0.05) +
    scale_fill_viridis(
      #"viridis", "plasma", "inferno"
      option = "plasma",
      name = "Number of flood-attributed dropouts", #Dropout rate/Total flood counts/PAF/Number of flood-attributed dropouts
      na.value = NA,  
      trans = "sqrt",
      labels = scales::label_comma(),
      guide = guide_colorbar(
        barwidth = 26,          
        barheight = 0.6,        
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    coord_sf(xlim = x_range_expanded, ylim = y_range_expanded, expand = FALSE) +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme_void() +
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      legend.position = "bottom",
      legend.text = element_text(size = 14),    
      legend.title = element_text(size = 16),   
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  # 7.1 Maps with / without Graticule Labels
  paf_map_no_graticule <- paf_base_map
  
  paf_map_with_graticule <- paf_base_map +
    geom_text(data = lon_labels, aes(x = x, y = y, label = label), 
              size = 4.5, color = "grey30", vjust = 0.5, hjust = 0.5) +  
    geom_text(data = lat_labels, aes(x = x, y = y, label = label), 
              size = 4.5, color = "grey30", vjust = 0.5, hjust = 0.5)    
  
  # 8. Save Maps
  output_dir <- "D:/fe_R/newcode/results/"
  # Dropout_Rate /Total_Flood_Counts / Flood_Attributed_Dropouts / PAF
  # ggsave(filename = paste0(output_dir, "Flood_Attributed_Dropouts_With_Graticule_transparent.png"), 
  #        plot = paf_map_with_graticule, 
  #        width = 14,      
  #        height = 7.5,    
  #        dpi = 1200,
  #        bg = "transparent") 
  # 
  # ggsave(filename = paste0(output_dir, "Flood_Attributed_Dropouts_No_Graticule_transparent.png"), 
  #        plot = paf_map_no_graticule, 
  #        width = 14,      
  #        height = 7.5,    
  #        dpi = 1200,
  #        bg = "transparent") 
  
  ggsave(filename = paste0(output_dir, "Flood_Attributed_Dropouts_With_Graticule.png"), 
         plot = paf_map_with_graticule, 
         width = 14,      
         height = 7.5,    
         dpi = 1200,
         bg = "white") 
  
  message("Maps saved successfully.")
  
}, error = function(e) {
  message("Error: ", e$message)
  print(e)
}, finally = {
  options(warn = 0)
})