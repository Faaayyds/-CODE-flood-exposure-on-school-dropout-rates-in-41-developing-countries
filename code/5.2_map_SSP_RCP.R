library(readxl)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(scales)
library(tidyr)

options(warn = -1)

# 1. Paths (including Excel files for 2030 and 2050)
excel_path_2030 <- "D:/fe_R/newcode/results/4.2_SSP_RCP/SSP_Projections_2030.xlsx"
excel_path_2050 <- "D:/fe_R/newcode/results/4.2_SSP_RCP/SSP_Projections_2050.xlsx"

adm1_path  <- "D:/fe_R/map_shp/ADM_1.shp"
china_path <- "D:/fe_R/map_shp/china.shp"
flood_path <- "D:/fe_R/map_shp/flood_41countries.shp"

target_crs <- "+proj=eqc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

tryCatch({
  # 2. Read and project spatial data
  adm1 <- st_read(adm1_path, quiet = TRUE)
  china <- st_read(china_path, quiet = TRUE)
  shapefile <- st_read(flood_path, quiet = TRUE)
  
  shapefile_proj <- st_transform(shapefile, crs = target_crs)
  adm1_proj      <- st_transform(adm1, crs = target_crs)
  china_proj     <- st_transform(china, crs = target_crs)
  
  combined_bbox <- st_bbox(c(st_bbox(adm1_proj), st_bbox(shapefile_proj)))
  x_range <- c(combined_bbox["xmin"], combined_bbox["xmax"])
  y_range <- c(combined_bbox["ymin"], combined_bbox["ymax"])
  x_range_expanded <- c(x_range[1] - diff(x_range) * 0.08, x_range[2] + diff(x_range) * 0.08)
  y_range_expanded <- c(y_range[1] - diff(y_range) * 0.08, y_range[2] + diff(y_range) * 0.08)
  
  # Construct graticules and labels
  lon_breaks <- seq(-180, 180, by = 30)
  lat_breaks <- seq(-90, 90, by = 30)
  
  meridians_list <- list()
  for(lon in lon_breaks) {
    points_df <- data.frame(lon = rep(lon, 181), lat = seq(-90, 90, by = 1))
    points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
    points_proj <- st_transform(points_sf, crs = target_crs)
    coords <- st_coordinates(points_proj)
    meridians_list[[length(meridians_list) + 1]] <- data.frame(x = coords[,1], y = coords[,2], group = lon)
  }
  meridians_df <- do.call(rbind, meridians_list)
  
  parallels_list <- list()
  for(lat in lat_breaks) {
    points_df <- data.frame(lon = seq(-180, 180, by = 1), lat = rep(lat, 361))
    points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
    points_proj <- st_transform(points_sf, crs = target_crs)
    coords <- st_coordinates(points_proj)
    parallels_list[[length(parallels_list) + 1]] <- data.frame(x = coords[,1], y = coords[,2], group = lat)
  }
  parallels_df <- do.call(rbind, parallels_list)
  
  lon_labels <- data.frame()
  for(lon in lon_breaks) {
    point_sf <- st_as_sf(data.frame(lon = lon, lat = -85), coords = c("lon", "lat"), crs = 4326)
    point_proj <- st_transform(point_sf, crs = target_crs)
    coords <- st_coordinates(point_proj)
    lon_labels <- rbind(lon_labels, data.frame(x = coords[1], y = y_range[1] - diff(y_range) * 0.05, label = paste0(lon, "°")))
  }
  
  lat_labels <- data.frame()
  for(lat in lat_breaks) {
    point_sf <- st_as_sf(data.frame(lon = -175, lat = lat), coords = c("lon", "lat"), crs = 4326)
    point_proj <- st_transform(point_sf, crs = target_crs)
    coords <- st_coordinates(point_proj)
    lat_labels <- rbind(lat_labels, data.frame(x = coords[1] - diff(x_range) * 0.05, y = coords[2], label = paste0(abs(lat), "°", ifelse(lat >= 0, "N", "S"))))
  }
  
  summary_list <- list()
  
  # 3. Configure files and SSP scenario mapping list
  file_years <- list(
    list(file_path = excel_path_2030, year = "2030"),
    list(file_path = excel_path_2050, year = "2050")
  )
  
  ssp_scenarios <- list(
    list(ssp_code = "SSP126", label = "SSP1-2.6", color = "viridis"),
    list(ssp_code = "SSP245", label = "SSP2-4.5", color = "plasma"),
    list(ssp_code = "SSP585", label = "SSP5-8.5", color = "inferno")
  )
  
  # 4. Generate 8 maps in batch using nested loop
  for(fy in file_years) {
    curr_excel <- fy$file_path
    curr_year  <- fy$year
    
    for(ssp in ssp_scenarios) {
      sheet_name <- paste0(ssp$ssp_code, "_", curr_year)
      ssp_label  <- ssp$label
      color_opt  <- ssp$color
      
      # Read data from the corresponding sheet
      data <- read_excel(curr_excel, sheet = sheet_name)
      
      # Join with spatial map data
      map_with_data <- shapefile_proj %>%
        left_join(data, by = c("COUNTRY" = "Country", "NAME_1" = "Province"))
      
      plot_var <- "AN_est"
      plot_title <- paste0("Predicted Dropout Children (5-17)-", ssp_label, "-", curr_year)
      output_filename <- paste0("Predicted_Dropout_Children_5-17_", ssp$ssp_code, "_", curr_year)
      
      total_val <- sum(data[[plot_var]], na.rm = TRUE)
      summary_list[[length(summary_list) + 1]] <- data.frame(
        Scenario = ssp_label,
        Year = curr_year,
        Value = total_val
      )
      
      # Plot map
      paf_map <- ggplot() +
        geom_line(data = meridians_df, aes(x = x, y = y, group = group), color = "grey60", size = 0.3, linetype = "dashed") +
        geom_line(data = parallels_df, aes(x = x, y = y, group = group), color = "grey60", size = 0.3, linetype = "dashed") +
        geom_sf(data = adm1_proj, aes(fill = NULL), fill = "grey96", color = "grey75", size = 0.2) +
        geom_sf(data = china_proj, aes(fill = NULL), fill = "grey96", color = "grey75", size = 0.2) +
        geom_sf(data = map_with_data, aes(fill = !!sym(plot_var)), color = "white", size = 0.05) +
        geom_text(data = lon_labels, aes(x = x, y = y, label = label), size = 4.5, color = "grey30", vjust = 0.5, hjust = 0.5) +
        geom_text(data = lat_labels, aes(x = x, y = y, label = label), size = 4.5, color = "grey30", vjust = 0.5, hjust = 0.5) +
        
        scale_fill_viridis(
          option = color_opt,
          name = plot_title,
          na.value = NA,
          trans = "sqrt",
          labels = label_comma(),
          guide = guide_colorbar(
            barwidth = 40, 
            barheight = 0.6,
            title.position = "top",
            title.hjust = 0.5,
            label.theme = element_text(size = 14) 
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
      
      path_white <- paste0("D:/fe_R/newcode/results/", output_filename, "_PROVINCE_white.png")
      ggsave(path_white, paf_map, width = 14, height = 7.5, dpi = 1200, bg = "white")
      
      # path_trans <- paste0("D:/fe_R/newcode/results/", output_filename, "_PROVINCE_transparent.png")
      # ggsave(path_trans, paf_map, width = 14, height = 7.5, dpi = 1200, bg = "transparent")
      
      cat("Exported map:", output_filename, "Sheet:", sheet_name, "Palette:", color_opt, "\n")
    }
  }
  
}, error = function(e) {
  cat("Error detected:", e$message, "\n")
}, finally = {
  options(warn = 0)
})