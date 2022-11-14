############################################
# Clean and process temperature data
############################################

rm(list = ls())

source(here::here("0-config.R"))

# Load state shape files
states = states(cb = TRUE) 
state_list = as.data.frame(states) %>% dplyr::select(NAME, STUSPS) %>% 
  filter(!(STUSPS %in% c("PR", "GU", "AS", "VI", "MP", "AK", "HI")))

# Directory containing tif files, from GEE 
temps_above_90_dir = paste0(data_dir, "temp_data/", "temps_above_90/")

# Dataframe with all month + years needed for data extraction
month_years_df = data.frame(year = sort(rep(2015:2020, 12)), 
                            month = rep(1:12, 6))

extract_days_over90_per_month = function(i) { 
  file_name = glue::glue("{temps_above_90_dir}{i-1}.tif")
  temp_raster = raster(file_name)
  
  extract_state_temps = function(state_name) {
    filtered_states = sf::as_Spatial(states %>% filter_state(state_name))
    
    # Filter by state shapefile
    temps = raster::extract(temp_raster, filtered_states) %>% 
      lapply(as.data.frame) %>% 
      bind_rows()
    
    # Find mean number of days over 90 degrees
    mean_temps_agg = temps %>% apply(2, function(t) mean(t, na.rm = T))

    
    return(data.frame(type = "mean_num_days_over90",
                      state = state_name,
                      val = as.numeric(mean_temps_agg)))
  }
  
  return (lapply(state_list$NAME, extract_state_temps) %>% 
            bind_rows() %>% 
            mutate(year = month_years_df$year[i],
                   month = month_years_df$month[i]) %>% 
            dplyr::select(type, state, year, month, val))
  
}

# Extract data for all months
days_over90_df = lapply(1:nrow(month_years_df), extract_days_over90_per_month) 

# Save results
saveRDS(days_over90_df %>% bind_rows(), paste0(data_dir, "clean_temp_days_over90.RDS"))
