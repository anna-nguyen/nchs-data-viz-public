############################################
# Clean and process temperature data
############################################

rm(list = ls())

source(here::here("0-config.R"))

# Load state shape files
states = states(cb = TRUE) 
state_list = as.data.frame(states) %>% dplyr::select(NAME, STUSPS) %>% 
  filter(!(STUSPS %in% c("PR", "GU", "AS", "VI", "MP", "AK", "HI")))

# Function to temperature from files
process_temp_ncs = function(type, year) {
  # Load temperature raster
  filename = glue::glue("{data_dir}temp_data/filtered/filtered_TerraClimate_t{type}_{year}.nc")
  temp_raster = raster::brick(filename)

  # Function to extract temperature estimates for each state
  extract_state_temps = function(state_name) {
    filtered_states = sf::as_Spatial(states %>% filter_state(state_name))
    
    # Filter by state shapefile
    temps = raster::extract(temp_raster, filtered_states) %>% 
      lapply(as.data.frame) %>% 
      bind_rows()
    
    # Find absolute maximums/minimums
    if (type == "max") {
      temps_agg = temps %>% apply(2, function(t) max(t, na.rm = T))
      type_label = "maximum"
    } else if (type == "min") {
      temps_agg = temps %>% apply(2, function(t) min(t, na.rm = T))
      type_label = "minimum"
    } 
    
    return(data.frame(type = type_label,
                      state = state_name,
                      year = year,
                      month = 1:12, 
                      val = as.numeric(temps_agg)))
  }
  return(lapply(state_list$NAME, extract_state_temps) %>% bind_rows())
}

# Process max/min temp data for all years
monthly_max_temp_df = lapply(2015:2020, function(year) process_temp_ncs("max", year)) %>% bind_rows()
monthly_min_temp_df = lapply(2015:2020, function(year) process_temp_ncs("min", year)) %>% bind_rows()
monthly_temp_df = bind_rows(monthly_max_temp_df, monthly_min_temp_df)

# Save results
saveRDS(monthly_temp_df, paste0(data_dir, "clean_monthly_temp_df.RDS"))

