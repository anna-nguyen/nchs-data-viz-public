############################################
# Load ASC data
############################################

rm(list = ls())

source(here::here("0-config.R"))

census_api_key("<API KEY REDACTED>", install = T, overwrite = T)

extract_asc_by_year = function(year) { 
  acs_data_long = lapply(c("B02001", "B01001"), 
                         function(table) get_acs(geography = "state", 
                                                 table = table,
                                                 output = "tidy", 
                                                 year = year)) %>% 
    bind_rows()

  acs_data_wide = acs_data_long %>% 
    bind_rows() %>% 
    dplyr::select(state = NAME, variable, estimate) %>% 
    spread(key = variable, value = estimate) %>% 
    mutate(year = year) %>% 
    dplyr::select(year, everything())
  
  return(acs_data_wide)
}

acs_data_wide_all_years = lapply(2015:2020, extract_asc_by_year) %>% bind_rows()

write.csv(acs_data_wide_all_years, here::here(data_dir, "acs_data_2015_to_2020.csv"))

