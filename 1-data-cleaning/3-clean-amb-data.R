rm(list = ls())

source(here::here("0-config.R"))

###############
# Definitions #
###############

# Data sets to be imported
data_links = list(
  "2015" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2015-sas.zip",
  "2016" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2016_sas.zip",
  "2017" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2017_sas.zip",
  "2018" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2018_sas.zip",
  "2019" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2019_sas.zip",
  "2020" = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHAMCS/sas/ed2020_sas.zip"
)

# Heat illness ICDs
heat_icds = tribble(
  ~version, ~code,  ~outcome,
  9,        "2765", "Volume depletion disorder",
  9,        "7802", "Regular syncope",
  9,        "9921", "Heat syncope",
  9,        "0580", "Orthostatic hypotension",
  10,       "E86",  "Volume depletion",
  10,       "R55",  "Regular syncope",
  10,       "T671", "Heat syncope",
  10,       "I951", "Orthostatic hypotension"
)

################
# Collect Data #
################

### Function to import, unzip, and select desired data
  # NOTE: Assumes .sas7bdat is the only file in the .zip file
process_amb_data = function(endpoint, year) {
  # Create a temporary file & download from endpoint
  temp = tempfile()
  download.file(endpoint, temp)
  # Unzip file
  temp=unzip(temp)
  # Unpack into data frame
  df = read_sas(temp)
  # Remove temporary file
  unlink(temp, recursive=T)
  
  # Determine if using ICD 9 or 10 codes
  if(year<=2015){
    heat_codes = heat_icds %>%
      filter(version==9) %>%
      pull(code)
  } else {
    heat_codes = heat_icds %>%
      filter(version==10) %>%
      pull(code)
  }
  
  # Filter raw data only for needed rows & columns
  df %>% 
    select(VMONTH, AGER, REGION, SEX, ETHIM, RACER, RACEUN,
           DIAG1, DIAG2, DIAG3, DIAG4, DIAG5,
           HDDIAG1, HDDIAG2, HDDIAG3, HDDIAG4, HDDIAG5) %>% 
    mutate(across(.cols=contains("DIAG"),
                  .fns=~apply(sapply(heat_codes, grepl, .x), MARGIN=1, FUN=any),
                  .names="{.col}_isHeat")) %>%
    rowwise() %>%
    mutate(isHeatOutcome = any(c_across(ends_with("_isHeat")))) %>%
    filter(isHeatOutcome) %>%
    mutate(YEAR = year) %>%
    ungroup() %>%
    select(VMONTH, AGER, REGION, SEX, ETHIM, RACER, RACEUN, YEAR)
}

### Apply function to list of endpoints
all_amb_data = mapply(process_amb_data, data_links, names(data_links), 
                      SIMPLIFY=F, USE.NAMES=F) %>% bind_rows()

########################
# Compile final tables #
########################

### Full data
outcomes_full = all_amb_data %>%
  group_by(VMONTH, YEAR) %>%
  summarize(count = n(),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(date = my(paste0(month.abb[VMONTH],YEAR))) %>%
  mutate(cat = "All",
         subg = "All") %>%
  select(date, cat, subg, count)

### By race
outcomes_race = all_amb_data %>%
  # Get date in lubridate format
  mutate(date = my(paste0(month.abb[VMONTH],YEAR))) %>%
  select(-VMONTH, -YEAR) %>%
  # Aggregate
  mutate(cat = "Race",
         subg = case_when(
           RACEUN==1        ~ "White",
           RACEUN==2        ~ "Black",
           RACEUN==3        ~ "Asian",
           RACEUN%in%c(4:6) ~ "Other",
           T                ~ "Missing"
         )) %>%
  # Get total outcomes in each group
  group_by(date, cat, subg) %>%
  summarize(count = n(),
            .groups = "keep") %>%
  ungroup()

### By age
outcomes_age = all_amb_data %>%
  # Get date in lubridate format
  mutate(date = my(paste0(month.abb[VMONTH],YEAR))) %>%
  select(-VMONTH, -YEAR) %>%
  # Aggregate
  mutate(cat = "Age",
         subg = case_when(
           AGER==1        ~ "<15 Years",
           AGER==2        ~ "15-24 Years",
           AGER%in%c(3,4) ~ "25-64 Years",
           AGER%in%c(5,6) ~ ">65 Years",
           T       ~ NA_character_
         )) %>%
  # Get total outcomes in each subgroup
  group_by(date, cat, subg) %>%
  summarize(count = n(),
            .groups = "keep") %>%
  ungroup()

### By sex
outcomes_sex = all_amb_data %>%
  group_by(VMONTH, YEAR, SEX) %>%
  summarize(count = n(),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(date = my(paste0(month.abb[VMONTH],YEAR))) %>%
  select(-VMONTH, -YEAR) %>%
  mutate(cat = "Sex",
         subg = case_when(
           SEX==1 ~ "Female",
           SEX==2 ~ "Male",
           T       ~ NA_character_
         )) %>%
  select(date, cat, subg, count)

### By region
outcomes_region = all_amb_data %>%
  group_by(VMONTH, YEAR, REGION) %>%
  summarize(count = n(),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(date = my(paste0(month.abb[VMONTH],YEAR))) %>%
  select(-VMONTH, -YEAR) %>%
  mutate(cat = "Region",
         subg = case_when(
           REGION==1 ~ "Northeast",
           REGION==2 ~ "Midwest",
           REGION==3 ~ "South",
           REGION==4 ~ "West",
           T       ~ NA_character_
         )) %>%
  select(date, cat, subg, count)

### Save data
outcome_data = rbind(outcomes_full, outcomes_race, outcomes_age, outcomes_sex, 
                     outcomes_region)
save(outcome_data, file="data/heat_outcomes_data.rds")
