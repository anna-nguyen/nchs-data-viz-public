library(lubridate)
library(tidyverse)

#############
# Load data #
#############

# ACS data
load("data/acs_clean.Rda")

# Health data
load("data/heat_outcomes_data.rds")

# Heat data
raw_heat_data = readRDS("data/clean_monthly_temp_df.RDS") %>% 
  bind_rows(readRDS("data/clean_temp_days_over90.RDS"))


# State-Region Crosswalk
state_region_crosswalk = tribble(
  ~state,        ~region,
  "Alabama",       "South",     "Montana",        "West",
  "Alaska",        "West",      "Nebraska",       "Midwest",
  "Arizona",       "West",      "Nevada",         "West",
  "Arkansas",      "South",     "New Hampshire",  "Northeast",
  "California",    "West",      "New Jersey",     "Northeast",
  "Colorado",      "West",      "New Mexico",     "West",
  "Connecticut",   "Northeast", "New York",       "Northeast",
  "Delaware",      "South",     "North Carolina", "South",
  "Florida",       "South",     "North Dakota",   "Midwest",
  "Georgia",       "South",     "Ohio",           "Midwest",
  "Hawaii",        "West",      "Oklahoma",       "South",
  "Idaho",         "West",      "Oregon",         "West",
  "Illinois",      "Midwest",   "Pennsylvania",   "Northeast",
  "Indiana",       "Midwest",   "Rhode Island",   "Northeast",
  "Iowa",          "Midwest",   "South Carolina", "South",
  "Kansas",        "Midwest",   "South Dakota",   "Midwest",
  "Kentucky",      "South",     "Tennessee",      "South",
  "Louisiana",     "South",     "Texas",          "South",
  "Maine",         "Northeast", "Utah",           "West",
  "Maryland",      "South",     "Vermont",        "Northeast",
  "Massachusetts", "Northeast", "Virginia",       "South",
  "Michigan",      "Midwest",   "Washington",     "West",
  "Minnesota",     "Midwest",   "West Virginia",  "South",
  "Mississippi",   "South",     "Wisconsin",      "Midwest",
  "Missouri",      "Midwest",   "Wyoming",        "West",
  "District of Columbia", "South"
)

#############
# Harmonize #
#############

# ACS data
region_acs = acs_clean %>%
  left_join(state_region_crosswalk) %>%
  filter(!is.na(region)) %>%
  group_by(year, region) %>%
  # Total states in each region
  summarize(rep_pop = sum(total),
            .groups = "keep") %>%
  ungroup()

all_acs = acs_clean %>%
  group_by(year) %>%
  # Add all states while also dropping _prop columns
  summarize(across(c(-ends_with("_prop"),-state), sum),
            .groups = "keep") %>%
  # Aggregate columns as needed
  mutate(age25to64 = agecat3 + agecat4,
         ageover65 = agecat5 + agecat6,
         race_other = race_2 + race_AIAN + race_NHPI) %>%
  select(-agecat3, -agecat4, -agecat5, -agecat6,
         -race_2, -race_AIAN, -race_NHPI) %>%
  # Rename columns to match final names
  rename(Sex_Female=female,           Race_White=race_white,
         Sex_Male=male,               Race_Black=race_black,
         "Age_<15 Years"=agecat1,     Race_Asian=race_asian,
         "Age_15-24 Years"=agecat2,   Race_Other=race_other,
         "Age_25-64 Years"=age25to64, All_All=total,
         "Age_>65 Years"=ageover65) %>%
  ungroup() %>%
  pivot_longer(cols = c(-year),
               names_sep = "_",
               names_to = c("cat", "subg"),
               values_to = "rep_pop")

# Health data
region_outcome = outcome_data %>%
  filter(cat == "Region")

all_outcome = outcome_data %>%
  filter(cat != "Region")

# Heat data
region_heat = raw_heat_data %>%
  mutate(date = my(paste0(month.abb[month], year))) %>%
  left_join(state_region_crosswalk) %>%
  filter(date >= min(outcome_data$date) & date<= max(outcome_data$date) &
           !is.na(region)) %>%
  dplyr::select(date, type, region, state, val) %>%
  pivot_wider(names_from=type,
              values_from=val) %>%
  group_by(date, region) %>%
  summarize(maximum = max(maximum),
            minimum = min(minimum),
            n_days_over90 = mean(mean_num_days_over90), 
            .groups = "keep") %>%
  ungroup() %>%
  pivot_longer(cols = c("maximum", "minimum", "n_days_over90"),
               names_to = "type",
               values_to = "value")

all_heat = region_heat %>%
  pivot_wider(names_from=type,
              values_from=value) %>%
  group_by(date) %>%
  summarize(maximum = max(maximum),
            minimum = min(minimum),
            n_days_over90 = mean(n_days_over90), 
            .groups = "keep") %>%
  mutate(region = "All") %>%
  ungroup() %>%
  pivot_longer(cols = c("maximum", "minimum", "n_days_over90"),
               names_to = "type",
               values_to = "value")

####################
# Combine & Export #
####################

region_combine = region_outcome %>%
  mutate(year = year(date)) %>%
  left_join(region_acs, by=c("year"="year", "subg"="region")) %>%
  dplyr::select(-year) %>%
  left_join(region_heat, by=c("date"="date", "subg"="region"))

all_combine = all_outcome %>%
  mutate(year = year(date)) %>%
  left_join(all_acs, by=NULL) %>%
  dplyr::select(-year) %>%
  left_join(all_heat, by=c("date"="date")) %>%
  dplyr::select(-region)

nchs_viz_data = rbind(region_combine, all_combine) %>%
  mutate(cat = factor(cat,
                      levels = c("All", "Age", "Race", "Region", "Sex"),
                      ordered = T),
         subg = factor(subg,
                       levels = c("All",
                                  "<15 Years", "15-24 Years", "25-64 Years", ">65 Years",
                                  "Asian", "Black", "Other", "White", "Missing",
                                  "Midwest", "Northeast", "South", "West",
                                  "Female", "Male"),
                       ordered = T),
         type = factor(type))

saveRDS(nchs_viz_data, file="nchs-data-viz/nchs_viz_data.rds")




