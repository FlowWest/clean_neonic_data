library(tidyverse)
library(lubridate)


# Read in data from https://www.cdpr.ca.gov/docs/emon/surfwtr/surfcont.htm
surface_water_data <- read_csv("data-raw/surf_water_2021.csv") 

# View data 
View(surface_water_data)

# Define neonics of interest 
neonics <- c("imidacloprid", "acetamiprid", "clothianidin", "dinotefuran", "thiamethoxam")

# Initial filter and clean of data 
neonics_data <- surface_water_data %>%
  janitor::clean_names() %>%
  filter(chemical_name %in% neonics) %>% 
  mutate(sample_date = as_date(sample_date, format = "%m/%d/%Y")) %>% glimpse

### Create tables to use in tableau --------------------------------------------

# For map - Create county summaries for entire period of record 
# Describe the percent containing a neonic, when measured for a neonic, by county and year. 

# Create a paste function to use in summarize statement 
my_paste <- function(x) {paste(x[!is.na(x)], collapse = ", ")}

# county summary table 
county_summaries <- neonics_data %>% 
  group_by(site_code, sample_date) %>%
  mutate(neonic = concentration_ppb > 0,
         neonic_name = if_else(neonic, chemical_name, NULL)) %>%
  group_by(county) %>% 
  summarise(date_range = paste(min(format(sample_date, "%m/%d/%Y")), "to", 
                               max(format(sample_date, "%m/%d/%Y"))), 
            number_samples_with_neonic = (sum(neonic)),
            total_samples = n(),
            neonics_found = unique(neonic_name) %>% my_paste(),
            range = paste(min(concentration_ppb), "-", max(concentration_ppb))) %>% 
  mutate(percent_samples_containing_a_neonic = round(number_samples_with_neonic/total_samples * 100, 2),
         neonics_found = ifelse(neonics_found == "", "None Detected", neonics_found)) %>% glimpse

# Save county summary table 
write_csv(county_summaries, "data/county_summary.csv")

# For bar chart - create county summaries by year 
# generate clean set that gives percent by year containing a neonic for each county 
clean_neonics <- neonics_data %>% 
  group_by(site_code, sample_date) %>%
  mutate(neonic = concentration_ppb > 0) %>% 
  ungroup()%>% 
  group_by(year = year(sample_date), county) %>% 
  summarise(total_samples = n(), 
            `Contains a Neonic` = sum(neonic),
            `Does Not Contain a Neonic` = total_samples - `Contains a Neonic`,
            `Percent Contining Neonics` = (`Contains a Neonic`/ total_samples) * 100) %>% 
  pivot_longer(-c(year, county), names_to = "neonic", values_to = "count") %>% 
  filter(neonic != "total_samples") %>% glimpse

# Create wide version for tableau 
clean_neonics_wide <- neonics_data %>% 
  group_by(site_code, sample_date) %>%
  mutate(neonic = concentration_ppb > 0) %>% 
  ungroup()%>% 
  group_by(year = year(sample_date), county) %>% 
  summarise(total_samples = n(), 
            `Contains a Neonic` = sum(neonic),
            `Does Not Contain a Neonic` = total_samples - `Contains a Neonic`,
            `Percent Contining Neonics` = (`Contains a Neonic`/ total_samples) * 100) %>% glimpse


# write_csv(clean_neonics_wide, "data/clean_neonics_wide_by_year.csv")
  

### Additional QC and test tables ----------------------------------------------
# review data 
summary(neonics_data)
neonics_data %>% ggplot() + 
  geom_point(aes(x = sample_date, y = concentration_ppb, color = chemical_name)) + 
  theme_minimal()

clean_neonics %>% ggplot(aes(x = year, y = count, fill = neonic)) +
  geom_col()

unique(neonics_data$sample_type)
unique(neonics_data$sample_type)
unique(neonics_data$county)
neonics_data %>% filter(is.na(chemical_name))

# Filter to just show data that has neonic contaminates (1895)
have_neonics <- neonics_data %>% filter(concentration_ppb > 0)

# Save data that has neonics detections  
write_csv(have_neonics, "data/surface_data_2021_contaning_neonics.csv")

# Look into the distinct concentration ranges for each neonic 
distinct_neonics <- neonics_data %>% 
    group_by(county, chemical_name) %>% 
    summarise(min_concentration = round(min(concentration_ppb, na.rm = T), 2),
              min_greater_than_o = round(as.numeric(str_replace(min(concentration_ppb[concentration_ppb > 0], 
                                                              na.rm = T), "Inf", NA_character_)), 2), 
              median_concentration = round(median(concentration_ppb, na.rm = T), 2),
              max_concentration = round(max(concentration_ppb, na.rm = T), 2)) %>% 
  mutate(range = ifelse(max_concentration == 0, "All Values 0", paste(min_concentration, "-", max_concentration))) %>%
  glimpse

# Join to county summaries to get total number of samples information 
neonic_ranges_with_num_samples <- left_join(distinct_neonics, county_summaries) %>% 
  select(county, neomic_type = chemical_name, min_concentration, median_concentration, max_concentration, total_samples_analysed_for_neomics = total_samples) %>% glimpse

summary(neonic_ranges_with_num_samples)

# Save neonic ranges with total number of samples table
write_csv(neonic_ranges_with_num_samples, "data/neonic_ranges_with_num_samples.csv")
