library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

#1.Read CSV 
file_path <- "C:/Users/sl137/OneDrive/Documents/StormEvents_details-ftp_v1.0_d2024_c20240620.csv"
data <- read_csv(file_path)
#2.Selecting
selected_data <- data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)
#3.Sorting
arranged_data <- selected_data %>%
  arrange(STATE)
#4.Title case
final_data <- arranged_data %>%
  mutate(
    STATE = str_to_title(STATE),
    CZ_NAME = str_to_title(CZ_NAME)
  )

#5 CZ_TYPE = “C” and then remove the CZ_TYPE column
county_fips_data <- final_data %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

#6 Pad the state and county FIPS with a “0” at the beginning
county_fips_data <- county_fips_data %>%
  mutate(
    STATE_FIPS = str_pad(STATE_FIPS, width = 3, side = "left", pad = "0"),
    CZ_FIPS = str_pad(CZ_FIPS, width = 3, side = "left", pad = "0")
  ) %>%
  unite("FIPS", STATE_FIPS, CZ_FIPS, sep = "")

#7 Change all column names to lower case
county_fips_data <- county_fips_data %>%
  rename_all(tolower)

#8  Load state data from base R
data("state")
state_info <- data.frame(
  state = str_to_title(tolower(state.name)),
  area = state.area,
  region = state.region
)

#9 Create a dataframe with the number of events per state. Merge in the state information
#Remove any states that are not in the state information dataframe.
events_per_state <- county_fips_data %>%
  group_by(state) %>%
  summarise(num_events = n())

merged_data <- merge(events_per_state, state_info, by = "state")
final_merged_data <- merged_data %>%
  filter(!is.na(region))
print(final_merged_data)

#10 scatter plot
ggplot(merged_data, aes(x = area, y = num_events, color = region)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Number of Storm Events in 2024 vs. Land Area",
       x = "Land area (square miles)",
       y = "# of storm events in 2024") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))