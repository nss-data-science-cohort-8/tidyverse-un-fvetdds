---
  An Exploration of UN Data
---
  
library(tidyverse)

#Using the `read_csv()` function, read the GDP dataset into your notebook as a tibble called `gdp_df`. After reading it in, inspect the first 10 rows and then inspect the last 10 rows. 

gdp_df <- read_csv("C:/Users/vetdd/Documents/NSS_project/tidyverse-un-fvetdds/data/gdp_per_capita.csv")
first_10_rows <- head(gdp_df, 10)
last_10_rows <- tail(gdp_df, 10)

#Drop the 'Value Footnotes' column, and rename the remaining columns to 'Country', 'Year', and 'GDP_Per_Capita'.

gdp_df_drop <- gdp_df %>%
  select(-`Value Footnotes`) %>%
  rename(Country = `Country or Area`,
         Year = Year,
         GDP_Per_Capita = Value)
#Which years are represented in this dataset? How many observations are there per year? Make a plot to view the number of observations per year.

unique_year_count <- gdp_df %>%
  count(Year, name = "Observation_count")

ggplot(unique_year_count, aes(x = Year, y = Observation_count))+
  geom_col()+
  labs(title = "Number of Observations per Year", x = "Year", y = "Observation Number per Year")

# How many countries are represented in this dataset? Which countries have the fewest observations?

total_country <- gdp_df_drop %>%
  summarise(unique_coun = n_distinct(Country))

country_obs_count <- gdp_df_drop %>%
  count(Country, name = "Observation_count")

sort_fewest_obs <- country_obs_count %>%
  arrange(Observation_count)
#Create a new tibble by subsetting `gdp_df` to just the year 2021. Call this new tibble `gdp_2021`.
gdp_2021 <- gdp_df_drop %>%
  filter(Year == 2021)
# Use `summary()` to find the summary statistics for GDP per capita in 2021. 
summary_stat_2021 = gdp_2021 %>%
  summarise(
    mean_gdp_per_capita = mean(GDP_Per_Capita),
    median_gdp_per_capita = median(GDP_Per_Capita),
    sd_gdp_per_capita = sd(GDP_Per_Capita),
    min_gdp_per_capita = min(GDP_Per_Capita),
    max_gdp_per_capita = max(GDP_Per_Capita)
    
  )
#Create a histogram of GDP Per Capita numbers for 2021 (you may wish to adjust the number of bins for your histogram).
ggplot(gdp_2021, aes(x = Country, y = GDP_Per_Capita))+
  geom.bar(stat = count, fill = "red")


