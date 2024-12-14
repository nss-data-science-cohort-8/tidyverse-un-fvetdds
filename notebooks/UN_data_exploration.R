---
  An Exploration of UN Data
---
  
library(tidyverse)

#Using the `read_csv()` function, read the GDP dataset into your notebook as a tibble called `gdp_df`. After reading it in, inspect the first 10 rows and then inspect the last 10 rows. 
```{r}
gdp_df <- read_csv("../data/gdp_per_capita.csv")
first_10_rows <- head(gdp_df, 10)
last_10_rows <- tail(gdp_df, 10)
```
#Drop the 'Value Footnotes' column, and rename the remaining columns to 'Country', 'Year', and 'GDP_Per_Capita'.
```{r}
gdp_df_drop <- gdp_df %>%
  select(-`Value Footnotes`) %>%
  rename(Country = `Country or Area`,
         Year = Year,
         GDP_Per_Capita = Value)
````
#Which years are represented in this dataset? How many observations are there per year? Make a plot to view the number of observations per year.
```{r}
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
````
#Create a new tibble by subsetting `gdp_df` to just the year 2021. Call this new tibble `gdp_2021`.
```{r}
gdp_2021 <- gdp_df_drop %>%
  filter(Year == 2021)
```
# Use `summary()` to find the summary statistics for GDP per capita in 2021. 
```{r}
summary_stat_2021 = gdp_2021 %>%
  summarise(
    mean_gdp_per_capita = mean(GDP_Per_Capita),
    median_gdp_per_capita = median(GDP_Per_Capita),
    sd_gdp_per_capita = sd(GDP_Per_Capita),
    min_gdp_per_capita = min(GDP_Per_Capita),
    max_gdp_per_capita = max(GDP_Per_Capita)
    
  )
```
# Create a histogram of GDP Per Capita numbers for 2021 (you may wish to adjust the number of bins for your histogram).
```{r}
ggplot(gdp_2021, aes(x = GDP_Per_Capita))+
  geom_histogram(binwidth =1000, color = "black", fill = "red") +
  labs(title = "GDP Per Capita in 2021", x = "GDP Per Capita", y = "Count (x 1000)")
  theme(axis.text.x = element_text(angle = 45)
```
# Find the top 5 counties and bottom 5 countries by GDP per capita in 2021.
```{r}
Top_five <- gdp_2021 %>%
  arrange(desc(GDP_Per_Capita)) %>%
  slice_head(n = 5)
bottom_five <- gdp_2021 %>%
  arrange(GDP_Per_Capita) %>%
  slice_head(n =5)
```
# now, return to the full dataset, `gdp_df`. Pivot the data for 1990 and 2021 (using `pivot_wider()` function) so that each row corresponds to a country, each column corresponds to a year, and the values in the table give the GDP_Per_Capita amount. Drop any rows that are missing values for either 1990 or 2021. Save the result to a tibble named `gdp_pivoted`.
```{r}
gdp_pivoted <- gdp_df %>%
  select(-`Value Footnotes`) %>%
  filter(Year < 2022) %>%
  pivot_wider(names_from = Year,
              values_from = Value)%>%
drop_na()
```
#Create a new column in `gdp_pivoted` named `Percent_Change`. This column should contain the percent change in GDP_Per_Capita from 1990 to 2021. Hint: Percent change is calculated as 100*(New Value - Old Value) / Old Value.
```{r}
gdp_pivoted <- gdp_pivoted %>%
  mutate(`Percent Change` = 100 * (`2021`- `1990`) / `1990`)
```

# How many countries experienced a negative percent change in GDP per capita from 1990 to 2021?
```{r}
negative_change_country = gdp_pivoted %>%
  filter(Percent_Change < 0)
neg_change_country_count <- nrow(negative_change_country)
```

# Which country had the highest % change in GDP per capita? Create a line plot showing these country's GDP per capita for all years for which you have data. Put both line charts on the same plot.
```{r}
highest_change_country = gdp_pivoted %>%
  filter(`Percent Change`== max(`Percent Change`)) %>%
  pull('Country or Area')

ggplot(gdp_pivoted, aes(x = reorder(`Country or Area`, -`Percent Change`), y = `Percent Change`))+
  geom_col() +
  labs(title = "Percent GDP per Capita Change by Country from 1990 to 2021",
       x = "Country",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(size =  5, angle = 90, hjust = 1))
```
# 
```{r}
```

