---
  An Exploration of UN Data
---
  
library(tidyverse)

#1. Using the `read_csv()` function, read the GDP dataset into your notebook as a tibble called `gdp_df`. After reading it in, inspect the first 10 rows and then inspect the last 10 rows. 
```{r}
gdp_df <- read_csv("../data/gdp_per_capita.csv")
first_10_rows <- head(gdp_df, 10)
last_10_rows <- tail(gdp_df, 10)
```
#2. Drop the 'Value Footnotes' column, and rename the remaining columns to 'Country', 'Year', and 'GDP_Per_Capita'.
```{r}
gdp_df_drop <- gdp_df %>%
  select(-`Value Footnotes`) %>%
  rename(Country = `Country or Area`,
         Year = Year,
         GDP_Per_Capita = Value)
````
#3. Which years are represented in this dataset? How many observations are there per year? Make a plot to view the number of observations per year.
```{r}
unique_year_count <- gdp_df %>%
  count(Year, name = "Observation_count")

ggplot(unique_year_count, aes(x = Year, y = Observation_count))+
  geom_col()+
  labs(title = "Number of Observations per Year", x = "Year", y = "Observation Number per Year")

#4. How many countries are represented in this dataset? Which countries have the fewest observations?

total_country <- gdp_df_drop %>%
  summarise(unique_coun = n_distinct(Country))

country_obs_count <- gdp_df_drop %>%
  count(Country, name = "Observation_count")

sort_fewest_obs <- country_obs_count %>%
  arrange(Observation_count)
````
#5. Create a new tibble by subsetting `gdp_df` to just the year 2021. Call this new tibble `gdp_2021`.
```{r}
gdp_2021 <- gdp_df_drop %>%
  filter(Year == 2021)
```
#6. Use `summary()` to find the summary statistics for GDP per capita in 2021. 
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
#7. Create a histogram of GDP Per Capita numbers for 2021 (you may wish to adjust the number of bins for your histogram).
```{r}
ggplot(gdp_2021, aes(x = GDP_Per_Capita))+
  geom_histogram(binwidth =1000, color = "black", fill = "red") +
  labs(title = "GDP Per Capita in 2021", x = "GDP Per Capita", y = "Count (x 1000)")
  theme(axis.text.x = element_text(angle = 45)
```
#8. Find the top 5 counties and bottom 5 countries by GDP per capita in 2021.
```{r}
Top_five <- gdp_2021 %>%
  arrange(desc(GDP_Per_Capita)) %>%
  slice_head(n = 5)
bottom_five <- gdp_2021 %>%
  arrange(GDP_Per_Capita) %>%
  slice_head(n =5)
```
#9. now, return to the full dataset, `gdp_df`. Pivot the data for 1990 and 2021 (using `pivot_wider()` function) so that each row corresponds to a country, each column corresponds to a year, and the values in the table give the GDP_Per_Capita amount. Drop any rows that are missing values for either 1990 or 2021. Save the result to a tibble named `gdp_pivoted`.
```{r}
gdp_pivoted <- gdp_df %>%
  select(-`Value Footnotes`) %>%
  filter(Year < 2022) %>%
  pivot_wider(names_from = Year,
              values_from = Value)%>%
drop_na()
```
#10. Create a new column in `gdp_pivoted` named `Percent_Change`. This column should contain the percent change in GDP_Per_Capita from 1990 to 2021. Hint: Percent change is calculated as 100*(New Value - Old Value) / Old Value.
```{r}
gdp_pivoted <- gdp_pivoted %>%
  mutate(`Percent Change` = 100 * (`2021`- `1990`) / `1990`)
```

#11. How many countries experienced a negative percent change in GDP per capita from 1990 to 2021?
```{r}
negative_change_country = gdp_pivoted %>%
  filter(Percent_Change < 0)
neg_change_country_count <- nrow(negative_change_country)
```

#12. Which country had the highest % change in GDP per capita? Put both line charts on the same plot.
```{r}
highest_change_country = gdp_pivoted %>%
  filter(`Percent Change`== max(`Percent Change`)) %>%
  pull('Country or Area')

ggplot(gdp_pivoted, aes(x = reorder(`Country or Area`, -`Percent Change`), y = `Percent Change`))+
  geom_col() +
  labs(title = "Percent GDP per Capita Change by Country from 1990 to 2021",
       x = "Country",
       y = "Percent Change (%)")+
  theme(axis.text.x = element_text(size =  2, angle = 90, hjust = 1))
```
# Create a line plot showing these country's GDP per capita for all years for which you have data, Put both line charts on the same plot.

```{r}
eg_china_yearly_GDP <- gdp_df %>%
  filter(`Country or Area` %in% c("Equatorial Guinea" ,"China"))

ggplot(eg_china_yearly_GDP, aes(x = Year, y = Value, color = `Country or Area`))+
  geom_line() +
  labs(title = "GDP per Capita of Top 2 Countries with Highest % Change (1990–2021)",
       x = "Year",
       y = "GDP per Capita") +
  theme(axis.text.x = element_text(size = 10))
```
#13. Read in continents.csv contained in the `data` folder into a new tibble called `continents`. We will be using this tibble to add a new column to our dataset.

```{r}
continents <- read.csv("../data/continents.csv")
```

#14. Merge gdp_df and continents. Keep only the countries that appear in both data frames. Save the result back to gdp_df.

```{r}
merged_table <- gdp_df %>%
  inner_join(continents, "Country")
gdp_df <- merged_table %>%
  select("Country", "Year.y", "Value.y", "Continent")
  ```
#15. Determine the number of countries per continent. Create a bar chart showing this

```{r}
country_per_continent <- gdp_df %>%
  group_by(Continent) %>%
  summarise(`Country Count` = n_distinct(Country))
ggplot(country_per_continent, aes(x = Continent, y = `Country Count`))+
  geom_col()+
  labs(title = "Number of Countries per Continent", 
       x = "Continent Name", 
       y = "Number of Countries")
```
#16. Create a boxplot showing GDP per capita in 2021 split out by continent. What do you notice?
# Europe continent had the highest median GDP per capita and 2 high outliers. Africa showed low GDP per capita compared to other continents with a narrow range indicates more similarity in GDP per capita among African countries. Asia had significant disparity between countries.
GDP_continents <- gdp_2021 %>%
  inner_join(continents, by = "Country")
  
ggplot(GDP_continents, aes(x = Continent, y = GDP_Per_Capita))+
  geom_boxplot()+
  labs(title = "GDP per Capita across Continents in 2021", x = "Continent Name", y = "GDP Per Capita")
```
#17. Read life_expectancy.csv into a tibble named life_expectancy. Do not modify the csv file in order to read this data in.
```{r}
life_expectancy <- read_csv("../data/life_expectancy.csv", header = FALSE)
```
#18.`Drop the Country Code, Indicator Name, and Indicator Code columns. Then use `pivot_longer` to convert your data from wide to long. That is, instead of having one row per country and multiple colums per year, we want to have multiple rows per country and a single column for year. After melting, rename the columns to `Country`, `Year`, and `Life_Expectancy`.
```{r}
life_expectancy_modified <- life_expectancy %>%
  filter(!(`Data Source` %in% c("Data Source", "Last Updated Date")))
  
```
  select(-`Country Code`,-`Indicator Name`,-`Indicator Code`)
#19. What was the first country with a life expectancy to exceed 80?

```{r}

```
#20. Merge `gdp_df` and `life_expectancy`, keeping all countries and years that appear in both tibbles. Save the result to a new tibble named `gdp_le`. If you get any errors in doing this, read them carefully and correct them. Look at the first five rows of your new data frame to confirm it merged correctly. Also, check the last five rows to make sure the data is clean and as expected.

```{r}

```
#21. Create a new tibble, named `gdp_le_2021` by extracting data for the year 2021 from `gdp_le`. How many countries have a life expectancy of at least 80 in 2021?


```{r}

#22. Find the countries that had the top 3 largest GDP per capita figures for 2021. Create a plot showing the change in life expectancy over time for these three countries. This plot should be faceted so that each country is contained in its own figure.``

```{r}

```
#23. Create a scatter plot of Life Expectancy vs GDP per Capita for the year 2021. What do you notice?

```{r}

```
#24. Find the correlation between Life Expectancy and GDP per Capita for the year 2021. What is the meaning of this number?
  
  
```{r}


```
#25. Add a column to `gdp_le_2021` and calculate the logarithm of GDP per capita. Find the correlation between the log of GDP per capita and life expectancy. How does this compare to the calculation in the previous part? Look at a scatter plot to see if the result of this calculation makes sense.


```{r}

```
