library(tidyverse)

# Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

# Pipe Operator: CTRL + SHFT + M --> %>% 
  # It takes the output of one function and passes it into another function as an argument.
  # This allows us to link a sequence of analysis steps.
  # BIG PICTURE: Makes code EASIER TO READ

# Mean life expectancy:
  # Overall
    gapminder_data %>%  # It saves "gapminder_data"....
      summarize(averageLifeExp = mean(lifeExp)) #... and puts it as 1st argument of "summarize"
  # Most recent year
    gapminder_data %>%
      filter(year == max(year)) %>% 
      summarize(averageLifeExp_mostRecent = mean(lifeExp))
  # Each year
    gapminder_data %>%
      group_by(year) %>% 
      summarize(averageLifeExp = mean(lifeExp))
  # Each continent
    gapminder_data %>%
      group_by(continent) %>% 
      summarize(averageLifeExp = mean(lifeExp))
    
# Mean population AND mean Life Expectancy:
  gapminder_data %>%
    summarize(meanPop = mean(pop/10^6), averageLifeExp = mean(lifeExp))

# Mean mean Life Expectancy AND GDPpc FOR EACH continent
    gapminder_data %>%
      group_by(continent) %>% 
      summarize(meanLifeExp = mean(lifeExp),meanGDPpc = mean(gdpPercap))  
    
# GDPpc for each country
  gapminder_data %>%
    group_by(country) %>% 
    summarize(meanGDPpc = mean(gdpPercap))
  
# GDP per capita for first year
  gapminder_data %>%
    filter(year == min(year)) %>% 
    summarize(meanGDPpc = mean(gdpPercap))

# Adding GDP + Population in millions
  # NOTE: adding a new column --> "mutate"
  gapminder_data_plus <- gapminder_data %>%
    mutate(gdp = gdpPercap*pop, popMil = pop/10^6)

# Select only country, continent, year, lifeexp
  # "select()" chooses a subset of columns from a dataset
    # few columns: select(col1,col2)
    # a lot of columns, except a few: select(-colX)
  gapminder_data %>% 
    select(country,continent,year,lifeExp)
  
  # Select "helper" function: start_with(), ends_with(), contains(), etc.
  gapminder_data %>% 
    select(year,starts_with("c"))

# GENERAL NOTE:
  # Conditions on ROWS --> use filter()
  # Conditions on COLUMNS --> use select()
  
# VECTOR = an array of values of the SAME type (ex: each column of tables)
  # Example:
  my_vec <- c(1,2,"a")
  # proof that columns are vectors
  proof <- gapminder_data %>% 
    pull(year)
  # Vectors are helpful. Example: want to identify certain ids in dataset
    # filter(id %in% c("id1","id2",etc))
  
################################################################################
# RESHAPE functions
  
  # WIDER --> lifeExp
  gapminder_data %>% 
    select(country,continent,year,lifeExp) %>% 
    pivot_wider(names_from = year, values_from = lifeExp)
  
  # WIDER --> GDPpc
  gapminder_data %>% 
    select(country,continent,year,gdpPercap) %>% 
    pivot_wider(names_from = year, values_from = gdpPercap)
  
  # LONGER --> 
  gapminder_data %>%
    pivot_longer(cols = c(pop,lifeExp,gdpPercap), 
                 names_to = "measurement type", 
                 values_to = "measurement")
    
# Relationship between GDP and CO2 emissions
  # Gapminder data
    # Focus on one year: 2007 & continent = Americas
    gapminder_data_2007 <- gapminder_data %>% 
      filter(year == 2007 & continent == "Americas") %>% 
      select(-year,-continent)
  
################################################################################
# CO2 data
    # PROBLEM 1: first row shouldnt be there, 2nd row has col names
    # PROBLEM 2: we don't like the names
  co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
                       col_names = c("region","country","year","series",
                                     "value","footnotes","source"))
    # Shortening a string using RECODE 
  co2_emissions <- co2_emissions_dirty %>% 
      select(country,year,series,value) %>% 
      mutate(series = recode(series,"Emissions (thousand metric tons of carbon dioxide)" =
                               "total_emissions",
                             "Emissions per capita (metric tons of carbon dioxide)" =
                             "emissions_pc"
                             )
             ) %>% 
    # Pivot wider
    pivot_wider(names_from = series,
                values_from = value) %>% 
    # Filtering
      filter(year == 2005) %>% 
      select(-year)
  
# Joining both datasets using INNER JOIN
inner_join(gapminder_data_2007,co2_emissions, by = "country")

# ... we ran out of time
    

# Percent of CO2 emissions acocunted by North America
