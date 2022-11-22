library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
# Data Summary values
#----------------------------------------------------------------------------#
# 1. What is the average value of prison population count by race in 2018? 
by_race_2018_df <- incarceration_df[,c('year','county_name',
                                       'aapi_jail_pop','black_jail_pop',
                                       'latinx_jail_pop','native_jail_pop',
                                       'white_jail_pop', 'other_race_jail_pop' 
)] %>% 
  group_by(year) %>% 
  filter(year == "2018")

avg_pop_by_race_2018 <- by_race_2018_df %>% 
  summarize(aapi_avg = mean(aapi_jail_pop, na.rm = TRUE),
            black_avg = mean(black_jail_pop, na.rm = TRUE),
            latinx_avg = mean(latinx_jail_pop, na.rm = TRUE),
            native_avg = mean(native_jail_pop, na.rm = TRUE),
            white_avg = mean(white_jail_pop, na.rm = TRUE),
            other_race_avg = mean(other_race_jail_pop, na.rm = TRUE)
  ) %>% 
  select(aapi_avg, black_avg, latinx_avg, 
         native_avg, white_avg, other_race_avg)
rounded_avg_race_pop <- round(avg_pop_by_race_2018, 1)
rounded_avg_race_pop
# 2. Where is prison population count by race the highest?  
highest_aapi <- incarceration_df %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE))%>% 
  select(county_name, aapi_jail_pop, year) %>% 
  rename(aapi_high = aapi_jail_pop)

highest_black <- incarceration_df %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))%>% 
  select(county_name, black_jail_pop, year) %>% 
  rename(black_high = black_jail_pop)
highest_black

highest_latinx <- incarceration_df %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE))%>% 
  select(county_name, latinx_jail_pop, year) %>% 
  rename(latinx_high = latinx_jail_pop)
highest_latinx

highest_native <- incarceration_df %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE))%>% 
  select(county_name, native_jail_pop, year) %>% 
  rename(native_high = native_jail_pop)
highest_native

highest_white <- incarceration_df %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE))%>% 
  select(county_name, white_jail_pop, year) %>% 
  rename(white_high = white_jail_pop)
highest_white

highest_other_race <- incarceration_df %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE))%>% 
  select(county_name, other_race_jail_pop, year) %>% 
  rename(other_race_high = other_race_jail_pop)
highest_other_race

# 3. How much has prison population count by race changed over the last 50 years?
sum_2018 <- summarize(
  by_race_2018_df,
  aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
  black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
  latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
  native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
  white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
  other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE),
  )

by_race_1993_df <- incarceration_df[,c('year','county_name',
                                       'aapi_jail_pop','black_jail_pop',
                                       'latinx_jail_pop','native_jail_pop',
                                       'white_jail_pop', 'other_race_jail_pop' 
)] %>% 
  group_by(year) %>% 
  filter(year == "1993")
  
sum_1993 <- summarize(
  by_race_1993_df,
  aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
  black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
  latinx_jail_pop = sum(latinx_jail_pop, na.rm=TRUE),
  native_jail_pop = sum(native_jail_pop, na.rm=TRUE),
  white_jail_pop = sum(white_jail_pop, na.rm=TRUE),
  other_race_jail_pop = sum(other_race_jail_pop, na.rm=TRUE),
)

# Calculate prison population difference by race between 2018 and 1993. (50 years)
aapi_50yr_difference <- round(sum_2018$aapi_jail_pop - sum_1993$aapi_jail_pop, 1)
black_50yr_difference <- round(sum_2018$black_jail_pop - sum_1993$black_jail_pop, 1)
latinx_50yr_difference <- round(sum_2018$latinx_jail_pop - sum_1993$latinx_jail_pop, 1)
native_50yr_difference <- round(sum_2018$native_jail_pop - sum_1993$native_jail_pop, 1)
white_50yr_difference <- round(sum_2018$white_jail_pop - sum_1993$white_jail_pop, 1)
other_race_50yr_difference <- round(sum_2018$other_race_jail_pop - sum_1993$other_race_jail_pop, 1)
#----------------------------------------------------------------------------#

## Section 3  ---- 
# Growth of the U.S. Prison Population
# Bar chart that shows growth of the U.S. prison population 
#----------------------------------------------------------------------------#
# Data Wrangling Function
get_year_jail_pop <- function() {
  data <- incarceration_df %>% 
    mutate(year = as.factor(incarceration_df$year)) %>% 
    filter(!is.na(total_jail_pop)) %>% 
    select(year, total_jail_pop) %>% 
    arrange(year)
  
  sum_year <- data %>% 
    group_by(year) %>% 
    summarize(sum_pop = sum(total_jail_pop))
  return(sum_year)
}
year_pop <- get_year_jail_pop()


# Plotting Function
plot_jail_pop_by_us <- function() {
  chart <- ggplot(year_pop) +
    geom_col(mapping =  aes(x = year, y = sum_pop)) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)"
    )
  return(chart)
}
year_pop_chart <- plot_jail_pop_by_us()
year_pop_chart
#----------------------------------------------------------------------------#

## Section 4 ---- 
# Growth of Prison Population by State  
#----------------------------------------------------------------------------#
# Data Wrangling Function
get_jail_pop_by_states <- function(states) {
  data2 <- incarceration_df %>% 
    group_by(state,year) %>%
    filter(state %in% states) %>%
    summarize(sum_pop = sum(total_jail_pop, na.rm = TRUE))
  return(data2)
}

# Plotting Function
plot_jail_pop_by_states <- function(states) {
  chart2 <- ggplot(get_jail_pop_by_states(states)) + 
    geom_line(mapping = aes(x = year, y = sum_pop, color = state)) +
    labs(
      x = "Year",
      y = "Population by State", 
      title = "Growth of Jail Populaton by state (1970-2018)",
      color = "State"
    )
  return(chart2)
}
plot_jail_pop_by_states(c("WA","CA","OR"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
# Comparison between gender population rate for different levels of urbanicity?
#----------------------------------------------------------------------------#
# Data Wrangling Function
perc_gender_pop <- function() {
  data3 <- incarceration_df %>% 
    filter(!is.na(total_jail_pop)) %>%
    filter(!is.na(female_jail_pop)) %>%
    filter(!is.na(male_jail_pop)) %>%
    select(urbanicity, total_jail_pop, female_jail_pop, male_jail_pop) %>% 
    summarize(f_perc = (female_jail_pop / total_jail_pop)*100,
              m_perc = (male_jail_pop / total_jail_pop)*100,
              urbanicity = urbanicity) %>%
    select(urbanicity, f_perc, m_perc)
  return(data3)
}
gender_urban_pop <- perc_gender_pop()


# Plotting Function
plot_gender_pop_by_urban <- function() {
  chart3 <- ggplot(gender_urban_pop) + 
    geom_point(mapping = aes(x = m_perc, y = f_perc, color = urbanicity),
               alpha = .6) +
    labs(
      x = "% of Male Prison Population",
      y = "% Female Prison Population", 
      title = "Comparison Between Gender Jail Population Rate 
    \nfor Different Levels of Urbanicity (1970-2018)",
      color = "urbanicity"
    )
  return(chart3)
}
gender_urban_chart <- plot_gender_pop_by_urban()
gender_urban_chart
#----------------------------------------------------------------------------#

## Section 6  ---- 
# Map that shows potential patterns of inequality that vary geographically
#----------------------------------------------------------------------------#
# Data Wrangling Function
  data4 <- incarceration_df %>%
    group_by(state) %>% 
    filter(year >= "2004") %>% 
    summarize(state_total = sum(total_jail_from_ice, na.rm = TRUE))
  state_shape1 <- map_data('state')
  state_abbrevs <- data.frame(state.abb, state.name)
  incarceration_trends_state <- left_join(data4, state_abbrevs, 
                                          by=c('state' = 'state.abb'))
  new_incarceration_trends_state <- incarceration_trends_state %>% 
    mutate(region = tolower(state.name))
  state_shape <- left_join(state_shape1, new_incarceration_trends_state)
    
# Plotting Function
  ice_map_plot <- ggplot(state_shape) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = state_total)) + 
    scale_fill_continuous(low = 'black', high = '#c8fe76', labels = 
                            scales::label_number_si()) + 
    coord_map() +
    labs(title = 'Number of People Placed in Jail Due to ICE By State', 
         fill = 'ICE Jail Population Count')
  ggplotly(ice_map_plot) %>% 
    config(displayModeBar = FALSE)
  
ice_map_plot
#----------------------------------------------------------------------------#
