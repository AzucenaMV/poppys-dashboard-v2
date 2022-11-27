library(tidyverse)
library(janitor)
library(lubridate)

kpi <- read_csv('./data/business/KPI1.csv')
kpi <- kpi %>%
  clean_names() %>%
  mutate(date = my(date))


gtrends <- read_csv('./data/business/weekly_ger_flow.csv')
colnames(gtrends) <- c('date','gerbera','flower','month','year')
month <- levels(month(gtrends$date, label = TRUE, abbr = FALSE))
year <- c(2021,2022)

prices <- read_csv('./data/business/prices.csv')
#colnames(prices) <- c('date','market_price','company_price','difference')
prices <- prices %>%
  clean_names() %>%
  mutate(date = mdy(date)) %>%
  filter(date >= as.Date('2021-01-01'))


inflation <- read_csv('./data/external/inflation_change_historical.csv')
#colnames(prices) <- c('date','market_price','company_price','difference')
inflation <- inflation %>%
  clean_names() %>%
  mutate(date = my(date)) %>%
  filter(date >= as.Date('2021-01-01')) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date))

weather <- read_csv('./data/greenhouse/clean_weather.csv')
weather <- weather %>%
  filter(datetime >= as.Date('2021-01-01')) %>%
  mutate(month = month(datetime)) %>%
  mutate(year = year(datetime))

#weather <- weather %>






  
#colnames(prices) <- c('date','market_price','company_price','difference')




