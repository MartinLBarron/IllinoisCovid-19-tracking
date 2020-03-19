#' Master file to produce all Covid-19 related charts
#' 


# Load necessary libraries ------------------------------------------------
library(rtweet)
library(tidyverse)
library(here)
library(scales)
library(ggrepel)
library(gghighlight)


# Load data ---------------------------------------------------------------

#get my time series of Illinois Data
IllinoisData <- read_csv("data/IL-covid-19-cases.csv") %>%
  mutate(Date=as.Date(Date, "%m/%d/%y")) %>%
  mutate(percentChange=(Cases-lag(Cases))/lag(Cases),
         newcases=Cases-lag(Cases),
         growthFactor=newcases/lag(newcases)) %>%
  arrange(Date)

#Get state-level data from Johns Hopkins
stateData <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

#get state-level testing data from Covid tracking project
stateTestingData <- read_csv("https://covidtracking.com/api/states/daily.csv") %>%
  mutate(Date=as.Date(dateChecked))

# confirm dates -----------------------------------------------------------
mostRecentDate <- format(IllinoisData[[nrow(IllinoisData), "Date"]],"%Y-%m-%d")

# Load individual graph functions -----------------------------------------
source("src/createTotalCaseChart.R")
source("src/createDailyGrowthCharts.R")
source("src/createStateComparisonChart.R")
source("src/createTotalTestingChart.R")

# Run functions -----------------------------------------------------------

#cumulative Illinois Chart
chartOverall <- createTotalCaseChart(IllinoisData)
chartOverall
fname <- paste0("chart-", mostRecentDate, ".png")
saveChart(chartOverall, fname, 8, 6)

#create Daily Change Charts
chartPercentChange <- createPercentChangeChart(IllinoisData)
chartPercentChange
fname <- paste0("percent_change-", mostRecentDate, ".png")
saveChart(chartPercentChange, fname, 8, 6)

chartGrowthRate <- createGrowthRateChangeChart(IllinoisData)
chartGrowthRate
fname <- paste0("growth_rate-", mostRecentDate, ".png")
saveChart(chartGrowthChange, fname, 8, 6)

#Create State Comparison Chart
chartStateComparison <- createStateComparisonChart(stateData, IllinoisData)
chartStateComparison
fname <- paste0("state-comparison-", mostRecentDate, ".png")
saveChart(chartStateComparison, fname, 8, 6)

#create State Comparison Testing chart
chartStateTestingComparison <-createStateTestingComparisonChart(stateTestingData)
chartStateTestingComparison
fname <- paste0("state-testing-comparison-", mostRecentDate, ".png")
saveChart(chartStateTestingComparison, fname, 12, 4)

