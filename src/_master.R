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
ILdt<- IllinoisData[[nrow(IllinoisData), "Date"]]
JHdt <- as.Date(names(stateData[,ncol(stateData)]), "%m/%d/%y")
CTdt <- max(stateTestingData$Date)

if (ILdt!=JHdt | ILdt!=CTdt | JHdt!=CTdt){
  warning("dates don't match")
}
#rm(ILdt, JHdt, CTdt)

# Load individual graph functions -----------------------------------------
source("src/createTotalCaseChart.R")
source("src/createDailyGrowthCharts.R")
source("src/createStateComparisonChart.R")
source("src/createTotalTestingChart.R")
source("src/createILTestingChart.R")
source("src/createILTestingDailyChart.R")

# Run functions -----------------------------------------------------------

#cumulative Illinois Chart
chartOverall <- createTotalCaseChart(IllinoisData)
chartOverall

#create Daily Change Charts
chartPercentChange <- createPercentChangeChart(IllinoisData)
chartPercentChange
chartGrowthRate <- createGrowthRateChangeChart(IllinoisData)
chartGrowthRate

#create IL testing charts
chartILTestingCummulative <- createILTestingChart(IllinoisData)
chartILTestingCummulative

#create IL testing charts daily
chartILTestingDaily <- createILTestingDailyChart(IllinoisData)
chartILTestingDaily


#Create State Comparison Chart
chartStateComparison <- createStateComparisonChart(stateData, IllinoisData)
chartStateComparison

#create State Comparison Testing chart
chartStateTestingComparison <-createStateTestingComparisonChart(stateTestingData)
chartStateTestingComparison





saveCharts <- function(){
  
  #These charts just rely on IL updated date
  fname <- paste0("chart-", mostRecentDate, ".png")
  saveChart(chartOverall, fname, 8, 6)
  
  
  fname <- paste0("percent_change-", mostRecentDate, ".png")
  saveChart(chartPercentChange, fname, 8, 6)
  
  
  fname <- paste0("growth_rate-", mostRecentDate, ".png")
  saveChart(chartGrowthRate, fname, 8, 6)
  
  fname <- paste0("IL-testing-cummulative-", mostRecentDate, ".png")
  saveChart(chartILTestingCummulative, fname, 8, 6)
  
  fname <- paste0("IL-testing-daily-", mostRecentDate, ".png")
  saveChart(chartILTestingDaily, fname, 8, 6)
  
  
  #These charts rely on Johns Hopkins data
  fname <- paste0("state-comparison-", mostRecentDate, ".png")
  saveChart(chartStateComparison, fname, 8, 6)
  
  #This chart relies on covid tracking data
  fname <- paste0("state-testing-comparison-", mostRecentDate, ".png")
  saveChart(chartStateTestingComparison, fname, 12, 4)
  
  
  
}




tweetResults <- function(){
  
  # Tweet these after IL data is updated----------------------------------------
  
  fname <- paste0("chart-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "Cummulative confirmed cases in Illinois.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("percent_change-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "% change in new coronavirus cases in Illinois change since March 1.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("IL-testing-daily-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Number of daily covid-19 tests reported.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("IL-testing-cummulative-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Cummulative covid-19 tests administered in IL.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  #Tweet this after covid tracking data updated --------------------------------
  fname <- paste0("state-testing-comparison-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL covid-19 testing to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  # Tweet this after Johns Hopkins data updated. ------------------------------
  fname <- paste0("state-comparison-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL trajectory to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  
}

