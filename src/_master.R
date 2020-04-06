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
  mutate(newCases=Cases-lag(Cases),
         newDeaths=Deaths-lag(Deaths),
         newTests=Tested-lag(Tested),
         percentChangeCases=(Cases-lag(Cases))/lag(Cases),
         percentChangeTests=(Tested-lag(Tested))/lag(Tested),
         percentChangeDeaths=(Deaths-lag(Deaths))/lag(Deaths)) %>%
  arrange(Date)

#Get state-level data from Johns Hopkins
# #stateData <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# stateData <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# #Get state-level death data from Johns Hopkins
# #stateDeathData <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
# stateDeathData <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

#get state-level testing data from Covid tracking project
stateData <- read_csv("https://covidtracking.com/api/states/daily.csv") %>%
  mutate(Date=as.Date(dateChecked)) %>%
  arrange(Date)

# confirm dates -----------------------------------------------------------
mostRecentDate <- format(IllinoisData[[nrow(IllinoisData), "Date"]],"%Y-%m-%d")
ILdt<- IllinoisData[[nrow(IllinoisData), "Date"]]
statedt <- max(stateData$Date)

if (ILdt!=statedt){
  warning("dates don't match")
}
#rm(ILdt, JHdt, CTdt)

# Load individual graph functions -----------------------------------------

#IL data only
source("src/createCumulativeChart.R")
source("src/createPercentChangeChart.R")

#require state data
source("src/createStateCasesComparisonChart.R")
source("src/createStateTestingComparisonChart.R")
source("src/createStateDeathsComparisonChart.R")

# source('src/createStateTestingPopComparisonChart.R')
# source("src/createILTestingDailyChart.R")
# source("src/createDailyCasesGrowthCharts.R")
# source("src/createDailyDeathsGrowthCharts.R")

# source("src/createUSDeathsCharts.R")
# source("src/createILDeathsChart.R")

# Run charts -----------------------------------------------------------

#cumulative Cases Charts ------------------------------------------------------

createCumulativeChart(IllinoisData, "Cases", Cases, newCases, percentChangeCases, "2020-03-01")
createCumulativeChart(IllinoisData, "Tests", Tested, newTests, percentChangeTests, "2020-03-08")
createCumulativeChart(IllinoisData, "Deaths", Deaths, newDeaths, percentChangeDeaths, "2020-03-15")


createPercentChangeChart(IllinoisData, "Cases", percentChangeCases, "2020-03-01", "2020-03-06")
createPercentChangeChart(IllinoisData, "Tests", percentChangeTests, "2020-03-08", "2020-03-13")
createPercentChangeChart(IllinoisData, "Deaths", percentChangeDeaths, "2020-03-20", "2020-03-20")



#Create State Comparison Chart - Cases
chartStateCasesComparison <- createStateCasesComparisonChart(stateData)
suppressWarnings(print(chartStateCasesComparison))
fname <- paste0("states-cases-", mostRecentDate, ".png")
saveChart(chartStateCasesComparison, fname, 8, 6)

chartStateTestingComparison <-createStateTestingComparisonChart(stateData)
suppressWarnings(print(chartStateTestingComparison))
fname <- paste0("states-tests-", mostRecentDate, ".png")
saveChart(chartStateTestingComparison, fname, 8, 6)

chartStateDeathsComparison <- createStateDeathsComparisonChart(stateData)
suppressWarnings(print(chartStateDeathsComparison))
fname <- paste0("states-deaths-", mostRecentDate, ".png")
saveChart(chartStateDeathsComparison, fname, 8, 6)



tweetResults <- function(){
  

# cases -------------------------------------------------------------------

  fname <- paste0("cummulative-cases-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "Cummulative confirmed cases in Illinois.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)

  fname <- paste0("daily-percent-change-cases-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "% change in new coronavirus cases in Illinois\nChange since March 1.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  

# tested ------------------------------------------------------------------

  fname <- paste0("cummulative-tests-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Cummulative covid-19 tests administered in IL.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
 
  fname <- paste0("daily-percent-change-tests-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "% change in new coronavirus tests in Illinois\nChange since March 8.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  # deaths ------------------------------------------------------------------

  fname <- paste0("cummulative-deaths-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Cummulative covid-19 deaths in IL.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("daily-percent-change-deaths-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "% change in new coronavirus deaths in Illinois\nChange since March 20.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  


# state comparisons -------------------------------------------------------
  
  # Tweet this after Covid tracking data updated. ------------------------------
  fname <- paste0("states-cases-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL cases trajectory to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("states-tests-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL test trajectory to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("states-deaths-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL deaths trajectory to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  

  
}

