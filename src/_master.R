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
         newDeaths=Deaths-lag(Deaths),
         growthFactor=newcases/lag(newcases)) %>%
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
source("src/createILCasesChart.R")
source("src/createILTestingChart.R")
source("src/createILDeathsChart.R")

#require state data
source("src/createStateCasesComparisonChart.R")
source("src/createStateTestingComparisonChart.R")
source("src/createStateDeathsComparisonChart.R")
source('src/createStateTestingPopComparisonChart.R')

source("src/createILTestingDailyChart.R")
source("src/createDailyGrowthCharts.R")

# source("src/createUSDeathsCharts.R")
# source("src/createILDeathsChart.R")

# Run functions -----------------------------------------------------------

#cumulative IL Charts
chartILCasesCummulative <- createILCasesChart(IllinoisData)
chartILCasesCummulative
fname <- paste0("cases-", mostRecentDate, ".png")
saveChart(chartILCasesCummulative, fname, 8, 6)


chartILTestingCummulative <- createILTestingChart(IllinoisData)
chartILTestingCummulative
fname <- paste0("tests-", mostRecentDate, ".png")
saveChart(chartILTestingCummulative, fname, 8, 6)

chartILDeathsCummulative <- createILDeathsChart(IllinoisData)
chartILDeathsCummulative
fname <- paste0("deaths-", mostRecentDate, ".png")
saveChart(chartILDeathsCummulative, fname, 8, 6)


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




#create Daily Change Charts
chartPercentChange <- createPercentChangeChart(IllinoisData)
chartPercentChange
fname <- paste0("daily-percent-change-", mostRecentDate, ".png")
saveChart(chartPercentChange, fname, 8, 6)


#create IL testing charts daily
chartILTestingDaily <- createILTestingDailyChart(IllinoisData)
chartILTestingDaily
fname <- paste0("daily-growth-", mostRecentDate, ".png")
saveChart(chartPercentChange, fname, 8, 6)


#This chart relies on covid tracking data
chartStatepercapitaTestingComparison <- createStateTestingpopComparisonChart(stateData)
chartStatepercapitaTestingComparison
fname <- paste0("per-capita-testing-comparison-", mostRecentDate, ".png")
saveChart(chartStatepercapitaTestingComparison, fname, 12, 4)


#Create State Death Charts
# chartUSDeath <- createUSDeathsCharts(stateTestingData)
# chartUSDeath

# #cumulative Illinois Death Chart
# chartOverallDeaths <- createTotalDeathsChart(IllinoisData)
# chartOverall



tweetResults <- function(){
  
  # Tweet these after IL data is updated----------------------------------------
  
  fname <- paste0("cases-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "Cummulative confirmed cases in Illinois.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("tests-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Cummulative covid-19 tests administered in IL.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("deaths-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Cummulative covid-19 deaths in IL.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  # Tweet this after Johns Hopkins data updated. ------------------------------
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
  
  
  
  #Tweet this after covid tracking data updated --------------------------------
  fname <- paste0("per-capita-testing-comparison-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing per-capita IL covid-19 testing to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  
  
  
  
  fname <- paste0("percent_change-", mostRecentDate, ".png")
  outfile <- here("output", fname)
  tweetText <- "% change in new coronavirus cases in Illinois change since March 1.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  fname <- paste0("IL-testing-daily-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Number of daily covid-19 tests reported.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  

  

  
  # Tweet this after Johns Hopkins data updated. ------------------------------
  fname <- paste0("state-comparison-", mostRecentDate, ".png")
  outfile<- here("output", fname)
  tweetText <- "Comparing IL trajectory to other states.\n#covid19 #coronavirus #IL #Illinois"
  post_tweet(tweetText, media = outfile)
  
  
  
}

