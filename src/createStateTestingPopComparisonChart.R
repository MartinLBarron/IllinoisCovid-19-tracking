
createStateTestingpopComparisonChart <- function(stateData){
  
  #get state population data
  statePop <- read_csv("data/nst-est2019-01.csv")
  
  #Join state testing data and state population
  stateData<- inner_join(stateData, statePop, by=c("state"="StateAbbr"))
  
  #Keep most recent data for each
  stateData <- stateData %>%
    group_by(State) %>%
    arrange(Date) %>%
    #filter(row_number()==n()) %>%
    mutate(testsPer10k = total/(Population/10000) )
  
  IL <- stateData %>%
    filter(state=="IL")
  
  gg <- ggplot(data=stateData)+
    geom_col(aes(x=reorder(state, -testsPer10k), y=testsPer10k))+
    geom_col(data=IL, aes(x=state, y=testsPer10k), fill="blue")+
    theme_minimal()+
    ggtitle("State Level Covid-19 Testing",
            subtitle=paste("Testing as of", mostRecentDate)) +
    xlab("") + 
    ylab("Test per 10k of population")+
    labs(caption = "data from https://covidtracking.com/api/states/daily.csv")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust=.5),
          panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"))

  return(gg)
  
}
