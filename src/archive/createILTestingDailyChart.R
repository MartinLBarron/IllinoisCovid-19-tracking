createILTestingDailyChart <- function(IllinoisData){
  #create a bar plot of new tests conducted each day
  df <- IllinoisData %>%
    arrange(Date) %>%
    mutate(dailyTests=Tested-lag(Tested)) %>%
    filter(Date>as.Date("2020-03-01"))

  
  gg<- ggplot(data=df)+
    geom_col(aes(x=Date, y=dailyTests))+
  theme_minimal()+
    ggtitle("Illinois Coronavirus Tests Administered (Daily)") +
    xlab("") + 
    ylab("Count of administered tests")+
    scale_x_date(breaks="2 days", date_labels = "%b-%d") +
    labs(caption = "data from https://bit.ly/3b65n9V")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"))
  return(gg)
}