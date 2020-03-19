#' create a state-based comparison to IL


# Get Data (remote) ------------------------------------------------------------

createStateComparisonChart <- function(stateData, IllinoisData){
  
  #clean up first two var names
  nms <- names(stateData)
  nms[1] <- "State"
  nms[2] <- "Country_Region"
  names(stateData) <- nms

  #limit to US States
  stateData <- stateData %>%
    filter(Country_Region=="US") %>%
    filter(!grepl(",", State)) %>%
    filter(!State %in% c("Diamond Princess", "Grand Princess")) %>%
    select(-Country_Region, -Lat, -Long) %>%
    gather(key="Date", "Count", -State) %>%
    mutate(Date=as.Date(Date, "%m/%d/%y")) %>%
    filter(Date>=as.Date("2020-03-10"))
  
  #Add data series that were missing for three states that hit 100 cases before 3/10
  NY <- tibble(State="New York",
               Date=as.Date(c("2020-03-08", "2020-03-09")),
               Count=c(106,142))
  Washington <-  tibble(State="Washington",
                        Date=as.Date(c("2020-03-07", "2020-03-08", "2020-03-09")),
                        Count=c(102,136, 162))
  
  California <-  tibble(State="California",
                        Date=as.Date(c("2020-03-09")),
                        Count=c(140))
  
  stateData <- bind_rows(stateData, NY, Washington, California) %>%
    arrange(State, Date)
  rm(NY, Washington, California)
  
  #replace there IL numbers with mine
  stateData <- stateData %>%
    filter (State!="Illinois")
  
  IllinoisData <- IllinoisData %>%
    select(Date, Cases) %>%
    mutate(State="Illinois", 
           Count=Cases) %>%
    select(-Cases)
  
  stateData <- bind_rows(stateData, IllinoisData)
  

  #number days since state hit 100 cases
  stateData <- stateData %>%
    filter(Count>100) %>%
    group_by(State) %>%
    mutate(daysSince100=row_number()) %>%
    ungroup()
  
  #get latest points for labels
  labels <- stateData %>%
    group_by(State) %>%
    arrange(Date)%>%
    filter(row_number()==n()) %>%
    ungroup()
  
  
  #create straight line
  maxDays <- stateData %>%
    group_by(State) %>%
    summarize(n=n())
  maxDays<- max(maxDays$n)
  maxValue <- 10^ceiling(log10(max(stateData$Count)))
  ceiling(log10(maxValue))
  
  dts <- seq(1, by=1, length.out = maxDays)
  refLine <- tibble(Date=dts)
  refLine[1,"p30"]<-100
  refLine <- refLine %>%
    mutate(p30 = round(accumulate(p30, ~ .x * .30 + .x)))
  rm(dts, maxDays)
  
  
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
  
  
  gg <- ggplot(data=stateData) +
    geom_line(aes(x=daysSince100, y=Count, group=State, col=State), col="blue", size=2)+
    geom_point(aes(x=daysSince100, y=Count, group=State, col=State), col="blue", size=2)+
    gghighlight(State=="Illinois", use_group_by = F, use_direct_label = F, 
                unhighlighted_params = list(size = 1))+
    #facet_wrap(~State)+
    #geom_text(aes(x=daysSince100, y=Count, label=State))+
    geom_line(data=refLine, aes(x=Date, y=p30), col="red", linetype="dashed")+
    #geom_text_repel(data=labs, aes(x=daysSince100, y=Count, label=State))+
    theme_minimal()+
    xlab("") +
    scale_y_log10(limits=c(100,maxValue),minor_breaks=minor_breaks)+
    ggtitle("Comparing IL to Other States Reported Cornoavirus Cases",
            subtitle="Cummulative number of cases, by number of days since 100th case") +
    ylab("Count of reported cases")+
    labs(caption = "data from https://github.com/CSSEGISandData/COVID-19")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          #panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"),
          legend.position = "none")
  return(gg)
}



