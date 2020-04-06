
createStateCasesComparisonChart <- function(stateData){
  
  
  #number days since state hit 100 cases
  stateData <- stateData %>%
    arrange(Date) %>%
    filter(positive>100) %>%
    group_by(state) %>%
    mutate(daysSince100=row_number()) %>%
    ungroup()
  
  #get latest points for labels
  labels <- stateData %>%
    group_by(state) %>%
    arrange(Date)%>%
    filter(row_number()==n()) %>%
    ungroup()
  
  
  #create straight line
  maxDays <- stateData %>%
    group_by(state) %>%
    summarize(n=n())
  maxDays<- max(maxDays$n)
  maxValue <- 10^ceiling(log10(max(stateData$positive)))

  
  dts <- seq(1, by=1, length.out = maxDays)
  refLine <- tibble(Date=dts)
  refLine[1,"p10"]<-100
  refLine[1,"p12"]<-100
  refLine[1,"p15"]<-100
  refLine[1,"p19"]<-100
  refLine[1,"p26"]<-100
  refLine[1,"p41"]<-100
  refLine[1,"p100"]<-100
  
  refLine <- refLine %>%
    mutate(p10 = round(accumulate(p10, ~ .x * .104 + .x)),
           p12 = round(accumulate(p12, ~ .x * .122 + .x)), 
           p15 = round(accumulate(p15, ~ .x * .149 + .x)), 
           p19 = round(accumulate(p19, ~ .x * .189 + .x)), 
           p26 = round(accumulate(p26, ~ .x * .26 + .x)), 
           p41 = round(accumulate(p41, ~ .x * .413 + .x)), 
           p100 = round(accumulate(p100, ~ .x * 1.0 + .x)), 
    )
  rm(dts, maxDays)
  
  
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
  options(scipen=10000)
  graycol = "gray20"
  stateData1 <- highlight_key(stateData, ~state )
  
  gg <- ggplot(data=stateData1) +
    geom_line(aes(x=daysSince100, y=positive, group=state, col=state))+
    geom_point(aes(x=daysSince100, y=positive, group=state, col=state))+
    #gghighlight(state=="IL", use_group_by = F, use_direct_label = F, 
    #            unhighlighted_params = list(size = 1))+
    #facet_wrap(~State)+
    #geom_text(aes(x=daysSince100, y=Count, label=State))+
    geom_line(data=refLine, aes(x=Date, y=p19), col=graycol, linetype="dashed")+
    geom_line(data=refLine, aes(x=Date, y=p26), col=graycol, linetype="dashed")+
    geom_line(data=refLine, aes(x=Date, y=p41), col=graycol, linetype="dashed")+
    geom_line(data=refLine, aes(x=Date, y=p100), col=graycol, linetype="dashed")+
    #geom_text_repel(data=labels, aes(x=daysSince100, y=positive, label=State))+
    annotate("text", x = 8.6, y = 25000, label = c("doubling every day"), angle=50, size=3, col=graycol)+
    annotate("text", x = 16.5, y = 27000, label = c("...every 2 days"), angle=33, size=3, col=graycol)+
    annotate("text", x = 16.5, y = 4500, label = c("...every 3 days"), angle=23, size=3, col=graycol)+
    annotate("text", x = 16.5, y = 1800, label = c("...every 4 days"), angle=20, size=3, col=graycol)+
    theme_minimal()+
    xlab("") +
    scale_y_log10(limits=c(100,maxValue),minor_breaks=minor_breaks, labels=scales::comma)+
    ggtitle("Comparing IL to Other States Reported Cornoavirus Cases",
            subtitle="Cummulative number of cases, by number of days since 100th case") +
    ylab("Count of reported cases")+
    labs(caption = "data from https://covidtracking.com/api/states/daily.csv")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          #panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"),
          legend.position = "none")

 gg
  
library(plotly)
 gg <- ggplotly(gg)
 gg
 highlight( gg, on = "plotly_hover", off = "plotly_deselect", color = "red" , fill="red")
 
 
 
 set.seed(357)
 xy <- data.frame(letters = rep(c("a", "b", "c"), times = 3),
                  values = runif(9),
                  groups = rep(c("group1", "group2", "group3"), each = 3))
 
 #create a SharedData object for use in the ggplot below, group by 'groups' 
 d <- highlight_key(xy, ~groups )
 
 #create a normal ggplot to fit your needs, but use the SharedData object as data for the chart
 p <- ggplot( d, aes(x = letters, y = values, group = groups)) + theme_bw() + geom_point()
 
 #now ggplotly the newly created ggplot, and add text for the tooltips as needed
 gg <- ggplotly( p, tooltip = "groups" )
 
 #set the highlight-options to your liking, and plot...
 highlight( gg, on = "plotly_hover", off = "plotly_deselect", color = "red" )
 
 
}



