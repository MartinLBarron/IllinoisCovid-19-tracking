#' Daily growth of cases charts

df <-IllinoisData
createDeathsPercentChangeChart <- function(df){
  
  
  # Limit to data since March 1 ---------------------------------------------
  df <- df %>%
    filter(Date>=as.Date("2020-03-20"))
  
  
  # calculate average percent change ----------------------------------------
  
  
  meangrowth <- mean(df$percentChangeDeaths, na.rm=T)
  meangrowthlabel = paste("Avg.", percent(meangrowth))
  
  
  # Create plot -------------------------------------------------------------
  gg<- ggplot(data=df) +
    geom_col(aes(x=Date, y=percentChangeDeaths), fill="#155F83") +
    geom_hline(yintercept = meangrowth, linetype="dashed")+
    theme_minimal()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
    ggtitle("Daily % Change in Illinois Coronavirus Deaths",
            subtitle="since March 20, 2020") +
    annotate("text", x=as.Date("2020-03-20"), y=meangrowth+.03 ,label=meangrowthlabel)+
    xlab("") + 
    ylab("% Change")+
    labs(caption = "data https://bit.ly/3b65n9V")+
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
