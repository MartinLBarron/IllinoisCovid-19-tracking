#' Daily growth of cases charts


createCasesPercentChangeChart <- function(df){
  
  
  # Limit to data since March 1 ---------------------------------------------
  df <- df %>%
    filter(Date>=as.Date("2020-03-01"))
  
  
  # calculate average percent change ----------------------------------------
  
  
  meangrowth <- mean(df$percentChangeCases)
  meangrowthlabel = paste("Avg.", percent(meangrowth))
  
  
  # Create plot -------------------------------------------------------------
  gg<- ggplot(data=df) +
    geom_col(aes(x=Date, y=percentChangeCases), fill="#155F83") +
    geom_hline(yintercept = meangrowth, linetype="dashed")+
    theme_minimal()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
    ggtitle("Daily % Change in Illinois Coronavirus Cases",
            subtitle="since March 1, 2020") +
    annotate("text", x=as.Date("2020-03-06"), y=meangrowth+.03 ,label=meangrowthlabel)+
    xlab("") + 
    ylab("% Change")+
    labs(caption = "data from https://bit.ly/2IDLMlc\nhttps://bit.ly/3b65n9V")+
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

createGrowthRateChangeChart <- function(df){
  
  # Limit to data since March 1 ---------------------------------------------
  df <- df %>%
    filter(Date>=as.Date("2020-03-01"))
  
  
  # calculate average percent change ----------------------------------------
  
  
  growth <- df$growthFactor[!is.infinite(df$growthFactor) & !is.nan(df$growthFactor)]
  meangrowth <- mean(growth, na.rm=T)
  meangrowthlabel = paste("Avg.", round(meangrowth,2))
  
  # Create plot -------------------------------------------------------------
  gg <- ggplot(data=df) +
    geom_col(aes(x=Date, y=growthFactor), fill="#155F83") +
    geom_hline(yintercept = meangrowth, linetype="dashed")+
    annotate("text", x=as.Date("2020-03-07"), y=meangrowth+.2 ,label=meangrowthlabel)+
    theme_minimal()+
    scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
    ggtitle("Daily Growth Rate in Illinois Coronavirus Cases",
            subtitle="since March 1, 2020") +
    xlab("") + 
    ylab("Growth Rate")+
    labs(caption = "data from https://bit.ly/2IDLMlc\nhttps://bit.ly/3b65n9V")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle=element_text(hjust=.5),
          panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"))
  return(gg)
}


