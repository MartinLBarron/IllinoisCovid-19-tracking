#' Daily growth of cases charts


createPercentChangeChart <- function(df, varname, percentChange, startDate, annoteLocation){
  
  
  # Limit to data since March 1 ---------------------------------------------
  df <- df %>%
    filter(Date>=as.Date(startDate))
  
  
  # calculate average percent change ----------------------------------------
  
  
  meangrowth <- df %>%
    summarize(mean=mean({{percentChange}}, na.rm=T))
  
  meangrowth<-meangrowth[[1,"mean"]]
  meangrowthlabel = paste("Avg.", percent(meangrowth))
  
  
  # Create plot -------------------------------------------------------------
  gg<- ggplot(data=df) +
    geom_col(aes(x=Date, y={{percentChange}}), fill="#155F83") +
    geom_hline(yintercept = meangrowth, linetype="dashed")+
    theme_minimal()+
    scale_y_continuous(labels = scales::percent)+
    scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
    ggtitle(paste("Daily % Change in Illinois Coronavirus", varname),
            subtitle=paste("since", format(as.Date(startDate), "%B %d, %Y"))) +
    annotate("text", x=as.Date(annoteLocation), y=meangrowth+.03 ,label=meangrowthlabel)+
    xlab("") + 
    ylab("% Change")+
    labs(caption = "data from https://bit.ly/3b65n9V")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust=.5),
          panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"))
  
  
  fname <- paste0("daily-percent-change-", tolower(varname), "-", mostRecentDate, ".png")
  saveChart(gg, fname, 8, 6)
  
  return(gg)
}

  