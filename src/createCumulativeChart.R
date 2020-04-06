#' Simple plot of ILinois Covid-19 cases over time

createCumulativeChart <- function(df, varname, var, new, pchange, startDate){
  df <- df %>%
    filter(Date>=as.Date(startDate))
  # Create dataframe with just most recent day --------------------------------
  dfMostRecent <- df %>%
    arrange(Date) %>%
    filter(row_number()==n()) %>%
    mutate(Date=as.Date(Date),
           lab=paste0(scales::comma({{new}}), " new cases\n", percent({{pchange}}), " growth"))
  
  # Create plot ------------------------------------------------------------
  gg<- ggplot(data=df) +
    geom_line(aes(x=Date, y={{var}}), size=2, col="gray50") +
    geom_point(aes(x=Date, y={{var}}), size=2, col="gray50")+
    geom_point(data=dfMostRecent, aes(x=Date, y={{var}}), size=4, col="#8F3931")+
    geom_text(data=dfMostRecent, aes(x=Date-1, y={{var}}, label=lab), hjust="right")+
    theme_minimal()+
    ggtitle(paste("Illinois Coronavirus", varname, "(Cummulative)"))+
    xlab("") + 
    ylab(paste("Count of reported", tolower(varname)))+
    scale_x_date(date_labels = "%b-%d") +
    scale_y_continuous(labels=scales::comma)+
    labs(caption = "data from https://bit.ly/3b65n9V")+
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          panel.grid.minor.y=element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.ticks.x=element_line(),
          axis.ticks.length.x = unit(3, "pt"))
  
  fname <- paste0("cummulative-", tolower(varname), "-", mostRecentDate, ".png")
  saveChart(gg, fname, 8, 6)
  
  return(gg)
  
}

saveChart <- function(chart, filename, width, height){
  ggsave(filename=here("output", filename), 
         plot=chart,
         width=width, 
         height=height)
}





