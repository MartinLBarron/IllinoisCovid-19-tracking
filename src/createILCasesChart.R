#' Simple plot of ILinois Covid-19 cases over time

createILCasesChart <- function(df){
  
  # Create dataframe with just most recent day --------------------------------
  dfMostRecent <- df %>%
    arrange(Date) %>%
    filter(row_number()==n()) %>%
    mutate(Date=as.Date(Date),
           lab=paste0(newCases, " new cases\n", percent(percentChangeCases), " growth"))
  
  # Create plot ------------------------------------------------------------
  gg<- ggplot(data=df) +
    geom_line(aes(x=Date, y=Cases), size=2, col="gray50") +
    geom_point(aes(x=Date, y=Cases), size=2, col="gray50")+
    geom_point(data=dfMostRecent, aes(x=Date, y=Cases), size=4, col="#8F3931")+
    geom_text(data=dfMostRecent, aes(x=Date-1, y=Cases, label=lab), hjust="right")+
    theme_minimal()+
    ggtitle("Illinois Coronavirus Cases (Cummulative)") +
    xlab("") + 
    ylab("Count of reported cases")+
    scale_x_date(breaks= as.Date(c("2020-01-24",
                                   "2020-03-01",
                                   mostRecentDate)),
                 date_labels = "%b-%d") +
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


saveChart <- function(chart, filename, width, height){
  ggsave(filename=here("output", filename), 
         plot=chart,
         width=width, 
         height=height)
}





