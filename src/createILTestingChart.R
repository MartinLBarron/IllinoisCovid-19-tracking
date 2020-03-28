
createILTestingChart <- function(IllinoisData){
  
  df <- IllinoisData %>%
    select(Date, Tested) %>%
    filter(Date>as.Date("2020-03-04")) %>%
    arrange(Date) %>%
    mutate(newTests = Tested - lag(Tested)) %>%
    mutate(percentChange=newTests/lag(Tested))
    
  
  # Create dataframe with just most recent day --------------------------------
  dfMostRecent <- df %>%
    arrange(Date) %>%
    filter(row_number()==n()) %>%
    mutate(Date=as.Date(Date),
           lab=paste0(newTests, " new tests\n", percent(percentChange), " growth"))
  
  
  # Create plot -------------------------------------------------------------
  
  CELColor::show_palette(CELColor::cel_pal(4))
  
  gg <- ggplot(df)+
    geom_line(aes(x=Date, y=Tested), col="gray50", size=2)+
    geom_point(aes(x=Date, y=Tested), size=2, col="gray50")+
    geom_point(data=dfMostRecent, aes(x=Date, y=Tested), size=4, col="#8F3931")+
    geom_text(data=dfMostRecent, aes(x=Date-1, y=Tested, label=lab), hjust="right")+
    theme_minimal()+
    ggtitle("Illinois Coronavirus Tests Administered (Cummulative)") +
    xlab("") + 
    ylab("Count of Administered Tests")+
    scale_x_date(date_labels = "%b-%d") +
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