library(ggplot2)
library(usmap)
library(sf)
library(gganimate)
library(tidyverse)

createGrowthMap <- function(){
  
  # Get County Data ---------------------------------------------------------
  
  #data from https://github.com/nytimes/covid-19-data
  counties_df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  mostRecentDate <- max(counties_df$date)
  print(mostRecentDate)
  
  # Get Map shape file ------------------------------------------------------
  counties_shp <- read_rds("data/us_counties_transformed.rds") %>%
    filter(STATEFP != "72") #No data for PR
  states_shp <- read_rds("data/us_states_transformed.rds") %>%
    filter(STATEFP != "72") #No data for PR
  counties_shp$GEOID <- as.character(counties_shp$GEOID)
  
  # merge county data with shape file ---------------------------------------
  maxCases<-max(counties_df$cases)
  print(maxCases)
  if (maxCases>100000){
    stop("More than 100k cases in a single area")
  }
  
  counties_df1 <- counties_df %>% 
    mutate(cases_fct = factor(case_when(cases<=0 ~ "None",
                                        cases<=10 ~ "1-10",
                                        cases<=100 ~ "11-100",
                                        cases<=1000~ "101-1,000",
                                        cases<=10000 ~"1,001-10,000",
                                        cases<=100000 ~"10,001-100,000",
                                        TRUE ~ "error"))) %>%
    mutate(cases_fct = fct_relevel(cases_fct, "None",
                                   "1-10",
                                   "11-100",
                                   "101-1,000",
                                   "1,001-10,000",
                                   "10,001-100,000"
    )) %>%
    select(fips, cases_fct, date)
  
  counties_df1$day <- counties_df1 %>%
    group_indices(date)

  # construct animation file ------------------------------------------------
  
  maxDays <- max(counties_df1$day)
  
  i=1
  for (i in 1:maxDays){
    day <- counties_df1 %>%
      filter(day==i) 
    
    dt<-day[[1,"date"]]
    
    
    day <- left_join(counties_shp, day, by=c("GEOID" ="fips")) %>%
      mutate(cases_fct=replace_na(cases_fct, "None"),
             day=i,
             date=dt,
             date1=format(date, "%b %d"))
    
    if (i==1){
      all <- day
    }else{
      all <- rbind(all,day)
    }
  }
  
  all1<-filter(all, day==74)
  
  # create map --------------------------------------------------------------
  anim <- ggplot() +
    geom_sf(data=states_shp, fill="gray50", col=NA)+
    geom_sf(data=all, aes(fill=cases_fct, group=date), col=NA )+
    geom_sf(data=states_shp, fill=NA)+
    #scale_fill_brewer(palette="YlOrRd")+
    scale_fill_manual(values=c("gray70", "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20"))+
    labs(title = "The Spread of Covid-19",
         subtitle = 'Date: {format(frame_time, "%b %d")}') +
    labs(caption = "data from https://github.com/nytimes/covid-19-data")+
    transition_time(date) +
    ease_aes('linear')+
    mbRutilities::theme_map()+
    theme(plot.subtitle = element_text(hjust=.5,
                                       size=15),
          plot.title = element_text(size=20),
          legend.text = element_text(size=10),
          legend.title = element_blank(),
          legend.position = c(.65, .02),
          plot.caption = element_text(size=10)
          
    )
  # anim
  # ggsave("test.png", anim, width=15, height=15)
  
  animate(
    plot=anim,
    nframes=200,
    end_pause=75,
    width= 650, 
    height=650
  )
  anim_save(paste0("output/covid-spread-map-", mostRecentDate, ".gif"))
  
}

createGrowthMap()
