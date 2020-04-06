library(tidyverse)
library(lubridate)
library(sf)

df <- read_csv("data/Medical_Examiner_Case_Archive.csv") 
precincts_shp <- read_sf("data/Boundaries - Police Districts (current)/geo_export_86ea2b67-7711-4016-bc7e-6db7d9488877.shp")  
city_shp <- read_sf("data/Boundaries - City/geo_export_06608c00-df7b-43f1-ba3f-d40762e7a947.shp")
temp <- names(df)
names(df) <- make.names(temp)

df <- df %>%
  mutate(dt=mdy_hms(Date.of.Death),
         dt2=date(dt)) %>%
  filter(dt2>as.Date("2020-03-01"))

df <- df %>%
  mutate(CoronaRelated = ifelse(grepl("corona",Primary.Cause, ignore.case = T),T,
                            ifelse(grepl("corona", Primary.Cause.Line.A, ignore.case=T),T,
                                   ifelse(grepl("corona", Primary.Cause.Line.B, ignore.case=T),T,
                                          ifelse(grepl("corona", Primary.Cause.Line.C, ignore.case=T),T,F
                                          ))))) %>%
  filter(CoronaRelated==T & !is.na(longitude))

df = st_as_sf(df, coords = c("longitude", "latitude"), 
                 crs = st_crs(city_shp), agr = "constant")

df$within<- st_within(df, city_shp, sparse=F) 
df <- df %>%
  filter(within==TRUE)

ggplot()+
  geom_sf(data=precincts_shp)+
  geom_sf(data=df) + 
  labs(title="Covid-19 Related Deaths in Chicago",
       subtitle="as of April 4, 2020",
       caption = "data from https://datacatalog.cookcountyil.gov/Public-Safety/\nMedical-Examiner-Case-Archive/cjeq-bs86/data")+ 
  theme_minimal()

ggsave("covid-19-deaths.png")
