#' Simple projection of ILinois Covid-19 cases over time


library(tidyverse)
library(here)

options(scipen=999)
df <- read_csv("data/IL-covid-19-cases.csv") %>%
  mutate(Date=as.Date(Date, "%m/%d/%y")) %>%
  mutate(percentChange=(Illinois-lag(Illinois))/lag(Illinois),
         newcases=Illinois-lag(Illinois))

dfMostRecent <- df %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  mutate(Date=as.Date(Date),
         lab=paste0(newcases, " new cases\n", percent(percentChange), " growth"))

todayDate <- format(dfMostRecent[[1, "Date"]],"%Y-%m-%d")


#expand series
dts <- seq(dfMostRecent[[1, "Date"]], by=1,length.out=28)
temp <- tibble(Date=dts, projection=NA)
temp[1,"p25"] <- dfMostRecent[[1,"Illinois"]]
temp[1,"p30"] <- dfMostRecent[[1,"Illinois"]]
temp[1,"p35"] <- dfMostRecent[[1,"Illinois"]]

temp <- temp %>%
  mutate(p25 = round(accumulate(p25, ~ .x * .25 + .x)),
         p30 = round(accumulate(p30, ~ .x * .30 + .x)),
         p35 = round(accumulate(p35, ~ .x * .35 + .x))) 

dfprojection <- bind_rows(df, temp)

dfprojection <- filter(dfprojection, Date>as.Date("2020-03-01"))

ggplot(data=dfprojection) +
  geom_line(aes(x=Date, y=Illinois), size=2, col="#155F83") +
  geom_line(aes(x=Date, y=p25), linetype="dashed")+
  geom_line(aes(x=Date, y=p30), linetype="dashed")+
  geom_line(aes(x=Date, y=p35), linetype="dashed")+
  scale_y_log10()+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  theme_minimal()+
  ggtitle("Projected Illinois Reported Coronavirus Cases") +
  xlab("") + 
  ylab("Count of reported cases")+
  labs(caption = "data from https://bit.ly/2IDLMlc")+
  theme(panel.grid.minor.x=element_blank(),
        plot.title = element_text(hjust = .5),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

fname <- paste0("projection-log-", todayDate, ".png")
ggsave(here("output", fname), w=8, h=6)

ggplot(data=dfprojection) +
  geom_line(aes(x=Date, y=Illinois), size=2, col="#155F83") +
  geom_line(aes(x=Date, y=p25), linetype="dashed")+
  geom_line(aes(x=Date, y=p30), linetype="dashed")+
  geom_line(aes(x=Date, y=p35), linetype="dashed")+
  #scale_y_log10()+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  theme_minimal()+
  ggtitle("Projected Illinois Reported Coronavirus Cases") +
  xlab("") + 
  ylab("Count of reported cases")+
  labs(caption = "data from https://bit.ly/2IDLMlc")+
  theme(panel.grid.minor.x=element_blank(),
        plot.title = element_text(hjust = .5),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

fname <- paste0("projection-", todayDate, ".png")
ggsave(here("output", fname), w=8, h=6)


dfprojection <- filter(dfprojection, Date>=as.Date(todayDate))
dfprojection <- dfprojection[-1,]
dfprojection <-select(dfprojection, Date, p25, p30, p35)
fname <- paste0("projection-", todayDate, "csv")
write_csv(dfprojection, fname)
