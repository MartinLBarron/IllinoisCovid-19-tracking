#' Simple plot of ILinois Covid-19 cases over time
#' 

library(tidyverse)
library(here)
library(scales)


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


ggplot(data=df) +
  geom_line(aes(x=Date, y=Illinois), size=2, col="#155F83") +
  geom_point(data=dfMostRecent, aes(x=Date, y=Illinois), size=4, col="#8F3931")+
  geom_text(data=dfMostRecent, aes(x=Date-1, y=Illinois, label=lab), hjust="right")+
  theme_minimal()+
  ggtitle("Illinois Coronavirus Cases") +
  xlab("") + 
  ylab("Count of reported cases")+
  scale_x_date(breaks= as.Date(c("2020-01-24",
                                 "2020-03-01",
                                 todayDate)),
               date_labels = "%b-%d") +
  labs(caption = "data from https://bit.ly/2IDLMlc")+
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(hjust = .5),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

fname <- paste0("chart-", todayDate, ".png")
ggsave(here("output", fname), w=8, h=6)
