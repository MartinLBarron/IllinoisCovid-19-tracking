#' Simple projection of ILinois Covid-19 cases over time


# Load necessary libraries ------------------------------------------------

library(tidyverse)
library(here)
library(scales)


# Load data and calc % change ---------------------------------------------

df <- read_csv("data/IL-covid-19-cases.csv") %>%
  mutate(Date=as.Date(Date, "%m/%d/%y")) %>%
  mutate(percentChange=(Illinois-lag(Illinois))/lag(Illinois),
         newcases=Illinois-lag(Illinois),
         growthFactor=newcases/lag(newcases))


dfMostRecent <- df %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  mutate(Date=as.Date(Date),
         lab=paste0(newcases, " new cases\n", percent(percentChange), " growth"))

todayDate <- format(dfMostRecent[[1, "Date"]],"%Y-%m-%d")

# Limit to data since March 1 ---------------------------------------------
df <- df %>%
  filter(Date>=as.Date("2020-03-01"))

meangrowth=mean(df$percentChange)

meangrowthlabel = paste("Avg.", percent(meangrowth))
ggplot(data=df) +
  geom_col(aes(x=Date, y=percentChange), fill="#155F83") +
  geom_hline(yintercept = meangrowth, linetype="dashed")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
  ggtitle("Daily % change in Illinois Coronavirus Cases") +
  annotate("text", x=as.Date("2020-03-06"), y=meangrowth+.03 ,label=meangrowthlabel)+
  xlab("") + 
  ylab("% Growth")+
  labs(caption = "data from https://bit.ly/2IDLMlc")+
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(hjust = .5),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

fname <- paste0("percent_change-", todayDate, ".png")

ggsave(here("output", fname), w=8, h=6)

ggplot(data=df) +
  geom_col(aes(x=Date, y=growthFactor), fill="#155F83") +
  theme_minimal()+
  scale_x_date(date_breaks = "2 day", date_labels = "%b %d")+
  ggtitle("Daily Growth Rate in Illinois Coronavirus Cases") +
  xlab("") + 
  ylab("% Growth")+
  labs(caption = "data from https://bit.ly/2IDLMlc")+
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(hjust = .5),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

fname <- paste0("growth_rate-", todayDate, ".png")

ggsave(here("output", fname), w=8, h=6)

