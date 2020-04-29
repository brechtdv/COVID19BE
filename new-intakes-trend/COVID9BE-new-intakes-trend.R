### COVID19BE / Evolution of COVID-19 hospital intakes
### brecht.devleesschauwer@sciensano.be

## required packages
library(ggplot2)
library(ggrepel)
library(zoo)

## import data
dta <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")

## aggregate new intakes
dta_agg <- aggregate(NEW_IN ~ DATE, dta, sum)

## calculate moving averages
new_in_walk <- rollmean(dta_agg$NEW_IN, 7)
growth_rate <- (tail(dta_agg$NEW_IN, -1) / head(dta_agg$NEW_IN, -1)) - 1
growth_rate_walk <- rollmean(growth_rate, 7)

## compile data frame for plotting
df_walk <-
data.frame(
  DATE = tail(dta_agg$DATE, -7),
  NEW_IN = tail(new_in_walk, -1),
  GROWTH = growth_rate_walk)
df_walk_lab <-
  df_walk[c(1, nrow(df_walk)), ]

## create plot
ggplot(df_walk, aes(x = NEW_IN, y = GROWTH)) +
  geom_point(size = 2) +
  geom_path(size = 1) +
  geom_label_repel(
    data = df_walk_lab,
    aes(x = NEW_IN, y = GROWTH, label = DATE),
    hjust = 1.5) +
  theme_bw() +
  scale_y_continuous(
    "Growth rate (7-days moving average)",
    labels = function(...) scales::percent(..., accuracy = 1)) +
  scale_x_continuous(
    "New intakes (7-days moving average)",
    limits = c(0, NA),
    breaks = seq(0, 600, 100)) +
  ggtitle(
    label = "Evolution of COVID-19 hospital intakes in Belgium",
    subtitle = "https://epistat.wiv-isp.be/covid/")
