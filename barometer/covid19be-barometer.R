### COVID19BE // BAROMETER
### 06/02/2022

## required packages
library(ggplot2)
library(zoo)

## import data
dta <-
read.csv(
  encoding = "UTF-8",
  "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")

## aggregate data
dta1_agg <- aggregate(NEW_IN ~ DATE, dta, sum)
dta1_agg$DATE <- as.Date(dta1_agg$DATE)
dta1_agg <-
merge(
  dta1_agg,
  expand.grid(DATE = unique(dta1_agg$DATE)),
  all = TRUE)
dta1_agg$NEW_IN[is.na(dta1_agg$NEW_IN)] <- 0
str(dta1_agg)

dta2_agg <- aggregate(TOTAL_IN_ICU ~ DATE, dta, sum)
dta2_agg$DATE <- as.Date(dta2_agg$DATE)
dta2_agg <-
merge(
  dta2_agg,
  expand.grid(DATE = unique(dta2_agg$DATE)),
  all = TRUE)
dta2_agg$TOTAL_IN_ICU[is.na(dta2_agg$TOTAL_IN_ICU)] <- 0
str(dta2_agg)

## compile dataframe
dt <- tail(dta2_agg$DATE, 1)  # guess the current date

## calculate trends
dta1_agg$NEW_IN_7 <-
  rollmean(dta1_agg$NEW_IN, 7, align = "right", na.pad = TRUE)
dta1_agg$NEW_IN_7_TREND <-
  c(rep(NA, 7),
    tail(dta1_agg$NEW_IN_7, -7) / head(dta1_agg$NEW_IN_7, -7) - 1)

dta1_agg$TOTAL_IN_ICU_7_TREND <-
  c(rep(NA, 7),
    tail(dta2_agg$TOTAL_IN_ICU, -7) / head(dta2_agg$TOTAL_IN_ICU, -7) - 1)

## compile data
df <-
data.frame(
  NEW_IN_7 = tail(dta1_agg$NEW_IN_7, 1),
  TOTAL_IN_ICU = tail(dta2_agg$TOTAL_IN_ICU, 1))

n_trend <- 14
df_trend <-
data.frame(
  DATE = tail(dta1_agg$DATE, n_trend),
  NEW_IN_7 = tail(dta1_agg$NEW_IN_7, n_trend),
  TOTAL_IN_ICU = tail(dta2_agg$TOTAL_IN_ICU, n_trend))

## plot
col <- c("#ffd515", "#f28c00", "#d31026")
xmax <- max(550, 1.1 * max(df$TOTAL_IN_ICU))
ymax <- max(175, 1.1 * max(df$NEW_IN_7))

#png("COVID9BE-barometer.png", 8, 6, units = "in", res = 300) 
ggplot(df, aes(x = TOTAL_IN_ICU, y = NEW_IN_7)) +
  annotate("rect",
    xmin = 0, xmax = 300, ymin = 0, ymax = 65,
    fill = col[1], color = "black") +
  annotate("rect",
    xmin = 300, xmax = 500, ymin = 0, ymax = 65,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 300, xmax = 500, ymin = 65, ymax = 150,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0, xmax = 300, ymin = 65, ymax = 150,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0, xmax = 300, ymin = 150, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 300, xmax = 500, ymin = 150, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 500, xmax = xmax, ymin = 150, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 500, xmax = xmax, ymin = 65, ymax = 150,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 500, xmax = xmax, ymin = 0, ymax = 65,
    fill = col[3], color = "black") +
  geom_path(
    data = df_trend,
    aes(alpha = DATE),
    size = 2) +
  geom_point(shape = 23, color = "white", size = 5, stroke = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Occupied ICU beds due to and with COVID-19",
       y = "New COVID-19 admissions, 7-day average") +
  ggtitle(
    "COVID19BE barometer",
    subtitle = paste("Situation on", dt, "and 14-day trend"))
dev.off()
