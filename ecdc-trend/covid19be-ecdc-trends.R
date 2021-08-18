### COVID19BE // ECDC TRENDS
### 13/08/2021

## required packages
library(ggplot2)

## import data
dta1 <-
read.csv(
  encoding = "UTF-8",
  "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")
dta2 <-
read.csv(
  encoding = "UTF-8",
  "https://epistat.sciensano.be/Data/COVID19BE_TESTS.csv")

## aggregate data
dta1_agg <- aggregate(CASES ~ DATE + REGION, dta1, sum)
dta1_agg$DATE <- as.Date(dta1_agg$DATE)
dta1_agg <-
merge(
  dta1_agg,
  expand.grid(DATE = unique(dta1_agg$DATE),
              REGION = unique(dta1_agg$REGION)),
  all = TRUE)
dta1_agg$CASES[is.na(dta1_agg$CASES)] <- 0
str(dta1_agg)

dta2_agg <- aggregate(TESTS_ALL ~ DATE + REGION, dta2, sum)
dta2_agg$DATE <- as.Date(dta2_agg$DATE)
dta2_agg <-
merge(
  dta2_agg,
  expand.grid(DATE = unique(dta2_agg$DATE),
              REGION = unique(dta2_agg$REGION)),
  all = TRUE)
dta2_agg$TESTS_ALL[is.na(dta2_agg$TESTS_ALL)] <- 0
str(dta2_agg)

dta3_agg <- aggregate(TESTS_ALL_POS ~ DATE + REGION, dta2, sum)
dta3_agg$DATE <- as.Date(dta3_agg$DATE)
dta3_agg <-
merge(
  dta3_agg,
  expand.grid(DATE = unique(dta3_agg$DATE),
              REGION = unique(dta3_agg$REGION)),
  all = TRUE)
dta3_agg$TESTS_ALL_POS[is.na(dta3_agg$TESTS_ALL_POS)] <- 0
str(dta3_agg)

## compile dataframe
dt <- tail(dta2_agg$DATE, 1)  # guess the current date

INC14 <-
with(subset(dta1_agg, DATE %in% seq(dt - 3 - 13, dt - 3, 1)),
     tapply(CASES, REGION, sum))
INC7 <-
with(subset(dta1_agg, DATE %in% seq(dt - 3 - 6, dt - 3, 1)),
     tapply(CASES, REGION, sum))
TST7 <-
with(subset(dta2_agg, DATE %in% seq(dt - 3 - 6, dt - 3, 1)),
     tapply(TESTS_ALL, REGION, sum))
TST_POS7 <-
with(subset(dta3_agg, DATE %in% seq(dt - 3 - 6, dt - 3, 1)),
     tapply(TESTS_ALL_POS, REGION, sum))

df <-
cbind(data.frame(INC14), data.frame(INC7),
      data.frame(TST7), data.frame(TST_POS7))
df$REGION <- names(INC14)
df$POP <- c(1219970, 6653062, 3648206)
df$INC14_RT <- 1e5 * df$INC14 / df$POP
df$PR7 <- df$TST_POS7 / df$TST7
str(df)

## plot
col <- c("#66b32f", "#f2a82f", "#b73d18", "#7c170f")
xmax <- max(0.06, max(df$PR) + 0.005)
ymax <- max(500, max(df$INC14_RT) + 10)

png("COVID9BE-ecdc-trend.png", 8, 6, units = "in", res = 300) 
ggplot(df, aes(x = PR7, y = INC14_RT, group = REGION)) +
  annotate("rect",
    xmin = 0, xmax = 0.01, ymin = 0, ymax = 50,
    fill = col[1], color = "black") +
  annotate("rect",
    xmin = 0, xmax = 0.01, ymin = 50, ymax = 75,
    fill = col[1], color = "black") +
  annotate("rect",
    xmin = 0.01, xmax = 0.04, ymin = 0, ymax = 50,
    fill = col[1], color = "black") +
  annotate("rect",
    xmin = 0, xmax = 0.01, ymin = 75, ymax = 200,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0.01, xmax = 0.04, ymin = 50, ymax = 75,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0.01, xmax = 0.04, ymin = 75, ymax = 200,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0.04, xmax = xmax, ymin = 0, ymax = 50,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0.04, xmax = xmax, ymin = 50, ymax = 75,
    fill = col[2], color = "black") +
  annotate("rect",
    xmin = 0, xmax = 0.01, ymin = 200, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 0.01, xmax = 0.04, ymin = 200, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 0.04, xmax = xmax, ymin = 200, ymax = ymax,
    fill = col[3], color = "black") +
  annotate("rect",
    xmin = 0.04, xmax = xmax, ymin = 75, ymax = 200,
    fill = col[3], color = "black") +
  geom_point(aes(fill = REGION),
    shape = 23, color = "white", size = 5, stroke = 2) +
  theme_bw() +
  labs(x = "7-day test positivity ratio",
       y = "14-day cumulative incidence rate") +
  scale_fill_manual(
    "Region",
    values = c("blue", "yellow", "red")) +
  scale_x_continuous(
    labels = scales::percent) +
  ggtitle(
    "COVID19BE regional classification according to ECDC criteria",
    subtitle = paste("Situation on", dt))
dev.off()

## update README
rmarkdown::render("README.Rmd")