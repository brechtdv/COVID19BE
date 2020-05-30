### COVID19BE / Key indicator trends
### brecht.devleesschauwer@sciensano.be

## required packages
library(ggplot2)
library(zoo)

## import data
dta_hosp <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")
dta_mort <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv")
dta_case <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

## aggregate data by date
dta_hosp_new_agg <- aggregate(NEW_IN ~ DATE, dta_hosp, sum)
dta_hosp_icu_agg <- aggregate(TOTAL_IN_ICU ~ DATE, dta_hosp, sum)
dta_mort_agg <- aggregate(DEATHS ~ DATE, dta_mort, sum)
dta_case_agg <- aggregate(CASES ~ DATE, dta_case, sum)

## subset to match dates
dta_mort_agg <- subset(dta_mort_agg, as.Date(DATE) >= "2020-03-15")
dta_case_agg <- subset(dta_case_agg, as.Date(DATE) >= "2020-03-15")

## expand cases data.frame if needed
dta_case_agg <- merge(dta_case_agg, dta_mort_agg["DATE"], all = TRUE)
dta_case_agg$CASES[is.na(dta_case_agg$CASES)] <- 0

## calculate moving averages
hosp_new_walk <- rollmean(dta_hosp_new_agg$NEW_IN, 7)
hosp_icu_walk <- rollmean(dta_hosp_icu_agg$TOTAL_IN_ICU, 7)
mort_walk <- rollmean(dta_mort_agg$DEATHS, 7)
case_walk <- rollmean(dta_case_agg$CASES, 7)

## calculate growth rates
hosp_new_gr <- (tail(hosp_new_walk, -1) / head(hosp_new_walk, -1)) - 1
hosp_icu_gr <- (tail(hosp_icu_walk, -1) / head(hosp_icu_walk, -1)) - 1
mort_gr <- (tail(mort_walk, -1) / head(mort_walk, -1)) - 1
case_gr <- (tail(case_walk, -1) / head(case_walk, -1)) - 1

## cases, deaths: set last 3 days to NA
case_gr[(length(case_gr)-2):length(case_gr)] <- NA
mort_gr[(length(mort_gr)-2):length(mort_gr)] <- NA

## compile data frame for plotting
df_trends <-
data.frame(
  DATE = rep(tail(dta_hosp_new_agg$DATE, -7), 4),
  INDICATOR = rep(c("HOSP_NEW", "HOSP_ICU", "MORT", "CASES"),
                    each = length(hosp_new_gr)),
  GR = c(hosp_new_gr, hosp_icu_gr, mort_gr, case_gr))
df_trends$DATE <-
  as.Date(df_trends$DATE)

## create plot
brks_by <-
  floor(as.numeric(as.Date("2020-03-22") - max(df_trends$DATE)) / 14)
brks <-
rev(
  seq(from = max(df_trends$DATE),
      to = as.Date("2020-03-22"),
      by = brks_by))

png("COVID19BE-key-indicator-trends-20200530.png",
    width = 8, height = 2, units = "in", res = 300)
ggplot(df_trends, aes(x = DATE, y = INDICATOR)) +
  geom_raster(aes(fill = GR)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(
    NULL,
    limits = c("MORT", "HOSP_ICU", "HOSP_NEW", "CASES"),
    labels = c("Deaths", "Occupied ICU beds", "Hospital admissions", "Cases"),
    expand = expansion(0)) +
  scale_x_date(
    NULL,
    breaks = brks,
    date_labels = "%d/%m",
    expand = expansion(0)) +
  scale_fill_gradient2(
    "Growth rate",
    low = "#120F71",
    mid = "white",
    high = "#F15A22",
    midpoint = 0,
    na.value = "grey",
    labels = scales::percent) +
  ggtitle(
    label = "Evolution of key COVID-19 indicators in Belgium",
    subtitle = "@brechtdv @sciensano | https://epistat.wiv-isp.be/covid/")
dev.off()
