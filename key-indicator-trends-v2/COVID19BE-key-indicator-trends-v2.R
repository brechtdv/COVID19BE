### COVID19BE / Key indicator trends / Version 2
### brecht.devleesschauwer@sciensano.be

## required packages
library(cowplot)
library(ggplot2)
library(zoo)

## helper functions

## .. calculate weekly trends
trend <-
function(x) {
  y <- rollsum(x, 7)
  1 - (1 + (head(y, -7) - tail(y, -7))/tail(y, -7))^(1/7)
}

## compile trend plot
trend_plot <-
function(counts, trends, title, ylab, drop = 0) {
  counts$WALK <- c(rep(NA, 6), rollmean(counts$N, 7))
  counts$WALK[tail(seq(nrow(counts)), drop)] <- NA

  brks <-
    rev(
      seq(from = max(counts$DATE),
          to = min(counts$DATE),
          by = -7))
  lims <- range(counts$DATE) + c(-1, 1)

  p.trend <-
  ggplot(counts) +
    geom_col(
      aes(x = DATE, y = N),
      width = 0.6,
      fill = c(rep("#B2D235", nrow(counts) - drop),
               rep("grey50", drop))) +
    geom_line(
      aes(x = DATE, y = WALK),
      color = "#39B54A",
      size = 2) + 
    labs(
      y = ylab) + 
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(b = 0)) +
    scale_x_date(
      NULL,
      date_labels = "%d/%m",
      breaks = brks,
      limits = lims) +
    scale_y_continuous(
      expand = expansion(c(0.01, 0.05))) +
    ggtitle(title)

  p.growth <-
  ggplot(trends) +
    geom_col(
      aes(x = DATE, y = GR, fill = GR > 0)) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(t = 0, b = 20)) +
    labs(
      y = "Growth ratio") +
    scale_y_continuous(
      labels = function(...) scales::percent(accuracy = 1, ...)) +
    scale_x_date(
      NULL,
      position = "top",
      breaks = brks,
      limits = lims) +
    scale_fill_manual(
      values = c("#524FA1", "#F15A22"))

  plot_grid(
    p.trend,
    p.growth,
    align = "v",
    axis = "n",
    ncol = 1,
    rel_heights = c(5, 2))
}

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

## calculate trends
hosp_new_trend <- trend(dta_hosp_new_agg$NEW_IN)
hosp_icu_trend <- trend(dta_hosp_icu_agg$TOTAL_IN_ICU)
mort_trend <- trend(dta_mort_agg$DEATHS)
case_trend <- trend(dta_case_agg$CASES)

## cases, deaths: set last 3 days to NA
case_trend[(length(case_trend)-2):length(case_trend)] <- NA
mort_trend[(length(mort_trend)-2):length(mort_trend)] <- NA

## compile data frames for plotting
df_counts <-
data.frame(
  DATE = rep(dta_hosp_new_agg$DATE, 4),
  INDICATOR = rep(c("HOSP_NEW", "HOSP_ICU", "MORT", "CASES"),
                    each = nrow(dta_hosp_new_agg)),
  N = c(dta_hosp_new_agg$NEW_IN,
        dta_hosp_icu_agg$TOTAL_IN_ICU,
        dta_mort_agg$DEATHS,
        dta_case_agg$CASES))
df_counts$DATE <-
  as.Date(df_counts$DATE)

df_trends <-
data.frame(
  DATE = rep(tail(dta_hosp_new_agg$DATE, -13), 4),
  INDICATOR = rep(c("HOSP_NEW", "HOSP_ICU", "MORT", "CASES"),
                    each = length(hosp_new_trend)),
  GR = c(hosp_new_trend, hosp_icu_trend, mort_trend, case_trend))
df_trends$DATE <-
  as.Date(df_trends$DATE)

##
##
##

## create traffic light plot
brks_by <-
  floor(as.numeric(as.Date("2020-03-22") - max(df_trends$DATE)) / 14)
brks <-
rev(
  seq(from = max(df_trends$DATE),
      to = as.Date("2020-03-22"),
      by = brks_by))

png("COVID19BE-key-indicator-trends-v2-20200526.png",
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
    label = "Evolution of key COVID-19 indicators in Belgium (v2)",
    subtitle = "@brechtdv @sciensano | https://epistat.wiv-isp.be/covid/")
dev.off()

##
##
##

p1 <-
trend_plot(
  subset(df_counts, INDICATOR == "CASES"),
  subset(df_trends, INDICATOR == "CASES"),
  "Reported cases",
  "Number of cases",
  drop = 3)

p2 <-
trend_plot(
  subset(df_counts, INDICATOR == "HOSP_NEW"),
  subset(df_trends, INDICATOR == "HOSP_NEW"),
  "New hospital intakes",
  "Number of patients")

p3 <-
trend_plot(
  subset(df_counts, INDICATOR == "HOSP_ICU"),
  subset(df_trends, INDICATOR == "HOSP_ICU"),
  "Occupied ICU beds",
  "Number of patients")

p4 <-
trend_plot(
  subset(df_counts, INDICATOR == "MORT"),
  subset(df_trends, INDICATOR == "MORT"),
  "Reported deaths",
  "Number of deaths",
  drop = 3)

png("COVID19BE-key-indicator-trends-hist-v2-20200526.png",
    width = 10, height = 10, units = "in", res = 300)
plot_grid(p1, p2, p3, p4, ncol = 2)
dev.off()
