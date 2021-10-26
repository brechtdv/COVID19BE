### COVID19BE // VACCINATION
### 24/10/2021

## required packages
library(ggplot2)
library(ggrepel)

## import data
last_day <- Sys.Date()
if (format(last_day, "%u") == 7) last_day <- last_day - 1
if (format(last_day, "%u") == 1) last_day <- last_day - 2

dt1 <- format(last_day - 14, "%Y%m%d")
dt2 <- format(last_day - 7, "%Y%m%d")
dt3 <- format(last_day, "%Y%m%d")

dta_inc1 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt1))
dta_inc2 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt2))
dta_inc3 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt3))

dta_vac <-
read.csv(
  encoding = "UTF-8",
  "https://epistat.sciensano.be/data/COVID19BE_VACC_MUNI_CUM.csv")
dta_vac_agg <-
aggregate(
  as.numeric(CUMUL) ~ NIS5,
  subset(dta_vac,
    YEAR_WEEK == max(dta_vac$YEAR_WEEK) & DOSE %in% c("B", "C")),
  sum, na.rm = TRUE)
names(dta_vac_agg) <- c("NIS5", "VAC")

## harmonize province names dta1 if needed
if (!("PROVINCE" %in% names(dta_inc1)))
  dta_inc1 <- merge(dta_inc1, dta_inc2[, c("NIS5", "PROVINCE")])

## compile data
dta <-
data.frame(
  CD_REFNIS = dta_inc1$NIS5,
  MUNI = dta_inc1$TX_DESCR_NL,
  PROV = dta_inc1$PROVINCE,
  N14 = as.numeric(dta_inc3$CASES) - as.numeric(dta_inc1$CASES),
  VAC = c(dta_vac_agg$VAC, NA))

## calculate incidence per 100k
load("POPNIS5.Rdata")  # objects 'POP' and 'POPNIS5'
dta <- merge(dta, POPNIS5)
dta$INC14 <- 1e5 * dta$N14 / dta$MS_POPULATION
dta$NON_VAC_RT <- 1 - dta$VAC / dta$MS_POPULATION

## identify top municipalities for labelling
id1 <- tail(dta[order(dta$INC14), "MUNI"], 5)
id2 <- tail(dta[order(dta$NON_VAC_RT), "MUNI"], 5)
(id <- unique(c(id1, id2)))

## spearman correlation coefficient
cor <- with(dta, cor.test(INC14, NON_VAC_RT, method = "spearman"))$estimate

##
## PLOT
##

png("covid19be-vaccination.png", 8, 6, units = "in", res = 300)
ggplot(dta, aes(x = INC14, y = NON_VAC_RT, size = N14, color = PROV)) +
  geom_smooth(
    aes(x = INC14, y = NON_VAC_RT),
    inherit.aes = FALSE,
    method = lm) +
  geom_point() +
  geom_text_repel(
    data = subset(dta, MUNI %in% id),
    aes(label = MUNI),
    show.legend = FALSE) +
  annotate(
    "text",
    x = c(-Inf, Inf, -Inf, Inf),
    y = c(-Inf, Inf, -Inf, Inf),
    label = c("\u2190 Low percentage non-vaccinated", "High percentage non-vaccinated \u2192",
              "\u2190 Low incidence", "High incidence \u2192"),
    angle = c(90, 90, 0, 0),
    hjust = c(-0.1, 1.1, -0.1, 1.1),
    vjust = c(1.5, -0.5, -0.5, 1.5),
    size = 3.5,
    color = "grey") +
  theme_bw() +
  theme(plot.caption.position =  "plot") +
  scale_x_continuous(
    expand = expansion(mult = 0.075)) +
  scale_y_continuous(
    expand = expansion(mult = 0.075),
    labels = scales::percent) +
  labs(
    x = paste("Cumulative 14-day incidence per 100,000",
              sep = "\n"),
    y = "Percentage not fully vaccinated",
    size = "Total nr of cases",
    color = "Province",
    title =
      sprintf("COVID19BE incidence and vaccination coverage by municipality, %s to %s",
              format(last_day - 15, "%d/%m"),
              format(last_day - 1, "%d/%m")),
    subtitle =
      sprintf("Spearman correlation coefficient: %s",
              formatC(cor, format = "f", digits = 3)),
    caption =
      paste("Note: data are cases by date of reporting, not by date of diagnosis",
            "Source code: https://github.com/brechtdv/COVID19BE",
            sep = "\n"))
dev.off()
