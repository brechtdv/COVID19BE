### COVID19BE // MUNICIPALITIES
### 06/10/2020

## required packages
library(ggplot2)
library(ggrepel)

## import data
dt1 <- format(Sys.Date() - 14, "%Y%m%d")
dt2 <- format(Sys.Date() - 7, "%Y%m%d")
dt3 <- format(Sys.Date(), "%Y%m%d")

dta1 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt1))
dta2 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt2))
dta3 <-
read.csv(
  encoding = "UTF-8",
  sprintf(
    "https://epistat.sciensano.be/Data/%1$s/COVID19BE_CASES_MUNI_CUM_%1$s.csv",
    dt3))

## harmonize province names dta1 if needed
if (!("PROVINCE" %in% names(dta1)))
  dta1 <- merge(dta1, dta2[, c("NIS5", "PROVINCE")])

## compile data
dta <-
data.frame(
  CD_REFNIS = dta1$NIS5,
  MUNI = dta1$TX_DESCR_NL,
  PROV = dta1$PROVINCE,
  N14 = as.numeric(dta3$CASES) - as.numeric(dta1$CASES),
  DELTA = (as.numeric(dta3$CASES) - as.numeric(dta2$CASES)) -
          (as.numeric(dta2$CASES) - as.numeric(dta1$CASES)))

## calculate incidence per 100k
load("POPNIS5.RData")  # objects 'POP' and 'POPNIS5'
dta <- merge(dta, POPNIS5)
dta$INC14 <- 1e5 * dta$N14 / dta$MS_POPULATION

## identify top municipalities for labelling
id1 <- tail(dta[order(dta$INC14), "MUNI"], 10)
id2 <- tail(dta[order(dta$DELTA), "MUNI"], 10)
(id <- unique(c(id1, id2)))

##
## PLOT
##

png("covid19be-municipalities.png", 12, 6, units = "in", res = 300)
ggplot(dta, aes(x = INC14, y = DELTA, size = N14, color = PROV)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text_repel(
    data = subset(dta, MUNI %in% id),
    aes(label = MUNI),
    show.legend = FALSE) +
  annotate(
    "text",
    x = c(-Inf, Inf, -Inf, Inf),
    y = c(-Inf, Inf, -Inf, Inf),
    label = c("\u2190 Decrease in cases", "Increase in cases \u2192",
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
    expand = expansion(mult = 0.075)) +
  labs(
    x = paste("Cumulative 14-day incidence per 100,000",
              sep = "\n"),
    y = paste("Absolute change past 14 days",
              "(cum. inc. week 2 - cum. inc. week 1)",
              sep = "\n"),
    size = "Total nr of cases",
    color = "Province",
    title =
      sprintf("COVID19BE incidence and trends by municipality, %s to %s",
              format(Sys.Date()-15, "%d/%m"),
              format(Sys.Date()-1, "%d/%m")),
    caption =
      paste("Note: data are cases by date of reporting, not by date of diagnosis",
            "Source code: https://github.com/brechtdv/COVID19BE",
            sep = "\n"))
dev.off()
