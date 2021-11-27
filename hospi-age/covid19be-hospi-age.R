### COVID19BE // HOSPI AGE
### 27/11/2021

## required packages
library(ggplot2)

## import data
dta_age <-
read.csv(
  encoding = "UTF-8",
  "Sciensano_Hospitalizations 2_Tijdreeks.csv")
names(dta_age) <- toupper(names(dta_age))
str(dta_age)

dta_hosp <-
read.csv(
  encoding = "UTF-8",
  "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")
str(dta_hosp)

## guess dta_age week-year
dta_age$WEEK <-
  as.numeric(gsub(")", "", gsub(".*Week ", "", dta_age$JAAR.WEEK)))
dta_age$YEAR <-
  as.numeric(gsub(".* ", "", gsub(" \\(Week.*", "", dta_age$JAAR.WEEK)))

## factorize dta_age AgeGroup
dta_age$AGE <-
factor(dta_age$AGEGROUP,
       levels = c("00–05", "06–19", "20–39", "40–59", "60–79", "80–++"),
       labels = c("0-5", "6-19", "20-39", "40-59", "60-79", "80+"))

## clean dta_age
dta_age$JAAR.WEEK <- NULL
dta_age$AGEGROUP <- NULL
dta_age <- dta_age[, c("YEAR", "WEEK", "AGE", "HOSPITALISATIONS")]
str(dta_age)

## aggregate NEW_IN by week
dta_hosp$YEARWEEK <-
  format(as.Date(dta_hosp$DATE), "%G-%V")
dta_hosp_wk <-
  aggregate(NEW_IN ~ YEARWEEK, dta_hosp, sum)

## compile data
dta_age$YEARWEEK <- paste0(dta_age$YEAR, "-", dta_age$WEEK)
dta <- merge(dta_hosp_wk, dta_age)
dta$NEW_IN_AGE <- dta$NEW_IN * dta$HOSPITALISATIONS
head(dta)

## save data
write.csv(dta, file = "covid19be-hospi-age.csv", row.names = FALSE)

##
## PLOT
##
brks_by <- seq(length(unique(dta$YEARWEEK)), 1, -2)
brks <- unique(dta$YEARWEEK)[brks_by]

png("covid19be-hospi-age-bars.png", 12, 6, units = "in", res = 300)
ggplot(dta, aes(x = YEARWEEK, y = NEW_IN_AGE, fill = AGE)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_discrete(breaks = brks) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y = "Number of new hospital admissions",
    x = NULL,
    fill = "Age group",
    title =
      sprintf("COVID19BE hospital admissions by age, %s to %s",
              min(dta$YEARWEEK),
              max(dta$YEARWEEK)),
    caption =
      paste("Data: https://epistat.wiv-isp.be/covid/",
            "Source code: https://github.com/brechtdv/COVID19BE",
            sep = "\n"))
dev.off()

png("covid19be-hospi-age-lines.png", 12, 6, units = "in", res = 300)
ggplot(dta, aes(x = YEARWEEK, y = NEW_IN_AGE, color = AGE, group = AGE)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = brks) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y = "Number of new hospital admissions",
    x = NULL,
    fill = "Age group",
    title =
      sprintf("COVID19BE hospital admissions by age, %s to %s",
              min(dta$YEARWEEK),
              max(dta$YEARWEEK)),
    caption =
      paste("Data: https://epistat.wiv-isp.be/covid/",
            "Source code: https://github.com/brechtdv/COVID19BE",
            sep = "\n"))
dev.off()
