### COVID19BE // TEST TRENDS
### 29/09/2020

## required packages
library(ggplot2)
library(zoo)

## settings
sciensano1 <- "#39B54A"  # medium green

## import data
dta <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_TESTS.csv")
str(dta)

## clean data
dta$PROVINCE[dta$PROVINCE == "LiÃ¨ge"] <- "Liège"
dta$DATE <- as.Date(dta$DATE)

## 7-day rolling average by province
dta2 <- with(dta, tapply(TESTS_ALL, PROVINCE, rollsum, k = 7))
dta3 <- with(dta, tapply(TESTS_ALL_POS, PROVINCE, rollsum, k = 7))

## compile dataframe
n <- length(dta2[[1]])
df_prov <-
data.frame(
  TESTS_ALL = unlist(dta2),
  TESTS_ALL_POS = unlist(dta3),
  PROVINCE = rep(names(dta2), each = n),
  DATE = rep(tail(unique(dta$DATE), -6), length(dta2)))

## add rates per 100k
df_pop <-
data.frame(
  PROVINCE = c("Antwerpen", "BrabantWallon", "Brussels", "Hainaut",
               "Liège", "Limburg", "Luxembourg", "Namur",
               "OostVlaanderen", "VlaamsBrabant", "WestVlaanderen"),
  POP = c(1869730,  406019, 1218255, 1346840,
          1109800,  877370,  286752,  495832,
          1525255, 1155843, 1200945))
df_prov <- merge(df_prov, df_pop)
df_prov$RATE <- 1e3 * df_prov$TESTS_ALL / df_prov$POP

## calculate BE rates
df_be <-
data.frame(
  TESTS_ALL =
    rollsum(aggregate(TESTS_ALL ~ DATE, dta, sum)$TESTS_ALL, k = 7),
  TESTS_ALL_POS =
    rollsum(aggregate(TESTS_ALL_POS ~ DATE, dta, sum)$TESTS_ALL, k = 7),
  PROVINCE = "Belgium",
  DATE = tail(unique(dta$DATE), -6),
  POP = sum(df_pop$POP))
df_be$RATE <- 1e3 * df_be$TESTS_ALL / df_be$POP

## merge prov & BE
df_all <-
rbind(
  cbind(df_be, SET = 1),
  cbind(df_prov, SET = 1),
  cbind(PROVINCE = df_prov$PROVINCE, df_be[, -3], SET = 2))

## calculate positivity ratio
df_all$RATIO <- df_all$TESTS_ALL_POS / df_all$TESTS_ALL

## final steps
df_all$PROVINCE <- relevel(factor(df_all$PROVINCE), "Belgium")

##
## PLOT
##

## subset dates
dt <- seq(as.Date("2020-06-01"), max(df_all$DATE)-3, 1)

## create plot - test rates per 100k
png("covid19be-test-rates.png", 12, 6, units = "in", res = 300)
ggplot(subset(df_all, DATE %in% dt),
       aes(x = DATE, y = RATE, group = SET)) +
  geom_line(aes(color = factor(SET))) +
  facet_wrap(~PROVINCE) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(
    values = c(sciensano1, "grey")) +
  scale_x_date(
    NULL) +
  scale_y_continuous(
    "Number of performed tests per 1000\n(7-day rolling sum)") +
  ggtitle(sprintf("Evolution of performed COVID-19 tests, %s to %s",
                  format(min(dt), "%d/%m"),
                  format(max(dt), "%d/%m")))
dev.off()

## create plot - test positivity ratios
png("covid19be-test-ratios.png", 12, 6, units = "in", res = 300)
ggplot(subset(df_all, DATE %in% dt),
       aes(x = DATE, y = RATIO, group = SET)) +
  geom_line(aes(color = factor(SET))) +
  facet_wrap(~PROVINCE) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(
    values = c(sciensano1, "grey")) +
  scale_x_date(
    NULL) +
  scale_y_continuous(
    "Test positivity ratio\n(7-day rolling window)",
    labels = function(...) scales::percent(..., accuracy = 1)) +
  ggtitle(sprintf("Evolution of COVID-19 test positivity ratios, %s to %s",
                  format(min(dt), "%d/%m"),
                  format(max(dt), "%d/%m")))
dev.off()


