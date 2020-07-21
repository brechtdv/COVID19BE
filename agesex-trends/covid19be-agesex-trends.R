### COVID19BE // AGE SEX TRENDS GIF
### 21/07/2020

## required packages
library(gganimate)
library(ggplot2)
library(zoo)

## settings
sciensano1 <- "#39B54A"  # medium green
sciensano2 <- "#006838"  # dark green

## import data
dta <-
  read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")
str(dta)

## collapse by date, age and sex
dta2 <- aggregate(CASES ~ DATE + AGEGROUP + SEX, dta, sum)
dta2$DATE <- as.Date(dta2$DATE)

## make full version - add zeroes
d <-
expand.grid(
  stringsAsFactors = FALSE,
  DATE = unique(dta2$DATE),
  AGEGROUP = unique(dta2$AGEGROUP),
  SEX = unique(dta2$SEX))

dta3 <- merge(d, dta2, all = TRUE)
dta3[is.na(dta3)] <- 0

## calculate rolling incidences
a <- with(dta3, tapply(CASES, list(AGEGROUP, SEX), rollsum, 14))
n <- length(a[[1]])

## compile dataframe
df <-
data.frame(
  CASES = unlist(a),
  AGEGROUP = rep(rep(unique(dta3$AGEGROUP), each = n), 2),
  SEX = rep(unique(dta3$SEX), each = n*10),
  DATE_FROM = rep(head(unique(dta3$DATE), -13), 2*10),
  DATE_TO = rep(tail(unique(dta3$DATE), -13), 2*10))
df$DATE_RANGE <-
  paste(format(df$DATE_FROM, "%d/%m"),
        format(df$DATE_TO, "%d/%m"),
        sep = "-")
df$DATE_RANGE <-
  factor(df$DATE_RANGE, unique(df$DATE_RANGE))

## add incidence
POP <- 
structure(list(SEX = c("F", "M", "F", "M", "F", "M", "F", 
"M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M"
), AGEGROUP = c("0-9", "0-9", "10-19", "10-19", "20-29", "20-29", 
"30-39", "30-39", "40-49", "40-49", "50-59", "50-59", "60-69", 
"60-69", "70-79", "70-79", "80-89", "80-89", "90+", "90+"), POP = c(624521, 
653781, 626569, 657008, 699447, 710611, 741647, 741911, 746138, 
760756, 791752, 801745, 677294, 648434, 487617, 415187, 326756, 
207895, 81437, 30900)), row.names = c(NA, -20L), class = "data.frame")
df <- merge(df, POP)
df$INC <- 1e5 * df$CASES / df$POP

## final steps
df$SEX <-
  factor(df$SEX, levels = c("M", "F"), labels = c("Men", "Women"))
head(df)


##
## ABSOLUTE CASES
##

gif <-
ggplot(
  df,
  aes(x = as.numeric(as.factor(AGEGROUP)), y = CASES, group = SEX)) +
  geom_col(
    aes(fill = SEX),
    position = "dodge") +
  labs(
    title = "COVID-19 cases by age and sex, rolling 14 days window, {closest_state}",
    x = NULL,
    y = NULL) +
  scale_x_continuous(
    breaks = 1:10,
    labels = unique(df$AGEGROUP)) +
  scale_fill_manual(
    NULL,
    values = c(sciensano2, sciensano1)) +
  theme_bw() +
  transition_states(DATE_RANGE, transition_length = 2, state_length = 0) +
  view_follow(fixed_x = TRUE) +
  ease_aes("linear")
animate(
  gif,
  fps = 10, duration = 20, end_pause = 20, width = 800, height = 450)
anim_save(filename = "covid19be-agesex-trends-cases.gif")

##
## INCIDENCE PER 100,000
##

gif <-
ggplot(
  df,
  aes(x = as.numeric(as.factor(AGEGROUP)), y = INC, group = SEX)) +
  geom_col(
    aes(fill = SEX),
    position = "dodge") +
  labs(
    title = "COVID-19 cases by age and sex per 100 000, rolling 14 days window, {closest_state}",
    x = NULL,
    y = NULL) +
  scale_x_continuous(
    breaks = 1:10,
    labels = unique(df$AGEGROUP)) +
  scale_fill_manual(
    NULL,
    values = c(sciensano2, sciensano1)) +
  theme_bw() +
  transition_states(DATE_RANGE, transition_length = 2, state_length = 0) +
  view_follow(fixed_x = TRUE) +
  ease_aes("linear")
animate(
  gif,
  fps = 10, duration = 20, end_pause = 20, width = 800, height = 450)
anim_save(filename = "covid19be-agesex-trends-incidence.gif")
