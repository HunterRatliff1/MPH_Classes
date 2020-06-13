require(tidyverse)
require(ggthemes)
require(scales)
require(lubridate)

# Read in data
clinical <- readxl::read_excel("~/SASUniversityEdition/myfolders/MPH/Homework/Project/clinical.xlsx") %>%
  mutate(DEATH_DT = as.Date(DEATH_DT))
demographic <- readxl::read_excel("~/SASUniversityEdition/myfolders/MPH/Homework/Project/demographic.xlsx") %>%
  mutate(BENE_DOB = as.Date(BENE_DOB))
names(clinical) <- str_to_lower(names(clinical))
names(demographic) <- str_to_lower(names(demographic))

# Make combine df
combine <- inner_join(clinical, demographic) %>%
  mutate(age = as.numeric(as.Date("2001-01-01")-bene_dob)/365) %>%
  mutate(
    age_grp = ifelse(age<75, 1, 2)
  ) %>%
  mutate(vital_2007 = ifelse(death_dt >= as.Date("2007-01-01") | is.na(death_dt), 0, 1)) %>%
  filter(year(bene_dob)>1919) %>% # excluding old people
  filter(state_cd=="45", race != "0") 

# Pull subcohort from export (last step of project)
subcohort <- read_csv("~/SASUniversityEdition/myfolders/MPH/Homework/Project/subcohort.csv")

# ----------- AGE -------------
combine %>% 
  filter(year(bene_dob)>1919) %>%
  ggplot(aes(x=age)) + geom_histogram()

MASS::fitdistr(combine$age, "lognormal")


# ----------- Comorbid -------------

lambda <- MASS::fitdistr(combine$pchrlson, "Poisson")$estimate[["lambda"]] 

df <- tibble(
  x        = c(1:nrow(combine)),
  expected = rpois(nrow(combine), lambda),
  actual   = combine$pchrlson
) 

df %>%
  gather("group", "value", -x) %>%
  ggplot(aes(x=value, fill=group)) + 
  geom_bar() + 
  facet_wrap("group")

x <- df %>% 
  gather("group", "value", -x) %>%
  select(-x) %>%
  group_by(group, value) %>%
  count() %>%
  spread(group, n) %>%
  replace_na(replace = list(expected=0))


chisq.test(x$actual, x$expected)

combine %>% 
  ungroup() %>%
  mutate(vital_2007 = factor(vital_2007)) %>%
  mutate(age_grp = factor(age_grp)) %>%
  ggplot(aes(x=age_grp, fill=vital_2007)) +
  geom_bar()
  geom_histogram()
  
  
combine %>%
  ggplot(aes(x=age, fill=factor(smam_2yr))) +
  geom_histogram()

combine %>%
  ggplot(aes(x=factor(age_grp), fill=factor(smam_2yr))) +
  geom_bar(position = "fill")
  


