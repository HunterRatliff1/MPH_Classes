require(tidyverse)
require(googlesheets)

# Age-adjusted death rate per 100,000 population
df <- googlesheets::gs_title("table017") %>%
  gs_read_csv() %>%
  gather("year", "deaths", `1950`:`2016`) %>%
  filter(Disease=="HIV") %>%
  filter(Category!="White, non-Hispanic", Category!="Black, non-Hispanic") %>%
  mutate(year = as.numeric(year)) %>%
  filter(year>1986)

" BELOW IS DISCUSSION POST
Table 17: Age-adjusted death rate due to HIV (per 100,000)
By time: The data for HIV begins in 1987 at overall national rate of 5.6 deaths per 100,000 population. Over the course of the next 8 years the death rate steadily increased, eventually peaking in 1995 after nearly quadrupling to a death rate of 16. The introduction of a three drug combination antiretroviral therapy (ART) in 1996 coincided with a rapid decrease in the death rate, with the death rate returning to 5 deaths per 100k in just three years (1998). This rate has continued to trend downward at a slower rate in the time since, with the most recent data showing a rate of 1.8 deaths per 100k in 2016.

By subgroups: Compared to the overall average, Black/African Americans, Males, and Hispanics/Latinos have higher death rates from HIV. This is reflective of the disparities of HIV infection in these groups, as HIV has a higher prevalence in these populations. Unfortunately the denominator in Table 17 is based on the census data (i.e. total population), not the number of people at risk (i.e. people with HIV).

Figure1
"


### ---  Overall trend  ---
df %>%
  filter(Category=="All persons") %>%
  mutate(Label = as.character(year)) %>%
  mutate(Label = case_when(
    year == 1987 ~ str_c(Label, ",\nRate: ", deaths),
    year == 1995 ~ str_c(Label, ",\nRate: ", deaths),
    year == 1998 ~ str_c(Label, ",\nRate: ", deaths),
    # year == 2016 ~ str_c(Label, ",\n", deaths),
    TRUE         ~ ""
  )) %>%
  mutate(Label = ifelse(Label=="", NA, Label)) %>%
  ggplot(aes(x=year, y=deaths, group=Category)) +
  ggrepel::geom_label_repel(aes(label=Label), 
                            arrow = arrow(length = unit(0.02, "npc")),
                            nudge_y = -2, nudge_x=4, color="red") +
  geom_point() + geom_line() + 
  theme_bw() +
  labs(y="Death Rate (per 100k)",
       caption = "Age-adjusted")

# SUBGROUP
# Compared to the overall average, Black/African Americans, Males, and 
# Hispanics/Latinos have higher death rates from HIV. This is reflective of
# the disparities of HIV infection in these groups, as HIV has a higher prevelance
# in these populations. Unfortunately the denominator in Table 17 is based on the 
# census data (i.e. total population), not the number of people at risk (i.e.
# people with HIV). 

# Over time, there doesn't appear to be a clear interaction 

# Over time, the standardized mortality ratio 


# https://minorityhealth.hhs.gov/omh/browse.aspx?lvl=4&lvlid=21

### ---  Subgroup stratifications  ---
df %>%
  left_join(
    select(filter(df, Category=="All persons"), year, Overall=deaths)
  ) %>%
  filter(CategoryType!="All") %>%
  ggplot(aes(x=year, y=deaths, group=Category, color=Category)) +
  geom_point(aes(y=Overall, shape="Overall"), color="black", alpha=0.5) +
  geom_line(aes(y=Overall), color="black", alpha=0.5) +
  geom_line() + 
  facet_wrap("CategoryType") +
  scale_shape_manual(values = c("Overall"=4), name="Overall") +
  theme_bw() +
  labs(title="HIV death rate, by sex/race",
       subtitle = "Age-adjusted, per 100,000", 
    y="Death Rate (per 100k)",
       caption = "Source: NCHS/HUS, Table 17 (2017)")


### ---  Standardized mortality ratio (SMR)  ---
df %>%
  left_join(
    select(filter(df, Category=="All persons"), year, Overall=deaths)
  ) %>%
  mutate(SMR = deaths/Overall) %>%
  mutate(se = sqrt(deaths)/Overall) %>%
  mutate(uCI = SMR+1.96*se) %>%
  mutate(lCI = SMR-1.96*se) %>%
  filter(CategoryType!="All") %>%
  ggplot(aes(x=year, y=SMR, group=Category, color=Category)) +
  geom_line() + 
  facet_wrap("CategoryType") +
  theme_bw() +
  labs(y="Standardized mortality ratio (SMR)", 
       title="Standardized mortality ratio of HIV deaths",
       subtitle="SMR: compared to overall mortality ratio",
       caption = "Source: NCHS/HUS, Table 17 (2017)")


# df %>%
#   left_join(
#     select(filter(df, Category=="All persons"), year, Overall=deaths)
#   ) %>%
#   mutate(SMR = deaths/Overall) %>%
#   mutate(se = sqrt(deaths)/Overall) %>%
#   mutate(uCI = SMR+1.96*se) %>%
#   mutate(lCI = SMR-1.96*se) %>%
#   ggplot(aes(x=year, y=SMR, group=Category, color=Category)) +
#   geom_line() + 
#   geom_line(aes(y=lCI), linetype=2) +
#   geom_line(aes(y=uCI), linetype=2) +
#   facet_wrap("Category") +
#   theme_bw() +
#   labs(y="SMR",
#        caption = "Compared to national mortality ratio")


# # # # # # # # # # # 
#     CLASS DATA    #
# # # # # # # # # # # 
require(plotly)
table17 <- googlesheets::gs_title("table017") %>%
  gs_read_csv() %>%
  gather("year", "deaths", `1950`:`2016`) %>%
  filter(Category!="White, non-Hispanic", Category!="Black, non-Hispanic") %>%
  mutate(year = as.numeric(year)) 
  
table17 <- table17 %>%
  filter(CategoryType=="All") %>%
  select(Disease, year, Overall=deaths) %>%
  left_join(table17)




# Disease_name <- "Drug overdose"
count(table17, Disease)

Disease_name <- "Malignant neoplasms"


p <- table17 %>%
  filter(!is.na(deaths)) %>%
  filter(Disease == Disease_name) %>%
  filter(CategoryType!="All") %>%
  ggplot(aes(x=year, y=deaths, group=Category, color=Category)) +
  geom_line(aes(y=Overall), color="black") +
  geom_line() + 
  facet_wrap("CategoryType") +
  theme_bw() + labs(title=Disease_name) 

ggplotly(p)
p

