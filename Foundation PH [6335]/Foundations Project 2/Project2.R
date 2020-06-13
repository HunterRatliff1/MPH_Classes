require(tidyverse)
require(ggthemes)
require(scales)
require(lubridate)



HP <- readxl::read_excel("~/Google Drive/'UTMB/- MPH/[6335] Foundations of Public Health/Project 2 [Foundations PH, 6335]/C-16-data.xlsx")

HP %>%
  mutate(PopTitle = str_replace(PopTitle, "Hispanic or Latino", "Latino")) %>%
  mutate(PopTitle = str_replace(PopTitle, " or African American", "")) %>%
  mutate(PopTitle = str_replace(PopTitle, "American Indian or Alaska Native", "AmIAK")) %>%
  filter(Year==2015) %>%
  filter(Grouping=="Percent_PovertyLine") %>%
  filter(Grouping %in% c("Age", "Race",
                         "Race", "Percent_PovertyLine", "Disability", "SexOrientation",
                         "Insurance")) %>%
  ggplot(aes(x=PopTitle, y=Estimate, ymin=LowerCI, ymax=UpperCI)) +
  geom_col() +
  geom_errorbar() +
  coord_flip() +
  facet_wrap("Grouping", scales = "free_y")

count(HP, Grouping)




