# what are the career interests of my students?
# rené dario herrera
# Estrella Mountain Community College
# rene dot herrera at estrellamountain dot edu
# 27 Aug 2021

# set up
# packages
library(here)
library(tidyverse)
library(AnthroTools)
library(knitr)
library(gt)
library(ggthemes)

# read data
responses <- read_csv("data/raw/survey_responses_career_interests.csv",
  col_types = cols(
    group = col_factor(levels = c("asb214", "asb222", "asb223", "asb252")),
    unsure = col_factor(levels = c("yes", "no"))
  ),
  na = ""
)

# inspect data
glimpse(responses)
unique(responses$group)
n_responses <- as.character(count(responses))

# how many unique class and date groups are there?
responses <- responses %>%
  mutate(group = as.character(group)) %>%
  mutate(date_group = str_c(date_start, group))

n_classes <- as.character(count(count(responses, date_group)))

# tidy data for use with anthrotools
coded_responses <- responses %>%
  rowid_to_column("Subj") %>%
  pivot_longer(
    cols = starts_with("recode"),
    names_to = "Order",
    values_to = "CODE"
  ) %>%
  select(
    "Subj",
    group = "date_group",
    "Order",
    "CODE"
  )

# inspect
glimpse(coded_responses)
class(coded_responses)

# convert to data frame for use with anthrotools package
coded_responses <- as.data.frame(coded_responses)

# inspect
class(coded_responses)

# clean data for anthrotools package
cleandata <- CleanFreeList(
  mydata = coded_responses,
  Order = "Order",
  Subj = "Subj",
  CODE = "CODE",
  ejectBadSubj = T,
  deleteDoubleCode = T,
  ConsolidateOrder = T,
  RemoveMissingData = T
)

# inspect
cleandata

# calculate salience
salience_calculated <- CalculateSalience(
  mydata = cleandata,
  Order = "Order",
  Subj = "Subj",
  CODE = "CODE",
  GROUPING = "group",
  Rescale = TRUE,
  Salience = "Salience"
)

# view
salience_calculated

# salience by code
salience_by_code <- SalienceByCode(
  mydata = salience_calculated,
  CODE = "CODE",
  Salience = "Salience",
  Subj = "Subj",
  dealWithDoubles = "DEFAULT"
)

n_classes
n_responses <- str_c(sep = " ", "for n =", n_responses)
n_responses

today <- Sys.Date()

tab_sub <- str_c(sep = " ", "Top 10 most salient career interests", n_responses, "students in", n_classes, "classrooms")

# print
career_interest_table <- salience_by_code %>%
  arrange(desc(SmithsS)) %>%
  kable(
    digits = 3,
    col.names = c("Occupation", "Average Salience", "Sum Salience", "Smith's S"),
    caption = tab_sub
  )


career_interest_table
write_rds(career_interest_table,
          file = "data/tidy/career_interest_table.rds")

# display table
salience_by_code %>%
  arrange(desc(SmithsS)) %>%
  select(
    "Occupation group" = CODE,
    "Mean Salience" = MeanSalience,
    "Sum Salience" = SumSalience,
    "Smith's S" = SmithsS
  ) %>%
  gt() %>%
  tab_header(
    title = "What do community college students say they want to be when they grow up?",
    subtitle = tab_sub
  ) %>%
  tab_source_note(
    source_note = "Source: Students enrolled in 100 & 200 level anthropology courses"
  ) %>%
  tab_source_note(
    source_note = "Reference: Occupational Outlook Handbook. U.S. Bureau of Labor Statistics. Accessed August 28, 2021. https://www.bls.gov/ooh/home.htm."
  )

# bar chart
career_interest_chart <- salience_by_code %>%
  arrange(desc(SmithsS)) %>%
  slice_max(
    order_by = SmithsS,
    n = 10
  ) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(CODE, SmithsS), y = SmithsS), fill = "purple") +
  coord_flip() +
  labs(
    title = "What do my students say they want to be when they grow up?",
    subtitle = paste(tab_sub, today, sep = " "),
    x = "Occupation group*",
    y = "Salience index",
    caption = "* Occupational Outlook Handbook. U.S. Bureau of Labor Statistics.
    Accessed August 28, 2021. https://www.bls.gov/ooh/home.htm."
  ) +
  ylim(0, 1) +
  theme_economist_white(base_size = 18) +
  theme(
    aspect.ratio = 9/16
    )

career_interest_chart

ggsave(career_interest_chart,
  filename = "figures/charts/salience_of_career_interest.png",
  width = 16,
  height = 9,
  scale = 2/3
)

write_rds(career_interest_chart,
          file = "data/tidy/career_interest_chart.rds")