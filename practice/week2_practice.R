## Week 2 practice
#1
library(tidyverse)
library(tidycensus)


# --
#2
pa_income <- get_acs(geography = "county",
                     variables = "B19013_001",
                     state = "PA",
                     year = 2023,
                     survey = "acs5")

pa_income
dim(pa_income)
glimpse(pa_income)
head(pa_income)

# Pennsylvania has 67 counties. Does my row count match?Why or why not?
# Yes, because the unit of observation in the get_acs call;
# the geography = "county" argument, pulls the estimates for each of PA's countties


# --
#3
pa_income$GEOID
as.numeric("01001")
# The leading zero got dropped, giving a nonstandard GEOID format.


# --
#4 filter()
# prediction: fewer rows
filter(pa_income, estimate > 60000)
# result: 56x5

# Counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)
# result: 15x5

# Counties where the estimate is under 50000
filter(pa_income, estimate < 50000)
# result: 1 x 5


# --
#5 select()
# predict: number of rows should not change
select(pa_income, NAME, estimate, moe)
# result: 67x3

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)


# --
#6 mutate()
# predict: same rows, +1 cols as orig
mutate(pa_income, moe_pct = moe / estimate * 100)
# result: 67x6

pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
# moe_pct tells us the margin of error in percent form

# --
#7 arrange()

# predict: shouldn't change the number of rows, just the order
arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct))
# county @ top of desc version: Cameron County


# --
#8 pipe
step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3
# 10x4

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)
# 10x4


# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
# save as worst. What has to change?
worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, moe_pct)

worst
#1x2


#9 group_by(), summarize()
pa_income <- mutate(pa_income, reliable = moe_pct < 5)

# predict: 2 rows, binary reliable yes and no rows
pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))
# 2x3


# --
#10 case_when()
pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low confidence"
  ))

count(pa_income, reliability)
# how many in each category?
# high: 26
# low: 7
# moderate: 34


# --
#11 shape

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001",
                "B01003_001"),
                state = "PA",
                year = 2023,
                survey = "acs5")
pa_two
# 134x5
# each county gets two rows, one for each var


pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA",
  year = 2023,
  survey = "acs5",
  output = "wide"
)

pa_wide # 67x6
# column names: E came from "estimate" value; M came from "moe" value


pa_wide %>%
  mutate(moe_pct = incomeM / incomeE *100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
# 10x4

# popE col for the 10 counties is generally very low.

