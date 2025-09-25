###############################################################################
# Matt Dancho's AB Testing Tutorial: Use case with Adspend campaign for hotel #
# bookings                                                                    #
###############################################################################


# Causal Inference
library(infer)

# Core
library(tidyverse)
library(lubridate)
library(plotly)
library(timetk)
library(ggplot2)

setwd("~/Downloads")

# Bookings and Adspend Analysis

# Questions
# 1. Does Adspend increase bookings
# 2. By how much? Was ther a return on adspending (ROAS)

bookings_df <- read.csv("https://raw.githubusercontent.com/business-science/free_r_tips/refs/heads/master/073_ab_testing_infer/data/hotel_bookings_geo_experiment.csv")

head(bookings_df)


# Convert date into date/time variable
bookings_df$date <- date(bookings_df$date)

### INTERVENTION PERIOD
PRE_INTERVENTION <- c("2015-01-05", "2015-02-15") |> as_date()
POST_INTERVENTION <- c("2015-02-16", "2015-03-15") |> as_date()

# Data Exploration

bookings_by_assignment <- bookings_df %>%
  group_by(assignment) %>%
  summarize_by_time(
    bookings = sum(bookings),
    cost  = sum(cost),
    .by   = "day",
  ) %>%
  ungroup()

bookings_by_assignment |>
  group_by(assignment) |>
  plot_time_series(
    date, bookings,
    .color_var = assignment,
    .interactive = FALSE,
    .title = "Adspend Effect"
  ) +
  annotate("rect",
           xmin = as_date("2015-02-16"),
           xmax = as_date("2015-03-15"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "blue"
  )


# AB Testing

## Split data into pre- and post- experiment
pre_intervention_only <- bookings_df |>
  filter_by_time(.date_var = date, .start_date = PRE_INTERVENTION[1],.end_date = PRE_INTERVENTION[2])

adspend_campaign <- bookings_df |>
  filter_by_time(.date_var = date, .start_date = POST_INTERVENTION[1],.end_date = POST_INTERVENTION[2])


# Check Balance and Sample Size
pre_intervention_only |> count(assignment)

adspend_campaign |> count(assignment)

# Check Sales Distribution
pre_intervention_only |>
  group_by(assignment) |>
  summarise(bookings = sum(bookings))

adspend_campaign |>
  group_by(assignment) |>
  summarise(bookings = sum(bookings))

# Visualize sales distribution
adspend_campaign |>
  ggplot(aes(bookings, fill = assignment)) +
  geom_density(color = "white", alpha = 0.75) +
  theme_minimal()

# Booking Summary
summary_adspend_campaign <- adspend_campaign |>
  select(assignment, bookings, cost) |>
  group_by(assignment) |>
  summarise(
    bookings_mean_xlog = mean(bookings),
    bookings_mean_log = mean(log(bookings)),
    cost_mean = mean(cost)
  )

summary_adspend_campaign

# Visualize the log distribution
g <- adspend_campaign |>
  ggplot(aes(log(bookings), fill = assignment)) +
  geom_density(color = "white", alpha = 0.75) +
  geom_vline(aes(xintercept = bookings_mean_log), color = "black",
             linetype = "dashed", data = summary_adspend_campaign) +
  theme_minimal()

ggplotly(g)


### 2-sample t-test
diff_in_means_data <- adspend_campaign |>
  select(assignment,bookings)

test_statistic_data <- diff_in_means_data |>
  t_test(
    bookings ~ assignment,
    order = c("treatment", "control"),
    alternative = "two-sided"
  )

test_statistic_data

### Linear Regression

lm(bookings ~ assignment, data = diff_in_means_data) |>
  summary()

# Average Treatment Effect (ATE)

ate = test_statistic_data$estimate
ate

N = diff_in_means_data |>
  count(assignment) |> pull(n) |> pluck(2)
N

# N * ATE
bookings_increase = 1393*96.2

# Return on Adspend Campaign (ROAS) = (bookings_increase) / cost
# Aggregate cost by assignment

adspend_campaingn_cost <- bookings_by_assignment |>
  group_by(assignment) |>
  summarise_at(vars(cost), list(name = sum))

### Adspend cost = $50k
### Calculate ROAS
ROAS = bookings_increase / 50000
ROAS

