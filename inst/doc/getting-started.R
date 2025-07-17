## ----setup, include=FALSE, message=FALSE, warning=FALSE-----------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

set.seed(1234)

library(cbcTools)

## -----------------------------------------------------------------------------
profiles <- cbc_profiles(
  price = c(1.0, 1.5, 2.0, 2.5, 3.0), # Price per pound ($)
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

profiles

## -----------------------------------------------------------------------------
priors <- cbc_priors(
  profiles = profiles,
  price = -0.25, # Negative = people prefer lower prices
  type = c(0.5, 1), # Gala and Honeycrisp preferred over Fuji (reference)
  freshness = c(0.6, 1.2) # Average and Excellent preferred over Poor (reference)
)

priors

## -----------------------------------------------------------------------------
design <- cbc_design(
  profiles = profiles,
  method = "stochastic", # D-optimal method
  n_alts = 3, # 2 alternatives per choice question
  n_q = 6, # 6 questions per respondent
  n_resp = 300, # 300 respondents
  priors = priors # Use our priors for optimization
)

design

## -----------------------------------------------------------------------------
cbc_inspect(design)

## -----------------------------------------------------------------------------
# Simulate choices using our priors
choices <- cbc_choices(design, priors = priors)

choices

## -----------------------------------------------------------------------------
choices_cat <- cbc_decode(choices)

# Filter for the chosen rows only
choices_cat <- choices_cat[which(choices_cat$choice == 1), ]

# Counts of choices made for each attribute level
table(choices_cat$price)
table(choices_cat$type)
table(choices_cat$freshness)

## -----------------------------------------------------------------------------
power <- cbc_power(choices)
power

## ----fig.alt = "Power analysis chart showing statistical power vs sample size for 5 parameters. A red dashed line marks 90% power threshold. Most parameters achieve adequate power by 100 respondents, though freshnessAverage and typeGala require larger sample sizes than price and other freshness/type parameters."----

plot(power, type = "power", power_threshold = 0.9)

## -----------------------------------------------------------------------------
summary(power, power_threshold = 0.9)

