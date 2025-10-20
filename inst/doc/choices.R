## ----setup, include=FALSE, message=FALSE, warning=FALSE-----------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

set.seed(123)

## -----------------------------------------------------------------------------
library(cbcTools)

profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

design <- cbc_design(
  profiles = profiles,
  method = "random",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design

## -----------------------------------------------------------------------------
# Random choice simulation (default)
choices_random <- cbc_choices(design)

head(choices_random)

# Check choice distribution
table(choices_random$choice, choices_random$altID)

## -----------------------------------------------------------------------------
# Create priors for utility-based simulation
priors <- cbc_priors(
  profiles = profiles,
  price = -0.25, # Negative preference for higher prices
  type = c(0.5, 1), # Gala and Honeycrisp preferred over Fuji
  freshness = c(0.6, 1.2) # Average and Excellent preferred over Poor
)

# Utility-based choice simulation
choices_utility <- cbc_choices(design, priors = priors)

head(choices_utility)

## -----------------------------------------------------------------------------
head(choices_utility)

## -----------------------------------------------------------------------------
# Create design with no-choice option
design_nochoice <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  no_choice = TRUE,
  method = "random"
)

# Create priors including no-choice utility
priors_nochoice <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = -0.5 # Negative = no-choice less attractive
)

# Simulate choices
choices_nochoice <- cbc_choices(
  design_nochoice,
  priors = priors_nochoice
)

# Examine no-choice rates
nochoice_rate <- mean(choices_nochoice$choice[choices_nochoice$no_choice == 1])
cat("No-choice selection rate:", round(nochoice_rate * 100, 1), "%\n")

## -----------------------------------------------------------------------------
# Create priors with random parameters
priors_random <- cbc_priors(
  profiles = profiles,
  price = rand_spec(dist = "n", mean = -0.1, sd = 0.05),
  type = rand_spec(dist = "n", mean = c(0.1, 0.2), sd = c(0.05, 0.1)),
  freshness = c(0.1, 0.2), # Keep some parameters fixed
  n_draws = 100
)

# Simulate choices with preference heterogeneity
choices_mixed <- cbc_choices(design, priors = priors_random)

## -----------------------------------------------------------------------------
# Create priors with interactions
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.1,
  type = c("Fuji" = 0.5, "Gala" = 1),
  freshness = c("Average" = 0.6, "Excellent" = 1.2),
  interactions = list(
    # Price sensitivity varies by apple type
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.5
    ),
    int_spec(
      between = c("price", "type"),
      with_level = "Gala",
      value = 0.2
    )
  )
)

# Simulate choices with interaction effects
choices_interactions <- cbc_choices(
  design,
  priors = priors_interactions
)

## -----------------------------------------------------------------------------
# Convert to standard encoding to get categorical variables
choices_standard <- cbc_encode(choices_utility, coding = "standard")

# Aggregate attribute choices across all respondents
choices <- choices_standard

# Price choices
price_choices <- aggregate(choice ~ price, data = choices, sum)
price_choices$prop <- price_choices$choice / sum(price_choices$choice)
print(price_choices)

# Type choices
type_choices <- aggregate(choice ~ type, data = choices, sum)
type_choices$prop <- type_choices$choice / sum(type_choices$choice)
print(type_choices)

# Freshness choices
freshness_choices <- aggregate(choice ~ freshness, data = choices, sum)
freshness_choices$prop <- freshness_choices$choice /
  sum(freshness_choices$choice)
print(freshness_choices)

## -----------------------------------------------------------------------------
# Create dataset with only chosen alternatives
# Convert to dummy coding to more easily select individual levels
chosen_alts <- choices_mixed[choices_mixed$choice == 1, ] |> 
  cbc_encode('dummy')

# Mean attribute levels chosen by each respondent
resp_means <- aggregate(
  cbind(
    price,
    typeGala,
    typeHoneycrisp,
    freshnessAverage,
    freshnessExcellent
  ) ~
    respID,
  data = chosen_alts,
  mean
)

# Look at variation across respondents
cat("Price variation across respondents:\n")
cat("Mean:", round(mean(resp_means$price), 2), "\n")
cat("SD:", round(sd(resp_means$price), 2), "\n")

cat("\nHoneycrisp choice rate variation:\n")
cat("Mean:", round(mean(resp_means$typeHoneycrisp), 2), "\n")
cat("SD:", round(sd(resp_means$typeHoneycrisp), 2), "\n")

## -----------------------------------------------------------------------------
# Create D-optimal design with priors
design_optimal <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "stochastic"
)

# Use SAME priors for choice simulation
choices_consistent <- cbc_choices(
  design_optimal,
  priors = priors
)

## -----------------------------------------------------------------------------
# Create different priors
different_priors <- cbc_priors(
  profiles = profiles,
  price = -0.2, # Different from design optimization
  type = c(0.2, 0.4),
  freshness = c(0.2, 0.4)
)

# This will generate a warning about inconsistent priors
choices_inconsistent <- cbc_choices(
  design_optimal,
  priors = different_priors
)

