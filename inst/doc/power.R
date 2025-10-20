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

# Create example data for power analysis
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

# Create design and simulate choices
design <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 600, # Large sample for power analysis
  method = "random"
)

priors <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2)
)

choices <- cbc_choices(design, priors = priors)
head(choices)

## -----------------------------------------------------------------------------
# Basic power analysis with auto-detected parameters
power_basic <- cbc_power(
  data = choices,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 10
)

# View the power analysis object
power_basic

# Access the detailed results data frame
head(power_basic$power_summary)
tail(power_basic$power_summary)

## -----------------------------------------------------------------------------
# Auto-detection works with dummy-coded data
power_auto <- cbc_power(
  data = choices,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 8
)

# Shows all parameters: price, typeGala, typeHoneycrisp, freshnessAverage, freshnessExcellent

power_auto

## -----------------------------------------------------------------------------
# First create dummy-coded version of the choices data
choices_dummy <- cbc_encode(choices, 'dummy')

# Focus on specific dummy-coded parameters
power_specific <- cbc_power(
  data = choices_dummy,
  pars = c(
    # Specific dummy variables
    "price",
    "typeHoneycrisp",
    "freshnessExcellent"
  ),
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 8
)

power_specific

## ----fig.alt = "Power analysis chart showing statistical power vs sample size for 5 parameters. A red dashed line marks 90% power threshold. Most parameters achieve adequate power by 100 respondents, though freshnessAverage and typeGala require larger sample sizes than price and other freshness/type parameters."----

# Plot power curves
plot(
  power_basic,
  type = "power",
  power_threshold = 0.9
)

## ----fig.alt = "Standard error chart showing decreasing standard errors as sample size increases from 100 to 600 respondents for 5 parameters. All parameters show the expected decline in standard error with larger samples, with price having consistently lower standard errors than the freshness and type parameters."----

# Plot standard error curves
plot(
  power_basic,
  type = "se"
)

## -----------------------------------------------------------------------------
# Sample size requirements for 90% power
summary(
  power_basic,
  power_threshold = 0.9
)

## -----------------------------------------------------------------------------
# Create choices with random parameters
priors_random <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.25,
    sd = 0.1
  ),
  type = rand_spec(
    dist = "n",
    mean = c(0.5, 1.0),
    sd = c(0.5, 0.5)
  ),
  freshness = c(0.6, 1.2)
)

choices_mixed <- cbc_choices(
  design,
  priors = priors_random
)

# Power analysis for mixed logit model
power_mixed <- cbc_power(
  data = choices_mixed,
  pars = c("price", "type", "freshness"),
  randPars = c(price = "n", type = "n"), # Specify random parameters
  outcome = "choice",
  obsID = "obsID",
  panelID = "respID", # Required for panel data
  n_q = 6,
  n_breaks = 10
)

# Mixed logit models generally require larger samples
power_mixed

## -----------------------------------------------------------------------------
# Create designs with different methods
design_random <- cbc_design(
  profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 200,
  method = "random"
)
design_shortcut <- cbc_design(
  profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 200,
  method = "shortcut"
)
design_optimal <- cbc_design(
  profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 200,
  priors = priors,
  method = "stochastic"
)

# Simulate choices with same priors for fair comparison
choices_random <- cbc_choices(
  design_random,
  priors = priors
)
choices_shortcut <- cbc_choices(
  design_shortcut,
  priors = priors
)
choices_optimal <- cbc_choices(
  design_optimal,
  priors = priors
)

# Conduct power analysis for each
power_random <- cbc_power(
  choices_random,
  n_breaks = 8
)
power_shortcut <- cbc_power(
  choices_shortcut,
  n_breaks = 8
)
power_optimal <- cbc_power(
  choices_optimal,
  n_breaks = 8
)

## ----fig.alt = "Power comparison across three experimental designs (Optimal, Random, Shortcut) shown in separate panels for 5 parameters. Each panel shows power curves with an 80% power threshold line. The Shortcut design generally performs best, followed by Optimal, then Random designs. Some parameters like freshnessExcellent and typeHoneycrisp achieve high power quickly across all designs, while others like typeGala show more variation between design methods."----

# Compare power curves
plot_compare_power(
  Random = power_random,
  Shortcut = power_shortcut,
  Optimal = power_optimal,
  type = "power"
)

## -----------------------------------------------------------------------------
# Return full models for additional analysis
power_with_models <- cbc_power(
  data = choices,
  outcome = "choice",
  obsID = "obsID",
  n_q = 6,
  n_breaks = 5,
  return_models = TRUE
)

# Examine largest model
largest_model <- power_with_models$models[[length(power_with_models$models)]]
summary(largest_model)

