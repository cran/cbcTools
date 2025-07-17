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

profiles

## -----------------------------------------------------------------------------
# Basic fixed priors
priors_fixed <- cbc_priors(
  profiles = profiles,
  price = -0.25, # Negative = prefer lower prices
  type = c(0.5, 1.0), # Preferences relative to reference level
  freshness = c(0.6, 1.2) # Preferences relative to reference level
)

priors_fixed

## -----------------------------------------------------------------------------
priors_named <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Gala" = 0.5, "Honeycrisp" = 1.0),
  freshness = c("Average" = 0.6, "Excellent" = 1.2)
)

## -----------------------------------------------------------------------------
identical(priors_fixed$pars, priors_named$pars)

## -----------------------------------------------------------------------------
priors_random <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.25,
    sd = 0.1
  ),
  type = c(0.5, 1.0),
  freshness = rand_spec(
    dist = "n",
    mean = c(0.6, 1.2),
    sd = c(0.1, 0.1)
  )
)

priors_random

## -----------------------------------------------------------------------------
priors_correlated <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.1,
    sd = 0.05,
    correlations = list(
      cor_spec(
        with = "type",
        with_level = "Honeycrisp",
        value = 0.3
      )
    )
  ),
  type = rand_spec(
    dist = "n",
    mean = c("Gala" = 0.1, "Honeycrisp" = 0.2),
    sd = c("Gala" = 0.05, "Honeycrisp" = 0.1)
  ),
  freshness = c(0.1, 0.2)
)

# View the correlation matrix
priors_correlated$correlation

## ----eval=FALSE---------------------------------------------------------------
# cor_spec(
#   with = "type",
#   value = -0.2
# )

## ----eval=FALSE---------------------------------------------------------------
# cor_spec(
#   with = "type",
#   with_level = "Honeycrisp",
#   value = 0.3
# )

## ----eval=FALSE---------------------------------------------------------------
# cor_spec(
#   with = "freshness",
#   level = "Gala",
#   with_level = "Excellent",
#   value = 0.4
# )

## -----------------------------------------------------------------------------
# Create priors with interaction effects
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Fuji" = 0.5, "Honeycrisp" = 1.0),
  freshness = c("Average" = 0.6, "Excellent" = 1.2),
  interactions = list(
    # Price sensitivity varies by apple type
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.1
    ),
    int_spec(
      between = c("price", "type"),
      with_level = "Honeycrisp",
      value = 0.2
    ),
    # Type preferences vary by freshness
    int_spec(
      between = c("type", "freshness"),
      level = "Honeycrisp",
      with_level = "Excellent",
      value = 0.3
    )
  )
)

priors_interactions

## ----eval=FALSE---------------------------------------------------------------
# int_spec(
#   between = c("price", "type"),
#   with_level = "Fuji",
#   value = 0.05
# )

## ----eval=FALSE---------------------------------------------------------------
# int_spec(
#   between = c("type", "freshness"),
#   level = "Gala",
#   with_level = "Excellent",
#   value = 0.1
# )

## ----eval=FALSE---------------------------------------------------------------
# int_spec(
#   between = c("price", "weight"),
#   value = 0.02
# )

## -----------------------------------------------------------------------------
# Fixed no-choice prior
priors_nochoice_fixed <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = -0.5 # Negative values make no-choice less attractive
)

# Random no-choice prior
priors_nochoice_random <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c(0.5, 1.0),
  freshness = c(0.6, 1.2),
  no_choice = rand_spec(dist = "n", mean = -0.5, sd = 0.2)
)

priors_nochoice_fixed

## ----fig.width=6, fig.height=4, fig.alt="Histogram showing the distribution of price parameter draws. The distribution appears roughly normal, centered around -0.25, with values ranging from approximately -0.5 to 0, indicating negative price sensitivity as expected."----

priors_bayesian <- cbc_priors(
  profiles = profiles,
  price = rand_spec(
    dist = "n",
    mean = -0.25,
    sd = 0.1
  ),
  type = rand_spec(
    dist = "n",
    mean = c(0.5, 1.0),
    sd = c(0.1, 0.2)
  ),
  freshness = c(0.6, 1.2),
  n_draws = 500, # Default = 100
  draw_type = "sobol" # Default = "halton"
)

# Inspect the parameter draws
price_draws <- priors_bayesian$par_draws[, 1]
cat("Parameter draws dimensions:", dim(priors_bayesian$par_draws), "\n")
cat("Mean of price draws:", mean(price_draws), "\n")
cat("SD of price draws:", sd(price_draws), "\n")

# Plot distribution of one parameter
hist(
  price_draws,
  main = "Distribution of Price Parameter Draws",
  xlab = "Price Coefficient"
)

## ----eval=FALSE---------------------------------------------------------------
# # Problem: Price in dollars, prior assumes price in cents
# profiles_dollars <- cbc_profiles(price = c(1.00, 2.00, 3.00), ...)
# priors_cents <- cbc_priors(profiles_dollars, price = -10, ...) # Too large!
# 
# # Solution: Match scales
# priors_dollars <- cbc_priors(profiles_dollars, price = -0.10, ...) # Appropriate

## -----------------------------------------------------------------------------
# If you want "Excellent" as reference, reorder profiles
profiles_reordered <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Excellent', 'Average', 'Poor') # Excellent now reference
)

priors_reordered <- cbc_priors(
  profiles_reordered,
  price = -0.1,
  type = c(0.1, 0.2),
  freshness = c(-0.1, -0.2) # Negative = worse than excellent
)

## ----eval=FALSE---------------------------------------------------------------
# # If you've restricted certain profile combinations,
# # make sure your priors don't assume those combinations are common
# restricted_profiles <- cbc_restrict(
#   profiles,
#   type == "Fuji" & price > 2.5
# )
# 
# # Prior should reflect that expensive Fuji combinations don't exist
# priors_compatible <- cbc_priors(restricted_profiles, ...)

