## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(cbcTools)

# Create profiles
profiles <- cbc_profiles(
  price = c(1, 1.5, 2, 2.5, 3),
  type = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c("Poor", "Average", "Excellent")
)

# Create design (uses standard encoding by default)
design <- cbc_design(
  profiles = profiles,
  n_alts = 3,
  n_q = 6,
  n_resp = 100,
  method = "random"
)

head(design)

## -----------------------------------------------------------------------------
design_dummy <- cbc_encode(design, coding = "dummy")
head(design_dummy)

## -----------------------------------------------------------------------------
design_effects <- cbc_encode(design, coding = "effects")
head(design_effects)

## -----------------------------------------------------------------------------
design_standard <- cbc_encode(design_dummy, coding = "standard")
head(design_standard)

## -----------------------------------------------------------------------------
# Use "Honeycrisp" as reference for type, "Excellent" for freshness
design_custom <- cbc_encode(
  design,
  coding = "dummy",
  ref_levels = list(
    type = "Honeycrisp",
    freshness = "Excellent"
  )
)

head(design_custom)

## -----------------------------------------------------------------------------
# Start with dummy coding
design_dummy <- cbc_encode(design, coding = "dummy")

# Update reference levels only (keeps dummy coding)
design_updated <- cbc_encode(
  design_dummy,
  ref_levels = list(type = "Gala")
)

head(design_updated)

## -----------------------------------------------------------------------------
# Create profiles
profiles_nc <- cbc_profiles(
  price = c(1, 2, 3),
  quality = c("Low", "High")
)

# Create priors including no-choice
priors_nc <- cbc_priors(
  profiles = profiles_nc,
  price = -0.1,
  quality = c("High" = 0.5),
  no_choice = -1.5
)

# Create design with no-choice
design_nc <- cbc_design(
  profiles = profiles_nc,
  priors = priors_nc,
  n_alts = 2,
  n_q = 4,
  n_resp = 50,
  no_choice = TRUE,
  method = "random"
)

# Simulate choices
choices_nc <- cbc_choices(design_nc, priors_nc)

head(choices_nc)

## ----eval=FALSE---------------------------------------------------------------
# # Convert to dummy coding for power analysis
# choices_dummy <- cbc_encode(choices_nc, coding = "dummy")
# 
# # Run power analysis
# power_result <- cbc_power(
#   data = choices_dummy,
#   n_breaks = 5
# )
# 
# power_result

## ----eval=FALSE---------------------------------------------------------------
# library(logitr)
# 
# # Convert to dummy coding
# choices_dummy <- cbc_encode(choices, coding = "dummy")
# 
# # Estimate model
# model <- logitr(
#   data = choices_dummy,
#   outcome = "choice",
#   obsID = "obsID",
#   pars = c("price", "typeGala", "typeHoneycrisp",
#            "freshnessAverage", "freshnessExcellent")
# )

## -----------------------------------------------------------------------------
# Work with categorical variables
choices_standard <- design

# Filter for chosen alternatives
chosen <- choices_standard[sample(1:nrow(choices_standard), 100), ]

# Examine choice frequencies by category
table(chosen$type)
table(chosen$freshness)

# Use cbc_inspect
cbc_inspect(choices_standard, sections = 'balance')

## ----eval=FALSE---------------------------------------------------------------
# # Dummy coding: estimates for each level
# power_dummy <- cbc_power(
#   cbc_encode(choices, coding = "dummy"),
#   n_breaks = 5
# )
# 
# # Standard coding: estimates categorical effect
# power_standard <- cbc_power(
#   cbc_encode(choices, coding = "standard"),
#   pars = c("price", "type", "freshness"),
#   n_breaks = 5
# )

