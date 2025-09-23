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

priors <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c('Gala' = 0.5, 'Honeycrisp' = 1.0),
  freshness = c('Average' = 0.6, 'Excellent' = 1.2)
)

## -----------------------------------------------------------------------------
design <- cbc_design(
  profiles = profiles,
  n_alts = 2, # Alternatives per question
  n_q = 6, # Questions per respondent
  n_resp = 100 # Number of respondents
)

design

## -----------------------------------------------------------------------------
design_decoded <- cbc_decode(design)
design_decoded

## -----------------------------------------------------------------------------
design_random <- cbc_design(
  profiles = profiles,
  method = "random",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

# Quick inspection
cbc_inspect(design_random, sections = "structure")

## -----------------------------------------------------------------------------
design_shortcut <- cbc_design(
  profiles = profiles,
  method = "shortcut",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design_minoverlap <- cbc_design(
  profiles = profiles,
  method = "minoverlap",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

design_balanced <- cbc_design(
  profiles = profiles,
  method = "balanced",
  n_alts = 2,
  n_q = 6,
  n_resp = 100
)

## -----------------------------------------------------------------------------
design_stochastic <- cbc_design(
  profiles = profiles,
  method = "stochastic",
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  n_start = 1 # Number of random starting points
)

design_modfed <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "modfed",
  n_start = 1
)

design_cea <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "cea",
  n_start = 1
)

## -----------------------------------------------------------------------------
cbc_compare(
  "Random" = design_random,
  "Shortcut" = design_shortcut,
  "Min Overlap" = design_minoverlap,
  "Balanced" = design_balanced,
  "Stochastic" = design_stochastic,
  "Modfed" = design_modfed,
  "CEA" = design_cea
)

## -----------------------------------------------------------------------------
# For D-optimal methods, must include no_choice in priors
priors_nochoice <- cbc_priors(
  profiles = profiles,
  price = -0.1,
  type = c(0.1, 0.2),
  freshness = c(0.1, 0.2),
  no_choice = -0.5 # Negative value makes no-choice less attractive
)

design_nochoice <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  no_choice = TRUE,
  priors = priors_nochoice,
  method = "stochastic"
)

head(design_nochoice)

## -----------------------------------------------------------------------------
design_labeled <- cbc_design(
  profiles = profiles,
  n_alts = 3, # Will be overridden to match number of type levels
  n_q = 6,
  n_resp = 100,
  label = "type", # Use 'type' attribute as labels
  method = "random"
)

head(design_labeled)

## -----------------------------------------------------------------------------
# Create profiles with attribute-specific feature
profiles_vehicles <- cbc_profiles(
  price = c(15, 20, 25), # Price in $1,000s
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  powertrain = c('gas', 'hybrid', 'electric'),
  range_electric = c(0, 100, 150, 200, 250) # EV driving range (miles)
) |> 
  # Restrict non-electric powertrains from having range_electric other than 0
  cbc_restrict(
    (powertrain == 'electric') & (range_electric == 0),
    (powertrain != 'electric') & (range_electric != 0)
  )

# Check the balance problem - electric vehicles are over-represented
table(profiles_vehicles$powertrain)

## -----------------------------------------------------------------------------
# Design without balance_by - electric powertrains over-represented
design_unbalanced <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 100,
  method = "random"
) |>
    cbc_decode() # Converts dummy-coded powertrain attributes back to "powertrain"

# Check powertrain distribution - again, electric is over-represented
table(design_unbalanced$powertrain)

## -----------------------------------------------------------------------------
# Design with balance_by - balanced powertrain representation
design_balanced <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 100,
  method = "random",
  balance_by = "powertrain" # Balance across powertrain levels
) |> 
    cbc_decode()

# Check improved powertrain distribution - now powertrain is balanced
table(design_balanced$powertrain)

## -----------------------------------------------------------------------------
# Balance across combinations of powertrain and price
design_multi_balance <- cbc_design(
  profiles = profiles_vehicles,
  n_alts = 3,
  n_q = 8,
  n_resp = 50,
  method = "random",
  balance_by = c("powertrain", "price")
) |> 
    cbc_decode()

# Check improved powertrain distribution - now powertrain and price are balanced
table(design_multi_balance$powertrain)
table(design_multi_balance$price)

## -----------------------------------------------------------------------------
design_blocked <- cbc_design(
  profiles = profiles,
  method = "stochastic",
  priors = priors,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  n_blocks = 2 # Create 2 different design blocks
)

# Check block allocation
table(design_blocked$blockID)

## -----------------------------------------------------------------------------
design_no_dominance <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors,
  method = "stochastic",
  remove_dominant = TRUE,
  dominance_types = c("total", "partial"),
  dominance_threshold = 0.8
)

## -----------------------------------------------------------------------------
# Create priors with interactions
priors_interactions <- cbc_priors(
  profiles = profiles,
  price = -0.25,
  type = c("Fuji" = 0.5, "Gala" = 1.0),
  freshness = c(0.6, 1.2),
  interactions = list(
    # Price is less negative (less price sensitive) for Fuji apples
    int_spec(
      between = c("price", "type"),
      with_level = "Fuji",
      value = 0.5
    ),
    # Price is slightly less negative for Gala apples
    int_spec(
      between = c("price", "type"),
      with_level = "Gala",
      value = 0.2
    )
    # Honeycrisp uses reference level (no additional interaction term)
  )
)

design_interactions <- cbc_design(
  profiles = profiles,
  n_alts = 2,
  n_q = 6,
  n_resp = 100,
  priors = priors_interactions,
  method = "stochastic"
)

## -----------------------------------------------------------------------------
# Detailed inspection of the stochastic design
cbc_inspect(
  design_stochastic,
  sections = "all"
)

## ----eval=FALSE---------------------------------------------------------------
# # Advanced stochastic design with custom settings
# design_advanced <- cbc_design(
#   profiles = profiles,
#   n_alts = 2,
#   n_q = 8,
#   n_resp = 300,
#   n_blocks = 2,
#   priors = priors,
#   method = "stochastic",
#   n_start = 10, # More starting points for better optimization
#   max_iter = 100, # More iterations per start
#   n_cores = 4, # Parallel processing
#   remove_dominant = TRUE,
#   dominance_threshold = 0.9,
#   randomize_questions = TRUE,
#   randomize_alts = TRUE
# )

