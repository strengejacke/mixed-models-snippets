# Time-varying covariates ----

# Time-varying covariates are most flexibly modelled with splines.
# Here are some examples, from simple longitudinal models to models
# with time-varying covariates.
#
# y = outcome (continuous / Gaussian)
# t = time-points
# tv = time-varying covariate
# tc = time-constant covariate
# id = subject-ID

# Model 1 - constant change in time ----
lmer(y ~ t + tv + tc + (1 + t | id), data)

# Model 2 - constant change in time, different slopes depending on covariate ----
# (interaction)
lmer(y ~ t * tv + tc + (1 + t | id), data)

# Model 3 - non-linear change in time ----
lmer(y ~ t + I(t^2) + tv + tc + (1 + t | id), data)

# Model 4 - non-linear change in time, different slopes depending on covariate ----
# (interaction)
lmer(y ~ t * tv + I(t^2) * tv + tc + (1 + t | id), data)

# Model 5 - non-linear change in time, time-varying covariate ----
# (interaction with non-linear covariate)
lmer(y ~ y ~ t * tv + I(t^2) * tv + t * I(tv^2) + I(t^2) * I(tv^2) + tc + (1 + t | id), data)

# Model 6 - non-linear change in time, time-varying covariate ---
# (cubic instead of quadratic interaction)
model6 <- lmer(
  y ~ t * tv + I(t^2) * tv + I(t^3) * tv + t * I(tv^2) + t * I(tv^3) +
    I(t^2) * I(tv^2) + I(t^2) * I(tv^3) + I(t^3) * I(tv^2) +
    I(t^3) * I(tv^3) + tc + (1 + t | id),
  data
)

# This final model with splines is almost identical to model 6,
# when comparing marginal effects. The time-varying covariate
# needs to be specified in the "by"-argument from the spline-term.
# Time "t" is non-linear (spline), "tv" varies over time.
model7 <- lmer(y ~ s(t) + s(t, by = tv) + tc + (1 + t | id), data)
