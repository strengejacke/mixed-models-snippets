library(lme4)
library(dplyr)
library(sjPlot)

# This example shows how to address the issue of correlating predictors (fixed
# effects) and group (random) effects, especially in panel data, for mixed models
# (so called Gauss-Markov-assumption). In particular in econometrics, fixed-effects
# models are the gold-standard in these cases, however, often it makes no sense
# to consider group-effects as "fixed" over a long time. Apart from this, there
# are also other shortcomings of FE-models as well, see e.g.
# Bell A, Fairbrother M, Jones K. Fixed and random effects models: making an
# informed choice. Quality & Quantity. 2018; doi:10.1007/s11135-018-0802-x

# This code snippet shows how to translate the Eq. 10 from the above mentioned
# paper into R-code, using lmer() from the lme4-package.

load("example.RData")

# Variables:
# x_tv  : time-varying variable
# z1_ti : first time-invariant variable, co-variate
# z2_ti : second time-invariant variable, co-variate
# QoL   : Response (quality of life of patient)
# ID    : patient ID
# time  : time-point of measurement


# "classical" growth-model for longitudinal data

m1 <- lmer(
  QoL ~ time + age + x_tv + z1_ti + z2_ti + (1 + time | ID),
  data = d
)

# Next is a model from Eq. 10, which includes the "de-meaned" time-varying
# variable as well as the "group-meaned" time-varying variable

# compute mean of "x_tv" for each subject (ID) and
# then "de-mean" x_tv

# x_tv_gm : time-varying variable with the mean of "x_tv" accross all
#           time-points, for each patient (ID).
# x_tv_dm : the de-meaned time-varying variable "x_tv"

d <- d %>%
  group_by(ID) %>%
  mutate(
    x_tv_gm = mean(x_tv, na.rm = T),
    x_tv_dm = x_tv - x_tv_gm
  ) %>%
  ungroup()

# The random-effect-within-between model (REWB)

# Eq. 10 suggests allowing the "within-effect" (de-meaned) vary across
# individuals, that's why "x_tv_dm" is added as random slope as well.
# This leads to an error (number of observations <= number of random effects),
# so the check for nobs vs. re is ignored here

# Here, the estimate of "x_tv_dm" indicates the within-subject effect,
# while the estimate of "x_tv_gm" indicates the between-subject effect.
# This model also allows for heterogenity across level-2 units, that's
# why "x_tv_dm" also appears in the random effects.

# Model from Equation 10

m2 <- lmer(
  QoL ~ time + age + x_tv_dm + x_tv_gm + z1_ti + z2_ti + (1 + time + x_tv_dm | ID),
  data = d,
  control = lmerControl(check.nobs.vs.nRE = "ignore")
)

# After email correspondance, the paper's authors suggest that, depending
# on the research interest and necessary complexity of the model, a
# "simple" random-slope might be suitable as well. As stated in the paper,
# this is useful if homogenity across level-2 units is assumed.

# ...however, I'm a bit unsure whether "time" or "x_tv_dm" should be used
# as random-slope. I think "m3" is better here. Again, the estimate of
# "x_tv_dm" indicates the within-subject effect, while the estimate of
# "x_tv_gm" indicates the between-subject effect.

# Model from Equation 2

m3 <- lmer(
  QoL ~ time + age + x_tv_dm + x_tv_gm + z1_ti + z2_ti + (1 + time | ID),
  data = d
)

m4 <- lmer(
  QoL ~ time + age + x_tv_dm + x_tv_gm + z1_ti + z2_ti + (1 + x_tv_dm | ID),
  data = d
)

# An alternativ would be the "Mundlak" model. Here, the estimate of "x_tv"
# indicates the within-subject effect, while the estimate of "x_tv_gm" indicates
# the "contextual" effect.

# Model from Equation 3

m5 <- lmer(
  QoL ~ time + age + x_tv + x_tv_gm + z1_ti + z2_ti + (1 + time | ID),
  data = d
)

tab_model(m1, m2, m3, m5, show.ci = F, show.se = T)
