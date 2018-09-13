# Nested Models ----

# Say we have a model with a dependent variable "DV", independent variable "IV"
# and groups as random effects ("Cluster", "Subject"). The "IV" varies accross
# "Cluster" and "Subject".
#
# Is this a nested, fully crossed or cross-classified design?
#
# The key distinction is whether each "Subject" receives a completely
# different "Cluster" set. If this is the case the design is nested.

lmer(DV ~ IV + (1 + IV | Cluster / Subject), data = ...)

# which expands to...

lmer(DV ~ IV + (1 + IV | Cluster ) + (1 + IV | Cluster:Subject), data = ...)


# Fully-crossed or cross-classified models ----

# If each "Subject" receives the same "Cluster", it is a fully crossed
# random factors design. If there is some mixture it is cross-classified
# and this is still the appropriate model:

lmer(DV ~ IV + (1 + IV | Cluster) + (1 + IV | Subject), data = ...)


# related post: https://www.researchgate.net/post/Multilevel_modelling_in_R

# see also: https://stats.stackexchange.com/a/228814/54740
