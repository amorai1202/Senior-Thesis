# Power analysis
library(pwr)

# Convert effect size corresponding to Shih et al., 2015
# F(4, 64) = 4.76, p = .001, η² = .23

eta2_to_f2 <- function(es) {
  es / (1 - es)
}
eta2_to_f <- function(es) {
  sqrt(eta2_to_f2(es))
}
eta2_to_f(.23)

# Translates to f = 0.55

# One-way ANOVA with 3 groups (i.e., control, implicit, explicit)

# Medium effect size: f = 0.25
pwr.anova.test(k = 3,
               f = 0.25,
               sig.level = 0.05,
               power = 0.8)

# n = 52.3966 per group
# Sample target = 159 participants

# Mediation analyses: n = 260