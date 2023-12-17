# Power analysis
library(pwr)

# One-way ANOVA with 3 groups (i.e., control, subtle, blatant)
small <- 0.10 
medium <- 0.25
alpha <- 0.05
power <- 0.95
groups <- 3  # num groups in ANOVA

# Small effect size
pwr.anova.test(k = groups,
               f = small,
               sig.level = alpha,
               power = power)

# Medium effect size
pwr.anova.test(k = groups,
               f = medium,
               sig.level = alpha,
               power = power)


# Effect size corresponding to Shih et al., 2015
# eta-squared = .23 is approximately equal to a Cohen's f of 0.3

pwr.anova.test(k = groups,
               f = 0.3,
               sig.level = alpha,
               power = power)
