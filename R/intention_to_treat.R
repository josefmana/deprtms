#
# This is a script used to analyse data via intention-to-treat analysis
#

#
# Goals:
#   1. lmerTest 'anova tables' and emmeans-based PWCs comparable to per-protocol ANOVAs
#   2. rstanarm models equivalent to the lmer4 ones + others with added patient-specific effects
#   3. brms multivariate models with patient-specific effects and covariance between measures
#
