
# Statistical Test - ANCOVA

set.seed(40)
rnorm_fixed = function(N, mu = 0, sd = 1)
  scale(rnorm(N)) * sd + mu

N = 20  # Number of samples per group
D = data.frame(
  value = c(rnorm_fixed(N, 0), rnorm_fixed(N, 1), rnorm_fixed(N, 0.5)),
  group = rep(c('a', 'b', 'c'), each = N),

  #group_a = rep(c(1, 0, 0), each=N),  # This is the intercept
  group_b = rep(c(0, 1, 0), each = N),
  group_c = rep(c(0, 0, 1), each = N)
)

D$age = D$value + rnorm_fixed(nrow(D), sd = 3)  # Correlated to value
View(D)

# Dedicated ANCOVA functions.

model = car::Anova(aov(value ~ group + age, D))
#model = aov(value ~ group + age, D)  # Predictor order matters!
model

# Testing main effect of age
null_age = lm(value ~ 1 + group_b + group_c, D)  # Full without age. One-way ANOVA!
result_age = anova(null_age, full)
result_age # same F, p-value with the result of 'model'

# Testing main effect of group using Likelihood-ratio test
null_group = lm(value ~ 1 + age, D)  # Full without group. Pearson correlation!
result_group = anova(null_group, full)
result_group # same F, p-value with the result of 'model'

# As dummy-coded linear model
full = lm(value ~ 1 + group_b + group_c + age, D)
summary(full)


