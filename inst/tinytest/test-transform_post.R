source("helpers.R", local = TRUE)

# exponentiate
acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
acs12$disability <- as.numeric(acs12$disability == "yes")
mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

cmp1 <- comparisons(
    mod,
    variables = "gender",
    transform_pre = "lnratioavg")
cmp2 <- comparisons(
    mod,
    variables = "gender",
    transform_pre = "lnratioavg",
    transform_post = exp)
expect_equivalent(exp(cmp1$comparison), cmp2$comparison)
expect_equivalent(exp(cmp1$conf.low), cmp2$conf.low)
expect_equivalent(exp(cmp1$conf.high), cmp2$conf.high)

expect_error(tidy(cmp2, transform_post = exp))
expect_error(summary(cmp2, transform_post = exp))

tid1 <- tidy(cmp1)
tid2 <- tidy(cmp1, transform_post = exp)
expect_equivalent(exp(tid1$estimate), tid2$estimate)
expect_equivalent(exp(tid1$conf.low), tid2$conf.low)
expect_equivalent(exp(tid1$conf.high), tid2$conf.high)


# back transform link-scale predictions
pre1 <- predictions(mod, type = "link")
pre2 <- predictions(mod, type = "response")
pre3 <- predictions(mod, type = "link", transform_post = insight::link_inverse(mod))
expect_equivalent(pre2$predicted, pre3$predicted)
expect_equivalent(pre2$conf.low, pre3$conf.low)
expect_equivalent(pre2$conf.high, pre3$conf.high)
expect_true(all(pre1$predicted != pre3$predicted))

# averaging before or after the transform makes a big difference
# I know this fails but I don't want to skip it because it is a critical question I need to understand better.
tid2 <- tidy(pre2)
tid3 <- tidy(pre3)
expect_equivalent(tid2$estimate, tid3$estimate)
