source("helpers.R")
requiet("prediction")
requiet("insight")


# lm: Average prediction vs. {prediction}
mod <- lm(am ~ mpg + drat + factor(cyl), data = mtcars)
pre <- predictions(mod)
tid <- tidy(pre)
expect_equal(nrow(tid), 1)
expect_equal(mean(pre$predicted), tid$estimate)
lee <- data.frame(summary(prediction::prediction(mod)))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(tid$std.error, lee$SE)

# lm: Group-Average Prediction (no validity)
pre <- predictions(mod)
tid <- tidy(pre, by = "cyl")
expect_equal(nrow(tid), 3)

# glm response scale
# CI retrieved by `insight::get_predicted()` for units
# CI not supported yet for `tidy()`
mod <- glm(am ~ mpg + drat + factor(cyl), data = mtcars, family = binomial)
pre <- predictions(mod)
tid <- tidy(pre)
lee <- data.frame(summary(prediction::prediction(mod)))
ins <- data.frame(insight::get_predicted(mod))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(pre$predicted, ins$Predicted)
expect_equivalent(pre$std.error, ins$SE)
expect_equivalent(pre$conf.low, ins$CI_low)
expect_equivalent(pre$conf.high, ins$CI_high)
expect_false("std.error" %in% colnames(tid))
expect_false("conf.low" %in% colnames(tid))

# glm link scale: CI fully supported
mod <- glm(am ~ mpg + drat + factor(cyl), data = mtcars, family = binomial)
pre <- predictions(mod, type = "link")
tid <- tidy(pre)
lee <- data.frame(summary(prediction::prediction(mod, type = "link")))
ins <- data.frame(insight::get_predicted(mod, predict = "link"))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(tid$std.error, lee$SE)
expect_equivalent(tid$conf.low, lee$lower)
expect_equivalent(tid$conf.high, lee$upper)
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(pre$predicted, ins$Predicted)
expect_equivalent(pre$std.error, ins$SE)
expect_equivalent(pre$conf.low, ins$CI_low)
expect_equivalent(pre$conf.high, ins$CI_high)




dat <- mtcars
dat$am <- factor(dat$am)
mod <- glm(vs ~ am, data = dat, family = binomial)
pre <- predictions(mod)
tidy(pre, by = "am")

comparisons(
    mod,
    newdata = pre,
    contrast_numeric = 0,
    transform_pre = function(hi, lo) mean(hi),

x <- pre
model <- attr(x, "model")
V <- attr(x, "vcov")
type <- attr(x, "type")

fun <- function(model, newdata, ...) {
    out <- get_predict(
        model,
        newdata = x,
        vcov = FALSE,
        ...)
    out <- mean(out[["predicted"]])
    return(out)
}

se <- get_se_delta(
    model,
    newdata = x,
    vcov = V,
    type = type,
    FUN = fun,
    eps = 1e-4)

    , # avoid pushing through ...
    ...)


pre <- predictions(
    mod,
    type = "link",
    )
tidy(pre, by = "am")
