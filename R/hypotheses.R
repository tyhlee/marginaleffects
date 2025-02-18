#' (Non-)Linear Tests for Null Hypotheses, Joint Hypotheses, Equivalence, Non Superiority, and Non Inferiority
#'
#' @description
#' Uncertainty estimates are calculated as first-order approximate standard errors for linear or non-linear functions of a vector of random variables with known or estimated covariance matrix. In that sense, [`hypotheses`] emulates the behavior of the excellent and well-established [car::deltaMethod] and [car::linearHypothesis] functions, but it supports more models; requires fewer dependencies; expands the range of tests to equivalence and superiority/inferiority; and offers convenience features like robust standard errors.
#' 
#' To learn more, read the hypothesis tests vignette, visit the
#' package website, or scroll down this page for a full list of vignettes:
#' 
#' * <https://marginaleffects.com/articles/hypothesis.html>
#' * <https://marginaleffects.com/>
#' 
#' Warning #1: Tests are conducted directly on the scale defined by the `type` argument. For some models, it can make sense to conduct hypothesis or equivalence tests on the `"link"` scale instead of the `"response"` scale which is often the default.
#' 
#' Warning #2: For hypothesis tests on objects produced by the `marginaleffects` package, it is safer to use the `hypothesis` argument of the original function.  Using `hypotheses()` may not work in certain environments, in lists, or when working programmatically with *apply style functions.
#' 
#' Warning #3: The tests assume that the `hypothesis` expression is (approximately) normally distributed, which for non-linear functions of the parameters may not be realistic. More reliable confidence intervals can be obtained using the \code{inferences()} function with `method = "boot"`.
#' 
#' @inheritParams comparisons
#' @param model Model object or object generated by the `comparisons()`, `slopes()`, `predictions()`, or `marginal_means()` functions.
#' @param FUN `NULL` or function. 
#' * `NULL` (default): hypothesis test on a model's coefficients, or on the quantities estimated by one of the `marginaleffects` package functions.
#' * Function which accepts a model object and returns a numeric vector or a data.frame with two columns called `term` and `estimate`. This argument can be useful when users want to conduct a hypothesis test on an arbitrary function of quantities held in a model object.
#' @param joint Joint test of statistical significance. The null hypothesis value can be set using the `hypothesis` argument.
#' - FALSE: Hypotheses are not tested jointly.
#' - TRUE: All parameters are tested jointly.
#' - String: A regular expression to match parameters to be tested jointly. `grep(joint, perl = TRUE)`
#' - Character vector of parameter names to be tested. Characters refer to the names of the vector returned by `coef(object)`.
#' - Integer vector of indices. Which parameters positions to test jointly.
#' @param joint_test A character string specifying the type of test, either "f" or "chisq". The null hypothesis is set by the `hypothesis` argument, with default null equal to 0 for all parameters.
#'
#' @section Joint hypothesis tests:
#' The test statistic for the joint Wald test is calculated as (R * theta_hat - r)' * inv(R * V_hat * R') * (R * theta_hat - r) / Q,
#' where theta_hat is the vector of estimated parameters, V_hat is the estimated covariance matrix, R is a Q x P matrix for testing Q hypotheses on P parameters,
#' r is a Q x 1 vector for the null hypothesis, and Q is the number of rows in R. If the test is a Chi-squared test, the test statistic is not normalized.
#' 
#' The p-value is then calculated based on either the F-distribution (for F-test) or the Chi-squared distribution (for Chi-squared test).
#' For the F-test, the degrees of freedom are Q and (n - P), where n is the sample size and P is the number of parameters. 
#' For the Chi-squared test, the degrees of freedom are Q.
#'
#' @template equivalence
#' @examples
#' library(marginaleffects)
#' mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)
#' 
#' # When `FUN` and `hypotheses` are `NULL`, `hypotheses()` returns a data.frame of parameters
#' hypotheses(mod)
#' 
#' # Test of equality between coefficients
#' hypotheses(mod, hypothesis = "hp = wt")
#' 
#' # Non-linear function
#' hypotheses(mod, hypothesis = "exp(hp + wt) = 0.1")
#' 
#' # Robust standard errors
#' hypotheses(mod, hypothesis = "hp = wt", vcov = "HC3")
#' 
#' # b1, b2, ... shortcuts can be used to identify the position of the
#' # parameters of interest in the output of FUN
#' hypotheses(mod, hypothesis = "b2 = b3")
#' 
#' # wildcard
#' hypotheses(mod, hypothesis = "b* / b2 = 1")
#' 
#' # term names with special characters have to be enclosed in backticks
#' hypotheses(mod, hypothesis = "`factor(cyl)6` = `factor(cyl)8`")
#' 
#' mod2 <- lm(mpg ~ hp * drat, data = mtcars)
#' hypotheses(mod2, hypothesis = "`hp:drat` = drat")
#' 
#' # predictions(), comparisons(), and slopes()
#' mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
#' cmp <- comparisons(mod, newdata = "mean")
#' hypotheses(cmp, hypothesis = "b1 = b2")
#' 
#' mfx <- slopes(mod, newdata = "mean")
#' hypotheses(cmp, hypothesis = "b2 = 0.2")
#' 
#' pre <- predictions(mod, newdata = datagrid(hp = 110, mpg = c(30, 35)))
#' hypotheses(pre, hypothesis = "b1 = b2")
#' 
#' # The `FUN` argument can be used to compute standard errors for fitted values
#' mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
#' 
#' f <- function(x) predict(x, type = "link", newdata = mtcars)
#' p <- hypotheses(mod, FUN = f)
#' head(p)
#' 
#' f <- function(x) predict(x, type = "response", newdata = mtcars)
#' p <- hypotheses(mod, FUN = f)
#' head(p)
#' 
#' # Equivalence, non-inferiority, and non-superiority tests
#' mod <- lm(mpg ~ hp + factor(gear), data = mtcars)
#' p <- predictions(mod, newdata = "median")
#' hypotheses(p, equivalence = c(17, 18))
#' 
#' mfx <- avg_slopes(mod, variables = "hp")
#' hypotheses(mfx, equivalence = c(-.1, .1))
#' 
#' cmp <- avg_comparisons(mod, variables = "gear", hypothesis = "pairwise")
#' hypotheses(cmp, equivalence = c(0, 10))
#' 
#' # joint hypotheses: character vector
#' model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
#' hypotheses(model, joint = c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp"))
#' 
#' # joint hypotheses: regular expression
#' hypotheses(model, joint = "cyl")
#' 
#' # joint hypotheses: integer indices
#' hypotheses(model, joint = 2:3)
#' 
#' # joint hypotheses: different null hypotheses
#' hypotheses(model, joint = 2:3, hypothesis = 1)
#' hypotheses(model, joint = 2:3, hypothesis = 1:2)
#' 
#' # joint hypotheses: marginaleffects object
#' cmp <- avg_comparisons(model)
#' hypotheses(cmp, joint = "cyl")
#'
#' @export
hypotheses <- function(
    model,
    hypothesis = NULL,
    vcov = NULL,
    conf_level = 0.95,
    df = Inf,
    equivalence = NULL,
    joint = FALSE,
    joint_test = "f",
    FUN = NULL,
    numderiv = "fdforward",
    ...) {

    if (!isFALSE(joint)) {
        out <- joint_test(model, joint_index = joint, joint_test = joint_test, hypothesis = hypothesis)
        return(out)
    }

    args <- list(
        conf_level = conf_level,
        vcov = vcov,
        df = df,
        equivalence = equivalence)

    # keep this NULL in case `hypothesis` was used in the previous call
    args[["hypothesis"]] <- hypothesis

    dots <- list(...)
    if (length(dots) > 0) {
        args <- c(args, dots)
    }

    xcall <- substitute(model)

    if (is.symbol(xcall)) {
        model <- eval(xcall, envir = parent.frame())

    } else if (is.call(xcall)) {
        internal <- c(
            "predictions", "avg_predictions", "comparisons",
            "avg_comparisons", "slopes", "avg_slopes", "marginal_means")
        # mfx object
        if (as.character(xcall)[[1]] %in% internal) {
            args[["x"]] <- model
            out <- do.call(recall, args)
            if (!is.null(out)) {
                class(out) <- c("hypotheses", class(out))
                return(out)
            }
        # non-mfx object
        } else {
            model <- eval(xcall, envir = parent.frame())
        }
    }

    # marginaleffects objects: recall()
    if (inherits(model, c("predictions", "comparisons", "slopes", "marginalmeans"))) {
        args[["x"]] <- attr(model, "call")
        out <- do.call(recall, args)
        if (!is.null(out)) {
            class(out) <- c("hypotheses", class(out))
            return(out)
        }
    }

    numderiv = sanitize_numderiv(numderiv)

    # after re-evaluation
    tmp <- sanitize_hypothesis(hypothesis, ...)
    hypothesis <- tmp$hypothesis
    hypothesis_null <- tmp$hypothesis_null

    vcov <- get_vcov(model = model, vcov = vcov)
    vcov.type <- get_vcov_label(vcov = vcov)

    if (is.null(FUN)) {
        FUNinner <- function(model, ...) {
            if (inherits(model, c("predictions", "slopes", "comparisons"))) {
                return(model)
            } else if (inherits(model, "data.frame")) { 
                if (!all(c("term", "estimate") %in% colnames(model))) {
                    insight::format_error("The model object is a data.frame but doesn't contain the columns 'term' or 'estimate'. Make sure these columns are present")
                }
                return(model[, c("term", "estimate")])
            } else {
                param <- insight::get_parameters(model, ...)
                colnames(param)[1:2] <- c("term", "estimate")
                return(param)
            }
        }
    } else {
        FUNinner <- FUN
    }

    FUNouter <- function(model, hypothesis) {
        out <- FUNinner(model)

        if (isTRUE(checkmate::check_numeric(out))) {
            out <- data.frame(
                term = seq_along(out),
                estimate = out)
        }

        if (!inherits(out, "data.frame") || any(!c("term", "estimate") %in% colnames(out))) {
            msg <- "`FUN` must return a numeric vector or a data.frame with two columns named `term` and `estimate`."
            insight::format_error(msg)
        }

        tmp <- get_hypothesis(out, hypothesis = hypothesis)
        out <- tmp$estimate
        if (!is.null(attr(tmp, "label"))) {
            attr(out, "label") <- attr(tmp, "label")
        } else {
            attr(out, "label") <- tmp$term
        }
        return(out)
    }

    b <- FUNouter(model = model, hypothesis = hypothesis)

    se <- get_se_delta(
        model = model,
        vcov = vcov,
        hypothesis = hypothesis,
        FUN = FUNouter,
        numderiv = numderiv,
        ...)

    hyplab <- attr(b, "label")
    if (!is.null(hypothesis)) {
        if (is.null(hyplab)) {
            hyplab <- attr(hypothesis, "label")
        }
        if (!is.null(hyplab)) {
            out <- data.frame(
                term = hyplab,
                estimate = b,
                std.error = se)
        } else {
            out <- data.frame(
                term = "custom",
                estimate = b,
                std.error = se)
        }
    } else {
        if (!is.null(hyplab) && length(hyplab) == length(b)) {
            out <- data.frame(
                term = hyplab,
                estimate = b,
                std.error = se)
        } else {
            out <- data.frame(
                term = paste0("b", seq_along(b)),
                estimate = b,
                std.error = se)
        }
    }

    out <- get_ci(
        out,
        conf_level = conf_level,
        vcov = vcov,
        draws = NULL,
        estimate = "estimate",
        null_hypothesis = hypothesis_null,
        df = df,
        model = model,
        ...)

    if (!is.null(equivalence)) {
        out <- equivalence(
            out,
            df = df,
            equivalence = equivalence)
    }

    out <- sort_columns(out)

    class(out) <- c("hypotheses", "deltamethod", class(out))
    attr(out, "model") <- model
    attr(out, "model_type") <- class(model)[1]
    attr(out, "jacobian") <- attr(se, "jacobian")
    attr(out, "call") <- match.call()
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- vcov.type
    attr(out, "conf_level") <- conf_level

    return(out)
}



#' `deltamethod()` is an alias to `hypotheses()`
#' 
#' This alias is kept for backward compatibility.
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
deltamethod <- hypotheses
