process_imputation <- function(x, call_attr, marginal_means = FALSE) {
    insight::check_if_installed("mice")
    mfx_list <- list()
    for (i in seq_along(x$analyses)) {
        calltmp <- call_attr
        calltmp[["model"]] <- x$analyses[[i]]

        # not sure why but this breaks marginal_means on "modeldata specified twice"
        if (isFALSE(marginal_means)) {
            calltmp[["modeldata"]] <- get_modeldata( x$analyses[[i]], additional_variables = FALSE)
        }

        mfx_list[[i]] <- evalup(calltmp)
        if (i == 1) {
            out <- mfx_list[[1]]
        }
        mfx_list[[i]]$term <- seq_len(nrow(mfx_list[[i]]))
        class(mfx_list[[i]]) <- c("marginaleffects_mids", class(mfx_list[[i]]))
    }
    mipool <- mice::pool(mfx_list)
    for (col in c("estimate", "statistic", "p.value", "conf.low", "conf.high")) {
        if (col %in% colnames(out) && col %in% colnames(mipool$pooled)) {
            out[[col]] <- mipool$pooled[[col]]
        } else {
            out[[col]] <- NULL
        }
    }
    if ("df" %in% colnames(mipool$pooled)) {
        out$df <- mipool$pooled$df
    }
    out$std.error <- sqrt(mipool$pooled$t)
    out <- get_ci(
        out,
        vcov = call_attr[["vcov"]],
        conf_level = call_attr[["conf_level"]],
        df = mipool$pooled$df)
    attr(out, "inferences") <- mipool
    attr(out, "model") <- mice::pool(lapply(mfx_list, attr, "model"))
    return(out)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.marginaleffects_mids <- function(x, ...) {
    if (!"std.error" %in% colnames(x)) {
        insight::format_error('The output of `marginal_means` does not include a `std.error` column. Some models do not generate standard errors when estimates are backtransformed (e.g., GLM models). One solution is to use `type="response"` for those models.')
    }
    out <- as.data.frame(x[, c("estimate", "std.error")])
    out$term <- seq_len(nrow(out))
    return(out)
}


#' glance helper
#' 
#' @noRd
#' @export
glance.marginaleffects_mids <- function(x, ...) {
    data.frame()
}
