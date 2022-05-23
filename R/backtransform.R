backtransform <- function(x, transform_post) {
    checkmate::assert_data_frame(x)
    checkmate::assert_function(transform_post)
    original <- list()
    cols <- intersect(colnames(x), c("comparison", "marginalmean", "predicted", "estimate", "conf.low", "conf.high"))
    for (col in cols) {
        original[[col]] <- x[[col]]
        x[[col]] <- transform_post(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        original[[col]] <- original[[col]]
        x[[col]] <- NULL
    }
    if (length(original) > 0) {
        attr(x, "transform_post_original") <- original
    }
    return(x)
}
