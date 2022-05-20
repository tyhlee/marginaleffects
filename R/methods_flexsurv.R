#' @rdname get_predict
#' @export
get_predict.flexsurvreg <- function(model,
                                    newdata = insight::get_data(model),
                                    vcov = FALSE,
                                    conf_level = 0.95,
                                    type = "response",
                                    ...) {


    out <- stats::predict(
        model,
        type = type,
        newdata = newdata,
        ...)
    out <- data.frame(
        rowid = seq_len(nrow(out)),
        predicted = out[[".pred"]])
    return(out)
}
                        
       
