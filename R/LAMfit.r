#' @title Function to be minimised within \code{LAM}
#'
#' @description
#'      Used internally by \code{LAM}
#'
#' @details
#'      Shouldn't be used directly.
#'
#' @param data data.frame of Q and Co values input to \code{LAM} or
#'      \code{LAMnls}
#' @param pars starting parameters
#'
#' @export

LAMfit <- function (data, pars){
        with(data, sum((Co - (pars[1] * (Q ^ (pars[2] -1)) + pars[3] *
                                      (Q ^ (pars[4] - 1)))) ^ 2))
}