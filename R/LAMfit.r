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

#' @title Function to be minimised within \code{LAM0}
#'
#' @description
#'      Used internally by \code{LAM0}
#'
#' @details
#'      Shouldn't be used directly.
#'
#' @param data data.frame of Q and Co values input to \code{LAM} or
#'      \code{LAMnls}
#' @param pars starting parameters
#'
#' @export

LAMfit0 <- function (data, pars){
    with(data, sum((Co - (pars[1] * (Q ^ (0 - 1)) + pars[2] *
                              (Q ^ (pars[3] - 1)))) ^ 2))
}