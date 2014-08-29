#' @title Produce fitted LAM values
#'
#' @description
#'      Used internally by \code{LAM} and \code{LAMnls} to produce fitted values
#'      and diffuse and ponit soruce contributions at each point.
#'
#' @details
#'      Shouldn't be used directly.
#'
#' @param data data.frame of Q and Co values input to \code{LAM} or
#'      \code{LAMnls}
#' @param pars estiamted parameters
#'
#' @return
#'      \itemize{
#'              \item \code{Fpred} fitted values
#'              \item \code{Fd} diffuse source contribution to \code{Fpred}
#'      }
#' @export

LAMout <- function (data, pars){
        Q <- data$Q
        Fpred <- pars[1] * (Q ^ (pars[2] -1)) + pars[3] * (Q ^ (pars[4] - 1))
        Fd <- pars[3] * (Q ^ (pars[4] -1))
        Fp <- pars[1] * (Q ^ (pars[2] -1))

        return(list(Fpred = Fpred, Fd = Fd, Fp = Fp))
}