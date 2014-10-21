#' @title Produce fitted LAM values
#'
#' @description
#'      Used internally by \code{LAM} and \code{LAMnls} to produce fitted values
#'      and diffuse and point soruce contributions at each point.
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
#'              \item \code{Fp} point source contribution to \code{Fpred}
#'              \item \code{Fd} diffuse source contribution to \code{Fpred}
#'              \item \code{Fp.load} instantaneous point source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fd.load} instantaneous diffuse source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fp.load.day} Daily point source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fd.load.day} Daily diffuse source load. Units
#'              are arbitary and dependent on input units
#'      }
#' @export

LAMout <- function (data, pars){
        Q <- data$Q
        Fpred <- pars[1] * (Q ^ (pars[2] -1)) + pars[3] * (Q ^ (pars[4] - 1))
        Fd <- pars[3] * (Q ^ (pars[4] -1))
        Fp <- pars[1] * (Q ^ (pars[2] -1))

        Fd.load <- Fd * Q
        Fp.load <- Fp * Q
        Fd.load.day <- (((Fd.load * 60) * 60) * 24)
        Fp.load.day <- (((Fp.load * 60) * 60) * 24)

        return(list(Fpred = Fpred, Fd = Fd, Fp = Fp, Fp.load = Fp.load,
                    Fd.load = Fd.load, Fp.load.day = Fp.load.day,
                    Fd.load.day = Fd.load.day))
}

#' @title Produce fitted LAM0 values
#'
#' @description
#'      Used internally by \code{LAM0} to produce fitted values
#'      and diffuse and point soruce contributions at each point.
#'
#' @details
#'      Shouldn't be used directly.
#'
#' @param data data.frame of Q and Co values input to \code{LAM0}
#' @param pars estiamted parameters
#'
#' @return
#'      \itemize{
#'              \item \code{Fpred} fitted values
#'              \item \code{Fp} point source contribution to \code{Fpred}
#'              \item \code{Fd} diffuse source contribution to \code{Fpred}
#'              \item \code{Fp.load} instantaneous point source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fd.load} instantaneous diffuse source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fp.load.day} Daily point source load. Units
#'              are arbitary and dependent on input units
#'              \item \code{Fd.load.day} Daily diffuse source load. Units
#'              are arbitary and dependent on input units
#'      }
#' @export

LAMout0 <- function (data, pars){
    Q <- data$Q
    Fpred <- pars[1] * (Q ^ (0 - 1)) + pars[2] * (Q ^ (pars[3] - 1))
    Fd <- pars[2] * (Q ^ (pars[3] -1))
    Fp <- pars[1] * (Q ^ (0 -1))

    Fd.load <- Fd * Q
    Fp.load <- Fp * Q
    Fd.load.day <- (((Fd.load * 60) * 60) * 24)
    Fp.load.day <- (((Fp.load * 60) * 60) * 24)

    return(list(Fpred = Fpred, Fd = Fd, Fp = Fp, Fp.load = Fp.load,
                Fd.load = Fd.load, Fp.load.day = Fp.load.day,
                Fd.load.day = Fd.load.day))
}