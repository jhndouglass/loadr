#' @title Non-linear lest squares Load Apportionment Model (old)
#'
#' @description
#'      Depreciated. Use \code{LAMnlxb}
#'
#' @details
#'      Depreciated. Use \code{LAMnlxb}
#'
#' @export
LAMnls <- function (){
    stop("Function is depreciated. Use LAMnlxb (existing code should work with
          new function)")
}

#' @title Non-linear lest squares Load Apportionment Model
#'
#' @description
#'      \code{LAMnlxb} Returns a LAM object by optimisation across A, B, C and D
#'      paramemeters using \code{nlxb} from the \code{nlmrt} package
#'
#' @details
#'      This is an R implementation of the Load Apportionment Model by Bowes et
#'      al.
#'
#'      The Load Apportionment Model attributes load at flow x between
#'      point and diffuse sources based on the relationship between flow and
#'      concentration. The model is described as
#'      \eqn{Co = A . Q ^ B-1 + C . Q ^ D-1}. Where Co is concentration, Q is
#'      discharge and A, B, C and D are parameters to be estimated empiraically.
#'
#'      This version uses \code{nlxb} to perform the optimisation. For the
#'      \code{optim} version use \code{LAM}. Optimisation is performed over
#'      four parameters; A, B, C and D. B is restircted to be less
#'      than 1. \code{LAMnlxb} attempts to solve the Load Apportionment Model
#'      using user supplied (or default) parameter starting values.
#'
#' @param Q A vector or single element data frame or matrix. Discharge at time
#'      of \code{Co}. Must be same length as Co
#' @param Co A vector or single element data frame or matrix. Concentration at
#'      time of \code{Q}. Must be same length as Q.
#' @param pars A list or vector of length 4. Provides starting values for the
#'      four optimisation parameters in the order A, B, C, D
#'
#' @return The function \code{summary} can be used to obtain and print a
#'      concise object of important outputs. The generic fucntions
#'      \code{fitted.values} and \code{resid} can be used on LAM objects.
#'      An object of class "LAMnl" consisting of the following components.
#'      \itemize{
#'              \item \code{nls.summary.out} standard output from
#'              \code{summary.nls}
#'              \item \code{pars} parameter estimates
#'              \item \code{fitted.values} fitted values
#'              \item \code{residuals} residuals
#'              \item \code{diffuse.point} list of 6 vectors; diffuse and
#'              point source contributions to \code{fitted.values}
#'              \item \code{raw} list of 2 vectors; input Q and Co
#'              \item \code{xover} value of Q where diffuse contributions first
#'              exceed point
#'              \item \code{ps.time} percentage of samples (or time in some
#'              cirumstances) where point source inputs are greater than diffuse
#'               source inputs
#'               \item \code{ps.load} percentage of load derived from point
#'               sources
#'      }
#'
#' @references Bowes MJ, Smith JT, Javie H, Neal C (2008). Modelling of
#'      phosphorus inputs to rivers from diffuse and point sources. Science
#'      of the Total Environment 395. pp125-138.
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMwensumnlxb <- LAMnlxb(Q, Co)
#'
#'      LAMwensumnlxb$pars ## get parameters
#'
#'      ## specift starting values
#'      LAMwensumnlxb2 <- LAMnlxb(Q, Co, pars = c(A= 50, B = 1, C = 300, D = 2))
#'      LAMnlplot(LAMwensumnlxb2)
#'
#' @export
LAMnlxb <- function (Q, Co, pars = c(A = 1, B = 1, C = 1, D = 1)){
    if (length(Q) != length(Co)){stop("vectors of differing length")}
    if (length(pars) != 4){stop("incorrect length parameter set")}
    dat <- list(Q = Q, Co = Co)
    lam.mod <- Co ~ (A * (Q ^ (B - 1))) + (C * (Q ^ (D - 1)))
    nl.out <- nlmrt::nlxb(lam.mod, start = pars,
                                data = dat,
                                lower = c(0, 0, 0, 1),
                                upper = c(Inf, 1, Inf, Inf))
    summ <- summary(nl.out)
    pars <- coef(nl.out)
    if (pars[4] > 0.9 && pars[4] < 1.1){
        warning("Check output. Conc ~ Q may have insufficient gradient")}
    if (pars[1] == 0 || pars[3] == 0) {
        warning("A or C paramter estimated at zero. Check output for
                        sense")}
    fitted <- LAMout(dat, pars)
    xover <- ((pars[[1]] / pars[[3]]) ^
                  (1 / (pars[[4]] - pars[[2]])))
    ps.time <- (length(Q[Q < xover]) / length(Q)) * 100
    ps.load <- (sum(fitted$Fp.load.day)/(sum(fitted$Fp.load.day) +
                                             sum(fitted$Fd.load.day))) * 100
    call <- match.call()
    ll <-  list(nl.summary.out = summ, pars = pars,
                fitted.values = fitted[[1]], residuals = nl.out$resid,
                diffuse.points = fitted[-1], raw = raw <- dat,
                xover = xover, ps.time = ps.time, ps.load = ps.load,
                nl.out = nl.out, call = call)
    class(ll) <- "LAMnl"
    return(ll)
}

#' @title Non-linear lest squares Load Apportionment Model
#'
#' @description
#'      \code{LAMnlxb} Returns a LAM object by optimisation across A, C and D
#'      paramemeters using \code{nlxb} from the \code{nlmrt} package. In this
#'      version the B parameter is fixed at zero.
#'
#' @details
#'      This is an R implementation of the Load Apportionment Model by Bowes et
#'      al.
#'
#'      The Load Apportionment Model attributes load at flow x between
#'      point and diffuse sources based on the relationship between flow and
#'      concentration. The model is described as
#'      \eqn{Co = A . Q ^ B-1 + C . Q ^ D-1}. Where Co is concentration, Q is
#'      discharge and A, C and D are parameters to be estimated empiraically. B
#'      is fixed at zero.
#'
#'      This version uses \code{nlxb} to perform the optimisation. For the
#'      \code{optim} version use \code{LAM}. Optimisation is performed over
#'      four parameters; A, B, C and D. B is fixed at zero.
#'      \code{LAMnlxb} attempts to solve the Load Apportionment Model
#'      using user supplied (or default) parameter starting values.
#'
#' @param Q A vector or single element data frame or matrix. Discharge at time
#'      of \code{Co}. Must be same length as Co
#' @param Co A vector or single element data frame or matrix. Concentration at
#'      time of \code{Q}. Must be same length as Q.
#' @param pars A list or vector of length 3. Provides starting values for the
#'      four optimisation parameters in the order A, C, D
#'
#' @return The function \code{summary} can be used to obtain and print a
#'      concise object of important outputs. The generic fucntions
#'      \code{fitted.values} and \code{resid} can be used on LAM objects.
#'      An object of class "LAMnl" consisting of the following components.
#'      \itemize{
#'              \item \code{nls.summary.out} standard output from
#'              \code{summary.nls}
#'              \item \code{pars} parameter estimates
#'              \item \code{fitted.values} fitted values
#'              \item \code{residuals} residuals
#'              \item \code{diffuse.point} list of 6 vectors; diffuse and
#'              point source contributions to \code{fitted.values}
#'              \item \code{raw} list of 2 vectors; input Q and Co
#'              \item \code{xover} value of Q where diffuse contributions first
#'              exceed point
#'              \item \code{ps.time} percentage of samples (or time in some
#'              cirumstances) where point source inputs are greater than diffuse
#'               source inputs
#'               \item \code{ps.load} percentage of load derived from point
#'               sources
#'      }
#'
#' @references Bowes MJ, Smith JT, Javie H, Neal C (2008). Modelling of
#'      phosphorus inputs to rivers from diffuse and point sources. Science
#'      of the Total Environment 395. pp125-138.
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMwensumnlxb0 <- LAMnlxb0(Q, Co)
#'
#'      LAMwensumnlxb0$pars ## get parameters
#'
#'      ## specift starting values
#'      LAMwensumnlxb02 <- LAMnlxb0(Q, Co, pars = c(A= 50, C = 300, D = 2))
#'      LAMnlplot(LAMwensumnlxb02)
#'
#' @export
LAMnlxb0 <- function (Q, Co, pars = c(A = 1, C = 1, D = 1)){
    if (length(Q) != length(Co)){stop("vectors of differing length")}
    if (length(pars) != 3){stop("incorrect length parameter set")}
    dat <- list(Q = Q, Co = Co)
    lam.mod <- Co ~ (A * (Q ^ (0 - 1))) + (C * (Q ^ (D - 1)))
    nl.out <- nlmrt::nlxb(lam.mod, start = pars,
                          data = dat,
                          lower = c(0, 0, 1),
                          upper = c(Inf, Inf, Inf))
    summ <- summary(nl.out)
    pars <- coef(nl.out)
    if (pars[3] > 0.9 && pars[3] < 1.1){
        warning("Check output. Conc ~ Q may have insufficient gradient")}
    if (pars[1] == 0 || pars[2] == 0) {
        warning("A or C paramter estimated at zero. Check output for
                        sense")}
    fitted <- LAMout0(dat, pars)
    xover <- ((pars[[1]] / pars[[2]]) ^
                  (1 / (pars[[3]] - 0)))
    ps.time <- (length(Q[Q < xover]) / length(Q)) * 100
    ps.load <- (sum(fitted$Fp.load.day)/(sum(fitted$Fp.load.day) +
                                             sum(fitted$Fd.load.day))) * 100
    call <- match.call()
    ll <-  list(nl.summary.out = summ, pars = pars,
                fitted.values = fitted[[1]], residuals = nl.out$resid,
                diffuse.points = fitted[-1], raw = raw <- dat,
                xover = xover, ps.time = ps.time, ps.load = ps.load,
                nl.out = nl.out, call = call)
    class(ll) <- "LAMnl"
    return(ll)
}

#' @title Summary of "LAMnl" objects
#'
#' @description
#'      \code{summary} method for class "LAMnl"
#'
#' @details
#'      Currently has no associated \code{print} method
#'
#'
#' @param object an object of class "LAMnl"
#' @param ... further arguments passed to and from other methods
#'
#' @return
#'      \itemize{
#'              \item \code{overview} output from \code{summary.nl}
#'              \item \code{pars} data frame of parameter estiamtes
#'              \item \code{SumofSq} Sum of Squared errors
#'              \item \code{CrossOverFlow} value of Q where diffuse
#'              contributions firstmexceed point
#'              \item \code{PointSourceDominatedPercentage} percentage of
#'              samples (or time in some cirumstances) where point source inputs
#'              are greater than diffuse source inputs
#'              \item \code{ps.load} percentage of load derived from point
#'               sources
#'      }
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMnleg <- LAMnlxb(Q, Co)
#'
#'      foo <- summary(LAMnleg)
#'      foo
#'      foo$pars ## get parameters
#'
#' @export
summary.LAMnl <- function(object, ...){
        output <- list()
        output$call <- object$call
        output$overview <- object$nl.out
        output$pars <- object$pars
        output$SumofSq <- sum(object$residuals^2)
        output$CrossOverFlow <- object$xover
        output$PointSourceDominatedPercentage <- object$ps.time
        output$PointSourceLoadPercentage <- object$ps.load
        output
}

#' @title Print "LAMnl" objects
#'
#' @description
#'      \code{print} method for class "LAMnl"
#'
#' @param x an object of class "LAMnl"
#' @param ... further arguments passed to and from other methods
#'
#' @export
print.LAMnl <- function(x, ...){
        cat("Call:\n")
        print(x$call)
        cat("Parameters:\n")
        print(x$pars)
}