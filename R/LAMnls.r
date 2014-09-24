#' @title Non-linear lest squares Load Apportionment Model
#'
#' @description
#'      \code{LAMnls} Returns a LAM object by optimisation across A, B, C and D
#'      paramemeters using \code{nls}
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
#'      This version uses \code{nls} to perform the optimisation. For the
#'      \code{optim} version use \code{LAM}. Optimisation is performed over
#'      four parameters; A, B, C and D. By default B is restircted to be 1.
#'      \code{LAMnls} attempts to solve the Load Apportionment Model using user
#'      supplied (or default) parameter starting values. If no solution can be
#'      found the equation is solved using \code{optim}. The output parameters
#'      are then used as the stating values for \code{nls}. Two methods for
#'      \code{nls} can be used. "port" allows for upper and lower parameter
#'      values to be set, "plinear" does not. More methods will be added later.
#'
#' @param Q A vector or single element data frame or matrix. Discharge at time
#'      of \code{Co}. Must be same length as Co
#' @param Co A vector or single element data frame or matrix. Concentration at
#'      time of \code{Q}. Must be same length as Q.
#' @param pars A list or vector of length 4. Provides starting values for the
#'      four optimisation parameters in the order A, B, C, D
#' @param b.upper Numeric. Upper limit of B.
#' @param method Character string. "port" or "plinear" See \code{?nls} for more
#'      info on each
#'
#' @return The function \code{summary} can be used to obtain and print a
#'      concise object of important outputs. The generic fucntions
#'      \code{fitted.values} and \code{resid} can be used on LAM objects.
#'      An object of class "LAM" consisting of the following components.
#'      \itemize{
#'              \item \code{nls.summary.out} standard output from
#'              \code{summary.nls}
#'              \item \code{pars} parameter estimates
#'              \item \code{fitted.values} fitted values
#'              \item \code{residuals} residuals
#'              \item \code{diffuse.point} list of 2 vectors; diffuse and
#'              point source contributions to \code{fitted.values}
#'              \item \code{raw} list of 2 vectors; input Q and Co
#'              \item \code{xover} value of Q where diffuse contributions first
#'              exceed point
#'              \item \code{ps.time} percentage of samples (or time in some
#'              cirumstances) where point source inputs are greater than diffuse
#'               source inputs
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
#'      LAMwensumnls <- LAMnls(Q, Co)
#'
#'      LAMwensumnls$pars ## get parameters
#'
#'      ## specift starting values
#'      LAMwensumnls2 <- LAMnls(Q, Co, pars = c(A= 50, B = 1, C = 300, D = 0))
#'      LAMnlsplot(LAMwensumnls2)
#'
#' @export
LAMnls <- function (Q, Co, pars = c(A = 1, B = 1, C = 1, D = 1), b.upper = 1,
                    method = "port"){
        if (length(Q) != length(Co)){stop("vectors of differing length")}
        if (length(pars) != 4){stop("incorrect length parameter set")}
        dat <- list(Q = Q, Co = Co)
        dat.optim <- data.frame(Q = Q, Co = Co)
        lam.mod <- Co ~ (A * (Q ^ (B - 1))) + (C * (Q ^ (D - 1)))
        if (method == "port") {
                type <- "nls-port"
                nls.out <- tryCatch(nls(lam.mod, start = pars,
                                        data = dat,
                                        lower = c(0, 0, 0, 1),
                                        upper = c(Inf, 1, Inf, Inf), algorithm = "port",
                                        control  = list(maxiter = 10000)), error = function(e){
                                                "convergeissue"})
        }
        if (method == "plinear"){
                type <- "nls-plinear"
                nls.out <- tryCatch(nls(lam.mod, start = pars,
                                        data = dat,
                                        algorithm = "plinear",
                                        control  = list(maxiter = 10000)), error = function(e){
                                                "convergeissue"})
        }
        if (nls.out == "convergeissue"){
                optim.out <- optim(pars, fn = LAMfit, data = dat.optim, method = "L-BFGS-B",
                                   lower = c(0, 0, 0, 1), upper = c(Inf, b.upper, Inf, Inf),
                                   control = list(maxit = 1000))
                pars2 <- optim.out[[1]]
                if (pars2[1] == 0 || pars2[3] == 0) {
                        warning("A or C paramter estimated at zero. Location: optim")}
                if (method == "port") {
                        type <- "nls-optim-port"
                        nls.out <- nls(lam.mod, start = pars2,
                                       data = dat,
                                       lower = c(0, 0, 0, 1),
                                       upper = c(Inf, 1, Inf, Inf), algorithm = "port",
                                       control  = list(maxiter = 10000))
                }
                if (method == "plinear"){
                        type <- "nls-optim-plinear"
                        nls.out <- nls(lam.mod, start = pars2,
                                       data = dat,
                                       algorithm = "plinear",
                                       control  = list(maxiter = 10000))
                }
        }
        summ <- summary(nls.out)
        pars3 <- coef(summ)
        if ((pars3[2] > 0.9 && pars3[2] < 1.1) ||
                    (pars3[4] > 0.9 && pars3[4] < 1.1)){
                warning("Check output. Conc ~ Q may have insufficient gradient")
        }
        if (pars3[1] == 0 || pars3[3] == 0) {
                warning("A or C paramter estimated at zero. Location: nls")}
        fitted <- LAMout(dat, pars3)
        xover <- ((pars3[[1]] / pars3[[3]]) ^
                          (1 / (pars3[[4]] - pars3[[2]])))
        ps.time <- (length(Q[Q < xover]) / length(Q)) * 100
        call <- match.call()
        ll <-  list(nls.summary.out = summ, pars = pars3,
                    fitted.values = fitted[[1]], residuals = resid(summ),
                    diffuse.points = fitted[-1], raw = raw <- dat,
                    xover = xover, ps.time = ps.time, type = type,
                    nls.out = nls.out, call = call)
        class(ll) <- "LAMnls"
        return(ll)
}

#' @title Summary of "LAMnls" objects
#'
#' @description
#'      \code{summary} method for class "LAMnls"
#'
#' @details
#'      Currently has no associated \code{print} method
#'
#'
#' @param object an object of class "LAMnls"
#' @param ... further arguments passed to and from other methods
#'
#' @return
#'      \itemize{
#'              \item \code{overview} output from \code{summary.nls}
#'              \item \code{pars} data frame of parameter estiamtes
#'              \item \code{SumofSq} Sum of Squared errors
#'              \item \code{CrossOverFlow} value of Q where diffuse
#'              contributions firstmexceed point
#'              \item \code{PointSourceDominatedPercentage} percentage of
#'              samples (or time in some cirumstances) where point source inputs
#'              are greater than diffuse source inputs
#'      }
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMnlseg <- LAMnls(Q, Co)
#'
#'      foo <- summary(LAMnlseg)
#'      foo
#'      foo$pars ## get parameters
#'
#' @export
summary.LAMnls <- function(object, ...){
        output <- list()
        output$call <- object$call
        output$overview <- object[[1]]
        output$pars <- object$pars
        output$SumofSq <- sum(object$residuals^2)
        output$CrossOverFlow <- object$xover
        output$PointSourceDominatedPercentage <- object$ps.time
        output
}

#' @title Print "LAMnls" objects
#'
#' @description
#'      \code{print} method for class "LAMnls"
#'
#' @param x an object of class "LAMnls"
#' @param ... further arguments passed to and from other methods
#'
#' @export
print.LAMnls <- function(x, ...){
        cat("Call:\n")
        print(x$call)
        cat("Parameters:\n")
        print(x$pars)
}