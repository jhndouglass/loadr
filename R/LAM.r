#' @title Load Apportionment Model
#'
#' @description
#'      \code{LAM} Returns a LAM object by optimisation across A, B, C and D
#'      paramemeters
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
#'      This version uses \code{optim} to perform the optimisation. For the
#'      \code{nls} version use \code{LAMnls}. Optimisation is performed over
#'      four parameters; A, B, C and D. By default B is restircted to be 1.
#'
#' @param Q A vector or single element data frame or matrix. Discharge at time
#'      of \code{Co}. Must be same length as Co
#' @param Co A vector or single element data frame or matrix. Concentration at
#'      time of \code{Q}. Must be same length as Q.
#' @param pars A list or vector of length 4. Provides starting values for the
#'      four optimisation parameters in the order A, B, C, D
#' @param b.upper Numeric. Upper limit of B.
#'
#' @return The function \code{summary} can be used to obtain and print a
#'      concise object of important outputs. The generic fucntions
#'      \code{fitted.values} and \code{resid} can be used on LAM objects.
#'      An object of class "LAM" consisting of the following components.
#'      \itemize{
#'              \item \code{optim.out} tandard output from \code{optim}
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
#'@references Bowes MJ, Smith JT, Javie H, Neal C (2008). Modelling of
#'      phosphorus inputs to rivers from diffuse and point sources. Science
#'      of the Total Environment 395. pp125-138.
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMwensum <- LAM(Q, Co)
#'
#'      LAMwensum$pars ## get parameters
#'
#'      ## specift starting values
#'      LAMwensum2 <- LAM(Q, Co, pars = c(50, 1, 300, 0))
#'      LAMplot(LAMwensum2)
#' @export
LAM <- function (Q, Co, pars = c(A = 1, B = 1, C = 1, D = 1), b.upper = 1){
        if (length(Q) != length(Co)){stop("vectors of differing length")}
        if (length(pars) != 4){stop("incorrect length parameter set")}
        dat <- data.frame(Q = Q, Co = Co)
        optim.out <- optim(pars, fn = LAMfit, data = dat, method = "L-BFGS-B",
                           lower = c(0, 0, 0, 1), upper = c(Inf, b.upper, Inf, Inf),
                           control = list(maxit = 1000))
        pars2 <- optim.out[[1]]
        if ((pars2[2] > 0.9 && pars2[2] < 1.1) &&
                    (pars2[4] > 0.9 && pars2[4] < 1.1)){
                warning("Check output. Conc ~ Q may have insufficient gradient")}
        if (pars2[1] == 0 || pars2[3] == 0) {
                stop("A or B paramter estimated at zero.")}
        fitted <- LAMout(dat, optim.out[[1]])
        xover <- ((optim.out[[1]][[1]] / optim.out[[1]][[3]]) ^
                          (1 / (optim.out[[1]][[4]] - optim.out[[1]][[2]])))
        ps.time <- (length(Q[Q < xover]) / length(Q)) * 100
        residuals <- dat$Co - fitted$Fpred
        call <- match.call()
        ll <-  list(optim.out = optim.out, pars = optim.out[[1]],
                    fitted.values = fitted[[1]], residuals = residuals,
                    diffuse.point = fitted[-1], raw = raw <- dat,
                    xover = xover, ps.time = ps.time, call = call)
        class(ll) <- "LAM"
        return(ll)
}

#' @title Summary of "LAM" objects
#'
#' @description
#'      \code{summary} method for class "LAM"
#'
#' @details
#'      Currently has no associated \code{print} method
#'
#'
#' @param object an object of class "LAM"
#' @param ... further arguments passed to and from other methods
#'
#' @return
#'      \itemize{
#'              \item \code{pars} data frame of parameter estiamtes
#'              \item \code{SumofSq} Sum of Squared errors
#'              \item \code{CrossOverFlow} value of Q where diffuse
#'              contributions firstmexceed point
#'              \item \code{PointSourceDominatedPercentage} percentage of
#'              samples (or time in some cirumstances) where point source inputs
#'              are greater than diffuse source inputs
#'      }
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMeg <- LAM(Q, Co)
#'
#'      foo <- summary(LAMeg)
#'      foo
#'      foo$pars ## get parameters
#'
#' @export
summary.LAM <- function(object, ...){
        output <- list()
        output$call <- object$call
        output$pars <- data.frame(object$pars)
        output$SumofSq <- object[[1]][[2]]
        output$CrossOverFlow <- object$xover
        output$PointSourceDominatedPercentage <- object$ps.time
        output
}

#' @title Print "LAM" objects
#'
#' @description
#'      \code{print} method for class "LAM"
#'
#' @param x an object of class "LAM"
#' @param ... further arguments passed to and from other methods
#'
#' @export
print.LAM <- function(x, ...){
        cat("Call:\n")
        print(x$call)
        cat("Parameters:\n")
        print(x[[1]][[1]])
}