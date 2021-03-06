% Generated by roxygen2 (4.0.2): do not edit by hand
\name{LAMnlxb}
\alias{LAMnlxb}
\title{Non-linear lest squares Load Apportionment Model}
\usage{
LAMnlxb(Q, Co, pars = c(A = 1, B = 1, C = 1, D = 1))
}
\arguments{
\item{Q}{A vector or single element data frame or matrix. Discharge at time
of \code{Co}. Must be same length as Co}

\item{Co}{A vector or single element data frame or matrix. Concentration at
time of \code{Q}. Must be same length as Q.}

\item{pars}{A list or vector of length 4. Provides starting values for the
     four optimisation parameters in the order A, B, C, D}
}
\value{
The function \code{summary} can be used to obtain and print a
     concise object of important outputs. The generic fucntions
     \code{fitted.values} and \code{resid} can be used on LAM objects.
     An object of class "LAMnl" consisting of the following components.
     \itemize{
             \item \code{nls.summary.out} standard output from
             \code{summary.nls}
             \item \code{pars} parameter estimates
             \item \code{fitted.values} fitted values
             \item \code{residuals} residuals
             \item \code{diffuse.point} list of 6 vectors; diffuse and
             point source contributions to \code{fitted.values}
             \item \code{raw} list of 2 vectors; input Q and Co
             \item \code{xover} value of Q where diffuse contributions first
             exceed point
             \item \code{ps.time} percentage of samples (or time in some
             cirumstances) where point source inputs are greater than diffuse
              source inputs
              \item \code{ps.load} percentage of load derived from point
              sources
     }
}
\description{
\code{LAMnlxb} Returns a LAM object by optimisation across A, B, C and D
     paramemeters using \code{nlxb} from the \code{nlmrt} package
}
\details{
This is an R implementation of the Load Apportionment Model by Bowes et
     al.

     The Load Apportionment Model attributes load at flow x between
     point and diffuse sources based on the relationship between flow and
     concentration. The model is described as
     \eqn{Co = A . Q ^ B-1 + C . Q ^ D-1}. Where Co is concentration, Q is
     discharge and A, B, C and D are parameters to be estimated empiraically.

     This version uses \code{nlxb} to perform the optimisation. For the
     \code{optim} version use \code{LAM}. Optimisation is performed over
     four parameters; A, B, C and D. B is restircted to be less
     than 1. \code{LAMnlxb} attempts to solve the Load Apportionment Model
     using user supplied (or default) parameter starting values.
}
\examples{
data(wensum)
     Q <- wensum$Q
     Co <- wensum$TP

     LAMwensumnlxb <- LAMnlxb(Q, Co)

     LAMwensumnlxb$pars ## get parameters

     ## specift starting values
     LAMwensumnlxb2 <- LAMnlxb(Q, Co, pars = c(A= 50, B = 1, C = 300, D = 2))
     LAMnlplot(LAMwensumnlxb2)
}
\references{
Bowes MJ, Smith JT, Javie H, Neal C (2008). Modelling of
     phosphorus inputs to rivers from diffuse and point sources. Science
     of the Total Environment 395. pp125-138.
}

