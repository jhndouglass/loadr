#' @title Plot "LAM" object
#'
#' @description
#'      Plots a four panel graphic summairsing Load Apportionment Model fits.
#'
#' @details
#'      \code{y} is included for consistency with generic \code{print} but is
#'      not used
#'
#' @param x a "LAM" object
#' @param y is ignored
#' @param ... further arguments passed to and from other methods
#'
#' @examples
#'      data(wensum)
#'      Q <- wensum$Q
#'      Co <- wensum$TP
#'
#'      LAMwensum <- LAM(Q, Co)
#'
#'      LAMplot(LAMwensum)
#'
#' @export
LAMplot <- function(x, y, ...){
        foo <- data.frame(Fpred = fitted(x), x$diffuse.point, x$raw)
        foo <- foo[order(foo$Q), ]
        linewidth <- 1
        pointsize <- 1.5
        titlesize <- 1
        pointalpha <- 0.5
        axissize <- 0.9
        plottext <- 3
        # linear regression
        gg <- ggplot(data = foo, aes(Q, Co)) + stat_smooth(method = lm,
                                                           colour = "goldenrod1",
                                                           fill = "goldenrod2",
                                                           size = rel(linewidth), alpha = 0.3)
        gg <- gg + geom_point(size = rel(pointsize), alpha = pointalpha)
        gg <- gg + labs(title = "Linear model", x = "Q", y = "Conc.")
        gg <- gg + theme(plot.title = element_text(size = rel(titlesize), face = "bold"),
                         axis.title.x = element_text(face = "bold", size = rel(axissize)),
                         axis.title.y = element_text(face = "bold", size = rel(axissize)),
                         panel.background = element_rect(fill = NA),
                         panel.grid.minor = element_line(colour = NA),
                         axis.line = element_line(colour = "black"))

        # lam
        gg2 <- ggplot(data = foo, aes(Q, Co))
        gg2 <- gg2 + geom_path(aes(Q, Fpred), size = linewidth, colour = "seagreen")
        gg2 <- gg2 + geom_point(size = rel(pointsize),
                                alpha = pointalpha)
        gg2 <- gg2 + labs(title = "LAM", x = "Q", y = "Conc.")
        gg2 <- gg2 + theme(plot.title = element_text(size = rel(titlesize), face = "bold"),
                           axis.title.x = element_text(face = "bold", size = rel(axissize)),
                           axis.title.y = element_text(face = "bold", size = rel(axissize)),
                           panel.background = element_rect(fill = NA),
                           panel.grid.minor = element_line(colour = NA),
                           axis.line = element_line(colour = "black"))

        # components
        gg3 <- ggplot(data = foo, aes(Q, Co))
        gg3 <- gg3 + geom_path(aes(Q, Fp), size = linewidth, colour = "firebrick4")
        gg3 <- gg3 + geom_path(aes(Q, Fd), size = linewidth, colour = "steelblue3")
        gg3 <- gg3 + geom_path(aes(Q, Fpred), size = linewidth, colour = "seagreen")
        gg3 <- gg3 + geom_point(size = rel(pointsize),
                                alpha = pointalpha)
        gg3 <- gg3 + labs(title = "LAM - Components", x = "Q", y = "Conc.")
        gg3 <- gg3 + theme(plot.title = element_text(size = rel(titlesize), face = "bold"),
                           axis.title.x = element_text(face = "bold", size = rel(axissize)),
                           axis.title.y = element_text(face = "bold", size = rel(axissize)),
                           panel.background = element_rect(fill = NA),
                           panel.grid.minor = element_line(colour = NA),
                           axis.line = element_line(colour = "black"))

        lm1 <- lm(Co ~ Q, data = foo)

        gg4 <- ggplot()
        gg4 <- gg4 + annotate("text", x = 0.3, y = 1.1,
                              label = paste("Linear model SS = ",
                                            round(sum(lm1$residuals ^ 2), 2)))
        gg4 <- gg4 + annotate("text", x = 0.3, y = 1,
                              label = paste("LAM SS = ", round(summary(x)$SumofSq, 2)))
        gg4 <- gg4 + annotate("text", x = 0.3, y = 0.9,
                              label = paste("Q xover point = ", round(x$xover, 2),
                                            " flow units"))
        gg4 <- gg4 + annotate("text", x = 0.3, y = 0.8,
                              label = paste("% samples PS dominated = ",
                                            round(x$ps.time, 2)))
        gg4 <- gg4 + annotate("text", x = 0.3, y = 0.7, label = "LAM fit",
                              col = "seagreen", face = "bold")
        gg4 <- gg4 + annotate("text", x = 0.3, y = 0.6, label = "LAM diffuse fit",
                              col = "steelblue3", face = "bold")
        gg4 <- gg4 + annotate("text", x = 0.3, y = 0.5, label = "LAM point fit",
                              col = "firebrick4", face = "bold")
        gg4 <- gg4 + theme(plot.title = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           panel.background = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.line = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank())

        grid.arrange(gg, gg2, gg3, gg4)
}