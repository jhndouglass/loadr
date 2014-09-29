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
#' @param unit unit of flow as a text string. Used in labelling of plots.
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
LAMplot <- function(x, unit = "Ml/d", ...){
        foo <- data.frame(Fpred = fitted(x), x$diffuse.point, x$raw)
        foo <- foo[order(foo$Q), ]
        linewidth <- 0.8
        pointsize <- 1
        titlesize <- 1
        pointalpha <- 0.5
        axissize <- 0.8
        plottext <- 3
        # linear regression
        gg <- ggplot2::ggplot(data = foo, ggplot2::aes(Q, Co)) + ggplot2::stat_smooth(method = lm,
                                                           colour = "goldenrod1",
                                                           fill = "goldenrod2",
                                                           size = ggplot2::rel(linewidth), alpha = 0.3)
        gg <- gg + ggplot2::geom_point(size = ggplot2::rel(pointsize), alpha = pointalpha)
        gg <- gg + ggplot2::labs(title = "Linear model", x = "Q", y = "Conc.")
        gg <- gg + ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(titlesize)),
                         axis.title.x = ggplot2::element_text(size = ggplot2::rel(axissize)),
                         axis.title.y = ggplot2::element_text(size = ggplot2::rel(axissize)),
                         panel.background = ggplot2::element_rect(fill = NA),
                         panel.grid.minor = ggplot2::element_line(colour = NA),
                         axis.line = ggplot2::element_line(colour = "black"))

        # lam
        gg2 <- ggplot2::ggplot(data = foo, ggplot2::aes(Q, Co))
        gg2 <- gg2 + ggplot2::geom_path(ggplot2::aes(Q, Fpred), size = linewidth, colour = "seagreen")
        gg2 <- gg2 + ggplot2::geom_point(size = ggplot2::rel(pointsize),
                                alpha = pointalpha)
        gg2 <- gg2 + ggplot2::labs(title = "LAM", x = "Q", y = "Conc.")
        gg2 <- gg2 + ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(titlesize)),
                           axis.title.x = ggplot2::element_text(size = ggplot2::rel(axissize)),
                           axis.title.y = ggplot2::element_text(size = ggplot2::rel(axissize)),
                           panel.background = ggplot2::element_rect(fill = NA),
                           panel.grid.minor = ggplot2::element_line(colour = NA),
                           axis.line = ggplot2::element_line(colour = "black"))

        # components
        gg3 <- ggplot2::ggplot(data = foo, ggplot2::aes(Q, Co))
        gg3 <- gg3 + ggplot2::geom_path(ggplot2::aes(Q, Fp), size = linewidth, colour = "firebrick4")
        gg3 <- gg3 + ggplot2::geom_path(ggplot2::aes(Q, Fd), size = linewidth, colour = "steelblue3")
        gg3 <- gg3 + ggplot2::geom_path(ggplot2::aes(Q, Fpred), size = linewidth, colour = "seagreen")
        gg3 <- gg3 + ggplot2::geom_point(size = ggplot2::rel(pointsize),
                                alpha = pointalpha)
        gg3 <- gg3 + ggplot2::labs(title = "LAM - Components", x = "Q", y = "Conc.")
        gg3 <- gg3 + ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(titlesize)),
                           axis.title.x = ggplot2::element_text(size = ggplot2::rel(axissize)),
                           axis.title.y = ggplot2::element_text(size = ggplot2::rel(axissize)),
                           panel.background = ggplot2::element_rect(fill = NA),
                           panel.grid.minor = ggplot2::element_line(colour = NA),
                           axis.line = ggplot2::element_line(colour = "black"))

        lm1 <- lm(Co ~ Q, data = foo)

        gg4 <- ggplot2::ggplot()
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 1.1,
                                       label = paste("Linear model SS = ",
                                                     round(sum(lm1$residuals ^ 2), 2)),
                                       size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 1,
                                       label = paste("LAM SS = ",
                                                     round(summary(x)$SumofSq, 2)),
                                       size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 0.9,
                                       label = paste("Q xover point = ", round(x$xover, 2),
                                                      unit),
                                       size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 0.8,
                                       label = paste("% samples PS dominated = ",
                                                     round(x$ps.time, 2)),
                                       size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 0.7, label = "LAM fit",
                                       col = "seagreen", size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 0.6, label = "LAM diffuse fit",
                                       col = "steelblue3", size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::annotate("text", x = 0.3, y = 0.5, label = "LAM point fit",
                                       col = "firebrick4", size = ggplot2::rel(plottext))
        gg4 <- gg4 + ggplot2::theme(plot.title = ggplot2::element_blank(),
                                    axis.title.x = ggplot2::element_blank(),
                                    axis.title.y = ggplot2::element_blank(),
                                    panel.background = ggplot2::element_blank(),
                                    panel.grid.minor = ggplot2::element_blank(),
                                    axis.line = ggplot2::element_blank(),
                                    axis.text = ggplot2::element_blank(),
                                    axis.ticks = ggplot2::element_blank())

        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
        print(gg, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
        print(gg2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
        print(gg3, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(gg4, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))

}