#' Cartesian coordinates with closed range on axis-line.
#' 
#' Use `theme(axis.line)` to set the axis-lines' style, as usual.
#' It might be necessary to set `theme(panel.border=element_blank())` to disable
#' the border that is drawn on top of the axis-lines.
#'
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand If \code{TRUE}, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If \code{FALSE},
#'   limits are taken exactly from the data or \code{xlim}/\code{ylim}.
#' @param horizontal Which end of the horizontal line to limit to a tick. Use 'left', 'right', 'both', or 'none'.
#' @param vertical Which end of the vertical line should not proceed across the outer tick? Use 'top', 'bottom', 'both', or 'none'.
#' @param gap Numeric value (usually between 0 and 1) to create a small gap where horizontal and vertical axes meet.
#' @examples
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30),
#'   cl = sample.int(3, 30, replace=TRUE),
#'   cl2 = sample(c('a','b','c'), 30, replace=TRUE)
#' )
#' ggplot(df, aes(gp, y)) +
#'   geom_point()  + coord_closed_cart(horizontal = 'left', vertical = 'top') + labs(title='hello') +
#'   facet_grid(cl ~ cl2)
#' 
coord_closed_cart <- function(xlim = NULL, ylim = NULL, expand = TRUE, horizontal=c('left','right','both','none'), vertical=c('top','bottom','both','none'), gap=0.05) {
  ggproto(NULL, ClosedCoordCartesian,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          horizontal=match.arg(horizontal),
          vertical=match.arg(vertical),
          gap=gap
  )
}


ClosedCoordCartesian <- ggproto('ClosedCoordCartesian', `_inherit`=CoordCartesian, 
    render_axis_h = function(self, scale_details, theme) {
      l <- ggproto_parent(CoordCartesian, self)$render_axis_h(scale_details, theme)
      l <- lapply(l, function(a) {
        if (a$name == 'NULL') return(a)
        
        i <- which(grepl('line', names(a$children)))
        r <- range(as.numeric(scale_details$x.major))
        a$children[[i]]$x <- switch(self$horizontal,
                           none =  unit(c(min(0+self$gap, r[1]), max(1-self$gap, r[2])), 'native'),
                           left =  unit(c(r[1], max(1-self$gap, r[2])), 'native'),
                           right = unit(c(min(0+self$gap, r[1]), r[2]), 'native'),
                           both =  unit(r, 'native')
        )
        
        return(a)
      })
      l
    },
    render_axis_v = function(self, scale_details, theme) {
      l <- ggproto_parent(CoordCartesian, self)$render_axis_v(scale_details, theme)
      l <- lapply(l, function(a) {
        if (a$name == 'NULL') return(a)
        
        i <- which(grepl('line', names(a$children)))
        r <- range(as.numeric(scale_details$y.major))
        a$children[[i]]$y <- switch(self$vertical,
                                    none =    unit(c(min(0+self$gap,r[1]), max(1-self$gap, r[2])), 'native'),
                                    bottom =  unit(c(r[1], max(1-self$gap, r[2])), 'native'),
                                    top = unit(c(min(0+self$gap, r[1]), r[2]), 'native'),
                                    both =  unit(r, 'native')
        )
        
        return(a)
      })
      l
    }
    
)

#' Closed axis-lines for `coord_flipped`.
#' @inheritsParams coord_closed_cart
coord_closed_flipped <- function(xlim = NULL, ylim = NULL, expand = TRUE, horizontal=c('left','right','both','none'), vertical=c('top','bottom','both','none'), gap=0.05) {
  ggproto(NULL, ClosedCoordCartesianFlipped,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          horizontal=match.arg(horizontal),
          vertical=match.arg(vertical),
          gap=gap
  )
}


ClosedCoordCartesianFlipped <- ggproto('ClosedCoordCartesianFlipped', `_inherit`=CoordFlip, 
  render_axis_h = function(self, scale_details, theme) {
    l <- ggproto_parent(CoordCartesian, self)$render_axis_h(scale_details, theme)
    l <- lapply(l, function(a) {
      if (a$name == 'NULL') return(a)
      
      i <- which(grepl('line', names(a$children)))
      r <- range(as.numeric(scale_details$x.major))
      a$children[[i]]$x <- switch(self$horizontal,
                                  none =  unit(c(min(0+self$gap, r[1]), max(1-self$gap, r[2])), 'native'),
                                  left =  unit(c(r[1], max(1-self$gap, r[2])), 'native'),
                                  right = unit(c(min(0+self$gap, r[1]), r[2]), 'native'),
                                  both =  unit(r, 'native')
      )
      
      return(a)
    })
    l
  },
  render_axis_v = function(self, scale_details, theme) {
    l <- ggproto_parent(CoordCartesian, self)$render_axis_v(scale_details, theme)
    l <- lapply(l, function(a) {
      if (a$name == 'NULL') return(a)
      
      i <- which(grepl('line', names(a$children)))
      if (length(i) == 0) return(a)
      r <- range(as.numeric(scale_details$y.major))
      a$children[[i]]$y <- switch(self$vertical,
                                  none =    unit(c(min(0+self$gap,r[1]), max(1-self$gap, r[2])), 'native'),
                                  bottom =  unit(c(r[1], max(1-self$gap, r[2])), 'native'),
                                  top = unit(c(min(0+self$gap, r[1]), r[2]), 'native'),
                                  both =  unit(r, 'native')
      )
      
      return(a)
    })
    l
  }
  
)

