
facet_grid2 <- function(facets, shrink=TRUE, repeat.tick.labels=FALSE, ...) {
  f <- facet_grid(facets, shrink=shrink, ...)
  params <- append(f$params, list(repeat.tick.labels=repeat.tick.labels))
  ggproto(NULL, FacetGrid2, 
          shrink=shrink,
          params=params)
}

remove_labels_from_axis <- function(axisgrob) {
  if (inherits(axisgrob, 'zeroGrob')) return(axisgrob)
  a <- which(sapply(axisgrob$children, `[[`, i='name') == 'axis')
  d <- grepl('titleGrob', sapply(axisgrob$children[[a]]$grobs, `[[`, i='name'))
  if (sum(d) > 0) {
    axisgrob$children[[a]] <- do.call(gList, axisgrob$children[[a]]$grobs[!d])
    if (length(axisgrob$width$arg1) == 2) axisgrob$width$arg1 <- axisgrob$width$arg1[attr(axisgrob$width$arg1, 'unit') != 'grobwidth']
    if (length(axisgrob$height$arg1) == 2) axisgrob$height$arg1 <- axisgrob$height$arg1[attr(axisgrob$height$arg1, 'unit') != 'grobheight']
    #if (length(axisgrob$children[[a]]$heights) == 2) axisgrob$children[[a]]$heights <- axisgrob$children[[a]]$heights[!d]
  }
  axisgrob
}
grid.draw(remove_labels_from_axis(axisgrob))

FacetGrid2 <- ggproto('FacetGrid2', `_inherit`=FacetGrid,
  draw_panels  = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    saveRDS(params, 'draw_panels_params.rds')
    saveRDS(table, 'draw_panels.rds')
    # Add axes across all panels
    
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    cord <- do.call(rbind, strsplit(panels$name, split='-', fixed = TRUE))
    panels$col <- as.integer(cord[,3])
    panels$row <- as.integer(cord[,2])
    
    axis_b <- table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
    axis_l <- table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
    axis_t <- table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
    axis_r <- table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]
    
    if (params$repeat.tick.labels == FALSE) {
      axis_b <- lapply(axis_b, remove_labels_from_axis)
      axis_l <- lapply(axis_l, remove_labels_from_axis)
      axis_t <- lapply(axis_t, remove_labels_from_axis)
      axis_r <- lapply(axis_r, remove_labels_from_axis)
    }
    
    panel_range <- ggplot2:::find_panel(table)
    panel_range[,c('col','row')] <- c(max(panels$col), max(panels$row))
    
    l_axis_column_added <- logical(panel_range$col)
    r_axis_column_added <- logical(panel_range$col)
    t_axis_row_added <- logical(panel_range$row)
    b_axis_row_added <- logical(panel_range$row)
    
    for (i in nrow(panels):1) {
      p <- panels[i,]
      # Bottom
      if (p$row != panel_range$row & !inherits(axis_b[[p$col]], 'zeroGrob')) { # panel not in bottom row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (b_axis_row_added[p$row] == FALSE) {
          b_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_b), coord$b)
        }
        table <- gtable_add_grob(table, axis_b[[p$col]], t=coord$b+1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-b-%d-%d', p$col, p$row))
      }
      # Left
      if (p$col > 1 & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not left-most column, (add column), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (l_axis_column_added[p$col] == FALSE) {
          l_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_l), coord$l-1)
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        } else {
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l-1, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        }
      }
      # Top
      if (p$row > 1 & !inherits(axis_t[[p$col]], 'zeroGrob')) { # panel not in top row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (t_axis_row_added[p$row] == FALSE) {
          t_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_t), coord$t-1)
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))
        } else {
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t-1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))        
        }
        
      }
      # Right
      if (p$col != panel_range$col & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not right-most, (add colun), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (r_axis_column_added[p$col] == FALSE) {
          r_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_r), coord$r)
        }
        table <- gtable_add_grob(table, axis_r[[p$row]], t=coord$t, b=coord$b, l=coord$r+1, clip='off', name=sprintf('axis-r-%d-%d', p$row, p$col))
      }
      
    }
    
    
    saveRDS(list(panels=panels, layout=layout, x_scales=x_scales, y_scales=y_scales, ranges=ranges, coord=coord, data=data, theme=theme, params=params), 'draw_panels_args.rds')
    
    table
  }
)
(p <- ggplot(dat1, aes(gp, y)) + geom_point() + facet_grid2(cl2 ~ cl, repeat.tick.labels = FALSE) + coord_closed_cart(horizontal='left', vertical='none') + scale_y_continuous(sec.axis=sec_axis(~.*pi)))
(p <- ggplot(dat1, aes(gp, y)) + geom_point() + facet_grid2(cl2 ~ cl) + coord_closed_cart(horizontal='left', vertical='none') + scale_y_continuous(sec.axis=sec_axis(~.*pi)) + scale_x_discrete(position='top') + theme(strip.placement = 'outside'))
g <- ggplot_gtable(ggplot_build(p))
grid.newpage(); grid.draw(g)

# Repeat axes:
# Repeat bottom axes
res <- readRDS('draw_panels.rds')
axis_pos <- which(grepl('axis-t-[[:digit:]]+', res$layout$name))
axis_pos <- which(grepl('axis-b-[[:digit:]]+', res$layout$name))
rows <- unique(args$layout$ROW)
cols <- unique(args$layout$COL)

panels <- which(grepl('panel-[[:digit:]]+-[[:digit:]]+', res$layout$name))
horisontal <- range(res$layout$l[panels])

axis <- res$grobs[axis_pos]
row_pos <- which(grepl('panel-1-[[:digit:]]+', res$layout$name))
for (row in row_pos[-length(row_pos)]) {
  res <- gtable_add_rows(res, axis[[1]]$height, res$layout$b[row])
  res <- gtable_add_grob(res, axis, t=res$layout$b[row]+1, l=horisontal[1], r=horisontal[2], clip='off')
}

