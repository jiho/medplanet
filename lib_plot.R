#
# Functions for nice plots
#

# theme with small font and no margins
theme_set(
  theme_grey(10) +
  theme(
    plot.margin=unit(c(0,0,0,0), "mm"),
    axis.text = element_text(size = rel(0.9))
  )
)

# nice colour scales
scale_fill_viridis <- function(...) { scale_fill_gradientn(colors=viridis.colors(10), ...) }
scale_color_viridis <- function(...) { scale_color_gradientn(colors=viridis.colors(10), ...) }

# remove y axis
no_y <- list(
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ),
  scale_y_continuous(breaks=0)
)
