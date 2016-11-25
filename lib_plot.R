#
# Functions for nice plots
#

# theme with small font and no margins
library("ggplot2")
theme_set(
  theme_grey(10) +
  theme(
    plot.margin=unit(c(0,0,0,0), "mm"),
    axis.text = element_text(size = rel(0.9))
  )
)

# nice colour scales
library("chroma")
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

# abbreviate species names for easy plotting
abbrev_sp <- function(x, n=4) {
  bits <- str_split_fixed(x, fixed(" "), 2)
  g <- str_sub(bits[,1],1,1)
  s <- str_sub(bits[,2],1,n)
  str_c(g, ". ", s)
}
