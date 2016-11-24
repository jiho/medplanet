#
# Set knitr options for all Rmd files
#

knitr::opts_chunk$set(
  # do not show the code, just the results
  echo=FALSE,
  # default figure size and aspect
  fig.width=5.4, fig.height=3, dpi=100, fig.retina=2,
  # use cache to speed things up
  cache=TRUE
)

# avoid scientific notation and too many digits
options(scipen=5)
options(digits=2)

