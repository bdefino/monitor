# Bailey Defino 2018
# <https://hiten2.github.io>

# CLARIFICATIONS:
#   1. "Authorship Information" regards the following:
#     "Hiten," "Hiten2," the year,
#     these clarifications, the disclaimer,
#     "<https://hiten2.github.io>," and this license.
#   2. "Illegal Activity" is relative to the end user.

# DISCLAIMER:
#   THIS CODE IS PROVIDED WITH NO WARRANTY NOR ANY GAURANTEES.
#   THE ORIGINAL AUTHOR IS NOT RESPONSIBLE FOR ANY DAMAGE
#   OR ILLEGAL ACTIVITY CAUSED BY THE USE OF THIS VERBATIM
#   OR MODIFIED CODE. BY VIEWING, STORING, OR EXECUTING THIS CODE
#   THE END USER ACKNOWLEDGES THESE IMPLICATIONS AND ASSUMES
#   ALL RESPONSIBILITY.

# LICENSE:
#   This verbatim code is intellectual property and may not
#   be redistributed. This code may be modified in any way
#   under either of these two conditions: (1) modified
#   versions are not distributed or (2) modified versions
#   satisfy the distribution conditions. Distribution
#   conditions are as follows and must all be met:
#   (1) the distributed code is not a verbatim copy,
#   (2) the distributed code retains the authorship information,
#   (3) the distributed code clarifies that the original author
#   had no part in the modifications.


# METHODS

Make.Map <- function(leg = NULL, title = NULL, x.window = c(-180, 180), y.window = c(-180, 180)) {
  # make a map
  # takes an optional leg argument to represent a legend
  # leg should be a list containing:
  # optional character expansion value(s): "$cex", color names: "$colors",
  # legend values: "$legend", and a title: "$title"
  
  # validate
  
  if (!is.null(leg)) {
    if (!is.list(leg)) {
      stop("\"leg\" must be a list or null")
    } else if (!is.null(leg$cex) && !is.numeric(leg$cex)) {
      stop("\"leg$cex\" must be a numeric or a null")
    } else if (!is.character(leg$colors)) {
      stop("\"leg$colors\" must be a character")
    } else if (!is.null(leg$legend) && !is.character(leg$legend)) {
      stop("\"leg$legend\" must be a character or null")
    } else if (!is.null(leg$title) && !is.character(leg$title)) {
      stop("\"leg$title\" must be a character or null")
    }
  }
  
  if (!is.null(title) && !is.character(title)) {
    stop("\"title\" must be a character or null")
  }
  
  if (!is.numeric(x.window)) {
    stop("\"x.window\" must be a numeric or null")
  } else if (!is.numeric(y.window)) {
    stop("\"y.window\" must be a numeric or null")
  }
  
  # create base plot
  
  # set margins
  
  par(mar = c(5, 5, 6, 4))
  
  # plot countries
  
  # need countries
  
  countries <- unique(read.csv("countries.csv", header = TRUE, stringsAsFactors = FALSE))
  
  smoothScatter(#points(
    as.numeric(countries$longitude),
    as.numeric(countries$latitude),
    axes = FALSE,
    bandwidth = 0.0625,
    main = title,
    nbin = c(360, 360),
    transformation = function(x) { return(x ** (1 / 16)) },
    useRaster = FALSE,
    xlab = "LONGITUDE",
    xlim = x.window,
    ylab = "LATITUDE",
    ylim = y.window
  )
  
  # plot the Equator and the Prime Meridian
  
  abline(h = 0, v = 0, col = "gray")
  
  text(
    c(-90, 0),
    c(0, 90),
    cex = 0.75,
    labels = c("Equator", "Prime Meridian"),
    offset = 0.5,
    pos = 3
  )
  
  # label axes
  
  for (side in 1:4) {
    axis(side = side, at = seq(-180, 180, 20))
  }
  
  if (!is.null(leg)) {
    # add legend
    
    # need cex
    
    if (is.null(leg$cex)) {
      # set cex
      
      leg$cex <- 1
    }
    
    legend(
      "bottomright",
      cex = 0.75,
      col = leg$colors,
      legend = leg$legend,
      pch = 1,
      pt.cex = leg$cex,
      title = leg$title
      )
  }
}

Map.Points <- function(x, y, cex = NULL, colors = "black", labels = NULL, pch = 1) {
  # map points to either the current, or a new graphics device
  
  # validate
  
  if (!is.numeric(x) && !is.vector(x)) {
    stop("\"x\" must be a numeric")
  } else if (!is.numeric(y) && !is.vector(y)) {
    stop("\"y\" must be a numeric")
  } else if (!is.null(cex) && !is.numeric(cex) && !is.vector(cex)) {
    stop("\"cex\" must be a numeric")
  } else if (!is.character(colors) && !is.vector(colors)) {
    stop("\"colors\" must be a character")
  } else if (!is.null(labels) && !is.character(labels) && !is.vector(labels)) {
    stop("\"labels\" must be a character or null")
  } else if (!is.numeric(pch)) {
    stop("\"pch\" must be a numeric")
  }
  
  if (length(dev.list()) == 0) {
    stop("no graphics device found")
  }
  
  # plot points
  
  points(x, y, cex = cex, col = colors, pch = 1)
  
  if (!is.null(labels)) {
    # plot labels
    
    text(x, y, col = colors, labels = labels, offset = 0.5, pos = 3)
  }
}