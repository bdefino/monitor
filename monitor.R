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

# need local
LOCAL <- "."#getSrcDirectory(function() {})

# import mapping.R
source(paste(LOCAL, "mapping.R", sep = "/"))

# import search.R
source(paste(LOCAL, "searching.R", sep = "/"))

# METHODS

Monitor <- function(path, contexts, file = NULL, map.data = NULL, on.detect = NULL, retain.points = FALSE, sep = ",", sleep = 1, timeout = Inf) {
  # monitors a file
  # intended for indefinite monitoring
  
  detected <- list()
  
  init.time <- Sys.time()
  
  # validate
  
  if (!is.character(path)) {
    stop("\"path\" argument must be a character")
  }
  
  if (!is.data.frame(contexts)) {
    stop("\"contexts\" argument must be a data frame")
  } else if (!is.character(contexts$key)) {
    stop("\"contexts$key\" must be a character")
  } else if (!is.character(contexts$oper)) {
    stop("\"contexts$oper\" must be a character")
  } else if (is.null(contexts$val)) {
    stop("\"contexts$val\" cannot be null")
  }
  
  if (!is.null(file) && !is.character(file)) {
    stop("\"file\" argument must be a character or null")
  }
  
  if (!is.null(map.data)) {
    if (!is.list(map.data)) {
      stop("\"map.data\" argument must be a list or null")
    } else if (!is.numeric(map.data$breaks)) {
      stop("\"map.data$breaks\" must be a numeric vector")
    } else if (!is.null(map.data$cex) && !is.numeric(map.data$cex)) {
      stop("\"map.data$cex\" must be a numeric or a null")
    } else if (!is.character(map.data$colors)) {
      stop("\"map.data$colors\" must be a character vector (of length(\"$breaks\") - 1)")
    } else if (!is.null(map.data$legend) && !is.character(map.data$legend)) {
      stop("\"map.data$legend\" must be a character or null")
    } else if (!is.null(map.data$legend.title) && !is.character(map.data$legend.title)) {
      stop("\"map.data$legend.title\" must be a character or null")
    } else if (!is.null(map.data$title) && !is.character(map.data$title)) {
      stop("\"map.data$title\" must be a character")
    }
  }
  
  if (!is.null(on.detect) && !is.function(on.detect)) {
    stop("\"on.detect\" must be a function or null")
  }
  
  if (!is.logical(retain.points)) {
    stop("\"retain.points\" must be a logical")
  }
  
  if (!is.numeric(sleep)) {
    stop("\"sleep\" must be a numeric")
  } else if (!is.numeric(timeout)) {
    stop("\"timeout\" must be a numeric")
  }
  
  if (!is.null(map.data) && is.logical(retain.points) && retain.points) {
    # points will be retained
    
    # make base map
    
    Make.Map(
      leg = list(
        cex = map.data$cex,
        colors = map.data$colors,
        legend = map.data$legend,
        title = map.data$legend.title
      ),
      title = map.data$title
      )
  }
  
  # monitor the file
  
  # continue until timeout is reached
  
  while (Sys.time() - init.time <= timeout) {
    # need timestamp
    
    tmp.timestamp <- Sys.time()
    
    tmp.detected <- NULL
    
    # this accounts for connectivity issues
    
    try(tmp.detected <- Search(path, contexts, sep), silent = TRUE)
    
    # was anything detected?
    
    if (length(tmp.detected) > 0) {
      # display output
      
      print(tmp.detected)
      
      detected[[as.character(tmp.timestamp)]] <- tmp.detected
      
      # react?
      
      if (!is.null(on.detect)) {
        on.detect()
      }
      
      # store?
      
      if (!is.null(file)) {
        dput(detected, file = file)
      }
      
      # map?
      
      if (!is.null(map.data)) {
        # retain points?
        
        if (retain.points) {
          # replot everything
          
          # "refresh" graphic device
          
          try(dev.off(dev.cur()), silent = TRUE)
          
          try(dev.new(), silent = TRUE)
          
          # make new map
          
          Make.Map(
            leg = list(
              cex = map.data$cex,
              colors = map.data$colors,
              legend = map.data$legend,
              legend.title = map.data$legend.title,
              title = map.data$title
              ),
            title = map.data$title
            )
        }
        
        # plot each output
        
        for (i in 1:nrow(contexts)) {
          # need element
          
          tmp.element <- tmp.detected[[i]]
          
          if (class(tmp.element) == "data.frame") {
            # is valid element: map it
            
            # need colors
            
            tmp.colors <- cut(
              tmp.element[[as.character(contexts[i, 1])]],
              breaks = map.data$breaks,
              labels = map.data$colors,
              right = FALSE
              )
            
            # plot
            
            Map.Points(
              tmp.element$longitude,
              tmp.element$latitude,
              cex = 0.25 * tmp.element[[as.character(contexts[i, 1])]],
              colors = as.character(tmp.colors),
              labels = tmp.element[[as.character(contexts[i, 1])]]
              )
          }
        }
      }
    }
    
    # decrease CPU usage
    
    Sys.sleep(sleep)
  }
  
  return(detected)
}