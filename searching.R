# Copyright (C) 2018 Bailey Defino
# <https://hiten2.github.io>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# METHODS

Search <- function(path, contexts, sep = ",") {
  # search a file for specific elements
  # takes contexts as a data frame with:
  # column name: "$key", comparison operator (as character): "$oper", and the value to compare to: "$val"
  # returns a list of matches for each context
  
  found.data <- list()
  
  # validate arguments
  
  if (!is.character(path)) {
    stop("\"path\" must be a character")
  }
  
  if (!is.data.frame(contexts)) {
    stop("\"contexts\" must be a data frame")
  } else if (!is.character(contexts$key)) {
    stop("\"contexts$key\" must be a character")
  } else if (!is.character(contexts$oper)) {
    stop("\"contexts$oper\" must be a character")
  } else if (is.null(contexts$val)) {
    stop("\"contexts$val\" must be a vector")
  }
  
  if (!is.character(sep)) {
    stop("\"sep\" must be a character")
  }
  
  # read file
  
  data.file <- read.table(path, header = TRUE, sep = sep, stringsAsFactors = FALSE)
  
  # go through each column/key pair
  
  for (i in 1:nrow(contexts)) {
    # search
    
    # need operator
    
    tmp.oper <- match.fun(as.character(contexts$oper[i]))
    
    # search
    
    tmp.indices <- which(tmp.oper(data.file[[contexts$key[i]]], contexts$val[1]))
    
    if (sum(tmp.indices) > 0) {
      # found something
      
      found.data[[contexts$key[i]]] <- data.file[tmp.indices, ]
    }
  }
  
  return(found.data)
}