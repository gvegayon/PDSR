rm(list=ls())

# PARSELINE:
# Gets the value and name of a property
parseLine <- function(x,lvl=0) {
  
  # Matching Line label
  pattern <- "(\\^?[A-Z_]+)+[\\t ]*[=](.*)*"
  m <- regexec(pattern, x)
  
  # If it matches
  if (m[[1]][1] > 0) {
    x <- regmatches(x, m)
    output <- as.list(gsub("(^[\\t ]*|[\\t ]*$)", "",x[[1]][3]))
    names(output) <- x[[1]][[2]]
    return(output)
  } # Else
  else return(NULL)
}

# Recursive nesting and parsing
parseLbl <- function(x) {
  
  # REGEX patterns
  strobj <- "^[\\t ]*OBJECT"
  endobj <- "^[\\t ]*END\\_OBJECT"
  
  nlines <- length(x)
  lbls <- NULL
  i <- 1
  i2 <- NULL
  curline <- x[i]
  
  while (i < nlines) {
    # If subcase
    if (grepl(strobj, curline)) {
      
      # Getting the corresponding END_OBJECT length
      i2 <- i
      nobject <- 1
      while (nobject>0) {
        i2 <- i2 + 1
        nobject <- nobject + grepl(strobj, x[i2]) - grepl(endobj, x[i2])
      }
      
      # Adding subelement
      lbls[[length(lbls)+1]] <- parseLbl(x[(i+1):(i2-1)])
      names(lbls)[[length(lbls)]] <- parseLine(x[i])[[1]]
      
      i <- i2 + 1
      curline <- x[i]
      
      next
    }
    
    # Normal case
    else lbls <- c(lbls, parseLine(curline))
    
    # Next
    i <- i + 1
    curline <- x[i]
  }
  return(lbls)
}


# Example 1 
# Removing white lines
lbls.txt <- readLines("data/apollo12_sws_28s_19760325.lbl.txt")
lbls.txt <- lbls.txt[grepl("[a-zA-Z0-9_]", lbls.txt)]
x <- parseLbl(lbls.txt)

# Example 2
# Removing white lines
lbls.txt <- readLines("data/apollo12_sws_28s_19760318.lbl.txt")
lbls.txt <- lbls.txt[grepl("[a-zA-Z0-9_]", lbls.txt)]
y <- parseLbl(lbls.txt)

# Example 3
# Removing white lines
lbls.txt <- readLines("data/lcross_mir1_raw_20090620025221236.lbl.txt")
lbls.txt <- lbls.txt[grepl("[a-zA-Z0-9_]", lbls.txt)]
z <- parseLbl(lbls.txt)