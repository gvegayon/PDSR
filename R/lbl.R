# parseLine
# description:
# Gets the value and name of a property
.trimString <- function(x) {
  return(gsub("(^\\s+|\\s+$)", "", x))
}

# rmQuotes
# description: 
# Removes leading/ending quotes
.rmQuotes <- function(x) {
  return(gsub("(^\\\"|\\\"$)", "", x))
}

# parseLine
# description:
# Gets the value and name of a property
parseLine <- function(x) {
  
  # Matching Line label
  pattern <- "(\\^?[A-Z_]+)+[\\t ]*[=](.*)*"
  m <- regexec(pattern, x)
  
  # If it matches
  if (m[[1]][1] > 0) {
    x <- regmatches(x, m)
    output <- as.list(.rmQuotes(.trimString(x[[1]][3])))
    names(output) <- x[[1]][[2]]
    return(output)
  } # Else
  else return(NULL)
}

# parseLbl
# Recursive nesting and parsing
parseLbl <- function(x) {
  
  # Trim lines
  x <- sapply(x, .trimString)
  
  # Removes comments
  x <- sapply(x, gsub, patter="/\\*.*", replace="")
  
  # REGEX patterns
  strobj <- "^[\\t ]*OBJECT"
  endobj <- "^[\\t ]*END\\_OBJECT"
  fulline <- "(\\^?[A-Z_]+)+[\\t ]*[=](.*)+"
  
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
    } # If its a continuation of the previous line
    else if (!grepl(fulline, curline) & nchar(curline) > 0) {
      
      # Loop while not finding a new label
      while (!grepl(fulline, curline) & i < nlines) {
        
        lbls[length(lbls)] <- paste(lbls[length(lbls)], .trimString(curline))
        i <- i + 1
        curline <- x[i]
      }
      
      # Remove border quotes
      lbls[length(lbls)] <- .rmQuotes(lbls[length(lbls)])
      next
    }
    
    # Normal case
    else {
      lbls <- c(lbls, parseLine(curline))
    }
    
    # Next
    i <- i + 1
    curline <- x[i]
  }
  return(lbls)
}

getColnames <- function(x) {
  if(any(grepl("^\\^TABLE$", names(x)))) {
    # Extract lists of tables
    tabs <- x[grepl("^TABLE$", names(x))]
    
    # Building list of tables
    output <- NULL
    for (i in 1:length(tabs)) {
      # Getting tabnames
      tabsnames <- x[grepl("^\\^HEADER$", names(x))]
      
      # Building list 
      cols <- tabs[[i]][grepl("^COLUMN$", names(tabs[[i]]))]
      suboutput <- NULL
      for (j in 1:length(cols)) {
        # Adding colname to list
        suboutput <- c(suboutput, cols[[j]][["NAME"]])
      }
      # Adding table colnames to table list
      output[[length(output) + 1]] <- suboutput
      names(output)[length(output)] <- tabsnames[[i]]
    }
    return(output)
  }
  else {
    message("x does not contains tables")
    return(NULL)
  }
}

# getMissions
# description:
# Returs a dataframe of the existing missions that suit a keyword
dirMissions <- function(keywords=NULL, updateit=FALSE, silent=FALSE) {
  
  # First checks if .mission DF already exists
  if (!exists(".missions", envir=.GlobalEnv) | !updateit) {
    if (!silent) warning("Object \'.missions\' already exists")
  }
  else { # Downloading data
    
    message("Conecting to http://pds.jpl.nasa.gov/tools/dsstatus/")
    pdestatusuri <- "http://pds.jpl.nasa.gov/tools/dsstatus/dsidStatus.jsp?sortOpt1=di.dsid&sortOpt2=&sortOpt3=&sortOpt4=&sortOpt5=&nodename=ALL&col2=dm.msnname&col3=&col4=&col5=&Go=Submit"
    assign(.missions,try(readHTMLTable(pdestatusuri, header=TRUE)[[3]]))
    
    if (class(.missions) == "try-error") stop("Connection failed.")
  }
  
  if (length(keywords)==0) return(.missions)
  else {
    # Unlisting
    keywords <- unlist(keywords, recursive=TRUE)
    if (length(keywords)==0) return(.missions)
    
    # Building regex
    if (length(keywords)>1) keywords <- paste(keywords, sep="|")
    
    return(.missions[grepl(keywords, .missions[,2]),])
  }  
}
