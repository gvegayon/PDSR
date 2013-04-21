readPDStable <- function(dataid) {
  
  # Stuff
  dataid <- gsub("\\.[0-9]+$","",tolower(dataid))
  
  # Reading files
  lbl <- parseLbl(readLines(paste(dataid, "lbl",sep=".")))
  lblcols <- getColnames(lbl)[[1]]
  
  # Seting width
  width <- as.numeric(lblcols$BYTES) + 2
  
  print(width)
  tab <- read.fwf(paste(dataid, "tab",sep="."), widths=width, skip=2, sep="^")
  
  # Adding colnames
  colnames(tab) <- lblcols$NAME

  output <- list(table=tab, lbl=lblcols, desc=lbl$TABLE$DESCRIPTION,
                 mission=lbl$MISSION_NAME, datasetid=lbl$DATA_SET_ID)
  class(output) <- "PDS"
  return(output)
}


summary.PDS <- function(object, ...) {
  
  # Priting some basic details
  print(list(Mission=object$mission,Description=object$desc))
  
  # Adding pretty tabnames
  #colnames(object$table) <- object$lbl[,2]
  
  
  # Picking which cols to summarize
  .ProcMeans(object$table)
}

.ProcMeans <- function(data, ...){
  res <- .Multi.Sapply(data[,laply(data, is.numeric)],
                       N = length,
                       N.miss = function(x) sum(is.na(x, ...)),
                       N.uniques = function(x) length(unique(x, ...)),
                       Min = function(x) min(x, na.rm = T),
                       Qu.1st = function(x) quantile(x, .25, na.rm = T,...),
                       Median = function(x) median(x, na.rm = T),
                       Mean = function(x) mean(x, na.rm = T),
                       Qu.3rd = function(x) quantile(x, .75, na.rm = T,...),
                       Max = function(x) max(x, na.rm = T),
                       StdDev = function(x) sd(x, na.rm = T),
                       ...)
  res <- data.frame(res)
  res$variable <- names(data[,laply(data, is.numeric)])
  res <- subset(res, select = unique(c("variable",names(res))))
  res
}

.Multi.Sapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}

downLoadMission <- function(datatree, maxdata=-1) {
  sources <- names(datatree)
  
  for (i in 1:length(sources)) {
    d <- datatree[[i]]
    
    # Test if there are tab or lbl files
    test <- grepl("(\\.tab$|\\.lbl$)",d)
    
    if (any(test)) {
      # Subseting
      dsub <- d[test]
      dsub2 <- unique(gsub("\\.tab^|\\.lbl", "", dsub))
      
      # Start parsing 
      for (j in 1:length(dsub2)) {
        
        # Subsets lbl and tab of one file
        ftpurllbl <- sprintf("%s/%s.lbl", sources[i], dsub2[j])
        ftpurltab <- sprintf("%s/%s.tab", sources[i], dsub2[j])
        
        message("Conecting to ",ftpurllbl," ...")
        lbl <- try(getURL(ftpurllbl), silent=TRUE)
        
        if (class(lbl) != "try-error") {
          message("Conecting to ",ftpurltab," ...")
          tab <- try(getURL(ftpurltab), silent=TRUE)
        }
        else next
        
        if (class(tab) != "try-error") {
          # Writing data
          message("Writing tables at ",getwd())
          writeLines(lbl, sprintf("%s.lbl", dsub2[j]))
          writeLines(tab, sprintf("%s.tab", dsub2[j]))
        }
        else next        
      }
      
    }
  }
}
#x <- readPDStable(header="data/apollo12_sws_1hr_1976c9388")
#summary(x, nvars=2)

