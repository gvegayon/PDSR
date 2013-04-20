readPDStable <- function(header) {
  # Reading files
  lbl <- parseLbl(readLines(paste(header, "lbl",sep=".")))
  tab <- read.table(paste(header, "tab",sep="."), skip=2)
  
  # Adding colnames
  colnames(tab) <- (lblcols <- getColnames(lbl)[[1]])$NAME
  
  return(list(table=tab, lbl=lblcols, desc=lbl$TABLE$DESCRIPTION))
}

#x <- readPDStable(header="data/apollo12_sws_1hr_1976c9388")