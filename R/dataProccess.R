readPDStable <- function(header) {
  lbl <- parseLbl(readLines(paste(header, "lbl",sep=".")))
  tab <- read.table(paste(header, "tab",sep="."), skip=2)
  
  colnames(tab) <- getColnames(lbl)[[1]]
  
  return(tab)
}

# x <- readPDStable(lbl="data/apollo12_sws_1hr_1976c9388.lbl", tab="data/apollo12_sws_1hr_1976c9388.tab")