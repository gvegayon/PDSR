getFolderStructure <- function(url, maxdep=-1, dep=0){
  
  # Retrieve the folder structure of a ftp folder
  # Mode of use: getFolderStructure(url_of_ftp)
  # Example: getFolderStructure("ftp://pds-geosciences.wustl.edu/earth/grsfe/")
  
  pattern <- "^([0-9]{2}[-][0-9]{2}[-][0-9]{2})[\\t ]*([0-9]{2}[:][0-9]{2}[A-Z]+)[\\t ]*(.*)"
  
  message("Accessing to ", url)
  
  # Get folder
  fileNames <- try(getURL(url))
  
  if (class(fileNames)!="try-error") {
    fileNames <- strsplit(fileNames, "\\\r?\\\n")[[1]]
    
    # Parsing lines
    m <- regexec(pattern, fileNames)
    matches <- regmatches(fileNames, m)
    directory <- laply(matches, function(x) x[4])
    
    # Separate folders and files
    folders <- .trimString(gsub("<DIR>", "", directory))[grepl("<DIR>", directory)]
    files <- sapply(directory,.trimString)[!grepl("<DIR>", directory)]
    
    # Make the list
    tree <- llply(c(folders, files), function(x) {x})
    names(tree) <- c(folders, files)
    
    # Verify if folder have some folder to execute the function again
    if(length(folders)!=0){
      
      # Checking if maxrec is not set
      if (maxdep > 0) {
        if (maxdep <= dep) {
          return(tree)
        }
      }
      
      # Otherwise
      for(i in 1:length(folders)){
        urltmp <- sprintf("%s/%s/",url, as.character(folders[i]))
        subtree <- getFolderStructure(urltmp, maxdep=maxdep, dep=dep+1)
        tree[folders[i]] <- list(subtree)
      }
    }
    
    return(tree)
  }
  else return(fileNames[1])
}

# getMissions
# description:
# Returs a dataframe of the existing missions that suit a keyword
dirMissions <- function(keywords=NULL, missions=NULL) {
  
  # First checks if .mission DF already exists
  if (length(missions)==0) {
    
    message("Conecting to http://pds.jpl.nasa.gov/tools/dsstatus/")
    pdestatusuri <- "http://pds.jpl.nasa.gov/tools/dsstatus/dsidStatus.jsp?sortOpt1=di.dsid&sortOpt2=&sortOpt3=&sortOpt4=&sortOpt5=&nodename=ALL&col2=dm.msnname&col3=&col4=&col5=&Go=Submit"
    missions <- try(readHTMLTable(pdestatusuri)[[3]])
    
    # Connection error
    if (class(missions) == "try-error") stop("Connection failed.")
    
    # Colnames
    colnames(missions) <- missions[1,]
    missions <- missions[-1,]
  }
  
  if (length(keywords)==0) return(missions)
  else {
    # Unlisting
    keywords <- unlist(keywords, recursive=TRUE)
    if (length(keywords)==0) return(missions)
    
    # Building regex
    if (length(keywords)>1) keywords <- paste(keywords, sep="|")
    
    return(missions[grepl(keywords, missions[,2], ignore.case=T),])
  }  
}

getMissionTree <- function(dataid) {
  
  # Getting missions lists
  atmoph <- getFolderStructure("ftp://pds-atmospheres.nmsu.edu/", maxdep=1)
  geosci <- getFolderStructure("ftp://pds-geosciences.wustl.edu/", maxdep=1)
  plasma <- getFolderStructure("ftp://pds-ppi.igpp.ucla.edu/", maxdep=1)
  ringno <- getFolderStructure("ftp://pds-rings.seti.org/", maxdep=1)
  
  return(list(atmoph, geosci, plasma, ringno))
}
