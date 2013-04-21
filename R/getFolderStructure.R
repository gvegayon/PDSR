getFolderStructure <- function(url){
  
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
      for(i in 1:length(folders)){
        subtree <- getFolderStructure(sprintf("%s/%s/",url, as.character(folders[i])))
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
    assign(missions,try(readHTMLTable(pdestatusuri, header=TRUE)[[3]]))
    
    if (class(missions) == "try-error") stop("Connection failed.")
  }
  
  if (length(keywords)==0) return(missions)
  else {
    # Unlisting
    keywords <- unlist(keywords, recursive=TRUE)
    if (length(keywords)==0) return(missions)
    
    # Building regex
    if (length(keywords)>1) keywords <- paste(keywords, sep="|")
    
    return(missions[grepl(keywords, missions[,2]),])
  }  
}