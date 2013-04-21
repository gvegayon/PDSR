getFolderStructure <- function(url, maxdep=-1, dep=0){
  
  # Retrieve the folder structure of a ftp folder
  # Mode of use: getFolderStructure(url_of_ftp)
  # Example: getFolderStructure("ftp://pds-geosciences.wustl.edu/earth/grsfe/")
  
  patternwin <- "^([0-9]{2}[-][0-9]{2}[-][0-9]{2})[\\t ]*([0-9]{2}[:][0-9]{2}[A-Z]+)[\\t ]*(.*)"
  patternuni <- "([a-zA-Z0-9_]+\\.*[a-zA-Z0-9_]+)+$"
  
  message("Accessing to ", url)
  
  # Small correction
  if (!grepl("/$",url)) url <- sprintf("%s/",url)
  
  # Get folder
  fileNames <- try(getURL(url), silent=TRUE)
  
  if (class(fileNames)!="try-error") {
    fileNames <- strsplit(fileNames, "\\\r?\\\n")[[1]]
    
    # Parsing lines
    if (grepl(patternwin, fileNames[1])) { # if windows like
      m <- regexec(patternwin, fileNames)
      matches <- regmatches(fileNames, m)
      directory <- laply(matches, function(x) x[4])
      
      # Separate folders and files
      folders <- .trimString(gsub("<DIR>", "", directory))[grepl("<DIR>", directory)]
      files <- gsub("[0-9]+[\\t ]+", "",
                    sapply(directory,.trimString)[!grepl("<DIR>", directory)]
      )
    }
    else {                                # if unix like
      m <- regexec(patternuni, fileNames)
      matches <- regmatches(fileNames, m)
      directory <- laply(matches, function(x) x[1])
      
      # Separate folders and files
      folders <- directory[!grepl("\\.[a-zA-Z]+$", directory)]
      files <- gsub("[0-9]+[\\t ]+", "",
                    directory[grepl("\\.[a-zA-Z]+$", directory)])
    }
    
    # Make the list
    tree <- llply(c(folders, files), function(x) {x})
    names(tree) <- c(folders, files)
    
    # Verify if folder have some folder to execute the function again
    if(length(folders)!=0){
      
      # Checking if maxrec is not set
      if (maxdep >= 0) {
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
  else {
    warning("A problem with ", url)
    return(fileNames[1])
  }
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

# getMissionsList
# Description:
# Search among all available FTP access and stores them in nested
# lists. With this function the dataset "fullMissionLists" was
# built.
getMissionsList <- function(maxdep=1) {
  
  # Getting missions lists
  atmoph <- getFolderStructure("ftp://pds-atmospheres.nmsu.edu/", maxdep=maxdep)
  geosci <- getFolderStructure("ftp://pds-geosciences.wustl.edu/", maxdep=maxdep)
  plasma <- getFolderStructure("ftp://pds-ppi.igpp.ucla.edu/", maxdep=maxdep)
  ringno <- getFolderStructure("ftp://pds-rings.seti.org/", maxdep=maxdep)
  
  # Building and naming
  output <- list(atmoph, geosci, plasma, ringno)
  names(output) <- c(
    "ftp://pds-atmospheres.nmsu.edu/",
    "ftp://pds-geosciences.wustl.edu/",
    "ftp://pds-ppi.igpp.ucla.edu/",
    "ftp://pds-rings.seti.org/"
    )
  
  return(output)
}

# flattenMissionTree
# Description:
# Returns a list of URLs from which a program can be matched
.flattenMissionTree <- function(object) {
  
  # Getting root URLs
  namestmp <- names(object)
  object <- unlist(object)
  names(object) <- gsub("\\.", "/", names(object))
  
  # Replacing / by .
  for (i in namestmp) {
    names(object) <- gsub(gsub("\\.", "/", i), i, names(object))
  }
  
  return(object)
}

# getMissionURL :
# Given a dataid, returns the corresponding root FTP path of the mission
getMissionURL <- function(dataid, fullMissions=NULL) {
  # Loads data
  if (length(fullMissions) == 0) 
    data(fullMissionsList, envir=environment())
  else fullMissionsList <- fullMissions
  
  # Matching mission
  pattern <- gsub("\\.[0-9]+","", dataid)
  pattern <- tolower(gsub("\\.","/", pattern))
  output <- tolower(names(.flattenMissionTree(fullMissionsList)))
  
  output <- output[grepl(pattern, output, fixed=TRUE)]
  
  if (length(output) > 0) {
    
    # Name replacing
    for (i in 1:length(output)) {
      names(output)[i] <- gsub(gsub("\\.","/",output[i]), output[i], names(output)[i])
    }
    return(output)
  }
  else {
    message("No mission with id ", dataid)
    return(NULL)
  }
}

# exploreMission
# Looks for the matching FTP path to access an specific mission
# data ID.
exploreMission <- function(dataid, fullMissions=NULL, maxdep=1) {
  
  # Getting the corresponding URLs
  urls <- getMissionURL(dataid, fullMissions)
  
  # Checking number of URLs
  if ((nm <- length(urls)) == 0) {
    message("0 missions fund.")
    return(NULL)
  }
  else if (nm > 1) warning(nm, " missions where found")
    
  # Fetching data from FTP
  output <- NULL
  
  # Looking over the URLs
  for (i in 1:nm) {
    
    # While no datafile found, keep looking
    suppressMessages(outputtmp <- getFolderStructure(urls[i], maxdep=0))
    nodata <- TRUE
    j <- 1
    while (nodata) {
      suppressMessages(
      outputtmp <- getFolderStructure(urls[i], maxdep=0+j)
      )
      j <- j + 1
      
      nodata <- !any(unlist(outputtmp) == "data")
    }

    # Getting the data URL
    outputtmp <- names(unlist(outputtmp))
    outputtmp <- outputtmp[grepl("data$", outputtmp)]
    
    # Fixing URL
    outputtmp <- gsub("\\.","/",outputtmp)
    
    # Reading the last line
    urls[i] <- sprintf("%s/%s/",urls[i],outputtmp)
    outputtmp <- getFolderStructure(urls[i], maxdep=maxdep)
    
    output[[i]] <- outputtmp
  }
  
  names(output) <- urls
  return(output)
}
