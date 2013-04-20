getFolderStructure <- function(url){
  
  # Retrieve the folder structure of a ftp folder
  # Mode of use: getFolderStructure(url_of_ftp)
  # Example: getFolderStructure("ftp://pds-geosciences.wustl.edu/earth/grsfe/")
  
  pattern <- "^([0-9]{2}[-][0-9]{2}[-][0-9]{2})[\\t ]*([0-9]{2}[:][0-9]{2}[A-Z]+)[\\t ]*(.*)"
  
  message(sprintf("Accessing to \"%s\"", url))
  
  # Get folder
  fileNames <- strsplit(getURL(url), "\\\r\\\n")[[1]]
  m <- regexec(pattern, fileNames)
  matches <- regmatches(fileNames, m)
  directory <- laply(matches, function(x) x[4])
  
  # Separate folders and files
  folders <- gsub("^\\s+|\\s+$", "", gsub("<DIR>", "", directory))[grepl("<DIR>", directory)]
  files <- gsub("^\\d+\\s*", "",directory)[!grepl("<DIR>", directory)]
  
  # Make the list
  tree <- llply(c(folders, files), function(x) {x})
  names(tree) <- c(folders, files)
  
  
  # Verify if folder have some folder to execute the function again
  if(length(folders)!=0){
    for(i in 1:length(folders)){
      tree[folders[i]] <- list(getFolderStructure(paste0(url, folders[i], "/")))
    }
  }
  
  return(tree)
}
