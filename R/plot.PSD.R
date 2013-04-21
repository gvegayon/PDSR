plot.PDS <- function(x, variables = sample(names(x$table), size = min(c(4,ncol(x$table)))), 
                     type = c("ggpairs", "plotmatrix", "time"), ...) {
  
  table <- x$table
  table <- subset(table, select = variables)
  if(type == "time"){
    stopifnot(any(x$lbl$DATA_TYPE == "TIME"))
    
    var_time <- names(x$table)[which(x$lbl$DATA_TYPE=="TIME")]  
    table$TIME <- as.POSIXct(as.character(x$table[[var_time]]), format = "%Y-%m-%dT%H:%M:%S")

    p <- ggplot(melt(table, id.vars="TIME")) +
      geom_line(aes(TIME, value, color = variable, group = variable)) +
      facet_grid(variable~.) + 
      scale_x_datetime(labels = date_format("%Y-%m-%d")) +
      theme(legend.position="none")
    p
    
  } else if(type == "ggpairs") {
    p <- ggpairs(table)
  } else {
    p <- plotmatrix(table)
  }
  p
}
# object <- readPDStable("data/apollo12_sws_1hr_1976c9388")
# plot.PDS(object, type= "time")
# plot.PDS(object, type= "ggpairs")
# plot.PDS(object, type= "plotmatrix")
