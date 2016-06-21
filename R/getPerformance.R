#Get the performance details of a particular algorithm for the specified tasks
getPerformance <- function(lrn,OMLTasks=NULL){
  lrn.name <- gsub(pattern = "[.]",replacement = "_",x = lrn$id)
  lrn.perf.file <- paste("R/performances/",lrn.name,"_space.RData",sep="")
  
  perf.lrn.var.name <- load(file = lrn.perf.file)
  perf.lrn <- get(perf.lrn.var.name)
  
  drop.cols <- c('id','algo','run.id')
  
  perf.lrn <- perf.lrn[,!names(perf.lrn) %in% drop.cols]
  
  if(is.null(OMLTasks)){
    return(perf.lrn)
  }else{
    #intersect the ids
    common.ids <- intersect(perf.lrn$prob,paste('OpenML','Task',OMLTasks$task.id,sep='_'))
    
    perf.lrn.req <- perf.lrn[perf.lrn$prob %in% common.ids,]
    return(perf.lrn.req)
  }
}