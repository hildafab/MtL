
getOMLTasks <- function(ids){
  oml.tasks <- lapply(ids,getOMLTask)
  return(oml.tasks)
}

getOMLTaggedTasks <- function(tag){
  oml.tasks <- listOMLTasks(tag=tag)
  return(oml.tasks)
}

getOMLDatasetsFromTasks <- function(ids){
  oml.tasks <- getOMLTasks(ids)
  oml.datasets <- list()
  for(i in 1:length(oml.tasks)){
    oml.datasets[[i]] <- oml.tasks[[i]]$input$data.set$desc$id
  }
  oml.datasets <- unlist(oml.datasets)
  return(oml.datasets)
}

getOMLDatasetsFromTaggedTasks <- function(tag){
  oml.tasks = getOMLTaggedTasks(tag)
  return(oml.tasks[,c('task.id','did')])
}
