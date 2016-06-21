#Get the meta data for the listed tasks (from OpenML or dump of OpenML)
getMetaDataForDatasets <- function(OMLTasks,saved=FALSE){
  library(OpenML)
  
  if(saved && file.exists("metafeatures.RData")){
    var.name <- load(file="metafeatures.RData")
    metadata <- get(var.name)
    return(metadata)
  }
  
  datasetIds <- OMLTasks$did
  
  remove('datasets.meta.features')
  
  for(datasetId in datasetIds){
    dataset.meta.features <- getOMLDataSetQualities(did=datasetId)
    dataset.meta.features$datasetId <- datasetId
    dataset.meta.features <- cast(dataset.meta.features,formula = datasetId ~ name)
    dataset.meta.features$taskId <- OMLTasks[OMLTasks$did==datasetId,c("task.id")]
    
    if(!exists('datasets.meta.features')){
      datasets.meta.features <- dataset.meta.features
    }else{
      if(ncol(dataset.meta.features)!=ncol(datasets.meta.features)){
        cols.not.present <- names(datasets.meta.features)[!names(datasets.meta.features) 
                                                             %in% 
                                names(dataset.meta.features)]
        for(col.not.present in cols.not.present){
          dataset.meta.features[,col.not.present] <- NA
        }
      }
      datasets.meta.features <- rbind(datasets.meta.features,dataset.meta.features)
    }
  }
  save(datasets.meta.features,file="metafeatures.RData")
  return(datasets.meta.features)
}