#Build the model using RF for the particular algorithm using particular tasks
createModel <- function(lrn,OMLTasks){
  #Get performance and meta features
  perf <- getPerformance(lrn,OMLTasks)
  meta.features <- getMetaDataForDatasets(OMLTasks,saved = TRUE)
  
  #Build the dataset
  remove('meta.model.dataset')
  for(i in 1:nrow(perf)){
    row.data <- perf[i,]
    task.id <- strsplit(row.data$prob,split = '_')[[1]][[3]]
    task.meat.features <- meta.features[meta.features$taskId==task.id,]
    feature.space <- cbind(row.data,task.meat.features)
    
    rows.to.drop <- c('prob','timepredict.test.mean','timetrain.test.mean','timetrain.test.sum',
                      'timepredict.test.sum','taskId','datasetId')
     feature.space <- feature.space[,!names(feature.space) %in% rows.to.drop]
     
     names(feature.space) <- gsub(x = names(feature.space), replacement = "", 
                                  pattern = ".test.mean")
     
     if(exists('meta.model.dataset')){
       meta.model.dataset <- rbind(meta.model.dataset,feature.space)
     }else{
       meta.model.dataset <- feature.space
     }
  }
  
  #TODO: improve with feature selection
  
  dataset <- meta.model.dataset
  
  # make a check is imputation is needed
  if (any(is.na(dataset))) {
    catf(" - Data imputation required ...")
    temp = mlr::impute(obj = dataset, classes = list(numeric = imputeMean(), 
                                                      factor = imputeMode()))
    dataset = temp$data
  }
  
  
  #create the model with Random Forest
  rf.lrn <- makeLearner("regr.randomForest")
  task <- makeRegrTask(data = dataset, target = "acc")
  
  
   model <- train(learner = rf.lrn, task = task)
   save(model, file = paste('R/models/',paste(lrn$short.name,'model.RData',sep = '_'),sep = ''))
   
   return(model)
}