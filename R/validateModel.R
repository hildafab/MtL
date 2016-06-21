#Validate if the meta model works
validate.model <- function(lrn,OMLtasks){
  library("parallelMap")
  
  parallelStart(mode="socket", cpus=3, level="mlr.resample", show.info=TRUE)
  
  resample(learner = rf.lrn, task = task, 
           resampling = makeResampleDesc(method = "CV", iters=10),
           measures = list(rmse))
  
  parallelStop()
}