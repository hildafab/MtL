#load libraries to use
library(OpenML)
library(mlr)
library(reshape)

#The main meta-learner creation function
main.mtl <- function(learner){
  rm(list = ls())
  devtools::load_all()

  #define the tasks and learner to use
  OMLTasks <- getOMLDatasetsFromTaggedTasks("study_14")
  lrn <- makeLearner(learner)

  #create the model for the learner and tasks
  meta.model <- createModel(lrn,OMLTasks)
}

learner = 'classif.rpart'
main.mtl(learner)
