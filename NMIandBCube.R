library("QuantumClone")
library("DPBBM")
NMIandBCube <- function(doctocluster){
  index <- length(doctocluster)
  ## get the corresponding cluster in temrms of each documents.
  NumC<-c()
  NumD<-c()
  for (m in 1:index){
    NumD<-append(NumD,doctocluster[[m]])
    for (i in 1: length(doctocluster[[m]])){
      NumC<-append(NumC,m)
    }
  }
  CDresult<-cbind(DocNum=NumD,ClusterNum=NumC)
  CDresult<-as.data.frame(CDresult)
  CDresult<-CDresult[order(CDresult$DocNum),]
  
  OriCNum<-dataset$V9[1:20]
  CDOresult<-cbind(DocNum=CDresult$DocNum,ClusterNum=CDresult$ClusterNum,OriginalV9=OriCNum)
  CDOresult<-as.data.frame(CDOresult)
  uniqueV9<-unique(dataset$V9)
  
  lengUniV9<-length(uniqueV9)
  OriCluResult<-cbind(uniqueV9,DefaultClusterIndex=1:lengUniV9)
  OriCluNum<-as.data.frame(OriCluResult)
  length(dataset$V9)
  NewV9<-dataset$V9[1:20]
  
  for (i in 1:length(NewV9)) {
    for (m in 1:length(OriCluNum$uniqueV9)){
      if (NewV9[i]==OriCluNum$uniqueV9[m]){
        NewV9[i]=OriCluNum$DefaultClusterIndex[m]
      }
    }
  }
  CDOresult<-cbind(CDOresult,NewV9)

  NMIresult<-NMI_cutree(CDOresult$ClusterNum,CDOresult$NewV9)  ## choose the first 20 docuements to do test
# NMIresult
# print("NMIresult")
# print(NMIresult)
  
  confusionTable<- table(CDOresult$ClusterNum,CDOresult$NewV9)
  confusionTable
  confusionTable<- table(CDOresult$ClusterNum,CDOresult$NewV9)
  confusionTable
  
  B_cub0.5<-BCubed_metric(CDOresult$NewV9,CDOresult$ClusterNum,0.5)
# B_cub0.5
#  print("B_cub0.5")
# print(B_cub0.5)
  NMIandBcube <- (NMIresult+B_cub0.5)/2
  return(NMIandBcube)
}


