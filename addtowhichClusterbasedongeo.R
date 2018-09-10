doctoclusterbasedongeo <- list()
doctoclusterbasedongeo[1] <- c(1)
addtowhichClusterbasedonTime <- function(i,doctoclusterbasedongeo,data){
  allsimilarity <- list()
  geo1 <- data$V8[i]
  for(clusterid in 1:length(doctoclusterbasedongeo)){
    similarityqueue <- c()
    for (docid in doctoclusterbasedongeo[clusterid]) {
      geo2 <- data$V8[docid]
      sim <- similarity_geo(geo1,geo2)
      similarityqueue <- c(similarityqueue,sim)
    }
    sumofsimilarityValue <- sum(similarityqueue)
    mean <- sumofsimilarityValue/length(similarityqueue)
    allsimilarity[clusterid] <- c(mean)
  }
  maxiumindex <- which.max(allsimilarity)
  largest <- allsimilarity[maxiumindex]
  if( largest > threshold ){
    return(maxiumindex)
    #    doctocluster[[maxiumindex]] <- c(doctocluster[[maxiumindex]], i)
  }else{
    index <- length(doctoclusterbasedongeo)+1
    return(index)
    #    print("a new cluster is created")
    #    doctocluster[[index]] <- c(1)
  }
}
