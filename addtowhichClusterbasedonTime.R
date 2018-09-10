doctoclusterbasedontime <- list()
doctocluster[1] <- c(1)
addtowhichClusterbasedonTime <- function(i,doctoclusterbasedontime,data){
  allsimilarity <- list()
  time1 <- data$V5[i]
  for(clusterid in 1:length(doctoclusterbasedontime)){
    similarityqueue <- c()
    for (docid in doctoclusterbasedontime[clusterid]) {
      time2 <- data$V5[docid]
      sim <- similarity_time(time1,time2)
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
    index <- length(doctoclusterbasedontime)+1
    return(index)
    #    print("a new cluster is created")
    #    doctocluster[[index]] <- c(1)
  }
}
