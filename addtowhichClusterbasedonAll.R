doctoclusterall <- list()
doctoclusterall[1] <- c(1)
####
####in this method , 2 should start from 2
####
addtowhichClusterbasedonAll <- function(i,doctoclusterall,data,doctocluster,doctoclusterbasedontime,doctoclusterbasedongeo){
  allsimilarity <- list()
  geo1 <- data$V8[i]
  time1 <- data$V5[i]
  di <- data.frame(text = get("content", corpus[i]))
  for(clusterid in 1:length(doctoclusterall)){
    similarityqueue <- c()
    for (docid in doctoclusterall[clusterid]) {
      ###
      time2 <- data$V5[i]
      geo2 <- data$V8[docid]
      doci <- data.frame(text = get("content", corpus[i]))
      ###
      simtime <- similarity_time(time1,time2)
      simgeo <- similarity_geo(geo1,geo2)
      simtag <- similarity_tag(di,doci)
      #####
      weighttag <- NMIandBCube(doctocluster = doctocluster)
      weighttime <- NMIandBCube(doctocluster = doctoclusterbasedontime)
      weightgeo <- NMIandBCube(doctocluster = doctoclusterbasedongeo)
      ####
      simsum <- weighttag*simtag + weighttime*simtime+weightgeo*simgeo
      fraction <- weighttag+weighttime+weightgeo
      sim <- simsum/fraction
      ####
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
    index <- length(doctoclusterall)+1
    return(index)
    #    print("a new cluster is created")
    #    doctocluster[[index]] <- c(1)
  }
}
