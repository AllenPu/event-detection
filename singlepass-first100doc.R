#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


#documentid to document
a <- list()
for( n in 1 : length(corpus)){
  a[n] <- data.frame(text = get("content", corpus[n]))
}
#the corresponding clusterid to the documrntid
doctocluster <- list()
#initial the first documnet to cluster 1
doctocluster[1] <- c(1)

#calculate the mean similarity of the cluster
meansimilarity <- function(cluster){
  a <- c()
  for(doc in cluster){
    s <- similarity(d,doc,n,occ)
    a <- c(a, s)
  }
  mean <- sum(a)/length(a)
  return(mean)
}

## assign the document di , di = a[i] is the i th of the documentid to document cluster
addtowhichCluster <- function(i,doctocluster,a){
  allsimilarity <- list()
  di <- data.frame(text = get("content", corpus[i]))
  for(clusterid in 1:length(doctocluster)){
    similarityqueue <- c()
    for (docid in doctocluster[clusterid]) {
      doc <- data.frame(text = get("content", corpus[docid]))
      sim <- similarity(di, doc,n,occ)
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
    index <- length(doctocluster)+1
    return(index)
#    print("a new cluster is created")
#    doctocluster[[index]] <- c(1)
  }
}


#########the first 100 documents to the cluster
for (i in 2:20) {
  index <- addtowhichCluster(i,doctocluster,a)
  if(index <= length(doctocluster)){
    doctocluster[[index]] <- c(doctocluster[[index]],i)
  }else{
    doctocluster[[index]] <- c(i)
  }
}

#############
di <- data.frame(text = get("content", corpus[1]))
doc <- data.frame(text = get("content", corpus[2]))
for (i in 1:4950) {
  similarity(di,doc,n,occ)
}
