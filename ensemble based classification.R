######################################################################################################################################
###classification-based techniques
UploadTData<-dataset[order(dataset$V4),]
data<-UploadTData[1:500,]
#View(data)
source('similarity_tag.R')
source('similarity_geo.R')
source('similarity_time.R')
source('similarity_title.r')
source('similarity_description.r')


occ_description<-occurrence(corpus.description(dataset))
occ_title<-occurrence(corpus.title(dataset))
occ_tag<-occurrence(corpus.tag(dataset))


#############################################################################################
################   there are five docuement features: tag, time, description, geographic，title


tagsimall<-c()
timesimall<-c()
descripsimall<-c()
geosimall<-c()
titlesimall<-c()

###################################################To pairs separately among the five features:
#######################################################all DD pairs for similarity (tag)
#tagsimall<-c()
tagSimPair<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      tagsim<-similarity_tag(data$V6[i],data$V6[m],corpusnumber,occ_tag)
      tagsimall<<-append(tagsimall,tagsim)
    }
  }
}

tagSimPair(20)  ## select front 20 docuements to do pairs


######################################################all DD pairs for similarity (time)
#timesimall<-c()
timeSimPair<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      timesim<-similarity_time(data$V5[i],data$V5[m])
      timesimall<<-append(timesimall,timesim)
    }
  }
}

timeSimPair(20)


############################################all DD pairs for similarity (description)
# descripsimall<-c()
descripSimPair<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      descripsim<-similarity_description(data$V3[i],data$V3[m],corpusnumber,occ_description)
      if(is.na(descripsim)){
        descripsim<-0
      }
      descripsimall<<-append(descripsimall,descripsim)
    }
  }
}

descripSimPair(20)
#View(descripsimall)



######################################################3all DD pairs for similarity (geo)
# geosimall<-c()
geoSimPair<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      geosim<-similarity_geo(data$V8[i],data$V8[m])
      if(is.na(geosim)){
        geosim<-0
      }
      geosimall<<-append(geosimall,geosim)
    }
  }
}

geoSimPair(20)
#View(geosimall)


######################################################3all DD pairs for similarity (title)
titlesimall<-c()
titleCorpus<-corpus.title(dataset)
titleSimPair<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      if(length(get("content", titleCorpus[i]))<=1 & get("content", titleCorpus[m])<=1){
        titlesim<-1
        titlesimall<<-append(titlesimall,titlesim)
      }else if(length(get("content", titleCorpus[i]))<=1 & get("content", titleCorpus[m])>1){
        titlesim<-0
        titlesimall<<-append(titlesimall,titlesim)
      }else{
        titlesim<-similarity_title(get("content", titleCorpus[i]),get("content", titleCorpus[m]),corpusnumber,occ_title)
        titlesimall<<-append(titlesimall,titlesim)
      }
    }
  }
}

titleSimPair(20)
#View(titlesimall)


pairSim<-cbind(tagsimall,timesimall,descripsimall,geosimall,titlesimall) 
pairSim<-as.data.frame(pairSim)  ### combine all similarities of all posibale pairs among the five feastures
View(pairSim)
class(pairSim)

############################################################################
############################################################################
#####Ground truth : whetehr pairs in same event id

eventJudgeResult<-c()
eventJudge<-function(NumberOfDoc){
  for (i in 1:(NumberOfDoc-1)){
    for (m in (i+1):NumberOfDoc){
      if(data$V9[i]==data$V9[m]){
        eventJudgeResult<<-append(eventJudgeResult,1)
      }
      else {
        eventJudgeResult<<-append(eventJudgeResult,0)
      }
    } 
  }
} 

eventJudge(20)
# View(eventJudgeResult)


pairSimWithEvent<-cbind(pairSim,eventJudgeResult)
class(pairSimWithEvent)
View(pairSimWithEvent)

######################################################################
################################################################do SVM
library("e1071")
test.set<-pairSimWithEvent[1:190,1:5]   ### the front 190 pairs  (use all train dataset to test for convinent)
# View(test.set)

model = svm(eventJudgeResult~., data=pairSimWithEvent, probability=F)
prob = predict(model, newdata=test.set, probability=F)
#View(prob)


for (i in 1:length(prob)){
  #print(i)
  if(prob[i]>=0.6){
    prob[i]<-1
  }else {
    prob[i]<-0
  }
}


# clusterNum
# clusterNum[1]<-c(1)
doc <- list()
for (i in 1:19) {
  for (j in i+1:20) {
    if(prob[j]==1){
      doc[[i]] <- c(doc[[i]],j)
    }
  }
}

