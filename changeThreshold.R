#i is the number of the first ith docment index

changeThreshold <- function(doctocluster){
  NMIandCubeList = c()
  for (j in range(0.1,1,0.1)) {
    threshold = j
    res = NMIandBCube(doctocluster)
    NMIandCubeList <- c(NMIandCubeList,res)
  }
  maxindex = which.max(NMIandCubeList)
  thres = maxindex/10
  return(thres)
}

####added
####instruction
####
####...{
####      for(j in range(0.1,1,0.1)){
####          for(i in judge and add to which cluster){
####            addtowhichCluster
####            NMIandBCube(doctocluster)
####            threshold <- j
####            res = NMIandBCube(doctocluster)
####            NMIandCubeList <- c(NMIandCubeList,res)}
####            maxindex = which.max(NMIandCubeList)
####            thres = maxindex/10
####        }
####...}
####






