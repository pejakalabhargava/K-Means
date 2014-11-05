
##This  function is used to calcualte the distance of each of the point to the
##input centroid  
centroidDistance <- function(inputData, inputCentroid) {
   	##For each row of inputData and input centroid find distance between them
	dists = apply(inputData, 1, function(subDataMatrix)
    	apply(inputCentroid,1,function(subCentroid)
		dist(rbind(subDataMatrix, subCentroid))
    	)
    )
    ##return transpose of dists calcualted
	return(t(dists))
}
##This method is used to assign the given input data to each of the clusters based
##on input centroid and minimum eucldean distance
assignToClusters<-function(inputData, inputCentroid) {
	distanceToCentroid <- centroidDistance(inputData, inputCentroid)	
	 clusterLabel = apply(distanceToCentroid, 1, function(subdistanceToCentroid) which.min(subdistanceToCentroid))
	 return(clusterLabel) 
}

##This function is used to calculate the SSE of each of the cluster.
##Input is assumed to be matrix of points belonging to a cluster and the centroid of that cluster. 
SSECalculator<-function(inputData, inputCentroid) {
	 ##For each row of inputData and input centroid find distance between them
	 dists = apply(inputData, 1, function(subDataMatrix)
    	apply(inputCentroid,1,function(subCentroid)
		dist(rbind(subDataMatrix, subCentroid))^2
    	)
    )
    ##return transpose of dists calcualted
	return(sum(dists)) 
}

##Implementation of k-means clustering algorithm to cluster input data of n-dimensions into k clusters
myKMeans <- function(inputdata,k) {

##convert input data to data matrix
dataMatrix <- as.matrix(inputdata)
##get number of rows
rows <- as.integer(nrow(dataMatrix))
##get number of columns
columns <- as.integer(ncol(dataMatrix))
##set the storage mode of matrix as double
storage.mode(dataMatrix)='double'
##Create a matrix to hold centroids
currentCentroid <- matrix(nrow=k,ncol=columns)
randomCentroidIndex <- sample(1:rows, k)
##select k initial centroids
currentCentroid <-dataMatrix[randomCentroidIndex,]

prevCentroid <- matrix(nrow=k,ncol=columns,0)

##Run a loop till centroids converge 
while(all(currentCentroid == prevCentroid)!=TRUE){
	prevCentroid <- currentCentroid
	##compute the distance to all the centroid and find the cluster each data row belongs to
	clusters  <-  assignToClusters(dataMatrix,currentCentroid)
	##update the centroids based on new clusters formed by finding column mean for each clusters
	for(i in 1:k)
		currentCentroid[i,] <- colMeans(dataMatrix[which(clusters == i), ])
		##Repalce all NAN with max intgere possible value so that empty cluster is handled
		currentCentroid <- replace(currentCentroid, is.na(currentCentroid), .Machine$integer.max)
}
##bind  datamatrix and cluster lables
newDataMatrix = cbind(dataMatrix,clusters)
result = data.frame(newDataMatrix)
centroid = currentCentroid;
SSE = matrix(nrow=k,ncol=1)
##calculate the SSE of each of the clusters
for(i in 1:k) {
	inputMat = newDataMatrix[(which(newDataMatrix[,ncol(newDataMatrix)] == i)),-c(ncol(newDataMatrix))]
	inputCent =matrix(centroid[i,],1,columns)
	SSE[i,] <-SSECalculator(inputMat,inputCent)
}
##return cluster , centroid matrix and SSE
return(list(result=result,centroid=centroid,SSE=SSE))
}