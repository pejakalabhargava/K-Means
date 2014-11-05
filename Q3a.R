data = read.table(file = "C:/Users/pejakalabhargava/Desktop/d-c4hw2.csv", header=T, sep=",")
colNames=c('Number Of clusters','Clustering SSE')
SSEmat = matrix(nrow=7,ncol=2,0)
colnames(SSEmat) <- colNames
for(i in 2:8) {
	result  = myBKMeans(data,i)
	SSE = result$SSE;
	clusteringSSE = colSums(SSE)
	SSEmat[i-1,1] = i
	SSEmat[i-1,2] = clusteringSSE
}

plot(SSEmat, col=c(1), main = "Elbow Plot", pch=c(19), sub="bkakran")