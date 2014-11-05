data = read.table(file = "C:/Users/pejakalabhargava/Desktop/d-c4hw2.csv", header=T, sep=",")
data = data[,-c(3)]
result  = myKMeans(data,4)
SSE = result$SSE;
clusteringSSE = colSums(SSE)
result = result$result
x1 = min(data[,1])
x2 = max(data[,1])
y1 = min(data[,2])
y2 = max(data[,2])
du.cl1 = subset(result, result$clusters == 1)
du.cl2 = subset(result, result$clusters == 2)
du.cl3 = subset(result, result$clusters == 3)
du.cl4 = subset(result, result$clusters == 4)
title= paste("Cluster Distributions for k=4 with clustering SSE", clusteringSSE, sep="=")
plot(0, col=c(1), main = title,xlim=c(x1,x2), ylim=c(y1,y2),sub="bkakran", xlab = "length", ylab = "width")
grid()
i=1
points(du.cl1, col=c(i+1), pch=c(2+i))
points(du.cl2, col=c(i+2), pch=c(3+i))
points(du.cl3, col=c(i+3), pch=c(4+i))
points(du.cl4, col=c(i+4), pch=c(5+i))
