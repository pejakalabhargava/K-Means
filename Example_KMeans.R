k<-4
data = read.table(file = "C:/Users/pejakalabhargava/Desktop/d-c4hw2.csv", header=T, sep=",")
#d = read.table(file = "C:/Users/pejakalabhargava/Desktop/sim-data1.csv", header=T, sep=",")

result  = myKMeans(data,3)


result = result$result
du = result[,-c(3)]
# you can subset data by cluster id
du.cl1 = subset(result, result$clusters == 1)
du.cl2 = subset(result, result$clusters == 2)
du.cl3 = subset(result, result$clusters == 3)

# you can plot the data
# first plot all data point

x1 = min(result[,1]) - 10
x2 = max(result[,1]) + 10
y1 = min(result[,2]) - 10
y2 = max(result[,2]) + 10

plot(du, col=c(1), main = "Cluster Distributions", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19), sub="bkakran K-Means")

# you should do this in a for loop: for (i in 1:3)
i = 1
points(du.cl1, col=c(i+1), pch=c(1+i))
points(du.cl2, col=c(i+2), pch=c(2+i))
points(du.cl3, col=c(i+3), pch=c(3+i))