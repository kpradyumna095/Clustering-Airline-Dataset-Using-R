airline <- read.csv(file.choose())
View(airline)

#Normalizing continuous columns to bring them under same scale
normalized_data<-scale(airline[,2:12]) #excluding the university name columnbefore normalizing

d <- dist(normalized_data, method = "euclidean") # distance matrix

fit <- hclust(d, method="complete")

plot(fit) # display dendrogram

plot(fit, hang=-1)

rect.hclust(fit, k=10, border="red")

groups <- cutree(fit, k=10) # cut tree into clusters
str(groups)
membership<-as.matrix(groups) # groups or cluster numbers

final <- data.frame(airline, membership)

View(final)

aggregate(airline[,-1],by=list(final$membership),mean)

# as dataset containing large no of observations we are not able to decide no.of clusters so we will go for 
# kmeans clustering
install.packages("kselection")
library(kselection)
#k selection method 
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)
k <- kselection(airline,parallel = TRUE, k_threshold = 0.85)  
k
#from k selection it shows that 2 clusters is optimum no. 

# choosing best clusters as 2 
k_2 <- kmeans(normalized_data,2)
str(k_2)
View(k_2)
airline$cluster <- as.matrix(k_2$cluster)
View(airline)
aggregate(airline[,-1],by=list(airline$cluster),mean)

# scree plot for selection of no.of clusters
twss <- NULL
for (i in 2:14){
  twss <- c(twss, kmeans(normalized_data,i)$tot.withinss)
}

plot(2:14, twss, type="b", xlab = "Number of Clusters", ylab = "within groups sum of squares", main ="screeplot")
#from scree plot first elbow bend is at 4 so we can say that we have to go with 4 no. of clusters

# choosing best clusters as 4 
k_4 <- kmeans(normalized_data,4)
str(k_4)
View(k_4)
airline$cluster <- as.matrix(k_4$cluster)
View(airline)
aggregate(airline[,-1],by=list(airline$cluster),mean)

# by forming 4 clusters value of total withiness is reduced and betweeness is increased but we will check that once by second elbow turn at 5
# by considering 5 clusters
# choosing best clusters as 4 
k_5 <- kmeans(normalized_data,4)
str(k_5)
View(k_5)
airline$cluster <- as.matrix(k_5$cluster)
View(airline)
aggregate(airline[,-1],by=list(airline$cluster),mean)
# by forming 5 clusters value of total withiness is reduced slightly and betweeness is increased slightly but we will check that once by second elbow turn at 5
# so there is no need to do seperate cluster for slight change so we will finalise with 4 no. of clusters

#Cluster 1 has the largest average values in Qualmiles,Bonus_miles,Bonus_trans,Flight_miles_12mo,Flight_trans_12
#Cluster 1 are infrequent but loyal customers. Mostly contains customers with few miles, but who have been with the airline the longest.
#Customers in cluster1 mostly preferred to travel in last 12 months,Day_since_enroll.
#Cluster 2 has the largest average values in Balance,cc1_miles,Day_since_enroll.
#these customers are oldest one and used our service less in last 12 months
#Cluster 3 are frequent user of our airline.as they are not new and uses frequently our service we need to give them promotinal packages.
#Cluster 4 are newest customers of our airline as value of day_since_enroll is having less value.

