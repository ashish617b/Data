#Ashish bandgar 
#TASK 1
#1.
#Recall the example that focused on the 'iris' dataset. 
#Using this dataset, calculate the three separate correlation 
#matrices with the four variables, which correspond to the 
#three levels in the Species factor.
#use cor example from Step 1 and be sure to review help for cor and cov
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])
#cor is a function in R to calculate correlation.
#The correlation between Sepal.Length and Petals.Length is 0.8717538
#so, there is a positive correlation between them.
#Sepal.Width is correlated to Sepal.Width with a factor of -0.1175698 
#so, there is a negative correlation between them.
#Sepal.Width and Petal.Length are correlated by a factor of -0.4284401
#hence, they are negatively correlated.
#Sepal.Length and Petal.Width are positively correlated by a factor of 0.8179411.
#
#2. Is the previous assertion that Petal.Length
#is highly positively correlated with Petal.Width justified?
#Yes, Petal.Length is highly positively correlated with Petal.Width as
#its correlation is 0.9628654 which is very close to 1.
#Hence, we can say that they are highly positively correlated.
#
#3. Is the previous assertion that Petal.Length
#is highly positively correlated with Sepal.Length justified?
#Yes, Petal.Length is highly positively correlated with Sepal.Length as
#its correlation is 0.8717538 which is very close to 1.
#Hence, we can say that they are highly positively correlated.
#
#4. Is the previous assertion that Petal.Length
#and Petal.Width are negatively correlated with Sepal.Width justified?
#The correlation between Petal.Length and Sepal.Width is -0.4284401.
#Hence, it is negatively correlated.
#The correlation between Petal.Width and Sepal.Width is -0.3661259.
#Hence, it is negatively correlated.
#
#5. Is the previous assertion that Sepal.Length and Sepal.Width
#are uncorrelated (but negatively correlated, if at all) justi???ed?
#The correlation between Sepal.Length and Sepal.Width is -0.1175698.
#This is very close to 0. Hence we can conclude that they are uncorrelated.
#
#6. What conclusions can you draw from the results of the correlation?
#Looking at the results here, we can see that the Sepal.Length and Petal.Width
#are highly correlated.
#But, Sepal.Length is negatively correlated with Sepal.Width.
#The correlation between Sepal.Length and Sepal.Width is -0.1175698.
#This is very close to 0. Hence we can conclude that they are uncorrelated.
#Sepal.Width is negatively correlated with all the other attributes.
#
#TASK 2
#1. Complete EDA on the CO2 file that comes with R
#start by looking up CO2 in R help
#then examine CO2
CO2
#and create data frame CO2df
CO2df <- CO2
CO2df
#first plan your approach to EDA
#then using examples from class, including Step 1 and 2 EDA,
#explore the data and prepare a report on your findings
#remember to standardize and/or bin numeric variables
#if necessary
#we are looking for the affect on CO2 update of chilling as
#compared to nonchilling
#turn in your report as a PDF
#and your R code file
summary(CO2)

hist(CO2$conc)
hist(CO2$uptake)

plot(density(CO2$conc)) 
plot(density(CO2$uptake)) 

table(CO2$Plant) 
pie(table(CO2$Plant)) 
barplot(table(CO2$Plant)) 

cov(CO2$conc, CO2$uptake)
cov(CO2[,4:5])

cor(CO2$conc, CO2$uptake)
cor(CO2[,4:5])

aggregate(uptake ~ Type, summary, data=CO2)

boxplot(uptake ~ Type, data=CO2) 

with(CO2, plot(uptake, conc, col=Type, pch=as.numeric(Type)))

pairs(CO2) 
library(scatterplot3d)
scatterplot3d(CO2$uptake, CO2$Type, CO2$conc)
distMatrix <- suppressWarnings(as.matrix(dist(CO2[,1:4])))
heatmap(distMatrix)

parallelplot(~CO2[1:4] | Treatment, data=CO2)
library(ggplot2)
qplot(uptake, Treatment, data=CO2, facets=Plant ~.)

CO2.chilled <- subset(CO2,CO2$Treatment == "chilled")
CO2.nonchilled <- subset(CO2,CO2$Treatment == "nonchilled")
t.test(CO2.chilled$uptake,CO2.nonchilled$uptake)

fit <- lm(CO2$uptake ~ CO2$conc)
summary(fit)

set.seed(3147)
x <- CO2$uptake

summary(x)
boxplot(x)  

y <- CO2$conc
df <- data.frame(x, y)
rm(x, y)
attach(df)
# find the index of outliers from x
(m <- which(x %in% boxplot.stats(x)$out))
# find the index of outliers from y
(n <- which(y %in% boxplot.stats(y)$out))
detach(df)
(outlier.list1 <- intersect(a,b))
plot(df)
points(df[outlier.list1,], col="red", pch="+", cex=2.5)
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)
print(CO22[outliers,])
n <- nrow(CO22)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(CO22), cex=.8, xlabs=labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(CO22, pch=pch, col=col)

CO22 <- CO2[,4:5]
kmeans.result <- kmeans(CO22, centers=3)
# cluster centers
kmeans.result$centers
kmeans.result$cluster
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((CO22 - centers)^2))
# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]
# who are outliers
print(outliers)
print(CO22[outliers,])

# plot clusters
plot(CO22[,c("conc", "uptake")], pch="o", col=kmeans.result$cluster, cex=0.3)
# plot cluster centers
points(kmeans.result$centers[,c("conc", "uptake")], col=1:3, pch=8, cex=1.5)
# plot outliers
points(CO22[outliers, c("conc", "uptake")], pch="+", col=4, cex=1.5)

# plot cluster centers
points(kmeans.result$centers[,c("conc", "uptake")], col=1:3, pch=8, cex=1.5)
# plot outliers
points(CO22[outliers, c("conc", "uptake")], pch="+", col=4, cex=1.5)