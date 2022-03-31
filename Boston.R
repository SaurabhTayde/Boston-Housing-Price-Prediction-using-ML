#Boston database is regression tree database

setwd('G:/DSP/Decision Trees Practice/Boston')
getwd()

boston = read.csv('boston.csv')

str(boston)

#Here MEDV is the output /target variable i.e price of the house to be predicted

#Let's first see how the points are laid out using the plot commands

plot(boston$LON, boston$LAT)


#we also have the Charles river attribute(CHAS),
#we want to also show all the points that lie along the Charles River in a blue colour.

points(boston$LON [boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = 'BLUE', pch=19)

#The air pollution variable in the data is NOX. Let's have a look at a distribution of NOX.

summary(boston$NOX)  #Mean value = 0.55

#let's just use the value of 0.55, as it is the centre most value.
#Let's look at the tracts that have above-average pollution.

points(boston$LON [boston$NOX >= 0.55], boston$LAT [boston$NOX >= 0.55], col = 'GREEN', pch =20)

#Now it kind of makes sense, since, the area most densely polluted,
#is the one which is also most densely populated.

#Now let us look at how the prices vary over the area as well.
#We can do this with the help of MEDV variable using the same methodology as done when plotting the pollution.

plot(boston$LON, boston$LAT)

summary(boston$MEDV)  #Mean = 22.53

points(boston$LON [boston$MEDV >= 22.53], boston$LAT [boston$MEDV >= 22.53], col = 'red',  pch =20)


#Lets try to use linear regression to the model just for testing purpose.

plot(boston$MEDV, boston$LON)
plot(boston$MEDV, boston$LAT)

#So we can see that in above two plot there's no linear plot.
#Lets verify what output does Linear model gives

latlonlm = lm(MEDV ~ LAT + LON, data = boston)

summary(latlonlm)

#So adjusted R is 0.10 only. Which shows linear model is not performing well here.



# Visualizing regression output:

plot(boston$LON, boston$LAT)

points(boston$LON [boston$MEDV >= 21.2], boston$LAT [boston$MEDV >= 21.2], col ='red', pch = 20)

latlonlm$fitted.values

points(boston$LON [latlonlm$fitted.values >= 21.2], boston$LAT [latlonlm$fitted.values >=21.2], col = 'blue', pch = '$')

#The blue $ and the red dots do not overlap especially in the east.
#It turns out, the linear regression model isn't really doing a good job.

#Applying Regression Trees to the problem :

library(rpart)

#install.packages('rpart.plot')
library(rpart.plot)


#CART Model:
#We would be predicting MEDV as a function of latitude and longitude, using the boston dataset.

latlontree = rpart(MEDV ~ LAT + LON, data = boston)

latlontree

#plot(latlontree)

#text(latlontree2, use.n = T, cex = 0.8)


# Plot the tree using prp command defined in rpart.plot package
prp(latlontree)

#Now, Let us visualise the output.We'll again plot the points with above median prices just like in Linear Regression

# Visualize output
plot(boston$LON, boston$LAT)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(latlontree)

points(boston$LON[fittedvalues>21.2],boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

#The Regression tree has done a much better job and has kind of overlapped the red dots

#But the tree obtained was very complicated and was overfitted.
#How to avoid overfitting? By changing the minbucket size.
#So let's build a new tree using the rpart command again.

latlontree2 = rpart(MEDV~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree2)
text(latlontree2, use.n = T, pretty = 0, cex = 1.0)

#Above part was completely explained to show that regression tress are better than linear regression


#Now we will focus on regression trees

#****************Prediction with Regression Trees*****************

library(caTools)

set.seed(123)

split = sample.split(boston$MEDV, SplitRatio = 0.7)

train = subset(boston, split == T)
test = subset(boston, split = F)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE
             + DIS + RAD + TAX + PTRATIO, data=train)

summary(tree)

prp(tree)

#OR

plot(tree)

text(tree, use.n = T, pretty = 0, cex = 0.8)

#Regression Tree Predictions

tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

#col1 = data.frame { = c(test$MEDV}

#TO check min max accuracy:

col1 = test$MEDV

col2 = tree.pred
 
bind = data.frame(col1, col2)
bind

min_max_accuracy2 <- mean(apply(bind, 1, min) / apply(bind, 1, max))  

#min_max_accuracy2 = 86.65 %
 