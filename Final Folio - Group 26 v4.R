#########################################
## Disclaimer
#########################################

# loans-script-1.R - Script for initial exploration of loans.csv data
# Copyright (c) 2021 by R. Ravi (ravi@cmu.edu).  Distributed under license CC BY-NC 4.0
# To view a copy of this license, https://creativecommons.org/licenses/by-nc/4.0/

#########################################
## Read file
#########################################

# Ensure that loans-default.csv is in the same directory as this R script
# Then use the Session menu item in RStudio to set working directory to source file location
loans.full <- read.csv("loans-default.csv")

# Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)
# This is a good place to make sure you understand what the different fields mean
# E.g., the fraction of defaulters in the data set can be computed by taking the mean
mean(loans.full$default)

#########################################
## Descriptive statistics and graphics ##
#########################################

# Single variable summary of continuous variables
boxplot(loans.full$fico,horizontal = TRUE,xlab="FICO")
boxplot(loans.full$int.rate,horizontal = TRUE,xlab="int.rate")
boxplot(loans.full$installment,horizontal = TRUE,xlab="installment")
boxplot(loans.full$days.with.cr.line,horizontal = TRUE,xlab="days.with.cr.line")
boxplot(loans.full$revol.bal,horizontal = TRUE,xlab="revol.bal")
boxplot(loans.full$revol.util,horizontal = TRUE,xlab="revol.util")
boxplot(loans.full$inq.last.6mths,horizontal = TRUE,xlab="inq.last.6mths")
boxplot(loans.full$delinq.2yrs,horizontal = TRUE,xlab="delinq.2yrs")
boxplot(loans.full$pub.rec,horizontal = TRUE,xlab="pub.rec")

#Cross tabulate how the target variable and other categorical variables are related
xtabs(~credit.policy+default,data=loans.full)
xtabs(~purpose+default,data=loans.full)

# Simple histograms of the default variable versus categorical variables
# barplot is called on the table command which automatically bins the variable given to it 
# This first one shows the histograms of the defaulters
barplot(table(loans.full$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))

#... and how the defaulters varies with the second coordinate, the credit policy
barplot(table(loans.full$default, loans.full$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

# !!!
# You can adapt the above command to visually represent the variation of defaulters with other categorical variables
#   like delinq.2yrs, loan purpose, inq.last.6mnths etc. that have few values represented

barplot(table(loans.full$purpose),
        main="Loan purpose",
        xlab="Purpose",
        border="black", 
        col=c("green","red"))

barplot(table(loans.full$default, loans.full$purpose),
        main="Default Distribution by Loan Purpose",
        xlab="Loan purpose",
        col=c("green","red"))

# Box-and-whisker plots to see dispersion of continuous vars across default and not default
# boxplot uses the formula syntax, with LHS being the y-var (vertical) and RHS being the x-variables (horizontal)
boxplot(loans.full$int.rate ~ loans.full$default,main="Interest Rates across Default")

# !!!
# You can adapt the above command to visually represent the variation of defaulters with other continuous variables
#   like installment, log.annual.inc, dti, fico, days with credit line, revolving credit balance and fraction of
#   revolving credit utilized
boxplot(loans.full$log.annual.inc ~ loans.full$default,main="Log.Annual.Inc across Default")

#compute correlation matrix among numerical attributes in one large matrix
# cor is the command for computing correlations, 3:14 indicate the column numbers 3 to 14 that have numerical 
# values and can be used to compute correlations
(correlations <- cor(loans.full[,3:14]))
# If you also want to include the first column which is numerical, you can concatenate the first row in the list
#  of columns using the c() command for concatenation
(correlations <- cor(loans.full[,c(1,3:14)]))

#Install a package for plotting correlations and include it
if (!require(corrplot)) {install.packages("corrplot"); library(corrplot)}
#Generate a heat map of correlated predictors
#  (the hclust parameter orders the rows and columns according to a hierarchical clustering method)
corrplot(correlations, order="hclust")

#NOTE: correlation groups are: 
## Correlated groups in the Correlation matrix:
## Group 1: Fico, dti, int.rate and revol.util
## Group 2: log.annual.inc, installment, revol.bal and days.with.cr.line
## Group 3: Installment, tot.payment, principal, and interest

## Adding derived variables

# The total payment on a 36 month loan if no default
loans.full$tot.payment = 36*loans.full$installment
# We use the compound interest formula for 3 years at the given interest rate to calculate the principal
loans.full$principal = loans.full$tot.payment/exp(3*loans.full$int.rate)
loans.full$interest = loans.full$tot.payment - loans.full$principal

#Save result with additional columns as a csv file?
write.csv(loans.full,row.names=FALSE,file ="loans-kpi.csv")

#########################################
## Descriptive analysis ##
#########################################

# !!! TO DO in the script 
# A1. K Means: Finalize the set of variables by examining the clusters against informative categorical variables
# A2. K Means: Finalize your value of K for cluster analysis B
# A3. K Means: name your clusters by varying the list of variables for the parallel lines plot and using the resulting plot
# B1. PCA: Finalize the set of variables by avoiding highly correlated variables
# B2. PCA: Bottom up (model-driven) analysis of the PCs
# B3. PCA: Top down (data-driven) analysis of the PCs

## K-Means clustering           

# variable list for clustering
varlist=c("credit.policy","log.annual.inc","fico","inq.last.6mths","delinq.2yrs","pub.rec","default")  

# !!! A1
# Modify the variable list above to remove redundant variables that capture similar information, 
#  and others variables that may skew the clustering results in your opinion.

# Note from the correlation plot that fico, dti, int,rate and revol.util for a correlated group and
# log.annual.inc, installment revol.bal and days.with.cr.line another such group
# You could pick one from each group for the cluster analysis, and leave out credit.policy and default
#  since they are behaviors encoded after the fact of the loan application
varlist=c(
  "credit.policy",
  #"int.rate",
  #"installment",
  "log.annual.inc",
  "dti",
  "fico",
  #"days.with.cr.line",
  #"revol.bal",
  #"revol.util",
  "inq.last.6mths",
  "delinq.2yrs",
  "pub.rec"
  #"default",
  #"tot.paymnt",
  #"principal",
  #'interest'
)   

xloan = scale(loans.full[varlist])

### k-means cluster with k=5
(grpA=kmeans(xloan,centers=5))
# Examine how the clusters split the loans that obey the credit policy versus those that do not
#  Are your clusters helpful in predicting the credit.policy variable?
xtabs(~loans.full$credit.policy+grpA$cluster)

# !!! A1
# Examine how the clusters split other categories to see if they correlate with the category
# E.g. by tabulating with default behavior, you can check if the clusters split loans into those
#  more or less prone to default.
xtabs(~loans.full$default+grpA$cluster)

## @scree plot to determine how many clusters to use by creating kmeans solutions
## with k from 2 to 30 and then we can plot the sum of square errors to 
## understand how much variation each solution explains

# compute multiple cluster solutions
set.seed(124895792)  # set the random number seed so the samples will be the same if regenerated
grpA2=kmeans(xloan[],centers=2,nstart=30)  # include nstart=30 as an option to evaluate 30 starting points and return best 
grpA3=kmeans(xloan[],centers=3,nstart=30)
grpA4=kmeans(xloan[],centers=4,nstart=30)
grpA5=kmeans(xloan[],centers=5,nstart=30)
grpA6=kmeans(xloan[],centers=6,nstart=30)
grpA7=kmeans(xloan[],centers=7,nstart=30)
grpA8=kmeans(xloan[],centers=8,nstart=30)
grpA9=kmeans(xloan[],centers=9,nstart=30)
grpA10=kmeans(xloan[],centers=10,nstart=30)
grpA15=kmeans(xloan[],centers=15,nstart=30)
grpA20=kmeans(xloan[],centers=20,nstart=30)
grpA30=kmeans(xloan[],centers=30,nstart=30)

# compute between and within SS
kclust=c(2:10,15,20,30)
bss=c(grpA2$betweenss,
      grpA3$betweenss,grpA4$betweenss,grpA5$betweenss,grpA6$betweenss,
      grpA7$betweenss,grpA8$betweenss,grpA9$betweenss,grpA10$betweenss,
      grpA15$betweenss,grpA20$betweenss,grpA30$betweenss)
wss=c(grpA2$tot.withinss,
      grpA3$tot.withinss,grpA4$tot.withinss,grpA5$tot.withinss,grpA6$tot.withinss,
      grpA7$tot.withinss,grpA8$tot.withinss,grpA9$tot.withinss,grpA10$tot.withinss,
      grpA15$tot.withinss,grpA20$tot.withinss,grpA30$tot.withinss)
# plot the results and look for the "Hockey-Stick" effect
par(mfrow=c(1,1))
plot(kclust,wss,type="l",main="Within SS for k-means")  # Within SS is variation of errors
points(kclust,wss)
# alternatively we can look at Between SS or R-Squared, notice Between+Within=Total, R-Squared=1-Within/Total,
# Total does not change with k.  The difference is the these graphs go up with fit.
plot(kclust,bss,type="l",main="Between SS for k-means") # Between SS is variation of predictions
points(kclust,bss)
plot(kclust,bss/(wss+bss),type="l",main="R-Squared for k-means")  # R-Squared is ratio of explained variation
points(kclust,bss/(wss+bss))

#NOTE: k = 4 is optimal no. of clusters to as wss value does not change as significantly with cluster centroids.


### cluster analysis
### Notice that the large number of variables may make interpretation difficult

# !!! A2 set your k value 
k=4

# compute a k-means cluster with k 
set.seed(1248765792)  # set the random number seed so the samples will be the same if regenerated
(grpB=kmeans(xloan,centers=k))

# !!! A3. set your cluster names !!,  update these after you have summarized the clusters below
knames=as.character(1:k)  # default is just name them 1, 2, ...
knames=c("1:Target","2:HighRisk","3:CreditWorthy","4:Recheck")    # edit the string, make sure you have one label for each kvalue

# plot the solutions against the credit.policy and inquiries in last 6 mos
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow=c(1,1),mar=c(5,4,4,1)+.1)
plot(jitter(xloan[,"credit.policy"]), jitter(xloan[,"inq.last.6mths"]),xlab="cr.pol",ylab="inq6m",col=grpB$cluster)
points(grpB$centers[,c("credit.policy","inq.last.6mths")],col=1:k,pch=8,cex=2)
legend("topright",pch=8,bty="n",col=1:k,knames)

# !!! A3 try different variables instead of these two 
plot(xloan[,"fico"],(xloan[,"dti"]),xlab="fico",ylab="dti",col=grpB$cluster)
points(grpB$centers[,c("fico","dti")],col=1:k,pch=8,cex=2)
legend("topleft",pch=8,bty="n",col=1:k,knames)

# compare the cluster solutions with loan purpose
(result = xtabs(~loans.full$purpose+grpB$cluster) )  # xtabs using formula notation unlike table

# slightly nicer cross tabulation
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}
CrossTable(loans.full$purpose,grpB$cluster)  

# Here is a more visual representation of a table with a BalloonPlot
if (!require(gplots)) {install.packages("gplots"); library(gplots)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
par(mfrow=c(1,1))  # reset to one plot in panel
balloonplot(result)

# summarize the centroids
round(grpB$centers,2)   # print the centroid values for each variable
# create a parallel plot to visualize the centroid values (scales cex changes the size of text font)
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
parallelplot(grpB$centers,varnames=varlist,auto.key=list(text=knames,space="top",columns=1,lines=T),scales=list(cex=.5))

# a parallel plot with just a few variables, since the previous plot is quite dense; 
# !!! A3. Try different lists of variables 
# Use the separation of the cluster centers among your chosen variables to "name" these clusters
shortvarlist=c("fico","dti","log.annual.inc")   # short list of variables, note: add variables if you like
#round(grpB$centers[,shortvarlist],2)
parallelplot(grpB$centers[,shortvarlist],varnames=shortvarlist,auto.key=list(text=knames,space="top",columns=3,lines=T))
# let's do a pairwise plot with the short list of variables
pairs(sapply(loans.full[shortvarlist],jitter,amount=.2),col=grpB$cluster)

# to save the data to CSV files, useful for importing into Excel for analysis
write.csv(grpB$cluster,file="Loans_ResultsBcluster.csv")  # cluster assignments
write.csv(grpB$centers,file="Loans_ResultsBcenters.csv")  # cluster centroids


# PCA
# Principal component analysis of the numerical columns in the data

# !!! B1.  Recall that the varlist defined above is also used here for the PCA.
# Same caveat applies: remove duplicate copies of the same signal in the data
# if you think some of the variables are highly correlated
# If you want to use a different set of variables for PCA than for the cluster analysis,
#  define a second varlist 'varlist2' using a command like
#   varlist2=c("credit.policy","int.rate","installment","log.annual.inc","dti","fico","days.with.cr.line","revol.bal","revol.util","inq.last.6mths","delinq.2yrs","pub.rec","default")  
#  (Remove what you don't want to include) and use that instead of 'varlist' below

# variable list for clustering
varlist=c("credit.policy",  "log.annual.inc", "dti", "fico", "inq.last.6mths",  "delinq.2yrs", "pub.rec")
#NOTE: We only considered 1 variable from each set correlation group (see note above)

# !!! A1
# Modify the variable list above to remove redundant variables that capture similar information, 
#  and others variables that may skew the clustering results in your opinion.

# Note from the correlation plot that fico, dti, int,rate and revol.util for a correlated group and
# log.annual.inc, installment revol.bal and days.with.cr.line another such group
# You could pick one from each group for the cluster analysis, and leave out credit.policy and default
#  since they are behaviors encoded after the fact of the loan application
#NOTE: We only considered 1 variable from each set correlation group (see note above)
varlist=c(
  "credit.policy",
  #"int.rate",
  #"installment",
  "log.annual.inc",
  "dti",
  "fico",
  #"days.with.cr.line",
  #"revol.bal",
  #"revol.util",
  "inq.last.6mths",
  "delinq.2yrs",
  "pub.rec"
  #"default",
  #"tot.paymnt",
  #"principal",
  #'interest'
)  

xloan = scale(loans.full[varlist])

pcs <- prcomp(loans.full[,varlist],scale=TRUE)
summary(pcs)
print(pcs)
plot(pcs, main="")
mtext(side=1, "Loan Data Principal Components",  line=1, font=2)

# The stdevn of each component 
pcs$sdev
# squaring which gives the variance explained by each of these components
pr.var=pcs$sdev^2
pr.var
# Percentage of variance explained by each component can be obtained by normalizing 
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component",  ylim=c(0,1), col="brown3")

#NOTE: Through the proportion of variance explained charts, we concluded that over 75% of behavior is explained through PC1, PC2, PC3, and PC4
#Top down analysis conclusions: 
# - PC1 is most highly loaded by credit.policy, fico, inq.last.6mths and denotes creditworthiness
# - PC2 is most highly loaded by  log.annual.inc, inq.last.6mths, and dti.
# - PC3 determined by delinq.2yrs
# - PC4 determined by pub.rec

# For a bottom-up interpretation of the PCs, examine how each PC combines the variables
# For this, examine the loadings of the top two PCs on the variables
t(round(pcs$rotation[,1:2],2) )

# create heatmap
if (!require(corrplot)) {install.packages("corrplot")}
library(corrplot)
corrplot(pcs$rotation,main="Correlation of each feature on PC's")
# !!!  B2.
# Use loadings and their heatmap above to construct a bottom-up interpretation

# For a bottom-up (or model-driven) interpretation of the first two PCs find extreme data points in the biplot
#  and try to identify their characters to "label" or "name" the PCs from your 
# understanding of those data points

# Smaller subset of data for biplot
loans.sample <- loans.full[sample(nrow(loans.full),size=500),]
pcs.sample <- prcomp(loans.sample[,varlist],scale=TRUE)
summary(pcs.sample)
print(pcs.sample)
biplot(pcs.sample, scale=0)

# !!! B3. Examine the biplot to find outermost points, examine them and use to 
#   make sense of the axes in a top-down (or data-driven) way

#Note: bottom-up interpretation:
# - Reinforces what we found in the top-down approach
# - PC1 loaded with fico, credit policy and inq.last.6mnths
# - PC2 determined by log.annual.inc, inq.last.6mths

#@descale
### summarize the centroids on the original scale instead of standardized

# setup
if (!require(lattice)) {install.packages("lattice"); library(lattice)}

# let's save the mean and std dev so we can reverse the scaling
xloans.mean=colMeans(loans.full[varlist])
xloans.sd=apply(loans.full[varlist],2,sd)

# translate the centers back to the original scale
grpBcenter.orig=sweep(grpB$centers,MARGIN=2,xloans.sd,'*')       # step 1) scale: multiply the corresponding sd
grpBcenter.orig=sweep(grpBcenter.orig,MARGIN=2,xloans.mean,'+')  # step 2) shift: add the original mean
# print the Centers on the original scale
print(t(grpBcenter.orig))
# summarize the centroids with parallel plot
parallelplot(grpBcenter.orig,auto.key=list(text=as.character(1:3),common.scale=TRUE,space="top",columns=3,lines=T))


#@moreclusterplots
### understand differences within the clusters (not just focus on mean
### but also look at the variance within the cluster) using boxplot, violinplot
### or simple descriptive statistics

# setup (useful for easier descriptions of each group)
if (!require(psych)) {install.packages("pysch"); library(pysch)}

# copy the labels from your clustering scheme (if you use a different cluster value define it here)
mycluster=grpB$cluster

# compute a table of descriptive statistics of standardized data for each cluster
describeBy(xloan,group=mycluster)  # notice this gives the mean and standard deviation of each cluster

# alternatively let's compute a boxplot for a selected variable to understand the distribution within each cluster
par(mfrow=c(1,1),mar=c(4,4,1,1))  # setup margins for one panel
boxplot(loans.full$fico~mycluster,data=loans.full,notch=TRUE,xlab="Cluster",ylab="fico")  # the notch gives the standard error of the mean
# the for loop goes across every variable (if you only want to have some variables replace c() with c("fico","dti")) or the list you want
par(mfrow=c(1,1),mar=c(3,4,1,1))  # mfrow=c(4,1) tells R to plot 4 rows and 1 column, and mar is the margin
for (i in varlist) {
  boxplot(loans.full[,i]~mycluster,data=loans.full,notch=TRUE,xlab="Cluster",ylab=i)  # the notch gives the standard error of the mean
}
par(mfrow=c(1,1))  # reset to one graph in the panel

# another way to do this is using a violin plot  (vioplot is in base r but violin plots in ggplot is more flexible)
# violin plots give smooth versions of the histogram and give more information than boxplots
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
par(mfrow=c(1,1),mar=c(4,4,1,1))  # setup margins for one panel
ggplot(loans.full, aes(factor(mycluster), dti)) + geom_violin() + geom_jitter(height = 0, width = 0.1)
ggplot(loans.full, aes(factor(mycluster), fico)) + geom_violin() + geom_jitter(height = 0, width = 0.1)

#@hierclust
### compute a hierarchical cluster
### this is an alternative clustering technique than k-means
### it works by starting with all observations in different clusters and then
### progressing by combing observations that look similar into new clusters

# try a hierarchical cluster on a subset of the loans
par(mfrow=c(1,1))
(grphQ=hclust(dist(xloan[subset=sample(nrow(xloan),100)]),method="complete"))
plot(grphQ,cex=.7)

# try a hierarchical cluster of the columns
(grphP=hclust(dist(as.matrix(t(xloan),method="complete"))))
plot(grphP,cex=.7)

###############################################
## Predictive analysis - Logistic Regression ##
###############################################

# !!! TO DO in the script !!!
# 1. Select variables to use in your final logistic regression model by removing correlated variables
#      or using your insights about relations between the variables from earlier weeks, or simply by
#      using the automatic way of running a stepwise regression and checking its output
# 2. Add more visualizations of the effect of the variables in your logistic regression model on the
#      probability of default and the log odds ratio of defaulting.
# 3. Use the spreadsheet output with the logistic regression model parameters to 
#   (a) identify the significant variables,
#   (b) provide a verbal interpretation of the coefficients and 
#   (c) rank the variables by their order of importance

## Prepare training and testing sets for modeling ##

# Prepare new values in 'randvalue' using a random uniform number generator
# Each record in it has a corresponding uniform random value which will be used 
# to decide if the obs is assigned to the training or test sample
Nfull <- nrow(loans.full)
randvalue <- runif(Nfull)
trainsample <- randvalue < .7
testsample <- (randvalue >= .7)
# Note that changing the .7 above to .8 will make the training set to go from
#  70% to 80% of the data
# Use the subsets to differentiate the training and testing data sets
train <- loans.full[trainsample,]
test <- loans.full[testsample,]

train$default <- as.factor(train$default)
test$default <- as.factor(test$default)

#########################
## Logistic Regression ##
#########################

# Basic Logistic regression model using all columns
lrfit = glm(default ~ ., data=train, family="binomial")
summary(lrfit)
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# !!! Select variables
# Note that there are several highly correlated variables so you can now rebuild the LR model
#  by excluding some the obviously correlated variables. Here is an example excluding the total payment
#  that you can modify to remove others. You can use your knowledge of the problem and your initial 
#  hunch from weeks 1 and 2 in this variable selection exercise
lrfit2 = glm(default ~ . - tot.payment -credit.policy, data=train, family="binomial")

####################################################
# Stepwise Regression
# Alternately you can run a step-wise regression to pick variables for you automatically 
# first estimate the null model (this just has an intercept)
null = glm(default~1,data=train,family="binomial")
# second estimate a complete model (with all variables that you are interested in)
full = glm(default~.,data=train,family="binomial") 
# finally estimate the step wise regression starting with the null model
# The next command can take some time to find the best set of 
#   predictors to include in a greedy fashion
# The method evaluates all possible choices of next predictor to add,
#  and chooses greedily among them; it starts with null and stops if it reaches full
#  The steps parameter limits the number of steps, which you can increase
#  The dir is the direction which is forward starting from the null model
swlrmdl = step(null, scope=formula(full),steps=15,dir="forward")  # can increase beyond 15 steps, just takes more time
summary(swlrmdl)

################################
# You can use the stepwise model (or your own model with carefully selected variables)
lrfit = swlrmdl

lrfit3 = glm(default ~ . - tot.payment -int.rate - installment - dti - days.with.cr.line - revol.bal - days.with.cr.line - revol.util - delinq.2yrs -pub.rec, data=train, family="binomial")

lrfit3 = glm(default ~ fico + purpose + inq.last.6mths + principal + log.annual.inc + revol.bal, data=train, family="binomial")
summary(lrfit3)

######################################
# Estimate the AUC (Area Under Curve)
#NOTE: for the stepwise regression model
# Predict the probability of not fully paying back using lrmodel and test set
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

#NOTE: AUC is 0.6855861 for the stepwise regression model

######################################
# Estimate the AUC (Area Under Curve)
#NOTE: for the lrfit3, or variables selected by the team
# Predict the probability of not fully paying back using lrmodel and test set
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
lr.default.prob3 = predict(lrfit3, newdata=test, type="response")
pred.lr3 = prediction(lr.default.prob3, test$default)
perf.lr3 = performance(pred.lr3, measure = "tpr", x.measure = "fpr") 
plot(perf.lr3, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp3 = performance(pred.lr3,"auc")
(auc.lr3 = as.numeric(auc.tmp3@y.values))

#NOTE: AUC is 0.6833848 for the lrfit3 model, lower than the stepwise regression model

####################################################
# Visualization of results against variables

# Plot the response of the model as a function of each of the other variables while holding
#  all other variables at their emedian values
if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
plotmo(lrfit)   # evaluates selected input but holds other values at median

# Use a regression visualization package
if (!require(visreg)) {install.packages("visreg"); library(visreg)}  # visualize regression
# plot the log of the odds ratio of Default as function of a variable
visreg(lrfit,"log.annual.inc",ylab="Log(OddsRatio of Default)")
visreg(lrfit,"fico",scale="response",ylab="Pr(Default)")
# create a contour plot to visualize two effects at the same time 
visreg2d(lrfit,"fico","log.annual.inc",plot.type="image",main="Log(OddsRatio of Default)")
visreg2d(lrfit,"fico","log.annual.inc",scale="response",plot.type="image",main="Pr(Default)")
# !!! In both plots for the log(Odds ratio) and Pr(default) above, change the variables to other 
#  variables in your final model to get other useful visualizations

## Spreadsheet output of regression model for further analysis:

# Export data for a simulator spreadsheet to "loans_lrmodeldata.csv"
### uses the models that were created above, so you must have trained your models
###
### the CSV file contains the:
###  a) the model parameters from our logistic regression, and
###  b) average and standard deviation of the original data

# a) retrieve coefficients from your model
coeflist=summary(lrfit)$coefficients  # extract coefficients estimates and std errors and z values
coefdata=data.frame(rn=rownames(coeflist),coeflist,row.names=NULL)  # change to dataframe
colnames(coefdata)=c("rn",colnames(coeflist))
print(coefdata)   # print out the coefficients

# b) retrieve averages and std dev across all users
modelall=model.matrix(lrfit,data=train)  # get a matrix of all data used in the model (just training sample)
meandata=apply(modelall,2,mean) # compute the average for the selected variables (the "2" means compute by column)
sddata=apply(modelall,2,sd)  # compute the standard deviation for selected variables
descdata=data.frame(rn=names(meandata),meandata,sddata,row.names=NULL)  # merge the vectors with the mean and stddev into a single dataframe
print(descdata)   # print out the descriptive values

# data manipulation package
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
# combine the data together to make it easier to dump out to a single spreadsheet
mdata=join(coefdata,descdata,type='full',by='rn')  # merge the coefficients and descriptive stats
print(mdata)    # print out the combined data

# write the data to a spreadsheet
write.csv(mdata,file="loans_lrmodeldata.csv")   

# !!! Use the output spreadsheet to (a) identify the significant variables,
#   (b) provide a verbal interpretation of the coefficients and 
#    (c) rank the variables by their order of importance
#Please refer to Final Folio - Group 26.xlsx file

###############################################
## Predictive analysis - Decision tree       ##
###############################################

# !!! TO DO in the script !!!
# 1. Run through the script two times, once with shuffling the rows before choosing the subsample and one without,
#    and decide on which run you should retain and why
# 2. Finalize the set of variables to include in the tree again based on your consideration of the 
#    meaning of the variables and how the decision tree method works to use these variables
# 3. Grow larger trees if you think your tree is not large enough to provide a fine enough explanation
#    by varying the cp parameter (Do this only if you are dissatisfied with your tree result)
# 4. Remember to cut and paste the script for the best logistic regression model from last week here.
# 5. Explore changing the cutoff for deploying your tree model (and later the logistic regression model)
#    on the test data. The script shows the accuracy and false negative rate and also outputs the loans
#    from the test set that pass this threshold.
# 6. Use the output loans based on your final chosen cutoff from the tree and LR models to do further analysis of 
#    metrics like total profit and return on invested principal.

######################################################
### Build a preliminary tree on the whole data set ###

if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}

(fulltree <- rpart(default ~ ., data=loans.full))
# Note that the parantheses around the command output the resulting object
summary(fulltree)
prp(fulltree)
#plot(ftree); text(ftree) 


# Both leaves predict that noone will default
# The whole tree just predicts that noone will default!
# The problem is that we have too few defaulters in the data
# We should preprocess the dataset by down-sampling the non-defaulters

####################################
### Down-sample the non-defaulters

# Create a smaller data set with as many samples from the non defaults as defaults
# Set the random seed so we can all get the same results
set.seed(77777)

Nfull <- nrow(loans.full)
# Uncomment the next line if you want to shuffle the data set and rerun starting from two lines above
# !!! TODO: Run through this script two times, once with the following line commented and then again with it uncommented
#    and decide on which run you should retain and why
loans.full <- loans.full[sample(Nfull),]

# Make two boolean indicator vectors called pos and neg of defaulters and non-defaulters in the data set
pos <- (loans.full$default == 1)
neg <- (loans.full$default == 0)
# Store their numbers; Remember the brackets will print them out
(np <- sum(pos))
(nn <- sum(neg))

# Save the indices of all the positive examples
idxpos=which(pos)  
# Save the indices of all the negative examples
allneg=which(neg)
# Pick out as many negative examples as np, number of positive examples
idxneg=allneg[1:np]  

# Concatenate equal number of pos and neg examples to create balanced dataset index
l = c(idxpos,idxneg)
# Check the length of l - it must be twice the size of the defaulters or 2 X 1533
length(l)

# Take the data set to only have the subset we collected in the list l
loans = loans.full[l,]
# Reset the number of observations in the dataset
N = nrow(loans)

## Repeat the histograms of the defaulters and non-defaulters after downsampling
barplot(table(loans$default),
        main="Paid versus not paid loans",
        xlab="Status",
        border="black", 
        col=c("green","red"))
#... and the distribution across categorical variables such as credit.policy
barplot(table(loans$default, loans$credit.policy),
        main="Default Distribution by Credit Policy",
        xlab="Cust meets underwriting criteria",
        col=c("green","red"))

####################################################
## Prepare training and testing sets for modeling ##

# Prepare new values in 'randvalue' using a random uniform number generator
# Each record in it has a corresponding uniform random value which will be used 
# to decide if the obs is assigned to the training or test sample
randvalue <- runif(N)
trainsample <- randvalue < .7
testsample <- (randvalue >= .7)
# Note that changing the .7 above to .8 will make the training set to go from
#  70% to 80% of the data
# Use the subsets to differentiate the training and testing data sets
train <- loans[trainsample,]
test <- loans[testsample,]

train$default <- as.factor(train$default)
test$default <- as.factor(test$default)

####################################
## Predictive Modeling with Trees ##

# Build default full tree model with all the variables, view summary and plot the tree
ftree <- rpart(default ~ ., data=train)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree); text(ftree)  # simple graph
prp(ftree)                # tree graph
prp(ftree,extra=101)      # add the size and proportion of data in the node

if (!require(plotmo)) {install.packages("plotmo"); library(plotmo)}  # show model response
plotmo(ftree)   # evaluates selected input but holds other values at median


#######################################################
##             Predicting with the tree model        ##

#The next line uses the ftree model to predict default for new test data
#  Specifying type to be "vector" outputs a probability for 0 and 1 values for default resply
#   Thus the values along the second column of this result are the probabilities of default
default.prediction.full <- predict(ftree,test,type="prob")
# The next line creates a boolean (True/False) array of those with default probability above threshold
pred.ftree50 <- (default.prediction.full[,2] > 0.5)
#The table command puts the first argument along the rows and the second along columns
table(test$default,pred.ftree50)

#######################################################
##        Calculate ROC and AUC 

if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree,test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

#NOTE: AUC  = 0.6623141
# We decided to keep the shuffled data for higher guarantee of an unbiased model

###############################################################################
###  !!! Change the set of variables in building the tree
###############################################################################
# You can try other trees, e.g. without the interest rate and credit.policy variables 
(ftree2 <- rpart(default ~ .-credit.policy, data=train))
summary(ftree2)
prp(ftree2,extra=101)

#######################################################
##        Calculate ROC and AUC 
#######################################################
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
fit.pr.tree <- predict(ftree2,test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))
#NOTE: AUC = 0.6489469

###############################################################################
#  !!! GROWING larger or smaller trees with rpart  ############################
# Note: you can change the size of the trees by changing the default value
#           cp=0.01 in the formula below
# You can try values for cp like 0.008 to get larger trees than the default
# Uncomment lines below to grow and visualize a larger tree 

ftree3 = rpart(default ~ ., data=train, control=rpart.control(cp=0.005))
summary(ftree3)
prp(ftree3)

fit.pr.tree <- predict(ftree3,test,type="prob")[,2]
# Recall that a prediction of type vector returns a matrix which has the probabilities in column 2
fit.pred.tree <- prediction(fit.pr.tree,test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree3); text(ftree3)  # simple graph
prp(ftree3)                # tree graph
prp(ftree3,extra=101)  
#Commands for calculating AUROC using ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

##
# plot all ROC curves together
ftree01 = rpart(default ~ ., data=train, control=rpart.control(cp=0.01))
ftree008 = rpart(default ~ ., data=train, control=rpart.control(cp=0.008))
ftree005 = rpart(default ~ ., data=train, control=rpart.control(cp=0.005))
ftree004 = rpart(default ~ ., data=train, control=rpart.control(cp=0.004))
ftree003 = rpart(default ~ ., data=train, control=rpart.control(cp=0.003))

fit.pr.tree01 <- predict(ftree01,test,type="prob")[,2]
fit.pred.tree01 <- prediction(fit.pr.tree01,test$default)
fit.perf.tree01 <- performance(fit.pred.tree01,"tpr","fpr")

fit.pr.tree008 <- predict(ftree008,test,type="prob")[,2]
fit.pred.tree008 <- prediction(fit.pr.tree008,test$default)
fit.perf.tree008 <- performance(fit.pred.tree008,"tpr","fpr")

fit.pr.tree005 <- predict(ftree005,test,type="prob")[,2]
fit.pred.tree005 <- prediction(fit.pr.tree005,test$default)
fit.perf.tree005 <- performance(fit.pred.tree005,"tpr","fpr")

fit.pr.tree004 <- predict(ftree004,test,type="prob")[,2]
fit.pred.tree004 <- prediction(fit.pr.tree004,test$default)
fit.perf.tree004 <- performance(fit.pred.tree004,"tpr","fpr")

fit.pr.tree003 <- predict(ftree003,test,type="prob")[,2]
fit.pred.tree003 <- prediction(fit.pr.tree003,test$default)
fit.perf.tree003 <- performance(fit.pred.tree003,"tpr","fpr")

plot(fit.perf.tree01,col="yellow"); abline(a=0,b=1)
plot(fit.perf.tree008,add=TRUE,col="blue")
plot(fit.perf.tree005,add=TRUE,col="green")
plot(fit.perf.tree004,add=TRUE,col="red")
plot(fit.perf.tree008,add=TRUE,col="black")
legend("bottomright",c("cp=0.01","cp=0.008","cp=0.005","cp=0.004","cp=0.003"),pch=15,col=c("yellow","blue","green","red",'black'),bty="n")

auc.tmp <- performance(fit.pred.tree003,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

# You can try values for cp like 0.006 and 0.005 to get larger trees than the default
# But remember the default tree is chosen to avoid overfitting!
######################################################################################

# Set the model to be the final model you want to test
rtree <- ftree3

#########################
## Logistic Regression ##
#########################

## !!! You can simply cut and paste your code for obtaining the best logistic regression model 
#   you converged on from last week here 

####################################################
# Using Stepwise Regression as a stub instead of your model
null = glm(default~1,data=loans,family="binomial")
# second estimate a complete model (with all variables that you are interested in)
full = glm(default~.- int.rate -credit.policy,data=loans,family="binomial") 
# finally estimate the step wise regression starting with the null model
swlrmdl = step(null, scope=formula(full),steps=15,dir="forward")  # can increase beyond 15 steps, just takes more time
summary(swlrmdl)

# Use the stepwise model for counterfactuals
lrfit = swlrmdl
summary(lrfit)
plotmo(lrfit)             # evaluates selected input but holds other values at median
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Estimate the AUC (Area Under Curve)
# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###############################################################################
### compare models using ROC plot
###############################################################################

# plot both ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")
###############################################################################################

#######################################################
## Counterfactual Analysis on the WHOLE data set
#
## Counterfactual on the WHOLE data set using tree
########################################################
full.loans = read.csv("loans-kpi.csv")
tree.default.prob = predict(rtree, newdata=full.loans, type="prob")
# Create the confusion matrix of the full set for tree model
(Conf.tree.tree = table(full.loans$default, tree.default.prob[,2] > 0.6))
########################################################
#!!   ^^^^^ EXPLORE HERE ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Try to modify the above code to change the cutoff from 0.5 to 
#   0.6 and 0.3 and examine the resulting confusion matrices
##############################################################

# False negative rate of the tree model on the full set
(Conf.tree.tree[2,1]/(Conf.tree.tree[1,1]+Conf.tree.tree[2,1]))

#Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#Output the subset of loans from the full set filtered by the rpart model for further ROI calc
write.csv(full.loans[tree.default.prob[,2]<0.5,],"loans_imputed_tree_05.csv")
# The csv files output have the set of loans that have been filtered according to the
#   threshold you set, and can be used to analyze profit, ROIC, market share etc. further

# Note that you can choose another threshold than 0.5 above based on the resulting confusion
#   matrices using the analysis you did in the morning, and the business KPI you care about
########################################################
## Counterfactual on the whole data set using LR
########################################################

full.lr.default.prob = predict(lrfit, newdata=full.loans, type="response")
# Create the confusion matrix of the full set
(Conf.lr.full = table(full.loans$default, full.lr.default.prob > 0.3))
########################################################
#!!   ^^^^^ EXPLORE HERE ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Try to modify the above code to change the cutoff from 0.5 to 
#   0.6 and 0.3 and examine the resulting confusion matrices
##############################################################

Conf.lr.full
# False negative rate of the lr model on the full set
(Conf.lr.full[2,1]/(Conf.lr.full[1,1]+Conf.lr.full[2,1]))

# False positive rate of the lr model on the full set
(Conf.lr.full[1,2]/(Conf.lr.full[1,2]+Conf.lr.full[2,2]))

#Baseline rate of default
(sum(full.loans$default)/nrow(full.loans))
#Output the subset of loans from the full set filtered by the lr model for further ROI calc
write.csv(full.loans[full.lr.default.prob<0.5,],"loans_imputed_lr_05.csv")
# You can again output different subsets filtered by different cutoffs modifying the above

################ Output predictions from all models for further analysis #############
# Output a final csv file that adds two columns with the default probability predicted by your models as the last two columns
write.csv(cbind(full.loans,tree.default.prob[,2],full.lr.default.prob),"loans_imputed_full_2models.csv")
###################################################################################################


###############################################################################
###  OPTIONAL: random forest and bagging

## Random Forest Model
if (!require(randomForest)) {install.packages("randomForest"); library(randomForest)} 
rf.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train)
rf.pred <- predict(rf.loans,test,type="class")
( rf.results <- table(rf.pred,test$default) )
importance(rf.loans)
varImpPlot(rf.loans)

# In a bagging model, we want to use random trees but using all variables
# so we set the number of vars to use mtry to all 10
bag.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train,mtry=10,importance=TRUE)
bag.pred <- predict(bag.loans,test,type="class")
( bag.results <- table(bag.pred,test$default) )
importance(bag.loans)
varImpPlot(bag.loans)


# plot all ROC curves together
fit.pr.tree <- predict(rtree,test,type="prob")[,2]
fit.pred.tree <- prediction(fit.pr.tree,test$default)
fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")

rf.default.prob = predict(rf.loans, newdata=test, type="prob")
pred.rf = prediction(rf.default.prob[,2], test$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr") 

plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
plot(perf.rf,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Tree", "Random Forest"),pch=15,col=c("red","blue","green"),bty="n")

###############################################
## Prescriptive analysis - Decision tree     ##
###############################################

# !!! TO DO in the script !!!
# 1. Remember to cut and paste the script for the best logistic regression model from past weeks here.
# 2. Finalize the set of variables to include in the random forest model that improves on the tree model from last week
#    again based on your consideration of the meaning of the variables and how the method works to use these variables
# 3. Pick one of the two models to use for your final recommendations
# 4. Use the output spreadsheet to construct an optimization model to find the optimal cutoff
#    based on your chosen mode's predictions

loans.full1213 = read.csv("loans-kpi-1213.csv")

# Display the structure of the object read in, and summarize its fields
str(loans.full1213)
summary(loans.full1213)
# The fraction of defaulters in the data set can be computed by taking the mean
mean(loans.full1213$default)

unique(loans.full1213$purpose)

barplot(table(loans.full1213$default, loans.full1213$purpose),
        main="Default Distribution by Purpose",
        xlab="Default vs Purpose", ylab = "Frequency",
        col=c("green","red"), beside=TRUE, legend = rownames(table(loans.full1213$default, loans.full1213$purpose)))

## Step 1 ###

## replace new purposes with "all_other" and convert "other" to "all_other" keep datatype sync'd
# Additional purposes in loans-kpi-1213.csv dataset were:
# house, moving, medical, car, vacation, renewable_energy, wedding

loans.full1213["purpose"][loans.full1213["purpose"]== "other"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "house"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "wedding"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "vacation"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "renewable_energy"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "car"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "medical"] <- "all_other"
loans.full1213["purpose"][loans.full1213["purpose"]== "moving"] <- "all_other"

loans.full = read.csv("loans-kpi.csv")
# Display the structure of the object read in, and summarize its fields
str(loans.full)
summary(loans.full)

# The fraction of defaulters in the data set can be computed by taking the mean
mean(loans.full$default)

barplot(table(loans.full$default, loans.full$purpose),
        main="Default Distribution by Purpose",
        xlab="Default vs Purpose", ylab = "Frequency",
        col=c("green","red"), beside=TRUE, legend = rownames(table(loans.full$default, loans.full$purpose)))


# Check whether the purposes match now
unique(loans.full$purpose)
unique(loans.full1213$purpose) # 
####################################
### Prepare the dataset for analysis

# Preprocessing #
# The fraction of default customers is very small to do a good job at prediction
# Create a smaller data set with as many samples from the non defaults as defaults

# Set the same random seed so the samples will be the same if repeated
set.seed(9872398)

loans <- loans.full
# Number of observations in the sampled dataset
N = nrow(loans)

# Prepare new values in 'randvalue' using a random uniform number generator
# Each record in it has a corresponding uniform random value which will be used 
# to decide if the obs is assigned to the training or prediction sample
randvalue = runif(N)


# Changed training data to include all loans.full data
# Changed test data to include new data (loans-kpi-1213) for new test data accuracy check
# Two tests done 
# 1. Removed rows with all new purpose types
# 2. Replaced the rows with all new purpose types with "all_other"
train = loans.full
test = loans.full1213

####################################################
## Prepare training and testing sets for modeling ##

train$default = as.factor(train$default)
test$default = as.factor(test$default)

################ Predictor Availability ###############################################
# Using credit.policy in building the model may be questionable given that it may not be 
#   available when screening the loan for defaulters (when the application is made)
# Similarly, int.rate may be the result of a detailed screening/negotiating process that may
#   not be complete when screening the loans.
# Hence in the sequel we remove these two variables from all model building
# We can use a similar logic to exclude installment but add the imputed principal instead
# We should also exclude the tot.payment and interest derived variables

## !!! You can simply cut and paste your code for obtaining the best logistic regression model 
#   you converged on from past weeks here 
####################################################
# Using Stepwise Regression as a stub instead of your model

null = glm(default ~ 1, data=train, family="binomial")
full = glm(default ~ . -credit.policy -int.rate, data=train, family="binomial")  
fwd = step(null, scope=formula(full),steps=15,dir="forward")
# give a summary of the model's trained parameters
lrfit = fwd

summary(lrfit)
# index of Significant predictors
id = which(summary(lrfit)$coeff[,4] < 0.05)
# Significant predictors
coeff.sig = summary(lrfit)$coeff[,1][id]
names(coeff.sig)

# Predict the probability of not fully paying back using lrmodel and test set
lr.default.prob = predict(lrfit, newdata=test, type="response")

# Add the predicted.risk variable to the test set
test$lr.default.prob = lr.default.prob
# Create the confusion matrix of the test set
Conf.lr = table(test$default, lr.default.prob > 0.15)
Conf.lr
# Accuracy of the logistic regression model
(Conf.lr[1,1]+Conf.lr[2,2])/sum(Conf.lr)
# False negative rate of the lr model
(Conf.lr[2,1]/(Conf.lr[1,1]+Conf.lr[2,1]))

# Estimate the AUC (Area Under Curve)
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
lr.default.prob = predict(lrfit, newdata=test, type="response")
pred.lr = prediction(lr.default.prob, test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 
plot(perf.lr, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.lr,"auc")
(auc.lr = as.numeric(auc.tmp@y.values))

###############################################################################
###  Random Forest
###############################################################################

## Random Forest Model
if (!require(randomForest)) {install.packages("randomForest"); library(randomForest)} 
rf.loans <- randomForest(default ~ log.annual.inc + dti + fico + days.with.cr.line + revol.bal + revol.util + inq.last.6mths + delinq.2yrs + pub.rec + principal, data=train)
# !! Play around with the set of variables in the final random forest model to see if you can improve its performance
#      and finalize this set for further analysis
importance(rf.loans)
varImpPlot(rf.loans)

# Predict the probability of not fully paying back using rfmodel and test set
rf.default.prob = predict(rf.loans, newdata=test, type="prob")
# Add the predicted.risk variable to the test set
test$rf.default.prob = rf.default.prob[,2]
# Create the confusion matrix of the test set
Conf.rf = table(test$default, rf.default.prob[,2] > 0.15)
Conf.rf
# Accuracy of the random forest model
(Conf.rf[1,1]+Conf.rf[2,2])/sum(Conf.rf)
# False negative rate of the rf model
(Conf.rf[2,1]/(Conf.rf[1,1]+Conf.rf[2,1]))

# Estimate the AUC (Area Under Curve)
if (!require(ROCR)) {install.packages("ROCR"); library(ROCR)}
pred.rf = prediction(rf.default.prob[,2], test$default)
perf.rf = performance(pred.rf, measure = "tpr", x.measure = "fpr") 
plot(perf.rf, col=rainbow(10))
#Commands for calculating AUROC using ROCR
auc.tmp = performance(pred.rf,"auc")
(auc.rf = as.numeric(auc.tmp@y.values))


# plot both ROC curves together
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(perf.rf,add=TRUE,col="green")
legend("bottomright",c("LogRegr","Random Forest"),pch=15,col=c("red","green"),bty="n")

################ Output predictions from lr models for further analysis and optimization ######
# Output a final csv file of the test rows with the default probability predicted by your models as the last two columns

## !!!Commented the file generation as it is not needed for Final folio submission
# write.csv(test,"Final Folio Test File - Group 26.csv")

# !! Use the output spreadsheet to construct an optimization model to find the optimal cutoff
#    after deciding on which of the models you prefer to use
# The variable is the cutoff threshold while the objective depends on how you decide to address your audience's
# concerns (and hence choose total profit, total principal funded, return on invested principal, or any 
# combination of these)
#Note: please refer to file Final Folio - Group 26.xlsx
