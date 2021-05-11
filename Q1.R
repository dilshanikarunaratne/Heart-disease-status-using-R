#Get the current working directory
getwd()
#Set the working directory
setwd("C:/Users/Dilshani/Documents/DA/CW/Supplement files")
A <- read.table("A.data", sep =",", na ="?", stringsAsFactors = FALSE, header = FALSE)
B <- read.table("B.data", sep =",", na ="?", stringsAsFactors = FALSE, header = FALSE)
C <- read.table("C.data", sep =",", na ="?", stringsAsFactors = FALSE, header = FALSE)
D <- read.table("D.data", sep =",", na ="?", stringsAsFactors = FALSE, header = FALSE)

#merging the datasets
total <- rbind(A, B, C, D)

#display first five rows of the dataset
head(total, 5)

#renaming column headers
names(total) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "disease_status")
#Display the names of the heders
names(total)

#Get a statistical summary
summary(total)

#get the total count of missing values
sum(is.na(total))
#sum(is.na(total$thal))

#replace values greater than 0 with 1 for the response variable
total$disease_status[total$disease_status > 0] <- 1
#total <- total[total$disease_status == 0 | total$disease_status == 1, ] 

#imputation using mean
total = transform(total, age = ifelse(is.na(age), mean(age, na.rm=TRUE), age))
total = transform(total, trestbps = ifelse(is.na(trestbps), mean(trestbps, na.rm=TRUE), trestbps))
total = transform(total, chol = ifelse(is.na(chol), mean(chol, na.rm=TRUE), chol))
total = transform(total, fbs = ifelse(is.na(fbs), mean(fbs, na.rm=TRUE), fbs))
total = transform(total, restecg = ifelse(is.na(restecg), mean(restecg, na.rm=TRUE), restecg))
total = transform(total, thalach = ifelse(is.na(thalach), mean(thalach, na.rm=TRUE), thalach))
total = transform(total, exang = ifelse(is.na(exang), mean(exang, na.rm=TRUE), exang))
total = transform(total, oldpeak = ifelse(is.na(oldpeak), mean(oldpeak, na.rm=TRUE), oldpeak))
total = transform(total, slope = ifelse(is.na(slope), mean(slope, na.rm=TRUE), slope))
total = transform(total, ca = ifelse(is.na(ca), mean(ca, na.rm=TRUE), ca))
total = transform(total, thal = ifelse(is.na(thal), mean(thal, na.rm=TRUE), thal))
total = transform(total, chol = ifelse(is.na(chol), mean(chol, na.rm=TRUE), chol))

#total[is.na(total)] <- colMeans(total, na.rm = TRUE)

library(ggplot2)
#EDA - Exploratory Data Analysis

#Boxplots to evaluate numerical variables
boxplot(total$age,
        main = "Heart disases status boxplot 1",
        xlab = "Age",
        ylab = "Years",
        col = "green",
        border = "black")


boxplot(total$trestbps,
        main = "Heart disases status boxplot 2",
        xlab = "Resting blood pressure",
        ylab = "in mm Hg on admission to the hospital",
        col = "blue",
        border = "black")

boxplot(total$chol,
        main = "Heart disases status boxplot 3",
        xlab = "serum cholestoral",
        ylab = "in mg/dl",
        col = "red",
        border = "black")

boxplot(total$thalach,
        main = "Heart disases status boxplot 4",
        xlab = "maximum heart rate",
        ylab = "Heart rate",
        col = "green",
        border = "black")


boxplot(total$oldpeak,
        main = "Heart disases status boxplot 5",
        xlab = "ST depression",
        ylab = "ST depression induced by exercise relative to rest",
        col = "purple",
        border = "black")

boxplot(total$age ~ total$disease_status ,
        main="Fate by Age",
        ylab="Age",xlab="Heart disease",
        col = "orange",
        border = "black")


install.packages("skimr")
library(skimr)
skim(total)

str(total)

#scatter plots for numberical variables
pairs(total.subset)

#Remove outliers from the drawn boxplots
outliers1 <- boxplot(total$trestbps, plot=FALSE)$out
total <- total[-which(total$trestbps %in% outliers1),]

outliers2 <- boxplot(total$chol, plot=FALSE)$out
total <- total[-which(total$chol %in% outliers2),]

outliers3 <- boxplot(total$oldpeak, plot=FALSE)$out
total <- total[-which(total$oldpeak %in% outliers3),]


#Barplots for categorical variables
counts <- table(total$sex, total$disease_status)
barplot(counts, main="Gender VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red"), beside=TRUE, 
       legend = c("Male", "Female"), args.legend = list(x ='top', bty='n',horiz = TRUE, inset=c(0,-0.1) )
       )

counts1 <- table(total$cp, total$disease_status)
barplot(counts1, main="Chest Pain VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","orange","yellow"), beside=TRUE
        #legend = c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic"), args.legend = list(x ='bottomleft', bty='n',inset=c(0,-0.5))
        )

counts2 <- table(total$fbs, total$disease_status)
barplot(counts2, main="Fasting blood sugar VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","yellow"), beside=TRUE
        #legend = c("True", "False"), args.legend = list(x="topright")
        )

counts <- table(total$restecg, total$disease_status)
barplot(counts, main="Resting electrocardiographic results VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","orange","red","yellow"), beside=TRUE
        #legend = c("0: normal", "1: having ST-T wave abnormality", "2: showing probable or definite left ventricular hypertrophy by Estes' criteria"), args.legend = list(x="topright")
        )

counts3 <- table(total$exang, total$disease_status)
barplot(counts3, main="Exercise induced angina VS Heart Disease status",  names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","yellow"), beside=TRUE
        #legend = c("Yes", "No"), args.legend = list(x="topright")
        )

counts4 <- table(total$slope, total$disease_status)
barplot(counts4, main="Slope of the peak exercise ST segment VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","yellow","orange"), beside=TRUE
        #legend = c("1: upsloping", "2: flat", "3: downsloping"), args.legend = list(x="topright")
        )

counts5 <- table(total$ca, total$disease_status)
barplot(counts5, main="Major vessels VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","yellow","orange","purple"), beside=TRUE)

counts6 <- table(total$thal, total$disease_status)
barplot(counts6, main="Thalassemia VS Heart Disease status", names.arg=c("No", "Yes" ),
        xlab="Heart Disease Status", col=c("navyblue","red","yellow","orange"), beside=TRUE
        #legend = c("3 = normal", "6 = fixed defect", "7 = reversable defect"), args.legend = list(x="topright")
        )

total.subset <- subset(total, select = c(age,trestbps,chol,thalach,oldpeak))
total[is.na(total)] <- 0
res <- cor(total)

install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")
chart.Correlation(res, histogram=TRUE, pch=19)

#ward hierarchical clustering
d <- dist(total, method = "euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)
#table(fit$cluster, total$age)

scaled = scale(total)
# Select a number of clusters
k = 5
# Run the k-means algorithms
first_clust = kmeans(scaled, centers = k, nstart = 1)
# How many patients are in each group was taken into consideration
first_clust$size
# Set the seed
seed_val = 38
set.seed(seed_val, kind = "Mersenne-Twister", normal.kind = "Inversion")
# Run the k-means algorithms
k = 5
second_clust = kmeans(scaled, k, nstart=1)
# How many patients are in each group?
second_clust$size
# Adding cluster assignments to the data
total['first_clust'] = first_clust$cluster
total['second_clust'] = second_clust$cluster
# Load ggplot2
library(ggplot2)
# Creating the plots of age and chol for the first clustering algorithm
plot1 = ggplot(total, aes(x =age, y = chol, color = as.factor(first_clust))) + 
  geom_point()
plot1 
# Creating the plots of age and chol for the second clustering algorithm
plot2 = ggplot(total, aes(x = age, y = chol, color = as.factor(second_clust))) + 
  geom_point()
plot2


#Split training and testing dataset
dim(total)
n <- nrow(total)
ID <- sample(n,n * 0.8, replace = F)
trainSet <- total[ID,]
dim(trainSet)
testSet <- total[-ID,]
dim(testSet)


#Logistic regression model 

glm.fit <- glm(disease_status ~ age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, data = trainSet, family = binomial)
summary(glm.fit)


#check the first five probabilities
#glm.probs <- predict(glm.fit, trainSet, type = "response")
#glm.probs[1:5]

#Make predictions using testing set
logRegPrediction <- predict(glm.fit, testSet, type = "response")
logRegPrediction[1:5] 
tail(logRegPrediction)

logRegPrediction <- ifelse(logRegPrediction > 0.5, 1, 0)
attach(testSet)
table(logRegPrediction,disease_status)

misClasificError <- mean(logRegPrediction != testSet)
print(paste('Accuracy',1-misClasificError))

#length(logRegPrediction)

install.packages("Rcpp")
library(Rcpp)
library(caret)
logRegConfMat <- confusionMatrix(as.factor(logRegPrediction), as.factor(testSet$disease_status))
logRegConfMat

#auc for logistic regression
prob=predict(glm.fit,type=c("response"))
trainSet$prob=prob
install.packages("pROC")
library(pROC)
g <- roc(disease_status ~ prob, data = trainSet)
plot(g) 

library(ROCR)
p <- predict(glm.fit, newdata=testSet, type="response")
pr <- prediction(p, testSet$disease_status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#Prediction <- as.factor(logRegPrediction)
#Actual <- as.factor(testSet$disease_status)
#Y      <- c(2816, 248, 34, 235)
#df <- data.frame(Prediction, Actual, Y)

#library(ggplot2)
#ggplot(data =  df, mapping = aes(x = Prediction, y = Actual)) +
 # geom_tile(aes(fill = Y), colour = "white") +
  #geom_text(aes(label = sprintf("%1.0f", Y)), vjust = .5) +
  #scale_fill_gradient(low = "blue", high = "red") +
  #theme_bw() + theme(legend.position = "none")

# all together
#grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
             #top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))

#confusion matrix
#glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
#attach(trainSet)
#table(glm.pred,disease_status)

#train_tab = table(predicted = logRegPrediction, actual = testSet$disease_status)
#install.packages("caret")
#library(caret)
#train_con_mat = confusionMatrix(train_tab, positive = "Yes")
#c(train_con_mat$overall["Accuracy"], 
 # train_con_mat$byClass["Sensitivity"], 
  #train_con_mat$byClass["Specificity"])


