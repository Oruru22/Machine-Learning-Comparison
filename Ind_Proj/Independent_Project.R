library(readr)
heart<-read_csv("~/Downloads/Ind_Proj/heart.csv")
heart
str(heart)
heartanalysis<-heart

#Project has 8 Parts:
#Analysis
#Preparation of Data
#Machine Learning Models
#Comparing Models
#Combining Models
#Deep Learning Model
#Comparing Models with Deep Learning Model
#Comparing combined Models with Deep Learning Model

#About dataset
#Age: age of the patient [years]
#Sex: sex of the patient [M: Male, F: Female]
#ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
#RestingBP: resting blood pressure [mm Hg]
#Cholesterol: serum cholesterol [mm/dl]
#FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
#RestingECG: resting electrocardiogram results [Normal: Normal, ST: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy by Estes' criteria]
#MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
#ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
#Oldpeak: oldpeak = ST [Numeric value measured in depression]
#ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, Flat: flat, Down: downsloping]
#HeartDisease:(dependent variable) output class [1: heart disease, 0: Normal]
summary(heartanalysis)
#going to normalize everything later.
#also change the dependent variable and some other columns to factors.
dim(heartanalysis)
#918 columns 12 variable
View(heartanalysis)
#all columns will be used for training and test.
#check for na's
any(is.na(heartanalysis))# no na's 
#check for duplicates
library(dplyr)
dup<-duplicated(heartanalysis)
table(dup)#no duplicates
#Proportions and Visualizations
#age
table(heartanalysis$Age)
prop.table(heartanalysis$Age)
library(ggplot2)
ggplot(data = heartanalysis, aes(x=Age)) +
  geom_histogram(binwidth = 1, fill = "green", color="black") +
  geom_text(stat = "count", aes(label=stat(count)), vjust = -0.5, color ="black") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution")
#sex
table(heartanalysis$Sex)
prop.table(table(heartanalysis$Sex))
ggplot(data = heartanalysis, aes(x = Sex, fill = Sex)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "Sex", y = "Count", title = "Sex Count") +
  guides(fill = FALSE)
#chestpaintype
table(heartanalysis$ChestPainType)
prop.table(table(heartanalysis$ChestPainType))
ggplot(data = heartanalysis, aes(x = ChestPainType, fill = ChestPainType)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "ChestPainType", y = "Count", title = "Chest Pain Type Count") +
  guides(fill = FALSE)
#RestingBP
table(heartanalysis$RestingBP)
prop.table(table(heartanalysis$RestingBP))
ggplot(data=heartanalysis, aes(x = RestingBP)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Resting Blood Pressure", x = "Resting Blood Pressure", y = "Frequency") +
  theme_minimal()
#Cholesterol
table(heartanalysis$Cholesterol)
prop.table(table(heartanalysis$Cholesterol))
ggplot(data=heartanalysis, aes(x = Cholesterol)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Cholesterol Levels", x = "Cholesterol", y = "Frequency") +
  theme_minimal()
#Fasting BS
heart
#as.factor(heart_BS$FastingBS)
table(heartanalysis$FastingBS)
prop.table(table(heartanalysis$FastingBS))
ggplot(data = heartanalysis, aes(x = FastingBS, fill = FastingBS)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "Fasting Blood Sugar", y = "Count", title = "Fasting Blood Sugar Count") +
  guides(fill = FALSE)
#the graph looks this way because ggplot had issues grouping the data, possibly because it hasn't been 
#converted to factors
#changed data but still no luck
#RestingECG
table(heartanalysis$RestingECG)
prop.table(table(heartanalysis$RestingECG))
ggplot(data = heartanalysis, aes(x = RestingECG, fill = RestingECG)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "RestingECG", y = "Count", title = "Resting ECG Type Count") +
  guides(fill = FALSE)
#MaxHR
table(heartanalysis$MaxHR)
prop.table(table(heartanalysis$MaxHR))
ggplot(data=heartanalysis, aes(x = MaxHR)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Heart Rate", x = "MaxHR", y = "Frequency") +
  theme_minimal()
#majority of the patients had a maximum heart rate between 100 and 170
#ExerciseAngina
table(heartanalysis$ExerciseAngina)
prop.table(table(heartanalysis$ExerciseAngina))
ggplot(data = heartanalysis, aes(x = ExerciseAngina, fill = ExerciseAngina)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "ExerciseAngina", y = "Count", title = "ExerciseAngina Count") +
  guides(fill = FALSE)
#majority of the patients had Exercised Induced Angina
#Oldpeak
table(heartanalysis$Oldpeak)
prop.table(table(heartanalysis$Oldpeak))
ggplot(data=heartanalysis, aes(x = Oldpeak)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Old Peak", x = "OldPeak", y = "Frequency") +
  theme_minimal()
#ST_Slope
table(heartanalysis$ST_Slope)
prop.table(table(heartanalysis$ST_Slope))
ggplot(data = heartanalysis, aes(x = ST_Slope, fill = ST_Slope)) + 
  geom_bar(stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "ST Slope", y = "Count", title = "ST Slope Count") +
  guides(fill = FALSE)
#majority of the patients had a Flat St slope.
#HeartDisease
table(heartanalysis$HeartDisease)
prop.table(table(heartanalysis$HeartDisease))
ggplot(data = heartanalysis, aes(x = HeartDisease, fill = HeartDisease)) + 
  geom_bar(stat = "count", color = "green") +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5) +
  labs(x = "Heart Disease", y = "Count", title = "Heart Disease Diagnosis Count") +
  guides(fill = FALSE)
#508 patients had a heart disease while 410 patients had normal heart conditions.
library(explore)
heartanalysis %>% explore_all()
#Correlation & Correlation Plot
cor(heartanalysis[c(1,4,5,6,8,10,12)])
#ask ada and chika about the correlations and what they mean.
library(corrplot)
#1,4,5,6,8,10,12
correlations<-cor(heartanalysis[c(1,4,5,6,8,10,12)])
corrplot(correlations, method="circle")
#Scatter Plot Matrix
pairs(correlations)
# Understanding Correlation results
#Age:
  #RestingBP: Positive correlation (0.254), suggesting that blood pressure tends to increase with age.
  #Cholesterol: Slightly negative correlation (-0.095), indicating a weak relationship 
  #where higher age might correlate with slightly lower cholesterol.
  #FastingBS: Positive correlation (0.198), indicating that older individuals might 
  #have a higher likelihood of elevated fasting blood sugar levels.
  #MaxHR: Strong negative correlation (-0.382), suggesting that maximum heart rate decreases as age increases.
  #Oldpeak: Positive correlation (0.259), indicating that older age is associated with higher exercise-induced ST depression.
  #HeartDisease: Positive correlation (0.282), showing that older age is associated with a higher occurence of heart disease.
#RestingBP
  #Cholesterol: Slightly positive correlation (0.101), but this is quite weak, suggesting only a minor relationship.
  #FastingBS: Very weak positive correlation (0.070).
  #MaxHR: Negative correlation (-0.112), indicating that higher resting blood pressure may be associated with lower max heart rates.
  #Oldpeak: Positive correlation (0.165), suggesting higher blood pressure may be related to more pronounced exercise-induced ST depression.
  #HeartDisease: Positive correlation (0.108), suggesting a relationship, 
  #though not very strong, between higher resting blood pressure and heart disease risk.
#Cholesterol:
  #FastingBS: Negative correlation (-0.261), indicating that higher cholesterol levels might be associated 
  #with lower fasting blood sugar levels.
  #MaxHR: Positive correlation (0.236), suggesting that higher cholesterol levels might correlate with higher maximum heart rates.
  #Old Peak: Very weak Positve correlation (0.050).
  #HeartDisease: Negative correlation (-0.233), showing that higher cholesterol levels might be
  #associated with a lower incidence of heart disease.
#FastingBS:
  #MaxHR: Negative correlation (-0.131), indicating that higher fasting blood sugar may be associated with
  #lower maximum heart rates.
  #Oldpeak: Weak positive correlation (0.053).
  #HeartDisease: Positive correlation (0.267), indicating a strong relationship between elevated 
  #fasting blood sugar and increased risk of heart disease.
#MaxHR:
  #Oldpeak: Negative correlation (-0.161), suggesting that higher maximum heart rates are associated with 
  #less exercise-induced ST depression.
  #HeartDisease: Strong negative correlation (-0.400), indicating that higher max heart rates are associated
  #with a lower risk of heart disease.
#Old Peak:
  #HeartDisease: Strong positive correlation (0.404), indicating that higher ST depression during exercise
  #is strongly associated with a higher risk of heart disease.
#Heart Disease:
  #Age: Strong positive correlation (0.282), indicting the older a person gets, the higher their chances of getting a heart disease are.
  #RestingBP: weak positive correlation.
  #Cholesterol: Negative correlation (-0.232), indicating that the there's a lower chance of heart disease with higher cholesterol levels.
  #FastingBS: Positive correlation (0.267), indicating that higher Fasting blood Sugar increases the chances of a heart disease.
  #MaxHr: Very strong negative correlation (-0.400) indicating that a high heart rate is associated with a low chance of heart disease.
  #OldPeak: Very Strong positve correlation (0.403) indicating that higher ST depression levels are associated with a higher chance of a heart disease.

#Data Preparation
#change columns to numeric
#Label Encoding
#Sex
heart_numeric<-heartanalysis
str(heart_numeric$Sex)
heart_numeric$Sex<-factor(heart_numeric$Sex, levels=c("F","M"), labels=c("1", "2"))
heart_numeric$Sex<-as.numeric(heart_numeric$Sex)
str(heart_numeric$Sex)
#ChestPainType
str(heart_numeric$ChestPainType)
heart_numeric$ChestPainType<-factor(heart_numeric$ChestPainType, levels=c("ASY","ATA","NAP","TA"), labels=c("1", "2", "3", "4"))
str(heart_numeric$ChestPainType)
heart_numeric$ChestPainType<-as.numeric(heart_numeric$ChestPainType)
str(heart_numeric$ChestPainType)
#RestingECG
str(heart_numeric$RestingECG)
heart_numeric$RestingECG<-factor(heart_numeric$RestingECG, levels=c("LVH","Normal","ST"), labels=c("1", "2", "3"))
str(heart_numeric$RestingECG)
heart_numeric$RestingECG<-as.numeric(heart_numeric$RestingECG)
str(heart_numeric$RestingECG)
#ExerciseAngina
str(heart_numeric$ExerciseAngina)
heart_numeric$ExerciseAngina<-factor(heart_numeric$ExerciseAngina, levels=c("N","Y"), labels=c("1", "2"))
str(heart_numeric$ExerciseAngina)
heart_numeric$ExerciseAngina<-as.numeric(heart_numeric$ExerciseAngina)
str(heart_numeric$ExerciseAngina)
#ST_SLOPE
str(heart_numeric$ST_Slope)
heart_numeric$ST_Slope<-factor(heart_numeric$ST_Slope, levels=c("Down","Flat","Up"), labels=c("1", "2","3"))
str(heart_numeric$ST_Slope)
heart_numeric$ST_Slope<-as.numeric(heart_numeric$ST_Slope)
str(heart_numeric$ST_Slope)
#change dependent variable to a factor
str(heart_numeric$HeartDisease)
#HeartDisease:(dependent variable) output class [1: heart disease, 0: Normal]
heart_numeric$HeartDisease<-factor(heart_numeric$HeartDisease, levels=c("0","1"), labels=c("Normal", "Heart Disease"))
str(heart_numeric$HeartDisease)
str(heart_numeric)

#Create separate datasets for each ML algorithm
#KNN:
  #needs a labelled and un-labelled datasets
#dataset for labelled should be changed to factors but left untouched.
heart_label<-heartanalysis
heart_label
#Sex
str(heart_label$Sex)
heart_label$Sex<-factor(heart_label$Sex, levels=c("F","M"), labels=c("F", "M"))
#ChestPainType
str(heart_label$ChestPainType)
heart_label$ChestPainType<-factor(heart_label$ChestPainType, levels=c("ASY","ATA","NAP","TA"), labels=c("ASY","ATA","NAP","TA"))
#RestingECG
str(heart_label$RestingECG)
heart_label$RestingECG<-factor(heart_label$RestingECG, levels=c("LVH","Normal","ST"), labels=c("LVH","Normal","ST"))
#ExerciseAngina
str(heart_label$ExerciseAngina)
heart_label$ExerciseAngina<-factor(heart_label$ExerciseAngina, levels=c("N","Y"), labels=c("N", "Y"))
#ST_SLOPE
str(heart_label$ST_Slope)
heart_label$ST_Slope<-factor(heart_label$ST_Slope, levels=c("Down","Flat","Up"), labels=c("Down","Flat","Up"))
#HeartDisease
str(heart_label$HeartDisease)
heart_label$HeartDisease<-factor(heart_label$HeartDisease, levels=c("0","1"), labels=c("Normal", "Heart Disease"))
str(heart_label)
View(heart_label)
#labelled
heart_knn_label<-heart_label
View(heart_knn_label)
str(heart_knn_label)
#unlabelled
heart_knn_unlabel<-heart_numeric[-12]
View(heart_knn_unlabel)
str(heart_knn_unlabel)
#Standardize dataset without labels.
#Standardize Function
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

heart_knn_unlabel<-as.data.frame(lapply(heart_knn_unlabel, normalize))
summary(heart_knn_unlabel)
#might not need heart_numeric
#use heart_label
#or just use one-hot encoding for knn & SVM. 
#1:yes
#0: no
one_hot_encode <- function(data, cols) {
  for (col in cols) {
    dummies <- as.data.frame(model.matrix(~ . - 1, data.frame(data[[col]])))
    names(dummies) <- sub("^.+\\.", paste0(col, "_"), names(dummies))
    data <- cbind(data, dummies)
    data[[col]] <- NULL  
  }
  return(data)
}
cols_to_encode <- c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")
heart_encoded<-heart_label
heart_encoded <- one_hot_encode(heart_encoded, cols_to_encode)
View(heart_encoded)
dim(heart_encoded)
#Normalize enocded dataset
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalized_heart_encoded<- heart_encoded
normalized_column<-c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
normalized_heart_encoded[normalized_column]<-as.data.frame(lapply(normalized_heart_encoded[normalized_column] , normalize))
summary(normalized_heart_encoded)
View(normalized_heart_encoded)
#KNN
#create labelled and unlabelled dataset
knn_heart_label<-normalized_heart_encoded
knn_heart_nolabel<-normalized_heart_encoded[-7]
View(knn_heart_nolabel)
#save as a new csv file
write.csv(knn_heart_label, "knn_heart_label.csv", row.names = FALSE)
write.csv(knn_heart_nolabel, "knn_heart_nolabel.csv", row.names = FALSE)
#SVM
svm_heart<-normalized_heart_encoded
write.csv(svm_heart, "svm_heart.csv", row.names = FALSE)
#Decison Tree
#use encoded label dataset for this and normalize it again
decision_tree_heart<-heart_label
normal_col<-c(1, 4, 5, 6, 8, 10)
decision_tree_heart[normal_col]<-as.data.frame(lapply(decision_tree_heart[normal_col], normalize))
summary(decision_tree_heart)
write.csv(decision_tree_heart, "decision_tree_heart.csv", row.names = FALSE)
#Random Forest
random_forest_heart<-heart_label
random_forest_heart[normal_col]<-as.data.frame(lapply(random_forest_heart[normal_col], normalize))
summary(random_forest_heart)
write.csv(random_forest_heart, "random_forest_heart.csv", row.names = FALSE)

#Machine Learning Models
library(reticulate)
#use_python("/Users/owner/opt/anaconda3/bin/python3", required = T)
#use_python("/Users/owner/miniconda3/bin/python3", required = TRUE)
#use_python("/Users/owner/miniconda3/bin/python", required = TRUE)
use_condaenv("myenv", required = TRUE)
#test
#test_vector<-c(d1,2,3,4)

#py$test_vector <- test_vector
#py_run_string("print(test_vector)"


py$knn_heart_label <- knn_heart_label
py$knn_heart_nolabel <- knn_heart_nolabel

py_run_string("print('Label Data:', knn_heart_label)")
py_run_string("print('No Label Data:', knn_heart_nolabel)")
#or
py$knn_heart_label <- r_to_py(knn_heart_label)
py$knn_heart_nolabel <- r_to_py(knn_heart_nolabel)
py_run_string("
import numpy as np
print('Label Data Shape:', np.array(knn_heart_label).shape)
print('No Label Data Shape:', np.array(knn_heart_nolabel).shape)
")
print(py$knn_heart_label)
#python script
py_run_string("
import numpy as np
print(knn_heart_label.shape)
print(knn_heart_nolabel.shape)
knn_train_nolabel=knn_heart_nolabel[0:734,:]
knn_test_nolabel=knn_heart_nolabel[735:918,:]
              
knn_train_label=knn_heart_label[0:734, 6]
knn_test_label=knn_heart_label[735:918, 6]
")




#/Users/owner/miniconda3/envs/myenv
#Knn
dim(knn_heart_label)
dim(knn_heart_nolabel)
#test and training dataset (no label)
knn_train_nolabel<-knn_heart_nolabel[1:734, ]
knn_test_nolabel<-knn_heart_nolabel[735:918, ]

#test and training dataset (label)
knn_train_label<-knn_heart_label[1:734, 7]
knn_test_label<-knn_heart_label[735:918, 7]
#Training Set Indices: 1 to 734
#Test Set Indices: 735 to 918
#use pandas for tabulary data

#choosing the best k-value
library(class)
predictions1 <- list()
for (k in 2:27) {
  predictions1[[paste("k=", k, sep="")]] <- knn(train=knn_train_nolabel, test=knn_test_nolabel, cl=knn_train_label, k=k)
}
predictions1
library(dplyr)
accuracies1 <-sapply(predictions1, function(pred, true_labels) {
  mean(pred == true_labels)
}, true_labels = knn_test_label)

accuracy_data <- data.frame(
  k = 2:27,
  Accuracy = accuracies1
)
#Visualizing k-values
library(ggplot2)
ggplot(accuracy_data, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs. k in K-NN", x = "k (Number of Neighbors)", y = "Accuracy") +
  theme_minimal()
#Evaluate Model Performance
library(gmodels)
CrossTable(x = knn_test_label, y=predictions1$`k=27`, prop.chisq = FALSE)

table(predictions1$`k=27`, knn_test_label)
knn_eval<-predictions1$`k=27` == knn_test_label
knn_eval
table(knn_eval)








#TO DO:
#make sure all columns are numeric and then normalize all columns(do in r).
#create another dataset and change characters to factors for the labelled dataset for knn.
#test label encoding and one-hot encoding.
#probably will up end using one-hot encoding.




#TEST
one_hot_encode <- function(data, cols) {
  for (col in cols) {
    dummies <- as.data.frame(model.matrix(~ . - 1, data.frame(data[[col]])))
    names(dummies) <- sub("^.+\\.", paste0(col, "_"), names(dummies))
    data <- cbind(data, dummies)
    data[[col]] <- NULL  
  }
  return(data)
}

cols_to_encode <- c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")
heart_test<-heart_label
heart_test <- one_hot_encode(heart_test, cols_to_encode)

View(heart_test)
dim(heart_test)

#use this for knn and svm.
#heart_encoded <- model.matrix(~ Sex + ChestPainType + RestingECG + ExerciseAngina + ST_Slope + HeartDisease - 1, data = heart_test)
#View(heart_encoded)
#numeric_data <- heart_test %>% select(-Sex, -ChestPainType, -RestingECG, -ExerciseAngina, -ST_Slope, -HeartDisease)
#final_data <- cbind(numeric_data, heart_encoded)



#Note: regular one-hot encoding takes out an attribute from each column(mostly to avoid multicollinearity) so to avoid that,
#a custom one-hot encoding function has to be created.

