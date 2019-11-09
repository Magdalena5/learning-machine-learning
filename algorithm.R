
## Created by Magdalena Smieszek
##Creating a predictive model that would predict propensity to subscribe a gym by a user based on
##available data.

library(ggplot2)
library(jsonlite)
library(rlist)
library(naivebayes)
library(eeptools)


#load data from json and csv  file

train_csv <- read.csv(file="C:/Users/magda/Desktop/accenture/train.csv", header=TRUE, sep=",",fileEncoding="UTF-8")
train_json <- read_json("C:/Users/magda/Desktop/accenture/train.json")
str(train_csv)
str(train_json)

#check how many activites do one person
id <- sapply(train_json$data,function(x) c(x$id))
N<-length(id)

groups <- sapply(train_json$data,function(x) c(x$groups))
number_of_groups <- numeric(N)
groups_name <- matrix(1:N,ncol =N,nrow=1)
groups_name <-as.character(groups_name)
date_joined <- matrix(1:N,ncol =N,nrow=1)
date_joined <-as.character(date_joined)

for (i in 1:N){
  number_of_groups[i] <-length(sapply(groups[i]$data,function(x) c(x$group_name)))
  for (ii in 1 :number_of_groups[i]){
    groups_name[i] <- paste(groups_name[i],sapply(groups[i]$data,function(x) c(x$group_name))[ii],sep =",")
    date_joined[i] <- paste(date_joined[i],sapply(groups[i]$data,function(x) c(x$date_joined))[ii],sep =",")
  }
}

#create table from train.json file
table_new <-data.frame(id,number_of_groups,groups_name,date_joined)

#change target go logical data and fector to numeric
target <- as.logical(train_csv$target)
train_csv$target <- as.logical(train_csv$target)
sex_numeric <- as.numeric(train_csv$sex)
occupation_numeric <-as.numeric(train_csv$occupation)
credit_card_type_numeric <-as.numeric(train_csv$credit_card_type)
relationship_status_numeric <-as.numeric(train_csv$relationship_status)

#calculate birth date to age in year
birth_as_data_train_csv <-as.Date(as.character(train_csv$dob), format = "%Y-%m-%d")
age_int_train_csv <-numeric(N)
for(i in 1:N){
  if(!is.na(birth_as_data_train_csv[i])){
    age_int_train_csv[i]<-floor(age_calc(birth_as_data_train_csv[i], units = "years"))}
}
age <- as.integer(age_int_train_csv)
train_csv$dob <- as.integer(age_int_train_csv)

#remove NA
train_csv$daily_commute[is.na(train_csv$daily_commute)] <- 0
train_csv$education[is.na(train_csv$education)] <- 0
#create data to check correlation and calculate correlation
train_new_table <-data.frame(sex_numeric,age,occupation_numeric,relationship_status_numeric,train_csv$friends_number,train_csv$location_population,train_csv$location_from_population,train_csv$daily_commute,train_csv$education,credit_card_type_numeric,table_new$number_of_groups)
str(train_new_table)
data.cor <- cor(train_new_table)

#creat boxplot and point plot for data visualization
ggplot (train_csv, aes (x =target, y = location_population  ,color =target))+geom_boxplot()+ ggtitle("location_population")
ggplot (train_csv, aes (x =occupation_numeric, y = user_id  ,color =target))+geom_point()+ ggtitle("Boxplot of number_of_groups")

#create table for model and calculate model
train_csv_new <-data.frame(train_csv[,4:5],"occupation"=train_csv$occupation,train_csv[13:15],"number_of_groups"=table_new$number_of_groups)
str(train_csv_new )
Model <-naive_bayes(target~., data=train_csv_new,kernel=T)

#calculate missclasifaied in train data
p <-predict(Model,train_csv_new)
prob <-predict(Model,train_csv_new,type='prob')
prawdo <- numeric(4000)
for (i in 1:4000){
  if (p[i]== TRUE){
    prawdo[i] <-prob[i,2]
    }else{
      prawdo[i] <-prob[i,1]}}

#accuracy for model
tab1 <-table(p,target)
accuracy <- 1-sum(diag(tab1))/sum(tab1)





## Test data

test_csv <-read.csv(file="C:/Users/magda/Desktop/accenture/test.csv", header=TRUE, sep=",",fileEncoding="UTF-8")
test_json <- read_json("C:/Users/magda/Desktop/accenture/test.json")
str(test_csv)
str(test_json)


#check how many activites do one person
id <- sapply(test_json$data,function(x) c(x$id))
N<-length(id)

groups <- sapply(test_json$data,function(x) c(x$groups))
number_of_groups <- numeric(N)
groups_name <- matrix(1:N,ncol =N,nrow=1)
groups_name <-as.character(groups_name)
date_joined <- matrix(1:N,ncol =N,nrow=1)
date_joined <-as.character(date_joined)

for (i in 1:N){
  number_of_groups[i] <-length(sapply(groups[i]$data,function(x) c(x$group_name)))
  for (ii in 1 :number_of_groups[i]){
    groups_name[i] <- paste(groups_name[i],sapply(groups[i]$data,function(x) c(x$group_name))[ii],sep =",")
    date_joined[i] <- paste(date_joined[i],sapply(groups[i]$data,function(x) c(x$date_joined))[ii],sep =",")
  }
}

table_new_test <-data.frame(id,number_of_groups,groups_name,date_joined)

#calculate birt date to age in year
birth_as_data_test_csv <-as.Date(as.character(test_csv$dob), format = "%Y-%m-%d")
age_int_test_csv <-numeric(N)
for(i in 1:N){
  if(!is.na(birth_as_data_test_csv[i])){
    age_int_test_csv[i]<-floor(age_calc(birth_as_data_test_csv[i], units = "years"))}
}
age <- as.integer(age_int_test_csv)
test_csv$dob <- as.integer(age_int_test_csv)

#remove NA
test_csv$daily_commute[is.na(test_csv$daily_commute)] <- 0
test_csv$education[is.na(test_csv$education)] <- 0
test_csv_new <-data.frame(test_csv[,4:5],"occupation"=test_csv$occupation,test_csv[,13:15],"number_of_groups"=table_new_test$number_of_groups)
str(test_csv_new)

p_test <-predict(Model,test_csv_new)
N <-length (p_test)
prob_test <-predict(Model,test_csv_new,type='prob')
proba <- numeric(N)
for (i in 1:N){
  if (p_test[i]== TRUE){
    proba[i] <-prob[i,2]
  }else{
    proba[i] <-prob[i,1]}}

target_test <- data.frame(proba,p_test)
target <- as.integer(as.logical(p_test))
finish_tabla <-data.frame("user_id"=test_csv$user_id,"probability_of"=target_test$proba,target,test_csv[,3:16])
str(finish_tabla)
write.csv(finish_tabla, file = "C:/Users/magda/Desktop/accenture/test.csv",fileEncoding="UTF-8")