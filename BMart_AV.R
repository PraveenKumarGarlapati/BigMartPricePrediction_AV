#BIG MART
  train_bm = read.csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Bigmart_AV/Train_UWu5bXk.csv")  
  test_bm = read.csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Bigmart_AV/Test_u94Q5KV.csv")

train_bm = Train_UWu5bXk

library(DataExplorer)
library(tidyverse)
library(party)
library(randomForest)
create_report(train_bm)

dim(train_bm)
colnames(train_bm)
[1] "Item_Identifier"           "Item_Weight"              
[3] "Item_Fat_Content"          "Item_Visibility"          
[5] "Item_Type"                 "Item_MRP"                 
[7] "Outlet_Identifier"         "Outlet_Establishment_Year"
[9] "Outlet_Size"               "Outlet_Location_Type"     
[11] "Outlet_Type"               "Item_Outlet_Sales"    

ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Outlet_Size, col = Outlet_Location_Type )) + geom_point() 
ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Item_Fat_Content, col = Item_Weight )) + geom_point() 
ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Item_Type )) + geom_boxplot() 

ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Item_MRP )) + geom_point() 
ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Item_Visibility )) + geom_point() 
ggplot(train_bm, aes(y = Item_Outlet_Sales, x = Outlet_Identifier )) + geom_point() 


train_bm%>%
  distinct(Item_Identifier)

train_1 = train_bm%>%
  sample_frac(0.05)
dim(train_1)
  
dt_bm = ctree(Item_Outlet_Sales ~ Item_MRP+ Outlet_Type+ Item_Fat_Content+Item_Weight+Item_Visibility  + Outlet_Identifier , data = train_bm )
rf = randomForest(Item_Outlet_Sales ~ Item_MRP+ Outlet_Type+ Item_Fat_Content+ Item_Visibility  + Outlet_Identifier , data = train_bm, ntree = 1000)
rf1 = randomForest(Item_Outlet_Sales ~ Item_MRP+ Outlet_Type +  Outlet_Identifier , data = train_bm)

rf2 = randomForest(Item_Outlet_Sales ~ Item_MRP+ Outlet_Type+ Item_Fat_Content+ Item_Visibility  + Outlet_Identifier +Item_Type + Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type, data = train_bm)
importance(rf2)

test_bm$Item_Outlet_Sales = predict(rf2, test_bm)
opfile = as.data.frame(test_bm[ ,c(1,7,12)])

write_csv(opfile, "rf_all.csv")

#########

library(neuralnet)

nn = neuralnet(Item_Outlet_Sales ~ Item_MRP + Item_Visibility, data = train_bm, threshold=0.01)
aa = compute(nn, test_bm[ , c(6,4)])

test_bm$Item_Outlet_Sales = aa$net.result
opfile = as.data.frame(test_bm[ ,c(1,7,12)])
write_csv(opfile, "nn_basic.csv")

lmodel = lm(Item_Outlet_Sales ~ . ,data = train_dum)
test_bm$Item_Outlet_Sales = predict(lmodel, test_bm)
opfile = as.data.frame(test_bm[ ,c(1,7,12)])

write_csv(opfile, "lmodel_all.csv")
#worst

##############

test_111 = test_bm%>%
  select(-c(12))

class(train_bm)

rm(train_1)


dim(train_bm)
opfile = opfile%>%
  mutate(Item_Outlet_Sales = if_else(is.na(Item_Outlet_Sales), 2126.601 , Item_Outlet_Sales))

opfile%>%
  filter(!is.na(Item_Outlet_Sales))%>%
  summarise(mean(Item_Outlet_Sales))

colSums(is.na(opfile))
dim(opfile)
hist(opfile$Item_Outlet_Sales)

train_bm%>%
  filter(!is.na(Item_Weight)) %>%
  summarise(mean(Item_Weight))



a = train_bm%>%
  filter(Item_Identifier %in% c("FDE52", "FDK57", "FDN52", "FDQ60")) %>%
  mutate(Item_Weight = 12.85)

train_bm = train_bm%>%
  mutate(Item_Weight = if_else(is.na(Item_Weight), 12.85, Item_Weight)) 

dim(a)         
dim(train_bm)

is.na(train_bm$Item_Weight)         



train_bm%>%
  filter(Item_Identifier %in% c("FDE52", "FDK57", "FDN52", "FDQ60"))
  


library(dummies)
colnames(dummies::dummy.data.frame(train_bm, names = c( "Outlet_Type" ,"Outlet_Location_Type" ,"Outlet_Size", "Item_Fat_Content","Item_Type", "Outlet_Identifier"), sep = "_"))


train_dum  = dummies::dummy.data.frame(train_bm, names = c( "Outlet_Type" ,"Outlet_Location_Type" ,"Outlet_Size", "Item_Fat_Content","Item_Type", "Outlet_Identifier"), sep = "_")
train_dum = dummies::dummy.data.frame(train_jj, names = c("joke_id", "user_id") , sep = "_")

rattle()
train_jj = as.data.frame(train_jj)

str(train_jj)

dim(train_jj)
colnames(train_jj)
dim(train_dum)

colnames(train_bm)

train_bm%>%
  filter(is.na("Outlet_Identifier")) 

colnames(trainj_sample)
trainj_sample = train_jj%>%
  sample_frac(0.1)

glimpse(train_jj)

colSums(is.na(a))
  
dim(train_bm)
colnames(train_bm)

glimpse(train_bm)

kmeans(train_bm[ , c(7,8,9,10,11,12)], 20)

a = kmeans(train_bm$Item_Visibility, 10)

xtabs(~train_bm$Outlet_Size)
table(train_bm$Item_Fat_Content)

dim(train)
colSums(is.na(train))

train <- na.omit(train_bm)
table(train$Item_Type,train$Item_Fat_Content)

library(rattle)
rattle()

dim(test_111)

Index <- which(train_bm$Item_Fat_Content=="LF"|train_bm$Item_Fat_Content=="low fat")
train_bm[Index,"Item_Fat_Content"] <- "Low Fat"
Index2 <- which(train_bm$Item_Fat_Content=="reg")
train_bm[Index2,"Item_Fat_Content"] <- "Regular"

a$size
train_bm[ , c(7,8,9,10,11,12)]

importance(rf)
varImpPlot(rf2)

aa = as.data.frame(aa)
aa$net.result

dim(test_bm)
colnames(test_bm)

glimpse(train_bm)
importance()
head(opfile)

colnames(test_bm)

##train_bm%>%
  select(Item_Identifier, Item_Weight,   Item_Fat_Content, Item_Visibility, Item_Type, Outlet_Identifier, Outlet_Location_Type)

glimpse(train_bm)

test_bm$sales1 = predict(dt_bm, test_bm)
train$sales = predict(dt_bm, test_bm)

library(rattle)

rattle()
head(test_bm)

train_jj = read_csv("C:/Users/Patrick Jane/Google Drive/Kaggle/Jester_Joke Prediction/train_MaefO4x/train.csv")

library(dummies)

dummies::dummy.data.frame()
