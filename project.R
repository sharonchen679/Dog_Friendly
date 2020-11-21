library(data.table)
library(ggplot2)# for the height of bars which represents the number of cases at each x position 
library(GGally) # for visual correlarion graph-matrix 
library(lattice)
library(caret) 
library(randomForest)
library(rpart)
library(rattle)
library(ROSE) # for randomly over sampling 
library(magrittr)# for %>% operator
library(dplyr) # for mutate fuc
library(rpart) # for decision tree graph
library(rpart.plot)  

data= fread("data_clean.csv")
orginalData=data

#####################
#   SET UP DATA     #
#####################

#NOTE: outlier! value in row 118 of data$dog_age is too high (511 years)
# fix: put median value instead
data$dog_age[118] = round(x = median(data$dog_age[c(1:117,119:203)]), digits = 1)
 

#change all strings to numeric values
data$find_dog_friendly_places =
  as.integer(replace(replace(replace(data$find_dog_friendly_places,
                          which(data$find_dog_friendly_places %in% "good idea"), 1),
                          which(data$find_dog_friendly_places %in% "use other platform"), 0),
                          which(data$find_dog_friendly_places %in% "not interested"), 0))

data$dog_walker = as.integer(replace(replace(replace(data$dog_walker,
                          which(data$dog_walker %in% "regularly"), 2),
                          which(data$dog_walker %in% "seldom"), 1),
                          which(data$dog_walker %in% "never"), 0))

replace_strings = function(column_to_replace) {
  return(as.integer(replace(replace(replace(column_to_replace,
                              which(column_to_replace %in% "interested"), 1),
                              which(column_to_replace %in% "use other app"), 0),
                              which(column_to_replace %in% "not interested"), 0)))
}

data$interest_dog_friendly        = replace_strings(data$interest_dog_friendly)
data$interest_dog_sitter          = replace_strings(data$interest_dog_sitter)
data$interest_dating              = replace_strings(data$interest_dating)
data$interest_price_comparison    = replace_strings(data$interest_price_comparison)
data$interest_dog_walker          = replace_strings(data$interest_dog_walker)
data$interest_dog_nearby          = replace_strings(data$interest_dog_nearby)
data$interest_second_hand_sharing = replace_strings(data$interest_second_hand_sharing)

data$owner_gender[data$owner_gender=="female"] <- 0
data$owner_gender[data$owner_gender=="male"] <- 1
data$owner_gender= as.integer(data$owner_gender)

data$occupation[data$occupation=="education"] <- 0
data$occupation[data$occupation== "finance and marketing"] <- 1
data$occupation[data$occupation== "high tech"] <- 1
data$occupation[data$occupation== "salaried employee"] <- 2
data$occupation[data$occupation== "self employed"] <- 3
data$occupation[data$occupation== "unemployed"] <- 4
data$occupation = as.integer(data$occupation)

data$district[data$district== "center"] <- 0
data$district[data$district== "north"] <- 1
data$district[data$district== "south"] <- 0
data$district= as.integer(data$district)

######################
# END OF SET UP DATA #
######################
#   Correlation      #
######################

#show graph of all correlation
plot(ggcorr(data, method = c("pairwise.complete.obs","pearson")))

#calculate correlation between variables:
print("best correlations are:")
for(i in 1:18)
  for(j in ((i+1):19)) {
    res = cor(data[[i]],data[[j]])
    if(res > 0.5 | res < -0.5) 
      print(paste0(names(data)[i]," ~ ",names(data)[j]," = ",res))
  }

######################
# END OF Correlation #
######################
#       Graphs       #
######################

#illustration of the best correlation 
ggplot(data, aes(interest_dog_walker, interest_dog_sitter))+
  geom_point(col = "blue")+
  ggtitle("interest_dog_sitter ~ interest_dog_walker")+
  theme(plot.title = element_text(hjust = .5))

ggplot(data, aes(petting_time,dog_age))+
  geom_point(col = "blue")+
  ggtitle("dog_age ~ petting_time")+
  theme(plot.title = element_text(hjust = .5))

ggplot(data, aes(interest_dating,interest_dog_nearby))+
  geom_point(col = "blue")+
  ggtitle("interest_dating ~ interest_dog_nearby")+
  theme(plot.title = element_text(hjust = .5))

######################
# Linear-Regression  #
######################

# after finding best parameters (which gives the highest correlation)
#   built a linear regression model for each pair

built_linear_regression_model = function(first, second, first_index, second_index) {
  s = summary(lm(formula = first~second))
  r.squared = s$r.squared
  p.value = pf(s$fstatistic[1],s$fstatistic[2],s$fstatistic[3],lower.tail=FALSE )
  attributes(p.value) = NULL
  slope = s$coefficients[2,1]
  color = ifelse(slope*100 > 0.5, "red", ifelse(slope*100 < -0.5,"green", "blue"))
  ylimit = c(min(first),max(first))
  xlimit = c(min(second),max(second))
  plot(ggplot(data, mapping = aes(second, first)) + coord_cartesian(xlim = xlimit, ylim = ylimit) + 
         geom_point() + geom_smooth(color = color, method = "lm") + theme_bw() +
         labs(title = paste0(names(data)[first_index],"~",names(data)[second_index]),
              x = paste0("p-value = ", round(p.value,4),"\nR-squared = ", round(r.squared,4)), y=""))
}
built_linear_regression_model(data$petting_time, data$dog_age, 1, 19)
built_linear_regression_model(data$interest_dating, data$interest_dog_nearby, 10, 13)
built_linear_regression_model(data$interest_dog_sitter, data$interest_dog_walker, 9, 12)

######################
# END OF L.Regression#
######################
#     more Graphs    #
######################

#graphs and models doesn't give much new information
#therefore, the focus will be on "interest_dog_friendly" as a general interest parameter

# simple graphs
plot(ggplot(data, aes(interest_dog_friendly, fill = as.factor(dog_age)))+
  geom_bar()+
  ggtitle("dog_age")+
  theme(plot.title = element_text(hjust = .5)))

plot(ggplot(data, aes(interest_dog_friendly, fill = as.factor(occupation)))+
  geom_bar()+
  ggtitle("occupation")+
  theme(plot.title = element_text(hjust = .5)))

plot(ggplot(data, aes(interest_dog_friendly, fill = as.factor(district)))+
  geom_bar()+
  ggtitle("district")+
  theme(plot.title = element_text(hjust = .5)))

plot(ggplot(data, aes(interest_dog_friendly, fill = as.factor(petting_time)))+
  geom_bar()+
  ggtitle("petting Time")+
  theme(plot.title = element_text(hjust = .5)))

# advanced box plot graphs
data %>%
  mutate(interest_dog_friendly = as.factor(interest_dog_friendly)) %>%
  ggplot(aes(interest_dog_friendly, dog_age, fill = interest_dog_friendly))+
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(color = interest_dog_friendly),alpha = 0.6)+
  xlab("interest_dog_friendly") +
  ylab("dog_age")

ggplot(data, aes(x = as.factor(interest_dog_friendly), y = data$dog_age)) + 
  geom_violin(aes(fill = as.factor(interest_dog_friendly))) + 
  geom_boxplot(width = 0.2)+
  labs(x="interest_dog_friendly",y="dog_age",title="dog_age Distribution")

data %>%
  mutate(interest_dog_friendly = as.factor(interest_dog_friendly)) %>%
  ggplot(aes(interest_dog_friendly, data$owner_age, fill = interest_dog_friendly))+
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(color = interest_dog_friendly),alpha = 0.6)+
  xlab("interest_dog_friendly") +
  ylab("owner_age")

ggplot(data, aes(x = as.factor(interest_dog_friendly), y = data$owner_age)) + 
  geom_violin(aes(fill = as.factor(interest_dog_friendly))) + 
  geom_boxplot(width = 0.2)+
  labs(x="interest_dog_friendly",y="owner_age",title="owner_age Distribution")

data %>%
  mutate(interest_dog_friendly = as.factor(interest_dog_friendly)) %>%
  ggplot(aes(interest_dog_friendly, data$petting_time, fill = interest_dog_friendly))+
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(color = interest_dog_friendly),alpha = 0.6)+
  xlab("interest_dog_friendly") +
  ylab("petting_time")

ggplot(data, aes(x = as.factor(interest_dog_friendly), y = data$petting_time)) + 
  geom_violin(aes(fill = as.factor(interest_dog_friendly))) + 
  geom_boxplot(width = 0.2)+
  labs(x="interest_dog_friendly",y="petting_time",title="petting_time Distribution")

data %>%
  mutate(interest_dog_friendly = as.factor(interest_dog_friendly)) %>%
  ggplot(aes(interest_dog_friendly, data$going_out_with_dog, fill = interest_dog_friendly))+
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(color = interest_dog_friendly),alpha = 0.6)+
  xlab("interest_dog_friendly") +
  ylab("going_out_with_dog")

ggplot(data, aes(x = as.factor(interest_dog_friendly), y = data$going_out_with_dog)) + 
  geom_violin(aes(fill = as.factor(interest_dog_friendly))) + 
  geom_boxplot(width = 0.2)+
  labs(x="interest_dog_friendly",y="going_out_with_dog",title="going_out_with_dog Distribution")

######################
#   END OF Graphs    #
######################
#   Data Scaling     #
######################

data[,1:7] <- lapply(data[,1:7], factor)
data[,8:17] <- lapply(data[,8:17], factor)  
data$owner_age<-as.numeric(data$owner_age)
data$dog_age<-as.numeric(data$dog_age)
data$owner_age<-scale(data$owner_age)
data$dog_age<-scale(data$dog_age)

######################
#   Data Partition   #
######################
#Ensure that When partition the data, we can get same training and testing data sets
set.seed(123)

# 70% of the data goes to train data set, and 30% goes to test data set 
#independent sampales
independent <- sample(2,nrow(data), replace = T, prob = c(0.7,0.3))
training_set <- data[independent==1,]
testing_set<- data [independent==2,]


########################
#  END Data Partition  #
########################
#  Random Forest Model #
########################

#Data for developing Predictive Model:
prop.table(table(training_set$interest_dog_friendly))#check

plot(ggplot(training_set, aes(interest_dog_friendly, fill = as.factor(interest_dog_friendly)))+
       geom_bar()+
       ggtitle("Training Set- Target Variable")+
       theme(plot.title = element_text(hjust = .5)))

#predective model evaluation with test
rftrain <- randomForest(interest_dog_friendly~., data=training_set)
confusionMatrix(predict(rftrain, testing_set, type= "class"),
                testing_set$interest_dog_friendly,positive='1')


#over sampling method: Handling Class Imbalance Problem
#N = 117*2=234
over= ovun.sample(interest_dog_friendly~.,data = training_set, method = "over",N=234)$data
prop.table(table(over$interest_dog_friendly))#check

plot(ggplot(over, aes(interest_dog_friendly, fill = as.factor(interest_dog_friendly)))+
       geom_bar()+
       ggtitle("Training Set- Target Variable")+
       theme(plot.title = element_text(hjust = .5)))

rfover <- randomForest(interest_dog_friendly~., data=over)
confusionMatrix(predict(rfover, testing_set, type="class"),testing_set$interest_dog_friendly,positive='1')


#under sampling method: Handling Class Imbalance Problem:
#N = 31*2=62
under<- ovun.sample(interest_dog_friendly~.,data = training_set, method = "under", N=62)$data
prop.table(table(under$interest_dog_friendly))#check

plot(ggplot(under, aes(interest_dog_friendly, fill = as.factor(interest_dog_friendly)))+
       geom_bar()+
       ggtitle("Training Set- Target Variable")+
       theme(plot.title = element_text(hjust = .5)))

rfunder <- randomForest(interest_dog_friendly~., data=under)
confusionMatrix(predict(rfunder, testing_set,type = "class"),testing_set$interest_dog_friendly,positive='1')

#BOTH- over and under sampling method: Handling Class Imbalance Problem:
#N = length(training_set) = 148
both<-ovun.sample(interest_dog_friendly~.,data = training_set, method = "both",
                  p=0.5,seed = 222, N=148)$data
prop.table(table(both$interest_dog_friendly))# check

plot(ggplot(both, aes(interest_dog_friendly, fill = as.factor(interest_dog_friendly)))+
       geom_bar()+
       ggtitle("Training Set- Target Variable")+
       theme(plot.title = element_text(hjust = .5)))

rfboth <- randomForest(interest_dog_friendly~., data=over)
confusionMatrix(predict(rfboth, testing_set, type="class"),testing_set$interest_dog_friendly,positive='1')

######################
#   Decision Tree    #
######################
tree <- rpart(interest_dog_friendly~., data = training_set, method="class")
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
predict_set=predict(tree, testing_set, type="class")
confusionMatrix(predict_set,testing_set$interest_dog_friendly,positive='1')

#over sampling method: Handling Class Imbalance Problem
tree_over<-rpart(interest_dog_friendly~., data = over, method="class")
confusionMatrix(predict(tree_over, testing_set, type="class"),testing_set$interest_dog_friendly,positive='1')

#under sampling method: Handling Class Imbalance Problem:
tree_under<-rpart(interest_dog_friendly~., data = under, method="class")
confusionMatrix(predict(tree_under, testing_set, type="class"),testing_set$interest_dog_friendly,positive='1')

#BOTH- over and under sampling method: Handling Class Imbalance Problem:
tree_both<-rpart(interest_dog_friendly~., data = both, method="class")
confusionMatrix(predict(tree_both, testing_set, type="class"),testing_set$interest_dog_friendly,positive='1')
