################################################################################
#                                O V E R V I E W                               # 
################################################################################

# Title:    Programming with R group assignment 

##load packages ----------------------------------------------------------------

library("base")
library("dplyr")
library("ggplot2")
library("caret")
library("kernlab")


################################################################################
#                            P R E P R O C E S S I N G                         # 
################################################################################

##open dataset -----------------------------------------------------------------

d1 <- read.csv("input/student-por.csv", stringsAsFactors = FALSE, sep = ";")

##explore dataset --------------------------------------------------------------

View(d1)
dim(d1)

#649 observations and 33 variables

##rename dataset ---------------------------------------------------------------

students <- d1
students

##information about the structure of objects -----------------------------------

str(students)
#types of variables: characters and integers

##preprocessing data -----------------------------------------------------------

##check for missing values -----------------------------------------------------

na_check <- sapply(students, function(x) sum(is.na(x)))
na_check

#no missing values

##change names -----------------------------------------------------------------

#colnames(students)
colnames(students) <- c("school" , "sex", "age", "address", "family_size", 
                        "parent's_status", "mother_edu", "father_edu", 
                        "mother_job", "father_job", "reason", "guardian", 
                        "travel_time", "study_time", "failures", 
                        "school_edu_support", "family_edu_support", 
                        "extra_paid_cl", "activities", "nursery", "higher_edu", 
                        "internet", "romantic_rel", "family_rel", "freetime", 
                        "go_out", "workday_alc", "weekend_alc", "health", 
                        "absences", "1st_period_grade", "2nd_period_grade", 
                        "final_grade")
View(students)

##transform categorical into factors -------------------------------------------

students_cat <- students[ , c(1, 2, 4, 5, 6, 9:12, 16:23)] 

students_cat <- data.frame(lapply(students_cat, factor))

students_num <- students[ , -c(1, 2, 4, 5, 6, 9:12, 16:23)]
new_students <- cbind(students_cat, students_num)
#dim(new_students)
View(new_students)
#str(new_students)

##correlation between variables "1st_period_grade" and "2nd_period_grade" ------

cor_targets <- data.frame(new_students$`1st_period_grade`, 
                new_students$`2nd_period_grade`, new_students$final_grade)
correlation <- as.matrix(cor(cor_targets))
correlation

##exclude variables "1st_period_grade" and "2nd_period_grade" ------------------

clean_data <- select(new_students, -c(31,32))
View(clean_data)

##exploratory data analysis of the dataset -------------------------------------

##number of students that passed or failed their exam, categorised by their sex 

grade_comparison <- clean_data %>%
  
  mutate(pass = ifelse(final_grade >= 10, 1, 0),
         fail = ifelse(final_grade < 10, 1, 0))%>%
        
  group_by(sex) %>%
  summarise(pass = sum(pass),
            fail = sum(fail))
            


grade_comparison %>%
  ggplot(aes(x = sex, y = pass)) +
  geom_bar(stat = 'identity')

##mothers' and fathers' educational level and its effect on the grade of their 
##children ---------------------------------------------------------------------

fathers_education <- ggplot(clean_data, aes(x = factor(father_edu),
                                           y = final_grade)) +
  geom_boxplot(aes(fill = factor(father_edu)), alpha = 0.5, size = 0.5) +
  scale_fill_brewer(palette = "RdGy") +
  ggtitle("Students' grades distribution per fathers' education") +
  theme(axis.title.y = element_blank()) + theme(axis.title.x = element_blank())
fathers_education  

mothers_education <- ggplot(clean_data,aes(x = factor(mother_edu),
                                           y = final_grade)) +
  geom_boxplot(aes(fill = factor(mother_edu)), alpha = 0.5, size = 0.5) +
  scale_fill_brewer(palette = "RdGy") +
  ggtitle("Students' grades distribution per mothers' education") +
  theme(axis.title.y = element_blank()) + theme(axis.title.x = element_blank())
mothers_education


##relationship between absences and students' final grade ----------------------
  
students$absences <- as.factor(students$absences)
absences_finalgrades <- clean_data %>%
  group_by(absences)%>%
  summarise(MeanScore = mean(final_grade, na.rm = TRUE))%>%
  arrange(desc(MeanScore))
  
absences_finalgrades
  

##relationship between students' final grade and their mothers' occupation -----

mother_occupation <- ggplot(data = clean_data, 
                  mapping = aes(x = final_grade, fill = mother_job)) +
                  geom_bar()+
  ggtitle('Students’ final grade and their mothers’ occupation')

mother_occupation

##relationship between access to the internet and students' grades -------------

internet_grades <- clean_data %>%
  group_by(internet) %>%
  ggplot(aes(final_grade,fill = internet)) +
  geom_histogram(binwidth = 0.5)
 
 
 internet_grades

##anova analysis to find the most important attributes with target--------------
 
res.aov <- aov(final_grade ~ ., data = clean_data)
summary(res.aov)

##final dataset with most important variables ----------------------------------
 
final_data <- clean_data[ ,c(1:4, 6, 8:10, 15, 17, 19, 22:23, 25, 27, 29)]  
View(final_data)

##binarize the target in order to run the algorithms ---------------------------
 
final_data$final_grade <- factor(
  ifelse(clean_data$final_grade < 10, 1, 0), 
  labels = c("pass", "fail") 
) 

View(final_data)

##fitting the model ------------------------------------------------------------

##create data partition for the algorithms -------------------------------------

set.seed(1)
trn_index = createDataPartition(y = final_data$final_grade, p = 0.70, 
                                list = FALSE)
trn_student = final_data[trn_index, ]
tst_student = final_data[-trn_index, ]

################################################################################
#                                     K N N                                    # 
################################################################################


##build the model --------------------------------------------------------------

set.seed(1)
knn_model <- train(final_grade ~., method = "knn", data = trn_student,
          trControl = trainControl(method = "cv", number = 5),
          preProcess = c("center", "scale"))

##plot the model ---------------------------------------------------------------

knn_model
plot(knn_model)

##find the optimum k -----------------------------------------------------------

best_k <- knn_model$bestTune$k
str(best_k)

##make prediction on the test set ----------------------------------------------

knn_predict <- predict(knn_model, tst_student)

##convert the target column into factor to match the model ---------------------

tst_student$final_grade <- factor(tst_student$final_grade)

##create the confusion matrix --------------------------------------------------

knn_confM <- confusionMatrix(knn_predict, tst_student$final_grade)

knn_confM

##precision, recall, f-score----------------------------------------------------

precision_knn <- knn_confM$byClass['Pos Pred Value']   
precision_knn
recall_knn <- knn_confM$byClass['Sensitivity']
recall_knn
f_score_knn <- 2 * ((precision_knn * recall_knn) / (precision_knn + recall_knn))
f_score_knn

################################################################################
#                     L O G I S T I C  R E G R E S S I O N                     # 
################################################################################


##build the model---------------------------------------------------------------

set.seed(1)
student_lgreg = train(final_grade ~ ., method = "glm",
                      family = binomial(link = "logit"), data = trn_student,
                      trControl = trainControl(method = 'cv', number = 5))
student_lgreg

##summary ----------------------------------------------------------------------

summary(student_lgreg)

##predicting -------------------------------------------------------------------

predicted <- predict(student_lgreg, tst_student)
accuracy <- sum(predicted == tst_student$final_grade) /
  length(tst_student$final_grade)
accuracy

##confusion matrix -------------------------------------------------------------

lgr_confM <- confusionMatrix(predicted, tst_student$final_grade)
lgr_confM

##precision, recall, f-score----------------------------------------------------

precision_lgr <- lgr_confM$byClass['Pos Pred Value']  
precision_lgr
recall_lgr <- lgr_confM$byClass['Sensitivity']
recall_lgr
f_score_lgr <- 2 * ((precision_lgr * recall_lgr) / (precision_lgr + recall_lgr))
f_score_lgr

################################################################################
#                     S U P P O R T  V E C T O R   M A C H I N E               # 
################################################################################

##build the model---------------------------------------------------------------

set.seed(1)
svm_linear = train(final_grade ~., method = "svmLinear", data = trn_student,
                   trControl = trainControl(method = 'cv',number = 5))
svm_linear          

##predicting -------------------------------------------------------------------

test_svm <- predict(svm_linear, tst_student)
test_svm

##confusion matrix -------------------------------------------------------------

svm_confM <- confusionMatrix(test_svm, tst_student$final_grade)
svm_confM

##precision, recall, f-score----------------------------------------------------

precision_svm <- svm_confM$byClass['Pos Pred Value'] 
precision_svm
recall_svm <- svm_confM$byClass['Sensitivity']
recall_svm
f_score_svm <- 2 * ((precision_svm * recall_svm) / (precision_svm + recall_svm))
f_score_svm
