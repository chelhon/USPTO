knitr::opts_chunk$set(echo = TRUE)
##### setup -----
rm(list = ls())
gc()
library(tidyverse)
library(skimr)
library(lubridate)
library(arrow)
library(modelsummary)
set.seed(10101)

examiner_gs <- read_csv("examiner_gs.csv")
examiner_gs <- read_csv("/Users/matthewbuttlerives/Desktop/Data Folder/USPTO_data/examiner_gs.csv")
##### >> calculate duration in grade -----
# Use latest observed date to replace NAs
max_end_date <- examiner_gs %>% 
  summarise(max(mdy(end_date,"m/d/y"), na.rm = TRUE)) %>% 
  pull()
# get number of days in a GS grade
examiner_gs <- examiner_gs %>% 
  mutate(
    start_date_old = start_date, # for manual verification
    end_date_old = end_date, # for manual verification
    start_date = mdy(start_date_old),
    end_date = if_else(is.na(end_date),max_end_date,mdy(end_date)),
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  select(-start_date_old,-end_date_old)
##### >> add examiner gender -----
# Using a modified example from https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html
library(gender)
#install_genderdata_package() # only run the first time
examiner_gender <- examiner_gs %>% 
  mutate(
    name = examiner_name,
    ff_name = str_extract(examiner_name,"(?<=,\\s)\\w+"), # extract first first name
  ) %>% 
  distinct(ff_name) %>% 
  do(results = gender(.$ff_name, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    ff_name = name,
    gender,
    proportion_female
  )
# joining gender back to the dataset
examiner_gs <- examiner_gs %>% 
  mutate(
    ff_name = str_extract(examiner_name,"(?<=,\\s)\\w+"), # extract first first name
  ) %>% 
  left_join(examiner_gender, by = "ff_name")
# cleaning up
rm(examiner_gender)
gc()

##### >> add examiner race -----
# Add a column race based on examiner's surname using package 'wru' (but unsure what to use as the county/tract/block code)
examiner_gs <- examiner_gs %>% 
  separate(examiner_name, c("surname", "firstname"))
examiner_gs <- select(examiner_gs,-c("firstname"))

library(wru)
examiner_gs$county <- "061"
examiner_gs <- predict_race(examiner_gs, census.surname = TRUE, surname.only = TRUE)
examiner_gs$race <- colnames(examiner_gs[12:16])[max.col(examiner_gs[12:16], ties.method = "first")]
examiner_gs$race = substr(examiner_gs$race,6,8)
examiner_gs <- select(examiner_gs,-c("pred.whi","pred.bla","pred.his","pred.asi","pred.oth"))


# Load AU data
examiner_au <- read_csv("examiner_aus.csv")
examiner_au <- read_csv("/Users/matthewbuttlerives/Desktop/Data Folder/USPTO_data/examiner_aus.csv")

# count examiner moves
examiner_moves <- examiner_au %>% 
  arrange(old_pid,year,month) %>% 
  distinct(old_pid,examiner_art_unit) %>% # keep unique examiner-AU combinations
  group_by(old_pid) %>% 
  mutate(
    au = examiner_art_unit,
    tc = floor(au/100)*100,
    moves = (n()-1)
  ) %>% 
  ungroup()
datasummary_skim(examiner_moves, histogram=FALSE)


# adjust for the wrong AU values
examiner_moves <- examiner_au %>% 
  arrange(old_pid,year,month) %>% 
  distinct(old_pid,examiner_art_unit) %>% 
  mutate(tc = floor(examiner_art_unit/100)*100) %>% 
  add_count(tc) %>% 
  mutate(
    tc = if_else(n<1000,NA_real_,tc), # TCs with fewer than 1K examiners are not real
  ) %>% 
  filter(!is.na(tc)) %>% # drop them
  select(-n) %>% 
  group_by(old_pid) %>% 
  mutate(
    first_au = if_else(row_number()==1,examiner_art_unit,0),
    moves = (n()-1),
    has_moved = if_else(moves>0,1,0)
  ) %>% 
  ungroup() %>% 
  filter(first_au!=0) %>% 
  select(
    old_pid,
    first_au,
    first_tc = tc,
    moves,
    has_moved
  )
datasummary_skim(examiner_moves, histogram=FALSE)





# add examiner race, gender and tenure
examiner_race_gender_tenure <- examiner_gs %>% 
  filter(!is.na(gender)) %>% 
  mutate(
    woman = if_else(gender=="female",1,0)
  ) %>%
  group_by(old_pid, race) %>% 
  summarise(
    woman = max(woman),
    tenure = sum(days_in_grade)/365
  )
datasummary_skim(examiner_race_gender_tenure, histogram=FALSE)

# remove negative tenure values
examiner_race_gender_tenure <- examiner_race_gender_tenure %>% 
  filter(tenure>0)
# add back to the examiner_moves
examiner_moves <- examiner_moves %>% 
  left_join(examiner_race_gender_tenure)
datasummary_skim(examiner_race_gender_tenure, histogram=FALSE)
# clean up
rm(examiner_race_gender_tenure)
gc()

# Check for Null Values
colSums(is.na(examiner_moves)) > 0 
# Race, Woman and Tenure have null values so I will drop rows with null values (with concerns dropping minority group which race is NA)
library(tidyr)
examiner_moves = examiner_moves %>% drop_na()

# Dummify race
library(fastDummies)
examiner_moves <- dummy_cols(examiner_moves, select_columns = 'race', remove_first_dummy=TRUE)
examiner_moves <- select(examiner_moves,-c("race"))

## Model selection (XGBoost)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)


# Training & Testing Data of examiner_moves dataset
set.seed(1) 
parts = createDataPartition(examiner_moves$has_moved, p = .8, list = F)
train = examiner_moves[parts, ]
test = examiner_moves[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[,c(2,3,6,7,8,9,10,11)])
train_y = data.matrix(train[,5])

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(2,3,6,7,8,9,10,11)])
test_y = data.matrix(test[, 5])

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

# understand feature importance with ranger package
library(ranger)
testmodel = ranger(has_moved ~ first_au+first_tc+woman+tenure+race_bla+race_his+race_oth+race_whi, data=test,importance='impurity')
testmodel$variable.importance
# By importance, moves > first_au > tenure > first_tc
# By importance, race_oth > race_his > woman > race_whi > first_tc > tenure

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)
# Minimum RMSE is 30, so our XGBoost will use 30 rounds

# compute feature importance
#xgb.importance(model = model)
names <- unlist(dimnames(xgb_train)[[2]])
importance_matrix <- xgb.importance(names, model=model)
xgb.plot.importance(importance_matrix[1:10,])
# xgboost only includes the features actually used in the fitted model, which is moves 

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 30, verbose = 0)

# Run Predictions
pred_y = predict(final, xgb_test)

# Error Terms
mean((test_y - pred_y)^2) #Mean sum of Error = 0.1680916
caret::MAE(test_y, pred_y) #Absolute Error = 0.3451435
caret::RMSE(test_y, pred_y) #Root Mean Error = 0.4099898
pred_y


## Prediction
table(pred = predict(final, test_x),
      true = test_y)
# Our model is 100% accurate. This is likely due to our model having a predictor
# that is perfectly correlated to has_moved, which I believe to be is moves

# add P_move from predictions
test$P_move=pred_y

# load model packages
library(modelr)
library(infer)

# correlation between prediction and true observed value 
corr_ols1 <- test %>% 
  observe(has_moved ~ P_move, stat = "correlation") %>% 
  pull(stat)
corr_ols1

# r squared
corr_ols1^2

# visualize probability of move `P_move`
g <- ggplot(data = test, mapping = aes(x = P_move))
g + geom_histogram(orientation = "")


# Let's pick a threshold that's close to the center of the distribution, say 0.7
tr1 <- 0.7

test <- test %>% 
  mutate(pr_moved_tr1 = if_else(P_move>tr1,1,0))
test

# Let's calculate TPR and FPR based on true/false positives and true/false negatives. 
gof_tr1 <- test %>% 
  filter(!is.na(P_move)) %>% 
  mutate(
    TP = if_else(pr_moved_tr1==1 & has_moved==1,1,0),
    FP = if_else(pr_moved_tr1==1 & has_moved==0,1,0),
    TN = if_else(pr_moved_tr1==0 & has_moved==0,1,0),
    FN = if_else(pr_moved_tr1==0 & has_moved==1,1,0)
  )  %>% 
  summarise(
    a = sum(TP),
    b = sum(FN),
    c = sum(FP),
    d = sum(TN)
  ) %>% 
  mutate(
    TPR = a/(a+b),
    FPR = c/(c+d)
  )

gof_tr1
# TPR = 0.644, FPR = 0.179

# Can we do better with a different threshold? Let's try a smaller one, say 0.6
tr2 <- 0.6

test <- test %>% 
  mutate(pr_moved_tr2 = if_else(P_move>tr2,1,0))

gof_tr2 <- test %>% 
  filter(!is.na(P_move)) %>% 
  mutate(
    TP = if_else(pr_moved_tr2==1 & has_moved==1,1,0),
    FP = if_else(pr_moved_tr2==1 & has_moved==0,1,0),
    TN = if_else(pr_moved_tr2==0 & has_moved==0,1,0),
    FN = if_else(pr_moved_tr2==0 & has_moved==1,1,0)
  )  %>% 
  summarise(
    a = sum(TP),
    b = sum(FN),
    c = sum(FP),
    d = sum(TN)
  ) %>% 
  mutate(
    TPR = a/(a+b),
    FPR = c/(c+d)
  )

gof_tr2
# TPR = 0.804, FPR = 0.804


# Can we do better with a different threshold? Let's try a smaller one, say 0.75
tr3 <- 0.75

test <- test %>% 
  mutate(pr_moved_tr3 = if_else(P_move>tr3,1,0))

gof_tr3 <- test %>% 
  filter(!is.na(P_move)) %>% 
  mutate(
    TP = if_else(pr_moved_tr3==1 & has_moved==1,1,0),
    FP = if_else(pr_moved_tr3==1 & has_moved==0,1,0),
    TN = if_else(pr_moved_tr3==0 & has_moved==0,1,0),
    FN = if_else(pr_moved_tr3==0 & has_moved==1,1,0)
  )  %>% 
  summarise(
    a = sum(TP),
    b = sum(FN),
    c = sum(FP),
    d = sum(TN)
  ) %>% 
  mutate(
    TPR = a/(a+b),
    FPR = c/(c+d)
  )

gof_tr3
# TPR = 0.520, FPR = 0.106

## ROC plots
start_end <- tibble(TPR = c(0,1), FPR = c(0,1))
## ROC
roc_data <- gof_tr1 %>% 
  add_row(gof_tr2) %>% 
  add_row(gof_tr3) %>% 
  select(TPR,FPR) %>% 
  add_row(start_end)

roc_data

g <- ggplot(roc_data, aes(x = FPR, y= TPR))

g + geom_point() +
  geom_smooth() +
  geom_abline(intercept = 0) +
  xlim(0,1)+
  ylim(0,1)