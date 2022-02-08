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
#examiner_gs <- read_csv("/Users/matthewbuttlerives/Desktop/Data Folder/USPTO_data/examiner_gs.csv")

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
#examiner_au <- read_csv("/Users/matthewbuttlerives/Desktop/Data Folder/USPTO_data/examiner_aus.csv")

# count examiner moves
library(dplyr)
detach(package:plyr)
examiner_moves <- examiner_au %>% 
  arrange(old_pid,year,month) %>% 
  distinct(old_pid,examiner_art_unit) %>% # keep unique examiner-AU combinations
  group_by(old_pid) %>% 
  mutate(
    au = examiner_art_unit,
    wg = floor(au/10)*10,
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


#### TRANSITION MATRIX - WORK GROUP ####

## Generate list of art unit
# Combine year and month
examiner_au <- examiner_au %>%
  mutate(date = make_date(year, month))

# Get tc from examiner_art_unit
examiner_au <- examiner_au %>%
  mutate(wg = (examiner_art_unit%/%10) * 10)

## Compute transition matrix
# First we extract list of all examiners' wg and dates  & consider wg movements from 2010
wg_date <- dplyr::select(examiner_au,c("old_pid","wg","date")) %>% filter(date >="2010-01-01")
# Remove invalid wg
wg_date <- wg_date[(wg_date$wg>=1000),]


# Compute the actual number of examiners per wg and time period
library(plyr)
art_unit <- ddply(wg_date, .(wg_date$wg,wg_date$date), nrow)
names(art_unit) <- c("wg","date","n_examiners")

# Reshape #examiners per wg against date
art_unit_reshaped <- spread(art_unit, date, n_examiners)
rownames(art_unit_reshaped) <- art_unit_reshaped$wg
art_unit_reshaped <- select(art_unit_reshaped,-c("wg"))

# Patch all NA to 0
art_unit_reshaped[is.na(art_unit_reshaped)] <- 0

# Export to csv
write.csv(art_unit_reshaped,"art_unit_actual.csv")

# Remove wg of less than 1 occurrence per time period
art_unit_reshaped <- art_unit_reshaped[rowSums(art_unit_reshaped) >=ncol(art_unit_reshaped), , drop=FALSE]

# Spread dates into column and keep each row per examiner corresponding to a single run of the Markov chain 
wg_date <- wg_date[wg_date$wg %in% row.names(art_unit_reshaped), ]
wglist <- spread(wg_date,date,wg)
wglist <- wglist[2:ncol(wglist)]

# Observed some blanks per examiner/row between consecutive time period. Fill out the missing
for (i in 1:nrow(wglist)){
  temp = NA
  lastpos = 1
  for (j in 1:ncol(wglist)){
    if (!is.na(wglist[i,j])){
      lastpos <- j
    }
  } 
  print(lastpos)
  for (j in 1:ncol(wglist)){
    if (!is.na(wglist[i,j])){
      temp <- wglist[i,j]
      message(j, " ", temp)
    }
    if ((is.na(wglist[i,j])) && (!is.na(temp)) && j<=lastpos){
      wglist[i,j] <- temp
      message(j, " ",wglist[i,j], "new")
    }
  }
}


# Compute first-order Markov transition matrix 
trans.matrix <- function(X)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  tt <- tt / rowSums(tt)
  tt
}

# Generate transition matrix
trans_matrix_table <- trans.matrix(as.matrix(wglist))
trans_matrix <- as.data.frame.matrix(trans_matrix_table) 

# Apply matrix transpose
trans_matrix_tranpose <- as.data.frame(t(as.matrix(trans_matrix)))

# Change all nan to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

trans_matrix_tranpose[is.nan(trans_matrix_tranpose)] <- 0

# Export to csv
write.csv(trans_matrix_tranpose,"trans_matrix_workgroup.csv")


#### TRANSITION MATRIX - GENDER ####
## Compute transition matrix
# Look up gender from directory
gender_date <- left_join(wg_date, examiner_moves, c("old_pid" = "old_pid"))
gender_date <- select(gender_date,c("old_pid","woman","date"))

# Compute the actual number of examiners per gender and time period
library(plyr)
gender <- ddply(gender_date, .(gender_date$woman,gender_date$date), nrow)
names(gender) <- c("gender","date","n_examiners")

# Drop records of NA gender
gender <- gender[complete.cases(gender), ]

# Reshape #examiners per gender against date
gender_reshaped <- spread(gender, date, n_examiners)
rownames(gender_reshaped) <- gender_reshaped$gender
gender_reshaped <- select(gender_reshaped,-c("gender"))

# Export to csv
write.csv(art_unit_reshaped,"gender_actual.csv")

# Spread dates into column and keep each row per examiner corresponding to a single run of the Markov chain 
genderlist <- spread(gender_date,date,woman)
genderlist <- genderlist[2:ncol(genderlist)]

# Observed some blanks per examiner/row between consecutive time period. Fill out the missing
for (i in 1:nrow(genderlist)){
  temp = NA
  lastpos = 1
  for (j in 1:ncol(genderlist)){
    if (!is.na(genderlist[i,j])){
      lastpos <- j
    }
  } 
  print(lastpos)
  for (j in 1:ncol(genderlist)){
    if (!is.na(genderlist[i,j])){
      temp <- genderlist[i,j]
      message(j, " ", temp)
    }
    if ((is.na(genderlist[i,j])) && (!is.na(temp)) && j<=lastpos){
      genderlist[i,j] <- temp
      message(j, " ",genderlist[i,j], "new")
    }
  }
}
rm(temp)

genderlist[] <- lapply(genderlist, as.character)

# Generate transition matrix
trans_matrix_table <- trans.matrix(as.matrix(genderlist))
trans_matrix <- as.data.frame.matrix(trans_matrix_table) 

# Apply matrix transpose
trans_matrix_tranpose <- as.data.frame(t(as.matrix(trans_matrix)))

# Export to csv
write.csv(trans_matrix_tranpose,"trans_matrix_gender.csv")

#### TRANSITION MATRIX - SENIORITY ####
# Extract list of all examiners' wg and dates  & consider wg movements from 2003 and look back 7 years+
wg_date2 <- dplyr::select(examiner_au,c("old_pid","wg","date")) %>% filter(date >="2003-01-01")
# Remove invalid wg
wg_date2 <- wg_date2[(wg_date2$wg>=1000),]

# Compute the actual number of examiners per wg and time period
library(plyr)
art_unit2 <- ddply(wg_date2, .(wg_date2$wg,wg_date2$date), nrow)
names(art_unit2) <- c("wg","date","n_examiners")

# Reshape #examiners per wg against date
art_unit_reshaped2 <- spread(art_unit2, date, n_examiners)
rownames(art_unit_reshaped2) <- art_unit_reshaped2$wg
art_unit_reshaped2 <- select(art_unit_reshaped2,-c("wg"))

# Patch all NA to 0
art_unit_reshaped2[is.na(art_unit_reshaped2)] <- 0

# Remove wg of less than 1 occurrence per time period
art_unit_reshaped2 <- art_unit_reshaped2[rowSums(art_unit_reshaped2) >=ncol(art_unit_reshaped2), , drop=FALSE]

# Spread dates into column and keep each row per examiner corresponding to a single run of the Markov chain 
wg_date2 <- wg_date2[wg_date2$wg %in% row.names(art_unit_reshaped2), ]
wglist2 <- spread(wg_date2,date,wg)
wglist2 <- wglist2[2:ncol(wglist2)]

# Merge 7years+ early records to wglist
wglist2 <- merge(wglist2[c(1:84)], wglist, by="row.names", all.y=TRUE)
wglist2 <- wglist2[2:ncol(wglist2)]

# Observed some blanks per examiner/row between consecutive time period. Fill out the missing
for (i in 1:nrow(wglist2)){
  temp = NA
  lastpos = 1
  for (j in 1:ncol(wglist2)){
    if (!is.na(wglist2[i,j])){
      lastpos <- j
    }
  } 
  print(lastpos)
  for (j in 1:ncol(wglist2)){
    if (!is.na(wglist2[i,j])){
      temp <- wglist2[i,j]
      message(j, " ", temp)
    }
    if ((is.na(wglist2[i,j])) && (!is.na(temp)) && j<=lastpos){
      wglist2[i,j] <- temp
      message(j, " ",wglist2[i,j], "new")
    }
  }
}

# Compute senior=1,junior=0 
sjlist <- wglist
for (i in 1:nrow(sjlist)){
  for (j in 1:ncol(sjlist)){
    for (k in 1:48){
      if (!is.na(wglist2[i,j+k-1]) && (!is.na(sjlist[i,j]))){
        sjlist[i,j] <- 1
      } else if (!is.na(sjlist[i,j])){
        sjlist[i,j] <- 0
      }
    }
  }
}

sjlist[] <- lapply(sjlist, as.character)

# Generate transition matrix
trans_matrix_table <- trans.matrix(as.matrix(sjlist))
trans_matrix <- as.data.frame.matrix(trans_matrix_table) 

# Apply matrix transpose
trans_matrix_tranpose <- as.data.frame(t(as.matrix(trans_matrix)))

# Export to csv
write.csv(trans_matrix_tranpose,"trans_matrix_seniority.csv")


#### TRANSITION MATRIX - GENDER X SENIORITY ####
# to multiply genderlist (2x2) and sjlist (2x2) for a combined matrix (4x4)
sjlist2 <- as.data.frame(lapply(sjlist, as.character))
genderlist2 <- as.data.frame(lapply(genderlist, as.character))
sjlist2[sjlist2 == 1] <- "Sr"
sjlist2[sjlist2 == 0] <- "Jr"
genderlist2[genderlist2 == 1] <- "Women"
genderlist2[genderlist2 == 0] <- "Men"

sjgenderlist<-sjlist2
sjgenderlist[] <- NA
for (i in 1:nrow(sjlist2)){
  for (j in 1:ncol(sjlist2)){
    if ((!is.na(sjlist2[i,j])) && (!is.na(genderlist2[i,j]))){
      sjgenderlist[i,j] <- paste(sjlist2[i,j], genderlist2[i,j], sep="-")
    }
  }
}

trans_matrix_table <- trans.matrix(as.matrix(sjgenderlist))
trans_matrix <- as.data.frame.matrix(trans_matrix_table) 

write.csv(trans_matrix,"trans_matrix_seniority_gender.csv")


# compute unique levels in data frame
lvls <- unique(unlist(sjgenderlist))
# apply the summation per value 
freq <- sapply(sjgenderlist, 
               function(x) table(factor(x, levels = lvls, 
                                        ordered = TRUE)))
# sort by row names in ascending order
freq <- freq[ order(row.names(freq)), ]

# export to csv
write.csv(freq,"seniority_gender_actual.csv")


# to check why sr->jr in sjlist is not 0
#sjlist[] <- lapply(sjlist, as.numeric)
#for (i in 1:nrow(sjlist2)){
#  for (j in 1:(ncol(sjlist2)-1)){
#    if (!is.na(sjlist2[i,j])){
#      if (!is.na(sjlist2[i,j+1])){
#        if (sjlist2[i,j]=="sj" && sjlist2[i,j+1]=="jr"){
#          message("row:",i," col:",j)
#        }
#      }
#    }
#  }
#}

