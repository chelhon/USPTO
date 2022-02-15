## Transitions by seniority and gender

We start with `applications` data file, because it’s the only source of
data on turnover. We load the file and add a gender column based on the
examiner’s name.

``` r
## load applications data
applications <- read_parquet(paste0(data_path,"app_data_sample.parquet")) %>% 
  rename(patex_id = examiner_id)

## get gender 
examiner_gender <- applications %>% 
  mutate(
    ff_name = str_extract(examiner_name_first,"\\w+"), # extract first first name
  ) %>% 
  distinct(ff_name) %>% 
  do(results = gender(.$ff_name, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    ff_name = name,
    gender,
    proportion_female
  ) %>% 
  mutate(woman = if_else(gender=="female",1,0))

## add gender back to the applications data
applications <- applications %>% 
  mutate(
    ff_name = str_extract(examiner_name_first,"\\w+"), # extract first first name
  ) %>% 
  left_join(examiner_gender, by = "ff_name")

## How many are missing?
applications %>% select(woman) %>% skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 2018477    |
| Number of columns                                | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| woman         |    249292 |          0.88 | 0.34 | 0.47 |   0 |   0 |   0 |   1 |    1 | ▇▁▁▁▅ |

### Add examiner seniority

Next, we can infer examiner tenure based on first/last observed date of
application. This will help us figure out junior/senior switch as well
as `onboard` and `exit` dates (if the examiner leaves).

``` r
## use timestamps on applications to approximate employment dates
examiner_tenure <- applications %>% 
  mutate(    
    filing_date_p = ymd(filing_date), # need to standardize data representation
    status_date_p = dmy_hms(appl_status_date) # so that min()/max() work properly
  ) %>% 
  group_by(patex_id) %>%
  summarise(
    min_filing_date = min(filing_date_p, na.rm = TRUE),
    max_filing_date = max(filing_date_p, na.rm = TRUE),
    min_status_date = min(status_date_p, na.rm = TRUE),
    max_status_date = max(status_date_p, na.rm = TRUE),
    ) %>% 
  rowwise() %>% # find minimum/maximum by row
  mutate(
    onboard_date = min(c(min_filing_date, min_status_date), na.rm = TRUE),
    exit_date = max(c(max_filing_date, max_status_date), na.rm = TRUE)
  ) %>% # clean up some date typos
  mutate(
    onboard_date = if_else(year(onboard_date)>2017 | year(onboard_date)<2000, NA_Date_, onboard_date),
    exit_date =    if_else(year(exit_date)>2017    | year(exit_date)<2000, NA_Date_,    exit_date   ),
    tenure = as.duration(onboard_date %--% exit_date) / dyears(1)
  ) %>% 
  select(
    patex_id,
    onboard_date,
    exit_date,
    tenure
  ) %>% 
  ungroup()

## plot tenure
examiner_tenure %>% 
  select(onboard_date,exit_date,tenure) %>% 
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 5649       |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| Date                                             | 2          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| onboard_date  |         0 |             1 | 2000-01-02 | 2016-03-03 | 2003-01-21 |     2325 |
| exit_date     |        24 |             1 | 2000-09-14 | 2017-12-06 | 2017-05-19 |      871 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |   sd |   p0 |  p25 |   p50 |   p75 |  p100 | hist  |
|:--------------|----------:|--------------:|------:|-----:|-----:|-----:|------:|------:|------:|:------|
| tenure        |        24 |             1 | 12.13 | 4.92 | 0.07 | 8.48 | 13.45 | 16.68 | 17.85 | ▂▂▂▅▇ |

Let’s find those examiners who left the agency. Of course, we have both
left and right truncation (censorship) in our sample, so if someone’s
last observed date is toward the end of our sample’s time window, that
person may still be employed when our observation stops. So let’s mark
those whom we don’t observe for at least 6 months as “left” the
organization. For them, we’ll keep the exit date. For the rest, will
change the exit date to missing, so that we don’t confuse ourselves.

``` r
## get min and max dates in the data to establish our sample time window
min_date <- examiner_tenure %>% 
  ungroup() %>% 
  summarise(m = min(onboard_date, na.rm = TRUE)) %>% 
  pull()

max_date <- examiner_tenure %>% 
  ungroup() %>% 
  summarise(m = max(onboard_date, na.rm = TRUE)) %>% 
  pull()

## only keep exit_date for those whom we don't see working for at least 6 months
examiner_tenure <- examiner_tenure %>% 
  mutate(
    exit_date = if_else(as.duration(exit_date %--% max_date) / dyears(1) > 0.5,
                        exit_date,
                        NA_Date_)
  )

examiner_tenure %>% 
  select(onboard_date,exit_date,tenure) %>% 
  skim()
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 5649       |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| Date                                             | 2          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| onboard_date  |         0 |          1.00 | 2000-01-02 | 2016-03-03 | 2003-01-21 |     2325 |
| exit_date     |      4738 |          0.16 | 2000-09-14 | 2015-09-02 | 2011-11-20 |      645 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |   sd |   p0 |  p25 |   p50 |   p75 |  p100 | hist  |
|:--------------|----------:|--------------:|------:|-----:|-----:|-----:|------:|------:|------:|:------|
| tenure        |        24 |             1 | 12.13 | 4.92 | 0.07 | 8.48 | 13.45 | 16.68 | 17.85 | ▂▂▂▅▇ |

### Calculate the transitions

Let’s assume a time period for our transition matrix of
*t*<sub>0</sub> = 2007 (the year we first get senior examiners,
according to our definition) and *t*<sub>1</sub> = 2011 (about a
mid-point in our time window). For each of the years, we need a vector
of indicators: + examiner is junior + examiner is senior + examiner is a
new hire <span style="color:blue">during this period </span> + examiner
exited <span style="color:blue">during this period </span>

<span style="color:blue">Note that examiners who have already exit in
previous periods will have state NA subsequently.</span>

``` r
   transitions <- examiner_tenure %>% 
  mutate(
    tenure_t0 = as.duration(onboard_date %--% ymd("2007-01-01")) / dyears(1),
    tenure_t1 = as.duration(onboard_date %--% ymd("2011-01-01")) / dyears(1),
    tenure_t2 = as.duration(onboard_date %--% ymd("2015-01-01")) / dyears(1),
    t0_state = case_when(
      tenure_t0<6 & tenure_t0>0 ~ "Junior"  , # Jr; not those yet to be hired!
      tenure_t0>=6              ~ "Senior"  , # Sr
      TRUE                      ~ NA_character_ # not yet hired
    ),
    t1_state = case_when(
      exit_date<ymd("2011-01-01")        ~ "Exit",
      onboard_date>ymd("2007-01-01") 
        & onboard_date<ymd("2011-01-01") ~ "New hire",
      tenure_t1<6 & tenure_t1>0          ~ "Junior"  , # Jr; not those yet to be hired!
      tenure_t1>=6                       ~ "Senior"  , # Sr
      TRUE                               ~ NA_character_ # not yet hired
      ),
    t2_state = case_when(
      t1_state=="Exit"                   ~ NA_character_,
      exit_date<ymd("2015-01-01")        ~ "Exit",
      onboard_date>ymd("2011-01-01") 
        & onboard_date<ymd("2015-01-01") ~ "New hire",
      tenure_t2<6 & tenure_t2>0          ~ "Junior"  , # Jr; not those yet to be hired!
      tenure_t2>=6                       ~ "Senior"  , # Sr
      TRUE                               ~ NA_character_ # not yet hired or already exit in previous period
      )
    )
```

<span style="color:blue">Now that we’ve identified an individual state
at *t*<sub>0</sub>, *t*<sub>1</sub> and *t*<sub>2</sub>, we can spread
dates into columns and keep each row per examiner corresponding to a
single run of Markov chain.</span>

``` r
transitions_seniority <- as.data.frame(select(transitions,c("patex_id","t0_state","t1_state","t2_state"))) %>% 
  drop_na(patex_id)
names(transitions_seniority) <- c("patex_id","t0","t1","t2")
rownames(transitions_seniority) <- transitions_seniority$patex_id
transitions_seniority <- select(transitions_seniority,-c("patex_id"))
head(transitions_seniority,10)
```

    ##           t0       t1       t2
    ## 59012 Junior   Senior   Senior
    ## 59025   <NA> New hire   Junior
    ## 59030 Junior   Junior   Senior
    ## 59040   <NA> New hire   Senior
    ## 59052 Junior     Exit     <NA>
    ## 59054 Senior   Senior   Senior
    ## 59055 Junior     Exit     <NA>
    ## 59056 Senior   Senior   Senior
    ## 59074 Senior   Senior   Senior
    ## 59081   <NA>     <NA> New hire

### Transitions matrix - general

<span style="color:blue">We can use the following function to compute
the overall seniority transition matrix by calculating the transition
combinations from one state to another as share of previous period. The
horizontal columns refer to the status before and the vertical rownames
refer to the status after </span>

``` r
# Function to compute first-order Markov transition matrix 
trans.matrix <- function(X)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  tt <- tt / rowSums(tt)
  t(tt)
}

# Function to reorder matrix to logical employment journey
order <- c("Junior", "Senior", "Exit")
trans.reorder <- function(X){
  X <- X %>%
  slice(match(order, rownames(X)))
  #X <- X[,order]
  X <- X %>% mutate(Exit=c(0,0,1)) # to hard code given only 3 periods are considered
  X <- X %>% mutate(across(where(is.numeric), ~ round(., 4)))  
}

# Create transition matrix (for t0 to t1, all)
trans_matrix_table <- trans.matrix(as.matrix(transitions_seniority[,c(1,2)]))
trans_matrix_all <- as.data.frame.matrix(trans_matrix_table) 
trans_matrix_all <- trans.reorder(trans_matrix_all)
trans_matrix_all
```

    ##        Junior Senior Exit
    ## Junior 0.2557 0.0000    0
    ## Senior 0.6258 0.9368    0
    ## Exit   0.1185 0.0632    1

``` r
rm(transitions_seniority)
```

### Transitions matrix - women

<span style="color:blue">Repeat the matrix generation for women.</span>

``` r
# First to look up gender from applications
id_gender <- unique(select(applications,c("patex_id","woman")))
transitions <- left_join(transitions, id_gender, by="patex_id", match="first")
rm(id_gender)

# Similarly, spread dates into columns and keep each row per examiner corresponding to a single run of Markov chain
transitions_seniority_women <- filter(transitions, woman==1)
transitions_seniority_women <- as.data.frame(select(transitions_seniority_women,c("patex_id","t0_state","t1_state","t2_state"))) %>% 
  drop_na(patex_id)
names(transitions_seniority_women) <- c("patex_id","t0","t1","t2")
rownames(transitions_seniority_women) <- transitions_seniority_women$patex_id
transitions_seniority_women <- select(transitions_seniority_women,-c("patex_id"))
#head(transitions_seniority_women,10)

# Create transition matrix (for t0 to t1, women)
trans_matrix_table <- trans.matrix(as.matrix(transitions_seniority_women[,c(1,2)]))
trans_matrix_women <- as.data.frame.matrix(trans_matrix_table) 
trans_matrix_women <- trans.reorder(trans_matrix_women)
trans_matrix_women
```

    ##        Junior Senior Exit
    ## Junior 0.2476 0.0000    0
    ## Senior 0.6388 0.9371    0
    ## Exit   0.1136 0.0629    1

``` r
rm(transitions_seniority_women)
```

### Transitions matrix - men

<span style="color:blue">Repeat the matrix generation for men this
time.</span>

``` r
# Similarly, spread dates into columns and keep each row per examiner corresponding to a single run of Markov chain
transitions_seniority_men <- filter(transitions, woman==0)
transitions_seniority_men <- as.data.frame(select(transitions_seniority_men,c("patex_id","t0_state","t1_state","t2_state"))) %>% 
  drop_na(patex_id)
names(transitions_seniority_men) <- c("patex_id","t0","t1","t2")
rownames(transitions_seniority_men) <- transitions_seniority_men$patex_id
transitions_seniority_men <- select(transitions_seniority_men,-c("patex_id"))
#head(transitions_seniority_men,10)

# Create transition matrix (for t0 to t1, women)
trans_matrix_table <- trans.matrix(as.matrix(transitions_seniority_men[,c(1,2)]))
trans_matrix_men <- as.data.frame.matrix(trans_matrix_table) 
trans_matrix_men <- trans.reorder(trans_matrix_men)
trans_matrix_men
```

    ##        Junior Senior Exit
    ## Junior 0.2567 0.0000    0
    ## Senior 0.6190 0.9348    0
    ## Exit   0.1243 0.0652    1

``` r
rm(transitions_seniority_men)
```

## Prediction for *t*<sub>2</sub> - general

<span style="color:blue">Now that all 3 matrices are ready. Let’s count
actual cells for *t*<sub>1</sub>.</span>

``` r
t1_counts <- transitions %>% 
  count(t1_state, name = "t1_count") %>% 
  filter(t1_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t1_state) %>% 
  slice(match(order, from_state))

t1_counts <- as.data.frame(t1_counts)
rownames(t1_counts) <- t1_counts$from_state
t1_counts <- select(t1_counts,-c("from_state"))

summary_all <- t1_counts
summary_all
```

    ##        t1_count
    ## Junior      615
    ## Senior     3373
    ## Exit        416

``` r
rm(t1_counts)
```

<span style="color:blue">Make prediction for *t*<sub>2</sub>.</span>

``` r
mmult <- as.matrix(trans_matrix_all) %*% as.matrix(summary_all)

summary_all <- summary_all %>%
  mutate(
    t2_pred = unlist(lapply(seq_len(ncol(mmult)), function(i) mmult[,i]))
  ) 
summary_all
```

    ##        t1_count   t2_pred
    ## Junior      615  157.2555
    ## Senior     3373 3544.6934
    ## Exit        416  702.0511

<span style="color:blue">Count the actual cells for
*t*<sub>2</sub>.</span>

``` r
t2_counts <- transitions %>% 
  count(t2_state, name = "t2_count") %>% 
  filter(t2_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t2_state) %>% 
  slice(match(order, from_state))

t2_counts <- as.data.frame(t2_counts)
rownames(t2_counts) <- t2_counts$from_state
t2_counts <- select(t2_counts,-c("from_state"))
t2_counts
```

    ##        t2_count
    ## Junior      275
    ## Senior     4035
    ## Exit        355

``` r
summary_all <- cbind(summary_all, t2_counts)
summary_all
```

    ##        t1_count   t2_pred t2_count
    ## Junior      615  157.2555      275
    ## Senior     3373 3544.6934     4035
    ## Exit        416  702.0511      355

``` r
rm(t2_counts)
```

<span style="color:blue">Calculate the % difference.</span>

``` r
summary_all <- summary_all %>%
  mutate(
    percent_diff = (t2_pred - t2_count)/t2_count
  ) 
summary_all
```

    ##        t1_count   t2_pred t2_count percent_diff
    ## Junior      615  157.2555      275   -0.4281618
    ## Senior     3373 3544.6934     4035   -0.1215134
    ## Exit        416  702.0511      355    0.9776087

## Prediction for *t*<sub>2</sub> - women

<span style="color:blue">Zooming into women, let’s count actual cells
for *t*<sub>1</sub>.</span>

``` r
t1_counts <- transitions %>% 
  filter(woman==1) %>%
  count(t1_state, name = "t1_count") %>% 
  filter(t1_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t1_state) %>% 
  slice(match(order, from_state))

t1_counts <- as.data.frame(t1_counts)
rownames(t1_counts) <- t1_counts$from_state
t1_counts <- select(t1_counts,-c("from_state"))

summary_women <- t1_counts
summary_women
```

    ##        t1_count
    ## Junior      157
    ## Senior      986
    ## Exit        111

``` r
rm(t1_counts)
```

<span style="color:blue">Make prediction for *t*<sub>2</sub>.</span>

``` r
mmult <- as.matrix(trans_matrix_women) %*% as.matrix(summary_women)

summary_women <- summary_women %>%
  mutate(
    t2_pred = unlist(lapply(seq_len(ncol(mmult)), function(i) mmult[,i]))
  ) 
summary_women
```

    ##        t1_count   t2_pred
    ## Junior      157   38.8732
    ## Senior      986 1024.2722
    ## Exit        111  190.8546

<span style="color:blue">Count the actual cells for
*t*<sub>2</sub>.</span>

``` r
t2_counts <- transitions %>% 
  filter(woman==1) %>%
  count(t2_state, name = "t2_count") %>% 
  filter(t2_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t2_state) %>% 
  slice(match(order, from_state))

t2_counts <- as.data.frame(t2_counts)
rownames(t2_counts) <- t2_counts$from_state
t2_counts <- select(t2_counts,-c("from_state"))
t2_counts
```

    ##        t2_count
    ## Junior       69
    ## Senior     1150
    ## Exit         93

``` r
summary_women <- cbind(summary_women, t2_counts)
summary_women
```

    ##        t1_count   t2_pred t2_count
    ## Junior      157   38.8732       69
    ## Senior      986 1024.2722     1150
    ## Exit        111  190.8546       93

``` r
rm(t2_counts)
```

<span style="color:blue">Calculate the % difference.</span>

``` r
summary_women <- summary_women %>%
  mutate(
    percent_diff = (t2_pred - t2_count)/t2_count
  ) 
summary_women
```

    ##        t1_count   t2_pred t2_count percent_diff
    ## Junior      157   38.8732       69   -0.4366203
    ## Senior      986 1024.2722     1150   -0.1093285
    ## Exit        111  190.8546       93    1.0522000

## Prediction for *t*<sub>2</sub> - men

<span style="color:blue">Zooming into women, let’s count actual cells
for *t*<sub>1</sub>.</span>

``` r
t1_counts <- transitions %>% 
  filter(woman==0) %>%
  count(t1_state, name = "t1_count") %>% 
  filter(t1_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t1_state) %>% 
  slice(match(order, from_state))

t1_counts <- as.data.frame(t1_counts)
rownames(t1_counts) <- t1_counts$from_state
t1_counts <- select(t1_counts,-c("from_state"))

summary_men <- t1_counts
summary_men
```

    ##        t1_count
    ## Junior      384
    ## Senior     2015
    ## Exit        266

``` r
rm(t1_counts)
```

<span style="color:blue">Make prediction for *t*<sub>2</sub>.</span>

``` r
mmult <- as.matrix(trans_matrix_men) %*% as.matrix(summary_men)

summary_men <- summary_men %>%
  mutate(
    t2_pred = unlist(lapply(seq_len(ncol(mmult)), function(i) mmult[,i]))
  ) 
summary_men
```

    ##        t1_count   t2_pred
    ## Junior      384   98.5728
    ## Senior     2015 2121.3180
    ## Exit        266  445.1092

<span style="color:blue">Count the actual cells for
*t*<sub>2</sub>.</span>

``` r
t2_counts <- transitions %>% 
  filter(woman==0) %>%
  count(t2_state, name = "t2_count") %>% 
  filter(t2_state %in% c("Junior","Senior","Exit")) %>% 
  rename(from_state = t2_state) %>% 
  slice(match(order, from_state))

t2_counts <- as.data.frame(t2_counts)
rownames(t2_counts) <- t2_counts$from_state
t2_counts <- select(t2_counts,-c("from_state"))
t2_counts
```

    ##        t2_count
    ## Junior      163
    ## Senior     2425
    ## Exit        231

``` r
summary_men <- cbind(summary_men, t2_counts)
summary_men
```

    ##        t1_count   t2_pred t2_count
    ## Junior      384   98.5728      163
    ## Senior     2015 2121.3180     2425
    ## Exit        266  445.1092      231

``` r
rm(t2_counts)
```

<span style="color:blue">Calculate the % difference.</span>

``` r
summary_men <- summary_men %>%
  mutate(
    percent_diff = (t2_pred - t2_count)/t2_count
  ) 
summary_men
```

    ##        t1_count   t2_pred t2_count percent_diff
    ## Junior      384   98.5728      163   -0.3952589
    ## Senior     2015 2121.3180     2425   -0.1252297
    ## Exit        266  445.1092      231    0.9268797

## Regression to predict attrition

``` r
# Collect Data for gender from Applications Dataset
Test_df = applications[ , c('patex_id', 'gender')]
#View(Test_df)

# Find only distinct values
Test_df = (distinct(Test_df))

# 5,649 values. Equal to the transitions dataset
# Merge dataset to transitions dataset

# merge two data frames by ID
New_df <- merge(transitions,Test_df,by="patex_id")
#View(New_df)

# Count null values
skim(New_df)
```

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | New_df |
| Number of rows                                   | 5649   |
| Number of columns                                | 12     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 4      |
| Date                                             | 2      |
| numeric                                          | 6      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| t0_state      |      1250 |          0.78 |   6 |   6 |     0 |        2 |          0 |
| t1_state      |       599 |          0.89 |   4 |   8 |     0 |        4 |          0 |
| t2_state      |       460 |          0.92 |   4 |   8 |     0 |        4 |          0 |
| gender        |       663 |          0.88 |   4 |   6 |     0 |        2 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| onboard_date  |         0 |          1.00 | 2000-01-02 | 2016-03-03 | 2003-01-21 |     2325 |
| exit_date     |      4738 |          0.16 | 2000-09-14 | 2015-09-02 | 2011-11-20 |      645 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |       p0 |      p25 |      p50 |      p75 |     p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|:------|
| patex_id      |         1 |          1.00 | 78752.86 | 13575.30 | 59012.00 | 66531.75 | 75346.00 | 93750.75 | 99990.00 | ▇▆▃▂▇ |
| tenure        |        24 |          1.00 |    12.13 |     4.92 |     0.07 |     8.48 |    13.45 |    16.68 |    17.85 | ▂▂▂▅▇ |
| tenure_t0     |         0 |          1.00 |     2.89 |     4.23 |    -9.17 |     0.59 |     3.95 |     6.82 |     7.00 | ▁▂▂▃▇ |
| tenure_t1     |         0 |          1.00 |     6.89 |     4.23 |    -5.17 |     4.59 |     7.95 |    10.82 |    11.00 | ▁▂▂▃▇ |
| tenure_t2     |         0 |          1.00 |    10.89 |     4.23 |    -1.17 |     8.59 |    11.95 |    14.82 |    15.00 | ▁▂▂▃▇ |
| woman         |       663 |          0.88 |     0.31 |     0.46 |     0.00 |     0.00 |     0.00 |     1.00 |     1.00 | ▇▁▁▁▃ |

<span style="color:blue">so t0 is missing 1250 values, gender is missing
663, but exit data is missing the most with 4738 values.</span>

<span style="color:blue">Replace missing vales with random
gender.</span>

``` r
attach(New_df)
table(gender)
```

    ## gender
    ## female   male 
    ##   1548   3438

``` r
# 3438 men and 1548 women. So there are 0.69 propration of woman and 0.31 of men.
```

``` r
# Replace all missing values at propotional rate
y = sum(is.na(gender))
New_df$gender[is.na(New_df$gender)] <- sample(c('male','female'),  prob=c(0.69, 0.31),y, replace = TRUE)

# Drop null values in t2
#New_df <- New_df[is.na(New_df$t2_state),] 
New_df = New_df[!(is.na(New_df$t2_state)| New_df$t2_state==""),]

# Drop New hires in t2
New_df<-New_df[New_df$t2_state!="New hire",]
```

<span style="color:blue">Creating an quadratic discriminat
regression</span> <span style="color:blue">We only have three values in
t2_state. Junior, Senior and Exit.</span>

``` r
library('fastDummies')
New_df <- dummy_cols(New_df, select_columns = 'gender')
attach(New_df)
```

    ## The following objects are masked from New_df (pos = 4):
    ## 
    ##     exit_date, gender, onboard_date, patex_id, t0_state, t1_state,
    ##     t2_state, tenure, tenure_t0, tenure_t1, tenure_t2, woman

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
#myqda = qda(t2_state~gender_male+tenure_t2+tenure_t1+tenure_t0)
```

<span style="color:blue">Creating an quadratic discriminat
regression.</span>

``` r
# Creating a numerical variable for attrition 
New_df$t2_state[New_df$t2_state=="Exit"]=1
New_df$t2_state[New_df$t2_state=="Junior"]=0
New_df$t2_state[New_df$t2_state=="Senior"]=0


New_df <- transform(New_df,t2_state = as.factor(as.numeric(New_df$t2_state)))
attach(New_df)
```

    ## The following objects are masked from New_df (pos = 4):
    ## 
    ##     exit_date, gender, gender_female, gender_male, onboard_date,
    ##     patex_id, t0_state, t1_state, t2_state, tenure, tenure_t0,
    ##     tenure_t1, tenure_t2, woman

    ## The following objects are masked from New_df (pos = 6):
    ## 
    ##     exit_date, gender, onboard_date, patex_id, t0_state, t1_state,
    ##     t2_state, tenure, tenure_t0, tenure_t1, tenure_t2, woman

``` r
table(New_df$t2_state)
```

    ## 
    ##    0    1 
    ## 4310  355

``` r
# First Dirty Model
search()
```

    ##  [1] ".GlobalEnv"          "New_df"              "package:MASS"       
    ##  [4] "New_df"              "package:fastDummies" "New_df"             
    ##  [7] "package:gender"      "package:arrow"       "package:lubridate"  
    ## [10] "package:skimr"       "package:forcats"     "package:stringr"    
    ## [13] "package:dplyr"       "package:purrr"       "package:readr"      
    ## [16] "package:tidyr"       "package:tibble"      "package:ggplot2"    
    ## [19] "package:tidyverse"   "package:stats"       "package:graphics"   
    ## [22] "package:grDevices"   "package:utils"       "package:datasets"   
    ## [25] "package:methods"     "Autoloads"           "package:base"

``` r
logit=glm(t2_state ~ gender_male+tenure_t2, family = "binomial",data = New_df) 
summary(logit)
```

    ## 
    ## Call:
    ## glm(formula = t2_state ~ gender_male + tenure_t2, family = "binomial", 
    ##     data = New_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6592  -0.4268  -0.3381  -0.3082   2.5220  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.94328    0.19457  -4.848 1.25e-06 ***
    ## gender_male  0.11362    0.12307   0.923    0.356    
    ## tenure_t2   -0.14637    0.01546  -9.470  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2511.0  on 4664  degrees of freedom
    ## Residual deviance: 2422.7  on 4662  degrees of freedom
    ## AIC: 2428.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
library(ggplot2)
plot=ggplot(New_df, aes(x=tenure,y=t2_state))+geom_point()
line=geom_smooth(method="glm", formula=t2_state~gender_male+tenure_t2, method.args=list(family=binomial))
plot+line
```

    ## Warning: Removed 23 rows containing non-finite values (stat_smooth).

    ## Warning: Computation failed in `stat_smooth()`:
    ## variable lengths differ (found for '(weights)')

    ## Warning: Removed 23 rows containing missing values (geom_point).

![](Exercise5_group5_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
# Predictions for male and female attrition
b0=coef(logit)[1]
b1=coef(logit)[2]
b2=coef(logit)[3]
b0
```

    ## (Intercept) 
    ##  -0.9432836

``` r
b1
```

    ## gender_male 
    ##   0.1136206

``` r
b2
```

    ##  tenure_t2 
    ## -0.1463734

``` r
sex = 0 # female
ten = 1 # One year
m=exp(b0+b1*sex+b2*ten)
prob = m/(1+m)
prob
```

    ## (Intercept) 
    ##   0.2516829

<span style="color:blue">24.7% Chance this junior woman quits.</span>

``` r
sex = 1 # male
ten = 1 # One year
m=exp(b0+b1*sex+b2*ten)
prob = m/(1+m)
prob
```

    ## (Intercept) 
    ##   0.2736789

<span style="color:blue">27.5% Chance this junior man quits.</span>

``` r
sex = 0 # Female
ten = 10 # Ten years
m=exp(b0+b1*sex+b2*ten)
prob = m/(1+m)
prob
```

    ## (Intercept) 
    ##  0.08263911

<span style="color:blue">8.1% Chance this senior woman quits.</span>

``` r
sex = 1 # Male
ten = 10 # Ten years
m=exp(b0+b1*sex+b2*ten)
prob = m/(1+m)
prob
```

    ## (Intercept) 
    ##  0.09167127

<span style="color:blue">9.2% chance this senior man will quit.</span>

<span style="color:blue">Appendix: more exploratory work</span>

``` r
#library('fastDummies')
#New_df <- dummy_cols(New_df, select_columns ='t1_state')
#attach(New_df)
```

``` r
# Second Dirty Model
#search()
#logit2=glm(t2_state ~ gender_male+tenure_t2+t1_state_Junior+t1_state_Senior, family = "binomial",data = New_df) 
#summary(logit2)
```

``` r
# Predictions for male and female attrition
#b0=coef(logit2)[1]
#b1=coef(logit2)[2]
#b2=coef(logit2)[3]
#b3=coef(logit2)[4]
#b4=coef(logit2)[5]
```

``` r
#sex = 1 # Male
#ten = 10 # Ten years
# Senior at t1
#m=exp(b0+b1*sex+b2*ten+b4*1)
#prob = m/(1+m)
#prob
```

``` r
#sex = 1 # Male
#ten = 1 # One year
# Junior at t1
#m=exp(b0+b1*sex+b2*ten+b3*1)
#prob = m/(1+m)
#prob
```

``` r
#sex = 0 # Female
#ten = 10 # Ten years
# Senior at t1
#m=exp(b0+b1*sex+b2*ten+b4*1)
#prob = m/(1+m)
#prob
```

``` r
#sex = 0 # Female
#ten = 1 # One year
# Junior at t1
#m=exp(b0+b1*sex+b2*ten+b3*1)
#prob = m/(1+m)
#prob
```
