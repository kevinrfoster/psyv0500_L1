Lab 4
================

### PSY V0500 Statistical Methods in Psychology

### Kevin R Foster, Colin Powell School, the City College of New York, CUNY

For this lab we will explore a simple machine-learning procedure, k-nn,
for k-th nearest neighbor estimation. The lecture videos explain more
details.

K-nn is a fancy name for a really simple procedure:

- take an unclassified observation
- look for classified observations near it
- guess that it is like its neighbors

We can understand the k-nn method without any statistics more
complicated than means (of subgroups) and standard deviations. We’ll
start by reviewing how to calculate those. Chapter 4 of Healy’s book
also goes through about how to graph categorical variables so you might
find that helpful too.

We want to understand how many days of the last month a person said
their mental health was not good (from zero to thirty), \`MENTHLTH’, and
see how much Adverse Childhood Experiences impact that.

We start with some of the same commands as last time.

``` r
library(ggplot2)
library(tidyverse)
require(class)
require(caret)
load("BRFSS2022_rev.RData")
brfss22$Age_midpt <- fct_recode(brfss22$X_AGEG5YR, "21" = "Age 18 to 24",
                                "27" = "Age 25 to 29", "32" = "Age 30 to 34",
                                "37" = "Age 35 to 39", "42" = "Age 40 to 44",
                                "47" = "Age 45 to 49", "52" = "Age 50 to 54",
                                "57" = "Age 55 to 59", "62" = "Age 60 to 64",
                                "67" = "Age 65 to 69", "72" = "Age 70 to 74",
                                "77" = "Age 75 to 79", "82" = "Age 80 or older",
                                NULL = "Dont know/Refused/Missing")
brfss22$Age_midpt <- as.numeric(levels(brfss22$Age_midpt))[brfss22$Age_midpt]
brfss22$Educ_number <- fct_recode(brfss22$EDUCA, 
                                  "0" = "Never attended school or only kindergarten", 
                                  "4.5" = "Grades 1 through 8 (Elementary)",
                                  "10" = "Grades 9 through 11 (Some high school)",
                                  "12" = "Grade 12 or GED (High school graduate)",
                    "14" = "College 1 year to 3 years (Some college or technical school)",
                    "16" = "College 4 years or more (College graduate)",
                    NULL = "Refused" )
brfss22$Educ_number <- as.numeric(levels(brfss22$Educ_number))[brfss22$Educ_number]

ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) & !is.na(brfss22$MENTHLTH) # with zero missing values for any of ACE questions and not missing MENTLHLTH
brfss_ACE <- subset(brfss22, select_ACE)
```

We calculate some simple statistics (for homework, construct hypothesis
tests about differences)

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   3.642   3.000  30.000

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "once"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   2.000   6.705  10.000  30.000

``` r
summary(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "more than once"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   3.000   9.094  15.000  30.000

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"], na.rm = TRUE)
```

    ## [1] 7.712734

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "once"], na.rm = TRUE)
```

    ## [1] 9.799491

``` r
sd(brfss_ACE$MENTHLTH[brfss_ACE$ACETOUCH == "more than once"], na.rm = TRUE)
```

    ## [1] 11.11077

``` r
summary(brfss_ACE$ACETOUCH) # N in each group
```

    ## Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually 
    ##                                                                                                                      40223 
    ##                                                                                                                       once 
    ##                                                                                                                       1901 
    ##                                                                                                             more than once 
    ##                                                                                                                       3614 
    ##                                                                                                         dont know not sure 
    ##                                                                                                                        193 
    ##                                                                                                                    refused 
    ##                                                                                                                       1151

``` r
# is there an easier way?!?
library(plyr)
summary1 <- ddply(brfss_ACE,.(ACETOUCH), summarize, mean_mentalhealth = mean(MENTHLTH), 
                  sd_mentalhealth = sd(MENTHLTH), n_obs = sum(!is.na(MENTHLTH)) )
summary1
```

    ##                                                                                                                     ACETOUCH
    ## 1 Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually
    ## 2                                                                                                                       once
    ## 3                                                                                                             more than once
    ## 4                                                                                                         dont know not sure
    ## 5                                                                                                                    refused
    ##   mean_mentalhealth sd_mentalhealth n_obs
    ## 1          3.642369        7.712734 40223
    ## 2          6.705418        9.799491  1901
    ## 3          9.094079       11.110770  3614
    ## 4          6.896373       10.287008   193
    ## 5          5.990443       10.162544  1151

There seem to be large differences although the hypothesis tests can
help understand if there’s more than just random error.

As a clinician, you might actually be interested in the opposite
prediction: knowing that somebody has bad mental health days (and wants
treatment), what is likelihood that they had adverse childhood
experiences?

``` r
summary2 <- ddply(brfss_ACE,.(MENTHLTH >0), summarize, 
                  zero_ACETOUCH = sum(ACETOUCH == "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually"), 
                  once_ACETOUCH = sum(ACETOUCH == "once"), 
                  mult_ACETOUCH = sum(ACETOUCH == "more than once") )
summary2
```

    ##   MENTHLTH > 0 zero_ACETOUCH once_ACETOUCH mult_ACETOUCH
    ## 1        FALSE         26334           859          1370
    ## 2         TRUE         13889          1042          2244

For homework (not now) please form hypothesis tests for questions such
as, is there a relationship between mental health days and being abused
as a child? Is there a statistically significant difference in mental
health days between those who reported “once” and those who reported
“dont know not sure”? You should also create some graphs to show these
as well.

In the lecture video, I explained about the two common usages of the
term, “standarize” – we’ll use the second one here. We will recode the
variables to the \[0,1\] interval.

``` r
brfss_ACE$ACETOUCH_recode <- fct_recode(brfss_ACE$ACETOUCH, 
                                        "0" = "Adverse Childhood Exper, never:  How often did anyone at least 5 years older than you or an adult, ever touch you sexually",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACEHURT_recode <- fct_recode(brfss_ACE$ACEHURT1, 
                                        "0" = "Adverse Childhood Exper, never: Not including spanking, (before age 18), how often did a parent or adult in your home ever hit, beat, kick, or physically hurt you in any way",
                                "0.5" = "once", 
                                "1" = "more than once",
                                NULL = "dont know not sure",
                                NULL = "refused"
)
brfss_ACE$ACETOUCH_recode <- as.numeric(levels(brfss_ACE$ACETOUCH_recode))[brfss_ACE$ACETOUCH_recode]
brfss_ACE$ACEHURT_recode <- as.numeric(levels(brfss_ACE$ACEHURT_recode))[brfss_ACE$ACEHURT_recode]

brfss_ACE$MENTHLTH_recode <- cut(brfss_ACE$MENTHLTH, breaks = c(-1,0,1,5,10,15,31))
summary(brfss_ACE$MENTHLTH_recode)
```

    ##  (-1,0]   (0,1]   (1,5]  (5,10] (10,15] (15,31] 
    ##   29340    1537    6932    2735    2010    4528

``` r
# create a function to standardize
standardize_varb_to01 <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}
```

Note that in this recoding we are implicitly assuming that having this
adverse experience happen once was exactly half as bad as having it
happen more than once. Which is obviously wrong! But we’ll see if it’s
useful. The way the question was asked, it’s implicitly assuming that
occurring twice is same as ten times or twenty or more.

Then, although MENTHLTH is a continuous variable up to 30, we lump
together some of the responses – remember from math classes that the set
`(5,10]` indicates that we’re looking at values from 5 (where
parentheses does not include 5) up to 10 \[where square brackets include
10\]. You can explore what happens if you change some of these
boundaries – maybe some people are thinking of weeks so 7 and 14 would
be more salient?

We could do more complicated recoding but for now we will use this.

There’s an old line that all statistics is prediction. If we ask whether
some X1 is related to X2, that’s asking if knowledge of X1 could help
predict X2. That connection is all the more salient these days since
machine learning creates lots of prediction methods that are now
available for us to use.

In prediction exercises we generally have some X variables that predict
a Y variable. Later we can worry about true causality – I am not
asserting that we have causation here.

``` r
X1 <- standardize_varb_to01(brfss_ACE$Age_midpt)
X2 <- standardize_varb_to01(brfss_ACE$Educ_number)
X3 <- brfss_ACE$ACETOUCH_recode
X4 <- brfss_ACE$ACEHURT_recode
# you could add more X variables...
Y <- brfss_ACE$MENTHLTH_recode

nonmissingobs <- complete.cases(Y,X1,X2,X3,X4)

X1 <- subset(X1, nonmissingobs)
X2 <- subset(X2, nonmissingobs)
X3 <- subset(X3, nonmissingobs)
X4 <- subset(X4, nonmissingobs)
dat_use <- data.frame(X1,X2,X3,X4)
Y <- subset(Y, nonmissingobs)
```

In most machine learning, we split the data into training and test sets.
We use the training data to train a model to do the categorization, then
see how it does on the test set – which hadn’t been used for training,
so is new to the algorithm.

If we wanted to train some algo to pick out cat pictures, we would show
it a lot of pictures and tell it “this is cat” or “this is not cat” (the
training set). But we would hold back some pictures that it hadn’t seen
yet (test set), to ask it “cat or no?” in order to figure out how well
it could do the … categorization. *sorry sorry sorry! I’ll show myself
out*

We randomly chose 60% of the observations to be the training set and 40%
to be the test set.

``` r
set.seed(1234)
NN_obs <- length(Y)
select1 <- (runif(NN_obs) < 0.6)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- Y[select1]
true_data <- Y[!select1]
```

Then run the classifier for different numbers of neighbors – pick odd
numbers of neighbors so we don’t worry about ties. This outputs the
number of neighbors and the fraction of the test set that are correctly
predicted.

``` r
for (indx in seq(1, 9, by= 2)) {
 pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
 num_correct_labels <- sum(pred_y == true_data)
 correct_rate <- num_correct_labels/length(true_data)
 print(c(indx,correct_rate))
}
```

    ## [1] 1.0000000 0.6165127
    ## [1] 3.0000000 0.6179121
    ## [1] 5.0000000 0.6183039
    ## [1] 7.000000 0.618248
    ## [1] 9.0000000 0.6193675

Discuss what you can infer from this classifier. Can you improve it with
different (or just more) X variables?
