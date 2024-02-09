Lab 3
================

### PSY V0500 Statistical Methods in Psychology

### Kevin R Foster, Colin Powell School, the City College of New York, CUNY

For this lab we will start by just going through Chapter 3 of the
textbook but with BRFSS data. Then we’ll extend with some stats.

I will show you one subset but you can work on your own and explore.

We start with some of the same commands as last time.

``` r
library(ggplot2)
library(tidyverse)
```

``` r
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
```

We’ll form a subset. Last time we just created a random group but let’s
be more intentional. Suppose we looked at people in the tristate area.
Create a logical variable that returns true if the person lives in any
of the three states (true if person lives in NY, or true if person lives
in NJ, or true if person lives in CT).

``` r
select_tristate <- (brfss22$X_STATE == "New York") | (brfss22$X_STATE == "New Jersey") | (brfss22$X_STATE == "Connecticut")
brfss_tristate <- subset(brfss22,select_tristate)
```

Since Healy uses two continuous variables we’ll use Age and BMI here, to
get some similar plots.

Before you run this, what would you expect to see, in a plot with Age on
the x-axis and BMI on the y-axis? Do you think it would be just a cloud
without any structure? Would it be an increase as people get older? A
decrease? I want you to learn about the world in addition to learning
about R. It’s useful to elicit your own opinions before looking at the
data, to highlight where are the surprises. That’s how we learn.

``` r
p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5))
p_tri + geom_smooth()
```

Healy adds the points as well; you can try adding `geom_points()` but I
think the bunching of the x-axis values doesn’t look very good. Try this
instead:

``` r
p_tri + geom_jitter(width = 2.5, height = NULL, alpha = 0.05)
```

(If you’re feeling grumpy about that vertical gap on the left, you can
go back to where we defined Age_midpt and replace “21” with “22”
instead. But then re-run the later parts too.)

The `jitter` adds some random noise to each value. The `width` and
`height` commands tell it to add up to 2.5 on each side of the x-value
but not to touch the y-value.

Then the `alpha` makes the points almost transparent, so you can better
see how they stack up in some places.

Where Healy then adds `color = continent` you can add `color = X_STATE`.
Again, before you graph it, which state of the three do you expect to
have the people with highest BMI?

``` r
p_tri <- ggplot(data = brfss_tristate,
                mapping = aes(x = Age_midpt,
                              y = X_BMI5,
                              color = X_STATE,
                              fill = X_STATE))
p_tri + geom_smooth()
```

Did you guess right?

Please work through the other examples in Chapter 3 (for homework).
Discuss with your group about what subgroup might be interesting,
instead of just people in tristate area.

\###statistics Now we’ll calculate some statistics about the data as
well. I will show you a few but you should continue exploring a topic
that interests you.

Let’s look at the Adverse Childhood Experiences ACE subgroup. This was
the most common response from students in HW1, they wanted to dig into
these questions. For more detail, read appendix below. To start, just
run this code,

``` r
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
brfss_ACE <- subset(brfss22, select_ACE)
```

Looking at the subset of people asked about Adverse Childhood
Experiences, let’s consider how some are inter-related.

``` r
xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS)
```

    ##                                                                                          brfss_ACE$ACEDRUGS
    ## brfss_ACE$ACEDRINK                                                                        Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications
    ##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                                                                                                                  3164
    ##   No                                                                                                                                                                                                       1483
    ##   dont know not sure                                                                                                                                                                                         13
    ##   refused                                                                                                                                                                                                     4
    ##                                                                                          brfss_ACE$ACEDRUGS
    ## brfss_ACE$ACEDRINK                                                                           No
    ##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic  8157
    ##   No                                                                                      33919
    ##   dont know not sure                                                                        107
    ##   refused                                                                                    64
    ##                                                                                          brfss_ACE$ACEDRUGS
    ## brfss_ACE$ACEDRINK                                                                        dont know not sure
    ##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                140
    ##   No                                                                                                      83
    ##   dont know not sure                                                                                      77
    ##   refused                                                                                                  5
    ##                                                                                          brfss_ACE$ACEDRUGS
    ## brfss_ACE$ACEDRINK                                                                        refused
    ##   Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic      11
    ##   No                                                                                           31
    ##   dont know not sure                                                                            4
    ##   refused                                                                                     710

Although the flattened table version is a bit easier to read,

``` r
ftable(xtabs(~ brfss_ACE$ACEDRINK + brfss_ACE$ACEDRUGS))
```

    ##                                                                                         brfss_ACE$ACEDRUGS Yes, Adverse Childhood Exper, lived with someone who used illegal street drugs or who abused prescription medications    No dont know not sure refused
    ## brfss_ACE$ACEDRINK                                                                                                                                                                                                                                               
    ## Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic                                                                                                                                     3164  8157                140      11
    ## No                                                                                                                                                                                                                          1483 33919                 83      31
    ## dont know not sure                                                                                                                                                                                                            13   107                 77       4
    ## refused                                                                                                                                                                                                                        4    64                  5     710

Neither version would go into a lab report or research paper, without
some fixing up.

Now try some calculations – I’d encourage you to do it the old-fashioned
way to start. Of course you can do it with R but sometimes it’s good to
slow down and think through each step. What fraction of the sample lived
with a problem drinker? What fraction lived with someone with a drug
problem? What fraction of people who lived with a problem drinker also
lived with someone with a drug problem? And vice versa? Form a
hypothesis test: is there a difference between the fraction of people
who lived with a problem drinker, and the same fraction, conditional on
also living with someone with drug problem?

One reason to go slow and careful is to consider what to do with those
who either refuse an answer or who give something like “don’t know” or
“not sure”. You might try that same hypothesis test with multiple
decisions – if you just drop the people who didn’t give “yes/no”
answers; if you count those all as “yes”; if you count those all as
“no”.

Discuss with your lab group: why do you imagine that people might
respond, “don’t know” or “not sure”?

Once you decide how to recode, you can fill in some of the blanks, for
instance,

``` r
brfss_ACE$ACEDRINK_recode <- fct_recode(brfss_ACE$ACEDRINK, 
                                        " " = "Yes, Adverse Childhood Exper, lived with someone who was a problem drinker or alcoholic",
                                " " = "No", 
                                " " = "dont know not sure",
                                " " = "refused"
)

# might want to set some values as missing,
#                                 NULL = "dont know not sure"
```

Maybe the first blank is “yes” but next three are “no”; or first blank
is “yes” then “no” then 2 more “yes”; or yes/no/NULL/NULL. Try each and
look how the hypothesis test varies.

As we talked about earlier, you have to be careful about where your data
fork (*else you’ll fork it up – sorry! dad joke*). I wrote that code to
change the variable in the `brfss_ACE` dataset. You might want to add it
all the way back at the top, into the `brfss22` data, or into the
`brfss_tristate` data – but it’s not there now. And if you add it into
the `brfss22` data then it won’t propagate into the later datasets
unless you explicitly tell it to do so.

This is hard! Actually all good work is hard, especially as you drill
down into the fine detail.

\###Appendix Just a note on the Adverse Childhood Experience ACE
subgroup. I created it by specifying a variable if the person was not
asked each of the questions, then

``` r
ACEdidntask <- (as.numeric(is.na(brfss22$ACEDEPRS)) + 
                        as.numeric(is.na(brfss22$ACEDRINK)) +
                        as.numeric(is.na(brfss22$ACEDRUGS)) +
                        as.numeric(is.na(brfss22$ACEPRISN)) +
                        as.numeric(is.na(brfss22$ACEDIVRC)) +
                        as.numeric(is.na(brfss22$ACEPUNCH)) +
                        as.numeric(is.na(brfss22$ACEHURT1)) +
                        as.numeric(is.na(brfss22$ACESWEAR)) +
                        as.numeric(is.na(brfss22$ACETOUCH)) )
select_ACE <- (ACEdidntask == 0) # with zero missing values for any of ACE questions
```

The `ACEdidntask` starts with a logical value, is an answer missing and
coded as NA, converts that to a number (zero for false; 1 for true) and
adds up the results.

But we might worry about how the questionnaire was structured – if lots
of people got asked just one of these questions then there might be
nobody with all non-missing.

How can we tell from the data? We can use some statistics to understand
the statistics (for MCU fans, think Thanos, “I used the stones to
destroy the stones”).

``` r
summary(ACEdidntask)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   9.000   9.000   8.027   9.000   9.000

``` r
quantile(ACEdidntask, probs = c(0.01,0.05,0.1,0.15,0.2))
```

    ##  1%  5% 10% 15% 20% 
    ##   0   0   0   9   9

That shows clearly that most people were not asked any of the ACE
questions while a few people got asked all of them. Or, perhaps I ought
to rewrite that previous sentence, “that shows clearly to me” – does it
show clearly to you? Think about the results. How would it change if you
recoded to `!is.na()`?

Please also check if this `ACE_data` is similar to the whole `brfss22`
sample – note that the distribution of states is different. What about
age, education, and some other demographics?

I don’t want you to think that this dataset is unusually bad – it is
very well designed and executed. Rather I want you to realize that all
datasets are messy, often in much worse ways! Every survey has issues.

And as you’re doing the homework, think about how to best communicate
about the differences. You could just show a mass of output,

``` r
summary(brfss22)
summary(brfss_ACE)
```

But does that help the reader? You’ve got to do the analysis and
explain.
