Lab 6
================

## PSY V0500 Statistical Methods in Psychology

## Kevin R Foster, Colin Powell School, the City College of New York, CUNY

For this lab, we fancy up the regression models to explain cannabis use.

We’ll start from where Lab 5 left off, then add a quadratic in age:

``` r
model_2 <- lm(MARIJAN1 ~ Age_midpt + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
summary(model_2)

model_3 <- lm(MARIJAN1 ~ Age_midpt + I(Age_midpt^2) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
summary(model_3)
```

You should examine the coefficient estimates to ascertain if those look
reasonable. Another consideration are the predicted values of the
models, which you can have R calculate, for each age between 25 and 55,
as something like this

``` r
to_be_predicted_2 <- data.frame(Age_midpt = 25:55, X_PRACE2 = "Black or African American",
                          X_HISPANC = "no", EDUCA = "Grade 12 or GED (High school graduate)")

to_be_predicted_2$yhat <- predict(model_2, newdata = to_be_predicted_2)
```

You should look at other predicted values for other factor values.

Next step is to add some interactions, such as:

``` r
model_4 <- lm(MARIJAN1 ~ Age_midpt*(X_PRACE2 + X_HISPANC) + I(Age_midpt^2)*(X_PRACE2 + X_HISPANC) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
summary(model_4)

anova(model_2,model_3,model_4)
```

The ANOVA command allows easier comparison between the full models. In
this case, adding just a squared term in Age does not make a significant
difference (that’s the comparison on line 2), but adding a full set of
interaction terms (comparison on line 3) does make a difference in the
model’s sum of squared residuals.

You should consider further extensions, including other interaction
terms. Look at some of the predicted values.

``` r
to_be_predicted_2$yhat3 <- predict(model_3, newdata = to_be_predicted_2)
to_be_predicted_2$yhat4 <- predict(model_4, newdata = to_be_predicted_2)


d_for_graphing <- data.frame(Age_midpt = 25:55, 
                             to_be_predicted_2$yhat,
                             to_be_predicted_2$yhat3,
                             to_be_predicted_2$yhat4)

p_predvals <- ggplot(d_for_graphing, aes(Age_midpt))
p_predvals + geom_line(aes(y = to_be_predicted_2.yhat)) + 
  geom_line(aes(y = to_be_predicted_2.yhat3), color = 'blue') +
  geom_line(aes(y = to_be_predicted_2.yhat4), color = 'red')
```

Which, ok, no big diff in this case.

At some point in your explorations, you should have noticed that the
dependent variable, `MARIJAN1`, does not include many casual users –
most report zero but the next most common response is 30 (days out of
last 30 when used cannabis). Here are the top percentiles, then a
listing of how many people responded to each number of days:

``` r
quantile(brfss_marijan$MARIJAN1, probs = c(0.99,0.95,0.94,0.93,0.92,0.91,0.9))

# or convert to a factor to see the counts
brfss_marijan$MARIJAN_factor <- as.factor(brfss_marijan$MARIJAN1)
summary(brfss_marijan$MARIJAN_factor)
```

This should raise a lot of questions. In the whole data, just 2 people
answered that they’d used on 11 out of past 30 days! I’d like to talk
with them a bit more – why such a particular number? Perhaps they’re
counting from when they quit? There were over 4300 who answered 30 days,
then almost 1300 who answered just one day, then another almost 1100 who
responded 2. Then bunches at 20, 15 and 10 – which seems to imply that
people are choosing round numbers and not thinking in weeks as much.

One approach to this would be to create a new variable, coding if
`MARIJAN1` was any number greater than zero. Our previous models
implicitly assumed that there was the same influence for a person to
choose to respond 1 instead of zero, as there was to respond 2 instead
of 1. That’s probably not the case! The linear model is estimating the
expected value of the dependent variable. For people who are 25 years
old, a larger fraction choose 30 days, compared with people who are 65
years old. The linear model picks up this decline in the expected value.
But are there other models that would not have this issue?

Yup, there are. We’ll work with logit and probit models next week. There
are also quantile models, which can estimate different percentiles. But
note that the workhorse `lm()` can deal with that restricted y-variable
too.

``` r
brfss_marijan$Marijan_01 <- as.numeric(brfss_marijan$MARIJAN1 > 0)
# do a summary to check that it looks right

model_5 <- lm(Marijan_01 ~ Age_midpt*(X_PRACE2 + X_HISPANC) + I(Age_midpt^2)*(X_PRACE2 + X_HISPANC) + X_PRACE2 + X_HISPANC + EDUCA, data = brfss_marijan)
summary(model_5)
```

How are those estimates different?

Note that `anova(model_4,model_5)` would break since the dependent
variables are different now.

Now go crazy with some models. Try a lot more interaction terms! What
else do you think is important? Go through Chapter 6 of the Healy text
for more suggestions.
