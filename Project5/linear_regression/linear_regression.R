##  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states_data <- readRDS("dataSets/states.rds") 
#get labels
states_info <- data.frame(attributes(states_data)[c("names", "var.labels")])
#look at last few labels
tail(states_info, 20)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts_ex_sat <- subset(states_data, select = c("expense", "csat"))
summary(sts_ex_sat)
# correlation between expense and csat
cor(sts_ex_sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts_ex_sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat_mod <- lm(csat ~ expense, # regression formula
              data = states_data) # data set
# Summarize and print the results
summary(sat_mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states_data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat_mod)
names(sat_mod)
methods(class = class(sat_mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat_mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat_mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat_voting_mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states_data))
sat_mod <- update(sat_mod, data = na.omit(states_data))
# compare using the anova() function
anova(sat_mod, sat_voting_mod)
coef(summary(sat_voting_mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
energy_metro <- subset(states_data, select = c("metro", "energy"))
summary(energy_metro)
plot(energy_metro)
cor(energy_metro, use = "complete.obs")
# Negative correlation value of -0.34

##   2. Print and interpret the model `summary'
energy_mod <- lm(energy ~ metro, data = na.omit(states_data))
summary(energy_mod)
# p-value 0.03, significant, R^2 is only 0.097

##   3. `plot' the model to look for deviations from modeling assumptions
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(energy_mod, which = c(1, 2))
# Normal Q-Q suggest the residuals exhibit heavy - tails

##  Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

##Creating model adding several values to examine results
energy_mod_2 <- lm(energy ~ metro + density + area + toxic + green + waste,
                   data = na.omit(states_data))
summary(energy_mod_2)
# p-value for metro increased from 0.03 to 0.93.
# new model has a positive slope.
# metro, waste, and density have the hightest p-values and will be dropped
# for the next model.

energy_mod_3 <- lm(energy ~ area + toxic + green, data = na.omit(states_data))
summary(energy_mod_3)
# p-value dropped even further for the model and each variable.
# Adj. R^2 increased from 0.7507 to 0.7652
# area has lowest p-value, will be dropped in the next model.

energy_mod_4 <- lm(energy ~ toxic + green, data = na.omit(states_data))
summary(energy_mod_4)
# p-value dropped even lower for the model and each of the remaining variables.

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(energy_mod_4, which = c(1, 2))
# Q-Q plot exhibits some right skew


anova(energy_mod_2, energy_mod_3, energy_mod_4)
anova(energy_mod_4, energy_mod_3, energy_mod_2)

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat_expense_by_percent <- lm(csat ~ expense * income,
                             data = states_data) 
#Show the results
  coef(summary(sat_expense_by_percent)) # show regression coefficients table
  
summary(sat_expense_by_percent)  

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states_data$region)
states_data$region <- factor(states_data$region)
#Add region to the model
sat_region <- lm(csat ~ region,
                 data = states_data) 
#Show the results
coef(summary(sat_region)) # show regression coefficients table
anova(sat_region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states_data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data = states_data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data = states_data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
# Toxic and green do not act independently on energy, there is some interaction.
energy_toxic_by_green <- lm(
    energy ~ toxic + green + toxic:green,
    data = states_data)
coef(summary(energy_toxic_by_green))
summary(energy_toxic_by_green)

# Toxic and green do not act independently on energy, there is some interaction.
# p-value of interaction is .0193

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

energy_region <- lm(formula = energy ~ toxic + green + toxic:green + region,
   data = na.omit(states_data))  

summary(energy_region)
anova(energy_region)
coef(summary(lm(energy ~ C(region, base = 4), data = states_data)))


  

  

