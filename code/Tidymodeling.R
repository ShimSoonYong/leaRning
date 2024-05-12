### Tidyverse principles
## Design for humans
library(tidyverse)

mtcars[order(mtcars$gear,mtcars$mpg),] # Core R
arrange(.data = mtcars, gear, mpg)

## Reuse existing data structures
boot_samp<-rsample::bootstraps(mtcars,times=3)
boot_samp
class(boot_samp)

## Design for the pipe and functional programming
small_mtcars<-arrange(mtcars,gear)
small_mtcars<-slice(small_mtcars,1:10)
small_mtcars<-slice(arrange(mtcars,gear),1:10)

# The pipe operator substitutes the value of the left-hand side of
# the operator as the first argument to the right-hand side
small_mtcars<-
  mtcars %>%
  arrange(gear) %>%
  slice(1:10)

ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+geom_smooth(method=lm)

# Using loop function
n<-nrow(mtcars)
ratios<-rep(NA_real_,n)
for (car in 1:n){
  ratios[car]<-log(mtcars$mpg[car]/mtcars$wt[car])
}
head(ratios)

# R's vector operation
ratios<-log(mtcars$mpg/mtcars$wt)
head(ratios)

# Functional programming
compute_log_ratio<-function(mpg,wt){
  log_base<-getOption("log_base",default=exp(1))
  results<-log(mpg/wt,base=log_base)
  print(mean(results))
  done<<-T
  results
} # Low level code
compute_log_ration<-function(mpg,wt,log_base=exp(1)){
  log(mpg/wt,base=log_base)
} # High level code

# purrr package
map(head(mtcars$mpg,3),sqrt) # map(vector, function)
log_ratios<-map2_dbl(mtcars$mpg,mtcars$wt,compute_log_ration)
head(log_rations)

map2_dbl(mtcars$mpg,mtcars$wt,~log(.x/.y)) %>%
  head()


## Examples of tidyverse syntax
# Automatically checks valid names
data.frame('variable 1'=1:2,two=3:4)
df<-data.frame('variable 1'=1:2,two=3:4,check.names = F)
df

# tibbles just get the names
tbbl<-tibble('variable 1'=1:2,two=3:4)
tbbl

df$tw # Enables partial matching of arguments
tbbl$tw # Prohibitted

df[,"two"] # The object is converted to a vector
tbbl[,"two"] # Tibbles never

url <- "https://data.cityofchicago.org/api/views/5neh-572f/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
all_stations<-
  # Read in the data
  read_csv(url) %>%
  # Filter columns and rename stationname
  select(station=stationname,date,rides) %>%
  # Convert  the character data field to a date encoding
  mutate(date=mdy(date),rides=rides/1000) %>%
  # Summarize the multiple records using the maximum
  group_by(date,station)%>%
  summarise(rides=max(rides),.groups="drop")
head(all_stations)



### A review of R modeling fundamentals
## An example
data(crickets,package = "modeldata")
names(crickets)

# Plot the temp on the x, the chirp rate on the y. The plot
# elements will be colored differently for each species
ggplot(crickets,
       aes(x=temp,y=rate,color=species,pch=species,lty=species))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=F,alpha=0.5)+
  scale_color_brewer(palette = "Paired")+
  labs(x="Temperature (C)",y="Chirp Rate (per minute")

# fitting linear model
interaction_fit<-lm(rate~(temp+species)^2,data=crickets)
summary(interaction_fit)

# Plotting diagnostic plots
par(mfrow=c(1,2))
plot(interaction_fit,which=1);plot(interaction_fit,which=2)

# Assessing if the inclusion of the interaction is necessary
main_effect_fit<-lm(rate~temp+species,data=crickets)
anova(main_effect_fit,interaction_fit)

summary(main_effect_fit)

# Estimating not observed value
new_values<-data.frame(species="O. exclamationis",temp=15:20)
predict(main_effect_fit,new_values)


## Why tidiness is important for modeling

# Three ways to plot
# plot(plot_data$x,plot_data$y)
# library(lattice)
# xyplot(y~x,data=plot_data)
# library(ggplot2)
# ggplot(plot_data,aes(x=x,y=y))+geom_point()

# NA action
new_values$temp[1]<-NA
predict(main_effect_fit,new_values) # Just passes NA
# Alternative
predict(main_effect_fit,new_values,na.action = na.fail)
predict(main_effect_fit,new_values,na.action = na.omit)

# Examples of Standardizing the structures of R objects
corr_res<-map(mtcars%>%select(-mpg),cor.test,y=mtcars$mpg)
corr_res[[1]] # 피어슨 곱 적률 상관
broom::tidy(corr_res[[1]]) 
# These can be stacked and added to a ggplot()
library(broom)
corr_res%>%
  map_dfr(tidy,.id="predictor")%>%
  ggplot(aes(x=fct_reorder(predictor,estimate)))+
  geom_point(aes(y=estimate))+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.1)+
  labs(x=NULL,y="Correlation with mpg")



### Combining base R models and the tidyverse
# Preprocessing
split_by_species<-
  crickets%>%
  group_nest(species)
split_by_species # Group is specific, but the others are data.

# Fitting
model_by_species<-
  split_by_species%>%
  mutate(model=map(data,~lm(rate~temp,data=.x))) 
model_by_species # Input model lists at "model" column

# Collecting the coefficients
model_by_species%>%
  mutate(coef=map(model,tidy))%>% # From list to tibble
  select(species,coef)%>% # Select important columns
  unnest(cols = c(coef)) # Spread out the data

# All can be done at once!
crickets%>%
  group_nest(species)%>%
  mutate(model=map(data,~lm(rate~temp,data=.x)))%>%
  mutate(coef=map(model,tidy))%>%
  select(species,coef)%>%
  unnest(cols = c(coef)) 

# Handling naming conflicts
library(conflicted)
conflict_prefer("filter", winner = "dplyr")
# Auto handling of naming conflicts
tidymodels::tidymodels_prefer(quiet = F)


### The Ames housing data
# house characteristics (bedrooms, garage, fireplace, pool, porch, etc.)
# location (neighborhood)
# lot information (zoning, shape, size, etc.)
# ratings of condition and quality
# sale price

library(tidymodels)
tidymodels_prefer()
data(ames)
dim(ames)

ggplot(ames,aes(x=Sale_Price))+
  geom_histogram(bins = 50,col="white")+ # Right-skewed
  scale_x_log10() # Logarithmic transform

ames<- # Complete the transformation
  ames%>%
  mutate(Sale_Price=log10(Sale_Price))

## Data spending
# Data is resource for modeling, and it is limited.
# So we can think there is an available data budget.

# Train-Test split
set.seed(501)
ames_split<-initial_split(ames,prop=0.8)
ames_split

# Gain resulting data sets
ames_train<-training(ames_split)
ames_test<-testing(ames_split)
dim(ames_train)

# Stratified split for skewed data
set.seed(502)
ames_split<-initial_split(ames,prop = 0.8,strata = Sale_Price)
ames_train<-training(ames_split)
ames_test<-testing(ames_split)
dim(ames_train)

# Validation split
set.seed(52)
ames_val_split<-initial_validation_split(ames,prop=c(0.6,0.2))
ames_val_split
# Getting data
ames_train<-training(ames_val_split)
ames_test<-testing(ames_val_split)
ames_val<-validation(ames_val_split)


### Fitting models with parsnip package!

## Creating a model
linear_reg()%>%set_engine("lm")
linear_reg()%>%set_engine("glmnet")
linear_reg()%>%set_engine("stan")

# Details on how parsnip converts user's code 
# to a package's syntax
linear_reg()%>%set_engine("lm")%>%translate()
linear_reg(penalty = 1)%>%set_engine("glmnet")%>%translate()
linear_reg()%>%set_engine("stan")%>%translate()

# Simple predictive model
lm_model<-
  linear_reg()%>%
  set_engine("lm")

lm_form_fit<-
  lm_model%>%
  # Sale_Price has been log-transformed
  fit(Sale_Price~Longitude+Latitude,data=ames_train)
lm_xy_fit<-
  lm_model%>%
  fit_xy(
    x=ames_train%>%select(Longitude,Latitude),
    y=ames_train%>%pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit

# Random forest argument names used by parsnip.
# Argument Type	parsnip
# # sampled predictors:	mtry
# # trees:	trees
# # data points to split:	min_n
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()

# Engine-specific arguments can be specified in set_engine()
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger", verbose = TRUE) %>%
  set_mode("regression")


## Use the model results
# examine the model output
# The outputs can be found in an element called fit
# which can be returned using the extract_fit_engine() function
lm_form_fit %>% extract_fit_engine()

# Normal methods can be applied to this object, 
# such as printing and plotting:
lm_form_fit %>% extract_fit_engine() %>% vcov()

# Saving particular results
model_res<-
  lm_form_fit %>%
  extract_fit_engine() %>%
  summary()

param_est <- coef(model_res)
class(param_est)
param_est

# Also possible
lm_form_fit %>%
  extract_fit_engine() %>%
  summary()%>%
  coef()

# the broom package can convert many types of model objects 
# to a tidy structure by methods like tidy().
tidy(lm_form_fit)


## Make predictions
# For predictions, parsnip always conforms to the following rules:
#   The results are always a tibble.
#   The column names of the tibble are always predictable.
#   There are always as many rows in the tibble 
#   as there are in the input data set.
ames_test_small<-ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)                                     

# These three rules make it easier 
# to merge predictions with the original data:
ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  # Add 95% prediction intervals to the results
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

# The tidymodels mapping of prediction types and column names.
#   type value |column name(s)
#   numeric    |.pred
#   class      |.pred_class
#   prob       |.pred_{class levels}
#   conf_int   |.pred_lower, .pred_upper
#   pred_int   |.pred_lower, .pred_upper
tree_model<-
  decision_tree(min_n = 2) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tree_fit<-
  tree_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(tree_fit, ames_test_small))

# A list of all of the models that can be used with parsnip 
# (across different packages that are on CRAN) can be found at 
ref<-"https://www.tidymodels.org/find/."
parsnip_addin() #Automatically writes the unique model name!





#### Workflow basics
library(tidymodels)
tidymodels_prefer()

lm_model<-
  linear_reg() %>%
  set_engine("lm")

# A workflow always requires a parsnip model object
lm_wflow<-
  workflow() %>%
  add_model(lm_model)
lm_wflow

lm_wflow<-
  lm_wflow %>%
  add_formula(Sale_Price~Longitude+Latitude)
lm_wflow

# Workflows have a fit method
lm_fit<-fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test%>%slice(1:3))

# Both the model and preprocessor can be removed or updated
lm_fit %>% update_formula(Sale_Price~Longitude)


## Adding raw variables to the workflow
lm_wflow<-
  lm_wflow%>%
  remove_formula() %>%
  add_variables(outcomes = Sale_Price,
                predictors = c(Longitude, Latitude))
lm_wflow

# Equivalent ways
lm_wflow<-
  lm_wflow%>%
  add_variables(outcomes = Sale_Price,
                predictors = c(ends_with("tude")))
lm_wflow<-
  lm_wflow%>%
  add_variables(outcomes = Sale_Price,
                predictors = everything())

fit(lm_wflow, ames_train)


### Special formulas and inline functions
# install.packages("multilevelmod")
library(multilevelmod)

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance,
                predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit

# install.packages("censored")
library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(fustat, futime),
                predictors = c(age, rx)) %>% 
  add_model(parametric_spec, 
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit



#### Creating multiple workflows at once
location<-list(
  longitude=Sale_Price~Longitude,
  latitude=Sale_Price~Latitude,
  coords=Sale_Price~Longitude+Latitude,
  neighborhood=Sale_Price~Neighborhood
)

location_models<-workflow_set(preproc=location,
                              models = list(lm=lm_model))
location_models
location_models$info[[1]]
extract_workflow(location_models, id="coords_lm")


location_models<-
  location_models %>%
  mutate(fit=map(info, ~fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]


### Evaluating the test set
final_lm_res<-last_fit(lm_wflow, ames_split)
final_lm_res
fitted_lm_wflow<-extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)




#### Feature Engineering with recipes
lm(Sale_Price ~ 
     Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, 
   data = ames)

simple_ames<-
  recipe(Sale_Price~
           Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type,
         data=ames_train) %>%
  step_log(Gr_Liv_Area, base=10) %>%
  step_dummy(all_nominal_predictors())
simple_ames

### Using recipes
lm_wflow %>%
  add_recipe(simple_ames) # Error
# Only one preprocessing method at a time
lm_wflow<-
  lm_wflow%>%
  remove_variables()%>%
  add_recipe(simple_ames)
lm_wflow

lm_fit<-fit(lm_wflow, ames_train)
predict(lm_fit, ames_test%>%slice(1:3))

## Get the recipe
lm_fit%>%extract_recipe(estimated=T)

# To tidy the model fit
lm_fit%>%
  #Return the parsnip object
  extract_fit_parsnip()%>%
  #Now tidy the linear model object
  tidy()%>%
  slice(1:5)


### Encoding qualitative data in a numeric format
library(tidymodels)
tidymodels_prefer()

ggplot(data=ames)+
  geom_bar(aes(y=Neighborhood))

simple_ames<-
  recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type,
         data=ames_train)%>%
  step_log(Gr_Liv_Area,base=10)%>%
  step_other(Neighborhood,threshold=0.01)%>%
  step_dummy(all_nominal_predictors())


#### Interaction Terms














