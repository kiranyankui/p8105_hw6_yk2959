---
title: "hw6_jw4007"
output: github_document
---


```{r}
library(tidyverse)
library(modelr)
library(mgcv)
library(p8105.datasets)
set.seed(1)
```

## Q2
### Importing data
```{r}
homicide_url = "https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"
homicide = read.csv(homicide_url) %>% janitor::clean_names()
```
There is `r nrow(homicide)` observations and `r ncol(homicide)` variables in the dataset. The variables are `r names(homicide)`

### Prepare data for analysis.
```{r}
homicide_analyze = homicide %>% 
  mutate(case_solve = if_else(disposition == "Closed by arrest", 1, 0),
         city_state = str_c(city, state, sep = ","),
         victim_age = as.numeric(victim_age)) %>%
  filter(city_state != "Dallas,TX",
         city_state != "Phoenix,AZ", 
         city_state != "Kansas City,MO", 
         city_state != "Tulsa,AL") %>% 
  filter(victim_race == "White" | victim_race == "Black")
  
```
Create a city_state variable and a binary variable indicating whether the homicide is solved. Omit cities, limit "victim_race" is white or black. Mutate "victim_age" as a numeric variable. There is `r nrow(homicide_analyze)` observations and `r ncol(homicide_analyze)` variables in the dataset. The variables are `r names(homicide_analyze)`

### Fit a logistic regression for Baltimore.
```{r}
Baltimore_glm = lm(case_solve ~ victim_age + victim_sex + victim_race, data = homicide_analyze) %>% 
  broom::tidy() 
Baltimore_glm %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(lower_bound = exp(estimate - 1.96*std.error),
         upper_bound = exp(estimate + 1.96*std.error),
         odds_ratio = exp(estimate)) %>% 
  select(estimate, odds_ratio, lower_bound, upper_bound) %>% knitr::kable(digits = 2)
```
Outcome = resolved vs unresolved;
Predictors =  victim age, sex and race.
The above table shows the estimate and confidence interval of the adjusted odds ratio for solving homicides comparing male victims to female victims keeping all other variables fixed.


### Fit a logistic regression for all the cities in the dataset.
```{r}
homicide_male_vs_female = homicide_analyze %>% 
  nest(data = !city_state) %>% 
  mutate(models = map(.x = data, ~lm(case_solve ~ victim_age + victim_sex + victim_race, data = .x)),
         results = map(models, broom::tidy)) %>% 
  select(-data, -models) %>% 
  unnest(results) %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(lower_bound = exp(estimate - 1.96*std.error),
         upper_bound = exp(estimate + 1.96*std.error),
         OR_male_vs_female = exp(estimate)) %>% 
  select(city_state, estimate, OR_male_vs_female, lower_bound, upper_bound) 
knitr::kable(homicide_male_vs_female, digits = 2)
  
```
The above table shows the adjusted odds ratios (and CIs) for solving homicides comparing male victims to female victims in all cities keeping all other variables fixed.


### Plot the ORs and CIs for each city.
```{r}
homicide_male_vs_female %>% 
  mutate(city_state = fct_reorder(city_state, OR_male_vs_female, .desc = TRUE)) %>% 
  ggplot(aes(x = OR_male_vs_female, y = city_state)) + 
  geom_point() +
  geom_errorbar(aes(xmin = lower_bound, xmax = upper_bound))
```
The above plot shows that New York City has the lowest odds for solving homicides when comparing male victims to female victims. The odds for solving homicides for male victims is more than 20% lower than female victims, keeping all other variables fixed. It is statistically significant at a level of 5%. On the other hand, the odds for solving homicides when comparing male victims to female victims Albuquerque is the highest, keeping all other variables fixed. The odds for solving homicides for male victims is 1.1 times the odds for solving homicides for female victims, keeping all other variables fixed. However, this odds is not statistically significant.


