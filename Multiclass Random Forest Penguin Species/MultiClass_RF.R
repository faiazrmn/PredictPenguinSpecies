rm(list = ls())

### Packages
require(tidyverse)
require(tidymodels)
require(palmerpenguins)
doParallel::registerDoParallel()


### Check data
penguins
penguins %>% select(species) %>% unique()
levels(penguins$species)


### Data for modellling
penguins_df <- penguins %>% drop_na() %>% select(-year)

GGally::ggpairs(penguins_df, aes(color = species))


### Split into train and test
penguin_split <- initial_split(penguins_df, strata = species)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)
penguin_split


### Define Random Forest Model
rf_spec <- rand_forest(trees = 1000,
                       mtry = tune(),
                       min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")
rf_spec


### Cross Validation Sets to tune RF
penguin_cv <- vfold_cv(penguins_df, strata = species)
penguin_cv


### Add formula and model together with workflow
tune_wf <- workflow() %>% 
  add_formula(species ~ .) %>% 
  add_model(rf_spec)
tune_wf


### Tune HyperParameters
rf_tune <- tune_grid(
  tune_wf,
  resamples = penguin_cv,
  control = control_resamples(save_pred = TRUE),
  grid = 20
)
rf_tune


### Check Grid
rf_tune %>% autoplot()


### Collect Metrics
rf_tune %>% collect_metrics()
rf_tune %>% collect_predictions() %>% conf_mat(.pred_class, species )


### ROC curves for 3 classes
rf_tune %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(species, .pred_Adelie:.pred_Gentoo) %>% 
  autoplot()


### Finalize Model according to accuracy
rf_final <- finalize_model(
  rf_spec,
  rf_tune %>% select_best("accuracy")
)
rf_final


### Testing Data
final_res <- workflow() %>% 
  add_formula(species ~ .)%>%
  add_model(rf_final) %>%
  last_fit(penguin_split)


### Metrics on Test data
final_res %>% collect_metrics()
final_res %>% collect_predictions() %>% conf_mat(.pred_class, species )


### Fit model on whole data and save for Production
final_prod <- workflow() %>% 
  add_formula(species ~ .)%>%
  add_model(rf_final) %>%
  fit(penguins_df)


### Final Model for Production
final_prod


library(vip)
rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(species ~ . ,
      data = penguins_df
  ) %>%
  vip(geom = "col")

saveRDS(final_prod, "Multiclass_RF_Model.rds")

saveRDS(rf_final, "Multiclass_RF_varimp.rds")


