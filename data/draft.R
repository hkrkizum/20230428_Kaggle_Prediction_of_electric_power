tmp <- 
  df_train |> 
  dplyr::mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
  dplyr::arrange(DATE)

tmp |> 
  glimpse()


df_train |> 
  dplyr::mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
  dplyr::arrange(DATE) |> 
  dplyr::group_by(DATE, SOT) |> 
  dplyr::summarise(n = n()) |> 
  dplyr::filter(n != 1)


tmp$DATE |> unique()

tmp |> 
  dplyr::filter(SOT == "2167A")

tmp |> 
  ggplot(aes(x = DATE, y = POWER, group = SOT)) +
  geom_line(alpha = 0.1)
  


library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

world_map <- rnaturalearth::ne_countries(scale = "large",
                                         returnclass = "sf")

world_map |> 
  ggplot() +
  geom_sf() +
  theme_void() # テーマは「何もない (void)」を選んでみる

world_map |> 
  filter(name  == "China") %>% # 「東アジア」に絞る  
  ggplot() +
  geom_sf() +
  geom_point(data = df_EDA |> 
               dplyr::filter(!is.na(POWER)) |> 
               dplyr::group_by(SOT, LAT, LON) |> 
               dplyr::summarise(mean = mean(POWER)),
             aes(x = LON, y = LAT, colour = mean)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw()
  




world_map |> 
  dplyr::filter(str_detect(name, "China"))


tar_load_everything()
ls()

df_train_time |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(mean = mean(POWER), median = median(POWER)) |> 
  ggplot(aes(x = DATE, y = mean)) +
  geom_line()


df_train_time |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(mean = mean(POWER), median = median(POWER)) |> 
  ggplot(aes(x = DATE, y = median)) +
  geom_line()


res_cor <- 
  dplyr::bind_rows(df_train_time, df_test_time) |> 
  dplyr::select(-ID) |> 
  dplyr::relocate(POWER) |> 
  correlation::correlation(redundant = TRUE,
                           p_adjust = "bonferroni")

res_cor_summary <- summary(res_cor, redundant = TRUE)

plot(res_cor_summary)



g

res_cor


df_EDA |> 
  ggplot(aes(x = V_1, colour = dataset)) +
  geom_histogram(aes(y=after_stat(density), fill = dataset), 
                 position = "identity", alpha = 0.4, colour = NA) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()


df_EDA |> 
  ggplot(aes(x = V_1, colour = dataset)) +
  geom_histogram(aes(y=after_stat(density), fill = dataset), 
                 position = "identity", alpha = 0.4, colour = NA) +
  geom_density(linewidth = 1) +
  scale_colour_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw()


make_gg_hist_single_Continuous <- function(df, param_var){
  df |> 
    ggplot(aes(x = !!as.name(param_var), colour = dataset)) +
    geom_histogram(aes(y=after_stat(density), fill = dataset), 
                   position = "identity", alpha = 0.4, colour = NA) +
    geom_density(linewidth = 1) +
    scale_colour_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    theme_bw()
}

make_gg_corr_single_Continuous <- function(df, param_var){
  df |> 
    ggplot(aes(x = !!as.name(param_var), y = POWER)) +
     geom_point(colour = "blue", alpha = 0.4, shape = 20, size = 2) +
    theme_bw()
}


df_train_time |> 
  ggplot(aes(x = V_1, y =POWER)) +
  geom_point(colour = "blue", alpha = 0.4, shape = 20, size = 2) +
  theme_bw()

df_train_time |> 
  ggplot(aes(x = V_1, y =POWER)) +
  geom_point(colour = "blue", alpha = 0.4, shape = 20, size = 2) +
  theme_bw() +
  scale_y_continuous(trans = scales::log10_trans())



df_train_time |> 
  ggplot(aes(x = POWER)) +
  geom_histogram(aes(y=after_stat(density)), 
                 position = "identity", alpha = 0.4, colour = NA) +
  geom_density(linewidth = 1) +
  theme_bw()

df_train_time |>
  dplyr::mutate(POWER = log10(POWER)) |> 
  ggplot(aes(x = POWER)) +
  geom_histogram(aes(y=after_stat(density)), 
                 position = "identity", alpha = 0.4, colour = NA) +
  geom_density(linewidth = 1) +
  theme_bw()

tmp <- 
  df_EDA |> 
  dplyr::select(-c("ID", "DATE", "dataset", "POWER", "SOT")) |> 
  colnames() |> 
  purrr::map(.f = make_gg_hist_single_Continuous, df = df_EDA, .progress = TRUE)

tmp

list(V_1, V_2)

as.name("V_1")

make_gg_hist_single_Continuous(df = df_EDA, param_var = "V_1")

make_gg_hist_single_Continuous(df = df_EDA, param_var = V_3)
make_gg_hist_single_Continuous(df = df_EDA, param_var = V_4)

purrr::map(.x = g_corr_all, .f = function(x){
  x + scale_y_continuous(trans = scales::log10_trans())
})


param_colnames <- 
  df_train_time |> 
  dplyr::select(-c("ID", "DATE", "POWER", "SOT")) |> 
  colnames()

param_colnames
tar_load_everything()
for(i in 1:length(param_colnames)){
  cat(str_c("### ", param_colnames[i], "\n"))
  plot( g_hist_all[[i]] /( g_corr_all[[i]] + g_corr_all_log10[[i]] ) )
}

df_EDA |> 
  dplyr::mutate(across(.cols = c(V_1,
                                 V_6,
                                 # V_8,
                                 V_9,
                                 V_11,
                                 V_12,
                                 ),
                       .fns = ~log10(.x + 0.1))) |> 
  dplyr::mutate(V_8 = (\(x) {
      res <- bestNormalize::yeojohnson(x = x)
      return(res$x.t)
    })(V_8)) |>  
  
  dplyr::mutate(V_5 = if_else(V_5 < 0, NA_real_, V_5)) |> 
  dplyr::mutate(V_5 = if_else(is.na(V_5), median(V_5, rm.na = TRUE), V_5)) |> 
  
  dplyr::select(-POWER) |> 
  dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), .fns = ~is.na(.x)))


df_EDA |> 
  dplyr::filter(V_8 < 0)

library(bestNormalize)
res <- bestNormalize::yeojohnson(x = df_EDA$V_8)

res$x.t |> hist()
df_EDA$V_8 |> hist()

df_EDA |> 
  dplyr::filter(dataset == "train") |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(mean = mean(POWER)) |> 
  ggplot(aes(x = DATE, y = mean)) +
  geom_line() +
  geom_point(
    data = df_EDA |> dplyr::filter(dataset == "test"),
    aes(x = DATE, y = 120), size = 1
  )


examples <- data.frame(
  times = ymd_hms("2022-05-06 23:51:07") +
    hours(1:5) + minutes(1:5) + seconds(1:5)
)
time_rec <- recipe(~ times, examples) %>%
  step_time(all_predictors(), keep_original_cols = FALSE)

time_rec
tidy(time_rec, number = 1)


time_rec <- prep(time_rec, training = examples)

time_values <- bake(time_rec, new_data = examples)
time_values

rec_base |> 
  prep() |> bake(new_data = NULL) |> glimpse()



df_model_train |> 
  recipes::recipe(POWER ~ .) |> 
  recipes::update_role(ID, new_role = "id variable") |> 
  recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
  recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
  recipes::step_integer(DATE_month, DATE_dow) |> 
  recipes::step_select(-SOT, ) |>
  prep() |> bake(new_data = df_model_test)




wkf_base_fit |> predict(new_data = df_model_test)



wkf_base_fit <- 
  wkf_base |> 
  fit(data = df_model_train)

wkf_base_fit

rec_base |> 
  prep() |> 
  bake(new_data = df_model_test) |> 
  predict()


pred <- bind_cols(df_model_test,
                  wkf_base_fit |>  predict(df_model_test))

pred


df_model_test |> 
  dplyr::mutate(estimate = predict(wkf_base_fit, ))
predict(wkf_base_fit, 
        new_data = df_model_test)

rmse(data = pred,
     truth = POWER, 
     estimate = .pred)

wkf_set_base <- 
  workflowsets::workflow_set(
  preproc = list(base = rec_base), 
  models = list(xgb      = spec_xgb_base,
                lightgbm = spec_lightgbm_base,
                lm       = spec_lm_base,
                nn       = spec_keras_base)
  )

wkf_set_base |> 
  dplyr::mutate(wkf = map(wflow_id, .f = function(x){
    wkf_set_base |> 
      extract_workflow(id = x)
  })) |> 
  dplyr::mutate()

library(reticulate)
reticulate::virtualenv_list()
use_python(python = "/home/rstudio/.virtualenvs/tidymodels/bin/python")
use_virtualenv("tidymodels")

library(tensorflow)
tf$constant("Hello Tensorflow!")

res <- make_fit_wkflow_set(wkflow_set = wkf_set_base, df_split = df_split)

wkf_set_base_fit |> 
  dplyr::mutate(metrics = map(best_params, function(obj){
    collect_metrics(obj)
  })) |> 
  tidyr::unnest_longer(metrics) |> 
  dplyr::filter(metrics$.metric == "rmse")

wkf_set_base_fit_metric

tmp <- wkf_set_base_fit_metric$best_params[[1]]$.workflow

tmp |> str()

res <- 
  wkf_set_base |> 
  extract_workflow(id = "base_xgb") |> 
  last_fit(df_split)


tmp <- 
  wkf_set_base_fit_metric |> 
  dplyr::mutate(plot_res = pmap(.l = list(best_params, wflow_id), function(obj, id){
    obj$.predictions[[1]] |> 
      ggplot(aes(x = POWER, y = .pred)) +
      geom_point() +
      ggtitle(id)
  }))

tmp$plot_res

tmp$plot_res[[1]]$.predictions

tmp <- 
  workflow() |> 
  add_recipe(recipe = rec_base_v2_scaling) |> 
  add_model(
    tabnet(
      epochs = 50,
      batch_size = 128) |> 
      set_engine("torch", verbose = TRUE) |> 
      set_mode("regression") 
  ) |> 
  last_fit(df_split)

tmp

wkf_set_base_fit_metric$metrics |> 
  tibble() |> 
  dplyr::mutate(wflow_id = wkf_set_base_fit_metric$wflow_id) |> 
  dplyr::select(wflow_id, .metric, .estimate) 


wkf_set_base_v2_fit_metric$metrics |> 
  tibble() |> 
  dplyr::mutate(wflow_id = wkf_set_base_v2_fit_metric$wflow_id) |> 
  dplyr::select(wflow_id, .metric, .estimate) |> 
  dplyr::mutate(wflow_id = str_replace_all(wflow_id, "base_", "base_v2_"))


wkf_tabnet_base_metric |> 
  dplyr::mutate(wflow_id = "base_tabnet") |> 
  dplyr::filter(.metric == "rmse") |> 
  dplyr::select(wflow_id, .metric, .estimate) 


wkf_tabnet_base_v2_metric |> 
  dplyr::mutate(wflow_id = "base_v2_tabnet") |> 
  dplyr::filter(.metric == "rmse") |> 
  dplyr::select(wflow_id, .metric, .estimate) 




base_lightgbm


base_lightgbd_prediction |> 
  dplyr::mutate(residual = .pred - POWER) |> 
  ggplot(aes(sample = residual)) +
  geom_qq() +
  theme_bw()




base_lightgbd_prediction |> 
  dplyr::select(-POWER) |> 
  dplyr::bind_cols(df_model_test) |> 
  dplyr::mutate(DATE = ymd(DATE)) |> 
  ggplot(aes(x = DATE, y = POWER)) +
  geom_line() +
  geom_line(aes(x = DATE, y = .pred), colour = "red") +
  theme_bw()


base_xgb_prediction$.predictions[[1]] |> 
  dplyr::mutate(residual = .pred - POWER) |> 
  ggplot(aes(sample = residual)) +
  geom_qq() +
  theme_bw()

base_xgb_prediction$.predictions[[1]] |> 
  ggplot(aes(x = POWER, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()

res <- 
  df_model_train |> 
  recipes::recipe(POWER ~ .) |> 
  recipes::update_role(ID, new_role = "id variable") |> 
  recipes::step_select(-SOT) |> 
  
  recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
  recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
  
  recipes::step_scale(all_numeric_predictors()) |>
  
  recipes::step_integer(DATE_month, DATE_dow) |> 
  
  recipes::step_isomap(all_outcomes(), LAT, LON, neighbors = 100) |> 
  prep() |> 
  bake(new_data = NULL)
  
  


df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::group_by(SOT) |> 
  dplyr::summarise(mean = mean(POWER)) |> 
  dplyr::arrange(desc(mean)) |> 
  dplyr::slice(1:3, 500:502, 1428:1430) |> 
  dplyr::left_join(
    df_model_train |> 
      dplyr::mutate(DATE = ymd(DATE)) |>
      dplyr::arrange(DATE)
  ) |> 
  ggplot(aes(x = DATE,
             y = POWER,
             colour = SOT)) +
  geom_line() +
  geom_point()

df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
  dim()

df_model_train |> 
  dplyr::mutate(SOT_ID = str_extract(SOT, "[0-9]*")) |> 
  dplyr::filter(!duplicated(SOT_ID))

# データ間の距離を算出
dist_Geo <- dist(df_model_train |> 
                   dplyr::mutate(DATE = ymd(DATE)) |>
                   dplyr::arrange(DATE) |> 
                   dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
                   dplyr::select(LAT, LON), method = "euclidean")

# 階層的クラスタリングの実行
hclust_Geo <- hclust(dist_Geo, method = "ward.D2")
# クラスタリングの結果をプロット
plot(hclust_Geo)

param_cluster <- cutree(hclust_Geo, k = 100)

df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
  dplyr::mutate(Geo_Group = param_cluster) |> 
  dplyr::mutate(Geo_Group = fct_relevel(as.character(Geo_Group), sort)) |> 
  ggplot(aes(x = DATE, y = POWER, colour = Geo_Group)) +
  geom_line()


df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::group_by(SOT) |> 
  dplyr::summarise(n = n()) |> 
  ggplot(aes(x = n)) +
  geom_histogram()


df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::group_by(SOT) |> 
  dplyr::summarise(n = n()) |> 
  ggplot(aes(x = n)) +
  geom_histogram()



df_model_train |> 
  dplyr::mutate(DATE = ymd(DATE)) |>
  dplyr::arrange(DATE) |> 
  dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
  dplyr::mutate(Geo_Group = param_cluster) |> 
  dplyr::mutate(Geo_Group = fct_relevel(as.character(Geo_Group), sort)) |> 
  dplyr::select(LAT, LON, Geo_Group) |> 
  dplyr::left_join(df_model_train) |> 
  dplyr::group_by(Geo_Group) |> 
  dplyr::summarise(n = n()) |> 
  ggplot(aes(x = n)) +
  geom_histogram()

df_data_number <- 
  df_model_train |> 
  dplyr::group_by(LAT, LON) |> 
  dplyr::summarise(n = n()) 

df_data_mean <- 
  df_model_train |> 
  dplyr::group_by(LAT, LON) |> 
  dplyr::summarise(mean = mean(POWER)) 

df_data_number |> 
  dplyr::left_join(df_data_mean) |> 
  ggplot(aes(x = n, y = mean)) +
  geom_point()


world_map <- rnaturalearth::ne_countries(scale = "large",
                                         returnclass = "sf")
world_map |> 
  filter(name  == "China") %>% # 「東アジア」に絞る  
  ggplot() +
  geom_sf() +
  geom_point(data = df_model_train |> 
               dplyr::group_by(LAT, LON) |> 
               dplyr::summarise(n = n()) |> 
               dplyr::left_join(df_model_train),
             aes(x = LON, y = LAT, colour = n), 
             alpha = 0.4) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw()
  

rec_base_bake |> glimpse()

rec_base_bake |> 
  correlation::correlation() |> 
  summary() |> 
  plot()

rec_base_bake |> 
  dplyr::mutate(across(.cols = c(DATE_month, DATE_week, DATE_dow), 
                       .fns = ~fct_relabel(as.character(.x), sort))) |> 
  skimr::skim()


rec_base |> 
  step_mutate(
    across(.cols = c(DATE_month, DATE_week, DATE_dow), 
           .fns = ~fct_relabel(as.character(.x), sort))
  ) |> 
  step_dummy(all_nominal_predictors()) |> 
  prep() |>
  bake(new_data = NULL) |> 
  skimr::skim()

tmp <- 
  make_fit_wkflow_metrics(param_recipe = rec_base,
                          param_model = spec_xgb_feature_enginerring)


df_tmp <- 
  df_EDA |> 
  dplyr::filter(dataset == "train") |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(POWER = mean(POWER)) |> 
  dplyr::arrange(DATE) |> 
  dplyr::mutate(POWER_lag_1 = lag(POWER, default = NA_real_)) |> 
  dplyr::mutate(POWER_delta_1 = POWER - POWER_lag_1) |> 
  dplyr::mutate(POWER_mean_7 = slider::slide_vec(.x = POWER,
                                                 .f = mean,
                                                 .before = 3,
                                                 .after = 3)) |> 
  dplyr::mutate(POWER_mean_5 = slider::slide_vec(.x = POWER,
                                                 .f = mean,
                                                 .before = 2,
                                                 .after = 2)) |>
  dplyr::mutate(POWER_mean_3 = slider::slide_vec(.x = POWER,
                                                 .f = mean,
                                                 .before = 1,
                                                 .after = 1)) |> 
  tidyr::pivot_longer(cols = -DATE,
                      names_to = "param",
                      values_to = "val")

df_tmp |> 
  dplyr::filter(!param %in% c("POWER_lag_1")) |> 
  ggplot(aes(x = DATE, y = val)) +
  facet_grid(param ~ ., scales = "free_y") +
  geom_line()

df_tmp |> 
  dplyr::filter(param == "POWER") |> 
  dplyr::pull(val) |> 
  acf()


dplyr::setdiff(df_train$DATE |> unique(),
               df_test$DATE |> unique())

# df_train$DATE |> 
df_test$DATE |>
  unique() |>
  str_match("2019/12/25|2019/6/17")

df_tmp |> 
  dplyr::filter(param == "POWER_mean_6") |> 
  dplyr::pull(val) |> 
  acf()


df_mean_POWER <- 
  df_train |> 
  dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(POWER_mean = mean(POWER)) |> 
  dplyr::mutate(POWER_mean_roll_1 = slider::slide_vec(.x = POWER_mean,
                                                      .f = mean,
                                                      .before = 1,
                                                      .after = 1)) |> 
  dplyr::mutate(POWER_mean_roll_5 = slider::slide_vec(.x = POWER_mean,
                                                        .f = mean,
                                                        .before = 2,
                                                        .after = 2)) |> 
  dplyr::mutate(POWER_mean_roll_7 = slider::slide_vec(.x = POWER_mean,
                                                      .f = mean,
                                                      .before = 3,
                                                      .after = 3)) |> 
  dplyr::mutate(POWER_mean_lag_1 = lag(POWER_mean, 1)) |> 
  dplyr::mutate(POWER_mean_lag_2 = lag(POWER_mean, 2)) |> 
  dplyr::mutate(POWER_mean_lag_3 = lag(POWER_mean, 3)) |> 
  dplyr::mutate(POWER_mean_lag_1 = if_else(is.na(POWER_mean_lag_1), POWER_mean, POWER_mean_lag_1)) |> 
  dplyr::mutate(POWER_mean_lag_2 = if_else(is.na(POWER_mean_lag_2), POWER_mean, POWER_mean_lag_2)) |> 
  dplyr::mutate(POWER_mean_lag_3 = if_else(is.na(POWER_mean_lag_3), POWER_mean, POWER_mean_lag_3))
  

df_test |> 
  dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |>
  dplyr::left_join(df_mean_POWER) |> 
  dplyr::filter(if_any(everything(), ~is.na(.x)))


df_model_train_mod |> 
  recipes::recipe(POWER ~ .) |> 
  recipes::update_role(ID, new_role = "id variable") |> 
  recipes::update_role(SOT, new_role = "id variable") |> 
  recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
  recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
  recipes::step_integer(DATE_month, DATE_dow) |> 
  recipes::step_select(
    !dplyr::matches("POWER_mean_"),
    POWER_mean
  ) |> 
  prep() |> 
  bake(new_data = NULL)

tar_load(wkf_FE_v3)
wkf_FE_v3[[1]] |> 
  autoplot(metric = "rmse") +
  geom_text(aes(label = wflow_id), nudge_x = 0.5) +
  coord_flip() +
  facet_wrap(.metric ~ ., scales = "free_x", ncol = 1)

wkf_FE_v3[[1]] |> 
  extract_workflow(id = "rec_v3_base_xgb_base") |> 
  fit(df_model_train_mod) |> 
  extract_preprocessor() |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


tmp$data |> str()


tar_meta() |> 
  dplyr::filter(str_detect(name, "rec_v3")) |> 
  view()


df_train |> 
  dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(POWER_mean = mean(POWER)) |> 
  dplyr::mutate(POWER_mean_roll_1 = slider::slide_vec(.x = POWER_mean,
                                                      .f = mean,
                                                      .before = 1,
                                                      .after = 1)) |> 
  dplyr::mutate(POWER_mean_roll_5 = slider::slide_vec(.x = POWER_mean,
                                                      .f = mean,
                                                      .before = 2,
                                                      .after = 2)) |> 
  dplyr::mutate(POWER_mean_roll_7 = slider::slide_vec(.x = POWER_mean,
                                                      .f = mean,
                                                      .before = 3,
                                                      .after = 3)) |> 
  dplyr::mutate(POWER_mean_lag_1 = lag(POWER_mean, 1)) |> 
  dplyr::mutate(POWER_mean_lag_2 = lag(POWER_mean, 2)) |> 
  dplyr::mutate(POWER_mean_lag_3 = lag(POWER_mean, 3)) |> 
  
  dplyr::mutate(POWER_mean_lead_1 = lead(POWER_mean, 1)) |> 
  dplyr::mutate(POWER_mean_lead_2 = lead(POWER_mean, 2)) |> 
  dplyr::mutate(POWER_mean_lead_3 = lead(POWER_mean, 3)) |> 
  
  # dplyr::mutate(POWER_mean_lag_1 = if_else(is.na(POWER_mean_lag_1), POWER_mean, POWER_mean_lag_1)) |> 
  # dplyr::mutate(POWER_mean_lag_2 = if_else(is.na(POWER_mean_lag_2), POWER_mean, POWER_mean_lag_2)) |> 
  # dplyr::mutate(POWER_mean_lag_3 = if_else(is.na(POWER_mean_lag_3), POWER_mean, POWER_mean_lag_3)) |> 
  
  dplyr::mutate(POWER_mean_roll_1_lag_1 = lag(POWER_mean_roll_1, 1)) |> 
  dplyr::mutate(POWER_mean_roll_1_lag_2 = lag(POWER_mean_roll_1, 2)) |> 
  dplyr::mutate(POWER_mean_roll_1_lag_3 = lag(POWER_mean_roll_1, 3)) |> 
  
  dplyr::mutate(POWER_mean_roll_5_lag_1 = lag(POWER_mean_roll_5, 1)) |> 
  dplyr::mutate(POWER_mean_roll_5_lag_2 = lag(POWER_mean_roll_5, 2)) |> 
  dplyr::mutate(POWER_mean_roll_5_lag_3 = lag(POWER_mean_roll_5, 3)) |> 
  
  dplyr::mutate(POWER_mean_roll_7_lag_1 = lag(POWER_mean_roll_7, 1)) |> 
  dplyr::mutate(POWER_mean_roll_7_lag_2 = lag(POWER_mean_roll_7, 2)) |> 
  dplyr::mutate(POWER_mean_roll_7_lag_3 = lag(POWER_mean_roll_7, 3)) |> 
  glimpse()

df_train |> 
  dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |> 
  dplyr::group_by(DATE) |> 
  dplyr::summarise(POWER_mean = mean(POWER)) |> 
  dplyr::filter(DATE < "2019-04-01") |> 
  dplyr::filter(DATE > "2019-02-01") |> 
  dplyr::filter(POWER_mean > 50)
  


# データ間の距離を算出
dist_Geo <- dist(df_EDA |> 
                   dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
                   dplyr::select(LAT, LON), method = "euclidean")

# 階層的クラスタリングの実行
hclust_Geo <- hclust(dist_Geo, method = "ward.D2")
# クラスタリングの結果をプロット
plot(hclust_Geo)

param_cluster <- cutree(hclust_Geo, k = 10)

param_cluster

df_Geo <- df_EDA |> 
  dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
  dplyr::mutate(Geo_Group = param_cluster)

df_Geo

all_cores <- 10

library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

wkf <- 
  workflowsets::workflow_set(
    preproc = list(
      rec_v3_base = rec_v3_base, 
      # rec_v1   = rec_v1,
      rec_v2   = rec_v2,
      rec_v3_mean = rec_v3_mean,
      
      rec_v3_roll_1 = rec_v3_roll_1,
      rec_v3_roll_5 = rec_v3_roll_5,
      rec_v3_roll_7 = rec_v3_roll_7,
      
      rec_v3_lag_1 = rec_v3_lag_1,
      rec_v3_lag_2 = rec_v3_lag_2,
      rec_v3_lag_3 = rec_v3_lag_3,
      
      rec_v3_lead_1 = rec_v3_lead_1,
      rec_v3_lead_2 = rec_v3_lead_2,
      rec_v3_lead_3 = rec_v3_lead_3,
      
      rec_v3_lag_1_2 = rec_v3_lag_1_2,
      rec_v3_lag_1_2_3 = rec_v3_lag_1_2_3,
      
      rec_v3_mean_lag_1 = rec_v3_mean_lag_1,
      rec_v3_mean_lag_1_2 = rec_v3_mean_lag_1_2,
      rec_v3_mean_lag_1_2_3 = rec_v3_mean_lag_1_2_3,
      
      rec_v3_lag_1_lead_1 = rec_v3_lag_1_lead_1,
      rec_v3_lag_1_2_lead_1_2 = rec_v3_lag_1_2_lead_1_2,
      rec_v3_lag_1_2_3_lead_1_2_3 = rec_v3_lag_1_2_3_lead_1_2_3,
      
      rec_v3_mean_lag_1_2_lead_1_2 = rec_v3_mean_lag_1_2_lead_1_2,
      rec_v3_mean_lag_1_2_3_lead_1_2_3 = rec_v3_mean_lag_1_2_3_lead_1_2_3,
      
      rec_v4 = rec_v4,
      rec_v5 = rec_v5
    ),
    models = list(xgb_base = spec_xgb_feature_enginerring),
    cross = TRUE
  ) |> 
  workflowsets::workflow_map(fn = "fit_resamples",verbose = TRUE,seed = 54147,
                             resamples = df_kvf_mod,
                             metrics = yardstick::metric_set(rmse, mae, mape),
                             control = control_resamples(save_pred = TRUE, 
                                                         parallel_over ="resamples"))
stopImplicitCluster(cl)

wkf$result[[1]]$.notes[[1]]$note

res <- 
  wkf_FE_tune_v1[[1]] |> 
  extract_workflow(id = "rec_v5_mod_xgb") |> 
  finalize_workflow(
    wkf_FE_tune_v1[[1]] |> 
      extract_workflow_set_result(id = "rec_v5_mod_xgb") |>
      select_best()    
  ) |>
  fit(df_train_mod)

tar_load_everything()

res_lastfit <- 
  wkf_FE_tune_v1[[1]] |> 
  extract_workflow(id = "rec_v5_mod_xgb") |> 
  finalize_workflow(
    wkf_FE_tune_v1[[1]] |> 
      extract_workflow_set_result(id = "rec_v5_mod_xgb") |>
      select_best()    
  ) |>
  last_fit(df_split_mod)




tar_meta() |> view()

res
tar_load(df_submitt)

predict(res, new_data = df_model_test_mod) |> 
  bind_cols(df_model_test_mod) |> 
  rmse(truth = POWER, estimate = .pred)


res_lastfit |> 
  collect_metrics()


tar_load(df_test_mod)
df_test
df_test_mod
df_train



df_submitt |> 
  bind_cols(predict(res, new_data = df_test_mod)) |> 
  dplyr::select(-POWER) |> 
  dplyr::rename(POWER = 2) |> 
  fwrite(here::here("Result", "submit.csv"))
  
tar_load(wkf_final_v1_validate)
tar_load(df_train_mod)

wkf_final_v1_validate[[1]] |> 
  extract_workflow() |> 
  fit(df_train_mod)
