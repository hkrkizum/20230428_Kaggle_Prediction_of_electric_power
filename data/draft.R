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
