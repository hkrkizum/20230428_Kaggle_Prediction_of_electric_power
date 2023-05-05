# 0. load package -------------------------
library(targets)

# 1. set optins --------------------------------------------------------------
options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = c("tidyverse",
               "forcats",
               "data.table",
               "here",
               "skimr",
               "qs",
               
               "gtsummary",
               "smd",
               
               "gghalves",
               "patchwork",
               
               "tidymodels",
               "doParallel",
               # "doFuture",
               # "parallel",
               
               "xgboost",
               "glmnet",
               "lightgbm",
               "bonsai",
               
               "reticulate",
               "keras",
               
               "torch",
               "tabnet",

               "correlation",
               "see",
               
               "bestNormalize",
               
               "rnaturalearth",
               "rnaturalearthdata",
               "rnaturalearthhires",
               "rgeos"),
  format = "qs",
  seed = 54147
)

reticulate::use_python(python = "/home/rstudio/.virtualenvs/tidymodels/bin/python")
reticulate::use_virtualenv("tidymodels")


# 2. set functions ----------------------------------------------------------
convert_df_num_to_factor <- function(df, param_col_to_factor){
  df |>
    dplyr::mutate(across(.cols = tidyselect::all_of(param_col_to_factor),
                         .fns = as.character)) |>
    dplyr::mutate(across(.cols = tidyselect::all_of(param_col_to_factor),
                         .fns = ~fct_relevel(.x, sort)))
}

make_gg_single_Continuous <- function(df, param_val){
  df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    # dplyr::group_by(target_label) |> 
    # dplyr::summarise(mean = mean({{ param_val }}),
    #                  sd = sd({{ param_val }})) |> 
    ggplot(aes(x = target_label,
               y = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    
    # geom_bar(stat = "identity") +
    geom_half_violin(nudge = 0.05, show.legend = FALSE) +
    geom_half_boxplot(nudge = 0.05,
                      side = "r",
                      fill = "white",
                      show.legend = FALSE) +
    # geom_half_point(transformation = position_jitter(height = 0,
    #                                                  width = 0.05),
    #                 alpha = 0.1, shape = 19) +
    
    ylab(as.character(as_label( enquo(param_val) ))) +
    theme_bw(base_size = 15)
}

make_gg_single_Categoly <- function(df, param_val){
  g1 <- 
    df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    ggplot(aes(x = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    geom_bar(stat = "count", show.legend = FALSE) +
    # ylab(as.character(as_label( enquo(param_val) ))) +
    theme_bw(base_size = 15)
  
  g2 <- 
    df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    ggplot(aes(x = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    geom_bar(position = "fill", show.legend = TRUE) +
    scale_y_continuous(labels = scales::label_percent()) +
    ylab("") +
    theme_bw(base_size = 15)
  
  g1 + g2
}

make_gg_single_Continuous_input_vec <- function(df, vec){
  vec |> 
    purrr::map(.f = ~make_gg_single_Continuous(
      df = df,
      param_val = !!as.name(.x)))
}

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

make_finalized_wkflow_set <- function(wkflow_set){
  wkflow_set |> 
    dplyr::mutate(best_params = map(.x = wflow_id,
                                    .f =  function(param_id){
                                      wkflow_set |> 
                                        workflowsets::extract_workflow_set_result(id = param_id) |> 
                                        tune::select_best()
                                    })) |> 
    dplyr::mutate(final_models = purrr::pmap(.l = list(wflow_id, best_params),
                                             .f = function(param_id, params){
                                               wkflow_set |> 
                                                 workflowsets::extract_workflow(id = param_id) |> 
                                                 finalize_workflow(parameters = params)
                                             }))
}

make_fit_wkflow_set <- function(wkflow_set, df_split){
  wkflow_set |> 
    dplyr::mutate(best_params = map(.x = wflow_id,
                                    .f =  function(param_id){
                                      wkflow_set |> 
                                        workflowsets::extract_workflow(id = param_id) |> 
                                        last_fit(df_split,
                                                 metrics = yardstick::metric_set(rmse, mae, mpe))
                                    })) 
}

make_fit_wkflow_metrics <- function(param_recipe,
                                    param_model,
                                    param_df_kvf = df_kvf){
  wkf <- 
    workflows::workflow() |> 
    add_recipe(recipe = param_recipe) |> 
    add_model(spec = param_model) |> 
    fit_resamples(param_df_kvf)
  
  wkf_metrics <- 
    wkf |> 
    collect_metrics()
  
  return(list(wkf, wkf_metrics))
}


# 3. define pipeline --------------------------------------------------------
list(
  ## 1. ファイル名の設定 -------------------------------------------------
  tar_target(
    name = in_f_train,
    command = {
      here::here("Rawdata", "train_df.csv")
    },
    format = "file"
  ),
  tar_target(
    name = in_f_test,
    command = {
      here::here("Rawdata", "test_df.csv")
    },
    format = "file"
  ),
  tar_target(
    name = in_f_submitt,
    command = {
      here::here("Rawdata", "submission.csv")
    },
    format = "file"
  ),
  
  ## 2. データの読み込み ------------------------------------
  tar_target(
    name = df_train,
    command = {
      data.table::fread(in_f_train)
    }
  ),
  tar_target(
    name = df_test,
    command = {
      data.table::fread(in_f_test)
    }
  ),
  tar_target(
    name = df_submit,
    command = {
      data.table::fread(in_f_submitt)
    }
  ),
  
  ## 3. EDA -----------------------------------------
  ### 1. 時系列に変換 -----------------------
  tar_target(
    name = df_train_time,
    command = {
      df_train |> 
        dplyr::mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        dplyr::arrange(DATE)
    }
  ),
  tar_target(
    name = df_test_time,
    command = {
      df_test |> 
        dplyr::mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        dplyr::arrange(DATE)
    }
  ),
  
  ### 2. 時系列トレンドの把握 -----------------------
  tar_target(
    name = g_EDA_1_trend,
    command = {
      df_train_time |> 
        ggplot(aes(x = DATE, y = POWER)) +
        geom_line() +
        theme_bw()
    }
  ),
  tar_target(
    name = g_EDA_1_trend_group,
    command = {
      df_train_time |> 
        ggplot(aes(x = DATE, y = POWER, group = SOT)) +
        geom_line(alpha = 0.3, colour = "gray50") +
        geom_line(
          data = df_train_time |> 
            dplyr::group_by(DATE) |> 
            dplyr::summarise(median = median(POWER)),
          aes(x = DATE, y = median), inherit.aes = FALSE,
          colour = "blue"
        ) +
        geom_line(
          data = df_train_time |> 
            dplyr::group_by(DATE) |> 
            dplyr::summarise(mean = mean(POWER)),
          aes(x = DATE, y = mean), inherit.aes = FALSE,
          colour = "red"
        ) +
        theme_bw()
    }
  ),
  
  ### 3. 変数の把握 --------------------------------------
  #### 1. データセット作成 --------
  tar_target(
    name = df_EDA,
    command = {
      dplyr::bind_rows(df_train_time |> dplyr::mutate(dataset = "train"),
                       df_test_time  |> dplyr::mutate(dataset = "test")) |> 
        dplyr::mutate(dataset = fct_relevel(dataset, "train")) 
    }
  ),
  
  #### 2. 相関行列 --------
  tar_target(
    name = g_corr_matrix,
    command = {
      res_cor <- 
        df_EDA |> 
        dplyr::select(-ID, -dataset) |> 
        dplyr::relocate(POWER) |> 
        correlation::correlation(redundant = TRUE,
                                 p_adjust = "bonferroni")
      
      res_cor_summary <- summary(res_cor, redundant = TRUE)
      
      g <- plot(res_cor_summary)
      g$layers[[2]] <- NULL
      g + geom_text(aes(x = Parameter1, y = Parameter2, label = Text), size = 2)
    }
  ),
  #### 3. ヒストグラム ------------
  tar_target(
    name = g_hist_target,
    command = {
      df_train_time |> 
        ggplot(aes(x = POWER)) +
        geom_histogram(aes(y=after_stat(density)), 
                       position = "identity", alpha = 0.4, colour = NA) +
        geom_density(linewidth = 1) +
        theme_bw()
    }
  ),
  tar_target(
    name = g_hist_target_log10,
    command = {
      g_hist_target +
        scale_x_continuous(trans = scales::log10_trans())
    }
  ),
  
  tar_target(
    name = param_colnames,
    command = {
      df_EDA |> 
        dplyr::select(-c("ID", "DATE", "dataset", "POWER", "SOT")) |> 
        colnames()
    }
  ),
  tar_target(
    name = g_hist_all,
    command = {
      param_colnames |> 
        purrr::map(.f = make_gg_hist_single_Continuous, df = df_EDA, .progress = TRUE)
    }
  ),
  
  tar_target(
    name = g_corr_all,
    command = {
      param_colnames |> 
        purrr::map(.f = make_gg_corr_single_Continuous, df = df_train_time, .progress = TRUE)
    }
  ),
  
  tar_target(
    name = g_corr_all_log10,
    command = {
      g_corr_all |> 
        purrr::map(.f = function(x){
          x + scale_y_continuous(trans = scales::log10_trans())
      })
    }
  ),
  
  ### 4. 変換 --------------------
  tar_target(
    name = df_EDA_mod,
    command = {
      df_EDA |> 
        dplyr::mutate(across(.cols = c(V_1,
                                       V_6,
                                       # V_8,
                                       V_9,
                                       V_11,
                                       V_12,
        ),
        .fns = ~log10(.x+0.1))) |> 
        dplyr::mutate(V_8 = (\(x) {
          res <- bestNormalize::yeojohnson(x = x)
          return(res$x.t)
        })(V_8)) |>  
        
        dplyr::mutate(V_5 = if_else(V_5 < 0, NA_real_, V_5)) |> 
        dplyr::mutate(V_5 = if_else(is.na(V_5), median(V_5, rm.na = TRUE), V_5)) 
    }
  ),
  
  tar_target(
    name = g_hist_all_mod,
    command = {
      param_colnames |> 
        purrr::map(.f = make_gg_hist_single_Continuous, df = df_EDA_mod, .progress = TRUE)
    }
  ),
  
  tar_target(
    name = g_corr_all_mod,
    command = {
      df_mod <- 
        df_train_time |> 
        dplyr::mutate(across(.cols = c(V_1,
                                       V_6,
                                       # V_8,
                                       V_9,
                                       V_11,
                                       V_12,
        ),
        .fns = ~log10(.x+0.1))) |> 
        dplyr::mutate(V_8 = (\(x) {
          res <- bestNormalize::yeojohnson(x = x)
          return(res$x.t)
        })(V_8)) |>  
        
        dplyr::mutate(V_5 = if_else(V_5 < 0, NA_real_, V_5)) |> 
        dplyr::mutate(V_5 = if_else(is.na(V_5), median(V_5, rm.na = TRUE), V_5))
      
      param_colnames |> 
        purrr::map(.f = make_gg_corr_single_Continuous, df = df_mod, .progress = TRUE)
    }
  ),
  
  tar_target(
    name = g_corr_all_log10_mod,
    command = {
      g_corr_all |> 
        purrr::map(.f = function(x){
          x + scale_y_continuous(trans = scales::log10_trans())
        })
    }
  ),
  tar_target(
    name = g_corr_matrix_mod,
    command = {
      res_cor <- 
        df_EDA_mod |> 
        dplyr::select(-ID, -dataset) |> 
        dplyr::relocate(POWER) |> 
        correlation::correlation(redundant = TRUE,
                                 p_adjust = "bonferroni")
      
      res_cor_summary <- summary(res_cor, redundant = TRUE)
      
      g <- plot(res_cor_summary)
      g$layers[[2]] <- NULL
      g + geom_text(aes(x = Parameter1, y = Parameter2, label = Text), size = 2)
    }
  ),
  ### 5. 地図 -------------------------------------
  tar_target(
    name = g_EDA_map,
    command = {
      world_map <- rnaturalearth::ne_countries(scale = "large",
                                               returnclass = "sf")
      world_map |> 
        filter(name  == "China") %>% # 「東アジア」に絞る  
        ggplot() +
        geom_sf() +
        geom_point(data = df_EDA |> 
                     dplyr::filter(!is.na(POWER)) |> 
                     dplyr::group_by(SOT, LAT, LON) |> 
                     dplyr::summarise(mean = mean(POWER)),
                   aes(x = LON, y = LAT, colour = mean), 
                   alpha = 0.4) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_bw()
    }
  ),
  
  ## 4. モデル作成 ----------------
  ### 1. split -------------
  tar_target(
    name = df_split,
    command = {
      df_train |> 
        rsample::initial_split(prop = 3/4, strata = POWER)
    }
  ),
  tar_target(
    name = df_model_train,
    command = {
      df_split |> 
        rsample::training()
    }
  ),
  tar_target(
    name = df_model_test,
    command = {
      df_split |> 
        rsample::testing()
    }
  ),
  tar_target(
    name = df_kvf,
    command = {
      df_model_train |> 
        rsample::vfold_cv(v = 10, strata = POWER)
    }
  ),
  
  ### 2. recipe --------------
  #### 1. base -----------------------
  tar_target(
    name = rec_base,
    command = {
      df_model_train |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, SOT, new_role = "id variable") |> 
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        recipes::step_integer(DATE_month, DATE_dow)
    }
  ),
  
  tar_target(
    name = rec_base_v2_scaling,
    command = {
      df_model_train |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID,SOT, new_role = "id variable") |> 

        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        
        recipes::step_scale(all_numeric_predictors()) |>
        
        recipes::step_integer(DATE_month, DATE_dow) 
        
    }
  ),  
  
  tar_target(
    name = rec_base_bake,
    command = {
      rec_base |> 
        prep() |> bake(new_data = NULL)
    }
  ),
  tar_target(
    name = rec_base_v2_scaling_bake,
    command = {
      rec_base_v2_scaling |> 
        prep() |> bake(new_data = NULL)
    }
  ),
  
  #### 2. feature engineering ---------
  ##### 1. v1 ---------------
  #
  # 月、曜日、週をカテゴリに
  #
  tar_target(
    name = rec_v1,
    command = {
      rec_base |> 
        step_mutate(
          across(.cols = c(DATE_month, DATE_week, DATE_dow), 
                 .fns = ~fct_relabel(as.character(.x), sort))
        ) |> 
        step_dummy(all_nominal_predictors())
    }
  ),
  
  ##### 2. v2 ----------------
  #
  # DATEごとの平均値、移動平均、ラグの値を持ったデータフレームを作成、データセットに結合させる
  # リークの可能性があるが、テストデータにも同じ日が出現しているので問題ないと想定する
  #
  
  ###### 1. データ変換 ----------
  tar_target(
    name = df_mean_POWER,
    command = {
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
        dplyr::mutate(POWER_mean_lag_3 = if_else(is.na(POWER_mean_lag_3), POWER_mean, POWER_mean_lag_3)) |>
        
        dplyr::mutate(POWER_mean_lead_1 = lead(POWER_mean, 1)) |> 
        dplyr::mutate(POWER_mean_lead_2 = lead(POWER_mean, 2)) |> 
        dplyr::mutate(POWER_mean_lead_3 = lead(POWER_mean, 3)) |> 
        
        dplyr::mutate(POWER_mean_lead_1 = if_else(is.na(POWER_mean_lead_1), POWER_mean, POWER_mean_lead_1)) |>
        dplyr::mutate(POWER_mean_lead_2 = if_else(is.na(POWER_mean_lead_2), POWER_mean, POWER_mean_lead_2)) |>
        dplyr::mutate(POWER_mean_lead_3 = if_else(is.na(POWER_mean_lead_3), POWER_mean, POWER_mean_lead_3)) 
    }
  ),
  tar_target(
    name = df_train_mod,
    command = {
      df_train |> 
        dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |> 
        dplyr::left_join(df_mean_POWER) |> 
        dplyr::left_join(df_Geo)
    }
  ),
  tar_target(
    name = df_test_mod,
    command = {
      df_test |> 
        dplyr::mutate(DATE = ymd(DATE, tz = "Asia/Tokyo")) |> 
        dplyr::left_join(df_mean_POWER) |> 
        dplyr::left_join(df_Geo)
    }
  ),
  ###### 2. Split ----------
  tar_target(
    name = df_split_mod,
    command = {
      df_train_mod |> 
        rsample::initial_split(prop = 3/4, strata = POWER)
    }
  ),
  tar_target(
    name = df_model_train_mod,
    command = {
      df_split_mod |> 
        rsample::training()
    }
  ),
  tar_target(
    name = df_model_test_mod,
    command = {
      df_split_mod |> 
        rsample::testing()
    }
  ),
  tar_target(
    name = df_kvf_mod,
    command = {
      df_model_train_mod |> 
        rsample::vfold_cv(v = 10, strata = POWER)
    }
  ),
  ###### 3. recipe ----------
  tar_target(
    name = rec_v2,
    command = {
      df_model_train_mod |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, new_role = "id variable") |> 
        recipes::update_role(SOT, new_role = "id variable") |> 
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        recipes::step_integer(DATE_month, DATE_dow) |> 
        recipes::step_mutate(Geo_Group = forcats::fct_relevel(as.character(Geo_Group), sort)) |> 
        recipes::step_dummy(Geo_Group)
    }
  ),
  
  ##### 3. v3 ----------
  #
  # 投入する変数を検討する
  #
  #
  
  #
  # base
  #
  tar_target(
    name = rec_v3_base,
    command = {
      rec_v2 |> 
        recipes::update_role(
          dplyr::matches("POWER_mean_|Geo_Group_"), new_role = "non-use variable"
          )
    }
  ),
  
  #
  # meanのみ
  #
  tar_target(
    name = rec_v3_mean,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean, new_role = "predictor")
    }
  ),
  
  #
  # mean_roll 1のみ
  #
  tar_target(
    name = rec_v3_roll_1,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_roll_1, new_role = "predictor")
    }
  ),
  
  #
  # mean_roll 5のみ
  #
  tar_target(
    name = rec_v3_roll_5,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_roll_5, new_role = "predictor")
    }
  ),
  
  #
  # mean_roll 7のみ
  #
  tar_target(
    name = rec_v3_roll_7,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_roll_7, new_role = "predictor")
    }
  ),
  
  #
  # lag 1のみ
  #
  tar_target(
    name = rec_v3_lag_1,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1, new_role = "predictor")
    }
  ),
  #
  # lag 2のみ
  #
  tar_target(
    name = rec_v3_lag_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_2, new_role = "predictor")
    }
  ),
  
  #
  # lag 3のみ
  #
  tar_target(
    name = rec_v3_lag_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_3, new_role = "predictor")
    }
  ),
  
  #
  # lag 1, 2
  #
  tar_target(
    name = rec_v3_lag_1_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             new_role = "predictor")
    }
  ),
  
  #
  # lag 1, 2, 3
  #
  tar_target(
    name = rec_v3_lag_1_2_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lag_3,
                             new_role = "predictor")
    }
  ),
  
  #
  # mean + lag
  #
  tar_target(
    name = rec_v3_mean_lag_1,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean,
                             POWER_mean_lag_1,
                             new_role = "predictor")
    }
  ),
  
  #
  # mean + lag
  #
  tar_target(
    name = rec_v3_mean_lag_1_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean,
                             POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             new_role = "predictor")
    }
  ),
  #
  # mean + lag
  #
  tar_target(
    name = rec_v3_mean_lag_1_2_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean,
                             POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lag_3,
                             new_role = "predictor")
    }
  ),
  
  #
  # lead
  #
  tar_target(
    name = rec_v3_lead_1,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lead_1, new_role = "predictor")
      
    }
  ),
  tar_target(
    name = rec_v3_lead_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lead_2, new_role = "predictor")
      
    }
  ),
  tar_target(
    name = rec_v3_lead_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lead_3, new_role = "predictor")
     
    }
  ),
  tar_target(
    name = rec_v3_lead_1_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor")
    }
  ),
  tar_target(
    name = rec_v3_lead_1_2_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             POWER_mean_lead_3,
                             new_role = "predictor")
    }
  ),
  
  #
  # lag and lead
  #
  tar_target(
    name = rec_v3_lag_1_lead_1,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lead_1,
                             new_role = "predictor")
    }
  ),
  tar_target(
    name = rec_v3_lag_1_2_lead_1_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor")
    }
  ),
  tar_target(
    name = rec_v3_lag_1_2_3_lead_1_2_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lag_3,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             POWER_mean_lead_3,
                             new_role = "predictor")
    }
  ),
  
  tar_target(
    name = rec_v3_mean_lag_1_2_lead_1_2,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean,
                             POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor")
    }
  ),
  tar_target(
    name = rec_v3_mean_lag_1_2_3_lead_1_2_3,
    command = {
      rec_v3_base |> 
        recipes::update_role(POWER_mean,
                             POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lag_3,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             POWER_mean_lead_3,
                             new_role = "predictor")
    }
  ),
  
  ##### 4. v4 ---------------------------------
  #
  # 2月末～3月頭にあるスパイク期間をフラグにする
  #
  #
  tar_target(
    name = rec_v4,
    command = {
      df_model_train_mod |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, new_role = "id variable") |> 
        recipes::update_role(SOT, new_role = "id variable") |> 
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        
        recipes::step_mutate(Flag_Spike = if_else(
          DATE >= "2019-02-21" & DATE <= "2019-03-05",
          1,
          0
        )) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        recipes::step_integer(DATE_month, DATE_dow) |> 
        recipes::update_role(dplyr::matches("POWER_mean_"), Geo_Group,
                             new_role = "non-use variable")
    }
  ),
  
  ##### 5. v5 ---------------------------------
  #
  #
  # 地域ごとにクラスタリングする
  #
  #
  tar_target(
    name = df_Geo,
    command = {
      # データ間の距離を算出
      dist_Geo <- dist(df_EDA |> 
                         dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
                         dplyr::select(LAT, LON), method = "euclidean")
      
      # 階層的クラスタリングの実行
      hclust_Geo <- hclust(dist_Geo, method = "ward.D2")
      # クラスタリングの結果をプロット
      # plot(hclust_Geo)
      
      param_cluster <- cutree(hclust_Geo, k = 10)
      
      # param_cluster
      
      df_Geo <- df_EDA |> 
        dplyr::filter(!duplicated(cbind(LAT, LON))) |> 
        dplyr::select(LAT, LON) |> 
        dplyr::mutate(Geo_Group = param_cluster)
      
      return(df_Geo)
    }
  ),
  
  tar_target(
    name = rec_v5,
    command = {
      rec_v3_base |> 
        recipes::update_role(dplyr::matches("Geo_Group_"),
                             new_role = "predictor"
        )
    }
  ),
  
  tar_target(
    name = rec_v5_mod,
    command = {
      rec_v5 |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor"
        )
    }
  ),
  
  tar_target(
    name = rec_v5_lag_1_2_lead_1_2_mean,
    command = {
      rec_v5 |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor"
        )
    }
  ),
  
  tar_target(
    name = rec_v5_mod_v4,
    command = {
      df_model_train_mod |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, new_role = "id variable") |> 
        recipes::update_role(SOT, new_role = "id variable") |> 
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        
        recipes::step_mutate(Flag_Spike = if_else(
          DATE >= "2019-02-21" & DATE <= "2019-03-05",
          1,
          0
        )) |> 
        
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        recipes::step_integer(DATE_month, DATE_dow) |> 
        
        recipes::update_role(dplyr::matches("POWER_mean_"), new_role = "non-use") |> 
        recipes::update_role(POWER_mean_lag_1,
                             POWER_mean_lag_2,
                             POWER_mean_lead_1,
                             POWER_mean_lead_2,
                             new_role = "predictor"
        )
    }
  ),
  
  ##### 6. v6 ----------------------------
  
  
  ### 3. model ----------------
  #### 1. base ----------------
  tar_target(
    name = spec_xgb_base,
    command = {
      boost_tree() |> 
        set_mode("regression") |> 
        set_engine("xgboost")
      
    }
  ),
  tar_target(
    name = spec_lightgbm_base,
    command = {
      boost_tree() |> 
        set_mode("regression") |> 
        set_engine("lightgbm")
      
    }
  ),
  tar_target(
    name = spec_lm_base,
    command = {
      linear_reg() |> 
        set_mode("regression") |> 
        set_engine("lm")
      
    }
  ),
  
  tar_target(
    name = spec_keras_base,
    command = {
      mlp(
        epochs = 20,
        activation = "relu",
        ) |> 
        set_mode("regression") |> 
        set_engine("keras") 
    }
  ),
  
  tar_target(
    name = spec_tabnet_base,
    command = {
      tabnet(
        epochs = 50,
        batch_size = 128) |> 
        set_engine("torch", verbose = TRUE) |> 
        set_mode("regression") 
    }
  ),
  
  #### 2. tune ------------
  ##### 1. spec ---------------
  tar_target(
    name = spec_lm_glmnet_lasso_tune,
    command = {
      linear_reg(mode = "regression", 
                   penalty = tune::tune(), 
                   mixture = 1) %>% 
        set_engine("glmnet") 
    }
  ),
  
  tar_target(
    name = spec_lm_glmnet_ridge_tune,
    command = {
      linear_reg(mode = "regression", 
                 penalty = tune::tune(),
                 mixture = 0) %>% 
        set_engine("glmnet") 
    }
  ),
  
  tar_target(
    name = spec_lm_glmnet_elastic_tune,
    command = {
      linear_reg(mode = "regression", 
                   penalty = tune::tune(),
                   mixture = tune::tune()) %>% 
        set_engine("glmnet") 
    }
  ),
  
  tar_target(
    name = spec_boost_xgboost_tune,
    command = {
      boost_tree(mode = "regression",
                 mtry = tune(),
                 trees = tune(),
                 min_n = tune(),
                 tree_depth = tune(),
                 learn_rate = tune(), 
                 loss_reduction = tune(),
                 sample_size = tune()
      ) |> 
        set_engine("xgboost")
    }
  ),
  
  tar_target(
    name = spec_boost_lightgbm_tune,
    command = {
      boost_tree(mode = "regression",
                 mtry = tune(),
                 trees = tune(),
                 min_n = tune(),
                 tree_depth = tune(),
                 learn_rate = tune(), 
                 loss_reduction = tune()) |> 
        set_engine("lightgbm") 
    }
  ),
  
  ##### 2. range ---------
  tar_target(
    name = param_tune_xgb,
    command = {
      spec_boost_xgboost_tune |> 
        hardhat::extract_parameter_set_dials() |> 
        update(mtry = finalize(mtry(), 
                               rec_base_bake))
    }
  ),
  
  tar_target(
    name = param_tune_lightgbm,
    command = {
      spec_boost_lightgbm_tune |> 
        hardhat::extract_parameter_set_dials() |> 
        update(mtry = finalize(mtry(), rec_base_bake))
    }
  ),
  
  #### 3. Future engineering -----------
  tar_target(
    name = spec_xgb_feature_enginerring,
    command = {
      boost_tree(
        mode = "regression",
        # learn_rate = 0.1
      ) |> 
        set_engine("xgboost") 
    }
  ),
  
  ### 4. workflow --------------
  #### 1. スクリーニング ---------------
  tar_target(
    name = wkf_set_base,
    command = {
      workflowsets::workflow_set(
        preproc = list(base    = rec_base,
                       base_v2 = rec_base_v2_scaling), 
        models = list(xgb      = spec_xgb_base,
                      lightgbm = spec_lightgbm_base,
                      lm       = spec_lm_base
                      # nn       = spec_keras_base
                      ),
        cross = TRUE
      ) 
    }
  ),
  # tar_target(
  #   name = wkf_tabnet_base,
  #   command = {
  #     workflow() |> 
  #       add_recipe(recipe = rec_base) |> 
  #       add_model(
  #         spec = tabnet(
  #           epochs = 50,
  #           batch_size = 128
  #           ) |> 
  #           set_engine("torch", verbose = TRUE) |> 
  #           set_mode("regression")
  #         ) |> 
  #       last_fit(df_split)
  #   }
  # ),
  
  # tar_target(
  #   name = wkf_set_base_v2,
  #   command = {
  #     workflowsets::workflow_set(
  #       preproc = list(base = rec_base_v2_scaling), 
  #       models = list(xgb      = spec_xgb_base,
  #                     lightgbm = spec_lightgbm_base,
  #                     lm       = spec_lm_base,
  #                     nn       = spec_keras_base)
  #     ) 
  #   }
  # ),
  # tar_target(
  #   name = wkf_tabnet_base_v2,
  #   command = {
  #     workflow() |> 
  #       add_recipe(recipe = rec_base_v2_scaling) |> 
  #       add_model(
  #         spec = tabnet(
  #           epochs = 50,
  #           batch_size = 128
  #         ) |> 
  #           set_engine("torch", verbose = TRUE) |> 
  #           set_mode("regression")
  #       )|> 
  #       last_fit(df_split)
  #   }
  # ),
  
  #### 2. base by XGB -------------------------
  tar_target(
    name = wkf_base_xgb,
    command = {
      workflows::workflow() |> 
        add_recipe(recipe = rec_base) |> 
        add_model(spec = spec_xgb_feature_enginerring) |> 
        fit_resamples(df_kvf)
    }
  ),
  
  ### 5. fit -------------------
  tar_target(
    name = wkf_set_base_fit,
    command = {
      wkf_set_base |> 
        workflowsets::workflow_map(fn = "fit_resamples",
                                   verbose = TRUE,
                                   resamples = df_kvf,
                                   metrics = yardstick::metric_set(rmse, mae, mape),
                                   control = control_resamples(
                                     verbose = FALSE,
                                     allow_par = TRUE,
                                     save_pred = TRUE,
                                   ))
    }
  ),
  # tar_target(
  #   name = wkf_tabnet_base_fit,
  #   command = {
  #     wkf_tabnet_base |> 
  #       last_fit(df_split)
  #   }
  # ),
  # tar_target(
  #   name = wkf_set_base_v2_fit,
  #   command = {
  #     make_fit_wkflow_set(wkflow_set = wkf_set_base_v2, df_split = df_split)
  #   }
  # ),
  # tar_target(
  #   name = wkf_tabnet_base_v2_fit,
  #   command = {
  #     wkf_tabnet_base_v2 |> 
  #       last_fit(df_split)
  #   }
  # ),
  
  
  ### 6. metrics --------------
  tar_target(
    name = wkf_set_base_fit_metric,
    command = {
      wkf_set_base_fit |> 
        collect_metrics()
    }
  ),
  # tar_target(
  #   name = wkf_tabnet_base_metric,
  #   command = {
  #     wkf_tabnet_base |> 
  #       collect_metrics()
  #   }
  # ),
  # tar_target(
  #   name = wkf_set_base_v2_fit_metric,
  #   command = {
  #     wkf_set_base_v2_fit |> 
  #       dplyr::mutate(metrics = map(best_params, function(obj){
  #         collect_metrics(obj)
  #       })) |> 
  #       tidyr::unnest_longer(metrics) |> 
  #       dplyr::filter(metrics$.metric == "rmse")
  #   }
  # ),
  # tar_target(
  #   name = wkf_tabnet_base_v2_metric,
  #   command = {
  #     wkf_tabnet_base_v2 |> 
  #       collect_metrics()
  #   }
  # ),
  
  ## 5. モデル改善 -----------------
  # tar_target(
  #  name = base_lightgbd_prediction,
  #  command = {
  #    wkf_set_base_fit$best_params[[2]]$.predictions[[1]]
  #  }
  # ),
  
  ### base by xgb --------------
  tar_target(
    name = wkf_base_xgb_metrics,
    command = make_fit_wkflow_metrics(param_recipe = rec_base,
                                      param_model = spec_xgb_feature_enginerring,
                                      param_df_kvf = df_kvf)
  ),
  
  ### v1 ----------------
  tar_target(
    name = wkf_FE_v1,
    command = make_fit_wkflow_metrics(param_recipe = rec_v1,
                                      param_model = spec_xgb_feature_enginerring,
                                      param_df_kvf = df_kvf)
  ),
  ### v2 ----------------
  # tar_target(
  #   name = wkf_FE_v2,
  #   command = make_fit_wkflow_metrics(param_recipe = rec_v2,
  #                                     param_model = spec_xgb_feature_enginerring,
  #                                     param_df_kvf = df_kvf_mod)
  # ),
  ### v3 & v4 & v5 ----------------
  tar_target(
    name = wkf_FE_v3,
    command = {
      # all_cores <- parallel::detectCores(logical = FALSE)
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
             rec_v5 = rec_v5,
             rec_v5_mod = rec_v5_mod,
             
             rec_v5_mod_v4 = rec_v5_mod_v4,
             
             rec_v5_lag_1_2_lead_1_2_mean = rec_v5_lag_1_2_lead_1_2_mean
            ),
          models = list(xgb_base = spec_xgb_feature_enginerring),
          cross = TRUE
          ) |> 
        workflowsets::workflow_map(fn = "fit_resamples",verbose = TRUE,seed = 54147,
                                   resamples = df_kvf_mod,
                                   metrics = yardstick::metric_set(rmse, mae, mape),
                                   control = control_resamples(save_pred = TRUE, 
                                                               parallel_over ="resamples"))
      
      wkf_metrics <- 
        wkf |> 
        collect_metrics()
      
      doParallel::stopImplicitCluster()
      
      return(list(wkf, wkf_metrics))
    }
  ),
  
  ### 4. selected models -----------------------------------
  # tar_target(
  #   name = wkf_FE_v4,
  #   command = {
  #     # all_cores <- parallel::detectCores(logical = FALSE)
  #     all_cores <- 10
  # 
  #     library(doParallel)
  #     cl <- makePSOCKcluster(all_cores)
  #     registerDoParallel(cl)
  # 
  # 
  #     wkf <-
  #       workflowsets::workflow_set(
  #         preproc = list(
  #           rec_v3_base = rec_v3_base,
  #           rec_v2   = rec_v2,
  # 
  #           rec_v3_mean_lag_1_2_lead_1_2 = rec_v3_mean_lag_1_2_lead_1_2,
  #           rec_v5_mod = rec_v5_mod,
  # 
  #         ),
  #         models = list(xgb_base = spec_xgb_feature_enginerring),
  #         cross = TRUE
  #       ) |>
  #       workflowsets::workflow_map(fn = "fit_resamples",verbose = TRUE,seed = 54147,
  #                                  resamples = df_kvf_mod,
  #                                  metrics = yardstick::metric_set(rmse, mae, mape),
  #                                  control = control_resamples(save_pred = TRUE,
  #                                                              parallel_over ="resamples"))
  # 
  #     wkf_metrics <-
  #       wkf |>
  #       collect_metrics()
  # 
  #     doParallel::stopImplicitCluster()
  # 
  #     return(list(wkf, wkf_metrics))
  #   }
  # ),
  
  ## 6. ハイパラチューニング ----------
  ### 1. workflow ----------------
  tar_target(
    name = wkf_set_base_tune,
    command = {
      workflowsets::workflow_set(
        preproc = list(base_v1 = rec_base,
                       base_v2 = rec_base_v2_scaling), 
        models = list(lasso    = spec_lm_glmnet_lasso_tune,
                      ridge    = spec_lm_glmnet_ridge_tune,
                      elastic  = spec_lm_glmnet_elastic_tune,
                      xgb      = spec_boost_xgboost_tune#,
                      # lightgbm = spec_boost_lightgbm_tune
                      )
        ) |>
        workflowsets::option_add(id = c("base_v1_xgb","base_v2_xgb"), 
                                 param_info = param_tune_xgb) 
    }
  ),
  
  ### 2. tune ------------
  tar_target(
    name = wkf_set_base_tune_fit,
    command = {
      
      all_cores <- parallel::detectCores(logical = FALSE)
      
      library(doParallel)
      cl <- makePSOCKcluster(all_cores)
      registerDoParallel(cl)
      
      res <- 
        wkf_set_base_tune |>
        workflowsets::workflow_map(verbose = TRUE,seed = 54147,
                                   resamples = df_kvf,
                                   metrics = yardstick::metric_set(rmse),
                                   
                                   # fn = "tune_bayes",
                                   # iter = 50,
                                   # objective = exp_improve(),
                                   # initial = 10,
                                   # control = control_bayes(verbose = TRUE,
                                   #                         verbose_iter = TRUE,
                                   #                         no_improve = 20,
                                   #                         
                                   #                         allow_par = TRUE,
                                   #                         parallel_over = "resamples")
                                   
                                   fn = "tune_grid",
                                   grid = 20,
                                   control = control_grid(
                                     verbose = TRUE,
                                     save_pred = TRUE,
                                     allow_par = TRUE
                                   )
                                   )
      stopImplicitCluster()
      
      return(res)
    }
  ),
  
  tar_target(
    name = wkf_FE_tune_v1,
    command = {
      # all_cores <- parallel::detectCores(logical = FALSE)
      all_cores <- 10

      library(doParallel)
      cl <- makePSOCKcluster(all_cores)
      registerDoParallel(cl)


      wkf <-
        workflowsets::workflow_set(
          preproc = list(
            rec_v5_mod = rec_v5_lag_1_2_lead_1_2_mean,
            rec_v3_mean_lag_1 = rec_v3_mean_lag_1
          ),
          models = list(xgb = spec_boost_xgboost_tune),
          cross = TRUE
        ) |>
        workflowsets::option_add(id = "rec_v3_mean_lag_1_xgb",
                                 param_info = spec_boost_xgboost_tune |>
                                   hardhat::extract_parameter_set_dials() |>
                                   update(mtry = finalize(
                                     mtry(),
                                     rec_v3_mean_lag_1 |>
                                       prep() |> bake(new_data = NULL)))
                                ) |>
        workflowsets::option_add(id = "rec_v5_mod_xgb",
                                 param_info = spec_boost_xgboost_tune |>
                                   hardhat::extract_parameter_set_dials() |>
                                   update(mtry = finalize(
                                     mtry(),
                                     rec_v5_lag_1_2_lead_1_2_mean |>
                                       prep() |> bake(new_data = NULL)))
        ) |>
        workflowsets::workflow_map(verbose = TRUE,seed = 54147,
                                   resamples = df_kvf_mod,
                                   metrics = yardstick::metric_set(rmse),

                                   fn = "tune_bayes",
                                   iter = 100,
                                   objective = exp_improve(),
                                   initial = 20,
                                   control = control_bayes(verbose = TRUE,
                                                           verbose_iter = TRUE,
                                                           save_pred = TRUE,
                                                           no_improve = 20,

                                                           allow_par = TRUE,
                                                           parallel_over = "resamples"
                                   ))

      wkf_metrics <-
        wkf |>
        collect_metrics()

      doParallel::stopImplicitCluster()

      return(list(wkf, wkf_metrics))
    }
  ),
  ### 3. metrics ---------------
  tar_target(
    name = base_xgb_prediction,
    command = {
      wkf_set_base_tune |> 
        extract_workflow(id = "base_v1_xgb") |> 
        finalize_workflow(
          wkf_set_base_tune_fit |> 
            extract_workflow_set_result(id = "base_v1_xgb") |> 
            select_best()
        ) |> 
        last_fit(df_split)
    }
  ),
  
  ## 7. Submission -------------------
  ### 1. v1 --------------
  #### 1. validate ---------
  tar_target(
    name = wkf_final_v1_validate,
    command = {
      res_lastfit <- 
        wkf_FE_tune_v1[[1]] |> 
        extract_workflow(id = "rec_v5_mod_xgb") |> 
        finalize_workflow(
          wkf_FE_tune_v1[[1]] |> 
            extract_workflow_set_result(id = "rec_v5_mod_xgb") |>
            select_best()    
        ) |>
        last_fit(df_split_mod)
      
      res_lastfit_metrics <- 
        res_lastfit |> 
        tune::collect_metrics()
      
      return(list(res_lastfit, res_lastfit_metrics))
    }
  ),
  
  #### 2. Submit ---------
  tar_target(
   name = wkf_final_v1,
   command = {
     res <- 
       wkf_final_v1_validate[[1]] |> 
       extract_workflow() |> 
       fit(df_train_mod)
     
     df_result <- 
       df_submit |> 
       bind_cols(predict(res, new_data = df_test_mod)) |> 
       dplyr::select(-POWER) |> 
       dplyr::rename(POWER = 2)
     
     fwrite(df_result, here::here("Result", "Submit_v1.csv"))
     
     return(df_result)
   }
  )
)
