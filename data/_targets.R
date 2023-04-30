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
               "doFuture",
               "parallel",
               
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
    name = df_submitt,
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
  tar_target(
    name = rec_base,
    command = {
      df_model_train |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, new_role = "id variable") |> 
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        recipes::step_integer(DATE_month, DATE_dow) |> 
        recipes::step_select(-SOT)
    }
  ),
  
  tar_target(
    name = rec_base_v2_scaling,
    command = {
      df_model_train |> 
        recipes::recipe(POWER ~ .) |> 
        recipes::update_role(ID, new_role = "id variable") |> 
        recipes::step_select(-SOT) |> 
        
        recipes::step_mutate(DATE = lubridate::ymd(DATE, tz = "Asia/Tokyo")) |> 
        recipes::step_date(DATE, features = c("month", "week", "dow", "doy"), keep_original_cols = FALSE) |> 
        
        recipes::step_scale(all_numeric_predictors()) |>
        
        recipes::step_integer(DATE_month, DATE_dow) 
        
    }
  ),  
  
  
  ### 3. model ----------------
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
        epochs = 100,
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
  
  ### 4. workflow --------------
  tar_target(
    name = wkf_set_base,
    command = {
      workflowsets::workflow_set(
        preproc = list(base = rec_base), 
        models = list(xgb      = spec_xgb_base,
                      lightgbm = spec_lightgbm_base,
                      lm       = spec_lm_base,
                      nn       = spec_keras_base)
      ) 
    }
  ),
  tar_target(
    name = wkf_tabnet_base,
    command = {
      workflow() |> 
        add_recipe(recipe = rec_base) |> 
        add_model(
          spec = tabnet(
            epochs = 50,
            batch_size = 128
            ) |> 
            set_engine("torch", verbose = TRUE) |> 
            set_mode("regression")
          ) |> 
        last_fit(df_split)
    }
  ),
  
  tar_target(
    name = wkf_set_base_v2,
    command = {
      workflowsets::workflow_set(
        preproc = list(base = rec_base_v2_scaling), 
        models = list(xgb      = spec_xgb_base,
                      lightgbm = spec_lightgbm_base,
                      lm       = spec_lm_base,
                      nn       = spec_keras_base)
      ) 
    }
  ),
  tar_target(
    name = wkf_tabnet_base_v2,
    command = {
      workflow() |> 
        add_recipe(recipe = rec_base_v2_scaling) |> 
        add_model(
          spec = tabnet(
            epochs = 50,
            batch_size = 128
          ) |> 
            set_engine("torch", verbose = TRUE) |> 
            set_mode("regression")
        )|> 
        last_fit(df_split)
    }
  ),
  
  
  ### 5. fit -------------------
  tar_target(
    name = wkf_set_base_fit,
    command = {
      make_fit_wkflow_set(wkflow_set = wkf_set_base, df_split = df_split)
    }
  ),
  # tar_target(
  #   name = wkf_tabnet_base_fit,
  #   command = {
  #     wkf_tabnet_base |> 
  #       last_fit(df_split)
  #   }
  # ),
  tar_target(
    name = wkf_set_base_v2_fit,
    command = {
      make_fit_wkflow_set(wkflow_set = wkf_set_base_v2, df_split = df_split)
    }
  ),
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
        dplyr::mutate(metrics = map(best_params, function(obj){
          collect_metrics(obj)
        })) |> 
        tidyr::unnest_longer(metrics) |> 
        dplyr::filter(metrics$.metric == "rmse")
    }
  ),
  tar_target(
    name = wkf_tabnet_base_metric,
    command = {
      wkf_tabnet_base |> 
        collect_metrics()
    }
  ),
  tar_target(
    name = wkf_set_base_v2_fit_metric,
    command = {
      wkf_set_base_v2_fit |> 
        dplyr::mutate(metrics = map(best_params, function(obj){
          collect_metrics(obj)
        })) |> 
        tidyr::unnest_longer(metrics) |> 
        dplyr::filter(metrics$.metric == "rmse")
    }
  ),
  tar_target(
    name = wkf_tabnet_base_v2_metric,
    command = {
      wkf_tabnet_base_v2 |> 
        collect_metrics()
    }
  ),
  
  ## 5. モデル改善 -----------------
  tar_target(
   name = base_lightgbd_prediction,
   command = {
     wkf_set_base_fit$best_params[[2]]$.predictions[[1]]
   }
  )
)
