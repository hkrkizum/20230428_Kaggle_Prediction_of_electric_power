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
               
               "keras",

               "correlation",
               "see"),
  format = "qs",
  seed = 54147
)

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
  )
)

  