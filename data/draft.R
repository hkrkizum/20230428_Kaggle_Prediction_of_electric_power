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

world_map <- ne_countries(scale = "large",
                          returnclass = "sf")

world_map |> 
  ggplot() +
  geom_sf() +
  theme_void() # テーマは「何もない (void)」を選んでみる

world_map |> 
  filter(name  == "China") %>% # 「東アジア」に絞る  
  ggplot() +
  geom_sf() +
  geom_point(data = tmp,
             aes(x = LON, y = LAT)) +
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
