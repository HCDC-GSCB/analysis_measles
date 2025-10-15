library(tidyverse)
library(skimr)
library(stringr)
library(lubridate)
library(gtsummary)
library(janitor)
library(EpiEstim)
library(purrr)

## 
process_data <- function(mod_data) {
  df_rt <- mod_data$R %>%
    mutate(
      dates = mod_data$dates[t_end],
      q1_rt = `Quantile.0.025(R)`,
      q3_rt = `Quantile.0.975(R)`,
      rt = `Mean(R)`,
      b_posterior = `Std(R)`^2 / `Mean(R)`,
      a_posterior = `Mean(R)` / b_posterior,
      pct = pgamma(1, shape = a_posterior, scale = b_posterior, lower.tail = FALSE),
      pl = case_when(
        pct > 0.9 ~ "tim",
        pct > 0.75 & pct <= 0.9 ~ "do",
        pct > 0.25 & pct <= 0.75 ~ "cam",
        pct > 0.1 & pct <= 0.25 ~ "vang",
        pct < 0.1 ~ "xanh"
      )
    )
  
  return(df_rt)
}


## Estimate_R() cho Age Group và Vaccinate

library(dplyr)
library(purrr)
library(EpiEstim)
library(tibble)

calc_rt_grouped <- function(data, group_var, 
                            date_col = "dates",
                            date_range = c("2018-08-27", "2020-04-30"),
                            mean_si = 14.5, std_si = 3.25) {
  stopifnot(date_col %in% names(data))
  group_var <- rlang::ensym(group_var)
  
  df_filtered <- data %>%
    filter(.data[[date_col]] >= as.Date(date_range[1]),
           .data[[date_col]] <= as.Date(date_range[2]))
  
  df_grouped <- df_filtered %>%
    group_by(!!group_var, .data[[date_col]]) %>%
    summarise(I = n(), .groups = "drop")
  
  df_rt <- df_grouped %>%
    group_split(!!group_var) %>%
    map_dfr(function(x) {
      est <- EpiEstim::estimate_R(
        incid = x$I,
        method = "parametric_si",
        config = make_config(list(
          mean_si = mean_si, std_si = std_si
        ))
      )
      
      Rtab <- est$R %>%
        mutate(
          group = unique(x[[rlang::as_string(group_var)]]),
          dates = x[[date_col]][t_end],
          b_posterior = (`Std(R)`^2) / `Mean(R)`,
          a_posterior = `Mean(R)` / b_posterior,
          pct = pgamma(1, shape = a_posterior, scale = b_posterior, lower.tail = FALSE),
          pl = case_when(
            pct > 0.9 ~ "tim",
            pct > 0.75 & pct <= 0.9 ~ "do",
            pct > 0.25 & pct <= 0.75 ~ "cam",
            pct > 0.1 & pct <= 0.25 ~ "vang",
            pct < 0.1 ~ "xanh"
          )
        ) %>%
        select(group, dates, rt = `Mean(R)`, q1_rt = `Quantile.0.025(R)`,
               q3_rt = `Quantile.0.975(R)`, a_posterior, b_posterior, pct, pl)
      
      return(Rtab)
    })
  
  return(df_rt)
}

