library(tidyverse)
library(skimr)
library(stringr)
library(lubridate)
library(gtsummary)
library(janitor)
library(EpiEstim)

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
