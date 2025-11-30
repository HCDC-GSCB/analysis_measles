library(strucchange)

analyze_breakpoints <- function(data, ep) {
  
  data_ep <- data %>%
    filter(episode == ep) %>%
    mutate(
      dates = as.Date(dates),
      time = as.numeric(dates - min(dates))
    )
  
  bp_model <- breakpoints(rt ~ time, data = data_ep)
  
  n_breaks <- which.min(BIC(bp_model))
  best_bp <- breakpoints(rt ~ time, data = data_ep)$breakpoints[n_breaks]
  break_dates <- if (!all(is.na(best_bp))) data_ep$dates[best_bp] else NULL
  
  # Dự đoán fitted values cho số breakpoint tối ưu
  fitted_vals <- fitted(bp_model, breaks = n_breaks)
  
  # Biểu đồ
  p <- ggplot(data_ep, aes(x = dates, y = rt)) +
    geom_line(color = "steelblue", linewidth = 0.7, alpha = 0.7) +
    geom_line(aes(y = fitted_vals), color = "red", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    geom_vline(xintercept = break_dates, linetype = "dashed", color = "darkorange", linewidth = 1) +
    labs(
      title = paste("Episode", ep, "- Estimated Rt Breakpoints"),
      subtitle = if (!is.null(break_dates)) paste("Breakpoints at:", paste(break_dates, collapse = ", ")) else "No breakpoints detected",
      x = "Date",
      y = "Rt"
    ) +
    theme_bw(base_size = 13)
  
  return(list(
    model = bp_model,
    plot = p,
    best_breaks = break_dates
  ))
}

res1 <- analyze_breakpoints(df_rt, 1)
res2 <- analyze_breakpoints(df_rt, 2)

summary(res1$model)

res2$best_breaks
