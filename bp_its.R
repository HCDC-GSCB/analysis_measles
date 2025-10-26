df_ep1 <- df_rt %>%
  filter(episode == 1) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates)) + 1)

# mô hình breakpoints
bp_model <- breakpoints(rt ~ time, data = df_ep1)

opt_m <- which.min(BIC(bp_model))
best_bp <- bp_model$breakpoints[opt_m]
break_dates_auto <- df_ep1$dates[best_bp]

# Fitted
fit_auto <- lm(rt ~ breakfactor(bp_model, breaks = opt_m), data = df_ep1)

# Ngày can thiệp
interventions <- as.Date(c("2018-12-01", "2019-02-01"))

df_ep1 <- df_ep1 %>%
  mutate(
    phase = case_when(
      dates < interventions[1] ~ "Trước 1",
      dates >= interventions[1] & dates < interventions[2] ~ "Sau 1",
      TRUE ~ "Sau 2"
    )
  )

# mô hình ITS
model_its <- lm(rt ~ time + phase, data = df_ep1)
summary(model_its)

# ---- 4. So sánh kết quả hai mô hình ----
comparison_tbl <- tibble(
  episode = 1,
  model_auto_breaks = paste(break_dates_auto, collapse = ", "),
  intervention_dates = paste(interventions, collapse = ", "),
  n_breaks_auto = length(break_dates_auto),
  n_interventions = length(interventions),
  lag_days_first = as.numeric(break_dates_auto[1] - interventions[1])
)

print(comparison_tbl)

# ---- 5. Vẽ biểu đồ ----
ggplot(df_ep1, aes(x = dates, y = rt)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_vline(xintercept = as.numeric(interventions),
             linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_vline(xintercept = as.numeric(break_dates_auto),
             linetype = "dotted", color = "darkgreen", linewidth = 1) +
  geom_line(aes(y=fit_auto$fitted.values), color = "red") +
  labs(
    title = "Phân tích Rt – Đợt 1 (Episode 1)",
    subtitle = paste0(
      "Breakpoints tự động: ", paste(break_dates_auto, collapse = ", "),
      " | Ngày can thiệp: ", paste(interventions, collapse = ", ")
    ),
    x = "Ngày",
    y = "Rt"
  ) +
  theme_minimal(base_size = 13)
