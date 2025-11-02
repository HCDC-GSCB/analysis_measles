df_ep2 <- df_rt %>%
  filter(episode == 2) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates)))

# Ngày can thiệp thực tế
intervention_dates <- as.Date(c("2024-08-31", "2024-10-01", "2024-11-01"))
lag_days <- 14 ## 14 ngày có hiệu lực

effective_dates <- intervention_dates + lag_days

df_ep2 <- df_ep2 %>%
  mutate(
    post1 = ifelse(dates >= effective_dates[1], 1, 0),
    time_after1 = ifelse(dates >= effective_dates[1], time - min(time[dates >= effective_dates[1]]), 0),
    post2 = ifelse(dates >= effective_dates[2], 1, 0),
    time_after2 = ifelse(dates >= effective_dates[2], time - min(time[dates >= effective_dates[2]]), 0),
    post3 = ifelse(dates >= effective_dates[3], 1, 0),
    time_after3 = ifelse(dates >= effective_dates[3], time - min(time[dates >= effective_dates[3]]), 0)
  )

model_its <- lm(rt ~ time + post1 + time_after1 + post2 + time_after2
                + post3 + time_after3, data = df_ep2)
summary(model_its)


p2 <- ggplot(df_ep2, aes(x = dates, y = rt)) +
  geom_line(aes(color = "Rt thực tế"), linewidth = 1) +
  geom_vline(xintercept = as.numeric(effective_dates),
             linetype = "dotted", color = "red", linewidth = 1) +
  annotate("text",
           x = effective_dates,
           y = 12,
           label = format(effective_dates, "%d/%m/%Y"),
           angle = 90,
           vjust = 1.5,
           hjust = 1,
           color = "red", size = 3.5) +
  geom_smooth(aes(y = model_its$fitted.values, color = "ITS fitted"),
              linewidth = 1.1, alpha = 0.7, se = FALSE) +
  scale_x_date(name = "Ngày nhập viện",
               date_breaks = "3 months",
               date_labels = "%m/%Y") +
  scale_y_continuous(name = expression(R[t]),
                     breaks = seq(0, 15.5, 1)) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Rt thực tế" = "black",
      "ITS fitted" = "red"
    )
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "black",
                             size = 12)
  )





df_ep1 <- df_rt %>%
  filter(episode == 1) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates)))

# Ngày can thiệp thực tế
intervention_dates <- as.Date(c("2018-12-01", "2019-02-01"))
lag_days <- 14 ## 14 ngày có hiệu lực

effective_dates <- intervention_dates + lag_days

df_ep1 <- df_ep1 %>%
  mutate(
    post1 = ifelse(dates >= intervention_dates[1], 1, 0),
    time_after1 = ifelse(dates >= intervention_dates[1], time - min(time[dates >= intervention_dates[1]]), 0),
    post2 = ifelse(dates >= intervention_dates[2], 1, 0),
    time_after2 = ifelse(dates >= intervention_dates[2], time - min(time[dates >= intervention_dates[2]]), 0)
  )

model_its <- lm(rt ~ time + post1 + time_after1 + post2 + time_after2
                , data = df_ep1)
summary(model_its)


p2 <- ggplot(df_ep1, aes(x = dates, y = rt)) +
  geom_line(aes(color = "Rt thực tế"), linewidth = 1) +
  geom_vline(xintercept = as.numeric(intervention_dates),
             linetype = "dotted", color = "red", linewidth = 1) +
  annotate("text",
           x = intervention_dates,
           y = 12,
           label = format(intervention_dates, "%d/%m/%Y"),
           angle = 90,
           vjust = 1.5,
           hjust = 1,
           color = "red", size = 3.5) +
  geom_smooth(aes(y = model_its$fitted.values, color = "ITS fitted"),
              linewidth = 1.1, alpha = 0.7, se = FALSE) +
  scale_x_date(name = "Ngày nhập viện",
               date_breaks = "3 months",
               date_labels = "%m/%Y") +
  scale_y_continuous(name = expression(R[t]),
                     breaks = seq(0, 15.5, 1)) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Rt thực tế" = "black",
      "ITS fitted" = "red"
    )
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "black",
                             size = 12)
  )
