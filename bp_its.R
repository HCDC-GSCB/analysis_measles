df_ep1 <- df_rt %>%
  filter(episode == 1) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates)))

# Ngày can thiệp có hiệu lực
interventions <- as.Date(c("2019-01-30", "2019-05-31"))

df_ep1 <- df_ep1 %>%
  mutate(
    post1 = ifelse(dates >= interventions[1], 1, 0),
    time_after1 = ifelse(dates >= interventions[1], time - min(time[dates >= interventions[1]]), 0),
    post2 = ifelse(dates >= interventions[2], 1, 0),
    time_after2 = ifelse(dates >= interventions[2], time - min(time[dates >= interventions[2]]), 0),
  )

# mô hình ITS
model_its <- lm(rt ~ time + post1 + time_after1 + post2 + time_after2, data = df_ep1)
summary(model_its)


ggplot(df_ep1, aes(x = dates, y = rt)) +
  geom_line(aes(color = "Rt thực tế"), linewidth = 1) +
  geom_vline(xintercept = as.numeric(interventions),
  linetype = "dotted", color = "red", linewidth = 1) +
  annotate("text",
           x = interventions,
           y = 12,
           label = format(interventions, "%d/%m/%Y"),
           angle = 90,
           vjust = 1.5,
           hjust = 1,
           color = "red", size = 3.5) +
  geom_line(aes(y = model_its$fitted.values, color = "ITS fitted"),
              linewidth = 1.1, alpha = 0.7, se = FALSE) +
  scale_x_date(name = "Ngày nhập viện",
               date_breaks = "1 months",
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
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 11),
    axis.text = element_text(color = "black",
                             size = 12)
  )





df_ep2 <- df_rt %>%
  filter(episode == 2) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates)))

# Ngày can thiệp có hiệu lực
interventions <- as.Date(c("2024-09-30", "2024-10-31"))

df_ep2 <- df_ep2 %>%
  mutate(
    post1 = ifelse(dates >= interventions[1], 1, 0),
    time_after1 = ifelse(dates >= interventions[1], time - min(time[dates >= interventions[1]]), 0),
    post2 = ifelse(dates >= interventions[2], 1, 0),
    time_after2 = ifelse(dates >= interventions[2], time - min(time[dates >= interventions[2]]), 0)
  )

# mô hình ITS
model_its <- lm(rt ~ time + post1 + time_after1 
                + post2 + time_after2,
                data = df_ep2)
summary(model_its)


ggplot(df_ep2, aes(x = dates, y = rt)) +
  geom_line(aes(color = "Rt thực tế"), linewidth = 1) +
  geom_vline(xintercept = as.numeric(interventions),
             linetype = "dotted", color = "red", linewidth = 1) +
  annotate("text",
           x = interventions,
           y = 12,
           label = format(interventions, "%d/%m/%Y"),
           angle = 90,
           vjust = 1.5,
           hjust = 1,
           color = "red", size = 3.5) +
  geom_line(aes(y = model_its$fitted.values, color = "ITS fitted"),
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
