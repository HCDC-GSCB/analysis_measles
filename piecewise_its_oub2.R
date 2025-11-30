df_rt <- readRDS("data_clean.rds")

##### 2024-2025 ######
df_ep2 <- df_rt %>%
  filter(episode == 2) %>%
  arrange(dates) %>% 
  slice(-(1:14)) %>%  
  mutate(time = as.numeric(dates - min(dates)) + 1)

## Piecewise regression
end_intervention_dates <- as.Date(c("2024-09-30","2024-10-31"))
knots  <- as.numeric(end_intervention_dates - min(df_ep2$dates))

m <- lm(rt ~ time + I((time-knots[1])*(time>=knots[1])) +
          I((time-knots[2])*(time>=knots[2])),
        data = df_ep2)

df_ep2$pred <- predict(m)

stats_text <- paste(
  "Intercept = 1.749",
  "Slope (Time) = -0.005",
  "Slope (After intervention 1) = +0.011",
  "Slope (After intervention 2) = -0.009",
  "Adj. R² = 0.535",
  "p-value < 0.001",
  sep = "\n"
)

piecewise <- df_ep2 %>% ggplot() +
  geom_line(aes(x = dates, y = rt, color = "Rt"), size = 0.8) + 
  geom_line(aes(x = dates, y = pred, color = "Piecewise Regression"), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = end_intervention_dates[1], 
                 color = "1st intervention end date"), 
             linetype = "dashed", size = 1) +
  
  geom_vline(aes(xintercept = end_intervention_dates[2], 
                 color = "2nd intervention end date"), 
             linetype = "dashed", size = 1) +
  # annotate("text", 
  #        x = max(df_ep2$dates), 
  #        y = 2,                
  #        label = stats_text, 
  #        hjust = 1,              
  #        vjust = 1,              
  #        size = 5.5,            
  #        color = "black") +
  scale_x_date(date_labels = "%m/%Y",
               date_breaks = "1 month",
               name = "Ngày nhập viện") +
  scale_y_continuous(name = expression(R[t])) +
  scale_color_manual(name = NULL, 
                     breaks = c("Rt", "Piecewise Regression", 
                                "1st intervention end date", "2nd intervention end date"),
                     values = c("Rt" = "grey50", 
                                "Piecewise Regression" = "blue",
                                "1st intervention end date" = "red",      
                                "2nd intervention end date" = "darkgreen")) +
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(color = "black", size = 15),
    legend.position = "top", 
    legend.text = element_text(size = 15, color = "black"),
    legend.background = element_rect(fill = "transparent") 
  ) +
  guides(color = guide_legend(override.aes = list(
    linetype = c("solid", "solid", "dashed", "dashed"),
    size = c(0.8, 1, 1, 1)
  )))

ggsave("piecewise_oub2.jpeg", piecewise, dpi = 500,
       height = 7, width = 11, bg = "white")

library(marginaleffects)
a <- knots[1]/2
b <- (knots[2]+knots[1])/2
c <- (max(df_ep2$time)+knots[2])/2
representative <- c(a,b,c)
slope_results <- slopes(
  m, 
  newdata = datagrid(time = representative), 
  variables = "time"
)
slope_results # Absolute slope

## ITS
df_ep2 <- df_ep2 %>%
  mutate(
    intervention = ifelse(dates >= end_intervention_dates[1], 1, 0),
    post.intervention.time = ifelse(dates >= end_intervention_dates[1], time - min(time[dates >= end_intervention_dates[1]]) + 1, 0),
    intervention.2 = ifelse(dates >= end_intervention_dates[2], 1, 0),
    post.intervention.2.time = ifelse(dates >= end_intervention_dates[2], time - min(time[dates >= end_intervention_dates[2]]) + 1, 0)
  )

model.d = gls(rt ~ time + intervention + 
                post.intervention.time + intervention.2 + post.intervention.2.time, 
              data = df_ep2,method="ML") 
              # correlation= corARMA(p=2,q=2, form = ~ time

df_ep2 <- df_ep2 %>% mutate(
  model.d.predictions = predictSE.gls (model.d, df_ep2, se.fit=T)$fit,
  model.d.se = predictSE.gls (model.d, df_ep2, se.fit=T)$se
)

ggplot(df_ep2,aes(time,rt))+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

#### Trước can thiệp lần 1
df4 <- filter(df_ep2, time < knots[1])
model.e = gls(rt ~ time, data = df4,
              method = "ML")

df_ep2 <- df_ep2 %>% mutate(
  model.e.predictions = predictSE.gls (model.e, newdata = df_ep2, se.fit=T)$fit,
  model.e.se = predictSE.gls (model.e, df_ep2, se.fit=T)$se
)

#### Trước can thiệp lần 2
df5 <- filter(df_ep2, time < knots[2])
model.f = gls(rt ~ time + intervention + post.intervention.time, 
              data = df5,
              method="ML")

df_ep2 <- df_ep2 %>% mutate(
  model.f.predictions = predictSE.gls (model.f, newdata = df_ep2, se.fit=T)$fit,
  model.f.se = predictSE.gls (model.f, df_ep2, se.fit=T)$se
)

p_its_overlap <- ggplot(df_ep2, aes(x = time)) +
  geom_ribbon(aes(ymin = model.e.predictions - (1.96*model.e.se), 
                  ymax = model.e.predictions + (1.96*model.e.se)), 
              fill = "pink", alpha = 0.5) +
  geom_ribbon(aes(ymin = model.f.predictions - (1.96*model.f.se), 
                  ymax = model.f.predictions + (1.96*model.f.se)), 
              fill = "lightblue", alpha = 0.5) +
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), 
                  ymax = model.d.predictions + (1.96*model.d.se)), 
              fill = "lightgreen", alpha = 0.5) +
  geom_line(aes(y = model.e.predictions, 
                color = "ITS model without intervention", 
                linetype = "ITS model without intervention"), size = 1) +
  geom_line(aes(y = model.f.predictions, 
                color = "ITS model without 2nd intervention", 
                linetype = "ITS model without 2nd intervention"), size = 1) +
  geom_line(aes(y = model.d.predictions, 
                color = "ITS model with intervention", 
                linetype = "ITS model with intervention"), size = 1) +
  geom_point(aes(y = rt, color = "Rt"), alpha = 0.5) +
  scale_color_manual(name = NULL,
                     breaks = c("ITS model with intervention", 
                                "ITS model without intervention", 
                                "ITS model without 2nd intervention", 
                                "Rt"),
                     values = c("ITS model with intervention" = "black",
                                "ITS model without intervention" = "red",
                                "ITS model without 2nd intervention" = "blue",
                                "Rt" = "gray30")) +
  scale_linetype_manual(name = NULL,
                        values = c("ITS model with intervention" = 1, 
                                   "ITS model without intervention" = 2,
                                   "ITS model without 2nd intervention" = 3,
                                   "Rt" = 0)) +
  
  scale_y_continuous(name = expression(R[t])) +
  scale_x_continuous(name = "Times (Days)",
                     breaks = seq(0,250,50)) +
  theme_classic() +
  theme(
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    legend.text = element_text(size = 18, color = "black"),
    legend.position = "top", 
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(
    linetype = "none", 
    color = guide_legend(
      override.aes = list(
        linetype = c(1, 2, 3, 0), 
        shape = c(NA, NA, NA, 16) 
      )
    )
  )

ggsave("its_oub2.jpeg", plot = p_its_overlap, dpi = 500,
       height = 6, width = 12, bg = "white")


