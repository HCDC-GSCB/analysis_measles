##### 2024-2025 ######
df3 <- df_rt %>%
  filter(episode == 2) %>%
  arrange(dates) %>%
  slice(-(1:10)) %>%    
  mutate(time = as.numeric(dates - min(dates)) + 1)

end_intervention_dates <- as.Date(c("2024-09-30","2024-10-31"))
knots  <- as.numeric(end_intervention_dates - min(df3$dates)+1)

## ITS
df3 <- df3 %>%
  mutate(
    intervention = ifelse(dates >= end_intervention_dates[1], 1, 0),
    post.intervention.time = ifelse(dates >= end_intervention_dates[1], time - min(time[dates >= end_intervention_dates[1]]) + 1, 0),
    intervention.2 = ifelse(dates >= end_intervention_dates[2], 1, 0),
    post.intervention.2.time = ifelse(dates >= end_intervention_dates[2], time - min(time[dates >= end_intervention_dates[2]]) + 1, 0)
  )

model.d = gls(rt ~ time + intervention + 
                post.intervention.time + intervention.2 + post.intervention.2.time, 
              data = df3,method="ML", 
              correlation= corARMA(p=2,q=2, form = ~ time))

df3 <- df3 %>% mutate(
  model.d.predictions = predictSE.gls (model.d, df3, se.fit=T)$fit,
  model.d.se = predictSE.gls (model.d, df3, se.fit=T)$se
)

ggplot(df3,aes(time,rt))+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)

#### Trước can thiệp lần 1
df4 <- filter(df3, time < knots[1])
model.e = gls(rt ~ time, data = df4, 
              correlation= corARMA(p=1, q=1, form = ~ time),
              method="ML")

df3 <- df3 %>% mutate(
  model.e.predictions = predictSE.gls (model.e, newdata = df3, se.fit=T)$fit,
  model.e.se = predictSE.gls (model.e, df3, se.fit=T)$se
)

#### Trước can thiệp lần 2
df5 <- filter(df3, time < knots[2])
model.f = gls(rt ~ time + intervention + post.intervention.time, 
              data = df5, 
              correlation= corARMA(p=1, q=1, form = ~ time),
              method="ML")

df3 <- df3 %>% mutate(
  model.f.predictions = predictSE.gls (model.f, newdata = df3, se.fit=T)$fit,
  model.f.se = predictSE.gls (model.f, df3, se.fit=T)$se
)

ggplot(df3,aes(time,rt))+
  geom_ribbon(aes(ymin = model.f.predictions - (1.96*model.d.se), ymax = model.f.predictions + (1.96*model.e.se)), fill = "lightblue")+
  geom_line(aes(time,model.f.predictions),color="blue",lty=2)+
  geom_ribbon(aes(ymin = model.e.predictions - (1.96*model.d.se), ymax = model.e.predictions + (1.96*model.e.se)), fill = "pink")+
  geom_line(aes(time,model.e.predictions),color="red",lty=2)+
  geom_ribbon(aes(ymin = model.d.predictions - (1.96*model.d.se), ymax = model.d.predictions + (1.96*model.d.se)), fill = "lightgreen")+
  geom_line(aes(time,model.d.predictions),color="black",lty=1)+
  geom_point(alpha=0.3)


