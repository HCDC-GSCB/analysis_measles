## Mốc can thiệp cho từng đợt
interv_ep1 <- as.Date(c("2018-12-01", "2019-02-01"))
interv_ep2 <- as.Date(c("2024-08-31", "2024-10-01", "2024-11-01"))  

######################################################
##### ITS model với Segment breakpoints (Unknow) #####
######################################################

### Episode==1
df1 <- df_rt %>%
  filter(episode == 1) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates))) 

model1_lm <- lm(rt ~ time, data = df1)
model1_seg <- segmented(model1_lm, seg.Z = ~time, psi = as.numeric(interv_ep1 - min(df1$dates)))
summary(model1_seg)

### Episode==2
df2 <- df_rt %>%
  filter(episode == 2) %>%
  arrange(dates) %>%
  mutate(time = as.numeric(dates - min(dates))) 

model2_lm <- lm(rt ~ time, data = df2)
model2_seg <- segmented(model2_lm, seg.Z = ~time, psi = as.numeric(interv_ep2 - min(df2$dates)))
summary(model2_seg)


######################################################
######### ITS model với Segment breakpoints  #########
######################################################

## Episode==1
as.numeric(interv_ep1 - min(df1$dates))
model11_seg <- segmented(model1_lm,
                         seg.Z = ~ time,
                         psi = c(82, 144), 
                         control = seg.control(it.max = 0, fix.npsi = TRUE))
summary(model11_seg)

## Episode==2
as.numeric(interv_ep2 - min(df2$dates))
model22_seg <- segmented(model2_lm,
                         seg.Z = ~ time,
                         psi = c(81, 112, 143), 
                         control = seg.control(it.max = 0, fix.npsi = TRUE))
summary(model22_seg)
