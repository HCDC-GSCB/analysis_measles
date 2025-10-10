
#-------- TÍNH HỆ SỐ LÂY NHIỄM PHÂN TẦNG (2018 - 2019) --------#

#----------------------
library(dplyr)
library(lubridate)


#---------------------------------
# ------ Rt theo nhóm tuổi -------
#---------------------------------

# Chuẩn bị data phân theo nhóm tuổi

df_1819_tuoi <- df_1819 %>% 
  mutate(
    ngay_sinh = dmy(as.character(ngay_sinh)),
    tuoi = 2025 - year(ngay_sinh), 
    nhom_tuoi = case_when(
      tuoi <1 ~ "<1", 
      tuoi >= 1 & tuoi <= 5 ~ "1-5",
      tuoi >=6 & tuoi <= 10 ~ "6-10",
      tuoi >= 11 & tuoi <= 15 ~ "11-15",
      TRUE ~ ">15")
  )

# Tạo hàm tính Rt theo nhóm
tinh_rt_nhom <- function(data) {
  
  # Đếm incidence
  df_incid <- data %>% 
    group_by(dates) %>% 
    summarise(I = n()) %>% 
    complete(dates = seq(min(dates), max(dates), by = "day"), fill = list(I = 0))
  
  # Nếu có ít dữ liệu, trả về NULL
  if (nrow(df_incid) < 15) {
    return(NULL)
  }
  
  t_start <- seq(2, nrow(df_incid) - 13)
  t_end <- t_start + 13
  
  # Mô hình EpiEstim
  mod <- estimate_R(
    incid = df_incid,
    method = "parametric_si",
    config = make_config(list(
      mean_si = 14.5, std_si = 3.25,
      t_start = t_start, t_end = t_end
    ))
  )
  
  df_result <- process_data(mod)
  
  return(df_result)
}

# Đếm incidence
df_rt_1819_tuoi <- df_1819_tuoi %>% 
  rename(dates = ngay_kp_hieu_chinh) %>% 
  filter(nhom_tuoi %in% c("<1", "1-5", "6-10", "11-15", ">15")) %>% 
  
  group_by(nhom_tuoi) %>% 
  nest() %>% 
  mutate(rt_results = map(data, tinh_rt_nhom)) %>% 
  select(nhom_tuoi, rt_results) %>% 
  unnest(cols = rt_results)


# Chuẩn bị df vẽ biểu đồ
df_plot_tuoi <- df_rt_1819_tuoi %>% 
  mutate(
    tuan = week(dates),
    nhom_tuoi_label = case_when(
      nhom_tuoi == "<1" ~ "<1 tuổi",
      nhom_tuoi == "1-5" ~ "1-5 tuổi",
      nhom_tuoi == "6-10" ~ "6-10 tuổi",
      nhom_tuoi == "11-15" ~ "11-15 tuổi",
      nhom_tuoi == ">15" ~ ">15 tuổi"
    )
  )

# Vẽ biểu đồ Rt theo nhóm tuổi
ggplot(df_plot_tuoi, aes(x = tuan, y = rt, color = nhom_tuoi_label, fill = nhom_tuoi_label)) +
  #geom_ribbon(aes(ymin = q1_rt, ymax = q3_rt, fill = nhom_tuoi_label), alpha = 0.3) +
  geom_line(aes(y = rt, color = nhom_tuoi_label), linewidth = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Hệ số Rt theo nhóm tuổi", x = "Tuần", y = "Rt") +
  theme_bw()

#----------------------------------
# ------ Phân tích mô tả Rt -------
#----------------------------------

# Thời gian đạt đỉnh dịch

# Chuẩn bị data 2018-2019
data_tuan_1819 <- df_clean1 %>% 
  # Lọc thời gian
  filter(ngay_kp_hieu_chinh >= ymd("2018-09-01")&
           ngay_kp_hieu_chinh <= ymd("2020-03-31")) %>% 
  arrange((ngay_kp_hieu_chinh)) %>% 
  
  # Làm tròn ngày khởi phát về ngày đầu tiên của tuần đó
  mutate(tuan = floor_date(ngay_kp_hieu_chinh, "week")) %>% 
  
  group_by(tuan) %>% 
  summarise(so_ca = n()) %>% 
  
  complete(tuan = seq.Date(min(tuan), max(tuan), by = "week"),
           fill = list(so_ca = 0))

# Ve do thi tuan

ggplot(data = data_tuan_1819, aes(x = tuan, y = so_ca)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  
  labs(
    title = "Measles by week",
    x = "week",
    y = "cases") +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


peak_1819 <-  data_tuan_1819 %>% 
  slice_max(order_by = so_ca, n = 1)
peak_1810


# Số tuần Rt > 1
# Số tuần Rt < 1 sau tiêm
# Thời gian đợt dịch xảy ra
# Tỷ lệ có biến chứng
# Số ca mới mắc/100.000 dân theo quận/huyện






