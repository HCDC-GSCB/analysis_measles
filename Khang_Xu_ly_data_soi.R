
#-------- CLEAN DATA --------#

# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(EpiEstim)
library(slider)
library(zoo)
library(data.table)
library(sf)
library(plotly)
library(crosstalk)
library(DT)


# Read data
df <- read_xlsx("D:/HCDC/RESEARCH/Measles Outbreaks comparision/SOI_2018-2025_EOC.xlsx", sheet = "SOI_2018-2025", col_types = "text")


# -----------------------
# --- 1. Xử lý data ---
#------------------------
# Filter column names
df1 <- data.frame(df) %>% 
  clean_names() %>% 
  mutate(
    ngay_bc = dmy(ngay_bc),
    ngay_kp = dmy(ngay_kp),
    ngay_nv = dmy(ngay_nv)
  )

# Hàm lọc trùng và tính ngày khởi phát mới 
process_data <-  function(df1, id_col = "id") {
  df_processed <- df1 %>% 
  # Step 1: Lọc trùng tuyệt đối tất cả cột
    distinct() %>% 
  
  # Step 2: Lọc trùng mã định danh, giữ hàng đầu tiên
    distinct(.data[[id_col]], .keep_all = TRUE) %>% 
    
  # Step 3: Tính và tạo cột ngày khởi phát mới
    mutate(
      ngay_kp_hieu_chinh = case_when(
        
        # 2018-2021: Không có ngày khởi phát
        nam_nv <= 2021 ~ ngay_nv - days(3),
        
        # 2024-2025: Có ngày khởi phát
          # TH1: NV trước KP
        (nam_nv >= 2024) & (ngay_nv < ngay_kp) ~ ngay_bc - days(3),
          # TH2: NV sau KP
          # 2a: 0 <= lag <= 9 -> giữ ngày KP
        (nam_nv >= 2024) & ((ngay_bc - ngay_kp >= 0) & (ngay_bc - ngay_kp <= 9)) ~ ngay_kp,
          # 2b: 0 <= lag <= 3-> ngày KP = ngày NV
        (nam_nv >= 2024) & ((ngay_bc - ngay_nv >= 0) & (ngay_bc - ngay_nv <= 3)) ~ ngay_nv,
        (nam_nv >= 2024) ~ ngay_nv - days(3),
        TRUE ~ NA_Date_
      )
    ) %>% 
    
    # Loại trừ ca được chẩn đoán "Loại trừ sởi"
    filter(pl_chan_doan != "Loại trừ sởi"| is.na(pl_chan_doan))
  return(df_processed) 
}
  
df_clean1 <- process_data(df1)
  
#-----------------------------------------------
# 2. Vẽ đường cong dịch tuần theo ngày khởi phát
#-----------------------------------------------

# Gom data theo tuần

data_tuan <- df_clean1 %>% 
  # Làm tròn ngày khởi phát về ngày đầu tiên của tuần đó
  mutate(tuan = floor_date(ngay_kp_hieu_chinh, "week")) %>% 
  
  group_by(tuan) %>% 
  summarise(so_ca = n())

data_tuan_full <- data_tuan %>% 
  complete(tuan = seq.Date(min(tuan), max(tuan), by = "week"),
           fill = list(so_ca = 0))

print(data_tuan_full)

# Ve do thi tuan

ggplot(data = data_tuan, aes(x = tuan, y = so_ca)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  
  labs(
    title = "Measles by week",
    x = "week",
    y = "cases") +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%b %Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Kết quả:
  # Dịch 2018-2019: từ 01/9/2018 (105 ca) - 31/3/2020 (37 ca)
  # Dịch 2024-2025: từ 01/6/2024 (53 ca) - 31/3/2025 (1035 ca)

#-----------------------------------------------
# 3. Chia 2 giai đoạn dịch
#-----------------------------------------------

# Dịch sởi 2018-2019
df_1819 <- df_clean1 %>% 
  filter(ngay_kp_hieu_chinh >= ymd("2018-09-01")&
         ngay_kp_hieu_chinh <= ymd("2020-03-31")) %>% 
  arrange((ngay_kp_hieu_chinh))


# Dịch sởi 2024-2025
df_2425 <- df_clean1 %>% 
  filter(ngay_kp_hieu_chinh > ymd("2024-05-31")) %>% # 27/5
  arrange((ngay_kp_hieu_chinh))

# Kiểm tra thời gian dịch
tail(df_1819$ngay_kp_hieu_chinh)
tail(df_2425$ngay_kp_hieu_chinh)

# Thống kê số liệu
skimr::skim(df_1819)
skimr::skim(df_2425)

#-----------------------------
# -------- Nháp ---------
#-----------------------------

# Kiểm tra độ trễ báo cáo dịch 2018-2019
df_with_lag <- df_1819 %>% 
  mutate(
    do_tre = as.numeric(ngay_bc - ngay_nv)
  )

summary_stats <- df_with_lag %>% 
  filter(do_tre >= 0 & !is.na(do_tre)) %>% 
  summarise(
    trung_binh = mean(do_tre),
    trung_vi = median(do_tre),
    min = min(do_tre),
    max = max(do_tre),
    so_luong = n()
  )
print(summary_stats)
# Kết quả:
  # Mean = 4.34, median = 2, min = 0, max = 374, n = 8910


# Thống kê số lượng ca trễ theo số ngày
tre_tren_10_ngay <- df_with_lag %>% 
  filter(do_tre > 10)

so_ca_tre <- nrow(tre_tren_10_ngay)
print(so_ca_tre) # 346 ca


tre_tren_9_ngay <- df_with_lag %>% 
  filter(do_tre > 9)
so_ca_tre9 <- nrow(tre_tren_9_ngay)
print(so_ca_tre9)  # 433 ca

tre_tren_14_ngay <- df_with_lag %>% 
  filter(do_tre > 14)
so_ca_tre14 <- nrow(tre_tren_14_ngay)
print(so_ca_tre14) # 194 ca


#----------------------
# Xem tổng số ca theo tháng 

april_2020 <- df_clean1 %>% 
  filter(("2020-04-01" < ngay_kp_hieu_chinh) & 
        (ngay_kp_hieu_chinh < "2020-04-30")) %>% 
  nrow()
print(april_2020) # 9 ca

june_2024 <- df_clean1 %>% 
  filter(("2024-06-01" < ngay_kp_hieu_chinh) & 
           (ngay_kp_hieu_chinh < "2024-06-30")) %>% 
  nrow()
print(june_2024) # 53 ca


mar_2025 <- df_clean1 %>% 
  filter(("2025-03-01" < ngay_kp_hieu_chinh) & 
           (ngay_kp_hieu_chinh < "2025-03-31")) %>% 
  nrow()
print(mar_2025) # 1035 ca

september_2018 <- df_clean1 %>% 
  filter(("2018-09-01" < ngay_kp_hieu_chinh) & 
           (ngay_kp_hieu_chinh < "2018-09-30")) %>% 
  nrow()
print(september_2018) # 105 ca
