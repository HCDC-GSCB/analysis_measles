library(readxl)
library(EpiEstim)
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)
library(knitr)
library(lubridate)
library(slider)
library(zoo)
library(data.table)

df1 <- read_xlsx("C:/Users/Admin/Desktop/Projects/measles/data/data_updated_v8.xlsx", sheet = "data")
df1 <- as.data.frame(df1)

df1 <- df1 %>% clean_names()

df1$tinh_trang_tiem_chung[is.na(df1$tinh_trang_tiem_chung)] <- "Không rõ"

df <- df1 %>% rename(dates = ngay_khoi_phat,
                     ngaysinh = ngay_sinh,
                     ngaynv = ngay_nhap_vien_kham,
                     ngaybc = thoi_gian_bao_cao,
                     tiemchung = tinh_trang_tiem_chung,
                     qh = qhth,
                     gioi = gioi_tinh,
                     tc = tinh_trang_tiem_chung,
                     id = stt)

df$cd <- ifelse(df$phan_loai_chan_doan == "Loại trừ sởi", NA, df$phan_loai_chan_doan)

df <- df[,c("id","dates", "ngaynv", "ngaybc", "cd", "qh", "ngaysinh", "tc")]

df_clean <- df[!is.na(df$cd), c("id","dates", "ngaynv", "ngaybc", 
                                "ngaysinh", "qh", "tc")]

df_clean$lagkpbc <- time_length(interval(df_clean$dates,
                                         df_clean$ngaybc),
                            "day")
df_clean$lagkpnv <- time_length(interval(df_clean$dates,
                                         df_clean$ngaynv),
                            "day")
df_clean$lagnvbc <- time_length(interval(df_clean$ngaynv,
                                         df_clean$ngaybc),
                            "day")

## Lọc theo delay báo cáo - khởi phát

### Từ khởi phát đến nhập viện (6 ngày) và đến báo cáo khoảng 3 ngày 
### (đã tính báo cáo trễ) nên tổng cộng khoảng 9 ngày từ KP đến BC.
df_nor <- filter(df_clean, lagkpbc >= 0 & lagkpbc <= 9)


### Đối chiếu lại cả 3 ngày: KP, NV, BC.
### 1. Kiểm tra có nhập sai ngày với tháng không?
### 2. Nếu không, xem ngày NV và KP có phù hợp không?
### Nếu lagnvbc >= 0 & lagnvbc <= 3 thì KP = NV
### Nếu không thỏa thì KP = BC - 3
df_abnor1 <- filter(df_clean, lagkpbc < 0) 

### Đối chiếu lại cả 3 ngày: KP, NV, BC.
### 1. Kiểm tra có nhập sai ngày với tháng không?
### 2. Nếu không, xem ngày NV và KP có phù hợp không?
### Nếu lagnvbc >= 0 & lagnvbc <= 3 thì KP = NV
### Nếu không thỏa thì KP = BC - 3
df_abnor2 <- filter(df_clean, lagkpbc > 9)

# Hiệu chỉnh dates
df_abnor1 <- df_abnor1 %>%
  mutate(dates = case_when(
    row_number() == 12 ~ as.Date(dates - years(1)),
    TRUE ~ as.Date(dates)
  ))

df_abnor2 <- df_abnor2 %>%
  mutate(dates = case_when(
    row_number() == 8 ~ as.Date(ngaybc - days(1)),
    row_number() == 11 ~ as.Date(ngaybc),
    row_number() == 13 ~ as.Date(ngaybc - days(1)),
    row_number() == 14 ~ as.Date(ngaybc),
    row_number() == 25 ~ as.Date(ngaybc),
    TRUE ~ as.Date(dates)
  ))

## So ngày NV và BC
df_abnor1 <- df_abnor1 %>%
  mutate(dates = case_when(
    lagnvbc >= 0 & lagnvbc <= 4 ~ as.Date(ngaynv),
    TRUE ~ as.Date(ngaybc - days(3))
  ))

df_abnor2 <- df_abnor2 %>%
  mutate(dates = case_when(
    lagnvbc >= 0 & lagnvbc <= 4 ~ as.Date(ngaynv),
    TRUE ~ as.Date(ngaybc - days(3))
  ))

df <- bind_rows(df_nor, df_abnor1, df_abnor2)

df <- df %>% mutate(
  qh = case_when(
    qh %in% c("Thủ Đức", "Quận 9", "Quận 2", "QUẬN 9",
              "THỦ ĐỨC") ~ "TP. Thủ Đức",
    qh %in% c("Bình chánh", "BÌNH CHÁNH") ~ "Bình Chánh",
    qh %in% c("Bình Tân", "BÌNH TÂN") ~ "Bình Tân",
    qh %in% c("Quận 11", "BV QUẬN 11", "QUẬN 11") ~ "Quận 11",
    qh %in% c("Quận 12", "QUẬN 12") ~ "Quận 12",
    qh %in% c("Quận 10", "QUẬN 10") ~ "Quận 10",
    qh %in% c("Quận 7", "QUẬN 7") ~ "Quận 7",
    qh %in% c("Củ Chi", "CỦ CHI") ~ "Củ Chi",
    qh %in% c("Gò Vấp", "GÒ VẤP") ~ "Gò Vấp",
    qh %in% c("Hóc Môn", "HÓC MÔN") ~ "Hóc Môn",
    qh %in% c("Quận 3", "QUẬN 3") ~ "Quận 3",
    qh %in% c("Quận 7", "QUẬN 7") ~ "Quận 7",
    qh %in% c("Tân Bình", "TÂN BÌNH") ~ "Tân Bình",
    qh %in% c("Tân Phú", "TÂN PHÚ") ~ "Tân Phú",
    TRUE ~ qh
  ))

saveRDS(df, "df_clean_02012025.rds")
