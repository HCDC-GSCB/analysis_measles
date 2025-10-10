
#-------- TÍNH HỆ SỐ LÂY NHIỄM THEO KHÔNG GIAN (2018 - 2019) --------#

#----------------------
library(dplyr)        
library(tidyr)      
library(purrr)        
library(tibble)       
library(lubridate)  
library(EpiEstim) 
library(sf)           
library(plotly)       
library(crosstalk)

#----------------------
# 1. Chuẩn bị data
#----------------------

# Đổi tên quận huyện

df_1819 <- df_1819 %>%
  mutate(
    quan_huyen = case_when(
      quan_huyen == "Quận Phú Nhuận" ~ "Phú Nhuận",
      quan_huyen == "Quận Bình Thạnh" ~ "Bình Thạnh",
      quan_huyen == "Quận Tân Phú" ~ "Tân Phú",
      quan_huyen == "Quận Tân Bình" ~ "Tân Bình",
      quan_huyen == "Quận Bình Tân" ~ "Bình Tân",
      quan_huyen == "Quận Gò Vấp" ~ "Gò Vấp",
      quan_huyen == "Huyện Củ Chi" ~ "Củ Chi",
      quan_huyen == "Huyện Hóc Môn" ~ "Hóc Môn",
      quan_huyen == "Huyện Nhà Bè" ~ "Nhà Bè",
      quan_huyen == "Huyện Cần Giờ" ~ "Cần Giờ",
      quan_huyen == "Huyện Bình Chánh" ~ "Bình Chánh",
      quan_huyen == "Quận 9" ~ "Thành phố Thủ Đức",
      quan_huyen == "Quận 2" ~ "Thành phố Thủ Đức",
      quan_huyen == "Quận Thủ Đức" ~ "Thành phố Thủ Đức",
      TRUE ~ quan_huyen
    ))

#------------------------------------
# 2. Tạo hàm tính Rt cho 1 quận/huyện
#------------------------------------
tinh_rt_qh <- function(data) {
  
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

# Đổi tên cột
df_1819_date <- df_1819 %>% 
  rename(dates = ngay_kp_hieu_chinh)


# Chạy hàm tính Rt từng quận trên data

df_map_1819 <-  df_1819_date %>% 
  
  group_by(quan_huyen) %>% 
  
  nest() %>% 
  
  mutate(rt_results = map(data, tinh_rt_qh)) %>% 
  
  select(quan_huyen, rt_results) %>% 
  unnest(cols = rt_results) %>% 
  
  rename(qh = quan_huyen)

# ---------------------------
# 3. Chuẩn bị dữ liệu bản đồ
# ---------------------------

# Đọc dữ liệu bản đồ
vn_qh <- st_read(dsn = file.path("D:/HCDC/Research/Sero/Measles Data/gadm41_VNM.gpkg"), layer = "ADM_ADM_2")

# Chọn khu vực Tp. HCM cũ
vn_qh_filter <- filter(vn_qh, NAME_1 == "Hồ Chí Minh")

# Gom Tp. Thủ Đức
vn_qh_filter <- vn_qh_filter %>%
  mutate(NAME_2 = case_when(
    NAME_2 %in% c("Thủ Đức", "Quận 9", "Quận 2") ~ "Thành phố Thủ Đức",
    TRUE ~ NAME_2
  )) %>%
  group_by(NAME_2) %>%
  summarise(geom = st_union(geom))

# Tạo bộ dữ liệu bản đồ mới từ data ca bệnh và data bản đồ,gom theo tên quận/ huyện
vn_qh_filter <- vn_qh_filter %>%
  left_join(df_map_1819, by = c("NAME_2" = "qh"))

vn_qh_filter <- vn_qh_filter %>%
  mutate(
    
    # Phân màu sắc theo US CDC
    
    label = case_when(
      is.na(pct) ~ "Không tính được",
      pct > 0.9 ~ "Đang tăng",
      pct > 0.75 ~ "Có thể tăng",
      pct > 0.25 ~ "Ổn định",
      pct > 0.1 ~ "Có thể giảm",
      TRUE ~ "Đang giảm"
    ),
    color = case_when(
      label == "Đang tăng" ~ "#7f3f98",
      label == "Có thể tăng" ~ "#ed1d24",
      label == "Ổn định" ~ "#f26522",
      label == "Có thể giảm" ~ "#ffde17",
      label == "Đang giảm" ~ "#00a14b",
      TRUE ~ "white"
    )
  )

# Thể hiện dữ liệu

vn_qh_filter <- vn_qh_filter %>%
  mutate(text = ifelse(is.na(pct),
                       paste0(NAME_2, "<br>Rt: Không tính được"),
                       paste0(
                         NAME_2, "<br>",
                         "Rt: ", round(rt, 2), " (", round(q1_rt, 2), " - ",
                         round(q3_rt, 2), ")"
                       )
  ))

# Chọn các cột cần thiết
vn_qh_filter <- vn_qh_filter %>%
  select(NAME_2, dates, label, rt, q1_rt, q3_rt, text, color, geom)

plot <- highlight_key(vn_qh_filter)

p_plotly <- plot_ly(
  data = plot,
  split = ~NAME_2,
  color = ~color,
  type = 'scatter',
  mode = 'none',
  hoveron = 'fills',
  text = ~text,
  hoverinfo = 'text',
  showlegend = FALSE
) %>%
  add_sf(
    fillcolor = ~color,
    line = list(color = "black", width = 1)
  ) %>%
  layout(
    geo = list(
      showland = TRUE,
      landcolor = "lightgray",
      bgcolor = "white",
      projection = list(type = "mercator"),
      visible = TRUE
    ),
    legend = list(
      title = list(text = "Xu hướng lây nhiễm"),
      orientation = "v",
      x = 1.1,
      y = 1,
      bgcolor = "rgba(255,255,255,0.8)",
      bordercolor = "black",
      borderwidth = 1
    ),
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 50, r = 50, t = 50, b = 50),
    annotations = list(
      list(
        x = 1.1,
        y = 1,
        text = "<b>Xu hướng lây nhiễm:</b><br>
        <span style='color:#7f3f98;'>● Số ca đang tăng</span><br>
        <span style='color:#ed1d24;'>● Số ca có thể tăng</span><br>
        <span style='color:#f26522;'>● Số ca ổn định</span><br>
        <span style='color:#ffde17;'>● Số ca có thể giảm</span><br>
        <span style='color:#00a14b;'>● Số ca nhiễm giảm</span><br>
        <span style='color:gray;'>● Không ước tính được Rt</span>",
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        align = 'left'
      )
    )
  )

p_plotly


























