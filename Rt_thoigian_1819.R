
#-------- TÍNH HỆ SỐ LÂY NHIỄM THEO THỜI GIAN (2018 - 2019) --------#
#---------------------- PHƯƠNG PHÁP CỦA EPIESTIM -----------------------#
#----------------------
library(dplyr)          
library(tidyr)          
library(purrr)          
library(epiestim)       
library(plotly)         
library(tibble)         
library(lubridate)      
#----------------------
# 1. Chuẩn bị data
#----------------------

# Đếm incidence
df_convert_1819 <- df_1819 %>% 
  rename(dates = ngay_kp_hieu_chinh) %>% 
  group_by(dates) %>% summarise(I = n())

# Fill số 0 
df_complete_1819 <- df_convert_1819 %>% 
  complete(dates = seq(min(dates), max(dates), by = "day")) %>% 
  replace_na(list(I = 0)) %>% 
  # Cửa sổ trượt 14 ngày
  mutate(mva_14d = slide_dbl(I, .f = ~mean(.x, na.rm = T), .before = 13))

# Tạo df đầu vào cho mô hình với ngày khởi phát và I
incid_data_1819 <- df_complete_1819 %>%
  select(dates, I)

#--------------------------------------------
# 2. Định nghĩa cửa sổ trượt, serial interval
#--------------------------------------------

# Định nghĩa cửa sổ thời gian trượt
t_start_1819 <- seq(2, nrow(incid_data_1819) - 13)
t_end_1819 <- t_start_1819 + 13

# Chạy mô hình estimate_R
mod_1819 <-estimate_R(
  incid = incid_data_1819,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 14.5,
    std_si = 3.25,
    t_start = t_start_1819,
    t_end = t_end_1819
  ))
)

#--------------------------------------------
# 3. Tính Rt
#--------------------------------------------

# Hàm tính Rt và phân chia khoảng
process_data <- function(mod_data) {
  df_rt <- mod_data$R %>%
    mutate(
      dates = mod_data$dates[t_end],
      
      # Rt trung bình và KTC 95%
      q1_rt = `Quantile.0.025(R)`,
      q3_rt = `Quantile.0.975(R)`,
      rt = `Mean(R)`,
      b_posterior = `Std(R)`^2 / `Mean(R)`,
      a_posterior = `Mean(R)` / b_posterior,
      
      # Tính toán xác suất P(Rt > 1)
      pct = pgamma(1, shape = a_posterior, scale = b_posterior, lower.tail = FALSE),
      
      # Phân loại mức độ dựa trên xác suất P(Rt > 1)
      pl = case_when(
        pct > 0.9 ~ "tim",
        pct > 0.75 & pct <= 0.9 ~ "do",
        pct > 0.25 & pct <= 0.75 ~ "cam",
        pct > 0.1 & pct <= 0.25 ~ "vang",
        pct < 0.1 ~ "xanh"
      )
    )
  
  return(df_rt)
}

# Áp dụng vào data 2018-2019
df_rt_1819 <- process_data(mod_1819)

# Tạo ID để vẽ biểu đồ
df_rt_1819 <- df_rt_1819 %>% 
  arrange(dates) %>% 
  mutate(id = consecutive_id(pl)) %>% 
  mutate(dates = as.Date(dates))


#------------------------------------------------
# 4. Vẽ biểu đồ Rt theo phân loại màu sắc nguy cơ
#------------------------------------------------

plot_rt <- function(data) {
    
    # ---- Step 1: Chuẩn bị data ----
  
    # Tạo palete dạng vector
    color_palette <- c(
      "tim" = "#7f3f98",
      "do" = "#ed1d24",
      "cam" = "#f26522",
      "vang" = "#ffde17",
      "xanh" = "#00a14b"
    )
    
    df_plot <- data %>% 
      arrange(id, dates) %>% 
      # Nhóm dữ liệu theo id, thêm 1 hàng NA vào cuối
      # Ghép tất cả các nhóm + hàng NA vào 1 df
      # Loại bỏ hàng NA cuối bộ df
      group_by(id) %>% 
      group_modify(~ bind_rows(.x, tibble(NA))) %>% 
      ungroup() %>% 
      slice_head(n = -1) %>% 
      
      # Fill giá trị NA bằng giá trị gần nhất phía trên
      fill(pl, id, .direction = "down") %>% 
      
      # Tạo cột màu = color palette
      mutate(color = recode(pl, !!!color_palette))
  
    # ---- Step 2: Vẽ biểu đồ ----
  
    p <- plot_ly()
    
    # Lớp 1: Dải băng màu (KTC 95%)
  p <- p %>% add_ribbons(
    data = df_plot,
    x = ~dates,
    ymin = ~q1_rt,
    ymax = ~q3_rt,
    fillcolor = ~color,
    line = list(color = 'transparent'),
    opacity = 0.5,
    hoverinfo = "text",
    text = ~paste0("<b>", format(dates,"%b %d"), "</b>",
                   "<br><b>Rt:  </b> ", round(rt,2),
                   "<br>  (", round(q1_rt, 2), " - ", round(q3_rt,2), ")"),
    showlegend = FALSE
  )
  
  # Lớp 2: Vẽ ĐƯỜNG THAM CHIẾU Rt = 1
  p <-  p %>% add_lines(
    x = ~dates,
    y = 1,
    line = list(dash = "dash", color = "black"),
    hoverinfo = "none",
    showlegend = FALSE
  )
  
  # Lớp 3: Vẽ đường Rt trung bình
  unique_ids <-  unique(df_plot$id)
  
  # Vòng lặp for vẽ từng màu dựa vào id
  for (i in unique_ids) {
    group_data <- df_plot[df_plot$id == i, ]
    p <- p %>% add_trace(
      data = group_data,
      x = ~dates,
      y = ~rt,
      type = 'scatter',
      mode = "lines",
      line = list(color = group_data$color[1]),
      hoverinfo = "text",
      text = ~paste0("<b>", format(dates,"%b %d"), "</b>",
                     "<br><b>Rt:  </b> ", round(rt,2),
                     "<br>  (", round(q1_rt, 2), " - ", round(q3_rt,2), ")"),
      showlegend = FALSE
    )
  }
  # ---- Step 3: Graph's UI ----  
  
  p <-  p %>%  layout(
    title = "",
    xaxis = list(tilte = "Date", showgrid = TRUE),
    yaxis = list(title = "Estimate Rt", showgrid = TRUE),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hoverlabel = list(bgcolor = "rgba(255,255,255,0.75")
  )
  return(p)
}

plot_1819 <- plot_rt(df_rt_1819)
plot_1819

#------------------------------
# 4. Ghép biểu đồ ca bệnh và Rt
#------------------------------

# 1. Biểu đồ ca bệnh

p_hist_1819 <- plot_ly(df_complete_1819, x = ~dates) %>% # Dùng cột ngày đúng
  add_bars(y = ~I, name = "Cases", marker = list(color = "#87A2FF")) %>%
  add_trace(y = ~mva_14d, name = "Moving Average", mode = 'lines', line = list(color = "#E78F81")) %>%
  layout(yaxis = list(title = "Incidence"), plot_bgcolor = 'white')

# 2. Biểu đồ Rt
p_rt_1819 <- plot_rt(df_rt_1819)

# 3. Ghép 2 biểu đồ
subplot(p_hist_1819, p_rt_1819, nrows = 2, shareX = T, titleY = TRUE)



#------------
df_1 <- df_rt_1819 %>% 
  mutate(
    tuan = week(dates),
    dot_dich = "2018-2019"
  ) %>% 
  select(tuan, dot_dich, rt)

ggplot(data = df_1, aes(x = tuan, y = rt, color = dot_dich, fill = dot_dich)) +
  
  geom_smooth(method = "loess", alpha = 0.2, linewidth = 1.2, se = TRUE, span = 0.4) +
  
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  
  scale_color_manual(values = c("2018-2019" = "#e41a1c", "2024-2025" = "#377eb8")) +
  scale_fill_manual(values = c("2018-2019" = "#e41a1c", "2024-2025" = "#377eb8")) +
  
  labs(
    title = "Hệ số lây nhiễm Rt theo thời gian",
    x = "Tuần",
    y = "Rt"
  ) +
  
  coord_cartesian(xlim = c(0,52)) + 
  theme_bw()



#--------------------------------------------------------------------
#-------- TÍNH HỆ SỐ LÂY NHIỄM THEO THỜI GIAN (2024 - 2025) --------#
#--------------------------------------------------------------------

# Đếm incidence
df_convert_2425 <- df_2425 %>% 
  rename(dates = ngay_kp_hieu_chinh) %>% 
  group_by(dates) %>% summarise(I = n())

# Fill số 0 
df_complete_2425 <- df_convert_2425 %>% 
  complete(dates = seq(min(dates), max(dates), by = "day")) %>% 
  replace_na(list(I = 0)) %>% 
  # Cửa sổ trượt 14 ngày
  mutate(mva_14d = slide_dbl(I, .f = ~mean(.x, na.rm = T), .before = 13))

# Tạo df đầu vào cho mô hình với ngày khởi phát và I
incid_data_2425 <- df_complete_2425 %>%
  select(dates, I)

#--------------------------------------------
# 2. Định nghĩa cửa sổ trượt, serial interval
#--------------------------------------------

# Định nghĩa cửa sổ thời gian trượt
t_start_2425 <- seq(2, nrow(incid_data_2425) - 13)
t_end_2425 <- t_start_2425 + 13

# Chạy mô hình estimate_R
mod_2425 <-estimate_R(
  incid = incid_data_2425,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 14.5,
    std_si = 3.25,
    t_start = t_start_2425,
    t_end = t_end_2425
  ))
)

#--------------------------------------------
# 3. Tính Rt
#--------------------------------------------

# Áp dụng vào data 2024-2025
df_rt_2425 <- process_data(mod_2425)

# Tạo ID để vẽ biểu đồ
df_rt_2425 <- df_rt_2425 %>% 
  arrange(dates) %>% 
  mutate(id = consecutive_id(pl)) %>% 
  mutate(dates = as.Date(dates))

plot_2425 <- plot_rt(df_rt_2425)
plot_2425

#--------------------------------------------------------------------
#---------------- SO SÁNH RT 2018-2019 VÀ 2024-2025 ----------------#
#--------------------------------------------------------------------

# Xác định ngày bắt đầu của mỗi đợt dịch
start_date_1819 <- min(df_rt_1819$dates)
start_date_2425 <- min(df_rt_2425$dates)

# Gộp data 2018-2019 và 2024-2025 
df_so_sanh <- bind_rows(
  df_rt_1819 %>% mutate(dot_dich = "2018-2019", ngay_dich = as.numeric(dates - start_date_1819)),
  df_rt_2425 %>% mutate(dot_dich = "2024-2025", ngay_dich = as.numeric(dates - start_date_2425))
)

# Biểu đồ so sánh Rt
ggplot(df_so_sanh, aes(x = ngay_dich, group = dot_dich)) +
  geom_ribbon(aes(ymin = q1_rt, ymax = q3_rt, fill = dot_dich), alpha = 0.3) +
  geom_line(aes(y = rt, color = dot_dich), linewidth = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "So sánh tốc độ bùng phát của 2 đợt dịch", x = "Dates", y = "Rt") +
  theme_bw()
