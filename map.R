library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(viridis)
library(ggspatial)
library(ggsci)

#============================
# 1. CASES: PROGRESSION OVER TIME (PHASES)
#============================
df_1819 <- readRDS("df_1819.rds") %>% mutate(outbreak = "Outbreak 2018-2020")
df_2425 <- readRDS("df_2425.rds") %>% mutate(outbreak = "Outbreak 2024-2025")

df_combined <- bind_rows(df_1819, df_2425)
num_phases <- 6 

# TỰ ĐỘNG TÍNH THỜI GIAN ĐỂ LÀM CHÚ THÍCH (CAPTION)
phase_info <- df_combined %>%
  group_by(outbreak) %>%
  summarise(
    start_date = min(ngay_nv),
    end_date = max(ngay_nv),
    total_days = as.numeric(difftime(max(ngay_nv), min(ngay_nv), units = "days")),
    days_per_phase = round(total_days / num_phases),
    .groups = "drop"
  ) %>%
  mutate(
    # Dùng month.abb để ép R luôn xuất ra Jan, Feb, Mar... bằng tiếng Anh
    start_str = paste(month.abb[month(start_date)], year(start_date)),
    end_str = paste(month.abb[month(end_date)], year(end_date)),
    info_text = paste0(outbreak, " (", start_str, " to ", end_str, "): ~", days_per_phase, " days/phase")
  )

# Tạo chuỗi text hoàn chỉnh cho Caption
caption_text <- paste(
  paste0("Note: Each outbreak period is divided into ", num_phases, " equal phases."),
  paste(phase_info$info_text, collapse = "\n"),
  sep = "\n"
)

# Gán Phase cho dữ liệu ca bệnh
cases_agg <- df_combined %>%
  group_by(outbreak) %>%
  mutate(
    period = cut(ngay_nv, breaks = num_phases, labels = paste("Phase", 1:num_phases))
  ) %>%
  ungroup() %>%
  group_by(outbreak, quan_huyen, period) %>%
  summarise(new_cases = n(), .groups = "drop") %>%
  complete(quan_huyen, nesting(outbreak, period), fill = list(new_cases = 0)) %>%
  arrange(outbreak, quan_huyen, period) %>%
  group_by(outbreak, quan_huyen) %>%
  mutate(cum_cases = cumsum(new_cases)) %>%
  ungroup()

#============================
# 2. POPULATION
#============================
pop <- readxl::read_excel("pop.xlsx") %>%
  mutate(across(starts_with("pop_"), as.numeric)) %>%
  select(quan_huyen, pop_2019, pop_2024)

#============================
# 3. MAP
#============================
map_hcm <- st_read("gadm41_VNM.gpkg", layer = "ADM_ADM_2") %>%
  filter(NAME_1 == "Hồ Chí Minh") %>%
  mutate(NAME_2 = case_when(
    NAME_2 %in% c("Thủ Đức", "Quận 9", "Quận 2") ~ "Thành phố Thủ Đức",
    TRUE ~ NAME_2
  )) %>%
  group_by(NAME_2) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  rename(quan_huyen = NAME_2)

#============================
# 4. JOIN DATA & CALCULATE RATES
#============================
cases_full <- cases_agg %>%
  left_join(pop, by = "quan_huyen") %>%
  mutate(
    pop_used = ifelse(outbreak == "Outbreak 2018-2019", pop_2019, pop_2024),
    incidence_rate = (cum_cases / pop_used) * 100000
  )

map_cases_sf <- map_hcm %>%
  right_join(cases_full, by = "quan_huyen", relationship = "many-to-many") %>%
  mutate(
    fill_var1 = ifelse(incidence_rate == 0, NA, incidence_rate)
  )

#============================
# 5. PLOTTING
#============================
p <- ggplot(map_cases_sf) +
  geom_sf(aes(fill = fill_var1), color = "black", size = 0.25) +
  scale_fill_gsea(
    name = "Cumulative incidence\n(per 100,000 population)",
    na.value = "white",
    limits = c(0, max(map_cases_sf$fill_var1, na.rm = TRUE))
  ) +
  facet_grid(outbreak ~ period) +
  
  # THÊM CAPTION VÀO ĐÂY
  labs(caption = caption_text) +
  
  theme_minimal(base_size = 15) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    # Căn chỉnh Caption cho đẹp
    plot.caption = element_text(hjust = 0, size = 12, face = "italic", color = "grey30", margin = margin(t = 15))
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 20, barheight = 0.8,
      ticks.colour = "black",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0.5
    )
  )

ggsave(
  filename = "Figure 3.jpeg",
  plot = p,
  width = 16, height = 8.5, 
  dpi = 500, bg = "white"
)
