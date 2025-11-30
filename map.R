library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(viridis)
library(ggspatial)

#============================
# 1️⃣ DỮ LIỆU CA BỆNH
#============================
df_ob1 <- df %>% 
  filter(dates >= "2018-08-27" & dates <= "2020-03-31") %>% 
  group_by(quan_huyen, dates) %>% 
  summarise(I = n(), .groups = "drop") %>% 
  group_by(quan_huyen) %>% 
  complete(dates = seq(min(dates), max(dates), by = "day")) %>%
  replace_na(list(I = 0)) %>% 
  ungroup()

cases_month <- df_ob1 %>%
  mutate(month = floor_date(dates, "month")) %>%
  group_by(quan_huyen, month) %>%
  summarise(cases = sum(I, na.rm = TRUE), .groups = "drop") %>%
  complete(quan_huyen, month, fill = list(cases = 0)) %>%
  arrange(quan_huyen, month) %>%   
  group_by(quan_huyen) %>%
  mutate(
    cum_cases = cumsum(cases),  
    month_lab = format(month, "%b-%Y"),
    month_lab = factor(month_lab, levels = unique(month_lab))
  ) %>%
  ungroup()

#============================
# 2️⃣ DÂN SỐ
#============================
pop <- readxl::read_excel("pop.xlsx") %>%
  mutate(across(starts_with("pop_"), as.numeric))

pop <- pop %>% select(quan_huyen, pop_2019) %>% rename(pop = pop_2019)

#============================
# 3️⃣ BẢN ĐỒ
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
# 4️⃣ GỘP DỮ LIỆU
#============================
map_cases_sf <- map_hcm %>%
  left_join(cases_month, by = "quan_huyen") %>% 
  left_join(pop, by = "quan_huyen") %>%
  mutate(incidence_rate = (cum_cases / pop) * 100000)

#============================
# 5️⃣ VẼ BẢN ĐỒ
#============================
map_cases_sf$fill_var <- ifelse(map_cases_sf$cum_cases == 0, NA, map_cases_sf$cum_cases)
map_cases_sf$fill_var1 <- ifelse(map_cases_sf$incidence_rate == 0, NA, map_cases_sf$incidence_rate)

p <- ggplot(map_cases_sf) +
  geom_sf(aes(fill = fill_var1), color = "black", size = 0.25) +
  scale_fill_gradientn(
    colours = c("lightblue", "purple", "red"),
    limits = c(min(map_cases_sf$fill_var1, na.rm = TRUE),
               max(map_cases_sf$fill_var1, na.rm = TRUE)),
    breaks = seq(0, 120, 30),
    name = expression("Cummulative incidence rate (per 100.0000 population)"),
    na.value = "white"
  ) +
  facet_wrap(~ month_lab, ncol = 7) +
  theme_minimal(base_size = 17) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 10, barheight = 0.6,
      ticks.colour = "black",
      label.position = "bottom",
      title.position = "top",
      title.hjust = 0.5
    )
  )

ggsave(
  filename = "map_oub1.jpeg",
  plot = p,
  width = 9, height = 7,
  dpi = 500, bg = "white"
)
