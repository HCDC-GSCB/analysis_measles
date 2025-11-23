library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(viridis)
library(ggspatial)

#============================
# 1️⃣ DỮ LIỆU CA BỆNH
#============================
df_ob2 <- df %>% 
  filter(dates >= "2024-05-28") %>% 
  group_by(quan_huyen, dates) %>% 
  summarise(I = n(), .groups = "drop") %>% 
  group_by(quan_huyen) %>% 
  complete(dates = seq(min(dates), max(dates), by = "day")) %>%
  replace_na(list(I = 0)) %>% 
  ungroup()

cases_month <- df_ob2 %>%
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

pop <- pop %>% select(quan_huyen, pop_2024) %>% rename(pop = pop_2024)

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
p <- ggplot(map_cases_sf) +
  geom_sf(aes(fill = incidence_rate), color = "black", size = 0.25) +
  scale_fill_gradientn(
    colours = c("white", "#0000FF", "#FF0000"),
    limits = c(0, max(map_cases_sf$incidence_rate, na.rm = TRUE)),
    name = expression("Incidence rate (per 100.0000 population)"),
    na.value = "white"
  ) +
  facet_wrap(~ month_lab, ncol = 7) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
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
  width = 11, height = 6,
  dpi = 1000, bg = "white"
)
