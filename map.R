# --- UTF-8 ---

# Разработано в июне 2025 — январе 2026 года А. Коротковым (МГУ) и Н. Синицыным (МГУ)

# при копировании любой части материалов ссылка обязательна

# выполняйте код последовательно

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ ШАГ 3 КАРТЫ --------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ---------------------------- ОБЩИЕ УСТАНОВКИ ------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# библиотеки
library(sf)
library(rmapshaper)

setwd("D:/Data/wave")



# читаем файлы с избыточной смертностью по предлагаемой методике с учётом пиков
# данные по холоду
tab_cold <- read.xlsx("tab_svod_region_cold_peak_95.xlsx")
tab_cold <- tab_cold %>% filter(!Region_name_ROSBKS %in% c("Чеченская Республика", "Ингушская республика", "Республика Дагестан"))

tab_hot <- read.xlsx("tab_svod_region_hot_peak_95.xlsx")
tab_hot <- tab_hot %>% filter(!Region_name_ROSBKS %in% c("Чеченская Республика", "Ингушская республика", "Республика Дагестан"))



# читаем основу карты
osnov <- st_read("rus_osnova/rus_lvl2.shp")
osnov <- ms_simplify(osnov, keep = 0.005)

osnov$NAME <- str_replace_all(osnov$NAME, pattern = c("Кемеровская область" = "Кемеровская область — Кузбасс", "город федерального значения Санкт-Петербург" = "город Санкт-Петербург", "город федерального значения Москва" = "город Москва", "республика" = "Республика", "Республика Чувашия" = "Чувашская Республика"))



# присоединяем данные к основе
cold_osnov <- left_join(x = osnov, y = tab_cold, by = c("NAME" = "Region_name_ROSBKS"))
hot_osnov <- left_join(x = osnov, y = tab_hot, by = c("NAME" = "Region_name_ROSBKS"))



# ----- РИСУЕМ ОТДЕЛЬНО ТОЛЬКО ДАННЫЕ ПО ПИКАМ -----


# КАРТА УЩЕРБА ОТ ЖАРЫ ПРЕДЛАГАЕМАЯ МЕТОДИКА

hot <-
  
  ggplot() + 
  
  geom_sf(data = hot_osnov, mapping = aes(fill = hot_osnov$excess_pr), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "red"), name = "доля смертей\nот волн жары, %\nот всех смертей",  na.value = "khaki") +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# КАРТА УЩЕРБА ОТ ХОЛОДА ПРЕДЛАГАЕМАЯ МЕТОДИКА

cold <-
  
  ggplot() + 
  
  geom_sf(data = cold_osnov, mapping = aes(fill = cold_osnov$excess_pr), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "deepskyblue"), name = "доля смертей\nот волн холода, %\nот всех смертей",  na.value = "khaki") + 
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- расчёт общего ущерба от жары и холода -----

obsh_osnov <- left_join(x = osnov, y = tab_cold, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov <- obsh_osnov %>% rename(cold_excess_hum = excess_hum, cold_sum_deads = sum_deads, cold_excess_pr = excess_pr)

obsh_osnov <- left_join(x = obsh_osnov, y = tab_hot, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov <- obsh_osnov %>% rename(hot_excess_hum = excess_hum, hot_sum_deads = sum_deads, hot_excess_pr = excess_pr)



# выбираем переобладающий ущерб от жары или от холода
obsh_osnov <- obsh_osnov %>% mutate(preobl = case_when(cold_excess_hum >= hot_excess_hum ~ "cold", cold_excess_hum < hot_excess_hum ~ "hot"))



# ----- КАРТА ПРЕОБЛАДАЮЩЕГО ТИПА ВОЛНЫ ПРЕДЛАГАЕМАЯ МЕТОДИКА -----

pre <-
  ggplot() +
  
  geom_sf(data = obsh_osnov, mapping = aes(fill = obsh_osnov$preobl)) + 
  scale_fill_manual(values = c("salmon", "skyblue"), breaks = c("hot", "cold"), labels = c("от волн жары", "от волн холода"), name = "преобладающий\nвклад",  na.value = "khaki") + 
  theme_void() +
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))

obsh_osnov$sum_percent = obsh_osnov$cold_excess_pr + obsh_osnov$hot_excess_pr



# ----- КАРТА ОБЩЕГО УЩЕРБА ОТ ЖАРЫ И ХОЛОДА -----

obsh <-
  
  ggplot() + 
  
  geom_sf(data = obsh_osnov, mapping = aes(fill = obsh_osnov$sum_percent), lwd = 0.001, color = "grey30") +
  
  scale_fill_gradientn(colours =  c("white", "grey5"), name = "доля смертей\nот волн жары\nи холода вместе, %\nот всех смертей", na.value = "khaki") +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- РИСУЕМ ВСЕ 4 КАРТЫ ПРЕДЛАГАЕМАЯ МЕТОДИКА -----

pick <- 
  plot_grid(hot, cold, pre, obsh, ncol = 1)

pick



# РИСУЕМ СРАВНЕНИЕ ПИКОВ РИСКОВ И ИХ РАЗНОСТЬ

# РИСУЕМ ПИКИ, НО С ЛЕГЕНДОЙ ОТ РИСКОВ

# ----- КАРТА УЩЕРБА ОТ ЖАРЫ ПРЕДЛАГАЕМАЯ МЕТОДИКА -----

hot <-
  
  ggplot() + 
  
  geom_sf(data = hot_osnov, mapping = aes(fill = hot_osnov$excess_pr), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "red"), name = "от волн\nжары,\n% от всех\nсмертей",  na.value = "khaki", limits = c(0, 1.4)) +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- КАРТА УЩЕРБА ОТ ХОЛОДА ПРЕДЛАГАЕМАЯ МЕТОДИКА -----

cold <-
  
  ggplot() + 
  
  geom_sf(data = cold_osnov, mapping = aes(fill = cold_osnov$excess_pr), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "deepskyblue"), name = "от волн\nхолода,\n% от всех\nсмертей",  na.value = "khaki", limits = c(0, 1.4)) + 
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))


# ----- расчёт общего ущерба от жары и холода -----

obsh_osnov <- left_join(x = osnov, y = tab_cold, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov <- obsh_osnov %>% rename(cold_excess_hum = excess_hum, cold_sum_deads = sum_deads, cold_excess_pr = excess_pr)

obsh_osnov <- left_join(x = obsh_osnov, y = tab_hot, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov <- obsh_osnov %>% rename(hot_excess_hum = excess_hum, hot_sum_deads = sum_deads, hot_excess_pr = excess_pr)

obsh_osnov$sum_percent = obsh_osnov$cold_excess_pr + obsh_osnov$hot_excess_pr



# ----- КАРТА ОБЩЕГО УЩЕРБА ОТ ЖАРЫ И ХОЛОДА -----

obsh <-
  
  ggplot() + 
  
  geom_sf(data = obsh_osnov, mapping = aes(fill = obsh_osnov$sum_percent), lwd = 0.001, color = "grey30") +
  
  scale_fill_gradientn(colours =  c("white", "grey5"), name = "от всех\nволн,\n% от всех\nсмертей", na.value = "khaki", limits = c(0, 2.2)) +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- РИСУЕМ ВСЕ 3 КАРТЫ ПРЕДЛАГАЕМАЯ МЕТОДИКА С ЛЕГЕНДОЙ ОТ РИСКОВ -----

pick <- 
  plot_grid(hot, cold, obsh, ncol = 1)



# ----- читаем файлы с избыточной смертностью по методике гаспаррини -----

tab_cold_risk <- read.xlsx("tab_svod_region_cold_95_risk.xlsx")
tab_cold_risk <- tab_cold_risk %>% filter(!Region_name_ROSBKS %in% c("Чеченская Республика", "Ингушская республика", "Республика Дагестан"))

tab_hot_risk <- read.xlsx("tab_svod_region_hot_95_risk.xlsx")
tab_hot_risk <- tab_hot_risk %>% filter(!Region_name_ROSBKS %in% c("Чеченская Республика", "Ингушская республика", "Республика Дагестан"))



# читаем основку
osnov <- st_read("rus_osnova/rus_lvl2.shp")
osnov <- ms_simplify(osnov, keep = 0.005)

osnov$NAME <- str_replace_all(osnov$NAME, pattern = c("Кемеровская область" = "Кемеровская область — Кузбасс", "город федерального значения Санкт-Петербург" = "город Санкт-Петербург", "город федерального значения Москва" = "город Москва", "республика" = "Республика", "Республика Чувашия" = "Чувашская Республика"))



cold_risk_osnov <- left_join(x = osnov, y = tab_cold_risk, by = c("NAME" = "Region_name_ROSBKS"))
hot_risk_osnov <- left_join(x = osnov, y = tab_hot_risk, by = c("NAME" = "Region_name_ROSBKS"))



# ----- КАРТА УЩЕРБА ОТ ЖАРЫ МЕТОДИКА ГАСПАРРИНИ -----

hot_risk <-
  
  ggplot() + 
  
  geom_sf(data = hot_risk_osnov, mapping = aes(fill = hot_risk_osnov$excess_pr_risk), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "red"), name = "от волн\nжары,\n% от всех\nсмертей",  na.value = "khaki", limits = c(0, 1.4)) +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- КАРТА УЩЕРБА ОТ ХОЛОДА МЕТОДИКА ГАСПАРРИНИ -----

cold_risk <-
  
  ggplot() + 
  
  geom_sf(data = cold_risk_osnov, mapping = aes(fill = cold_risk_osnov$excess_pr_risk), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradientn(colours =  c("white", "deepskyblue"), name = "от волн\nхолода,\n% от всех\nсмертей",  na.value = "khaki", limits = c(0, 1.4)) + 
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- СЧИТАЕМ ОБЩИЙ УЩЕРБ ОТ ХОЛОДА И ЖАРЫ ПО МЕТОДИКЕ ГАСПАРРИНИ -----

obsh_osnov_risk <- left_join(x = osnov, y = tab_cold_risk, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov_risk <- obsh_osnov_risk %>% rename(cold_excess_hum_risk = excess_hum_risk, cold_excess_pr_risk = excess_pr_risk)

obsh_osnov_risk <- left_join(x = obsh_osnov_risk, y = tab_hot_risk, by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov_risk <- obsh_osnov_risk %>% rename(hot_excess_hum_risk = excess_hum_risk, hot_excess_pr_risk = excess_pr_risk)

obsh_osnov_risk$sum_percent = obsh_osnov_risk$cold_excess_pr_risk + obsh_osnov_risk$hot_excess_pr_risk



# ----- КАРТА ОБЩЕГО УЩЕРБА ПО МЕТОДИКЕ ГАСПАРРИНИ -----

obsh_risk <-
  
  ggplot() + 
  
  geom_sf(data = obsh_osnov_risk, mapping = aes(fill = obsh_osnov_risk$sum_percent), lwd = 0.001, color = "grey30") +
  
  scale_fill_gradientn(colours =  c("white", "grey5"), name = "от всех\nволн,\n% от всех\nсмертей", na.value = "khaki", limits = c(0, 2.2)) +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- РИСУЕМ ВСЕ 3 КАРТЫ РИСКОВ -----
risk <- 
  plot_grid(hot_risk, cold_risk, obsh_risk, ncol = 1)



# ----- СЧИТАЕМ РАЗНОСТЬ ПРЕДЛАГАЕМОЙ И РИСКОВ ХОЛОД -----
tab_cold_raz <- left_join(x = tab_cold %>% select(Region_name_ROSBKS, excess_hum, excess_pr),
                          y = tab_cold_risk %>% select(Region_name_ROSBKS, excess_hum_risk, excess_pr_risk), by = c("Region_name_ROSBKS"))

tab_cold_raz$excess_pr_raz <- tab_cold_raz$excess_pr_risk - tab_cold_raz$excess_pr



# СЧИТАЕМ РАЗНОСТЬ ПРЕДЛАГАЕМОЙ И РИСКОВ ЖАРА
tab_hot_raz <- left_join(x = tab_hot %>% select(Region_name_ROSBKS, excess_hum, excess_pr),
                         y = tab_hot_risk %>% select(Region_name_ROSBKS, excess_hum_risk, excess_pr_risk), by = c("Region_name_ROSBKS"))

tab_hot_raz$excess_pr_raz <- tab_hot_raz$excess_pr_risk - tab_hot_raz$excess_pr

# ПРИСОЕДНИЯЕМ РАЗНОСТЬ К ОСНОВЕ
cold_osnov_raz <- left_join(x = osnov, y = tab_cold_raz, by = c("NAME" = "Region_name_ROSBKS"))
hot_osnov_raz <- left_join(x = osnov, y = tab_hot_raz, by = c("NAME" = "Region_name_ROSBKS"))



# РАЗНОСТЬ ЖАРА
hot_raz <-
  
  ggplot() + 
  
  geom_sf(data = hot_osnov_raz, mapping = aes(fill = hot_osnov_raz$excess_pr_raz), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradient2(low = "green", mid = "white", high = "darkred", midpoint = 0, limits = c(-0.3, 1.5), name = "разница\nоценок\nдля волн\nхолода, %",  na.value = "khaki") +

  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))

# РАЗНОСТЬ ХОЛОД  
cold_raz <-
  
  ggplot() + 
  
  geom_sf(data = cold_osnov_raz, mapping = aes(fill = cold_osnov_raz$excess_pr_raz), lwd = 0.001, color = "grey30") + 
  
  scale_fill_gradient2(low = "green", mid = "white", high = "darkred", midpoint = 0,  limits = c(-0.3, 1.5), name = "разница\nоценок\nдля волн\nжары, %",  na.value = "khaki") + 
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))



# ----- СЧИТАЕМ ОБЩУЮ РАЗНОСТЬ -----

obsh_osnov_raz <- left_join(x = osnov, y = tab_cold_raz %>% select(Region_name_ROSBKS, excess_pr_raz), by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov_raz <- obsh_osnov_raz %>% rename(cold_excess_pr_raz = excess_pr_raz)

obsh_osnov_raz <- left_join(x = obsh_osnov_raz, y = tab_hot_raz %>% select(Region_name_ROSBKS, excess_pr_raz), by = c("NAME" = "Region_name_ROSBKS"))
obsh_osnov_raz <- obsh_osnov_raz %>% rename(hot_excess_pr_raz = excess_pr_raz)

obsh_osnov_raz$sum_percent_raz = obsh_osnov_raz$cold_excess_pr_raz + obsh_osnov_raz$hot_excess_pr_raz



# РАЗНОСТЬ ОБЩАЯ
obsh_raz <-
  
  ggplot() + 
  
  geom_sf(data = obsh_osnov_raz, mapping = aes(fill = obsh_osnov_raz$sum_percent_raz), lwd = 0, color = "grey30") +
  
  scale_fill_gradient2(low = "green", mid = "white", high = "darkred",  limits = c(-0.3, 1.5), name = "разница\nоценок\nдля всех\nволн, %", na.value = "khaki") +
  
  theme_void() +
  
  guides(fill = guide_colorbar(
    ticks = TRUE,             # Show ticks (tips) on legend
    ticks.linewidth = 0.1,
    ticks.colour = "black",      # Thickness of ticks
    frame.colour = "black",   # Black border around legend bar
    frame.linewidth = 0.1       # Thickness of border line
  )) +
  
  theme(legend.text = element_text(size = 8), title = element_text(size = 8))

# РИСУЕМ 3 КАРТЫ РАЗНОСТЬ
raz <- 
    plot_grid(hot_raz, cold_raz, obsh_raz, ncol = 1)


# РИСУЕМ ПРЕДЛАГАЕМУЮ КАРТУ ПО РИСКАМ + РАЗНОСТЬ  
plot_grid(pick, risk, raz,  nrow = 1)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------- ДАЛЕЕ ПЕРЕХОДИТЕ К ШАГУ 4 -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #