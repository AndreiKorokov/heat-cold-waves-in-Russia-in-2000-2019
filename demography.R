# --- UTF-8 ---



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# -------------- СНАЧАЛА ВЫПОЛНЯЮТСЯ ФАЙЛЫ meteo И geoinf--------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# библиотеки, уже запущенные
library(openxlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(rmapshaper)

# дома
# setwd("F:/Аспирантура/Исследование/R_processing")

# на работе
setwd("D:/Аспирантура/Исследование/R_processing")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------- ЗАГРУЗКА ДАННЫХ РОСБКС ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# записываем в таблицу адреса (муниципалитет, область, код ОКТМО) метеостанций
tab_station_mun <- st_drop_geometry(station_coords_join)

# список регионов из таблицы
tab_regions <- unique(tab_station_mun$region_name) %>% as.data.frame()

# переименование названия столбца для целей увязывания
names(tab_regions)[names(tab_regions) == '.'] <- 'region_name'

# # сохраняем таблицу. Вне R вручную сопоставляем названия регионов с РосБКC
# write.xlsx(tab_regions, "tab_regions.xlsx")

# загружаем таблицу-матрицу регионов
tab_region_matrix <- read.xlsx("region_matrix.xlsx")

# связываем таблицы: матрицу и список регионов с метеостанциями
tab_station_ROSBKS <- left_join(x = tab_regions, y = tab_region_matrix, by = "region_name")

# еще одно связывание таблиц: матрицу и данные по метеостанциям
tab_station_ROSBKS <- left_join(x = tab_station_ROSBKS, y = tab_station_mun, by = "region_name")

# результирующее объединение в одну таблицу (жара)
tab_waves_hot_raw <- left_join(x = output_waves_hot, 
                               y = tab_station_ROSBKS %>%
                                 select(Num, ROSBKS_code, FTS_code, region_name, Region_name_ROSBKS, Index, municipal_district_name, territory_id, osm_ref, oktmo), 
                               by = "Index")

# результирующее объединение в одну таблицу (холод)
tab_waves_cold_raw <- left_join(x = output_waves_cold, 
                                y = tab_station_ROSBKS %>%
                                  select(Num, ROSBKS_code, FTS_code, region_name, Region_name_ROSBKS, Index, municipal_district_name, territory_id, osm_ref, oktmo), 
                                by = "Index")

# удаляем лишний пробел в конце строк с названием регионов
tab_waves_hot_raw$Region_name_ROSBKS <- str_trim(tab_waves_hot_raw$Region_name_ROSBKS)
tab_waves_cold_raw$Region_name_ROSBKS <- str_trim(tab_waves_cold_raw$Region_name_ROSBKS)

# загружаем данные РосБКС
tab_rosbks_raw <- read.csv("rosbks_data_raw.csv")

# загружаем файл с назаниями регионов для РБКС
tab_rosbks_names <- read.xlsx("rosbks_names.xlsx")

# удаляем лишний пробел в конце строк
tab_rosbks_names$Region <- str_trim(tab_rosbks_names$Region)

# сшиваем данные РосБКС и названия регионов
tab_rosbks <- left_join(x = tab_rosbks_raw, 
                        y = tab_rosbks_names,
                        by = "PopCode")

# фильтруем оба пола и даты до конца 2019 года
tab_rosbks <- tab_rosbks %>% filter(Sex == "b") %>% filter(Year < 2020)

# переводим "год" в табицу в текстовый вид 
tab_rosbks$Year <- as.character(tab_rosbks$Year)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------- СРЕДНЕГОДОВАЯ ЧИСЛЕННОСТЬ НАСЕЛЕНИЯ -------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# загружаем таблицу со среднегодовыми численностями населения (ЕМИСС)
tab_region_population <- read_xls("region_average_population.xls")

# убираем ненужные знаки перед названиями регионов
tab_region_population$Region <- str_remove(string = tab_region_population$Region, pattern = "\\d{11} ")

# фильтруем "все население"
tab_region_population <- tab_region_population %>% filter(Type == "w2:p_mest:11 все население")

# удаляем столбец Type
tab_region_population$Type <- NULL

# write.xlsx(tab_region_population, "tab_region_population.xlsx")


# приводим таблицу с населением в длинный вид
tab_population_long <- pivot_longer(data = tab_region_population, names_to = "Year", values_to = "population", cols = !"Region")

# замены несовпадающих написаний названий регионов
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Республика Ингушетия", replacement = "Ингушская республика") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Кемеровская область - Кузбасс", replacement = "Кемеровская область") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Город Санкт-Петербург город федерального значения", replacement = "Санкт-Петербург") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Город Москва столица Российской Федерации город федерального значения", replacement = "Москва") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Город федерального значения Севастополь", replacement = "Севастополь") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Республика Адыгея \\(Адыгея\\)", replacement = "Республика Адыгея") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Республика Северная Осетия-Алания", replacement = "Республика Северная Осетия") 
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Республика Татарстан \\(Татарстан\\)", replacement = "Республика Татарстан")
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Чеченская Республика", replacement = "Чеченская республика")
tab_population_long$Region <- str_replace(tab_population_long$Region, pattern = "Чувашская Республика - Чувашия", replacement = "Чувашская Республика") 

# убираем неиспользуемую строку
tab_rosbks <- tab_rosbks %>% filter(Region != "Россия в целом")

# сшиваем данные РосБКС и среднегодовые численности населения
tab_cdr <- left_join(x = tab_rosbks, 
                     y = tab_population_long,
                     by = c("Region", "Year"))

# переводим годы в числовой вид
tab_cdr$Year <- as.numeric(tab_cdr$Year)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------- ПОНЕДЕЛЬНОЕ ЧИСЛО УМЕРШИХ ИЗ ОКС ----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# определяем число умерших в неделю из ОКС по формуле РосБКС из статьи в PopEcon
tab_cdr$deads <- ifelse(tab_cdr$Year %% 4 == 0, round((tab_cdr$population / 366 * 7) * tab_cdr$CDR, digits = 0),
                        round((tab_cdr$population / 365 * 7) * tab_cdr$CDR, digits = 0))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ БОЛЬШОЙ ЦИКЛ -------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# цикл, который подсчитывает число умерших за время прохождения волн жары или холода



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ БЛОК ЖАРА ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# создаем вектора для записи результатов подсчета смертей
list_hot_deads <- c()
list_hot_deads_pred_week <- c()
list_hot_deads_pred_year1 <- c()
list_hot_deads_pred_year2 <- c()
list_hot_deads_pred_year3 <- c()

# создаем вектора с проверками предыдущих периодов на волны жары
list_proverka_pred_week <- c()
list_proverka_pred_year1 <- c()
list_proverka_pred_year2 <- c()
list_proverka_pred_year3 <- c()



# ЦИКЛ
for (x in c(1:nrow(tab_waves_hot_raw))) {
  
  # год и неделя начала события
  y1 <- tab_waves_hot_raw$Year_RosBKS_start[x]
  w1 <- tab_waves_hot_raw$Week_RosBKS_start[x]
  
  # год и неделя конца события
  y2 <- tab_waves_hot_raw$Year_RosBKS_last[x]
  w2 <- tab_waves_hot_raw$Week_RosBKS_last[x]
  
  # регион
  reg <- tab_waves_hot_raw$region_name[x]
  
  # номер метеостанции
  index <-  tab_waves_hot_raw$Index[x]
  
  # если год начала волны и год конца — один и тот же (для жары всегда так)
  if (y1 == y2) {
    
    # число недель с жарой
    t <- w2 - w1
    
    # число смертей в период волны жары
    tab_deads <- tab_cdr %>% filter(Region == reg, Year == y1, Week %in% c(w1 : (w1 + t)))
    
    deads_count <- sum(tab_deads$deads, na.rm = T)
    
    # число смертей в предыдущие недели того же года
    tab_deads_pred_week <- tab_cdr %>% filter(Region == reg, Year == y1, Week %in% c((w1 - t - 1) : (w1 - 1)))
    
    deads_count_pred_week <- sum(tab_deads_pred_week$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну жары". Если она была, то сравнивать с ней не имеет смысла
    zhara_proverka_pred_week <- tab_waves_hot_raw %>% filter(region_name == reg,
                                                             Index == index,
                                                             Year_RosBKS_start == y1,
                                                             Week_RosBKS_start %in% c((w1 - t - 1) : (w1 - 1)) |
                                                               Week_RosBKS_last %in% c((w1 - t - 1) : (w1 - 1))) %>% nrow()
    
    # число смертей в тот же период, но ГОДОМ ранее
    tab_deads_pred_year <- tab_cdr %>% filter(Region == reg, Year == y1 - 1, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year <- sum(tab_deads_pred_year$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну жары". Если она была, то сравнивать с ней не имеет смысла
    zhara_proverka_pred_year1 <- tab_waves_hot_raw %>% filter(region_name == reg,
                                                              Index == index,
                                                              Year_RosBKS_start == y1 - 1)
    
    if (nrow(zhara_proverka_pred_year1) > 0) {
      
      number_week_zhara_pred <- 0
      
      for (a in c(1:nrow(zhara_proverka_pred_year1))) {
        
        week_zhara_tek <- c(w1 : w2)
        
        week_zhara_pred <- c(zhara_proverka_pred_year1$Week_RosBKS_start[a] : zhara_proverka_pred_year1$Week_RosBKS_last[a])
        
        week_sovpad <- week_zhara_tek %in% week_zhara_pred %>% sum()
        
        number_week_zhara_pred <- number_week_zhara_pred + week_sovpad
        
      }
      
    }
    
    if (nrow(zhara_proverka_pred_year1) == 0) {
      
      number_week_zhara_pred <-  NA
      
    }
    
    # число смертей в тот же период, но ДВУМЯ годами ранее
    tab_deads_pred_year2 <- tab_cdr %>% filter(Region == reg, Year == y1 - 2, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year2 <- sum(tab_deads_pred_year2$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну жары". Если она была, то сравнивать с ней не имеет смысла
    zhara_proverka_pred_year2 <- tab_waves_hot_raw %>% filter(region_name == reg,
                                                              Index == index,
                                                              Year_RosBKS_start == y1 - 2)
    
    if (nrow(zhara_proverka_pred_year2) > 0) {
      
      number_week_zhara_pred2 <- 0
      
      for (a in c(1:nrow(zhara_proverka_pred_year2))) {
        
        week_zhara_tek <- c(w1 : w2)
        
        week_zhara_pred2 <- c(zhara_proverka_pred_year2$Week_RosBKS_start[a] : zhara_proverka_pred_year2$Week_RosBKS_last[a])
        
        week_sovpad2 <- week_zhara_tek %in% week_zhara_pred2 %>% sum()
        
        number_week_zhara_pred2 <- number_week_zhara_pred2 + week_sovpad2
        
      }
      
    }
    
    if (nrow(zhara_proverka_pred_year2) == 0) {
      
      number_week_zhara_pred2 <-  NA
      
    }
    
    
    # число смертей в тот же период, но ТРЕМЯ годами ранее
    tab_deads_pred_year3 <- tab_cdr %>% filter(Region == reg, Year == y1 - 3, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year3 <- sum(tab_deads_pred_year3$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну жары". Если она была, то сравнивать с ней не имеет смысла
    zhara_proverka_pred_year3 <- tab_waves_hot_raw %>% filter(region_name == reg,
                                                              Index == index,
                                                              Year_RosBKS_start == y1 - 3)
    
    if (nrow(zhara_proverka_pred_year3) > 0) {
      
      number_week_zhara_pred3 <- 0
      
      for (a in c(1:nrow(zhara_proverka_pred_year3))) {
        
        week_zhara_tek <- c(w1 : w2)
        
        week_zhara_pred3 <- c(zhara_proverka_pred_year3$Week_RosBKS_start[a] : zhara_proverka_pred_year3$Week_RosBKS_last[a])
        
        week_sovpad3 <- week_zhara_tek %in% week_zhara_pred3 %>% sum()
        
        number_week_zhara_pred3 <- number_week_zhara_pred3 + week_sovpad3
        
      }
      
    }
    
    if (nrow(zhara_proverka_pred_year3) == 0) {
      
      number_week_zhara_pred3 <-  NA
      
    }
    
    
  }
  
  # записываем результаты расчета
  list_hot_deads[x] <- deads_count
  
  list_hot_deads_pred_week[x] <- deads_count_pred_week
  list_hot_deads_pred_year1[x] <- deads_count_pred_year
  list_hot_deads_pred_year2[x] <- deads_count_pred_year2
  list_hot_deads_pred_year3[x] <- deads_count_pred_year3
  
  list_proverka_pred_week[x] <- zhara_proverka_pred_week
  list_proverka_pred_year1[x] <- number_week_zhara_pred
  list_proverka_pred_year2[x] <- number_week_zhara_pred2
  list_proverka_pred_year3[x] <- number_week_zhara_pred3
  
  
  # счетчик показывает на каком этапе находится процесс. Отображается каждый тысячный проход цикла из всех запланированных
  if (x %% 1000 == 0) {
    
    print(paste("Processing completed for", x, "from", nrow(tab_waves_hot_raw), "heat waves"))
    
  }
  
}

# сохранем таблицу по числу жертв в каждой волне жары
tab_waves_hot_raw$deads_count <- list_hot_deads

# записываем число умерших в каждый из периодов
tab_waves_hot_raw$hot_deads_pred_week <- list_hot_deads_pred_week
tab_waves_hot_raw$hot_deads_pred_year1 <- list_hot_deads_pred_year1
tab_waves_hot_raw$hot_deads_pred_year2 <- list_hot_deads_pred_year2
tab_waves_hot_raw$hot_deads_pred_year3 <- list_hot_deads_pred_year3

# записываем проверочное число волн жары в каждого из периодов
tab_waves_hot_raw$proverka_pred_week <- list_proverka_pred_week
tab_waves_hot_raw$proverka_pred_year1 <- list_proverka_pred_year1
tab_waves_hot_raw$proverka_pred_year2 <- list_proverka_pred_year2
tab_waves_hot_raw$proverka_pred_year3 <- list_proverka_pred_year3

# записываем результаты в результирующую таблицу
tab_waves_hot_raw <- tab_waves_hot_raw %>% 
  mutate(deads_pred = case_when(
    proverka_pred_week == 0 ~ hot_deads_pred_week,
    proverka_pred_week != 0 & proverka_pred_year1 == 0 ~ hot_deads_pred_year1,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 == 0 ~ hot_deads_pred_year2,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 != 0 & proverka_pred_year3 == 0 ~ hot_deads_pred_year3,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 != 0 & proverka_pred_year3 != 0 ~ NA))



# приросты умерших в волну жары в людях
tab_waves_hot_raw$excessive <- tab_waves_hot_raw$deads_count - tab_waves_hot_raw$deads_pred

# приросты умерших в волну жары в процентах
tab_waves_hot_raw$excessiv_pr <- round(x = tab_waves_hot_raw$deads_count / tab_waves_hot_raw$deads_pred * 100 - 100, digits = 1)

# сохраняем таблицу
write.xlsx(x = tab_waves_hot_raw, file = "hot_waves_90.xlsx")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------- БЛОК ХОЛОД ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# создаем вектора для записи результатов подсчета смертей
list_cold_deads <- c()
list_cold_deads_pred_week <- c()
list_cold_deads_pred_year1 <- c()
list_cold_deads_pred_year2 <- c()
list_cold_deads_pred_year3 <- c()

# создаем вектора с проверками предыдущих периодов на волны холода
list_proverka_pred_week <- c()
list_proverka_pred_year1 <- c()
list_proverka_pred_year2 <- c()
list_proverka_pred_year3 <- c()


# ЦИКЛ
for (x in c(1:nrow(tab_waves_cold_raw))) {

  
  # год и неделя начала события
  y1 <- tab_waves_cold_raw$Year_RosBKS_start[x]
  w1 <- tab_waves_cold_raw$Week_RosBKS_start[x]
  
  # год и неделя конца события
  y2 <- tab_waves_cold_raw$Year_RosBKS_last[x]
  w2 <- tab_waves_cold_raw$Week_RosBKS_last[x]
  
  # регион
  reg <- tab_waves_cold_raw$region_name[x]
  
  # номер метеостанции
  index <-  tab_waves_cold_raw$Index[x]
  
  # если год начала волны и год конца — один и тот же
  if (y1 == y2) {
    
    # число недель с волной холода
    t <- w2 - w1
    
    # число смертей в период волны холода
    tab_deads <- tab_cdr %>% filter(Region == reg, Year == y1, Week %in% c(w1 : (w1 + t)))
    
    deads_count <- sum(tab_deads$deads, na.rm = T)
    
    # число смертей в предыдущие недели того же года
    tab_deads_pred_week <- tab_cdr %>% filter(Region == reg, Year == y1, Week %in% c((w1 - t - 1) : (w1 - 1)))
    
    deads_count_pred_week <- sum(tab_deads_pred_week$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_week <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                              Index == index,
                                                              Year_RosBKS_start == y1,
                                                              Week_RosBKS_start %in% c((w1 - t - 1) : (w1 - 1)) |
                                                                Week_RosBKS_last %in% c((w1 - t - 1) : (w1 - 1))) %>% nrow()
    
    # число смертей в тот же период, но ГОДОМ ранее
    tab_deads_pred_year <- tab_cdr %>% filter(Region == reg, Year == y1 - 1, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year <- sum(tab_deads_pred_year$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year1 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 2)
    
    if (nrow(holod_proverka_pred_year1) > 0) {
      
      number_week_holod_pred <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year1))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred <- c(holod_proverka_pred_year1$Week_RosBKS_start[a] : holod_proverka_pred_year1$Week_RosBKS_last[a])
        
        week_sovpad <- week_holod_tek %in% week_holod_pred %>% sum()
        
        number_week_holod_pred <- number_week_holod_pred + week_sovpad
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year1) == 0) {
      
      number_week_holod_pred <-  NA
      
    }
    
    # число смертей в тот же период, но ДВУМЯ годами ранее
    tab_deads_pred_year2 <- tab_cdr %>% filter(Region == reg, Year == y1 - 2, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year2 <- sum(tab_deads_pred_year2$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year2 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 2)
    
    if (nrow(holod_proverka_pred_year2) > 0) {
      
      number_week_holod_pred2 <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year2))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred2 <- c(holod_proverka_pred_year2$Week_RosBKS_start[a] : holod_proverka_pred_year2$Week_RosBKS_last[a])
        
        week_sovpad2 <- week_holod_tek %in% week_holod_pred2 %>% sum()
        
        number_week_holod_pred2 <- number_week_holod_pred2 + week_sovpad2
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year2) == 0) {
      
      number_week_holod_pred2 <-  NA
      
    }
    
    # число смертей в тот же период, но ТРЕМЯ годами ранее
    tab_deads_pred_year3 <- tab_cdr %>% filter(Region == reg, Year == y1 - 3, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year3 <- sum(tab_deads_pred_year3$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year3 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 3)
    
    if (nrow(holod_proverka_pred_year3) > 0) {
      
      number_week_holod_pred3 <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year3))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred3 <- c(holod_proverka_pred_year3$Week_RosBKS_start[a] : holod_proverka_pred_year3$Week_RosBKS_last[a])
        
        week_sovpad3 <- week_holod_tek %in% week_holod_pred3 %>% sum()
        
        number_week_holod_pred3 <- number_week_holod_pred3 + week_sovpad3
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year3) == 0) {
      
      number_week_holod_pred3 <-  NA
      
    }
    
  }
  
  # если волна холода началась в одном году, а закончилась в другом
  if (y1 != y2) {
    
    # считаем число недель в году по ISO
    count_week <- lubridate::isoweek(sprintf("%d-12-28", y1))
    
    # число смертей в период волны холода
    # в год начала
    tab_deads_y1 <- tab_cdr %>% filter(Region == reg, Year == y1, Week %in% c(w1 : count_week))
    
    # в год окончания
    tab_deads_y2 <- tab_cdr %>% filter(Region == reg, Year == y2, Week %in% c(1 : w2))
    
    # склеиваем две таблицы и получаем число смертей за всю волну жары
    tab_deads <- rbind(tab_deads_y1, tab_deads_y2)
    
    deads_count <- sum(tab_deads$deads, na.rm = T)
    
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_week <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                              Index == index,
                                                              Year_RosBKS_start == y1,
                                                              Week_RosBKS_start %in% c((w1 - t - 1) : (w1 - 1)) |
                                                                Week_RosBKS_last %in% c((w1 - t - 1) : (w1 - 1))) %>% nrow()
    
    # число смертей в тот же период, но годом ранее
    tab_deads_pred_year <- tab_cdr %>% filter(Region == reg, Year == y1 - 1, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year <- sum(tab_deads_pred_year$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year1 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 1)
    
    if (nrow(holod_proverka_pred_year1) > 0) {
      
      number_week_holod_pred <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year1))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred <- c(holod_proverka_pred_year1$Week_RosBKS_start[a] : holod_proverka_pred_year1$Week_RosBKS_last[a])
        
        week_sovpad <- week_holod_tek %in% week_holod_pred %>% sum()
        
        number_week_holod_pred <- number_week_holod_pred + week_sovpad
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year1) == 0) {
      
      number_week_holod_pred <-  NA
      
    }
    
    # число смертей в тот же период, но ДВУМЯ годами ранее
    tab_deads_pred_year2 <- tab_cdr %>% filter(Region == reg, Year == y1 - 2, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year2 <- sum(tab_deads_pred_year2$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year2 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 2)
    
    if (nrow(holod_proverka_pred_year2) > 0) {
      
      number_week_holod_pred2 <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year2))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred2 <- c(holod_proverka_pred_year2$Week_RosBKS_start[a] : holod_proverka_pred_year2$Week_RosBKS_last[a])
        
        week_sovpad2 <- week_holod_tek %in% week_holod_pred2 %>% sum()
        
        number_week_holod_pred2 <- number_week_holod_pred2 + week_sovpad2
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year2) == 0) {
      
      number_week_holod_pred2 <-  NA
      
    }
    
    
    # число смертей в тот же период, но ТРЕМЯ годами ранее
    tab_deads_pred_year3 <- tab_cdr %>% filter(Region == reg, Year == y1 - 3, Week %in% c(w1 : (w1 + t)))
    
    deads_count_pred_year3 <- sum(tab_deads_pred_year3$deads, na.rm = T)
    
    # проверка предыдущих периодов на "волну холода". Если она была, то сравнивать с ней не имеет смысла
    holod_proverka_pred_year3 <- tab_waves_cold_raw %>% filter(region_name == reg,
                                                               Index == index,
                                                               Year_RosBKS_start == y1 - 3)
    
    if (nrow(holod_proverka_pred_year3) > 0) {
      
      number_week_holod_pred3 <- 0
      
      for (a in c(1:nrow(holod_proverka_pred_year3))) {
        
        week_holod_tek <- c(w1 : w2)
        
        week_holod_pred3 <- c(holod_proverka_pred_year3$Week_RosBKS_start[a] : holod_proverka_pred_year3$Week_RosBKS_last[a])
        
        week_sovpad3 <- week_holod_tek %in% week_holod_pred3 %>% sum()
        
        number_week_holod_pred3 <- number_week_holod_pred3 + week_sovpad3
        
      }
      
    }
    
    if (nrow(holod_proverka_pred_year3) == 0) {
      
      number_week_holod_pred3 <-  NA
      
    }
    
    
    
  }
  
  # записываем результаты расчета
  list_cold_deads[x] <- deads_count
  
  list_cold_deads_pred_week[x] <- deads_count_pred_week
  list_cold_deads_pred_year1[x] <- deads_count_pred_year
  list_cold_deads_pred_year2[x] <- deads_count_pred_year2
  list_cold_deads_pred_year3[x] <- deads_count_pred_year3
  
  list_proverka_pred_week[x] <- holod_proverka_pred_week
  list_proverka_pred_year1[x] <- number_week_holod_pred
  list_proverka_pred_year2[x] <- number_week_holod_pred2
  list_proverka_pred_year3[x] <- number_week_holod_pred3
  
  # счетчик показывает на каком этапе находится процесс. Отображается каждый тысячный проход цикла из всех запланированных
  if (x %% 1000 == 0) {
    
    print(paste("Processing completed for", x, "from", nrow(tab_waves_cold_raw), "cold waves"))
    
  }
  
}

# сохранем таблицу по числу жертв в каждой волне холода
tab_waves_cold_raw$deads_count <- list_cold_deads

# записываем число умерших в каждый из периодов
tab_waves_cold_raw$cold_deads_pred_week <- list_cold_deads_pred_week
tab_waves_cold_raw$cold_deads_pred_year1 <- list_cold_deads_pred_year1
tab_waves_cold_raw$cold_deads_pred_year2 <- list_cold_deads_pred_year2
tab_waves_cold_raw$cold_deads_pred_year3 <- list_cold_deads_pred_year3

# записываем проверочное число волн холода в каждого из периодов
tab_waves_cold_raw$proverka_pred_week <- list_proverka_pred_week
tab_waves_cold_raw$proverka_pred_year1 <- list_proverka_pred_year1
tab_waves_cold_raw$proverka_pred_year2 <- list_proverka_pred_year2
tab_waves_cold_raw$proverka_pred_year3 <- list_proverka_pred_year3

# записываем результаты в результирующую таблицу
tab_waves_cold_raw <- tab_waves_cold_raw %>% 
  mutate(deads_pred = case_when(
    proverka_pred_week == 0 ~ cold_deads_pred_week,
    proverka_pred_week != 0 & proverka_pred_year1 == 0 ~ cold_deads_pred_year1,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 == 0 ~ cold_deads_pred_year2,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 != 0 & proverka_pred_year3 == 0 ~ cold_deads_pred_year3,
    proverka_pred_week != 0 & proverka_pred_year1 != 0 & proverka_pred_year2 != 0 & proverka_pred_year3 != 0 ~ NA))

# приросты умерших в волну холода в людях
tab_waves_cold_raw$excessive <- tab_waves_cold_raw$deads_count - tab_waves_cold_raw$deads_pred

# приросты умерших в волну холода в процентах
tab_waves_cold_raw$excessiv_pr <- round(x = tab_waves_cold_raw$deads_count / tab_waves_cold_raw$deads_pred * 100 - 100, digits = 1)

# сохраняем таблицу
write.xlsx(x = tab_waves_cold_raw, file = "cold_waves_90.xlsx")





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# -------------------------- ОТСЕВ РЕГИОНОВ ---------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# загружаем таблицу с регионами, в которых:
# 1. большинство населения проживает в столице или вблизи,
# 2. без орографии или в пределах одной зоны
# 3. без сложной климатической картины
# всего 69 регионов с общим наслением 127 миллионов человек на 2019 год
# нет НАО (включена в Архангельскую область), Тюменской матрёшки (всей), Республики Алтай, Якутии, Чукотки, Краснодарского края, Крыма, Севастополя, национальных республик Северного Кавказа

# таблица создана за пределами R
reg_sel <- read.xlsx("reg_sel.xlsx")

# отфильтровываем только те значения, которые даны по отобранным регионам из таблицы reg_sel
# по жаре
test_hot <- tab_waves_hot_raw %>% filter(ROSBKS_code %in% reg_sel$ROSBKS_code)

# по холоду
test_cold <- tab_waves_cold_raw %>% filter(ROSBKS_code %in% reg_sel$ROSBKS_code)

# таблица с метеостанциями в отобранных регионах (все метеостанции)
tab_meteostation_sel <- test_hot %>% group_by(Name) %>% summarise(count = n(), reg = first(region_name))



# ДОБАВИТЬ
# Метеостанция Переславль-Залесский	-- Владимирская область
# 
# Метеостанция Кострома	-- Ивановская область
# 
# Метеостанция Поныри	-- Орловская область
# 
# ВСТАВКА	-- Архангельская область 
# 
# Метеостанция Краснодар, Круглик -- Республика Адыгея




# список метеостанций, которые репрезентативно отражают погоду в регионе (одна метеостанция на регион)
list_meteostation_sel <- c("Астрахань", "Готня", "Брянск ", "Волгоград ", "Вологда,Прилуки", "Воронеж",
                           "Смидович", "Калининград ", "Сухиничи", "Кострома", "Курган ", "Курск",
                           "Белогорка", "Конь-Колодезь", "Магадан", "Москва, ВДНХ", "Можайск", "Мурманск",
                           "Нижний Новгород", "Старая Русса", "Огурцово(Новосибирск)", "Омск", "Оренбург",
                           "Пенза", "Псков ", "Элиста", "Йошкар-Ола", "Краснослободск", "Казань", "Ростов-на-Дону ",
                           "Рязань", "Самара", "Санкт-Петербург", "Саратов", "Смоленск", "Тамбов", "Старица",
                           "Плавск", "Ижевск ", "Инза", "Порецкое", "Рыбинск,ГМО")





# фильтруем данные по списку метеостанций
test_hot_sel <- test_hot %>% filter(test_hot$Name %in% list_meteostation_sel)

test_cold_sel <- test_cold %>% filter(test_cold$Name %in% list_meteostation_sel)

# считаем суммы умерших в волны жары и холода ЗА ВСЕ ГОДЫ и сопоставляем их с нормальными значениями
# жара
test_deaths_hot <- test_hot_sel %>% filter(deads_pred != "") %>% group_by(region_name) %>% summarise(deads_hot = sum(deads_count), deads_norm = sum(deads_pred))

# превышения в людях
test_deaths_hot$exess_hum <- test_deaths_hot$deads_hot - test_deaths_hot$deads_norm

# превышения в процентах
test_deaths_hot$exess_pr <- round(x = test_deaths_hot$deads_hot / test_deaths_hot$deads_norm * 100 - 100, digits = 1)

# холод
test_deaths_cold <- test_cold_sel %>% filter(deads_pred != "") %>% group_by(region_name) %>% summarise(deads_cold = sum(deads_count), deads_norm = sum(deads_pred))

# превышения в людях
test_deaths_cold$exess_hum <- test_deaths_cold$deads_cold - test_deaths_cold$deads_norm

# превышения в процентах
test_deaths_cold$exess_pr <- round(x = test_deaths_cold$deads_cold / test_deaths_cold$deads_norm * 100 - 100, digits = 1)


# сохраняем таблицы с результатами
# жара
write.xlsx(x = test_deaths_hot, file = "deaths_hot_90.xlsx")
# холод
write.xlsx(x = test_deaths_cold, file = "deaths_cold_90.xlsx")


# таблица по годами регионам

# считаем суммы умерших в волны жары и холода ЗА КАЖДЫЙ ГОД и сопоставляем их с нормальными значениями
# жара
test_deaths_hot_an <- test_hot_sel %>% filter(deads_pred != "") %>% group_by(region_name, Year_RosBKS_last) %>% summarise(deads_hot = sum(deads_count), deads_norm = sum(deads_pred))

# превышения в людях
test_deaths_hot_an$exess_hum <- test_deaths_hot_an$deads_hot - test_deaths_hot_an$deads_norm

# превышения в процентах
test_deaths_hot_an$exess_pr <- round(x = test_deaths_hot_an$deads_hot / test_deaths_hot_an$deads_norm * 100 - 100, digits = 1)

# холод
test_deaths_cold_an <- test_cold_sel %>% filter(deads_pred != "") %>% group_by(region_name, Year_RosBKS_last) %>% summarise(deads_cold = sum(deads_count), deads_norm = sum(deads_pred))

# превышения в людях
test_deaths_cold_an$exess_hum <- test_deaths_cold_an$deads_cold - test_deaths_cold_an$deads_norm

# превышения в процентах
test_deaths_cold_an$exess_pr <- round(x = test_deaths_cold_an$deads_cold / test_deaths_cold_an$deads_norm * 100 - 100, digits = 1)


# сохраняем таблицы с результатами
# жара
write.xlsx(x = test_deaths_hot_an, file = "deaths_hot_an_90.xlsx")
# холод
write.xlsx(x = test_deaths_cold_an, file = "deaths_cold_an_90.xlsx")



# график соотношения избыточных смертей и длительности волн холода
plot(x = test_cold_sel$Count, y = test_cold_sel$excessive, cex = 0.1)
lines(y = rep(0,50), x = c(1:50), type = "l")


# график соотношения избыточных смертей и длительности волн жары
plot(x = test_hot_sel$Count, y = test_hot_sel$excessive, cex = 0.2)
lines(y = rep(0,50), x = c(1:50), type = "l")



# построение ящика с усами по жаре
ggplot() +
  geom_boxplot(test_hot_sel %>% arrange(Count), mapping = aes(x = factor(x = Count, levels = unique(Count)), y = excessive))
  # ggtitle("Diamonds' price by cut") +
  # xlab("Cut") +
  # ylab("Price")


# построение ящика с усами по холоду
ggplot() +
  geom_boxplot(test_cold_sel %>% arrange(Count),
               # %>% filter(excessive < 2500), # берём случаи только ДО 2500 избыточных смертей, чтобы разобраться в околонулевой динамике
               mapping = aes(x = factor(x = Count, levels = unique(Count)), y = excessive))
# ggtitle("Diamonds' price by cut") +
# xlab("Cut") +
# ylab("Price")

# рисуем по всем регионам вместе
ggplot() +
  geom_boxplot(test_cold_sel %>% arrange(Count)
               %>% filter(excessiv_pr < 200), # берём случаи только ДО 2500 избыточных смертей, чтобы разобраться в околонулевой динамике
               mapping = aes(x = factor(x = Count, levels = unique(Count)), y = excessiv_pr)) +
  facet_wrap(facet = vars(region_name)) #задаёт отрисовку всех регионов на одном листе
# ggtitle("Diamonds' price by cut") +
# xlab("Cut") +
# ylab("Price")

# сохраняем как изображение
ggsave(filename = "cold_pr.jpg", width = 1350, height = 775, units = "mm", dpi = 300, limitsize = FALSE)







# визуализация: 1. Как присылал картинку, 2. буква V (кривая смертей)!!!

# как определяют процентиль!!!

# как отбирают референсный перид для вычисления избыточных смертей!!!

# критерии эффекта косьбы!!!

# нужен ли лаг 21 день!!!




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# --------------------------------- ИДЕИ ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# 0. Сделать сравнение с 3 годами до (СДЕЛАНО)

# 0.1. Отсортировать интересующие регионы (СДЕЛАНО)

# 0.2. Посчитать суммарные потери от волн жары и волн холода в людях и в процентах к норме (СДЕЛАНО)

# 1. С помошью пакета gander и токена подключить ChatGPT для R. Скормить ему задачу выдавать в таблицу соответсвия ОКТМО и ТЕРСОН-МО всех муниципалитетов
# и областей, где есть метеостанции. (НЕ СДЕЛАНО)

# 2. Сообразно составить лист названий регионов в РБКC и СберИндекса. Вставить коды регионов РБК и ФНС в таблицу tab_station_mun (СДЕЛАНО)

# 3. Скачать данные ЕМИСС по среднегодовому числу населения за 2000-2019 гг. по всем регионам России (СДЕЛАНО)

# 4. Взять все данные РБКС и, домножив ОКС на среднегодовое население, получить число умерших по неделям (СДЕЛАНО)

# 5. Сравнить автоматически скользящее среднее (или просто понедельно) число умерших по регионам с прошлогодними значениями или средним за последние 2-3-5 лет (НЕ СДЕЛАНО)

# 6. Выбрать регионы, где:
# а. большинство населения проживает в столице, (СДЕЛАНО)
# б. без орографии и не протяженнее 400 км (все нечерноземье, ЦФО, 2/3 регионов), (СДЕЛАНО)
# в. без Краснодарского и Ставропольского краев, Кавказа, Крыма (СДЕЛАНО)

# 7. Совместить по номерам недель и метеостанциям число умерших, приросты с волнами жары / холода (СДЕЛАНО)

# 8. Просуммировать избыточно умерших по 1. регионам, (СДЕЛАНО) 2. всем волнам холода / жары, (СДЕЛАНО) 3. по годам, 4. за все годы (СДЕЛАНО)

# 9. Сравнить точность вычислений по процентелям 90, 93, 95, 97, 99

# ------------------------------------------------------------------------------

# 10. Собрать климатические нормы по срочным данным

# 11. Рассчитать (для жары) дни с высокой душностью по температуре точки росы

# 11а. Совместить по номерам недель и метеостанциям число умерших, приросты с волнами жары / холода

# 11б. Просуммировать избыточно умерших по 1. регионам, 2. всем волнам холода / жары, 3. по годам, 4. за все годы

# 11в. Сравнить точность вычислений по процентелям 90, 93, 95, 97, 99

