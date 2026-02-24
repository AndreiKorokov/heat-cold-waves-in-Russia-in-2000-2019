# --- UTF-8 ---

# Разработано в июне 2025 — январе 2026 года А. Коротковым (МГУ) и Н. Синицыным (МГУ)

# при копировании любой части материалов ссылка обязательна

# выполняйте код последовательно

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------- ШАГ 2 ДЕМОГРАФИЯ ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# библиотеки
library(ISOweek)
library(aweek)
library(cowplot)
library(ggtext)
library(splines)
library(dlnm)

setwd("D:/Data/wave")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ---------------------------- НАЧАЛО ОБРАБОТКИ -----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# загрузка результатов предыдущего шага
output_waves_hot <- read.xlsx("output_waves_hot_95per.xlsx", detectDates = T)
output_waves_cold <- read.xlsx("output_waves_cold_95per.xlsx", detectDates = T)



# добавляем Краснодарский край (жара)
adigeya <- output_waves_hot %>% filter(Region_name_ROSBKS == "Республика Адыгея")

adigeya$ROSBKS_code <- 1103

adigeya$Region_name_ROSBKS <- "Краснодарский край"

output_waves_hot <- rbind(output_waves_hot, adigeya)

# результирующее объединение в одну таблицу (жара)
tab_waves_hot_raw <- output_waves_hot


# добавляем Краснодарский край (холод)
adigeya <- output_waves_cold %>% filter(Region_name_ROSBKS == "Республика Адыгея")

adigeya$ROSBKS_code <- 1103

adigeya$Region_name_ROSBKS <- "Краснодарский край"

output_waves_cold <- rbind(output_waves_cold, adigeya)

# результирующее объединение в одну таблицу (холод)
tab_waves_cold_raw <- output_waves_cold


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

# переводим "год" в табЛицу в текстовый вид 
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

# приводим таблицу с населением в длинный вид
tab_population_long <- pivot_longer(data = tab_region_population, names_to = "Year", values_to = "population", cols = !"Region")

# замены несовпадающих написаний названий регионов
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Республика Ингушетия", replacement = "Ингушская республика") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Кемеровская область - Кузбасс", replacement = "Кемеровская область") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Город Санкт-Петербург город федерального значения", replacement = "Санкт-Петербург") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Город Москва столица Российской Федерации город федерального значения", replacement = "Москва") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Город федерального значения Севастополь", replacement = "Севастополь") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Республика Адыгея \\(Адыгея\\)", replacement = "Республика Адыгея") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Республика Татарстан \\(Татарстан\\)", replacement = "Республика Татарстан")
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Чеченская Республика", replacement = "Чеченская республика")
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Чувашская Республика - Чувашия", replacement = "Чувашская Республика") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Республика Ингушетия", replacement = "Ингушская Республика") 
tab_population_long$Region <- str_replace(tab_population_long$Region, 
                                          pattern = "Республика Северная Осетия-Алания", replacement = "Республика Северная Осетия — Алания") 



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



# определяем число умерших в неделю из ОКС по формуле РосБКС из статьи Shchur A, Timonin S, Churilova E et al., 2023, P. 192 (https://doi.org/10.3897/popecon.7.e114628)
tab_cdr$deads <- ifelse(tab_cdr$Year %% 4 == 0, round((tab_cdr$population / 366 * 7) * tab_cdr$CDR, digits = 0),
                        round((tab_cdr$population / 365 * 7) * tab_cdr$CDR, digits = 0))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------- ЦИКЛ ХОЛОД --------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# создаём пустые листы для записей результатов
list_cold_waves <- c()
list_cold_waves_pred <- c()
list_proverka_cold_wave_before <- c()
list_cold_waves_posled <- c()
list_proverka_cold_wave_posled <- c()
list_cold_waves_1 <- c()
list_cold_waves_2 <- c()
list_cold_waves_3 <- c()
list_cold_waves_min <- c()



# цикл
for (x in c(1 : nrow(tab_waves_cold_raw))) {
  
  # --- расчёт САМОЙ волны ---
  
  # определяем даты, в которые попадает волна
  days_wave <- as.Date(c(tab_waves_cold_raw$Date_start[x] : tab_waves_cold_raw$Date_last[x]))
  
  # переводим даты в номера недель по стандарту ISO
  days_week <- lubridate::isoweek(days_wave)
  
  # переводим даты в номера годов по стандарту ISO
  days_year <- lubridate::isoyear(days_wave)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave <- data.frame(days_week = (days_week), days_year = (days_year))
  
  # оставляем только уникальные значения недель и годов
  tab_wave <- tab_wave %>% distinct(days_week, days_year)
  
  # считаем число недель, затронутых волной
  num_week_wave <- nrow(tab_wave)
  
  # записываем код региона
  tab_wave$ROSBKS_code <- tab_waves_cold_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave$Region_name_ROSBKS <- tab_waves_cold_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_join <- left_join(tab_wave, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave <- (sum(tab_wave_join$deads, na.rm = TRUE) / (num_week_wave * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave <- round(x = sum_wave, digits = 0)
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves[x] <- sum_wave
  
  
  
  # --- СРАВНЕНИЯ С ПРЕДЫДУЩИМИ ПЕРИОДАМИ ДЛЯ ВЫЧИСЛЕНИЯ ИЗБЫТОЧНОЙ СМЕРТНОСТИ ---
  
  # --- расчёт в период В ПРЕДЫДУЩИЙ ПЕРИОД до волны --- 
  
  # записываем продолжительность волны (в днях)
  period_wave <- tab_waves_cold_raw$Count[x]
  
  # определяем период (даты), предшествующие рассматриваемой волне
  days_wave_pred <- days_wave - lubridate::days(period_wave)
  
  # переводим даты в номера недель по стандарту ISO
  days_week_pred <- lubridate::isoweek(days_wave_pred)
  
  # переводим даты в номера годов по стандарту ISO
  days_year_pred <- lubridate::isoyear(days_wave_pred)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave_pred <- data.frame(days_week_pred = (days_week_pred), days_year_pred = (days_year_pred))
  
  # оставляем только уникальные значения недель и годов
  tab_wave_pred <- tab_wave_pred %>% distinct(days_week_pred, days_year_pred)
  
  # вспомогательный цикл, решающий проблему с совпадающим значением числа смертей в предыдущий период с исследуемым,
  # если волна продолжалась меньше недели
  if (sum(tab_wave_pred$days_week_pred %in% tab_wave$days_week) > 0) {
    
    # отнимаем на семь дней (но не на календарную неделю) больше
    days_wave_pred <- days_wave - lubridate::days(period_wave + 7)
    
    # переводим даты в номера недель по стандарту ISO
    days_week_pred <- lubridate::isoweek(days_wave_pred)
    
    # переводим даты в номера годов по стандарту ISO
    days_year_pred <- lubridate::isoyear(days_wave_pred)
    
    # создаём таблицу с неделями и годами прохождения волны
    tab_wave_pred <- data.frame(days_week_pred = (days_week_pred), days_year_pred = (days_year_pred))
    
    # оставляем только уникальные значения недель и годов
    tab_wave_pred <- tab_wave_pred %>% distinct(days_week_pred, days_year_pred)
    
  }
  
  # число недель, предшествующих периоду, затронутого волной
  num_week_wave_pred <- nrow(tab_wave_pred)
  
  # записываем код региона
  tab_wave_pred$ROSBKS_code <- tab_waves_cold_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave_pred$Region_name_ROSBKS <- tab_waves_cold_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_pred_join <- left_join(tab_wave_pred, tab_cdr, by = c("days_week_pred" = "Week", "days_year_pred" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_pred <- (sum(tab_wave_pred_join$deads, na.rm = TRUE) / (num_week_wave_pred * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_pred <- round(x = sum_wave_pred, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение января 2000 года с декабрём 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_pred) | sum_wave_pred == 0) {sum_wave_pred <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves_pred[x] <- sum_wave_pred
  
  
  
  
  # --- расчёт в период ПОСЛЕ волны для оценки косьбы и лагов
  
  # записываем продолжительность волны (в днях)
  period_wave <- tab_waves_cold_raw$Count[x]
  
  # определяем период (даты), предшествующие рассматриваемой волне
  days_wave_posled <- days_wave + lubridate::days(period_wave)
  
  # переводим даты в номера недель по стандарту ISO
  days_week_posled <- lubridate::isoweek(days_wave_posled)
  
  # переводим даты в номера годов по стандарту ISO
  days_year_posled <- lubridate::isoyear(days_wave_posled)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave_posled <- data.frame(days_week_posled = (days_week_posled), days_year_posled = (days_year_posled))
  
  # оставляем только уникальные значения недель и годов
  tab_wave_posled <- tab_wave_posled %>% distinct(days_week_posled, days_year_posled)
  
  # вспомогательный цикл, решающий проблему с совпадающим значением числа смертей в предыдущий период с исследуемым,
  # если волна продолжалась меньше недели
  if (sum(tab_wave_posled$days_week_posled %in% tab_wave$days_week) > 0) {
    
    # отнимаем на семь дней (но не на календарную неделю) больше
    days_wave_posled <- days_wave + lubridate::days(period_wave + 7)
    
    # переводим даты в номера недель по стандарту ISO
    days_week_posled <- lubridate::isoweek(days_wave_posled)
    
    # переводим даты в номера годов по стандарту ISO
    days_year_posled <- lubridate::isoyear(days_wave_posled)
    
    # создаём таблицу с неделями и годами прохождения волны
    tab_wave_posled <- data.frame(days_week_posled = (days_week_posled), days_year_posled = (days_year_posled))
    
    # оставляем только уникальные значения недель и годов
    tab_wave_posled <- tab_wave_posled %>% distinct(days_week_posled, days_year_posled)
    
  }
  
  # число недель, предшествующих периоду, затронутого волной
  num_week_wave_posled <- nrow(tab_wave_posled)
  
  # записываем код региона
  tab_wave_posled$ROSBKS_code <- tab_waves_cold_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave_posled$Region_name_ROSBKS <- tab_waves_cold_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_posled_join <- left_join(tab_wave_posled, tab_cdr, by = c("days_week_posled" = "Week", "days_year_posled" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_posled <- (sum(tab_wave_posled_join$deads, na.rm = TRUE) / (num_week_wave_posled * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_posled <- round(x = sum_wave_posled, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение января 2000 года с декабрём 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_posled) | sum_wave_posled == 0) {sum_wave_posled <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves_posled[x] <- sum_wave_posled
  
  
  
  # --- проверям, была ли другая волна в период после волны ---
  
  if (x < nrow(tab_waves_cold_raw)) {
    
    days_wave_real_posled <- as.Date(c(tab_waves_cold_raw$Date_start[x+1] : tab_waves_cold_raw$Date_last[x+1]))
    
    proverka_posled <- sum(days_wave_posled %in% days_wave_real_posled)
    
    list_proverka_cold_wave_posled[x] <- proverka_posled
    
  }
  
  if (x == nrow(tab_waves_cold_raw)) {
    
    list_proverka_cold_wave_posled[x] <- NA
    
  }
  
  
  
  # --- проверям, была ли другая волна в период до волны ---
  
  if (x > 1) {
    
    days_wave_real_before <- as.Date(c(tab_waves_cold_raw$Date_start[x-1] : tab_waves_cold_raw$Date_last[x-1]))
    
    proverka_before <- sum(days_wave_pred %in% days_wave_real_before)
    
    list_proverka_cold_wave_before[x] <- proverka_before
    
  }
  
  if (x == 1) {
    
    list_proverka_cold_wave_before[x] <- NA
    
  }
  
  
  
  # --- расчёт в период за ГОД до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_1 <- tab_wave
  
  # отнимаем ОДИН год от даты волны
  tab_wave_1$days_year <- tab_wave_1$days_year - 1
  
  # отобранный год должен быть не меньше 2000
  tab_wave_1 <- tab_wave_1 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_1) > 0) {
    
    if (max(tab_wave_1$days_week) > 52) {
      
      tab_wave_1$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в году, затронутом волной
  num_week_wave_1 <- nrow(tab_wave_1)
  
  # обогащаем таблицу другими данными
  tab_wave_1_join <- left_join(tab_wave_1, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_1 <- (sum(tab_wave_1_join$deads, na.rm = TRUE) / (num_week_wave_1 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_1 <- round(x = sum_wave_1, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_1) | sum_wave_1 == 0) {sum_wave_1 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves_1[x] <- sum_wave_1
  
  
  
  # --- расчёт в период за ДВА ГОДА до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_2 <- tab_wave
  
  # отнимаем ДВА года от даты волны
  tab_wave_2$days_year <- tab_wave_2$days_year - 2
  
  # отобранный год должен быть не меньше 2000
  tab_wave_2 <- tab_wave_2 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_2) > 0) {
    
    if (max(tab_wave_2$days_week) > 52) {
      
      tab_wave_2$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в позапрошлом году, затронутом волной
  num_week_wave_2 <- nrow(tab_wave_2)
  
  # обогащаем таблицу другими данными
  tab_wave_2_join <- left_join(tab_wave_2, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_2 <- (sum(tab_wave_2_join$deads, na.rm = TRUE) / (num_week_wave_2 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_2 <- round(x = sum_wave_2, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_2) | sum_wave_2 == 0) {sum_wave_2 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves_2[x] <- sum_wave_2
  
  
  
  # --- расчёт в период за ТРИ ГОДА до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_3 <- tab_wave
  
  # отнимаем ТРИ года от даты волны
  tab_wave_3$days_year <- tab_wave_3$days_year - 3
  
  # отобранный год должен быть не меньше 2000
  tab_wave_3 <- tab_wave_3 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_3) > 0) {
    
    if (max(tab_wave_3$days_week) > 52) {
      
      tab_wave_3$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в поза-позапрошлом году, затронутом волной
  num_week_wave_3 <- nrow(tab_wave_3)
  
  # обогащаем таблицу другими данными
  tab_wave_3_join <- left_join(tab_wave_3, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_3 <- (sum(tab_wave_3_join$deads, na.rm = TRUE) / (num_week_wave_3 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_3 <- round(x = sum_wave_3, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_3) | sum_wave_3 == 0) {sum_wave_3 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_cold_waves_3[x] <- sum_wave_3
  
  # расчёт минимумов
  
  # определяем период с минимальным числом умерших. С ним будет проводится дальнейшее сравнение
  list_cold_waves_min[x] <- min(c(sum_wave_pred, sum_wave_1, sum_wave_2, sum_wave_3)[c(sum_wave_pred, sum_wave_1, sum_wave_2, sum_wave_3) > 0], na.rm = TRUE)
  
  # счётчик цикла
  print(x)
  
}



# сохраняем полученные из цикла данные как столбцы в ИТОГОВОЙ ТАБЛИЦЕ
tab_waves_cold_raw$deaths <- list_cold_waves
tab_waves_cold_raw$deaths_pred <- list_cold_waves_pred
tab_waves_cold_raw$proverka_cold_wave_before <- list_proverka_cold_wave_before
tab_waves_cold_raw$deaths_posled <- list_cold_waves_posled
tab_waves_cold_raw$proverka_cold_wave_posled <- list_proverka_cold_wave_posled
tab_waves_cold_raw$deaths_1 <- list_cold_waves_1
tab_waves_cold_raw$deaths_2 <- list_cold_waves_2
tab_waves_cold_raw$deaths_3 <- list_cold_waves_3
tab_waves_cold_raw$deaths_min <- list_cold_waves_min



# перезаписываем таблицу для удобства дальнейшей работы
tab_waves_cold_select <- tab_waves_cold_raw 



# --- превышения для КАЖДЫЙ ВОЛНЫ ---

# считаем превышения в людях
tab_waves_cold_select$excess_hum <- tab_waves_cold_select$deaths - tab_waves_cold_select$deaths_min

# считаем превышения в процентах
tab_waves_cold_select$excess_pr <- round(x = tab_waves_cold_select$deaths / tab_waves_cold_select$deaths_min * 100 - 100, digits = 1)

# убираем значения с inf (они нерелевантны для расчётов)
tab_waves_cold_select <- tab_waves_cold_select %>% filter(deaths_min != Inf)



# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_waves_cold_regions <- tab_waves_cold_select %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum = sum(excess_hum))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_cold <- left_join(tab_waves_cold_regions, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_cold$excess_pr <- round(((tab_svod_region_cold$excess_hum / tab_svod_region_cold$sum_deads) * 100), digits = 3)











# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------- ЦИКЛ ЖАРА ---------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# создаём пустые листы для записей результатов
list_hot_waves <- c()
list_hot_waves_pred <- c()
list_proverka_hot_wave_before <- c()
list_hot_waves_posled <- c()
list_proverka_hot_wave_posled <- c()
list_hot_waves_1 <- c()
list_hot_waves_2 <- c()
list_hot_waves_3 <- c()
list_hot_waves_min <- c()



# цикл
for (x in c(1 : nrow(tab_waves_hot_raw))) {
  
  # --- расчёт САМОЙ волны ---
  
  # определяем даты, в которые попадает волна
  days_wave <- as.Date(c(tab_waves_hot_raw$Date_start[x] : tab_waves_hot_raw$Date_last[x]))
  
  # переводим даты в номера недель по стандарту ISO
  days_week <- lubridate::isoweek(days_wave)
  
  # переводим даты в номера годов по стандарту ISO
  days_year <- lubridate::isoyear(days_wave)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave <- data.frame(days_week = (days_week), days_year = (days_year))
  
  # оставляем только уникальные значения недель и годов
  tab_wave <- tab_wave %>% distinct(days_week, days_year)
  
  # считаем число недель, затронутых волной
  num_week_wave <- nrow(tab_wave)
  
  # записываем код региона
  tab_wave$ROSBKS_code <- tab_waves_hot_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave$Region_name_ROSBKS <- tab_waves_hot_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_join <- left_join(tab_wave, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave <- (sum(tab_wave_join$deads, na.rm = TRUE) / (num_week_wave * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave <- round(x = sum_wave, digits = 0)
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves[x] <- sum_wave
  
  
  
  # --- СРАВНЕНИЯ С ПРЕДЫДУЩИМИ ПЕРИОДАМИ ДЛЯ ВЫЧИСЛЕНИЯ ИЗБЫТОЧНОЙ СМЕРТНОСТИ ---
  
  # --- расчёт в период В ПРЕДЫДУЩИЙ ПЕРИОД до волны --- 
  
  # записываем продолжительность волны (в днях)
  period_wave <- tab_waves_hot_raw$Count[x]
  
  # определяем период (даты), предшествующие рассматриваемой волне
  days_wave_pred <- days_wave - lubridate::days(period_wave)
  
  # переводим даты в номера недель по стандарту ISO
  days_week_pred <- lubridate::isoweek(days_wave_pred)
  
  # переводим даты в номера годов по стандарту ISO
  days_year_pred <- lubridate::isoyear(days_wave_pred)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave_pred <- data.frame(days_week_pred = (days_week_pred), days_year_pred = (days_year_pred))
  
  # оставляем только уникальные значения недель и годов
  tab_wave_pred <- tab_wave_pred %>% distinct(days_week_pred, days_year_pred)
  
  # вспомогательный цикл, решающий проблему с совпадающим значением числа смертей в предыдущий период с исследуемым,
  # если волна продолжалась меньше недели
  if (sum(tab_wave_pred$days_week_pred %in% tab_wave$days_week) > 0) {
    
    # отнимаем на семь дней (но не на календарную неделю) больше
    days_wave_pred <- days_wave - lubridate::days(period_wave + 7)
    
    # переводим даты в номера недель по стандарту ISO
    days_week_pred <- lubridate::isoweek(days_wave_pred)
    
    # переводим даты в номера годов по стандарту ISO
    days_year_pred <- lubridate::isoyear(days_wave_pred)
    
    # создаём таблицу с неделями и годами прохождения волны
    tab_wave_pred <- data.frame(days_week_pred = (days_week_pred), days_year_pred = (days_year_pred))
    
    # оставляем только уникальные значения недель и годов
    tab_wave_pred <- tab_wave_pred %>% distinct(days_week_pred, days_year_pred)
    
  }
  
  # число недель, предшествующих периоду, затронутого волной
  num_week_wave_pred <- nrow(tab_wave_pred)
  
  # записываем код региона
  tab_wave_pred$ROSBKS_code <- tab_waves_hot_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave_pred$Region_name_ROSBKS <- tab_waves_hot_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_pred_join <- left_join(tab_wave_pred, tab_cdr, by = c("days_week_pred" = "Week", "days_year_pred" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_pred <- (sum(tab_wave_pred_join$deads, na.rm = TRUE) / (num_week_wave_pred * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_pred <- round(x = sum_wave_pred, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение января 2000 года с декабрём 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_pred) | sum_wave_pred == 0) {sum_wave_pred <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves_pred[x] <- sum_wave_pred
  
  
  
  # --- расчёт в период ПОСЛЕ волны для оценки косьбы и лагов
  
  # записываем продолжительность волны (в днях)
  period_wave <- tab_waves_hot_raw$Count[x]
  
  # определяем период (даты), предшествующие рассматриваемой волне
  days_wave_posled <- days_wave + lubridate::days(period_wave)
  
  # переводим даты в номера недель по стандарту ISO
  days_week_posled <- lubridate::isoweek(days_wave_posled)
  
  # переводим даты в номера годов по стандарту ISO
  days_year_posled <- lubridate::isoyear(days_wave_posled)
  
  # создаём таблицу с неделями и годами прохождения волны
  tab_wave_posled <- data.frame(days_week_posled = (days_week_posled), days_year_posled = (days_year_posled))
  
  # оставляем только уникальные значения недель и годов
  tab_wave_posled <- tab_wave_posled %>% distinct(days_week_posled, days_year_posled)
  
  # вспомогательный цикл, решающий проблему с совпадающим значением числа смертей в предыдущий период с исследуемым,
  # если волна продолжалась меньше недели
  if (sum(tab_wave_posled$days_week_posled %in% tab_wave$days_week) > 0) {
    
    # отнимаем на семь дней (но не на календарную неделю) больше
    days_wave_posled <- days_wave + lubridate::days(period_wave + 7)
    
    # переводим даты в номера недель по стандарту ISO
    days_week_posled <- lubridate::isoweek(days_wave_posled)
    
    # переводим даты в номера годов по стандарту ISO
    days_year_posled <- lubridate::isoyear(days_wave_posled)
    
    # создаём таблицу с неделями и годами прохождения волны
    tab_wave_posled <- data.frame(days_week_posled = (days_week_posled), days_year_posled = (days_year_posled))
    
    # оставляем только уникальные значения недель и годов
    tab_wave_posled <- tab_wave_posled %>% distinct(days_week_posled, days_year_posled)
    
  }
  
  # число недель, предшествующих периоду, затронутого волной
  num_week_wave_posled <- nrow(tab_wave_posled)
  
  # записываем код региона
  tab_wave_posled$ROSBKS_code <- tab_waves_hot_raw$ROSBKS_code[x]
  
  # записываем название региона
  tab_wave_posled$Region_name_ROSBKS <- tab_waves_hot_raw$Region_name_ROSBKS[x]
  
  # обогащаем таблицу другими данными
  tab_wave_posled_join <- left_join(tab_wave_posled, tab_cdr, by = c("days_week_posled" = "Week", "days_year_posled" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_posled <- (sum(tab_wave_posled_join$deads, na.rm = TRUE) / (num_week_wave_posled * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_posled <- round(x = sum_wave_posled, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение января 2000 года с декабрём 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_posled) | sum_wave_posled == 0) {sum_wave_posled <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves_posled[x] <- sum_wave_posled
  
  
  
  # --- проверям, была ли другая волна в период после волны ---
  
  if (x < nrow(tab_waves_hot_raw)) {
    
    days_wave_real_posled <- as.Date(c(tab_waves_hot_raw$Date_start[x+1] : tab_waves_hot_raw$Date_last[x+1]))
    
    proverka_posled <- sum(days_wave_posled %in% days_wave_real_posled)
    
    list_proverka_hot_wave_posled[x] <- proverka_posled
    
  }
  
  if (x == nrow(tab_waves_hot_raw)) {
    
    list_proverka_hot_wave_posled[x] <- NA
    
  }
  
  
  
  
  # --- проверям, была ли другая волна в период до волны ---
  
  if (x > 1) {
    
    days_wave_real_before <- as.Date(c(tab_waves_hot_raw$Date_start[x-1] : tab_waves_hot_raw$Date_last[x-1]))
    
    proverka_before <- sum(days_wave_pred %in% days_wave_real_before)
    
    list_proverka_hot_wave_before[x] <- proverka_before
    
  }
  
  if (x == 1) {
    
    list_proverka_hot_wave_before[x] <- NA
    
  }
  
  
  
  
  # --- расчёт в период за ГОД до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_1 <- tab_wave
  
  # отнимаем ОДИН год от даты волны
  tab_wave_1$days_year <- tab_wave_1$days_year - 1
  
  # отобранный год должен быть не меньше 2000
  tab_wave_1 <- tab_wave_1 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_1) > 0) {
    
    if (max(tab_wave_1$days_week) > 52) {
      
      tab_wave_1$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в году, затронутом волной
  num_week_wave_1 <- nrow(tab_wave_1)
  
  # обогащаем таблицу другими данными
  tab_wave_1_join <- left_join(tab_wave_1, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_1 <- (sum(tab_wave_1_join$deads, na.rm = TRUE) / (num_week_wave_1 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_1 <- round(x = sum_wave_1, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_1) | sum_wave_1 == 0) {sum_wave_1 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves_1[x] <- sum_wave_1
  
  
  
  # --- расчёт в период за ДВА ГОДА до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_2 <- tab_wave
  
  # отнимаем ДВА года от даты волны
  tab_wave_2$days_year <- tab_wave_2$days_year - 2
  
  # отобранный год должен быть не меньше 2000
  tab_wave_2 <- tab_wave_2 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_2) > 0) {
    
    if (max(tab_wave_2$days_week) > 52) {
      
      tab_wave_2$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в позапрошлом году, затронутом волной
  num_week_wave_2 <- nrow(tab_wave_2)
  
  # обогащаем таблицу другими данными
  tab_wave_2_join <- left_join(tab_wave_2, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_2 <- (sum(tab_wave_2_join$deads, na.rm = TRUE) / (num_week_wave_2 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_2 <- round(x = sum_wave_2, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_2) | sum_wave_2 == 0) {sum_wave_2 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves_2[x] <- sum_wave_2
  
  
  
  # --- расчёт в период за ТРИ ГОДА до волны --- 
  
  # перезаписываем данные о исследуемой волне
  tab_wave_3 <- tab_wave
  
  # отнимаем ТРИ года от даты волны
  tab_wave_3$days_year <- tab_wave_3$days_year - 3
  
  # отобранный год должен быть не меньше 2000
  tab_wave_3 <- tab_wave_3 %>% filter(days_year >= 2000)
  
  # в случае разного числа недель в сопоставляемых годах (52 сравнимается с 53) последняя неделя сопоставляемого периода принимается за 52
  if (nrow(tab_wave_3) > 0) {
    
    if (max(tab_wave_3$days_week) > 52) {
      
      tab_wave_3$days_week <- 52
      
    }
    
  }
  
  # число недель, предшествующих периоду в поза-позапрошлом году, затронутом волной
  num_week_wave_3 <- nrow(tab_wave_3)
  
  # обогащаем таблицу другими данными
  tab_wave_3_join <- left_join(tab_wave_3, tab_cdr, by = c("days_week" = "Week", "days_year" = "Year", "ROSBKS_code" = "PopCode"))
  
  # для удобства расчётов и во избежание завышенного или заниженного числа умерших при сравнении вводится допущение
  # о равномерном распределении числа умерших внутри недели (иное недоказуемо на недельных данных)
  # число умерших за период волны (округление числа недель вверх) делится на число дней продолжительности волны (по метеоданным)
  sum_wave_3 <- (sum(tab_wave_3_join$deads, na.rm = TRUE) / (num_week_wave_3 * 7)) * length(days_wave)
  
  # округляем число умерших до целого значения вверх
  sum_wave_3 <- round(x = sum_wave_3, digits = 0)
  
  # в случае нулевого числа умерших (например, сравнение марта 2000 года с марта 1999 года, с периодом, когда данных нет)
  # результат записывается как NA и в дальнейших расчётах игнорируется
  # в общем случае, подсчёт избыточно умерших для 2000 года невозможен, кроме как при сравнении с предыдущими периодами того же года
  if (is.nan(sum_wave_3) | sum_wave_3 == 0) {sum_wave_3 <- NA}
  
  # запись результатов расчётов по каждой волне в лист
  list_hot_waves_3[x] <- sum_wave_3
  
  # расчёт минимумов
  
  # определяем период с минимальным числом умерших. С ним будет проводится дальнейшее сравнение
  list_hot_waves_min[x] <- min(c(sum_wave_pred, sum_wave_1, sum_wave_2, sum_wave_3)[c(sum_wave_pred, sum_wave_1, sum_wave_2, sum_wave_3) > 0], na.rm = TRUE)
  
  # счётчик цикла
  print(x)
  
}



# сохраняем полученные из цикла данные как столбцы в ИТОГОВОЙ ТАБЛИЦЕ
tab_waves_hot_raw$deaths <- list_hot_waves
tab_waves_hot_raw$deaths_pred <- list_hot_waves_pred
tab_waves_hot_raw$proverka_hot_wave_before <- list_proverka_hot_wave_before
tab_waves_hot_raw$deaths_posled <- list_hot_waves_posled
tab_waves_hot_raw$proverka_hot_wave_posled <- list_proverka_hot_wave_posled
tab_waves_hot_raw$deaths_1 <- list_hot_waves_1
tab_waves_hot_raw$deaths_2 <- list_hot_waves_2
tab_waves_hot_raw$deaths_3 <- list_hot_waves_3
tab_waves_hot_raw$deaths_min <- list_hot_waves_min



# перезаписываем таблицу для удобства дальнейшей работы
tab_waves_hot_select <- tab_waves_hot_raw 



# --- превышения для КАЖДЫЙ ВОЛНЫ ---

# считаем превышения в людях
tab_waves_hot_select$excess_hum <- tab_waves_hot_select$deaths - tab_waves_hot_select$deaths_min

# считаем превышения в процентах
tab_waves_hot_select$excess_pr <- round(x = tab_waves_hot_select$deaths / tab_waves_hot_select$deaths_min * 100 - 100, digits = 1)

# убираем значения с inf (они нерелевантны для расчётов)
tab_waves_hot_select <- tab_waves_hot_select %>% filter(deaths_min != Inf)



# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_waves_hot_regions <- tab_waves_hot_select %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum = sum(excess_hum))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_hot <- left_join(tab_waves_hot_regions, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_hot$excess_pr <- round(((tab_svod_region_hot$excess_hum / tab_svod_region_hot$sum_deads) * 100), digits = 3)



# --- сохранение итоговых таблиц --- 

# сохраняем в таблицы результаты
write.xlsx(tab_svod_region_cold,"tab_svod_region_cold.xlsx")
write.xlsx(tab_waves_cold_regions_years,"waves_cold_regions_years_95.xlsx")

write.xlsx(tab_svod_region_hot,"tab_svod_region_hot.xlsx")
write.xlsx(tab_waves_hot_regions_years,"waves_hot_regions_years_95.xlsx")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------- ----- ОТРИСОВКА ГРАФИКОВ ------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# получаем дату начала каждой недели
tab_cdr$lubridate <- get_date(week = tab_cdr$Week, year = tab_cdr$Year, day = 1)

# переводим даты в текстовый формат для дальнейшей обработки
tab_cdr$lubridate_text <- as.character(tab_cdr$lubridate)



# получаем номер месяца
tab_cdr$lubridate_months <- str_extract(tab_cdr$lubridate_text, pattern = "-\\d\\d-") %>% str_remove_all(pattern = "-") %>% as.numeric()

# переводим даты в текстовый формат для дальнейшей обработки
tab_waves_hot_select$Date_start_text <- as.character(tab_waves_hot_select$Date_start)

# получаем номер месяца
tab_waves_hot_select$Date_start_months <- str_extract(tab_waves_hot_select$Date_start_text, pattern = "-\\d\\d-") %>% str_remove_all(pattern = "-") %>% as.numeric()

# переводим даты в текстовый формат для дальнейшей обработки
tab_waves_cold_select$Date_start_text <- as.character(tab_waves_cold_select$Date_start)

# получаем номер месяца
tab_waves_cold_select$Date_start_months <- str_extract(tab_waves_cold_select$Date_start_text, pattern = "-\\d\\d-") %>% str_remove_all(pattern = "-") %>% as.numeric()



# ---------------------------- жара ----------------------------



# общая таблица для цикла
tab_region <- tab_waves_hot_select %>% select(ROSBKS_code, Region_name_ROSBKS) %>% group_by(ROSBKS_code) %>% summarise(reg_name = first(Region_name_ROSBKS))

# список кодов регионов для анализа
code_region <- tab_region$ROSBKS_code

# список регионов для анализа
name_region <- tab_region$reg_name

# пустой лист
plot_region <- list()



# цикл по всем регионам
for (r in c(1 : length(code_region))) {
  
  # пустой лист для отрисовки по каждому региону
  plot_year <- list()
  
  # список годов
  years <- c(2000 : 2019)
  
  # выделяем регион для анализа
  cdr_reg <- tab_cdr %>% filter(PopCode == code_region[r], lubridate_months >= 4 & lubridate_months <= 9)
  
  # выставляем шкалу
  tab_scale <- data.frame(max = max(cdr_reg$deads, na.rm = TRUE), min = min(cdr_reg$deads, na.rm = TRUE))
  
  # цикл по всем годам одного региона
  for (z in 1 : 20) {
    
    # границы сезона
    year_reg <- cdr_reg %>% filter(lubridate >= as.POSIXct(paste0(years[z], "-04-01")) & lubridate <= as.POSIXct(paste0(years[z], "-09-30")))
    
    # вводим переменную-фильтр для выделения тёплого периода года, когда возможны волны жары и выделяем регион  для анализа
    waves_reg <- tab_waves_hot_select %>% filter(tab_waves_hot_select$Date_start_months >= 4 & 
                                                   tab_waves_hot_select$Date_start_months <= 9 & 
                                                   tab_waves_hot_select$Year_RosBKS_start == years[z] &
                                                   ROSBKS_code == code_region[r])
    
    
    if (nrow(waves_reg) > 0) {
      
      # размах шкалы
      waves_reg$min_rect <- tab_scale$min
      waves_reg$max_rect <- tab_scale$max
      
    }
    
    # визуализация графика смертей и волн жары
    pictt <-
      ggplot(data = tab_scale) +
      
      # линия числа смертей
      geom_line(data = year_reg, mapping = aes(x = lubridate, y = deads), color = "grey30", lwd = 0.5) + 
      
      `if`(nrow(waves_reg) > 0, 
           # прямоугольники волн жары
           geom_rect(data = waves_reg, mapping = aes(xmin = Date_start,
                                                     xmax = Date_last,
                                                     ymin = min_rect[1],
                                                     ymax = max_rect[1], fill = "zhara"),
                     alpha = 0.25, show.legend = FALSE),
           
           geom_blank()) +
      
      scale_fill_manual(values = "red", breaks = "zhara", labels = "Волна\nжары") +
      
      # сброс оформления темы графика
      theme_bw() +
      
      # подпись горизонтальной шкалы
      xlab(label = "") +
      
      # подпись вертикальной шкалы
      ylab(label = "Смертей в неделю") +
      
      # масштаб шкалы
      scale_x_date(date_labels = "%B", date_breaks = "month") +
      
      # ggtitle(label = "Волны жары (97%)", subtitle = paste0("Алтайский край, ", year_reg$Year[1], " г.")) + # подписи "Волны жары" и регион на каждом графике
      
      # подпись графика за год
      ggtitle(label = paste0(year_reg$Year[1], " год")) + # в подписи только год
      
      # масштабирование оси Y
      scale_y_continuous(limits = c(tab_scale$min, tab_scale$max)) +
      
      # оформление графика
      theme(axis.title = element_text(family = "sans", size = 10), # подпись названия оси
            axis.text = element_text(family = "sans", size = 9), # подпись делений оси
            title = element_text(family = "sans", size = 11), # заголовок всего рисуника
            # legend.text = element_text(family = "sans", size = 9), # подпись легенды
            # legend.position = c(0.12, 0.83), legend.title = element_blank(), # положение легенды
            plot.margin = unit(c(0, 0.8, 0, 0.1), "cm") # отступы. Верх, право, низ, лево
      )
    
    # записываем результат по каждому году в лист
    plot_year[[z]] <- pictt 
    
    
  }
  
  # создание сетки из 20 маленьких графиков
  facet_waves <- plot_grid(plot_year[[1]], plot_year[[2]], plot_year[[3]], plot_year[[4]], plot_year[[5]], 
                           plot_year[[6]], plot_year[[7]], plot_year[[8]], plot_year[[9]], plot_year[[10]],
                           plot_year[[11]], plot_year[[12]], plot_year[[13]], plot_year[[14]], plot_year[[15]],
                           plot_year[[16]], plot_year[[17]], plot_year[[18]], plot_year[[19]], plot_year[[20]], ncol = 4)
  
  # основная подпись
  title <- ggdraw() + 
    draw_label(paste0("Волны жары (", percentil_input, "%), ", name_region[r]), fontface = 'bold', x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 0, 7))
  
  # малая подпись
  subtitle <- ggdraw() + 
    draw_label("Волны жары показаны красными вертикальными полосами", size = 11, x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 10, 7))
  
  # создаём общий рисунок
  grid <- plot_grid(title, subtitle, facet_waves, ncol = 1, rel_heights = c(0.03, 0.03, 0.94))
  
  # сохраняем как рисунок в папку
  ggsave(filename = paste0("D:/Data/wave/pics/hot_waves_95_week_0209/", name_region[r], " волны жары.jpeg"), plot = grid, width = 526.8, height = 295.8, units = "mm", dpi = 300, limitsize = FALSE)
  
}



# ---------------------------- холод ----------------------------



# указываем годы (зимний период проходит через Новый год)
tab_cdr <- tab_cdr %>% mutate(Year_2t = case_when(lubridate_months >= 1 & lubridate_months <= 3 ~ Year - 1,
                                                  lubridate_months >= 10 & lubridate_months <= 12 ~ Year + 1),
                              Year_1t = case_when(lubridate_months >= 1 & lubridate_months <= 3 ~ Year,
                                                  lubridate_months >= 10 & lubridate_months <= 12 ~ Year))

# создаём пустой столбец
tab_cdr$season <- ""

# цикл записывает номера сезонов
for (q in c(1 : nrow(tab_cdr))) {
  
  tab_cdr$season[q] <- paste0(min(tab_cdr$Year_1t[q], tab_cdr$Year_2t[q]), "_", max(tab_cdr$Year_1t[q], tab_cdr$Year_2t[q]))
  
}

# удаляем пробелы в данных (летние месяцы)
tab_cdr$season[tab_cdr$season == "NA_NA"] <- NA

# указываем сезон (зимний период проходит через Новый год)
tab_waves_cold_select <- tab_waves_cold_select %>% mutate(Year_2t = case_when(Date_start_months >= 1 & Date_start_months <= 3 ~ Year_RosBKS_start - 1,
                                                                              Date_start_months >= 10 & Date_start_months <= 12 ~ Year_RosBKS_start + 1),
                                                          Year_1t = case_when(Date_start_months >= 1 & Date_start_months <= 3 ~ Year_RosBKS_start,
                                                                              Date_start_months >= 10 & Date_start_months <= 12 ~ Year_RosBKS_start))

# создаём пустой столбец
tab_waves_cold_select$season <- ""

# цикл записывает корректные номера сезонов
for (q in c(1 : nrow(tab_waves_cold_select))) {
  
  tab_waves_cold_select$season[q] <- paste0(min(tab_waves_cold_select$Year_1t[q], tab_waves_cold_select$Year_2t[q]), "_", max(tab_waves_cold_select$Year_1t[q], tab_waves_cold_select$Year_2t[q]))
  
}

# удаляем пробелы в данных
tab_waves_cold_select$season[tab_waves_cold_select$season == "NA_NA"] <- NA

# группировка по сезонам
tab_region <- tab_waves_cold_select %>% select(ROSBKS_code, Region_name_ROSBKS) %>% group_by(ROSBKS_code) %>% summarise(reg_name = first(Region_name_ROSBKS))

# список кодов регионов для анализа
code_region <- tab_region$ROSBKS_code

# список регионов для анализа
name_region <- tab_region$reg_name

# пустой лист
plot_region <- list()



# цикл по всем регионам
for (r in c(1 : length(code_region))) {
  
  # пустой лист для отрисовки по каждому региону
  plot_year <- list()
  
  # список зимних сезонов
  years <- unique(tab_cdr$season) %>% na.omit()
  
  # выделяем регион для анализа
  cdr_reg <- tab_cdr %>% filter(PopCode == code_region[r], lubridate_months >= 10 | lubridate_months <= 3)
  
  # выставляем шкалу
  tab_scale <- data.frame(max = max(cdr_reg$deads, na.rm = TRUE), min = min(cdr_reg$deads, na.rm = TRUE))
  
  # z = 1
  
  # цикл по всем годам одного региона
  for (z in 1 : 20) {
    
    # границы сезона
    year_reg <- cdr_reg %>% filter(season == years[z])
    
    # работа с форматом названий сезонов
    year_first <- str_remove(str_extract(string = year_reg$season[1], pattern = ".*_"), "_")
    
    # работа с форматом названий сезонов
    year_last <- str_remove(str_extract(string = year_reg$season[1], pattern = "_.*"), "_")
    
    # работа с форматом названий сезонов
    year_reg <- year_reg %>% filter(lubridate >= as.POSIXct(paste0(year_first, "-01-01")) & 
                                      lubridate <= as.POSIXct(paste0(year_last, "-12-31")))
    
    # вводим переменную-фильтр для выделения холодного периода года, когда возможны волны холода и выделяем регион для анализа
    waves_reg <- tab_waves_cold_select %>% filter((tab_waves_cold_select$Date_start_months >= 10 | 
                                                     tab_waves_cold_select$Date_start_months <= 3) & 
                                                    tab_waves_cold_select$season == years[z] &
                                                    ROSBKS_code == code_region[r])
    
    # уточням переменную-фильтр
    waves_reg <- waves_reg %>% filter(Date_start >= as.POSIXct(paste0(year_first, "-01-01")) & 
                                        Date_start <= as.POSIXct(paste0(year_last, "-12-31")))
    
    
    
    if (nrow(waves_reg) > 0) {
      
      # размах шкалы
      waves_reg$min_rect <- tab_scale$min
      waves_reg$max_rect <- tab_scale$max
      
    }
    
    # визуализация графика смертей и волн жары
    pictt <-
      ggplot(data = tab_scale) +
      
      # линия числа смертей
      geom_line(data = year_reg, mapping = aes(x = lubridate, y = deads), color = "grey30", lwd = 0.5) +
      
      `if`(nrow(waves_reg) > 0, 
           # прямоугольники волн жары
           geom_rect(data = waves_reg, mapping = aes(xmin = Date_start,
                                                     xmax = Date_last,
                                                     ymin = min_rect[1],
                                                     ymax = max_rect[1], fill = "zhara"),
                     alpha = 0.25, show.legend = FALSE), 
           
           geom_blank())  + 
      
      scale_fill_manual(values = "blue", breaks = "zhara", labels = "Волна\nжары") +
      
      # сброс оформления темы графика
      theme_bw() +
      
      # подпись горизонтальной шкалы
      xlab(label = "") +
      
      # подпись вертикальной шкалы
      ylab(label = "Смертей в неделю") +
      
      # масштаб шкалы
      scale_x_date(date_labels = "%B", date_breaks = "month") +
      
      # подпись графика за один сезон
      ggtitle(label = paste0("Зима ", str_replace(year_reg$season[1], pattern = "_", replacement = " / "), " годов")) + # в подписи только год
      
      # масштабирование оси Y
      scale_y_continuous(limits = c(tab_scale$min, tab_scale$max)) + # масштабирование оси Y
      
      # оформление графика
      theme(axis.title = element_text(family = "sans", size = 10), # подпись названия оси
            axis.text = element_text(family = "sans", size = 9), # подпись делений оси
            title = element_text(family = "sans", size = 11), # заголовок всего рисуника
            # legend.text = element_text(family = "sans", size = 9), # подпись легенды
            # legend.position = c(0.12, 0.83), legend.title = element_blank(), # положение легенды
            plot.margin = unit(c(0, 0.8, 0, 0.1), "cm") # отступы. Верх, право, низ, лево
      )
    
    # записываем результат по каждому году в лист
    plot_year[[z]] <- pictt 
    
    
  }
  
  # создание сетки из 20 маленьких графиков (исключили зиму 2019 / 2020)
  facet_waves <- plot_grid(plot_year[[1]], plot_year[[2]], plot_year[[3]], plot_year[[4]], plot_year[[5]], 
                           plot_year[[6]], plot_year[[7]], plot_year[[8]], plot_year[[9]], plot_year[[10]],
                           plot_year[[11]], plot_year[[12]], plot_year[[13]], plot_year[[14]], plot_year[[15]],
                           plot_year[[16]], plot_year[[17]], plot_year[[18]], plot_year[[19]], plot_year[[20]], ncol = 4)
  
  # основная подпись
  title <- ggdraw() + 
    draw_label(paste0("Волны холода (", percentil_input, "%), ", name_region[r]), fontface = 'bold', x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 0, 7))
  
  # малая подпись
  subtitle <- ggdraw() + 
    draw_label("Волны холода показаны голубыми вертикальными полосами", size = 11, x = 0, hjust = 0) + 
    theme(plot.margin = margin(0, 0, 10, 7))
  
  # создаём общий рисунок
  grid <- plot_grid(title, subtitle, facet_waves, ncol = 1, rel_heights = c(0.03, 0.03, 0.94))
  
  # сохраняем как рисунок в папку
  ggsave(filename = paste0("D:/Data/wave/pics/cold_waves_95_week_0209/", name_region[r], " волны холода.jpeg"), plot = grid, width = 526.8, height = 295.8, units = "mm", dpi = 300, limitsize = FALSE)
  
}


# число дней за 20 лет с волнами жары по регионам
reg_time_hot_wave <- tab_waves_hot_select %>% group_by(Region_name_ROSBKS) %>% summarise(times = sum(Count))

# число дней за 20 лет с волнами холода по регионам
reg_time_cold_wave <- tab_waves_cold_select %>% group_by(Region_name_ROSBKS) %>% summarise(times = sum(Count))

# число дней в каждый год за 20 лет с волнами жары по регионам
reg_year_time_hot_wave <- tab_waves_hot_select %>% group_by(Region_name_ROSBKS, Year_RosBKS_start) %>% summarise(times = sum(Count))

# число дней в каждый год за 20 лет с волнами холода по регионам
reg_year_time_cold_wave <- tab_waves_cold_select %>% group_by(Region_name_ROSBKS, Year_RosBKS_start) %>% summarise(times = sum(Count))

# жара
ggplot()+
  geom_point(data = reg_year_time_hot_wave, mapping = aes(x = Year_RosBKS_start, y = times)) +
  facet_wrap(facet = vars(Region_name_ROSBKS))

# холод
ggplot()+
  geom_point(data = reg_year_time_cold_wave, mapping = aes(x = Year_RosBKS_start, y = times)) +
  facet_wrap(facet = vars(Region_name_ROSBKS))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# -------- НОВЫЙ ПОДХОД. ВЫДЕЛЕНИЕ ПИКОВ СМЕРТЕЙ, СОВПАДАЮЩИХ С ВОЛНАМИ -----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# ---------------------------- жара ----------------------------

# лист по всем регионам в целом
list_waves_hot_pick <- list()

for (r in c(1 : length(code_region))) {
  
  # список годов
  years <- c(2000 : 2019)
  
  # выделяем регион для анализа
  cdr_reg <- tab_cdr %>% filter(PopCode == code_region[r], lubridate_months >= 4 & lubridate_months <= 9)
  
  # лист по отдельным годам одного региона
  waves_hot_pick_reg <- list()
  
  # цикл по всем годам одного региона
  for (z in 1 : 20) {
    
    # границы сезона
    year_reg <- cdr_reg %>% filter(lubridate >= as.POSIXct(paste0(years[z], "-04-01")) & lubridate <= as.POSIXct(paste0(years[z], "-09-30")))
    
    # вводим переменную-фильтр для выделения тёплого периода года, когда возможны волны жары и выделяем регион  для анализа
    waves_reg <- tab_waves_hot_select %>% filter(tab_waves_hot_select$Date_start_months >= 4 & 
                                                   tab_waves_hot_select$Date_start_months <= 9 & 
                                                   tab_waves_hot_select$Year_RosBKS_start == years[z] &
                                                   ROSBKS_code == code_region[r])
    
    # в случае отсутствия волн в конкретном годе, цикл переходит на следующий сезон
    if (nrow(waves_reg) == 0) {
      
      next
      
    }
    
    # сортируем таблицу по дате (от старого к новому)
    year_reg <- year_reg %>% arrange(lubridate)
    
    # расчёт производной числа смертей по времени (прирост между неделями)
    year_reg$prirost <- c(0, diff(year_reg$deads))
    
    # создаём столбец для записи пиков (определения локальных минимумом и максимумов)
    year_reg$pick <- NA
    
    # первое значение (неделя) не учитывается в подсчёте
    year_reg$pick[1] <- FALSE
    
    # последнее значение (неделя) не учитывается в подсчёте
    year_reg$pick[length(year_reg$pick)] <- FALSE
    
    # цикл записываем значения в зависимости от смены знака производной. С плюса на минус = TRUE = максимум
    for (w in c(2 : (length(year_reg$pick) - 1))) {
      
      if (year_reg$prirost[w + 1] <= 0 & year_reg$prirost[w] > 0) {
        
        year_reg$pick[w] <- TRUE
        
      }
      
      if (year_reg$prirost[w + 1] > 0 & year_reg$prirost[w] < 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w + 1] > 0 & year_reg$prirost[w] > 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w + 1] < 0 & year_reg$prirost[w] < 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w] == 0) {
        
        year_reg$pick[w] <- year_reg$pick[w - 1]
        
      }
      
    }
    
    # поиск максимумов
    tab_max <- year_reg %>% filter(pick == TRUE)
    
    # вводим отсечку, чтобы "максимумы" были больше среднего + одно среднего квадратического отклонения
    tab_max <- tab_max %>% filter(deads > (mean(year_reg$deads, na.rm = T) + sd(year_reg$deads, na.rm = T)))
    
    # вводим понятие пиковой недели. Это промежуток между понедельниками, в который попадает волна жары, короче 7 дней и не проходящая через понедельники
    tab_max_reserve <- tab_max
    
    for (u in c(1 : 6)) {
      
      tab_max_add <- tab_max_reserve
      
      # добавляем шесть дней (до воскресенья включительно)
      tab_max_add$lubridate <- tab_max_add$lubridate + (1 * u)
      
      # объединяем таблицы
      tab_max <- rbind(tab_max, tab_max_add)
      
    }
    
    # сортировка
    tab_max <- tab_max %>% arrange(lubridate)
    
    # столбец наличия пика смертности в период волны
    waves_reg$pick <- NA
    
    # цикл проверяет совпадения пика смертности с наличием температурной волны в это же время
    for (t in c(1 : length(waves_reg$pick))) {
      
      wave_max <- tab_max %>% filter(lubridate >= waves_reg$Date_start[t] & lubridate <= waves_reg$Date_last[t])
      
      if (nrow(wave_max) > 0) {
        
        waves_reg$pick[t] <- TRUE
        
      }
      
      if (nrow(wave_max) == 0) {
        
        waves_reg$pick[t] <- FALSE
        
      }
      
    }
    
    # записываем совпадения пиков смертности и температурных волн
    waves_reg_pick <- waves_reg %>% filter(pick == TRUE)
    
    waves_hot_pick_reg[[z]] <- waves_reg_pick
    
  }
  
  list_waves_hot_pick[[r]] <- waves_hot_pick_reg
  
}

# вытаскиваем данные из листа
pick_hot_list <- unlist(list_waves_hot_pick, recursive = FALSE)

tab_hot_wave_pick <- data.table::rbindlist(pick_hot_list, fill = TRUE, use.names = TRUE)

tab_hot_wave_pick$excess_hum_posle <- tab_hot_wave_pick$deaths_posled - tab_hot_wave_pick$deaths_min
sum(tab_hot_wave_pick$excess_hum_posle[tab_hot_wave_pick$excess_hum_posle > 0])

# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_hot_wave_pick_reg <- tab_hot_wave_pick %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum = sum(excess_hum))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_hot_peak <- left_join(tab_hot_wave_pick_reg, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_hot_peak$excess_pr <- round(((tab_svod_region_hot_peak$excess_hum / tab_svod_region_hot_peak$sum_deads) * 100), digits = 3)

# сохраняем результирующую таблицу
write.xlsx(x = tab_svod_region_hot_peak, file = "tab_svod_region_hot_peak_95.xlsx")



# ----- СЧИТАЕМ, ЕСТЬ ИЛИ НЕТ КОСЬБА ИЛИ ЛАГИ -----



tab_waves_hot_proverk <- tab_hot_wave_pick %>% filter(proverka_hot_wave_posled == 0)

tab_waves_hot_proverk$otnoshenie_posle <- tab_waves_hot_proverk$deaths_posled / tab_waves_hot_proverk$deaths_min

tab_waves_hot_proverk$otnoshenie_before <- tab_waves_hot_proverk$deaths_pred / pmin(tab_waves_hot_proverk$deaths_1, tab_waves_hot_proverk$deaths_2, tab_waves_hot_proverk$deaths_3, na.rm = T)

tab_waves_hot_proverk$excess_hum_posle <- tab_waves_hot_proverk$deaths_posled - tab_waves_hot_proverk$deaths_min

tab_waves_hot_proverk$otnoshenie_posle <- (tab_waves_hot_proverk$otnoshenie_posle - 1) * 100

sum(tab_waves_hot_proverk$excess_hum_posle[tab_waves_hot_proverk$excess_hum_posle > 0])



# ---------------------------- холод ----------------------------

# лист по всем регионам в целом
list_waves_cold_pick <- list()

for (r in c(1 : length(code_region))) {
  
  # список зимних сезонов
  years <- unique(tab_cdr$season) %>% na.omit()
  
  # выделяем регион для анализа
  cdr_reg <- tab_cdr %>% filter(PopCode == code_region[r], lubridate_months >= 10 | lubridate_months <= 3)
  
  # выставляем шкалу
  # tab_scale <- data.frame(max = max(cdr_reg$deads, na.rm = TRUE), min = min(cdr_reg$deads, na.rm = TRUE))
  
  # лист по отдельным годам одного региона
  waves_cold_pick_reg <- list()
  
  # цикл по всем годам одного региона
  for (z in 1 : 20) {
    
    # границы сезона
    year_reg <- cdr_reg %>% filter(season == years[z])
    
    # работа с форматом названий сезонов
    year_first <- str_remove(str_extract(string = year_reg$season[1], pattern = ".*_"), "_")
    
    # работа с форматом названий сезонов
    year_last <- str_remove(str_extract(string = year_reg$season[1], pattern = "_.*"), "_")
    
    # работа с форматом названий сезонов
    year_reg <- year_reg %>% filter(lubridate >= as.POSIXct(paste0(year_first, "-01-01")) & 
                                      lubridate <= as.POSIXct(paste0(year_last, "-12-31")))
    
    # вводим переменную-фильтр для выделения холодного периода года, когда возможны волны холода и выделяем регион для анализа
    waves_reg <- tab_waves_cold_select %>% filter((tab_waves_cold_select$Date_start_months >= 10 | 
                                                     tab_waves_cold_select$Date_start_months <= 3) & 
                                                    tab_waves_cold_select$season == years[z] &
                                                    ROSBKS_code == code_region[r])
    
    # уточням переменную-фильтр
    waves_reg <- waves_reg %>% filter(Date_start >= as.POSIXct(paste0(year_first, "-01-01")) & 
                                        Date_start <= as.POSIXct(paste0(year_last, "-12-31")))
    
    # в случае отсутствия волн в конкретном сезоне, цикл переходит на следующий сезон
    if (nrow(waves_reg) == 0) {
      
      next
      
    }
    
    # сортируем таблицу по дате (от старого к новому)
    year_reg <- year_reg %>% arrange(lubridate)
    
    # расчёт производной числа смертей по времени (прирост между неделями)
    year_reg$prirost <- c(0, diff(year_reg$deads))
    
    # создаём столбец для записи пиков (определения локальных минимумом и максимумов)
    year_reg$pick <- NA
    
    # первое значение (неделя) не учитывается в подсчёте
    year_reg$pick[1] <- FALSE
    
    # последнее значение (неделя) не учитывается в подсчёте
    year_reg$pick[length(year_reg$pick)] <- FALSE
    
    # цикл записываем значения в зависимости от смены знака производной. С плюса на минус = TRUE = максимум
    for (w in c(2 : (length(year_reg$pick) - 1))) {
      
      if (year_reg$prirost[w + 1] <= 0 & year_reg$prirost[w] > 0) {
        
        year_reg$pick[w] <- TRUE
        
      }
      
      if (year_reg$prirost[w + 1] > 0 & year_reg$prirost[w] < 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w + 1] > 0 & year_reg$prirost[w] > 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w + 1] < 0 & year_reg$prirost[w] < 0) {
        
        year_reg$pick[w] <- FALSE
        
      }
      
      if (year_reg$prirost[w] == 0) {
        
        year_reg$pick[w] <- year_reg$pick[w - 1]
        
      }
      
    }
    
    # поиск максимумов
    tab_max <- year_reg %>% filter(pick == TRUE)
    
    # вводим отсечку, чтобы "максимумы" были больше среднего + одно среднего квадратического отклонения
    tab_max <- tab_max %>% filter(deads > (mean(year_reg$deads, na.rm = T) + sd(year_reg$deads, na.rm = T)))
    
    # вводим понятие пиковой недели. Это промежуток между понедельниками, в который попадает волна жары, короче 7 дней и не проходящая через понедельники
    tab_max_reserve <- tab_max
    
    for (u in c(1 : 6)) {
      
      tab_max_add <- tab_max_reserve
      
      # добавляем шесть дней (до воскресенья включительно)
      tab_max_add$lubridate <- tab_max_add$lubridate + (1 * u)
      
      # объединяем таблицы
      tab_max <- rbind(tab_max, tab_max_add)
      
    }
    
    # сортировка
    tab_max <- tab_max %>% arrange(lubridate)
    
    # столбец наличия пика смертности в период волны
    waves_reg$pick <- NA
    
    # цикл проверяет совпадения пика смертности с наличием температурной волны в это же время
    for (t in c(1 : length(waves_reg$pick))) {
      
      wave_max <- tab_max %>% filter(lubridate >= waves_reg$Date_start[t] & lubridate <= waves_reg$Date_last[t])
      
      if (nrow(wave_max) > 0) {
        
        waves_reg$pick[t] <- TRUE
        
      }
      
      if (nrow(wave_max) == 0) {
        
        waves_reg$pick[t] <- FALSE
        
      }
      
    }
    
    # записываем совпадения пиков смертности и температурных волн
    waves_reg_pick <- waves_reg %>% filter(pick == TRUE)
    
    waves_cold_pick_reg[[z]] <- waves_reg_pick
    
  }
  
  list_waves_cold_pick[[r]] <- waves_cold_pick_reg
  
}


# вытаскиваем данные из листа
pick_cold_list <- unlist(list_waves_cold_pick, recursive = FALSE)

tab_cold_wave_pick <- data.table::rbindlist(pick_cold_list, fill = TRUE, use.names = TRUE)

tab_cold_wave_pick$excess_hum_posle <- tab_cold_wave_pick$deaths_posled - tab_cold_wave_pick$deaths_min
sum(tab_cold_wave_pick$excess_hum_posle[tab_cold_wave_pick$excess_hum_posle > 0])


# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_cold_wave_pick_reg <- tab_cold_wave_pick %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum = sum(excess_hum))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_cold_peak <- left_join(tab_cold_wave_pick_reg, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_cold_peak$excess_pr <- round(((tab_svod_region_cold_peak$excess_hum / tab_svod_region_cold_peak$sum_deads) * 100), digits = 3)

# сохраняем результирующую таблицу
write.xlsx(x = tab_svod_region_cold_peak, file = "tab_svod_region_cold_peak_95.xlsx")



# ----- ЧИТАЕМ ЕСТЬ ИЛИ НЕТ КОСЬБА ИЛИ ЛАГИ -----



tab_waves_cold_proverk <- tab_cold_wave_pick %>% filter(proverka_cold_wave_posled == 0)

tab_waves_cold_proverk$otnoshenie_posle <- tab_waves_cold_proverk$deaths_posled / tab_waves_cold_proverk$deaths_min

tab_waves_cold_proverk$otnoshenie_before <- tab_waves_cold_proverk$deaths_pred / pmin(tab_waves_cold_proverk$deaths_1, tab_waves_cold_proverk$deaths_2, tab_waves_cold_proverk$deaths_3, na.rm = T)

tab_waves_cold_proverk$excess_hum_posle <- tab_waves_cold_proverk$deaths_posled - tab_waves_cold_proverk$deaths_min

tab_waves_cold_proverk$otnoshenie_posle <- (tab_waves_cold_proverk$otnoshenie_posle - 1) * 100

sum(tab_waves_cold_proverk$otnoshenie_posle[tab_waves_cold_proverk$otnoshenie_posle > 0])



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------ ОТРИСОВКА ГРАФИКОВ C КОСЬБОЙ И ЛАГАМИ ------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



tab_waves_cold_proverk <- tab_waves_cold_proverk %>% filter(otnoshenie_posle > -40 & otnoshenie_posle < 50)



ttt_cold <-  hist(x = tab_waves_cold_proverk$otnoshenie_posle, binwidth = 5)
ttt_hot <-  hist(x = tab_waves_hot_proverk$otnoshenie_posle, binwidth = 5)

hist_cold <- data.frame(middle = ttt_cold$mids, count = ttt_cold$counts)
hist_hot <- data.frame(middle = ttt_hot$mids, count = ttt_hot$counts)



hist_cold$type <- case_when(hist_cold$middle > 5 ~ "lag",
                            hist_cold$middle < 5 & hist_cold$middle > -5 ~ "no",
                            hist_cold$middle < -5 ~ "kosba")



hist_cold_st <- hist_cold %>% group_by(type) %>% summarise(sum_type = sum(count))



hist_hot$type <- case_when(hist_hot$middle > 5 ~ "lag",
                           hist_hot$middle < 5 & hist_hot$middle > -5 ~ "no",
                           hist_hot$middle < -5 ~ "kosba")



hist_hot_st <- hist_hot %>% group_by(type) %>% summarise(sum_type = sum(count))



hist_st <- rbind(hist_cold_st, hist_hot_st)

hist_st_vse <- hist_st %>% group_by(type) %>% summarise(sum_stype = sum(sum_type))


# отрисовка
plot_grid(
  
  ggplot() +
    
    geom_col(data = hist_cold, mapping = aes(x = hist_cold$middle, y = hist_cold$count), fill = "lightblue", color = "blue") +
    
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    scale_x_continuous(breaks = seq(-40, 50, 5)) +
    
    scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250)) +
    
    xlab("избыточные смерти\nза аналогичный период после волны,\nв % от аналогичного периода без волны ") +
    
    ylab("количество волн") +
    
    ggtitle("волны холода") +
    
    theme_bw() + 
    
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 9)),
  
  ggplot() +
    
    geom_col(data = hist_hot, mapping = aes(x = hist_hot$middle, y = hist_hot$count), fill = "pink", color = "red") +
    
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    scale_x_continuous(breaks = seq(-40, 50, 5)) +
    
    scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250)) +
    
    xlab("избыточные смерти\nза аналогичный период после волны,\nв % от аналогичного периода без волны ") +
    
    ylab("") +
    
    ggtitle("волны жары") +
    
    theme_bw() + 
    
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 9))
  
)



# ----- РАСЧЁТ ЧИСЛА ИЗБЫТОЧНО УМЕРШИХ ПО МЕТОДИКЕ ГАСПАРРИНИ. ЛАГ НЕ УЧИТЫВАЕТСЯ -----

tab_meteo_all <- read.csv("meteo_all.csv", sep = ";")

list_station_tab <- openxlsx::read.xlsx("meteostations_list.xlsx")

tab_met_st <- tab_meteo_all %>% filter(Station %in% list_station_tab$Meteostation_number)

rm(tab_meteo_all)

tab_met_st <- left_join(x = tab_met_st, y = list_station_tab %>% select(Meteostation_number, ROSBKS_code, Region_name_ROSBKS), by = c("Station" = "Meteostation_number"))


# меняем формат данных
tab_met_st$Date <- as.Date(tab_met_st$Date)

# переводим даты в номера недель по стандарту ISO
tab_met_st$week <- lubridate::isoweek(tab_met_st$Date)

# переводим даты в номера годов по стандарту ISO
tab_met_st$year <- lubridate::isoyear(tab_met_st$Date)



tab_met_st$TMIN <- as.numeric(str_replace(tab_met_st$TMIN, ",", "."))

tab_met_st$TMEAN <- as.numeric(str_replace(tab_met_st$TMEAN, ",", "."))

tab_met_st$TMAX <- as.numeric(str_replace(tab_met_st$TMAX, ",", "."))

tab_met_st$Height <- as.numeric(str_replace(tab_met_st$Height, ",", "."))



tab_met_week <- tab_met_st %>% group_by(week, year, ROSBKS_code) %>% summarise(tmean = mean(TMEAN, na.rm = T), tmin = min(TMIN, na.rm = T), tmax = max(TMAX, na.rm = T), index = first(Index), date = first(Date), name = first(Name), height = first(Height), ROSBKS_code = first(ROSBKS_code), Region_name_ROSBKS = first(Region_name_ROSBKS))

tab_met_week <- tab_met_week %>% group_by(ROSBKS_code, year) %>% arrange((week), .by_group = T)



tab_cdr_risk <- tab_cdr

colnames(tab_cdr_risk) <- str_to_lower(colnames(tab_cdr_risk))



ewe <- left_join(x = tab_met_week, y = tab_cdr_risk, by = c("week" = "week", "year" = "year", "ROSBKS_code" = "popcode"))

ewe <- ewe[!is.na(ewe$region), ]

ewe <- ewe %>% filter(!is.na(deads))



reg_list <- unique(ewe$Region_name_ROSBKS)
list_rr_all <- list()

for (z in c(1:70)) {
  
  ewe_reg <- ewe %>% filter(Region_name_ROSBKS == reg_list[z])
  
  ewe_reg <- ewe_reg %>% arrange(date)
  
  cbtemp <- crossbasis(ewe_reg$tmean, lag = 4,
                       argvar = list(fun = "ns", knots = quantile(ewe_reg$tmean, c(0.1, 0.5, 0.9))),
                       arglag = list(fun = "ns", df = 4))
  
  colnames(cbtemp) <- paste("temp", colnames(cbtemp), sep = "_")
  
  model <- glm(deads ~ cbtemp + ns(date, df = 8), 
               family = quasipoisson(), data = ewe_reg)
  
  tab_mmt <- data.frame(y_predict = model$fitted.values, tmean = model$data$tmean[-c(1:4)])
  
  mmt <- tab_mmt %>% arrange(y_predict) %>% slice(1) %>% .$tmean
  
  pred.temp <-  crosspred(cbtemp, model, lag = 4, by = 1, cen = mmt)
  
  data_rr_all <- data.frame(gradus = pred.temp$predvar, rr_predict = pred.temp$allRRfit, reg = reg_list[z])
  
  list_rr_all[[z]] <- data_rr_all
  
}



plot(x = pred.temp$predvar, y = pred.temp$allRRfit)



tab_rr_all <- rbindlist(list_rr_all)
tab_rr_all$reg <- str_trim(tab_rr_all$reg)



# ----- РИСУЕМ ДЛЯ ПРОВЕРКИ КРИВЫЕ ОТНОСИТЕЛЬНОГО РИСКА -----

ggplot() +
  
  geom_point(data = tab_rr_all %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tab_rr_all$gradus, y = tab_rr_all$rr_predict), shape = 21, col = "green", fill = "green", size = 0.4) +
  
  facet_wrap(facets = vars(reg), scale = "free_x", ncol = 9) +
  
  theme(text = element_text(size = 8),
        strip.text = element_text(
          size = 8,                    # small text size
          margin = margin(0.2, 0.2, 0.2, 0.2), # zero margins all around
          hjust = 0.5,                 # center text
          vjust = 0.5                  # center vertically
        ))



# ----- РАСЧЁТ КОЛИЧЕСТВО ИЗБЫТОЧНО УМЕРШИХ ЧЕРЕЗ ОТНОСИТЕЛЬНЫЙ РИСК ДЛЯ ВОЛН ХОЛОДА -----

tab_waves_cold_select$gradus <- round(tab_waves_cold_select$TMEAN_mean)

tab_waves_cold_select <- left_join(x = tab_waves_cold_select, y = tab_rr_all, by = c("gradus" = "gradus", "Region_name_ROSBKS" = "reg"))

tab_waves_cold_select$excess_hum_risk <- tab_waves_cold_select$deaths * (tab_waves_cold_select$rr_predict-1)

sum(tab_waves_cold_select$excess_hum_risk, na.rm = T)

sum(tab_cold_wave_pick$excess_hum, na.rm = T)



# ----- АГРЕГИРУЕМ КОЛИЧЕСТВО ИЗБЫТОЧНО УМЕРШИМ, РАССЧИТАННЫХ ЧЕРЕЗ РИСК ПО РЕГИОНАМ ДЛЯ ВОЛН ХОЛОДА -----

# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_waves_cold_select_reg_risk <- tab_waves_cold_select %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum_risk = sum(excess_hum_risk, na.rm = T))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_cold_risk <- left_join(tab_waves_cold_select_reg_risk, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_cold_risk$excess_pr_risk <- round(((tab_svod_region_cold_risk$excess_hum_risk / tab_svod_region_cold_peak$sum_deads) * 100), digits = 3)

# сохраняем результирующую таблицу
write.xlsx(x = tab_svod_region_cold_risk, file = "tab_svod_region_cold_95_risk.xlsx")



# ----- РАСЧЁТ КОЛИЧЕСТВО ИЗБЫТОЧНО УМЕРШИХ ЧЕРЕЗ ОТНОСИТЕЛЬНЫЙ РИСК ДЛЯ ВОЛН ЖАРЫ -----

tab_waves_hot_select$gradus <- round(tab_waves_hot_select$TMEAN_mean)

tab_waves_hot_select <- left_join(x = tab_waves_hot_select, y = tab_rr_all, by = c("gradus" = "gradus", "Region_name_ROSBKS" = "reg"))

tab_waves_hot_select$excess_hum_risk <- tab_waves_hot_select$deaths * (tab_waves_hot_select$rr_predict-1)

sum(tab_waves_hot_select$excess_hum_risk, na.rm = T)

sum(tab_hot_wave_pick$excess_hum, na.rm = T)



# ----- АГРЕГИРУЕМ КОЛИЧЕСТВО ИЗБЫТОЧНО УМЕРШИМ, РАССЧИТАННЫХ ЧЕРЕЗ РИСК ПО РЕГИОНАМ ДЛЯ ВОЛН ХОЛОДА -----

# в таблицу собираются данные, о числе избыточно умерших в пики смертей при волнах холода
tab_waves_hot_select_reg_risk <- tab_waves_hot_select %>% group_by(Region_name_ROSBKS) %>% summarise(excess_hum_risk = sum(excess_hum_risk, na.rm = T))

tab_svod_region <- tab_cdr %>% group_by(Region) %>% summarise(sum_deads = sum(deads))

# замены несовпадающих написаний названий регионов
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Кемеровская область", replacement = "Кемеровская область — Кузбасс") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Санкт-Петербург", replacement = "город Санкт-Петербург") 
tab_svod_region$Region <- str_replace(tab_svod_region$Region, 
                                      pattern = "Москва", replacement = "город Москва") 

# результирующая таблица по расчётам
tab_svod_region_hot_risk <- left_join(tab_waves_hot_select_reg_risk, tab_svod_region, by = c("Region_name_ROSBKS" = "Region"))

# избыточные смерти в приростах
tab_svod_region_hot_risk$excess_pr_risk <- round(((tab_svod_region_hot_risk$excess_hum_risk / tab_svod_region_hot_risk$sum_deads) * 100), digits = 3)

# сохраняем результирующую таблицу
write.xlsx(x = tab_svod_region_hot_risk, file = "tab_svod_region_hot_95_risk.xlsx")



# ----- ДАННЫЕ ДЛЯ ЯЩИКОВ С УСАМИ -----

test_cold_sel <- data.table::rbindlist(pick_cold_list, fill = TRUE, use.names = TRUE)
test_hot_sel <- data.table::rbindlist(pick_hot_list, fill = TRUE, use.names = TRUE)

test_cold_sel <- test_cold_sel %>% filter(excess_pr < 100)

test_cold_sel$gradus <- round(test_cold_sel$TMEAN_mean)
test_hot_sel$gradus <- round(test_hot_sel$TMEAN_mean)



# график соотношения избыточных смертей и длительности волн холода
plot(x = test_cold_sel$Count, y = test_cold_sel$excess_pr, cex = 0.1)
lines(y = rep(0,50), x = c(1:50), type = "l")



# график соотношения избыточных смертей и длительности волн жары
plot(x = test_hot_sel$Count, y = test_hot_sel$excess_pr, cex = 0.2)
lines(y = rep(0,50), x = c(1:50), type = "l")


# ----- ЯЩИКИ С УСАМИ -----



usy_zara <-
  # построение ящика с усами по жаре
  ggplot() +
  geom_boxplot(test_hot_sel %>% arrange(Count), mapping = aes(x = factor(x = Count, levels = unique(Count)), y = excess_pr)) +
  ggtitle("Избыточные смерти от волн жары") +
  xlab("длительность волны, дней") +
  ylab("избыточные смерти,\nв % от аналогичного периода без волны") + theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 9))



usy_holod <-
  # построение ящика с усами по холоду
  ggplot() +
  geom_boxplot(test_cold_sel %>% arrange(Count),
               mapping = aes(x = factor(x = Count, levels = unique(Count)), y = excess_pr)) +
  ggtitle("Избыточные смерти от волн холода") +
  xlab("длительность волны, дней") +
  ylab("избыточные смерти,\nв % от аналогичного периода без волны") + theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 9))



plot_grid(usy_zara, usy_holod, ncol = 1)



# ----- ГРАФИК ДИНАМИКИ ПО ГОДАМ -----
test_god_cold_year <- test_cold_sel %>% group_by(Year_RosBKS_start) %>% summarise(sum_year = sum(excess_hum, na.rm = T))
test_god_hot_year <- test_hot_sel %>% group_by(Year_RosBKS_start) %>% summarise(sum_year = sum(excess_hum, na.rm = T))

test_god_cold_year$type <- "cold"
test_god_hot_year$type <- "hot"

test_god_year <- rbind(test_god_cold_year, test_god_hot_year)

ggplot() + 
  
  geom_col(data = test_god_year, mapping = aes(x = Year_RosBKS_start, y = sum_year/1000, fill = type), position = "stack") + 
  
  scale_fill_manual(values = c("steelblue", "salmon"), breaks = c("cold", "hot"), labels = c("холод", "жара"), name = "тип волны") +
  
  xlab("год") +
  
  ylab("количество избыточно умерших\nв волны холода и жары, тыс. чел.") +
  
  theme_bw() + 
  
  theme(text = element_text(size = 11))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------- ДАЛЕЕ ПЕРЕХОДИТЕ К ШАГУ 3 -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #