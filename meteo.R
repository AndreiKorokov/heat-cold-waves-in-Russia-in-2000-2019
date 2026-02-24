# --- UTF-8 ---

# Разработано в июне 2025 — январе 2026 года А. Коротковым (МГУ) и Н. Синицыным (МГУ)

# при копировании любой части материалов ссылка обязательна

# выполняйте код последовательно

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------- ШАГ 1 МЕТЕОДАННЫЕ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ---------------------------- ОБЩИЕ УСТАНОВКИ ------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# библиотеки
library(openxlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)

setwd("D:/Data/wave")



# путь к исходному файлу
path <- "meteo_all.csv"

# загрузка исходного файла
tab_meteo_all <- read.csv(path, sep = ";")

# перевод дат в формат DATE
tab_meteo_all$Date <- as.Date.character(tab_meteo_all$Date, format = "%Y-%m-%d")

# меняем формат данных с character на numeric
tab_meteo_all <- mutate_at(tab_meteo_all,
                           .vars = vars(TMIN, TMAX, TMEAN, Height), # столбцы
                           .funs = function(x){as.numeric(gsub(",", ".", x))}) # замена запятых на точки



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ ВВОД ДАННЫХ --------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# ввод перцентиля для волн жары и холода
# ВНИМАНИЕ! Указывать всегда перцентиль от большего. Не важно, речь о холоде или о жаре
# (если требуется 3% — писать 97, если требуется 10% — писать 90)

percentil_input <- 95 # пример: percentil_input <- 90, значит, отбирается 10% экстремальных значений



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ ОСНОВНОЙ ЦИКЛ ------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# список релевантных метеостанций для каждого из отобранных регионов
list_station_tab <- read.xlsx("meteostations_list.xlsx")

# список индексов релевантных метеостанций
list_station <- unique(list_station_tab$Meteostation_number)

# создаем листы, куда будут записываться волны жары и холода по каждой метеостанции по каждому дню (days) или по каждой волне (waves)
list_days_hot <- list()
list_days_cold <- list()
list_waves_hot <- list()
list_waves_cold <- list()



# сам цикл. Он проходит по каждой метеостанции из списка
for (a in c(1:length(list_station))) {
  
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # ---------------------------- ПОДГОТОВКА ДАННЫХ ----------------------------
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  
  
  # если на предыдущем этапе указан неправильный перцентиль, то выдается ошибка и выполнение цикла прекращается
  if (percentil_input > 99.9 | percentil_input < 80) {
    
    stop("Specify the correct percentil. Check your 'percentil_input'")
    
  }
  
  # фильтрация метеостанции из списка
  tab_station_all <- filter(tab_meteo_all, Index == list_station[a]) # например, 27612 — Индекс метеостанции ВДНХ (Москва)
  
  # фильтрация нужных дат (с 2000 по 2019 год)
  tab_station_clip <- filter(tab_station_all, Date >= "2000-01-01" & Date <= "2019-12-31")
  
  # если в эти даты у метеостанции не было наблюдений, то она пропускается
  if (nrow(tab_station_clip) == 0) {
    
    next
    
  }
  
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # ------------------------------ БЛОК ЖАРА ----------------------------------
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  
  
  # таблица по убыванию максимальной температуры (от самых жарких дней к самым холодным)
  tab_station_hot <- tab_station_clip %>% arrange(desc(TMAX))
  
  # ввод перцентиля. Округление результата вниз. На выходе получается число дней
  percentil <- floor(nrow(tab_station_hot) * (100 - percentil_input) / 100)
  
  # отбираем самые жаркие дни по сответствующему перцентилю
  tab_station_hot_95 <- tab_station_hot[1 : percentil, ]
  
  # сортировка самых жарких дней в хронологическом порядке
  tab_station_hot_95 <- tab_station_hot_95 %>% arrange(Date)
  
  # создаем столбец, в который будет записываться метка о волне жары
  tab_station_hot_95$Waves <- ""
  
  # переменная числа строк в таблице (дней)
  n_row <- nrow(tab_station_hot_95)
  
  # цикл с условиями выполнения определения "волна жары продолжается более 2 дней"
  for (x in c(1 : n_row)) {
    
    t0 <- tab_station_hot_95$Date[x]
    
    if (x <= n_row - 2) {
      
      t1 <- tab_station_hot_95$Date[x + 1]
      t2 <- tab_station_hot_95$Date[x + 2]
      dif1 <- (t1 - t0) %>% as.numeric()
      dif2 <- (t2 - t0) %>% as.numeric()
      
      if (dif1 == 1 & dif2 == 2) {
        
        tab_station_hot_95$Waves[x] <- "Волна жары"
        
      }
      
    }
    
    if (x >= 3) {
      
      t_1 <- tab_station_hot_95$Date[x - 1]
      t_2 <- tab_station_hot_95$Date[x - 2]
      dif_1 <- (t0 - t_1) %>% as.numeric()
      dif_2 <- (t0 - t_2) %>% as.numeric()
      
      if (dif_1 == 1 & dif_2 == 2) {
        
        tab_station_hot_95$Waves[x] <- "Волна жары"
        
      }
      
    }
    
    if (x != 1 & x != n_row) {
      
      t1 <- tab_station_hot_95$Date[x + 1]
      t_1 <- tab_station_hot_95$Date[x - 1]
      dif1 <- (t1 - t0) %>% as.numeric()
      dif_1 <- (t0 - t_1) %>% as.numeric()
      
      if (dif1 == 1 & dif_1 == 1) {
        
        tab_station_hot_95$Waves[x] <- "Волна жары"
        
      }
      
    }
    
  }
  
  if (tab_station_hot_95$Waves[1] == "Волна жары") {
    
    z <- 1 #переменная счетчика волн
    
  }
  
  if (tab_station_hot_95$Waves[1] != "Волна жары") {
    
    z <- 0 #переменная счетчика волн
    
  }
  
  # цикл расставляет номера волн жары
  for (y in c(1 : n_row)) {
    
    ty <- tab_station_hot_95$Waves[y]
    ty_1 <- tab_station_hot_95$Waves[y + 1]
    
    if (y <= n_row - 1) {
      
      if (ty != "Волна жары" & ty_1 == "Волна жары") {
        
        z <- z + 1
        
      }
      
      if (ty == "Волна жары") {
        
        tab_station_hot_95$Waves[y] <- paste0("Волна жары ", z)
        
      }
      
      if (as.numeric(tab_station_hot_95$Date[y + 1] - tab_station_hot_95$Date[y]) != 1 & ty == "Волна жары" & ty_1 == "Волна жары") {
        
        z <- z + 1
        
      }
      
    }
    
    if (y == n_row) {
      
      if (ty == "Волна жары") {
        
        tab_station_hot_95$Waves[y] <- paste0("Волна жары ", z)
        
      }
      
    }
    
  }
  
  # удаляем строки без метки "Волна жары"
  tab_station_hot_95 <- tab_station_hot_95 %>% filter(Waves != "")
  
  # таблица с перечислением волн жары, их начала и окончания, продолжительности, дней с ночными максимумами и дневными максимумами
  station_waves_hot <- tab_station_hot_95 %>% group_by(Waves) %>% summarise(Date_start = first(Date),
                                                                            Date_last = last(Date),
                                                                            Count = n(),
                                                                            Station = first(Station),
                                                                            Index = first(Index),
                                                                            Name = first(Name),
                                                                            Height = first(Height),
                                                                            TMIN_mean = mean(TMIN, na.rm = T),
                                                                            TMEAN_mean = mean(TMEAN, na.rm = T),
                                                                            TMAX_mean = mean(TMAX, na.rm = T),
                                                                            TMIN_absolut = max(TMIN, na.rm = T),
                                                                            TMAX_absolut = max(TMAX, na.rm = T),
                                                                            geometry = first(geometry)) %>% arrange(Date_start)
  
  # создает вспомогательную таблицу с датами самых жарких дней каждой из волн
  tab_day_max <- tab_station_hot_95 %>% group_by(Waves) %>% arrange(desc(TMAX), .by_group = TRUE) %>% summarise(Date_TMAX_absolut = first(Date))
  tab_day_min <- tab_station_hot_95 %>% group_by(Waves) %>% arrange(desc(TMIN), .by_group = TRUE) %>% summarise(Date_TMIN_absolut = first(Date))
  
  # соединяем две таблицы по общему столбцу
  station_waves_hot <- left_join(x = station_waves_hot, y = tab_day_max, by = "Waves")
  station_waves_hot <- left_join(x = station_waves_hot, y = tab_day_min, by = "Waves")
  
  # перенос столбцов для удобства
  station_waves_hot <- relocate(.data = station_waves_hot, Date_TMIN_absolut, .before = geometry)
  station_waves_hot <- relocate(.data = station_waves_hot, Date_TMAX_absolut, .before = geometry)
  
  # устанавливаем год и номера недель для увязывания с базой РосБКС
  station_waves_hot <- station_waves_hot %>% mutate(Week_RosBKS_start = isoweek(Date_start), Week_RosBKS_last = isoweek(Date_last)) # только номера недель
  station_waves_hot <- station_waves_hot %>% mutate(Year_RosBKS_start = isoyear(Date_start), Year_RosBKS_last = isoyear(Date_last)) # год
  
  # перенос столбцов для удобства
  station_waves_hot <- station_waves_hot %>% relocate(Year_RosBKS_start, .before = Count) %>% relocate(Year_RosBKS_last, .before = Count)
  station_waves_hot <- station_waves_hot %>% relocate(Week_RosBKS_start, .before = Count) %>% relocate(Week_RosBKS_last, .before = Count)
  
  # сохраняем данные о волнах жары и днях с волнами жары в листы
  list_days_hot[[a]] <- tab_station_hot_95
  list_waves_hot[[a]] <- station_waves_hot
  
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # ----------------------------- БЛОК ХОЛОД ----------------------------------
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  
  
  # таблица по убыванию миксимальной температуры (от самых холодных дней к самым жарким)
  tab_station_cold <- tab_station_clip %>% arrange(TMIN)
  
  # ввод перцентиля. Округление результата вниз. На выходе получается число дней
  percentil <- floor(nrow(tab_station_cold) * (100 - percentil_input) / 100)
  
  # отбираем самые холодные дни по сответствующему перцентилю
  tab_station_cold_95 <- tab_station_cold[1 : percentil, ]
  
  # сортировка самых холодных дней в хронологическом порядке
  tab_station_cold_95 <- tab_station_cold_95 %>% arrange(Date)
  
  # создаем столбец, в который будет записываться метка о волне холода
  tab_station_cold_95$Waves <- ""
  
  # переменная числа строк в таблице (дней)
  n_row <- nrow(tab_station_cold_95)
  
  # цикл с условиями выполнения определения "волна холода продолжается от 3 дней и более"
  for (x in c(1 : n_row)) {
    
    t0 <- tab_station_cold_95$Date[x]
    
    if (x <= n_row - 2) {
      
      t1 <- tab_station_cold_95$Date[x + 1]
      t2 <- tab_station_cold_95$Date[x + 2]
      dif1 <- (t1 - t0) %>% as.numeric()
      dif2 <- (t2 - t0) %>% as.numeric()
      
      if (dif1 == 1 & dif2 == 2) {
        
        tab_station_cold_95$Waves[x] <- "Волна холода"
        
      }
      
    }
    
    if (x >= 3) {
      
      t_1 <- tab_station_cold_95$Date[x - 1]
      t_2 <- tab_station_cold_95$Date[x - 2]
      dif_1 <- (t0 - t_1) %>% as.numeric()
      dif_2 <- (t0 - t_2) %>% as.numeric()
      
      if (dif_1 == 1 & dif_2 == 2) {
        
        tab_station_cold_95$Waves[x] <- "Волна холода"
        
      }
      
    }
    
    if (x != 1 & x != n_row) {
      
      t1 <- tab_station_cold_95$Date[x + 1]
      t_1 <- tab_station_cold_95$Date[x - 1]
      dif1 <- (t1 - t0) %>% as.numeric()
      dif_1 <- (t0 - t_1) %>% as.numeric()
      
      if (dif1 == 1 & dif_1 == 1) {
        
        tab_station_cold_95$Waves[x] <- "Волна холода"
        
      }
      
    }
    
  }
  
  if (tab_station_cold_95$Waves[1] == "Волна холода") {
    
    z <- 1 #переменная счетчика волн
    
  }
  
  if (tab_station_cold_95$Waves[1] != "Волна холода") {
    
    z <- 0 #переменная счетчика волн
    
  }
  
  # цикл расставляет номера волн холода
  for (y in c(1 : n_row)) {
    
    ty <- tab_station_cold_95$Waves[y]
    ty_1 <- tab_station_cold_95$Waves[y + 1]
    
    if (y <= n_row - 1) {
      
      if (ty != "Волна холода" & ty_1 == "Волна холода") {
        
        z <- z + 1
        
      }
      
      if (ty == "Волна холода") {
        
        tab_station_cold_95$Waves[y] <- paste0("Волна холода ", z)
        
      }
      
      if (as.numeric(tab_station_cold_95$Date[y + 1] - tab_station_cold_95$Date[y]) != 1 & ty == "Волна холода" & ty_1 == "Волна холода") {
        
        z <- z + 1
        
      }
      
    }
    
    if (y == n_row) {
      
      if (ty == "Волна холода") {
        
        tab_station_cold_95$Waves[y] <- paste0("Волна холода ", z)
        
      }
      
    }
    
  }
  
  # удаляем строки без метки "Волна холода"
  tab_station_cold_95 <- tab_station_cold_95 %>% filter(Waves != "")
  
  # таблица с перечислением волн холода, их начала и окончания, продолжительности, дней с ночными минимумами
  station_waves_cold <- tab_station_cold_95 %>% group_by(Waves) %>% summarise(Date_start = first(Date),
                                                                              Date_last = last(Date),
                                                                              Count = n(),
                                                                              Station = first(Station),
                                                                              Index = first(Index),
                                                                              Name = first(Name),
                                                                              Height = first(Height),
                                                                              TMIN_mean = mean(TMIN, na.rm = T),
                                                                              TMEAN_mean = mean(TMEAN, na.rm = T),
                                                                              TMAX_mean = mean(TMAX, na.rm = T),
                                                                              TMIN_absolut = min(TMIN, na.rm = T),
                                                                              geometry = first(geometry)) %>% arrange(Date_start)
  
  # создает вспомогательную таблицу с датами самых холодных дней каждой из волн
  tab_day_min <- tab_station_cold_95 %>% group_by(Waves) %>% arrange(desc(TMIN), .by_group = TRUE) %>% summarise(Date_TMIN_absolut = first(Date))
  
  # соединяем две таблицы по общему столбцу
  station_waves_cold <- left_join(x = station_waves_cold, y = tab_day_min, by = "Waves")
  
  # перенос столбца для удобства
  station_waves_cold <- relocate(.data = station_waves_cold, Date_TMIN_absolut, .before = geometry)
  
  # устанавливаем год и номера недель для увязывания с базой РосБКС
  station_waves_cold <- station_waves_cold %>% mutate(Week_RosBKS_start = isoweek(Date_start), Week_RosBKS_last = isoweek(Date_last)) # только номера недель
  station_waves_cold <- station_waves_cold %>% mutate(Year_RosBKS_start = isoyear(Date_start), Year_RosBKS_last = isoyear(Date_last)) # год
  
  # перенос столбцов для удобства
  station_waves_cold <- station_waves_cold %>% relocate(Year_RosBKS_start, .before = Count) %>% relocate(Year_RosBKS_last, .before = Count)
  station_waves_cold <- station_waves_cold %>% relocate(Week_RosBKS_start, .before = Count) %>% relocate(Week_RosBKS_last, .before = Count)
  
  # сохраняем данные о волнах холода и днях с волнами холода в листы
  list_days_cold[[a]] <- tab_station_cold_95
  list_waves_cold[[a]] <- station_waves_cold
  
  # счетчик показывает на каком этапе находится процесс. Отображается каждый десятый проход цикла из всех запланированных
  if (a %% 10 == 0) {
    
    print(paste("Processing completed for", a, "from", length(list_station), "weather stations"))
    
  }
  
}



# сохраняются сводные таблицы по дням с аномальными температурами (дни) из листов
tab_days_hot <- rbindlist(l = list_days_hot, use.names = TRUE)
tab_days_cold <- rbindlist(l = list_days_cold, use.names = TRUE)

# сохраняются сводные таблицы по эпизодам аномальных температур (волны) из листов
tab_waves_hot <- rbindlist(l = list_waves_hot, use.names = TRUE)
tab_waves_cold <- rbindlist(l = list_waves_cold, use.names = TRUE)

# сохраняем результирующие таблицы
output_days_hot <- tab_days_hot
output_days_cold <- tab_days_cold
output_waves_hot <- tab_waves_hot
output_waves_cold <- tab_waves_cold

# обогащаем данные метеостанций ключами для РосБКС
output_days_hot <- left_join(x = output_days_hot, y = list_station_tab, by = c("Index" = "Meteostation_number"))
output_days_cold <- left_join(x = output_days_cold, y = list_station_tab, by = c("Index" = "Meteostation_number"))
output_waves_hot <- left_join(x = output_waves_hot, y = list_station_tab, by = c("Index" = "Meteostation_number"))
output_waves_cold <- left_join(x = output_waves_cold, y = list_station_tab, by = c("Index" = "Meteostation_number"))


# сохраняем в табличном виде итоговые таблицы
write.xlsx(output_days_hot, "output_days_hot_95per.xlsx")
write.xlsx(output_days_cold, "output_days_cold_95per.xlsx")
write.xlsx(output_waves_hot, "output_waves_hot_95per.xlsx")
write.xlsx(output_waves_cold, "output_waves_cold_95per.xlsx")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------- ДАЛЕЕ ПЕРЕХОДИТЕ К ШАГУ 2 -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #