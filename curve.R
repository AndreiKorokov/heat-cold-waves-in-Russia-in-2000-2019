# --- UTF-8 ---

# Разработано в июне 2025 — январе 2026 года А. Коротковым (МГУ) и Н. Синицыным (МГУ)

# при копировании любой части материалов ссылка обязательна

# выполняйте код последовательно

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ------------------------------ ШАГ 4 КРИВЫЕ -------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# переводим даты в номера недель по стандарту ISO
tab_meteo_all$week <- lubridate::isoweek(tab_meteo_all$Date)

# переводим даты в номера годов по стандарту ISO
tab_meteo_all$year <- lubridate::isoyear(tab_meteo_all$Date)

# берём только необходимые метеостанции
tab_met_st <- tab_meteo_all %>% filter(Station %in% list_station_tab$Meteostation_number)

# присоединяем названия метеостанций
tab_met_st <- left_join(x = tab_met_st, y = list_station_tab %>% select(Meteostation_number, ROSBKS_code, Region_name_ROSBKS), by = c("Station" = "Meteostation_number"))

# данные по смертности
colnames(tab_cdr) <- str_to_lower(colnames(tab_cdr))



# по каждой неделе по каждой метеостанции считаем среднюю и другие температуры
tab_met_week <- tab_met_st %>% group_by(week, year, ROSBKS_code) %>% summarise(tmean = mean(TMEAN, na.rm = T), tmin = min(TMIN, na.rm = T), tmax = max(TMAX, na.rm = T), index = first(Index), date = first(Date), name = first(Name), height = first(Height), ROSBKS_code = first(ROSBKS_code), Region_name_ROSBKS = first(Region_name_ROSBKS))

# сортируем по коду даты
tab_met_week <- tab_met_week %>% group_by(ROSBKS_code, year) %>% arrange((week), .by_group = T)

# присоединяем к метеоданным количество умерших в неделю
ewe <- left_join(x = tab_met_week, y = tab_cdr, by = c("week" = "week", "year" = "year", "ROSBKS_code" = "popcode"))
ewe <- ewe[!is.na(ewe$region), ]
ewe$Region_name_ROSBKS <- str_replace_all(ewe$Region_name_ROSBKS, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))


# ----- РАССМАТРИВАЕМ ДЛЯ ПРИМЕРА ОДИН РЕГИОН -----
ewe_reg <- ewe %>% filter(Region_name_ROSBKS == "Саратовская область")

# соотношение температуры и количества смертей
plot(x = ewe_reg$tmean, y = ewe_reg$deads, cex = 0.1)

# соотношение по каждому региону
ggplot() +
  
  geom_point(data = ewe, mapping = aes(x = tmean, y = deads), shape = ".") +
  
  facet_wrap(vars(Region_name_ROSBKS), scales = "free")



# ----- РАСЧЁТ ПО МЕТОДИКЕ ГАСПАРРИНИ ДЛЯ ОДНОГО РЕГИОНА -----
ewe_reg <- ewe %>% filter(Region_name_ROSBKS == "Ленинградская область")
plot(x = ewe_reg$tmean, y = ewe_reg$deads, cex = 1)



# ----- ДЕЛАЕМ КРОСС-БАЗИС СПЛАЙНА ТЕМПЕРАТУРЫ -----
cbtemp <- crossbasis(ewe_reg$tmean, lag = 0,
                     argvar = list(fun = "ns", knots = quantile(ewe_reg$tmean, c(0.1, 0.5, 0.9))),
                     arglag = list(fun = "ns", df = 4))

colnames(cbtemp) <- paste("temp", colnames(cbtemp), sep = "_")



# ----- ДЕЛАЕМ ЛИНЕЙНУЮ ОБОБЩЁННУЮ МОДЕЛЬ ИЗ КРОСС-БАЗИСА И СПЛАЙНА ДАТЫ -----
model <- glm(deads ~ cbtemp + ns(date, df = 8), 
             family = quasipoisson(), data = ewe_reg)

plot(x = model$data$deads, y = model$fitted.values)

plot(x = model$data$tmean, y = model$fitted.values)

plot(x = model$data$date, y = model$data$deads, type = "l")

lines(x = model$data$date, y = model$fitted.values, type = "l", col = "red")



# ----- ИЩЁМ ММТ (температура минимальной смертности) -----
tab_mmt <- data.frame(y_predict = model$fitted.values, tmean = model$data$tmean)

mmt <- tab_mmt %>% arrange(y_predict) %>% slice(1) %>% .$tmean



# ----- СЧИТАЕМ ОТНОСИТЕЛЬНЫЙ РИСК -----
pred.temp <-  crosspred(basis = cbtemp, lag = 0, model = model, by = 1, cen = mmt)

plot(x = pred.temp$predvar, y = pred.temp$allRRfit/min(pred.temp$allRRfit))

ggplot() +
  
  geom_point(data = ewe_reg, mapping = aes(x = tmean, y = deads), shape = ".") +
  
  geom_line(mapping = aes(x = pred.temp$predvar, y = pred.temp$allRRfit * 1000), shape = 21, fill = "red", color = "red")



# сплайн простой один регион
ewe_ge <- ewe %>% filter(Region_name_ROSBKS == reg_list[w])

spline_standart <-  lm(ewe_ge$deads ~ ns(x = ewe_ge$tmean, df = 5))

ewe_ge$spline_standartniy <- predict(object = spline_standart, ewe_ge[, "tmean"])

points(x = ewe_ge$tmean, y = ewe_ge$spline_standartniy, col = "red")



# ----- ПРОСТОЙ СПЛАЙН ПО ВСЕМ ДАННЫМ -----
spline_simple_list <- list()
reg_list <- unique(ewe$Region_name_ROSBKS)

for (w in c(1:69)) {
  
  ewe_reg <- ewe %>% filter(Region_name_ROSBKS == reg_list[w])
  
  spline <-  lm(ewe_reg$deads ~ ns(x = ewe_reg$tmean, df = 5, knots = quantile(ewe_reg$tmean, c(0.1, 0.5, 0.9))))
  
  ewe_reg$deads_predict <- predict(object = spline, ewe_reg[, "tmean"])
  
  ewe_reg$rr_predict <- ewe_reg$deads_predict/min(ewe_reg$deads_predict)
  
  tab_simple <- data.frame(gradus = ewe_reg$tmean, rr_predict = ewe_reg$rr_predict, reg = reg_list[w])
  
  spline_simple_list[[w]] <- tab_simple
  
}

spline_simple_table <- rbindlist(spline_simple_list)

spline_simple_table$region <- str_replace_all(spline_simple_table$region, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))



# ----- МЕТОДИКА ГАСПАРРИНИ ПО ВСЕМ ДАННЫМ -----
reg_list <- unique(ewe$Region_name_ROSBKS)
list_rr_all <- list()

for (z in c(1:69)) {
  
  ewe_reg <- ewe %>% filter(Region_name_ROSBKS == reg_list[z])
  
  ewe_reg <- ewe_reg %>% arrange(date)
  
  cbtemp <- crossbasis(ewe_reg$tmean, lag = 0,
                       argvar = list(fun = "ns", knots = quantile(ewe_reg$tmean, c(0.1, 0.5, 0.9))),
                       arglag = list(fun = "ns", df = 4))
  
  colnames(cbtemp) <- paste("temp", colnames(cbtemp), sep = "_")
  
  model <- glm(deads ~ cbtemp + ns(date, df = 8), 
               family = quasipoisson(), data = ewe_reg)
  
  tab_mmt <- data.frame(y_predict = model$fitted.values, tmean = model$data$tmean)
  
  mmt <- tab_mmt %>% arrange(y_predict) %>% slice(1) %>% .$tmean
  
  pred.temp <-  crosspred(cbtemp, model, lag = 0, by = 1, cen = mmt)
  
  data_rr_all <- data.frame(gradus = pred.temp$predvar, rr_predict = pred.temp$allRRfit, reg = reg_list[z])
  
  data_rr_all$predict_value <- data_rr_all$rr_predict * mean(tab_mmt$y_predict)
  
  list_rr_all[[z]] <- data_rr_all
  
}



tab_rr_all <- rbindlist(list_rr_all)
tab_rr_all$reg <- str_replace_all(tab_rr_all$reg, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))



# ----- РИСУЕМ СПЛАЙН И ГАСПАРРИНИ ПО ВСЕМ ДАННЫМ -----
ggplot() +
  
  geom_point(data = tab_rr_all %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tab_rr_all$gradus, y = tab_rr_all$rr_predict), shape = 21, col = "green", fill = "green", size = 0.4) +
  
  geom_point(data = spline_simple_table %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = spline_simple_table$gradus, y = spline_simple_table$rr_predict), shape = ".", col = "brown", fill = "brown", size = 0.1) +
  
  facet_wrap(facets = vars(reg), scale = "free_x", ncol = 9) +
  
  theme(text = element_text(size = 8),
        strip.text = element_text(
          size = 8,                    # small text size
          margin = margin(0.2, 0.2, 0.2, 0.2), # zero margins all around
          hjust = 0.5,                 # center text
          vjust = 0.5                  # center vertically
        ))



# ----- ДЕЛАЕМ МЕТОДИКУ ГАСПАРРИНИ ПО ТЕМПЕРАТУРЕ ОТ 5 ДО 95 ПРОЦЕНТИЛЯ -----
list_rr_osnov <- list()

for (z in c(1:69)) {
  
  ewe_reg <- ewe %>% filter(Region_name_ROSBKS == reg_list[z])
  
  
  ewe_reg <- ewe_reg %>% arrange(desc(tmean)) %>% .[-c(c(1:round(nrow(ewe_reg)*0.05), c((round(nrow(ewe_reg)*0.95)) : round(nrow(ewe_reg))))), ]
  
  ewe_reg <- ewe_reg %>% arrange(date)
  
  cbtemp <- crossbasis(ewe_reg$tmean, lag = 0,
                       argvar = list(fun = "ns", knots = quantile(ewe_reg$tmean, c(0.1, 0.5, 0.9))),
                       arglag = list(fun = "ns", df = 4))
  
  colnames(cbtemp) <- paste("temp", colnames(cbtemp), sep = "_")
  
  model <- glm(deads ~ cbtemp + ns(date, df = 8), 
               family = quasipoisson(), data = ewe_reg)
  
  tab_mmt <- data.frame(y_predict = model$fitted.values, tmean = model$data$tmean)
  
  mmt <- tab_mmt %>% arrange(y_predict) %>% slice(1) %>% .$tmean %>% round()
  
  pred.temp <-  crosspred(cbtemp, model, lag = 0, by = 1, cen = mmt)
  
  data_rr_osnov <- data.frame(tmean = pred.temp$predvar, rr_osnov = pred.temp$allRRfit, reg = reg_list[z])
  
  data_rr_osnov$predict_value <- data_rr_osnov$rr_osnov * mean(tab_mmt$y_predict)
  
  list_rr_osnov[[z]] <- data_rr_osnov
  
}

tab_rr_osnov <- rbindlist(list_rr_osnov)
tab_rr_osnov$reg <- str_replace_all(tab_rr_osnov$reg, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))



# ----- ДЕЛАЕМ ЛИНИЙНУЮ РЕГИРЕССИЮ ПО ТЕМПЕРАТУРЕ ОТ 5 ДО 95 ПРОЦЕНТИЛЯ -----
reg_list <- unique(ewe$Region_name_ROSBKS)

list_line <- list()

for (z in c(1:69)) {
  ewe_reg <- ewe %>% filter(Region_name_ROSBKS == reg_list[z])
  
  ewe_reg <- ewe_reg %>% arrange(desc(tmean)) %>% .[-c(c(1:round(nrow(ewe_reg)*0.05), c((round(nrow(ewe_reg)*0.95)) : round(nrow(ewe_reg))))), ]
  
  ewe_reg <- ewe_reg %>% arrange(date)
  
  reg_line <- lm(ewe_reg$deads ~ ewe_reg$tmean)
  
  data_line <- data.frame(tmean = ewe_reg$tmean, reg_line = reg_line$fitted.values, reg = reg_list[z])
  
  list_line[[z]] <- data_line
  
  list_line_fitvalue[[z]] <- data_fitvalue
  
}

tab_line <- rbindlist(list_line)
tab_line$reg <- str_replace_all(tab_line$reg, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))

mean_deads_line <- tab_line %>% group_by(reg) %>% summarise(min_reg = min(reg_line))

tab_line <- left_join(x = tab_line, y = mean_deads_line, by = c("reg" = "reg"))

tab_line$deads_otnosit <- tab_line$reg_line / tab_line$min_reg

tab_line_fitvalue <- rbindlist(list_line_fitvalue)
tab_line_fitvalue$reg <- str_replace_all(tab_line_fitvalue$reg, c("Кемеровская область — Кузбасс" = "Кемеровская область", "Еврейская автономная область" = "Еврейская авт. область"))



# ОТНОСИТЕЛЬНЫЙ РИСК - ЭТО СМЕРТНОСТЬ ОПРЕДЕЛЁННОГО ГРАДУСА ДЕЛИТЬ НА СМЕРТНОСТЬ ММТ
# ЧТОБЫ ТОЧКИ КОЛИЧЕСТВА СМЕРТЕЙ БЫЛИ В ТОМ ЖЕ МАСШТАБЕ, ВЫРАЗИМ ИХ ЧЕРЕЗ ОТНОШЕНИЕ К СРЕДНЕЙ СМЕРТНОСТИ ПО РЕГИОНУ
# ТОГДА ОТНОСИТЕЛЬНЫЙ РИСК И СМЕРТНОСТЬ СТАНУТ В ДОЛЯХ ОТ ЕДИНИЦЫ

ewe_point <- ewe %>% rename(reg = Region_name_ROSBKS)

mean_deads <- ewe_point %>% group_by(reg) %>% summarise(mean_reg = mean(deads))

ewe_point <- left_join(x = ewe_point, y = mean_deads, by = c("reg" = "reg"))

ewe_point$deads_otnosit <- ewe_point$deads / ewe_point$mean_reg


# отдельно фильтруем температуру до 5 и после 95 процентиля
ewe_vibrosy <- ewe_point %>% group_by(reg) %>% arrange(tmean, .by_group = T) %>% slice(c(c(1:round(n()*0.05), c((round(n()*0.95)) : round(n())))))




# РИСУЕМ СМЕРТНОСТЬ В НЕДЕЛЮ, СНАЧАЛА ВСЮ СЕРУЮ, ПОТОМ ДО 5 И ПОСЛЕ 95 ПРОЦЕНТИЛЯ ЧЕРНЫМ ЦВЕТОМ
# ДАЛЬШЕ ДОБАВЛЯЕМ ТОЧКИ ИНТЕРПОЛАЦИИ ПО МЕТОДИКЕ ГАСПАРРИНИ ПО ВСЕМ ДАННЫМ
# ПОТОМ ИНТЕРПОЛЯЦИЮ ПО МЕТОДИКЕ ГАСПАРРИНИ ПО ТОЧКАМ ОТ 5 ДО 95 ПРОЦЕНТИЛЯ ТЕМПЕРАТУРЫ
# ПОТОМ ИНТЕРОЛЯЦИЮ ПО ЛИНЕЙНОЙ ФУНКЦИИ ПО ТОЧКАМИ ОТ 5 ДО 95 ПРОРЦЕНТИЛЯ ТЕМПЕРАТУРЫ

ggplot() +
  
  geom_point(data = ewe_point %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tmean, y = deads_otnosit), shape = 20, color = "grey60", size = 0.001) +
  
  geom_point(data = ewe_vibrosy %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tmean, y = deads_otnosit), shape = 20, color = "grey20", size = 0.001) +
  
  geom_point(data = tab_rr_all %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = gradus, y = rr_predict), shape = 21, fill = "green", color = "green", size = 1) +
  
  geom_point(data = tab_rr_osnov %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tmean, y = rr_osnov), shape = 21, fill = "blue", color = "blue", size = 0.8) +
  
  geom_point(data = tab_line %>% mutate(reg = str_remove_all(reg, "область|край|[Рр]еспублика|город |авт\\.| |Санкт-")), mapping = aes(x = tmean, y = deads_otnosit), shape = 21, fill = "magenta", color = "magenta", size = 0.1) +
  
  facet_wrap(vars(reg), scales = "free", ncol = 8) +
  
  theme_bw() +
  
  theme(text = element_text(size = 8),
        strip.text = element_text(
          size = 8,                    # small text size
          margin = margin(0.2, 0.2, 0.2, 0.2), # zero margins all around
          hjust = 0.5,                 # center text
          vjust = 0.5                  # center vertically
        ))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------- ЗАВЕРШЕНИЕ РАБОТЫ ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
