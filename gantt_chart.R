# Диаграмма Ганта для планирования исследований ------------------------
## Код: https://insileco.github.io/2017/09/20/gantt-charts-in-r/
## Заголовок диаграммы на строке 141

# Пакеты ---------------------------------------------------------------
library(openxlsx)
# library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(dplyr)

# Данные ---------------------------------------------------------------
df <- read.xlsx("~/data/research_plan.xlsx", detectDates = TRUE)
# Преобразование дат
df$startDate <- as.Date(df$startDate, origin = "1899-12-30")
df$dueDate <- as.Date(df$dueDate, origin = "1899-12-30")

# Функция для отрисовки графика ----------------------------------------
ganttR <- function(df, type = 'all') {
  nameMilestones <- unique(df$milestones)
  nMilestones <- length(nameMilestones)
  rbPal <- colorRampPalette(c("#3fb3b2", "#ffdd55", "#c7254e", 
                              "#1b95e0", "#8555b4"))
  cols <- data.frame(milestones = nameMilestones,
                     col = rbPal(nMilestones),
                     stringsAsFactors = FALSE)
  cols <- cols[1:nMilestones, ]
  # Преобразование таблицы
  df <- df %>%
    group_by(milestones, num) %>% # группировка по проектам и номеру
    summarise(startDate = min(startDate), # начало и ->
              dueDate = max(dueDate)) %>% # -> конец каждого проекта 
    mutate(tasks = milestones, status = 'M') %>% # проект как задача
    bind_rows(df) %>% # объединение проектов с задачами
    # ширина линий в графике
    mutate(lwd = ifelse(milestones == tasks, 8, 6)) %>% 
    left_join(cols, by = 'milestones') %>% # цвета
    # цвета в соответствии со статусом
    mutate(col = ifelse(status == 'I', paste0(col,'BB'), col)) %>% 
    mutate(col = ifelse(status == 'C', paste0(col,'33'), col)) %>% 
    mutate(cex = ifelse(status == 'M', 0.8, 0.75)) %>%
    mutate(adj = ifelse(status == 'M', 0, 1)) %>%
    mutate(line = ifelse(status == 'M', 8, 0.5)) %>%
    mutate(font = ifelse(status == 'M', 2, 1)) %>%
    # сортировка по номеру проекта
    arrange(desc(num),desc(startDate),dueDate) 
  
  # временной отрезок для которого создаётся диаграмма
  dateRange <- c(min(df$startDate), max(df$dueDate))
  
  # даты для настройки нижней оси (семидневные интервалы)
  # dateSeq <- seq.Date(dateRange[1], dateRange[2], by = 7)
  forced_start <- as.Date(paste0(format(dateRange[1], "%Y-%m"), "-01"))
  yEnd <- format(dateRange[2], "%Y")
  mEnd <- as.numeric(format(dateRange[2], "%m")) + 1
  if(mEnd == 13) {
    yEnd <- as.numeric(yEnd) + 1
    mEnd <- 1
  }
  forced_end <- as.Date(paste0(yEnd, "-", mEnd,"-01"))
  dateSeq <- seq.Date(forced_start, forced_end, by = "month")
  lab <- format(dateSeq, "%b")
  
  # построение диаграммы: проекты + задачи
  if(type == 'all') {
    nLines <- nrow(df)
    par(family = "PT Sans", mar = c(6,9,2,0))
    plot(x = 1, y = 1, col = 'transparent', 
         xlim = c(min(dateSeq), max(dateSeq)), ylim = c(1,nLines), 
         bty = "n", ann = FALSE, xaxt = "n", yaxt = "n", type = "n",
         bg = 'grey')
    mtext(lab[-length(lab)], side = 1, at = dateSeq[-length(lab)], 
          las = 0, line = 1.5, cex = .75, adj = 0)
    axis(1, dateSeq, labels = F, line = 0.5)
    extra <- nLines * 0.03
    for(i in seq(1, length(dateSeq - 1), by = 2)) {
      polygon(x = c(dateSeq[i], dateSeq[i + 1], dateSeq[i + 1], dateSeq[i]),
              y = c(1 - extra, 1 - extra, nLines + extra, nLines + extra),
              border = 'transparent',
              col = '#f1f1f155')
    }
    
    for(i in 1:nLines) {
      lines(c(i,i) ~ c(df$startDate[i], df$dueDate[i]),
            lwd = df$lwd[i],
            col = df$col[i])
      mtext(df$tasks[i],
            side = 2,
            at = i,
            las = 1,
            adj = df$adj[i],
            line = df$line[i],
            cex = df$cex[i],
            font = df$font[i])
    }
    
    # вертикальная линия для сегодняшней даты
    abline(h = which(df$status == 'M') + 0.5, col = '#634d42')
    abline(v = as.Date(format(Sys.time(), format = "%Y-%m-%d")), 
           lwd = 1, lty = 2)
  }
  
  # построение диаграммы: только проекты
  if(type == 'milestones') {
    nLines <- nMilestones
    ms <- which(df$status == 'M')
    par(family = "PT Sans", mar = c(6, 9, 2, 0))
    plot(x = 1, y = 1, col = 'transparent', 
         xlim = c(min(dateSeq), max(dateSeq)), 
         ylim = c(1, nLines), bty = "n", ann = FALSE, xaxt = "n", 
         yaxt = "n", type = "n", bg = 'grey')
    mtext(lab[-length(lab)], side = 1, at = dateSeq[-length(lab)], 
          las = 0, line = 1.5, cex = .75, adj = 0)
    axis(1, dateSeq, labels = F, line = .5)
    extra <- nLines * 0.03
    for(i in seq(1,length(dateSeq - 1), by = 2)) {
      polygon(x = c(dateSeq[i], dateSeq[i + 1], dateSeq[i + 1], dateSeq[i]),
              y = c(1 - extra, 1 - extra, nLines + extra, nLines + extra),
              border = 'transparent',
              col = '#f1f1f155')
    }
    
    for(i in 1:nLines) {
      lines(c(i, i) ~ c(df$startDate[ms[i]], df$dueDate[ms[i]]),
            lwd = df$lwd[ms[i]],
            col = df$col[ms[i]])
      mtext(df$tasks[ms[i]],
            side = 2,
            at = i,
            las = 1,
            adj = 1,
            line = .5,
            cex = df$cex[ms[i]],
            font = df$font[ms[i]])
    }
    abline(v = as.Date(format(Sys.time(), format = "%Y-%m-%d")), 
           lwd = 1, lty = 2)
  }
  par(cex.main = 1, cex.sub = .75, adj = .9, family = "PT Sans")
  title(main = "План-график исследований (2020 год)", 
        sub = paste0("Обновлено ", 
                     as.Date(format(Sys.time(), format = "%Y-%m-%d"))))
}

ganttR(df)

# ganttR(df, "milestones")

# Сохранение -----------------------------------------------------------
# tiff("research_plan.tiff", width = 297, height = 210, units = 'mm', res = 300)
# ganttR(df)
# dev.off()
