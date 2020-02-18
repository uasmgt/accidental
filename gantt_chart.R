# Диаграмма Ганта для планирования исследований ------------------------
## Код: https://insileco.github.io/2017/09/20/gantt-charts-in-r/
## Заголовок диаграммы на строке 149

# Пакеты ---------------------------------------------------------------
library(lubridate)
library(dplyr)

research.plan <- read.csv2("~/data/research_plan.csv", 
                           stringsAsFactors = FALSE, 
                           fileEncoding = "UTF-8")

# Функция для построения графика ---------------------------------------
ganttR <- function(df, type = 'all') {
  df <- df %>% 
    mutate(startDate = dmy(startDate),
           dueDate = dmy(dueDate)) %>% 
    arrange(startDate)
  helper <- data.frame(milestone = unique(df$milestones))
  helper$index <- rownames(helper)
  df$index <- as.numeric(helper$index[match(df$milestones, 
                                            helper$milestone)])
  # Преобразование таблицы
  df <- df %>%
    group_by(milestones, index) %>% # группировка по проектам и номеру
    summarise(startDate = min(startDate), # начало и ->
              dueDate = max(dueDate)) %>% # -> конец каждого проекта 
    mutate(tasks = milestones, status = 'M') %>% # проект как задача
    bind_rows(df) %>% # объединение проектов с задачами
    filter(is.na(tasks) == FALSE) %>% 
    # ширина линий в графике
    mutate(lwd = ifelse(milestones == tasks, 6.5, 4.5)) %>% 
    # цвета в соответствии со статусом
    mutate(col =  case_when(
      status == "P" ~ "firebrick4",
      status == "N" ~ "darkslateblue",
      status == "O" ~ "yellowgreen",
      status == "C" ~ "forestgreen",
      status == "M" ~ "slategray"
    )) %>% 
    mutate(cex = ifelse(status == 'M', 0.7, 0.6)) %>%
    mutate(adj = ifelse(status == 'M', 0, 1)) %>%
    mutate(line = ifelse(status == 'M', 8, 0.5)) %>%
    mutate(font = ifelse(status == 'M', 2, 1)) %>%
    # сортировка по номеру проекта
    arrange(desc(index), desc(startDate), dueDate) 
  # временной отрезок для которого создаётся диаграмма
  dateRange <- c(min(df$startDate), max(df$dueDate))
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
    par(family = "PT Sans", mar = c(3, 9, .5, 0))
    plot(x = 1, y = 1, col = 'transparent', 
         xlim = c(min(dateSeq) - 7.5, max(dateSeq) + 7.5), ylim = c(1, nLines), 
         bty = "n", ann = FALSE, xaxt = "n", yaxt = "n", type = "n",
         bg = 'grey')
    mtext(lab[-length(lab)], side = 1, at = dateSeq[-length(lab)], 
          las = 0, line = .75, cex = .75, adj = 0)
    axis(1, dateSeq, labels = F, line = 1, lwd = .5, tck = -.005)
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
      text(x = min(df$startDate) - 20,
           y = i,
           labels = df$exec[i],
           adj = 0,
           cex = df$cex[i],
           font = df$font[i])
      text(x = df$dueDate[i] + 1.5,
           y = i,
           labels = df$result[i],
           adj = 0,
           cex = df$cex[i],
           font = df$font[i])
    }
    # вертикальная линия для сегодняшней даты
    abline(h = which(df$status == 'M') + 0.5, col = "paleturquoise4", lwd = .5)
    abline(v = as.Date(format(Sys.time(), format = "%Y-%m-%d")), 
           lwd = .5, lty = 2)
  }
  
  # построение диаграммы: только проекты
  if(type == 'milestones') {
    nLines <- length(unique(df$milestones))
    ms <- which(df$status == 'M')
    par(family = "PT Sans", mar = c(6, 9, 2, 0))
    plot(x = 1, y = 1, col = 'transparent', 
         xlim = c(min(dateSeq), max(dateSeq)), 
         ylim = c(1, nLines), bty = "n", ann = FALSE, xaxt = "n", 
         yaxt = "n", type = "n", bg = 'grey')
    mtext(lab[-length(lab)], side = 1, at = dateSeq[-length(lab)], 
          las = 0, line = 1.5, cex = .75, adj = 0)
    axis(1, dateSeq, labels = F, line = 1, lwd = .5, tck = -.005)
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
           lwd = .5, lty = 2)
  }
  par(cex.main = .85, cex.sub = .75, adj = .95, family = "PT Sans")
  title(main = "План исследований (2020 год)", line = -.95,
        sub = paste0("Обновлено ", 
                     as.Date(format(Sys.time(), format = "%Y-%m-%d"))))
}

ganttR(research.plan)
# ganttR(research.plan, "milestones")

# Сохранение -----------------------------------------------------------
# tiff("research_plan.tiff", width = 277, height = 190, units = 'mm', res = 300)
# ganttR(research.plan)
# dev.off()
# 
# tiff("research_plan_short.tiff", width = 277, height = 190, units = 'mm', res = 300)
# ganttR(research.plan, "milestones")
# dev.off()