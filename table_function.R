# Назначение -----------------------------------------------------------
# Две простых функции для удобного представления частотных таблиц в R

# Частотная таблица с процентами ---------------------------------------
TblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl, round(prop.table(tbl) * 100, 2))
  colnames(res) <- c("Count", "Percentage")
  res
}

# Сортированная частотная таблица с процентами -------------------------
TblSort <- function(x){
  tbl <- table(x)
  res <- cbind(tbl, round(prop.table(tbl) * 100, 2))
  colnames(res) <- c("Count", "Percentage")
  res <- data.frame(res)
  res <- res[order(res$Count, decreasing = TRUE), ]
  res
}