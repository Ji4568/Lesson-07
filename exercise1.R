###練習1：學會利用自訂函數組合出想要的數據格式
#現在請利用自訂函數功能，寫出兩個函數，規格如下：
#函數之input必須為一類別變項及一連續變項
#函數中必須設定一個變項，讓使用者能決定顯示小數點的位數
#3-1. 第一個函數必須輸出在該類別變項為不同類別時，該連續變項之『平均數±標準差』
#3-2. 第二個函數必須輸出在該類別變項為不同類別時，該連續變項之『中位數(25百分位-75百分位)』
#注意：請考慮類別數不只是2的情形，以及類別項目並非為0、1、2、…的狀況

###練習1答案
#這是第一個函數：
func.1 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = mean(y[x==lvl.x[i]], na.rm = TRUE)
    s = sd(y[x==lvl.x[i]], na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "±", formatC(s, digits = dig, format = "f"))
  }
  
  result
  
}

func.1(x = dat[,"Disease"], y = dat[,"eGFR"])

func.1(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)

#這是第二個函數：
func.2 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = median(y[x==lvl.x[i]], na.rm = TRUE)
    q.25 = quantile(y[x==lvl.x[i]], 0.25, na.rm = TRUE)
    q.75 = quantile(y[x==lvl.x[i]], 0.75, na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "(", formatC(q.25, digits = dig, format = "f"), "-", formatC(q.75, digits = dig, format = "f"), ")")
  }
  
  result
  
}

func.2(x = dat[,"Disease"], y = dat[,"eGFR"])

func.2(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)

