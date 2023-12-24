###練習3：擴充剛剛的預測函數
#剛剛我們是使用SBP以及DBP來預測eGFR，現在我們還要額外增加Income以及Education兩個因子。
#同樣的，假設使用者不願意填寫Income或是Education的情形，那就請你用剩餘的資訊進行預測。
#這裡你就可以發現，假設我們有n個變項可供使用者填答，那他填與不填的所有組合就共有2n???1種，因此剛剛的第二種寫法又更為重要了，試著完成他吧！

###練習3答案
#這個函數的重點的關鍵在於將Income以及Education做one-hot encoding：
dat = read.csv("Example data.csv", header = TRUE, fileEncoding = 'CP950')

coding_Income = one_hot_coding(dat[,"Income"])
coding_Education = one_hot_coding(dat[,"Education"])

dat1 = cbind(dat, coding_Income, coding_Education)
colnames(dat1)[11:14] = c('Income1', 'Income2', 'Education1', 'Education2')

pred_func = function (SBP = NA, DBP = NA, Income = NA, Education = NA) {
  
  if (is.na(SBP) & is.na(DBP) & is.na(Income) & is.na(Education)) {stop('需要至少輸入一個數值')} else {
    
    if (is.na(Income)) {
      coding_Income = c(NA, NA)
    } else if (Income == 0) {
      coding_Income = c(0, 0)
    } else if (Income == 1) {
      coding_Income = c(1, 0)
    } else if (Income == 2) {
      coding_Income = c(0, 1)
    }
    
    if (is.na(Education)) {
      coding_Education = c(NA, NA)
    } else if (Education == 0) {
      coding_Education = c(0, 0)
    } else if (Education == 1) {
      coding_Education = c(1, 0)
    } else if (Education == 2) {
      coding_Education = c(0, 1)
    }
    
    var_name = c('SBP', 'DBP', 'Income1', 'Income2', 'Education1', 'Education2')
    var_vec = c(1, SBP, DBP, coding_Income, coding_Education)
    
    var_name = var_name[!is.na(var_vec)[-1]]
    var_vec = var_vec[!is.na(var_vec)]
    
    model = lm(dat[,"eGFR"] ~ ., data = dat1[,var_name,drop=FALSE])
    
    result = model$coefficients %*% var_vec
    result
    
  }
  
}

pred_func(SBP = 100, DBP = 100, Income = 1)

pred_func(SBP = 100, DBP = 100, Education = 2)

pred_func(SBP = 100, DBP = 100, Income = 2, Education = 0)