###練習2：學會使用線性迴歸得到的結果產生預測公式
#現在我們想要使用SBP以及DBP來預測eGFR，所以希望你能寫出一個自訂函數，讓使用者輸入SBP與DBP的值，並為他計算出eGFR的值。
#但是這時候我們遇到一個問題，那就是預測公式鐵定是以這樣的型式表現，因此假設有一個新的個案缺少SBP或DBP其中一項，那他將無法使用這個式子：

#因此這個函數需要有一個功能，就是當使用者沒有輸入SBP而只輸入DBP時，你必須用這個式子協助他進行預測(依此類推)：

#有兩種寫法能完成這個工作，第一種方法就是你先行計算出3個預測式的係數，接著再寫下rule決定什麼情境要用什麼式子；而第二種寫法更為自由，那就是根據使用者輸入的變項狀況，到時候再利用資料進行預測模型的建立，
#最後再利用預測式給出答案！第二種方法明顯更為自由，但難度也頗高，希望你能做到！

###練習2答案
#這個函數的重點在於我們知道模型的係數在哪，熟練列表物件能幫助我們找到係數並進行運算。
#這裡只展示第二種解法的函數：

pred_func = function (SBP = NA, DBP = NA) {
  
  if (is.na(SBP) & is.na(DBP)) {stop('需要至少輸入一個數值')} else {
    
    var_name = c('SBP', 'DBP')
    var_vec = c(1, SBP, DBP)
    
    var_name = var_name[!is.na(var_vec)[-1]]
    var_vec = var_vec[!is.na(var_vec)]
    
    model = lm(dat[,"eGFR"] ~ ., data = dat[,var_name,drop=FALSE])
    
    result = model$coefficients %*% var_vec
    result
    
  }
  
}

pred_func(SBP = 100)

pred_func(DBP = 100)

pred_func(SBP = 100, DBP = 100)