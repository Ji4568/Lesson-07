###第三節：在線性迴歸中使用類別變項(1)
#你可能有注意到剛剛的資料集中，其中有些變項是被編碼成0、1以及0、1、2，這樣的變項可以放入線性迴歸中嗎?
#答案是可以的，對於已經編碼成0、1的變項我們可以直接把他放入線性迴歸，但是0、1、2的變項我們就不能直接放入線性迴歸之中，請想想為什麼?
#那我們該怎樣處理這種有3個類別以上的類別變項呢?方法很簡單就是要把他改成這樣的編碼格式(他們稱之為one-hot encoding)：
x = c(0, 1, 2, 1, NA, 2)
coding_x = matrix(c(0, 1, 0, 1, NA, 0,
                    0, 0, 1, 0, NA, 1), nrow = 6, ncol = 2)
coding_x

#在線性迴歸中使用類別變項(2)
#至於要怎樣把他轉換成這個格式呢?第一種方法就是用迴圈填數字，我們可以用這種方式：
x = c(0, 1, 2, 1, NA, 2)

lvl.cat = levels(factor(x))
n.cat = length(lvl.cat)
coding_x = matrix(0, nrow = length(x), ncol = n.cat - 1)

for (i in 1:(n.cat-1)) {
  coding_x[x == lvl.cat[i+1],i] = 1
}

coding_x[is.na(x),] = NA

coding_x

#另外有一種更簡單的方式，就是使用R內建的函數「model.matrix」
x = c(0, 1, 2, 1, NA, 2)
coding_x = model.matrix(~as.factor(x))
coding_x
#你應該會注意到使用「model.matrix」與我們想要的不太一樣(多了Intercept以及NA被移除了)，所以我們還要稍微修正一下他：
new_index = 1:length(x)
new_index[is.na(x)] = NA
new_index[!is.na(new_index)] = 1:sum(!is.na(x))
coding_x = coding_x[new_index,-1]
rownames(coding_x) = 1:nrow(coding_x)
#我們把他整理成同個函數：
one_hot_coding = function (x) {
  
  coding_x = model.matrix(~as.factor(x))
  new_index = 1:length(x)
  new_index[is.na(x)] = NA
  new_index[!is.na(new_index)] = 1:sum(!is.na(x))
  coding_x = coding_x[new_index,-1]
  rownames(coding_x) = 1:nrow(coding_x)
  coding_x
  
}

x = c(0, 1, 2, 1, NA, 2)
one_hot_coding(x)

#在線性迴歸中使用類別變項(3)
#有了這項技術以後，那就可以把剛剛的Income以及Education都納入回歸之內了：
coding_Income = one_hot_coding(dat[,"Income"])
coding_Education = one_hot_coding(dat[,"Education"])

dat1 = cbind(dat, coding_Income, coding_Education)

lm(dat[,"eGFR"] ~ ., data = dat1[,c(7:8, 11:14)])

#不過其實這整個過程都可以更簡單，你只要告訴R說Income以及Education都是因子變項即可：
lm(dat[,"eGFR"] ~ dat[,"SBP"] + dat[,"DBP"] + factor(dat[,"Income"]) + factor(dat[,"Education"]))



