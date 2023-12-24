###第一節：描述性統計(1)
#這節課我們終於要開始進行數據分析了，在最開始我們要簡單介紹幾個描述性統計的函數
#請下載本週的範例資料 “Example data.csv”
dat = read.csv("Example data.csv", header = TRUE, fileEncoding = 'CP950')
head(dat)

#以下幾個函數為常見的描述性統計，分別是：
#函數「mean()」可以幫助我們計算平均值
mean(dat$eGFR, na.rm = TRUE)

#函數「sd()」可以幫助我們計算標準差
sd(dat$eGFR, na.rm = TRUE)

#函數「var()」可以幫助我們計算變異數
var(dat$eGFR, na.rm = TRUE)

#函數「median()」可以幫助我們計算變異數
median(dat$eGFR, na.rm = TRUE)

#函數「quantile()」可以幫助我們計算百分位數
quantile(dat$eGFR, na.rm = TRUE)

quantile(dat$eGFR, 0.5, na.rm = TRUE)

quantile(dat$eGFR, 0.95, na.rm = TRUE)

#函數「min()」可以幫助我們找出最小值
min(dat$eGFR, na.rm = TRUE)

#函數「max()」可以幫助我們找出最大值
max(dat$eGFR, na.rm = TRUE)

#函數「table()」可以幫助我們產生列聯表
table(dat$Cancer)

table(dat$Cancer, dat$Diabetes)

#函數「prop.table()」可以幫助我們產生列聯表的百分比
tab1 = table(dat$Cancer)
prop.table(tab1)

tab2 = table(dat$Cancer, dat$Diabetes)
prop.table(tab2)

prop.table(tab2, 1)

prop.table(tab2, 2)

#描述性統計(2)
#利用索引函數，我們將能得到某種條件下的描述性統計，舉例來說我們想獲得case組的eGFR平均數，可以使用下列語法
mean(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE)

#Control組的亦同
mean(dat[dat[,"Disease"] == 0,]$eGFR, na.rm = TRUE)

#如果我們想要把平均數±標準差表示出來，可以利用函數「paste()」或函數「paste0()」：
paste(mean(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE), "±", sd(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE), sep = "")

paste0(mean(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE), "±", sd(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE))

#我們發現這樣的呈現相當醜，我們可以使用函數「formatC()」來指定我們想要的小數點位數
m = mean(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE)
s = sd(dat[dat[,"Disease"] == 1,]$eGFR, na.rm = TRUE)
formatC(m, digits = 3, format = "f")

formatC(s, digits = 3, format = "f")

paste0(formatC(m, digits = 3, format = "f"), "±", formatC(s, digits = 3, format = "f"))









