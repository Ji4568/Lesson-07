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
