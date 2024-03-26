#PC可能找不到最好說明也沒關係（PC1有哪個顏色最深最相關 和味道最有關之類的
library(tidyverse)
findata <- read.csv("financialdata.csv")
findata <- findata[,-1]
attach(findata)
#先轉型！！！有些數字莫名其妙是chr
str(findata)
findata$op_profit_growth_rate <- as.numeric(gsub(",","", unlist(op_profit_growth_rate)))
class(findata$op_profit_growth_rate)#gsub可替換符號
findata$current_ratio <- as.numeric(gsub(",","", unlist(current_ratio)))
findata$quick_rartio <- as.numeric(gsub(",","", unlist(quick_rartio)))
str(findata)

#1以PCA或SPCA分析，找出每個主成份能解釋多少變異？大概需要多少個PC來解釋這筆資料？
library(nsprcomp)
spca <- nscumcomp(findata, k=80, nneg=T, scale=T)#如果有錯要懲罰項(感覺是非負整數)提高就16*5->16*6
summary(spca)

########
#以下為pca可先不看
library(stats)
pca<- prcomp(findata, center = TRUE, scale = TRUE)
#做pca先標準化
summary(pca)
#看印出來結果 一種是要到PC6累積才到80% 可以把PC16刪到PC6 這大概就是答案
#propotion of variance 2_1的答案
plot(pca, type="line")
abline(h=1, col="red")
#用另一個kaiser方法(var>=1)只有5個
######

#2找出前三個主成份分別重點變數為何並解釋。
library(nsprcomp)
spca <- nscumcomp(findata[,-1], k=80, nneg=T, scale=T)
summary(spca)
screeplot(spca)

library(reshape2)
ggplot(melt(spca$rotation[,1:3]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#pca方法
library(reshape2)
ggplot(melt(pca$rotation[,1:3]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())#紅大負藍大正
#3找出適合投資的公司。(不需指出是哪間公司，只需依第一主成份結果說明，例如：適合投資資產報酬率高的公司)
#承2圖中PC1，選roa, roe, profit_margin_rate高的公司即答案？
