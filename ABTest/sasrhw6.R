library(tidyverse)
fb <- read.csv("hw6-fb.csv")

tips_like <- nrow(fb %>%
                    filter(condition=="tips" & clicked_like=="1"))
tips_visitnum <- nrow(fb %>%
                        filter(condition=="tips"))
tips_likerate <- (tips_like/tips_visitnum)

tools_like <- nrow(fb %>%
                     filter(condition=="tools" & clicked_like=="1"))
tools_visitnum <- nrow(fb %>%
                        filter(condition=="tools"))
tools_likerate <- (tools_like/tools_visitnum)

likeprop <- (tips_likerate-tools_likerate)/tools_likerate*100

tips_share <- nrow(fb %>%
                     filter(condition=="tips" & clicked_share=="1"))
tips_sharerate <- (tips_share/tips_visitnum)
tools_share <- nrow(fb %>%
                     filter(condition=="tools" & clicked_share=="1"))
tools_sharerate <- (tools_share/tools_visitnum)
shareprop <- (tips_sharerate-tools_sharerate)/tools_sharerate*100

col1 <- c(tips_like,tools_like)
col2 <- c(tips_share,tools_share)
col3 <- c("tips","tools")
df <- data.frame("clicked_like"=col1,"clicked_share"=col2,"condition"=col3)
ggplot(df, aes(x=condition,y=clicked_like)) +
  geom_bar(position = "dodge",stat="identity") +
  xlab("clicked_like") +
  ylab("count") +
  ggtitle("like amount boxplot") +
  theme_bw()
ggplot(df, aes(x=condition,y=clicked_share)) +
  geom_bar(position = "dodge",stat="identity") +
  xlab("clicked_share") +
  ylab("count") +
  ggtitle("share amount boxplot") +
  theme_bw()
#假設檢定
#H0:tips和tools的案讚綠沒顯著差異/分享綠沒顯著差異
#H1:tips各項率都大於tools(所以前面要tips-tools用tools除當參照)
library(pwr)
I=c(tips_like,tools_like)
II=c(tips_share,tools_share)
III=c(tips_visitnum,tools_visitnum)
prop.test(I,III)#p-value小 顯著 拒絕H0 粉絲比較會按讚分享tips相關
prop.test(II,III)#反
#所以應該多tips文章
