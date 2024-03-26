sales.df <- read.csv("salesdata.csv")
client.df <- read.csv("client_list.csv")
prod.df <- read.csv("product_list.csv")
library(tidyverse)

#1
prod.df <- prod.df %>% separate(Item, into=c("Product", "Item"), sep = "_")
prod.df

#2
str(sales.df)
str(client.df)
str(prod.df)
#檢查資料型態發現prod.df之Product型態為chr 應轉成int
prod.df$Product <-as.integer(prod.df$Product)
full.table <- sales.df %>% inner_join(client.df) %>% inner_join(prod.df)

#3
full.table <-full.table %>% mutate(spend = UnitPrice*Quantity )
full.table

#4
group1 <- full.table %>% filter(Membership=="gold" | Membership=="diamond")
group2 <- full.table %>% filter(Membership!="gold" & Membership!="diamond")
#平均年紀:group2(非gold, diamond)較高
mean(group1$Age)
mean(group2$Age)
#性別:
table(group1$Gender)
table(group2$Gender)
#國家
barplot(table(group1$Region))
barplot(table(group2$Region))
#消費情況差異
barplot(table(group1$Product), names.arg = c("iPhone","iPad",
"MacBook","iMac","AirPods","AppleWatch"), main = "gold&diamond")
barplot(table(group2$Product), names.arg = c("iPhone","iPad",
"MacBook","iMac","AirPods","AppleWatch"), main = "others")

#5
male.table <- full.table %>% filter(Gender=="male")
#平均年紀
mean(male.table$Age)
#國家
table(male.table$Region)
#消費情況
table(male.table$Product)
#不同產品的「總消費」
spend_product <-
  male.table %>%
  group_by(Product) %>%
  summarise(TotalSale=sum(spend))
spend_product %>%
  ggplot(aes(x=Product, y=TotalSale)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=TotalSale), vjust=1.5, color="white",size=5)

spend_item <-
  male.table %>%
  group_by(Item) %>%
  summarise(TotalSale=sum(spend))
spend_item %>%
  ggplot(aes(x=Item, y=TotalSale)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=TotalSale), vjust=1.5, color="white",size=5)
