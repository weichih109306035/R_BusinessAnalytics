library(tidyverse)
#1
#1.a. 結果要截
x<-c()
while(length(x)<20){#0-19 所以要<20
  a=runif(1,0,10)
  e=rnorm(1,0,2)
  if(0<=a+e & a+e<=11){
    xi = a+e
  }
  x=c(x,xi)
}
x

#1.b.貼code即可
cauchy <- function(theta, xi) {
  f=(-2)*sum((theta-xi)/(1+(theta-xi)^2))
  return(f)
}
#1.c. 會跑出一個值
cauchy(0.3,x)

#2
song <- read_csv("song_data.csv")

#2.a.
song$instru_prob=NA
song$instru_prob[which(song$instrumentalness>=0 &song$instrumentalness<0.4)]="low"
song$instru_prob[which(song$instrumentalness>=0.4 &song$instrumentalness<0.8)]="mid"
song$instru_prob[which(song$instrumentalness>=0.8)]="high"

#2.b.
song$instru_prob <- as.factor(song$instru_prob)
str(song)
attach(song)

library(broom)

newsong<-song[,-c(1,7)]
str(newsong)

f1<-lm(song_popularity ~ . ,data=newsong)

library(MASS)
step=stepAIC(f1, direction="backward")#最後出來的最好，AIC會拋棄一些不太有用的變數
f2=lm(song_popularity ~ .-audio_mode-speechiness, data=newsong)

glance(f1)
glance(f2)
anova(f2,f1)#簡單的模型放前面，得到的為複雜模型p-value，大簡小複






summary(f2)
summary(f1)
f3=lm(song_popularity ~ .-audio_mode-key-speechiness, data=newsong)#-audio_mode-key-speechiness
summary(f3)
glance(f3)
f4=lm(song_popularity ~ .-audio_mode-key-speechiness-song_duration_ms, data=newsong)#-audio_mode-key-speechiness
summary(f4)
glance(f4)
#2.b. 寫出自己的模型得出一個結果 先做multicolinear再去掉太大的 audio modeu應該是0,1 time_signature應該是0,1,2,3,4,5
#感覺danceability, energy(可能有interact), tempo,time_signature(可能有interact) 兩個audio會不會太多？
#先試做danceability energy 和他們的anova
#loudness plot稍微有點東西
attach(song)
str(song)
plot(danceability,song_popularity,xlab="danceability", ylab="song_popularity")
fit.1 <- lm (song_popularity ~ danceability)
summary(fit.1)
print(fit.1)
curve (coef(fit.1)[1] + coef(fit.1)[2]*x, add=TRUE,col="red") #or abline
library(broom)
fit.2 <- lm (song_popularity ~ danceability+energy)
tidy(fit.2)
curve (cbind (1, 1, x) %*% coef(fit.2), add=TRUE, col="blue")
curve (cbind (1, 0, x) %*% coef(fit.2), add=TRUE, col="green")
fit.3 <- lm (song_popularity ~ danceability*energy)
tidy(fit.3)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.3), add=TRUE, col="yellow")#1*x:交互作用

ggplot(song, aes( danceability,song_popularity)) + geom_point() + geom_smooth(method="lm") 

lm(formula = song_popularity ~ as.factor(time_signature))#有dummy variable概念
lm(formula = song_popularity ~ time_signature) #R will consider mom_work as numerical var
contrasts(as.factor(time_signature))

#有點東西的
song %>% group_by(audio_mode) %>% do(tidy(lm(song_popularity ~ danceability, .)))

f = lm(song_popularity ~ danceability+energy)
f2 <- lm (song_popularity ~ danceability*energy)
tidy(f)
tidy(f2)
anova(f,f2)#p很小f2比較好

### stepwise .會爆掉 好像免搶可以用+的
full <- lm(song_popularity~.,data=song)
glance(full) %>% select(AIC,BIC)
null <-lm(song_popularity~1,data=song)

#AIC
step(full, direction="backward")
step(null, scope=list(lower=null, upper=full), direction="forward")
#下面的幾乎都沒什麼用 快兩萬個點配一條差不多的線
## model with no interaction
fit.4 <- lm (song_popularity ~ audio_mode + audio_valence)
colors <- ifelse (audio_mode==1, "black", "gray") #0 no hs
plot (audio_valence, song_popularity, xlab="audiov", ylab="popu",
      col=colors, pch=20) 
curve (cbind (1, 1, x) %*% coef(fit.2), add=TRUE, col="blue")
curve (cbind (1, 0, x) %*% coef(fit.2), add=TRUE, col="red")
#下面的幾乎都沒什麼用 快兩萬個點配一條差不多的線
plot(danceability,song_popularity,xlab="dance", ylab="popular")
curve (coef(fit.1)[1] + coef(fit.1)[2]*x, add=TRUE,col="red") #or abline
abline(fit.1)#配適成一條直線
#alternately (in matrix form)
curve (cbind(1,x) %*% coef(fit.1), add=TRUE,col="blue") #%*%矩陣相乘方式
#or use ggplot
library(tidyverse)
ggplot(song, aes(danceability,song_popularity)) + geom_point() + geom_smooth(method="lm") 

ggplot(song, aes(x = audio_valence, y = song_popularity, color = as.factor(audio_mode))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = as.factor(audio_mode)))

#interaction plot 會直接爆掉
#一次是所有變數也會直接爆掉
fit.05 <- lm (song_popularity ~ .,data=song)
#這好像有點用
x.new <- data.frame(audio_mode=1, audio_valence=100)
predict(fit.2, x.new, interval="prediction", level=0.95)

