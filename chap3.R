### MMM研修 Chap.3 コード
###   2020/04/03 Shigeru ONO

library(tidyverse)
library(assertr)
library(patchwork)
library(rgl)
windowsFonts(MeiryoUI = "Meiryo UI")

### サンプルデータの読込
# 読込
dfMansion <- read_csv("./Mansion.csv")

### Code 1. 中古マンションの価格
# 散布図
g1 <- ggplot(data = dfMansion, aes(x = Hirosa, y = Price)) +
      labs(x = "広さ(平方m)", y = "価格(百万円)")
g2 <- ggplot(data = dfMansion, aes(x = Nensu, y = Price)) + 
      labs(x = "築年数", y = "価格(百万円)")
g3 <- ggplot(data = dfMansion, aes(x = Hirosa, y = Nensu)) + 
      labs(x = "広さ", y = "築年数")
# patchworkパッケージによってチャートを結合
g <- (g1 + g2 + g3) 
# patchworkパッケージでは、結合したチャートを一括して修正できる
g <- g & geom_point(size = 4) 
g <- g & theme_bw(base_family = "MeiryoUI")
print(g)
# 平均
summary(dfMansion)
# 相関
cor(dfMansion %>% dplyr::select(Price, Hirosa, Nensu))

### Code 2. 最小二乗法のシミュレーション(2事例)
# デモンストレーション用の仮のパラメータを定義
lParam <- list(
  cand1 = list(a = 8,  b = 0.55),
  cand2 = list(a = -2, b = 0.8)
)
# 散布図に仮の直線を載せる
g <- ggplot(data = dfMansion, aes(x = Hirosa, y = Price))
g <- g + geom_point(size = 4)
g <- g + geom_abline(intercept = lParam$cand1$a, slope = lParam$cand1$b)
g <- g + geom_abline(intercept = lParam$cand2$a, slope = lParam$cand2$b)
g <- g + labs(x = "広さ(平方m)", y = "価格(百万円)")
g <- g + coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
g <- g + theme_bw(base_family = "MeiryoUI")
print(g)
# 残差二乗和の計算例
out <- lParam %>%
  # purrr::map_dfr() は、リストを受け取り、リストの各要素にたいして
  # 関数 .f を適用し、その結果得られるデータフレームに要素名を持つ
  # 列(列名は.id)を追加し、縦に積んで返す
  map_dfr(
    .f = function(lCurrent){
      dfMansion %>%
        dplyr::select(No, Hirosa, Price) %>%
        mutate(gPred = lCurrent$a + Hirosa * lCurrent$b)
    }, 
    .id = "sID"
  ) %>%
  # 残差二乗を求める
  mutate(gSQError = (Price - gPred)^2) %>%
  # 予測値と残差二乗をlongに
  gather(varname, value, c(gPred, gSQError)) %>%
  # 変数名を作成する
  mutate(
    varname = paste0(varname, sub("^cand", "", sID))
  ) %>%
  # wideに
  dplyr::select(-sID) %>%
  spread(varname, value)
# 出力
print(out)
write.csv(out, "./chap3_code2.csv", row.names = FALSE)

### Code 3. 最小二乗法のシミュレーション(グリッドサーチ)
# sub_SQError(): パラメータをもらって残差平方和を返す関数
sub_SQError <- function(gA,gB){
  agPred <- gA + gB * dfMansion$Hirosa
  sum( (dfMansion$Price - agPred)^2 )
}
# sub_SQErrors(): パラメータのベクトルをもらって
# 要素ごとにsub_SQError()をコールする関数
sub_SQErrors <- function(agA, agB){
  stopifnot(length(agA) == length(agB))
  sapply(seq_along(agA), function(nID) sub_SQError(agA[nID], agB[nID]))
}
# 描画
agX <- seq(-2, 10,  length = 50)
agY <- seq(0.4, 1,  length = 50)
mgZ <- outer(agX, agY, sub_SQErrors)
persp(
  agX, agY, mgZ,
  ticktype = "detailed", expand = 0.5, theta = -20, phi = 45,
  xlab = "a", ylab = "b", zlab = ""
)

### Code 4. 回帰分析
# 関数を使わずに
x <- dfMansion$Hirosa
y <- dfMansion$Price
b <- sum( (x - mean(x))*(y - mean(y))) / sum( (x - mean(x))^2 )
a <- mean(y) - b * mean(x)
print(a)
print(b)
# 関数を使って
summary(lm(Price ~ Hirosa, data = dfMansion))

### Code 5. 散布図(広さ vs 家賃) with regression line
g <- ggplot(data = dfMansion, aes(x = Hirosa, y = Price))
g <- g + geom_point(size = 4)
g <- g + geom_smooth(method = "lm", color = "red", se = FALSE)
g <- g + labs(x = "広さ(平方m)", y = "価格(百万円)")
g <- g + theme_bw(base_family = "MeiryoUI")
print(g)

### Code 6. 正規密度の計算例
1/(sqrt(2*pi) * 2) * exp(-(12 - 10)^2 / (2*4))
dnorm(12, 10, 2)
dnorm(12, 10, 2) * dnorm(8, 10, 2) * dnorm(9, 10, 2)

### Code 7. 尤度シミュレーション (2事例)
# map_dfr()についてはCode 2を参照
out <- lParam %>%
  map_dfr(
    .f = function(lCurrent){
      dfMansion %>%
        dplyr::select(No, Hirosa, Price) %>%
        mutate(gPred = lCurrent$a + Hirosa * lCurrent$b)
    }, 
    .id = "sID"
  ) %>%
  mutate( gL = dnorm(Price, gPred, 5)) %>%
  gather(varname, value, c(gPred, gL)) %>%
  mutate(varname = paste0(varname, sub("^cand", "", sID))) %>%
  dplyr::select(-sID) %>%
  spread(varname, value)
print(out)
write.csv(out, "./chap3_code7.csv", row.names = FALSE)

### Code 8. a, bについてグリッドサーチして描画
# sub_Likelihood(): パラメータをもらって尤度を返す関数
sub_Likelihood <- function(gA,gB){
  agPred <- gA + gB * dfMansion$Hirosa
  exp(sum(log(dnorm(dfMansion$Price, agPred, 5))))
}
# sub_Likelihoods(): パラメータのベクトルをもらって
# 要素ごとにsub_Likelihood()をコールする関数
sub_Likelihoods <- function(agA, agB){
  stopifnot(length(agA) == length(agB))
  sapply(seq_along(agA), function(nID) sub_Likelihood(agA[nID], agB[nID]))
}
# 描画
agX <- seq(-2, 10,  length = 50)
agY <- seq(0.4, 1,  length = 50)
mgZ <- outer(agX, agY, sub_Likelihoods)
persp(
  agX, agY, mgZ,
  ticktype = "detailed", expand = 0.5, theta = -20, phi = 45,
  xlab = "a", ylab = "b", zlab = ""
)

### Code 9. 平方和の分解
agPredict <- predict(lm(Price ~ Hirosa, data = dfMansion))
a <- sum((dfMansion$Price - mean(dfMansion$Price))^2)
b <- sum((agPredict - mean(dfMansion$Price))^2)
c <- sum((dfMansion$Price - agPredict)^2)
print(a)
print(b)
print(c)
print(b/a)

### Code 10. 重回帰のパラメータ推定
# 関数を使わずに
X <- dfMansion %>%
  mutate(Intercept = 1) %>%
  dplyr::select(Intercept, Hirosa, Nensu) %>%
  as.matrix(.)
Y <- dfMansion %>%
  dplyr::select(Price) %>%
  as.matrix(.)
betahat <- solve(t(X) %*% X) %*% t(X) %*% Y
print(betahat)
# 関数を使って
oModel <- lm(Price ~ Hirosa + Nensu, data = dfMansion)
agCoef <- coef(oModel)
summary(oModel)
# チャート
plot3d(
  dfMansion[c("Hirosa", "Nensu", "Price")], 
  size = 5, box = FALSE, axis = FALSE, type = "p"
)
plot3d(
  dfMansion[c("Hirosa", "Nensu", "Price")], 
  size = 5, box = FALSE, axis = FALSE, type = "h", add = TRUE
)
planes3d(agCoef[2], agCoef[3], -1, agCoef[1], col = "blue", alpha = 0.2)

### Code 11. 最大対数尤度とAIC
# 関数を使わずに
X <- dfMansion %>%
  mutate(Intercept = 1) %>%
  dplyr::select(Intercept, Hirosa, Nensu) %>%
  as.matrix(.)
Y <- dfMansion %>%
  dplyr::select(Price) %>%
  as.matrix(.)
betahat <- solve(t(X) %*% X) %*% t(X) %*% Y
ehat <- Y - X %*% betahat
sshat <- as.vector((1/length(Y)) * t(ehat) %*% ehat)
LL <- -(length(Y)/2) * (log(sshat) + 1 + log(2 * pi))
AIC <- -2 * LL + 2 * 4 
print(LL)
print(AIC)
# 関数を使って
oModel <- lm(Price ~ Hirosa + Nensu, data = dfMansion)
print(logLik(oModel))
print(AIC(oModel))

### 以上