### MMM研修 Chap.4 コード
###   2020/04/03 Shigeru ONO

library(tidyverse)
library(assertr)
library(patchwork)
library(GGally)
library(broom)
library(car)
windowsFonts(MeiryoUI = "Meiryo UI")

sub_makeData_MVN.1 <- function(nSize, agMu, agVar, mgTriCorr){
  # called by: makeData_Shop.1()
  # purpose: 多変量正規乱数データの生成
  # args: 
  #   nSize:     データサイズ
  #   agMu:      平均ベクトル
  #   agVar:     分散ベクトル
  #   mgTriCorr: 相関行列。下三角のみ指定し、上三角と対角はNAにすること
  
  # trap: サイズは整合しているか
  stopifnot(length(agMu) == length(agVar))
  stopifnot(length(agMu) == nrow(mgTriCorr))
  stopifnot(length(agMu) == ncol(mgTriCorr))
  
  # 相関行列(下三角行列)を相関行列に変換
  mgCorr <- mgTriCorr
  # NAに0を埋めて
  mgCorr[is.na(mgCorr)] <- 0
  # 転置行列を足して上三角を埋め
  mgCorr <- mgCorr + t(mgCorr)
  # 対角を1に
  diag(mgCorr) <- 1
  # trap: ランクおちしていない
  stopifnot(qr(mgCorr)$rank == nrow(mgCorr))
  
  # 共分散行列の作成
  mgCov <- diag(agVar) %*% mgCorr %*% diag(agVar)
  
  # 生成
  out <- MASS::mvrnorm(nSize, agMu, mgCov)
  return(out)
}
sub_AddNoise.1 <- function(agIn, agProp){
  # called by: makeData_Shop.1()
  # purpose: データベクトルに正規ノイズを加える
  # args: 
  #   agIn: データベクトル
  #   agProp: 出力の分散に占める正規ノイズの分散の割合
  # return: 
  #   ベクトル

  # gVar: データベクトルの分散
  gVar    <- var(agIn)
  # gNewVar: 出力ベクトルの分散
  # agProp = (gNewVar-gVar)/gNewVar より
  gNewVar <- gVar / (1 - agProp)
  
  # 出力
  out <- agIn + rnorm(length(agIn), 0, sqrt(gNewVar - gVar))
  return(out)
}
makeData_Shop.1 <- function(){
  # purpose: 架空データ dfShopの作成
  
  # 定数 - - - - - - 
  # 店舗数
  nNUMSHOP <- 200  
  # 地域数
  nNUMAREA <- 12
  
  # ブランド1: 説明変数は{販売補助金, 宣材個数, 訪問回数}
  # 説明変数の平均ベクトル  
  agMU1 <- c(50000, 7, 4)  
  # 説明変数の分散ベクトル
  agVAR1 <- c(5000, 1, 2) 
  # 相関行列(下三角行列)
  mgTRICORR1 <- matrix(c(
     NA,  NA, NA, 
    0.3,  NA, NA, 
    0.5, 0.2, NA
  ), nrow = 3)

  # ブランド2: 説明変数は{販売補助金, 宣材個数}
  # 説明変数の平均ベクトル  
  agMU2 <- c(40000, 5)  
  # 説明変数の分散ベクトル
  agVAR2 <- c(5000, 1) 
  # 相関行列(下三角行列)
  mgTRICORR2 <- matrix(c(
     NA, NA, 
    0.2, NA
  ), nrow = 2)
  
  # ブランド3: 説明変数は{販売補助金, 宣材個数}
  # 説明変数の平均ベクトル  
  agMU3 <- c(70000, 9)  
  # 説明変数の分散ベクトル
  agVAR3 <- c(4000, 2) 
  # 相関行列(下三角行列)
  mgTRICORR3 <- matrix(c(
     NA, NA, 
    0.3, NA
  ), nrow = 2)
  
  # ほかの定数はコードに埋め込んでいる
  # - - - - - - - - - -
  
  # 乱数のシードを設定
  set.seed(12345)
  
  # 説明変数の真値を発生
  mgX1 <- sub_makeData_MVN.1(nNUMSHOP, agMU1, agVAR1, mgTRICORR1)
  mgX2 <- sub_makeData_MVN.1(nNUMSHOP, agMU2, agVAR2, mgTRICORR2)
  mgX3 <- sub_makeData_MVN.1(nNUMSHOP, agMU3, agVAR3, mgTRICORR3)
  
  # 目的変数の真値を発生
  # ブランド1
  agY1 <- 4 * log(mgX1[,1]) + 2 * mgX1[,2] + 1 * mgX1[,3] 
  # ブランド2は交互作用がある
  agY2 <- 2 * log(mgX2[,1]) + 2 * mgX2[,2] + 2*log(mgX2[,1])*mgX2[,2]
  # ブランド3はX2が効かない
  agY3 <- 4 * log(mgX3[,1]) + 0 * mgX3[,2]
  # ノイズをかぶせる
  agY1 <- sub_AddNoise.1(agY1, 0.2)
  agY2 <- sub_AddNoise.1(agY2, 0.2)
  agY3 <- sub_AddNoise.1(agY3, 0.2)
  # 出力をみながら、サイズを適当に調整する
  agY1 <- agY1 * 0.085
  agY2 <- agY2 * 0.045
  agY3 <- agY3 * 0.08
  
  # データ生成
  out <- tibble(nShopID = 1:nNUMSHOP) %>%
    mutate(
      # 来店者数
      gSize = runif(n = nNUMSHOP, min = 5000, max = 30000),
      # 売上
      nSales1 = as.integer(exp(agY1) * gSize / 1000) ,
      nSales2 = as.integer(exp(agY2) * gSize / 1000),
      nSales3 = as.integer(exp(agY3) * gSize / 1000),
      # nSales2に外れ値が出ちゃったのでまるめる(やりすぎだったかも)
      nSales2 = if_else(nSales2 > 10000, 10000L, nSales2),
      # シェア
      gShare1 = nSales1 / (nSales1 + nSales2 + nSales3),
      gShare2 = nSales2 / (nSales1 + nSales2 + nSales3),
      gShare3 = nSales3 / (nSales1 + nSales2 + nSales3),
      # 販売補助金
      nSubsidy1 = as.integer(mgX1[,1]),
      nSubsidy2 = as.integer(mgX2[,1]),
      nSubsidy3 = as.integer(mgX3[,1]),
      # 宣材個数
      nMaterial1 = as.integer(mgX1[,2]),
      nMaterial2 = as.integer(mgX2[,2]),
      nMaterial3 = as.integer(mgX3[,2]),
      # 隠れ説明変数。販売員の訪問回数ってことにしよう
      nVisit1 = as.integer(mgX1[,3]),
      nVisit1 = nVisit1 - min(nVisit1)
    )
  ## print(summary(out))

  # ブランド1の販売補助金を2種類に分割
  # まず宣材個数から予測して
  oModel <- lm(log(nSubsidy1) ~ nMaterial1, data = out)
  # 予測に0.49から0.51を掛けた値とする。ただし上限は宣材個数-1, 下限は1
  out <- out %>%
    mutate(
      nSubsidy1A = as.integer(exp(predict(oModel)) * runif(nNUMSHOP, min = 0.49, max = 0.51)), 
      nSubsidy1B = nSubsidy1 - nSubsidy1A
    )
  ## print(summary(out))
  
  # チェーンを決める
  # gX1 と gState1の第二主成分でチェーンを決める。ずるい...
  ## plot(agY1, mgX1[,1])
  oPCA <- prcomp(data.frame(gY1 = agY1, mgX1[,1]), scale. = T)
  gScore <- oPCA$x[,2]
  out <- out %>% 
    mutate( nAreaID = as.integer(cut(gScore, breaks = nNUMAREA)) )

  # 変数選択
  out <- out %>%
    dplyr::select(
      nShopID, nAreaID, gSize, starts_with("nSales"), starts_with("gShare"),
      starts_with("nSubsidy"), starts_with("nMaterial"), nVisit1
    )
  return(out)
}
# ここから本編 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

### サンプルデータの作成 - - - - - - - - - - -
dfShop <- makeData_Shop.1()
write.csv(dfShop, "./Shop.csv", row.names = FALSE)

### サンプルデータの読み込み - - - - - - - - - - - - - -
### dfShop <- read_csv("./Shop.csv")

### Code 1. サンプルデータの要約 - - - - - - - - - - - - - - 
out <- dfShop %>%
  gather(sVar, gValue) %>%
  group_by(sVar) %>%
  summarize(
    nValid = n(),
    gMin   = min(gValue),
    gMean  = mean(gValue),
    gMax   = max(gValue)
  ) %>%
  ungroup()
print(out)
write.csv(out, "./chap4_code1.csv")

### Code 2. ブランド1の変数の散布図行列 - - - - - - - - - - - - - - 
g <- ggpairs(
  dfShop %>% dplyr::select(gSize, nSubsidy1, nMaterial1, nSales1),
  aes_string(alpha=0.1),
  diag = list(continuous = "barDiag")
)
print(g)
ggsave(g, file = "./chap4_code2.png", height = 13, width = 15, units = "cm")

### Code 3. 売上の調整 - - - - - - - - - - - -
dfIn <- dfShop %>% mutate(gSalesAdj1 = nSales1 / (gSize / 1000))
g <- ggpairs(
  dfIn %>% dplyr::select(nSubsidy1, nMaterial1, gSalesAdj1),
  aes_string(alpha = 0.1),
  diag = list(continuous = "barDiag")
)
print(g)
ggsave(g, file = "./chap4_code3.png", height = 10, width = 12, units = "cm")

### Code 4. 関数形の図示 - - - - - - - - - - - - - - -
dfPlot <- data.frame(X = 1:100)

# 線形
g <- ggplot(data = dfPlot, aes(x = X, y = 20 + 3 * X))
g <- g + geom_line(size = 2)
g <- g + labs(y = "Y", title = "Y = 20 + 3 * X")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code4_1.png", width = 12, height = 8, units = "cm")

# 片側対数
g <- ggplot(data = dfPlot, aes(x = X, y = 20 + 3 * log(X)))
g <- g + geom_line(size = 2)
g <- g + labs(y = "Y", title = "Y = 20 + 3 * log(X)")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code4_2.png", width = 12, height = 8, units = "cm")

# 両側対数
g <- ggplot(data = dfPlot, aes(x = X, y = exp(3 + 0.05 * log(X))))
g <- g + geom_line(size = 2)
g <- g + labs(y = "Y", title = "Y = exp(3 + 0.05 * log(X))")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code4_3.png", width = 12, height = 8, units = "cm")

# 指数モデル
g <- ggplot(data = dfPlot, aes(x = X, y = exp(1 + 0.05 * X)))
g <- g + geom_line(size = 2)
g <- g + labs(y = "Y", title = "Y = exp(1 + 0.05 * X)")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code4_4.png", width = 12, height = 8, units = "cm")

# 逆関数モデルとロジスティックモデル
g1 <- ggplot(data = dfPlot, aes(x = X, y = exp(5 - 40/X)))
g1 <- g1 + geom_line(size = 2)
g1 <- g1 + labs(y = "Y", title = "Y = exp(5 - 40/X)")
g1 <- g1 + theme_bw()
g2 <- ggplot(data = dfPlot, aes(x = X, y = 100 / (1 + exp(5 - 0.1*X))))
g2 <- g2 + geom_line(size = 2)
g2 <- g2 + labs(y = "Y", title = "Y = 100/(1+exp(5-0.1*X))")
g2 <- g2 + theme_bw()
g <- g1 + g2
print(g)
ggsave(g, file = "./chap4_code4_5.png", width = 21, height = 8, units = "cm")

# 横軸は0:1, 縦軸はその対数ロジット
g <- ggplot(data = dfPlot, aes(x = X, y = 1 / (1 + exp(10 - 0.2*X))))
g <- g + geom_line(size = 2)
g <- g + labs(y = "S", title = "S = exp(0.01 * X / (1- 0.01 * X)))")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code4_6.png", width = 12, height = 8, units = "cm")

### Code 5. 売上の関数形(フィッティング) - - - - - - - - - - - -

# 準備
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gLogSubsidy1  = log(nSubsidy1),
    gLogSalesAdj1 = log(gSalesAdj1)
  )

# 線形
oModel1 <- lm(gSalesAdj1 ~ nSubsidy1, data = dfIn)

# 片側対数
oModel2 <- lm(gSalesAdj1 ~ gLogSubsidy1, data = dfIn)

# 両側対数
oModel3 <- lm(gLogSalesAdj1 ~ gLogSubsidy1, data = dfIn)

# 指数
oModel4 <- lm(gLogSalesAdj1 ~ nSubsidy1, data = dfIn)

# ロジスティック関数 a / (1 + exp((b-X)/c))
# これはNLS推定する。横着してSSlogis()を使う
oModel5 <- nls(
  gSalesAdj1 ~ SSlogis(nSubsidy1, a, b, c),
  data = as.data.frame(dfIn)
)

# チャートの準備
dfPredict <- data.frame(
  nSubsidy1 = seq(min(dfIn$nSubsidy1), max(dfIn$nSubsidy1), by = 100)
) %>%
  mutate(gLogSubsidy1 = log(nSubsidy1))
dfPredict <- dfPredict %>%
  mutate(
    gPredict_1 = as.vector(predict(oModel1, newdata = dfPredict)), 
    gPredict_2 = as.vector(predict(oModel2, newdata = dfPredict)),
    gPredict_3 = as.vector(exp(predict(oModel3, newdata = dfPredict))),
    gPredict_4 = as.vector(exp(predict(oModel4, newdata = dfPredict))),
    gPredict_5 = as.vector(predict(oModel5, newdata = dfPredict))
  ) %>%
  gather(sVar, gPredict, starts_with("gPredict_")) %>%
  separate(sVar, c("sVar1", "sVar2")) %>%
  mutate(
    nModel = as.integer(sVar2),
    fModel = factor(
      nModel,
      levels = 1:5,
      labels = c("線形", "片側対数", "両側対数", "指数", "ロジスティック")
    )
  )

# チャート
g <- ggplot(data = dfIn, aes(x = nSubsidy1, y = gSalesAdj1))
g <- g + geom_point(alpha = 0.2)
g <- g + geom_line(data = dfPredict, aes(x = nSubsidy1, y = gPredict, color = fModel), size = 0.8)
g <- g + scale_color_discrete(name = "")
g <- g + theme_bw(base_family = "MeiryoUI")
g1 <- g + labs(x = "販売補助支出額", y = "来店客1000人あたり売上数量")
g2 <- g + scale_x_log10() + labs(x = "販売補助支出額(対数目盛)", y = "来店客1000人あたり売上数量")
g3 <- g + scale_y_log10() + labs(x = "販売補助支出額", y = "来店客1000人あたり売上数量(対数目盛)")
g4 <- g + scale_x_log10() + scale_y_log10() +
      labs(x = "販売補助支出額(対数目盛)", y = "来店客1000人あたり売上数量(対数目盛)")
g <- g1 + g2 + g3 + g4 + plot_layout(ncol = 2)
print(g)
ggsave(g, file = "./chap4_code5.png", width = 23, height = 14, units = "cm")

### Code 6. 売上モデルの推定 - - - - - - - - - - - 

# 準備
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gSubsidyHT1   = nSubsidy1 / 100000,
    gLogSubsidy1  = log(nSubsidy1),
    gLogSalesAdj1 = log(gSalesAdj1),
    gLogMaterial1 = log(if_else(nMaterial1 == 0, 1L, nMaterial1))
  )

# 4つのモデルを推定
lModel <- list()
lModel$oModel1 <- lm(gLogSalesAdj1 ~ gSubsidyHT1    + nMaterial1, data = dfIn)
lModel$oModel2 <- lm(gLogSalesAdj1 ~ gLogSubsidy1   + nMaterial1, data = dfIn)
lModel$oModel3 <- lm(gLogSalesAdj1 ~ gSubsidyHT1    + gLogMaterial1, data = dfIn)
lModel$oModel4 <- lm(gLogSalesAdj1 ~ gLogSubsidy1   + gLogMaterial1, data = dfIn)

# 出力
out <- lModel %>%
  map_df(
    .f = function(oModel){
      # broomパッケージのtidy()関数で、推定結果をデータフレームに変換
      dfCoef1 <- tidy(oModel) %>% dplyr::select(term, estimate, std.error)
      dfCoef2 <- tibble(term = "RSQ", estimate = summary(oModel)$r.squared)
      dfCoef3 <- tibble(term = "AIC", estimate = AIC(oModel))
      out <- bind_rows(dfCoef1, dfCoef2, dfCoef3) 
      return(out)
    }, 
    .id = "sID"
  ) %>%
  gather(sVar, gValue, c(estimate, std.error)) %>%
  mutate(sVar = paste0("M", sub("^oModel", "", sID), "_", sVar)) %>%
  dplyr::select(term, sVar, gValue) %>%
  spread(sVar, gValue)
print(out)
write.csv(out, "./chap4_code6.csv", row.names = FALSE)

# 回帰診断
par(mfrow=c(2,2))
plot(lModel$oModel2)

### Code 7. シェアの観察 - - - - - - - - -
g1 <- ggpairs(
  dfShop %>% dplyr::select(nSubsidy1, nMaterial1, gShare1),
  aes_string(alpha=0.1),
  diag = list(continuous = "barDiag")
)
g2 <- ggpairs(
  dfShop %>% dplyr::select(nSubsidy2, nMaterial2, gShare2),
  aes_string(alpha=0.1),
  diag = list(continuous = "barDiag")
)
g3 <- ggpairs(
  dfShop %>% dplyr::select(nSubsidy3, nMaterial3, gShare3),
  aes_string(alpha=0.1),
  diag = list(continuous = "barDiag")
)
print(g1)
print(g2)
print(g3)
ggsave(g1, file = "./chap4_code7_1.png", width = 23, height = 14, units = "cm")
ggsave(g2, file = "./chap4_code7_2.png", width = 23, height = 14, units = "cm")
ggsave(g3, file = "./chap4_code7_3.png", width = 23, height = 14, units = "cm")

## Code 8. シェアの単純な関数形(フィッティング) - - - - - - - - - - -

# 準備
dfIn <- dfShop %>%
  mutate(
    gLogSubsidy1 = log(nSubsidy1),
    gOdds        = gShare1 / (1 - gShare1),
    gLogit       = log(gOdds)
  )

# 線形
oModel1 <- lm(gShare1 ~ nSubsidy1, data = dfIn)
# 線形, 対数
oModel2 <- lm(gShare1 ~ gLogSubsidy1, data = dfIn)
# ロジット
oModel3 <- lm(gLogit ~ nSubsidy1, data = dfIn)
# ロジット, 対数
oModel4 <- lm(gLogit ~ gLogSubsidy1, data = dfIn)

# 予測の準備
dfPredict <- tibble(
  nSubsidy1 = seq(30000, 70000, by = 10)
) %>%
  mutate(gLogSubsidy1 = log(nSubsidy1))

# 予測
dfPredict <- dfPredict %>%
  mutate(
    gPredict_1 = predict(oModel1, newdata = dfPredict),
    gPredict_2 = predict(oModel2, newdata = dfPredict),
    gPredict_3 = 1/(1 + exp(-predict(oModel3, newdata = dfPredict))),
    gPredict_4 = 1/(1 + exp(-predict(oModel4, newdata = dfPredict))),
  ) %>%
  gather(sVar, gPredict, starts_with("gPredict_")) %>%
  mutate(gLogit = log(gPredict / (1 - gPredict))) %>%
  separate(sVar, c("sVar1", "sVar2")) %>%
  mutate(
    nModel = as.integer(sVar2),
    fModel = factor(
      nModel,
      levels = 1:4,
      labels = c("線形", "線形, Xを対数変換", "対数オッズ", "対数オッズ,Xを対数変換")
    )
  )
# チャート
g <- ggplot(data = dfIn, aes(x = nSubsidy1, y = gShare1))
g <- g + geom_point(alpha = 0.2)
g <- g + geom_line(data = dfPredict, aes(x = nSubsidy1, y = gPredict, color=fModel), size = 1)
g <- g + theme_bw()
g1 <- g + labs(x = "補助金", y = "売上シェア")
g2 <- g + scale_x_log10() + labs(x = "補助金(対数目盛)", y = "売上シェア")

g <- ggplot(data = dfIn, aes(x = nSubsidy1, y = gLogit))
g <- g + geom_point(alpha = 0.2)
g <- g + geom_line(
  data = dfPredict, 
  aes(x = nSubsidy1, y = gLogit, color = fModel), 
  size = 1
)
g <- g + theme_bw()
g3 <- g + labs(x = "補助金", y = "売上シェア(対数オッズ)")
g4 <- g + scale_x_log10() + labs(x = "補助金(対数目盛)", y = "売上シェア(対数オッズ)")

g <- g1 + g2 + g3 + g4 + plot_layout(ncol = 2)
print(g)
ggsave(g, file = "./chap4_code8.png", width = 23, height = 14, units = "cm")

### Code 9. シェアモデル - - - - - - - - - - - - - - - - -
# 以下、添字 i は略記する。
# ここでは吸引力モデルを
#   log(A_b) = \beta_{1b} + \beta_{2b} log(X_{2b}) + \beta_{3b} X_{3b}
# とする。いっぽうシェアモデルは、対数中心化したシェア S^*_{bi}について
#   S^*_{b} = log(A_b) - (1/B) \sum_j log(A_j)
# これに代入して
# S^*_{b} = \beta_{1b}
#           + \beta_{2b} log(X_{2b})
#           + \beta_{3b} log(X_{3b})
#           - (1/B) \sum_j \beta_{1j}
#           - (1/B) \sum_j \beta_{2j} log(X_{2j})
#           - (1/B) \sum_j \beta_{3j} log(X_{3j})
# いま、b=jのとき1, そうでないときに0となる関数をI(b,j)とすれば
# S^*_{b} = \beta_{1b} - (1/B) \sum_j \beta_{1j}
#           + \beta_{2b} log(X_{2b})
#           + \sum_j \beta_{2j} { I(b,j) log(X_{2b}) - (1/B) log(X_{2j})}
#           + \beta_{3b} X_{3b}
#           + \sum_j \beta_{3j} { I(b,j) X_{3b} - (1/B) X_{2j} }
# なので、b=(1,2,3)について
# X^*_{2b}: ブランドbの行では 2/3 * log(X_{2b}), 他の行では -(1/3)*log(X_{2b})
# X^*_{3b}: ブランドbの行では 2/3 * X_{3b},      他の行では -(1/3)*X_{3b}
# として、回帰モデル
# S^*_{b} = \beta^*_{1b} + \sum_j \beta^*_{2j} X^*_{2j} + \sum_j \beta^*_{3j} X^*_{3j}
# を推定すればよいはず。(ちがってたらすいません)

dfIn <- dfShop %>%
  # 必要な変数を選び
  dplyr::select(
    nShopID, starts_with("gShare"), starts_with("nSubsidy"),
    nMaterial1, nMaterial2, nMaterial3
  ) %>%
  # シェアをlong型にする
  gather(sVar, gShare, starts_with("gShare")) %>%
  mutate(
    nBrand = as.integer(sub("gShare", "", sVar))
  ) %>%
  dplyr::select(-sVar) %>%
  # シェアを対数に変換して店舗ごとに中心化
  mutate(gLogShare = log(gShare)) %>%
  group_by(nShopID) %>%
  mutate(gLCShare = gLogShare - mean(gLogShare)) %>%
  ungroup() %>%
  # 説明変数をつくる
  mutate(
    X11 = if_else(nBrand == 1, 2/3, -1/3) * log(nSubsidy1),
    X12 = if_else(nBrand == 2, 2/3, -1/3) * log(nSubsidy2),
    X13 = if_else(nBrand == 3, 2/3, -1/3) * log(nSubsidy3),
    X21 = if_else(nBrand == 1, 2/3, -1/3) * nMaterial1,
    X22 = if_else(nBrand == 2, 2/3, -1/3) * nMaterial2,
    X23 = if_else(nBrand == 3, 2/3, -1/3) * nMaterial3
  )
oModel <- lm(
  gLCShare ~ 0 + as.factor(nBrand) + X11 + X12 + X13 + X21 + X22 + X23,
  data = dfIn
)
dfCoef <- tidy(oModel) %>%
  dplyr::select(term, estimate, std.error) %>%
  # termの最後の文字がブランド番号, その前が項の名前
  mutate(
    nBrand = as.integer(substr(term, nchar(term), nchar(term))),
    sTerm  = substr(term, 1, nchar(term) - 1)
  ) %>%
  dplyr::select(sTerm, nBrand, estimate, std.error) %>%
  arrange(sTerm, nBrand)
print(dfCoef)
write.csv(dfCoef, "./chap4_code9.csv", row.names = FALSE)

## Code 10. 訪問回数がわかった - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>% mutate(gSalesAdj1 = nSales1 / gSize * 1000)
g <- ggpairs(
  dfIn %>% dplyr::select(nSubsidy1, nMaterial1, nVisit1, gSalesAdj1),
  aes_string(alpha = 0.1),
  diag = list(continuous = "barDiag")
)
print(g)
ggsave(g, file = "./chap4_code10.png", height = 10, width = 12, units = "cm")

### Code 11. 訪問回数を追加 - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>% mutate(
  gLogSalesAdj1 = log(nSales1 / gSize * 1000),
  gLogSubsidy1  = log(nSubsidy1),
)
lModel <- list()
lModel$oModel1 <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1, data = dfIn)
lModel$oModel2 <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1 + nVisit1, data = dfIn)
lOut <- lapply(
  seq_along(lModel),
  function(nModelID){
    oModel <- lModel[[nModelID]]
    dfCoef1 <- tidy(oModel)
    dfCoef2 <- tibble(term = "RSQ", estimate = summary(oModel)$r.squared)
    dfCoef3 <- tibble(term = "AIC", estimate = AIC(oModel))
    out <- bind_rows(dfCoef1, dfCoef2, dfCoef3) %>%
      mutate(nModelID = nModelID) %>%
      dplyr::select(nModelID, term, estimate, std.error)
    return(out)
  }
)
out <- bind_rows(lOut) %>%
  gather(sVar, gValue, c(estimate, std.error)) %>%
  mutate(sVar = paste0("B", nModelID, "_", sVar)) %>%
  dplyr::select(-nModelID) %>%
  spread(sVar, gValue)
print(out)
write.csv(out, "./chap4_code11.csv", row.names = FALSE)

### Code 12. タイプ別補助金 - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>% mutate(gSalesAdj1 = nSales1 / gSize * 1000)
g <- ggpairs(
  dfIn %>% dplyr::select(nSubsidy1A, nSubsidy1B, nMaterial1, gSalesAdj1),
  aes_string(alpha = 0.1),
  diag = list(continuous = "barDiag")
)
print(g)
ggsave(g, file = "./chap4_code12.png", height = 10, width = 12, units = "cm")

### Code 13. タイプ別補助金の投入 - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gLogSalesAdj1 = log(gSalesAdj1),
    gLogSubsidy1  = log(nSubsidy1),
    gLogSubsidy1A  = log(nSubsidy1A),
    gLogSubsidy1B  = log(nSubsidy1B)
  )
lModel <- list()
lModel$oModel1 <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1, data = dfIn)
lModel$oModel2 <- lm(gLogSalesAdj1 ~ gLogSubsidy1A + gLogSubsidy1B + nMaterial1, data = dfIn)
out <- lModel %>%
  map_dfr(
    .f = function(oModel){
      dfCoef1 <- tidy(oModel) %>% dplyr::select(term, estimate, std.error)
      agVIF <- vif(oModel)
      dfVIF <- tibble(term = names(agVIF), gVIF = as.vector(agVIF))
      dfCoef1 <- dfCoef1 %>% left_join(dfVIF, by = "term")
      dfCoef2 <- tibble(term = "RSQ", estimate = summary(oModel)$r.squared)
      dfCoef3 <- tibble(term = "AIC", estimate = AIC(oModel))
      out <- bind_rows(dfCoef1, dfCoef2, dfCoef3) 
      return(out)
    }, 
    .id = "sID"
  ) %>%
  gather(sVar, gValue, c(estimate, std.error, gVIF)) %>%
  mutate(sVar = paste0("M", sub("^oModel", "", sID), "_", sVar)) %>%
  dplyr::select(term, sVar, gValue) %>%
  spread(sVar, gValue)
print(out)
write.csv(out, "./chap4_code13.csv", row.names = FALSE)

### Code 14. 交互作用と関数形 - - - - - - - - - - - - - - - - -

dfPlot <- tibble(
  X1 = factor(c(0, 0, 1, 1)),
  X2 = factor(c(0, 1, 0, 1)),
  Y  = c(100,200,300,400)
)
g <- ggplot(data = dfPlot, aes(x = X1, y = Y, color = X2, group = X2))
g <- g + geom_line(size = 2)
g <- g + geom_point(size = 4)
g <- g + theme_bw()
g1 <- g
g2 <- g + scale_y_log10() + labs(y = "Y(対数目盛)")
g <- g1 + g2
print(g)
ggsave(g, file = "./chap4_code14_1.png", width = 15, height = 5, units = "cm")

g <- ggplot(data = dfPlot, aes(x = X1, y = exp(Y/100), color = X2, group = X2))
g <- g + geom_line(size = 2)
g <- g + geom_point(size = 4)
g <- g + theme_bw()
g1 <- g + labs(y = "Y")
g2 <- g + scale_y_log10() + labs(y = "Y(対数目盛)")
g <- g1 + g2
print(g)
ggsave(g, file = "./chap4_code14_2.png", width = 15, height = 5, units = "cm")

### Code 15. 交互作用を探す - - - - - - - - - - - - - - - - -

# 準備
dfPlot <- dfShop %>%
  dplyr::select(
    nShopID, gSize, 
    one_of(paste0("nSales", 1:3)), 
    one_of(paste0("nSubsidy", 1:3)), 
    one_of(paste0("nMaterial", 1:3))
  ) %>%
  gather(sVar, gValue, -c(nShopID, gSize)) %>%
  mutate(
    nBrand = as.integer(substr(sVar, nchar(sVar), nchar(sVar))),
    sVar   = substr(sVar, 1, nchar(sVar) - 1)
  ) %>%
  spread(sVar, gValue) %>%
  mutate(
    gLogSalesAdj = log(nSales / gSize * 1000),
    gLogSubsidy  = log(nSubsidy)
  )　

# プロット
lOut <- lapply(
  1:3,
  function(nCurrent){
    g <- ggplot(
      data = dfPlot %>% 
        filter(nBrand == nCurrent) %>% 
        mutate(fMaterial = cut(nMaterial, breaks = 2)),
      aes(x = gLogSubsidy, y = gLogSalesAdj, color = fMaterial)
    )
    g <- g + geom_point()
    g <- g + geom_smooth(method = "lm", se = FALSE)
    g <- g + scale_color_discrete(name = "宣材送付個数")
    g <- g + labs(
      x = "販売補助支出額(対数)", y = "来店者数あたり売上数量(対数)",
      title = paste0("ブランド", nCurrent)
    )
  }
)
g <- wrap_plots(lOut) & theme_bw()
print(g)
ggsave(g, file = "./chap4_code15.png", width = 24, height = 9, units = "cm")

### Code 16. 交互作用項の投入 - - - - - - - - - - - - - - - - -

# 準備
dfIn <- dfShop %>%
  mutate(
    gSalesAdj2    = nSales2 / gSize * 1000,
    gLogSalesAdj2 = log(gSalesAdj2),
    gLogSubsidy2  = log(nSubsidy2)
  )
# 推定
lModel <- list()
lModel$oModel1 <- lm(gLogSalesAdj2 ~ gLogSubsidy2 + nMaterial2, data=dfIn)
lModel$oModel2 <- lm(gLogSalesAdj2 ~ gLogSubsidy2 * nMaterial2, data=dfIn)
# 出力
out <- lModel %>%
  map_dfr(
    .f = function(oModel){
      dfCoef1 <- tidy(oModel) %>% dplyr::select(term, estimate, std.error)
      dfCoef2 <- tibble(term = "RSQ", estimate = summary(oModel)$r.squared)
      dfCoef3 <- tibble(term = "AIC", estimate = AIC(oModel))
      agVIF <- vif(oModel)
      dfVIF <- tibble(gVIF = agVIF, term = names(agVIF))
      out <- bind_rows(dfCoef1, dfCoef2, dfCoef3) %>%
        left_join(dfVIF, by = "term") 
      return(out)
    }, 
    .id = "sID"
  ) %>%
  gather(sVar, gValue, c(estimate, std.error, gVIF)) %>%
  mutate(sVar = paste0("B", sub("^oModel", "", sID), "_", sVar)) %>%
  dplyr::select(-sID) %>%
  spread(sVar, gValue)
print(out)
write.csv(out, "chap4_code16.csv", row.names = FALSE)

### Code 17. 不均一性・非正規性 - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gLogSubsidy1  = log(nSubsidy1)
  )
g <- ggplot(data=dfIn, aes(x = gLogSubsidy1, y = gSalesAdj1))
g <- g + geom_point()
g <- g + geom_smooth(method = "lm", se = FALSE)
g <- g + labs(x = "販売補助支出額(対数)", y = "来店客あたり売上数量")
g <- g + theme_bw()
print(g)

# 回帰診断
oModel <- lm(gSalesAdj1 ~ gLogSubsidy1, data=dfIn)
par(mfrow=c(2,2))
plot(oModel)

### Code 18. 集計バイアス - - - - - - - - - - - - - - - - -
dfAggregate <- dfShop %>%
  group_by(nAreaID) %>%
  summarize(
    nNumShop = n(),
    gSize = sum(gSize),
    nSales1 = sum(nSales1),
    gSalesAdj1 = nSales1 / gSize * 1000,
    nSubsidy1 = sum(nSubsidy1),
    nMaterial1 = sum(nMaterial1),
    gLogSalesAdj1 = log(gSalesAdj1),
    gLogSubsidy1  = log(nSubsidy1)
  ) %>%
  ungroup()
g <- ggpairs(
  dfAggregate %>% dplyr::select(nSubsidy1, nMaterial1, gSalesAdj1),
  aes_string(alpha = 0.1),
  diag = list(continuous = "barDiag")
)
print(g)
ggsave(g, file = "./chap4_code18.png", height = 10, width = 12, units = "cm")

oModel <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1, data = dfAggregate)
dfCoef1 <- tidy(oModel) %>%
  dplyr::select(term, estimate, std.error)
dfCoef2 <- tibble(term = "RSQ", estimate = summary(oModel)$r.squared)
out <- bind_rows(dfCoef1, dfCoef2)
print(out)
write.csv(out, "./chap4_code18.csv", row.names = FALSE)

### Code 19. 種明かし - - - - - - - - - - - - - - - - -
dfIn <- dfShop %>% 
  mutate(
    gSalesAdj1 = nSales1 / (gSize / 1000), 
    fAreaID = factor(nAreaID)
  )
g1 <- ggplot(data = dfIn, aes(x = nSubsidy1, y = gSalesAdj1, color = fAreaID))
g1 <- g1 + geom_point(size = 3, alpha = 0.5)
g1 <- g1 + labs(x = "販売補助支出額", y = "来店客1000人あたり売上数量")
g1 <- g1 + scale_color_discrete(name = "チェーンID")
g1 <- g1 + theme_bw()
ggsave(g1, file = "./chap4_code19.png", width = 24, height = 12, units = "cm")

### Code 20. パラメータ推定の信頼区間 - - - - - - - - - - - - - - - - -

# dfShop <- read_csv("./Shop.csv")
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gLogSalesAdj1 = log(gSalesAdj1),
    gLogSubsidy1  = log(nSubsidy1)
  )
oModel <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1, data = dfIn)
# 係数の推定値
coef(oModel)
# 信頼区間
confint(oModel)

## Code 21. シミュレーション - - - - - - - - - - - - - - - - -

# データの読み込み
# dfShop <- read_csv("./Shop.csv")

# 必要な変数を選ぶ
dfIn <- dfShop %>%
  mutate(
    gSalesAdj1    = nSales1 / gSize * 1000,
    gLogSalesAdj1 = log(gSalesAdj1),
    gLogSubsidy1  = log(nSubsidy1)
  )
# モデル推定
oModel <- lm(gLogSalesAdj1 ~ gLogSubsidy1 + nMaterial1, data = dfIn)
# パラメータの推定値
agCoef <- coef(oModel)
# パラメータ推定量の共分散行列
mgVcov <- vcov(oModel)
# 撹乱項のSD
gShat <- summary(oModel)$sigma

# パラメータをドローする回数
nM1 <- 10000
# 撹乱項をドローする回数
nM2 <- 500
# シナリオのベクトル。補助金の店舗あたり増減を表す
agC <- seq(from = -20000, to = 20000, by = 5000)

set.seed(123)

# シナリオでループする
# シナリオ数を調べてプログレスバーをセット
oPG <- progress_estimated(length(agC))

cat("[code 21] make mgLogYhat ...\n")
dfSimulation <- map_dfr(
  agC, 
  .f = function(gC){
    oPG$tick()
    oPG$print()
    # X行列を生成する
    mgX <- dfIn %>%
      mutate(
        Intercept = 1,
        gLogSubsidy1 = log(nSubsidy1 + gC)
      ) %>%
      dplyr::select(Intercept, gLogSubsidy1, nMaterial1) %>%
      as.matrix(.)
    # パラメータをnM1回ドローする
    # 行: ドロー, 列: パラメータ
    mgParamDraw <- MASS::mvrnorm(nM1, agCoef, mgVcov)
    # 来店客1000人あたり売上数量の対数の予測値(撹乱項なし)
    # 行: 店舗, 列:パラメータドロー
    mgLogYhat <- mgX %*% t(mgParamDraw)
    colnames(mgLogYhat) <- paste0("gLogYhat_", 1:ncol(mgLogYhat))
    # 横にくっつけて
    out <- data.frame(
      dfIn %>% dplyr::select(nShopID, gSize, nSales1),
      mgLogYhat
    ) %>%
      # longにする。行は店舗xパラメータドロー
      gather(sVar, gLogYhat, starts_with("gLogYhat_")) %>%
      separate(sVar, c("sVar1", "sVar2")) %>%
      mutate(nCoefDraw = as.integer(sVar2)) %>%
      dplyr::select(-c(sVar1, sVar2)) %>%
      # gCを記録しておく
      mutate(gC = gC)
    return(out)
  }
) 

# 行数を調べてプログレスバーをセット
oPG <- progress_estimated(nrow(dfSimulation))

cat("[code 21] make prediction & expectation ...\n")
dfSimulation <- dfSimulation %>%
  mutate(
    # gPrediction: 予測値のシミュレーション
    # 撹乱項をドローして足して指数をとる
    gPrediction = exp(gLogYhat + rnorm(n = length(gLogYhat), mean = 0, sd = gShat)),
    # gExpect: 期待値のシミュレーション
    # 撹乱項をたくさんドローして足して指数をとって平均する
    # ここでプログレスバーを使っている
    gExpect = purrr::map_dbl(
      gLogYhat,
      .f = function(x){
        oPG$tick()
        oPG$print()
        mean(exp(x + rnorm(n = nM2, mean = 0, sd = gShat)))
      }
    ),
    gPrediction = gPrediction * gSize / 1000,
    gExpect     = gExpect * gSize / 1000,
    fC          = as.factor(gC)
  )

# 統計量
dfStat <- dfSimulation %>%
  group_by(fC) %>%
  summarize(
    gPrediction = mean(gPrediction),
    gExpect     = mean(gExpect)
  ) %>%
  ungroup() %>%
  mutate(
    sPrediction = as.integer(gPrediction),
    sExpect     = as.integer(gExpect)
  )
# print(dfStat)
# stop()

cat("plot prediction ... \n")
g <- ggplot(data = dfSimulation, aes(x = fC, y = gPrediction))
g <- g + geom_violin(linetype = "blank", fill = "blue", alpha = 0.2)
g <- g + geom_path(data = dfStat, group = 1, color = "blue")
g <- g + geom_point(data = dfStat, color = "blue", size = 2)
g <- g + geom_point(
  data = dfSimulation %>% filter(gC == 0, nCoefDraw == 1),
  aes(y = nSales1),
  color = "black", alpha = 0.2, size  = 1, shape = 1
)
g <- g + geom_text(
  data = dfStat, aes(label = sPrediction),
  color = "blue", size = 4, nudge_y = -300
)
g <- g + coord_cartesian(ylim = c(0, 12000))
g <- g + labs(x = "店舗の販売補助支出額の増減(円)", y = "店舗の売上数量の予測値(個)")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code21_1.png", height = 11, width = 17, units = "cm")

cat("plot expect ... \n")
g <- ggplot(data = dfSimulation, aes(x = fC, y = gExpect))
g <- g + geom_violin(linetype = "blank", fill = "blue", alpha = 0.2)
g <- g + geom_path(data = dfStat, group = 1, color = "blue")
g <- g + geom_point(data = dfStat, color = "blue", size = 2)
g <- g + geom_point(
  data = dfSimulation %>% filter(gC == 0, nCoefDraw == 1),
  aes(y = nSales1),
  color = "black", alpha = 0.2, size  = 1, shape = 1
)
g <- g + geom_text(
  data = dfStat, aes(label = sExpect),
  color = "blue", size = 4, nudge_y = -300
)
g <- g + coord_cartesian(ylim = c(0, 12000))
g <- g + labs(x = "店舗の販売補助支出額の増減(円)", y = "店舗の売上数量の期待値(個)")
g <- g + theme_bw()
print(g)
ggsave(g, file = "./chap4_code21_2.png", height = 11, width = 17, units = "cm")
