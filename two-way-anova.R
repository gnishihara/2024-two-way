# 二元配置分散分析
# 2024年7月8日，7月16日
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(emmeans)

# フォルダー名定義して・フォルダ内のファイ名を抽出する
folder = "./Data"
filename = dir(folder, full.names = TRUE)

# データの読み込み
# read.csv() よりも read_csv() を使うこと
d1 = read_csv(filename)

# 生データの確認（作図と集計）
ggplot(d1) +
  geom_point(aes(x = ph, y = weight, color = Calluna),
             position = position_dodge(width = 0.2))

d2 = d1 |> 
  group_by(ph, Calluna) |> 
  summarise(weight_mean = mean(weight),
            weight_sd = sd(weight),
            weight_n  = length(weight),
            .groups = "drop") |> 
  mutate(weight_se = weight_sd / sqrt(weight_n - 1))

d2

ggplot(d2) +
  geom_point(aes(x = ph, y = weight_mean, 
                 color = Calluna),
             position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(x = ph,
                    ymin = weight_mean - weight_se,
                    ymax = weight_mean + weight_se,
                    color = Calluna),
                position = position_dodge(width = 0.2),
                width = 0.2)


# データ解析: 二元配置分散分析
# 説明変数:　ph, Calluna の有り無し
# 応答変数: Festuca の重量 (weight)
# H0: ph による影響はない
# H1: Callunaの有り無しによる影響はない
# H2: ph と Calluna に相互作用はない

mfull = lm(weight ~ ph + Calluna + ph:Calluna, data = d1) # フルモデル (full model)
mfull |> summary.aov() # summary.aov(mfull)

# 診断図：正規性と等分散性の確認

plot(mfull, which = 1) # 等分散性　残渣たい期待値
plot(mfull, which = 3) # 等分散性　標準化した残渣の絶対値の平方根たい期待値
plot(mfull, which = 2) # 正規性 (qqplot) 


# 観測値を変換する
# log(), sqrt()
# weight の平方根で解析した
e1 = d1 |> 
  mutate(weight = sqrt(weight))

e2 = e1 |> 
  group_by(ph, Calluna) |> 
  summarise(weight_mean = mean(weight),
            weight_sd = sd(weight),
            weight_n  = length(weight),
            .groups = "drop") |> 
  mutate(weight_se = weight_sd / sqrt(weight_n - 1))


ggplot(e2) +
  geom_point(aes(x = ph, y = weight_mean, 
                 color = Calluna),
             position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(x = ph,
                    ymin = weight_mean - weight_se,
                    ymax = weight_mean + weight_se,
                    color = Calluna),
                position = position_dodge(width = 0.2),
                width = 0.2)

# データ解析: 二元配置分散分析
# 説明変数:　ph, Calluna の有り無し
# 応答変数: Festuca の重量 (weight)
# H0: ph による影響はない
# H1: Callunaの有り無しによる影響はない
# H2: ph と Calluna に相互作用はない

mfull = lm(weight ~ ph + Calluna + ph:Calluna, data = e1) # フルモデル (full model)
mfull |> summary.aov() # summary.aov(mfull)

# 診断図：正規性と等分散性の確認

plot(mfull, which = 1) # 等分散性　残渣たい期待値
plot(mfull, which = 3) # 等分散性　標準化した残渣の絶対値の平方根たい期待値
plot(mfull, which = 2) # 正規性 (qqplot) 



# weight のログで解析した
#
l1 = d1 |> 
  mutate(weight = log(weight))

l2 = l1 |> 
  group_by(ph, Calluna) |> 
  summarise(weight_mean = mean(weight),
            weight_sd = sd(weight),
            weight_n  = length(weight),
            .groups = "drop") |> 
  mutate(weight_se = weight_sd / sqrt(weight_n - 1))


ggplot(l2) +
  geom_point(aes(x = ph, y = weight_mean, 
                 color = Calluna),
             position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(x = ph,
                    ymin = weight_mean - weight_se,
                    ymax = weight_mean + weight_se,
                    color = Calluna),
                position = position_dodge(width = 0.2),
                width = 0.2)

# データ解析: 二元配置分散分析
# 説明変数:　ph, Calluna の有り無し
# 応答変数: Festuca の重量 (weight)
# H0: ph による影響はない
# H1: Callunaの有り無しによる影響はない
# H2: ph と Calluna に相互作用はない

mfull = lm(weight ~ ph + Calluna + ph:Calluna, data = l1) # フルモデル (full model)
mfull |> summary.aov() # summary.aov(mfull)

# 診断図：正規性と等分散性の確認

plot(mfull, which = 1) # 等分散性　残渣たい期待値
plot(mfull, which = 3) # 等分散性　標準化した残渣の絶対値の平方根たい期待値
plot(mfull, which = 2) # 正規性 (qqplot) 


