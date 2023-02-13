#### メッシュ農業気象データ：気候変化シナリオ ####
library(tidyverse)

#--作業用ディレクトリを設定
# setwd("write your path")

#### 関数 ####
#--複数ファイルを読み込んでデータフレームを返す
MakeDataFrame <- function(files) {
  data <- NULL
  for (i in seq_along(files)) {
    df <- read.csv(files[i]) %>%
      mutate(mmdd = format(as.POSIXlt(date), "%m-%d")) %>%
      select(c(mmdd, TMP_max, APCP)) %>%
      filter(mmdd != "02-29")
    colnames(df) <- c("mmdd", sprintf("Tmax%02d",i), sprintf("APCP%02d",i))
    if( is.null(data) ){
      data <- df
    } else {
      data <- inner_join(data, df, by="mmdd")
    }
  }
  return(data)
}

#### データ読み込み ####
#--データ読み込み
curr_files <- list.files("data", pattern = "tsukuba_anl_", full.names = T)
rcp26_files <- list.files("data", pattern = "tsukuba_MIROC5_RCP2.6_", full.names = T)
rcp85_files <- list.files("data", pattern = "tsukuba_MIROC5_RCP8.5_", full.names = T)

df_curr <- MakeDataFrame(curr_files)
df_rcp26 <- MakeDataFrame(rcp26_files)
df_rcp85 <- MakeDataFrame(rcp85_files)

#--最高気温
tmax_curr <- df_curr %>% select(c(mmdd, starts_with("Tmax")))
tmax_rcp26 <- df_rcp26 %>% select(c(mmdd, starts_with("Tmax")))
tmax_rcp85 <- df_rcp85 %>% select(c(mmdd, starts_with("Tmax")))
# 20年平均と標準偏差
tmax_curr$mean <- tmax_curr %>% select(-"mmdd") %>% apply(1, mean)
tmax_curr$sd <- tmax_curr %>% select(-"mmdd") %>% apply(1, sd)
tmax_rcp26$mean <- tmax_rcp26 %>% select(-"mmdd") %>% apply(1, mean)
tmax_rcp26$sd <- tmax_rcp26 %>% select(-"mmdd") %>% apply(1, sd)
tmax_rcp85$mean <- tmax_rcp85 %>% select(-"mmdd") %>% apply(1, mean)
tmax_rcp85$sd <- tmax_rcp85 %>% select(-"mmdd") %>% apply(1, sd)

#--降水量
apcp_curr <- df_curr %>% select(c(mmdd, starts_with("APCP")))
apcp_rcp26 <- df_rcp26 %>% select(c(mmdd, starts_with("APCP")))
apcp_rcp85 <- df_rcp85 %>% select(c(mmdd, starts_with("APCP")))

#### 最高気温グラフ ####
#--時系列
# 各データとも一番先の年代のデータを使う
tmax_curr %>%
  mutate(mmdd = as.Date(mmdd, format = "%m-%d")) %>%
  mutate(Tmax_curr = Tmax20, Tmax_rcp26 = tmax_rcp26$Tmax20, Tmax_rcp85 = df_rcp85$Tmax20) %>%
  ggplot(aes(x = mmdd, y = Tmax_curr, color = "Current")) +
  geom_line() + 
  geom_line(aes(y = Tmax_rcp26, color = "RCP2.6")) +
  geom_line(aes(y = Tmax_rcp85, color = "RCP8.5")) +
  scale_x_date(date_labels = "%m-%d", name = "Date") +
  labs(x = "Date", y = "Max Temperature", color = "") +
  labs(title = "Max Temperature of Current (2020) and RCP2.6, RCP8.5 (2070)") +
  scale_color_manual(values = c("black","blue","red")) + 
  theme(legend.position = "top")

# 各データとも20年平均のデータを使う
tmax_curr %>%
  mutate(mmdd = as.Date(mmdd, format = "%m-%d")) %>%
  mutate(Tmax_curr = mean, Tmax_rcp26 = tmax_rcp26$mean, Tmax_rcp85 = tmax_rcp85$mean) %>%
  ggplot(aes(x = mmdd, y = Tmax_curr, color = "Current")) +
  geom_line() + 
  geom_line(aes(y = Tmax_rcp26, color = "RCP2.6")) +
  geom_line(aes(y = Tmax_rcp85, color = "RCP8.5")) +
  scale_x_date(date_labels = "%m-%d", name = "Date") +
  labs(x = "Date", y = "Max Temperature", color = "") +
  labs(title = "Mean Max Temperature of Current (2001-2020) and RCP2.6, RCP8.5 (2051-2070)") +
  scale_color_manual(values = c("black","blue","red")) +
  theme(legend.position = "top")

#### 最高気温の要約統計量 ####
# 平均や最大値
curr_long <- tmax_curr %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "Tmax")
curr_long %>% summary()

rcp26_long <- tmax_rcp26 %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "Tmax")
rcp26_long %>% summary()

rcp85_long <- tmax_rcp85 %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "Tmax")
rcp85_long %>% summary()

# 平均の差
rcp26_long %>% summarise(mean(Tmax)) - curr_long %>% summarise(mean(Tmax))
rcp85_long %>% summarise(mean(Tmax)) - curr_long %>% summarise(mean(Tmax))

# 30度以上の日数(年平均)
curr_long %>% filter(Tmax >= 30) %>% summarise(n() / 20)
rcp26_long %>% filter(Tmax >= 30) %>% summarise(n() / 20)
rcp85_long %>% filter(Tmax >= 30) %>% summarise(n() / 20)

# 日付ごとに30度以上になる確率を計算（正規分布を仮定）
tmax_curr$proba30C <- 1- apply(tmax_curr[c("mean","sd")], 1, function(i,p){ pnorm(p,i[1],i[2])}, 30)
tmax_rcp26$proba30C <- 1 - apply(tmax_rcp26[c("mean","sd")], 1, function(i,p){ pnorm(p,i[1],i[2])}, 30)
tmax_rcp85$proba30C <- 1 - apply(tmax_rcp85[c("mean","sd")], 1, function(i,p){ pnorm(p,i[1],i[2])}, 30)

# 70%以上の確率で30度以上になる日数をカウント
tmax_curr %>% filter(proba30C >= 0.7) %>% summarise(n())
tmax_rcp26 %>% filter(proba30C >= 0.7) %>% summarise(n())
tmax_rcp85 %>% filter(proba30C >= 0.7) %>% summarise(n())

#### 降水量の要約統計量 ####
# 平均や最大値
curr_long <- apcp_curr %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "APCP")
curr_long %>% summary()
curr_long %>% filter(APCP >= 0.5) %>% summary()

rcp26_long <- apcp_rcp26 %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "APCP")
rcp26_long %>% summary()
rcp26_long %>% filter(APCP >= 0.5) %>% summary()

rcp85_long <- apcp_rcp85 %>% 
  pivot_longer(cols = -"mmdd", names_to = "year", values_to = "APCP")
rcp85_long %>% summary()
rcp85_long %>% filter(APCP >= 0.5) %>% summary()

#### 降水量のグラフ ####
curr_long %>%
  mutate(APCP_curr = APCP, APCP_rcp26 = rcp26_long$APCP, APCP_rcp85 = rcp85_long$APCP) %>%
  ggplot(aes(x = APCP_curr, color = "Current", fill = "Current")) +
  geom_histogram(alpha = 0.5, binwidth = 10, na.rm = TRUE) + 
  geom_histogram(aes(x = APCP_rcp85, color = "RCP8.5", fill = "RCP8.5"), alpha = 0.5, binwidth = 10, na.rm = TRUE) +
  xlim(c(20, 200)) +
  labs(x = "Precipitation (mm)", y = "Days", color = "", fill = "") +
  labs(title = "Daily Precipitation (>=20mm) of Current (2001-2020) and RCP8.5 (2051-2070)")

#### 確率雨量の解析 ####
# 年最大日降水量
max_apcp <- cbind(
  apcp_curr %>% select(-mmdd) %>% apply(2, max),
  apcp_rcp26 %>% select(-mmdd) %>% apply(2, max),
  apcp_rcp85 %>% select(-mmdd) %>% apply(2, max)
)
colnames(max_apcp) <- c("Current","RCP26","RCP85")
max_apcp <- as.data.frame(max_apcp)

# 対数正規分布を仮定
qlnorm(0.99, meanlog = mean(log(max_apcp$Current)), sdlog = sd(log(max_apcp$Current)))
qlnorm(0.99, meanlog = mean(log(max_apcp$RCP26)), sdlog = sd(log(max_apcp$RCP26)))
qlnorm(0.99, meanlog = mean(log(max_apcp$RCP85)), sdlog = sd(log(max_apcp$RCP85)))

# 降水量の上位
curr_long %>% top_n(20, APCP)
rcp26_long %>% top_n(20, APCP)
rcp85_long %>% top_n(20, APCP)
