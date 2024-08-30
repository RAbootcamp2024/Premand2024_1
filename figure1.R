#Figure 1: Effect of cash transfers on conflict events during and after the end of the program

if (library("pacman", logical.return =TRUE) == FALSE) install.packages("pacman")
pacman::p_load(dplyr, haven, here ,fixest,tidyverse, ggplot2)

village_250 <- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/final/village_250.dta")

#回帰を行う
Nearest_neighbor <- feols(nbfeatures18_250 ~  CT_1y + CT_2y + CT_3y + CT_4y | communeid * year, data = village_250)
summary(Nearest_neighbor)

ten_km_radius<- feols(hadconflict6_250 ~  CT_1y + CT_2y + CT_3y + CT_4y | communeid * year, data = village_250)
summary(ten_km_radius)

#残差の自由度を求める
df_residual <- df.residual(Nearest_neighbor)
# 結果を表示
print(df_residual)

# 初期化
Event_l <- NULL
Event_b <- NULL
Event_se <- NULL

# 行列作成
Event_l <- c(1,2,3,4)
Event_b <- c(coef(Nearest_neighbor))
Event_se <- c(sqrt(diag(vcov(Nearest_neighbor))))

Event <- rbind(Event_l, Event_b, Event_se)
Event <- t(Event)

# ９５％信頼区間
ci_upper <- c("","","","")
ci_upper <- Event_b + qt(0.975, df_residual) * Event_se
ci_lower <- Event_b - qt(0.975, df_residual) * Event_se

# とりあえず転置したがコードの順番は要修正
Event_r <- rbind(Event,ci_upper,ci_lower)
Event_r <- t(Event_r)
ypos <- 0.014
title <- "Nearest neighbor"
# プロットの作成
ggplot(Event_r, aes(x = Event_l, y = Event_b)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "black") +
  annotate("text", x = 0.61, y = ypos, label = "Beneficiaries start\nreceiving CT", size = 3, hjust = -0.2) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
  annotate("text", x = 2.51, y = ypos, label = "Beneficiaries stop\nreceiving CT", size = 3, hjust = -0.2) +
  ggtitle(title) +
  ylab("Severe conflict") +
  xlab("Years from initial Cash Transfer receipt") +
  scale_x_continuous(limits = c(0.5, 4.5), breaks = 1:4, labels = c("1st year", "2nd year", "3rd year", "4th year")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(hjust = 0.5))

#残差の自由度を求める
df_residual2 <- df.residual(ten_km_radius)
# 結果を表示
print(df_residual2)

# 初期化
Event_l_2 <- NULL
Event_b_2 <- NULL
Event_se_2 <- NULL

# 行列作成
Event_l_2 <- c(1,2,3,4)
Event_b_2 <- c(coef(ten_km_radius))
Event_se_2 <- c(sqrt(diag(vcov(ten_km_radius))))

Event_2 <- rbind(Event_l_2, Event_b_2, Event_se_2)

# ９５％信頼区間
ci_upper_2 <- c("","","","")
ci_upper_2 <- Event_b_2 + qt(0.975, df_residual2) * Event_se_2
ci_lower_2 <- Event_b_2 - qt(0.975, df_residual2) * Event_se_2

# とりあえず転置したがコードの順番は要修正
Event_r_2 <- rbind(Event_2,ci_upper_2,ci_lower_2)
Event_r_2 <- t(Event_r_2)

ypos_2 <- 0.035
title_2 <- "10 km radius"
# プロットの作成
ggplot(Event_r_2, aes(x = Event_l_2, y = Event_b_2)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower_2, ymax = ci_upper_2), width = 0.2) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "black") +
  annotate("text", x = 0.61, y = ypos_2, label = "Beneficiaries start\nreceiving CT", size = 3, hjust = -0.2) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
  annotate("text", x = 2.51, y = ypos_2, label = "Beneficiaries stop\nreceiving CT", size = 3, hjust = -0.2) +
  ggtitle(title_2) +
  ylab("Severe conflict") +
  xlab("Years from initial Cash Transfer receipt") +
  scale_x_continuous(limits = c(0.5, 4.5), breaks = 1:4, labels = c("1st year", "2nd year", "3rd year", "4th year")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = margin(2, 2, 2, 2, "cm"),
        plot.title = element_text(hjust = 0.5))

