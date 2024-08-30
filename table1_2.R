# 必要なパッケージを読み込み
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(
  dplyr,
  haven,
  here,
  fixest,
  xtable
)

# データの読み込み
village_250 <- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/final/village_250.dta")

# 回帰分析の結果を保存するリスト
results <- list()

# 変数リスト
varlist <- c("nbfeatures18_250", "hadconflict6_250")

prefix<-0
for (var in varlist) {
  
  # (2) モデル2
  model2 <- feols(as.formula(paste(var, "~ villagehadCT2y |year + communeid")), data = village_250, cluster = ~communeid)
  results[[paste0("(",prefix,")", " ", "TWFE")]] <- model2
  
  # (1) モデル1
  model1 <- feols(as.formula(paste(var, "~ villagehadCT2y | year*communeid")), data = village_250, cluster = ~communeid)
  results[[paste0("(",prefix,")", " ", "RCT")]] <- model1
  
  prefix<-prefix+1
}

# 結果のテーブルを作成
tex_code <-etable(
  results,
  tex = TRUE,
  title = "EFFECT OF CASH TRANSFERS ON SEVERE CONFLICT EVENTS",
  label = "tab:regression_results",
  dict = c(
    villagehadCT2y = "Treated (Last 2 years)",
    nbfeatures18_250 = "nearest neighbor",
    hadconflict6_250 = "10km radius",
    year = "year FE",
    communeid = "commune FE",
    `year:communeid` = "commune x year FE"
  ),
  fitstat = ~ n + r2,
  digits = 5
)

cat(tex_code)

# 不要な行を削除
tex_code <- gsub("\\\\multicolumn\\{5\\}\\{l\\}\\{\\\\emph\\{Clustered \\(commune FE\\) standard-errors in parentheses\\}\\}\\\\\\\\", "", tex_code)

# 文字列を追加
tex_code <- gsub(
  "Dependent Variables: & \\\\multicolumn\\{2\\}\\{c\\}\\{nearest neighbor\\} & \\\\multicolumn\\{2\\}\\{c\\}\\{10km radius\\}\\\\",
  "Dependent Variables: & \\\\multicolumn\\{2\\}\\{c\\}\\{nearest neighbor\\} & \\\\multicolumn\\{2\\}\\{c\\}\\{10km radius\\}\\\\\\\\\n & TWFE & RCT & TWFE & RCT\\\\",
  tex_code
)




# commune x year FEをcommune × year FEに変更し、空白をNOに置換
tex_code <- gsub("commune x year FE", "commune $\\\\times$ year FE", tex_code)
tex_code <- gsub("&                &", "& No            &", tex_code)
tex_code_final <- gsub("&                 &", "& No            &", tex_code)


# tex_code変数に格納されたtexコードを表示
cat(tex_code_final)

writeLines(tex_code_final, "table1.tex")


