#TABLE 1: Effect of cash transfers on severe conflict events


if (library("pacman", logical.return =TRUE) == FALSE) install.packages("pacman")
pacman::p_load(dplyr, haven, here ,fixest)

village_250 <- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/final/village_250.dta")


# Table1の(1)
# 固定効果回帰を実行
R_nbfeatures18_250_fe <- feols(nbfeatures18_250 ~  villagehadCT2y | communeid + year, data = village_250)
summary(R_nbfeatures18_250_fe)
# 純粋な決定係数を表示
R_squared_fe1 <- fitstat(R_nbfeatures18_250_fe, "r2")
print(R_squared_fe1)




#Table1の(2)
# 交互固定効果回帰を実行
R_nbfeatures18_250_rct <- feols(nbfeatures18_250 ~  villagehadCT2y | communeid * year, data = village_250)
summary(R_nbfeatures18_250_rct)
# 純粋な決定係数を表示
R_squared_rct1 <- fitstat(R_nbfeatures18_250_rct, "r2")
print(R_squared_rct1)




#Table1の(3)
# 固定効果回帰を実行
hadconflict6_250_fe <- feols(hadconflict6_250 ~  villagehadCT2y | communeid + year, data = village_250)
summary(hadconflict6_250_fe)
# 純粋な決定係数を表示
R_squared_fe2 <- fitstat(hadconflict6_250_fe, "r2")
print(R_squared_fe2)




# Table1の(4)
# 交互固定効果回帰を実行
hadconflict6_250_rct <- feols(hadconflict6_250 ~  villagehadCT2y | communeid * year, data = village_250)
summary(hadconflict6_250_rct)
# 純粋な決定係数を表示
R_squared_rct2 <- fitstat(hadconflict6_250_rct, "r2")
print(R_squared_rct2)





# 表を出力
# print(R_squared_rct2)
# stargazer(R_nbfeatures18_250_fe, R_nbfeatures18_250_rct,hadconflict6_250_fe,hadconflict6_250_rct
#           type = "latex",
#           stargazer(R_nbfeatures18_250_fe, R_nbfeatures18_250_rct,hadconflict6_250_fe,hadconflict6_250_rct,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(R_nbfeatures18_250_fe), nobs(R_nbfeatures18_250_rct),nobs(hadconflict6_250_fe),nobs(hadconflict6_250_rct)),
#                                      c("$R^2$", round(summary(R_nbfeatures18_250_fe)$r.squared, 3), round(summary(R_nbfeatures18_250_rct)$r.squared, 3),
#                                        round(summary(hadconflict6_250_fe)$r.squared, 3),round(summary(hadconflict6_250_rct)$r.squared, 3))),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           pacman::p_load(dplyr, haven, here ,fixest,stargazer)
#           stargazer(R_nbfeatures18_250_fe, R_nbfeatures18_250_rct,hadconflict6_250_fe,hadconflict6_250_rct,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(R_nbfeatures18_250_fe), nobs(R_nbfeatures18_250_rct),nobs(hadconflict6_250_fe),nobs(hadconflict6_250_rct)),
#                                      c("$R^2$", round(summary(R_nbfeatures18_250_fe)$r.squared, 3), round(summary(R_nbfeatures18_250_rct)$r.squared, 3),
#                                        round(summary(hadconflict6_250_fe)$r.squared, 3),round(summary(hadconflict6_250_rct)$r.squared, 3))),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           # LaTeX形式で出力
#           stargazer(R_nbfeatures18_250_fe, R_nbfeatures18_250_rct, hadconflict6_250_fe, hadconflict6_250_rct,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(R_nbfeatures18_250_fe), nobs(R_nbfeatures18_250_rct), nobs(hadconflict6_250_fe), nobs(hadconflict6_250_rct)),
#                                      c("$R^2$", round(r2_fe, 3), round(r2_rct, 3), round(r2_hadconflict_fe, 3), round(r2_hadconflict_rct, 3))),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           ? if
#           q
#           stargazer(R_nbfeatures18_250_fe, R_nbfeatures18_250_rct,hadconflict6_250_fe,hadconflict6_250_rct,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     dep.var.labels = c("\\makecell{TWFE \\\\ Nearest neighbor}",
#                                        "\\makecell{RCT \\\\ Nearest neighbor}",
#                                        "\\makecell{TWFE \\\\ 10km radius}",
#                                        "\\makecell{RCT \\\\ 10km radius}"),
#                     add.lines = list(c("Commune and year FE", "Yes", "Yes", "Yes", "Yes"),
#                                      c("Commune x year FE", "Yes", "Yes", "Yes", "Yes")),
#                     column.labels = c("Observations", "$R^2$"),
#                     digits = 3,
#                     header = FALSE,
#                     table.placement = "H",
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           pacman::p_load(dplyr, haven, here ,fixest,stargazer,kableExtra)
#           # 結果を抽出
#           results <- list(
#             summary(R_nbfeatures18_250_fe),
#             summary(R_nbfeatures18_250_rct),
#             summary(hadconflict6_250_fe),
#             summary(hadconflict6_250_rct)
#           )
#           village_250 <- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/final/village_250.dta")
#           # Table1の(1)
#           # 固定効果回帰を実行
#           R_nbfeatures18_250_fe <- feols(nbfeatures18_250 ~  villagehadCT2y | communeid + year, data = village_250)
#           # 結果を表示
#           summary(R_nbfeatures18_250_fe)
#           R_squared_fe1 <- fitstat(R_nbfeatures18_250_fe, "r2")
#           print(R_squared_fe1)
#           # Table1の(2)
#           # 交互固定効果回帰を実行
#           R_nbfeatures18_250_rct <- feols(nbfeatures18_250 ~  villagehadCT2y | communeid * year, data = village_250)
#           # 結果を表示
#           summary(R_nbfeatures18_250_rct)
#           R_squared_rct1 <- fitstat(R_nbfeatures18_250_rct, "r2")
#           # 結果を表示
#           print(R_squared_rct1)
#           # Table1の(3)
#           # 固定効果回帰を実行
#           hadconflict6_250_fe <- feols(hadconflict6_250 ~  villagehadCT2y | communeid + year, data = village_250)
#           # 結果を表示
#           summary(hadconflict6_250_fe)
#           R_squared_fe2 <- fitstat(hadconflict6_250_fe, "r2")
#           # 結果を表示
#           print(R_squared_fe2)
#           # Table1の(4)
#           # 交互固定効果回帰を実行
#           hadconflict6_250_rct <- feols(hadconflict6_250 ~  villagehadCT2y | communeid * year, data = village_250)
#           # 結果を表示
#           summary(hadconflict6_250_rct)
#           R_squared_rct2 <- fitstat(hadconflict6_250_rct, "r2")
#           # 結果を表示
#           print(R_squared_rct2)
#           # 結果を抽出
#           results <- list(
#             summary(R_nbfeatures18_250_fe),
#             summary(R_nbfeatures18_250_rct),
#             summary(hadconflict6_250_fe),
#             summary(hadconflict6_250_rct)
#           )
#           # 表の作成
#           table <- data.frame(
#             Model = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#             `Treated (Last 2 years)` = sapply(results, function(x) coef(x)["villagehadCT2y", "Estimate"]),
#             `Std. Error` = sapply(results, function(x) coef(x)["villagehadCT2y", "Std. Error"]),
#             `t value` = sapply(results, function(x) coef(x)["villagehadCT2y", "t value"]),
#             `Pr(>|t|)` = sapply(results, function(x) coef(x)["villagehadCT2y", "Pr(>|t|)"]),
#             Observations = sapply(results, function(x) x$nobs),
#             `$R^2$` = sapply(results, function(x) x$r.squared)
#           )
#           # 表の作成
#           modelsummary(models,
#                        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
#                        coef_map = c("villagehadCT2y" = "Treated (Last 2 years)"),
#                        gof_map = c("nobs" = "Observations", "r.squared" = "$R^2$"),
#                        output = "latex",
#                        title = "Regression Results",
#                        notes = c("Commune and year FE = *.year *.communeid", "Commune x year FE = _IyeaXcom_*"),
#                        escape = FALSE,
#                        file = "path/to/your/table.tex")
#           # モデルリストの作成
#           models <- list(
#             "TWFE \\ Nearest neighbor" = R_nbfeatures18_250_fe,
#             "RCT \\ Nearest neighbor" = R_nbfeatures18_250_rct,
#             "TWFE \\ 10km radius" = hadconflict6_250_fe,
#             "RCT \\ 10km radius" = hadconflict6_250_rct
#           )
#           pacman::p_load(dplyr, haven, here ,fixest,stargazer,kableExtra,modelsummary)
#           # モデルリストの作成
#           models <- list(
#             "TWFE \\ Nearest neighbor" = R_nbfeatures18_250_fe,
#             "RCT \\ Nearest neighbor" = R_nbfeatures18_250_rct,
#             "TWFE \\ 10km radius" = hadconflict6_250_fe,
#             "RCT \\ 10km radius" = hadconflict6_250_rct
#           )
#           # 表の作成
#           modelsummary(models,
#                        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
#                        coef_map = c("villagehadCT2y" = "Treated (Last 2 years)"),
#                        gof_map = c("nobs" = "Observations", "r.squared" = "$R^2$"),
#                        output = "latex",
#                        title = "Regression Results",
#                        notes = c("Commune and year FE = *.year *.communeid", "Commune x year FE = _IyeaXcom_*"),
#                        escape = FALSE,
#                        file = "path/to/your/table.tex")
#           # 表の作成
#           modelsummary(models,
#                        stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
#                        coef_map = c("villagehadCT2y" = "Treated (Last 2 years)"),
#                        gof_map = c("nobs" = "Observations", "r.squared" = "$R^2$"),
#                        output = "markdown",
#                        title = "Regression Results",
#                        notes = c("Commune and year FE = *.year *.communeid", "Commune x year FE = _IyeaXcom_*"),
#                        escape = FALSE)
#           A<-R_nbfeatures18_250_fe
#           B<-R_nbfeatures18_250_rct
#           C<-hadconflict6_250_fe
#           D<-hadconflict6_250_rct
#           stargazer(A,B,C,D,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(A), nobs(B),nobs(C),nobs(D)),
#                                      c("$R^2$", round(summary(A)$r.squared, 3), round(summary(B)$r.squared, 3),
#                                        round(summary(C)$r.squared, 3),round(summary(D)$r.squared, 3))),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           print(A)
#           print(A$r.squared)
#           stargazer(A,B,C,D,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(A), nobs(B),nobs(C),nobs(D)),
#                                      c("$R^2$", round(R_squared_fe1, 3), round(R_squared_rct1, 3),
#                                        round(R_squared_fe2, 3),round(R_squared_rct2, 3))),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           print(R_squared_fe1)
#           stargazer(A,B,C,D,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(A), nobs(B),nobs(C),nobs(D)),
#                                      c("$R^2$",R_squared_fe1,R_squared_rct1,
#                                        R_squared_fe2,R_squared_rct2)),
#                     dep.var.labels.include = FALSE,
#                     table.layout = "l*{2}{c}",
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           stargazer(A,B,C,D,
#                     type = "latex",
#                     keep = "villagehadCT2y",
#                     star.cutoffs = c(0.1, 0.05, 0.01),
#                     covariate.labels = c("Treated (Last 2 years)"),
#                     column.labels = c("TWFE \\ Nearest neighbor", "RCT \\ Nearest neighbor", "TWFE \\ 10km radius", "RCT \\ 10km radius"),
#                     add.lines = list(c("Observations", nobs(A), nobs(B),nobs(C),nobs(D)),
#                                      c("$R^2$",R_squared_fe1,R_squared_rct1,
#                                        R_squared_fe2,R_squared_rct2)),
#                     dep.var.labels.include = FALSE,
#                     header = FALSE,
#                     float = FALSE,
#                     no.space = TRUE,
#                     title = "Regression Results",
#                     label = "tab:regression_results",
#                     out = "path/to/your/table.tex")
#           