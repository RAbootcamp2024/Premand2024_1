# 20240827_Analysis_Datasets.do をRに
 if (library("dplyr", logical.return =TRUE) == FALSE) install.packages("dplyr")
 if (library("haven", logical.return =TRUE) == FALSE) install.packages("haven")

village_all <- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/intermediate/SPConflict_Village.v7.6.dta")
summary(village_all$communeid)

# communeidで昇順ソート
village_all<- village_all %>%
  arrange(communeid)

# 重複行を確認
duplicates <- village_all %>%
  group_by(codelocalite,year) %>%
  filter(n() > 1)

# 重複行を表示
print(duplicates)

# codelocalite, yearで昇順ソート
village_all<- village_all %>%
  arrange(codelocalite, year)

# 新しい列 'hadconflict6' を作成
village_all <- village_all %>% 
  mutate(hadconflict6 = nbfeatures18_10k > 0 & !is.na(nbfeatures18_10k)) %>%
  mutate(hadconflict3_250 = nbfeatures_10k > 0 & !is.na(nbfeatures_10k) & excludesmallvillage_250==0) %>%
  mutate(hadconflict5_250 = nbfeatures18_5k > 0 & !is.na(nbfeatures18_5k) & excludesmallvillage_250==0) %>%
  mutate(hadconflict6_250 = nbfeatures18_10k > 0 & !is.na(nbfeatures18_10k) & excludesmallvillage_250==0) %>%
  mutate(hadconflict7_250 = nbfeatures18_15k > 0 & !is.na(nbfeatures18_15k) & excludesmallvillage_250==0) %>%
  mutate(hadconflict6_namesonly = nbfeatures18_10k>0 & is.na(nbfeatures18_10k) & excludesmallvillage_namesonly==0)

colnames(village_all)  

# install.packages("tidyverse")
library("tidyverse")

# 'hadconflict'で始まる列の合計を計算
village_all %>%
  dplyr::select(starts_with("hadconflict")) %>%
  summary()

# foreign conflict variables
village_all <- village_all %>% 
  mutate(Foreign4_18_250 = nbfeat_foreign_4_250>0 & nbfeatures18_250==1 &  !is.na(nbfeat_foreign18_4_10k)) %>%
  mutate(NonForeign4_18_250 = case_when(
    nbfeatures18_250 == 0 ~ 0,
    Foreign4_18_250 == 0 & nbfeatures18_250 == 1 ~ 1,
    Foreign4_18_250 == 1 & nbfeatures18_250 == 1 ~ 0
  ))
  
village_all_2021 <- village_all %>% 
  filter(year==2021)
duplicates_2021 <- village_all_2021 %>%
  group_by(codelocalite) %>%
  filter(n() > 1)

village_code_2021 <- village_all_2021$codelocalite


conflict_daily<- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/intermediate/ConflictDataNigerSP_daily_Aug23_2021.dta")
duplicates_conflict_daily <- conflict_daily %>%
  group_by(ufi,sqldate) %>%
  filter(n() > 1)
sum(conflict_daily$articles)

conflict_entries<- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/intermediate/ConflictDataNigerSP_entries_July8_2021.dta")




