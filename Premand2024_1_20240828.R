# 20240827_Analysis_Datasets.do をRに
##pacmanのp_loadに全く同じ関数がある
 if (library("dplyr", logical.return =TRUE) == FALSE) install.packages("dplyr")
 if (library("haven", logical.return =TRUE) == FALSE) install.packages("haven")
 if (library("pacman", logical.return =TRUE) == FALSE) install.packages("pacman")
 pacman::p_load(dplyr, haven, here)

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
#複製作業を飛ばす、今後必要な場合戻って行う

conflict_entries<- read_dta("C:/Users/Owner/Desktop/Premand2024_1/AEA_repr_package/datasets/intermediate/ConflictDataNigerSP_entries_July8_2021.dta")
#複製作業を飛ばす、今後必要な場合戻って行う
conflict_entries <- conflict_entries %>% 
  mutate(ufi = actiongeo_featureid) %>% 
  mutate(daily = "") 
conflict_merge <- conflict_entries %>% 
  inner_join(conflict_daily,by=c("ufi","sqldate","monthyear","year","fractiondate"))

colnames(conflict_merge)

conflictmerge2 <- conflict_merge %>% dplyr::select(,c(1:69,128:149))
colnames(conflictmerge2)

conflict_merge_village <- conflictmerge2 %>% 
  inner_join(village_all_2021,by=c("codelocalite"))

conflict_merge_village <- conflict_merge_village %>%
  arrange(codelocalite, year.x)

#conflict_merge_village %>% 
 # dplyr::select(c(year.x, year.y)) 

#stata doの99行目のデータ
conflict_merge_village_id <- conflict_merge_village %>% drop_na(globaleventid)
#conflict_merge_village %>% is.na(globaleventid)

# 'value'列に欠損値があるかどうか確認
#has_na <- any(is.na(conflict_merge_village$globaleventid))

# 結果を表示
#print(has_na)





#FOREIGN VARIABLE
conflict_merge_village_id <- conflict_merge_village_id %>% mutate(foreign_terror = 0)

#stata Analysis_Datasets.do ファイルの105行目~106行目までをRコードに置換する

conflict_merge_terror <- conflict_merge_village_id %>% 
mutate(foreign_terror = ifelse(grepl("boko|  haram |  terror |  qaeda |  jihad |  qaida |  islamic-state  |  isgs |  gsim |  isis |  mujao |  extremist |  suicide-bomb |  militant |  kidnap |  islamist |  abduct |  sharia |  suicide-attack |  daesh |  raid |  training-camp |  hijack |  radical |  execut |  insurg |  jnim |  jamaat |  iswa |  mourabitoun |  ansar-dine |  explos |  massacr", sourceurl, ignore.case = TRUE),1,foreign_terror))

#stata Analysis_Datasets.do ファイルの109行目~224行目までをRコードに置換する

# 条件を含むテキストファイルの読み込み
conditions <- readLines("C:/Users/Owner/Desktop/Premand2024_1/url.txt")
# 条件をRコードに変換
r_code <- " conflict_merge_village_id<- conflict_merge_village_id %>%\n  
mutate(foreign_terror = ifelse(sourceurl == \"\", NA, foreign_terror)) 
%>%\n  mutate(foreign_terror = ifelse(grepl(\"http\", sourceurl) == FALSE, NA, foreign_terror))"

for (condition in conditions) {
  if (grepl("strpos", condition)) {
    url <- sub(".*strpos\\(sourceurl, \"(.*)\"\\).*", "\\1", condition)
    r_code <- paste0(r_code, " %>%\n  mutate(foreign_terror = ifelse(grepl(\"", url, "\", sourceurl), NA, foreign_terror))")
  }
}

# 結果を表示
cat(r_code)

conflict_merge_terror <- conflict_merge_terror　%>% 
  mutate(foreign_terror = ifelse(sourceurl == "", NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http", sourceurl) == FALSE, NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://af.reuters.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://africacenter.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://allafrica.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://axisoflogic.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://china.org.cn", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://en.alalam.ir", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://en.ce.cn", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://en.farsnews.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://en.shafaqna.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://english.sina.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://eurasia.ro", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://freerepublic.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://gbcghana.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://gulftoday.ae", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://hamptonroads.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://mobile.reuters.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://moderntokyotimes.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://moonreports.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://murnosti.blogspot.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://news.sudanvisiondaily.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://news.trust.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://news.webindia123.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://news.xinhuanet.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://one.trust.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://osundefender.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://schema-root.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://smartraveller.gov.au", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://somaliamediamonitoring.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://sputniknews.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://timesofoman.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.africaleader.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.africanelections.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.afronline.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.almanar.com.lb", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.arabnews.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.azerbaijannews.net", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.bbc.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.bernama.com.my", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.bna.bh", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.bssnews.net", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.catholicculture.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.centralctcommunications.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.cfr.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.china.org.cn", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.chinadaily.com.cn", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.copts-united.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.crisisgroup.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.ekklesia.co.uk", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.ennaharonline.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.gbcghana.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.gistmania.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.globalsecurity.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.globaltimes.cn", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.google.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.heraldtribune.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.informationclearinghouse.info", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.infozine.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.irishsun.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.isn.ethz.ch", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.jamestown.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.kforcegov.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.kfqd.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.ktbb.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.kuna.net.kw", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.laht.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.legalbrief.co.za", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.londonmercury.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.macon.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.middle-east-online.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.nerc.ac.uk", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.netindia123.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.netnebraska.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.newkerala.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.newsonair.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.newstimeafrica.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.nigeriasun.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.omantribune.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.philippinetimes.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.refworld.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.sanantoniopost.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.saudigazette.com.sa", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.scnow.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.shanghaidaily.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.spa.gov.sa", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.starrfmonline.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.strategypage.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.taipeitimes.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.tolerance.ca", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.trust.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.turkishpress.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.vision.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.wgme.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.wwmt.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("http://www.xinhuanet.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://af.reuters.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://allafrica.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://isp.netscape.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://news.trust.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://news.un.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://sierraexpressmedia.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://strategypage.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://today.rtl.lu", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.bbc.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.charlotteobserver.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.globalsecurity.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.haaretz.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.indcatholicnews.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.jagonews24.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.justsecurity.org", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.kuna.net.kw", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.msf.org.uk", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.osac.gov", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.strategypage.com", sourceurl), NA, foreign_terror)) %>%
  mutate(foreign_terror = ifelse(grepl("https://www.themalaysianinsight.com", sourceurl), NA, foreign_terror))

#227groupby→summarizeする
conflict_merge_terror_it <- conflict_merge_terror %>% 
  dplyr::group_by(codelocalite,year.x) %>% 
  dplyr::summarise(foreign_terror_Max = max(foreign_terror))

conflict_merge_terror_rename <- conflict_merge_terror_it %>% 
  dplyr::rename(year=year.x)


#234 restore village_allに戻る
village_all_terror <- village_all %>% 
  dplyr::left_join(conflict_merge_terror_rename,by=c("codelocalite","year"))
  
village_all_terror <- village_all_terror %>% 
  mutate(terror18 = case_when(
    nbfeatures18_250 == 1 & foreign_terror_Max==1 ~ 1,
    nbfeatures18_250== 0 ~ 0,
    foreign_terror_Max=="" ~ 0,
    nbfeatures18_250==1 & foreign_terror_Max==0 ~ 0))


