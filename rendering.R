rmarkdown::render("toxic trio map.rmd",output_file = "./docs/index.html")

rmarkdown::render_site()

#####

require(dplyr)
require(tidyr)

pcPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pCon0-15.csv")

bounds<-readRDS("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/bounds_simple.rds")

pcEst<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pcEst.csv")
resConf_rose_use <-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pcEst_rose.csv")

outLab<-readxl::read_excel("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/coding sheets_web.xlsx","outcomes")

popSize<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/popSize.csv")

depRank<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/depRank.csv")

depDec<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/depDec.csv")

mpList<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/mpList_out.csv")


pc_use<-pcEst %>% bind_rows() %>%
  select(pc,outcome,fRate=popRate,fLower,fUpper,estRate=fRate) %>% 
  bind_rows(resConf_rose_use %>%
       left_join(outLab %>% select(outcome,popRate)) %>%
      select(pc,outcome,fRate=popRate,fLower,fUpper,estRate=fRate)) %>%
  mutate(est=fRate) %>%
  left_join(popSize %>% select(-pop),by=c("pc"="pc")) %>%
  select(-fRate)

pc_use<-pc_use %>%
  left_join(pcPop %>% select(PCON11NM,pop=pop015)) %>%
  mutate(popRate=round(est*pop,0),
   modRate_low=round(fLower*pop,0),
    modRate_high=round(fUpper*pop,0),
    modRate_mid=round(estRate*pop,0)) %>% select(-pop)



require(cdata)

cT<-data.frame(outcome=unique(pc_use$outcome),estRate=paste0(unique(pc_use$outcome),"_model"),
  est=paste0(unique(pc_use$outcome),"_pop"),popRate=paste0(unique(pc_use$outcome),"_popN"),
  modRate_low=paste0(unique(pc_use$outcome),"_modLow"),
  modRate_high=paste0(unique(pc_use$outcome),"_modHigh"),
  modRate_mid=paste0(unique(pc_use$outcome),"_modMid"),stringsAsFactors = F)

pc_use<-blocks_to_rowrecs(pc_use,keyColumns = "pc",controlTable = cT,columnsToCopy = c("PCON11CD","PCON11NM","LAnum"))



laBounds<-readRDS("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/laBounds_simple.rds")

laBounds<-spTransform(laBounds,CRS("+init=epsg:4326"))

laEst<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/laEstimates.csv") %>% filter(!grepl("synthetic",outcome))

laEst_rose<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/laEstimates_rose.csv") 



laEst<-laEst %>% #filter(!outcome %in% c("alcordrugsep","submisuse")) %>%
  group_by(outcome) %>%
  mutate(m=mean(mid)) %>%
  ungroup() %>%
  left_join(outLab %>% select(outcome,popRate,labOutcome) %>% mutate(popRate=popRate/100)) %>%
  mutate(popMult=mid/m) %>%
  mutate(confU=upper-mid,confL=mid-lower) %>%
  mutate(fRate=popRate*popMult) %>%
  mutate(fUpper=fRate+confU,fLower=ifelse(fRate-confL<0,0,fRate-confL))

laEst_rose<-laEst_rose %>% ungroup() %>%
  mutate(fRate=scales::rescale(mid,c(min(laEst$fRate[laEst$outcome=="toxictrionarrow"]),max(laEst$fRate[laEst$outcome=="toxictrionarrow"])))) %>%
  mutate(outcome="toxictrionarrow_synthetic")

la_use<-laEst %>%
  select(la=LA_152,cd=LA152_Code,fRate,outcome) %>% 
  bind_rows(laEst_rose %>% select(la=LA_152,cd=LA152_Code,fRate,outcome)) %>%
  mutate(est=fRate*100) %>%
  select(-fRate) %>%
  spread(key=outcome,value=est)

laPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/Gangs/hotspots_summit/onsPop.csv") %>%
  filter(age<16) %>%
  group_by(area) %>%
  summarise(pop=sum(population_2017))

pc_use<-pcEst %>% bind_rows() %>%
  select(pc,outcome,fRate=popRate,fLower,fUpper,estRate=fRate) %>% 
  bind_rows(resConf_rose_use %>%
       left_join(outLab %>% select(outcome,popRate)) %>%
      select(pc,outcome,fRate=popRate,fLower,fUpper,estRate=fRate)) %>%
  mutate(est=fRate) %>%
  left_join(popSize %>% select(-pop),by=c("pc"="pc")) %>%
  select(-fRate)

pcPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pCon0-15.csv")



pc_use_d<-pc_use


pcDep<-pc_use_d %>%
  left_join(depRank %>% select(Constituency,depDec),by=c("PCON11NM"="Constituency")) %>%
  left_join(depDec %>% ungroup() %>% select(outcome,depDec,min,max) %>% distinct()) %>%
 # filter(!outcome %in% c("alcordrugdep","submisuse")) %>%
  mutate(split=paste0(outcome,"_",depDec)) %>%
  split(.$split) %>%
  purrr::map_df(function(x){
    
    l<-unique(x$min)
    t<-unique(x$max)
    
    x %>% mutate(estRate_dep=scales::rescale(estRate,c(l,t)))
  }) %>%
  select(pc,outcome,fRate=estRate_dep) %>% 
  mutate(est=fRate) %>%
  left_join(popSize %>% select(-pop),by=c("pc"="pc")) %>%
  select(-fRate) %>%
  spread(key=outcome,value=est) 

names(la_use)
names(pcDep)

pcCSV<-pcDep %>% select(-toxictrionarrow,-PCON11CD,-pc,-LAnum) %>%
  rename(Constituency=PCON11NM) %>%
  gather(key=outcome,val=rate,alcordrugdep:twoplusharmsnarrow) %>%
  mutate(rate=round(rate*100,2)) %>%
  left_join(outLab %>% select(-popRate)) %>% 
  mutate(labOutcome=ifelse(grepl("Adult has 2 of 3 issues",labOutcome),"Adult has 2 of 3 'toxic trio' issues",
    ifelse(labOutcome=="Adult has ever experienced DV&A","Adult has ever experienced domestic abuse",
      ifelse(grepl("Adult has any of the above risks",labOutcome),"Adult has any of the 'toxic trio' issues",
        ifelse(grepl("Adult has all 3 of the above risks",labOutcome),"Adult has all 3 of the 'toxic trio' issues",
          ifelse(grepl("Adult has 2 or more of the above risks",labOutcome),"Adult has 2 or more of the toxic trio issues",
          labOutcome)))))) %>%
  mutate(labOutcome=gsub("^Adult","adult",labOutcome)) %>%
  mutate(labOutcome=paste0("Projected % of children in a household where an ",labOutcome)) %>%
  mutate(labOutcome=ifelse(grepl("broad",outcome),paste0(labOutcome," (broad measures)"),
    ifelse(grepl("narrow",outcome),paste0(labOutcome," (narrow measures)"),labOutcome))) %>%
  select(-outcome) %>% spread(key=labOutcome,value=rate)

order<-c("Projected % of children in a household where an adult experienced domestic abuse in last year",
  "Projected % of children in a household where an adult has ever experienced domestic abuse",
  "Projected % of children in a household where an adult has moderate or higher mental ill-health symptoms",
  "Projected % of children in a household where an adult has severe mental ill-health symptoms",
  "Projected % of children in a household where an adult reports any substance misuse",
  "Projected % of children in a household where an adult has an alcohol or drug dependency",
  "Projected % of children in a household where an adult has any of the 'toxic trio' issues (broad measures)",
  "Projected % of children in a household where an adult has any of the 'toxic trio' issues (narrow measures)",
  "Projected % of children in a household where an adult has 2 of 3 'toxic trio' issues (broad measures)",
  "Projected % of children in a household where an adult has 2 of 3 'toxic trio' issues (narrow measures)",
  "Projected % of children in a household where an adult has 2 or more of the toxic trio issues (broad measures)",
  "Projected % of children in a household where an adult has 2 or more of the toxic trio issues (narrow measures)",
  "Projected % of children in a household where an adult has all 3 of the 'toxic trio' issues (broad measures)",
  "Projected % of children in a household where an adult has all 3 of the 'toxic trio' issues (narrow measures)")

pcCSV[,c("Constituency",order)] %>%
  write.csv(.,"toxicTrio_pc_data.csv",row.names=F)

laCSV<-la_use %>% select(-toxictrionarrow,-cd) %>%
  rename(LA=la) %>%
  gather(key=outcome,val=rate,alcordrugdep:twoplusharmsnarrow) %>%
  mutate(rate=round(rate,2)) %>%
  left_join(outLab %>% select(-popRate)) %>% 
  mutate(labOutcome=ifelse(grepl("Adult has 2 of 3 issues",labOutcome),"Adult has 2 of 3 'toxic trio' issues",
    ifelse(labOutcome=="Adult has ever experienced DV&A","Adult has ever experienced domestic abuse",
      ifelse(grepl("Adult has any of the above risks",labOutcome),"Adult has any of the 'toxic trio' issues",
        ifelse(grepl("Adult has all 3 of the above risks",labOutcome),"Adult has all 3 of the 'toxic trio' issues",
          ifelse(grepl("Adult has 2 or more of the above risks",labOutcome),"Adult has 2 or more of the toxic trio issues",
          labOutcome)))))) %>%
  mutate(labOutcome=gsub("^Adult","adult",labOutcome)) %>%
  mutate(labOutcome=paste0("Projected % of children in a household where an ",labOutcome)) %>%
  mutate(labOutcome=ifelse(grepl("broad",outcome),paste0(labOutcome," (broad measures)"),
    ifelse(grepl("narrow",outcome),paste0(labOutcome," (narrow measures)"),labOutcome))) %>%
  select(-outcome) %>% spread(key=labOutcome,value=rate) 

laCSV[,c("LA",order)] %>%
  write.csv(.,"toxicTrio_la_data.csv",row.names=F)
