#####

require(dplyr)
require(tidyr)

pcPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pCon0-17.csv")

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
  left_join(pcPop %>% select(PCON11NM,pop=pop017)) %>%
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

pcPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/vulnerability db/Toxic trio LA level/Parliamentary constituency/pCon0-17.csv")



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
  left_join(outLab %>% select(-popRate)) %>% 
  mutate(labOutcome=ifelse(grepl("Adult has 2 of 3 issues",labOutcome),"Adult has 2 of 3 'toxic trio' issues",
    ifelse(labOutcome=="Adult has ever experienced DV&A","Adult has ever experienced domestic abuse",
      ifelse(grepl("Adult has any of the above risks",labOutcome),"Adult has any of the 'toxic trio' issues",
        ifelse(grepl("Adult has all 3 of the above risks",labOutcome),"Adult has all 3 of the 'toxic trio' issues",
          ifelse(grepl("Adult has 2 or more of the above risks",labOutcome),"Adult has 2 or more of the toxic trio issues",
          labOutcome)))))) %>%
  mutate(labOutcome=gsub("^Adult","adult",labOutcome)) %>%
  mutate(labOutcome=paste0("Projected % of 0-17 yr olds in a household where an ",labOutcome)) %>%
  mutate(labOutcome=ifelse(grepl("broad",outcome),paste0(labOutcome," (broad measures)"),
    ifelse(grepl("narrow",outcome),paste0(labOutcome," (narrow measures)"),labOutcome))) %>%
  select(-outcome) %>%
  left_join(pcPop %>% select(PCON11NM,pop017),by=c("Constituency"="PCON11NM"))

order<-c("Projected % of 0-17 yr olds in a household where an adult experienced domestic abuse in last year",
  "Projected % of 0-17 yr olds in a household where an adult has ever experienced domestic abuse",
  "Projected % of 0-17 yr olds in a household where an adult has moderate or higher mental ill-health symptoms",
  "Projected % of 0-17 yr olds in a household where an adult has severe mental ill-health symptoms",
  "Projected % of 0-17 yr olds in a household where an adult reports any substance misuse",
  "Projected % of 0-17 yr olds in a household where an adult has an alcohol or drug dependency",
  "Projected % of 0-17 yr olds in a household where an adult has any of the 'toxic trio' issues (broad measures)",
  "Projected % of 0-17 yr olds in a household where an adult has any of the 'toxic trio' issues (narrow measures)",
  "Projected % of 0-17 yr olds in a household where an adult has 2 of 3 'toxic trio' issues (broad measures)",
  "Projected % of 0-17 yr olds in a household where an adult has 2 of 3 'toxic trio' issues (narrow measures)",
  "Projected % of 0-17 yr olds in a household where an adult has 2 or more of the toxic trio issues (broad measures)",
  "Projected % of 0-17 yr olds in a household where an adult has 2 or more of the toxic trio issues (narrow measures)",
  "Projected % of 0-17 yr olds in a household where an adult has all 3 of the 'toxic trio' issues (broad measures)",
  "Projected % of 0-17 yr olds in a household where an adult has all 3 of the 'toxic trio' issues (narrow measures)")

pcCSV_rate<-pcCSV %>%
  select(-pop017) %>%
  mutate(rate=round(rate*100,2)) %>%
  spread(key=labOutcome,value=rate) %>%
  left_join(mpList %>% select(Constituency,MP=nm,Party=party))


pcCSV_rate[,c("Constituency","MP","Party",order)] %>%
  write.csv(.,"toxicTrio_pc_data_rate.csv",row.names=F)

pcCSV_count<-pcCSV %>%
  mutate(count=round(rate*pop017,-1)) %>%
 select(-pop017,-rate) %>%
  mutate(labOutcome=gsub("[%]","number",labOutcome)) %>%
   spread(key=labOutcome,value=count) %>%
  left_join(mpList %>% select(Constituency,MP=nm,Party=party))

orderNum<-gsub("[%]","number",order)

pcCSV_count[,c("Constituency","MP","Party",orderNum)] %>%
  write.csv(.,"toxicTrio_pc_data_count.csv",row.names=F)


regLookup<-readr::read_csv("./output_files/la_region_lookup.csv")

regLookup$new_la_code[regLookup$new_la_code=="E08000020"]<-"E08000037"
regLookup$new_la_code[regLookup$new_la_code=="E06000048"]<-"E06000057"

laPop<-readr::read_csv("C:/Users/TCLARKE1/OneDrive - Department for Education/Documents/Gangs/hotspots_summit/onsPop.csv") %>%
  filter(age<18) %>%
  group_by(area) %>%
  summarise(pop=sum(population_2017))

laCSV<-la_use %>% select(-toxictrionarrow) %>%
  left_join(laPop,by=c("cd"="area")) %>%
  left_join(regLookup,by=c("cd"="new_la_code")) %>% select(-cd) %>%
  rename(LA=la,Region=region_name) %>%
  gather(key=outcome,val=rate,alcordrugdep:twoplusharmsnarrow) %>%
  left_join(outLab %>% select(-popRate)) %>% 
  mutate(labOutcome=ifelse(grepl("Adult has 2 of 3 issues",labOutcome),"Adult has 2 of 3 'toxic trio' issues",
    ifelse(labOutcome=="Adult has ever experienced DV&A","Adult has ever experienced domestic abuse",
      ifelse(grepl("Adult has any of the above risks",labOutcome),"Adult has any of the 'toxic trio' issues",
        ifelse(grepl("Adult has all 3 of the above risks",labOutcome),"Adult has all 3 of the 'toxic trio' issues",
          ifelse(grepl("Adult has 2 or more of the above risks",labOutcome),"Adult has 2 or more of the toxic trio issues",
          labOutcome)))))) %>%
  mutate(labOutcome=gsub("^Adult","adult",labOutcome)) %>%
  mutate(labOutcome=paste0("Projected % of 0-17 yr olds in a household where an ",labOutcome)) %>%
  mutate(labOutcome=ifelse(grepl("broad",outcome),paste0(labOutcome," (broad measures)"),
    ifelse(grepl("narrow",outcome),paste0(labOutcome," (narrow measures)"),labOutcome))) %>%
  select(-outcome)

laCSV_rate<-laCSV %>%
  select(-pop) %>%
  spread(key=labOutcome,value=round(rate,2))

laCSV_rate[,c("LA","Region",order)] %>%
  write.csv(.,"toxicTrio_la_data_rate.csv",row.names=F)


laCSV_count<-laCSV %>%
  mutate(count=round(rate/100*pop,-1)) %>%  
 select(-pop,-rate) %>%
  mutate(labOutcome=gsub("[%]","number",labOutcome)) %>%
  spread(key=labOutcome,value=count)

laCSV_count[,c("LA","Region",orderNum)] %>%
  write.csv(.,"toxicTrio_la_data_count.csv",row.names=F)

for(i in names(la_use)[3:length(names(la_use))]){
nVar<-rlang::sym(paste0(i,"_fill"))
var<-rlang::sym(i) 

la_use <- la_use %>% ungroup() %>%
  mutate(!!nVar:=scales::rescale((!!var),c(0,1))) }

la_use<-la_use %>%
  left_join(laPop,by=c("cd"="area"))

c_la<-broom::tidy(sp::merge(laBounds,as.data.frame(la_use),by.x="ctyua16cd",by.y="cd",all.x=T,all.y=T),region="ctyua16cd")

require(ggplot2)
require(viridis)
require(ggthemes)
require(extrafont)

ggsave(c_la %>%
  left_join(la_use,by=c("id"="cd")) %>%
  mutate(twoplusharmsnarrow_fill=twoplusharmsnarrow_fill) %>%
    filter(grepl("^E",id)) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=-twoplusharmsnarrow_fill))+geom_polygon(colour="grey20",alpha=0.7)+
  scale_fill_viridis(name="Projected % of children\naffected in LA",direction=-1,labels=c("Highest rate",rep("",3),"Lowest rate"))+
  coord_fixed(1.5)+ggtitle(label = "Projected % of children in a household where an\nadult has 2 or more of the 'toxic trio' issues")+
    theme(
    panel.background = element_blank(),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.17,0.85),
    title = element_text(family="sans",size = 17,face="bold"),
      legend.title = element_text(family="sans",size = 14,face="bold"),
      legend.text = element_text(family="sans",size = 12)
      
  ),filename = "la_map.pdf",device="pdf",dpi="retina",height=8,width=7)

for(i in names(pcDep)[5:length(names(pcDep))]){
nVar<-rlang::sym(paste0(i,"_fill"))
var<-rlang::sym(i) 

pcDep <- pcDep %>% ungroup() %>%
  mutate(!!nVar:=scales::rescale((!!var),c(0,1))) }

bounds@data$pcon18nm<-gsub("St[.] ","St ",bounds@data$pcon18nm)

c_pc<-broom::tidy(sp::merge(bounds,as.data.frame(pcDep),by.x="pcon18nm",by.y="PCON11NM",all.x=T,all.y=T),region="pcon18nm")

require(ggplot2)
require(viridis)
require(ggthemes)
require(extrafont)

ggsave(c_pc %>%
  left_join(pcDep,by=c("id"="PCON11NM")) %>%
  mutate(twoplusharmsnarrow_fill=twoplusharmsnarrow_fill) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=-twoplusharmsnarrow_fill))+geom_polygon(colour="grey20",alpha=0.7)+
  scale_fill_viridis(name="Projected % of children\naffected in constituency",direction=-1,labels=c("Highest rate",rep("",3),"Lowest rate"))+
  coord_fixed(1.5)+ggtitle(label = "Projected % of children in a household where an\nadult has 2 or more of the 'toxic trio' issues")+
    theme(
    panel.background = element_blank(),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.17,0.85),
    title = element_text(family="sans",size = 17,face="bold"),
      legend.title = element_text(family="sans",size = 14,face="bold"),
      legend.text = element_text(family="sans",size = 12)
      
  ),filename = "pc_map.pdf",device="pdf",dpi="retina",height=8,width=7)


ggsave(c_pc %>%
  left_join(pcDep,by=c("id"="PCON11NM")) %>%
  mutate(twoplusharmsnarrow_fill=twoplusharmsnarrow_fill) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=-twoplusharmsnarrow_fill))+geom_polygon(colour="grey20")+
  scale_fill_gradient(low="#F7FBFF",high="#2171B5",name="Projected % of children\naffected in constituency",labels=c("Highest rate",rep("",3),"Lowest rate"))+
  coord_fixed(1.5)+ggtitle(label = "Projected % of children in a household where an\nadult has 2 or more of the 'toxic trio' issues")+
    theme(
    panel.background = element_blank(),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.17,0.85),
    title = element_text(family="sans",size = 17,face="bold"),
      legend.title = element_text(family="sans",size = 14,face="bold"),
      legend.text = element_text(family="sans",size = 12)
      
  ),filename = "pc_map_blues.pdf",device="pdf",dpi="retina",height=8,width=7)

ggsave(c_la %>%
  left_join(la_use,by=c("id"="cd")) %>%
  mutate(twoplusharmsnarrow_fill=twoplusharmsnarrow_fill) %>%
    filter(grepl("^E",id)) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=-twoplusharmsnarrow_fill))+geom_polygon(colour="grey20",alpha=0.7)+
  scale_fill_gradient(low="#F7FBFF",high="#2171B5",name="Projected % of children\naffected in LA",labels=c("Highest rate",rep("",3),"Lowest rate"))+
  coord_fixed(1.5)+ggtitle(label = "Projected % of children in a household where an\nadult has 2 or more of the 'toxic trio' issues")+
    theme(
    panel.background = element_blank(),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.17,0.85),
    title = element_text(family="sans",size = 17,face="bold"),
      legend.title = element_text(family="sans",size = 14,face="bold"),
      legend.text = element_text(family="sans",size = 12)
      
  ),filename = "la_map_blues.pdf",device="pdf",dpi="retina",height=8,width=7)
