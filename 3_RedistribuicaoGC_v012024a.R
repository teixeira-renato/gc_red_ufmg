rm(list=ls())
gc()

start_time <- Sys.time()

if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
pacman::p_load(tidyverse,readr, dtplyr,rio) # pacotes necessários

options(scipen=999)

path = "/Users/renatoteixeira/Library/CloudStorage/OneDrive-UniversidadeFederaldeMinasGerais/Small Areas Estimation/Redistribuição GC/SAS to R/Script V7 2023/Base"
setwd(path)
resultado = "SIM_2010e2019_2"

# Bases----

load("PASSO_2_2023-12-19SIM_2010e2019_2.Rdata")

# base.3 <- base.3%>%
#   filter(grepl("^1", cdmun))%>%
#   filter(ano==2016)

ICD <- import("ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx", sheet = 2, colClasses="character")

`%notin%` <- Negate(`%in%`)
#Codigo de municipio

mumime <- read_csv("2021-04-19_codmun_ibge_gbd.csv",
                   locale = locale(encoding = "WINDOWS-1252"))
mumime <- mumime %>%
  select(municode.6, microcode, mesocode)
colnames(mumime) <- c('cdmun','micro', 'meso')
mumime$cdmun <- as.character(mumime$cdmun)

cdmun <- unique(as.character(base.3$cdmun))

notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                     230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                     350000, 410000,420000,430000,500000,510000,520000,000000)

cdmun <- cdmun[!cdmun %in% notwantedlevels]

# Definição de causas targets e códigos garbage para redistribuir

causas <- c( "Injuries - Falls"    , "_pneumo",
             "Injuries - Homicide" , "Injuries - Others"  ,
             "Injuries - Road"     , "Injuries - Suicide" ,
             "other_causes_all","other_causes-lri",
             "other_desnutricao_all_ages","Injuries - Other transport injuries",
             "materna_ectopica"    , "materna_hipertensiva",
             "materna_trab_parto"  , "materna_aborto_induzido",
             "materna_tardia"      , "materna_aborto_espontaneo",
             "materna_sepsis"      , "materna_indiretas",
             "materna_outras"      , "materna_hemorragia",
             "trans_dengue"        , "materna_materna_hiv",
             "trans_encefatlite"   , "trans_schistosomiasis",
             "trans_chagas"        , "trans_tuberculose",
             "trans_hiv_aids"      , "trans_doenças_diarreicas",
             "trans_varicela"      , "trans_leishmaniose",
             "trans_zoonoticas"    , "trans_hepatite",
             "trans_meningites"    , "trans_sexualmente_transmissíveis",
             "trans_desnutricao"   , "trans_febre_amarela",
             "trans_infec_urinaria", "trans_malaria",
             "dcnt_neoplasms"      , "dcnt_chronic respiratory",
             "dcnt_diabetes"       , "dcnt_cardiovascular",
             "anom_congenitas"     , "aspiracao_pulmunar",
             "lri_post_neo" , "infant_neonatal_encefalopatia",
             "infant_subita"       , "infant_neonatal_hemolitica",
             "obst_intestinal"     , "infant_neonatal_prematuridade",
             "infant_neonatal_other","infant_neonatal_sepsis")

redis <- c("_injuries" , "_inj (hom,suic, fall,road)", "_all", "_inj (hom,suic,other)" ,
           "_pneumo","_inj (hom,sui)" , "_inj (hom,sui,transp)","_maternas", "_x59", "_y34","_infant_neonat")

setdiff(causas,unique(ICD$CLASS_GPEAS_EXT))
setdiff(causas,unique(ICD$CLASS_GPEAS_EXT_pre_opas))

# Montagem da Base SEM os GB ----

base.4 <- base.3 %>%
  filter(GBD %in% causas) %>%
  select(cdmun:meso, obitos.2, uf)


####Base cheia, incluindo os casos sem registro, sem os Garbage
mat <-c(causas[grepl("^materna",causas)])
infant <-c(causas[grepl("^infant",causas)])
sexo <- unique(base.3$sexo)
anos <- unique(base.3$ano)
age <- c(seq(0,90,5), "Early Neonatal", "Post Neonatal", "Late Neonatal")

base.1 <- expand.grid(cdmun,anos,age,sexo,causas)
colnames(base.1) <- c('cdmun','ano','idade','sexo', 'GBD')

base.1 <- base.1%>%
  mutate(to_exclude= case_when(
    GBD %in% mat & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5","60","65","70","75","80","85","90") ~ 1,
    GBD %in% mat & sexo == "Masculino"  ~ 1,
    GBD %in% mat & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ 0,
    # GBD %in% infant & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year") ~ 1,
    TRUE ~ 0
  ))%>%
  filter(to_exclude==0)%>%
  select(-to_exclude)

base.5 <- left_join(base.1,mumime, by='cdmun')

base.5 <- base.5 %>%
  mutate(uf=str_sub(cdmun,1,2),
         reg=str_sub(cdmun,1,1))

###Merge da base limpa com a base de óbitos, considerando as causas sem GB

base.5 <-   left_join(base.5, base.4, by=c('cdmun','micro','meso','idade','GBD', 'sexo', 'ano', 'uf'))


base.r <- base.3  %>%
  filter(GBD %in% redis) %>%
  select(cdmun:meso, obitos.2, uf)
colnames(base.r)[8] <- 'redis'
colnames(base.r)[5] <- 'c.red'

rm(base.1,base.3,base.4,mumime,age,anos,cdmun,sexo)

#Redistribuicao GC----

##### Injuries  -----

inj <-c(causas[grepl("^Injuries",causas)])

base.5 <- base.5 %>%
  mutate(c.red=ifelse(GBD %in% inj, '_injuries', NA)) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções INJ

###PRMUN
muni.inj <- base.5 %>%
  filter(GBD %in% inj) %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.2, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.inj <- base.5 %>%
  filter(GBD %in% inj) %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.2, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.inj <- base.5 %>%
  filter(GBD %in% inj) %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.2, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.inj <- base.5 %>%
  filter(GBD %in% inj) %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.2, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
reg.inj <- base.5 %>%
  filter(GBD %in% inj) %>%
  mutate(reg=str_sub(cdmun,1,1)) %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.2, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)


base.5 <- base.5 %>%
  left_join(muni.inj, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(micro.inj, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(meso.inj, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.inj, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(reg.inj, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))

base.5 <- base.5 %>%
  mutate(inj.1=redis*pr.mu,
         redis.2=ifelse(is.na(inj.1) & ob.mu==0, redis,NA),
         inj.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(inj.2) & ob.mi==0, redis.2,NA),
         inj.3=redis.3*pr.me,
         redis.4=ifelse(is.na(inj.3) & ob.me==0, redis.3,NA),
         inj.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(inj.4) & ob.uf==0, redis.4,NA),
         inj.5=redis.5*pr.rg,
         obitos.3= ifelse(!is.na(inj.1), obitos.2+inj.1,
                          ifelse(!is.na(inj.2), obitos.2+inj.2,
                                 ifelse(!is.na(inj.3), obitos.2+inj.3,
                                        ifelse(!is.na(inj.4), obitos.2+inj.4,
                                               ifelse(!is.na(inj.5), obitos.2+inj.5, obitos.2))))))

#Validação

obitos_para_redis <- sum(base.r[grepl("_injuries",base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.2,na.rm = T)
round(sum(base.5$obitos.3,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

rm(inj,muni.inj,micro.inj,meso.inj,uf.inj,reg.inj)

############_Injuries-hom-sui ----

inj.hom.sui <-c("Injuries - Homicide", "Injuries - Suicide" )

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% inj.hom.sui, '_inj (hom,sui)',NA))%>%
  mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções _Injuries-hom-sui

###PRMUN
mu.inj.h.s <- base.5 %>%
  filter(c.red == '_inj (hom,sui)') %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.3, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
mi.inj.h.s <- base.5 %>%
  filter(c.red == '_inj (hom,sui)') %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.3, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
me.inj.h.s <- base.5 %>%
  filter(c.red == '_inj (hom,sui)') %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.3, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.inj.h.s <- base.5 %>%
  filter(c.red == '_inj (hom,sui)') %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.3, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.inj.h.s <- base.5 %>%
  filter(c.red == '_inj (hom,sui)') %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.3, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(mu.inj.h.s, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(mi.inj.h.s, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(me.inj.h.s, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.inj.h.s, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.inj.h.s, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))

base.5 <- base.5 %>%
  mutate(ihs.1=redis*pr.mu,
         redis.2=ifelse(is.na(ihs.1) & ob.mu==0, redis,NA),
         ihs.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(ihs.2) & ob.mi==0, redis.2,NA),
         ihs.3=redis.3*pr.me,
         redis.4=ifelse(is.na(ihs.3) & ob.me==0, redis.3,NA),
         ihs.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(ihs.4) & ob.uf==0, redis.4,NA),
         ihs.5=redis.5*pr.rg,
         obitos.4= ifelse(!is.na(ihs.1), obitos.3+ihs.1,
                          ifelse(!is.na(ihs.2), obitos.3+ihs.2,
                                 ifelse(!is.na(ihs.3), obitos.3+ihs.3,
                                        ifelse(!is.na(ihs.4), obitos.3+ihs.4,
                                               ifelse(!is.na(ihs.5), obitos.3+ihs.5, obitos.3))))))

rm(inj.hom.sui,me.inj.h.s,mi.inj.h.s,mu.inj.h.s,rg.inj.h.s,uf.inj.h.s)

#Validação
obitos_para_redis <- sum(base.r[grepl('_inj (hom,sui)',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.3,na.rm = T)
round(sum(base.5$obitos.4,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

########_inj (hom,suic, fall,road): -----

inj.hsf <-c("Injuries - Falls", "Injuries - Homicide", "Injuries - Suicide", "Injuries - Road")

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% inj.hsf, '_inj (hom,suic, fall,road)',NA))%>%
  mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções o	_inj (hom,suic, fall):
# unique(mu.inj.hsf$GBD)
# unique(mu.inj.hsf$idade)
# mu.inj.hsf <- base.5 %>%
#   filter(GBD %in% inj.hsf)%>%
#   filter(GBD %notin% "Injuries - Suicide" & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"))

###PRMUN
mu.inj.hsf <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic, fall,road)')%>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.4, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
mi.inj.hsf <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic, fall,road)')%>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.4, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)



###PR.MESO
me.inj.hsf <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic, fall,road)')%>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.4, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)



###PR.UF
uf.inj.hsf <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic, fall,road)')%>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.4, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)


###PR.REG
rg.inj.hsf <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic, fall,road)')%>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.4, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)


base.5 <- base.5 %>%
  left_join(mu.inj.hsf, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(mi.inj.hsf, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(me.inj.hsf, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.inj.hsf, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.inj.hsf, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))



base.5 <- base.5 %>%
  mutate(ihsf.1=redis*pr.mu,
         redis.2=ifelse(is.na(ihsf.1) & ob.mu==0, redis,NA),
         ihsf.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(ihsf.2) & ob.mi==0, redis.2,NA),
         ihsf.3=redis.3*pr.me,
         redis.4=ifelse(is.na(ihsf.3) & ob.me==0, redis.3,NA),
         ihsf.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(ihsf.4) & ob.uf==0, redis.4,NA),
         ihsf.5=redis.5*pr.rg,
         obitos.5= ifelse(!is.na(ihsf.1), obitos.4+ihsf.1,
                          ifelse(!is.na(ihsf.2), obitos.4+ihsf.2,
                                 ifelse(!is.na(ihsf.3), obitos.4+ihsf.3,
                                        ifelse(!is.na(ihsf.4), obitos.4+ihsf.4,
                                               ifelse(!is.na(ihsf.5), obitos.4+ihsf.5, obitos.4))))))

rm(inj.hsf,me.inj.hsf,mi.inj.hsf,mu.inj.hsf,rg.inj.hsf,uf.inj.hsf)
#Validação
obitos_para_redis <- sum(base.r[grepl('(hom,suic, fall,road)',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.4,na.rm = T)
round(sum(base.5$obitos.5,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

########	_inj (hom,sui,transp):-----

inj.hst <-c("Injuries - Road", "Injuries - Suicide", "Injuries - Homicide", "Injuries - Other transport injuries")

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% inj.hst, '_inj (hom,sui,transp)', NA)) %>%
  mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))


###Proporções o	_inj (hom,sui,transp):

###PRMUN
mu.inj.hst <- base.5 %>%
  filter(c.red %in% '_inj (hom,sui,transp)')%>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.5, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
mi.inj.hst <- base.5 %>%
  filter(c.red %in% '_inj (hom,sui,transp)')%>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.5, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
me.inj.hst <- base.5 %>%
  filter(c.red %in% '_inj (hom,sui,transp)')%>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.5, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.inj.hst <- base.5 %>%
  filter(c.red %in% '_inj (hom,sui,transp)')%>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.5, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.inj.hst <- base.5 %>%
  filter(c.red %in% '_inj (hom,sui,transp)')%>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.5, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(mu.inj.hst, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(mi.inj.hst, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(me.inj.hst, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.inj.hst, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.inj.hst, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


base.5 <- base.5 %>%
  mutate(ihst.1=redis*pr.mu,
         redis.2=ifelse(is.na(ihst.1) & ob.mu==0, redis,NA),
         ihst.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(ihst.2) & ob.mi==0, redis.2,NA),
         ihst.3=redis.3*pr.me,
         redis.4=ifelse(is.na(ihst.3) & ob.me==0, redis.3,NA),
         ihst.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(ihst.4) & ob.uf==0, redis.4,NA),
         ihst.5=redis.5*pr.rg,
         obitos.6= ifelse(!is.na(ihst.1), obitos.5+ihst.1,
                          ifelse(!is.na(ihst.2), obitos.5+ihst.2,
                                 ifelse(!is.na(ihst.3), obitos.5+ihst.3,
                                        ifelse(!is.na(ihst.4), obitos.5+ihst.4,
                                               ifelse(!is.na(ihst.5), obitos.5+ihst.5, obitos.5))))))

rm(inj.hst,me.inj.hst,mi.inj.hst,mu.inj.hst,rg.inj.hst,uf.inj.hst)

#Validação
obitos_para_redis <- sum(base.r[grepl('(hom,sui,transp)',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.5,na.rm = T)
round(sum(base.5$obitos.6,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

########	_inj(hom,suic,other):-----

inj.hso <-c("Injuries - Others", "Injuries - Suicide", "Injuries - Homicide")

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4, -redis.5,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% inj.hso, '_inj (hom,suic,other)', NA)) %>%
  mutate(c.red=ifelse(GBD %in% "Injuries - Suicide" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0"),NA,c.red))%>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))


###Proporções _inj(hom,suic,other):

###PRMUN
mu.inj.hso <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic,other)') %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.6, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
mi.inj.hso <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic,other)') %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.6, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
me.inj.hso <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic,other)') %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.6, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.inj.hso <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic,other)') %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.6, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.inj.hso <- base.5 %>%
  filter(c.red %in% '_inj (hom,suic,other)') %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.6, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)


base.5 <- base.5 %>%
  left_join(mu.inj.hso, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(mi.inj.hso, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(me.inj.hso, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(uf.inj.hso, by=c( 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(rg.inj.hso, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


base.5 <- base.5 %>%
  mutate(ihso.1=redis*pr.mu,
         redis.2=ifelse(is.na(ihso.1) & ob.mu==0, redis,NA),
         ihso.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(ihso.2) & ob.mi==0, redis.2,NA),
         ihso.3=redis.3*pr.me,
         redis.4=ifelse(is.na(ihso.3) & ob.me==0, redis.3,NA),
         ihso.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(ihso.4) & ob.uf==0, redis.4,NA),
         ihso.5=redis.5*pr.rg,
         obitos.7= ifelse(!is.na(ihso.1), obitos.6+ihso.1,
                          ifelse(!is.na(ihso.2), obitos.6+ihso.2,
                                 ifelse(!is.na(ihso.3), obitos.6+ihso.3,
                                        ifelse(!is.na(ihso.4), obitos.6+ihso.4,
                                               ifelse(!is.na(ihso.5), obitos.6+ihso.5, obitos.6))))))

rm(inj.hso,me.inj.hso,mi.inj.hso,mu.inj.hso,rg.inj.hso,uf.inj.hso)
#Validação
obitos_para_redis <- sum(base.r[grepl('(hom,suic,other)',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.6,na.rm = T)
round(sum(base.5$obitos.7,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

##### Óbito Materno----

mat <-c(causas[grepl("^materna",causas)])

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% mat & sexo == "Feminino" & idade%in%c("10","15","20","25","30","35","40","45","50"), '_maternas', NA)) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções mat

###PRMUN
muni.mat <- base.5 %>%
  filter(GBD %in% mat & sexo == "Feminino") %>%
  filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.7, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.mat <- base.5 %>%
  filter(GBD %in% mat & sexo == "Feminino") %>%
  filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.7, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.mat <- base.5 %>%
  filter(GBD %in% mat & sexo == "Feminino") %>%
  filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.7, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.mat <- base.5 %>%
  filter(GBD %in% mat & sexo == "Feminino") %>%
  filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.7, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.mat <- base.5 %>%
  filter(GBD %in% mat & sexo == "Feminino") %>%
  filter(idade%in%c("10","15","20","25","30","35","40","45","50"))%>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.7, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(muni.mat, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(micro.mat, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(meso.mat, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(uf.mat, by=c( 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(rg.mat, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


base.5 <- base.5 %>%
  mutate(mat.1=(redis)*pr.mu,
         redis.2=ifelse(is.na(mat.1) & ob.mu==0, redis,NA),
         mat.2=(redis.2)*pr.mi,
         redis.3=ifelse(is.na(mat.2) & ob.mi==0, redis.2,NA),
         mat.3=(redis.3)*pr.me,
         redis.4=ifelse(is.na(mat.3) & ob.me==0, redis.3,NA),
         mat.4=(redis.4)*pr.uf,
         redis.5=ifelse(is.na(mat.4) & ob.uf==0, redis.4,NA),
         mat.5=(redis.5)*pr.rg,
         obitos.8= ifelse(!is.na(mat.1), obitos.7+mat.1,
                          ifelse(!is.na(mat.2), obitos.7+mat.2,
                                 ifelse(!is.na(mat.3), obitos.7+mat.3,
                                        ifelse(!is.na(mat.4), obitos.7+mat.4,
                                               ifelse(!is.na(mat.5), obitos.7+mat.5, obitos.7))))))

#validação

obitos_para_redis <- sum(base.r[grepl('_maternas',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.7,na.rm = T)
round(sum(base.5$obitos.8,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

##### Óbito Neonatal----

infant <-c(causas[grepl("^infant|anom_congenitas|aspiracao_pulmunar|obst_intestinal|lri_post_neo",causas)])

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year"), '_infant_neonat', NA)) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções infant

###PRMUN
muni.infant <- base.5 %>%
  filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.8, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.infant <- base.5 %>%
  filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.8, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.infant <- base.5 %>%
  filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.8, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.infant <- base.5 %>%
  filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.8, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.infant <- base.5 %>%
  filter(GBD %in% infant & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year")) %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.8, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(muni.infant, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(micro.infant, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(meso.infant, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(uf.infant, by=c( 'GBD','idade', 'ano', 'sexo', 'uf'))
base.5 <- base.5%>%
  left_join(rg.infant, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


base.5 <- base.5 %>%
  mutate(infant.1=(redis)*pr.mu,
         redis.2=ifelse(is.na(infant.1) & ob.mu==0, redis,NA),
         infant.2=(redis.2)*pr.mi,
         redis.3=ifelse(is.na(infant.2) & ob.mi==0, redis.2,NA),
         infant.3=(redis.3)*pr.me,
         redis.4=ifelse(is.na(infant.3) & ob.me==0, redis.3,NA),
         infant.4=(redis.4)*pr.uf,
         redis.5=ifelse(is.na(infant.4) & ob.uf==0, redis.4,NA),
         infant.5=(redis.5)*pr.rg,
         obitos.9= ifelse(!is.na(infant.1), obitos.8+infant.1,
                          ifelse(!is.na(infant.2), obitos.8+infant.2,
                                 ifelse(!is.na(infant.3), obitos.8+infant.3,
                                        ifelse(!is.na(infant.4), obitos.8+infant.4,
                                               ifelse(!is.na(infant.5), obitos.8+infant.5, obitos.8))))))

# validação
obitos_para_redis <- sum(base.r[grepl('_infant_neonat',base.r$c.red),]$redis, na.rm = T) #TODO: Conferir
obitos_pre_redis <- sum(base.5$obitos.8,na.rm = T)
round(sum(base.5$obitos.9,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

##### x59----

trans <-c(causas[grepl("^trans",causas)])
inj <- c(causas[grepl("^Injuries",causas)])
mat <-c(causas[grepl("^materna",causas)])

ICD_x59 <- ICD%>%
  filter(CG=="_x59")%>%
  select(target,weight)

x59 <- data.frame(target = c(inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

x59 <- x59%>%
  left_join(ICD_x59,by="target")

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red) %>%
  mutate(c.red=ifelse(GBD %in% x59$target, '_x59', NA)) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções x59

###PRMUN
muni.x59 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.9, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.x59 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.9, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.x59 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.9, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.x59 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.9, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.x59 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.9, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(muni.x59, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(micro.x59, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(meso.x59, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.x59, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.x59, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))

base.5 <- base.5 %>%
  left_join(x59,by=c("GBD"="target"))%>%
  mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
  mutate(x59.1=ifelse(GBD %in% inj,redis,redis*pr.mu),
         redis.2=ifelse(is.na(x59.1)| x59.1 == 0  & ob.mu==0, redis,NA),
         x59.2=ifelse(GBD %in% inj,redis.2,redis.2*pr.mi),
         redis.3=ifelse(is.na(x59.2) & ob.mi==0, redis.2,NA),
         x59.3=ifelse(GBD %in% inj,redis.3,redis.3*pr.me),
         redis.4=ifelse(is.na(x59.3) & ob.me==0, redis.3,NA),
         x59.4=ifelse(GBD %in% inj,redis.4,redis.4*pr.uf),
         redis.5=ifelse(is.na(x59.4) & ob.uf==0, redis.4,NA),
         x59.5=ifelse(GBD %in% inj,redis.5,redis.5*pr.rg),
         obitos.10= ifelse(!is.na(x59.1), obitos.9+x59.1,
                           ifelse(!is.na(x59.2), obitos.9+x59.2,
                                  ifelse(!is.na(x59.3), obitos.9+x59.3,
                                         ifelse(!is.na(x59.4), obitos.9+x59.4,
                                                ifelse(!is.na(x59.5), obitos.9+x59.5, obitos.9))))))

rm(trans,inj,mat,ICD_x59)
gc()
#Validação

obitos_para_redis <- sum(base.r[grepl('_x59',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.9,na.rm = T)
round(sum(base.5$obitos.10,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis))

##### y34----

trans <-c(causas[grepl("^trans",causas)])
dcnt<- c(causas[grepl("^dcnt",causas)])
inj <- c(causas[grepl("^Injuries",causas)])
mat <-c(causas[grepl("^materna",causas)])

ICD_y34 <- ICD%>%
  filter(CG=="_y34")%>%
  select(target,weight)

y34 <- data.frame(target = c(dcnt,inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

y34 <- y34%>%
  left_join(ICD_y34,by="target")

base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red,-weight) %>%
  mutate(c.red=ifelse(GBD %in% y34$target, '_y34', NA)) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções y34

###PRMUN
muni.y34 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(cdmun,micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.10, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.y34 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(micro,meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.10, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.y34 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by(meso, GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.10, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.y34 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by( GBD,idade, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.10, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.y34 <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  group_by( GBD,idade, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.10, na.rm = T))%>%
  ungroup() %>%
  group_by(idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(muni.y34, by=c('cdmun','micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(micro.y34, by=c('micro','meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(meso.y34, by=c('meso', 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.y34, by=c( 'GBD','idade', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.y34, by=c( 'GBD','idade', 'ano', 'sexo', 'reg'))


base.5 <- base.5 %>%
  left_join(y34,by=c("GBD"="target"))%>%
  mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
  mutate(y34.1=ifelse(GBD %in% c(inj,dcnt),redis,redis*pr.mu),
         redis.2=ifelse(is.na(y34.1)| y34.1 == 0  & ob.mu==0, redis,NA),
         y34.2=ifelse(GBD %in% c(inj,dcnt),redis.2,redis.2*pr.mi),
         redis.3=ifelse(is.na(y34.2) & ob.mi==0, redis.2,NA),
         y34.3=ifelse(GBD %in% c(inj,dcnt),redis.3,redis.3*pr.me),
         redis.4=ifelse(is.na(y34.3) & ob.me==0, redis.3,NA),
         y34.4=ifelse(GBD %in% c(inj,dcnt),redis.4,redis.4*pr.uf),
         redis.5=ifelse(is.na(y34.4) & ob.uf==0, redis.4,NA),
         y34.5=ifelse(GBD %in% c(inj,dcnt),redis.5,redis.5*pr.rg),
         obitos.11= ifelse(!is.na(y34.1), obitos.10+y34.1,
                           ifelse(!is.na(y34.2), obitos.10+y34.2,
                                  ifelse(!is.na(y34.3), obitos.10+y34.3,
                                         ifelse(!is.na(y34.4), obitos.10+y34.4,
                                                ifelse(!is.na(y34.5), obitos.10+y34.5, obitos.10))))))

rm(trans,inj,mat,dcnt,ICD_y34)
rm(list = ls()[grepl("^meso|^micro|^muni|^rg|^uf",ls())])
gc()

#Validação

obitos_para_redis <- sum(base.r[grepl('_y34',base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.10,na.rm = T)
round(sum(base.5$obitos.11,na.rm = T))==round(sum(obitos_para_redis,obitos_pre_redis),0)

#### Pneumo DCV ----

trans <-c(causas[grepl("^trans",causas)])
dcnt<- c(causas[grepl("^dcnt",causas)])
inj <- c(causas[grepl("^Injuries",causas)])
mat <-c(causas[grepl("^materna",causas)])

ICD_pneumo <- ICD%>%
  filter(CG=="_pneumo")%>%
  select(target,age,weight)

pneumo <- data.frame(target = c("_pneumo",dcnt,inj,trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages"))

pneumo <- pneumo%>%
  left_join(ICD_pneumo,by="target")%>%
  mutate(c.red=case_when(age == "<10" ~ '_pneumo_inf',
                         age == "10 a 59" ~ '_pneumo_adult',
                         age == "60 emais" ~ '_pneumo_idoso',))%>%
  select(-age)

base.r.pneumo <- base.r%>%
  filter(c.red == "_pneumo")%>%
  mutate(c.red=case_when(c.red == "_pneumo" & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                         c.red == "_pneumo" & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                         c.red == "_pneumo" & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso'))


base.5 <- base.5 %>%
  select(!(pr.mu:ob.rg),-redis,-redis.2, -redis.3,-redis.4,-c.red,-weight) %>%
  mutate(c.red=case_when(GBD %in% pneumo$target & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5") ~ '_pneumo_inf',
                         GBD %in% pneumo$target & idade %in% c("10","15","20","25","30","35","40","45","50","55") ~ '_pneumo_adult',
                         GBD %in% pneumo$target & idade %in% c("60","65","70","75","80","85","90") ~ '_pneumo_idoso')) %>%
  left_join(base.r.pneumo, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

###Proporções PNE

###PRMUN
muni.pne <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
  group_by(cdmun,micro,meso,c.red,idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.11, na.rm = T))%>%
  ungroup() %>%
  group_by(cdmun,micro,meso,c.red, idade, ano, sexo, uf) %>%
  mutate(pr.mu=ob/sum(ob),
         ob.mu=sum(ob)) %>%
  select(-ob)

###PR.MICRO
micro.pne <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
  group_by(micro,meso, c.red,idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.11, na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso, c.red,idade, ano, sexo, uf) %>%
  mutate(pr.mi=ob/sum(ob),
         ob.mi=sum(ob)) %>%
  select(-ob)

###PR.MESO
meso.pne <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
  group_by(meso, c.red,idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.11, na.rm = T))%>%
  ungroup() %>%
  group_by(meso,c.red, idade, ano, sexo, uf) %>%
  mutate(pr.me=ob/sum(ob),
         ob.me=sum(ob)) %>%
  select(-ob)

###PR.UF
uf.pne <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
  group_by( c.red, idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos.11, na.rm = T))%>%
  ungroup() %>%
  group_by(c.red, idade, ano, sexo, uf) %>%
  mutate(pr.uf=ob/sum(ob),
         ob.uf=sum(ob)) %>%
  select(-ob)

###PR.REG
rg.pne <- base.5 %>%
  filter(GBD %in% c(trans,mat,"other_causes_all","other_causes-lri","other_desnutricao_all_ages")) %>%
  filter(c.red %in% c('_pneumo_inf','_pneumo_adult','_pneumo_idoso'))%>%
  group_by(c.red, idade, GBD, ano, sexo, reg) %>%
  summarise(ob=sum(obitos.11, na.rm = T))%>%
  ungroup() %>%
  group_by(c.red, idade, ano, sexo, reg) %>%
  mutate(pr.rg=ob/sum(ob),
         ob.rg=sum(ob)) %>%
  select(-ob)

base.5 <- base.5 %>%
  left_join(muni.pne, by=c('cdmun','micro','meso','c.red', 'idade','GBD', 'ano', 'sexo', 'uf')) %>%
  left_join(micro.pne, by=c('micro','meso', 'c.red', 'idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
  left_join(meso.pne, by=c('meso','c.red', 'idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
  left_join(uf.pne, by=c( 'c.red','idade', 'GBD', 'ano', 'sexo', 'uf')) %>%
  left_join(rg.pne, by=c( 'c.red','idade','GBD', 'ano', 'sexo', 'reg'))

base.5 <- base.5 %>%
  left_join(pneumo,by=c("GBD"="target","c.red"="c.red"))%>%
  mutate(redis=ifelse(!is.na(weight),redis*weight,redis))%>%
  mutate(pne.1=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis,redis*pr.mu),
         redis.2=ifelse(is.na(pne.1)  & ob.mu==0, redis,NA),
         pne.2=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.2,redis.2*pr.mi),
         redis.3=ifelse(is.na(pne.2) & ob.mi==0, redis.2,NA),
         pne.3=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.3,redis.3*pr.me),
         redis.4=ifelse(is.na(pne.3) & ob.me==0, redis.3,NA),
         pne.4=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.4,redis.4*pr.uf),
         redis.5=ifelse(is.na(pne.4) & ob.uf==0, redis.4,NA),
         pne.5=ifelse(GBD %in% c("_pneumo",inj,dcnt),redis.5,redis.5*pr.rg),
         obitos.12= ifelse(!is.na(pne.1) , obitos.11+pne.1,
                           ifelse(!is.na(pne.2), obitos.11+pne.2,
                                  ifelse(!is.na(pne.3), obitos.11+pne.3,
                                         ifelse(!is.na(pne.4), obitos.11+pne.4,
                                                ifelse(!is.na(pne.5), obitos.11+pne.5, obitos.11))))))%>%
  mutate(obitos.12=ifelse(GBD%in%"_pneumo",obitos.12-obitos.11,obitos.12))


rm(trans,inj,mat,dcnt,ICD_pneumo)
gc()

#Validação

base.r.sp <- base.r %>%
  filter(c.red%notin%c("_pneumo","_injuries","_inj (hom,sui)","_inj (hom,suic, fall)","_inj (hom,sui,transp)","_inj (hom,suic,other)","_maternas","_infant_neonat","_x59","_y34")) %>%
  group_by(cdmun,sexo,idade) %>%
  summarise(redis=sum(redis,na.rm=T))

sum(base.r.sp$redis)+sum(base.5$obitos.12,na.rm=T)


#### Casos redistribuídos entre todas as causas------
#### Criação da variável com a causa CG para nortear a redistribuição
base.5 <- base.5 %>%
  select(-redis,-redis.2, -redis.3, -redis.4, -redis.5) %>%
  mutate(c.red=ifelse(GBD%in%"_pneumo",NA,'_all')) %>%
  left_join(base.r, by=c('cdmun','micro','meso',  'ano', 'sexo','idade', 'uf', 'c.red'))

base.5 <- base.5 %>%
  mutate(all.1=redis*pr.mu,
         redis.2=ifelse(is.na(all.1) & ob.mu==0, redis,NA),
         all.2=redis.2*pr.mi,
         redis.3=ifelse(is.na(all.2) & ob.mi==0, redis.2,NA),
         all.3=redis.3*pr.me,
         redis.4=ifelse(is.na(all.3) & ob.me==0, redis.3,NA),
         all.4=redis.4*pr.uf,
         redis.5=ifelse(is.na(all.4) & ob.uf==0, redis.4,NA),
         all.5=redis.5*pr.rg,
         obitos.13=ifelse(!is.na(all.1), obitos.12+all.1,
                          ifelse(!is.na(all.2), obitos.12+all.2,
                                 ifelse(!is.na(all.3), obitos.12+all.3,
                                        ifelse(!is.na(all.4), obitos.12+all.4,
                                               ifelse(!is.na(all.5), obitos.12+all.5, obitos.12))))))

#Validação
obitos_para_redis <- sum(base.r[grepl("_all",base.r$c.red),]$redis, na.rm = T)
obitos_pre_redis <- sum(base.5$obitos.12,na.rm = T)
round(sum(base.5$obitos.13,na.rm = T),0)==round(sum(obitos_para_redis,obitos_pre_redis),0)

#Tempo de processamento
end_time <- Sys.time()
start_time -end_time



#Validações finais----
# getwd()
# load("PASSO_1_2023-12-01SIM_2016a2018_2.Rdata")#carrega o b.raiz para fazer as validações
#
# b.raiz <- b.raiz%>%
#   #filter(grepl("^1",perl = T, cdmun))%>%
#   filter(ano==2010)

#round(sum(base.5$obitos.13,na.rm = T),0)==round(sum(b.raiz$obitos),0)

#round(sum(b.raiz$obitos),0)-round(sum(base.5$obitos.13,na.rm = T),0)

save(base.5, file=paste0('PASSO_3_',Sys.Date(),resultado,'.Rdata'))
#
# export(base.5, file=paste0('PASSO_3_',Sys.Date(),resultado,'.csv'))
base.5 %>%
  group_by(ano) %>%
  summarise(n=sum(obitos.13,na.rm=T))


a <- base.5 %>%
  group_by(ano) %>%
summarise(ob=sum(obitos.13,na.rm=T))
