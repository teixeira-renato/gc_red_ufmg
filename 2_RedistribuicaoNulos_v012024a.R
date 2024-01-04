rm(list=ls())
gc()

start_time <- Sys.time()

pacman::p_load(tidyverse,readr, dtplyr,rio) # pacotes necessários

options(scipen=999)

path = "/Users/renatoteixeira/Library/CloudStorage/OneDrive-UniversidadeFederaldeMinasGerais/Small Areas Estimation/Redistribuição GC/SAS to R/Script V7 2023/Base"
setwd(path)
resultado = "SIM_2010e2019_2"

# Bases----

load("PASSO_1_2023-12-19SIM_2010e2019_2.Rdata")

ICD <- import("ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx", sheet = 1, colClasses="character")

#Codigo de municipio

# b.raiz <- b.raiz%>%
#   filter(grepl("^31", cdmun))%>%
#   filter(ano==2016)

mumime <- read_csv("2021-04-19_codmun_ibge_gbd.csv",
                   locale = locale(encoding = "WINDOWS-1252"))
mumime <- mumime %>%
  select(municode.6, microcode, mesocode)
colnames(mumime) <- c('cdmun','micro', 'meso')
mumime$cdmun <- as.character(mumime$cdmun)

cdmun <- unique(as.character(b.raiz$cdmun))

notwantedlevels <- c(110000,120000,130000,140000, 150000, 160000, 170000, 210000, 220000,
                     230000,240000,250000,260000,270000,280000,290000, 310000,320000,330000,
                     350000, 410000,420000,430000,500000,510000,520000,000000)

cdmun <- cdmun[!cdmun %in% notwantedlevels]

####base Pop
pop <- read_csv(paste0(path,'/Produto4-2010-2030_AJ_Cairo.csv'),locale = locale(encoding = "ISO-8859-7"))
pop <- pop %>%
  filter(Ano%in%b.raiz$ano)

variable.names(pop)
colnames(pop) <- c('ano', 'uf', 'sexo', 'cdmun.7', 'nome', as.character(seq(0,90,5)), 'total')

head(pop)
cols <- variable.names(pop[,6:25])
pop <- pop %>%
  mutate_at(all_of(cols), ~ str_replace(., ",", "."))


pop[cols] <- as.data.frame(lapply(pop[cols],  function(y) as.numeric(y)))
pop.2 <- pop %>%
  select(-total, -nome, -uf) %>%
  pivot_longer(!(ano:cdmun.7), names_to='idade', values_to='pop') %>%
  mutate(cdmun=str_sub(cdmun.7,1,6),
         uf=str_sub(cdmun.7,1,2)) %>%
  select(-cdmun.7)

rm(pop)

pop.2$sexo <- recode(pop.2$sexo, 'f'='Feminino', 'm'='Masculino')
pop.2$idade <- as.character(pop.2$idade)
pop.2$ano <- as.character(pop.2$ano)

cd.2 <- unique(pop.2$cdmun)

`%notin%` <- Negate(`%in%`)


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
             "infant_lri_post_neo" , "infant_neonatal_encefalopatia",
             "infant_subita"       , "infant_neonatal_hemolitica",
             "obst_intestinal"     , "infant_neonatal_prematuridade",
             "infant_neonatal_other","infant_neonatal_sepsis")

redis <- c("_injuries" , "_inj (hom,suic, fall)"  , "_all", "_inj (hom,suic,other)" ,
           "_pneumo","_inj (hom,sui)" , "_inj (hom,sui,transp)","_maternas", "_x59", "_y34","_infant_neonat")

##### Dados SIM IGNORADOS

ign <- b.raiz%>%
  ungroup() %>%
  mutate(idade=ifelse(idade ==999,'IGN',idade))%>%
  filter(cdmun %in% notwantedlevels | sexo=='IGN' | idade =='IGN')

colnames(ign)[8] <- 'ign'
sum(ign$ign) # total de registros com campos ignorados


base <- b.raiz %>%
  filter(cdmun %notin% notwantedlevels & sexo!='IGN' & idade!=999 & idade!='IGN')

if ((sum(nrow(base),nrow(ign)))==nrow(b.raiz)) print("TRUE"); # rm(b.raiz)

#####Base todos
causa <- unique(base$GBD)
sexo <- unique(base$sexo)
anos <- unique(base$ano)
# age <- as.character(c(seq(0,90,5)))
age <- c(seq(0,90,5), "Early Neonatal", "Post Neonatal", "Late Neonatal")
mat <-c(causas[grepl("^materna",causas)])
infant <-c(causas[grepl("^infant",causas)])
base.1 <- expand.grid(cdmun,anos,age,sexo,causa)
colnames(base.1) <- c('cdmun','ano','idade','sexo', 'GBD')

base.1 <- base.1%>%
  mutate(to_exclude= case_when(
    GBD %in% mat & idade %in% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year","0","5","60","65","70","75","80","85","90") ~ 1,
    GBD %in% mat & sexo == "Masculino"  ~ 1,
    # GBD %in% infant & idade %notin% c("Early Neonatal","Post Neonatal","Late Neonatal","<1 year") ~ 1,
    TRUE ~ 0
  ))%>%
  filter(to_exclude==0)%>%
  select(-to_exclude)

base.1 <- left_join(base.1,mumime, by='cdmun')

base.1$idade <- as.character(base.1$idade)
base.2 <- left_join(base.1, base[1:8], by=c(colnames(base)[1:7]))
base.2 <- base.2 %>%
  mutate(uf=str_sub(cdmun,1,2))

#perde um cara aqui
sum(base.2$obitos,na.rm=T)+sum(ign$ign)

base.2 <- left_join(base.2,pop.2, by=c('cdmun','uf', 'ano', 'sexo', 'idade'))

base.2 %>%
  mutate(reg=substr(cdmun,1,1)) %>%
  filter(GBD=="Injuries - Homicide",
         reg==5) %>%
  group_by(GBD) %>%
  summarise(n=sum(obitos,na.rm=T))

sum(base.2$obitos,na.rm=T)+sum(ign$ign)

table(is.na(base.2$pop))
if(sum(base.2$obitos, na.rm = T) == sum(base$obitos, na.rm = T)) print("manteve o mesmo total de óbitos"); rm(base,base.1)
base.2$obitos[is.na(base.2$obitos)] <- 0
gc()

#Proporções----

####Município
base.2 <- base.2 %>%
  lazy_dt()%>%
  group_by(cdmun,micro,meso, GBD, ano, sexo, uf) %>%
  mutate(mu.id=sum(obitos,na.rm = T),
         pr.mu.id=obitos/sum(obitos,na.rm = T)) %>%
  ungroup() %>%
  group_by(cdmun, GBD, ano, idade, uf) %>%
  mutate(mu.s=sum(obitos,na.rm = T),
         pr.mu.s=obitos/sum(obitos,na.rm = T)) %>%
  ungroup() %>%
  ###Sem sexo, sem idade
  group_by(cdmun, GBD, ano, uf) %>%
  mutate(mu=sum(obitos,na.rm = T),
         pr.mu=obitos/sum(obitos,na.rm = T)) %>%
  ungroup() %>%
  ####Pela População Geral(Município Conhecido)
  group_by(cdmun, ano,GBD, idade,sexo, uf) %>%
  mutate(pop.id.s=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem ID(Município Conhecido)
  group_by(cdmun, ano,GBD, sexo, uf) %>%
  mutate(pop.id=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem sexo(Município Conhecido)
  group_by(cdmun, ano,GBD, idade, uf) %>%
  mutate(pop.s=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem ID e sexo(Município Conhecido)
  group_by(cdmun, ano,GBD,  uf) %>%
  mutate(pop.t=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População Geral(Em Município)
  group_by(ano,GBD, idade,sexo, uf) %>%
  mutate(pmu.id.s=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem ID
  group_by(ano,GBD, sexo, uf) %>%
  mutate(pmu.id=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem sexo
  group_by(ano,GBD, idade, uf) %>%
  mutate(pmu.s=pop/sum(pop)) %>%
  ungroup() %>%
  ####Pela População sem ID e sexo
  group_by(ano,GBD,  uf) %>%
  mutate(pmu.t=pop/sum(pop)) %>%
  ungroup()%>%
  as_tibble()
gc()

###base.micro
micro <- base.2 %>%
  lazy_dt()%>%
  group_by(micro,meso,idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos,na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso, GBD, ano, sexo, uf) %>%
  mutate(pr.mi.id=ob/sum(ob,na.rm = T),
         ob.mi.id=sum(ob,na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso, GBD, ano, idade, uf) %>%
  mutate(pr.mi.s=ob/sum(ob,na.rm = T),
         ob.mi.s=sum(ob,na.rm = T))%>%
  ungroup() %>%
  group_by(micro,meso, GBD, ano, uf) %>%
  mutate(pr.mi.id.s=ob/sum(ob,na.rm = T),
         ob.mi.id.s=sum(ob,na.rm = T))%>%
  ungroup()%>%
  select(-ob)%>%
  as_tibble()
gc()

###base.meso

meso <- base.2 %>%
  lazy_dt()%>%
  group_by(meso,idade, GBD, ano, sexo, uf) %>%
  summarise(ob=sum(obitos,na.rm = T))%>%
  ungroup() %>%
  group_by(meso, GBD, ano, sexo, uf) %>%
  mutate(pr.me.id=ob/sum(ob,na.rm = T),
         ob.me.id=sum(ob,na.rm = T))%>%
  ungroup() %>%
  group_by(meso, GBD, ano, idade, uf) %>%
  mutate(pr.me.s=ob/sum(ob,na.rm = T),
         ob.me.s=sum(ob,na.rm = T))%>%
  ungroup() %>%
  group_by(meso, GBD, ano, uf) %>%
  mutate(pr.me.id.s=ob/sum(ob,na.rm = T),
         ob.me.id.s=sum(ob,na.rm = T))%>%
  ungroup()  %>%
  select(-ob)%>%
  as_tibble()
gc()

###base.uf

uf <- base.2 %>%
  lazy_dt()%>%
  group_by(idade, GBD, ano, sexo, uf) %>%
  summarise(ob.uf=sum(obitos,na.rm = T))%>%
  ungroup() %>%
  group_by(GBD, ano, sexo, uf) %>%
  mutate(pr.uf.id=ob.uf/sum(ob.uf,na.rm = T),
         ob.uf.id=sum(ob.uf,na.rm = T))%>%
  ungroup() %>%
  group_by( GBD, ano, idade, uf) %>%
  mutate(pr.uf.s=ob.uf/sum(ob.uf,na.rm = T),
         ob.uf.s=sum(ob.uf,na.rm = T))%>%
  ungroup() %>%
  group_by( GBD, ano, uf) %>%
  mutate(pr.uf.id.s=ob.uf/sum(ob.uf,na.rm = T),
         ob.uf.id.s=sum(ob.uf,na.rm = T))%>%
  ungroup()%>%
  as_tibble()

gc()

###Redistribuição
####Sem Idade
##No município

base.3 <- base.2 %>%
  left_join( micro, by=c('micro','idade','sexo', 'meso', 'GBD', 'ano', 'uf'))

rm(base.2, micro)
gc();gc()

base.3 <- base.3 %>%
  left_join( meso, by=c('meso','idade','sexo', 'GBD', 'ano',  'uf'))

rm(meso)
gc();gc()

base.3<- base.3 %>%
  left_join( uf, by=c( 'GBD', 'idade','sexo', 'ano','uf'))

rm(uf)
gc();gc()



#Redistribuição ign----

####Sem Idade
##No município
ig.id <- ign %>%
  filter(idade=='IGN'& cdmun %notin% notwantedlevels & sexo!='IGN') %>%
  select(-idade)%>%
  group_by(cdmun,micro,meso,sexo,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))

base.3 <- dtplyr::lazy_dt(base.3)
ig.id <- dtplyr::lazy_dt(ig.id)

base.3 <- left_join(base.3, ig.id, by=c('cdmun','micro', 'meso', 'uf', 'GBD', 'sexo', 'ano'))
base.3 <- as_tibble(base.3)
ig.id <- as_tibble(ig.id)

base.3 <- mutate(base.3,id.1=ign*pr.mu.id,
                 ign.2=ifelse(is.na(id.1) & mu.id==0, ign,NA),
                 id.2=ign.2*pr.mi.id,
                 ign.3=ifelse(is.na(id.2) & ob.mi.id==0, ign,NA),
                 id.3=ign.3*pr.me.id,
                 ign.4=ifelse(is.na(id.3) & ob.me.id==0, ign,NA),
                 id.4=ign.4*pr.uf.id,
                 ign.5=ifelse(is.na(id.4) & ob.uf.id==0, ign,NA),
                 id.5=ign.5*pop.id,
                 obi.2= ifelse(!is.na(id.1), obitos+id.1,
                               ifelse(!is.na(id.2), obitos+id.2,
                                      ifelse(!is.na(id.3), obitos+id.3,
                                             ifelse(!is.na(id.4), obitos+id.4,
                                                    ifelse(!is.na(id.5), obitos+id.5,obitos)))))) %>%
  select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)%>%
  as_tibble()

gc();gc()

###########Sem sexo
ig.sex <- ign %>%
  filter(idade!='IGN'& !cdmun %in% notwantedlevels & sexo=='IGN') %>%
  select(-sexo)%>%
  group_by(cdmun,micro,meso,idade,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))

base.3 <- left_join(base.3, ig.sex, by=c('cdmun','micro', 'meso', 'uf', 'GBD', 'idade', 'ano'))%>%
  mutate(s.1=ign*pr.mu.s,
         ign.2=ifelse(is.na(s.1) & mu.s==0, ign,NA),
         s.2=ign.2*pr.mi.s,
         ign.3=ifelse(is.na(s.2) & ob.mi.s==0, ign,NA),
         s.3=ign.3*pr.me.s,
         ign.4=ifelse(is.na(s.3) & ob.me.s==0, ign,NA),
         s.4=ign.4*pr.uf.s,
         ign.5=ifelse(is.na(s.4) & ob.uf.s==0, ign,NA),
         s.5=ign.5*pop.s,
         obi.3=ifelse(!is.na(s.1), obi.2+s.1,
                      ifelse(!is.na(s.2), obi.2+s.2,
                             ifelse(!is.na(s.3), obi.2+s.3,
                                    ifelse(!is.na(s.4), obi.2+s.4,
                                           ifelse(!is.na(s.5), obi.2+s.5,obi.2)))))) %>%
  select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)



#### Sem sexo e idade
ig.id.sex <- ign %>%
  filter(idade=='IGN'& !cdmun %in% notwantedlevels & sexo=='IGN') %>%
  select(-c(idade,sexo))%>%
  group_by(cdmun,micro,meso,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))



base.3 <- left_join(base.3, ig.id.sex, by=c('cdmun','micro', 'meso', 'uf', 'GBD',  'ano')) %>%
  mutate(id.s.1=ign*pr.mu,
         ign.2=ifelse(is.na(id.s.1) & mu==0, ign,NA),
         id.s.2=ign.2*pr.mi.id.s,
         ign.3=ifelse(is.na(id.s.2) & ob.mi.id.s==0, ign,NA),
         id.s.3=ign.3*pr.me.id.s,
         ign.4=ifelse(is.na(id.s.3) & ob.me.id.s==0, ign,NA),
         id.s.4=ign.4*pr.uf.id.s,
         ign.5=ifelse(is.na(id.s.4) & ob.uf.id.s==0, ign,NA),
         id.s.5=ign.5*pop.id.s,
         obi.4=ifelse(!is.na(id.s.1),obi.3+id.s.1,
                      ifelse(!is.na(id.s.2),obi.3+id.s.2,
                             ifelse(!is.na(id.s.3),obi.3+id.s.3,
                                    ifelse(!is.na(id.s.4),obi.3+id.s.4,
                                           ifelse(!is.na(id.s.5),obi.3+id.s.5,obi.3)))))) %>%
  select(-ign,-ign.2, -ign.3, -ign.4, -ign.5)


##### Sem MUN
ig.mun <- ign %>%
  filter(idade!='IGN'& cdmun %in% notwantedlevels & sexo!='IGN') %>%
  select(!(cdmun:meso))%>%
  group_by(sexo,idade,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))



base.3 <- left_join(base.3, ig.mun, by=c( 'uf', 'GBD', 'ano','idade','sexo')) %>%
  mutate(ig.mu=ign*(obitos/ob.uf),
         ig.pop=ifelse(is.na(ig.mu) & ob.uf==0, ign*pmu.id.s, NA),
         obi.5= ifelse(!is.na(ig.mu),  obi.4+ig.mu,
                       ifelse(!is.na(ig.pop), obi.4+ig.pop, obi.4)))  %>%
  select(-ign)

##### Sem MUN  e id
ig.mun.id <- ign %>%
  filter(idade=='IGN'& cdmun %in% notwantedlevels & sexo!='IGN') %>%
  select(!(cdmun:meso), -idade) %>%
  group_by(sexo,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))


base.3 <- left_join(base.3, ig.mun.id, by=c( 'uf', 'GBD', 'ano','sexo')) %>%
  mutate(ig.mu.id=ign*(obitos/ob.uf.id),
         ig.id.pop=ifelse(is.na(ig.mu.id) & ob.uf.id==0, ign*pmu.id, NA),
         obi.6=ifelse(!is.na(ig.mu.id), obi.5+ig.mu.id,
                      ifelse(!is.na(ig.id.pop),obi.5+ig.id.pop, obi.5)))  %>%
  select(-ign)

##### Sem MUN  e sexo
ig.mun.s <- ign %>%
  filter(idade!='IGN'& cdmun %in% notwantedlevels & sexo=='IGN') %>%
  select(!(cdmun:sexo)) %>%
  group_by(idade,ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))

base.3 <- left_join(base.3, ig.mun.s, by=c( 'uf', 'GBD', 'ano','idade')) %>%
  mutate(ig.mu.s=ign*(obitos/ob.uf.s),
         ig.s.pop=ifelse(is.na(ig.mu.s) & ob.uf.s==0, ign*pmu.s, NA),
         obi.7=ifelse(!is.na(ig.mu.s),  obi.6+ig.mu.s,
                      ifelse(!is.na(ig.s.pop), obi.6+ig.s.pop, obi.6)))  %>%
  select(-ign)


##### Sem mun, idade e sexo
rm(ig.id,ig.id.sex,ig.mun,ig.mun.id,ig.mun.s,ig.sex,pop.2,cd.2,causa,cols)
gc();gc()


ig.mun.s.id <- ign %>%
  filter(idade=='IGN'& cdmun %in% notwantedlevels & sexo=='IGN') %>%
  select(!(cdmun:idade))%>%
  group_by(ano,GBD,uf) %>%
  summarise(ign=sum(ign,na.rm=T))

base.3 <- base.3%>%
  left_join(ig.mun.s.id, by=c( 'uf', 'GBD', 'ano'))

base.3 <- base.3 %>%
  mutate(ig.mu.s.id=ign*(obitos/ob.uf.id.s))
base.3 <- base.3%>%
  mutate(ig.s.id.pop=ifelse(is.na(ig.mu.s.id) & ob.uf.id.s==0, ign*pmu.t, NA))
base.3 <- base.3%>%
  mutate(obitos.2=ifelse(!is.na(ig.mu.s.id),  obi.7+ig.mu.s.id,
                         ifelse(!is.na(ig.s.id.pop), obi.7+ig.s.id.pop, obi.7)))
base.3 <- base.3%>%
  select(-ign)

rm(ign,ig.mun.s.id)
gc();gc()

# validacao
sum(base.3$obitos.2,na.rm = T)==sum(b.raiz$obitos,na.rm = T)

sum(b.raiz$obitos,na.rm = T)-sum(base.3$obitos.2,na.rm = T)

unique(base.3$idade)

save(base.3, file=paste0("PASSO_2_",Sys.Date(),resultado,'.Rdata'))

sum(base.3$obitos.2)
sum(b.raiz$obitos)

#Tempo de processamento
end_time <- Sys.time()
start_time -end_time

