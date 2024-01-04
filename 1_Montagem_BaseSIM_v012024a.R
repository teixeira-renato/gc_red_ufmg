# Preparação de dados SIM
rm(list=ls())
gc()


## Carragamento das dependências ----

if (!require("pacman")) install.packages("pacman") #garantir que o pacman está instalado
pacman::p_load(read.dbc,tidyverse,readr,rio) # pacotes necessários

path = "/Users/renatoteixeira/Library/CloudStorage/OneDrive-UniversidadeFederaldeMinasGerais/Small Areas Estimation/Redistribuição GC/SAS to R/Script V7 2023/Base"
setwd(path)
resultado = "SIM_2010e2019_2"

# Bases----

#arquivo com o código das idades no SIM
cod.idades <- read.csv('IDADE_SIM_GBD.csv', colClasses="character")

cod.idades.2 <- cod.idades[,1:2]

cod.idades.2 <- cod.idades.2 %>%
  mutate(IDADE=str_pad(IDADE,3,"left",pad="0")) %>%
  unique()

rm(cod.idades)

###Arquivo com as categoria das causas e os CID
causas <- import("ICD_MAPPING_V6_2023 pos OPAS_nov2023.xlsx", sheet = 1, colClasses="character")
colnames(causas)[1] <- 'CAUSABAS'

##Arquivo com as micros e as mesos regiões
mumime <- read_csv("2021-04-19_codmun_ibge_gbd.csv",
                   locale = locale(encoding = "WINDOWS-1252"))
mumime <- mumime %>%
  select(municode.6, microcode, mesocode)%>%
  mutate(cdmun = as.factor(municode.6))%>%
  select(-municode.6)

###Função para criar os grupos de idades
age.cat = function(age){
  result = 5*(age %/% 5)
  nove = age>=90
  na =(is.na(age))
  result[nove] = 90
  result[na]= NA
  return(result)
}

###Leitura dos DBCs



path2 <- "/Users/renatoteixeira/Library/CloudStorage/OneDrive-UniversidadeFederaldeMinasGerais/Small Areas Estimation/Redistribuição GC/SAS to R/Script V7 2023/Rdata"

file.names <- list.files(path2, pattern ="2010.Rdata|2019.Rdata",recursive = T)
setwd(path2)


# for(i in 1:length(file.names)){
#   print(file.names[i])
#   if(grepl(".dbc|.DBC",file.names[i])){
#     file <- read.dbc(file.names[i])
#   }else{
#     file <- import(file.names[i])
#   }
#   out.file <- bind_rows(out.file, file)
#   rm(file)
# }

selected_variables <- c("DTOBITO","IDADE","SEXO","CODMUNRES","CAUSABAS")
out.file <- data.frame()
for(i in 1:length(file.names)){
  print(file.names[i])
  load(file.names[i])
  object_name_without_ext <- tools::file_path_sans_ext(file.names[i])
  print(nrow(get(object_name_without_ext)))
  selected_data <- get(object_name_without_ext)[, selected_variables]
  out.file <- bind_rows(selected_data, out.file)
  rm(selected_data)
}

out.file %>%
  mutate(ano=str_sub(DTOBITO,5,8)) %>%
  group_by(ano) %>%
  summarise(ob=n())

setwd(path)
#Paraimportar a sua base sem passar pelo comando anterior carregabdo os dbc ou dbf originais do SIM.
#out.file <- import("/Users/Ademar/Library/CloudStorage/OneDrive-MinistériodaSaúde/Documentos/TabWin/Arquivos da Rede/DO/DOBR2016.DBF", as.is=T)
#Tratamento dos dados ----

# total_base_2010_2021 <- out.file %>%
#   group_by(ANO) %>%
#   summarise(n=n())

###Merge Códigos de Idade

out.file <- left_join(out.file, cod.idades.2, by='IDADE') # Categoriza todas as idade do SIM em faixas etárias

###Tratamento da Variável Idade
out.file<- out.file %>%
  mutate(age=IDADE_ANOS)%>%
  mutate(age= recode(age, 'MENOR 1 ANO IGN'='Post Neonatal', '0 A 6 DIAS'='Early Neonatal', '7 A 27 DIAS'= 'Late Neonatal','28 A 364 DIAS'='Post Neonatal', '1 ANO'='1 ANOS'))

###Criação da Variável ano e cdmun com 6 dígitos
out.file <- out.file %>%
  mutate(idade.cat=ifelse(grepl("^\\d{1}",perl = T,.$age),
                          age.cat(as.numeric(str_sub(age, end = -6))),.$age),
         ano=str_sub(DTOBITO,5,8),
         cdmun=str_sub(CODMUNRES,end=6))

table(out.file$ano, exclude = NULL) # Total de registros no SIM
conf=filter(out.file,is.na(idade.cat))
if(nrow(conf)<1) rm(conf)

base <- out.file %>%
  select(cdmun, SEXO, CAUSABAS, age, idade.cat, ano)

# Unindo as tabelas ----

###Merge Causas GBD

base <- left_join(base, causas, by='CAUSABAS')

table(base$CLASS_GPEAS_EXT, exclude = NULL)
sum(table(base$CLASS_GPEAS_EXT, exclude = NULL))==length(out.file$DTOBITO) # garante que todos os registros receberam uma causa

###Merge Micro, Meso

base <- left_join(base, mumime, by='cdmun')

###Base por idade, sexo e etc.

base.2 <- base %>%
  group_by(cdmun,microcode, mesocode,  SEXO, idade.cat, ano, CLASS_GPEAS_EXT) %>%
  summarise(ob=n())

sum(base.2$ob)==nrow(base) #garante a contagem adequada nas causas

base.2$idade.cat[is.na(base.2$idade.cat)] <- 'IGN'
sum(base.2$ob[base.2$idade.cat=='IGN'])

table(base.2$SEXO, exclude = NULL)
base.2$SEXO <- recode(base.2$SEXO, '1'='Masculino', '2'='Feminino', '0'='IGN', '9'='IGN')
table(base.2$SEXO, exclude = NULL)
sum(base.2$ob[base.2$SEXO=='IGN'])==sum(base$SEXO=='0'|base$SEXO=='9')

b.raiz <- base.2
colnames(b.raiz) <- c('cdmun','micro', 'meso',  'sexo', 'idade','ano', 'GBD',  'obitos')
b.raiz <- b.raiz %>%
  mutate(uf=str_sub(cdmun,1,2))

sum(base.2$ob, na.rm = T) #Total de registros

save(b.raiz, file=paste0('PASSO_1_',Sys.Date(),resultado,'.Rdata'))


b.raiz %>%
  group_by(ano) %>%
  summarise(ob=sum(obitos,na.rm=T))
