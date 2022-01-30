library(tidyverse)
library(readxl)

#Prepara os links para os downloads dos arquivos
vigitel_ano <- tibble("http://svs.aids.gov.br/download/Vigitel/Vigitel-", seq(from=2006, to=2020, by=1),"-peso-rake.xls")  
links <- apply(vigitel_ano, 1,  list) %>% sapply(., function(x) x[[1]] %>% unname() %>% paste(collapse=""))

#Download dos arquivos
download.file(links, basename(links), method = "libcurl")

#Junta todos os anos em um DF
#Full_vigitel <- list.files(pattern=".xls") %>% lapply(., read_excel) 
## Não compensa usar, pq as planilhas tem variáveis diferentes

v2020_backup <- read_excel("Vigitel-2020-peso-rake.xls")
v2020 <- v2020_backup

Helper_2020 <-  read_excel("Dicionario-de-dados-Vigitel 2020.xls", 
                           skip = 2)

#Escreve tabela para adicionar o nome das colunas manualmente
write_csv(x=tibble(colnames=v2020 %>% colnames()),
          file="vig2020.csv")


#Carega as colunas para serem mantidas e os nomes corretos
cols_keep <- read_delim("vig2020.csv", delim = ";", 
           escape_double = FALSE, trim_ws = TRUE)%>% drop_na()


#Ajusta o nome das colunas e seleciona as colunas que serão mantidas no DF original
#Alem disso troca os valores 777 e 888 por NA
v2020 <- v2020 %>% select(cols_keep$colnames) %>%
  rename_with(~cols_keep[[2]],
              .cols=cols_keep$colnames) %>%
  sapply(., function(x) replace(x, x %in% c("777", "888"), NA))
  
#Cria tabela para adicionar os níveis dos fatores
write.csv(x=v2020_n %>% colnames()%>% t(), file="clean_vig2020.csv")

levels_2020 <- read_excel("levels_vig2020a.xlsx")  %>% mutate(across(.cols=everything(), as.factor))

#Transforma em fator as varíaveis que são fatores
v2020<- as_tibble(v2020) %>% mutate(across(.cols=which(colnames(v2020)  %in% colnames(levels_2020)),
                          ~as.factor(.)))


#Corrige o nome dos fatores
levels_2020<-pivot_longer(levels_2020, 
                 cols=everything(), values_to= "labels") %>% drop_na()


v2020 <- v2020 %>% 
  pivot_longer(cols=everything()) %>% 
  drop_na() %>% 
  left_join(levels_2020) %>% 
  mutate(labels= coalesce(labels, value)) %>% 
  select(-value) %>% 
  group_by(name) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from = "name", values_from = "labels") %>%
  select(-row) %>% 
  filter(!is.na(chave))
  
