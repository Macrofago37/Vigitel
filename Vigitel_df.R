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

v2020 <- read_excel("Vigitel-2020-peso-rake.xls") 


#Wrangling
v2020nn <-v2020 %>% rename(idade=q6, sexo=q7) %>%  mutate(cidade= as.factor(cidade), cidade=recode(cidade,
                                                                                     "1"="aracaju",
                                                                                     "2"="belem",
                                                                                     "3"="BH",
                                                                                     "4"="boa_vista",
                                                                                     "5"="campo_grande",
                                                                                     "6"="cuiaba",
                                                                                     "7"="curitiba",
                                                                                     "8"="florianopolis",
                                                                                     "9"="fortaleza",
                                                                                     "10"="goiania",
                                                                                     "11"="joao_pessoa",
                                                                                     "12"="macapa",
                                                                                     "13"="maceio",
                                                                                     "14"="manaus",
                                                                                     "15"= "natal",
                                                                                     "16"= "palmas",
                                                                                     "17"= "porto_alegre",
                                                                                     "18"= "porto_velho",
                                                                                     "19"= "recife",
                                                                                     "20"= "rio_branco",
                                                                                     "21"="rio_de_janeiro",
                                                                                     "22"= "salvador",
                                                                                     "23"= "sao_luis",
                                                                                     "24"= "sao_paulo",
                                                                                     "25"= "teresina",
                                                                                     "26"= "vitoria",
                                                                                     "27"= "distrito_federal"),
                                                          sexo=sexo %>% recode(.,"1"="M",
                                                                               "2"="F"))
  
                                                                  
