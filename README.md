# Estratificação do risco de doença nos estados do Brasil 
Base de dados: Vigitel 2020


Pacotes utilizados:

```r
x <- c("readxl", "tidyverse", "leaflet", "caret", "scales", "broom", "knitr", "kableExtra", "rgdal")
(function(x){
  sapply(x, function(x) if(!x %in% installed.packages()){
    install.packages(x, dependencies = T)
  })
  sapply(x, library, character.only=T)
})(x)
```



Download dos dados dos VIGITEL


Carrega os dados de 2020 e Helper

```r
v2020<- read_excel("Vigitel-2020-peso-rake.xls")

Helper_2020 <-  read_excel("Dicionario-de-dados-Vigitel 2020.xls", 
                           skip = 2)
```
Ajusta os dados de interesse

```r
# Escreve tabela para adicionar o nome das colunas manualmente
# write_csv(x=tibble(colnames=v2020 %>% colnames()),
#         file="vig2020.csv")


#Carega as colunas para serem mantidas e os nomes corretos
cols_keep <- read_delim("vig2020.csv", delim = ";", 
           escape_double = FALSE, trim_ws = TRUE)%>% drop_na()
```

```
## New names:
## * `` -> ...2
```

```
## Rows: 205 Columns: 2
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ";"
## chr (2): colnames, ...2
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
#Ajusta o nome das colunas e seleciona as colunas que serão mantidas no DF original
#Alem disso troca os valores 777 e 888 por NA
v2020 <- v2020 %>% select(cols_keep$colnames) %>%
  rename_with(~cols_keep[[2]],
              .cols=cols_keep$colnames) %>%
  sapply(., function(x) replace(x, x %in% c("777", "888"), NA))

  
# Cria tabela para adicionar os níveis dos fatores
# write.csv(x=v2020_n %>% colnames()%>% t(), file="clean_vig2020.csv")

levels_2020 <- read_excel("levels_vig2020ab.xlsx")  %>% mutate(across(.cols=everything(), as.factor))


#Transforma variáveis character em númericas
v2020<- as_tibble(v2020) %>% 
  mutate(across(which(!(colnames(v2020)  %in% colnames(levels_2020))),
                ~as.numeric(.)))
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduzidos por coerção
```

```r
#Transforma em fator as varíaveis que são fatores
v2020<- v2020 %>% mutate(across(.cols=which(colnames(v2020)  %in% colnames(levels_2020)),
                          ~as.factor(.)))

#Corrige o nome dos fatores
v2020 <- (function(x, y){
  colfactors <- y %>% select(
  match(colnames(x), colnames(.)))
  
  leng <- seq(1, ncol(x))
  
  map2(.x=levels_2020, .y=leng, .f=function(x,y){
  tibble(x[colfactors[[y]]]) 
})
})(levels_2020, v2020) %>%
  as.data.frame() %>% 
  rename_with(~colnames(levels_2020)) %>% 
  mutate(chave=v2020$chave) %>% 
  left_join( v2020 %>% select(
  setdiff(colnames(.), colnames(levels_2020))),
  by="chave")
  




#Adc estado
Estado_capital <- read_excel("Estado-capital.xlsx") %>% mutate(cidade=tolower(cidade),
                                                               estado=toupper(estado))
v2020 <- v2020 %>% left_join(Estado_capital, by="cidade")
```

Calcula novas variáveis:
- IMC
- Classificação do IMC
- Nível de atividade física (AF) em minutos
- Tempo em tela (Screen time), que será considerado como tempo em comportamento sedentário


```r
#Parse numbers of observations and average
Parse_avg <- function(x){
  as.data.frame(str_extract_all(x, "(\\d)+", simplify = T)) %>% 
  map_df(as.numeric) %>% 
  mutate(V2= coalesce(V2, V1)) %>% 
  rowMeans() 
}



#IMC e classificação
v2020<-v2020%>% mutate(IMC=Massa_corporal/(Altura/100)^2,
                 #Cut is right opened, therefore 25 will be overweight
                 IMC_classif= cut(IMC,
                                  breaks=c(0, 18.5, 24.9, 29.9, 34.9, 35.9, Inf),
                                  labels=c("Baixo peso", "Peso normal", "Sobrepeso", "Obesidade grau I", "Obesidade grau II", "Obesidade grau III")),
                 #Parse numbers (p) of weekly frequency and average 
                 p_freq_AF_sem= as.character(Freq_AF_semanal) %>%
                                 replace(., grepl("todos os dias",., fixed = TRUE), "7"),
                 p_freq_AF_sem= ifelse(`Praticou AF`=="sim", Parse_avg(Freq_AF_semanal), NA),
                 p_Dur_AF= ifelse(`Praticou AF`=="sim",Parse_avg(Duracao_AF), NA),
                 Dur_AF_sem= p_freq_AF_sem * p_Dur_AF,
                 #Assumindo 5 dias por semana
                 Dur_AF_locomocao= ifelse(is.na(tempo_AF_locomocao_trabalho),0, Parse_avg(tempo_AF_locomocao_trabalho))*5 + 
                   ifelse(is.na(tempo_AF_locomocao_curso),0, Parse_avg(tempo_AF_locomocao_curso))*5,
                 #Faxina_dur_semana
                 Dur_faxina_sem= Freq_faxina_semanal * 
                   (ifelse(is.na(Duracao_faxina_horas), 0, Duracao_faxina_horas)*60 +
                      ifelse(is.na(Duracao_faxina_minutos),0, Duracao_faxina_minutos)),
                 #SDaily screen time
                 screen_time_day= as.numeric(Parse_avg(Tempo_TV) %>% replace_na("0")) +
                   as.numeric(Parse_avg(Tempo_comp_cel_tab) %>% replace_na("0"))
                 #Alcohol 
                 )
```
# Estratificação de risco

O peso de cada preditor é baseado na premissa de uma relação linear entre ele e o desfecho. O desfecho é a hipertensão

Esse método de análise foi baseado em:
1.Zhang, Z., Zhang, H. & Khanal, M. K. Development of scoring system for risk stratification in clinical medicine: a step-by-step tutorial. Ann. Transl. Med. 5, 436–436 (2017).

##Pontos de corte e fatores de risco utilizados

### Screen time: 
Ekelund. 2016. Does physical activity attenuate, or even eliminate, the detrimental association of sitting time with mortality? A harmonised meta-analysis of data from more than 1 million men and women
- >8h
- >4
- <4


```r
v2020 <- v2020 %>% mutate(
  Screen_time_classif= cut(screen_time_day,
                           breaks= c(0, 4, 8, Inf),
                           labels= c("Baixo", "Moderado", "Alto"), right=FALSE)# 4 será considerado moderado
)
```


### Atividade física
Apenas o tempo de AF de lazer será considerado, pois AF no trabalho não parece reduzir tanto o risco de doenças:
Holtermann, A., Schnohr, P., Nordestgaard, B. G. & Marott, J. L. The physical activity paradox in cardiovascular disease and all-cause mortality: the contemporary Copenhagen General Population Study with 104 046 adults. European Heart Journal 42, 1499–1511 (2021).
AF no deslocamento também não:
Pitanga, F. J. G., Matos, S. M. A., Almeida, M. da C., Barreto, S. M. & Aquino, E. M. L. Leisure-Time Physical Activity, but not Commuting Physical Activity, is Associated with Cardiovascular Risk among ELSA-Brasil Participants. Arquivos Brasileiros de Cardiologia (2017) doi:10.5935/abc.20170178.

A prática de mais de 420 minutos de AF por semana parece combater o risco de comportamento sedentário
Holtermann, A., Schnohr, P., Nordestgaard, B. G. & Marott, J. L. The physical activity paradox in cardiovascular disease and all-cause mortality: the contemporary Copenhagen General Population Study with 104 046 adults. European Heart Journal 42, 1499–1511 (2021).

Pratica de Af:
- >150 
- >420

```r
v2020 <- v2020 %>% mutate(
  AF_classif= cut(Dur_AF_sem,
                           breaks= c(0, 150, 420, Inf),
                           labels= c("Inativo", "Ativo", "Super_ativo"), right=FALSE))
```


### IMC
Aune, D. et al. BMI and all cause mortality: systematic review and non-linear dose-response meta-analysis of 230 cohort studies with 3.74 million deaths among 30.3 million participants. BMJ i2156 (2016) doi:10.1136/bmj.i2156.
Portanto será utilizada a classificaçao do IMC


### Idade
Ngufor, C. et al. Development and Validation of a Risk Stratification Model Using Disease Severity Hierarchy for Mortality or Major Cardiovascular Event. JAMA Netw Open 3, e208270 (2020).

- >45 & <60
- >60 & <75
- >75

```r
v2020 <- v2020 %>% mutate(
  Idade_classif= cut(Idade,
                           breaks= c(-Inf, 45, 60, 75, Inf),
                           labels= c("Jovem", "Old_I", "Old_I", "Old_III"), right=FALSE))
```


### Tabaco
Será considerado se é fumante passivo em casa (será desconsiderado no trabalho, pois é proibido fumar em locais fechados e há uma grande chance de que ao não fumar, a pessoa não terá contato direto com os fumantes em quanto eles fumam), se já fumou e se ainda fuma e se fuma
- <5 cigarros/dia
- >5 & <20 cigarros/dia
- >20 cigarros/dia

```r
v2020 <- v2020 %>% mutate(
  cigarro_classif= cut(Cigarros_dias,
                           breaks= c(0, 5, 20, Inf),
                           labels= c("Baixo", "Moderado", "Alto"), right=FALSE))
```

### Consumo de álcool
Frequência semanal

## Modelo estatístico 
- Regressão logística


```r
Var_x <- c("Sexo", "Freq_bebida_alcoolica", "Fumante", "Ex_fumante", "Algum_fumante_em_casa", "cor_raca", "Aval_saude", "Diabetes", "Depressao", "IMC_classif", "Screen_time_classif", "AF_classif", "Idade_classif", "cigarro_classif")

Formula <-paste("y ~ ", 
                paste(Var_x, collapse = "+"), 
                sep="") %>% as.formula()

#NAs precisam ser removidos para a regressão logística

v2020x <- map_df(v2020 %>% select(Var_x), as.character) %>% 
  mutate(across(everything(), ~replace_na(., 0)))
```

```
## Note: Using an external vector in selections is ambiguous.
## ℹ Use `all_of(Var_x)` instead of `Var_x` to silence this message.
## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
v2020x$y <- as.numeric(v2020$Pressao_alta=="sim")

Fit<- v2020x %>% 
  group_by(across(Var_x)) %>% 
  glm(Formula, data=., family="binomial")

Fit %>% summary()
```

```
## 
## Call:
## glm(formula = Formula, family = "binomial", data = .)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4714  -0.8570  -0.4032   0.9367   2.7392  
## 
## Coefficients: (1 not defined because of singularities)
##                                                                   Estimate
## (Intercept)                                                       0.300194
## Sexomasculino                                                    -0.095501
## Freq_bebida_alcoolica1 a 2 dias por semana                       -0.056398
## Freq_bebida_alcoolica3 a 4 dias por semana                        0.041114
## Freq_bebida_alcoolica5 a 6 dias por semana                       -0.273099
## Freq_bebida_alcoolicamenos de 1 dia por mês                      -0.144825
## Freq_bebida_alcoolicamenos de 1 dia por semana                   -0.203418
## Freq_bebida_alcoolicatodos os dias ( inclusive sábado e domingo)  0.064628
## Fumantesim, diariamente                                          -0.309691
## Fumantesim, mas não diariamente                                  -0.258650
## Ex_fumantenão                                                    -0.183765
## Ex_fumantesim, diariamente                                        0.110506
## Ex_fumantesim, mas não diariamente                                      NA
## Algum_fumante_em_casanao                                         -0.213109
## Algum_fumante_em_casasim                                         -0.192726
## cor_racaamarela                                                  -0.176895
## cor_racabranca                                                   -0.023555
## cor_racaindígena                                                 -0.037824
## cor_racaoutra                                                     0.273848
## cor_racaparda                                                    -0.046161
## cor_racapreta                                                     0.205657
## Aval_saudebom                                                    -0.236244
## Aval_saudemuito bom                                              -0.698181
## Aval_saudemuito ruim                                              0.319396
## Aval_sauderegular                                                 0.270797
## Aval_sauderuim                                                    0.279939
## Diabetesnao                                                      -1.844448
## Diabetessim                                                      -0.772836
## Depressaonao_lembra                                               0.144979
## Depressaosim                                                      0.209873
## IMC_classifBaixo peso                                            -0.882181
## IMC_classifObesidade grau I                                       0.494045
## IMC_classifObesidade grau II                                      0.711638
## IMC_classifObesidade grau III                                     0.882824
## IMC_classifPeso normal                                           -0.552544
## IMC_classifSobrepeso                                              0.045878
## Screen_time_classifBaixo                                         -0.016832
## Screen_time_classifModerado                                      -0.012993
## AF_classifAtivo                                                  -0.051787
## AF_classifInativo                                                -0.044766
## Idade_classifOld_I                                                1.735136
## Idade_classifOld_III                                              2.277040
## cigarro_classifAlto                                               0.149321
## cigarro_classifBaixo                                             -0.003588
## cigarro_classifModerado                                          -0.002618
##                                                                  Std. Error
## (Intercept)                                                        0.873329
## Sexomasculino                                                      0.033189
## Freq_bebida_alcoolica1 a 2 dias por semana                         0.041766
## Freq_bebida_alcoolica3 a 4 dias por semana                         0.082408
## Freq_bebida_alcoolica5 a 6 dias por semana                         0.188027
## Freq_bebida_alcoolicamenos de 1 dia por mês                        0.058609
## Freq_bebida_alcoolicamenos de 1 dia por semana                     0.062254
## Freq_bebida_alcoolicatodos os dias ( inclusive sábado e domingo)   0.126943
## Fumantesim, diariamente                                            0.271085
## Fumantesim, mas não diariamente                                    0.138958
## Ex_fumantenão                                                      0.054331
## Ex_fumantesim, diariamente                                         0.060330
## Ex_fumantesim, mas não diariamente                                       NA
## Algum_fumante_em_casanao                                           0.043642
## Algum_fumante_em_casasim                                           0.074290
## cor_racaamarela                                                    0.215458
## cor_racabranca                                                     0.151238
## cor_racaindígena                                                   0.193010
## cor_racaoutra                                                      0.159140
## cor_racaparda                                                      0.151571
## cor_racapreta                                                      0.158725
## Aval_saudebom                                                      0.175855
## Aval_saudemuito bom                                                0.179095
## Aval_saudemuito ruim                                               0.224068
## Aval_sauderegular                                                  0.176324
## Aval_sauderuim                                                     0.189689
## Diabetesnao                                                        0.839720
## Diabetessim                                                        0.840557
## Depressaonao_lembra                                                0.463970
## Depressaosim                                                       0.044523
## IMC_classifBaixo peso                                              0.132130
## IMC_classifObesidade grau I                                        0.059796
## IMC_classifObesidade grau II                                       0.130974
## IMC_classifObesidade grau III                                      0.085594
## IMC_classifPeso normal                                             0.053651
## IMC_classifSobrepeso                                               0.052683
## Screen_time_classifBaixo                                           0.067709
## Screen_time_classifModerado                                        0.068764
## AF_classifAtivo                                                    0.035193
## AF_classifInativo                                                  0.040752
## Idade_classifOld_I                                                 0.042246
## Idade_classifOld_III                                               0.056122
## cigarro_classifAlto                                                0.289085
## cigarro_classifBaixo                                               0.315203
## cigarro_classifModerado                                            0.279743
##                                                                  z value
## (Intercept)                                                        0.344
## Sexomasculino                                                     -2.877
## Freq_bebida_alcoolica1 a 2 dias por semana                        -1.350
## Freq_bebida_alcoolica3 a 4 dias por semana                         0.499
## Freq_bebida_alcoolica5 a 6 dias por semana                        -1.452
## Freq_bebida_alcoolicamenos de 1 dia por mês                       -2.471
## Freq_bebida_alcoolicamenos de 1 dia por semana                    -3.268
## Freq_bebida_alcoolicatodos os dias ( inclusive sábado e domingo)   0.509
## Fumantesim, diariamente                                           -1.142
## Fumantesim, mas não diariamente                                   -1.861
## Ex_fumantenão                                                     -3.382
## Ex_fumantesim, diariamente                                         1.832
## Ex_fumantesim, mas não diariamente                                    NA
## Algum_fumante_em_casanao                                          -4.883
## Algum_fumante_em_casasim                                          -2.594
## cor_racaamarela                                                   -0.821
## cor_racabranca                                                    -0.156
## cor_racaindígena                                                  -0.196
## cor_racaoutra                                                      1.721
## cor_racaparda                                                     -0.305
## cor_racapreta                                                      1.296
## Aval_saudebom                                                     -1.343
## Aval_saudemuito bom                                               -3.898
## Aval_saudemuito ruim                                               1.425
## Aval_sauderegular                                                  1.536
## Aval_sauderuim                                                     1.476
## Diabetesnao                                                       -2.197
## Diabetessim                                                       -0.919
## Depressaonao_lembra                                                0.312
## Depressaosim                                                       4.714
## IMC_classifBaixo peso                                             -6.677
## IMC_classifObesidade grau I                                        8.262
## IMC_classifObesidade grau II                                       5.433
## IMC_classifObesidade grau III                                     10.314
## IMC_classifPeso normal                                           -10.299
## IMC_classifSobrepeso                                               0.871
## Screen_time_classifBaixo                                          -0.249
## Screen_time_classifModerado                                       -0.189
## AF_classifAtivo                                                   -1.472
## AF_classifInativo                                                 -1.098
## Idade_classifOld_I                                                41.072
## Idade_classifOld_III                                              40.573
## cigarro_classifAlto                                                0.517
## cigarro_classifBaixo                                              -0.011
## cigarro_classifModerado                                           -0.009
##                                                                  Pr(>|z|)    
## (Intercept)                                                      0.731045    
## Sexomasculino                                                    0.004008 ** 
## Freq_bebida_alcoolica1 a 2 dias por semana                       0.176907    
## Freq_bebida_alcoolica3 a 4 dias por semana                       0.617847    
## Freq_bebida_alcoolica5 a 6 dias por semana                       0.146377    
## Freq_bebida_alcoolicamenos de 1 dia por mês                      0.013473 *  
## Freq_bebida_alcoolicamenos de 1 dia por semana                   0.001085 ** 
## Freq_bebida_alcoolicatodos os dias ( inclusive sábado e domingo) 0.610674    
## Fumantesim, diariamente                                          0.253282    
## Fumantesim, mas não diariamente                                  0.062695 .  
## Ex_fumantenão                                                    0.000719 ***
## Ex_fumantesim, diariamente                                       0.066997 .  
## Ex_fumantesim, mas não diariamente                                     NA    
## Algum_fumante_em_casanao                                         1.04e-06 ***
## Algum_fumante_em_casasim                                         0.009480 ** 
## cor_racaamarela                                                  0.411638    
## cor_racabranca                                                   0.876233    
## cor_racaindígena                                                 0.844633    
## cor_racaoutra                                                    0.085288 .  
## cor_racaparda                                                    0.760707    
## cor_racapreta                                                    0.195084    
## Aval_saudebom                                                    0.179140    
## Aval_saudemuito bom                                              9.68e-05 ***
## Aval_saudemuito ruim                                             0.154030    
## Aval_sauderegular                                                0.124589    
## Aval_sauderuim                                                   0.140004    
## Diabetesnao                                                      0.028056 *  
## Diabetessim                                                      0.357869    
## Depressaonao_lembra                                              0.754680    
## Depressaosim                                                     2.43e-06 ***
## IMC_classifBaixo peso                                            2.45e-11 ***
## IMC_classifObesidade grau I                                       < 2e-16 ***
## IMC_classifObesidade grau II                                     5.53e-08 ***
## IMC_classifObesidade grau III                                     < 2e-16 ***
## IMC_classifPeso normal                                            < 2e-16 ***
## IMC_classifSobrepeso                                             0.383845    
## Screen_time_classifBaixo                                         0.803681    
## Screen_time_classifModerado                                      0.850128    
## AF_classifAtivo                                                  0.141149    
## AF_classifInativo                                                0.271991    
## Idade_classifOld_I                                                < 2e-16 ***
## Idade_classifOld_III                                              < 2e-16 ***
## cigarro_classifAlto                                              0.605486    
## cigarro_classifBaixo                                             0.990917    
## cigarro_classifModerado                                          0.992533    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 35566  on 27056  degrees of freedom
## Residual deviance: 28043  on 27013  degrees of freedom
##   (20 observations deleted due to missingness)
## AIC: 28131
## 
## Number of Fisher Scoring iterations: 5
```

```r
#Avaliando o modelo
p_hat_logit <- predict(Fit, newdata=v2020x, type="response")
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
## prediction from a rank-deficient fit may be misleading
```

```r
y_hat_logit <- ifelse(p_hat_logit > 0.5, "1", "0") %>% factor
confusionMatrix(y_hat_logit, as.factor(v2020x$y))#$overall[["Accuracy"]]
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction     0     1
##          0 14294  4431
##          1  2838  5494
##                                          
##                Accuracy : 0.7313         
##                  95% CI : (0.726, 0.7366)
##     No Information Rate : 0.6332         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.4014         
##                                          
##  Mcnemar's Test P-Value : < 2.2e-16      
##                                          
##             Sensitivity : 0.8343         
##             Specificity : 0.5536         
##          Pos Pred Value : 0.7634         
##          Neg Pred Value : 0.6594         
##              Prevalence : 0.6332         
##          Detection Rate : 0.5283         
##    Detection Prevalence : 0.6921         
##       Balanced Accuracy : 0.6939         
##                                          
##        'Positive' Class : 0              
## 
```
Os coeficientes de cada variável serão extraídos e utilizados como score para stratificação de risco

```r
Estimates <- (function(x, y){
  a <- map_df(x, function(x){
    a <- str_split_fixed(y, x, 2) %>% as_tibble()
    a %>% mutate(V1=  ifelse(grepl(x, y), x, V1)) 
  }
    )})(x=colnames(v2020x), y=tidy(Fit)$term) %>% 
  na_if("") %>%
  drop_na(V2) %>% 
  cbind(estimate=tidy(Fit)$estimate[-1]) %>% 
  drop_na(estimate)
```

```
## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
## Using compatibility `.name_repair`.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

```r
#Calcula o risco levando em conta os valores da fórmula da glm
v2020$risk_index <- apply(v2020x, 1, function(x){
  row_index <- v2020x[x,]
  logi <- Estimates$V1 %in% colnames(row_index) & Estimates$V2 %in% t(x)
  sum(Estimates$estimate[logi])
})

#Normaliza os valores entre 0 e 10 e clasifica em Baixo, Moderado, Alto ou Muito alto
v2020 <- v2020 %>% mutate(
  Risk_classif= cut(risk_index, breaks=c(-Inf, quantile(.$risk_index, probs=c(.25, .50, .75)), Inf),
                    labels= c("Baixo", "Moderado", "Alto", "Muito_alto"), right=FALSE),
  risk_index= scales::rescale(risk_index, to=c(0,10)))
```
### Risco da população em cada estado

```r
Risco_resumo <- v2020 %>%
  group_by(estado, Risk_classif) %>%
  summarise(Risco=n()) %>% 
  mutate(Risco= round((Risco/sum(Risco))*100, 2),
         estado=str_to_title(estado),
         Risk_classif= gsub("_", " ", x=Risk_classif, perl=T ))
```

```
## `summarise()` has grouped output by 'estado'. You can override using the `.groups` argument.
```

```r
Risco_resumo %>% kable() %>% kable_styling(full_width = TRUE,
                                           stripe_color = "azure",
                                           bootstrap_options = "striped")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> estado </th>
   <th style="text-align:left;"> Risk_classif </th>
   <th style="text-align:right;"> Risco </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Acre </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 23.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acre </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 29.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acre </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acre </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 21.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alagoas </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 30.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alagoas </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 22.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alagoas </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 22.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alagoas </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 24.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amapá </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 20.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amapá </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amapá </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 29.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amapá </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 26.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amazonas </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 16.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amazonas </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amazonas </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 29.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Amazonas </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 31.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bahia </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 39.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bahia </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 27.72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bahia </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 17.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bahia </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 15.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ceará </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 16.63 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ceará </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 29.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ceará </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 27.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ceará </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 26.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Distrito Federal </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 22.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Distrito Federal </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 26.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Distrito Federal </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 26.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Distrito Federal </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 25.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Espiríto Santo </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 22.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Espiríto Santo </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 28.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Espiríto Santo </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 24.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Espiríto Santo </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 24.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Goiás </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 27.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Goiás </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 25.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Goiás </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 24.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Goiás </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 23.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maranhão </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 19.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maranhão </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maranhão </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 30.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maranhão </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 27.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 30.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 19.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 25.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso Do Sul </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 25.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso Do Sul </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso Do Sul </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mato Grosso Do Sul </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 24.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Minas Gerais </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 25.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Minas Gerais </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 22.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Minas Gerais </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Minas Gerais </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 26.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pará </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 19.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pará </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 26.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pará </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 27.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pará </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 25.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraíba </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 19.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraíba </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraíba </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraíba </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 31.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraná </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 23.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraná </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraná </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 26.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Paraná </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 25.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pernambuco </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 17.61 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pernambuco </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pernambuco </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pernambuco </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 32.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Piauí </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 21.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Piauí </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Piauí </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 27.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Piauí </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 27.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio De Janeiro </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 23.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio De Janeiro </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 28.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio De Janeiro </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio De Janeiro </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 23.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Norte </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 22.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Norte </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Norte </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Norte </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 29.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Sul </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 22.92 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Sul </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Sul </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 27.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rio Grande Do Sul </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 24.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rondônia </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 28.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rondônia </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rondônia </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rondônia </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 22.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Roraima </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 37.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Roraima </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 26.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Roraima </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 18.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Roraima </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 17.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa Catarina </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 34.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa Catarina </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 25.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa Catarina </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 19.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Santa Catarina </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 20.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> São Paulo </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 23.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> São Paulo </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 23.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> São Paulo </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> São Paulo </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 27.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sergipe </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 26.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sergipe </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 22.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sergipe </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 25.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sergipe </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 25.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tocantins </td>
   <td style="text-align:left;"> Baixo </td>
   <td style="text-align:right;"> 34.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tocantins </td>
   <td style="text-align:left;"> Moderado </td>
   <td style="text-align:right;"> 24.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tocantins </td>
   <td style="text-align:left;"> Alto </td>
   <td style="text-align:right;"> 20.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tocantins </td>
   <td style="text-align:left;"> Muito alto </td>
   <td style="text-align:right;"> 20.58 </td>
  </tr>
</tbody>
</table>
