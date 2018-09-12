# Juntando geral com dados financeiros.
library(dplyr)
library(data.table)
library(janitor)
library(tidyr)
library(scales)
library(knitr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(googledrive)
library(stringr)

#obras, já estou pegando o arquivo final
load("situacao_todas_obras.Rdata")

#dados da raspagem
load("arquivos_simec_fin_v2.RData")

# tenho informações sobre 12695 das 14432
# 0.88 % das obras receberam algum repasse

#Vou acertar o repasse com a inflação:

ipca <- fread("bcdata.sgs.4447.csv")

ipca1 <- ipca %>%
  mutate(data = as.Date(data, "%d/%m/%Y"),
         mes_ano = format(data, "%m/%Y"),
         valor = gsub(",", ".", valor),    #substituindo os decimais
         valor = as.numeric(valor)) %>%    
  filter(data > "2007-01-01") %>%
  mutate(indice = cumprod(1+valor/100),    #funciona como juros compostos
         indice_max = last(indice),
         indice = indice/indice_max)       #ajeita o valor da multiplicação dependendo do ano


repasses <- simec_fin2 %>%
  mutate(mes_ano = format(data_de_pagamento, "%m/%Y")) %>%
  left_join(ipca1, by=c("mes_ano")) %>%
  mutate(pagamento_cte_ago2018 = round(valor_do_pagamento/indice, 0))   #ajusta

save(repasses, file="repasses_desagregados.Rdata")

# Fazendo um arquivo agregado contendo só informações finais

repasses_agr <- repasses %>%
  group_by(id) %>%
  summarise(pagto_total_cte_ago2018 = sum(pagamento_cte_ago2018),
            data_primeiro = min(data_de_pagamento),    #primeira data registrada do repasse
            data_ultimo_repasse = max(data_de_pagamento),      #última data registrada do repasse
            qtd_repasses = n())   

#corrigindo o valor pactuado com o FNDE, vou tratar sempre como junho do ano:

pacto <- geral %>%
  select(id, valor_pactuado_com_o_fnde, ano_convenio) %>%
  mutate(mes_ano = paste("06", ano_convenio, sep="/"),
         valor_fnde = as.numeric(valor_pactuado_com_o_fnde)) %>%
  left_join(ipca1, by=c("mes_ano")) %>%
  mutate(pactuado_cte_ago2018 = round(valor_fnde/indice, 0)) %>%
  select(id, ano_convenio, pactuado_cte_ago2018)
  
sit_obras_final <- geral %>%
  left_join(repasses_agr, by=c("id")) %>%
  mutate(qtd_repasses = ifelse(is.na(qtd_repasses), 0, qtd_repasses)) %>%
  select(-c(valor_pactuado_com_o_fnde, ano_convenio)) %>%
  left_join(pacto, by = c("id")) %>%
  mutate_all(as.character)
  
save(sit_obras_final, file="sit_obras_final.Rdata")
write.csv(sit_obras_final , file="sit_obras_final.csv", sep=";", quote = TRUE,
          row.names = FALSE)

drive_find(n_max=10) #autenticação

lista_final_sheet <- drive_upload(
  "sit_obras_final.csv",
  path="~/TB/Tadepé/Transparência Brasil - Tá de Pé 2018",
  name = "sit_obras_final_v14",
  type = "spreadsheet")
