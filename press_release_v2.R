# Versão atualizada do press release

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

`%notin%` = function(x,y) !(x %in% y)

perc <- function(x) { 
  paste0(round(x,2)*100, "%")
}

setwd("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018")

#Base de obras do FNDE (04/09/2018)
# obras <- read.csv(url("http://simec.mec.gov.br/painelObras/download.php"), sep=";")
# save(obras , file="obras04092018.Rdata")
load("obras04092018.Rdata")

obras <- obras %>%
  mutate_all(as.character) %>%
  clean_names()

# Ids de controle para avaliacao de imapcto
load("ids_controle_final.Rdata") #objeto id_controle_campanha_final

not_project<- c("COBERTURA DE QUADRA ESCOLAR - PROJETO PRÓPRIO",
                "COBERTURA DE QUADRA ESCOLAR GRANDE - PROJETO FNDE",
                "COBERTURA DE QUADRA ESCOLAR PEQUENA - PROJETO FNDE",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO ",
                "QUADRA ESCOLAR COBERTA COM VESTIÁRIO- PROJETO FNDE",
                "Reforma",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO",
                "Ampliação",
                "QUADRA ESCOLAR COBERTA COM PALCO- PROJETO FNDE",
                "Quadra Escolar Coberta e Vestiário - Modelo 2",
                "Ampliação Módulo Tipo B", 
                "")

#  Tempo de execução dos projetos-padrão:

tempo_projeto <- data.frame(tipo_do_projeto = c("Escola de Educação Infantil Tipo B",
                                                "Escola de Educação Infantil Tipo C",
                                                "MI - Escola de Educação Infantil Tipo B",
                                                "MI - Escola de Educação Infantil Tipo C",
                                                "Espaço Educativo - 12 Salas",
                                                "Espaço Educativo - 01 Sala",
                                                "Espaço Educativo - 02 Salas",
                                                "Espaço Educativo - 04 Salas",
                                                "Espaço Educativo - 06 Salas",
                                                "Projeto 1 Convencional",
                                                "Projeto 2 Convencional",
                                                "Construção",
                                                "Escola com projeto elaborado pelo concedente",
                                                "Escola com Projeto elaborado pelo proponente",
                                                "Espaço Educativo - 08 Salas",
                                                "Espaço Educativo Ensino Médio Profissionalizante"),
                            tempo_execucao_dias = c(270,180,180,120,390,150,150,210,210,
                                                    330,270,720,720,720,720,720))

# Tempo de execução das obras estaduais que temos o cronograma
execucao_cronogramas_lai <- fread("tempo_obra.csv")
execucao_cronogramas_lai <- execucao_cronogramas_lai  %>%
  mutate(project_id = as.character(project_id),
         tempo_obra_dias = tempo_obra*30) %>%
  rename(tempo_obra_dias_via_lai = tempo_obra_dias) %>%
  select(1,3)

#obras do obra transparente:
load("ot.Rdata")

ot1 <- ot %>%
  mutate_all(as.character) %>%
  mutate(projeto_obra_transparente = 1)  %>%
  select(id_obra, projeto_obra_transparente )

# Objeto geral:
geral <- obras %>%
  filter(!id %in% id_controle_campanha_final,
         tipo_do_projeto %notin% not_project) %>% #apenas constru de esc e creches
  rename(responsabilidade = rede_de_ensino_publico) %>%
  left_join(execucao_cronogramas_lai, by=c("id" = "project_id")) %>% #infos que pegamos via lai
  left_join(tempo_projeto, by=c("tipo_do_projeto")) %>%      #tempo que o projeto padrão dura
  mutate(data_de_assinatura_do_contrato = as.Date(data_de_assinatura_do_contrato, format="%Y-%m-%d %H:%M:%S"),
         data_prevista_de_conclusao_da_obra= as.Date(data_prevista_de_conclusao_da_obra, format="%d/%m/%Y"),
         final_previsto = if_else(!is.na(tempo_obra_dias_via_lai), tempo_obra_dias_via_lai + data_de_assinatura_do_contrato, 
                                  if_else(!is.na(data_prevista_de_conclusao_da_obra), data_prevista_de_conclusao_da_obra, 
                                          data_de_assinatura_do_contrato+tempo_execucao_dias)),
         nao_iniciada = ifelse( percentual_de_execucao == 0 & situacao %notin%
                                  c("Inacabada","Paralisada", "Obra Cancelada", "Concluída" ), 1, 0),
         paralisada = if_else(!is.na(data_de_assinatura_do_contrato) & situacao != "Execução" & nao_iniciada == 0|
                                  percentual_de_execucao > 0 & situacao != "Execução"  & nao_iniciada == 0| 
                                  !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Licitação" & nao_iniciada == 0|
                                  !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Em Reformulação" & nao_iniciada == 0 |
                                  !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Contratação" & nao_iniciada == 0|
                                  !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Planejamento pelo proponente" & nao_iniciada == 0,  
                                1 , 0),
         paralisada = ifelse(situacao %in% c("Inacabada","Paralisada"), 1, paralisada),
         paralisada = ifelse(situacao %in% c("Obra Cancelada", "Concluída"), 0, paralisada),#retirando concluidas e canceladas
         concluida = ifelse(situacao == "Concluída", 1, 0),
         cancelada = ifelse(situacao == "Obra Cancelada", 1, 0),
         atrasada = if_else(final_previsto < "2018-09-04" & situacao %notin% c("Concluída","Obra Cancelada"), 
                            1, 0),
         atrasada = if_else(is.na(final_previsto), 0, atrasada),
         execucao = if_else(situacao == "Execução" & nao_iniciada == 0 , 1, 0),
         responsabilidade = as.character(responsabilidade),
         responsabilidade = if_else(id == "1063221" | id == "29054", 
                                    "Municipal", responsabilidade),
         logradouro = tolower(logradouro),
         logradouro = str_trim(logradouro), # retirar espaços no fim
         logradouro = ifelse(logradouro=="", NA, logradouro),
         sem_end = if_else(is.na(logradouro), 1, 0), #obras que não tÊm endereço.
         problema_detectado = ifelse(paralisada == 1 & sem_end == 1, #obras com problemas
                                     "paralisada; sem endereço",
                                     if_else(paralisada == 1 & atrasada == 1, "paralisada; atrasada",
                                             ifelse(paralisada == 1 & atrasada == 0, "paralisada",
                                                    ifelse(atrasada == 1 & sem_end == 1, "atrasada; sem endereço",
                                                           ifelse(atrasada == 1 & paralisada == 0, "atrasada",
                                                                  ifelse(sem_end == 1, "sem endereço",
                                                                         NA))))))) %>%
  mutate(status = ifelse(paralisada == 1, "paralisada",
                         ifelse(cancelada == 1, "cancelada",
                                ifelse(nao_iniciada == 1, "não iniciada",
                                       ifelse(concluida == 1, "concluida",
                                              ifelse(execucao == 1, "execucao", "ERROOOOOOO"))))),
    situacao_segundo_tbrasil = ifelse(paralisada == 1 & atrasada == 0, "paralisada",
                                         ifelse(paralisada == 1 & atrasada == 1, "paralisada e já devia ter sido entregue",
                                                ifelse(execucao == 1 & atrasada == 1, "em andamento e já devia ter sido entregue",
                                                       ifelse(execucao == 1 & atrasada == 0, "em andamento",
                                                              ifelse(concluida == 1, "obra concluída",
                                                                     ifelse(cancelada == 1, "obra cancelada",
                                                                            ifelse(nao_iniciada == 1 & atrasada == 0, "não iniciada",
                                                                                   ifelse(nao_iniciada == 1 & atrasada == 1, 
                                                                                          "não iniciada e já devia ter sido entregue", "ERROOOOOO")))))))),
    logradouro = ifelse(is.na(logradouro), "Não informado", logradouro),
    ano_convenio = str_sub(termo_convenio, start= -4),
    ano_fim_vigencia_convenio = str_sub(fim_da_vigencia_termo_convenio, start= -4),
    ano_data_final_prevista_e_estimada = str_sub(final_previsto, 1, 4),
    fim_da_vigencia_termo_convenio = ifelse(fim_da_vigencia_termo_convenio == "", NA, 
                                            fim_da_vigencia_termo_convenio)) %>%
   select(id, nome, municipio, uf, responsabilidade, logradouro, 
         ano_convenio, valor_pactuado_com_o_fnde, cancelada, concluida, paralisada, nao_iniciada, execucao, atrasada, sem_end, 
         status, situacao_segundo_tbrasil, situacao, ano_fim_vigencia_convenio, termo_convenio, 
         data_prevista_de_conclusao_da_obra, final_previsto, ano_data_final_prevista_e_estimada, tipo_do_projeto) %>%
  rename(status_segundo_simec = situacao,
         data_final_prevista_e_estimada = final_previsto)  %>%
  left_join(ot1 , by=c("id" = "id_obra")) %>%
  mutate(projeto_obra_transparente = ifelse(is.na(projeto_obra_transparente), 0, 
                                            projeto_obra_transparente))


#Valid - Garantindo que cada status adquira um valor único
# v1 <- geral %>%
#   filter(paralisada == 1 & concluida == 1) #ok
# 
# v1 <- geral %>%
#   filter(paralisada == 1 & cancelada == 1) #ok
# 
# v1 <- geral %>%
#   filter(paralisada == 1 & execucao == 1) #ok
# 
# v1 <- geral %>%
#   filter(paralisada == 1 & nao_iniciada == 1) #ok
# 
# v1 <- geral %>%
#   filter(concluida == 1 & cancelada == 1) #ok
# 
# v1 <- geral %>%
#   filter(concluida == 1 & execucao == 1) #ok
# 
# v1 <- geral %>%
#   filter(concluida == 1 & nao_iniciada == 1) #ok
# 
# v1 <- geral %>%
#   filter(cancelada == 1 & nao_iniciada == 1) #ok
# 
# v1 <- geral %>%
#   filter(cancelada == 1 & execucao == 1) #ok
# 
# v1 <- geral %>%
#   filter(nao_iniciada == 1 & execucao == 1) #ok

# test_ot <- geral %>%
#   filter(projeto_obra_transparente == 1)  #ok

# 

save(geral, file="situacao_todas_obras.Rdata")
write.csv(geral , file="situacao_todas_obras.csv", sep=";", quote = TRUE,
           row.names = FALSE)
 