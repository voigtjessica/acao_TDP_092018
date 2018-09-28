#custo das obras:
library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(readr)
library(googledrive)

#Estimativa de quanto custam as obras
#obras
load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/obras04092018.Rdata")

obras1 <- obras %>%
  clean_names()

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

#tempo de execução dos projetos padrão:
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

#Obras todas:
# Objeto geral:
obras_todas <- obras1 %>%
  mutate(id = as.character(id)) %>%
  rename(responsabilidade = rede_de_ensino_publico) %>%
  left_join(execucao_cronogramas_lai, by=c("id" = "project_id")) %>% #infos que pegamos via lai
  left_join(tempo_projeto, by=c("tipo_do_projeto")) %>%      #tempo que o projeto padrão dura
  mutate(tem_convenio = ifelse(!grepl("[0-9]", termo_convenio), "sem informações", "com informações"),
         construcao = ifelse(tipo_do_projeto %in% not_project, "reforma / quadra", "construção") ,
         data_de_assinatura_do_contrato = as.Date(data_de_assinatura_do_contrato, format="%Y-%m-%d %H:%M:%S"),
          data_prevista_de_conclusao_da_obra= as.Date(data_prevista_de_conclusao_da_obra, format="%d/%m/%Y"),
          final_previsto = if_else(!is.na(tempo_obra_dias_via_lai), tempo_obra_dias_via_lai + data_de_assinatura_do_contrato, 
                                  if_else(!is.na(data_prevista_de_conclusao_da_obra), data_prevista_de_conclusao_da_obra, 
                                          data_de_assinatura_do_contrato+tempo_execucao_dias)),
         nao_iniciada = ifelse( percentual_de_execucao == 0 & !situacao %in%
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
         atrasada = if_else(final_previsto < "2018-09-04" & !situacao %in% c("Concluída","Obra Cancelada"), 
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
         data_prevista_de_conclusao_da_obra, final_previsto, ano_data_final_prevista_e_estimada, tipo_do_projeto, construcao, tem_convenio) %>%
  rename(status_segundo_simec = situacao,
         data_final_prevista_e_estimada = final_previsto) 


obras_sem_conv <- obras_todas %>%
  filter(is.na(termo_convenio))

convenios <- obras_todas %>%
  select(id, termo_convenio, construcao) %>%
  group_by(termo_convenio) %>%
  mutate( obras_pactuadas_ref_cons = n()) %>%
  filter(obras_pactuadas_ref_cons == 1 & construcao == "construção")


#essa é a lista dos ids cujos convênios só tem uma obra de construção pactuada.
#validando:

conv <- unique(convenios$termo_convenio)

valid <- obras_todas %>%
  filter(termo_convenio %in% conv) 

#mesmo número, tá validado.

id_conv <- unique(convenios$id)
save(id_conv, file="convenios_com_apenas_uma_obra.Rdata")

# Juntando com os dados do repasse:

load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/arquivos_simec_fin_v2.RData")

simec_fin2 <- lista_objetos[[3]]

#primeiro ajustando para valores correntes:
ipca <- read_csv2("bcdata.sgs.4447.csv")
ipca <- bind_rows(ipca, data.frame(data="01/08/2018", valor=0))
ipca <- ipca %>%
  mutate(data= as.Date(data, "%d/%m/%Y"),
         mes_ano = format(data, "%m/%Y")) %>%
  filter(data > as.Date("2007-01-01")) %>%
  mutate(indice = cumprod(1+valor/100),
         indice_max = last(indice),
         indice = indice/indice_max)

#obras que eu vou usar:
load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/situacao_todas_obras.Rdata")

repasses <- simec_fin2 %>%
  mutate(mes_ano = format(data_de_pagamento, "%m/%Y")) %>%
  left_join(ipca, by=c("mes_ano")) %>%
  mutate(pagamento_cte_ago2018 = round(valor_do_pagamento/indice, 0)) %>%
  select(id, valor_do_pagamento, pagamento_cte_ago2018)  %>%
  filter(id %in% id_conv)

# %>%
#   left_join(geral, by=c("id")) %>%
#   filter(!is.na(status)) #todo mundo que tem no repasses tá em geral, ok

repasses %>% group_by(id) %>%  summarise(n()) %>% nrow() #7101

valor_medio_obras <- repasses %>%
  group_by(id) %>%
  mutate(valor_total_obra_cte2018 = sum(pagamento_cte_ago2018, na.rm=TRUE)) %>%
  distinct(id, status, valor_total_obra_cte2018, ano_convenio) %>%
  group_by(status, ano_convenio) %>%
  summarise(repasse_medio = mean(valor_total_obra_cte2018, na.rm=TRUE))

ids_com_repasse_mais_de_um_con <- setdiff(simec_fin2$id, id_conv)

# todo mundo q recebeu repasse é id_conv + ids_com_repasse_mais_de_um_con

rep <- repasses %>%
  group_by(id) %>%
  summarise(repasse_total_obra_cte2018 = sum(pagamento_cte_ago2018, na.rm=TRUE))

geral_repasses_corrigidos <- geral %>%
  left_join(rep, by=c("id")) %>%
  mutate(estimar_repasse = ifelse(id %in% ids_com_repasse_mais_de_um_con, 1, 0)) %>%
  left_join(valor_medio_obras, by=c("status", "ano_convenio")) %>%
  mutate(valor_repassado_final = ifelse(!is.na(repasse_total_obra_cte2018), repasse_total_obra_cte2018,
                                        ifelse(is.na(repasse_total_obra_cte2018) & estimar_repasse == 1, 
                                               repasse_medio, 0))) %>%
  select(-c(repasse_medio))
  
save(geral_repasses_corrigidos, file="situacao_todas_obras_final.Rdata")
write.csv(geral_repasses_corrigidos, file="situacao_todas_obras_final.csv", sep=";", quote = TRUE,
          row.names = FALSE)

teste_gdrive_df_sheet <- drive_upload(
  "situacao_todas_obras_final.csv",
  path="~/TB/Tá de Pé/Transparência Brasil - Tá de Pé 2018",
  name = "sit_obras_final_v15",
  type = "spreadsheet")

