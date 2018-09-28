#planilha upload ação:

library(janitor)
library(dplyr)
library(data.table)
library(stringr)

#obras
load("obras04092018.Rdata")

#execução dos cronogramas
execucao_cronogramas_lai <- fread("tempo_obra.csv")
execucao_cronogramas_lai <- execucao_cronogramas_lai  %>%
  mutate(project_id = as.character(project_id),
         tempo_obra_dias = tempo_obra*30) %>%
  rename(tempo_obra_dias_via_lai = tempo_obra_dias) %>%
  select(1,3)

load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/sit_obras_final.Rdata")
#sit_obras_final

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


incongruencias <- data.frame('ID das Incongruências' = c("7", "8", "9", "10", "11"),
                             )

# 7 	A obra já deveria ter sido entregue, de acordo com seu cronograma. 	Informe o motivo do atraso na execução da obra e a nova data de entrega. 	Editar
# 8 	A obra não possui endereço nos bancos de dados oficiais. 	O FNDE exige, para liberação dos recursos referentes à construção de tal obra, comprovação de posse do terreno. Por isso, informe o endereço correto, completo e atualizado da obra. 	Editar
# 9 	A obra encontra-se paralisada de acordo com os dados oficiais. 	Informe o motivo da paralisação na execução da obra e a nova data de entrega. 	Editar
# 10 	A obra encontra-se paralisada. 	Dados apontam que houve a contratação de uma empresa para construção, mas a obra não está em execução. Informe o motivo da paralisação na execução da obra e a nova data de entrega. 	Editar
# 11 	Obra ainda não foi iniciada, segundo informações oficiais.



envio_acao <- obras %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  filter(!id %in% id_controle_campanha_final,
         !tipo_do_projeto %in% not_project) %>% #apenas constru de esc e creches
  left_join(execucao_cronogramas_lai, by=c("id" = "project_id")) %>% #infos que pegamos via lai
  left_join(tempo_projeto, by=c("tipo_do_projeto")) %>%      #tempo que o projeto padrão dura
  mutate(data_de_assinatura_do_contrato = as.Date(data_de_assinatura_do_contrato, format="%Y-%m-%d %H:%M:%S"),
         data_prevista_de_conclusao_da_obra= as.Date(data_prevista_de_conclusao_da_obra, format="%d/%m/%Y"),
         final_previsto = if_else(!is.na(tempo_obra_dias_via_lai), tempo_obra_dias_via_lai + data_de_assinatura_do_contrato, 
                                  if_else(!is.na(data_prevista_de_conclusao_da_obra), data_prevista_de_conclusao_da_obra, 
                                          data_de_assinatura_do_contrato+tempo_execucao_dias)),
         nao_iniciada = ifelse( percentual_de_execucao == 0 & !situacao %in%
                                  c("Inacabada","Paralisada", "Obra Cancelada", "Concluída" ), "11", "NA"),
         paralisada_nao_off = if_else(!is.na(data_de_assinatura_do_contrato) & situacao != "Execução" & nao_iniciada == 0|
                                percentual_de_execucao > 0 & situacao != "Execução"  & nao_iniciada == 0| 
                                !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Licitação" & nao_iniciada == 0|
                                !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Em Reformulação" & nao_iniciada == 0 |
                                !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Contratação" & nao_iniciada == 0|
                                !is.na(data_prevista_de_conclusao_da_obra) & situacao == "Planejamento pelo proponente" & nao_iniciada == 0,  
                              "10" , "NA"),
         paralisada_oficial = ifelse(situacao %in% c("Inacabada","Paralisada"), "9", "NA"),
         paralisada_nao_off = ifelse(situacao %in% c("Obra Cancelada", "Concluída"), "NA", paralisada_nao_off),
         paralisada_oficial = ifelse(situacao %in% c("Obra Cancelada", "Concluída"), "NA", paralisada_oficial), #retirando concluidas e canceladas
         atrasada = if_else(final_previsto < "2018-09-04" & !situacao %in% c("Concluída","Obra Cancelada"), 
                            "7", "NA"),
         atrasada = if_else(is.na(final_previsto), "", atrasada),
         rede_de_ensino_publico = if_else(id == "1063221" | id == "29054", 
                                    "Municipal", rede_de_ensino_publico),
         logradouro = tolower(logradouro),
         logradouro = str_trim(logradouro),                            # retirar espaços no fim
         logradouro = ifelse(logradouro=="", NA, logradouro),
         sem_end = if_else(is.na(logradouro), "8", "NA"),                #obras que não tÊm endereço.
         id_da_inc = paste(nao_iniciada, paralisada_nao_off, paralisada_oficial, atrasada, sem_end, sep=";" ),
         id_da_inc = gsub("NA", "", id_da_inc),
         id_da_inc = gsub("^;", "", id_da_inc),
         id_da_inc = gsub("^;", "", id_da_inc),
         id_da_inc = gsub("^;", "", id_da_inc),
         id_da_inc = gsub("^;", "", id_da_inc),
         id_da_inc = gsub(";$", "", id_da_inc),
         id_da_inc = gsub(";$", "", id_da_inc),
         id_da_inc = gsub(";$", "", id_da_inc),
         id_da_inc = gsub(";$", "", id_da_inc),
         id_da_inc = gsub(";;;", ";;", id_da_inc),
         id_da_inc = gsub(";;", ";", id_da_inc),
         id_da_inc = gsub(";;", ";", id_da_inc)) %>%
  filter(grepl("\\d", id_da_inc)) %>%                      #deu certo!
  mutate('Usuário' = "TDP2018",
         'Comentário' = NA) %>%
  select(67, 1, 2, 68, 66) %>%
  rename('ID da OBRA' = id,
         'Nome da Obra' = nome,
         'ID das Incongruências' = id_da_inc)

save()