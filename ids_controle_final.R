# vendo se há match entre obras do grupo controle e obras do OT:

library(dplyr)
library(googlesheets)
library(googledrive)

gs_ls() 
load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/randomization_TDP_phase2.RData")
sheet <- gs_title("Obras - Obra Transparente")
obra_transparente <- gs_read(sheet)

ot <- obra_transparente %>%
  clean_names() %>%
  mutate(controle_campanha = ifelse(id_obra %in% ids_control_campaign, 1, 0),
         controle_app = ifelse(id_obra %in% ids_control_app , 1, 0),
         id_obra = ifelse(id_obra == "1067788 (1001904)", "1067788", id_obra))

#novos arquivos de controle:

rem_cont_camp <- ot %>%
  filter(controle_campanha == 1)

ids_remover_controle_campanha <- unique(rem_cont_camp$id_obra)

rem_cont_app <- ot %>%
  filter(controle_app == 1)

ids_remover_controle_app <- unique(rem_cont_app$id_obra)

id_controle_campanha_final <- setdiff(ids_control_campaign, ids_remover_controle_campanha)
id_controle_app_final <- setdiff(ids_control_app, ids_remover_controle_app)

save(id_controle_campanha_final,id_controle_app_final , file="ids_controle_final.Rdata" )
save(ot, file="ot.Rdata")

# #juntando bianca e TDP:
# 
# load("situacao_todas_obras.Rdata")
# 
# press_release_fant <- ot %>%
#   select(id, nome, municipio, uf, levant_os_jun_17) %>%
#   left_join(situacao_todas_obras, by=c("id")) %>%
#   select(id, nome.x, municipio.x, uf.x, levant_os_jun_17, responsabilidade,
#          atrasada, nao_iniciada, execucao, sem_end, status_segundo_tbrasil)   %>%
#   rename(avaliacao_observatorios = levant_os_jun_17,
#          nome = nome.x,
#          municipio = municipio.x,
#          uf = uf.x)
# 
# #pq tem Nas :
# 
# ot_id <- unique(press_release_fant$id)
# 
# load("obras05072018.Rdata")
# 
# obras <- obras %>%
#   clean_names() %>%
#   filter(id %in% ot_id) %>%
#   select(id, situacao) %>%
#   mutate_all(as.character)
# 
# ot_vs_tdp <- press_release_fant %>%
#   left_join(obras, by=c("id")) %>%
#   mutate(status_segundo_tbrasil = ifelse(is.na(status_segundo_tbrasil), "não fez parte da amostra", 
#                                          status_segundo_tbrasil)) %>%
#   rename(status_segundo_simec = situacao)
# 
# write.csv(ot_vs_tdp , file="press_release_fant.csv", sep=";", quote = TRUE,
#           row.names = FALSE ) 
# 
# drive_find(n_max=10) #autenticação
# 
# save(ot_vs_tdp, file="press_release_fant.Rdata")
# 
# press_release_fant_sheet <- drive_upload(
#   "press_release_fant.csv",
#   path="~/TB/Tadepé/Transparência Brasil - Tá de Pé 2018",
#   name = "ot_vs_tb_v3",
#   type = "spreadsheet")
