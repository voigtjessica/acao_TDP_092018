#planilhas públicas

library(janitor)
library(googledrive)
library(dplyr)

load("sit_obras_final.Rdata")

obras <- read.csv(url("http://simec.mec.gov.br/painelObras/download.php"), sep=";")

cep <- obras %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  select(id, cep)

sit_obras_final %>%
  left_join(cep, by=c("id")) %>%
  mutate(na_cep = ifelse(is.na(cep), 1, 0),
         sem_end = as.numeric(sem_end)) %>%
  summarise(sem_end = sum(sem_end),
            na_cep = sum(na_cep))

#Planilhas com dados e endereço de cada uma das obras que fazem parte da campanha.
#Apenas as que receberão alertas:

obras_campanha <- sit_obras_final %>%
  mutate(situacao_segundo_tbrasil = ifelse(sem_end == 1, 
         paste0(situacao_segundo_tbrasil, " / sem endereço"),
         situacao_segundo_tbrasil)) %>%
  filter(nao_iniciada ==1 |
           atrasada == 1 |
           paralisada == 1 |
           sem_end == 1,
         !status %in% c("concluida", "cancelada")) %>%
  select(nome, responsabilidade, municipio, uf, logradouro, termo_convenio, 
         data_final_prevista_e_estimada,
         pagto_total_cte_ago2018, pactuado_cte_ago2018, situacao_segundo_tbrasil) %>%
  rename(data_de_entrega = data_final_prevista_e_estimada,
         pagamento_total = pagto_total_cte_ago2018,
         valor_pacutado = pactuado_cte_ago2018,
         problema_encontrado = situacao_segundo_tbrasil) %>%
  mutate(pagamento_total = ifelse(is.na(pagamento_total), 0, pagamento_total),
         valor_pacutado = ifelse(valor_pacutado %in% c(NA, 0), "sem informação", valor_pacutado),
         problema_encontrado = gsub(" e ", " / ", problema_encontrado))

save(obras_campanha, file="obras_campanha.Rdata")
write.csv(obras_campanha, file="obras_campanha.csv", dec = ",", sep=";")

drive_find(n_max=10) #autenticação

lista_final_sheet <- drive_upload(
  "obras_campanha.csv",
  path="~/TB",
  name = "obras que irão participar da campanha",
  type = "spreadsheet")

## Dados com os municípios notificados:

municipios_notificados <- obras_campanha %>%
  filter(responsabilidade == "Municipal") %>%
  mutate(pagamento_total = as.numeric(pagamento_total)) %>%
  group_by(uf, municipio) %>%
  summarise(quantidade_obras_notificadas = n(),
            valor_pago_obras_notificadas = sum(pagamento_total))

write.csv(municipios_notificados, file="municipios_notificados.csv", dec = ",", sep=";")
lista_final_sheet <- drive_upload(
  "municipios_notificados.csv",
  path="~/TB",
  name = "obras que irão participar da campanha",
  type = "spreadsheet")

# Dados com as UFs notificadas:
ufs_notificadas <- obras_campanha %>%
  filter(responsabilidade == "Estadual") %>%
  mutate(pagamento_total = as.numeric(pagamento_total)) %>%
  group_by(uf) %>%
  summarise(quantidade_obras_notificadas = n(),
            valor_pago_obras_notificadas = sum(pagamento_total))


write.csv(ufs_notificadas, file="ufs_notificadas.csv", dec = ",", sep=";")
lista_final_sheet <- drive_upload(
  "ufs_notificadas.csv",
  path="~/TB",
  name = "obras que irão participar da campanha",
  type = "spreadsheet")
