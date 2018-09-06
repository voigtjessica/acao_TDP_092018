
setwd("C:\\Users\\jvoig\\OneDrive\\Documentos\\acao_TDP_092018\\validacao_obras")
load("obras28072017.Rdata")
#obras
obras2017 <- obras

load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/obras04092018.Rdata")
#obras
obras2018 <- obras

#Vendo quantas linhas têm os arquivos de datas diferentes
valid1 <- data.frame(ano = c("2017", "2018"),
                     obras = c(nrow(obras2017), nrow(obras2018)))

28870 - 28569
# 301 obras a mais em 2018.

#vendo se tem obras em 2017 que não aparecem em 2018:

ids_2017 <- unique(obras2017$ID)

ids_2018 <- unique(obras2018$ID)

#O que tem em 2017 e que não tem em 2018
dif_ids <-  setdiff(ids_2017, ids_2018)
length(dif_ids)
# [1] 123

#reproduzindo o relatório de 2017:

tipo_do_projeto <- c("Escola de Educação Infantil Tipo B",
                     "Escola de Educação Infantil Tipo C",
                     "MI - Escola de Educação Infantil Tipo B",
                     "MI - Escola de Educação Infantil Tipo C",
                     "Espaço Educativo - 12 Salas",
                     "Espaço Educativo - 01 Sala",
                     "Espaço Educativo - 02 Salas",
                     "Espaço Educativo - 04 Salas",
                     "Espaço Educativo - 06 Salas",
                     "Projeto 1 Convencional",
                     "Projeto 2 Convencional")
tempo_exe_meses <- c(9,6,6,4,13,5,5,7,7,11,9)
execucao <- data.frame(tipo_do_projeto, tempo_exe_meses)

base2017 <- obras2017 %>%
  clean_names() %>%
  filter(tipo_da_obra == "Construção") %>%
  inner_join(execucao)

nrow(base2017) #12925

base_2018 <- obras2018 %>%
  clean_names() %>%
  filter(tipo_da_obra == "Construção") %>%
  inner_join(execucao)

nrow(base_2018) #usando os mesmos critérios, o número de obras aumentou para 13006

# Vendo as diferenças entre os critérios que eu usei no relatório e ness press release

load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/situacao_todas_obras.Rdata")
#geral

nrow(geral) #14435

proj_press <- unique(geral$tipo_do_projeto)   #22 projetos
proj_2017 <- unique(base2017$tipo_do_projeto) #11 projetos

setdiff(proj_press,proj_2017 )

ids_rel <- unique(base2017$id)
ids_press <- unique(geral$id)

dif_ids_entre_estudos <- setdiff(ids_rel, ids_press)
length(dif_ids_entre_estudos)

# Vendo os ids que não estão no press release 
# id 1017168 

x <- base2017 %>% 
  filter(id == "1017168")

#CRECHE DO BAIRRO CENTRO - Abel Figueiredo - PA , nome bate com o site.

y <- geral %>%
  filter(nome == "CRECHE DO BAIRRO CENTRO - Abel Figueiredo - PA") #não encontra

y <- geral %>%
  filter(id == "1017168") #não encontra

y <- base_2018 %>%
  filter(id == "1017168") #encontra

#descobri que são as obras q  ue estão no grupo controle. Mesmo assim, resta uma diferença de 4 obras.
load("C:/Users/jvoig/OneDrive/Documentos/acao_TDP_092018/ids_controle_final.Rdata")

dif_controle_sumidos <- setdiff(dif_ids_entre_estudos, id_controle_campanha_final)
#restam 77

# 1002937 não tem site
# 24492 não tem site
# 1016311 náo tem site
# 1009748 não tem site
# 1004676 não tem site
# 1017997 não tem site
# 1006593 não tem site
# 8582 não tem site

#Ver quem são esses caras:

info_sumidos <- base2017 %>%
  filter(id %in% dif_controle_sumidos) %>%
  rename(id2017 = id) %>%
  select(nome, municipio, uf, id2017) %>%
  mutate(sumido = 1)

valid2 <- geral %>%
  left_join(info_sumidos) %>%
  filter(sumido == 1) %>%
  rename(id2018 = id) %>%
  select(nome, municipio, uf, id2017, id2018)

save(valid2, "obras_que_mudaram_de_id.Rdata")
write.csv(valid2, file="obras_que_mudaram_de_id.csv", sep=";", quote = TRUE,
          row.names = FALSE)

drive_find(n_max=10) #autenticação

lista_final_sheet <- drive_upload(
  "obras_que_mudaram_de_id.csv",
  path="~/TB/Tadepé/Transparência Brasil - Tá de Pé 2018",
  name = "obras_que_mudaram_de_id",
  type = "spreadsheet")
