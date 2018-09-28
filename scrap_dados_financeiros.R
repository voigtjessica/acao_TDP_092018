
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)

locale(date_names = "pt", date_format = "%AD", time_format = "%AT",
       decimal_mark = ",", grouping_mark = ".")

#url simec:
url <- "http://simec.mec.gov.br/painelObras/recurso.php?obra="

#obras, já estou pegando o arquivo final
load("situacao_todas_obras.Rdata")

simec <- geral %>%
  clean_names() %>%
  mutate(url_base = url,
         url_final = paste(url_base, id, sep=""))

## importa planilha e pega os ids das urls

# infos1 <- infos
# 15:15 10min
# 15:26
# 15:38

obras <- list()
infos <- list()
n <- nrow(simec)


for ( i in 1:n) {
  download.file(simec$url_final[i], destfile = "scrapedpage.html", quiet=TRUE)
  obras[[i]] <- read_html("scrapedpage.html", encoding = "latin1")
  infos[[i]] <- obras[[i]] %>% 
    html_nodes("table") %>%
    html_table(fill=T)
  Sys.sleep(.01)
  if ( i %% 100 == 0 ) print(i)
}

##################################
infos_backup <- infos

vec <- numeric()
for ( i in 1:length(infos)) {
  infos[[i]]$id <- simec$id[i]
  infos[[i]][grepl("Nenhum resultado encontrado", infos[[i]])] <- NULL
  infos[[i]][grepl("CNPJ", infos[[i]])] <- NULL
  infos[[i]][grepl("Nome da Obra", infos[[i]])] <- NULL
  infos[[i]] <- as.data.frame(infos[[i]])
  vec[i] <- ncol(infos[[i]])
}

simec_fin1 <- bind_rows(infos[vec==4]) %>%
  clean_names() %>%
  mutate(valor_do_pagamento = str_trim(valor_do_pagamento),
         valor_do_pagamento = gsub("R\\$ ", "", valor_do_pagamento),
         valor_do_pagamento = gsub("\\.", "", valor_do_pagamento),
         valor_do_pagamento = as.numeric(gsub(",", "\\.", valor_do_pagamento)),
         data_de_pagamento = as.Date(data_de_pagamento, "%d/%m/%Y")) %>%
  select(-percentual_pagamento)


simec_fin2 <- bind_rows(infos[vec==5]) %>%
  clean_names() %>%
  mutate(valor_repassado = str_trim(valor_repassado),
         valor_repassado = gsub("R\\$ ", "", valor_repassado),
         valor_repassado = gsub("\\.", "", valor_repassado),
         valor_repassado = as.numeric(gsub(",", "\\.", valor_repassado)),
         data_do_repasse = as.Date(data_do_repasse, "%d/%m/%Y")) %>%
  select(-c(ob, dados_bancarios)) %>%
  rename(data_de_pagamento = data_do_repasse,
         valor_do_pagamento = valor_repassado) %>%
  bind_rows(simec_fin1)

lista_objetos <- list(infos, infos_backup, simec_fin2)
save(lista_objetos, file = "arquivos_simec_fin_v2.RData")

