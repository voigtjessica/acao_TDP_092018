#repases ot

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
  select(id, mes_ano, valor_do_pagamento, pagamento_cte_ago2018)  %>%
  left_join(geral) %>%
  filter(projeto_obra_transparente == 1)

save(repasses, file="repasses_ot.Rdata")
write.csv(repasses, file="repasses_ot.csv", row.names = FALSE)
