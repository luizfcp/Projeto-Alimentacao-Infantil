
################################################################################
# Manipulação dos dados para Construção da participação da exportação, segundo #
# o arquivo "Sugestões para construção da participação de exportação.docx"     #
################################################################################

# Pacotes -----------------------------------------------------------------

library(magrittr)
library(scales)
library(tidyr)
library(dplyr)

`%notin%` <- Negate(`%in%`)

# Dados -------------------------------------------------------------------

# Codigo IBGE
cod_ibge <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% 
  select(uf, regiao) %>% 
  `colnames<-`(c("UF", "Região")) %>% 
  distinct_all()

########################## Quantidade Produzida ########################### 

# Soma dos Produtos por Estado
quantidade_produzida_est <- quantidade_produzida %>% 
  group_by(`Estado (UF)`, Tipologia, Produto) %>% 
  summarise(
    Quantidade = sum(Quantidade, na.rm = TRUE),
    `Quantidade (Toneladas)` = sum(`Quantidade (Toneladas)`, na.rm = TRUE)
  ) %>% 
  ungroup()

# Soma dos Produtos Brasil
quantidade_produzida_br <- quantidade_produzida %>% 
  group_by(Tipologia, Produto) %>% 
  summarise(
    Quantidade = sum(Quantidade, na.rm = TRUE),
    `Quantidade (Toneladas)` = sum(`Quantidade (Toneladas)`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate("Estado (UF)" = "Brasil") %>% 
  select(`Estado (UF)`, everything())

# Soma dos Prdutos - Juntando
quantidade_produzida_jun <- quantidade_produzida_est %>% 
  bind_rows(quantidade_produzida_br) %>% 
  mutate( # Manter como NA quem era NA e virou 0
    `Quantidade (Toneladas)` = ifelse(`Quantidade (Toneladas)`==0 & Produto %notin% c("Juta (fibra)", "Cana-de-açúcar", "Leite de vaca"), NA, `Quantidade (Toneladas)`)
  )

# Calculando o percentual entre Familiar Sim e Não
quantidade_produzida_perc <- quantidade_produzida_jun %>% 
  group_by(`Estado (UF)`, Produto) %>% 
  summarise(perc = paste(Tipologia, "_", Quantidade, "__", Quantidade/sum(Quantidade), "--", `Quantidade (Toneladas)`)) %>% 
  ungroup() %>% 
  separate(perc, c("Tipologia", "col_aux"), sep = " _ ") %>% 
  separate(col_aux, c("Quantidade", "col_aux2"), sep = " __ ") %>% 
  separate(col_aux2, c("perc_quantidade", "Quantidade_Ton"), sep = " -- ") %>% 
  mutate(
    perc_quantidade = as.numeric(perc_quantidade),
    perc_quantidade = ifelse(is.nan(perc_quantidade), NA, perc_quantidade),
    Quantidade_Ton = as.numeric(Quantidade_Ton)
  )

## Produtos a serem removidos, pois tem quantidade 0 em ambos (Fam Sim e Não)
#quantidade_produzida_perc %>% filter(is.nan(perc_quantidade))
#
## Removendo
#quantidade_produzida_perc %<>% filter(!is.nan(perc_quantidade))

########################### Valor da Produção ############################# 

# Soma dos Produtos por Estado
valor_da_producao_est <- valor_da_producao %>% 
  group_by(`Estado (UF)`, Tipologia, Produto) %>% 
  summarise(
    `Valor da Produção` = sum(`Valor da Produção`, na.rm = TRUE),
    `Valor FOB (US$)` = sum(`Valor FOB (US$)`, na.rm = TRUE)
  ) %>% 
  ungroup()

# Soma dos Produtos Brasil
valor_da_producao_br <- valor_da_producao %>% 
  group_by(Tipologia, Produto) %>% 
  summarise(
    `Valor da Produção` = sum(`Valor da Produção`, na.rm = TRUE),
    `Valor FOB (US$)` = sum(`Valor FOB (US$)`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate("Estado (UF)" = "Brasil") %>% 
  select(`Estado (UF)`, everything())

# Soma dos Prdutos - Juntando
valor_da_producao_jun <- valor_da_producao_est %>% 
  bind_rows(valor_da_producao_br) %>% 
  mutate( # Manter como NA quem era NA e virou 0
    `Valor FOB (US$)` = ifelse(`Valor FOB (US$)`==0 & Produto!="Cana-de-açúcar", NA, `Valor FOB (US$)`)
  )

# Calculando o percentual entre Familiar Sim e Não
valor_da_producao_perc <- valor_da_producao_jun %>% 
  group_by(`Estado (UF)`, Produto) %>% 
  summarise(perc = paste(Tipologia, "_", `Valor da Produção`, "__", `Valor da Produção`/sum(`Valor da Produção`), "--", `Valor FOB (US$)`)) %>% 
  ungroup() %>% 
  separate(perc, c("Tipologia", "col_aux"), sep = " _ ") %>% 
  separate(col_aux, c("Valor_prod", "col_aux2"), sep = " __ ") %>% 
  separate(col_aux2, c("perc_valor_prod", "Valor_FOB"), sep = " -- ") %>% 
  mutate(
    perc_valor_prod = as.numeric(perc_valor_prod),
    perc_valor_prod = ifelse(is.nan(perc_valor_prod), NA, perc_valor_prod),
    Valor_FOB = as.numeric(Valor_FOB)
  )

# Separação em Agricultura e Pecuária Familiar Sim e Não ------------------

quantidade_produzida_perc_agr_sim <- quantidade_produzida_perc %>% filter(Tipologia=="Agricultura Familiar Sim")
quantidade_produzida_perc_agr_nao <- quantidade_produzida_perc %>% filter(Tipologia=="Agricultura Familiar Não")
valor_da_producao_perc_agr_sim <- valor_da_producao_perc %>% filter(Tipologia=="Agricultura Familiar Sim")
valor_da_producao_perc_agr_nao <- valor_da_producao_perc %>% filter(Tipologia=="Agricultura Familiar Não")

quantidade_produzida_perc_pec_sim <- quantidade_produzida_perc %>% filter(Tipologia=="Pecuária Familiar Sim")
quantidade_produzida_perc_pec_nao <- quantidade_produzida_perc %>% filter(Tipologia=="Pecuária Familiar Não")
valor_da_producao_perc_pec_sim <- valor_da_producao_perc %>% filter(Tipologia=="Pecuária Familiar Sim")
valor_da_producao_perc_pec_nao <- valor_da_producao_perc %>% filter(Tipologia=="Pecuária Familiar Não")

# Construindo as Matrizes -------------------------------------------------

########################## Quantidade Produzida ########################### 

# Matriz Quantidade Produzida Agricultura Familiar Sim
quantidade_produzida_agr_sim_matriz <- quantidade_produzida_perc_agr_sim %>% 
  select(`Estado (UF)`, Produto, perc_quantidade) %>% 
  `colnames<-`(c("UF", "Produto", "perc_quantidade")) %>% 
  mutate(
    perc_quantidade = perc_quantidade %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# Matriz Quantidade Produzida Pecuária Familiar Sim
quantidade_produzida_pec_sim_matriz <- quantidade_produzida_perc_pec_sim %>% 
  select(`Estado (UF)`, Produto, perc_quantidade) %>% 
  `colnames<-`(c("UF", "Produto", "perc_quantidade")) %>% 
  mutate(
    perc_quantidade = perc_quantidade %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# Matriz ComexStat Quantidade Tonelada ~ Quantidade Produzida da Agricultura Familiar Sim
quantidade_ton_agr_sim_matriz <- quantidade_produzida_perc_agr_sim %>% 
  mutate(quantidade_ton_sim = perc_quantidade*Quantidade_Ton) %>% 
  select(`Estado (UF)`, Produto, quantidade_ton_sim) %>% 
  `colnames<-`(c("UF", "Produto", "Quantidade")) %>% 
  pivot_wider(names_from = Produto, values_from = Quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

## Vetor da Matriz Quantidade Tonelada ~ Quantidade Produzida da Agricultura Familiar Sim
#valor_fob_agr_sim_vetor <- quantidade_produzida_perc_agr_sim %>% 
#  mutate(quantidade_ton_sim = perc_quantidade*Quantidade_Ton) %>% 
#  select(`Estado (UF)`, Produto, quantidade_ton_sim) %>% 
#  `colnames<-`(c("UF", "Produto", "Quantidade")) %>% 
#  group_by(UF) %>% 
#  summarise(Valor_soma = sum(Quantidade, na.rm = TRUE)) %>% 
#  ungroup() %>% 
#  left_join(cod_ibge, by = c("UF")) %>% 
#  select(`Região`, UF, everything()) %>% 
#  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
#  arrange(`Região`) %>% 
#  `colnames<-`(c("Região", "UF", "Agricultura Familiar"))
  
# Matriz ComexStat Quantidade Tonelada ~ Quantidade Produzida da Pecuária Familiar Sim
quantidade_ton_pec_sim_matriz <- quantidade_produzida_perc_pec_sim %>% 
  mutate(quantidade_ton_sim = perc_quantidade*Quantidade_Ton) %>% 
  select(`Estado (UF)`, Produto, quantidade_ton_sim) %>% 
  `colnames<-`(c("UF", "Produto", "Quantidade")) %>% 
  pivot_wider(names_from = Produto, values_from = Quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

## Vetor da Matriz Quantidade Tonelada ~ Quantidade Produzida da Pecuária Familiar Sim
#valor_fob_pec_sim_vetor <- quantidade_produzida_perc_pec_sim %>% 
#  mutate(quantidade_ton_sim = perc_quantidade*Quantidade_Ton) %>% 
#  select(`Estado (UF)`, Produto, quantidade_ton_sim) %>% 
#  `colnames<-`(c("UF", "Produto", "Quantidade")) %>% 
#  group_by(UF) %>% 
#  summarise(Valor_soma = sum(Quantidade, na.rm = TRUE)) %>% 
#  ungroup() %>% 
#  left_join(cod_ibge, by = c("UF")) %>% 
#  select(`Região`, UF, everything()) %>% 
#  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
#  arrange(`Região`) %>% 
#  `colnames<-`(c("Região", "UF", "Pecuária Familiar"))
  
########################### Valor da Produção ############################# 

# Matriz Valor da Produção Agricultura Familiar Sim
valor_da_producao_agr_sim_matriz <- valor_da_producao_perc_agr_sim %>% 
  select(`Estado (UF)`, Produto, perc_valor_prod) %>% 
  `colnames<-`(c("UF", "Produto", "perc_valor_prod")) %>% 
  mutate(
    perc_valor_prod = perc_valor_prod %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_valor_prod) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# Matriz Valor da Produção Pecuária Familiar Sim
valor_da_producao_pec_sim_matriz <- valor_da_producao_perc_pec_sim %>% 
  select(`Estado (UF)`, Produto, perc_valor_prod) %>% 
  `colnames<-`(c("UF", "Produto", "perc_valor_prod")) %>% 
  mutate(
    perc_valor_prod = perc_valor_prod %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_valor_prod) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# Matriz ComexStat Valor FOB ~ Valor da Produção da Agricultura Familiar Sim
valor_fob_agr_sim_matriz <- valor_da_producao_perc_agr_sim %>% 
  mutate(valor_fob_sim = perc_valor_prod*Valor_FOB) %>% 
  select(`Estado (UF)`, Produto, valor_fob_sim) %>% 
  `colnames<-`(c("UF", "Produto", "Valor")) %>% 
  pivot_wider(names_from = Produto, values_from = Valor) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# Matriz ComexStat Valor FOB ~ Valor da Produção da Pecuária Familiar Sim
valor_fob_pec_sim_matriz <- valor_da_producao_perc_pec_sim %>% 
  mutate(valor_fob_sim = perc_valor_prod*Valor_FOB) %>% 
  select(`Estado (UF)`, Produto, valor_fob_sim) %>% 
  `colnames<-`(c("UF", "Produto", "Valor")) %>% 
  pivot_wider(names_from = Produto, values_from = Valor) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)


# -------------------------------------------------------------------------

rm(
  quantidade_produzida_est, quantidade_produzida_br,
  quantidade_produzida_jun, quantidade_produzida_perc, cod_ibge,
  quantidade_produzida_perc_agr_sim, quantidade_produzida_perc_agr_nao,
  quantidade_produzida_perc_pec_sim, quantidade_produzida_perc_pec_nao,
  quantidade_produzida_agr, quantidade_produzida_pec,
  valor_da_producao_est, valor_da_producao_br,
  valor_da_producao_jun, valor_da_producao_perc,
  valor_da_producao_perc_agr_sim, valor_da_producao_perc_agr_nao,
  valor_da_producao_perc_pec_sim, valor_da_producao_perc_pec_nao,
  valor_da_producao_agr, valor_da_producao_pec, `%notin%`
)

gc()

# -------------------------------------------------------------------------
