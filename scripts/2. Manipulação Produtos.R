
#############################################################################
#  Manipulação e Agrupamento de Produtos utilizando o arquivo Produtos.xlsx #
#############################################################################

# Pacotes -----------------------------------------------------------------

library(readxl)
library(magrittr)
library(dplyr)

# Dados -------------------------------------------------------------------

def_produtos <- read_excel("data/IBGE/Produtos.xlsx") %>% mutate(join = "T")

# Mudança nos valores X/../.../-  -----------------------------------------

quantidade_produzida %<>% 
  mutate(
    Quantidade = case_when(
      Quantidade=="X"   ~ "NA",
      Quantidade=="-"   ~ "0",
      Quantidade==".."  ~ "0",
      Quantidade=="..." ~ "0",
      TRUE ~ Quantidade
    ) %>% as.numeric()
  )

valor_da_producao %<>%
  mutate(
    `Valor da Produção` = case_when(
      `Valor da Produção`=="X"   ~ "NA",
      `Valor da Produção`=="-"   ~ "0",
      `Valor da Produção`==".."  ~ "0",
      `Valor da Produção`=="..." ~ "0",
      TRUE ~ `Valor da Produção`
    ) %>% as.numeric()
  )

# Separação das bases em Agricultura e Pecuaria ---------------------------

quantidade_produzida_agr <- quantidade_produzida %>% filter(str_detect(Tipologia, "Agricultura"))
quantidade_produzida_pec <- quantidade_produzida %>% filter(str_detect(Tipologia, "Pecuária"))

valor_da_producao_agr <- valor_da_producao %>% filter(str_detect(Tipologia, "Agricultura"))
valor_da_producao_pec <- valor_da_producao %>% filter(str_detect(Tipologia, "Pecuária"))

# Manipulação - Agrupamento dos produtos ----------------------------------

## Quantidade Produzida
quantidade_produzida_agr_join <- quantidade_produzida_agr %>% left_join(def_produtos, by = "Produto")

quantidade_produzida_agr_T <- quantidade_produzida_agr_join %>% 
  filter(join=="T") %>% 
  mutate(
    Agregado = case_when(
      str_detect(Produto, "Café")   ~ "Café em grão",
      str_detect(Produto, "Feijão") ~ "Feijão em grão",
      Produto=="Cenoura"            ~ "Cenoura e Nabo",
      Produto=="Nabo"               ~ "Cenoura e Nabo",
      Produto=="Goiaba"             ~ "Goiaba e Manga",
      Produto=="Manga"              ~ "Goiaba e Manga",
      TRUE ~ Produto
    )
  ) %>% 
  mutate(Produto = Agregado) %>% 
  group_by(Produto, Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, Quantidade)

quantidade_produzida_agr_outros <- 
  quantidade_produzida_agr_join %>% 
  filter(is.na(join)) %>%
  mutate(
    Agregado = case_when(
      str_detect(Grupo, "lavoura temporária") ~ "Sorgo",
      str_detect(Grupo, "lavoura permanente") ~ "Sorgo",
      TRUE ~ Grupo
    )
  ) %>% 
  mutate(Grupo = Agregado) %>% 
  group_by(Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Produto = ifelse(Grupo=="Sorgo", "Sorgo", paste("Outros -", Grupo))) %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, Quantidade)

quantidade_produzida_agr <- quantidade_produzida_agr_T %>% bind_rows(quantidade_produzida_agr_outros)

## Valor da Produção
valor_da_producao_agr_join <- valor_da_producao_agr %>% left_join(def_produtos, by = "Produto")

valor_da_producao_agr_T <- valor_da_producao_agr_join %>% 
  filter(join=="T") %>% 
  mutate(
    Agregado = case_when(
      str_detect(Produto, "Café")   ~ "Café em grão",
      str_detect(Produto, "Feijão") ~ "Feijão em grão",
      Produto=="Cenoura"            ~ "Cenoura e Nabo",
      Produto=="Nabo"               ~ "Cenoura e Nabo",
      Produto=="Goiaba"             ~ "Goiaba e Manga",
      Produto=="Manga"              ~ "Goiaba e Manga",
      TRUE ~ Produto
    )
  ) %>% 
  mutate(Produto = Agregado) %>% 
  group_by(Produto, Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise("Valor da Produção" = sum(`Valor da Produção`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, `Valor da Produção`)

valor_da_producao_agr_outros <- 
  valor_da_producao_agr_join %>% 
  filter(is.na(join)) %>%
  mutate(
    Agregado = case_when(
      str_detect(Grupo, "lavoura temporária") ~ "Sorgo",
      str_detect(Grupo, "lavoura permanente") ~ "Sorgo",
      TRUE ~ Grupo
    )
  ) %>% 
  mutate(Grupo = Agregado) %>% 
  group_by(Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise("Valor da Produção" = sum(`Valor da Produção`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Produto = ifelse(Grupo=="Sorgo", "Sorgo", paste("Outros -", Grupo))) %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, `Valor da Produção`)

valor_da_producao_agr <- valor_da_producao_agr_T %>% bind_rows(valor_da_producao_agr_outros)

# Montando as tabelas finais ----------------------------------------------

quantidade_produzida <- quantidade_produzida_agr %>% bind_rows(quantidade_produzida_pec)
valor_da_producao <- valor_da_producao_agr %>% bind_rows(valor_da_producao_pec)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_agr, quantidade_produzida_agr_join, quantidade_produzida_agr_outros, quantidade_produzida_agr_T,
  valor_da_producao_agr, valor_da_producao_agr_join, valor_da_producao_agr_outros, valor_da_producao_agr_T,
  quantidade_produzida_pec, valor_da_producao_pec, def_produtos
)

gc()

# -------------------------------------------------------------------------
