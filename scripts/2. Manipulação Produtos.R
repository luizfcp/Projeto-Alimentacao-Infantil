
#############################################################################
#  Manipulação e Agrupamento de Produtos utilizando o arquivo Produtos.xlsx #
#############################################################################

# Pacotes -----------------------------------------------------------------

library(readxl)
library(magrittr)
library(dplyr)

# Dados -------------------------------------------------------------------

def_produtos_agr <- read_excel("data/IBGE/Produtos.xlsx", sheet = 1) %>% 
  select(Produto) %>% 
  mutate(join = "T") %>% 
  filter(!str_detect(Produto, "^Outros"))

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

# -------------------------------------------------------------------------

rm(quantidade_produzida, valor_da_producao)

# Manipulação Agricultura - Agrupamento dos produtos ----------------------

## Quantidade Produzida
quantidade_produzida_agr_join <- quantidade_produzida_agr %>% left_join(def_produtos_agr, by = "Produto")

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
  group_by(Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    Produto = case_when(
      Grupo=="Produtos da silvicultura"                  ~ "Produtos da Silvicultura",
      str_detect(Grupo, "aquicultura x Condição do")     ~ "Pesca e aquicultura (peixe, crustáceos e moluscos)",
      Grupo=="Condição do produtor em relação às terras" ~ "Leite de vaca",
      TRUE ~ paste("Outros", Grupo)
    )
  ) %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, Quantidade)

quantidade_produzida_agr <- quantidade_produzida_agr_T %>% bind_rows(quantidade_produzida_agr_outros)

## Valor da Produção
valor_da_producao_agr_join <- valor_da_producao_agr %>% left_join(def_produtos_agr, by = "Produto")

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
  group_by(Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise("Valor da Produção" = sum(`Valor da Produção`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    Produto = case_when(
      Grupo=="Produtos da silvicultura"                  ~ "Produtos da Silvicultura",
      str_detect(Grupo, "aquicultura x Condição do")     ~ "Pesca e aquicultura (peixe, crustáceos e moluscos)",
      Grupo=="Condição do produtor em relação às terras" ~ "Leite de vaca",
      TRUE ~ paste("Outros", Grupo)
    )
  ) %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, `Valor da Produção`)

valor_da_producao_agr <- valor_da_producao_agr_T %>% bind_rows(valor_da_producao_agr_outros)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_agr_join, quantidade_produzida_agr_T, quantidade_produzida_agr_outros,
  valor_da_producao_agr_join, valor_da_producao_agr_T, valor_da_producao_agr_outros,
  def_produtos_agr
)

# Manipulação Pecuária - Agrupamento dos produtos -------------------------

## Quantidade Produzida
quantidade_produzida_pec %<>% 
  mutate(
    Agregado = case_when(
      str_detect(Produto, "50 cabeças e menos") ~ "Carne bovina",
      str_detect(Produto, "mais de 50 cabeças") ~ "Carne bovina",
      str_detect(Produto, "cabeças de galinhas, galos") ~ "Carne de Aves e Frangos",
      str_detect(Produto, "cabeças de suínos") ~ "Carne Suína",
      str_detect(Produto, "ovos de galinhas") ~ "Ovos de galinhas",
      TRUE ~ Produto
    )
  ) %>% 
  mutate(Produto = Agregado) %>% 
  group_by(Produto, Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise(Quantidade = sum(Quantidade, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, Quantidade)

## Valor da Produção
valor_da_producao_pec %<>% 
  mutate(
    Agregado = case_when(
      str_detect(Produto, "50 cabeças e menos") ~ "Carne bovina",
      str_detect(Produto, "mais de 50 cabeças") ~ "Carne bovina",
      str_detect(Produto, "cabeças de galinhas, galos") ~ "Carne de Aves e Frangos",
      str_detect(Produto, "cabeças de suínos") ~ "Carne Suina",
      str_detect(Produto, "ovos de galinhas") ~ "Ovos de galinhas",
      TRUE ~ Produto
    )
  ) %>% 
  mutate(Produto = Agregado) %>% 
  group_by(Produto, Grupo, `Unidade de Medida`, `Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia) %>% 
  summarise("Valor da Produção" = sum(`Valor da Produção`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`Código IBGE`, `Nome do Município`, `Estado (UF)`, Tipologia, Grupo, Produto, `Unidade de Medida`, `Valor da Produção`)

# -------------------------------------------------------------------------

gc()

# -------------------------------------------------------------------------
