
#########################################
#  Dados trabalhados neste script:      #
#  - 3. Dados Agricultura Familiar Não  #
#########################################

# Pacotes -----------------------------------------------------------------

library(tidyr)
library(magrittr)
library(purrr)
library(readxl)
library(openxlsx)
library(stringr)
library(qdapRegex)
library(naniar)
library(dplyr)

# Dados -------------------------------------------------------------------

# Caminho dos arquivos para leitura
path <- list.files("data/IBGE/3. Dados Agricultura Familiar Não/", full.names = T)

# Conferir nome das Sheets
map(path, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro

# Leitura das bases de dados
lista_quantidade_produzida <- map(path, ~ .x %>% read_excel(sheet = 1))
lista_valor_da_producao <- map(path, ~ .x %>% read_excel(sheet = 2))

# Conferindo se todas as bases possuem assinatura do IBGE ao final
lista_quantidade_produzida %>% map(~ .x %>% tail(1))
lista_valor_da_producao %>% map(~ .x %>% tail(1))

# Manipulação dos dados - Quantidade Produzida ----------------------------

# Padronizando para o loop
lista_quantidade_produzida_4 <- lista_quantidade_produzida[[4]]
lista_quantidade_produzida[[4]] <- NULL
lista_quantidade_produzida[[5]] %<>% .[-6,]

# Loops
lista_quantidade_produzida_mod <- 
  lista_quantidade_produzida %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(Grupo = .x[[2,4]]) %>% 
      pivot_longer(!c("Municipio", "Grupo"), names_to = "Produto", values_to = "Quantidade")
  )

# Manipulacao da lista 4: destoa das demais
lista_quantidade_produzida_mod[[ {length(lista_quantidade_produzida_mod)+1} ]] <- 
  lista_quantidade_produzida_4 %>% 
  select(-2) %>% 
  `colnames<-`(c("Municipio", str_remove(lista_quantidade_produzida_4[1,1], "Variável - "))) %>% 
  mutate(Grupo = .[[2,2]]) %>% 
  .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
  .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
  pivot_longer(!c("Municipio", "Grupo"), names_to = "Produto", values_to = "Quantidade")

# Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
quantidade_produzida <- 
  lista_quantidade_produzida_mod %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x ")
  ) %>% 
  mutate(
    Produto = Produto %>% str_replace_all("[[:punct:]]Tonelada", "__Tonelada"),
    Produto = Produto %>% str_replace_all("[[:punct:]]Mil", "__Mil"),
    Produto = Produto %>% str_replace_all("[[:punct:]]não", "__não")
  ) %>% 
  separate(Produto, c("Produto", "UMedida"), sep = "__") %>% 
  mutate(
    CodigoIBGE = NA,
    Tipologia = NA,
    Produto = Produto %>% str_trim(),
    UMedida = UMedida %>% str_remove_all("[[:punct:]]")
  ) %>% 
  select(CodigoIBGE, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade"))

# Manipulação dos dados - Valor da Produção -------------------------------

# Padronizando para o loop
lista_valor_da_producao_4 <- lista_valor_da_producao[[4]]
lista_valor_da_producao[[4]] <- NULL
lista_valor_da_producao[[5]] %<>% .[-6,]

# Loops
lista_valor_da_producao_mod <- 
  lista_valor_da_producao %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(
        Grupo = .x[[2,4]],
        UMedida = .x[[1,1]]
      ) %>% 
      pivot_longer(!c("Municipio", "Grupo", "UMedida"), names_to = "Produto", values_to = "Quantidade")
  )

# Manipulacao da lista 4: destoa das demais
lista_valor_da_producao_mod[[ {length(lista_valor_da_producao_mod)+1} ]] <- 
  lista_valor_da_producao_4 %>% 
  select(-2) %>% 
  `colnames<-`(c("Municipio", ex_between(lista_valor_da_producao_4[1,1], "-", "("))) %>% 
  mutate(
    Grupo = .[[2,2]],
    UMedida = .[[1,1]]
  ) %>% 
  .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
  .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
  pivot_longer(!c("Municipio", "Grupo", "UMedida"), names_to = "Produto", values_to = "Quantidade")

# Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
valor_da_producao <- 
  lista_valor_da_producao_mod %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x "),
    UMedida = UMedida %>% sub(".*\\(", "", .) %>% str_remove_all("[[:punct:]]")
  ) %>% 
  mutate(
    CodigoIBGE = NA,
    Tipologia = NA
  ) %>% 
  select(CodigoIBGE, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade"))

# -------------------------------------------------------------------------

rm(
  path, lista_quantidade_produzida, lista_quantidade_produzida_4, lista_quantidade_produzida_mod,
  lista_valor_da_producao, lista_valor_da_producao_4, lista_valor_da_producao_mod
)
gc()
