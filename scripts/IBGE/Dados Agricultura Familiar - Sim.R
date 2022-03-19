
#########################################
#  Dados trabalhados neste script       #
#  - 2. Dados Agricultura Familiar Sim  #
#########################################

# Pacotes -----------------------------------------------------------------

library(tidyr)
library(magrittr)
library(purrr)
library(readxl)
library(stringr)
library(naniar)
library(dplyr)

# Dados -------------------------------------------------------------------

# Caminho dos arquivos para leitura
path <- list.files("data/IBGE/2. Dados Agricultura Familiar Sim/", full.names = T)

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
lista_quantidade_produzida %<>% map_at(c(-4), ~ .x %>% select(-2))
lista_quantidade_produzida[[6]] %<>% .[-6,]

# Loops
lista_quantidade_produzida_mod <- 
  lista_quantidade_produzida %>%
  map(
    ~ .x %>% 
      select(-c(2:3)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(Grupo = .x[[2,3]])
  )

# Join nas bases de dados
## full_join para nao ter perda de linhas
quantidade_produzida_join <- 
  lista_quantidade_produzida_mod[[1]] %>% 
  full_join(lista_quantidade_produzida_mod[[2]], by = c("Municipio", "Outros produtos (Toneladas)", "Grupo")) %>% 
  full_join(lista_quantidade_produzida_mod[[3]], by = c("Municipio", "Outros produtos (Toneladas)", "Grupo")) %>% 
  full_join(lista_quantidade_produzida_mod[[4]], by = c("Municipio", "Grupo")) %>% 
  full_join(lista_quantidade_produzida_mod[[5]], by = c("Municipio", "Outros produtos (Toneladas)", "Grupo")) %>% 
  full_join(lista_quantidade_produzida_mod[[6]], by = c("Municipio", "Grupo")) %>% 
  full_join(lista_quantidade_produzida_mod[[7]], by = c("Municipio", "Outros produtos (Toneladas)", "Grupo")) %>% 
  pivot_longer(!c("Municipio", "Grupo"), names_to = "Produto", values_to = "Quantidade")

# Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
quantidade_produzida <- 
  quantidade_produzida_join %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x ")
  ) %>% 
  separate(Produto, c("Produto", "UMedida"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    CodigoIBGE = NA,
    Tipologia = NA,
    UMedida = UMedida %>% str_remove_all("[[:punct:]]")
  ) %>% 
  select(CodigoIBGE, Municipio, Estado, Tipologia, Grupo, UMedida, Quantidade) %>% 
  `colnames<-`(c(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade")))

# Manipulação dos dados - Valor da Produção -------------------------------



