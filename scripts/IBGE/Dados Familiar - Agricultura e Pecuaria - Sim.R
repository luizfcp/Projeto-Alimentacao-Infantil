
#########################################
#  Dados trabalhados neste script:      #
#  - 2. Dados Agricultura Familiar Sim  #
#  - 2. Dados Pecuaria Familiar Sim     #
#########################################

# Pacotes -----------------------------------------------------------------

library(tidyr)
library(magrittr)
library(purrr)
library(readxl)
library(stringr)
library(qdapRegex)
library(dplyr)

# Dados -------------------------------------------------------------------

# Codigo IBGE
cod_ibge <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(1, mun_uf) %>% `colnames<-`(c("cod_ibge", "mun_uf"))

# Caminho dos arquivos para leitura
path_agricultura_fam_sim <- list.files("data/IBGE/1. Agricultura/2. Dados Agricultura Familiar Sim/", full.names = T)
path_pecuaria_fam_sim <- list.files("data/IBGE/2. Pecuária/2. Dados Pecuária Familiar Sim/", full.names = T)

# Conferir nome das Sheets
map(path_agricultura_fam_sim, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro
map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets()) # Os arquivos possuem ordem variadas

# Leitura das bases de dados - Agricultura
lista_quantidade_produzida_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 1))
lista_valor_da_producao_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 2))

# Leitura das bases de dados - Pecuaria - Quantidade Produzida
pos_quantidade_produzida_pec <- # Posicionamento das sheets "Quantidade"
  map(
    path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Número|Quantidade|Tabela 1|Tabela 3") %>% which()
  )

lista_quantidade_produzida_pec_aux <- 
  map2(
    path_pecuaria_fam_sim, pos_quantidade_produzida_pec, function(path, pos) {
      map(pos, ~ read_excel(path, sheet = .x))
    }
  )

lista_quantidade_produzida_pec <- lista_quantidade_produzida_pec_aux[[1]] %>% 
  append(lista_quantidade_produzida_pec_aux[[2]]) %>% 
  append(lista_quantidade_produzida_pec_aux[[3]])

# Leitura das bases de dados - Pecuaria - Valor da Producao
pos_valor_da_producao_pec <- # Posicionamento das sheets "Valor"
  map(
    path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Valor|Tabela 2|Tabela 4") %>% which()
  )

lista_valor_da_producao_pec_aux <- 
  map2(
    path_pecuaria_fam_sim, pos_valor_da_producao_pec, function(path, pos) {
      map(pos, ~ read_excel(path, sheet = .x))
    }
  )

lista_valor_da_producao_pec <- lista_valor_da_producao_pec_aux[[1]] %>% 
  append(lista_valor_da_producao_pec_aux[[2]]) %>% 
  append(lista_valor_da_producao_pec_aux[[3]])

# Conferindo se todas as bases possuem assinatura do IBGE ao final
lista_quantidade_produzida_agr %>% map(~ .x %>% tail(1))
lista_valor_da_producao_agr %>% map(~ .x %>% tail(1))
lista_quantidade_produzida_pec %>% map(~ .x %>% tail(1))
lista_valor_da_producao_pec %>% map(~ .x %>% tail(1))

# Manipulação dos dados - Quantidade Produzida ----------------------------

# Padronizando para o loop
lista_quantidade_produzida_agr_4 <- lista_quantidade_produzida_agr[[4]]
lista_quantidade_produzida_agr[[4]] <- NULL
lista_quantidade_produzida_agr[[5]] %<>% .[-6,]

# Loops
lista_quantidade_produzida_agr_mod_1 <- 
  lista_quantidade_produzida_agr %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(
        Tipologia = "Agricultura Familiar Sim",
        Grupo = .x[[2,4]]
      ) %>% 
      pivot_longer(!c("Municipio", "Grupo", "Tipologia"), names_to = "Produto", values_to = "Quantidade")
  )

lista_quantidade_produzida_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Sim")
lista_quantidade_produzida_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Sim"))

lista_quantidade_produzida_agr_mod_2_aux <- list()
lista_quantidade_produzida_agr_mod_2_aux[[1]] <- lista_quantidade_produzida_agr_4

lista_quantidade_produzida_agr_mod_2 <- 
  lista_quantidade_produzida_agr_mod_2_aux %>% 
  append(lista_quantidade_produzida_pec) %>% 
  map(~ .x %>% 
        select(-2) %>% 
        `colnames<-`(c("Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
        mutate(Grupo = .[[2,2]]) %>% 
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        pivot_longer(!c("Municipio", "Grupo", "Tipologia"), names_to = "Produto", values_to = "Quantidade")
  )

# Separando Municipio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
quantidade_produzida_fam_sim <- 
  lista_quantidade_produzida_agr_mod_1 %>% 
  append(lista_quantidade_produzida_agr_mod_2) %>% 
  map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x ")
  ) %>% 
  mutate(
    Produto = Produto %>% str_replace_all("[[:punct:]]Tonelada", "__Tonelada"),
    Produto = Produto %>% str_replace_all("[[:punct:]]Mil", "__Mil"),
    Produto = Produto %>% str_replace_all("[[:punct:]]Cabeças", "__Cabeças"),
    Produto = Produto %>% str_replace_all("[[:punct:]]não", "__não")
  ) %>% 
  separate(Produto, c("Produto", "UMedida"), sep = "__") %>% 
  mutate(
    Produto = Produto %>% str_trim(),
    UMedida = UMedida %>% str_remove_all("[[:punct:]]")
  ) %>% 
  select(cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade"))

# -------------------------------------------------------------------------

rm(
  path_agricultura_fam_sim, path_pecuaria_fam_sim,
  lista_quantidade_produzida_agr, lista_quantidade_produzida_agr_4,
  lista_quantidade_produzida_agr_mod_1, lista_quantidade_produzida_agr_mod_2,
  lista_quantidade_produzida_agr_mod_2_aux, pos_quantidade_produzida_pec,
  lista_quantidade_produzida_pec, lista_quantidade_produzida_pec_aux
)
gc()

# Manipulação dos dados - Valor da Produção -------------------------------

# Padronizando para o loop
lista_valor_da_producao_agr_4 <- lista_valor_da_producao_agr[[4]]
lista_valor_da_producao_agr[[4]] <- NULL
lista_valor_da_producao_agr[[5]] %<>% .[-6,]

# Loops
lista_valor_da_producao_agr_mod_1 <- 
  lista_valor_da_producao_agr %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(
        Tipologia = "Agricultura Familiar Sim",
        Grupo = .x[[2,4]],
        UMedida = .x[[1,1]]
      ) %>% 
      pivot_longer(!c("Municipio", "Grupo", "Tipologia", "UMedida"), names_to = "Produto", values_to = "Quantidade")
  )

lista_valor_da_producao_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Sim")
lista_valor_da_producao_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Sim"))

lista_valor_da_producao_agr_mod_2_aux <- list()
lista_valor_da_producao_agr_mod_2_aux[[1]] <- lista_valor_da_producao_agr_4

lista_valor_da_producao_agr_mod_2 <- 
  lista_valor_da_producao_agr_mod_2_aux %>% 
  append(lista_valor_da_producao_pec) %>% 
  map(~ .x %>% 
        select(-2) %>% 
        `colnames<-`(c("Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
        mutate(
          Grupo = .[[2,2]],
          UMedida = .[[1,1]]
        ) %>% 
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "UMedida"), names_to = "Produto", values_to = "Quantidade")
  )

# Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
valor_da_producao_fam_sim <- 
  lista_valor_da_producao_agr_mod_1 %>% 
  append(lista_valor_da_producao_agr_mod_2) %>% 
  map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x "),
    UMedida = UMedida %>% sub(".*\\(", "", .) %>% str_remove_all("[[:punct:]]")
  ) %>% 
  select(cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade"))

# -------------------------------------------------------------------------

rm(
  cod_ibge, lista_valor_da_producao_agr, lista_valor_da_producao_agr_4,
  lista_valor_da_producao_agr_mod_1, lista_valor_da_producao_agr_mod_2,
  lista_valor_da_producao_agr_mod_2_aux, pos_valor_da_producao_pec,
  lista_valor_da_producao_pec, lista_valor_da_producao_pec_aux
)
gc()

# -------------------------------------------------------------------------
