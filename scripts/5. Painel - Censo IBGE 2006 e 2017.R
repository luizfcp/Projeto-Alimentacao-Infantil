
#################################################################
# Construção do Painel - IBGE 2006 e 2017                       #
#################################################################

# Pacotes -----------------------------------------------------------------

library(readxl)
library(janitor)
library(tidyr)
library(purrr)
library(magrittr)
library(stringr)
library(dplyr)

`%notin%` <- Negate(`%in%`)

options(scipen = 999999999)

# Dados -------------------------------------------------------------------

# Codigo IBGE
cod_ibge <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(1, mun_uf, regiao) %>% `colnames<-`(c("cod_ibge", "mun_uf", "regiao"))

# Integração Censos
int_censo <- read_excel("data/IBGE/IntegraçãoCensosAgropecuarios 2006 e 2017.xlsx") %>% clean_names() %>% select(2, 5, 3, 4)

# 2006 e 2017 - Censo População
data_populacao <- read_excel("data/IBGE/Populacao.xls") %>%
  clean_names() %>%
  mutate(
    municipios = municipios %>% str_to_lower(),
    ufmun = paste(uf, "__", municipios),
    codmun = paste0(cod_uf, cod_munic) %>% as.numeric(),
  ) %>%
  select(ufmun, codmun, populacao_2006, populacao_2017)

################################## 2017 ################################### 

# ----------------------------------------------------------------------- #
########################## 2017 - Familiar Não ############################
# ----------------------------------------------------------------------- #

{
  # Caminho dos arquivos para leitura
  path_agricultura_fam_nao <- list.files("data/IBGE/Dados 2017/1. Agricultura/3. Dados Agricultura Familiar Nao/", full.names = T)
  path_pecuaria_fam_nao <- list.files("data/IBGE/Dados 2017/2. Pecuária/3. Dados Pecuária Familiar Nao/", full.names = T)
  
  # Conferir nome das Sheets
  map(path_agricultura_fam_nao, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro
  map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets()) # Os arquivos possuem ordem variadas
  
  # Leitura das bases de dados - Agricultura
  lista_quantidade_produzida_agr <- map(path_agricultura_fam_nao, ~ .x %>% read_excel(sheet = 1))
  lista_valor_da_producao_agr <- map(path_agricultura_fam_nao, ~ .x %>% read_excel(sheet = 2))
  
  # Leitura das bases de dados - Pecuaria - Quantidade Produzida
  pos_quantidade_produzida_pec <- # Posicionamento das sheets "Quantidade"
    map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets() %>% str_detect("Número|Quantidade|Tabela 1|Tabela 3") %>% which())
  
  lista_quantidade_produzida_pec_aux <- 
    map2(
      path_pecuaria_fam_nao, pos_quantidade_produzida_pec, function(path, pos) {
        map(pos, ~ read_excel(path, sheet = .x))
      }
    )
  
  lista_quantidade_produzida_pec <- lista_quantidade_produzida_pec_aux[[1]] %>% 
    append(lista_quantidade_produzida_pec_aux[[2]]) %>% 
    append(lista_quantidade_produzida_pec_aux[[3]])
  
  # Leitura das bases de dados - Pecuaria - Valor da Producao
  pos_valor_da_producao_pec <- # Posicionamento das sheets "Valor"
    map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets() %>% str_detect("Valor|Tabela 2|Tabela 4") %>% which())
  
  lista_valor_da_producao_pec_aux <- 
    map2(
      path_pecuaria_fam_nao, pos_valor_da_producao_pec, function(path, pos) {
        map(pos, ~ read_excel(path, sheet = .x))
      }
    )
  
  lista_valor_da_producao_pec <- lista_valor_da_producao_pec_aux[[1]] %>% 
    append(lista_valor_da_producao_pec_aux[[2]]) %>% 
    append(lista_valor_da_producao_pec_aux[[3]])
  
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
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[5,][-c(1:2)])) %>%
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Não",
          Grupo = .x[[2,4]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_quantidade_produzida_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Não")
  lista_quantidade_produzida_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Não"))
  
  lista_quantidade_produzida_agr_mod_2_aux <- list()
  lista_quantidade_produzida_agr_mod_2_aux[[1]] <- lista_quantidade_produzida_agr_4
  
  lista_quantidade_produzida_agr_mod_2 <- 
    lista_quantidade_produzida_agr_mod_2_aux %>% 
    append(lista_quantidade_produzida_pec) %>% 
    map(~ .x %>% 
          select(-2) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(Grupo = .[[2,3]]) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  quantidade_produzida_fam_nao_2017 <- 
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
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Qt", "Quantidade")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de galinhas, galos, frangas, frangos e pintos vendidas nos estabelecimentos agropecuários" ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários"  ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Número de cabeças de suínos vendidas nos estabelecimentos agropecuários"                                     ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de suínos nos estabelecimentos agropecuários"                                      ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Quantidade produzida de leite de vaca"                                                                       ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"                                                                          ~ "Leite de vaca",
        Produto=="Quantidade vendida de ovos de galinhas"                                                                      ~ "Ovos de galinhas",
        Produto=="Valor da venda dos ovos de galinhas"                                                                         ~ "Ovos de galinhas",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    path_agricultura_fam_nao, path_pecuaria_fam_nao,
    lista_quantidade_produzida_agr, lista_quantidade_produzida_agr_4,
    lista_quantidade_produzida_agr_mod_1, lista_quantidade_produzida_agr_mod_2,
    lista_quantidade_produzida_agr_mod_2_aux, pos_quantidade_produzida_pec,
    lista_quantidade_produzida_pec, lista_quantidade_produzida_pec_aux
  )
  gc()
  
  # -------------------------------------------------------------------------
  
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
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[5,][-c(1:2)])) %>%
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Não",
          Grupo = .x[[2,4]],
          UMedida = .x[[1,1]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_valor_da_producao_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Não")
  lista_valor_da_producao_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Não"))
  
  lista_valor_da_producao_agr_mod_2_aux <- list()
  lista_valor_da_producao_agr_mod_2_aux[[1]] <- lista_valor_da_producao_agr_4
  
  lista_valor_da_producao_agr_mod_2 <- 
    lista_valor_da_producao_agr_mod_2_aux %>% 
    append(lista_valor_da_producao_pec) %>% 
    map(~ .x %>% 
          select(-2) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(
            Grupo = .[[2,3]],
            UMedida = .[[1,2]]
          ) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  valor_da_producao_fam_nao_2017 <- 
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
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Val", "Valor da Produção")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de galinhas, galos, frangas, frangos e pintos vendidas nos estabelecimentos agropecuários" ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários"  ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Número de cabeças de suínos vendidas nos estabelecimentos agropecuários"                                     ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de suínos nos estabelecimentos agropecuários"                                      ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Quantidade produzida de leite de vaca"                                                                       ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"                                                                          ~ "Leite de vaca",
        Produto=="Quantidade vendida de ovos de galinhas"                                                                      ~ "Ovos de galinhas",
        Produto=="Valor da venda dos ovos de galinhas"                                                                         ~ "Ovos de galinhas",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    lista_valor_da_producao_agr, lista_valor_da_producao_agr_4,
    lista_valor_da_producao_agr_mod_1, lista_valor_da_producao_agr_mod_2,
    lista_valor_da_producao_agr_mod_2_aux, pos_valor_da_producao_pec,
    lista_valor_da_producao_pec, lista_valor_da_producao_pec_aux
  )
  gc()
  
}

# ----------------------------------------------------------------------- #
########################## 2017 - Familiar Sim ############################
# ----------------------------------------------------------------------- #

{
  
  # Caminho dos arquivos para leitura
  path_agricultura_fam_sim <- list.files("data/IBGE/Dados 2017/1. Agricultura/2. Dados Agricultura Familiar Sim/", full.names = T)
  path_pecuaria_fam_sim <- list.files("data/IBGE/Dados 2017/2. Pecuária/2. Dados Pecuária Familiar Sim/", full.names = T)
  
  # Conferir nome das Sheets
  map(path_agricultura_fam_sim, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro
  map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets()) # Os arquivos possuem ordem variadas
  
  # Leitura das bases de dados - Agricultura
  lista_quantidade_produzida_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 1))
  lista_valor_da_producao_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 2))
  
  # Leitura das bases de dados - Pecuaria - Quantidade Produzida
  pos_quantidade_produzida_pec <- # Posicionamento das sheets "Quantidade"
    map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Número|Quantidade|Tabela 1|Tabela 3") %>% which())
  
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
    map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Valor|Tabela 2|Tabela 4") %>% which())
  
  lista_valor_da_producao_pec_aux <- 
    map2(
      path_pecuaria_fam_sim, pos_valor_da_producao_pec, function(path, pos) {
        map(pos, ~ read_excel(path, sheet = .x))
      }
    )
  
  lista_valor_da_producao_pec <- lista_valor_da_producao_pec_aux[[1]] %>% 
    append(lista_valor_da_producao_pec_aux[[2]]) %>% 
    append(lista_valor_da_producao_pec_aux[[3]])
  
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
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[5,][-c(1:2)])) %>%
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Sim",
          Grupo = .x[[2,4]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
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
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(Grupo = .[[2,3]]) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municipio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  quantidade_produzida_fam_sim_2017 <- 
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
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Qt", "Quantidade")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de galinhas, galos, frangas, frangos e pintos vendidas nos estabelecimentos agropecuários" ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários"  ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Número de cabeças de suínos vendidas nos estabelecimentos agropecuários"                                     ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de suínos nos estabelecimentos agropecuários"                                      ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Quantidade produzida de leite de vaca"                                                                       ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"                                                                          ~ "Leite de vaca",
        Produto=="Quantidade vendida de ovos de galinhas"                                                                      ~ "Ovos de galinhas",
        Produto=="Valor da venda dos ovos de galinhas"                                                                         ~ "Ovos de galinhas",
        TRUE ~ Produto
      )
    )
  
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
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[5,][-c(1:2)])) %>%
        .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Sim",
          Grupo = .x[[2,4]],
          UMedida = .x[[1,1]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
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
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(
            Grupo = .[[2,3]],
            UMedida = .[[1,2]]
          ) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  valor_da_producao_fam_sim_2017 <- 
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
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Val", "Valor da Produção")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"             ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças"              ~ "Cabeças de bovinos nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com 50 cabeças e menos"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com 50 cabeças e menos",
        Produto=="Número de cabeças de bovinos para abate vendidas nos estabelecimentos agropecuários com mais de 50 cabeças"  ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Valor da venda de cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças"   ~ "Cabeças de bovinos para abate nos estabelecimentos agropecuários com mais de 50 cabeças",
        Produto=="Número de cabeças de galinhas, galos, frangas, frangos e pintos vendidas nos estabelecimentos agropecuários" ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários"  ~ "Cabeças de galinhas, galos, frangas, frangos e pintos nos estabelecimentos agropecuários",
        Produto=="Número de cabeças de suínos vendidas nos estabelecimentos agropecuários"                                     ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Valor da venda de cabeças de suínos nos estabelecimentos agropecuários"                                      ~ "Cabeças de suínos nos estabelecimentos agropecuários",
        Produto=="Quantidade produzida de leite de vaca"                                                                       ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"                                                                          ~ "Leite de vaca",
        Produto=="Quantidade vendida de ovos de galinhas"                                                                      ~ "Ovos de galinhas",
        Produto=="Valor da venda dos ovos de galinhas"                                                                         ~ "Ovos de galinhas",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    lista_valor_da_producao_agr, lista_valor_da_producao_agr_4,
    lista_valor_da_producao_agr_mod_1, lista_valor_da_producao_agr_mod_2,
    lista_valor_da_producao_agr_mod_2_aux, pos_valor_da_producao_pec,
    lista_valor_da_producao_pec, lista_valor_da_producao_pec_aux
  )
  gc()
  
}

# ----------------------------------------------------------------------- #
############################# 2017 - Painel ###############################
# ----------------------------------------------------------------------- #

fam_sim_2017 <- quantidade_produzida_fam_sim_2017 %>% 
  left_join(
    valor_da_producao_fam_sim_2017, 
    by = c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto")
  )

fam_nao_2017 <- quantidade_produzida_fam_nao_2017 %>% 
  left_join(
    valor_da_producao_fam_nao_2017, 
    by = c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto")
  )

painel_2017 <- fam_sim_2017 %>% 
  bind_rows(fam_nao_2017) %>% 
  mutate(
    num_tabela = num_tabela %>% as.numeric(),
    censo = 2017,
  ) %>% 
  left_join(int_censo, by = c("num_tabela" = "numero_das_tabelas_extraidas_do_censo_agropecuaria_de_2017")) %>% 
  left_join(data_populacao, by = c("Código IBGE"="codmun")) %>% 
  select(Tipologia, regiao, `Estado (UF)`, `Nome do Município`, `Código IBGE`, censo, Produto, setores, grupos_do_produto, 
         `Valor da Produção`, `Unidade de Medida Val`, Quantidade, `Unidade de Medida Qt`, populacao_2017) %>% 
  rename("População 2017"=populacao_2017)

# Removendo unidades de medida
painel_2017 <- painel_2017 %>% filter(`Unidade de Medida Qt` %notin% c("não se aplica", "Mil unidades", "Cabeças", "Mil cabeças")) 

# Separando os dados de quantidade para trabalhar com os numericos
painel_2017_aux_chr <- painel_2017 %>% filter(Quantidade %in% c("-", "X"))
painel_2017_aux_num <- painel_2017 %>% filter(Quantidade %notin% c("-", "X")) %>% mutate(Quantidade = as.numeric(Quantidade))

painel_2017 <- painel_2017_aux_num %>% 
  mutate(
    Quantidade = case_when(
      # Mil duzias
      `Unidade de Medida Qt`=="Mil dúzias" & Produto=="Ostras/vieiras"                         ~ Quantidade*0.0006,
      `Unidade de Medida Qt`=="Mil dúzias" & Produto=="Quantidade vendida de ovos de galinhas" ~ Quantidade*0.001,
      # Quilograma
      `Unidade de Medida Qt`=="Quilograma" ~ Quantidade*0.001,
      # Mil metros cubicos
      `Unidade de Medida Qt`=="Mil metros cúbicos" ~ Quantidade*0.001,
      # Mil frutos
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Abacaxi"      ~ Quantidade*0.00145,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Coco-da-baía" ~ Quantidade*0.0023,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Graviola"     ~ Quantidade*0.002,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Jaca"         ~ Quantidade*0.012,
      TRUE ~ Quantidade
    )
  ) %>% 
  mutate(Quantidade = as.character(Quantidade)) %>% 
  bind_rows(painel_2017_aux_chr) %>% 
  mutate(`Unidade de Medida Qt` = "Toneladas")


# -------------------------------------------------------------------------

rm(
  fam_nao_2017, fam_sim_2017, quantidade_produzida_fam_nao_2017, quantidade_produzida_fam_sim_2017,
  valor_da_producao_fam_nao_2017, valor_da_producao_fam_sim_2017, painel_2017_aux_chr, painel_2017_aux_num
)

gc()


################################## 2006 ################################### 

# ----------------------------------------------------------------------- #
########################## 2006 - Familiar Não ############################
# ----------------------------------------------------------------------- #

{
  # Caminho dos arquivos para leitura
  path_agricultura_fam_nao <- list.files("data/IBGE/Dados 2006/1. Agricultura/3. Dados Agricultura Familiar Nao/", full.names = T)
  #path_pecuaria_fam_nao <- list.files("data/IBGE/Dados 2006/2. Pecuária/3. Dados Pecuária Familiar Nao/", full.names = T)
  
  # Conferir nome das Sheets
  map(path_agricultura_fam_nao, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro
  #map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets()) # Os arquivos possuem ordem variadas
  
  # Leitura das bases de dados - Agricultura
  lista_quantidade_produzida_agr <- map(path_agricultura_fam_nao, ~ .x %>% read_excel(sheet = 1))
  lista_valor_da_producao_agr <- map(path_agricultura_fam_nao, ~ .x %>% read_excel(sheet = 2))
  
  # # Leitura das bases de dados - Pecuaria - Quantidade Produzida
  # pos_quantidade_produzida_pec <- # Posicionamento das sheets "Quantidade"
  #   map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets() %>% str_detect("Número|Quantidade|Tabela 1|Tabela 3") %>% which())
  # 
  # lista_quantidade_produzida_pec_aux <- 
  #   map2(
  #     path_pecuaria_fam_nao, pos_quantidade_produzida_pec, function(path, pos) {
  #       map(pos, ~ read_excel(path, sheet = .x))
  #     }
  #   )
  # 
  # lista_quantidade_produzida_pec <- lista_quantidade_produzida_pec_aux[[1]] %>% 
  #   append(lista_quantidade_produzida_pec_aux[[2]]) %>% 
  #   append(lista_quantidade_produzida_pec_aux[[3]])
  # 
  # # Leitura das bases de dados - Pecuaria - Valor da Producao
  # pos_valor_da_producao_pec <- # Posicionamento das sheets "Valor"
  #   map(path_pecuaria_fam_nao, ~ .x %>% excel_sheets() %>% str_detect("Valor|Tabela 2|Tabela 4") %>% which())
  # 
  # lista_valor_da_producao_pec_aux <- 
  #   map2(
  #     path_pecuaria_fam_nao, pos_valor_da_producao_pec, function(path, pos) {
  #       map(pos, ~ read_excel(path, sheet = .x))
  #     }
  #   )
  # 
  # lista_valor_da_producao_pec <- lista_valor_da_producao_pec_aux[[1]] %>% 
  #   append(lista_valor_da_producao_pec_aux[[2]]) %>% 
  #   append(lista_valor_da_producao_pec_aux[[3]])
  
  # Manipulação dos dados - Quantidade Produzida ----------------------------
  
  # Padronizando para o loop
  lista_quantidade_produzida_agr_4 <- lista_quantidade_produzida_agr[[4]]
  lista_quantidade_produzida_agr[[1]] %<>% .[-c(4:5),]
  lista_quantidade_produzida_agr[[2]] %<>% .[-c(4:5),]
  lista_quantidade_produzida_agr[[3]] %<>% .[-c(4:5),]
  lista_quantidade_produzida_agr[[6]] %<>% .[-c(4,6),]
  lista_quantidade_produzida_agr[[4]] <- NULL
  
  # Loops
  lista_quantidade_produzida_agr_mod_1 <- 
    lista_quantidade_produzida_agr %>%
    map(
      ~ .x %>% 
        select(-c(2:5)) %>% 
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[4,][-c(1:2)])) %>%
        .[-c(1:4),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Não",
          Grupo = .x[[2,5]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_quantidade_produzida_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Não")
  #lista_quantidade_produzida_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Não"))
  
  lista_quantidade_produzida_agr_mod_2_aux <- list()
  lista_quantidade_produzida_agr_mod_2_aux[[1]] <- lista_quantidade_produzida_agr_4
  
  lista_quantidade_produzida_agr_mod_2 <- 
    lista_quantidade_produzida_agr_mod_2_aux %>% 
    #append(lista_quantidade_produzida_pec) %>% 
    map(~ .x %>% 
          select(-c(2:3)) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(Grupo = .[[2,3]]) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  quantidade_produzida_fam_nao_2006 <- 
    lista_quantidade_produzida_agr_mod_1 %>% 
    append(lista_quantidade_produzida_agr_mod_2) %>% 
    map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
    bind_rows() %>% 
    separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
    mutate(
      Estado = Estado %>% str_remove_all("[[:punct:]]"),
      Grupo = Grupo %>% str_remove_all("Ano x ")
    ) %>% 
    mutate(
      Produto = Produto %>% str_replace_all("[[:punct:]]Tonelada", "__Tonelada"),
      Produto = Produto %>% str_replace_all("[[:punct:]]Quilogramas", "__Quilogramas"),
      Produto = Produto %>% str_replace_all("[[:punct:]]Mil", "__Mil"),
      Produto = Produto %>% str_replace_all("[[:punct:]]Cabeças", "__Cabeças"),
      Produto = Produto %>% str_replace_all("[[:punct:]]não", "__não")
    ) %>% 
    separate(Produto, c("Produto", "UMedida"), sep = "__") %>% 
    mutate(
      Produto = Produto %>% str_trim(),
      UMedida = UMedida %>% str_remove_all("[[:punct:]]")
    ) %>% 
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Qt", "Quantidade")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]|[[:punct:]]Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Quantidade produzida de leite de vaca" ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"    ~ "Leite de vaca",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    path_agricultura_fam_nao, path_pecuaria_fam_nao,
    lista_quantidade_produzida_agr, lista_quantidade_produzida_agr_4,
    lista_quantidade_produzida_agr_mod_1, lista_quantidade_produzida_agr_mod_2,
    lista_quantidade_produzida_agr_mod_2_aux, pos_quantidade_produzida_pec,
    lista_quantidade_produzida_pec, lista_quantidade_produzida_pec_aux
  )
  gc()
  
  # -------------------------------------------------------------------------
  
  # Manipulação dos dados - Valor da Produção -------------------------------
  
  # Padronizando para o loop
  lista_valor_da_producao_agr_4 <- lista_valor_da_producao_agr[[4]]
  lista_valor_da_producao_agr[[1]] %<>% .[-c(4:5),]
  lista_valor_da_producao_agr[[2]] %<>% .[-c(4:5),]
  lista_valor_da_producao_agr[[3]] %<>% .[-c(4:5),]
  lista_valor_da_producao_agr[[6]] %<>% .[-c(4,6),]
  lista_valor_da_producao_agr[[4]] <- NULL
  
  # Loops
  lista_valor_da_producao_agr_mod_1 <- 
    lista_valor_da_producao_agr %>%
    map(
      ~ .x %>% 
        select(-c(2:5)) %>% 
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[4,][-c(1:2)])) %>%
        .[-c(1:4),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Não",
          Grupo = .x[[2,5]],
          UMedida = .x[[1,1]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_valor_da_producao_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Não")
  #lista_valor_da_producao_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Não"))
  
  lista_valor_da_producao_agr_mod_2_aux <- list()
  lista_valor_da_producao_agr_mod_2_aux[[1]] <- lista_valor_da_producao_agr_4
  
  lista_valor_da_producao_agr_mod_2 <- 
    lista_valor_da_producao_agr_mod_2_aux %>% 
    #append(lista_valor_da_producao_pec) %>% 
    map(~ .x %>% 
          select(-c(2:3)) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(
            Grupo = .x[[2,4]],
            UMedida = .[[1,2]]
          ) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  valor_da_producao_fam_nao_2006 <- 
    lista_valor_da_producao_agr_mod_1 %>% 
    append(lista_valor_da_producao_agr_mod_2) %>% 
    map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
    bind_rows() %>% 
    separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
    mutate(
      Estado = Estado %>% str_remove_all("[[:punct:]]"),
      Grupo = Grupo %>% str_remove_all("Ano x "),
      UMedida = UMedida %>% sub(".*\\(", "", .) %>% str_remove_all("[[:punct:]]")
    ) %>% 
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Val", "Valor da Produção")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]|[[:punct:]]Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Quantidade produzida de leite de vaca" ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"    ~ "Leite de vaca",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    lista_valor_da_producao_agr, lista_valor_da_producao_agr_4,
    lista_valor_da_producao_agr_mod_1, lista_valor_da_producao_agr_mod_2,
    lista_valor_da_producao_agr_mod_2_aux, pos_valor_da_producao_pec,
    lista_valor_da_producao_pec, lista_valor_da_producao_pec_aux
  )
  gc()
  
}

# ----------------------------------------------------------------------- #
########################## 2006 - Familiar Sim ############################
# ----------------------------------------------------------------------- #

{
  
  # Caminho dos arquivos para leitura
  path_agricultura_fam_sim <- list.files("data/IBGE/Dados 2006/1. Agricultura/2. Dados Agricultura Familiar Sim/", full.names = T)
  path_pecuaria_fam_sim <- list.files("data/IBGE/Dados 2006/2. Pecuária/2. Dados Pecuária Familiar Sim/", full.names = T)
  
  # Conferir nome das Sheets
  map(path_agricultura_fam_sim, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro
  #map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets()) # Os arquivos possuem ordem variadas
  
  # Leitura das bases de dados - Agricultura
  lista_quantidade_produzida_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 1))
  lista_valor_da_producao_agr <- map(path_agricultura_fam_sim, ~ .x %>% read_excel(sheet = 2))
  
  # # Leitura das bases de dados - Pecuaria - Quantidade Produzida
  # pos_quantidade_produzida_pec <- # Posicionamento das sheets "Quantidade"
  #   map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Número|Quantidade|Tabela 1|Tabela 3") %>% which())
  # 
  # lista_quantidade_produzida_pec_aux <- 
  #   map2(
  #     path_pecuaria_fam_sim, pos_quantidade_produzida_pec, function(path, pos) {
  #       map(pos, ~ read_excel(path, sheet = .x))
  #     }
  #   )
  # 
  # lista_quantidade_produzida_pec <- lista_quantidade_produzida_pec_aux[[1]] %>% 
  #   append(lista_quantidade_produzida_pec_aux[[2]]) %>% 
  #   append(lista_quantidade_produzida_pec_aux[[3]])
  # 
  # # Leitura das bases de dados - Pecuaria - Valor da Producao
  # pos_valor_da_producao_pec <- # Posicionamento das sheets "Valor"
  #   map(path_pecuaria_fam_sim, ~ .x %>% excel_sheets() %>% str_detect("Valor|Tabela 2|Tabela 4") %>% which())
  # 
  # lista_valor_da_producao_pec_aux <- 
  #   map2(
  #     path_pecuaria_fam_sim, pos_valor_da_producao_pec, function(path, pos) {
  #       map(pos, ~ read_excel(path, sheet = .x))
  #     }
  #   )
  # 
  # lista_valor_da_producao_pec <- lista_valor_da_producao_pec_aux[[1]] %>% 
  #   append(lista_valor_da_producao_pec_aux[[2]]) %>% 
  #   append(lista_valor_da_producao_pec_aux[[3]])
  
  # Manipulação dos dados - Quantidade Produzida ----------------------------
  
  # Padronizando para o loop
  lista_quantidade_produzida_agr_4 <- lista_quantidade_produzida_agr[[4]]
  lista_quantidade_produzida_agr[[1]] %<>% .[-c(4:5),] %>% mutate(temp = ...5) %>% select(1:4, temp, everything())
  lista_quantidade_produzida_agr[[6]] %<>% .[-4,] %>% mutate(temp = ...5) %>% select(1:4, temp, everything())
  lista_quantidade_produzida_agr[[4]] <- NULL
  
  # Loops
  lista_quantidade_produzida_agr_mod_1 <- 
    lista_quantidade_produzida_agr %>%
    map(
      ~ .x %>% 
        select(-c(2:5)) %>% 
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[4,][-c(1:2)])) %>%
        .[-c(1:4),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Sim",
          Grupo = .x[[2,5]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_quantidade_produzida_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Sim")
  #lista_quantidade_produzida_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Sim"))
  
  lista_quantidade_produzida_agr_mod_2_aux <- list()
  lista_quantidade_produzida_agr_mod_2_aux[[1]] <- lista_quantidade_produzida_agr_4
  
  lista_quantidade_produzida_agr_mod_2 <- 
    lista_quantidade_produzida_agr_mod_2_aux %>% 
    #append(lista_quantidade_produzida_pec) %>% 
    map(~ .x %>% 
          select(-c(2:3)) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(Grupo = .[[2,3]]) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municipio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  quantidade_produzida_fam_sim_2006 <- 
    lista_quantidade_produzida_agr_mod_1 %>% 
    append(lista_quantidade_produzida_agr_mod_2) %>% 
    map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
    bind_rows() %>% 
    separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
    mutate(
      Estado = Estado %>% str_remove_all("[[:punct:]]"),
      Grupo = Grupo %>% str_remove_all("Ano x ")
    ) %>% 
    mutate(
      Produto = Produto %>% str_replace_all("[[:punct:]]Quilogramas", "__Quilogramas"),
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
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Qt", "Quantidade")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]|[[:punct:]]Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Quantidade produzida de leite de vaca" ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"    ~ "Leite de vaca",
        TRUE ~ Produto
      )
    )
  
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
  lista_valor_da_producao_agr[[1]] %<>% .[-c(4:5),] %>% mutate(temp = ...5) %>% select(1:4, temp, everything())
  lista_valor_da_producao_agr[[6]] %<>% .[-c(4,6),] %>% mutate(temp = ...5) %>% select(1:4, temp, everything())
  lista_valor_da_producao_agr[[4]] <- NULL
  
  # Loops
  lista_valor_da_producao_agr_mod_1 <- 
    lista_valor_da_producao_agr %>%
    map(
      ~ .x %>% 
        select(-c(2:5)) %>% 
        mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
        select(num_tabela, 1, everything()) %>% 
        `colnames<-`(., c("num_tabela", "Municipio", .[4,][-c(1:2)])) %>%
        .[-c(1:4),] %>%  # Removendo linhas iniciais que nao serao utilizadas
        .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
        mutate(
          Tipologia = "Agricultura Familiar Sim",
          Grupo = .x[[2,5]],
          UMedida = .x[[1,1]]
        ) %>% 
        pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  lista_valor_da_producao_agr_4 %<>% mutate(Tipologia = "Agricultura Familiar Sim")
  #lista_valor_da_producao_pec %<>% map(~ .x %>% mutate(Tipologia = "Pecuária Familiar Sim"))
  
  lista_valor_da_producao_agr_mod_2_aux <- list()
  lista_valor_da_producao_agr_mod_2_aux[[1]] <- lista_valor_da_producao_agr_4
  
  lista_valor_da_producao_agr_mod_2 <- 
    lista_valor_da_producao_agr_mod_2_aux %>% 
    #append(lista_valor_da_producao_pec) %>% 
    map(~ .x %>% 
          select(-c(2:3)) %>% 
          mutate(num_tabela = str_extract(names(.x[,1]), "[:digit:]{1,}")) %>% 
          select(num_tabela, 1, everything()) %>% 
          `colnames<-`(c("num_tabela", "Municipio", str_remove(.x[1,1], "Variável - "), "Tipologia")) %>% 
          mutate(
            Grupo = .[[2,3]],
            UMedida = .[[1,2]]
          ) %>% 
          .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
          .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
          pivot_longer(!c("Municipio", "Grupo", "Tipologia", "num_tabela", "UMedida"), names_to = "Produto", values_to = "Quantidade")
    )
  
  # Separando Municio de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
  valor_da_producao_fam_sim_2006 <- 
    lista_valor_da_producao_agr_mod_1 %>% 
    append(lista_valor_da_producao_agr_mod_2) %>% 
    map(~ .x %>% left_join(cod_ibge, by = c("Municipio"="mun_uf"))) %>% 
    bind_rows() %>% 
    separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
    mutate(
      Estado = Estado %>% str_remove_all("[[:punct:]]"),
      Grupo = Grupo %>% str_remove_all("Ano x "),
      UMedida = UMedida %>% sub(".*\\(", "", .) %>% str_remove_all("[[:punct:]]")
    ) %>% 
    select(num_tabela, regiao, cod_ibge, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
    `colnames<-`(c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida Val", "Valor da Produção")) %>% 
    mutate(
      Produto = Produto %>% str_remove_all("[[:punct:]]Mil Reais[[:punct:]]|[[:punct:]]Reais[[:punct:]]") %>% str_trim(),
      Produto = case_when(
        Produto=="Quantidade produzida de leite de vaca" ~ "Leite de vaca",
        Produto=="Valor da produção de leite de vaca"    ~ "Leite de vaca",
        TRUE ~ Produto
      )
    )
  
  # -------------------------------------------------------------------------
  
  rm(
    lista_valor_da_producao_agr, lista_valor_da_producao_agr_4,
    lista_valor_da_producao_agr_mod_1, lista_valor_da_producao_agr_mod_2,
    lista_valor_da_producao_agr_mod_2_aux, pos_valor_da_producao_pec,
    lista_valor_da_producao_pec, lista_valor_da_producao_pec_aux
  )
  gc()
  
}

# ----------------------------------------------------------------------- #
############################# 2006 - Painel ###############################
# ----------------------------------------------------------------------- #

fam_sim_2006 <- quantidade_produzida_fam_sim_2006 %>% 
  left_join(
    valor_da_producao_fam_sim_2006, 
    by = c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto")
  )

fam_nao_2006 <- quantidade_produzida_fam_nao_2006 %>% 
  left_join(
    valor_da_producao_fam_nao_2006, 
    by = c("num_tabela", "regiao", "Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto")
  )

painel_2006 <- fam_sim_2006 %>% 
  bind_rows(fam_nao_2006) %>% 
  mutate(
    num_tabela = num_tabela %>% as.numeric(),
    censo = 2006,
  ) %>% 
  left_join(int_censo, by = c("num_tabela" = "numero_das_tabelas_extraidas_do_censo_agropecuaria_de_2006")) %>% 
  left_join(data_populacao, by = c("Código IBGE"="codmun")) %>% 
  select(Tipologia, regiao, `Estado (UF)`, `Nome do Município`, `Código IBGE`, censo, Produto, setores, grupos_do_produto, 
         `Valor da Produção`, `Unidade de Medida Val`, Quantidade, `Unidade de Medida Qt`, populacao_2006) %>% 
  rename("População 2006"=populacao_2006)

# Removendo unidades de medida
painel_2006 <- painel_2006 %>% filter(`Unidade de Medida Qt` %notin% c("não se aplica", "Mil unidades", "Cabeças", "Mil cabeças")) 

# Separando os dados de quantidade para trabalhar com os numericos
painel_2006_aux_chr <- painel_2006 %>% filter(Quantidade %in% c("-", "X"))
painel_2006_aux_num <- painel_2006 %>% filter(Quantidade %notin% c("-", "X")) %>% mutate(Quantidade = as.numeric(Quantidade))

painel_2006 <- painel_2006_aux_num %>% 
  mutate(
    Quantidade = case_when(
      # Quilograma
      `Unidade de Medida Qt`=="Quilograma" ~ Quantidade*0.001,
      # Mil metros cubicos
      `Unidade de Medida Qt`=="Mil metros cúbicos" ~ Quantidade*0.001,
      # Mil frutos
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Abacaxi"      ~ Quantidade*0.00145,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Coco-da-baía" ~ Quantidade*0.0023,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Graviola"     ~ Quantidade*0.002,
      `Unidade de Medida Qt`=="Mil frutos" & Produto=="Jaca"         ~ Quantidade*0.012,
      TRUE ~ Quantidade
    )
  ) %>% 
  mutate(Quantidade = as.character(Quantidade)) %>% 
  bind_rows(painel_2006_aux_chr) %>% 
  mutate(`Unidade de Medida Qt` = "Toneladas")

# -------------------------------------------------------------------------

rm(
  fam_nao_2006, fam_sim_2006, quantidade_produzida_fam_nao_2006, quantidade_produzida_fam_sim_2006,
  valor_da_producao_fam_nao_2006, valor_da_producao_fam_sim_2006, cod_ibge, int_censo, data_populacao,
  painel_2006_aux_chr, painel_2006_aux_num
)

gc()

# ----------------------------------------------------------------------- #
############################## Painel Final ###############################
# ----------------------------------------------------------------------- #

# painel <- painel_2006 %>% 
#   bind_rows(painel_2017) %>% 
#   arrange(Tipologia, regiao, `Código IBGE`, Produto, censo) %>% 
#   `colnames<-`(
#     c("Tipologia", "Região", "Estado", "Municípios", "Código IBGE do Município", "Censo", "Produto", "Setor do produto", "Grupo do Produto", 
#       "Valor da produção", "Unidade de Medida do valor da produção", "Quantidade produzida", "Unidade de Medida da Quantidade produzida")
#   )
# 
# # -------------------------------------------------------------------------
# 
# rm(painel_2006, painel_2017)
# 
# gc()
# 
# # -------------------------------------------------------------------------
