
#################################################################
# Construção do Painel - Censo Educacional 2006 e 2017          #
#################################################################

# Pacotes -----------------------------------------------------------------

library(readxl)
library(readr)
library(janitor)
library(tidyr)
library(purrr)
library(magrittr)
library(stringr)
library(dplyr)
library(abjutils)

# Dados -------------------------------------------------------------------

# Codigo IBGE
cod_ibge_ufre <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(uf, regiao) %>% `colnames<-`(c("Estado_Sigla", "Regiao")) %>% distinct_all()
cod_ibge_mun <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(mun, 1) %>% `colnames<-`(c("NO_MUNICIPIO", "CO_MUNICIPIO")) %>% distinct_all() %>% 
  mutate(NO_MUNICIPIO = NO_MUNICIPIO %>% rm_accent() %>% str_to_lower())
cod_ibge_micro <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(1, micro, cod_micro) %>% `colnames<-`(c("Codigo_Municipio", "Microrregiao", "Codigo_Microrregiao"))

# 2006 - Filtro para o Censo Educacional
filtro_2006 <- read_excel("data/Dados Educacionais/Censo 2006/Filtro_2006.xlsx") %>% 
  clean_names() %>% 
  select(1,2) %>% 
  filter(!is.na(variaveis))

# 2006 - Censo Educacional
censo_educ_2006 <- read_delim(
  "data/Dados Educacionais/Censo 2006/CENSOESC_2006.CSV", delim = "|", escape_double = FALSE, trim_ws = TRUE
) %>%
  select(c(SIGLA, MUNIC, ANO, filtro_2006$variaveis)) %>% 
  filter(DEP=="Municipal")

# 2017 - Filtro para o Censo Educacional
filtro_2017 <- read_excel("data/Dados Educacionais/Censo 2017/Filtro_2017.xlsx") %>% clean_names()

# 2017 - Censo Educacional
censo_educ_2017 <- read.csv("data/Dados Educacionais/Censo 2017/Censo.Ed.Basica_2017.csv", sep=";") %>%
  as_tibble() %>% 
  select(-NO_MUNICIPIO) %>% 
  left_join(cod_ibge_mun, by = "CO_MUNICIPIO") %>% 
  select(c(NO_REGIAO, SG_UF, NO_MUNICIPIO, CO_MUNICIPIO, NU_ANO_CENSO, filtro_2017$variaveis)) %>% 
  filter(TP_DEPENDENCIA==3)

# -------------------------------------------------------------------------

rm(filtro_2006, filtro_2017)

gc()

# -------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
###################### Painel Censo Educacional 2006 ######################
# ----------------------------------------------------------------------- #

censo_educ_2006 <- censo_educ_2006 %>% 
  mutate_at(c("ENER_PUB", "COZINHA", "REFEITOR"), ~ ifelse(.x=="s", 1, 0)) %>% 
  mutate_at(5:43, ~ ifelse(is.na(.x), 0, .x) %>% as.numeric()) %>% 
  mutate(
    # Padronização: Soma de variaveis
    Educacao_Basica = DPE119+NPE119+DPE11D+NPE11D+DEF11C+DEF11D+DEF11E+DEF11F+NEF11C+NEF11D+NEF11E+NEF11F+DEF11G+DEF11H+DEF11I+DEF11J+NEF11G+
      NEF11H+NEF11I+NEF11J+DEM118+DEM119+DEM11A+NEM118+NEM119+NEM11A+DES101F+DES101G+DES101H+DES101I+NES101F+NES101G+NES101H+NES101I+DES101A+NES101A,
    Educacao_Infantil = DPE119+NPE119+DPE11D+NPE11D,
    Educacao_Infantil_Creche = DPE119+NPE119,
    Educacao_Infantil_PreEscola = DPE11D+NPE11D,
    Educacao_Fundamental = DEF11C+DEF11D+DEF11E+DEF11F+NEF11C+NEF11D+NEF11E+NEF11F+DEF11G+DEF11H+DEF11I+DEF11J+NEF11G+NEF11H+NEF11I+NEF11J,
    Educacao_Fundamental_AI = DEF11C+DEF11D+DEF11E+DEF11F+NEF11C+NEF11D+NEF11E+NEF11F,
    Educacao_Fundamental_AF = DEF11G+DEF11H+DEF11I+DEF11J+NEF11G+NEF11H+NEF11I+NEF11J,
    Educacao_Medio = DEM118+DEM119+DEM11A+NEM118+NEM119+NEM11A,
    Educacao_EJA = DES101F+DES101G+DES101H+DES101I+NES101F+NES101G+NES101H+NES101I+DES101A+NES101A,
    Educacao_EJA_Fundamental = DES101F+DES101G+DES101H+DES101I+NES101F+NES101G+NES101H+NES101I,
    Educacao_EJA_Medio = DES101A+NES101A
  ) %>% 
  rename(c(Energia_Eletrica_Publica=ENER_PUB, Cozinha=COZINHA, Refeitorio=REFEITOR, Censo=ANO)) %>% 
  select(
    SIGLA, MUNIC, Censo, DEP, Energia_Eletrica_Publica, Cozinha, Refeitorio, Educacao_Basica, Educacao_Infantil, Educacao_Infantil_Creche, Educacao_Infantil_PreEscola, 
    Educacao_Fundamental, Educacao_Fundamental_AI, Educacao_Fundamental_AF, Educacao_Medio, Educacao_EJA, Educacao_EJA_Fundamental, Educacao_EJA_Medio
  )

# Calculo das variaveis por municipio
censo_educ_2006 <- censo_educ_2006 %>% 
  group_by(SIGLA, MUNIC, Censo, DEP) %>% 
  summarise(
    # Binarias: Energia Eletrica Publica, Cozinha, Refeitorio
    Energia_Eletrica_Publica = sum(Energia_Eletrica_Publica, na.rm = T)/n(),
    Cozinha = sum(Cozinha, na.rm = T)/n(),
    Refeitorio = sum(Refeitorio, na.rm = T)/n(),
    # Demais variaveis: Não são Binarias
    Educacao_Basica = sum(Educacao_Basica, na.rm = T), 
    Educacao_Infantil = sum(Educacao_Infantil, na.rm = T), 
    Educacao_Infantil_Creche = sum(Educacao_Infantil_Creche, na.rm = T), 
    Educacao_Infantil_PreEscola = sum(Educacao_Infantil_PreEscola, na.rm = T), 
    Educacao_Fundamental = sum(Educacao_Fundamental, na.rm = T), 
    Educacao_Fundamental_AI = sum(Educacao_Fundamental_AI, na.rm = T), 
    Educacao_Fundamental_AF = sum(Educacao_Fundamental_AF, na.rm = T), 
    Educacao_Medio = sum(Educacao_Medio, na.rm = T), 
    Educacao_EJA = sum(Educacao_EJA, na.rm = T), 
    Educacao_EJA_Fundamental = sum(Educacao_EJA_Fundamental, na.rm = T), 
    Educacao_EJA_Medio = sum(Educacao_EJA_Medio, na.rm = T)
  ) %>% 
  ungroup() %>% 
  rename(c(Estado_Sigla=SIGLA, Municipio=MUNIC, Dependencia_Administrativa=DEP))

# Join para nome da Regiao
censo_educ_2006 <- censo_educ_2006 %>% 
  left_join(cod_ibge_ufre, by = "Estado_Sigla") %>% 
  mutate(Municipio = Municipio %>% rm_accent() %>% str_to_lower()) %>% 
  left_join(cod_ibge_mun, by = c("Municipio"="NO_MUNICIPIO")) %>% 
  left_join(cod_ibge_micro, by = c("CO_MUNICIPIO"="Codigo_Municipio")) %>% 
  rename(c(Codigo_Municipio=CO_MUNICIPIO)) %>% 
  select(Regiao, Estado_Sigla, Municipio, Codigo_Municipio, Microrregiao, Codigo_Microrregiao, Censo, everything())

# -------------------------------------------------------------------------

gc()

# -------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
###################### Painel Censo Educacional 2017 ######################
# ----------------------------------------------------------------------- #

censo_educ_2017 <- censo_educ_2017 %>% 
  select(-c(QT_PROF_NUTRICIONISTA, QT_PROF_ALIMENTACAO )) %>% 
  mutate_at(7:26, ~ ifelse(is.na(.x), 0, .x) %>% as.numeric()) %>% 
  `colnames<-`(c(
    "Regiao", "Estado_Sigla", "Municipio", "Codigo_Municipio", "Censo", "Dependencia_Administrativa", "Energia_Eletrica_Publica", "Cozinha", "Refeitorio",  
    "FNDE", "Educacao_Basica", "Educacao_Infantil", "Educacao_Infantil_Creche", "Educacao_Infantil_PreEscola", "Educacao_Fundamental",
    "Educacao_Fundamental_AI", "Educacao_Fundamental_AF", "Educacao_Medio", "Educacao_Profissional", "Educacao_Profissional_Tecnico", "Educacao_EJA", 
    "Educacao_EJA_Fundamental", "Educacao_EJA_Medio", "Educacao_Especial", "Educacao_Especial_Inclusiva", "Educacao_Especial_Exclusiva"
  )) %>% 
  select(Regiao, Estado_Sigla, Municipio, Codigo_Municipio, Censo, everything()) %>% 
  group_by(Regiao, Estado_Sigla, Municipio, Codigo_Municipio, Censo, Dependencia_Administrativa) %>% 
  summarise(
    # Binarias: Energia Eletrica Publica, Cozinha, Refeitorio, Nutricionista, Profissionais_Cozinha, FNDE
    Energia_Eletrica_Publica = sum(Energia_Eletrica_Publica, na.rm = T)/n(),
    Cozinha = sum(Cozinha, na.rm = T)/n(),
    Refeitorio = sum(Refeitorio, na.rm = T)/n(),
    FNDE = sum(FNDE, na.rm = T)/n(),
    # Demais variaveis: Não são Binarias
    Educacao_Basica = sum(Educacao_Basica, na.rm = T), 
    Educacao_Infantil = sum(Educacao_Infantil, na.rm = T), 
    Educacao_Infantil_Creche = sum(Educacao_Infantil_Creche, na.rm = T), 
    Educacao_Infantil_PreEscola = sum(Educacao_Infantil_PreEscola, na.rm = T), 
    Educacao_Fundamental = sum(Educacao_Fundamental, na.rm = T), 
    Educacao_Fundamental_AI = sum(Educacao_Fundamental_AI, na.rm = T), 
    Educacao_Fundamental_AF = sum(Educacao_Fundamental_AF, na.rm = T), 
    Educacao_Medio = sum(Educacao_Medio, na.rm = T), 
    Educacao_Profissional = sum(Educacao_Profissional, na.rm = T), 
    Educacao_Profissional_Tecnico = sum(Educacao_Profissional_Tecnico, na.rm = T), 
    Educacao_EJA = sum(Educacao_EJA, na.rm = T), 
    Educacao_EJA_Fundamental = sum(Educacao_EJA_Fundamental, na.rm = T), 
    Educacao_EJA_Medio = sum(Educacao_EJA_Medio, na.rm = T),
    Educacao_Especial = sum(Educacao_Especial, na.rm = T),
    Educacao_Especial_Inclusiva = sum(Educacao_Especial_Inclusiva, na.rm = T),
    Educacao_Especial_Exclusiva = sum(Educacao_Especial_Exclusiva, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(Dependencia_Administrativa = "Municipal") %>% 
  left_join(cod_ibge_micro, by = c("Codigo_Municipio")) %>% 
  select(Regiao, Estado_Sigla, Municipio, Codigo_Municipio, Microrregiao, Codigo_Microrregiao, Censo, everything())

# -------------------------------------------------------------------------

gc()

# -------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
#################### Painel Censo Educacional - FINAL #####################
# ----------------------------------------------------------------------- #

painel_educacional <- censo_educ_2006 %>% 
  bind_rows(censo_educ_2017) %>% 
  arrange(Regiao, Estado_Sigla, Municipio, Censo)

# -------------------------------------------------------------------------

rm(cod_ibge_mun, cod_ibge_ufre, censo_educ_2006, censo_educ_2017)

gc()

# -------------------------------------------------------------------------

saveRDS(painel_educacional, "output/painel_educacional.rds")
