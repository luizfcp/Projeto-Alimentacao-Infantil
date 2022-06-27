
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

# Dados -------------------------------------------------------------------

# Codigo IBGE
cod_ibge_ufre <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(uf, regiao) %>% `colnames<-`(c("Estado_Sigla", "Regiao")) %>% distinct_all()
cod_ibge_mun <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% select(mun, 1) %>% `colnames<-`(c("NO_MUNICIPIO", "CO_MUNICIPIO")) %>% distinct_all()

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
  select(c(NO_REGIAO, SG_UF, NO_MUNICIPIO, NU_ANO_CENSO, filtro_2017$variaveis)) %>% 
  mutate(NO_MUNICIPIO = str_to_title(NO_MUNICIPIO)) %>% 
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

# --------------------------------------------------------------------------------------------------------------------------------- #
# Exemplo por extenso do Agrupamento 1: Binaria Energia Eletrica Publica
# censo_educ_2006 %>% 
#   group_by(SIGLA, MUNIC, Censo, DEP) %>%
#   summarise(
#     total_EEP = length(Energia_Eletrica_Publica), # total de escolas no municipio
#     soma_EEP = sum(Energia_Eletrica_Publica, na.rm = T) # total de escolas com Energia no municipio
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     EEP = total_EEP/2, # variavel auxiliar: media do total de escolas no municipio
#     Energia_Eletrica_Publica = ifelse(soma_EEP>=EEP, 1, 0) # se soma_EEP for maior ou igual a EEP (media), retorna 1, cc 0
#   )
# --------------------------------------------------------------------------------------------------------------------------------- #

# Calculo das variaveis por municipio
censo_educ_2006 <- censo_educ_2006 %>% 
  group_by(SIGLA, MUNIC, Censo, DEP) %>% 
  summarise(
    # Binarias: Energia Eletrica Publica, Cozinha, Refeitorio
    Energia_Eletrica_Publica = ifelse(sum(Energia_Eletrica_Publica, na.rm = T)>={length(Energia_Eletrica_Publica)/2}, 1, 0),
    Cozinha = ifelse(sum(Cozinha, na.rm = T)>={length(Cozinha)/2}, 1, 0),
    Refeitorio = ifelse(sum(Refeitorio, na.rm = T)>={length(Refeitorio)/2}, 1, 0),
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
  rename(c(Estado_Sigla=SIGLA, Municipio=MUNIC, Dependencia_Administrativa=DEP)) %>% 
  mutate(Municipio = str_to_title(Municipio))

# Join para nome da Regiao
censo_educ_2006 <- censo_educ_2006 %>% 
  left_join(cod_ibge_ufre, by = "Estado_Sigla") %>% 
  select(Regiao, Estado_Sigla, Municipio, Censo, everything())

# -------------------------------------------------------------------------

gc()

# -------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
###################### Painel Censo Educacional 2017 ######################
# ----------------------------------------------------------------------- #

censo_educ_2017 <- censo_educ_2017 %>% 
  mutate_at(6:27, ~ ifelse(is.na(.x), 0, .x) %>% as.numeric()) %>% 
  `colnames<-`(c(
    "Regiao", "Estado_Sigla", "Municipio", "Censo", "Dependencia_Administrativa", "Energia_Eletrica_Publica", "Cozinha", "Refeitorio", "Nutricionista", 
    "Profissionais_Cozinha", "FNDE", "Educacao_Basica", "Educacao_Infantil", "Educacao_Infantil_Creche", "Educacao_Infantil_PreEscola", "Educacao_Fundamental",
    "Educacao_Fundamental_AI", "Educacao_Fundamental_AF", "Educacao_Medio", "Educacao_Profissional", "Educacao_Profissional_Tecnico", "Educacao_EJA", 
    "Educacao_EJA_Fundamental", "Educacao_EJA_Medio", "Educacao_Especial", "Educacao_Especial_Inclusiva", "Educacao_Especial_Exclusiva"
  )) %>% 
  select(Regiao, Estado_Sigla, Municipio, Censo, everything()) %>% 
  group_by(Regiao, Estado_Sigla, Municipio, Censo, Dependencia_Administrativa) %>% 
  summarise(
    # Binarias: Energia Eletrica Publica, Cozinha, Refeitorio
    Energia_Eletrica_Publica = ifelse(sum(Energia_Eletrica_Publica, na.rm = T)>={length(Energia_Eletrica_Publica)/2}, 1, 0),
    Cozinha = ifelse(sum(Cozinha, na.rm = T)>={length(Cozinha)/2}, 1, 0),
    Refeitorio = ifelse(sum(Refeitorio, na.rm = T)>={length(Refeitorio)/2}, 1, 0),
    Nutricionista = ifelse(sum(Nutricionista, na.rm = T)>={length(Nutricionista)/2}, 1, 0),
    Profissionais_Cozinha = ifelse(sum(Profissionais_Cozinha, na.rm = T)>={length(Profissionais_Cozinha)/2}, 1, 0),
    FNDE = ifelse(sum(FNDE, na.rm = T)>={length(FNDE)/2}, 1, 0),
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
  mutate(Dependencia_Administrativa = "Municipal")

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

painel_educacional %>% writexl::write_xlsx(path = "output/painel_educacional.xlsx")
