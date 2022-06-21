
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

# 2006 - Filtro para o Censo Educacional
filtro_2006 <- read_excel("data/Dados Educacionais/Censo 2006/Filtro_2006.xlsx") %>% 
  clean_names() %>% 
  select(1,2) %>% 
  filter(!is.na(variaveis))

# 2006 - Censo Educacional
censo_educ_2006 <- read_delim(
  "data/Dados Educacionais/Censo 2006/CENSOESC_2006.CSV", delim = "|", escape_double = FALSE, trim_ws = TRUE
) %>% 
  select(c(SIGLA, MUNIC, CODMUNIC, ANO, filtro_2006$variaveis)) %>% 
  filter(DEP=="Municipal")

# 2017 - Filtro para o Censo Educacional
filtro_2017 <- read_excel("data/Dados Educacionais/Censo 2017/Filtro_2017.xlsx") %>% clean_names()

# 2017 - Censo Educacional
censo_educ_2017 <- read_delim(
  "data/Dados Educacionais/Censo 2017/Censo.Ed.Basica_2017.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>% 
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
  mutate_at(9:44, ~ .x %>% as.numeric()) %>% 
  mutate(
    # Soma de variaveis
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
    SIGLA, MUNIC, CODMUNIC, Censo, Energia_Eletrica_Publica, Cozinha, Refeitorio, Educacao_Basica, Educacao_Infantil, Educacao_Infantil_Creche, Educacao_Infantil_PreEscola, 
    Educacao_Fundamental, Educacao_Fundamental_AI, Educacao_Fundamental_AF, Educacao_Medio, Educacao_EJA, Educacao_EJA_Fundamental, Educacao_EJA_Medio
  )

# ----------------------------------------------------------------------- #
###################### Painel Censo Educacional 2017 ######################
# ----------------------------------------------------------------------- #

censo_educ_2017 <- censo_educ_2017 %>% 
  `colnames<-`(c(
    "Regiao", "Sigla", "Municipio", "CodMunicipio", "Censo", "TP_DEPENDENCIA", "Energia_Eletrica_Publica", "Cozinha", "Refeitorio", "Nutricionista", 
    "Profissionais_Cozinha", "FNDE", "Educacao_Basica", "Educacao_Infantil", "Educacao_Infantil_Creche", "Educacao_Infantil_PreEscola", "Educacao_Fundamental",
    "Educacao_Fundamental_AI", "Educacao_Fundamental_AF", "Educacao_Medio", "Educacao_Profissional", "Educacao_Profissional_Tecnico", "Educacao_EJA", 
    "Educacao_EJA_Fundamental", "Educacao_EJA_Medio", "Educacao_Especial", "Educacao_Especial_Inclusiva", "Educacao_Especial_Exclusiva"
  ))
  

# -------------------------------------------------------------------------













