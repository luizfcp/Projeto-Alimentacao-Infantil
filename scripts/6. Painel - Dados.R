
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
  select(filtro_2006$variaveis) %>% 
  filter(DEP=="Municipal")

# 2017 - Filtro para o Censo Educacional
filtro_2017 <- read_excel("data/Dados Educacionais/Censo 2017/Filtro_2017.xlsx") %>% clean_names()

# 2017 - Censo Educacional
censo_educ_2017 <- read_delim(
  "data/Dados Educacionais/Censo 2017/Censo.Ed.Basica_2017.CSV", delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>% 
  select(filtro_2017$variaveis) %>% 
  filter(TP_DEPENDENCIA==3)

# -------------------------------------------------------------------------

rm(filtro_2006, filtro_2017)

gc()

# -------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
############################## Painel 2006 ################################
# ----------------------------------------------------------------------- #

