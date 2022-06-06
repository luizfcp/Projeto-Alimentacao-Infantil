
#################################################################
# Construção do Painel - IBGE 2006 e 2017                       #
#################################################################

# Pacotes -----------------------------------------------------------------

library(readxl)
library(janitor)
library(tidyr)
library(dplyr)

# Dados -------------------------------------------------------------------

# Código IBGE
cod_ibge <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% 
  select(1, regiao) %>% 
  `colnames<-`(c("Código IBGE", "Região")) %>% 
  distinct_all()

# Integração Censos
int_censo <- read_excel("data/IBGE/IntegraçãoCensosAgropecuarios 2006 e 2017.xlsx")

# Bases de dados 2017
suppressMessages({
  # Leitura dos Scripts
  tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
  tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim
})





# Manipulação -------------------------------------------------------------

############################### Agricultura ###############################

agr <- quantidade_produzida %>% 
  left_join(
    valor_da_producao, 
    by = c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto")
  ) %>% 
  left_join(cod_ibge, by = c("Código IBGE")) %>% 
  filter(str_detect(Tipologia, "Agricultura"))

agr %>% 
  select(-c(`Quantidade (Toneladas)`, `Valor FOB (US$)`)) %>% 
  mutate(Censo = 2017) %>% 
  select(Tipologia, `Região`, `Estado (UF)`, `Nome do Município`, `Código IBGE`, Censo, Produto)
  



# -------------------------------------------------------------------------

rm()

gc()

# -------------------------------------------------------------------------