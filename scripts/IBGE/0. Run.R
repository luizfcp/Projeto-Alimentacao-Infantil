
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tictoc)

# Dados -------------------------------------------------------------------

path_scripts <- list.files("scripts/IBGE/", full.names = T)

# Leitura dos Scripts
tic(); source(path_scripts[2], encoding = "UTF-8"); toc() # Familiar Não
tic(); source(path_scripts[3], encoding = "UTF-8"); toc() # Familiar Sim

# Data - IBGE: Familiar Não e Sim

data_ibge <- data_fam_nao %>% bind_rows(data_fam_sim)

# -------------------------------------------------------------------------

rm(
  path_scripts, data_fam_nao, data_fam_sim
)

# -------------------------------------------------------------------------
