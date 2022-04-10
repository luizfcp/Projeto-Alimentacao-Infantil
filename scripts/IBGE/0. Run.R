
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(tictoc)

# Dados -------------------------------------------------------------------

# Leitura dos Scripts
tic(); source("scripts/IBGE/Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
tic(); source("scripts/IBGE/Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim

# Data - Quantidade e Valor: Familiar Não e Sim

quantidade_produzida <- quantidade_produzida_fam_nao %>% bind_rows(quantidade_produzida_fam_sim)
valor_da_producao <- valor_da_producao_fam_nao %>% bind_rows(valor_da_producao_fam_sim)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_fam_nao, quantidade_produzida_fam_sim,
  valor_da_producao_fam_nao, valor_da_producao_fam_sim
)

# -------------------------------------------------------------------------