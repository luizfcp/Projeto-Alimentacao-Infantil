
options(scipen = 999999999)

# Pacotes -----------------------------------------------------------------

library(tictoc)
library(dplyr)
library(writexl)

# Dados - Familiar Sim e Não ----------------------------------------------

# Leitura dos Scripts
tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim

# Dados - Quantidade e Valor: Familiar Não e Sim

quantidade_produzida <- quantidade_produzida_fam_nao %>% bind_rows(quantidade_produzida_fam_sim)
valor_da_producao <- valor_da_producao_fam_nao %>% bind_rows(valor_da_producao_fam_sim)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_fam_nao, quantidade_produzida_fam_sim,
  valor_da_producao_fam_nao, valor_da_producao_fam_sim
)

# Dados - Manipulação Produtos --------------------------------------------

tic(); source("scripts/2. Manipulação Produtos.R", encoding = "UTF-8"); toc() # Manipulação da coluna Produtos - Juntando os menos exportados

# Dados - Merge ComexStat -------------------------------------------------

tic(); source("scripts/3. Merge ComexStat.R", encoding = "UTF-8"); toc() # Adicionando as colunas Quantidade (Toneladas) e Valor FOB (US$)

# Juntando as Bases de dados ----------------------------------------------

quantidade_produzida <- quantidade_produzida_agr %>% bind_rows(quantidade_produzida_pec)
valor_da_producao <- valor_da_producao_agr %>% bind_rows(valor_da_producao_pec)

# Dados - Share -----------------------------------------------------------

tic(); source("scripts/4. Participação de Exportação.R", encoding = "UTF-8"); toc() # Participação de Exportação

# -------------------------------------------------------------------------

gc()

# -------------------------------------------------------------------------
