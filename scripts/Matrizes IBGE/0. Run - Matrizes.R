
options(scipen = 999999999)

# Pacotes -----------------------------------------------------------------

library(tictoc)
library(dplyr)
library(writexl)

# Dados - Familiar Sim e Não ----------------------------------------------

suppressMessages({
  # Leitura dos Scripts
  tic(); source("scripts/Matrizes IBGE/1. Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
  tic(); source("scripts/Matrizes IBGE/1. Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim
})

# Dados - Quantidade e Valor: Familiar Não e Sim

quantidade_produzida <- quantidade_produzida_fam_nao %>% bind_rows(quantidade_produzida_fam_sim)
valor_da_producao <- valor_da_producao_fam_nao %>% bind_rows(valor_da_producao_fam_sim)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_fam_nao, quantidade_produzida_fam_sim,
  valor_da_producao_fam_nao, valor_da_producao_fam_sim
)

# Dados - Manipulação Produtos --------------------------------------------

suppressMessages({
  tic(); source("scripts/Matrizes IBGE/2. Manipulação Produtos.R", encoding = "UTF-8"); toc() # Manipulação da coluna Produtos - Juntando os menos exportados
})

# Dados - Merge ComexStat -------------------------------------------------

suppressMessages({
  tic(); source("scripts/Matrizes IBGE/3. Merge ComexStat.R", encoding = "UTF-8"); toc() # Adicionando as colunas Quantidade (Toneladas) e Valor FOB (US$)
})

# Juntando as Bases de dados ----------------------------------------------

quantidade_produzida <- quantidade_produzida_agr %>% bind_rows(quantidade_produzida_pec)
valor_da_producao <- valor_da_producao_agr %>% bind_rows(valor_da_producao_pec)

# Dados - Share -----------------------------------------------------------

suppressMessages({
  tic(); source("scripts/Matrizes IBGE/4. Participação de Exportação.R", encoding = "UTF-8"); toc() # Participação de Exportação
})

# Salvando tabelas --------------------------------------------------------

# Quantidade Produzida
list(
  "Quantidade Produzida" = quantidade_produzida,
  "Matriz QuaPr - AGR %" = quantidade_produzida_agr_sim_matriz,
  "Matriz QuaPr - PEC %" = quantidade_produzida_pec_sim_matriz,
  "Matriz QuaPr - AGR QTonelada" = quantidade_ton_agr_sim_matriz,
  "Matriz QuaPr - PEC QTonelada" = quantidade_ton_pec_sim_matriz
) %>% 
  write_xlsx(path = "output/quantidade_produzida.xlsx")

# Valor da Produção
list(
  "Valor da Produção" = valor_da_producao,
  "Matriz ValPr - AGR %" = valor_da_producao_agr_sim_matriz,
  "Matriz ValPr - PEC %" = valor_da_producao_pec_sim_matriz,
  "Matriz ValPr - AGR Valor FOB" = valor_fob_agr_sim_matriz,
  "Matriz ValPr - PEC Valor FOB" = valor_fob_pec_sim_matriz
) %>% 
  write_xlsx(path = "output/valor_da_producao.xlsx")

# -------------------------------------------------------------------------
