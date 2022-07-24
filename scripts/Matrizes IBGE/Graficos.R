
# Pacotes -----------------------------------------------------------------

library(dplyr)
library(forcats)
library(scales)
library(ggplot2)

# Gráfico Geral -----------------------------------------------------------

## Quantiade Produzida
quantidade_produzida %>% 
  mutate(
    label = case_when(
      Quantidade=="X" ~ "X",
      Quantidade=="-" ~ "-",
      Quantidade==".." ~ "..",
      Quantidade=="..." ~ "...",
      Quantidade=="0" ~ "0",
      TRUE ~ "Quantidade>0"
    )
  ) %>% 
  group_by(label) %>% 
  count() %>% 
  bind_rows(
    tibble("label" = c("..", "..."), "n" = c(0, 0))
  ) %>% 
  ungroup() %>% 
  mutate(
    ord = c(1,2,6,3,4,5),
    perc = percent(n/sum(n))
  ) %>% 
  ggplot(aes(x = fct_reorder(label, ord), y = n)) +
  geom_col(fill = "#1e542d") +
  geom_label(aes(label = paste(n, "-", perc))) +
  labs(x = "Símbolo", y = "Quantidade Produzida", 
       title = "Quantidade Produzida \n Agricultura e Pecuária Familiar Sim e Não") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

## Valor da Producao
valor_da_producao %>% 
  mutate(
    label = case_when(
      Quantidade=="X" ~ "X",
      Quantidade=="-" ~ "-",
      Quantidade==".." ~ "..",
      Quantidade=="..." ~ "...",
      Quantidade=="0" ~ "0",
      TRUE ~ "Quantidade>0"
    )
  ) %>% 
  group_by(label) %>% 
  count() %>% 
  bind_rows(
    tibble("label" = c("..", "..."), "n" = c(0, 0))
  ) %>% 
  ungroup() %>% 
  mutate(
    ord = c(1,2,6,3,4,5),
    perc = percent(n/sum(n))
  ) %>% 
  ggplot(aes(x = fct_reorder(label, ord), y = n)) +
  geom_col(fill = "#1e542d") +
  geom_label(aes(label = paste(n, "-", perc))) +
  labs(x = "Símbolo", y = "Quantidade Produzida", 
       title = "Valor da Produção \n Agricultura e Pecuária Familiar Sim e Não") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

# -------------------------------------------------------------------------
