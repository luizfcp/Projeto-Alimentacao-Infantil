
library(tictoc)
tic(); source("scripts/IBGE/0. Run.R", encoding = "UTF-8"); toc()

# DRAFT -------------------------------------------------------------------

library(abjutils)
library(janitor)

def_grupos <- read_excel("data/IBGE/prohort-grupo-produto.xlsx") %>% 
  select(Grupo, Produto) %>% 
  distinct_all() %>% 
  mutate_all(~ .x %>% str_replace_all("-", " ") %>% rm_accent() %>% str_to_lower()) %>% 
  `colnames<-`(c("Grupos", "Produto"))

# Join na base de dados ---------------------------------------------------

# quantidade_produzida
quantidade_produzida %>% 
  mutate(
    Produto_save = Produto,
    Produto = Produto %>% str_replace_all("-", " ") %>% rm_accent() %>% str_to_lower()
  ) %>% 
  left_join(def_grupos, by = "Produto") %>% 
  mutate(Grupos = ifelse(is.na(Grupos), Produto_save, Grupos)) %>% 
  mutate(Produto = Produto_save) %>%
  select(-Produto_save) %>% 
  select(1:5, Grupos, everything()) %>% 
  rename("Grupo - Planilha"="Grupos") %>% 
  mutate(
    Quantidade = case_when(
      Quantidade=="X" ~ "NA",
      Quantidade=="-" ~ "0",
      Quantidade==".." ~ "0",
      Quantidade=="..." ~ "0",
      TRUE ~ Quantidade
    ) %>% as.numeric()
  )

# valor_da_producao
valor_da_producao %>% 
  mutate(
    Produto_save = Produto,
    Produto = Produto %>% str_replace_all("-", " ") %>% rm_accent() %>% str_to_lower()
  ) %>% 
  left_join(def_grupos, by = "Produto") %>% 
  mutate(Grupos = ifelse(is.na(Grupos), Produto_save, Grupos)) %>% 
  mutate(Produto = Produto_save) %>%
  select(-Produto_save) %>% 
  select(1:5, Grupos, everything()) %>% 
  rename("Grupo - Planilha"="Grupos") %>% 
  mutate(
    Quantidade = case_when(
      Quantidade=="X" ~ "NA",
      Quantidade=="-" ~ "0",
      Quantidade==".." ~ "0",
      Quantidade=="..." ~ "0",
      TRUE ~ Quantidade
    ) %>% as.numeric()
  )
