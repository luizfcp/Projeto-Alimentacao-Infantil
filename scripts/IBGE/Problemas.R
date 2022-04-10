
library(tictoc)
tic(); source("scripts/IBGE/0. Run.R", encoding = "UTF-8"); toc()

def_grupos <- read_excel("data/IBGE/prohort-grupo-produto.xlsx") %>% 
  select(Grupo, Produto) %>% 
  distinct_all() %>% 
  mutate_all(~ .x %>% str_replace_all("-", " ") %>% rm_accent() %>% str_to_lower()) %>% 
  `colnames<-`(c("Grupos", "Produto"))

# Duplicados
duplicados <- read_excel("data/IBGE/prohort-grupo-produto.xlsx") %>% select(Grupo, Produto) %>% get_dupes()
read_excel("data/IBGE/prohort-grupo-produto.xlsx") %>% filter(Produto %in% duplicados$Produto) %>% arrange(Produto)

# O que fazer? ------------------------------------------------------------

quantidade_produzida %>% 
  filter(str_detect(Produto, "\\(")) %>% 
  distinct(Produto) %>% 
  separate(Produto, c("Produto", "Problema"), sep = "\\(") %>% 
  mutate(Produto = Produto %>% str_replace_all("-", " ") %>% str_trim() %>% rm_accent() %>% str_to_lower()) %>% 
  left_join(def_grupos, by = "Produto") %>% 
  na.omit()
