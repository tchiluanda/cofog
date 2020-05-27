library(readxl)
library(ggplot2)
library(dplyr)
COFOG_GC <- read_excel("COFOG/COFOG GC.xlsx", 
                       sheet = "despesa_funcao", col_types = c("text", 
                                                               "text", "skip", "skip", "skip", "skip", 
                                                               "skip", "skip", "skip", "skip", "skip"), 
                       skip = 2)


names(COFOG_GC)<- c("codigo_cofog","descricao_cofog")

Base_COFOG_2019_TT <- read_excel("COFOG/Base COFOG 2019 TT.xlsx")

names(Base_COFOG_2019_TT)[13:14]<-c("valor","codigo_cofog")



dados_cofog<-
Base_COFOG_2019_TT %>%
  mutate(codigo_cofog = stringr::str_sub(codigo_cofog,1,3)) %>%
  inner_join(COFOG_GC) %>%
  group_by(codigo_cofog, descricao_cofog) %>%
  summarise(
    total_gasto = sum(valor)
  )


write.csv2(dados_cofog, file = "dados_cofog_2019.csv", fileEncoding = "UTF-8")

library(ggdark)
library(rcartocolor)
library(hrbrthemes)

import_roboto_condensed()

dados_cofog %>%
  ungroup() %>%
  mutate(descricao_cofog = reorder(descricao_cofog, total_gasto)) %>%
  ggplot() +
    geom_col(aes(x= descricao_cofog, y= total_gasto/10^6, fill= descricao_cofog)) +
    coord_flip()+
  dark_theme_minimal(base_family = font_rc, base_size = 16)+
  scale_fill_carto_d(palette = "Mint", direction = -1, guide = NULL)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    title = "Expenses on common functions of government",
    y = "Expenses (R$ mi)",
    x= "Common functions of government" 
  )
  
  
  