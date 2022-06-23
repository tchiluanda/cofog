library(readxl)
library(ggplot2)
library(dplyr)
library(ckanr)


####################VErsão antiga (início)

COFOG_GC <- read_excel("COFOG/COFOG GC.xlsx", 
                       sheet = "despesa_funcao", col_types = c("text", 
                                                               "text", "skip", "skip", "skip", "skip", 
                                                               "skip", "skip", "skip", "skip", "skip"), 
                       skip = 2)


names(COFOG_GC)<- c("codigo_cofog","descricao_cofog")

Base_COFOG_2019_TT <- read_excel("COFOG/Base COFOG 2019 TT.xlsx")

names(Base_COFOG_2019_TT)[13:14]<-c("valor","codigo_cofog")



dados_cofog_pai<-
  Base_COFOG_2019_TT %>%
  mutate(codigo_cofog = stringr::str_sub(codigo_cofog,1,3)) %>%
  inner_join( COFOG_GC) %>%
  mutate(descricao_cofog_pai = "Gastos com funções de governo") %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(valor)
  )

dados_total<-
  dados_cofog_pai %>%
  ungroup() %>%
  mutate(descricao_cofog = descricao_cofog_pai) %>%
  mutate(descricao_cofog_pai = NA) %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(total_gasto)
  )




dados_cofog_raiz<-  
  Base_COFOG_2019_TT %>%
  mutate(codigo_pai = stringr::str_sub(codigo_cofog,1,3)) %>%
  inner_join( COFOG_GC) %>%
  mutate(codigo_filho = codigo_cofog) %>%
  mutate(descricao_cofog_filho = descricao_cofog) %>%
  mutate(codigo_cofog = codigo_pai) %>%
  select(codigo_pai,codigo_cofog, descricao_cofog_filho, valor) %>%
  inner_join( COFOG_GC) %>%
  mutate(descricao_cofog_pai = descricao_cofog ) %>%
  mutate(descricao_cofog = descricao_cofog_filho) %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(valor)
  )

dados_cofog_completo <-
  dados_cofog_pai %>%
  bind_rows(dados_cofog_raiz,
            dados_total)


dados_cofog_completo<-
  dados_cofog_completo%>%
  ungroup() %>%
  mutate (source = row_number() -1) 

pai<-
  (dados_cofog_completo %>%
     filter(!is.na(descricao_cofog_pai)) %>%
     distinct(descricao_cofog_pai))$descricao_cofog_pai

pos_pai <-
  dados_cofog_completo %>%
  filter(descricao_cofog %in% pai) %>%
  distinct(descricao_cofog, source) %>%
  mutate(descricao_cofog_pai =descricao_cofog ) %>%
  mutate(destination = source) %>%
  select(descricao_cofog_pai, destination)

dados_net<-
  dados_cofog_completo %>%
  inner_join(pos_pai)


nodes<- dados_cofog_completo %>%
  select(descricao_cofog)


links<- 
  dados_net %>%
  select(source,
         destination,
         total_gasto)

networkD3::
  
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                Target = "destination", Value = "total_gasto", NodeID = "descricao_cofog",
                units = "", fontSize = 12, nodeWidth = 30)


dados_tree_view<-
  dados_cofog_raiz %>%
  mutate(raiz= "Gastos com funções de governo") %>%
  mutate(total_norm = (scale(total_gasto, center = FALSE))+1)


library(collapsibleTree)

dados_tree_view%>%
  mutate(total_gasto = total_gasto /10^9) %>%
  collapsibleTreeSummary(
    hierarchy = c("descricao_cofog_pai", "descricao_cofog"),
    root = "Gastos com funções de governo",
    width = 800,
    attribute = "total_gasto",
    #nodeSize = "total_norm",
    zoomable = FALSE
  )



Geography %>%
  group_by(continent, type) %>%
  summarize(`Number of Countries` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("continent", "type"),
    root = "Geography",
    width = 800,
    attribute = "Number of Countries",
    zoomable = FALSE
  )

networkD3::
  
  
  write.csv2(dados_cofog, file = "dados_cofog_2019.csv", fileEncoding = "UTF-8")

library(ggdark)
library(rcartocolor)
library(hrbrthemes)

import_roboto_condensed()

dados_cofog %>%
  ungroup() %>%
  mutate(descricao_cofog = reorder(descricao_cofog, total_gasto)) %>%
  ggplot() +
  geom_col(aes(x= descricao_cofog, y= total_gasto/10^9, fill= descricao_cofog)) +
  coord_flip()+
  dark_theme_minimal(base_family = font_rc, base_size = 16)+
  scale_fill_carto_d(palette = "Mint", direction = -1, guide = NULL)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(
    title = "Gastos com as funções de governo",
    y = "Gastos (R$ bi)",
    x= "Funções de governo" 
  )


library(purrr)

Base_COFOG<- map_dfr(2010:2019, function(a_ano){
  file<- paste0("Base-COFOG-",a_ano,"-TT.xlsx")
  print(file)
  
  # read_excel("Base-COFOG-2019-TT.xlsx", 
  #            col_types = c("numeric", "text", "numeric", 
  #                          "text", "text", "text", "text", "text", 
  #                          "text", "text", "text", "text", "numeric", 
  #                          "text"))
  ds_file<- read_excel(file)
  ds_file<-ds_file[,c(1:14)]
  #names(Base_COFOG)[c(7,13:14)]<-c("acao_governo","valor","codigo_cofog")
  #names(Base_COFOG)[c(11,13:14)]<-c("unidade_orcamentaria","valor","codigo_cofog")
  
  
  names(ds_file)[c(1,3,5,7,11,13,14)]<-c("ano", "ndd","cod_mod_aplic","acao_governo","unidade_orcamentaria","valor","codigo_cofog")
  ds_file$cod_mod_aplic<-as.character(ds_file$cod_mod_aplic)
  ds_file$ndd<- as.character(ds_file$ndd)
  ds_file
})


save(list=c("Base_COFOG"),file="COFOG.RData")
####################VErsão antiga (fim)




##########################Versão 2021 para dados até 2020

library(readxl)
library(ckanr)
library(purrr)

ckanr::package_search()

package<- ckanr::package_show(id= "22d13d17-bf69-4a1a-add2-25cc1e25f2d7", 
                              url= "https://www.tesourotransparente.gov.br/ckan") #busca todos os dados do dataset que se refere aos dados de COFOG






df_base_cofog<- #percorre todos os recursos do dataset para montar um único dataframe com os dados anuais com maio nível de detalhe
map_dfr(1:length(package[["resources"]]), function(a_pos){
  print(a_pos)
  if (substr(package[["resources"]][[a_pos]]$name,1,10)== "Base COFOG"){ #testa se é uma tabela dos dados anuais completa
    tmp = tempfile(fileext = ".xlsx")
    
    URL_add<- package[["resources"]][[a_pos]]$url
    
    df_cofog<-download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")
    df_cofog <- readxl::read_xlsx(tmp)
    df_cofog <- janitor::clean_names(df_cofog)
    df_cofog$natureza_despesa_detalhada <- as.character(df_cofog$natureza_despesa_detalhada)
    df_cofog
    
  }
  
})

Base_COFOG<- df_base_cofog

save(list=c("Base_COFOG"),file="COFOG.RData")
