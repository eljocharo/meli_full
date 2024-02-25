  

library(tidyverse)
library(tidyquant)

 
   inv <-  data.table::fread("dados/inventarios_jan_a_14_fev.csv") %>% 
     janitor::clean_names() %>% dplyr::as_tibble() %>% 
     mutate(pais = case_when( 
       str_sub(  warehouse_id, 1,2 ) == "BR" ~"MLB",
       str_sub(  warehouse_id, 1,2 ) == "MX" ~"MLM",
       TRUE ~ "ConoSur" ) ) %>% 
     dplyr::relocate(pais, .before = warehouse_id)
   
   
   # erro geral
   inv %>% 
     filter( divergencia_tt != 0) %>% 
     summarise( ads = n_distinct(shp_warehouse_address),
                tuplas = n_distinct(shp_inventory_id),
                .by = c( "pais" ) ) %>% 
     arrange(pais, desc(tuplas) ) %>% 
     mutate( pct_geral = tuplas / sum(tuplas) *100) 
   


# Preparando e gerando o grafico de frquencia em erros tuplas -------------

   inv %>%
     filter(divergencia_tt != 0, level <= 7, area == "MZ", ) %>%
     group_by(pais, warehouse_id, shp_warehouse_address, level) %>%
     summarise(sku = n_distinct(shp_inventory_id), .groups = "drop") %>%
     group_by(pais, warehouse_id, level) %>%
     summarise(sku = sum(sku), .groups = "drop") %>%
     group_by(pais, warehouse_id) %>%
     mutate(pct_sku = sku / sum(sku),
            level = as.character(level)) %>%
     ungroup() %>%
     add_count(pais, warehouse_id, wt = sku) %>%
     mutate(warehouse_id = str_c(warehouse_id, " (Tuplas error ", n, ")")) %>% 
     mutate(warehouse_id = fct_reorder(warehouse_id, -sku, .fun = sum)) %>%
     group_by(pais, warehouse_id) %>% 
     mutate(F = ifelse(rank(-sku) <= 2, TRUE, FALSE)) %>%
     ggplot(aes(sku, level,  fill = factor(F)) ) +
     geom_col(data = . %>% filter(F == TRUE), fill = "#c40000", color = "black", alpha = .7 ) +
     geom_col(data = . %>% filter(F == FALSE), fill = "#f7e13c", color = "black", alpha = .9 ) +
     geom_text( data = . %>% filter(F == FALSE),
                aes(label = sku), size = 3, color = "black", vjust = .5, hjust = 1) +
     geom_text( data = . %>% filter(F == TRUE),
                aes(label = sku), size = 4, color = "white", vjust =.5, hjust = 1) +
     facet_wrap(~warehouse_id, scales = "free_x") +
     labs(title = "MLB - Conteos mostreo 2024",
          subtitle = "Errores tuplas y niveles",
          x = "tuplas con errores",
          y = "niveles") +
     theme(axis.text.x = element_text(size = 12)) +
     scale_x_continuous(labels = scales::number) +
     theme_tq() 
   

# Preparando dados para gerar a tabela de contingencia --------------------

   inv_anl <-  inv %>% 
     filter( divergencia_tt != 0,
           level<=7, area == "MZ",pais=="MLB" ) %>% 
     group_by(pais, warehouse_id, shp_warehouse_address, level) %>% 
     summarise( sku = n_distinct(shp_inventory_id), 
                .groups = "drop") %>% 
     group_by(pais, warehouse_id, level) %>% 
     summarise(sku = sum(sku) ) %>% 
     mutate( pct_sku = sku / sum(sku)) %>% 
     ungroup()
   
# Tabela de % contigencia erros de tuplas por nivel -----------------------

   inv_anl %>% 
     ggplot(aes(x = level %>% as.character(), y = reorder(warehouse_id, (sku)), fill = pct_sku)) +
     geom_tile(color = "white") +
     geom_text(aes(label = scales::percent(pct_sku, accuracy = 0.1)),
               color = "black", size = 4) +
     scale_fill_gradientn(colors =  rev(hcl.colors(5, "RdYlGn")), #c("#ffffff","#00aa7f","#00aa00" ,"#f7e13c","#005500"),
                          labels = scales::percent,
                          name = "error") +
     labs(title = str_c( "2024-feb " ,"INAC Error"),
                          y = "", x = "Level") +
     scale_x_discrete(position = "top") +
     theme(axis.title.y = element_blank(),
           axis.text.y = element_text(size = 10),  # Ajusta o tamanho do texto do eixo y
           axis.ticks.y = element_blank(),
           axis.text.x = element_text(size = 12),  # Ajusta o tamanho do texto do eixo x
           axis.title.x = element_text(size = 12),
           legend.position = c(.04, 1.07),
           legend.background =  element_blank(),
           # Move a legenda para o topo do gráfico
           legend.direction = "horizontal",
           legend.text = element_text(color = "black", size = 6, vjust = 25),  # Define a cor do texto da legenda para branco
           legend.title = element_text(color = "black", vjust = .9, hjust = 0.5, ),  # Define a cor do título da legenda para branco
           panel.background = element_rect(fill = "#2d2d2d"),   # Define a cor de fundo do painel para preto
           panel.grid.major = element_line(color = "#2d2d2d"),  # Define a cor das linhas de grade principais para cinza
           panel.grid.minor = element_line(color = "black"),  # Define a cor das linhas de grade menores para cinza, se necessário
           plot.title = element_text(color = "#2d2d2d", 
           hjust = .5, vjust = -2,
           size = 10))  # Define a cor do título do gráfico para branco)  # Define a direção da legenda como horizontal) 
   

# Erros de Tuplas por SKU no endereço -------------------------------------
   
  base_sku_erro <-  inv %>% 
     mutate( erro = if_else( divergencia_tt != 0, "Erro", "OK")) %>% 
     select(pais,warehouse_id,shp_warehouse_address, erro, shp_inventory_id) %>% 
     group_by(pais,warehouse_id,shp_warehouse_address, erro) %>% 
     summarise( sku_ads = n_distinct(shp_inventory_id)) %>% 
    ungroup() %>% 
     pivot_wider( names_from = erro,
                  values_from = sku_ads, values_fill = 0) %>% 
    mutate(total_tuplas = OK+Erro)
   
   
   sku_per_ads <- base_sku_erro %>% 
     filter(str_detect(shp_warehouse_address, "^MZ"), pais == "MLM") %>% 
     # filter( total_tuplas == 7) %>% 
     # count(   area = str_sub(shp_warehouse_address, 1,2),
              # warehouse_id, wt = Erro)
     group_by(pais,warehouse_id) %>% 
     mutate( erro = if_else(Erro != 0, "erro", "ok")) %>% 
     ungroup() %>% 
     count(pais,warehouse_id,erro, total_tuplas, wt = total_tuplas) %>% 
     pivot_wider( names_from = erro,
                  values_from = n,
                  values_fill = 0) %>% 
     mutate(inac =1- erro / (erro +ok)) %>% 
     group_by(pais, warehouse_id) %>% 
     slice(1:7) %>% 
     ungroup()
   
   sku_per_ads %>% 
     ggplot(aes(x = total_tuplas , y =  erro )) +
     geom_col( fill = "#f7e13c", color = "black", alpha = 1 ) +
     geom_text( aes(label = inac %>% 
      scales::percent(accuracy = .1)), size = 3, color = "black", vjust = 1) +
     facet_wrap(~warehouse_id, scales = "free_y") +
     labs(title = "MLB - Conteos mostreo 2024",
          subtitle = "INAC per SKU / address",
          y = "tuplas con errores",
          x = "SKU per Address",
          fill = "F") +
     scale_x_continuous(breaks = seq(1, 7))
   
   
   # Analisando Impacto
   
   sku_per_ads %>% 
     filter( warehouse_id=="MXCD02") %>% 
     mutate(Pct_total_erros = erro  / sum(erro),
          ) %>% 
     rename(   sku_per_address = total_tuplas) %>% 
     gt::gt() %>% 
     gt::fmt_percent(columns = c("inac", "Pct_total_erros")) %>% 
     gt::tab_header(title = "INAC - Fevereiro SP04")
   
   

# Impacto tuplas por SKU endereço -----------------------------------------

   
  cor_bd <-  base_sku_erro %>% 
     filter(str_detect(shp_warehouse_address, "^MZ")) %>% 
     group_by(pais,warehouse_id) %>% 
     mutate( erro = if_else(Erro != 0, "erro", "ok")) %>% 
     ungroup() %>% 
     filter(total_tuplas <=7) %>% 
     group_by(pais,warehouse_id,erro, total_tuplas) %>% 
     summarise( tuplas = sum(total_tuplas),
                address = n_distinct(shp_warehouse_address)) %>% 
     # filter(  warehouse_id =="BRSP04") %>% 
     pivot_wider( names_from = erro,
                  values_from = c(tuplas,address)) %>% 
     ungroup() %>% 
     select(2:4)
   
   correlation_by_warehouse <- cor_bd %>%
     group_by(warehouse_id) %>%
     summarise(cor_test = list(cor.test(tuplas_erro, total_tuplas))) %>%
     mutate(correlation = map_dbl(cor_test, ~ .$estimate),
            p_value = map_dbl(cor_test, ~ .$p.value)) %>%
     select(-cor_test) # Remover a coluna cor_test que contém os resultados do teste completo
   
   correlation_by_warehouse %>% 
     arrange(correlation)
   
   
   
   erro_tuplas_cor <- base_sku_erro %>% 
     filter(str_detect(shp_warehouse_address, "^MZ"), pais == "MLB") %>% 
      group_by(pais,warehouse_id) %>% 
     mutate( erro = if_else(Erro != 0, "erro", "ok")) %>% 
     ungroup() %>% 
     filter(total_tuplas <=7) %>% 
     group_by(pais,warehouse_id,erro, total_tuplas) %>% 
     summarise( tuplas = sum(total_tuplas),
                address = n_distinct(shp_warehouse_address) ) %>% 
                  pivot_wider( names_from = erro,
                               values_from = c(tuplas,address))
   
   base_sku_erro %>% 
     filter(str_detect(shp_warehouse_address, "^MZ"), pais == "MLB") %>% 
     group_by(pais,warehouse_id) %>% 
     mutate( erro = if_else(Erro != 0, "erro", "ok")) %>% 
     ungroup() %>% 
     filter(total_tuplas <=7) %>% 
     count(pais,warehouse_id,erro, total_tuplas, wt = total_tuplas) %>% 
     ggplot(aes(x = total_tuplas, y = n, fill = erro  )) +
     geom_col(color = "black", alpha = 0.8, position = position_dodge(width = 0.5)) +
     geom_text(aes(label = n, color = erro), 
               position = position_dodge(width = 0.5), vjust = -0.5, size = 3) +
     scale_x_continuous(breaks = 1:7) +
     scale_fill_manual(values = c("ok" = "#3c64f7", "erro" = "#c40000"), name = "") +
     scale_color_manual(values = c("ok" = "#3c64f7", "erro" = "#c40000")) +
     guides(color = FALSE) +
     labs(title = "MLM Type error - Conteo Mostreo Febrero",
          x = "Level address", y = "Tuplas con error") +
     facet_wrap(~warehouse_id )
   
   

# analise por categoria ---------------------------------------------------

  inv %>% 
     filter( divergencia_tt != 0, area =="MZ", warehouse_id=="BRSP04") %>% 
     
     count( categ_1, warehouse_id, sort = T) %>% 
     mutate( pct = n / sum(n),
             pct_acc = cumsum(pct))
     
   
     
    
   