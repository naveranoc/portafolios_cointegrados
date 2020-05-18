#################LOAD PACKAGES######################
library(tidyverse)
library(corrplot)
library(lubridate)
library(Hmisc) #for calculate correlation matrix
library(plotly)
#####################LOAD DATA ####################################
precios_historicos <- read_rds('data/historic_prices_v3.rds')
pesos_historicos <- read_rds('data/composicion_historica_2.rds')
#################CREATE CORRELATION MATRIX#########################
#esto obtiene los pesos de las acciones mas liquidas durante los periodos 20140101, 20180501
pesos_mas_liquidas <- pesos_historicos %>% 
                      filter(fecha_comienzo >= ymd(20140101), fecha_comienzo < ymd(20180501)) %>% 
                      group_by(fecha_comienzo) %>% 
                      mutate(rango_periodo = dplyr::dense_rank(desc(peso))) %>% 
                      ungroup() %>% 
                      filter(rango_periodo <= 10)
###Obtener las acciones mas liquidas
acciones_mas_liquidas <- pesos_mas_liquidas %>% 
                         .[['nemo']] %>% 
                         unique()
###############SERIES DE PRECIOS HISTORICOS ########################
###crear dataset for calculate correlation matrix
series_calcular_cor <- precios_historicos %>% 
                       filter(Nemo %in% acciones_mas_liquidas) %>% 
                       filter(fecha >= ymd(20140101), fecha < ymd(20180501)) %>% 
                       pivot_wider(names_from = Nemo,values_from = precio_cierre_cor) 
###############CALCULAR Y DIBUJAR MATRIZ DE CORRELACION########################
res <- rcorr(series_calcular_cor %>% select(-fecha) %>% as.matrix())
corrplot(res$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,method = "number",p.mat = res$P, sig.level = 0.01)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res$r, col = col, symm = TRUE)
##############DIBUJAR SERIES INTERES##################################################
series_to_draw <- c('BCOLOMBIA', 'PFBCOLOM')
p<- series_calcular_cor %>% 
    select(fecha, series_to_draw) %>% 
    pivot_longer(cols = -fecha, names_to = 'Nemo', values_to = 'valor_accion') %>% 
    ggplot(aes(x = fecha, y = valor_accion, col = Nemo)) + 
    geom_path(size = 0.8) + 
    geom_point(size = 0.7) +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_labels = '%b - %Y', date_breaks = 'month') +
    ggtitle('Grafico de las series historicas') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
ggplotly(p)



