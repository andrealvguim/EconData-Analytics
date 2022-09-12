##### Importação de Pacotes ####
library(tidyverse)
library(plotly)
library(lubridate)
library(usethis)

##### Gráfico ICBR ##### 
icbr <- inflacao %>% select(1, 25:28) %>%
  pivot_longer(-date,
               names_to = "Tipo",
               values_to = "Valores")

graph_icbr <- icbr %>% ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%b/%Y",
               breaks = scales::pretty_breaks(10))  +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  theme_minimal() +
  labs(x = "", y = "Valores", title = "ICBR", subtitle = "Desagregações ICBR",
                       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold"),
        legend.position = "bottom") +
  scale_color_manual(values = c("icbr" = "dark blue",
                                "icbr_agro" = "red",
                                "icbr_energia" = "purple",
                                "icbr_metal" = "green"), labels = c("ICBR", "IBR Agro", "ICBR Energia", "ICBR Metal"))

##### Gráfico IPCA e seus componentes ##### 

#Limpeza de dados para plotar os gráficos do IPCA e dos seus componentes
inflacao_atual <- inflacao %>%
  select(1:11) %>%
  pivot_longer(-date, names_to = "Tipo", values_to = "Valores") 

#Estabelecimento de um labeller para facilitar a renomeação das colunas nos gráficos
ipca_names <- as_labeller(c('ipca' = "IPCA",
                                'ipca_alimentacao_e_bebidas' = "Alimentacao e Beb.",
                                'ipca_artigos_de_resid_ncia' = "Art. de Residencia",
                                'ipca_comunicacao' = "Comunicacao",
                                'ipca_despesas_pessoais' = "Desp. Pessoais",
                                'ipca_educacao' = "Educacao",
                                'ipca_habitacao' = "Habitacao",
                                'ipca_saude_e_cuidados_pessoais' = "Saude",
                                'ipca_transportes' = "Transportes",
                               'ipca_vestuario' =  "Vestuario"))

graph_ipca1 <- list()

for(i in 1:5){ # Loop para cobrir todos os anos entre 2018 e 2023. Ele armazena todos os gráficos em uma lista chamada graph_ipca
  
  dates_iniciais <- c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01")
  dates_finais <- c("2018-01-01", "2019-03-01", "2020-03-01", "2021-03-01", "2022-03-01", "2023-03-01") #o objetivo do dates_finais é fazer com que as legendas das datas no gráfico não fiquem sobrepostas, dificultando a visualização  
  anos <- c("2018", "2019", "2020", "2021", "2022", "2023")
  
  if(dates_iniciais[i] == "2022-01-01"){
    breaks = 1
  }
  else{
    breaks = 2
  }
  
  graph_ipca1[[i]] <- inflacao_atual %>% filter(date >= ymd(dates_iniciais[i]) & date <= ymd(dates_finais[i + 1])) %>% ggplot(aes(x = date, y = Valores, color = Tipo))+
    geom_line() +
    facet_grid(cols = vars(Tipo),
               labeller = as_labeller(ipca_names)) +
    scale_x_date(date_labels = "%Y",
                 breaks = scales::pretty_breaks(breaks)) +
    scale_y_continuous(breaks = scales::pretty_breaks(4))  +
    scale_color_manual(values = c("ipca" = "dark blue",
                                  "ipca_alimentacao_e_bebidas" = "green",
                                  "ipca_artigos_de_resid_ncia" = "red",
                                  "ipca_comunicacao" = "purple",
                                  "ipca_despesas_pessoais" = "#572C00",
                                  "ipca_educacao" = "#00FFFF",
                                  "ipca_habitacao" = "blue",
                                  "ipca_saude_e_cuidados_pessoais" = "orange",
                                  "ipca_transportes" = "#017901",
                                  "ipca_vestuario" = "magenta"),labels = c("IPCA", 
                                                                           "Alimentacao e Bebidas",
                                                                           "Artigos de Residencia",
                                                                           "Comunicacao", 
                                                                           "Despesas Pessoais",
                                                                           "Educacao",
                                                                           "Habitacao",
                                                                           "Saude",
                                                                           "Transportes",
                                                                           "Vestuario")) +
    labs(x = "", y = "Valores", title = paste("IPCA",anos[i], "-", anos[i+1], sep= " "), subtitle = "Desagregações IPCA",
         caption = "Econdata Analytics, fonte: BCB", colour = "") +
    theme(plot.title = element_text(size = 18,
                                    face = "bold")) +
    theme_minimal() 
}

inflacao_atual2 <- inflacao %>%
  select(1:11) %>%
  pivot_longer(-c(date, ipca), names_to = "Tipo", values_to = "Valores")

graph_ipca2 <- inflacao_atual2 %>% filter(year(date) >= 2020) %>% ggplot(aes(x = month(date), y = Valores, fill = Tipo))+
  geom_col(color = "black") +
  geom_line(aes(x = month(date), y = ipca), color = "white", size = 0.8) +
  facet_grid(rows = vars(year(date))) +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = "Meses", y = "Valores", title = "IPCA", subtitle = "Desagregações IPCA",
       caption = "Econdata Analytics, fonte: BCB", fill = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_fill_manual(values = c("ipca_alimentacao_e_bebidas" = "green",
                               "ipca_artigos_de_resid_ncia" = "red",
                               "ipca_comunicacao" = "purple",
                               "ipca_despesas_pessoais" = "#572C00",
                               "ipca_educacao" = "#00FFFF",
                               "ipca_habitacao" = "blue",
                               "ipca_saude_e_cuidados_pessoais" = "orange",
                               "ipca_transportes" = "#017901",
                               "ipca_vestuario" = "magenta"), labels = c("Alimentacao e Bebidas",
                                                                         "Artigos de Residencia",
                                                                         "Comunicacao", 
                                                                         "Despesas Pessoais",
                                                                         "Educacao",
                                                                         "Habitacao",
                                                                         "Saude",
                                                                         "Transportes",
                                                                         "Vestuario")) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.name
  ) +
  theme_minimal() + 
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#Esse inf_temp serve para corrigir um erro, em que ao colocar o labeller no gráfico ipca3, os anos dos facets apareceiam como NA
inf_temp <- inflacao %>% select(1:11)
names(inf_temp) <- c("date", "IPCA","Alimentacao e Beb.","Habitacao","Art. de Residencia","Vestuario","Transportes",
                     "Comunicacao","Saude","Desp. Pessoais","Educacao")
inf_temp <- inf_temp %>%
  pivot_longer(-date, names_to = "Tipo", values_to = "Valores") 

graph_ipca3 <- inf_temp %>% filter(year(date) >= 2018) %>% ggplot(aes(x = month(date), y = Valores, color = Tipo))+
  geom_line(size = 0.75) +
  facet_grid(rows = vars(year(date)),
             cols = vars(Tipo)) +
  scale_y_continuous(breaks = scales::pretty_breaks(3)) +
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = "Meses", y = "Valores", title = "IPCA", subtitle = "Desagregações IPCA",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("IPCA" = "dark blue",
                                "Alimentacao e Beb." = "green",
                                "Art. de Residencia" = "red",
                                "Comunicacao" = "purple",
                                "Desp. Pessoais" = "#572C00",
                                "Educacao" = "#00FFFF",
                                "Habitacao" = "blue",
                                "Saude" = "orange",
                                "Transportes" = "#017901",
                                "Vestuario" = "magenta"),labels = c("IPCA", 
                                                                         "Alimentacao e Bebidas",
                                                                         "Artigos de Residencia",
                                                                         "Comunicacao", 
                                                                         "Despesas Pessoais",
                                                                         "Educacao",
                                                                         "Habitacao",
                                                                         "Saude",
                                                                         "Transportes",
                                                                         "Vestuario")) +
  theme_minimal()

ipca_comercializaveis <- inflacao %>% select(1:2, 12:13) %>%
  pivot_longer(-date, names_to = "Tipo", values_to = "Valores") 


graph_ipca_comercializaveis <- ipca_comercializaveis %>%
  ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Valores", title = "IPCA Comercializáveis",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_comercializaveis" = "red",
                                "ipca_nao_comercializaveis" = "green"),
                     labels = c("IPCA",
                                "IPCA Comercializáveis",
                                "IPCA Não Comercializáveis")) +
  theme_minimal()

graph_ipca_comercializaveis_atual <- ipca_comercializaveis %>% filter(date >= "2018-01-01") %>%
  ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Valores", title = "IPCA Comercializáveis 2018-2022",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_comercializaveis" = "red",
                                "ipca_nao_comercializaveis" = "green"),
                     labels = c("IPCA",
                                "IPCA Comercializáveis",
                                "IPCA Não Comercializáveis")) +
  theme_minimal()

graph_difusao_barra <- inflacao %>% 
  ggplot(aes(x = date, y = indice_de_difusao)) +
  geom_col(fill = "blue", color = "black") +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Índice de Difusão", title = "Índice de Difusão da Inflação (em %)",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  coord_cartesian(ylim=c(40, 80)) +
  theme_minimal()

graph_difusao_linha <- inflacao %>%
  ggplot(aes(x = date, y = indice_de_difusao)) +
  geom_line(color = "blue") +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Índice de Difusão", title = "Índice de Difusão da Inflação (em %)",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  coord_cartesian(ylim=c(40, 80)) +
  theme_minimal()

ipca_duraveis <- inflacao %>% select(1, 2, 19) %>% pivot_longer(-date, names_to = "Tipo", values_to = "Valores")

graph_duraveis <- ipca_duraveis %>%
  ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Valores", title = "IPCA e Bens Duráveis",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_duraveis" = "orange"),
                     labels = c("IPCA",
                                "IPCA Duráveis")) +
  theme_minimal()
  
ipca_subgrupos <- inflacao %>% select(1,2, 20:22) %>% pivot_longer(-date, names_to = "Tipo", values_to = "Valores")

graph_subgrupos <- ipca_subgrupos %>% 
  ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Valores", title = "IPCA e SubGrupos",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_industriais" = "red",
                                "ipca_livres" = "green",
                                "ipca_servicos" = "orange"),
                     labels = c("IPCA",
                                "IPCA Industriais",
                                "IPCA Livres",
                                "IPCA Serviços")) +
  theme_minimal()

graph_subgrupos_atual <- ipca_subgrupos %>% filter(date >= "2018-01-01") %>%
  ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(10)) +
  labs(x = "", y = "Valores", title = "IPCA e SubGrupos 2018-2022",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_industriais" = "red",
                                "ipca_livres" = "green",
                                "ipca_servicos" = "orange"),
                     labels = c("IPCA",
                                "IPCA Industriais",
                                "IPCA Livres",
                                "IPCA Serviços")) +
  theme_minimal()

##### Análise 2020-2022 ##### 

graph_ipca_compilado <- inflacao_atual %>% ggplot(aes(x = date, y = Valores, color = Tipo))+
  geom_line() +
  scale_x_date(date_labels = "%Y",
               breaks = scales::pretty_breaks(5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(2))  +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_alimentacao_e_bebidas" = "green",
                                "ipca_artigos_de_resid_ncia" = "red",
                                "ipca_comunicacao" = "purple",
                                "ipca_despesas_pessoais" = "#572C00",
                                "ipca_educacao" = "#00FFFF",
                                "ipca_habitacao" = "blue",
                                "ipca_saude_e_cuidados_pessoais" = "orange",
                                "ipca_transportes" = "#017901",
                                "ipca_vestuario" = "magenta"),labels = c("IPCA", 
                                                                         "Alimentacao e Bebidas",
                                                                         "Artigos de Residencia",
                                                                         "Comunicacao", 
                                                                         "Despesas Pessoais",
                                                                         "Educacao",
                                                                         "Habitacao",
                                                                         "Saude",
                                                                         "Transportes",
                                                                         "Vestuario")) +
  labs(x = "", y = "Valores", title = "IPCA", subtitle = "Desagregações IPCA",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold")) +
  theme_minimal()

graph_educacao <- inflacao_atual %>% filter((Tipo == "ipca" | Tipo == "ipca_educacao") & year(date) >= 2020) %>% ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(15)) +
  labs(x = "", y = "Valores", title = "IPCA X IPCA Educacao",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_educacao" = "#00FFFF"),labels = c("IPCA", 
                                                                        "Educacao"))
#É possível ver que há um aumento do ipca educacao no começo dos anos com o reajuste da mensalidade das escolas particulares. É possível ver também que há uma grande queda entre julho e outubro de 2020 que foi causada pelos descontos na mensalidade em razão das atividades escolares serem passadas para o ambiente online durante a pandemia.

graph_transportes <- inflacao_atual %>% filter((Tipo == "ipca" | Tipo == "ipca_transportes") & year(date) >= 2020) %>% ggplot(aes(x = date, y = Valores, color = Tipo)) +
  geom_line() +
  scale_x_date(date_labels = "%B/%Y",
               breaks = scales::pretty_breaks(15)) +
  labs(x = "", y = "Valores", title = "IPCA X IPCA Transportes",
       caption = "Econdata Analytics, fonte: BCB", colour = "") +
  theme(plot.title = element_text(size = 18,
                                  face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  scale_color_manual(values = c("ipca" = "dark blue",
                                "ipca_transportes" = "#017901"),labels = c("IPCA", 
                                                                           "Transportes"))
#É possível ver que após recorrentes aumentos do componente de transportes no período pós-pandêmico, em razão das altas do preço do barril de petróleo no mundo todo, há uma queda brusca no último mês gerada pelo corte de impostos sobre combustível

#Outro fato que podemos constatar é que é de extrema importância que o IPCA seja construído como a conjunção de diversos fatores, pois caso não fosse, esse índice seria muito suscetível a mudanças bruscas como as observadas no setor da educação e de transportes, de modo que seria muito mais difícil coordenar a política monetária e fiscal.



##### Plota todos o gráficos ##### 
plot_graficos <- function(){
  for(i in 1:length(graph_ipca1)){
    plot(graph_ipca1[[i]])
  }
  plot(graph_ipca2)
  plot(graph_ipca3)
  plot(graph_ipca_comercializaveis)
  plot(graph_ipca_comercializaveis_atual)
  plot(graph_difusao_barra)
  plot(graph_difusao_linha)
  plot(graph_duraveis)
  plot(graph_subgrupos)
  plot(graph_subgrupos_atual)
}

#Gráficos bons com interatividade
graph_ipca_comercializaveis
plotly::ggplotly()
graph_duraveis
plotly::ggplotly()
graph_subgrupos
plotly::ggplotly()