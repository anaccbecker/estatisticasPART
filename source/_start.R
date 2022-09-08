# CLICAR EM SESSION, SET WORKING DIRECTORY,  SELECIONAR A PASTA "ESTATISTICAS PART"
# ATENÇÃO: ESCREVER PASTA COM OS DADOS NA LINHA 34

# -------------------------------------------------------------------------
# Bibliotecas 
# -------------------------------------------------------------------------

library(R.matlab)
library(tidyverse)
library(patchwork)
library(stringi)
#library(htmlwidgets)
#library(gganimate)  #gif

# -------------------------------------------------------------------------
# Configurações
# -------------------------------------------------------------------------

theme_set(theme_bw())
options(
    OutDec = ",",
    scipen = 999
)

# -------------------------------------------------------------------------
# Funções
# -------------------------------------------------------------------------

source("source/_functions.R", encoding = "UTF-8")

# -------------------------------------------------------------------------
# Leitura dos dados - ESCOLHA A PASTA
# -------------------------------------------------------------------------

pasta <- "mat/Relatório Julho/Todos"
old_titles <- c('c1_sem','ce_sem','c1_com','ce_com','c1_desv','ce_desv','cb_desv','cf_desv')  #sem espaço
new_titles  <- c('Cenário 1\nsem estruturas de retenção','Cenário E\nsem estruturas de retenção', 
                 'Cenário 1\ncom log boom','Cenário E\ncom log boom', 
                 'Cenário 1\ncom log boom e desviador/retentor','Cenário E\ncom log boom e desviador/retentor', 
                 'Cenário B\ncom log boom e desviador/retentor','Cenário F\ncom log boom e desviador/retentor')


source("source/_readData.R", encoding = "UTF-8")
df$cenario <- stri_replace_all_regex(df$cenario,pattern= old_titles,replacement= new_titles,vectorize=FALSE)
df$cenario <- factor(df$cenario, levels = new_titles)


source("source/readTab.R", encoding = "UTF-8")
tab$cenario <- stri_replace_all_regex(tab$cenario,pattern= old_titles,replacement= new_titles,vectorize=FALSE)
tab$cenario <- factor(tab$cenario, levels = new_titles)


# Caso queira salvar RDS pra não ter que fazer a leitura de novo:
# saveRDS(df, paste0("RDS/",gsub('/', ' - ', nome),".rds"))

# -------------------------------------------------------------------------
# Geração dos gráficos automáticos
# -------------------------------------------------------------------------

source("source/[Planta] Profundidade em cores.R", encoding = "UTF-8")
source("source/[Histogram] Profundidades.R", encoding = "UTF-8")
source("source/[Planta] Destino em cores.R", encoding = "UTF-8")
source("source/[Piechart] Destino das partículas.R", encoding = "UTF-8")
source("source/[Boxplot] Tempo de chegada.R", encoding = "UTF-8")
source("source/[3x1] Seções Transversais - todas.R", encoding = "UTF-8")
source("source/[3x1] Seções Transversais - primeira1.R", encoding = "UTF-8")



