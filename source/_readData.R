# -------------------------------------------------------------------------
# Leitura dos resultados 
# -------------------------------------------------------------------------

nome <- gsub("^[^/]*[/]|.mat$", "", pasta)

if (file.exists(paste0("RDS/",gsub('/', ' - ', nome),".rds"))) {
    ## Leitura do arquivo RDS com a base já processada:
    df <- readRDS(paste0("RDS/",gsub('/', ' - ', nome),".rds")) 
    
} else {
    df <- dir(pasta,
              full.names = T,
              recursive = T) %>% 
        map(function(a){
            suppressMessages(suppressWarnings(
                readMat(a)
            )) %>% 
                .readPART() %>% 
                mutate(cenario = gsub("^[^/]*[/][^/]*[/][^/]*[/]|.mat$", "", a))    
        })   %>% 
        bind_rows()%>% 
        na.omit()
    # Comente a linha a seguir caso o destino da partícula não importe no momento:
    source("source/[Output] Destino das partículas.R", encoding = "UTF-8")
}

#  OBS: cada nível de pasta até chegar nos arquivos repete uma vez [^/]*[/] antes da | na linha 52

# -------------------------------------------------------------------------
# Leitura do arquivo de logboom 
# -------------------------------------------------------------------------

arquivo <- readMat("mat/logboom.mat")

#log boom esquerdo
lbl1 <-data.frame(
    x = arquivo$data[[1]][1:27,1],
    y = arquivo$data[[2]][1:27,1]
)

#log boom direito
lbl2 <-data.frame(
    x = arquivo$data[[1]][29:54,1],
    y = arquivo$data[[2]][29:54,1]
)


# -------------------------------------------------------------------------
# Leitura do arquivo de retentor/desviador 
# -------------------------------------------------------------------------



#retentor/desviador direito 
rdmd <-data.frame(
    x = c(318685.5949, 319183.4864),
    y = c(8973639.2888, 8975164.1922)
)

#retentor/desviador esquerdo
rdme <-data.frame(
    x = c(317077.6306, 318189.4475),
    y = c(8974056.7943, 8974903.835)
)

# -------------------------------------------------------------------------
# Leitura do arquivo do contorno
# -------------------------------------------------------------------------

arquivo <- readMat("mat/contorno_novo_com_diques.mat")

contorno <-data.frame(
    x = arquivo$data[[1]],
    y = arquivo$data[[2]]
)

#Remoção do dique inconveniente do meio do caminho
dique_remover <- contorno %>% 
    filter(x > 318100 & x < 318600 & y> 8975000 & y < 8975800)

contorno <-  anti_join(contorno, dique_remover)
