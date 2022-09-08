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
    source("source/[Output] Destino das partículas HLES.R", encoding = "UTF-8")
}

#  OBS: cada nível de pasta até chegar nos arquivos repete uma vez [^/]*[/] antes da | na linha 52

# -------------------------------------------------------------------------
# Leitura do arquivo de logboom 
# -------------------------------------------------------------------------

arquivo <- readMat("mat/logboom.mat")

lbl1 <-data.frame(
    x = arquivo$data[[1]][1:27,1],
    y = arquivo$data[[2]][1:27,1]
)

lbl2 <-data.frame(
    x = arquivo$data[[1]][29:54,1],
    y = arquivo$data[[2]][29:54,1]
)

# -------------------------------------------------------------------------
# Leitura do arquivo do contorno
# -------------------------------------------------------------------------

arquivo <- readMat("mat/contorno.mat")

contorno <-data.frame(
    x = arquivo$data[[1]],
    y = arquivo$data[[2]]
)

# -------------------------------------------------------------------------
# Leitura do arquivo de vazões 
# -------------------------------------------------------------------------

vazao <- read.csv2("csv/vazoes.csv")

