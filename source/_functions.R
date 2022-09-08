# -------------------------------------------------------------------------
# Filtro de seção transversal
# -------------------------------------------------------------------------

df_secao <-  data.frame(x = NA, y = NA, z= NA, t = NA, particula = NA, nomeArquivo = NA)
.FiltraSecao <- function (df=df, x1, y1, x3, y3, delta=40){
    x2 <- x1 + delta/2      
    y2 <- y1 + delta/2
    x4 <- x3 + delta/2
    y4 <- y3 + delta/2
    
    x5 <- x1 - delta/2
    y5 <- y1 - delta/2
    x6 <- x3 - delta/2
    y6 <- y3 - delta/2
  
    m <- (y5-y6)/(x5-x6)
    m2<- (y5-y2)/(x5-x2)
    df_secao <- filter(df, y > y6+m*(x-x6) & 
                           y < y4+m*(x-x4) &
                           y > min(y5,y6) &
                           y < max(y5,y6)) %>% 
        mutate(Secao = paste(round(x1,0),round(y1,0),round(x3,0),round(y3,0),delta),
               x1=x1,
               y1=y1,
               x3=x3,
               y3=y3
        )
    df_secao
}

# -------------------------------------------------------------------------
# Orientação da seção transversal
# -------------------------------------------------------------------------

.OrientacaoX <- function(dm, maximo, minimo, lon){
    if (lon == F){
        return(abs(dm-maximo))
    }
    else if (lon == T){
        return(dm-minimo)
    }
}


# -------------------------------------------------------------------------
# Leitura dos dados de fundo
# -------------------------------------------------------------------------

.readFundo <- function(name){
    read.csv2(name, sep=",", skip = 1) %>% 
        mutate(dp= sqrt(as.numeric(x.coordinate)^2+as.numeric(y.coordinate)^2),
               dm= dp-min(dp),
               z = -as.numeric(water.depth..m.),
               dm_max = max(dm),
               dm_min = min(dm))
}


# -------------------------------------------------------------------------
# Interpretação dos dados do PART
# -------------------------------------------------------------------------

.readPART <- function (arquivo){
    df <- tibble()
    i <- 1
    for (i in seq(1,ncol(arquivo$data[[1]]))){
        df_add <-data.frame(
            x = arquivo$data[[1]][,i],
            y = arquivo$data[[2]][,i],
            z = arquivo$data[[5]][,i],
            t = arquivo$data[[7]],
            particula = i
        )
        i <- i+1
        df <- bind_rows(df, df_add)
    }
    return(df)
}

# -------------------------------------------------------------------------
# Funções Auxiliares
# -------------------------------------------------------------------------

contar <- function (df){
    df%>% 
        group_by(particula) %>%
        count() %>% 
        nrow()
}

particulas <- function (df){
    df%>% 
        group_by(particula,cenario) %>%
        count() %>% 
        mutate(tempo_min = min(df$t))
}

# -------------------------------------------------------------------------
# Insere NAs no dataframe (entre os grupos selecionados)
# -------------------------------------------------------------------------

add_blank <- function(x, n=5) {
  tibble::add_row(x, x=rep(NA, n), y=rep(NA, n))
}
