nRTools::createDir(paste0("txt/df/",gsub('/', ' - ', nome)))
# -------------------------------------------------------------------------
# Transformação da base de dados
# -------------------------------------------------------------------------

dic <- data.frame(cenario = unique(df$cenario), cenario_n = seq(1:length(unique(df$cenario))))
df2 <- left_join(df, dic)

# -------------------------------------------------------------------------
# Função para descobrir o primeiro destino das particulas
# -------------------------------------------------------------------------

i <- 1
j <- 1
k <- 1
cat("\n")
.inSecao <- function(df){
    CFME.x1 <- 318975
    CFME.y1 <- 8977000
    CFME.x3 <- 318975
    CFME.y3 <- 8977800
    CFME.delta <- 200
    
    CFME.x2 <- CFME.x1 + CFME.delta                     
    CFME.y2 <- CFME.y1 + CFME.delta
    CFME.x4 <- CFME.x3 + CFME.delta
    CFME.y4 <- CFME.y3 + CFME.delta
    CFME.m <- (CFME.y1-CFME.y3)/(CFME.x1-CFME.x3)
    
    CFMD.x1 <- 320000
    CFMD.y1 <- 8974450
    CFMD.x3 <- 319152
    CFMD.y3 <- 8975060
    CFMD.delta <- 500
    
    CFMD.x2 <- CFMD.x1 + CFMD.delta                     
    CFMD.y2 <- CFMD.y1 + CFMD.delta
    CFMD.x4 <- CFMD.x3 + CFMD.delta
    CFMD.y4 <- CFMD.y3 + CFMD.delta
    CFMD.m <- (CFMD.y1-CFMD.y3)/(CFMD.x1-CFMD.x3)
    
    VER.x1 <- 319152
    VER.y1 <- 8975060
    VER.x3 <- 318625
    VER.y3 <- 8975378
    VER.delta <- 500
    
    VER.x2 <- VER.x1 + VER.delta                     
    VER.y2 <- VER.y1 + VER.delta
    VER.x4 <- VER.x3 + VER.delta
    VER.y4 <- VER.y3 + VER.delta
    VER.m <- (VER.y1-VER.y3)/(VER.x1-VER.x3) 
    
    VT.x1 <- 318850
    VT.y1 <- 8975850
    VT.x3 <- 318850
    VT.y3 <- 8976200
    VT.delta <- 125
    
    VT.x2 <- VT.x1 + VT.delta                     
    VT.y2 <- VT.y1 + VT.delta
    VT.x4 <- VT.x3 + VT.delta
    VT.y4 <- VT.y3 + VT.delta
    VT.m <- (VT.y1-VT.y3)/(VT.x1-VT.x3) 
    
    df<- df %>% mutate(i=1:nrow(df), LocalAtual = "undefined")
        for (i in 1:nrow(df)){
                if (df[i, "y"] >= CFMD.y3+CFMD.m*(df[i, "x"]-CFMD.x3) & 
                    df[i, "y"] <= CFMD.y4+CFMD.m*(df[i, "x"]-CFMD.x4) &
                    df[i, "y"] >= min(CFMD.y1,CFMD.y3) &
                    df[i, "y"] <= max(CFMD.y1,CFMD.y3)){
                    df[i, "LocalAtual"] = "CFMD"
                } else if (df[i, "y"] >= VER.y3+VER.m*(df[i, "x"]-VER.x3) & 
                           df[i, "y"] < VER.y4+VER.m*(df[i, "x"]-VER.x4) &
                           df[i, "y"] >= min(VER.y1,VER.y3) &
                           df[i, "y"] <= max(VER.y1,VER.y3)) {
                    df[i, "LocalAtual"] = "VER"
                } else if (df[i, "y"] >= CFME.y3+CFME.m*(df[i, "x"]-CFME.x3) & 
                           df[i, "y"] <= CFME.y4+CFME.m*(df[i, "x"]-CFME.x4) &
                           df[i, "y"] >= min(CFME.y1,CFME.y3) &
                           df[i, "y"] <= max(CFME.y1,CFME.y3)){
                    df[i, "LocalAtual"] = "CFME"
                } else if (df[i, "y"] >= VT.y3+VT.m*(df[i, "x"]-VT.x3) & 
                           df[i, "y"] <= VT.y4+VT.m*(df[i, "x"]-VT.x4) &
                           df[i, "y"] >= min(VT.y1,VT.y3) &
                           df[i, "y"] <= max(VT.y1,VT.y3)) {
                    df[i, "LocalAtual"] = "VT"
                } else {
                    df[i, "LocalAtual"] = "Reservatório"
                }

            if (df[i, "LocalAtual"] == "CFMD"|
                df[i, "LocalAtual"] == "CFME"|
                df[i, "LocalAtual"] == "VER"|
                df[i, "LocalAtual"] == "VT"){
                print(paste(df[i, "cenario"],df[i, "particula"], df[i, "LocalAtual"]))
                write(paste(df[i, "cenario"],
                            df[i, "particula"], 
                            df[i, "LocalAtual"],
                            (df[i,"t"]-738522)*24,
                            (df[i,"t"]-min(df$t))*24),
                      file=paste0("txt/",gsub('/', ' - ', nome),".txt"),
                      append=TRUE)
                break

            }            
            cat(sprintf("\r- Progresso: %.2f%%", i/nrow(df)))
            i <- i + 1

        }

return(df)
}

# -------------------------------------------------------------------------
# Aplicação da função em cada cenário e partícula
# -------------------------------------------------------------------------

for(j in unique(df2$cenario_n)){
    df2$particula <- as.numeric(df2$particula)
    df3 <- df2 %>% filter(cenario_n==j)
    for (k in unique(df3$particula)){
        df4 <- df3 %>% filter(particula==k)
        df_teste<- df4 %>% 
            .inSecao() 
        write.csv2(df_teste,
              file=paste0("txt/df/",gsub('/', ' - ', nome),"/",gsub('/', ' - ', nome), ' - ',j , ' - ',k,".csv"))
        k <- k+1
        }
j <- j+1
}


# -------------------------------------------------------------------------
# Adicionando o destino das partículas ao data frame
# -------------------------------------------------------------------------

tab <- read.table(paste0("txt/",gsub('/', ' - ', nome),".txt"), 
                  header = FALSE, sep = " ", 
                  col.names = c("cenario",
                                "particula",
                                "Destino", 
                                "Tempo Chegada", 
                                "Tempo Trajeto"))
tab$cenario <- as.factor(tab$cenario)
tab$cenario <- stri_replace_all_regex(tab$cenario,pattern= old_titles,replacement= new_titles,vectorize=FALSE)
tab$cenario <- factor(tab$cenario, levels = new_titles)

df_2 <- dir(paste0("txt/df/",gsub('/', ' - ', nome)),
    full.names = T,
    recursive = T) %>% 
    map(function(x){
        read.csv2(x)
    }) %>% 
    bind_rows()%>% 
    filter(LocalAtual != 'undefined') %>% 
    mutate(cenario = as.factor(cenario))

df_2$cenario <- stri_replace_all_regex(df_2$cenario,pattern= old_titles,replacement= new_titles,vectorize=FALSE)
df_2$cenario <- factor(df_2$cenario, levels = new_titles)

df <- left_join(df_2,
                tab)

df$Destino[is.na(df$Destino)] <- "Reservatório"

df<- df %>%
  mutate(Destino= factor(Destino, levels = c("CFME","CFMD","VER","VT", "Reservatório")))

saveRDS(df, paste0("RDS/",gsub('/', ' - ', nome),".rds"))

