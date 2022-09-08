
tab <- read.table(paste0("txt/",gsub('/', ' - ', nome),".txt"), 
                  header = FALSE, sep = " ", 
                  col.names = c("cenario",
                                "particula",
                                "Destino", 
                                "Tempo Chegada", 
                                "Tempo Trajeto"))
tab$cenario <- as.factor(tab$cenario)