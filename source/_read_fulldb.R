full_db <-
    dir('txt/df',
        full.names = T,
        recursive = T) %>% 
    map(function(x){
        read.csv2(x)
    }) %>% 
    bind_rows()%>% 
    filter(LocalAtual != 'undefined')
 
