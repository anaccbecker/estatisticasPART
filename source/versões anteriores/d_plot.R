# Falta remover x> ... y> .... para arrumar escala :)

View(na.omit(df))
# Lineplot em planta com usina---------------------------------------------------------------
plot <- 
    ggplot (data = NULL)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z, group =particula), 
               size =0.5, alpha = 0.5)+
    geom_path(data = usina, 
               aes(x = x,y = y), 
               color = 'black', size =1)+
    scale_color_gradientn (colors = rainbow (12))+#, 
    #breaks = c(-40,-30,-20,-10,0,10,20), 
    #limits = c(-10,0))+
    labs (x = "Coordenada x",
          y = "Coordenada y",
          color = "Profundidade (m)")+
    coord_fixed()

plot

ggsave(
    filename = sprintf("[Lineplot] Profundidade [%s].png",df$nomeArquivo[1]),
    path = nRTools::createDir("img"),
    plot = plot,
    device = "png",
    width = 20,
    height = 20,
    units = "cm"
)

htmlwidgets::saveWidget(plotly::as_widget(plotly::ggplotly(plot)),
                        file = sprintf("[Lineplot] Profundidade [%s].html",df$nomeArquivo[1]))
file.rename(
    from = sprintf("[Lineplot] Profundidade [%s].html",df$nomeArquivo[1]),
    to =   sprintf("html/[Lineplot] Profundidade [%s].html",df$nomeArquivo[1])
)

# Lineplot em planta por particula--------------------------------------------------------------
plot <- 
    ggplot (data = NULL)+
    geom_path(data = filter(df, particula ==3), 
               aes(x = x, y = y, color = particula), 
               size =0.5, alpha = 0.5)+
    geom_path(data = usina, 
              aes(x = x,y = y), 
              color = 'black', size =1)+
    #scale_color_gradientn (colors = rainbow (12))+#, 
    #breaks = c(-40,-30,-20,-10,0,10,20), 
    #limits = c(-10,0))+
    labs (x = "Coordenada x",
          y = "Coordenada y",
          color = "Profundidade (m)")+
    coord_fixed()

plot

# Lineplot em planta com log boom---------------------------------------------------------------
plot <- 
    ggplot (data = NULL)+
    geom_line (data = df, 
               aes(x = x, y = y, color = z, group =particula), 
               size =0.5, alpha = 0.5)+
    geom_point(data = df, 
               aes(x = x, y = y, color = z, group =particula), 
               size =0.5, alpha = 0.5)+
    geom_line (data = lbl1, 
               aes(x = x,y = y), 
               color = 'red', size =1.5)+
    geom_line (data = lbl2, 
              aes(x = x,y = y), 
              color = 'red', size =1.5)+
    scale_color_gradientn (colors = rainbow (12))+#, 
                           #breaks = c(-40,-30,-20,-10,0,10,20), 
                           #limits = c(-10,0))+
    labs (x = "Coordenada x",
          y = "Coordenada y",
          color = "Profundidade (m)")+
    coord_fixed()

plot

ggsave(
    filename = sprintf("[Lineplot] Profundidade [%s].png",df$nomeArquivo[1]),
    path = nRTools::createDir("img"),
    plot = plot,
    device = "png",
    width = 20,
    height = 20,
    units = "cm"
)

htmlwidgets::saveWidget(plotly::as_widget(plotly::ggplotly(plot)),
                        file = sprintf("[Lineplot] Profundidade [%s].html",df$nomeArquivo[1]))
file.rename(
    from = sprintf("[Lineplot] Profundidade [%s].html",df$nomeArquivo[1]),
    to =   sprintf("html/[Lineplot] Profundidade [%s].html",df$nomeArquivo[1])
)


# Lineplot xz ----------------------------------------------------------------------

plot <- 
    ggplot (data = NULL)+
    geom_point(data = df, aes(x = x, y = z, color = z, group =particula), size =0.5)+
    scale_color_gradientn(colors = rainbow (6))

plot

# Lineplot yz ----------------------------------------------------------------------

plot <- 
    ggplot (data = NULL)+
    geom_point(data = df, aes(x = y, y = z, color = z, group =particula), size =0.5)+
    scale_color_gradientn(colors = rainbow (6))

plot

# Lineplot outro ----------------------------------------------------------------------

plot <- 
    ggplot (data = NULL)+
    geom_point(data = df, aes(x = sqrt(x^2+y^2), y = z, color = z, group =particula), size =0.5)+
    scale_color_gradientn(colors = rainbow (6))

plot

# Lineplot outro ----------------------------------------------------------------------

plot <- 
    ggplot (data = NULL)+
    geom_point(data = df, aes(x = 1/sqrt(x^2+y^2), y = z, color = z, group =particula), size =0.5)+
    scale_color_gradientn(colors = rainbow (6))

plot

# Histograma ----------------------------------------------------------------------

plot <- df %>% 
    ggplot (aes( z, group = particula))+
    geom_histogram()+
    coord_flip()+
    labs(x = "Profundidade (m)", y = "Número de partículas")+
    scale_y_continuous(limits = c(0,20000))+
    scale_x_continuous(limits = c(-50,2))

plot

ggsave(
    filename = sprintf("[Histogram] Profundidade [%s].png",df$nomeArquivo[1]),
    path = nRTools::createDir("img"),
    plot = plot,
    device = "png",
    width = 10,
    height = 10,
    units = "cm"
)


# lineplot ------------------------------------------------------------------

df_plot <- df %>% 
    na.omit() %>% 
    filter(particula == 1) %>% 
    mutate(t = round(t-737481,1)) %>% 
    group_by(t, particula) %>% 
    summarise(t = mean(t),z = mean(z), particula= mean(particula), n = n())
df_plot$t <- as.numeric(df_plot$t)
plot <- df_plot %>% 
    ggplot (aes( x = t, y = z), color = particula)+ 
    geom_line()+
    labs(x = "Tempo (dias)", y = "Profundidade (m)")
plot


# Boxplot profundidade ao longo do tempo ----------------------------------
df$particula <- as.numeric(df$particula )
df_plot <- df %>% 
    na.omit() %>% 
    filter(particula == 1) %>% 
    mutate(t = round(t-737481,1)) %>% 
    group_by(t, particula) %>% 
    summarise(t = mean(t),z = mean(z), particula= mean(particula), n = n())
View(df_plot)

df_plot$t <- as.factor(df_plot$t )
plot <- df_plot %>% 
    ggplot (aes( x = t, y = z))+ 
    geom_boxplot()+
    labs(x = "Tempo (dias)", y = "Profundidade (m)")
plot

