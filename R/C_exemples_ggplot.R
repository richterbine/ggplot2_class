#### CONCEITOS BASICOS DE GGPLOT2 ####
###### ALINE RICHTER 06/05/20 ######

library(ggplot2)
library(rgdal)
library(dplyr)
library(ggrepel)
library(viridis)
library(grid)
library(ggridges)
#library(RcolorBrewer)
library(cowplot)
#install.packages("mapproj")
library(mapproj)

# funcao que permite plotar varios objetos do ggplot
source(here::here("R", "functions", "Multiplot_function.R"))

## carregar os dados que serao usados
data<- read.table(here::here("data", "TUR_loggers.txt"), header = T) # dados de riqueza e aundancia
coords<- read.table(here::here("data", "coords_dataset.txt"), header = T) # Dataset de frugivoras
data$Month_f = factor(data$Month, levels=c("October", "November", "December", "January", "February", "March"))

##################################################################################
#### 1.Barplots ####
p1<- ggplot(data, aes(x= Area)) + geom_bar(aes(fill= Strata, weight= Richness))
p1
  
p1 + theme(title = element_text(size = rel(1.5), angle = 75),
                            plot.background = element_rect(fill = "gray80"))

p1.1<- ggplot(data, aes(x = Mean_day, y = Month_f, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [ºC]", option = "C") +
  labs(title = 'Temperaturas') 
p1.1

#### 2.Boxplots ####
p2<- ggplot(data) + geom_boxplot(aes(x= Strata, y= Richness, colour= Month))
p2

p2 + coord_flip() # Rotate the box plot

p2 + scale_x_discrete(limits= c("Canopy")) # escolhendo quais dados plotar

p2 + facet_grid(~Month) # ou plotando em dois graficos

ggplot(data) + geom_boxplot(aes(x= Strata, y= Richness, colour= Month), 
                            outlier.alpha = .3, outlier.shape = 2) # mudando os outliers

# ordenando os meses
p2 + facet_grid(~Month) +
  scale_color_discrete(limits= c("October", "November", "December", "January", "February", "March"))

# so mudou a ordem na legenda
p2<- ggplot(data) + geom_boxplot(aes(x= Strata, y= Richness, colour= Month_f))

p2 + facet_grid(~ Month_f)

p2.1<- ggplot(data) + geom_violin(aes(x= Month_f, y= Richness)) # juntando densidade com um boxplot
p2.1

#### 3. Linhas ####
p3<- ggplot(data, aes(y= Richness, x= Richness))
p3 + geom_line(aes(colour=Strata))

p3 + geom_line(arrow= arrow(type = "closed"), aes(colour= Strata), 
               linetype= 1, size= 1) # colocar setas e mudar o tipo de linha

p3 + geom_line(aes(colour= Shannon)) +
  scale_color_viridis_c(direction = -1, option = "A") # trocar para B, C ou E, muda a paleta de cores

# exemplo
pred.spp<- read.table(file = here::here("data", "pred_spp.txt"))

posit<- which(pred.spp[,1]==c("Caligo_martia", "Zaretis_strigosus", 
                      "Morpho_epistrophus", "Taygetis_ypthima", "Epiphile_orea"))
pred.sel<- pred.spp[posit, ]

p3.1<- ggplot(pred.sel, aes(x= o.temp, y=predict, colour=species)) + geom_line() +
  scale_y_sqrt() + theme(legend.position="right") + scale_color_viridis_d(option = "A") +
  ylab("Abundancia esperada") + xlab("Temperatura (ºC)")
p3.1

#### 4. Pontos ####
p4<- ggplot(data, aes(x= Mean_day, y= Richness, colour= Strata)) +
  geom_point(na.rm = T)
p4

## adicionando a linha de tendencia
m4<- glm(Richness ~ Mean_day + Strata, data = data, family = poisson)
summary(m4)
p4.1<- p4 + geom_abline(intercept = exp(coef(m4)[1]), slope = exp(coef(m4)[2]))
p4.2<- p4 + geom_smooth(method = "glm",formula = y~x+colour, se=T, method.args = list(family = "poisson"))

cowplot::plot_grid(p4.1 + theme(legend.position = "none"),
                   p4.2 + theme(legend.position = "none"), ncol = 2) 

## separando os niveis do fator em dois graficos distintos
p4.3<- p4 + geom_smooth(aes(fill= Strata), method = "glm") + facet_grid(~Strata)
p4.3

## Modificando argumentos textuais
windowsFonts()

p4.3 + labs(title="Grafico 1",tag = "a)", subtitle="subtitulo", y="Abundancia",
          x="Temperatura media do dia", caption = "dados da bine", fill= "Estrato") +
  theme(title = element_text(size = 15, family = "serif")) # modifica fonte e tamanho dos titulos

## modificando fonte e tamanho separadamente para parametros internos
# legenda
p4.3 + theme(axis.text = element_text(family= "serif",face = "italic", 
                                    size = 12, angle = 45), legend.title = element_text(colour = "blue", size = 20),
      legend.text = element_text(colour = "red", size= 15))

# valores e fonte dos eixos separadamente
p4.3 + theme(axis.text.x = element_text(face = "italic", color = "#993333", 
                                      size = 12, angle = 45),
           axis.text.y = element_text(family = "sans", face = "bold", color = "blue", 
                                      size = 12, angle = 45)) # Times New Roman

#### 5. Mapas ####
head(coords)
names(coords)[4]<- "Richness"

BR <- map_data("world")%>% filter(region=="Brazil")

# sobrepondo os pontos com o shapefile
map.bfly<- ggplot() +
  geom_polygon(data = BR, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data= coords, aes(x=Longitude, y=Latitude, colour= Richness)) +
  coord_map() # + theme_void
map.bfly

# encontrando os sitios com maior abundancia
map.bfly + geom_text_repel(data= coords %>% arrange(Richness) %>% tail(5), aes(x=Longitude, y=Latitude, label=Sites), size=5) +
  geom_point(data= coords %>% arrange(Richness) %>% tail(5), aes(x=Longitude, y=Latitude), color="red", size=3)
 
# usando tamanhos e cores para mapear os pontos
ggplot() +
  geom_polygon(data = BR, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data= coords, aes(x=Longitude, y=Latitude, size= Richness, colour= Richness)) +
  scale_size_continuous(range=c(1,5)) +
  scale_color_viridis(option = "C") # + tans= "log"
 
# Build the map
mybreaks <- c(20, 50, 70, 90, 120)

p5<- coords %>%
  ggplot() +
  geom_polygon(data = BR, aes(x=long, y = lat, group = group), fill="grey", alpha=0.4) +
  geom_point(  aes(x= Longitude, y= Latitude, size= Richness, color= Richness, alpha= Richness), 
               shape=20, stroke=FALSE) +
  scale_size_continuous(name="Riqueza", range=c(1,8), breaks=mybreaks) +
  scale_alpha_continuous(name="Riqueza", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", breaks=mybreaks, name="Riqueza") +
  coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("Dataset borboletas frugívoras") + xlab("Longitude") + ylab("Latitude") +
  theme(
    legend.position = "right", #c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 10, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
p5


# exportando a figura
tiff(here::here("data", "mapa_abundancia.tif"), height = 10, width = 17.4, res = 300, units = "cm")
p5
dev.off()

# outra forma de salvar o plot
cowplot::save_plot(here::here("data/Mapa2.png"), 
                   p5,
                   base_height = 6, base_width = 8)

library(ggplot2)
library(cowplot)
p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line()
plot.mpg <- ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + geom_point(size=2.5)

# Note that these cannot be aligned vertically due to the legend in the plot.mpg
ggdraw(plot_grid(p1, plot.mpg, ncol=1, align='v'))

# now extract the legend
legend <- get_legend(plot.mpg)

# and replot suppressing the legend
plot.mpg <- plot.mpg + theme(legend.position='none')

# Now plots are aligned vertically with the legend to the right
ggdraw(plot_grid(plot_grid(p1, plot.mpg,p4, legend, ncol=2, align='h'),
                # plot_grid(NULL, legend, ncol=1, align = "hv"),
                 rel_widths=c(1, 0.2)))


### plotting with cowplot
library(ggplot2)
library(cowplot)

df <- data.frame(
  x = 1:10, y1 = 1:10, y2 = (1:10)^2, y3 = (1:10)^3, y4 = (1:10)^4
)

p1 <- ggplot(df, aes(x, y1)) + geom_point()
p2 <- ggplot(df, aes(x, y2)) + geom_point()
p3 <- ggplot(df, aes(x, y3)) + geom_point()
p4 <- ggplot(df, aes(x, y4)) + geom_point()
p5 <- ggplot(mpg, aes(as.factor(year), hwy)) +
  geom_boxplot() +
  facet_wrap(~class, scales = "free_y")

# simple grid
plot_grid(p1, p2, p3, p4)

# simple grid with labels and aligned plots
plot_grid(
  p1, p2, p3, p4,
  labels = c('A', 'B', 'C', 'D'),
  align="hv"
)

# manually setting the number of rows, auto-generate upper-case labels
plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v"
)

# making rows and columns of different widths/heights
plot_grid(
  p1, p2, p3, p4,
  align = 'hv',
  rel_heights = c(2,1),
  rel_widths = c(1,2)
)

# aligning complex plots in a grid
plot_grid(
  p1, p5,
  align = "h", axis = "b", nrow = 1, rel_widths = c(1, 2)
)

# more examples
# }
# NOT RUN {
#' # missing plots in some grid locations, auto-generate lower-case labels
plot_grid(
  p1, NULL, NULL, p2, p3, NULL,
  ncol = 2,
  labels = "auto",
  label_size = 12,
  align = "v"
)

# can align top of plotting area as well as bottom
plot_grid(
  p1, p5,
  align = "h", axis = "tb",
  nrow = 1, rel_widths = c(1, 2)
)

# other types of plots not generated with ggplot
p6 <- ~{
  par(
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0)
  )
  plot(sqrt)
}

p7 <- function() {
  par(
    mar = c(2, 2, 1, 1),
    mgp = c(2, 1, 0)
  )
  image(volcano)
}
p8 <- grid::circleGrob()

plot_grid(p1, p6, p7, p8, labels = "AUTO", scale = c(1, .9, .9, .7))
# }
