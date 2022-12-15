# Phylogenetic Tree Visualization
## install.packages("devtools")
devtools::install_github("GuangchuangYu/treeio")

library("treeio")
library("ggtree")

nwk <- system.file("extdata", "sample.nwk", package="treeio")
tree <- read.tree(nwk)

ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()

beast_file <- system.file("examples/MCC_FluA_H3.tree", 
                          package="ggtree")

beast_tree <- read.beast(beast_file)

ggtree(beast_tree, mrsd="2013-01-01") + theme_tree2()

ggtree(tree, layout="circular") + geom_tiplab(aes(angle=angle), color='blue')

library(ggplot2)
ggtree(beast_tree, aes(color=rate)) +
  scale_color_continuous(low='darkgreen', high='red') +
  theme(legend.position="right")

#  Combining tree with external data
nhxfile <- system.file("extdata/NHX", "phyldog.nhx", package="treeio")
nhx <- read.nhx(nhxfile)
# write.beast(nhx, file = "phyldog.tree")
write.beast(nhx)

mlcfile <- system.file("extdata/PAML_Codeml", "mlc", package="treeio")
ml <- read.codeml_mlc(mlcfile)
# write.beast(ml, file = "codeml.tree")
write.beast(ml)

phylo <- as.phylo(nhx)
## print the newick text
write.tree(phylo)
plot(phylo)

N <- Nnode2(phylo)
library(tidyverse)
fake_data <- tibble(node = 1:N, fake_trait = rnorm(N), another_trait = runif(N))
fake_tree <- full_join(phylo, fake_data, by = "node")
write.beast(fake_tree)


# read data ---------------------------------------------------------------
raw_data <- read.csv(here::here("data", "osm-raw-data_AR.csv"), sep=";")
data <- raw_data[-which(duplicated(raw_data$SpecCode)==TRUE),]
data$Genus.species <- gsub("[.]","_",data$ï..Genus.species)
species_list <- data$Genus.species
length(species_list)

# read results ------------------------------------------------------------

res_afrotropics <- readRDS(here::here("output", "phylo_afrotropics.rds"))
res_indomalay <- readRDS(here::here("output", "phylo_indomalay.rds"))
res_neartic <- readRDS(here::here("output", "phylo_neartic.rds"))
res_neotropic <- readRDS(here::here("output", "phylo_neotropic.rds"))

res_afrotropics$Insertions_data$basin  <- "Afrotropic"
res_indomalay$Insertions_data$basin  <- "Indomalay"
res_neartic$Insertions_data$basin  <- "Neartic"
res_neotropic$Insertions_data$basin  <- "Neotropic"

new_data <- rbind(res_afrotropics$Insertions_data, res_indomalay$Insertions_data, 
                  res_neartic$Insertions_data, res_neotropic$Insertions_data)
new_data$insertions <- as.factor(new_data$insertions)
new_data$insertions2 <- factor(new_data$insertions, levels = c("Present_in_Tree", "Congeneric_insertion", "Congeneric_insertion_roundFamily", "Family_insertion", "Order_insertion"))
head(new_data)

# Stacked + percent
library(ggplot2)
ggplot(new_data, aes(x = insertions2, group = basin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  labs(y = "Percent", fill = "Insertion type") +
  facet_wrap(~basin) +
  scale_y_continuous(labels = scales::percent)  +
  scale_x_discrete(labels = c("Present in Tree", 
                              "Congeneric insertion",
                              "Family insertion",
                              "Congeneric at Family",
                              "Order insertion")) +
  rcartocolor::scale_fill_carto_d(palette = "SunsetDark", 
                                  direction = 1) +
  theme(legend.position = "none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# read the tree for all species

library(tidyverse)
sum(phylo_all$tip.label == "Paraneetroplus_synspilus")
new_data[is.na(match(new_data$s, phylo_all$tip.label)), "s"]

phylo_all <- readRDS(file = here::here("output", "phylo_all.rds"))
str(phylo_all)

ordered()
tree_full <- full_join(phylo_all, new_data, by = "s")
library(ggtree)
ggtree(phylo_all, layout = "circular")
