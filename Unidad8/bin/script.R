##Análisis de datos de Illumina desde amptk

install.packages("vegan")
BiocManager::install("phyloseq")

library("vegan")
library("ggplot2")
library("phyloseq")

suelo <- import_biom("taxonomy.biom")
suelo
head(otu_table(suelo))
otu_table(suelo)

colnames(tax_table(suelo)) <- c("Kingdom", "Phylum", "Class", "Order", "Family","Genera", "Species")


help(decostand,vegan)
suelo_bin <- decostand(otu_table(suelo), "pa")
suelo_bin

otu_table(sample1)



##Transferir y verificar datos
##Para eliminar contaminaciones eliminar reads del control negativo

##Todas las muestras deben de tener el mismo número de reads, usar frecuencias relativas
no_reads <- sort(sample_sums(suelo))
no_reads


p <- plot_bar(suelo, "Treatment", fill= "Phylum") + 
  geom_bar(aes(color =Phylum, fill = Phylum), stat ="identity", position = "stack") +
  facet_wrap("Host")

p


q <- plot_bar(suelo_bin, "Sample", fill ="OTU")   #geom_bar(aes(color = Sample, fill = OTU), stat ="identity") #+ facet_wrap("OTU")

q


##Análisis de diversidad

diversity <- estimate_richness (suelo, measures=c("Observed", "Fisher"))

diversity

my_data <- cbind(sample_data(suelo), diversity)  ##Unión de riqueza y de análisis estadístico
my_data



anova<- aov(Observed ~Treatment*Host, data = my_data)
summary(anova)
boxplot(Observed ~ Host, data = my_data)


diversity_bin <- estimate_richness (suelo_bin, measures=c("Observed", "Fisher"))
diversity_bin

my_data_bin <- cbind(sample_data(suelo), diversity_bin)
my_data_bin

anova_bin <- aov(Observed ~Treatment*Host, data = my_data_bin)
summary(anova_bin)
boxplot(Observed ~ Host, data = mydata_bin)

##Ordinación para comparar Diversidad Beta
##Matriz de disimilitud raup NDMS
##Nivel de estrés menor a 0.15 

raup <- distance(suelo, method="raup")
raup

raup_bin <- distance(suelo_bin, method="raup")
raup_bin

NMDS <- ordinate(suelo, method= "NMDS", distance = "raup")
NMDS

r <- plot_ordination(suelo, NDMS, color= "Host") + geom_point(size =4) +
  facet_wrap("Treatment") +
  theme_light()

r

#Prueba de adonis para comparar varianzas entre comunidades

adonis <- adonis (raup ~ Host*Treatment, data =my_data)
adonis

adonis_bin <- adonis(raup_bin ~ Host*Treatment, data = my_data_bin)
adonis_bin
