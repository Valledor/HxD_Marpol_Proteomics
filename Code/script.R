######################PHYSIOLOGY PLOTS####################################

##Load libraries

library (readxl)
library (ggplot2)
library(viridis) 
library(dplyr)
library(plyr)
library(ggthemes)

##Load data (Three columns:  Accession - Treatment - Numeric value)

##Fv/Fm (fvfm)
fvfmforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Free aminoacids (faa)
faaforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Total phenolic compounds (tpc)
tpcforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Malondialdehyde (mda)
mdaforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Chlorophyll a (chla)
chlaforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Chlorophyll b (chlb)
chlbforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Carotenoids (carot)
carotforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")

##Graphic generation

##Free aminoacids boxplot (faa)
faa_boxplot <- ggplot(faaforplot, aes(x =Treatment, y=faa, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "FAA (μg/mg DW)", x = "Sexual accession")
faa_boxplot
##Total phenolic compounds boxplot (tpc)
tpc_boxplot <- ggplot(tpcforplot, aes(x =Treatment, y=tpc, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "TPC (μgmg DW)", x = "Sexual accession")
tpc_boxplot
##Malondialdehyde boxplot (mda)
mda_boxplot <- ggplot(mdaforplot, aes(x =Treatment, y=mda, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "MDA (ng/mg DW)", x = "Sexual accession")
mda_boxplot
##Chlorophyll a boxplot (chla)
chla_boxplot <- ggplot(chlaforplot, aes(x =Treatment, y=chla, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Chlorophyll a (μg/mg DW)", x = "Sexual accession")
chla_boxplot
##Chlorophyll b boxplot (chlb)
chlb_boxplot <- ggplot(chlbforplot, aes(x =Treatment, y=chlb, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Chlorophyll b (μgmg DW)", x = "Sexual accession")
chlb_boxplot
##Carotenoids boxplot (carot)
carot_boxplot <- ggplot(carotforplot, aes(x =Treatment, y=carot, fill =Accession)) + geom_boxplot() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.75)) + theme_bw() + theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs("Carotenoids (μgmg DW)", x = "Sexual accession")
carot_boxplot
##Fv/Fm barplot (fvfm)
fvfm_info <- ddply(fvfmforplot, c("Treatment", "Accession"), summarise,
                    N = length(fvfm),
                    mean = mean(fvfm),
                    sd = sd(fvfm),
                    se = sd/sqrt(N))
fvfm_barplot <- ggplot(data=fvfm_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
fvfm_barplot

##Statistics

##Load libraries

library(agricolae)
library(mblm)
library(nlme)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(scales)
library (readxl)

##Load data (Two columns:  Accession and Treatment - Numeric value)

##Fv/Fm (fvfm)
fvfmforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Free aminoacids (faa)
faaforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Total phenolic compounds (tpc)
tpcforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Malondialdehyde (mda)
mdaforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Chlorophyll a (chla)
chlaforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Chlorophyll b (chlb)
chlbforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##Carotenoids (carot)
carotforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")

##Statistics performing

##Fv/Fm (fvfm)
fvfm_anovamodel= aov(fvfm ~ as.factor(Treatment), data = fvfmforstatistics)
summary(fvfm_anovamodel)
fvfm_tukey <- TukeyHSD(fvfm_anovamodel, ordered = TRUE)
fvfm_tukey
fvfm_letters <-HSD.test(fvfm_anovamodel, trt="as.factor(Treatment)")
fvfm_letters
##Free aminoacids (faa)
faa_anovamodel= aov(faa ~ as.factor(Treatment), data = faaforstatistics)
summary(faa_anovamodel)
faa_tukey <- TukeyHSD(faa_anovamodel, ordered = TRUE)
faa_tukey
faa_letters <-HSD.test(faa_anovamodel, trt="as.factor(Treatment)")
faa_letters
##Total phenolic compounds (tpc)
tpc_anovamodel= aov(tpc ~ as.factor(Treatment), data = tpcforstatistics)
summary(tpc_anovamodel)
tpc_tukey <- TukeyHSD(tpc_anovamodel, ordered = TRUE)
tpc_tukey
tpc_letters <-HSD.test(tpc_anovamodel, trt="as.factor(Treatment)")
tpc_letters
##Malondialdhyde (mda)
mda_anovamodel= aov(mda ~ as.factor(Treatment), data = mdaforstatistics)
summary(mda_anovamodel)
mda_tukey <- TukeyHSD(mda_anovamodel, ordered = TRUE)
mda_tukey
mda_letters <-HSD.test(mda_anovamodel, trt="as.factor(Treatment)")
mda_letters
##Chlorophyll a (chla)
chla_anovamodel= aov(chla ~ as.factor(Treatment), data = chlaforstatistics)
summary(chla_anovamodel)
chla_tukey <- TukeyHSD(chla_anovamodel, ordered = TRUE)
chla_tukey
chla_letters <-HSD.test(chla_anovamodel, trt="as.factor(Treatment)")
chla_letters
##Chlorophyll b (chlb)
kruskal_result_chlb <-kruskal(chlbforstatistics$chlb, chlbforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_chlb
##Carotenoids (carot)
kruskal_result_carotenoids <-kruskal(carotforstatistics$carot, carotforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_carotenoids

####################DESCRIPTIVE PROTEOMICS##############################

##Load library (available guide through all analysis on this package by wizard)

library(xlsx)
library(pRocessomics)
library(pheatmap)
library(dendextend)

##Data import

importfromexcel()

##Data preprocessing------------------------------------------------------------

preprocesstabla <- pRocessomics::preprocess_omic_list(datalist = proteomic_tabla,
                                                      initialrow = 1, 
                                                      initialcolumn = 3,
                                                      treatment1col = 1, 
                                                      treatment2col = 2, 
                                                      treatment = 3,
                                                      imputation = "RF",
                                                      imputhld = 0.2,
                                                      parallel =  T,
                                                      varsel = T, 
                                                      varselthld = 0,45,
                                                      abdbal = "AvgIntensity")
View(preprocesstabla$proteomics)

##Mapman functional assignment (from Mercator4)---------------------------

importannotation()

View(preprocesstabla_annot)

##Accession data separation----------------------------------------------

#Tak-1: male
preprocessmale <- which(preprocesstabla$proteomics[,1] == "male")
malepreprocesstabla <- list("proteomics" = preprocesstabla$proteomics[preprocessmale,])
class(malepreprocesstabla) <- "POL"
#Tak-2: female
preprocessfemale <- which(preprocesstabla$proteomics[,1] == "female")
femalepreprocesstabla <- list("proteomics" = preprocesstabla$proteomics[preprocessfemale,])
class(femalepreprocesstabla) <- "POL"

##Venn diagrams---------------------------------------------------------------

#Tak-2: female
ven_preprocess_female <- Venn_analysis(datalist = femalepreprocesstabla, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, omiclevel = "proteomics", threshold = 4)
Venn_plot(datalist = ven_preprocess_female)
write.xlsx(ven_preprocess_male$treatments, file = "ven_male.xlsx", sheetName = "1", append = FALSE)
write.xlsx(ven_preprocess_male$data, file = "ven_male.xlsx", sheetName="2", append=TRUE)
#Tak-1: male
ven_preprocess_male <- Venn_analysis(datalist = malepreprocesstabla, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, omiclevel = "proteomics", threshold = 4)
Venn_plot(datalist = ven_preprocess_male)
write.xlsx(ven_preprocess_female$treatments, file = "ven_female.xlsx", sheetName = "1", append = FALSE)
write.xlsx(ven_preprocess_female$data, file = "ven_female.xlsx", sheetName="2", append=TRUE)

##Univariate differential analysis-------------------------------------------------

#Tak-1: male
univariate_preprocesstabla_male <- univariate(datalist = malepreprocesstabla, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, parametric = FALSE, posthoc = TRUE, FDR = TRUE, round=6, annotatefile = NULL)
export_table(univariate_preprocesstabla_male, "univariate_analysis_male.xlsx")
#Tak-2: female
univariate_preprocesstabla_female <- univariate(datalist = femalepreprocesstabla, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, parametric = FALSE, posthoc = TRUE, FDR = TRUE, round=6, annotatefile = NULL)
export_table(univariate_preprocesstabla_female, "univariate_analysis_female.xlsx")

##Heatmaps-----------------------------------------------------------------

#Tak-1: male
male_MMOlist <- mapman_group(datalist = malepreprocesstabla, annotation = preprocesstabla_annot, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, threshold = 2, group = TRUE, omiclevel = "proteomics")
male_values <- male_MMOlist$mean
rownames(male_MMOlist$mean) <- paste(rownames(male_MMOlist$mean),"(",male_MMOlist$number_bin_elements,")")
male_sd <- male_MMOlist$sd
male_treatments <- factor(male_MMOlist$treatments, levels = unique(male_MMOlist$treatments))
male_values <- sweep(male_MMOlist$mean, 1, rowSums(male_MMOlist$mean), FUN = "/")
male_hmparameters = c("manhattan","manhattan","ward.D",TRUE,"row")
#Tak-2: female
female_MMOlist  <- mapman_group(datalist = femalepreprocesstabla, annotation = preprocesstabla_annot, initialrow = 1, initialcolumn = 3, treatment1col = 1, treatment2col = 2, treatment = 2, threshold = 2, group = TRUE, omiclevel = "proteomics")
rownames(female_MMOlist$mean) <- paste(rownames(female_MMOlist$mean),"(",female_MMOlist$number_bin_elements,")")
female_values <- female_MMOlist$mean
female_sd <- female_MMOlist$sd
female_treatments <- factor(female_MMOlist$treatments, levels = unique(female_MMOlist$treatments))
female_values <- sweep(female_MMOlist$mean, 1, rowSums(female_MMOlist$mean), FUN = "/")
female_hmparameters = c("manhattan","manhattan","ward.D",TRUE,"row")
female_values_ordered <- female_values
female_values_ordered<-data.matrix(female_values_ordered)
set.seed(1512)
distance.row_female_ordered = dist(female_values_ordered, method = "manhattan")
cluster.row_female_ordered = hclust(distance.row_female_ordered, method = "ward.D")
distance.col_female_ordered = dist(t(female_values_ordered), method = "manhattan")
cluster.col_female_ordered = hclust(distance.col_female_ordered, method = "ward.D")
plot(cluster.col_female_ordered,main="base dendrogram")
cluster.col_female_ordered<-rotate(cluster.col_female_ordered,c(2,3,1))
plot(cluster.col_female_ordered,main="reordered dendrogram")

Breaks <- seq(0.3, 0.37, length.out=100)
colors2<- colorRampPalette(c("#B8E68A", "#ACE6A1", "#69BF8D", "#3D998A", "#094C59"))(n = 100)

pheatmap::pheatmap(male_values, breaks = Breaks, clustering_distance_rows = male_hmparameters[1], clustering_distance_cols = male_hmparameters[2], clustering_method = male_hmparameters[3], display_numbers = male_hmparameters[4], angle_col = "0",  color  =  colors2)
pheatmap::pheatmap(female_values, breaks = Breaks, clustering_distance_rows = female_hmparameters[1], clustering_distance_cols = female_hmparameters[2],  clustering_method = female_hmparameters[3], display_numbers = female_hmparameters[4], angle_col = "0",   color  = colors2, cluster_cols = cluster.col_female_ordered)

##Kmeans clustering--------------------------------------------------------

fem <- which(preprocesstabla$proteomics[,1] == "female")
mal <- which(preprocesstabla$proteomics[,1] == "male")

female <- preprocesstabla$proteomics[fem,]
male <- preprocesstabla$proteomics[mal,]

marc_HD <- list("female"=female,"male"=male)
class(marc_HD) <- "POL"

#Tak-1: male
firstmale_kmeans <- kmeans_analysis(datalist = marc_HD,annotation = NULL,initialrow = 1,initialcolumn = 3,treatment1col = 1,treatment2col = 2,treatment = 3,omiclevel = "male",scalation = 3,clusters = c(1:5),show.elbow.plot = F)
malemeans <- firstmale_kmeans$kmeans_matrix
View(malemeans)
rownames(malemeans)<-c("male&T0","male&T1","male&T2")

#Tak-2: female
firstfemale_kmeans <- kmeans_analysis(datalist = marc_HD,annotation = NULL,initialrow = 1,initialcolumn = 3,treatment1col = 1,treatment2col = 2,treatment = 3,omiclevel = "female",scalation = 3,clusters = c(1:5),show.elbow.plot = F)
femalemeans <- firstfemale_kmeans$kmeans_matrix 
View(femalemeans)

notfoundinmale <- setdiff(colnames(femalemeans),colnames(malemeans)) 
notfoundinmale
notfoundinfemale <- setdiff(colnames(malemeans),colnames(femalemeans)) 
notfoundinfemale

which(notfoundinmale %in% colnames(femalemeans))
which(notfoundinmale %in% colnames(malemeans))
which(notfoundinfemale %in% colnames(femalemeans))
which(notfoundinfemale %in% colnames(malemeans))

m_desap <- rep(c(0),each=4) 
mzeros <- rbind(m_desap,m_desap,m_desap)
f_desap <- rep(c(0),each=3)
fzeros <- rbind(f_desap,f_desap,f_desap)
colnames(fzeros) <- notfoundinmale
colnames(mzeros) <- notfoundinfemale

rownames(fzeros) <- rownames(malemeans)
rownames(mzeros) <- rownames(femalemeans)

femalemeans <- cbind(femalemeans,mzeros)
malemeans <- cbind(malemeans,fzeros)

length(unique(colnames(femalemeans)))
length(colnames(femalemeans))

femalemeans_ord <- femalemeans[,order(colnames(femalemeans))] 
malemeans_ord <- malemeans[,order(colnames(malemeans))] 

length(which(colnames(malemeans_ord) != colnames(femalemeans_ord))) 

kmedias <- rbind(femalemeans_ord,malemeans_ord) 

Treatment=c("female&T0","female&T1","female&T2","male&T0","male&T1","male&T2")
kmedias2 <- as.data.frame(
  cbind(
    as.data.frame(Treatment),
    as.data.frame(kmedias)))
View(kmedias2)

forkmeans <- list("gender"=kmedias2) 
class(forkmeans) <- "POL"

final_kmeans <- kmeans_analysis(datalist = forkmeans,annotation = NULL,initialrow = 1,initialcolumn = 2,treatment1col = 1,treatment2col = 1,treatment = 1,omiclevel = "gender",scalation = 2,clusters = c(10,25),show.elbow.plot = T) 
kmeans_plot(final_kmeans)

final_kmeans <- kmeans_analysis(datalist = forkmeans,
                                annotation = NULL,initialrow = 1,
                                initialcolumn = 2,treatment1col = 1,
                                treatment2col = 1,treatment = 1,
                                omiclevel = "gender",scalation = 2,
                                clusters = c(15,15),show.elbow.plot = T) 

export_table(final_kmeans,"kmeans_15.xlsx")

kmeansobject <- final_kmeans 

clusternumber <- 15 

cluster <-
  which(as.numeric(gsub(
    " Clusters", "", names(kmeansobject$kmeans_list), fixed = TRUE
  )) == clusternumber) 
datosggplot <-
  kmeansobject$kmeans_list[[cluster]] 
datosggplot <-
  reshape2::melt(datosggplot, (ncol(datosggplot) - 1):(ncol(datosggplot))) 
datosggplot <- datosggplot[order(datosggplot$groups), ]
datosggplot2 <- datosggplot
datosggplot2[, 1] <- paste("Cluster", datosggplot2[, 1])
a <-
  factor(datosggplot2[, 1], levels = c(paste(
    "Cluster", unique(datosggplot[, 1]), sep = " "
  )))
View(datosggplot2)

library(tidyverse)
datosggplot3 <- tidyr::separate(data=datosggplot2,col=3,into=c("Gender","Treatment"),sep="&")
sosorted <- unique(datosggplot3$groups) 
df_tidy_mean <- datosggplot3 %>%
  group_by(groups, Gender, Treatment) %>%
  summarise(n = n(),
            mean = mean(value),
            median = median(value),
            sd = sd(value),
            maxim= max(value),
            minim= min(value)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)

dff2 <- as.data.frame(df_tidy_mean)
dff <- dff2[order(unlist(sapply(dff2$groups, function(x) which(sosorted==x)))),]
dff$groups <- paste(dff$groups," (",dff$n,")", sep="")
colorpop <- c("#6a329f","#fdc50b")
colorpop80 <- c("#6a329f80","#fdc50b80")

myNEWplot <-
  ggplot2::ggplot(
    dff,
    ggplot2::aes(
      x = Treatment,
      y = mean,
      group = Gender,
      color = Gender),
    text = Gender
  ) +
  
  ggplot2::scale_fill_manual(values=colorpop80) +
  ggplot2::scale_color_manual(values=colorpop) +
  ggplot2::facet_wrap(facets = factor(dff$groups, levels= unique(dff$groups)) , scales = "free") +
  ggplot2::geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Gender), colour=NA) +
  #ggplot2::geom_ribbon(aes(ymin=minim,ymax=maxim,fill=Gender), colour=NA) +
  ggplot2::geom_line(aes(x=Treatment, y= dff$mean, color=Gender)) +
  
  ggplot2::theme_minimal() + ggplot2::theme(
    legend.position = "top",
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 0,
      vjust = 1,
      hjust = 1
    )
  )

myNEWplot

gg <- plotly::ggplotly(myNEWplot, tooltip = c("text"))
fontsizes <- c(12,14,16,14)
gg <-
  plotly::layout(
    gg,
    title = list(text="Marchantia proteins K-means clustering", y=1.5, x=0.05),
    titlefont = list(size = fontsizes[3]),
    showlegend = F,
    legend = list(orientation = "h", y=1.1, x = 0.6)
    #yanchor ="top")
    #legend = list(font = list(size = fontsizes[4]))
  )

gg


##Kmeans functional enrichments---------------------------------------------

#Mapman functional assignment (from Mercator4)
annotfilename <- file.choose()

fileextension <- strsplit(annotfilename, "[.]")[[1]]
fileextension <- fileextension[[length(fileextension)]]
fileextension <- toupper(fileextension)

annottable <-
  utils::read.delim(annotfilename, quote = '') 
annottable <- apply(annottable, 2, function(x)
  gsub("'", "", x))
annottable <- apply(annottable, 2, function(x)
  gsub("\"", "", x))
annottable <- annottable[!is.na(annottable[,5]),]
annottable <- annottable[,1:3]


bins <- strsplit(annottable[,1],"[.]")
binanmes <-strsplit(annottable[,2],"[.]")

n<-3

bindots <- sapply(bins, function(x) paste(x[1:n], collapse = "."))
binnamesdots <- sapply(binanmes, function(x) paste(x[1:n], collapse = "."))

annottable[,1] <- paste("MM",bindots, sep = ":")
annottable[,2] <- binnamesdots

terminos_mapman<-unique(annottable[,1])
aux <- unique(annottable[,1:2])
annota <- annottable[,c(1,3)]
lista<-sapply(terminos_mapman, function(x) annota[annota[,1]==x,2])
tabla_lista <-sapply(lista, '[', seq(max(sapply(lista, length))))
tabla_lista<-tidyr::replace_na(tabla_lista,"")
almost.there <- as.data.frame(t(tabla_lista))
there <- cbind("MM"=rownames(almost.there),almost.there)
forgmt <- merge(there, aux, by.x = "MM", by.y = "BINCODE", all.x = T )
lastgmt <- forgmt[,c(1,ncol(forgmt),2:(ncol(forgmt)-1))]

#gmt table for functional enrichments
write.table(lastgmt,"prueba3_gmt_enriquecimientofuncional.gmt",row.names=F,col.names=FALSE, sep="\t", quote = FALSE) 

library(gprofiler2)

gprofiler2::upload_GMT_file(gmtfile = "prueba3_gmt_enriquecimientofuncional.gmt")
a<-gprofiler2::upload_GMT_file(gmtfile = "prueba3_gmt_enriquecimientofuncional.gmt")

#Your custom annotations ID is gp__et6G_pXDo_vW4
#You can use this ID as an 'organism' name in all the related enrichment tests against this custom source.
#Just use: gost(my_genes, organism = 'gp__et6G_pXDo_vW4')

#Selection of cluster and ID columns from kmeans table 
kme <- read.delim("clipboard", header=T, stringsAsFactors = F)
kme[,2] <- tolower(kme[,2])


usthre <- 0.05
cl_table <- list()
for (i in 1:max(kme[,1])){ 
  cluster1 <- kme[kme[,1]==i,2]
  auxiliar <- paste("Cluster",i, sep=" ")
  cl1_gostres <- gprofiler2::gost(query = cluster1, organism = 'gp__8t4w_Spl2_FkM', user_threshold = usthre, significant = F)
  imp7 <- cl1_gostres$result[,c("term_name","p_value","term_size")]
  imp7 <- cbind(imp7,"log10p"= -1*log(imp7$p_value), "group" = rep(auxiliar, nrow(imp7)))
  cl_table[[i]] <- imp7
}
names(cl_table)<- paste("Cluster", 1:i)

capture.output(cl_table$`Cluster 1`, file = "prueba3_enriquecimiento_funcional.txt")
View(auxiliar)

colorpal= c("#7b86ad","#f9af30","#ffe8bd",
                     "#48b4bd","#710000","#f9a20c",
                     "#005005","#ffc657","#a4ff4d",
                     "#80ff80","#c9953c","#62f4ff",
                     "#de621e","#f85a04","#fff457",
                     "#ffb9c2","#886425","#ffeb06",
                     "#908aff","#3d251a","#271811",
                     "#4b4885","#5c3827","#0021da")
                     
enrichment <- do.call(rbind, cl_table)
enrichment <- enrichment[enrichment[,2]<= usthre,]
enrichment <- enrichment[rev(rownames(enrichment)),]
dim(enrichment)

prefuncional <- enrichment
prefuncional[,1] <- paste(LETTERS[1:12],prefuncional[,1], sep=" ")
prefuncional[,1] <- factor(prefuncional[,1], levels = unique(prefuncional[,1]))
asdf <- ggplot(prefuncional, aes(log10p, term_name, fill = term_name)) +
  geom_col(alpha = .6, width = .5) +
  scale_fill_manual(values=colorpal) +
  scale_y_discrete(labels = prefuncional$group) +
  geom_text(aes(label = term_name),
            x = 0.1,
            hjust = 0, 
            vjust = -1.3,
            size = 3) +
  geom_text(aes(label = term_size),
            x = prefuncional$log10p + 0.3,
            hjust = 0, 
            size = 3) +
  geom_vline(xintercept = 1.3, size = 0.1, color="red")+
  labs(
    title = "Functional enrichment",
    
    x = "-log10(pvalue)",
    y = ""
  ) + 
  
  theme_minimal()+
  theme(
    axis.line.x = element_line(size=0.3, colour ="black"),
    plot.title = element_text(size = 15, hjust =1),
    legend.position = "none")
View(asdf$data)


##########################MOFA2##########################################

####Source for MOFA analyses: https://biofam.github.io/MOFA2/index.html

##Set directory

setwd("D:/DSMarchantia_mofa2/")

## Load libraries

library(MOFA2)
library(MOFAdata)
library(data.table)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(purrr)
library(cowplot)

## Prepare inputs: preprocessed protein table

AllProteins <- read.delim("clipboard", header = T, row.names = 1)
AllProteins <- t(AllProteins)
imputedmaleT0 <- read.delim("clipboard", header = T)
AllProteins[,6] <- NA
for(j in 1:nrow(AllProteins)){
  if(length(which(colnames(imputedmaleT0) == rownames(AllProteins)[j])) > 0){
    AllProteins[j,6] <- imputedmaleT0[1,which(colnames(imputedmaleT0) == rownames(AllProteins)[j])]  
  } else if(length(which(colnames(imputedmaleT0) == rownames(AllProteins)[j])) == 0){
    AllProteins[j,6] <- 0
  }
  
}

##Select the top 2000 loadings with higher variance

AllProteins.vars <- apply(AllProteins, 1, var)
AllProteinsHVF2000 <- AllProteins[names(sort(AllProteins.vars, decreasing = TRUE)[1:2000]),] 

##Check number of factors and sparsity + input data transformation | filter low variance prots. Prepare data for global and grouped MOFA

reticulate::use_python("C:\\Users\\bboyl\\AppData\\Local\\Programs\\Python\\Python38")
AllProteinsList_T1 <- list(Proteins = (log10(AllProteinsHVF2000[,11:20]+10)))
AllProteinsList <- list(Proteins = (log10(AllProteinsHVF2000+10)))
groups <- do.call(rbind,strsplit(x = colnames(AllProteins), split = "_", fixed = T))[,2]
groups_2 <- do.call(rbind,strsplit(x = colnames(AllProteins), split = "_", fixed = T))[,3]

MOFAglobal <- create_mofa(data = AllProteinsList, save_metadata = TRUE)
MOFAglobal_T1 <- create_mofa(data = AllProteinsList_T1, save_metadata = TRUE)
MOFAgrouped <- create_mofa(data = AllProteinsList, groups = groups, save_metadata = TRUE)
MOFAgrouped_2 <- create_mofa(data = AllProteinsList, groups = groups_2, save_metadata = TRUE)

data_opts <- get_default_data_options(MOFAgrouped)
#data_opts$scale_views <- TRUE
model_opts <- get_default_model_options(MOFAgrouped)
model_opts$num_factors <- 3
train_opts <- get_default_training_options(MOFAgrouped)
train_opts$maxiter <- "100000"
train_opts$convergence_mode <- "slow"
train_opts$seed <- 404

outfileGrouped <- "D:/DSMarchantia_mofa2/outputs/MOFA2grouped_log101byme_F8maleT0imputed_HVF2000.hdf5"
MOFAgrouped <- prepare_mofa(MOFAgrouped, data_options = data_opts, model_options = model_opts, training_options = train_opts)

data_opts <- get_default_data_options(MOFAglobal)
#data_opts$scale_views <- TRUE
model_opts <- get_default_model_options(MOFAglobal)
model_opts$num_factors <- 7
train_opts <- get_default_training_options(MOFAglobal)
train_opts$maxiter <- "100000"
train_opts$convergence_mode <- "slow"
train_opts$seed <- 404

outfileGlobal <- "D:/DSMarchantia_mofa2/outputs/MOFA2global_log101byme_F8T0imputed_HVF2000.hdf5"
MOFAglobal <- prepare_mofa(MOFAglobal, data_options = data_opts, model_options = model_opts, training_options = train_opts)

data_opts <- get_default_data_options(MOFAgrouped_2)
#data_opts$scale_views <- TRUE
model_opts <- get_default_model_options(MOFAgrouped_2)
model_opts$num_factors <- 3
train_opts <- get_default_training_options(MOFAgrouped_2)
train_opts$maxiter <- "100000"
train_opts$convergence_mode <- "slow"
train_opts$seed <- 404

outfileGrouped_2 <- "D:/DSMarchantia_mofa2/outputs/MOFA2grouped_2_log101byme_F8maleT0imputed_HVF2000.hdf5"
MOFAgrouped_2 <- prepare_mofa(MOFAgrouped_2, data_options = data_opts, model_options = model_opts, training_options = train_opts)


data_opts <- get_default_data_options(MOFAglobal_T1)
#data_opts$scale_views <- TRUE
model_opts <- get_default_model_options(MOFAglobal_T1)
model_opts$num_factors <- 3
train_opts <- get_default_training_options(MOFAglobal_T1)
train_opts$maxiter <- "100000"
train_opts$convergence_mode <- "slow"
train_opts$seed <- 404

outfileGlobal_T1 <- "D:/DSMarchantia_mofa2/outputs/MOFA2global_T1_log101byme_F8maleT0imputed_HVF2000.hdf5"
MOFAglobal_T1 <- prepare_mofa(MOFAglobal_T1, data_options = data_opts, model_options = model_opts, training_options = train_opts)

sample_metadata <- data.frame(
  sample = colnames(AllProteinsList$Proteins),
  stress = c(rep("C",10), rep("DS",20)),
  treatment = c(rep("T0",10), rep("T1",10), rep("T2",10)),
  group = c(rep("female",5), rep("male",5),rep("female",5), rep("male",5),rep("female",5), rep("male",5)),
  sample_number = c(1:30)
)

samples_metadata(MOFAgrouped) <- sample_metadata
plot_data_overview(MOFAgrouped)

sample_metadata <- data.frame(
  sample = colnames(AllProteinsList$Proteins),
  stress = c(rep("C",10), rep("DS",20)),
  treatment = c(rep("T0",10), rep("T1",10), rep("T2",10)),
  sex = c(rep("female",5), rep("male",5),rep("female",5), rep("male",5),rep("female",5), rep("male",5)),
  sample_number = c(1:30)
)

samples_metadata(MOFAglobal) <- sample_metadata
plot_data_overview(MOFAglobal)

sample_metadata <- data.frame(
  sample = colnames(AllProteinsList$Proteins),
  stress = c(rep("C",10), rep("DS",20)),
  group = c(rep("T0",10), rep("T1",10), rep("T2",10)),
  sex = c(rep("female",5), rep("male",5),rep("female",5), rep("male",5),rep("female",5), rep("male",5)),
  sample_number = c(1:30)
)

samples_metadata(MOFAgrouped_2) <- sample_metadata
plot_data_overview(MOFAgrouped_2)

sample_metadata <- data.frame(
  sample = colnames(AllProteinsList_T1$Proteins),
  sex = c(rep("female",5), rep("male",5)),
  sample_number = c(1:10)
)

samples_metadata(MOFAglobal_T1) <- sample_metadata
plot_data_overview(MOFAglobal_T1)

MOFAgrouped.trained <- run_mofa(MOFAgrouped, outfile = outfileGrouped)
MOFAglobal.trained <- run_mofa(MOFAglobal, outfile = outfileGlobal)
MOFAgrouped_2.trained <- run_mofa(MOFAgrouped_2, outfile = outfileGrouped_2)
MOFAglobal_T1.trained <- run_mofa(MOFAglobal_T1, outfile = outfileGlobal_T1)

##Downstream analysis of both models

MOFAf8_bad_grouped <- load_model("D:/DSMarchantia_mofa2/outputs/MOFA2grouped_log1010byme.hdf5")
MOFAf8_bad_global <- load_model("D:/DSMarchantia_mofa2/outputs/MOFA2global_log1010byme.hdf5")

##Diagnosis plot: corr between LFs

plot_factor_cor(MOFAgrouped.trained)
plot_factor_cor(MOFAglobal.trained)
plot_factor_cor(MOFAgrouped_2.trained)
plot_factor_cor(MOFAglobal_T1.trained)

##Disentangling the heterogeneity. Calculation of variance explained by each factor in each view

plot_variance_explained(MOFAgrouped.trained, x = "group", y = "factor")
plot_variance_explained(MOFAgrouped.trained, x = "group", y = "factor", plot_total = T)

plot_variance_explained(MOFAglobal.trained, x = "group", y = "factor")
plot_variance_explained(MOFAglobal.trained, x = "group", y = "factor", plot_total = T)

plot_variance_explained(MOFAgrouped_2.trained, x = "group", y = "factor")
plot_variance_explained(MOFAgrouped_2.trained, x = "group", y = "factor", plot_total = T)

plot_variance_explained(MOFAglobal_T1.trained, x = "group", y = "factor")
plot_variance_explained(MOFAglobal_T1.trained, x = "group", y = "factor", plot_total = T)

MOFAgrouped.trained@cache$variance_explained
MOFAgrouped.trained@cache$variance_explained$r2_per_factor$female
MOFAgrouped.trained@cache$variance_explained$r2_per_factor$male

a <- plot_variance_explained(MOFAgrouped.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAgrouped.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)

a <- plot_variance_explained(MOFAglobal.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAglobal.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)

a <- plot_variance_explained(MOFAgrouped_2.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAgrouped_2.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)

MOFAgrouped_2.trained@cache$variance_explained
MOFAgrouped_2.trained@cache$variance_explained$r2_per_factor$female
MOFAgrouped_2.trained@cache$variance_explained$r2_per_factor$male

##Biological decoding of LFs

##Single LF

plot_factor(MOFAgrouped.trained, 
            factor = 3,
            color_by = "treatment",
            shape_by = "stress", dot_size = 3
)

plot_factor(MOFAgrouped_2.trained, 
            factor = 2,
            color_by = "sex",
            shape_by = "stress", dot_size = 3
)

plot_factor(MOFAglobal.trained, 
            factor = 1:7,
            color_by = "treatment",
            shape_by = "sex", dot_size = 3
)

plot_factor(MOFAglobal_T1.trained, 
            factor = 1:3,
            color_by = "sex",
            shape_by = "sex", dot_size = 3
)

##Multiple LFs

plot_factors(MOFAgrouped.trained, 
             factors = c(1,2,3),
             color_by = "treatment", dot_size = 3, shape_by = "group"
)

plot_factors(MOFAgrouped_2.trained, 
             factors = c(1,2,3),
             color_by = "stress", dot_size = 3, shape_by = "sex"
)

plot_factors(MOFAglobal.trained, 
             factors = c(1,2,3),
             color_by = "treatment", dot_size = 3, shape_by = "sex"
)

##Weights of biologically relevant LFs

plot_weights(MOFAgrouped.trained,
             view = "Proteins",
             factor = 2,
             nfeatures = 10,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)

plot_weights(MOFAgrouped_2.trained,
             view = "Proteins",
             factor = 3,
             nfeatures = 10,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)

plot_weights(MOFAglobal.trained,
             view = "Proteins",
             factor = 1,
             nfeatures = 10,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)


sex_markers <- c("Mapoly0048s0002.1.p", "Mapoly008s0177.1.p", "Mapoly0004s0012.1.p")

p <- plot_factor(MOFAgrouped_2.trained, factors = 2, color_by =  "Mapoly0015s0198.1.p") + scale_colour_gradientn(colours = terrain.colors(10))

p <- plot_data_scatter(MOFAgrouped_2.trained, 
                       view = "Proteins", 
                       factor = 3, 
                       features = 16,         # Number of features to show
                       sign = "both",     # select top 6 features with positive weights
                       color_by = "sex",
                       shape_by = "stress", # color cells by lineage
                       add_lm = T,          # add linear regression estimates
                       lm_per_group = F, 
                       dot_size = 2
)

##Functional enrichments: biological functions in LFs of interest

bins <- read.delim("clipboard", header = T) #from 2.Heatmap
bin_db <- matrix(0, nrow = 31, ncol = length(bins$BINCODE.1))
bin_names <- read.delim("clipboard", header = T) #from 2.Heatmap
rownames(bin_db) <- bin_names$NAME.1
colnames(bin_db) <- bins$IDENTIFIER


for(i in colnames(bin_db)){
  bn <- as.numeric(bins$BINCODE.1[which(bins$IDENTIFIER == i)])
  bin_db[bn,i] <- 1
}

bin_db_2 <- bin_db[,rownames(MOFAgrouped_2.trained@data$Proteins$T0)]

enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = 1,
                                        feature.sets = bin_db_2,
                                        set.statistic = "rank.sum",
                                        sign = "negative",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)

enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = 1:2,
                                        feature.sets = bin_db,
                                        sign = "negative",
                                        statistical.test = "parametric"
)

enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = 1:2,
                                        feature.sets = bin_db,
                                        sign = "positive",
                                        statistical.test = "parametric"
)

enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = 3,
                                        feature.sets = bin_db,
                                        sign = "all",
                                        statistical.test = "parametric"
)

plot_enrichment_heatmap(enrichment.parametric)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 2
)

plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)

##Export section

pdf("D:/DSMarchantia_mofa2/outputs/DemostrationwhynotF8maleT0.pdf")
plot_variance_explained(MOFAf8_bad_global, x = "group", y = "factor", plot_total = T)
plot_factor(MOFAf8_bad_global, 
            factor = 1:5,
            color_by = "sex",
            shape_by = "stress", dot_size = 3
)
plot_factor(MOFAf8_bad_global, 
            factor = 1:5,
            color_by = "treatment",
            shape_by = "sex", dot_size = 3
)
plot_variance_explained(MOFAf8_bad_grouped, x = "group", y = "factor", plot_total = T)
plot_factor(MOFAf8_bad_grouped, 
            factor = 1:5,
            color_by = "group",
            shape_by = "stress", dot_size = 3
)
plot_factor(MOFAf8_bad_grouped, 
            factor = 1:5,
            color_by = "treatment",
            shape_by = "group", dot_size = 3
)
dev.off()

pdf("D:/DSMarchantia_mofa2/outputs/MOFAglobal_log1010_imputedF8T0_HVF2000.pdf")
plot_data_overview(MOFAglobal)
plot_factor_cor(MOFAglobal.trained)
a <- plot_variance_explained(MOFAglobal.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAglobal.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)
plot_factor(MOFAglobal.trained, 
            factor = 1:7,
            color_by = "sex",
            shape_by = "treatment", dot_size = 3
)
plot_factor(MOFAglobal.trained, 
            factor = 1:7,
            color_by = "treatment",
            shape_by = "sex", dot_size = 3
)
# LF 1 == sex, LF2 tries to T2 and the rest, LF3 all points only for males
plot_factors(MOFAglobal.trained, 
             factors = c(1,2,3,4),
             color_by = "treatment", dot_size = 3, shape_by = "sex"
)
plot_weights(MOFAglobal.trained,
             view = "Proteins",
             factor = 1,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAglobal.trained,
             view = "Proteins",
             factor = 2,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAglobal.trained,
             view = "Proteins",
             factor = 3,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_top_weights(MOFAglobal.trained,
                 view = "Proteins",
                 factor = 1,
                 nfeatures = 20
)
plot_top_weights(MOFAglobal.trained,
                 view = "Proteins",
                 factor = 2,
                 nfeatures = 20
)
plot_top_weights(MOFAglobal.trained,
                 view = "Proteins",
                 factor = 3,
                 nfeatures = 20
)
rownames(sample_metadata) <- sample_metadata$sample
plot_data_heatmap(MOFAglobal.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 30,          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAglobal.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 30,          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAglobal.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 30,          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAglobal.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
enrichment.parametric <- run_enrichment(MOFAglobal.trained,
                                        view = "Proteins", factors = c(1,2,3),
                                        feature.sets = bin_db,
                                        sign = "negative",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)
write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/global_imputed_HVF2000/enrichment_negative_LF123.txt", sep = "\t", col.names = T, row.names = T)
enrichment.parametric <- run_enrichment(MOFAglobal.trained,
                                        view = "Proteins", factors = c(1,2,3),
                                        feature.sets = bin_db,
                                        sign = "positive",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)
write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/global_imputed_HVF2000/enrichment_positive_LF123.txt", sep = "\t", col.names = T, row.names = T)

genes <- list("Q03033","Mapoly0001s0424.1.p")
plot_factors(MOFAglobal.trained, 
             factors = c(1,3), 
             color_by = "treatment",
             shape_by = "sex",
             scale = T,
             legend = T, dot_size = 3
)
genes %>% map(~ plot_factors(MOFAglobal.trained, 
                             factors = c(1,3), 
                             color_by = .,
                             shape_by = "sex",
                             scale = T,
                             legend = T, dot_size = 3
)) %>% cowplot::plot_grid(plotlist=., nrow=1)
plot_factors(MOFAglobal.trained, 
             factors = c(1,2), 
             color_by = "treatment",
             shape_by = "sex",
             scale = T,
             legend = T, dot_size = 3
)
genes %>% map(~ plot_factors(MOFAglobal.trained, 
                             factors = c(1,2), 
                             color_by = .,
                             shape_by = "sex",
                             scale = T,
                             legend = T, dot_size = 3
)) %>% cowplot::plot_grid(plotlist=., nrow=1)
factors <- get_factors(object = MOFAglobal.trained, as.data.frame = T)
weights <- get_weights(object = MOFAglobal.trained, as.data.frame = T)
variance <- MOFA2::get_variance_explained(object = MOFAglobal.trained, as.data.frame = T)
write.table(factors, file = "D:/DSMarchantia_mofa2/outputs/global_imputed_HVF2000/FACTORS_MOFA2global_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(weights, file = "D:/DSMarchantia_mofa2/outputs/global_imputed_HVF2000/WEIGHTS_MOFA2global_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(variance$r2_per_factor, file = "D:/DSMarchantia_mofa2/outputs/global_imputed_HVF2000/VARIANCE_MOFA2global_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
dev.off()

pdf("D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000/MOFAgrouped_sex_log1010_imputedF8T0_HVF2000.pdf")

plot_data_overview(MOFAgrouped.trained)
plot_factor_cor(MOFAgrouped.trained)
a <- plot_variance_explained(MOFAgrouped.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAgrouped.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)
plot_factor(MOFAgrouped.trained, 
            factor = 1:3,
            color_by = "group",
            shape_by = "treatment", dot_size = 3
)
plot_factor(MOFAgrouped.trained, 
            factor = 1:3,
            color_by = "treatment",
            shape_by = "group", dot_size = 3
)

plot_factors(MOFAgrouped.trained, 
             factors = c(1,2,3),
             color_by = "treatment", dot_size = 3, shape_by = "group"
)
plot_weights(MOFAgrouped.trained,
             view = "Proteins",
             factor = 1,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAgrouped.trained,
             view = "Proteins",
             factor = 2,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAgrouped.trained,
             view = "Proteins",
             factor = 3,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_top_weights(MOFAgrouped.trained,
                 view = "Proteins",
                 factor = 1,
                 nfeatures = 20
)
plot_top_weights(MOFAgrouped.trained,
                 view = "Proteins",
                 factor = 2,
                 nfeatures = 20
)
plot_top_weights(MOFAgrouped.trained,
                 view = "Proteins",
                 factor = 3,
                 nfeatures = 20
)
rownames(sample_metadata) <- sample_metadata$sample
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 50, groups = "female",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 30, groups = "male",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 30, groups = "female",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 30, groups = "male",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 30, groups = "female",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 30, groups = "male",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)

plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "treatment", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "treatment",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
enrichment.parametric <- run_enrichment(MOFAgrouped.trained,
                                        view = "Proteins", factors = c(1,2,3),
                                        feature.sets = bin_db,
                                        sign = "negative",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 6
)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)
write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000/enrichment_negative_LF123.txt", sep = "\t", col.names = T, row.names = T)
enrichment.parametric <- run_enrichment(MOFAgrouped.trained,
                                        view = "Proteins", factors = c(1,2,3),
                                        feature.sets = bin_db,
                                        sign = "positive",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)
write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000/enrichment_positive_LF123.txt", sep = "\t", col.names = T, row.names = T)
factors <- get_factors(object =MOFAgrouped.trained, as.data.frame = T)
weights <- get_weights(object = MOFAgrouped.trained, as.data.frame = T)
variance <- MOFA2::get_variance_explained(object = MOFAgrouped.trained, as.data.frame = T)
write.table(factors, file = "D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000//FACTORS_MOFA2grouped_sex_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(weights, file = "D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000//WEIGHTS_MOFA2grouped_sex_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(variance$r2_per_factor, file = "D:/DSMarchantia_mofa2/outputs/grouped_sex_imputed_HVF2000//VARIANCE_MOFA2grouped_sex_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
dev.off()

pdf("D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000//MOFAgrouped_treatment_log1010_imputedF8T0_HVF2000.pdf")
plot_data_overview(MOFAgrouped_2.trained)
plot_factor_cor(MOFAgrouped_2.trained)
a <- plot_variance_explained(MOFAgrouped_2.trained, x="view", y="factor")
b <- plot_variance_explained(MOFAgrouped_2.trained, x="group", y="factor", plot_total = T)[[2]]
grid.arrange(b, a, ncol = 1, nrow = 2)
plot_factor(MOFAgrouped_2.trained, 
            factor = 1:3,
            color_by = "sex",
            shape_by = "group", dot_size = 3
)
plot_factor(MOFAgrouped_2.trained, 
            factor = 1:3,
            color_by = "group",
            shape_by = "sex", dot_size = 3
)

plot_factors(MOFAgrouped_2.trained, 
             factors = c(1,2,3),
             color_by = "group", dot_size = 3, shape_by = "sex"
)
plot_weights(MOFAgrouped_2.trained,
             view = "Proteins",
             factor = 1,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAgrouped_2.trained,
             view = "Proteins",
             factor = 2,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_weights(MOFAgrouped_2.trained,
             view = "Proteins",
             factor = 3,
             nfeatures = 20,     # Number of features to highlight
             scale = T,          # Scale weights from -1 to 1
             abs = F             # Take the absolute value?
)
plot_top_weights(MOFAgrouped_2.trained,
                 view = "Proteins",
                 factor = 1,
                 nfeatures = 30
)
plot_top_weights(MOFAgrouped_2.trained,
                 view = "Proteins",
                 factor = 2,
                 nfeatures = 30
)
plot_top_weights(MOFAgrouped_2.trained,
                 view = "Proteins",
                 factor = 3,
                 nfeatures = 30
)
rownames(sample_metadata) <- sample_metadata$sample
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 50, groups = "T0",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 50, groups = "T1",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 1,             # factor of interest
                  features = 50, groups = "T2",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)

plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 50, groups = "T0",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 50, groups = "T1",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 2,             # factor of interest
                  features = 50, groups = "T2",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 50, groups = "T0",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 50, groups = "T1",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)
plot_data_heatmap(MOFAgrouped_2.trained,
                  view = "Proteins",         # view of interest
                  factor = 3,             # factor of interest
                  features = 50, groups = "T2",          # number of features to plot (they are selected by weight)
                  
                  # extra arguments that are passed to the `pheatmap` function
                  cluster_rows = TRUE, cluster_cols = TRUE,
                  show_rownames = TRUE, show_colnames = TRUE, annotation_samples = sample_metadata[,c(-1,-5)]
)

plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "positive",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "negative",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 1, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "positive",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "negative",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = F, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 2, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "sex",
                  shape_by = "group", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)
plot_data_scatter(MOFAgrouped_2.trained, 
                  view = "Proteins", 
                  factor = 3, 
                  features = 20,         # Number of features to show
                  sign = "both",     # select top 6 features with positive weights
                  color_by = "group",
                  shape_by = "sex", # color cells by lineage
                  add_lm = T,          # add linear regression estimates
                  lm_per_group = T, 
                  dot_size = 2
)

enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = c(1,2),
                                        feature.sets = bin_db,
                                        sign = "negative",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 7
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 7
)

write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000/enrichment_negative_LF12(female-linked).txt", sep = "\t", col.names = T, row.names = T)
enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = c(1,2),
                                        feature.sets = bin_db,
                                        sign = "positive",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 1, 
                max.pathways = 5
)
plot_enrichment(enrichment.parametric, 
                factor = 2, 
                max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 1, 
                         max.genes = 10, 
                         max.pathways = 5
)
plot_enrichment_detailed(enrichment.parametric, 
                         factor = 2, 
                         max.genes = 10, 
                         max.pathways = 5
)
write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000/enrichment_positive_LF12(male-linked).txt", sep = "\t", col.names = T, row.names = T)
enrichment.parametric <- run_enrichment(MOFAgrouped_2.trained,
                                        view = "Proteins", factors = c(1,2,3),
                                        feature.sets = bin_db,
                                        sign = "all",
                                        # statistical.test = "permutation",
                                        min.size = 7
                                        #nperm = 5000
)
plot_enrichment(enrichment.parametric, 
                factor = 3, 
                max.pathways = 5
)

plot_enrichment_detailed(enrichment.parametric, 
                         factor = 3, 
                         max.genes = 10, 
                         max.pathways = 5
)

write.table(enrichment.parametric$pval.adj, "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000//enrichment_positivenegative_LF123.txt", sep = "\t", col.names = T, row.names = T)
factors <- get_factors(object =MOFAgrouped_2.trained, as.data.frame = T)
weights <- get_weights(object = MOFAgrouped_2.trained, as.data.frame = T)
variance <- MOFA2::get_variance_explained(object = MOFAgrouped_2.trained, as.data.frame = T)
write.table(factors, file = "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000//FACTORS_MOFA2grouped_treatment_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(weights, file = "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000/WEIGHTS_MOFA2grouped_treatment_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
write.table(variance$r2_per_factor, file = "D:/DSMarchantia_mofa2/outputs/grouped_treatment_imputed_HVF2000/VARIANCE_MOFA2grouped_treatment_log1010byme_F8T0imputed_HVF2000.txt", sep = "\t", col.names = T)
dev.off()

genes <- list("Q03033","Mapoly0001s0424.1.p", "Mapoly0013s0011.1.p", "Mapoly0019s0065.1.p")

plot_factors(MOFAglobal.trained, 
             factors = c(1,3), 
             color_by = "treatment",
             shape_by = "sex",
             scale = T,
             legend = T, dot_size = 3
)
genes %>% map(~ plot_factors(MOFAglobal.trained, 
                             factors = c(1,3), 
                             color_by = .,
                             shape_by = "sex",
                             scale = T,
                             legend = T, dot_size = 3
)) %>% cowplot::plot_grid(plotlist=., nrow=2)
plot_factors(MOFAglobal.trained, 
             factors = c(1,2), 
             color_by = "treatment",
             shape_by = "sex",
             scale = T,
             legend = T, dot_size = 3
)
genes %>% map(~ plot_factors(MOFAglobal.trained, 
                             factors = c(1,2), 
                             color_by = .,
                             shape_by = "sex",
                             scale = T,
                             legend = T, dot_size = 3
)) %>% cowplot::plot_grid(plotlist=., nrow=1)

genes <- "A3ANB5"
plot_factors(MOFAgrouped.trained, 
             factors = c(1,3), 
             color_by = "treatment",
             shape_by = "group",
             scale = T,
             legend = T, dot_size = 3, 
)
genes %>% map(~ plot_factors(MOFAglobal.trained, 
                             factors = c(1,3), 
                             color_by = ,
                             shape_by = "sex",
                             scale = T,
                             legend = T, dot_size = 3
)) %>% cowplot::plot_grid(plotlist=., nrow=1)


#######################################META-NETWORKS#####################################################

##Source for metanetwork analyses: https://seidr.readthedocs.io/en/latest/source/getting_started/getting_started.html

##Import values, normalize (same as MOFA2) and center around median (improvement for regulatory networks)

SariMetaNetworks <- read.delim("clipboard", header = T)
SariMetaNetworks[3:ncol(SariMetaNetworks)] <- log10(SariMetaNetworks[,3:ncol(SariMetaNetworks)]+10)
rownames(SariMetaNetworks) <- SariMetaNetworks$SamplesF
SariMetaNetworks <- SariMetaNetworks[,-c(1,2,3578)]

vars <- apply(SariMetaNetworks, 2, var)
filt_id <- which(is.finite(vars))
SariMetaNetworks <- SariMetaNetworks[, filt_id]
medians <- apply(SariMetaNetworks, 1, median)
vst <- sweep(SariMetaNetworks, MARGIN=1, FUN='-', STATS=medians)

MASS::write.matrix(x=unname(SariMetaNetworks[1:5,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpControl_Female_Log1010_abundance.tsv')
MASS::write.matrix(x=unname(SariMetaNetworks[6:10,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpControl_Male_Log1010_abundance.tsv')
MASS::write.matrix(x=unname(SariMetaNetworks[11:15,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT1_Female_Log1010_abundance.tsv')
MASS::write.matrix(x=unname(SariMetaNetworks[16:20,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT1_Male_Log1010_abundance.tsv')
MASS::write.matrix(x=unname(SariMetaNetworks[21:25,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT2_Female_Log1010_abundance.tsv')
MASS::write.matrix(x=unname(SariMetaNetworks[26:30,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT2_Male_Log1010_abundance.tsv')

MASS::write.matrix(x=unname(vst[1:5,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpControl_Female_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[6:10,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpControl_Male_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[11:15,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT1_Female_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[16:20,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT1_Male_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[21:25,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT2_Female_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[26:30,]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/MpT2_Male_Log1010medianCentered_abundance.tsv')

MASS::write.matrix(x=unname(vst[c(1:5,11:15,21:25),]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/medianCentered_sex/Female/Mp_Female_Log1010medianCentered_abundance.tsv')
MASS::write.matrix(x=unname(vst[c(6:10,16:20,26:30),]), sep='\t', file='D:/DSMarchantia_mofa2/MetaNetworks/input/medianCentered_sex/Male/Mp_Male_Log1010medianCentered_abundance.tsv')

write(colnames(vst), file="D:/DSMarchantia_mofa2/MetaNetworks/input/Proteins.txt")

##Let's go to metanetwork_bash script. To perform networks inference, backbonning, ranking, final meta-network

Control_Compara_bb1 <- read.delim("clipboard", header = F)
Control_Compara_bb128 <- read.delim("clipboard", header = F)
table(as.factor(Control_Compara_bb1$V2)) # 0(common nodes):3266, 1(female-specific nodes):151, 2(male-specific):151 | TOTAL NODES:3568
table(as.factor(Control_Compara_bb128$V2)) # 0(common nodes):1639, 1(female-specific nodes):844, 2(male-specific):721 | TOTAL NODES:3204
T1_Compara_bb1 <- read.delim("clipboard", header = F)
T1_Compara_bb128 <- read.delim("clipboard", header = F)
table(as.factor(T1_Compara_bb1$V2))# 0(common nodes):3410, 1(female-specific nodes):88, 2(male-specific):73 | TOTAL NODES:3571
table(as.factor(T1_Compara_bb128$V2))# 0(common nodes):2243, 1(female-specific nodes):624, 2(male-specific):541 | TOTAL NODES:3408
T2_Compara_bb1 <- read.delim("clipboard", header = F)
T2_Compara_bb128 <- read.delim("clipboard", header = F)
table(as.factor(T2_Compara_bb1$V2))# 0(common nodes):3150, 1(female-specific nodes):73, 2(male-specific):343 | TOTAL NODES:3566
table(as.factor(T2_Compara_bb128$V2))# 0(common nodes):1590, 1(female-specific nodes):467, 2(male-specific):1143 | TOTAL NODES:3200

MaleOnly <- read.delim("clipboard", header = T)  
FemaleOnly <- read.delim("clipboard", header = T)  
BothOnly <- read.delim("clipboard", header = T)  
MetaNetwork <- read.delim("clipboard", header = F)  

length(which(MetaNetwork$V1 %in% MaleOnly$Male & MetaNetwork$V2 %in% MaleOnly$Male)) # 455 Control edges between Male Specific Nodes Only 
MetaNetwork_MaleOnly <- MetaNetwork[which(MetaNetwork$V1 %in% MaleOnly$Male & MetaNetwork$V2 %in% MaleOnly$Male),] 
length(which(MetaNetwork$V1 %in% FemaleOnly$Female & MetaNetwork$V2 %in% FemaleOnly$Female)) # 463 Control edges between Female Specific Nodes Only
MetaNetwork_FemaleOnly <- MetaNetwork[which(MetaNetwork$V1 %in% FemaleOnly$Female & MetaNetwork$V2 %in% FemaleOnly$Female),]
length(which(MetaNetwork$V1 %in% BothOnly$Both & MetaNetwork$V2 %in% BothOnly$Both)) # 5710 Control edges between Both Specific Nodes Only
MetaNetwork_BothOnly <- MetaNetwork[which(MetaNetwork$V1 %in% BothOnly$Both & MetaNetwork$V2 %in% BothOnly$Both),]
write.table(MetaNetwork_MaleOnly, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/Control_Diff/backbonning_compare/Metanetwork_Compare_Control_FemaleVSMale_Network_MaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_FemaleOnly, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/Control_Diff/backbonning_compare/Metanetwork_Compare_Control_FemaleVSMale_Network_FemaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_BothOnly, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/Control_Diff/backbonning_compare/Metanetwork_Compare_Control_FemaleVSMale_Network_BothOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)

MaleOnly_T1 <- read.delim("clipboard", header = T)  
FemaleOnly_T1 <- read.delim("clipboard", header = T)  
BothOnly_T1 <- read.delim("clipboard", header = T)  
MetaNetwork_T1 <- read.delim("clipboard", header = F)  

length(which(MetaNetwork_T1$V1 %in% MaleOnly_T1$Male & MetaNetwork_T1$V2 %in% MaleOnly_T1$Male)) # 200 T1 edges between Male Specific Nodes Only 
MetaNetwork_MaleOnlyT1 <- MetaNetwork_T1[which(MetaNetwork_T1$V1 %in% MaleOnly_T1$Male & MetaNetwork_T1$V2 %in% MaleOnly_T1$Male),] 
length(which(MetaNetwork_T1$V1 %in% FemaleOnly_T1$Female & MetaNetwork_T1$V2 %in% FemaleOnly_T1$Female)) # 370 T1 edges between Female Specific Nodes Only
MetaNetwork_FemaleOnlyT1 <- MetaNetwork_T1[which(MetaNetwork_T1$V1 %in% FemaleOnly_T1$Female & MetaNetwork_T1$V2 %in% FemaleOnly_T1$Female),]
length(which(MetaNetwork_T1$V1 %in% BothOnly_T1$Both & MetaNetwork_T1$V2 %in% BothOnly_T1$Both)) # 12943 T1 edges between Both Specific Nodes Only
MetaNetwork_BothOnlyT1 <- MetaNetwork_T1[which(MetaNetwork_T1$V1 %in% BothOnly_T1$Both & MetaNetwork_T1$V2 %in% BothOnly_T1$Both),]
write.table(MetaNetwork_MaleOnlyT1, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T1_Diff/backbonning_compare/Metanetwork_Compare_T1_FemaleVSMale_Network_MaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_FemaleOnlyT1, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T1_Diff/backbonning_compare/Metanetwork_Compare_T1_FemaleVSMale_Network_FemaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_BothOnlyT1, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T1_Diff/backbonning_compare/Metanetwork_Compare_T1_FemaleVSMale_Network_BothOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)

MaleOnly_T2 <- read.delim("clipboard", header = T)  
FemaleOnly_T2 <- read.delim("clipboard", header = T)  
BothOnly_T2 <- read.delim("clipboard", header = T)  
MetaNetwork_T2 <- read.delim("clipboard", header = F)  

length(which(MetaNetwork_T2$V1 %in% MaleOnly_T2$Male & MetaNetwork_T2$V2 %in% MaleOnly_T2$Male)) # 1096 T2 edges between Male Specific Nodes Only 
MetaNetwork_MaleOnlyT2 <- MetaNetwork_T2[which(MetaNetwork_T2$V1 %in% MaleOnly_T2$Male & MetaNetwork_T2$V2 %in% MaleOnly_T2$Male),] 
length(which(MetaNetwork_T2$V1 %in% FemaleOnly_T2$Female & MetaNetwork_T2$V2 %in% FemaleOnly_T2$Female)) # 235 T2 edges between Female Specific Nodes Only
MetaNetwork_FemaleOnlyT2 <- MetaNetwork_T2[which(MetaNetwork_T2$V1 %in% FemaleOnly_T2$Female & MetaNetwork_T2$V2 %in% FemaleOnly_T2$Female),]
length(which(MetaNetwork_T2$V1 %in% BothOnly_T2$Both & MetaNetwork_T2$V2 %in% BothOnly_T2$Both)) # 7130 T2 edges between Both Specific Nodes Only
MetaNetwork_BothOnlyT2 <- MetaNetwork_T2[which(MetaNetwork_T2$V1 %in% BothOnly_T2$Both & MetaNetwork_T2$V2 %in% BothOnly_T2$Both),]
write.table(MetaNetwork_MaleOnlyT2, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T2_Diff/backbonning_compare/Metanetwork_Compare_T2_FemaleVSMale_Network_MaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_FemaleOnlyT2, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T2_Diff/backbonning_compare/Metanetwork_Compare_T2_FemaleVSMale_Network_FemaleSpecificOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)
write.table(MetaNetwork_BothOnlyT2, file = "D:/DSMarchantia_mofa2/MetaNetworks/output_SexXTreatment_diff/T2_Diff/backbonning_compare/Metanetwork_Compare_T2_FemaleVSMale_Network_BothOnly.txt", quote = F, sep = "\t", col.names = F, row.names = F)

##UpSet plots 

library(UpSetR)
NodesOnly <- list(MaleControl = MaleOnly$Male, FemaleControl = FemaleOnly$Female, BothControl = BothOnly$Both,
                  MaleT1 = MaleOnly_T1$Male, FemaleT1 = FemaleOnly_T1$Female, BothT1 = BothOnly_T1$Both,
                  MaleT2 = MaleOnly_T2$Male, FemaleT2 = FemaleOnly_T2$Female, BothT2 = BothOnly_T2$Both)
SaveNodesOnly <- upset(fromList(NodesOnly), order.by = "freq", text.scale = 2, empty.intersections = "on", nsets = 9)
### pasar tabla y sets interesantes para inspeccionar por enrichment a parte de los nodos especificos en si,
### por ejemplo: nodos especificos de ambos sexos en control y T1 pero que en T2 pasan a ser exclusivos de male (segunda intersecci?n)
library(nVennR)
VennRSets <- nVennR::plotVenn(NodesOnly, nCycles = 15000)
ListsVenn <- nVennR::listVennRegions(VennRSets)
for(i in 1:length(ListsVenn)){
  write.table(ListsVenn[i], file = paste0("D:/DSMarchantia_mofa2/MetaNetworks/input/",names(ListsVenn[i]), ".txt"), sep = "\t", quote = F, row.names = F) 
}

EdgesOnly <- list(MaleControl = paste0(MetaNetwork_MaleOnly$V1, ";", MetaNetwork_MaleOnly$V2), 
                  FemaleControl = paste0(MetaNetwork_FemaleOnly$V1, ";", MetaNetwork_FemaleOnly$V2),
                  BothControl = paste0(MetaNetwork_BothOnly$V1, ";", MetaNetwork_BothOnly$V2),
                  MaleT1 = paste0(MetaNetwork_MaleOnlyT1$V1, ";", MetaNetwork_MaleOnlyT1$V2),
                  FemaleT1 = paste0(MetaNetwork_FemaleOnlyT1$V1, ";", MetaNetwork_FemaleOnlyT1$V2),
                  BothT1 = paste0(MetaNetwork_BothOnlyT1$V1, ";", MetaNetwork_BothOnlyT1$V2),
                  MaleT2 = paste0(MetaNetwork_MaleOnlyT2$V1, ";", MetaNetwork_MaleOnlyT2$V2),
                  FemaleT2 = paste0(MetaNetwork_FemaleOnlyT2$V1, ";", MetaNetwork_FemaleOnlyT2$V2),
                  BothT2 = paste0(MetaNetwork_BothOnlyT2$V1, ";", MetaNetwork_BothOnlyT2$V2))
upset(fromList(EdgesOnly), order.by = "freq", text.scale = 2, empty.intersections = "on", nsets = 9)

##Enrichment analysis

##ORA Node specific per group/treatment

library(clusterProfiler)

##Database preparation zone: Mercator4 Proteome, Mercator4 Genome
##Mercator4 Proteome

MpMercator4Proteome <- read.delim("clipboard", header = T)
Mp.backgroundProteome <- data.frame(BIN = MpMercator4$BINCODE.1, Gene = MpMercator4$IDENTIFIER, Terms = MpMercator4$NAME.1) 

##Mercator4 Genome

Mp.backgroundGenome <- read.delim("clipboard", header = T)
Mp.backgroundGenome$BINCODE <- do.call(rbind,strsplit(gsub("'", "", Mp.backgroundGenome$BINCODE, fixed = T), split = ".", fixed = T))[,1]
Mp.backgroundGenome$NAME <- do.call(rbind,strsplit(gsub("'", "", Mp.backgroundGenome$NAME, fixed = T), split = ".", fixed = T))[,1]
Mp.backgroundGenome$IDENTIFIER <- gsub("'","", Mp.backgroundGenome$IDENTIFIER)
Mp.backgroundGenome$IDENTIFIER <- gsub("m","M", Mp.backgroundGenome$IDENTIFIER)
Mp.backgroundGenome <- Mp.backgroundGenome[grep("Mapoly", Mp.backgroundGenome$IDENTIFIER),]
Mp.backgroundGenome <- data.frame(BIN = Mp.backgroundGenome$BINCODE, Gene = Mp.backgroundGenome$IDENTIFIER, Terms = Mp.backgroundGenome$NAME) 

##GO Genome
Mp.backgroundGO <- read.delim("clipboard", header = T)
Mp_GO <- Mp.backgroundGO[,c(4,10)]
colnames(Mp_GO) <- c("ID", "GO")
Mp_GO <- Mp_GO[,c(2,1)]
Mp.GOmap <- buildGOmap(gomap = Mp_GO)
Mp.Ont <- go2ont(Mp.GOmap$GO)
Mp.background.genes <- Mp.GOmap[Mp.GOmap$GO %in% Mp.Ont$go_id[which(Mp.Ont$Ontology == "BP")],]

Mp.Term2name <- go2term(goid = Mp.background.genes$GO)
Mp.terms <- list()
Remove <- list()
for(i in 1:nrow(Mp.background.genes)){
  hit <- which(Mp.Term2name$go_id == Mp.background.genes$GO[i])
  if(length(hit) == 0){
    Remove[[i]] <- i
    next
  }
  Mp.terms[[i]] <- Mp.Term2name$Term[hit]
}

if(length(Remove) != 0){
  Mp.background.genes <- Mp.background.genes[-do.call(rbind, Remove)[,1],]  
}
Mp.background.genes$Terms <- do.call(rbind, Mp.terms)[,1]
write.table(Mp.background.genes, file = "D:/DSMarchantia_mofa2/input/annotation/Mp_GO_BP_Universe.txt", quote = F, sep = "\t", col.names = T, row.names = F)
Mp.background.genes <- read.table("D:/DSMarchantia_mofa2/input/annotation/Mp/Mp_GO_BP_Universe.txt", sep = "\t", header = T)

##Perform enrichment computation with NodeExclusiveOnly sets
##MercatorBins Proteome as background

myResults.MaleOnlyProteome <- enricher(gene = do.call(rbind,strsplit(MaleOnly$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyProteome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyProteome <- enricher(gene = do.call(rbind,strsplit(BothOnly$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
#geneListControl <- list(Both = do.call(rbind,strsplit(BothOnly$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly$Female, split = ",", fixed = T))[,1])
#ControlProteome <- compareCluster(geneClusters = geneListControl, fun = enricher, universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)])
#dotplot.ControlProteome <- dotplot(ControlProteome, showCategory = 30)

myResults.MaleOnlyT1Proteome <- enricher(gene = do.call(rbind,strsplit(MaleOnly_T1$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyT1Proteome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly_T1$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyT1Proteome <- enricher(gene = do.call(rbind,strsplit(BothOnly_T1$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
#geneListT1 <- list(Both = do.call(rbind,strsplit(BothOnly_T1$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly_T1$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly_T1$Female, split = ",", fixed = T))[,1])
#T1Proteome <- compareCluster(geneClusters = geneListT1, fun = enricher, universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)])
#dotplot.T1Proteome <- dotplot(T1Proteome, showCategory = 30)

myResults.MaleOnlyT2Proteome <- enricher(gene = do.call(rbind,strsplit(MaleOnly_T2$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyT2Proteome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly_T2$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyT2Proteome <- enricher(gene = do.call(rbind,strsplit(BothOnly_T2$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)], pvalueCutoff = 1)
#geneListT2 <- list(Both = do.call(rbind,strsplit(BothOnly_T2$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly_T2$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly_T2$Female, split = ",", fixed = T))[,1])
#T2Proteome <- compareCluster(geneClusters = geneListT2, fun = enricher, universe = levels(as.factor(Mp.backgroundProteome$Gene)), TERM2GENE = Mp.backgroundProteome[,c(1,2)], TERM2NAME = Mp.backgroundProteome[,c(1,3)])
#dotplot.T2Proteome <- dotplot(T2Proteome, showCategory = 30)


##MercatorBin Genome as background

myResults.MaleOnlyGenome <- enricher(gene = do.call(rbind,strsplit(MaleOnly$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyGenome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyGenome <- enricher(gene = do.call(rbind,strsplit(BothOnly$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
geneListControl <- list(Both = do.call(rbind,strsplit(BothOnly$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly$Female, split = ",", fixed = T))[,1])
ControlGenome <- compareCluster(geneClusters = geneListControl, fun = enricher, universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)])
dotplot.ControlGenome <- dotplot(ControlGenome, showCategory = 30)
write.table(dotplot.ControlGenome$data, file = "D:/DSMarchantia_mofa2/MetaNetworks/dotplotControlGenomeMercator4.txt", sep = "\t", row.names = F, col.names = T, quote = F)

myResults.MaleOnlyT1Genome <- enricher(gene = do.call(rbind,strsplit(MaleOnly_T1$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyT1Genome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly_T1$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyT1Genome <- enricher(gene = do.call(rbind,strsplit(BothOnly_T1$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
geneListT1 <- list(Both = do.call(rbind,strsplit(BothOnly_T1$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly_T1$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly_T1$Female, split = ",", fixed = T))[,1])
T1Genome <- compareCluster(geneClusters = geneListT1, fun = enricher, universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)])
dotplot.T1Genome <- dotplot(T1Genome, showCategory = 30)
write.table(dotplot.T1Genome$data, file = "D:/DSMarchantia_mofa2/MetaNetworks/dotplotT1GenomeMercator4.txt", sep = "\t", row.names = F, col.names = T, quote = F)

myResults.MaleOnlyT2Genome <- enricher(gene = do.call(rbind,strsplit(MaleOnly_T2$Male, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.FemaleOnlyT2Genome <- enricher(gene = do.call(rbind,strsplit(FemaleOnly_T2$Female, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
myResults.BothOnlyT2Genome <- enricher(gene = do.call(rbind,strsplit(BothOnly_T2$Both, split = ",", fixed = T))[,1], universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)], pvalueCutoff = 1)
geneListT2 <- list(Both = do.call(rbind,strsplit(BothOnly_T2$Both, split = ",", fixed = T))[,1], Male = do.call(rbind,strsplit(MaleOnly_T2$Male, split = ",", fixed = T))[,1], Female = do.call(rbind,strsplit(FemaleOnly_T2$Female, split = ",", fixed = T))[,1])
T2Genome <- compareCluster(geneClusters = geneListT2, fun = enricher, universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)])
dotplot.T2Genome <- dotplot(T2Genome, showCategory = 30)
write.table(dotplot.T2Genome$data, file = "D:/DSMarchantia_mofa2/MetaNetworks/dotplotT2GenomeMercator4.txt", sep = "\t", row.names = F, col.names = T, quote = F)

##GO Genome as background

##ControlGenomeGO <- compareCluster(geneClusters = geneListControl, fun = enricher, universe = levels(as.factor(Mp.background.genes$Gene)), TERM2GENE = Mp.background.genes[,c(1,2)], TERM2NAME = Mp.background.genes[,c(1,3)])
##dotplot.ControlGenomeGO <- dotplot(ControlGenomeGO, showCategory = 30)

##T1GenomeGO <- compareCluster(geneClusters = geneListT1, fun = enricher, universe = levels(as.factor(Mp.background.genes$Gene)), TERM2GENE = Mp.background.genes[,c(1,2)], TERM2NAME = Mp.background.genes[,c(1,3)])
##dotplot.T1GenomeGO <- dotplot(T1GenomeGO, showCategory = 30)

##T2GenomeGO <- compareCluster(geneClusters = geneListT2, fun = enricher, universe = levels(as.factor(Mp.background.genes$Gene)), TERM2GENE = Mp.background.genes[,c(1,2)], TERM2NAME = Mp.background.genes[,c(1,3)])
##dotplot.T2GenomeGO <- dotplot(T2GenomeGO, showCategory = 30)

##ORA Node Upset Sets

names(ListsVenn) <- do.call(rbind,strsplit(names(ListsVenn), split = "(", fixed = T))[,2]
names(ListsVenn) <- gsub(")","",names(ListsVenn), fixed = T)
names(ListsVenn) <- gsub(" ","",names(ListsVenn), fixed = T)
ListVennInput <- lapply(ListsVenn, function(x){
  do.call(rbind, strsplit(x, split = ",", fixed = T))
})
UpsetGenome <- compareCluster(geneClusters = ListVennInput, fun = enricher, universe = levels(as.factor(Mp.backgroundGenome$Gene)), TERM2GENE = Mp.backgroundGenome[,c(1,2)], TERM2NAME = Mp.backgroundGenome[,c(1,3)])
dotplot.UpsetGenome <- dotplot(UpsetGenome, showCategory = 55)
write.table(dotplot.UpsetGenome$data, file = "D:/DSMarchantia_mofa2/MetaNetworks/dotplotUpsetSetsGenomeMercator4.txt", sep = "\t", row.names = F, col.names = T, quote = F)

##NEAT. Infomap topology enrichment based in general metanetwork of male and female (no compare) with NEAT

library(neat)

Metanetwork_Female <- read.delim("clipboard", header = T)
Metanetwork_Male <- read.delim("clipboard", header = T)
cluster_set_infomapFemale <- read.delim("clipboard", header = T)
cluster_set_infomapMale <- read.delim("clipboard", header = T)

##GenomeBackground
functional_set <- read.delim("clipboard", header = T) # antes lo llame Mp.backgroundGenome
binnames <- functional_set[!duplicated(functional_set$BIN),3]

##ProtemeBackground
functional_set <- read.delim("clipboard", header = T) # antes lo llame Mp.backgroundGenome

##Define format

cluster_sets_dfs <- list(Female = cluster_set_infomapFemale, Male = cluster_set_infomapMale)

cluster_sets_list <- lapply(cluster_sets_dfs, function(x){
  cluster_sets <- list()
  for(i in levels(as.factor(x$InfomapModule))){
    cluster_sets[[i]] <- x$MpID[which(x$InfomapModule == i)]
  }
  cluster_sets
})

functional_sets_list <- list()
for(i in levels(as.factor(functional_set$BIN))){
  functional_sets_list[[i]] <- functional_set$Gene[which(functional_set$BIN == i)]
}
names(functional_sets_list) <- c("Photosynthesis", "Cellular respiration", "Carbohydrate metabolism", "Amino acid metabolism",
                                 "Lipid metabolism", "Nucleotide metabolism", "Coenzyme metabolism", "Polyamine metabolism",
                                 "Secondary metabolism", "Redox homeostasis", "Phytohormone action", "Chromatin organisation",
                                 "Cell division", "DNA damage response", "RNA biosynthesis", "RNA processing", "Protein biosynthesis",
                                 "Protein modification", "Protein homeostasis", "Cytoskeleton organisation", "Cell wall organisation",
                                 "Vesivle trafficking", "Protein translocation", "Solute transport", "Nutrient uptake", "not assigned", "External stimuli response", 
                                 "Multi-process regulation", "Plant reproduction", "Clade-specific metabolism", "Enzyme classification")
names(functional_sets_list) <- c("Photosynthesis", "Redox homeostasis", "Phytohormone action", "Chromatin organisation", "Cell division",
                                 "DNA damage response", "RNA biosynthesis", "RNA processing", "Protein biosynthesis", "Protein modification",
                                 "Protein homeostasis", "Cellular respiration", "Cytoskeleton organisation", "Cell wall organisation", "Vesicle trafficking",
                                 "Protein translocation", "Solute transport", "Nutrient uptake", "External stimuli response", "Multi-process regulation",
                                 "Plant reproduction", "Carbohydrate metabolism", "Clade-specific metabolism", "not assigned", "Amino acid metabolism",
                                 "Lipid metabolism", "Enzyme classification", "Nucleotide metabolism", "Coenzyme metabolism", "Polyamine metabolism", 
                                 "Secondary metabolism")
network_list <- list(Female = Metanetwork_Female[,c(1,2)], Male = Metanetwork_Male[,c(1,2)])

##Compute enrichment

Network_Enrichment_Female <- neat(alist = cluster_sets_list[[1]], blist = functional_sets_list, network = as.matrix(network_list[[1]]), 
                                  nettype = 'directed', nodes = cluster_sets_dfs[[1]]$MpID, alpha = 0.05)
Network_Enrichment_Male <- neat(alist = cluster_sets_list[[2]], blist = functional_sets_list, network = as.matrix(network_list[[2]]), 
                                nettype = 'directed', nodes = cluster_sets_dfs[[2]]$MpID, alpha = 0.05)
write.table(Network_Enrichment_Female, file = "D:/DSMarchantia_mofa2/MetaNetworks/NEAT_MetanetworkGeneralFemale_ProteomeBackground.txt", sep = "\t", quote = F, col.names = T, row.names = F)
write.table(Network_Enrichment_Male, file = "D:/DSMarchantia_mofa2/MetaNetworks/NEAT_MetanetworkGeneralMale_ProteomeBackground.txt", sep = "\t", quote = F, col.names = T, row.names = F)

##plot the results: all radars show bins for which some Over/Under enrichment is detected

library(RColorBrewer)
library(scales)
library(fmsb)
coul <- brewer.pal(3, "YlGnBu")
colors_border <- coul
colors_in <- alpha(coul,0.3)

Cluster.Enrichments <- list()
pdf("D:/DSMarchantia_mofa2/MetaNetworks/NEAT_MetanetworkGeneralFemale_ProteomeBackground_radarplots.pdf")
for(j in levels(as.factor(Network_Enrichment_Female$A))){
  if(length(which(Network_Enrichment_Male$A == j & Network_Enrichment_Male$conclusion == "No enrichment")) == 29){
    cat(paste0("\n No enrichments detected for ", j, " Informap Cluster \n"))
    next
  }
  categories <- unique(Network_Enrichment_Male$B[which(Network_Enrichment_Male$conclusion != "No enrichment")])
  df <- Network_Enrichment_Male[which(Network_Enrichment_Male$A == j),]
  df <- df[which(df$B %in% categories),]
  df$value <- NA
  df$adjusted_p[which(df$conclusion == "No enrichment")] <- 1
  df$value <- -log10(df$adjusted_p)
  max <- rep(13 , nrow(df))
  min <- rep(0  , nrow(df))
  # prepare rows
  df <- df[order(df$B, decreasing = F),]
  up <- df$value
  down <- df$value
  up[which(df$conclusion == "Underenrichment")] <- 0
  down[which(df$conclusion == "Overenrichment")] <- 0
  # categories in same order
  Rad = rbind(max, min,up , down)
  colnames(Rad) <- df$B
  Cluster.Enrichments[[j]] <- radarchart(as.data.frame(Rad), axistype = 0, 
                                         # polygon
                                         pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
                                         # grid 
                                         cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8,
                                         # label
                                         vlcex = 1, title = paste0("Infomap Cluster:", j)
  )}
dev.off()

###########################################PROTEIN LOCATION#################################################

##Packages

library(GenomicRanges)
library(rtracklayer)
library(pheatmap)

##First export all sets of proteins from univariate table --> sex x 2 (pref male or pref fem), sex and stress x 2, stress,  and no regulated

univariate_analysis <- read.delim("clipboard", header = T)
femalesex_stress <- univariate_analysis[c(which(univariate_analysis$female.T0...female.T1 <= 0.05),which(univariate_analysis$female.T0...female.T2 <= 0.05),which(univariate_analysis$female.T1...female.T2 <= 0.05)),1]
femalesex_stress <- femalesex_stress[!duplicated(femalesex_stress)]

malesex_stress <- univariate_analysis[c(which(univariate_analysis$male.T0...male.T1 <= 0.05),which(univariate_analysis$male.T0...male.T2 <= 0.05),which(univariate_analysis$male.T1...male.T2 <= 0.05)),1]
malesex_stress <- malesex_stress[!duplicated(malesex_stress)]

femalesex_stress_exclusive <- femalesex_stress[!femalesex_stress %in% malesex_stress]
malesex_stress_exclusive <- malesex_stress[!malesex_stress %in% femalesex_stress]

stress_0 <- c(femalesex_stress[femalesex_stress %in% malesex_stress], malesex_stress[malesex_stress %in% femalesex_stress])
stress_0 <- stress_0[!duplicated(stress_0)]

sex_female_exclusive <- univariate_analysis$Variables[c(which(univariate_analysis$female.T0...male.T0 <= 0.05 & univariate_analysis$Mean.female.T0 > univariate_analysis$Mean.male.T0), which(univariate_analysis$female.T1...male.T1 <= 0.05 & univariate_analysis$Mean.female.T1 > univariate_analysis$Mean.male.T1), which(univariate_analysis$female.T2...male.T2 <= 0.05 & univariate_analysis$Mean.female.T2 > univariate_analysis$Mean.male.T2))]
sex_female_exclusive <- sex_female_exclusive[!duplicated(sex_female_exclusive)]

sex_male_exclusive <- univariate_analysis$Variables[c(which(univariate_analysis$female.T0...male.T0 <= 0.05 & univariate_analysis$Mean.female.T0 < univariate_analysis$Mean.male.T0), which(univariate_analysis$female.T1...male.T1 <= 0.05 & univariate_analysis$Mean.female.T1 < univariate_analysis$Mean.male.T1), which(univariate_analysis$female.T2...male.T2 <= 0.05 & univariate_analysis$Mean.female.T2 < univariate_analysis$Mean.male.T2))]
sex_male_exclusive <- sex_male_exclusive[!duplicated(sex_male_exclusive)]

sex_female_exclusive <- sex_female_exclusive[!sex_female_exclusive %in% sex_male_exclusive]
sex_male_exclusive <- sex_male_exclusive[!sex_male_exclusive %in% sex_female_exclusive]

stress_1 <- univariate_analysis[c(which(univariate_analysis$female.T1...male.T0 <= 0.05),which(univariate_analysis$female.T2...male.T0 <= 0.05),which(univariate_analysis$female.T0...male.T1 <= 0.05), which(univariate_analysis$female.T2...male.T1 <= 0.05), which(univariate_analysis$female.T0...male.T2 <= 0.05), which(univariate_analysis$female.T1...male.T2 <= 0.05)),1]
stress <- c(stress_0, stress_1)
stress <- stress[!duplicated(stress)]
stress_exclusive <- stress[!stress %in% malesex_stress_exclusive]
stress_exclusive <- stress_exclusive[!stress_exclusive %in% femalesex_stress_exclusive]

malesex_stress_exclusive <- malesex_stress_exclusive[!malesex_stress_exclusive %in% sex_male_exclusive]
malesex_stress_exclusive <- malesex_stress_exclusive[!malesex_stress_exclusive %in% sex_female_exclusive]

femalesex_stress_exclusive <- femalesex_stress_exclusive[!femalesex_stress_exclusive %in% sex_male_exclusive]
femalesex_stress_exclusive <- femalesex_stress_exclusive[!femalesex_stress_exclusive %in% sex_female_exclusive]

stress_exclusive <- stress_exclusive[!stress_exclusive %in% sex_male_exclusive]
stress_exclusive <- stress_exclusive[!stress_exclusive %in% sex_female_exclusive]

##Double check all exclusive
duplicated(c(stress_exclusive, femalesex_stress_exclusive, malesex_stress_exclusive, sex_male_exclusive, sex_female_exclusive))

noregulation <- univariate_analysis$Variables[!univariate_analysis$Variables %in% c(stress_exclusive, femalesex_stress_exclusive, malesex_stress_exclusive, sex_male_exclusive, sex_female_exclusive)]
MyProteinSetsJGI <- list(NoRegulation = noregulation,
                         AllSex = c(sex_female_exclusive, sex_male_exclusive, femalesex_stress_exclusive, malesex_stress_exclusive),
                         AllStress = c(stress_exclusive, femalesex_stress_exclusive, malesex_stress_exclusive),
                         AllFemale = c(sex_female_exclusive, femalesex_stress_exclusive),
                         AllMale = c(sex_male_exclusive, malesex_stress_exclusive),
                         Female_NoStress_Exclusive = sex_female_exclusive,
                         Male_NoStress_Exclusive = sex_male_exclusive,
                         Female_Stress_Exclusive = femalesex_stress_exclusive,
                         Male_Stress_Exclusive = malesex_stress_exclusive,
                         Stress_Exclusive = stress_exclusive)

##Change IDs to match Map6.1 genome version from JGI 3.1 version

Mpgenomeconversion <- read.delim("clipboard", header = T)
MyProteinSets6.1 <- list(NoRegulation = c(), AllSex = c(), AllStress = c(), AllFemale = c(), AllMale = c(), 
                         Female_NoStress_Exclusive = c(),
                         Male_NoStress_Exclusive = c(),
                         Female_Stress_Exclusive = c(),
                         Male_Stress_Exclusive = c(),
                         Stress_Exclusive = c())

for(i in 1:length(MyProteinSetsJGI)){
  for(j in 1:length(MyProteinSetsJGI[[i]])){
    query <- strsplit(MyProteinSetsJGI[[i]][j], split = ".", fixed = T)[[1]][1]
    if(length(which(Mpgenomeconversion$JGI3.1 == query)) == 0){
      MyProteinSets6.1[[i]][[j]] <- NA
    }else if(length(which(Mpgenomeconversion$JGI3.1 == query)) > 0){
      substituted <- Mpgenomeconversion$MpTak_v6.1[which(Mpgenomeconversion$JGI3.1 == query)]
      MyProteinSets6.1[[i]][[j]] <- substituted[!duplicated(substituted)]
    }
  }
}

write.table(x = as.matrix(MyProteinSets6.1$NoRegulation[!is.na(MyProteinSets6.1$NoRegulation)]), "../Genomics/queryIDs/NoRegulation.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$AllSex[!is.na(MyProteinSets6.1$AllSex)]), "../Genomics/queryIDs/AllSex.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$AllStress[!is.na(MyProteinSets6.1$AllStress)]), "../Genomics/queryIDs/AllStress.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$AllFemale[!is.na(MyProteinSets6.1$AllFemale)]), "../Genomics/queryIDs/AllFemale.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$AllMale[!is.na(MyProteinSets6.1$AllMale)]), "../Genomics/queryIDs/AllMale.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$Female_NoStress_Exclusive[!is.na(MyProteinSets6.1$Female_NoStress_Exclusive)]), "../Genomics/queryIDs/Female_NoStress_Exclusive.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$Male_NoStress_Exclusive[!is.na(MyProteinSets6.1$Male_NoStress_Exclusive)]), "../Genomics/queryIDs/Male_NoStress_Exclusive.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$Female_Stress_Exclusive[!is.na(MyProteinSets6.1$Female_Stress_Exclusive)]), "../Genomics/queryIDs/Female_Stress_Exclusive.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$Male_Stress_Exclusive[!is.na(MyProteinSets6.1$Male_Stress_Exclusive)]), "../Genomics/queryIDs/Male_Stress_Exclusive.txt", quote = F, col.names = F, row.names = F)
write.table(x = as.matrix(MyProteinSets6.1$Stress_Exclusive[!is.na(MyProteinSets6.1$Stress_Exclusive)]), "../Genomics/queryIDs/Stress_Exclusive.txt", quote = F, col.names = F, row.names = F)

##Manual hypergeometric test and * number of chrosomes (test) for each test or storing in p-adjust 

AllFemale <- read.delim("clipboard", header = F)
AllMale <- read.delim("clipboard", header = F)
AllSex <- read.delim("clipboard", header = F)
AllStress <- read.delim("clipboard", header = F)
Female_NoStress_Exclusive <- read.delim("clipboard", header = F)
Female_Stress_Exclusive <- read.delim("clipboard", header = F)
Male_NoStress_Exclusive <- read.delim("clipboard", header = F)
Male_Stress_Exclusive <- read.delim("clipboard", header = F)
NoRegulation <- read.delim("clipboard", header = F)
Stress_Exclusive<- read.delim("clipboard", header = F)

##Universe definition: genome universe JGI3.1 biased and proteome universe JGI3.1 biased

GenomeUniverse <- read.delim("clipboard", header = T)
GenomeUniverse <- GenomeUniverse$MpTak_v6.1[which(GenomeUniverse$JGI3.1 != "-" & GenomeUniverse$MpTak_v6.1 != "-")]
GenomeUniverse <- GenomeUniverse[!duplicated(GenomeUniverse)]
ProteomeUniverse <- c(Female_NoStress_Exclusive$V1, Female_Stress_Exclusive$V1, Male_NoStress_Exclusive$V1, Male_Stress_Exclusive$V1, NoRegulation$V1, Stress_Exclusive$V1)
ProteomeUniverse <- ProteomeUniverse[!duplicated(ProteomeUniverse)]

##Genome Universe Enrichments

##AllFemale Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",AllFemale$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllFemale$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllFemale$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllFemale$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllFemale$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllFemale$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllFemale$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllFemale$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllFemale$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllFemale$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(AllFemale$V1), lower.tail = F)
AllFemale.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllMale Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",AllMale$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllMale$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllMale$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllMale$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllMale$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllMale$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllMale$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllMale$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllMale$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllMale$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(AllMale$V1), lower.tail = F)
AllMale.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllSex Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",AllSex$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllSex$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllSex$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllSex$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllSex$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllSex$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllSex$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllSex$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllSex$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllSex$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(AllSex$V1), lower.tail = F)
AllSex.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllStress Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",AllStress$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllStress$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllStress$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllStress$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllStress$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllStress$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllStress$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllStress$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllStress$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllStress$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(AllStress$V1), lower.tail = F)
AllStress <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Female_NoStress_Exclusive Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",Female_NoStress_Exclusive$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Female_NoStress_Exclusive$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Female_NoStress_Exclusive$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Female_NoStress_Exclusive$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Female_NoStress_Exclusive$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Female_NoStress_Exclusive$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Female_NoStress_Exclusive$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Female_NoStress_Exclusive$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Female_NoStress_Exclusive$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Female_NoStress_Exclusive$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
Female_NoStress_Exclusive.p <-  p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Female_Stress_Exclusive Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",Female_Stress_Exclusive$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Female_Stress_Exclusive$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Female_Stress_Exclusive$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Female_Stress_Exclusive$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Female_Stress_Exclusive$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Female_Stress_Exclusive$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Female_Stress_Exclusive$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Female_Stress_Exclusive$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Female_Stress_Exclusive$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Female_Stress_Exclusive$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
Female_Stress_Exclusive.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Male_NoStress_Exclusive Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",Male_NoStress_Exclusive$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Male_NoStress_Exclusive$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Male_NoStress_Exclusive$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Male_NoStress_Exclusive$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Male_NoStress_Exclusive$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Male_NoStress_Exclusive$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Male_NoStress_Exclusive$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Male_NoStress_Exclusive$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Male_NoStress_Exclusive$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Male_NoStress_Exclusive$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
Male_NoStress_Exclusive.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Male_Stress_Exclusive Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",Male_Stress_Exclusive$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Male_Stress_Exclusive$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Male_Stress_Exclusive$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Male_Stress_Exclusive$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Male_Stress_Exclusive$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Male_Stress_Exclusive$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Male_Stress_Exclusive$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Male_Stress_Exclusive$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Male_Stress_Exclusive$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Male_Stress_Exclusive$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
Male_Stress_Exclusive.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Stress_Exclusive Enrichment in chr from 1:8 and U/V 

pU <- phyper(length(grep("MpU",Stress_Exclusive$V1))-1, length(grep("MpU", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpU", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Stress_Exclusive$V1))-1, length(grep("MpV", GenomeUniverse)), length(GenomeUniverse) - length(grep("MpV", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Stress_Exclusive$V1))-1, length(grep("Mp1", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp1", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Stress_Exclusive$V1))-1, length(grep("Mp2", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp2", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Stress_Exclusive$V1))-1, length(grep("Mp3", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp3", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Stress_Exclusive$V1))-1, length(grep("Mp4", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp4", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Stress_Exclusive$V1))-1, length(grep("Mp5", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp5", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Stress_Exclusive$V1))-1, length(grep("Mp6", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp6", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Stress_Exclusive$V1))-1, length(grep("Mp7", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp7", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Stress_Exclusive$V1))-1, length(grep("Mp8", GenomeUniverse)), length(GenomeUniverse) - length(grep("Mp8", GenomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
Stress_Exclusive.p <- p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Proteome Universe Enrichments

##AllFemale Enrichment in chr from 1:8 and U/V == only enriched chr1

pU <- phyper(length(grep("MpU",AllFemale$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllFemale$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllFemale$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllFemale$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllFemale$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllFemale$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllFemale$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllFemale$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllFemale$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllFemale$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(AllFemale$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllMale Enrichment in chr from 1:8 and U/V == only enriched chr1

pU <- phyper(length(grep("MpU",AllMale$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllMale$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllMale$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllMale$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllMale$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllMale$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllMale$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllMale$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllMale$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllMale$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(AllMale$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllSex Enrichment in chr from 1:8 and U/V == only enriched chr1

pU <- phyper(length(grep("MpU",AllSex$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllSex$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllSex$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllSex$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllSex$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllSex$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllSex$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllSex$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllSex$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllSex$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(AllSex$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##AllStress Enrichment in chr from 1:8 and U/V == no Enrichment

pU <- phyper(length(grep("MpU",AllStress$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",AllStress$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",AllStress$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",AllStress$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",AllStress$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",AllStress$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",AllStress$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",AllStress$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",AllStress$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",AllStress$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(AllStress$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Female_NoStress_Exclusive Enrichment in chr from 1:8 and U/V == chr1 enrichment

pU <- phyper(length(grep("MpU",Female_NoStress_Exclusive$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Female_NoStress_Exclusive$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Female_NoStress_Exclusive$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Female_NoStress_Exclusive$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Female_NoStress_Exclusive$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Female_NoStress_Exclusive$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Female_NoStress_Exclusive$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Female_NoStress_Exclusive$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Female_NoStress_Exclusive$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Female_NoStress_Exclusive$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(Female_NoStress_Exclusive$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Female_Stress_Exclusive Enrichment in chr from 1:8 and U/V == no Enrichment

pU <- phyper(length(grep("MpU",Female_Stress_Exclusive$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Female_Stress_Exclusive$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Female_Stress_Exclusive$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Female_Stress_Exclusive$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Female_Stress_Exclusive$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Female_Stress_Exclusive$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Female_Stress_Exclusive$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Female_Stress_Exclusive$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Female_Stress_Exclusive$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Female_Stress_Exclusive$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(Female_Stress_Exclusive$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Male_NoStress_Exclusive Enrichment in chr from 1:8 and U/V == chr1 enrichment

pU <- phyper(length(grep("MpU",Male_NoStress_Exclusive$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Male_NoStress_Exclusive$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Male_NoStress_Exclusive$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Male_NoStress_Exclusive$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Male_NoStress_Exclusive$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Male_NoStress_Exclusive$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Male_NoStress_Exclusive$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Male_NoStress_Exclusive$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Male_NoStress_Exclusive$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Male_NoStress_Exclusive$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(Male_NoStress_Exclusive$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Male_Stress_Exclusive Enrichment in chr from 1:8 and U/V == chr3 Enrichment

pU <- phyper(length(grep("MpU",Male_Stress_Exclusive$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Male_Stress_Exclusive$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Male_Stress_Exclusive$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Male_Stress_Exclusive$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Male_Stress_Exclusive$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Male_Stress_Exclusive$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Male_Stress_Exclusive$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Male_Stress_Exclusive$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Male_Stress_Exclusive$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Male_Stress_Exclusive$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(Male_Stress_Exclusive$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

##Stress_Exclusive Enrichment in chr from 1:8 and U/V == chr3 Enrichment

pU <- phyper(length(grep("MpU",Stress_Exclusive$V1))-1, length(grep("MpU", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpU", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
pV <- phyper(length(grep("MpV",Stress_Exclusive$V1))-1, length(grep("MpV", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("MpV", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p1 <- phyper(length(grep("Mp1",Stress_Exclusive$V1))-1, length(grep("Mp1", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp1", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p2 <- phyper(length(grep("Mp2",Stress_Exclusive$V1))-1, length(grep("Mp2", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp2", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p3 <- phyper(length(grep("Mp3",Stress_Exclusive$V1))-1, length(grep("Mp3", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp3", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p4 <- phyper(length(grep("Mp4",Stress_Exclusive$V1))-1, length(grep("Mp4", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp4", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p5 <- phyper(length(grep("Mp5",Stress_Exclusive$V1))-1, length(grep("Mp5", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp5", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p6 <- phyper(length(grep("Mp6",Stress_Exclusive$V1))-1, length(grep("Mp6", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp6", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p7 <- phyper(length(grep("Mp7",Stress_Exclusive$V1))-1, length(grep("Mp7", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp7", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p8 <- phyper(length(grep("Mp8",Stress_Exclusive$V1))-1, length(grep("Mp8", ProteomeUniverse)), length(ProteomeUniverse) - length(grep("Mp8", ProteomeUniverse)),  length(Stress_Exclusive$V1), lower.tail = F)
p.adjust(c(pU,pV, p1, p2, p3, p4, p5, p6, p7, p8), method = "bonferroni")

#Plot results only for Genome Universe in a heatmap fashion. Proteome Universe is too much biased in order see enrichments with these sets

Genomic <- data.frame( -log10(AllStress),
                       -log10(AllSex.p),
                       -log10(AllFemale.p),
                       -log10(AllMale.p),
                       -log10(Female_NoStress_Exclusive.p),
                       -log10(Female_Stress_Exclusive.p),
                       -log10(Male_NoStress_Exclusive.p),
                       -log10(Male_Stress_Exclusive.p),
                       -log10(Stress_Exclusive.p))

rownames(Genomic) <- c("chrU", "chrV", "chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", "chr8")
colnames(Genomic) <- c("AllStress", "AllSex", "AllFemale", "AllMale", "ExclusiveFemaleNoStress", "ExclusiveFemaleStress", "ExclusiveMaleNoStress", "ExclusiveMaleStress", "ExclusiveStress")
require(RColorBrewer)
col <- colorRampPalette(brewer.pal(9, "Greens"))(250)
my_col <- data.frame(chr = c(rep("Sexual", 2), rep ("Autosomic", 8)))
rownames(my_col) <- rownames(Genomic[-1])
pdf(file = "../Genomics/ProteinSets_chrEnrichment.pdf", paper = "a4r", height = 51, width = 58, onefile = T)
pheatmap(t(Genomic[,-1]), scale = "none", color = col,  cluster_rows = F, cluster_cols = F,  clustering_method = "ward.D2", cellwidth = 75, angle_col = 0, annotation_col = my_col)
dev.off()

write.table(Genomic, file = "../Genomics/log10adjpval.txt", col.names = T, row.names = T, sep = "\t", quote = F)

##########################################EVOLUTIONARY PROTEOMICS########################################

##Source: https://drostlab.github.io/myTAI/articles/Introduction.html

##Load packages and format data

library(myTAI)
library(limma)

genAges <- read.delim("clipboard", header = T)
AbundanceData <- as.data.frame(AllProteins)
AbundanceData$Phylostratum <- rep(NA, nrow(AbundanceData))
AbundanceData$GeneID <- rownames(AbundanceData)

for(i in 1:nrow(AbundanceData)){
  AbundanceData$Phylostratum[i] <- genAges$rank[which(genAges$X.gene == AbundanceData$GeneID[i])] 
}
AbundanceData <- AbundanceData[!is.na(AbundanceData$Phylostratum),]
write.table(AbundanceData, "D:/DSMarchantia_mofa2/EvolutionaryProteomics/AbundanceData.txt", col.names = T, sep = "\t", quote = F, row.names = F)

AbundanceData_F <- data.frame(Phylostratum = AbundanceData$Phylostratum, GeneID = AbundanceData$GeneID, 
                              Female_T0 = rowMeans(AbundanceData[,1:5]), Male_T0 = rowMeans(AbundanceData[,6:10]),
                              Female_T1 = rowMeans(AbundanceData[,11:15]), Male_T1 = rowMeans(AbundanceData[,16:20]),
                              Female_T2 = rowMeans(AbundanceData[,21:25]), Male_T2 = rowMeans(AbundanceData[,26:30]))

##Check format
is.ExpressionSet(AbundanceData_F)
write.table(AbundanceData_F, "D:/DSMarchantia_mofa2/EvolutionaryProteomics/AbundanceData_F.txt", col.names = T, row.names = F, sep = "\t", quote = F)

##Data transformations

AbundanceData_StressxSex_absolute <- AbundanceData_F
AbundanceData_StressxSex_scale <- AbundanceData_StressxSex_absolute
AbundanceData_StressxSex_scale[,3:8] <- scale(AbundanceData_StressxSex_scale[,3:8], center = T, scale = T)
AbundanceData_StressxSex_log10 <- AbundanceData_StressxSex_absolute
AbundanceData_StressxSex_log10[,3:8] <- log10(AbundanceData_StressxSex_log10[,3:8] + 1)
AbundanceData_StressxSex_quantile <- AbundanceData_StressxSex_absolute
AbundanceData_StressxSex_quantile[,3:8] <- normalizeQuantiles(as.matrix(AbundanceData_StressxSex_quantile[,3:8]), ties = T)
AbundanceData_SexxStress_absolute <- AbundanceData_F[,c(1,2,3,5,7,4,6,8)]
AbundanceData_SexxStress_quantile <- AbundanceData_SexxStress_absolute
AbundanceData_SexxStress_quantile[,3:8] <- normalizeQuantiles(as.matrix(AbundanceData_SexxStress_quantile[,3:8]), ties = T)
AbundanceData_SexxStress_log10 <- AbundanceData_SexxStress_absolute
AbundanceData_SexxStress_log10[,3:8] <- log10(AbundanceData_SexxStress_absolute[,3:8] + 1)
EvoProteomicsList <- list(AbundanceData_StressxSex_absolute = AbundanceData_StressxSex_absolute,
                          AbundanceData_StressxSex_quantile = AbundanceData_StressxSex_quantile,
                          AbundanceData_StressxSex_log10 = AbundanceData_StressxSex_log10,
                          AbundanceData_SexxStress_absolute = AbundanceData_SexxStress_absolute,
                          AbundanceData_SexxStress_quantile = AbundanceData_SexxStress_quantile,
                          AbundanceData_SexxStress_log10 = AbundanceData_SexxStress_log10)

lapply(EvoProteomicsList, is.ExpressionSet) # allTRUE

##TAI Analyses

EvoProteomicsList_Factors <- lapply(EvoProteomicsList, function(x){
  x$Phylostratum <- gsub(x = EvoProteomicsList$AbundanceData_StressxSex_absolute_Factors$Phylostratum, pattern = "11", replacement = "7")
  x$Phylostratum <- gsub(x = EvoProteomicsList$AbundanceData_StressxSex_absolute_Factors$Phylostratum, pattern = "12", replacement = "8")
  x$Phylostratum <- as.numeric(x$Phylostratum)
  x
})

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_absolute,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_absolute_Factors,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_quantile,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_log10,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_SexxStress_absolute,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_SexxStress_quantile,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_SexxStress_log10,
               measure       = "TAI", 
               TestStatistic = "FlatLineTest",
               xlab          = "Ontogeny", 
               ylab          = "TAI" , permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_absolute,
               measure       = "TAI",
               TestStatistic = "ReductiveHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_quantile,
               measure       = "TAI",
               TestStatistic = "ReductiveHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_log10,
               measure       = "TAI",
               TestStatistic = "ReductiveHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_absolute,
               measure       = "TAI",
               TestStatistic = "EarlyConservationTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_quantile,
               measure       = "TAI",
               TestStatistic = "EarlyConservationTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_log10,
               measure       = "TAI",
               TestStatistic = "EarlyConservationTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" , shaded.area = T, permutations = 10000)

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_absolute,
               measure       = "TAI",
               TestStatistic = "ReverseHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" )

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_quantile,
               measure       = "TAI",
               TestStatistic = "ReverseHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" )

PlotSignature( ExpressionSet = EvoProteomicsList$AbundanceData_StressxSex_log10,
               measure       = "TAI",
               TestStatistic = "ReverseHourglassTest",
               modules       = list(early = 1:2, mid = 3:4, late = 5:6),
               xlab          = "Ontogeny", 
               ylab          = "TAI" )

##Export tables
write.table(TAI(EvoProteomicsList$AbundanceData_StressxSex_quantile), file = "D:/DSMarchantia_mofa2/EvolutionaryProteomics/TAI_StressxSex_quantile.txt", quote = F, row.names = T, col.names = T, sep = "\t")
write.table(pTAI(EvoProteomicsList$AbundanceData_StressxSex_quantile), file = "D:/DSMarchantia_mofa2/EvolutionaryProteomics/pTAI_StressxSex_quantile.txt", quote = F, row.names = T, col.names = T, sep = "\t") 
write.table(pStrata(EvoProteomicsList$AbundanceData_StressxSex_quantile),  file = "D:/DSMarchantia_mofa2/EvolutionaryProteomics/pStrata_StressxSex_quantile.txt", quote = F, row.names = T, col.names = T, sep = "\t") 
write.table(pMatrix(EvoProteomicsList$AbundanceData_SexxStress_quantile),   file = "D:/DSMarchantia_mofa2/EvolutionaryProteomics/pMatrix_StressxSex_quantile.txt", quote = F, row.names = T, col.names = T, sep = "\t") 

##Plots of interest

PlotDistribution( PhyloExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile, 
                  xlab               = "Phylostratum" )

PlotDistribution( PhyloExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile, 
                  as.ratio           = TRUE, 
                  xlab               = "Phylostratum")

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = T)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = T)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F)

PlotMeans(EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
          Groups = list(c(1),c(2,3,4,5,6,7,8)), 
          legendName = "PS",
          adjust.range = TRUE)

PlotContribution( ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                  legendName    = "PS",
                  xlab           = "Ontogeny",
                  ylab           = "Transcriptome Age Index",
                  y.ticks        = 10)

PlotRE(EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
       Groups = list(c(1,3,8,5), c(2,4,7)), 
       legendName = "PS",
       adjust.range = TRUE)

PlotRE(EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
       Groups = list(c(1,2,3,4,5,6,7,8)), 
       legendName = "PS",
       adjust.range = TRUE)

PlotBarRE( ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile, 
           Groups        = list(group_1 = 1:2, group_2 = 3:8), 
           xlab          = "Ontogeny", 
           ylab          = "Mean Relative Expression", 
           cex           = 1.5, ratio = T)

PlotBarRE( ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile, 
           Groups        = list(group_1 = c(1,3,5,8), group_2 = c(2,4,7)), 
           xlab          = "Ontogeny", 
           ylab          = "Mean Relative Expression", 
           cex           = 1.5, ratio = T)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$NoRegulation)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$NoRegulation)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllSex)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllSex)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllStress)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllStress)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllFemale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllFemale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllFemale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllFemale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllMale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$AllMale)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_NoStress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_Stress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Female_Stress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_Stress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Male_Stress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Stress_Exclusive)

PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Supervised.v3$Stress_Exclusive)

##No supervised plots of interest
PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "category-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Unsupervised.v3$Kmeans1)
PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_quantile,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Unsupervised.v3$Kmeans15)
PlotCategoryExpr(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_StressxSex_log10,
                 legendName    = "PS",
                 test.stat     = TRUE,
                 type          = "stage-centered",
                 distr.type    = "boxplot",
                 log.expr      = F,
                 gene.set      = Unsupervised.v3$Kmeans14)


##Enrichments

pdf("D:/DSMarchantia_mofa2/EvolutionaryProteomics/Enrichments_SupervisedSets.pdf")
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$AllFemale, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$AllMale, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$Female_NoStress_Exclusive, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$Male_NoStress_Exclusive, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$Female_Stress_Exclusive, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$Male_Stress_Exclusive, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$Stress_Exclusive, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Supervised.v3$NoRegulation, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
dev.off()

##Non supervised enrichments

pdf("D:/DSMarchantia_mofa2/EvolutionaryProteomics/Enrichments_UnsupervisedSets.pdf")
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans1, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans2, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans3, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans4, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans5, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans6, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans7, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans8, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans9, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans10, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans11, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans12, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans13, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
print(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans14, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
prin(PlotEnrichment(ExpressionSet = EvoProteomicsList_Factors$AbundanceData_SexxStress_quantile, test.set = Unsupervised.v3$Kmeans15, use.only.map = F, measure = "log-foldchange", complete.bg = T, epsilon = 0.1, plot.bars = T))
dev.off()

###################################GENE QUANTIFICATION PLOTS##########################################3
##Load libraries

library (readxl)
library (ggplot2)
library(viridis) 
library(dplyr)
library(plyr)
library(ggthemes)

##Load data (Three columns:  Accession - Treatment - Numeric value)

##Fv/Fm (fvfm)
fvfmforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##PAO (pao)
paoforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##PP5 (pp5)
pp5forplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##GST (gst)
gstforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##ClpC (clpc)
clpcforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##RuBisCO (rubisco)
rubiscoforplot <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")

##Fv/Fm barplot (fvfm)
fvfm_info <- ddply(fvfmforplot, c("Treatment", "Accession"), summarise,
                   N = length(fvfm),
                   mean = mean(fvfm),
                   sd = sd(fvfm),
                   se = sd/sqrt(N))
fvfm_barplot <- ggplot(data=fvfm_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
fvfm_barplot

##PAO barplot (pao)
pao_info <- ddply(paoforplot, c("Treatment", "Accession"), summarise,
                   N = length(pao),
                   mean = mean(pao),
                   sd = sd(pao),
                   se = sd/sqrt(N))
pao_barplot <- ggplot(data=pao_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
pao_barplot

##PP5 barplot (pp5)
pp5_info <- ddply(pp5forplot, c("Treatment", "Accession"), summarise,
                   N = length(pp5),
                   mean = mean(pp5),
                   sd = sd(pp5),
                   se = sd/sqrt(N))
pp5_barplot <- ggplot(data=pp5_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
pp5_barplot

##GST barplot (gst)
gst_info <- ddply(gstforplot, c("Treatment", "Accession"), summarise,
                   N = length(gst),
                   mean = mean(gst),
                   sd = sd(gst),
                   se = sd/sqrt(N))
gst_barplot <- ggplot(data=gst_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
gst_barplot

##ClpC barplot (clpc)
clpc_info <- ddply(clpcforplot, c("Treatment", "Accession"), summarise,
                   N = length(clpc),
                   mean = mean(clpc),
                   sd = sd(clpc),
                   se = sd/sqrt(N))
clpc_barplot <- ggplot(data=clpc_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
clpc_barplot

##RuBisCO barplot (rubisco)
rubisco_info <- ddply(rubiscoforplot, c("Treatment", "Accession"), summarise,
                   N = length(rubisco),
                   mean = mean(rubisco),
                   sd = sd(rubisco),
                   se = sd/sqrt(N))
rubisco_barplot <- ggplot(data=rubisco_info, aes(x=Accession, y=mean, fill=Treatment)) + geom_bar(position = 'dodge', stat='identity') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + geom_text(aes(label=""), position=position_dodge(width=0.9), vjust=-0.25) + scale_fill_viridis(discrete=TRUE) + scale_color_viridis (discrete=TRUE, option="D") + geom_point(aes(color = Label), colour = "black", size =1.5, position = position_dodge(0.9)) + theme_bw() +  theme_clean() + theme(legend.key.size = unit(0.5, "cm")) + labs(y = "Fv/Fm", x = "Sexual accession")
rubisco_barplot

##Statistics

##Load libraries

library(agricolae)
library(mblm)
library(nlme)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(scales)
library (readxl)

##Load data (Two columns:  Accession and Treatment - Numeric value)

##Fv/Fm (fvfm)
fvfmforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##PAO (pao)
paoforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##PP5 (pp5)
pp5forstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##GST (gst)
gstforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##ClpC (clpc)
clpcforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")
##RuBisCO (rubisco)
rubiscoforstatistics <- read.delim("clipboard", header=T, stringsAsFactors = F, dec = ".")

##Statistics performing

##Fv/Fm (fvfm)
kruskal_result_fvfm <-kruskal(fvfmforstatistics$fvfm, fvfmforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_fvfm

##PAO (pao)
kruskal_result_pao <-kruskal(paoforstatistics$pao, paoforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_pao

##PP5 (pp5)
kruskal_result_pp5 <-kruskal(pp5forstatistics$pp5, pp5forstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_pp5

##GST (gst)
kruskal_result_gst <-kruskal(gstforstatistics$gst, gstforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_gst

##ClpC (clpc)
kruskal_result_clpc <-kruskal(clpcforstatistics$clpc, clpcforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_clpc

##RuBisCO (rubisco)
kruskal_result_rubisco <-kruskal(rubiscoforstatistics$rubisco, rubiscoforstatistics$Treatment, alpha = 0.05, p.adj=c("none"), group=TRUE, main = NULL,console=TRUE)
kruskal_result_rubisco
