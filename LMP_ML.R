setwd("~/R/LMP_ML/")


options("scipen"=5, "digits"=2)




library(factoextra)
library(NbClust)
library(corrplot)
library(readxl)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(liver)
library(reshape2)
library(car)
library(mia)
library(ggthemes)
library(vegan)
library(phyloseq)
library(ANCOMBC)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(rcompanion) 
library(GUniFrac)
library(gtsummary)
library(gt)
library(rstatix)


metadata <- read_excel("metadata.xlsx")

nutrients <- read_excel("nutrients_v2.xlsx")

VIGDB <- read.delim("~/R/LMP_alco/final.txt")
VIGDB <- VIGDB[, -which(names(VIGDB) == "Age" )]


VIGDB$Participant <- rownames(VIGDB)


#Drop participant with low kraken and mpa read count
nutrients <- nutrients[!(nutrients$Participant=="LM0265"),]

#Drop participant with mising metagenome sample
nutrients <- nutrients[!(nutrients$Participant=="LM0019"),]

#Drop participant with uncompleted food records
nutrients <- nutrients[!(nutrients$Participant=="LM0014"),]
nutrients <- nutrients[!(nutrients$Participant=="LM0108"),]
nutrients <- nutrients[!(nutrients$Participant=="LM0114"),]




nutrients$total_kcal_group_1 <- nutrients$Carbs1*4+nutrients$Prot1*4+nutrients$Fat1*9
nutrients$total_kcal_group_2 <- nutrients$Carbs2*4+nutrients$Prot2*4+nutrients$Fat2*9
nutrients$total_kcal_group_3 <- nutrients$Carbs3*4+nutrients$Prot3*4+nutrients$Fat3*9
nutrients$total_kcal_group_4 <- nutrients$Carbs4*4+nutrients$Prot4*4+nutrients$Fat4*9
nutrients$total_kcal_group_5 <- nutrients$Carbs5*4+nutrients$Prot5*4+nutrients$Fat5*9
nutrients$total_kcal_group_6 <- nutrients$Carbs6*4+nutrients$Prot6*4+nutrients$Fat6*9
nutrients$total_kcal_group_7 <- nutrients$Carbs7*4+nutrients$Prot7*4+nutrients$Fat7*9
nutrients$total_kcal_group_8 <- nutrients$Carbs8*4+nutrients$Prot8*4+nutrients$Fat8*9
nutrients$total_kcal_group_9 <- nutrients$Carbs9*4+nutrients$Prot9*4+nutrients$Fat9*9
nutrients$total_kcal_group_10 <- nutrients$Carbs10*4+nutrients$Prot10*4+nutrients$Fat10*9
nutrients$total_kcal_group_11 <- nutrients$Carbs11*4+nutrients$Prot11*4+nutrients$Fat11*9
nutrients$total_kcal_group_12 <- nutrients$Carbs12*4+nutrients$Prot12*4+nutrients$Fat12*9
nutrients$total_kcal_group_13 <- nutrients$Carbs13*4+nutrients$Prot13*4+nutrients$Fat13*9
nutrients$total_kcal_group_14 <- nutrients$Carbs14*4+nutrients$Prot14*4+nutrients$Fat14*9
nutrients$total_kcal_group_15 <- nutrients$Carbs15*4+nutrients$Prot15*4+nutrients$Fat15*9
nutrients$total_kcal_group_16 <- nutrients$Carbs16*4+nutrients$Prot16*4+nutrients$Fat16*9
nutrients$total_kcal_group_17 <- nutrients$Carbs17*4+nutrients$Prot17*4+nutrients$Fat17*9
nutrients$total_kcal_group_18 <- nutrients$Carbs18*4+nutrients$Prot18*4+nutrients$Fat18*9
nutrients$total_kcal_group_19 <- nutrients$Carbs19*4+nutrients$Prot19*4+nutrients$Fat19*9


nutrients$total_carbohydrates <- nutrients$Carbs1+nutrients$Carbs2+nutrients$Carbs3+nutrients$Carbs4+nutrients$Carbs5+nutrients$Carbs6+nutrients$Carbs7+
  nutrients$Carbs8+nutrients$Carbs9+nutrients$Carbs10+nutrients$Carbs11+nutrients$Carbs12+nutrients$Carbs13+nutrients$Carbs14+nutrients$Carbs15+
  nutrients$Carbs16+nutrients$Carbs17+nutrients$Carbs18+nutrients$Carbs19

nutrients$total_sugar <- nutrients$Sugars1+nutrients$Sugars2+nutrients$Sugars3+nutrients$Sugars4+nutrients$Sugars5+nutrients$Sugars6+nutrients$Sugars7+
  nutrients$Sugars8+nutrients$Sugars9+nutrients$Sugars10+nutrients$Sugars11+nutrients$Sugars12+nutrients$Sugars13+nutrients$Sugars14+nutrients$Sugars15+
  nutrients$Sugars16+nutrients$Sugars17+nutrients$Sugars18+nutrients$Sugars19

nutrients$protein <- nutrients$Prot1+nutrients$Prot2+nutrients$Prot3+nutrients$Prot4+nutrients$Prot5+nutrients$Prot6+nutrients$Prot7+
  nutrients$Prot8+nutrients$Prot9+nutrients$Prot10+nutrients$Prot11+nutrients$Prot12+nutrients$Prot13+nutrients$Prot14+nutrients$Prot15+
  nutrients$Prot16+nutrients$Prot17+nutrients$Prot18+nutrients$Prot19

nutrients$total_fat <- nutrients$Fat1+nutrients$Fat2+nutrients$Fat3+nutrients$Fat4+nutrients$Fat5+nutrients$Fat6+nutrients$Fat7+
  nutrients$Fat8+nutrients$Fat9+nutrients$Fat10+nutrients$Fat11+nutrients$Fat12+nutrients$Fat13+nutrients$Fat14+nutrients$Fat15+
  nutrients$Fat16+nutrients$Fat17+nutrients$Fat18+nutrients$Fat19

nutrients$saturated_fat <- nutrients$Saturated1+nutrients$Saturated2+nutrients$Saturated3+nutrients$Saturated4+nutrients$Saturated5+nutrients$Saturated6+nutrients$Saturated7+
  nutrients$Saturated8+nutrients$Saturated9+nutrients$Saturated10+nutrients$Saturated11+nutrients$Saturated12+nutrients$Saturated13+nutrients$Saturated14+nutrients$Saturated15+
  nutrients$Saturated16+nutrients$Saturated17+nutrients$Saturated18+nutrients$Saturated19

nutrients$fiber <- nutrients$Fiber1+nutrients$Fiber2+nutrients$Fiber3+nutrients$Fiber4+nutrients$Fiber5+nutrients$Fiber6+nutrients$Fiber7+
  nutrients$Fiber8+nutrients$Fiber9+nutrients$Fiber10+nutrients$Fiber11+nutrients$Fiber12+nutrients$Fiber13+nutrients$Fiber14+nutrients$Fiber15+
  nutrients$Fiber16+nutrients$Fiber17+nutrients$Fiber18+nutrients$Fiber19


# Grains consumed: Starch from grains + starch from tubers (e.g. potatoes) / 30 (defined portion of grains)
nutrients$grain_portions_consumed <- ((nutrients$Carbs1-nutrients$Sugars1) + (nutrients$Carbs2-nutrients$Sugars2))/30

# Nuts_seeds_legumes consumed : Protein from nuts_seed_legumes / 7 (defined portion of nuts_seed_legumes)
nutrients$nuts_seeds_legumes_portions_consumed <- nutrients$Prot3 / 7

# Milk products consumed : Protein from milk products / 7  (defined portion of milk products)
nutrients$milk_product_portions_consumed <- nutrients$Prot4 / 7

# seafood portion : Protein from seafood / 14 (defined portion of seafood)
nutrients$seafood_portions_consumed <- nutrients$Prot6 / 14

# meat_egg portion : Protein from meat products and eggs / 14  (defined portion of meat products/eggs)
nutrients$meat_egg_portions_consumed <- (nutrients$Prot5+nutrients$Prot7) / 14

# Vegetabke/fruit portion : Total vegetables of fruits consumed in grams / 80  (defined portion of vegetables/fruits)

nutrients$fruit_vegetable_portions_consumed <- (nutrients$Vegetables_grams + nutrients$Fruits_grams) / 80


final <- dplyr::inner_join(nutrients,metadata, by="Participant")

final <- dplyr::inner_join(final,VIGDB, by="Participant")



# Remove children and those who are older than 64

final <- final[which(final$Age >= 18 & final$Age <= 64),]


final <- final %>%
  mutate(agesplit=case_when(
    (between(final$Age, 18, 30) ~ "18-30"),
    (between(final$Age, 31, 50) ~ "31-50"),
    (between(final$Age, 51, 64) ~ "51-64")))



final <- final %>%
  mutate(bmisplit=case_when(
    (between(final$BMI, 18.5, 25) ~ "Normal"),
    (between(final$BMI, 0, 18.5) ~ "Underweight"),
    (between(final$BMI, 25, 30) ~ "Overweight"),
    (between(final$BMI, 30, 100) ~ "Obese"),
  ))





final <- final %>%
  mutate(bmisplit=case_when(
    (between(final$BMI, 18.5, 25) ~ "Normal"),
    (between(final$BMI, 0, 18.5) ~ "Underweight"),
    (between(final$BMI, 25, 30) ~ "Overweight"),
    (between(final$BMI, 30, 100) ~ "Obese"),
  ))



rownames(final) <- final$Participant


final <- final[!(final$bmisplit=="Obese"),]
final <- final[!(final$bmisplit=="Underweight"),]




#column with haris-benedict BMR


#RMR males: (9.65 × weight in kg) + (573 × height in m) − (5.08 × age in years) + 260
#RMR females: (7.38 × weight in kg) + (607 × height in m) − (2.31 × age in years) + 43

final <- final %>%
  mutate(HB_BMR=case_when(
    (`S`== "V") ~ 260 + (9.65 * final$`Ideal weight` ) + (5.73 * as.numeric(final$`Height, cm`)) - (5.08 * final$Age), #For men
    (`S`== "S") ~ 43 + (7.38 * final$`Ideal weight` ) + (6.07  * as.numeric(final$`Height, cm`)) - (2.31 * final$Age))) #For women


final$HB_total <- final$HB_BMR * 1.35

final$Grains_need <- final$HB_total/(2000/6) #1 grain portion = 30 g starch consumed with grain products
final$Nuts_seeds_legumes_need <- (final$HB_total/(2000/5))/7 #1 nut/seed/legume portion = 7 g of protein consumed with nuts/seeds/legumes
final$Milk_products_need <- final$HB_total/(2000/3) #1 milk product portion = 7 g of protein consumed with milk products
final$Seafood_need <- (final$HB_total/(2000/3))/7 #1 seafood portion = 14 g of protein consumed with seafood
final$Meat_eggs_need <- (final$HB_total/(2000/9))/7 #1 meat/egg portion = 14 g of protein consumed with meat/eggs
final$Vegetables_fruits_need <- final$HB_total/(2000/5) #1 vegetable/fruit portion = 80 g of non-dried fruits/vegetables




final$Grains_ratio <- final$grain_portions_consumed/final$Grains_need
final$Nuts_seeds_legumes_ratio <- final$nuts_seeds_legumes_portions_consumed/final$Nuts_seeds_legumes_need
final$Milk_products_ratio <- final$milk_product_portions_consumed/final$Milk_products_need
final$Seafood_ratio <- final$seafood_portions_consumed/final$Seafood_need
final$Meat_eggs_ratio <- final$meat_egg_portions_consumed/final$Meat_eggs_need
final$Vegetables_fruits_ratio <- final$fruit_vegetable_portions_consumed/final$Vegetables_fruits_need


final <- final[!(is.na(final$Participant)), ]

rownames(final) <- final$Participant



#df with nutrients per ideal weight + food groups

nonnormalizednutrientsandfoodgroups <- as.data.frame(cbind(final$Participant,
                                                           final$Grains_ratio,
                                                           final$Nuts_seeds_legumes_ratio,
                                                           final$Milk_products_ratio,
                                                           final$Seafood_ratio,
                                                           final$Meat_eggs_ratio,
                                                           final$Vegetables_fruits_ratio
                                                           ))


#colnames(nonnormalizednutrientsandfoodgroups) <- c("Participant","Grains_ratio","Nuts_seeds_legumes_ratio","Milk_products_ratio","Seafood_ratio","Meat_eggs_ratio","Vegetables_fruits_ratio")
colnames(nonnormalizednutrientsandfoodgroups) <- c("Participant","Graudaugu_attiecība","Riekstu_sēklu_un_pākšaugu_attiecība","Piena_produktu_attiecība","Jūras_produktu_attiecība","Gaļas_produktu_un_olu_attiecība","Dārzeņu_un_augļu_attiecība")



nonnormalizednutrientsandfoodgroups <- data.frame(nonnormalizednutrientsandfoodgroups, row.names = 1)


nonnormalizednutrientsandfoodgroups <- na.omit(nonnormalizednutrientsandfoodgroups)

#logfoodgroups <- lapply(nonnormalizednutrientsandfoodgroups, function(x) sqrt(as.numeric(as.character(x))) )
#logfoodgroups

normalizednutrientsandfoodgroups <- liver::transform(nonnormalizednutrientsandfoodgroups, method = "zscore", columns = NULL, na.rm = TRUE )




# Fitting K-Means clustering Model to training dataset

set.seed(711) # Setting seed

kmeans_nonnormalizednutrients <- kmeans(normalizednutrientsandfoodgroups, centers = 2,iter.max = 10000, nstart = 1000)


#PCA

pca_zscorenormalizednutrients <- prcomp(normalizednutrientsandfoodgroups, scale = TRUE)


pca_minmaxnormalizednutrients1 <- fviz_pca_biplot(pca_zscorenormalizednutrients, # Color by the quality of representation
                                               habillage=kmeans_nonnormalizednutrients[["cluster"]],
                                               col.var="black",
                                               addEllipses=TRUE,
                                               title = NULL,
                                               label="var",
                                               labelsize = 5,
                                               alpha.ind = 0.6,
                                               ggtheme = theme_minimal(),
                                               pointsize = 4,
                                               repel = TRUE) + # Avoid text overlapping
  scale_fill_manual(values=c("#2a9d8f", "#9e0940"))+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"))



pca_minmaxnormalizednutrients1




cluster <- as.data.frame(kmeans_nonnormalizednutrients[["cluster"]])




colnames(cluster) <- "cluster"


cluster$cluster <- with(cluster, factor(cluster, levels = c('1', '2'), labels = c("Grupa 1", "Grupa 2")))

cluster$Participant <- rownames(cluster)



table(cluster$cluster)


final$Participant <- row.names(final)



final <- dplyr::inner_join(final,cluster, by="Participant")



final$cluster <- as.factor(final$cluster)

row.names(final) <- final$Participant




#Compare clusters

final$Lifestyle_ATM <- with(final, factor(Lifestyle_ATM, levels=c("Pārsvarā sēžu, guļu, pārvietojos ar automašīnu, fiziska slodze ne biežāk kā 1 stundu nedēļā. Fiziski neaktīvs", 
                                                                                                    "Eju kājām iepirkties, pastaigāties, bet fiziska slodze ne biežāk kā 4 reizes nedēļā. Fiziski mazaktīvs", 
                                                                                                    "Strādāju fiziski smagu darbu, bieži braucu ar velosipēdu, skrienu, peldu, sportoju vairāk kā 4 reizes nedēļā. Fiziski aktīvs", 
                                                                                                    "Esmu profesionāls sportists"), 
                                                                                 labels = c("Fiziski neaktīvs", 
                                                                                            "Fiziski mazaktīvs", 
                                                                                            "Fiziski aktīvs", 
                                                                                            "Profesionāls sportists")))

names(final)[names(final) == 'Gender'] <- 'Dzimums'
names(final)[names(final) == 'agesplit'] <- 'Vecuma grupa'
names(final)[names(final) == 'BMI'] <- 'ĶMI'
names(final)[names(final) == 'Lifestyle_ATM'] <- 'Dzīvesveids'
names(final)[names(final) == 'Education'] <- 'Izglitības līmenis'


rstatix::wilcox_test(final,ĶMI~cluster) %>%
  p_round(digits = 2)

rstatix::wilcox_effsize(final,ĶMI~cluster)





tbl_summary <- tbl_summary(
  final,
  include = c(Dzimums,'Vecuma grupa',ĶMI,Dzīvesveids,'Izglitības līmenis'),
  by = cluster, # split table by group
  missing = "no") %>% # don't list missing data separately
  add_n() %>% # add column with total number of non-missing observations
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>% # test for a difference between groups
  modify_header(label = "",p.value = "**P-vērtība**") %>% # update the column header
  bold_labels() 




tbl_summary



tbl_summary%>% 
  as_gt(include = -tab_footnote) %>%  # if using gt, can exclude footnotes this way 
  tab_footnote( # and can modify/add footnotes this way
    footnote = "Pīrsona hī kvadrāta (χ2) tests, Fišera tiešais tests, Manna-Vitnija U-tests",
    locations = cells_column_labels(columns = c(p.value))
  )%>%  # if using gt, can exclude footnotes this way 
  tab_footnote( # and can modify/add footnotes this way
    footnote = "Mediāna, (IQR); N, (%)",
    locations = cells_column_labels(columns = c(stat_1))
  )%>%  # if using gt, can exclude footnotes this way 
  tab_footnote( # and can modify/add footnotes this way
    footnote = "Mediāna, (IQR); N, (%)",
    locations = cells_column_labels(columns = c(stat_2))
  )%>%
  gt::gtsave(filename = "tbl_summary.html")

webshot::webshot("tbl_summary.html", "cohort_description.png",
        vwidth = 800,
        vheight = 700)





final$total_kcal <- final$total_carbohydrates*4+final$protein*4+final$total_fat*9

final$total_kcal_ratio <- final$total_kcal/final$HB_total*100
final$total_carbohydrates_kcal <- final$total_carbohydrates*4/final$HB_total*100
final$total_sugar_kcal <- final$total_sugar*4/final$HB_total*100
final$protein_kcal <- final$protein*4/final$HB_total*100
final$total_fat_kcal <- final$total_fat*9/final$HB_total*100
final$saturated_fat_kcal <- final$saturated_fat*9/final$HB_total*100

row.names(final) <- final$Participant



rstatix::wilcox_test(final,total_kcal_ratio~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,total_kcal_ratio~cluster)

total_kcal_ratio <- ggplot(final,aes(as.factor(x=cluster),y=total_kcal_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 180, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Uzņemtās kkal")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,50,100,150,200,500,1000,1500,2000,2500,3000,3500),limits = c(0, 200))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  ylab("kkal % no IKD")


total_kcal_ratio


rstatix::wilcox_test(final,total_carbohydrates_kcal~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,total_carbohydrates_kcal~cluster)


Total_carbohydrates <- ggplot(final,aes(x=cluster,y=total_carbohydrates_kcal))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 90, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Ogļhidrāti kopā")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("kkal % no IKD")

Total_carbohydrates



rstatix::wilcox_test(final,total_sugar_kcal~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,total_sugar_kcal~cluster)


Sugar <- ggplot(final,aes(x=cluster,y=total_sugar_kcal))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 90, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Cukuri")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("kkal % no IKD")

Sugar


rstatix::wilcox_test(final,protein_kcal~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,protein_kcal~cluster)


Protein <- ggplot(final,aes(x=cluster,y=protein_kcal))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 92.5, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Olbaltumvielas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("kkal % no IKD")

Protein


rstatix::wilcox_test(final,total_fat_kcal~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,total_fat_kcal~cluster)

Fats <- ggplot(final,aes(x=cluster,y=total_fat_kcal))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 92.5, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Tauki")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("kkal % no IKD")

Fats



rstatix::wilcox_test(final,saturated_fat_kcal~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,saturated_fat_kcal~cluster)


Saturated_fats <- ggplot(final,aes(x=cluster,y=saturated_fat_kcal))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 90, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Piesātinātas taukskābes")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=14),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("kkal % no IKD")

Saturated_fats





nutrients_ggplot <- ggarrange(total_kcal_ratio,Total_carbohydrates,Sugar,Protein,Fats,Saturated_fats,nrow = 2,ncol=3,  common.legend = TRUE,legend = "bottom",align = "hv")

nutrients_ggplot <- annotate_figure(nutrients_ggplot,
                                    bottom = text_grob("\nManna-Vitnija U-tests, ns: p > 0.05;  *: p <= 0.05;  **: p <= 0.01", 
                                                       color = "black", vjust = 0.5, x = 0.5, face = "italic", size = 16))

nutrients_ggplot

###############################################################################################################################################################




rstatix::wilcox_test(final,fiber~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,fiber~cluster)


Fiber <- ggplot(final,aes(x=cluster,y=fiber))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 95, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Šķiedrvielas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,25,50,75,100),limits = c(0, 100)) +
  ylab("Daudzums gramos")

Fiber


final <- final %>%
  mutate(fiber_recommended=case_when(
    (between(final$Age,18,30) & final$S == "S") ~ "25",
    (between(final$Age,31,50) & final$S == "S") ~ "25",
    (between(final$Age,51,70) & final$S == "S") ~ "21",
    (between(final$Age,18,30) & final$S == "V") ~ "38",
    (between(final$Age,31,50) & final$S == "V") ~ "38",
    (between(final$Age,51,70) & final$S == "V") ~ "30"
  ))


final$fiber_ratio <- final$fiber/as.numeric(final$fiber_recommended)


row.names(final) <- final$Participant





rstatix::wilcox_test(final,fiber_ratio~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,fiber_ratio~cluster)




Fiber_ratio <- ggplot(final,aes(x=cluster,y=fiber_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox",label="p.signif",  position = "identity", size = 6, label.y = 2.7, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Škiedrvielu attiecība")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 12))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,1,2,3),limits = c(0, 3)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Fiber_ratio



Fibers_ggplot <- ggarrange(Fiber,Fiber_ratio,nrow = 1,ncol=2,  common.legend = TRUE,legend = "bottom",align = "hv")

Fibers_ggplot <- annotate_figure(Fibers_ggplot,
                                    bottom = text_grob("\nManna-Vitnija U-tests, ****: p <= 0.0001", 
                                                       color = "black", vjust = 0.5, x = 0.5, face = "italic", size = 16))

Fibers_ggplot



#################################################################################################################################################



rstatix::wilcox_test(final,Grains_ratio~cluster) %>%
  p_round(digits = 3)

rstatix::wilcox_effsize(final,Grains_ratio~cluster)


Grains_ratio <- ggplot(final,aes(x=cluster,y=Grains_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 1.8, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Graudaugu produkti")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,1,2),limits = c(-0.1, 2))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Grains_ratio


rstatix::wilcox_test(final,Nuts_seeds_legumes_ratio~cluster) %>%
  p_round(digits = 5)

rstatix::wilcox_effsize(final,Nuts_seeds_legumes_ratio~cluster)

Nuts_seeds_legumes_ratio <- ggplot(final,aes(x=cluster,y=Nuts_seeds_legumes_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 7.2, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Rieksti, sēklas, pākšaugi")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,4,8),limits = c(-0.1, 8))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Nuts_seeds_legumes_ratio



rstatix::wilcox_test(final,Milk_products_ratio~cluster) %>%
  p_round(digits = 5)

rstatix::wilcox_effsize(final,Milk_products_ratio~cluster)

Milk_products_ratio <- ggplot(final,aes(x=cluster,y=Milk_products_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 5.7, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Piena produkti")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,3,6,7,8,9,10,15,20),limits = c(-0.1, 6)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Milk_products_ratio


rstatix::wilcox_test(final,Seafood_ratio~cluster) %>%
  p_round(digits = 5)

rstatix::wilcox_effsize(final,Seafood_ratio~cluster)


Seafood_ratio <- ggplot(final,aes(x=cluster,y=Seafood_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 7.2, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Jūras produkti")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,4,8),limits = c(-0.1, 8)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Seafood_ratio




rstatix::wilcox_test(final,Meat_eggs_ratio~cluster) %>%
  p_round(digits = 5)

rstatix::wilcox_effsize(final,Meat_eggs_ratio~cluster)

Meat_eggs_ratio <- ggplot(final,aes(x=cluster,y=Meat_eggs_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 5.4, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Gaļas produkti, olas")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,3,6,7),limits = c(-0.1, 6)) + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Meat_eggs_ratio



rstatix::wilcox_test(final,Vegetables_fruits_ratio~cluster) %>%
  p_round(digits = 5)

rstatix::wilcox_effsize(final,Vegetables_fruits_ratio~cluster)


Vegetables_fruits_ratio <- ggplot(final,aes(x=cluster,y=Vegetables_fruits_ratio))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = cluster))+ 
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(final$cluster))," (n = ",table(final$cluster),")    "))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 3.6, label.x=1.5)+
  theme_hc()+
  scale_fill_hc()+
  ggtitle(label = "Dārzeņi, augļi")+ 
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.text.y = element_text(size = 13),axis.title.y = element_text(size = 14))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,2,4,5,6,7,11),limits = c(-0.1, 4))+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", linewidth=0.6)+
  ylab("Paterētā/ieteicamā attiecība")

Vegetables_fruits_ratio



food_groups_ratio_ggplot <- ggarrange(Grains_ratio, Nuts_seeds_legumes_ratio, Milk_products_ratio,Seafood_ratio,Meat_eggs_ratio,Vegetables_fruits_ratio,nrow = 2,ncol=3,  common.legend = TRUE,legend = "bottom",align = "hv")


food_groups_ratio_ggplot <- annotate_figure(food_groups_ratio_ggplot,
                                 bottom = text_grob("\nManna-Vitnija U-tests, ns: p > 0.05;  *: p < 0.05;  ****: p <= 0.0001", 
                                                    color = "black", vjust = 0.5, x = 0.5, face = "italic", size = 16))

food_groups_ratio_ggplot





# Load files




mpa_counts  <- read.delim("~/R/LMP_ML/mpa/LMP_mpa_abs_species.txt", row.names=1)   # Abundance table (e.g. ASV data; to assay data)

bracken_counts  <- read.delim("~/R/LMP_ML/kraken/bracken_all_species.txt", row.names=1)   # Abundance table (e.g. ASV data; to assay data)



#SRS START


library(SRS)

#SRS.shiny.app(mpa_counts) # 13000000 
#1 sample(s) discarded:  LM0265

#SRS.shiny.app(bracken_counts) #650000
#1 sample(s) discarded:  LM0265

SRS_mpa <- SRS(data = mpa_counts,Cmin = 13000000,set_seed = TRUE,seed = 7111996)

rownames(SRS_mpa) <- rownames(mpa_counts)

SRS_bracken <- SRS(data = bracken_counts,Cmin = 6500000,set_seed = TRUE,seed = 7111996)

rownames(SRS_bracken) <- rownames(bracken_counts)


# Leave only those samples which remains after SRS

mtch <- intersect(colnames(SRS_mpa), colnames(SRS_bracken))


#SRS END




# Match colnames to rownames

mtch1 <- intersect(mtch, rownames(final))


# DROP UNMATCHED FROM SRS-normalized count data for bracken-/mpa-count data and fullmatrix

SRS_mpa <- SRS_mpa[,mtch1 , drop=FALSE]

SRS_bracken <- SRS_bracken[,mtch1 , drop=FALSE]

final <- final[mtch1,,drop=FALSE]


# LOAD TAX FILE FOR TSE


mpa_tax <- read.delim("~/R/LMP_ML/mpa/MPA_taxa.txt")
bracken_tax <- read.delim("~/R/LMP_ML/kraken/Bracken_taxa.txt")



# PREPARE MPA

mpa_new_order = sort(colnames(SRS_mpa))
SRS_mpa <- SRS_mpa[, mpa_new_order]


row.names(final) <- final$Participant

final <- final[order(row.names(final)), ]

row.names(final) <- final$Participant

# Let us ensure that the data is in correct (numeric matrix) format:
SRS_mpa <- as.matrix(SRS_mpa)

# coldata rownames match assay colnames
setdiff(rownames(final),colnames(SRS_mpa))

all(rownames(final) == colnames(SRS_mpa)) # our dataset

class(final) # should be data.frame or DataFrame

# Counts 
class(SRS_mpa) # should be a numeric matrix


# PREPARE BRACKEN


bracken_new_order = sort(colnames(SRS_bracken))
SRS_bracken <- SRS_bracken[, mpa_new_order]


row.names(final) <- final$Participant

final <- final[order(row.names(final)), ]

row.names(final) <- final$Participant

# Let us ensure that the data is in correct (numeric matrix) format:
SRS_bracken <- as.matrix(SRS_bracken)

# coldata rownames match assay colnames
setdiff(rownames(final),colnames(SRS_bracken))

all(rownames(final) == colnames(SRS_bracken)) # our dataset

class(final) # should be data.frame or DataFrame

# Counts 
class(SRS_bracken) # should be a numeric matrix




# TSE 

tse_mpa <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_mpa),
                                    colData = DataFrame(final),
                                    rowData = DataFrame(mpa_tax))


tse_bracken <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_bracken),
                                        colData = DataFrame(final),
                                        rowData = DataFrame(bracken_tax))



#####################################################################################################################################################################



# TSE 


tse_bracken_diet <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_bracken),
                                             colData = DataFrame(final),
                                             rowData = DataFrame(bracken_tax))





# X in TSE
tse_mpa_diet <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_mpa),
                                         colData = DataFrame(final),
                                         rowData = DataFrame(mpa_tax))

tse_mpa_diet_wo_subset <- estimateRichness(tse_mpa_diet)

colData_tse_mpa_diet_wo_subset <- as.data.frame(colData(tse_mpa_diet_wo_subset))


richness_mpa_wo_subset <- ggplot(colData_tse_mpa_diet_wo_subset,aes(x=cluster,y=observed))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_wo_subset$cluster))," (n = ",table(colData_tse_mpa_diet_wo_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Richness before subset")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,150,300,450,600),limits = c(0, 600))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_mpa_wo_subset



tse_mpa_diet_subset <- agglomerateByRank(tse_mpa_diet, rank = "Species") %>%
  transformAssay(assay.type = "counts",
                 method = "relabundance",
                 MARGIN = "samples") %>%
  # subset based on the relative abundance assay		       
  subsetByPrevalentFeatures(detection = 1/10000,
                            prevalence = 5/100, # in at least 5% out of number of included participants
                            assay.type = "relabundance")


tse_mpa_diet_subset <- estimateRichness(tse_mpa_diet_subset)

colData_tse_mpa_diet_subset <- as.data.frame(colData(tse_mpa_diet_subset))

richness_mpa_w_subset <- ggplot(colData_tse_mpa_diet_subset,aes(x=cluster,y=observed))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Richness after subset")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,150,300,450,600),limits = c(0, 600))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_mpa_w_subset


percent_left_mpa_w_subset <- as.data.frame(t(rbind(colSums(tse_mpa_diet_subset@assays@data@listData[["relabundance"]][,]),as.factor(tse_mpa_diet_subset$cluster)))) %>%
  ggplot(.,aes(x=as.factor(V2),y=V1))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(tse_mpa_diet_subset$cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "% after subset")+ 
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(breaks = c(0,0.2,0.4, 0.6,0.8,1),limits = c(0, 1))+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))

percent_left_mpa_w_subset


stats_subset_mpa <- ggarrange(richness_mpa_wo_subset,richness_mpa_w_subset,percent_left_mpa_w_subset,nrow = 1, ncol = 3,  common.legend = TRUE,legend = "bottom",align = "hv")
stats_subset_mpa <- annotate_figure(stats_subset_mpa, top = text_grob("Filtering of rare taxa for mpa\n", color = "black", face = "bold", size = 20),
                                    fig.lab = "",
                                    fig.lab.pos="top.right",
                                    fig.lab.size=16)


stats_subset_mpa




# X in TSE


tse_bracken_diet <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_bracken),
                                             colData = DataFrame(final),
                                             rowData = DataFrame(bracken_tax))


tse_bracken_diet_wo_subset <- estimateRichness(tse_bracken_diet)

colData_tse_bracken_diet_wo_subset <- as.data.frame(colData(tse_bracken_diet_wo_subset))


richness_bracken_wo_subset <- ggplot(colData_tse_bracken_diet_wo_subset,aes(x=cluster,y=observed))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_wo_subset$cluster))," (n = ",table(colData_tse_bracken_diet_wo_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Richness before subset")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,500,1000,1500,2000),limits = c(0, 2000))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_bracken_wo_subset



tse_bracken_diet_subset <- agglomerateByRank(tse_bracken_diet, rank = "Species") %>%
  transformAssay(assay.type = "counts",
                 method = "relabundance",
                 MARGIN = "samples") %>%
  # subset based on the relative abundance assay		       
  subsetByPrevalentFeatures(detection = 1/10000,
                            prevalence = 10/100, # in at least 10% out of number of included participants
                            assay.type = "relabundance")


tse_bracken_diet_subset <- estimateRichness(tse_bracken_diet_subset)

colData_tse_bracken_diet_subset <- as.data.frame(colData(tse_bracken_diet_subset))

richness_bracken_w_subset <- ggplot(colData_tse_bracken_diet_subset,aes(x=cluster,y=observed))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_subset$cluster))," (n = ",table(colData_tse_bracken_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Richness after subset")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,500,1000,1500,2000),limits = c(0, 2000))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_bracken_w_subset


percent_left_bracken_w_subset <- as.data.frame(t(rbind(colSums(tse_bracken_diet_subset@assays@data@listData[["relabundance"]][,]),as.factor(tse_bracken_diet_subset$cluster)))) %>%
  ggplot(.,aes(x=as.factor(V2),y=V1))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(tse_mpa_diet_subset$cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_subset$cluster))," (n = ",table(colData_tse_bracken_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "% after subset")+ 
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(breaks = c(0,0.2,0.4, 0.6,0.8,1),limits = c(0, 1))+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))

percent_left_bracken_w_subset


stats_subset_bracken <- ggarrange(richness_bracken_wo_subset,richness_bracken_w_subset,percent_left_bracken_w_subset,nrow = 1, ncol = 3,  common.legend = TRUE,legend = "bottom",align = "hv")
stats_subset_bracken <- annotate_figure(stats_subset_bracken, top = text_grob("Filtering of rare taxa for bracken\n", color = "black", face = "bold", size = 20),
                                        fig.lab = "",
                                        fig.lab.pos="top.right",
                                        fig.lab.size=14)

stats_subset_bracken

df_lm <- as.data.frame(t(rbind(colSums(tse_mpa_diet_subset@assays@data@listData[["relabundance"]]), colSums(tse_bracken_diet_subset@assays@data@listData[["relabundance"]]))))

colnames(df_lm) <- c("mpa4_subset_5","kraken2_subset_10")


ggplot(df_lm, aes(x = mpa4_subset_5, y = kraken2_subset_10)) +
  geom_point() +
  theme_bw()+
  scale_y_continuous(breaks = c(0,0.2,0.4,0.5, 0.6,0.7,0.8,0.9,1),limits = c(0.35, 1))+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.5, 0.6,0.7,0.8,0.9,1),limits = c(0.35, 1))+
  stat_smooth(method = "lm", se=FALSE) +
  stat_regline_equation(label.x.npc = "center")



#Calculate alphas for mpa

tse_mpa_diet_subset <- estimateDiversity(tse_mpa_diet_subset)

colData_tse_mpa_diet_subset <- as.data.frame(colData(tse_mpa_diet_subset))


#Visualize alphas for mpa

library(rstatix)

rstatix_shannon <- rstatix::wilcox_test(colData_tse_mpa_diet_subset,shannon~cluster)%>%
  p_round(digits = 3)

rstatix_shannon_effect_size <- rstatix::wilcox_effsize(colData_tse_mpa_diet_subset,shannon~cluster)


shannon_mpa <- ggplot(colData_tse_mpa_diet_subset,aes(as.factor(x=cluster),y=shannon))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 5.8, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Šanona indekss")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0:6),limits = c(1, 6))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

shannon_mpa


rstatix_invsimpson <- rstatix::wilcox_test(colData_tse_mpa_diet_subset,inverse_simpson~cluster) %>%
  p_round(digits = 3)

rstatix_invsimpson_effect_size <- rstatix::wilcox_effsize(colData_tse_mpa_diet_subset,inverse_simpson~cluster)


inverse_simpson_mpa <- ggplot(colData_tse_mpa_diet_subset,aes(as.factor(x=cluster),y=inverse_simpson))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 72, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Apgrieztais Simpsona indekss")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,15,30,45,60,75),limits = c(-0.01, 75))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))



inverse_simpson_mpa


diversity_mpa <- ggarrange(shannon_mpa,inverse_simpson_mpa,nrow = 1, ncol = 2,  common.legend = TRUE,legend = "bottom",align = "hv")
diversity_mpa <- annotate_figure(diversity_mpa, 
                                 bottom = text_grob("\nManna-Vitnija U-tests, ns: p > 0.05",
                                                    color = "black", vjust = 0.5, x = 0.5, face = "italic", size = 16))

diversity_mpa



#Calculate alphas for kraken

tse_bracken_diet_subset <- estimateDiversity(tse_bracken_diet_subset)

colData_tse_bracken_diet_subset <- as.data.frame(colData(tse_bracken_diet_subset))


#Visualize alphas for kraken2


rstatix_shannon <- rstatix::wilcox_test(colData_tse_bracken_diet_subset,shannon~cluster)%>%
  p_round(digits = 3)

rstatix_shannon_effect_size <- rstatix::wilcox_effsize(colData_tse_bracken_diet_subset,shannon~cluster)


shannon_bracken <- ggplot(colData_tse_bracken_diet_subset,aes(as.factor(x=cluster),y=shannon))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_subset$cluster))," (n = ",table(colData_tse_bracken_diet_subset$cluster),")"))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 5.8, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Šanona indekss")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0:6),limits = c(1, 6))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))


shannon_bracken



rstatix_invsimpson <- rstatix::wilcox_test(colData_tse_bracken_diet_subset,inverse_simpson~cluster) %>%
  p_round(digits = 3)

rstatix_invsimpson_effect_size <- rstatix::wilcox_effsize(colData_tse_bracken_diet_subset,inverse_simpson~cluster)



inverse_simpson_bracken <- ggplot(colData_tse_bracken_diet_subset,aes(as.factor(x=cluster),y=inverse_simpson))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f", "#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_subset$cluster))," (n = ",table(colData_tse_bracken_diet_subset$cluster),")"))+
  stat_compare_means(method = "wilcox.test",label="p.signif",  position = "identity", size = 6, label.y = 95, label.x=1.5)+
  theme_hc()+
  ggtitle(label = "Apgrieztais Simpsona indekss")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = c(0,20,40,60,80,100),limits = c(-0.01, 100))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))


inverse_simpson_bracken

diversity_bracken <- ggarrange(shannon_bracken,inverse_simpson_bracken,nrow = 1, ncol = 2,  common.legend = TRUE,legend = "bottom",align = "hv")

diversity_bracken <- annotate_figure(diversity_bracken, 
                                     bottom = text_grob("\nManna-Vitnija U-tests, ns: p > 0.05",
                                                        color = "black", vjust = 0.5, x = 0.5, face = "italic", size = 16))
diversity_bracken




#CALCULATE BETA


#makePhyloseqFromTreeSummarizedExperiment for mpa

phyloseq_mpa <- makePhyloseqFromTreeSummarizedExperiment(tse_mpa_diet_subset)


#BC+PCoA ordination

phyloseqin_bc <- microbiome::transform(phyloseq_mpa, "compositional")

ordination_bc <- ordinate(phyloseqin_bc, "PCoA", "bray")

distances_bc_matrix = phyloseq::distance(phyloseqin_bc, method="bray")

#ADONIS test
adonis2_mpa_bray <- adonis2(distances_bc_matrix ~ sample_data(phyloseqin_bc)$cluster,permutations = 9999,set.seed(71196))

adonis2_effect_size <- MicEco::adonis_OmegaSq(adonis2_mpa_bray)


#BETADISPER test
betadisper_mpa_bray <- permutest(betadisper(distances_bc_matrix, phyloseq::sample_data(phyloseqin_bc)$cluster),permutations = 9999)


mpa_bc_plot <- phyloseq::plot_ordination(phyloseqin_bc, ordination_bc, type="samples", color="cluster")+
  ggplot2::geom_point(size=3)+
  ggplot2::stat_ellipse(geom = "polygon", alpha=0.25, aes(fill=cluster))+
  ggplot2::scale_color_manual(values=c("#2a9d8f", "#9e0940"))+
  ggplot2::scale_fill_manual(values=c("#2a9d8f", "#9e0940"))+
  annotate("text", x=0.215, y=0.34, size = 4.5, label= paste("PERMDISP, P =", round(betadisper_mpa_bray[["tab"]][["Pr(>F)"]][[1]],3), "\nPERMANOVA, P =",round(adonis2_mpa_bray[1,5],3)))+
  theme_bw()+
  ggtitle(label = "PCoA ar Breja-Kērtisa nevienlīdzībam")+
  theme(text=element_text(size=13),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank())+
  theme(legend.position="none")+
  theme(legend.title=element_blank())

mpa_bc_plot


#RCLR+RDA(PCA) ordination

phyloseq_mpa <- makePhyloseqFromTreeSummarizedExperiment(tse_mpa_diet_subset)

ps_rclr <- microbiome::transform(phyloseq_mpa, "rclr")

ord_rclr <- phyloseq::ordinate(ps_rclr, "RDA")

rclr_dist_matrix <- phyloseq::distance(ps_rclr, method = "euclidean") 

#ADONIS test
adonis2_mpa_rclr <- vegan::adonis2(rclr_dist_matrix ~ phyloseq::sample_data(ps_rclr)$cluster,permutations = 9999,set.seed(71196))

adonis2_effect_size <- MicEco::adonis_OmegaSq(adonis2_mpa_rclr)


#BETADISPER test
betadisper_mpa_rclr <- permutest(betadisper(rclr_dist_matrix, phyloseq::sample_data(ps_rclr)$cluster),permutations = 9999)

mpa_rclr_plot <- phyloseq::plot_ordination(ps_rclr, ord_rclr, type="samples", color="cluster")+
  ggplot2::geom_point(size=3)+
  ggplot2::stat_ellipse(geom = "polygon", alpha=0.25, aes(fill=cluster))+
  ggplot2::scale_color_manual(values=c("#2a9d8f", "#9e0940"))+
  ggplot2::scale_fill_manual(values=c("#2a9d8f", "#9e0940"))+
  annotate("text", x=2.3, y=4.6, size = 4.5, label= paste("PERMDISP, P =", round(betadisper_mpa_rclr[["tab"]][["Pr(>F)"]][[1]],3), "\nPERMANOVA, P =",round(adonis2_mpa_rclr[1,5],3)))+
  theme_bw()+
  ggtitle(label = "PCA ar rCLR transformētiem Eiklīda attālumiem")+
  theme(text=element_text(size=13),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank())+
  theme(legend.position="none")+
  theme(legend.title=element_blank())


mpa_rclr_plot


mpa_beta_ggplot <- ggarrange(mpa_bc_plot, mpa_rclr_plot,nrow = 1,ncol=2,  common.legend = TRUE,legend = "bottom",align = "hv")

mpa_beta_ggplot <- annotate_figure(mpa_beta_ggplot)

mpa_beta_ggplot





#makePhyloseqFromTreeSummarizedExperiment for bracken

phyloseq_bracken <- makePhyloseqFromTreeSummarizedExperiment(tse_bracken_diet_subset)


#BC+PCoA ordination

phyloseqin_bc <- microbiome::transform(phyloseq_bracken, "compositional")

ordination_bc <- ordinate(phyloseqin_bc, "PCoA", "bray")

distances_bc_matrix = phyloseq::distance(phyloseqin_bc, method="bray")

#ADONIS test
adonis2_bracken_bray <- adonis2(distances_bc_matrix ~ sample_data(phyloseqin_bc)$cluster,permutations = 9999,set.seed(71196))

adonis2_effect_size <- MicEco::adonis_OmegaSq(adonis2_bracken_bray)


#BETADISPER test
betadisper_bracken_bray <- permutest(betadisper(distances_bc_matrix, phyloseq::sample_data(phyloseqin_bc)$cluster),permutations = 9999)


bracken_bc_plot <- phyloseq::plot_ordination(phyloseqin_bc, ordination_bc, type="samples", color="cluster")+
  ggplot2::geom_point(size=3)+
  ggplot2::stat_ellipse(geom = "polygon", alpha=0.25, aes(fill=cluster))+
  ggplot2::scale_color_manual(values=c("#2a9d8f", "#9e0940"))+
  ggplot2::scale_fill_manual(values=c("#2a9d8f", "#9e0940"))+
  annotate("text", x=0.2, y=0.32, size = 4.5, label= paste("PERMDISP, P =", round(betadisper_bracken_bray[["tab"]][["Pr(>F)"]][[1]],3), "\nPERMANOVA, P =",round(adonis2_bracken_bray[1,5],3)))+
  theme_bw()+
  ggtitle(label = "PCoA ar Breja-Kērtisa nevienlīdzībam")+
  theme(text=element_text(size=13),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none")+
  theme(legend.title=element_blank())

bracken_bc_plot


#RCLR+RDA(PCA) ordination

phyloseq_bracken <- makePhyloseqFromTreeSummarizedExperiment(tse_bracken_diet_subset)

ps_rclr <- microbiome::transform(phyloseq_bracken, "rclr")

ord_rclr <- phyloseq::ordinate(ps_rclr, "RDA")

rclr_dist_matrix <- phyloseq::distance(ps_rclr, method = "euclidean") 

#ADONIS test
adonis2_bracken_rclr <- vegan::adonis2(rclr_dist_matrix ~ phyloseq::sample_data(ps_rclr)$cluster,permutations = 9999,set.seed(71196))

adonis2_effect_size <- MicEco::adonis_OmegaSq(adonis2_bracken_rclr)


#BETADISPER test
betadisper_bracken_rclr <- permutest(betadisper(rclr_dist_matrix, phyloseq::sample_data(ps_rclr)$cluster),permutations = 9999)

bracken_rclr_plot <- phyloseq::plot_ordination(ps_rclr, ord_rclr, type="samples", color="cluster")+
  ggplot2::geom_point(size=3)+
  ggplot2::stat_ellipse(geom = "polygon", alpha=0.25, aes(fill=cluster))+
  ggplot2::scale_color_manual(values=c("#2a9d8f", "#9e0940"))+
  ggplot2::scale_fill_manual(values=c("#2a9d8f", "#9e0940"))+
  annotate("text", x=3.45, y=7.75, size = 4.5, label= paste("PERMDISP, P =", round(betadisper_bracken_rclr[["tab"]][["Pr(>F)"]][[1]],3), "\nPERMANOVA, P =",round(adonis2_bracken_rclr[1,5],3)))+
  theme_bw()+
  ggtitle(label = "PCA ar rCLR transformētiem Eiklīda attālumiem")+
  theme(text=element_text(size=13),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none")+
  theme(legend.title=element_blank())


bracken_rclr_plot


bracken_beta_ggplot <- ggarrange(bracken_bc_plot, bracken_rclr_plot,nrow = 1,ncol=2,  common.legend = TRUE,legend = "bottom",align = "hv")

bracken_beta_ggplot <- annotate_figure(bracken_beta_ggplot)

bracken_beta_ggplot




# DAA




output_bracken = ancombc2(data = tse_bracken_diet_subset, assay_name = "counts", tax_level = "Species",
                  fix_formula = "cluster + Gender + agesplit + bmisplit", rand_formula = NULL,
                  p_adj_method = "holm", pseudo_sens = TRUE,
                  s0_perc = 0.05,
                  group = "cluster", struc_zero = TRUE, neg_lb = TRUE,
                  alpha = 0.05, n_cl = 20, verbose = TRUE,
                  global = TRUE, pairwise = TRUE, dunnet = TRUE, trend = TRUE,
                  iter_control = list(tol = 1e-2, max_iter = 20, 
                                      verbose = TRUE),
                  em_control = list(tol = 1e-5, max_iter = 100),
                  lme_control = lme4::lmerControl(),
                  mdfdr_control = list(fwer_ctrl_method = "holm", B = 100),
                  trend_control = list(contrast = list(matrix(c(1, 0, -1, 1),
                                                              nrow = 2, 
                                                              byrow = TRUE),
                                                       matrix(c(-1, 0, 1, -1),
                                                              nrow = 2, 
                                                              byrow = TRUE),
                                                       matrix(c(1, 0, 1, -1),
                                                              nrow = 2, 
                                                              byrow = TRUE)),
                                       node = list(2, 2, 1),
                                       solver = "ECOS",
                                       B = 100))





output_bracken$res







library(benchdamic)






#co-abundance networks


library(SPRING)
library(SpiecEasi)
library(NetCoMi)
library(mia)


mpa_tax_rename_phyla <- mpa_tax

table(mpa_tax_rename_phyla$Phylum)

mpa_tax_rename_phyla$Phylum[mpa_tax_rename_phyla$Phylum != c("p__Firmicutes") & mpa_tax_rename_phyla$Phylum != c("p__Proteobacteria") & mpa_tax_rename_phyla$Phylum != c("p__Actinobacteria") & mpa_tax_rename_phyla$Phylum != c("p__Bacteroidetes") ] <- "p__Cita"


table(mpa_tax_rename_phyla$Phylum)


# X in TSE
tse_mpa_diet_network <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_mpa),
                                         colData = DataFrame(final),
                                         rowData = DataFrame(mpa_tax_rename_phyla))

#clean from rare taxa for mpa

tse_mpa_diet_transformed <- agglomerateByRank(tse_mpa_diet_network, rank = "Species") %>%
  transformAssay(assay.type = "counts",
                 method = "relabundance",
                 MARGIN = "samples") %>%
  # subset based on the relative abundance assay		       
  subsetByPrevalentFeatures(detection = 1/10000,
                            prevalence = 10/100, # at least in 5% out of number of included participants
                            assay.type = "relabundance")



phyloseq_mpa <- makePhyloseqFromTreeSummarizedExperiment(tse_mpa_diet_transformed)

any(duplicated(phyloseq_mpa@tax_table[, "Species"]))

#phyloseq_mpa <- renameTaxa(phyloseq_mpa, pat = "<name>", substPat = "<name>_<subst_name>(<subst_R>)", numDupli = c("Species"))
#any(duplicated(phyloseq_mpa@tax_table[, "Species"]))

cluster_1_mpa <- phyloseq::subset_samples(phyloseq_mpa, cluster == "1")

cluster_2_mpa  <- phyloseq::subset_samples(phyloseq_mpa, cluster == "2")



#colSums(cluster_1_mpa@otu_table)


#n_yes <- phyloseq::nsamples(cluster_2_mpa)



nutrients_mpa <- netConstruct(data = cluster_1_mpa,
                              data2 = cluster_2_mpa,
                              normMethod = "mclr",
                              sparsMethod = "t-test",
                              measure = "spearman",
                              verbose = 3,
                              thresh = 0.6,
                              alpha=0.05,
                              seed=71196)



colSums(as.data.frame(nutrients_mpa[["edgelist1"]][["asso"]]))
1-colSums(as.data.frame(nutrients_mpa[["edgelist1"]][["asso"]])<0)/colSums(as.data.frame(nutrients_mpa[["edgelist1"]][["asso"]])>0)

colSums(as.data.frame(nutrients_mpa[["edgelist2"]][["asso"]]))
1-colSums(as.data.frame(nutrients_mpa[["edgelist2"]][["asso"]])<0)/colSums(as.data.frame(nutrients_mpa[["edgelist2"]][["asso"]])>0)



netprops_nutrients_mpa <- netAnalyze(nutrients_mpa, 
                                     clustMethod = "hierarchical",
                                     hubPar = c("degree",  #The degree centrality denotes the number of adjacent nodes. 
                                                "betweenness",   #The betweenness centrality measures the fraction of times a node lies on the shortest path between all other nodes. 
                                                "closeness"),   #The closeness centrality of a node is the reciprocal of the sum of shortest paths between this node and all other nodes.
                                     normDeg = TRUE,
                                     gcmHeatLCC=FALSE)



# Get phyla names
taxtab <- as(tax_table(phyloseq_mpa), "matrix")
phyla <- as.factor(gsub("p__", "", taxtab[, "Phylum"]))
names(phyla) <- taxtab[, "Species"]
phylcol <- c("#ff4f81","#ff6c5f","#ffc168","#5e4fa2","#1cc7d0")





plot(netprops_nutrients_mpa, 
     sameLayout = TRUE, 
     rmSingles = "inboth",
     layoutGroup = "union",
     featVecCol = phyla,
     colorVec =  phylcol,
     hubBorderCol = "red",
     borderCol = "black",
     nodeColor = "feature",
     charToRm = "s__",
     nodeSize = "mclr", 
     edgeTranspLow = 100, 
     edgeTranspHigh = 25,
     groupNames = c("Zaļie (n = 57) ","Sarkanie (n = 49)"),
     showTitle = TRUE, 
     cexTitle = 1.7,
     cexHubs = 2,
     cexLabels = 0,
     cexHubLabels =0,
     cexNodes = 1,
     mar = c(1,2,3,2), 
     repulsion = 0.85, 
     labels = TRUE, 
     nodeFilter = "none", 
     nodeTransp = 25, 
     hubTransp = 25)




legend(-0.12, 1, cex = 0.8, pt.cex = 1.5, title = "Dzimta:\n", 
       legend=levels(phyla), col = phylcol, bty = "n", pch = 16,y.intersp=0.4,x.intersp=0.5,title.adj = 0.25) 






spring_netcomp_diet_mpa <- netCompare(netprops_nutrients_mpa, 
                                          permTest = TRUE,
                                          nPerm = 200,
                                          cores = 25,
                                          seed = 071196,
                                          storeAssoPerm = TRUE,
                                          fileStoreAssoPerm = "netprops_nutrients_mpa_species",
                                          verbose = TRUE)





summary(spring_netcomp_diet_mpa, 
        groupNames = c("cluster_1", "cluster_2"),
        showCentr = c("degree", "between", "closeness"), 
        numbNodes = 10)





##################################################################################################################################################################################




bracken_tax_rename_phyla <- bracken_tax

table(bracken_tax_rename_phyla$Phylum)

bracken_tax_rename_phyla$Phylum[bracken_tax_rename_phyla$Phylum != c("p__Firmicutes_A") & bracken_tax_rename_phyla$Phylum != c("p__Proteobacteria") & bracken_tax_rename_phyla$Phylum != c("p__Actinobacteriota") & bracken_tax_rename_phyla$Phylum != c("p__Bacteroidota") ] <- "p__Cita"


table(bracken_tax_rename_phyla$Phylum)


# TSE 


tse_bracken_diet <- TreeSummarizedExperiment(assays =  SimpleList(counts = SRS_bracken),
                                        colData = DataFrame(final),
                                        rowData = DataFrame(bracken_tax_rename_phyla))




#clean from rare taxa for mpa


tse_bracken_diet_subset <- agglomerateByRank(tse_bracken_diet, rank = "Species") %>%
  transformAssay(assay.type = "counts",
                 method = "relabundance",
                 MARGIN = "samples") %>%
  # subset based on the relative abundance assay		       
  subsetByPrevalentFeatures(detection = 1/10000,
                            prevalence = 20/100, # in at least 10% out of number of included participants
                            assay.type = "relabundance")







phyloseq_bracken <- makePhyloseqFromTreeSummarizedExperiment(tse_bracken_diet_subset)

any(duplicated(phyloseq_bracken@tax_table[, "Species"]))

#phyloseq_mpa <- renameTaxa(phyloseq_mpa, pat = "<name>", substPat = "<name>_<subst_name>(<subst_R>)", numDupli = c("Species"))
#any(duplicated(phyloseq_mpa@tax_table[, "Species"]))

cluster_1_bracken <- phyloseq::subset_samples(phyloseq_bracken, cluster == "1")
cluster_2_bracken  <- phyloseq::subset_samples(phyloseq_bracken, cluster == "2")



rowSums(cluster_1_bracken@otu_table)

which(rowSums(cluster_1_bracken@otu_table) == 0)


#colSums(cluster_1_mpa@otu_table)


#n_yes <- phyloseq::nsamples(cluster_2_bracken)




nutrients_bracken <- netConstruct(data = cluster_1_bracken,
                              data2 = cluster_2_bracken,
                              normMethod = "mclr",
                              sparsMethod = "t-test",
                              measure = "spearman",
                              verbose = 3,
                              thresh = 0.6,
                              alpha=0.05,
                              seed=71196)





netprops_nutrients_bracken <- netAnalyze(nutrients_bracken, 
                                         clustMethod = "hierarchical",
                                         hubPar = c("degree",  #The degree centrality denotes the number of adjacent nodes. 
                                                    "betweenness",   #The betweenness centrality measures the fraction of times a node lies on the shortest path between all other nodes. 
                                                    "closeness"),   #The closeness centrality of a node is the reciprocal of the sum of shortest paths between this node and all other nodes.
                                         normDeg = TRUE,
                                         gcmHeatLCC=FALSE)



# Get phyla names
taxtab <- as(tax_table(phyloseq_bracken), "matrix")
phyla <- as.factor(gsub("p__", "", taxtab[, "Phylum"]))
names(phyla) <- taxtab[, "Species"]
phylcol <- c("#ff4f81","#ff6c5f","#ffc168","#5e4fa2","#1cc7d0")




plot(netprops_nutrients_bracken, 
     sameLayout = TRUE, 
     rmSingles = "inboth",
     featVecCol = phyla,
     colorVec =  phylcol,
     hubBorderCol = "red",
     borderCol = "black",
     nodeColor = "feature",
     charToRm = "s__",
     nodeSize = "mclr", 
     edgeTranspLow = 100, 
     edgeTranspHigh = 25,
     groupNames = c("Zaļie (n = 57) ","Sarkanie (n = 49)"),
     showTitle = TRUE, 
     cexTitle = 1.7,
     cexHubs = 2,
     cexLabels = 0,
     cexHubLabels =0,
     cexNodes = 1,
     mar = c(1,2,3,2), 
     repulsion = 0.85, 
     labels = TRUE, 
     nodeFilter = "none", 
     nodeTransp = 25, 
     hubTransp = 25)


legend(-0.15, 1, cex = 0.8, pt.cex = 1.5, title = "Dzimta:\n", 
       legend=levels(phyla), col = phylcol, bty = "n", pch = 16,y.intersp=0.45,x.intersp=0.45,title.adj = 0.3) 





plot(netprops_nutrients_bracken, 
     sameLayout = TRUE, 
     featVecCol = phyla,
     colorVec =  phylcol,
     layoutGroup = "union",
     hubBorderCol = "red",
     nodeColor = "feature",
     charToRm = "s__",
     shortenLabels = "simple",
     labelLength = 20,
     nodeSize = "fix", 
     edgeTranspLow = 100, 
     edgeTranspHigh = 85,
     groupNames = c("prudent diet (n = 57) ","Western diet (n = 49)"),
     showTitle = TRUE, 
     cexTitle = 2,
     cexHubs = 8,
     cexLabels = 0,
     cexHubLabels =5,
     cexNodes = 0.75,
     mar = c(1,2,3,2), 
     repulsion = 0.25, 
     labels = TRUE, 
     nodeFilter = "none", 
     nodeTransp = 0, 
     hubTransp = 40)





spring_netcomp_diet_bracken <- netCompare(netprops_nutrients_bracken, 
                                      permTest = TRUE,
                                      nPerm = 200,
                                      cores = 25,
                                      seed = 071196,
                                      storeAssoPerm = TRUE,
                                      fileStoreAssoPerm = "netprops_nutrients_bracken_species",
                                      verbose = TRUE)






summary(spring_netcomp_diet_bracken, 
        groupNames = c("cluster_1", "cluster_2"),
        showCentr = c("degree", "between", "closeness"), 
        numbNodes = 10)







library(phyloseq)
data(Lee)

divnet_species <-  divnet(tax_glom(phyloseq_bracken, taxrank="Species"),
                         X = "cluster",
                         ncores = 4)

divnet_species$`bray-curtis`

plot(divnet_species)






