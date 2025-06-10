





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
  ggtitle(label = "Bagātība pirms filtrēšanas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))+
  scale_y_continuous(breaks = c(0,150,300,450,600),limits = c(0, 600))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_mpa_wo_subset



tse_mpa_diet_subset <- agglomerateByRank(tse_mpa_diet, rank = "Species") %>%
  transformAssay(assay.type = "counts",
                 method = "relabundance",
                 MARGIN = "samples") %>%
  # subset based on the relative abundance assay		       
  subsetByPrevalentFeatures(detection = 1/10000,
                            prevalence = 5/100, # in at least 10% out of number of included participants
                            assay.type = "relabundance")


tse_mpa_diet_subset <- estimateRichness(tse_mpa_diet_subset)

colData_tse_mpa_diet_subset <- as.data.frame(colData(tse_mpa_diet_subset))

richness_mpa_w_subset <- ggplot(colData_tse_mpa_diet_subset,aes(x=cluster,y=observed))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Bagātība pēc filtrēšanas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))+
  scale_y_continuous(breaks = c(0,150,300,450,600),limits = c(0, 600))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_mpa_w_subset


percent_left_mpa_w_subset <- as.data.frame(t(rbind(colSums(tse_mpa_diet_subset@assays@data@listData[["relabundance"]][,]),as.factor(tse_mpa_diet_subset$cluster)))) %>%
  ggplot(.,aes(x=as.factor(V2),y=V1))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(tse_mpa_diet_subset$cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_mpa_diet_subset$cluster))," (n = ",table(colData_tse_mpa_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Atlikušais % pēc filtrēšanas")+ 
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.4, 0.6,0.8,1),limits = c(0, 1))+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))

percent_left_mpa_w_subset


stats_subset_mpa <- ggarrange(richness_mpa_wo_subset,richness_mpa_w_subset,percent_left_mpa_w_subset,nrow = 1, ncol = 3,  common.legend = TRUE,legend = "bottom",align = "hv")
stats_subset_mpa <- annotate_figure(stats_subset_mpa, 
                                  fig.lab.size=14)


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
  ggtitle(label = "Bagātība pirms filtrēšanas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))+
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
  ggtitle(label = "Bagātība pēc filtrēšanas")+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))+
  scale_y_continuous(breaks = c(0,500,1000,1500,2000),limits = c(0, 2000))+
  theme(legend.title=element_blank(),legend.text=element_text(size=18))

richness_bracken_w_subset


percent_left_bracken_w_subset <- as.data.frame(t(rbind(colSums(tse_bracken_diet_subset@assays@data@listData[["relabundance"]][,]),as.factor(tse_bracken_diet_subset$cluster)))) %>%
  ggplot(.,aes(x=as.factor(V2),y=V1))+
  geom_boxplot(outlier.shape = NA,width=0.85)+
  geom_jitter(aes(colour = as.factor(tse_mpa_diet_subset$cluster)), alpha=0.85,width = 0.325)+
  scale_color_manual(values=c("#2a9d8f","#9e0940"),labels = paste0(levels(as.factor(colData_tse_bracken_diet_subset$cluster))," (n = ",table(colData_tse_bracken_diet_subset$cluster),")"))+
  theme_hc()+
  ggtitle(label = "Atlikušais % pēc filtrēšanas")+ 
  guides(colour = guide_legend(override.aes = list(size=4)))+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = element_blank())+ 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.4, 0.6,0.8,1),limits = c(0, 1))+
  theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.y = element_text(size = 16))

percent_left_bracken_w_subset


stats_subset_bracken <- ggarrange(richness_bracken_wo_subset,richness_bracken_w_subset,percent_left_bracken_w_subset,nrow = 1, ncol = 3,  common.legend = TRUE,legend = "bottom",align = "hv")
stats_subset_bracken <- annotate_figure(stats_subset_bracken, 
                                    fig.lab.size=14)


stats_subset_bracken









#df_lm1 <- as.data.frame(t(rbind(colSums(tse_mpa_diet_subset@assays@data@listData[["relabundance"]]), colSums(tse_bracken_diet_subset@assays@data@listData[["relabundance"]]))))

#colnames(df_lm1) <- c("mpa4_subset_5","kraken2_subset_5")



ggplot_lm_1 <- ggplot(df_lm1, aes(x = mpa4_subset_5, y = kraken2_subset_5)) +
  geom_point() +
  theme_bw()+
  stat_smooth(method = "lm", se=FALSE) +
  stat_regline_equation(label.x.npc = 0.15,label.y.npc = "top",size=6.45)+ 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.5, 0.75,1),limits = c(0.45, 1))+
  scale_x_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.5, 0.75,1),limits = c(0.45, 1))+ 
  annotate(geom="text", x=0.475, y=0.975, label="A",size = 10)+
  xlab("Nolasījumu % pēc Metaphlan4 datu filtrācijas") + ylab("Nolasījumu % pēc Kraken2 datu filtrācijas")+
  theme(legend.position="none",axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))




ggplot_lm_1



df_lm2 <- as.data.frame(t(rbind(colSums(tse_mpa_diet_subset@assays@data@listData[["relabundance"]]), colSums(tse_bracken_diet_subset@assays@data@listData[["relabundance"]]))))

colnames(df_lm2) <- c("mpa4_subset_5","kraken2_subset_10")



ggplot_lm_2 <- ggplot(df_lm2, aes(x = mpa4_subset_5, y = kraken2_subset_10)) +
  geom_point() +
  theme_bw()+
  stat_smooth(method = "lm", se=FALSE) +
  stat_regline_equation(label.x.npc = 0.15,label.y.npc = "top",size=6.45)+ 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.5, 0.75,1),limits = c(0.45, 1))+
  scale_x_continuous(labels = function(x) paste0(x*100, "%"),breaks = c(0,0.2,0.5, 0.75,1),limits = c(0.45, 1))+ 
  annotate(geom="text", x=0.475, y=0.975, label="B",size = 10)+
  xlab("Nolasījumu % pēc Metaphlan4 datu filtrācijas") + ylab("Nolasījumu % pēc Kraken2 datu filtrācijas")+
  theme(legend.position="none",axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))



ggplot_lm_2



ggarrange(ggplot_lm_1,ggplot_lm_2)





library(dplyr)
library(tidyr)


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


outliers <- nonnormalizednutrientsandfoodgroups %>% 
  pivot_longer(cols = c(colnames(nonnormalizednutrientsandfoodgroups)), names_to = 'Food_group', values_to = 'ratio') %>%
  mutate(outlier = ifelse(is_outlier(as.numeric(ratio)), round(as.numeric(ratio),1), as.numeric(NA))) %>%
  ggplot(., aes(x = Food_group, y = as.numeric(ratio),fill =Food_group)) +
  geom_boxplot(width=0.6,  outlier.color = "red",outlier.size = 2.5)+
  theme(text=element_text(size=12),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = (element_text(angle=0)))+
  theme_hc()+
  theme(legend.position="bottom",axis.title.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(breaks = c(0:7),limits = c(0, 7))+ 
  scale_fill_manual(values=c("green", "red","yellow","blue","lightblue","brown"))+
  geom_jitter(size=0.75, alpha=0.25,width = 0.25)+
  theme(legend.title=element_blank())+ 
  ylab("Produktu grupu attiecība")+
  theme(legend.title=element_blank(),legend.text=element_text(size=16),axis.title.y = element_text(size = 16))

outliers



outliers_zscore <- normalizednutrientsandfoodgroups %>% 
  pivot_longer(cols = c(colnames(normalizednutrientsandfoodgroups)),
               names_to = 'Food_group', values_to = 'ratio') %>%
  mutate(outlier = ifelse(is_outlier(as.numeric(ratio)), round(as.numeric(ratio),1), as.numeric(NA))) %>%
  ggplot(., aes(x = Food_group, y = as.numeric(ratio),fill =Food_group)) +
  geom_boxplot(width=0.6,  outlier.color = "red",outlier.size = 2.5)+
  theme(text=element_text(size=12),plot.title = element_text(hjust = 0.5),strip.text.x = element_blank(),axis.text.x = (element_text(angle=0)))+
  theme_hc()+
  theme(legend.position="bottom",axis.title.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(breaks = c(-2.5:2.5),limits = c(-2.5, 2.5))+
  scale_fill_manual(values=c("green", "red","yellow","blue","lightblue","brown"))+
  geom_jitter(size=0.75, alpha=0.25,width = 0.25)+
  theme(legend.title=element_blank())+ 
  ylab("z-novērtējuma normalizēta attiecība")+
  theme(legend.title=element_blank(),legend.text=element_text(size=16),axis.title.y = element_text(size = 16))

outliers_zscore

ggsave(file="outliers_zscore.svg", plot=outliers_zscore, width=8, height=4)


library(reshape2)
library(grafify)












kcal_table <- as.data.frame(cbind(final$Participant,
                                  final$cluster,
                                  (final$total_kcal_group_8+final$total_kcal_group_11+final$total_kcal_group_12+
                                     final$total_kcal_group_13+final$total_kcal_group_14+final$total_kcal_group_15+
                                     final$total_kcal_group_16+final$total_kcal_group_17+final$total_kcal_group_18+final$total_kcal_group_19)/final$total_kcal,#Other
                                  (final$total_kcal_group_1+final$total_kcal_group_2)/final$total_kcal, #cereals_grains+tubers
                                  final$total_kcal_group_3/final$total_kcal,#nuts_seeds_legumes
                                  final$total_kcal_group_4/final$total_kcal,#milk_products
                                  (final$total_kcal_group_5+final$total_kcal_group_7)/final$total_kcal,#eggs+meat
                                  (final$total_kcal_group_9+final$total_kcal_group_10)/final$total_kcal,#vegetables+fruits
                                  final$total_kcal_group_6/final$total_kcal))#seafood


colnames(kcal_table) <- c("Participant","cluster","Citas gr.","Graudaugu gr.","Rieksti, sēklas un pākšaugi","Piena produkti","Gaļas produkti un olas","Dārzeņi un augļi","Jūrasprodukti")


kcal_table$cluster <- with(kcal_table, factor(cluster, levels=c("1", "2"), labels = c("Grupa 1 (zaļie)", "Grupa 2 (sarkānie)")))





kcal_ggplot <- melt(kcal_table, id.vars=c("Participant","cluster")) %>%
  ggplot(., aes(fill=variable, y=as.numeric(value), x=Participant)) + 
  geom_bar(stat="identity")+
  theme_hc()+ 
  scale_fill_manual(values=c("purple","yellow", "brown","lightblue","red","green","blue"))+
  facet_wrap(~cluster,scales = "free_x",  labeller = labeller(dose = cluster, supp = supp.labs))+ 
  theme(strip.background =element_rect(fill="white"))+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  ylab("% kkal no kopējā uzņēmta daudzuma")+
  xlab(NULL)+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),axis.text.x = element_blank())+
  theme(legend.title=element_blank())



kcal_ggplot 






#####################################################################################################################################################################








kcal_table <- as.data.frame(cbind(final$Participant,
                                  final$cluster,
                                  (final$total_kcal_group_8+final$total_kcal_group_11+final$total_kcal_group_12+
                                    final$total_kcal_group_13+final$total_kcal_group_14+final$total_kcal_group_15+
                                    final$total_kcal_group_16+final$total_kcal_group_17+final$total_kcal_group_18+final$total_kcal_group_19)/final$HB_total,#Other
                                  (final$total_kcal_group_1+final$total_kcal_group_2)/final$HB_total, #cereals_grains+tubers
                                  final$total_kcal_group_3/final$HB_total,#nuts_seeds_legumes
                                  final$total_kcal_group_4/final$HB_total,#milk_products
                                  (final$total_kcal_group_5+final$total_kcal_group_7)/final$HB_total,#eggs+meat
                                  (final$total_kcal_group_9+final$total_kcal_group_10)/final$HB_total,#vegetables+fruits
                                  final$total_kcal_group_6/final$HB_total))#seafood

colnames(kcal_table) <- c("Participant","cluster","Citas gr.","Graudaugu gr.","Rieksti, sēklas un pākšaugi","Piena produkti","Gaļas produkti un olas","Dārzeņi un augļi","Jūrasprodukti")




kcal_table$cluster <- with(kcal_table, factor(cluster, levels=c("1", "2"), labels = c("Grupa 1 (zaļie)", "Grupa 2 (sarkānie)")))




kcal_ggplot <- melt(kcal_table, id.vars=c("Participant","cluster")) %>%
  ggplot(., aes(fill=variable, y=as.numeric(value), x=Participant)) + 
  geom_bar(stat="identity")+
  theme_hc()+ 
  scale_fill_manual(values=c("purple","yellow", "brown","lightblue","red","green","blue"))+
  facet_wrap(~cluster,scales = "free_x",  labeller = labeller(dose = cluster, supp = supp.labs))+ 
  geom_hline(yintercept = c(1),linetype="dashed", color = c("red"))+
  theme(strip.background =element_rect(fill="white"))+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  ylab("% kkal no kopējā ieteicama daudzuma")+
  xlab(NULL)+
  theme(text=element_text(size=16),plot.title = element_text(hjust = 0.5),axis.text.x = element_blank())+
  theme(legend.title=element_blank())


kcal_ggplot 




