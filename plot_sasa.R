library(readxl)
library(writexl)
library(gplots)
library(tidyverse)
library(tidyr)
library(ggrepel)

exp_secb_data <- read_excel("SecB_COLAB_DOCKING_ANALYSIS.xlsx", sheet="SecB_substrates_NMR")

plot(exp_secb_data$`PhoAa average`)
df <- exp_secb_data %>% select(ResID, Residue, ResID_allchain, `PhoAa average`)

df <- df %>% 
  unite('label', Residue, ResID, sep = "_", remove = F)


ggplot(df, aes(ResID_allchain, `PhoAa average`, label = label)) + 
  geom_point(color = "blue", size = 3) + 
  #geom_text(aes(label = ifelse(`PhoAa average` > 5, as.character(label), '')), hjust=0, vjust=0) + 
  geom_label_repel(aes(label = ifelse(`PhoAa average` > 5, as.character(label), '')), 
                   box.padding = 0.35,
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_classic(base_size = 16) + 
  theme(legend.position="top") +
  labs(title="PhoAa",x="SecB tetramer (ResID)", y = "<change in free sasa>")+
  theme(axis.text.x = element_text(colour = "black", size = 14)) + 
  theme(axis.text.y = element_text(colour = "black", size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 14))  

