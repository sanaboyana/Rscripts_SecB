library(readxl)
library(writexl)
library(gplots)
library(tidyverse)
library(tidyr)
library(ggrepel)
library(gridExtra)

exp_secb_data <- read_excel("SecB_COLAB_DOCKING_ANALYSIS.xlsx", sheet="SecB_substrates_NMR")

exp_secb_data <- exp_secb_data %>% 
  unite('label', Residue, ResID, sep = "_", remove = F)

plot_sasa <- function (colname, man_col) {
  ggplot(exp_secb_data, aes(ResID_allchain, get(colname), label = label)) + 
  geom_point(color = man_col, size = 3) + 
  geom_label_repel(aes(label = ifelse(get(colname) > 5, as.character(label), '')), 
                   box.padding = 0.35,
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_classic(base_size = 16) + 
  coord_cartesian(xlim=c(0,620), ylim =c(0,15)) +
  theme(legend.position="top") +
  labs(title=colname,x="SecB monomer (ResID)", y = "<change in free sasa>")+
  theme(axis.text.x = element_text(colour = "black", size = 14)) + 
  theme(axis.text.y = element_text(colour = "black", size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 14)) +
  theme(aspect.ratio=1) 
}

P1 <- plot_sasa("PhoAa average", "blue")
P2 <- plot_sasa("PhoAc average", "blue")
P3 <- plot_sasa("PhoAd average", "blue")
P4 <- plot_sasa("PhoAe average", "blue")
P5 <- plot_sasa("MBPd average", "red")
P6 <- plot_sasa("MBPe average", "red")


P_Pho <- grid.arrange(P1, P2, P3, P4, nrow = 2)
P_MBP <- grid.arrange(P5, P6, nrow = 2)

ggsave("PhoA_free_sasa.png", P_Pho, units = "in", width = 10, height = 10)
ggsave("MBP_free_sasa.png", P_MBP, units = "in", width = 5, height = 10)
