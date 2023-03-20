# If you use this script, please cite the original paper:
# "New insights from GWAS on BMI-related growth traits in a longitudinal cohort of admixed children with Native American and European ancestry."
# "Vicuña L, Barrientos E, Norambuena T, Alvares D, Gana JC, Leiva-Yamaguchi V, Meza C, Santos JL, Mericq V, Pereira A, Eyheramendy S.
# iScience. 2023 Jan 31;26(2):106091. doi: 10.1016/j.isci.2023.106091. eCollection 2023 Feb 17.
# PMID: 36844456

library(readr)
library(dplyr)

df <- read_tsv('~/Genetica/data/freq_loc.tsv')
df1 <- read_tsv('~/Genetica/BMI/BMIxAge/paper/BMI_0.5_1.5_GWAS.tsv') %>% select(SNP)
df1 <- df1 %>% mutate(SNP=paste0('GT_',SNP))
df2 <- df %>% inner_join(df1) %>% filter(FREQ>.01)

df2 %>% 
  ggplot( aes(x = FREQ)) + 
  geom_histogram(
    aes(y =..density..,color='observed'),
    colour = "black", 
    fill = "white") +
  stat_function(aes(color='theoretical'),fun = dexp, args = list(rate = 3.041907),color='#A52A2A')+
  labs(x='Frequency of associated allele',y='Density')+
  theme(axis.text = element_text(face="bold"))
