# If you use this script, please cite the original paper:
# "New insights from GWAS on BMI-related growth traits in a longitudinal cohort of admixed children with Native American and European ancestry."
# "Vicuña L, Barrientos E, Norambuena T, Alvares D, Gana JC, Leiva-Yamaguchi V, Meza C, Santos JL, Mericq V, Pereira A, Eyheramendy S.
# iScience. 2023 Jan 31;26(2):106091. doi: 10.1016/j.isci.2023.106091. eCollection 2023 Feb 17.
# PMID: 36844456


library(dplyr)

x = X2$SNP_pos
y = -log10(X2$pval)
z = data.frame(x,y)

z = z[z$y>2,]

# Cut in segments:
my_segments = c(48210,100848,145702,186543,223330,266721,
                300915,333096,359821,390431,420320,448935,
                470867,490494,509175,528626,546438,564732,
                577262,592132,600843,609122)

gr <- cut(z$x, my_segments,labels = FALSE, right = T)
gr[is.na(gr)] <- 0

# Create color vector:
z$color <- ifelse(gr %% 2 == 0, "red", "deepskyblue2")


pdf("/data/lucas/bmi/figures/manhattan_ageAR.pdf",  
    width=6, height=4.2)

plot(z$x, z$y, type="p", cex = 0.6, pch = 19,
     col = z$color,
     #     col="black", 
     lwd=0.1,
     # xlab= "SNP position",
     frame.plot = F,
     xaxt = 'n', # removes x labels,
     # ylab= "Mean amerindian ancestry",
     ylim = c(2, 8),
     las = 2,
     xlim = c(0, 609122),
     cex.lab=1, # size of axis labels
     ann = FALSE, # remove axis titles
     mgp = c(3, 0.8, 0)) 

mtext(side = 1, text = "Chromosome number", line = 1) 
mtext(side = 2, 
      text = expression(paste("-log"[10]," ",italic("P"),"-value",sep="")),
      line = 1.7, adj = 0.5, cex = 0.9) 

# Adjust y axis label size:
par(cex.axis= 1.1, tck=-0.03)

abline(h = 7.3, col= "black", lty=3, lwd=0.7)
options(scipen = 999) # unables scientific notation por axes

# Add ticks to X axis
axis(side=1, at = c(48210,100848,145702,186543,223330,266721,
                    300915,333096,359821,390431,420320,448935,
                    470867,490494,509175,528626,546438,564732,
                    577262,592132,600843,609122), labels = F, las=1, cex.axis=0.6, font = 2,
     tck=-0.03, pos = 1.8)

# Add tick at "0"
axis(side=1, at = c(0, 48210), labels = F, las=1, cex.axis=0.6, font = 2,
     tck=-0.03, pos = 1.8)

# Add labels between ticks
axis(side=1, at = c(24106,74530,123276,166124,204938,245027,
                    283819,317007,346460,375127,405377,434629,
                    459902,480682,499836,518902,537533,555586,
                    570998,584698,596489,604984), labels = 1:22, 
     las=2, cex.axis=0.7, font = 1, tck= F, lwd=0, mgp = c(3, 0.12, 0))

mtext(side = 3, text = "rs445398", line = 1, 
      adj = 0.58, padj = 5.0, las = 1, cex = 0.8, col="black")

dev.off()
