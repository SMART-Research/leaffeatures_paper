## R packages
library(tidyverse)
library(here)
library(Rtsne)
library(ggplot2)
library(MASS)
library(patchwork)
library(ggpubr) # to use ggarrange

## PCA projection
calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3],
                           PC4 = pcaY_cal$x[, 4],
                           PC5 = pcaY_cal$x[, 5],
                           PC6 = pcaY_cal$x[, 6],
                           PC7 = pcaY_cal$x[, 7],
                           PC8 = pcaY_cal$x[, 8])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}
pca_projection <- function(prcomp_out, data_to_project){
  
  PCA <- scale(data_to_project, prcomp_out$center, prcomp_out$scale) %*% prcomp_out$rotation
  pca_projected <- data.frame(PC1=PCA[,1], PC2=PCA[,2], PC3=PCA[,3], PC4=PCA[,4], PC5=PCA[,5], PC6=PCA[,6], PC7=PCA[,7], PC8=PCA[,8]) 
  return(pca_projected)
  
}
pca_summary <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  
  return(summary(pcaY_cal))
}


## Swedish -----
data_new <- read.csv("data_all_with_label_with_species.csv", header = TRUE)
head(data_new)

features <- data_new[, c(3:10,12:53)] # remove Outlying_polar and Outlying_contour
pca_ref_calc <- calculate_pca(features)
# combine features and PCs' into a one dataframe
data_new$PC1 <- pca_ref_calc$pca_components$PC1
data_new$PC2 <- pca_ref_calc$pca_components$PC2
data_new$PC3 <- pca_ref_calc$pca_components$PC3
data_new$PC4 <- pca_ref_calc$pca_components$PC4
data_new$PC5 <- pca_ref_calc$pca_components$PC5
data_new$PC6 <- pca_ref_calc$pca_components$PC6
data_new$PC7 <- pca_ref_calc$pca_components$PC7
data_new$PC8 <- pca_ref_calc$pca_components$PC8

p11 <- ggplot(data_new, aes(x=PC1, y=PC2, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) + xlab("PCA1") + ylab("PCA2") + theme(aspect.ratio = 1) 
p12 <- ggplot(data_new, aes(x=PC1, y=PC3, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA1") + ylab("PCA3") + theme(aspect.ratio = 1) 
p13 <- ggplot(data_new, aes(x=PC1, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA1") + ylab("PCA4") + theme(aspect.ratio = 1) 
p14 <- ggplot(data_new, aes(x=PC1, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA1") + ylab("PCA5") + theme(aspect.ratio = 1) 
p15 <- ggplot(data_new, aes(x=PC2, y=PC3, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) + xlab("PCA2") + ylab("PCA3") + theme(aspect.ratio = 1) 
p16 <- ggplot(data_new, aes(x=PC2, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA2") + ylab("PCA4") + theme(aspect.ratio = 1) 
p17 <- ggplot(data_new, aes(x=PC2, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA2") + ylab("PCA5") + theme(aspect.ratio = 1) 
p18 <- ggplot(data_new, aes(x=PC3, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA3") + ylab("PCA4") + theme(aspect.ratio = 1) 
p19 <- ggplot(data_new, aes(x=PC3, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a")) +  xlab("PCA3") + ylab("PCA5") + theme(aspect.ratio = 1) 

#p1h1 <- p11 + theme(legend.position = "none") + theme(plot.title = element_blank())
#p2h1 <- p12 + theme(legend.position = "none") + theme(plot.title = element_blank())
#p3h1 <- p13 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p4h1 <- p14 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p5h1 <- p15 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p6h1 <- p16 + theme(legend.position = "none") + theme(plot.title = element_blank())
#p7h1 <- p17 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p8h1 <- p18 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p9h1 <- p19 + theme(plot.title = element_blank()) + theme(legend.position = "none")
#p1h1 + p2h1 + p3h1 + p4h1 + p5h1 + p6h1 + p7h1 + p8h1 + p9h1 + plot_annotation(
#  title = 'PCA with Actual Species',
#  tag_levels = 'A'
#) & theme(plot.tag = element_text(size = 5))

ggarrange(p11, p12, p13, p14, p15, p16, p17,p18, p19,
          ncol=3, nrow=3, common.legend = TRUE, legend="bottom")


## swedish-lda
LDA <- lda(Species~ diameter + area + perimeter + physiological_length + physiological_width + aspect_ratio + rectangularity + circularity + compactness + NF + Perimeter_ratio_diameter + Perimeter_ratio_length + Perimeter_ratio_lw + No_of_Convex_points + perimeter_convexity + area_convexity + area_ratio_convexity + equivalent_diameter + contrast + correlation_texture + inverse_difference_moments + entropy + cx + cy + eccentriciry + Mean_R_val + Mean_G_val + Mean_B_val + Std_R_val + Std_G_val + Std_B_val + correlation  + Skewed_polar + Clumpy_polar + Sparse_polar + Striated_polar + Convex_polar + Skinny_polar + Stringy_polar + Monotonic_polar +  Skewed_contour + Clumpy_contour + Sparse_contour + Striated_contour + Convex_contour + Skinny_contour + Stringy_contour + Monotonic_contour + No_of_max_ponits + No_of_min_points, data= data_new)


LDA.values <- predict(LDA)

lda_data_new <- data.frame(LDA1 = LDA.values$x[,1], LDA2 = LDA.values$x[,2], LDA3 = LDA.values$x[,3], LDA4 = LDA.values$x[,4], Species = data_new$Species)

lda_data_new %>% head(5)

lda_data <- data.frame(x = LDA.values$x[,1], y = LDA.values$x[,2], col = data_new$Species)
scagnostic_lda <- bind_cols(data_new, lda_data)


## LDA data
p1 <- ggplot(scagnostic_lda, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8")) + ggtitle("LDA1 Vs LDA2 by Actual Species") + xlab("LDA1") + ylab("LDA2") + theme(aspect.ratio = 1) 
p1b <- p1 + theme(legend.position = "none")

lda_data_1 <- data.frame(x = LDA.values$x[,1], y = LDA.values$x[,3], col = data_new$Species)
scagnostic_lda_1 <- bind_cols(data_new, lda_data_1)
p1_1 <- ggplot(scagnostic_lda_1, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8")) + ggtitle("LDA1 Vs LDA3 by Actual Species") + xlab("LDA1") + ylab("LDA3") + theme(aspect.ratio = 1) 
p2b <- p1_1 + theme(legend.position = "none")


lda_data_2 <- data.frame(x = LDA.values$x[,2], y = LDA.values$x[,3], col = data_new$Species)
scagnostic_lda_2 <- bind_cols(data_new, lda_data_2)
p1_2 <- ggplot(scagnostic_lda_2, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8")) + ggtitle("LDA2 Vs LDA3 by Actual Species") + xlab("LDA2") + ylab("LDA3") + theme(aspect.ratio = 1) 
p3b <- p1_2 + theme(legend.position = "none")

p1h <- p1b + theme(plot.title = element_blank())
p2h <- p2b + theme(plot.title = element_blank())
p3h <- p1_2 + theme(plot.title = element_blank())


ggarrange(p1h, p2h, p3h,
          ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


##--------------------------------
## Flavia-read data and PCA

data_new1 <- read.csv("data_all_with_label_flavia_with_species.csv", header = TRUE)
features1 <- data_new1[, c(3:10,12:53)] # remove Outlying_polar and Outlying_contour
pca_ref_calc1 <- calculate_pca(features1)
# combine features and PCs' into a one dataframe
data_new1$PC1 <- pca_ref_calc1$pca_components$PC1
data_new1$PC2 <- pca_ref_calc1$pca_components$PC2
data_new1$PC3 <- pca_ref_calc1$pca_components$PC3
data_new1$PC4 <- pca_ref_calc1$pca_components$PC4
data_new1$PC5 <- pca_ref_calc1$pca_components$PC5
data_new1$PC6 <- pca_ref_calc1$pca_components$PC6
data_new1$PC7 <- pca_ref_calc1$pca_components$PC7
data_new1$PC8 <- pca_ref_calc1$pca_components$PC8

# PCA components
#pca_summary(features1)

# PCA table
#pca_ref_calc1

p111 <- ggplot(data_new1, aes(x=PC1, y=PC2, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA1 Vs PCA2 by Actual Species") + xlab("PCA1") + ylab("PCA2") + theme(aspect.ratio = 1) 
p121 <- ggplot(data_new1, aes(x=PC1, y=PC3, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA1 Vs PCA3 by Actual Species") + xlab("PCA1") + ylab("PCA3") + theme(aspect.ratio = 1) 
p131 <- ggplot(data_new1, aes(x=PC1, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA1 Vs PCA4 by Actual Species") + xlab("PCA1") + ylab("PCA4") + theme(aspect.ratio = 1) 
p141 <- ggplot(data_new1, aes(x=PC1, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA1 Vs PCA5 by Actual Species") + xlab("PCA1") + ylab("PCA5") + theme(aspect.ratio = 1) 
p151 <- ggplot(data_new1, aes(x=PC2, y=PC3, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA2 Vs PCA3 by Actual Species") + xlab("PCA2") + ylab("PCA3") + theme(aspect.ratio = 1) 

p161 <- ggplot(data_new1, aes(x=PC2, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA2 Vs PCA4 by Actual Species") + xlab("PCA2") + ylab("PCA4") + theme(aspect.ratio = 1) 

p171 <- ggplot(data_new1, aes(x=PC2, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA2 Vs PCA5 by Actual Species") + xlab("PCA2") + ylab("PCA5") + theme(aspect.ratio = 1) 

p181 <- ggplot(data_new1, aes(x=PC3, y=PC4, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("PCA3 Vs PCA4 by Actual Species") + xlab("PCA3") + ylab("PCA4") + theme(aspect.ratio = 1) 


p191 <- ggplot(data_new1, aes(x=PC3, y=PC5, col=Species)) + geom_point(alpha = 0.5) +coord_equal()+ 
  scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3",
                                 "#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb",
                                 "#b2df8a","#3f007d","#67000d","#525252","#006d2c",
                                 "#3690c0","#88419d","#8c96c6","#238B45","#fed976",
                                 "#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a",
                                 "#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + 
  ggtitle("PCA3 Vs PCA5 by Actual Species") + xlab("PCA3") + ylab("PCA5") +
  theme(aspect.ratio = 1) 

a=-0.05
p1h2 <- p111 + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.title = element_blank()) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p2h2 <- p121 + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.title = element_blank()) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p3h2 <- p131 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p4h2 <- p141 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p5h2 <- p151 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p6h2 <- p161 + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.title = element_blank()) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p7h2 <- p171 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p8h2 <- p181 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))
p9h2 <- p191 + theme(plot.title = element_blank()) + theme(legend.position = "none", text = element_text(size=8)) + theme(plot.margin = unit(c(a,a,a,a), "lines"))

ggarrange(p1h2, p2h2, p3h2, p4h2, p5h2, p6h2, p7h2, p8h2, p9h2,
          ncol=3, nrow=3, common.legend = TRUE, legend="none")

leg <- get_legend(p191, position = "bottom")
as_ggplot(leg)

## LDA 

LDA1 <- lda(Species~ diameter + 
              area + perimeter + 
              physiological_length + 
              physiological_width + aspect_ratio + rectangularity + 
              circularity + compactness + NF + Perimeter_ratio_diameter + 
              Perimeter_ratio_length + Perimeter_ratio_lw + No_of_Convex_points + 
              perimeter_convexity + area_convexity + area_ratio_convexity +
              equivalent_diameter + contrast + correlation_texture + 
              inverse_difference_moments + entropy + cx + cy + 
              eccentriciry + Mean_R_val + Mean_G_val + Mean_B_val + 
              Std_R_val + Std_G_val + Std_B_val + correlation  +
              Skewed_polar + Clumpy_polar + Sparse_polar + Striated_polar +
              Convex_polar + Skinny_polar + Stringy_polar + Monotonic_polar + 
              Skewed_contour + Clumpy_contour + Sparse_contour + Striated_contour + 
              Convex_contour + Skinny_contour + Stringy_contour + Monotonic_contour +
              No_of_max_ponits + No_of_min_points, data= data_new1)

## LDA data
LDA.values1 <- predict(LDA1)
lda_data_new1 <- data.frame(LDA1 = LDA.values1$x[,1], LDA2 = LDA.values1$x[,2], LDA3 = LDA.values1$x[,3], Species = data_new1$Species)
lda_data1 <- data.frame(x = LDA.values1$x[,1], y = LDA.values1$x[,2], col = data_new1$Species)
scagnostic_lda1 <- bind_cols(data_new1, lda_data1)
p1 <- ggplot(scagnostic_lda1, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("LDA1 Vs LDA2 by Actual Species") + xlab("LDA1") + ylab("LDA2") + theme(aspect.ratio = 1) 
p1b <- p1 + theme(legend.position = "none")
lda_data_11 <- data.frame(x = LDA.values1$x[,1], y = LDA.values1$x[,3], col = data_new1$Species)
scagnostic_lda_11 <- bind_cols(data_new1, lda_data_11)
p1_11 <- ggplot(scagnostic_lda_11, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("LDA1 Vs LDA3 by Actual Species") + xlab("LDA1") + ylab("LDA3") + theme(aspect.ratio = 1) 
p2b1 <- p1_11 + theme(legend.position = "none")
lda_data_21 <- data.frame(x = LDA.values1$x[,2], y = LDA.values1$x[,3], col = data_new1$Species)
scagnostic_lda_21 <- bind_cols(data_new1, lda_data_21)
p1_21 <- ggplot(scagnostic_lda_21, aes(x=x, y=y, color=Species)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#1b9e77","#3182bd","#d95f02","#7570b3","#fc8d92","#31a354","#7b3294","#d01c8b","#8da0cb","#b2df8a","#3f007d","#67000d","#525252","#006d2c","#3690c0","#88419d","#8c96c6","#238B45","#fed976","#fc4e2a","#fe9929","#dd3497","#3690c0","#e7298a","#7f0000","#bcbddc","#66c2a4","#045a8d","#253494")) + ggtitle("LDA2 Vs LDA3 by Actual Species") + xlab("LDA2") + ylab("LDA3") + theme(aspect.ratio = 1) 
p3b1 <- p1_21 + theme(legend.position = "none")
p1h <- p1b + theme(plot.title = element_blank())
p2h <- p2b1 + theme(plot.title = element_blank())
p3h <- p1_21 + theme(plot.title = element_blank())

ggarrange(p1h, p2h, p3h,
          ncol=3, nrow=1, common.legend = TRUE, legend="none")
leg2 <- get_legend(p3h, position = "bottom")
as_ggplot(leg2)



