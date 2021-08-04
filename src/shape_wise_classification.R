library(packrat)
library(tidyverse)
library(here)
library(Rtsne)
library(ggpubr)

## PCA Projection - SWEDISH SHAPE

calculate_pca <- function(feature_dataset){
  pcaY_cal <- prcomp(feature_dataset, center = TRUE, scale = TRUE)
  PCAresults <- data.frame(PC1 = pcaY_cal$x[, 1], 
                           PC2 = pcaY_cal$x[, 2], 
                           PC3 = pcaY_cal$x[, 3],
                           PC4 = pcaY_cal$x[, 4],
                           PC5 = pcaY_cal$x[, 5])
  return(list(prcomp_out =pcaY_cal,pca_components = PCAresults))
}
pca_projection <- function(prcomp_out, data_to_project){
  
  PCA <- scale(data_to_project, prcomp_out$center, prcomp_out$scale) %*% prcomp_out$rotation
  pca_projected <- data.frame(PC1=PCA[,1], PC2=PCA[,2], PC3=PCA[,3]) 
  return(pca_projected)
  
}
data_new <- read.csv("data/Swedish_dataset/data_all_with_label.csv", header = TRUE)
features <- data_new[, c(3:10,12:53)] # remove Outlying_polar and Outlying_contour
pca_ref_calc <- calculate_pca(features)
# combine features and PCs' into a one dataframe
data_new$PC1 <- pca_ref_calc$pca_components$PC1
data_new$PC2 <- pca_ref_calc$pca_components$PC2
data_new$PC3 <- pca_ref_calc$pca_components$PC3
data_new$PC4 <- pca_ref_calc$pca_components$PC4
data_new$PC5 <- pca_ref_calc$pca_components$PC5


p11 <- ggplot(data_new, aes(x=PC1, y=PC2, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA2 by Actual Shape Label") + xlab("PCA1") + ylab("PCA2") + theme(aspect.ratio = 1) 

p12 <- ggplot(data_new, aes(x=PC1, y=PC3, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA3 by Actual Shape Label") + xlab("PCA1") + ylab("PCA3") + theme(aspect.ratio = 1) 

p13 <- ggplot(data_new, aes(x=PC1, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA4 by Actual Shape Label") + xlab("PCA1") + ylab("PCA4") + theme(aspect.ratio = 1) 

p14 <- ggplot(data_new, aes(x=PC1, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA5 by Actual Shape Label") + xlab("PCA1") + ylab("PCA5") + theme(aspect.ratio = 1) 

p15 <- ggplot(data_new, aes(x=PC2, y=PC3, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA3 by Actual Shape Label") + xlab("PCA2") + ylab("PCA3") + theme(aspect.ratio = 1) 

p16 <- ggplot(data_new, aes(x=PC2, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA4 by Actual Shape Label") + xlab("PCA2") + ylab("PCA4") + theme(aspect.ratio = 1) 

p17 <- ggplot(data_new, aes(x=PC2, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA5 by Actual Shape Label") + xlab("PCA2") + ylab("PCA5") + theme(aspect.ratio = 1) 

p18 <- ggplot(data_new, aes(x=PC3, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA3 Vs PCA4 by Actual Shape Label") + xlab("PCA3") + ylab("PCA4") + theme(aspect.ratio = 1) 

p19 <- ggplot(data_new, aes(x=PC3, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA3 Vs PCA5 by Actual Shape Label") + xlab("PCA3") + ylab("PCA5") + theme(aspect.ratio = 1) 

a = -0.02
p1h1 <- p11 + theme(legend.position = "none") + theme(plot.title = element_blank()) + theme(plot.margin = unit(c(a,a,a,a), "lines"))

p2h1 <- p12 + theme(legend.position = "none") + theme(plot.title = element_blank())+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p3h1 <- p13 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p4h1 <- p14 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p5h1 <- p15 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p6h1 <- p16 + theme(legend.position = "none") + theme(plot.title = element_blank())+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p7h1 <- p17 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p8h1 <- p18 + theme(plot.title = element_blank()) + theme(legend.position = "bottom")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p9h1 <- p19 + theme(plot.title = element_blank()) +   theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))


ggarrange(p1h1, p2h1, p3h1, p4h1, p5h1, p6h1, p7h1, p8h1, p9h1 ,
          ncol=3, nrow=3, common.legend = TRUE, legend="bottom")

## Swedish - LDA
LDA <- lda(Shape_label~ diameter + area + perimeter + physiological_length + physiological_width + aspect_ratio + rectangularity + circularity + compactness + NF + Perimeter_ratio_diameter + Perimeter_ratio_length + Perimeter_ratio_lw + No_of_Convex_points + perimeter_convexity + area_convexity + area_ratio_convexity + equivalent_diameter + contrast + correlation_texture + inverse_difference_moments + entropy + cx + cy + eccentriciry + Mean_R_val + Mean_G_val + Mean_B_val + Std_R_val + Std_G_val + Std_B_val + correlation  + Skewed_polar + Clumpy_polar + Sparse_polar + Striated_polar + Convex_polar + Skinny_polar + Stringy_polar + Monotonic_polar +  Skewed_contour + Clumpy_contour + Sparse_contour + Striated_contour + Convex_contour + Skinny_contour + Stringy_contour + Monotonic_contour + No_of_max_ponits + No_of_min_points, data= data_new)


LDA.values <- predict(LDA)

lda_data_new <- data.frame(LDA1 = LDA.values$x[,1], LDA2 = LDA.values$x[,2], LDA3 = LDA.values$x[,3], Shape_label = data_new$Shape_label)



lda_data <- data.frame(x = LDA.values$x[,1], y = LDA.values$x[,2], col = data_new$Shape_label)
scagnostic_lda <- bind_cols(data_new, lda_data)

p1 <- ggplot(scagnostic_lda, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA1 Vs LDA2 by Actual Shape Label") + xlab("LDA1") + ylab("LDA2") + theme(aspect.ratio = 1) 

p1b <- p1 + theme(legend.position = "none")

lda_data_1 <- data.frame(x = LDA.values$x[,1], y = LDA.values$x[,3], col = data_new$Shape_label)
scagnostic_lda_1 <- bind_cols(data_new, lda_data_1)

p1_1 <- ggplot(scagnostic_lda_1, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA1 Vs LDA3 by Actual shape label") + xlab("LDA1") + ylab("LDA3") + theme(aspect.ratio = 1) 
p2b <- p1_1 + theme(legend.position = "none")

lda_data_2 <- data.frame(x = LDA.values$x[,2], y = LDA.values$x[,3], col = data_new$Shape_label)
scagnostic_lda_2 <- bind_cols(data_new, lda_data_2)

p1_2 <- ggplot(scagnostic_lda_2, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA2 Vs LDA3 by Actual shape label") + xlab("LDA2") + ylab("LDA3") + theme(aspect.ratio = 1) 

p3b <- p1_2 + theme(legend.position = "none")

p1h <- p1b + theme(plot.title = element_blank())

p2h <- p2b + theme(plot.title = element_blank())

p3h <- p1_2 + theme(plot.title = element_blank())

ggarrange(p1h, p2h, p3h,
          ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


## Flavia - PCA ----------------------

data_new1 <- read.csv("data/Flavia_dataset/data_all_with_label_flavia.csv", header = TRUE)
features1 <- data_new1[, c(3:10,12:53)] # remove Outlying_polar and Outlying_contour
pca_ref_calc1 <- calculate_pca(features1)
# combine features and PCs' into a one dataframe
data_new1$PC1 <- pca_ref_calc1$pca_components$PC1
data_new1$PC2 <- pca_ref_calc1$pca_components$PC2
data_new1$PC3 <- pca_ref_calc1$pca_components$PC3
data_new1$PC4 <- pca_ref_calc1$pca_components$PC4
data_new1$PC5 <- pca_ref_calc1$pca_components$PC5

p111 <- ggplot(data_new1, aes(x=PC1, y=PC2, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA2 by Actual Shape Label") + xlab("PCA1") + ylab("PCA2") + theme(aspect.ratio = 1) 

p121 <- ggplot(data_new1, aes(x=PC1, y=PC3, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA3 by Actual Shape Label") + xlab("PCA1") + ylab("PCA3") + theme(aspect.ratio = 1) 

p131 <- ggplot(data_new1, aes(x=PC1, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA4 by Actual Shape Label") + xlab("PCA1") + ylab("PCA4") + theme(aspect.ratio = 1) 

p141 <- ggplot(data_new1, aes(x=PC1, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA1 Vs PCA5 by Actual Shape Label") + xlab("PCA1") + ylab("PCA5") + theme(aspect.ratio = 1) 

p151 <- ggplot(data_new1, aes(x=PC2, y=PC3, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA3 by Actual Shape Label") + xlab("PCA2") + ylab("PCA3") + theme(aspect.ratio = 1) 

p161 <- ggplot(data_new1, aes(x=PC2, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA4 by Actual Shape Label") + xlab("PCA2") + ylab("PCA4") + theme(aspect.ratio = 1) 

p171 <- ggplot(data_new1, aes(x=PC2, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA2 Vs PCA5 by Actual Shape Label") + xlab("PCA2") + ylab("PCA5") + theme(aspect.ratio = 1) 

p181 <- ggplot(data_new1, aes(x=PC3, y=PC4, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA3 Vs PCA4 by Actual Shape Label") + xlab("PCA3") + ylab("PCA4") + theme(aspect.ratio = 1) 

p191 <- ggplot(data_new1, aes(x=PC3, y=PC5, col=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("PCA3 Vs PCA5 by Actual Shape Label") + xlab("PCA3") + ylab("PCA5") + theme(aspect.ratio = 1) 

a = -0.02
p1h2f <- p111 + theme(legend.position = "none") + theme(plot.title = element_blank())+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p2h2f <- p121 + theme(legend.position = "none") + theme(plot.title = element_blank())+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p3h2f <- p131 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p4h2f <- p141 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p5h2f <- p151 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p6h2f <- p161 +   theme(plot.title = element_blank())+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p7h2f <- p171 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p8h2f <- p181 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))

p9h2f <- p191 + theme(plot.title = element_blank()) + theme(legend.position = "none")+ theme(plot.margin = unit(c(a,a,a,a), "lines"))


ggarrange(p1h2f, p2h2f, p3h2f, p4h2f, p5h2f, p6h2f, p7h2f, p8h2f, p9h2f ,
          ncol=3, nrow=3, common.legend = TRUE, legend="bottom")



## Flavia - LDA ----------------------

data_new1 <- read.csv("data/Flavia_dataset/data_all_with_label_flavia.csv", header = TRUE)
features1 <- data_new1[, c(3:10,12:53)] # remove Outlying_polar and Outlying_contour

LDA1 <- lda(Shape_label~ diameter + area + perimeter + physiological_length + physiological_width + aspect_ratio + rectangularity + circularity + compactness + NF + Perimeter_ratio_diameter + Perimeter_ratio_length + Perimeter_ratio_lw + No_of_Convex_points + perimeter_convexity + area_convexity + area_ratio_convexity + equivalent_diameter + contrast + correlation_texture + inverse_difference_moments + entropy + cx + cy + eccentriciry + Mean_R_val + Mean_G_val + Mean_B_val + Std_R_val + Std_G_val + Std_B_val + correlation  + Skewed_polar + Clumpy_polar + Sparse_polar + Striated_polar + Convex_polar + Skinny_polar + Stringy_polar + Monotonic_polar +  Skewed_contour + Clumpy_contour + Sparse_contour + Striated_contour + Convex_contour + Skinny_contour + Stringy_contour + Monotonic_contour + No_of_max_ponits + No_of_min_points, data= data_new1)


LDA.values1 <- predict(LDA1)

lda_data_new1 <- data.frame(LDA1 = LDA.values1$x[,1], LDA2 = LDA.values1$x[,2], LDA3 = LDA.values1$x[,3], Shape_label = data_new1$Shape_label)



lda_data1 <- data.frame(x = LDA.values1$x[,1], y = LDA.values1$x[,2], col = data_new1$Shape_label)
scagnostic_lda1 <- bind_cols(data_new1, lda_data1)

p1 <- ggplot(scagnostic_lda1, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA1 Vs LDA2 by Actual Shape Label") + xlab("LDA1") + ylab("LDA2") + theme(aspect.ratio = 1) 

p1b <- p1 + theme(legend.position = "none")

lda_data_11 <- data.frame(x = LDA.values1$x[,1], y = LDA.values1$x[,3], col = data_new1$Shape_label)
scagnostic_lda_11 <- bind_cols(data_new1, lda_data_11)

p1_11 <- ggplot(scagnostic_lda_11, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA1 Vs LDA3 by Actual shape label") + xlab("LDA1") + ylab("LDA3") + theme(aspect.ratio = 1) 

p2b1 <- p1_11 + theme(legend.position = "none")

lda_data_21 <- data.frame(x = LDA.values1$x[,2], y = LDA.values1$x[,3], col = data_new1$Shape_label)
scagnostic_lda_21 <- bind_cols(data_new1, lda_data_21)

p1_21 <- ggplot(scagnostic_lda_21, aes(x=x, y=y, color=Shape_label)) + geom_point(alpha = 0.5) +coord_equal()+ scale_colour_manual(values = c("#a6611a","#d7191c","#e66101","#7b3294","#1b9e77")) + ggtitle("LDA2 Vs LDA3 by Actual shape label") + xlab("LDA2") + ylab("LDA3") + theme(aspect.ratio = 1) 
p3b1 <- p1_21 + theme(legend.position = "none")

p1hf <- p1b + theme(plot.title = element_blank())

p2hf <- p2b1 + theme(plot.title = element_blank())

p3hf <- p1_21 + theme(plot.title = element_blank())

ggarrange(p1hf, p2hf, p3hf,
          ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
