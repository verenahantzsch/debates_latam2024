# librerias
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gplots)

# DATA
#df_mca_xdebate

current_df <- df_mca_xdebate8
row.names(current_df) <- paste(current_df$cat_pais, row.names(current_df))
res.mca <- MCA(current_df,
               quanti.sup=which(colnames(current_df) == "ncat_eleccion"), 
               quali.sup=which(colnames(current_df) == "cat_pais"), 
               graph = FALSE) 

# PRE

# Graph # no funca
balloonplot(t(current_df), xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# chi # no funca
chisq <- chisq.test(current_df)

# Summary 
summary(current_df)


# POS

print(res.mca) # aca podemos ver el chi # no para mca

# Chi-square statistics
chi2 <- 
# Degree of freedom
df <- (nrow(current_df) - 1) * (ncol(current_df) - 1)
# P-value
pval <- pchisq(chi2, df = df, lower.tail = FALSE)

# eigenvalues

eig.val <- get_eigenvalue(res.mca)


#% inertia explained

expectedrow_intertia <- 1/(nrow(current_df)-1)
expectedcol_intertia <- 1/(ncol(current_df)-1)

max_expected <- max(expectedrow_intertia, expectedcol_intertia)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 25)) +
  geom_hline(yintercept=max_expected, linetype=2, color="red")


# ROWS

row <- get_mca_ind(res.mca)

# Coordinates
head(row$coord)
# Cos2: quality on the factore map
head(row$cos2)


# Color by cos2 values: quality on the factor map
fviz_mca_ind(res.mca, col.ind= "cos2", # acá podemos cambiar este parámetro
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
            )
fviz_mca_ind(res.mca, col.ind= "cos2", # acá podemos cambiar este parámetro
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes=c(3,4)
)

corrplot(row$cos2, is.corr=FALSE)

# Cos2 of rows on Dim.1 and Dim.2
fviz_cos2(res.mca, choice = "ind", axes = 1:4, top = 100)
fviz_cos2(res.mca, choice = "ind", axes = 1:4)
fviz_cos2(res.mca, choice = "ind", axes = 1, top = 15)
fviz_cos2(res.mca, choice = "ind", axes = 2, top = 15)

# Contributions to the principal components
head(row$contrib)
corrplot(row$contrib, is.corr=FALSE) 

# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "ind", axes = 1, top = 20)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "ind", axes = 2, top = 20)
# Contributions of rows to dimension 3
fviz_contrib(res.mca, choice = "ind", axes = 3, top = 20)
# Contributions of rows to dimension 4
fviz_contrib(res.mca, choice = "ind", axes = 4, top = 20)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 10)

fviz_mca_ind(res.mca, col.ind = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") )

fviz_mca_ind(res.mca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes=c(3,4))

# COLUMNS 

col <- get_mca_var(res.mca)

# Coordinates of column points
head(col$coord)

# Quality of representation
head(col$cos2)
fviz_mca_var(res.mca, col.var = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_mca_var(res.mca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes=c(3,4))

fviz_cos2(res.mca, choice = "var", axes = 1:4)
fviz_cos2(res.mca, choice = "var", axes = 5)

# Contributions
head(col$contrib)
fviz_contrib(res.mca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.mca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.mca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.mca, choice = "var", axes = 5, top = 10)

fviz_mca_var(res.mca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes=c(3,4))
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes=c(4,5))
# biplot
  
fviz_ca_biplot(res.mca, 
               map ="rowprincipal") # no funca

fviz_mca_biplot(res.mca, ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             ggtheme= theme_minimal())

fviz_mca_var(res.mca, choice = "mca.cor",
             ggtheme= theme_minimal())
fviz_mca_var(res.mca, choice = "mca.cor",
             ggtheme= theme_minimal(),
             axes = c(3, 4))
fviz_mca_var(res.mca, choice = "quanti.sup",
             ggtheme = theme_minimal())


# Dimension description #no funca
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1 by row points
head(res.desc[[1]]$ind, 10)
# Description of dimension 1 by column points
head(res.desc[[1]]$var, 10)
