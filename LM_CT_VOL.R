df <- read.table('df_tsa_tca.csv', header = TRUE, sep = ',')
df <- read.table('df_aims.csv', header = TRUE, sep = ',')
df <- read.table('abide.csv', header = TRUE, sep = ',')



df["ASD"][df["ASD"] == "TCA_pr"] <- "TCA"
df["ASD"][df["ASD"] == "TCA_ac"] <- "TCA"

df <- df[df$ASD == 'Yes' 
         |
           df$ASD == 'No'
         | df$ASD == 'Relative'
         ,]
# Remove one category from datasetdf

df["ASD"][df["ASD"] == "Relative"] <- "No"

# Filter on age
df <- df[(df$age_at_scan > 6) & (df$age_at_scan < 16) ,]
min(df$age_at_scan)
max(df$age_at_scan)

# Encode as factor

df$Sex <- as.factor(df$Sex)
df$ASD <- as.factor(df$ASD)

# 
# table(df$group_random)
# df <- df[(df$group_random == 0) |(df$group_random == 2), ]
# Filter on sex 
# df <- df[df$Sex == 'Female',]
table(df$ASD)
#RDB
df$scanner <- as.factor(df$scanner)
df$Protocole <- as.factor(df$Protocole)
df$scanner
df$Protocole
# Eu-aims
# df$site <- as.factor(df$site)
# df$site
# Define ROIs


# Remove NA from weight
# df<- df[complete.cases(df$patient_weight), ]

vol_cols = c('Left_Accumbens_area', 'Left_Amygdala', 'Left_Caudate',
             'Left_Hippocampus','Left_Pallidum','Left_Putamen', 
             'Left_Thalamus', 'Left_Lateral_Ventricle',
             'Right_Accumbens_area', 'Right_Amygdala', 'Right_Caudate',
             'Right_Hippocampus','Right_Pallidum','Right_Putamen', 
             'Right_Thalamus', 'Right_Lateral_Ventricle')

vol_cols = c('lh_bankssts_thickness',
               'lh_caudalanteriorcingulate_thickness',
               'lh_caudalmiddlefrontal_thickness',
               'lh_cuneus_thickness',
               'lh_entorhinal_thickness',
               'lh_fusiform_thickness',
               'lh_inferiorparietal_thickness',
               'lh_inferiortemporal_thickness',
               'lh_isthmuscingulate_thickness',
               'lh_lateraloccipital_thickness',
               'lh_lateralorbitofrontal_thickness',
               'lh_lingual_thickness',
               'lh_medialorbitofrontal_thickness',
               'lh_middletemporal_thickness',
               'lh_parahippocampal_thickness',
               'lh_paracentral_thickness',
               'lh_parsopercularis_thickness',
               'lh_parsorbitalis_thickness',
               'lh_parstriangularis_thickness',
               'lh_pericalcarine_thickness',
               'lh_postcentral_thickness',
               'lh_posteriorcingulate_thickness',
               'lh_precentral_thickness',
               'lh_precuneus_thickness',
               'lh_rostralanteriorcingulate_thickness',
               'lh_rostralmiddlefrontal_thickness',
               'lh_superiorfrontal_thickness',
               'lh_superiorparietal_thickness',
               'lh_superiortemporal_thickness',
               'lh_supramarginal_thickness',
               'lh_frontalpole_thickness',
               'lh_temporalpole_thickness',
               'lh_transversetemporal_thickness',
               'lh_insula_thickness',
               'rh_bankssts_thickness',
               'rh_caudalanteriorcingulate_thickness',
               'rh_caudalmiddlefrontal_thickness',
               'rh_cuneus_thickness',
               'rh_entorhinal_thickness',
               'rh_fusiform_thickness',
               'rh_inferiorparietal_thickness',
               'rh_inferiortemporal_thickness',
               'rh_isthmuscingulate_thickness',
               'rh_lateraloccipital_thickness',
               'rh_lateralorbitofrontal_thickness',
               'rh_lingual_thickness',
               'rh_medialorbitofrontal_thickness',
               'rh_middletemporal_thickness',
               'rh_parahippocampal_thickness',
               'rh_paracentral_thickness',
               'rh_parsopercularis_thickness',
               'rh_parsorbitalis_thickness',
               'rh_parstriangularis_thickness',
               'rh_pericalcarine_thickness',
               'rh_postcentral_thickness',
               'rh_posteriorcingulate_thickness',
               'rh_precentral_thickness',
               'rh_precuneus_thickness',
               'rh_rostralanteriorcingulate_thickness',
               'rh_rostralmiddlefrontal_thickness',
               'rh_superiorfrontal_thickness',
               'rh_superiorparietal_thickness',
               'rh_superiortemporal_thickness',
               'rh_supramarginal_thickness',
               'rh_frontalpole_thickness',
               'rh_temporalpole_thickness',
               'rh_transversetemporal_thickness',
               'rh_insula_thickness')
length(vol_cols)
# Normalise values for each ROIs

for (i in vol_cols){
  df[, i] <- ((df[, i] - mean(df[df$ASD == 'No', i]))/  sd(df[df$ASD == 'No', i]) )
}



# Perform linear regression
reg <- lm(cbind(Left_Accumbens_area, Left_Amygdala, Left_Caudate,
                Left_Hippocampus,Left_Pallidum,Left_Putamen, 
                Left_Thalamus, Left_Lateral_Ventricle,
                Right_Accumbens_area, Right_Amygdala, Right_Caudate,
                Right_Hippocampus,Right_Pallidum,Right_Putamen, 
                Right_Thalamus, Right_Lateral_Ventricle)~ASD+EstimatedTotalIntraCranialVol+Sex+scanner+Protocole+age_at_scan^2+age_at_scan,
          data=df)
reg <- lm(cbind(lh_bankssts_thickness,
                lh_caudalanteriorcingulate_thickness,
                lh_caudalmiddlefrontal_thickness,
                lh_cuneus_thickness,
                lh_entorhinal_thickness,
                lh_fusiform_thickness,
                lh_inferiorparietal_thickness,
                lh_inferiortemporal_thickness,
                lh_isthmuscingulate_thickness,
                lh_lateraloccipital_thickness,
                lh_lateralorbitofrontal_thickness,
                lh_lingual_thickness,
                lh_medialorbitofrontal_thickness,
                lh_middletemporal_thickness,
                lh_parahippocampal_thickness,
                lh_paracentral_thickness,
                lh_parsopercularis_thickness,
                lh_parsorbitalis_thickness,
                lh_parstriangularis_thickness,
                lh_pericalcarine_thickness,
                lh_postcentral_thickness,
                lh_posteriorcingulate_thickness,
                lh_precentral_thickness,
                lh_precuneus_thickness,
                lh_rostralanteriorcingulate_thickness,
                lh_rostralmiddlefrontal_thickness,
                lh_superiorfrontal_thickness,
                lh_superiorparietal_thickness,
                lh_superiortemporal_thickness,
                lh_supramarginal_thickness,
                lh_frontalpole_thickness,
                lh_temporalpole_thickness,
                lh_transversetemporal_thickness,
                lh_insula_thickness,
                rh_bankssts_thickness,
                rh_caudalanteriorcingulate_thickness,
                rh_caudalmiddlefrontal_thickness,
                rh_cuneus_thickness,
                rh_entorhinal_thickness,
                rh_fusiform_thickness,
                rh_inferiorparietal_thickness,
                rh_inferiortemporal_thickness,
                rh_isthmuscingulate_thickness,
                rh_lateraloccipital_thickness,
                rh_lateralorbitofrontal_thickness,
                rh_lingual_thickness,
                rh_medialorbitofrontal_thickness,
                rh_middletemporal_thickness,
                rh_parahippocampal_thickness,
                rh_paracentral_thickness,
                rh_parsopercularis_thickness,
                rh_parsorbitalis_thickness,
                rh_parstriangularis_thickness,
                rh_pericalcarine_thickness,
                rh_postcentral_thickness,
                rh_posteriorcingulate_thickness,
                rh_precentral_thickness,
                rh_precuneus_thickness,
                rh_rostralanteriorcingulate_thickness,
                rh_rostralmiddlefrontal_thickness,
                rh_superiorfrontal_thickness,
                rh_superiorparietal_thickness,
                rh_superiortemporal_thickness,
                rh_supramarginal_thickness,
                rh_frontalpole_thickness,
                rh_temporalpole_thickness,
                rh_transversetemporal_thickness,
                rh_insula_thickness)~ASD+Sex+Protocole+scanner+age_at_scan^2+age_at_scan+meanCT,
          data=df)
# Visualise outputs

summary(reg)
# Encode outputs into dataframe

# ASDYes
# ASDTCA
coef <- c()

pval <- c()
for (i in 1:length(vol_cols)){
  pval[i] <- summary(reg)[[i]]$coefficients['ASDYes',4]
  coef[i] <- summary(reg)[[i]]$coefficients['ASDYes',1]
  
}

cbind(pval, coef)

# Sans relatives
df_tca_td<- data.frame(ROI = vol_cols, pval)

pval_vols <- pval


df_tca_td_thick <- data.frame(ROI = vol_cols, pval)
pval_thick <- pval

adj_p <- p.adjust(c(pval_thick, pval_vols), method = "bonferroni", n = length(c(pval_vols, pval_thick)))

tca_td <- rbind(df_tca_td_thick, df_tca_td)

tca_td['adj_p'] <- adj_p
tca_td[,'sign'] = 'Non-signif'
tca_td[tca_td$adj_p <= 0.05, 'sign'] = 'adj-p < 0.05'


write.csv(df_tca_td, 'coef_tca_vol.csv')
# PR
df_td_asd <- data.frame(ROI = vol_cols, pval, coef)
pval_vols <- pval

df_asd_td_thick <- data.frame(ROI = vol_cols, pval, coef)
pval_thick <- pval

adj_p <- p.adjust(c(pval_thick, pval_vols), method = "bonferroni", n = length(c(pval_vols, pval_thick)))

asd_td <- rbind(df_asd_td_thick, df_td_asd)
df_td_asd
asd_td['adj_p'] <- adj_p
asd_td[,'sign'] = 'Non-signif'
asd_td[asd_td$adj_p <= 0.05, 'sign'] = 'adj-p < 0.05'





double_df <- data.frame("TCA" = tca_td$sign,
                        "ASD" = asd_td$sign)

double_df[(double_df$TCA == 'Non-signif' & double_df$ASD == 'Non-signif'), 'Sum'] = 1
double_df[(double_df$TCA == 'adj-p < 0.05' & double_df$ASD == 'Non-signif'), 'Sum'] = 4
double_df[(double_df$TCA == 'Non-signif' & double_df$ASD == 'adj-p < 0.05'), 'Sum'] = 3
double_df[(double_df$TCA == 'adj-p < 0.05' & double_df$ASD == 'adj-p < 0.05'), 'Sum'] = 2

plot(x = df_tca_td$coef, y = df_td_asd$coef, col = double_df$Sum,
     xlab = 'TCA',  ylab =  'ASD')
text(df_tca_td$coef, df_td_asd$coef, vol_cols, cex = 0.7,
     col = double_df$Sum)

legend(x = 'topright', legend=c('TCA', "ASD", "Both"),
       col=c(4, 3,2), lty = 1, cex=0.8)
cor.test(df_tca_td$coef, df_td_asd$coef,method = 'pearson')
table(df$ASD)
df_asd_td