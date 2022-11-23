df <- read.table('df_tsa_tca.csv', header = TRUE, sep = ',')
df <- read.table('df_aims.csv', header = TRUE, sep = ',')
df <- read.table('abide.csv', header = TRUE, sep = ',')
df
plot(df[, c('age_at_scan', 'meanCT', 'EstimatedTotalIntraCranialVol','totalSA', 'CSF' )])
df["ASD"][df["ASD"] == "TCA_pr"] <- "TCA"
df["ASD"][df["ASD"] == "TCA_ac"] <- "TCA"

df <- df[df$ASD == 'TCA' 
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

# Remove NA from weight 

# df<- df[complete.cases(df$patient_weight), ]

# Encode as factor

df$Sex <- as.factor(df$Sex)
df$ASD <- as.factor(df$ASD)

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

vol_cols = c('meanCT', 'totalSA', 'EstimatedTotalIntraCranialVol', 'CSF'
             ,'GreyMatter',
             'WhiteMatter'
             )
length(vol_cols)
# Normalise values for each ROIs

for (i in vol_cols){
  df[, i] <- ((df[, i] - mean(df[df$ASD == 'No', i]))/  sd(df[df$ASD == 'No', i]) )
}

df[, vol_cols]
# Perform linear regression
reg <- lm(cbind(meanCT, totalSA, EstimatedTotalIntraCranialVol, CSF, GreyMatter, WhiteMatter)~Sex+scanner+Protocole+age_at_scan^2+age_at_scan,
          data=df)
# Visualise outputs

summary(reg)
# Encode outputs into dataframe

# ASDYes
# ASDTCA
coef <- c()

pval <- c()
for (i in 1:length(vol_cols)){
  pval[i] <- summary(reg)[[i]]$coefficients['patient_weight',4]
  coef[i] <- summary(reg)[[i]]$coefficients['patient_weight',1]
  
}

cbind(pval, coef)

adj_p <- p.adjust(pval, method = "bonferroni", n = length(pval))

# Sans relatives
df_tca_td<- data.frame(ROI = vol_cols, pval, coef, adj_p)
df_tca_td[,'sign'] = 'Non-signif'
df_tca_td[df_tca_td$adj_p <= 0.05, 'sign'] = 'adj-p < 0.05'


write.csv(df_tca_td, 'coef_tca_vol.csv')
# PR
df_td_asd <- data.frame(ROI = vol_cols, pval, coef, adj_p)
df_td_asd[,'sign'] = 'Non-signif'
df_td_asd[df_td_asd$adj_p <= 0.05, 'sign'] = 'adj-p < 0.05'

barplot(coef ~ ROI, data = df_tca_td, horiz =  TRUE)
write.csv(df_td_asd, 'coef_asd_vol.csv')


double_df <- data.frame("TCA" = df_tca_td$sign,
                        "ASD" = df_td_asd$sign)

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