library(data.table)
library(pscl)
library(ggplot2)
library(openxlsx)


#prepare covariates and regional information
#data_all <- read.xlsx("biobank_data_individual_district.xlsx")
data_all$region <- factor(data_all$region,levels = c("North", "Central", "South", "East"))
region_map <- unique(data_all[, .(IID, region)])

covar_new <- fread("final_remove_o_gwas_pca_5_1%_age_sex.txt")
colnames(covar_new) <- c("FID", "IID", "PC1", "PC2", "PC3","PC4", "PC5", "sex", "age")
covar_new <- merge(covar_new,region_map,by = "IID",all.x = TRUE,sort = FALSE)
#create region covariates for PLINK GWAS
data_all$region <- factor(data_all$region,levels = c("North", "Central", "South", "East"))
covar_new$regionCentral <- ifelse(covar_new$region == "Central", 1, 0)
covar_new$regionSouth   <- ifelse(covar_new$region == "South", 1, 0)
covar_new$regionEast    <- ifelse(covar_new$region == "East", 1, 0)
covar_plink <- covar_new[, .(FID,IID,PC1,PC2,PC3,PC4,PC5,sex,age,regionCentral,regionSouth,regionEast)]
#fwrite(covar_plink,"final_remove_o_gwas_pca_5_1%_age_sex_region.txt",sep = "\t",na = "-9")


#PLINK command:
#ren "final_remove_o_gwas_pca_5_1%_age_sex_region.txt" covar_region.txt
#plink.exe --bfile final_100_remove_o --covar covar_region.txt --covar-name PC1,PC2,PC3,PC4,PC5,sex,age,regionCentral,regionSouth,regionEast --logistic --out final_logistic_pca_age_sex_region_1

#Identify significant SNPs
assoc_new <- fread("final_logistic_pca_age_sex_region_1.assoc.logistic")
covar_new <- fread("covar_region.txt")
assoc_add_new <- assoc_new[TEST == "ADD" & !is.na(P)]
top_snps_new <- assoc_add_new[P < 1e-5]
#fwrite(top_snps_new,"top28_snps_region_adjusted_P1e-5.csv")
#writeLines(top_snps_new$SNP, "top28_snps_region_adjusted.txt")

#extract significant SNP genotypes
#PLINK command:
#plink --bfile final_100_remove_o --extract top28_snps_region_adjusted.txt --recode A --out top28_snps_region_adjusted

snps_new <- fread("top28_snps_region_adjusted.raw")
geno_new <- snps_new[, 7:34]

#correlation analysis 
cor_new <- cor(geno_new,use = "pairwise.complete.obs",method = "pearson")
cor_long_new <- as.data.table(as.table(cor_new))
setnames(cor_long_new, c("SNP1", "SNP2", "r"))
cor_long_new <- cor_long_new[SNP1 != SNP2]
cor_long_new <- cor_long_new[abs(r) > 0.8]
cor_long_new <- cor_long_new[match(SNP1, colnames(cor_new)) < match(SNP2, colnames(cor_new))]
cor_long_new[order(-abs(r))]
snp_cols_new <- colnames(geno_new)
parent_new <- setNames(snp_cols_new, snp_cols_new)
find_parent_new <- function(x) {
  while (parent_new[x] != x) {
    x <- parent_new[x]
  }
  return(x)
}

union_sets_new <- function(a, b) {
  pa <- find_parent_new(a)
  pb <- find_parent_new(b)
  if (pa != pb) {
    parent_new[pb] <<- pa
  }
}

for (i in 1:nrow(cor_long_new)) {
  union_sets_new(
    cor_long_new$SNP1[i],
    cor_long_new$SNP2[i])
}
groups_new <- data.table(SNP = snp_cols_new,GROUP = sapply(snp_cols_new, find_parent_new))
groups_new <- merge(groups_new,assoc_add_new[, .(SNP, P)],by = "SNP",all.x = TRUE)
groups_new[, SNP_ID := sub("_[ATCG]+$", "", SNP)]
groups_new <- merge(groups_new,assoc_add_new[, .(SNP_ID = SNP, P)],by = "SNP_ID",all.x = TRUE)
best_snps_new <- groups_new[order(GROUP, P.y),.SD[1],by = GROUP]
best_snps_new[, .(GROUP, SNP, P.y)]

#prepare data for multivaraible SNP analysis
covar_r <- merge(covar_new,region_map,by = "IID",all.x = TRUE)
covar_r <- covar_r[, .(IID, PC1, PC2, PC3, PC4, PC5,sex, age, region)]
data_final_new <- merge(snps_new,covar_r,by = "IID",all = FALSE)
best_snp_cols_new <- sapply(
  best_snps_new$SNP_ID,
  function(x) {
    hits <- grep(paste0("^", x, "_"), colnames(snps_new), value = TRUE)
    if (length(hits) != 1) return(NA_character_)
    hits
  }
)
model_vars_new <- c("PHENOTYPE",best_snp_cols_new,"PC1", "PC2", "PC3", "PC4", "PC5","age", "sex", "region")
data_model_new <- data_final_new[PHENOTYPE != -9]
complete_model_new <- complete.cases(data_model_new[, ..model_vars_new])
data_glm_new <- data_model_new[complete_model_new]
data_glm_new[, dengue := ifelse(PHENOTYPE == 2, 1, 0)]
data_glm_new[, region := factor(region,levels = c("North", "Central", "South", "East"))]

#multi-snp regression
formula_new <- as.formula(paste("dengue ~",
    paste(best_snp_cols_new, collapse = " + "),"+ PC1 + PC2 + PC3 + PC4 + PC5 + age + sex + region"))

model_new <- glm(formula_new,data = data_glm_new,family = binomial)
coef_summary <- summary(model_new)$coefficients

result_new <- data.table(
  Variable = rownames(coef_summary),
  Beta = coef_summary[, "Estimate"],
  SE = coef_summary[, "Std. Error"],
  OR = exp(coef_summary[, "Estimate"]),
  CI_lower = exp(coef_summary[, "Estimate"] -1.96 * coef_summary[, "Std. Error"]),
  CI_upper = exp(coef_summary[, "Estimate"] +1.96 * coef_summary[, "Std. Error"]),
  P = coef_summary[, "Pr(>|z|)"])
result_new[, Sig := ifelse(P < 0.05, "Yes", "No")]

pR2(model_new) #McFadden pseudo-R2 

#Bonferroni correction
results_pruned_new <- result_new[Variable %in% best_snp_cols_new]
results_pruned_new[, P_Bonferroni := p.adjust(P,method = "bonferroni")]
results_pruned_new[, Bonf_sig := ifelse(P_Bonferroni < 0.05,"Yes","No")]


#Supplementary figure 7: significant SNPs
sig_snps_new <- results_pruned_new[Bonf_sig == "Yes"]
sig_snps_new[, `:=`(logOR = Beta,CI_low = Beta - 1.96 * SE,CI_high = Beta + 1.96 * SE)]
sig_snps_new[, SNP_label := sub("_.*", "", Variable)]
supp_fig_7 <- ggplot(sig_snps_new,aes(x = reorder(SNP_label, logOR), y = logOR)) +
  geom_col(fill = "#2C5F8A") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(x = "SNP",y = "Log(OR)")

#Supplementary table S4
bim <- fread("final_100_remove_o.bim", header = FALSE)
colnames(bim) <- c("CHR", "SNP", "cM", "BP", "A1", "A2")
gwas_new <- copy(results_pruned_new)
gwas_new[, SNP_clean := sub("_.*", "", Variable)]
gwas_merge_new <- merge(gwas_new,bim[, .(SNP, CHR, BP, A1, A2)],by.x = "SNP_clean",by.y = "SNP",all.x = TRUE)
supp_table_new <- gwas_merge_new[, .(SNP = Variable,CHR,BP,A1,A2,OR,CI_low = CI_lower,CI_high = CI_upper,P,P_Bonferroni,Bonf_sig)]
fwrite(supp_table_new,"Supplementary_Table_S4.csv")


