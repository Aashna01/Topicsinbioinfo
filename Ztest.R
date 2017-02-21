source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
biocLite("multtest")
library("multtest")
library("qvalue")

dataset = as.data.frame(t(read.table('ALL_AML_dataset.txt', header = T)))
all = dataset[1:27,]
aml = dataset[28:38,]
all_mean = apply(all, 2, mean)
aml_mean = apply(aml, 2, mean)

foldchange = log(aml_mean/all_mean, 2)
hist(foldchange, n = 100, main = "histogram of fold change AML/ALL", xlab = "log2FC")

zscore = (foldchange - mean(foldchange)) / (sd(foldchange))
p_raw = 2* pnorm(zscore)
mt = mt.rawp2adjp(p_raw, proc=c("Bonferroni","Holm","BH"))
head(mt)

sig_gene_idx = mt$index[1:200]
sig_genename = colnames(dataset)[sig_gene_idx]
write.table(sig_genename, 'DE_ranked_pvalue.txt', quote = F)

