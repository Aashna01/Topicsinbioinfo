#Clustering method for class discovery
library(gplots)

data<-read.table("/Users/angelina_volkova/Desktop/Topics_BMI/ALL_AML_dataset.txt",header=TRUE, sep="\t")
data_test<-read.table("/Users/angelina_volkova/Desktop/Golub_Test_Normalized.txt",header=TRUE, sep="\t")
data_training<-t(as.data.frame(data))
rownames(data_test) <- c("ALL_39", "ALL_40", "ALL_42", "ALL_47", "ALL_48", "ALL_49", "ALL_41", 
                     "ALL_43", "ALL_44", "ALL_45",  "ALL_46", "ALL_70", "ALL_71","ALL_72", "ALL_68",
                     "ALL_69", "ALL_67", "ALL_55", "ALL_56", "ALL_59", "AML_52","AML_53", "AML_51", "AML_50",
                     "AML_54", "AML_57", "AML_58", "AML_60", "AML_61", "AML_65", "AML_66", "AML_63", "AML_64",
                     "AML_62")

dist_training=as.dist(1-cor(t(data_training)))
hc_tr=hclust(dist_training, method="complete")
plot(hc_tr, main="Golub Training Data \n Complete Linkage with Correlation-Based Distance", xlab="", sub="")
rect.hclust(hc_tr, k = 4, border = 2:5) # add rectangle

dist_test=as.dist(1-cor(t(data_test)))
hc_test=hclust(dist_test, method="complete")
plot(hc_test, main="Golub Test Data \n Complete Linkage with Correlation-Based Distance", xlab="", sub="")
rect.hclust(hc_test, k = 2, border = 2:5) # add rectangle

#Heatmap with 50 genes
genes <- c("X82240_rna1_at", "X76223_s_at","M89957_at", "L33930_s_at", "M16336_s_at", "M28826_at",
           "K01911_at", "S76617_at", "D88270_at","Z49194_at", "U89922_s_at", "X00437_s_at",
           "X12530_s_at", "X59871_at", "M27394_s_at", "X07203_at", "X67325_at", "M21624_at",
           "M54992_at", "M29474_at", "U16954_at", "U05259_rna1_at", "X04145_at",
           "U46006_s_at", "X99920_at", "D31764_at", "X58072_at","M22489_at", "U90546_at",
           "L00058_at", "M28170_at", "M11722_at", "X66945_at", "U33822_at", "M83233_at",
           "U52682_at", "X15573_at", "X52142_at", "J04132_at", "D63391_at", "U36922_at",
           "X55740_at", "U23852_s_at", "Z69881_at", "M92934_at", "L36983_at", "M58286_s_at",
            "HG4582.HT4987_at", "X66533_at")

training_map <-subset(data_training, select=genes)
test_map <- subset(data_test, select=genes)
heatmap(training_map, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column",margins=c(8,8), 
        main="\n Training Data Set")

heatmap(as.matrix(test_map), Rowv=NA, Colv=NA, col = heat.colors(256), scale="column",margins=c(8,8), 
        main="\n Test Data Set")

"X62891_s_at",