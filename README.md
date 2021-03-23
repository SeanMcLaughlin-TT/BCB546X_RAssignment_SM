# Sean's assignment reviewed by Ashenafi Beyi
# Date: March 23, 2021

---
  title: "Sean-Assignment"
author: "Ashenafi Beyi"
date: "3/23/2021"
output: html_document
---

  #  Import genotypes and snp_positions data from repository
fang_et_al_genotypes <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/master/assignments/UNIX_Assignment/fang_et_al_genotypes.txt",
                             header=T, sep="\t", stringsAsFactors =F)
dim(fang_et_al_genotypes) # 2782 observations/rows and 986 variables/columns

snp_position <- read.table("https://raw.githubusercontent.com/EEOB-BioData/BCB546-Spring2021/master/assignments/UNIX_Assignment/snp_position.txt",
                           header=T, sep="\t", stringsAsFactors =F)
dim(snp_position) # 983 observations or rows and 15 variables or columns
head(snp_position) 
  
##Data Inspection

# Select SNP_ID, chromosome and Position from SNP_position data set
library(dplyr)
snp_position.selected <- select(snp_position, c("SNP_ID", "Chromosome", "Position"))
str(snp_position.selected) # 983 rows and 3 columns


# Number of rows and columns in each data file
nrow(fang_et_al_genotypes) # 2782
ncol(fang_et_al_genotypes) # 986
NROW(na.omit(fang_et_al_genotypes)) # 2782


# Summarize the frequencies of maize and teaosinte genotypes
# Three approaches/function are used: levels, summary, and table
summary(select(fang_et_al_genotypes, Group))
levels(as.factor(fang_genotypes$Group)) # 15 groups: "TRIPS" "ZDIPL" "ZLUXR" "ZMHUE" "ZMMIL" "ZMMLR" 
# "ZMMMR" "ZMPBA" "ZMPIL" "ZMPJA" "ZMXCH" "ZMXCP" "ZMXIL" 
# "ZMXNO" "ZMXNT" "ZPERR"
summary(as.factor(fang_et_al_genotypes$Group))
# TRIPS ZDIPL ZLUXR ZMHUE ZMMIL ZMMLR ZMMMR ZMPBA ZMPIL ZMPJA ZMXCH ZMXCP ZMXIL ZMXNO ZMXNT ZPERR 
# 22    15    17    10   290  1256    27   900    41    34    75    69     6     7     4     9 


# Summarize frequencies of chromosomes in snp_position data file
summary(as.factor(snp_position$Chromosome))
# chrosome 1 = 155, 2 =127, 3=107, 4=91, 5=122,  6=76, 7=97, 8=62, 9=60, 10=53, multiple=6, unknown=27 

# Data subsetting
#......Maize genotypes....................................
#Subset data 3 maize genotypes > transpose > merge with snp data
maize_genotypes <- filter(fang_et_al_genotypes, Group == 'ZMMIL' | Group == 'ZMMLR' | Group == 'ZMMMR')
dim(maize_genotypes) # 1573 rows/observations  986 columns/variables
#  head(maize_genotypes, 10) # top ten rows
levels(as.factor(maize_genotypes$Group)) # "ZMMIL" "ZMMLR" "ZMMMR"
summary(as.factor(maize_genotypes$Group)) # ZMMIL = 290, ZMMLR = 1256, ZMMMR = 27 

#Transpose
library(tidyverse)
# reading: https://tibble.tidyverse.org/reference/rownames.html
maize_genotypes <- column_to_rownames(maize_genotypes, var = "Sample_ID")
maize_genotypes.tr <- t(maize_genotypes)%>%as.data.frame()%>%rownames_to_column(., var = "SNP_ID")
maize_genotypes.tr <- maize_genotypes.tr[3:nrow(maize_genotypes.tr),]

maize_snp <- merge(snp_position.selected, maize_genotypes.tr, by="SNP_ID")

maize_snp <- select(maize_snp, SNP_ID, Chromosome, Position, everything())

dim(maize_snp) # 983 1576

summary(as.factor(maize_snp$Chromosome))


#......Teosinte genotypes....................................
# Subset data 3 teosinte genotypes > transpose > merge with snp data

teosinte_genotypes <- filter(fang_et_al_genotypes, Group == 'ZMPBA' | Group == 'ZMPIL' | Group == 'ZMPJA')
dim(teosinte_genotypes) # 975 rows/observations and 986 columns/variables
head(teosinte_genotypes)
levels(as.factor(teosinte_genotypes$Group)) # "ZMPBA" "ZMPIL" "ZMPJA"
summary(as.factor(teosinte_genotypes$Group)) # ZMPBA = 900, ZMPIL = 41, ZMPJA = 34

#Transpose
teosinte_genotypes <- column_to_rownames(teosinte_genotypes, var = "Sample_ID")
teosinte_genotypes.tr <- t(teosinte_genotypes)%>%as.data.frame()%>%rownames_to_column(., var = "SNP_ID")
teosinte_genotypes.tr <- teosinte_genotypes.tr[3:nrow(teosinte_genotypes.tr),]

teosinte_snp <- merge(snp_position.selected, teosinte_genotypes.tr, by="SNP_ID")

teosinte_snp <- select(teosinte_snp, SNP_ID, Chromosome, Position, everything())

dim(teosinte_snp) # 983 978


#.........subset joined data set by 'Chromose' and order them by 'position'.....

# Subset data by chromosome, order in increasing position
maize_chromosome1 <- subset(maize_snp, Chromosome==1)%>%arrange(Position)
teosinte_chromosome1 <- subset(teosinte_snp, Chromosome==1)%>%arrange(Position)

#Subset each chromosome, order in position decreasing order, replace missed values
maize_chromosome.dec1 <- subset(maize_snp, Chromosome==1)%>%arrange(desc(Position))
teosinte_chromosome.dec1 <- subset(teosinte_snp, Chromosome==1)%>%arrange(desc(Position))


# Plots
#.......................................
fang_genotypes1 <- column_to_rownames(fang_et_al_genotypes, var = "Sample_ID")
fang_genotypes.tr <- t(fang_genotypes1)%>%as.data.frame()%>%rownames_to_column(., var = "SNP_ID")
fang_genotypes.tr <- fang_genotypes.tr[3:nrow(maize_genotypes.tr),]

joined_snp_genotypes <- merge(snp_position, fang_genotypes.tr, by= "SNP_ID")

# Chromosome counts
chromosome_count <- ggplot(joined_snp_genotypes, aes(x= Chromosome) ) + 
  geom_histogram(stat= "Count", color = "blue", fill = "green") + ggtitle ("Number of SNPs per chromosome")

chromosome_count + theme(plot.title = element_text(color = "blue", size = 12, face = "bold", hjust = 0.5))

# SNP distribution across chromosomes

chromosome_snp <- ggplot(joined_snp_genotypes, aes(x= Chromosome, y= Position))+ 
  geom_point(stat=, color = "red", alpha= 0.01)+ ggtitle ("SNPs distribution per chromosome")

chromosome_snp + theme(plot.title = element_text(color = "green", size = 12, face = "bold", hjust = 0.5))











  
  ```
###Various Code not working as of yet for merging data frames###
###Please give me Ideas##

Chromosome Outputs
```
Comp.data.1.I <- Merged.data %>%
  filter(Merged.data, c(ZMMIL,ZMMLR,ZMMMR))%>%
  filter(Merged.data$Chromosomes int = 1)%>%
  sort(Merged.data$Position, increasing = TRUE)%>%
  is.na(Merged.data) <- '?'
write.table(Comp.data.1.I, "Comp_data_1_I.txt", sep=";",col.names = FALSE, row.names = FALSE)
##Repeat for each Chromosome 1-10  

```
```
Comp.data.1.D <- Merged.data %>%
  filter(Merged.data, c(ZMMIL,ZMMLR,ZMMMR))%>%
  filter(Merged.data$Chromosomes int = 1)%>%
  sort(Merged.data$Position, increasing = False)%>%
  is.na(Merged.data) <- '-'
write.table(Comp.data.1.D, "Comp_data_1_D.txt", sep=";",col.names = FALSE, row.names = FALSE)

```

###Teosinte Data

```
Comp.data.1.I <- Merged.data %>%
  filter(Merged.data, c(ZMPBA,ZMPIL,ZMPJA))%>%
  filter(Merged.data$Chromosomes int = 1)%>%
  sort(Merged.data$Position, increasing = TRUE)%>%
  is.na(Merged.data) <- '?'
write.table(Comp.data.1.I, "Comp_data_1_I.txt", sep=";",col.names = FALSE, row.names = FALSE)
```
Repeat for each Chromosome 1-10
```
Comp.data.1.D <- Merged.data %>%
  filter(Merged.data, c(ZMPBA,ZMPIL,ZMPJA))%>%
  filter(Merged.data$Chromosomes int = 1)%>%
  sort(Merged.data$Position, increasing = False)%>%
  is.na(Merged.data) <- '-'
write.table(Comp.data.1.D, "Comp_data_1_D.txt", sep=";",col.names = FALSE, row.names = FALSE)
```
Repeat for each Chromosome 1-10


##Workflow Walkthrough Part 1

This code is designed to look through the merged data from fang et al and snp position text files to 
# filter out only maize and teosinte data. Following the assignment instructions, further filtering 
# is used to isolate specific chromosomes from the data as indicated by integer 1. After that the data 
# is sorted by increasing position number values as indicated by the sort command. Lastly a Text file is 
# outputted which can be modified to fit each chromosome by either manually inputting chromosome values, 
# or developing a nested loop that automatically created the requisite text files.

###Part II

##SNPs per Chromosome
```
ggplot(data = snp_position) + geom_point(mapping=aes(x = Chromosome, y = (count(SNP_ID))
                                                     
ggplot(data = snp_position) + geom_point(mapping=aes(x = c(Chromosome, SNP_ID), y = [1:10]
```
##Missing data and amount of heterozygousity
```
Merged.data.HTHO <- Merged.data %>%
mutate(Merged.data c(A/A, C/C, G/G, T/T)) <- HO %>%
mutate(Merged.data c(A/T, T/A, C/G, G/C)) <- Ht %>%

ggplot(data = Merged.data.HTHO) + geom_density(mapping = aes(x = c(HO, HT), linetype = GC.binned), alpha = 0.5)


```
##Own Visualization

ggplot(data = snp_position) + geom_point(mapping=aes(x = Chromosome, y = Position)

##Workflow Walkthrough

In these various plots I seek out to display the fang et al and snp positions data in a more understanding and 
# intuitive way. We use the command ggplot and its various options to show the distribution SNPs throughout 
# the chromosome and integrating homo/heterozygous seen in the missing data and heterozygosity part. 
# Lastly I have decided to look at how position values match up with their associated chromosomes. 
# This seems like it could make for an interesting visualization.





