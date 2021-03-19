#README.md File for data discription using R

#R_Assignment_SM

##Data Inspection

###Attributes of `fang_et_al_genotypes.txt`

```
typeof(fang_et_al_genotypes)
dim(fang_et_al_genotypes)
sapply(fang_et_al_genotypes, class)
names(fang_et_al_genotypes)
file.info(list.files(path = "/Users/Sean McLaughlin/Documents/GitHub/R_Assignment_SM/Data", full.names = TRUE))
```

By inspecting this file I learned that:

1. Fang et al contains a heterogeneous data structure type called a list.
2. The data type it contains is all character data in each row
3. The file contains 2782 columns and 986 rows
4. The file is approximately 11054722 bits in file size

###Attributes of `snp_position.txt`

```
typeof(snp_position)
dim(snp_position)
sapply(snp_position, class)
names(snp_position)
file.info(list.files(path = "/Users/Sean McLaughlin/Documents/GitHub/R_Assignment_SM/Data", full.names = TRUE))
```

By inspecting this file I learned that:

1. snp positions is a text file that contains a list data structure type
2. The data type it contains is all character data in each row
3. There are 983 rows and 15 columns in the file
4. File size is 83747 bytes

##Data Processing

###Maize Data
Formatting Data
```
fang.trans <- t(fang_et_al_genotypes)
Merged.data <- ??merge(fang.trans, snp_position)??

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

This code is designed to look through the merged data from fang et al and snp position text files to filter out only maize and teosinte data. Following the assignment instructions, further filtering is used to isolate specific chromosomes from the data as indicated by integer 1. After that the data is sorted by increasing position number values as indicated by the sort command. Lastly a Text file is outputted which can be modified to fit each chromosome by either manually inputting chromosome values, or developing a nested loop that automatically created the requisite text files.

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

In these various plots I seek out to display the fang et al and snp positions data in a more understanding and intuitive way. We use the command ggplot and its various options to show the distribution SNPs throughout the chromosome and integrating homo/heterozygous seen in the missing data and heterozygosity part. Lastly I have decided to look at how position values match up with their associated chromosomes. This seems like it could make for an interesting visualization.





