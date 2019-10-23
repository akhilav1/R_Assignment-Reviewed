---
title: "AKhil_R_Assignment"
author: "Akhil"
date: "10/23/2019"
output: html_document
---

# RProject

# Part 1

## Prep workspace
```{r}
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("gtools")) install.packages("gtools")
library(gtools)
if (!require("readr")) install.packages("readr")
library(readr)
```

## for work files
```{r}
SNP <- read_tsv("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2019/master/assignments/UNIX_Assignment/snp_position.txt")
Fang <- read_tsv("https://raw.githubusercontent.com/EEOB-BioData/BCB546X-Fall2019/master/assignments/UNIX_Assignment/fang_et_al_genotypes.txt")

#For directories 
```{r}
dir.create("Maize_data")
dir.create("Teosinte_data")
dir.create("graphs_data")


  

##Data Inspection 
```{r}
##Fang
#For Rows
```{r}
nrow(Fang)
## 2782
#For columns
```{r}
ncol(Fang)
## 986
#memory usage in R
```{r}
Object_size(Fang)
## 22.6 MB

## Snp
#For Rows
```{r}
nrow(SNP)
## 983
#For columns
```{r}
ncol(SNP)
## 15
#memory usage in R
```{r}
object_size(SNP)
## 320 KB

##Data Analysis
  
# For Keeping columns 1, 3, and 4 only from SNP data
```{r}
Snp1 = select(SNP, 1,-2,3,4)

#For separate files for Maize and Teosinte based on group name, and remove extra columns and then transpose
```{r}
Maize_ <- filter(Fang, Group %in% c("ZMMLR", "ZMMMR", "ZMMIL"))
Teosinte_ <- filter(Fang, Group %in% c("ZMPBA", "ZMPIL", "ZMPJA"))
col_Maize=select(Maize_, -1,-2,-3,4:986)
col_Teosinte=select(Teosinte_, -1,-2,-3,4:986)

#Transpose 
```{r}
transposed_maize <- t(col_Maize)
transposed_Teosinte <- t(col_Teosinte)

#Join files
```{r}
Join_Maize <- cbind(Snp1, transposed_maize)
Join_Teosinte <- cbind(Snp1, transposed_Teosinte)
# After adding the above code,  new copies of Snp1 i.e 3 columns formed. Hence cutting the columns
Join_Maize <- Join_Maize[,-(4:6)]
Join_Teosinte  <- Join_Teosinte [,-(4:6)]

## Replace "?/?" to "?"
```{r}
Join_Maize <- data.frame(lapply(Join_Maize, as.character), stringsAsFactors=FALSE)
Join_Maize <- data.frame(sapply(Join_Maize,function(x) {x <- gsub("?/?","?",x,fixed=TRUE)}))
Join_Teosinte <- data.frame(lapply(Join_Teosinte, as.character), stringsAsFactors=FALSE)
Join_Teosinte<- data.frame(sapply(Join_Teosinte,function(x) {x <- gsub("?/?","?",x,fixed=TRUE)}))

#Increasing position values
```{r}
for (i in 1:10) {
  maize <- filter(Join_Maize, Chromosome == i)
  maize <- arrange(maize, Position)
  outpath <- "C:\\Users\\akhil\\Desktop\\R\\New folder\\R-Assignment-Updated\\Maize_data\\"
  nam <- sapply(
    names(maize),function(x){
      paste("maize_in", i, ".csv", sep='')
    })
  out_filePath <- sapply(nam, function(x){
    paste(outpath, x, sep='/')})
  write.csv(maize, file=out_filePath[i])
}
  
for (i in 1:10) {
  Teosinte <- filter(Join_Teosinte, Chromosome == i)
  Teosinte <- arrange(Teosinte, Position)
  outpath <- "C:\\Users\\akhil\\Desktop\\R\\New folder\\R-Assignment-Updated\\Teosinte_data\\"
  nam <- sapply(
    names(Teosinte),function(x){
      paste("Teosinte_in", i, ".csv", sep='')
    })
  out_filePath <- sapply(nam, function(x){
    paste(outpath, x, sep='/')})
  write.csv(Teosinte, file=out_filePath[i])
}

#replace "?" to "-"
```{r}
Join_Maize2 <- data.frame(lapply(Join_Maize, as.character), stringsAsFactors=FALSE)
Join_Maize2 <- data.frame(sapply(Join_Maize2,function(x) {x <- gsub("?","-",x,fixed=TRUE)}))
Join_Teosinte2 <- data.frame(lapply(Join_Teosinte, as.character), stringsAsFactors=FALSE)
Join_Teosinte2<- data.frame(sapply(Join_Teosinte2,function(x) {x <- gsub("?","-",x,fixed=TRUE)}))

#Decreasing postion valuesposition values
```{r}
for (i in 1:10) {
  maize2 <- filter(Join_Maize2, Chromosome == i)
  maize2 <- arrange(maize, desc(Position))
  outpath <- "C:\\Users\\akhil\\Desktop\\R\\New folder\\R-Assignment-Updated\\Maize_data\\"
  nam <- sapply(
    names(maize2),function(x){
      paste("maize_decreasing", i, ".csv", sep='')
    })
  out_filePath <- sapply(nam, function(x){
    paste(outpath, x, sep='/')})
  write.csv(maize, file=out_filePath[i])
}

for (i in 1:10) {
  Teosinte2 <- filter(Join_Teosinte2, Chromosome == i)
  Teosinte2 <- arrange(Teosinte2, desc(Position))
  outpath <- "C:\\Users\\akhil\\Desktop\\R\\New folder\\R-Assignment-Updated\\Teosinte_data\\"
  nam <- sapply(
    names(Teosinte),function(x){
      paste("Teosinte_decreasing", i, ".csv", sep='')
    })
  out_filePath <- sapply(nam, function(x){
    paste(outpath, x, sep='/')})
  write.csv(Teosinte, file=out_filePath[i])
}

## Graph

install.packages("tidyverse")
library(tidyverse)
install.packages("reshape2")
library(reshape2)

##Melt maize and teosinte files.
```{r}
melt(Join_Maize, na.rm = T,)
melt(Join_Teosinte, na.rm = T,)
# SNPS per chromosome
```{r}
ggplot(data = Join_Maize) + geom_bar(mapping = aes(x = Join_Maize$Chromosome)) + xlab(label = "Chromosome") + ylab(label = "Number of SNPs")
ggplot(data = Join_Teosinte) + geom_bar(mapping = aes(x = Join_Teosinte$Chromosome)) + xlab(label = "Chromosome") + ylab(label = "Number of SNPs")
# Density
ggplot(Join_Maize, aes(x= Chromosome))+
  geom_density(color="darkblue", fill="lightblue")
ggplot(Join_Teosinte, aes(x= Chromosome))+
  geom_density(color="darkblue", fill="lightblue")

#My Visulaization-Scatter plot
```{r}
plot(x=Join_Maize$Chromosome)
plot(x=Join_Teosinte$Chromosome) 


