[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2474420.svg)](https://doi.org/10.5281/zenodo.2474420)

# marshal-pipeline
How to use MARSHAL from the R environment

[The full text PDF can be found here](https://github.com/MARSHAL-ROOT/marshal-pipeline/blob/master/PipelineMARSHAL.pdf)

## Install MARSHAL

MARSHAL is a R package that can be loaded with the following line of code :
```{r echo=TRUE, eval=FALSE}
install.packages("devtools")
library(devtools)
install_github("MARSHAL-ROOT/marshal")
library(marshal)
```

## Load or create MARSHAL input

### 1. Soil water potential along the depth profile

![](www/fig/soil.PNG)

### 2. Root type conductivities (e.g. Doussan et al., 1998)

![](www/fig/conductivities.PNG)

### 3. Water pressure head at the collar

`-15.000 hPa`

### 4. Root system architecture (CRootBox)

#### Run [CRootBox](https://plant-root-soil-interactions-modelling.github.io/CRootBox/)

The "[a](https://github.com/MARSHAL-ROOT/marshal-pipeline/tree/master/17_06%20CRootBox)" executer

![](www/fig/RSA.PNG)

```{r, echo = TRUE, eval = FALSE}
source("R/io_function.R")

all_roots <- NULL
for(param_name in param_choise){
  # Load param of the selected example
  rparam <- read_rparam(path = paste0("./www/", param_name, ".rparam"))
  pparam <- read_pparam(path = paste0("./www/", param_name, ".pparam"))
  # Overwrite the param files that will be use
  write_rparam(rparam, "./www/param.rparam")
  write_pparam(pparam, "./www/param.pparam")
  
  for(i in c(1:nrep)){
  ################## 
  #  Run CRootBox  #
  ##################
  system("www/a.exe")

  # Get all the .txt files exported by CRootBox
  fls <- list.files("./")
  fls <- fls[grepl("rootsystem.txt", fls)]
  
  for(f in fls){
    temp <- fread(f, header = T)
    temp$age <- strsplit(f, "_")[[1]][1]
    temp$rep <- i
    temp$ex <- param_name
    # Save the temporary data in a permanent file to use.
    all_roots <- rbind(all_roots, temp)
    }
  }
}

```

## Run MARSHAL

`getSUF` function to add hydraulic macro-properties on a root system architecture

## output
### Krs

![](www/fig/Krs.PNG)

### Transpiration (Tpot \& Tact)

In wet condition
![](www/fig/transpi.PNG)

### SUF, Kr \& Kx

Leitner 2010
![](www/fig/suf_l0.PNG)
Postma 2011
![](www/fig/suf_p1.PNG)
Leitner 2014
![](www/fig/suf_l4.PNG)

### Radial and Axial water fluxes

![](www/fig/radial.PNG)
![](www/fig/radial2.PNG)

### water pressure head at the soil-root interface

![](www/fig/psi.PNG)

# [Source code](https://github.com/MARSHAL-ROOT/marshal-pipeline/blob/master/R/PipelineMARSHAL.Rmd) 
