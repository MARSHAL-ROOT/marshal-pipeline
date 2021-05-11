# -----------------------
# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
#Disclaimer
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.
#---------------

# FUNCTION TO READ THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME
read_rparam <- function(path){

  fileName <- path
  param <- read_file(fileName)
  
  
  param <- strsplit(param, "#")
  dataset_init <- NULL
  for(k in c(2:length(param[[1]]))){
    spl <- strsplit(param[[1]][k], "\n")
    type <- ""
    name <- ""
    for(i in c(1:length(spl[[1]]))){
      temp <- spl[[1]][i]
      pos <- regexpr("//", temp)
      if(pos != -1) temp <- substr(temp, 0, pos-1)
      if(nchar(temp) > 0){
        temp <- strsplit(temp, "\t")
        temp2 <- data.frame("type" = character(0), "name" = character(0), 
                            "param" = character(0), "val1" = numeric(0),
                            #Addition of val4
                            "val2" = numeric(0), "val3" = numeric(0), "val4" = numeric(0), stringsAsFactors = F)
        
        if(temp[[1]][1] == "type"){ type <- temp[[1]][2]
        } else if(temp[[1]][1] == "name"){ name <- temp[[1]][2]
        } else if(grepl("Param", temp[[1]][1])){
        } else if(temp[[1]][1] == "tropism") {
          temp2[[1,3]] <- "n_tropism"
          temp2$val1 <- temp[[1]][3]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)
          temp2$param <- "sigma_tropism"
          temp2$val1 <- temp[[1]][4]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)  
          temp2$param <- "tropism"
          temp2$val1 <- temp[[1]][2]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)  
        } else {
          for(j in c(1:5)){
            temp2[[1,j+2]] <- temp[[1]][j]
            temp2$type <- type
            temp2$name <- name
          }
          dataset_init <- rbind(dataset_init, temp2)
        }
      }
    }
  } 
  
  return(dataset_init)
}

read_pparam <- function(path){
  ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
  data <- read_file(path)
  # READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
  plant_init <- NULL
  spl <- strsplit(data, "\n")
  for(i in c(1:length(spl[[1]]))){
    temp <- spl[[1]][i]
    if(nchar(temp) > 0){
      temp <- strsplit(temp, "\t")
      temp2 <- data.frame( "param" = character(0), "val1" = numeric(0), stringsAsFactors = F)
      for(j in c(1:2)){
        temp2[[1,j]] <- temp[[1]][j]
      }
      plant_init <- rbind(plant_init, temp2)
    }
  }      
  
  colnames(plant_init) <- c("param", "val1")  
  return(plant_init)
}

write_rparam <- function(dataset, files){
  
  types <- unique(dataset$type)
  text <- NULL
  for(t in types){
    if(is.null(text)){text <- "# Parameter set for type"
    }else{
      text <- paste(text, "# Parameter set for type", sep="\n")
    }
    
    temp <- dataset[dataset$type == t,]
    
    str <- paste("type", temp$type[1], sep="\t")
    text <- paste(text, str, sep="\n")
    
    str <- paste("name", temp$name[1], sep="\t")
    text <- paste(text, str, sep="\n")
    
    for(i in c(1:nrow(temp))){
      if(temp[i, 3] == "n_tropism"){
        str <- paste("tropism", temp[i+2, 4], temp[i, 4], temp[i+1, 4], sep="\t")
        text <- paste(text, str, sep="\n")
      }else if(temp[i, 3] == "sigma_tropism" | temp[i, 3] == "tropism"){
      }else if(temp[i, 3] == "dx"){
        str <- paste(temp[i, 3], temp[i, 4], sep="\t")
        text <- paste(text, str, sep="\n")
      }else{
        str <- paste(temp[i, 3], temp[i, 4], temp[i, 5], temp[i, 6], temp[i, 7], sep="\t")
        text <- paste(text, str, sep="\n")
      }
    }
    
  }
  text <- gsub("\tNA", "", text)
  for(f in files){
    cat(text, file=f)
  }

}

write_pparam <- function(plant, files){
  text <- NULL
  for(i in c(1:nrow(plant))){
    str <- paste(plant[i, 1], plant[i, 2], sep="\t")
    text <- paste(text, str, sep="\n")
  }
  
  text <- gsub("\tNA", "", text)

  for(f in files){
    cat(text, file=f)
  }
}

getSigma <- function(rs, order = 1){
  
  require(plyr)
  
  rs <- rs %>% 
    filter(root_order == order) %>% 
    ddply(.(#image, 
      root), summarise, 
          diff_o = abs(diff(theta)), 
          diff_l = diff(distance_from_base)) %>% 
    filter(diff_o > 0) %>%
    ddply(.(image), summarise, theta = mean(diff_o / diff_l))
  
  rs$sigma <- exp(1.484 - 0.00798 * rs$theta)
  
  return(rs)
  
}

getSigma2_0 <- function(rs, name = "Lat"){
  
  # require(plyr)
  
  rs <- rs %>% 
    filter(root_name == name) %>% 
    group_by(image, root) %>% 
    mutate(diff_o = c(0,abs(diff(theta))), 
           diff_l = c(0,diff(distance_from_base))) %>% 
    filter(diff_o > 0) %>%
    ungroup %>% 
    group_by(image) %>% 
    summarise(theta = mean(diff_o / diff_l)) %>% 
    mutate(sigma = exp(1.484 - 0.00798 * theta))
  # 
  # rs3 <- rs %>% 
  #   filter(root_name == name) %>% 
  #   ddply(.(image, root), summarise, diff_o = abs(diff(theta)), 
  #          diff_l = diff(distance_from_base)) %>% 
  #   filter(diff_o > 0) %>%
  #   ddply(.(image), summarise, theta = mean(diff_o / diff_l))
  
  # rs$sigma <- exp(1.484 - 0.00798 * rs$theta)
  
  return(rs)
  
}

CRootBox <- function(){

all_roots <- NULL

  system("cd CPlantBox-master/tutorial/examples/python &&  python3 RSAscript.py")
      # Get all the .txt files exported by CRootBox
  fls <- list.files("./CPlantBox-master/tutorial/examples/python/")
  fls <- fls[grepl("rootsystem.txt", fls)]
  
  all_roots <- fread(paste0("./CPlantBox-master/tutorial/examples/python/",fls[1]), header = T)
  all_roots <- as.data.table(all_roots)
  all_roots <- all_roots%>%
                  transmute(node1ID = node1ID,
                            node2ID = node2ID,
                            branchID = branchID-1,
                            x1 = x1, y1 = y1, z1 = z1, x2 = x2, y2= y2, z2 = z2,
                            radius = radius,
                            length = sqrt((all_roots$x2 - all_roots$x1)^2 + (all_roots$y2 - all_roots$y1)^2 + (all_roots$z2 - all_roots$z1)^2),
                            R = R,
                            G = G,
                            B = B,
                            time = time,
                            type = type,
                            age = age)
  return(all_roots) 
}

CRoot_hull <- function(all_roots, age_filter = c(20,40,60), sam){
  pol_ch = poly_area = ALL_ROOTS <- NULL
  
  for(ag in age_filter){
    for(xy in c("x", "y")){
      tmp <- all_roots%>%
        filter(time <= ag)%>%
        select(paste0(xy,"2"),z2)
      first_col <- tmp[,1]
      x <- matrix(c(first_col, tmp$z2), nc = 2)
      # Get the indices of the unique points lying on the convex hull
      ch <- chull(x= first_col, y = tmp$z2)
      ch <- c(ch, ch[1])
      
      # Area of the convex hull
      xy.coords <- cbind(first_col, tmp$z2)
      chull.coords <- xy.coords[ch,]
      chull.poly <- sp::Polygon(chull.coords, hole = F)
      chull.area <- chull.poly@area
      
      polyg <- cbind(as.data.frame(x[ch,]), sam, rep = paste0(xy), age = ag)
      pol_area <- c(chull.area/10000)%>% # Transformation into square meter
        as.tibble()%>%
        transmute(area = value, rep= paste0(xy),sam, age = as.numeric(ag))
      poly_area <- rbind(poly_area, pol_area)
      pol_ch <- rbind(pol_ch, polyg)
    }
    tmp <- all_roots%>%
        filter(time <= ag)%>%
        mutate(age_RS = ag)
        
    ALL_ROOTS <- rbind(ALL_ROOTS, tmp)
  }
  
  RLD <- ALL_ROOTS%>%
    dplyr::group_by(age_RS, rz1 = round(z1/2)*2, # on every two cm of the profile  
                    type)%>%
    dplyr::summarise(root = sum(length),
                     rooting_depth = min(z2))%>%
    ungroup()
  return(list(RLD, pol_ch, poly_area) )
}

py_to_r_2 <- function(x){
  
  ele <- str_split(paste0(x), " ")[[1]]
  kr <- as.numeric(unlist(regmatches(ele,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",ele))))
  return(kr)
}

MECHA_R <- function(path = "MECHA_GRANAR/MECHAv4_GRANAR.py", Kx = F, kw = 2.4E-04, km = 3E-5, id_passage_cell = 0){
  
  #Load the xml file with the hydraulics input for MECHA
  require(stringr)
  require(xml2)
  kw <- as.character(kw)
  km <- as.character(km)
  p_file <- str_c(strsplit(path, "/")[[1]][1],"/Projects/GRANAR/in/")
  x <- read_xml(paste0(p_file,"Maize_Hydraulics.xml"))
  kw_tmp <- xml_children(x)[2]
  km_tmp <- xml_children(x)[4]
  temp <- xml_find_all(kw_tmp, ".//kw")
  temp_km <- xml_find_all(km_tmp, ".//km")
  xml_attr(temp, "value") <- kw
  xml_attr(temp_km, "value") <- km
  write_xml(x, file = paste0(p_file,"Maize_Hydraulics.xml")) # then overwrite the hydraulics xml file
  
  tic <- Sys.time()
  message("Launch MECHA")
  MECHA <- try(source_python(path), silent = T)
  if(is.null(MECHA)){MECHA <- c(0)}
  if(str_detect(MECHA[1], "Error ")){
    message("error NaN: kr_tot <- NA")
    kr_tot <- c(NA, NA, NA)
    Kx <- F
  }
  if(MECHA[1] == "Error in py_run_file_impl(file, local, convert) : \n  IndexError: index 25 is out of bounds for axis 0 with size 25\n"){
    message("Undersized matrix of connected cell.")
    kr_tot <- c(NA, NA, NA)
    Kx <- F
  }
  print(Sys.time()-tic)
  if(Kx){
    kr <- c(kr_tot[1,1], kr_tot[2,1],kr_tot[3,1])
    kx <- py_to_r_2(K_xyl_spec)
    K <- tibble(kr1 = kr[1],kr2 = kr[2],kr3 = kr[3], kx = kx)
    return(K)
  }else{
    kr <- c(kr_tot[1,1], kr_tot[2,1],kr_tot[3,1])
    K <- tibble(kr1 = kr[1],kr2 = kr[2],kr3 = kr[3])
    return(K)
  }
}

radial_corr <- function(RS_tot,x1,x2){ 
  # x1 where endodermis is fully suberized
  # x2 where exodermis has a Casparian strip 
  
  RS_tot$Kr_corr[RS_tot$apo_barriers == 1 & RS_tot$dist <= x1] <- RS_tot$kr[RS_tot$apo_barriers == 1 & RS_tot$dist <= x1]
  RS_tot$Kr_corr[RS_tot$apo_barriers == 2 & RS_tot$dist <= x2 & RS_tot$dist > x1 ] <- RS_tot$kr[RS_tot$apo_barriers == 2 & RS_tot$dist <= x2 & RS_tot$dist > x1]
  RS_tot$Kr_corr[RS_tot$apo_barriers == 4 & RS_tot$dist > x2] <- RS_tot$kr[RS_tot$apo_barriers == 4 & RS_tot$dist > x2]
  return(RS_tot)
}

Get_granarparam <- function(Data_params){
  
  Data_params <- Data_params%>%
    dplyr::group_by(Image, tissue, dist, Root, Root_id)%>%
    dplyr::summarise(cell = mean(cell, na.rm = T),
                     layer = mean(layer, na.rm = T))%>%
    ungroup()%>%
    filter(!is.na(tissue))
  
  tiss_param <- NULL
  for(tiss in unique(na.omit(Data_params$tissue[Data_params$tissue != "proto"]))){
    
    fit<- lm(cell~dist, data = Data_params%>%filter(tissue == tiss))
    summary(fit)
    tis_temp <- Data_params%>%
      filter(tissue == tiss)%>%
      mutate(reg = cell-(fit$coefficients[1]+fit$coefficients[2]*dist))
    
    tmp <- tibble(one = fit$coefficients[1], 
                  slope = fit$coefficients[2], 
                  tiss, 
                  p = summary(fit)$coefficients[2,4],
                  va = var(tis_temp$reg, na.rm = T))
    if(summary(fit)$coefficients[2,4] > 0.05){
      tmp$slope <- 0
      tmp$one = median(Data_params$cell[Data_params$tissue == tiss])
      tmp$va = var(Data_params$cell[Data_params$tissue == tiss], na.rm = T)
    }
    tiss_param <- rbind(tiss_param, tmp)
  }
  layer_param <- NULL
  for(lay in c("stele", "cortex", "xylem", "proto")) {
    fit<- lm(layer~dist, data = Data_params%>%filter(tissue == lay))
    summary(fit)
    lay_temp <- Data_params%>%
      filter(tissue == lay)%>%
      mutate(reg = layer-(fit$coefficients[1]+fit$coefficients[2]*dist))
    
    tmp <- tibble(one = fit$coefficients[1], 
                  slope = fit$coefficients[2], 
                  lay, 
                  r = summary(fit)$r.squared,
                  va = var(lay_temp$reg, na.rm = T))
    
    
    ok <- summary(fit)$coefficients[2,4]
    if(is.na(summary(fit)$coefficients[2,4])){
      ok <- 1
      tmp$one <- median(Data_params$layer[Data_params$tissue == lay], na.rm = T)
      tmp$va <- var(Data_params$layer[Data_params$tissue == lay], na.rm = T)
      message(paste0("No enough data to make a regression for ",lay))
      
    }
    if(ok > 0.05){
      tmp$slope <- 0
      tmp$one = median(Data_params$layer[Data_params$tissue == lay], na.rm = T)
      tmp$va <- var(Data_params$layer[Data_params$tissue == lay], na.rm = T)
    }
    layer_param <- rbind(layer_param, tmp)
  }
  
  for (im in unique(Data_params$Image)) {
    # Get the total diameter of the root
    
    tmp <- Data_params%>%
      filter(Image == im)
    if(nrow(tmp) == 8){
      
      tot <- tmp$cell[tmp$tissue == "endo"]+
        tmp$cell[tmp$tissue == "pericycle"]+
        tmp$cell[tmp$tissue == "exo"]+
        tmp$cell[tmp$tissue == "epi"]+
        tmp$layer[tmp$tissue == "stele"]/2+
        tmp$layer[tmp$tissue == "cortex"]
      
      total <- tibble(Image = im, 
                      tissue = c("total"),
                      cell = NA, 
                      layer = tot, 
                      dist = unique(tmp$dist), 
                      Root = unique(tmp$Root),
                      Root_id = unique(tmp$Root_id))
      
      Data_params <- rbind(Data_params, total)
    }
  }
  
  param <- list(tiss_param, layer_param)
  return(param)
}

rain <- function(tot_rainfall = 160, n_dryDay = 23, Days = 60, seed = 4){
  set.seed(seed)
  prop <- 1 - n_dryDay/Days
  x <- runif(Days)
  x[x < prop] <- 0
  nD <- length(x[x >= prop])
  frac <- sort(runif(nD))
  frac[2:length(frac)] <- frac[2:length(frac)]-frac[1:(length(frac)-1)]
  x[x >= prop] <- tot_rainfall*frac
  return(x)
}

create_soil <- function(wheather){
  time_step = 1
  endTime = 60
  ntimesteps = 60
  profile_depth = 200
  
  RLD <- all_roots%>%
    mutate(age_RS = ceiling(time))%>%
    dplyr::group_by(age_RS)%>%
    dplyr::summarise(rooting_depth = min(z2))%>%
    ungroup()
  
  soil = atmosph <- NULL
  fit_rdepth <- aov(abs(RLD$rooting_depth)~seq(time_step, endTime, by = 1))
  
  atm_bc_data <- data.frame(tAtm = seq(time_step, endTime, by = time_step),
                            # Precipitation (cm/d)
                            Prec = wheather,
                            # Soil evaporation (cm/d)
                            rSoil = rep(0, ntimesteps),
                            # Transpiration (cm/d)
                            rRoot = seq(2/ntimesteps, 2, by = 2/ntimesteps),
                            hCritA = rep(15000, ntimesteps),
                            rB = numeric(ntimesteps),
                            hB = numeric(ntimesteps),
                            ht = numeric(ntimesteps),
                            RootDepth = fit_rdepth$coefficients[1]+fit_rdepth$coefficients[2]*c(1:60))

  # overwrite the atmposheric boundary condition of hydrus.
  write.atmosph.in("Hydrus/1DRAINAG/",
                   maxAL = ntimesteps,
                   deltaT = time_step,
                   atm_bc_data,
                   hCritS = 15000,
                   input.pet = F)
  # setwd("C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx")
  # Run Hydrus1D ---
    
    system("./h1d_calc")
  #Load hydrus profile information
  hydrus <- read.nod_inf(project.path = "Hydrus/1DRAINAG/", 
                         out.file = paste0("Nod_Inf.out"), output = NULL,
                         warning = FALSE)
  #Hydrus dataset is large, then to optimize computation time, a subset is taken.
  soil_tmp <- hydrus%>%
    filter(Time %in% seq(time_step,endTime,time_step), #Time resolution of the overall exemple
           Depth %in% seq(-profile_depth, 0, by = 0.2))
  
  #formating the soil data to be compatible with MARSHAL
  soil <- soil_tmp%>%
    transmute(id = c(1:nrow(soil_tmp)),
              z = Depth,
              value = Time,
              psi = Head)
  
  if(is.na(soil$psi[soil$value == 60][1])){
    message("hydrus crash once")
    write.selector.in("Hydrus/1DRAINAG/", "2")
    # Run Hydrus1D ---
    system("./h1d_calc")
    #Load hydrus profile information
    hydrus <- read.nod_inf(project.path = "Hydrus/1DRAINAG/", 
                           out.file = paste0("Nod_Inf.out"), output = NULL,
                           warning = FALSE)
    #Hydrus dataset is large, then to optimize computation time, a subset is taken.
    soil_tmp <- hydrus%>%
      filter(Time %in% seq(time_step,endTime,time_step), #Time resolution of the overall exemple
             Depth %in% seq(-profile_depth, 0, by = 0.2))
    
    #formating the soil data to be compatible with MARSHAL
    soil <- soil_tmp%>%
      transmute(id = c(1:nrow(soil_tmp)),
                z = Depth,
                value = Time,
                psi = Head)
    write.selector.in("Hydrus/1DRAINAG/", "2")
    if(is.na(soil$psi[soil$value == 60][1])){
      message("hydrus crash twice ...")
    }
  }
  return(list(soil, atm_bc_data))
}

make_pparam <- function(MatSobol, sample){
  pp <- c("plantingdepth", "firstB","delayB", "maxB","nC", "firstSB", "delaySB", "delayRC", "nz", "simtime")
  
  va <- c(2,0.2,0,
          round(unlist(MatSobol[sam,"maxB"])),
          round(unlist(MatSobol[sam,"nC"])),
          round(unlist(MatSobol[sam,"firstSB"])),
          round(unlist(MatSobol[sam,"delaySB"])),
          round(unlist(MatSobol[sam,"delayRC"])),
          0, 60)
  pparam <- data.frame(param = pp, val1 = as.character(va))
  return(pparam)
}

make_rparam <- function(MatSobol, sample){
  sam <- sample
  # Create rparam ---------
  ty <- sort(rep(1:5, 16))
  name <- c("tap", "LC", "LA", "basal", "SBR" )
  na <- ty
  for(i in unique(ty)){
    na[na==i] <- name[i] 
  }
  pp_n <- rep(c("lb", "la", "ln", "lmax", "r", "a", "color", "n_tropism", "sigma_tropism", "tropism", "dx", "successors", "successorP",
                "theta", "rlt", "gf"),5)
  
  val1 <- c(0.1,unlist(MatSobol[sam,"la_T"]),unlist(MatSobol[sam,"ln_T"]),
            1000, unlist(MatSobol[sam,"r_T"]), unlist(MatSobol[sam,"a_T"]),
            1,1, unlist(MatSobol[sam,"trop_T"])/10, 1,0.25,2,
            2,unlist(MatSobol[sam,"theta_T"]),
            1E9,
            1,
            0.1,
            unlist(MatSobol[sam,"la_LC"]),
            1,
            unlist(MatSobol[sam,"lm_LC"]),
            unlist(MatSobol[sam,"r_LC"]),
            unlist(MatSobol[sam,"a_LC"]),
            1,
            1,
            unlist(MatSobol[sam,"trop_LC"])/10,
            1,
            0.10,
            0,
            0,
            unlist(MatSobol[sam,"theta_LC"]),
            1E9,
            1,
            0.1, 
            unlist(MatSobol[sam,"la_LA"]), 
            unlist(MatSobol[sam,"ln_LA"]),
            unlist(MatSobol[sam,"lm_LA"]),
            unlist(MatSobol[sam,"r_LA"]),
            unlist(MatSobol[sam,"a_LA"]),
            1,
            1,
            unlist(MatSobol[sam,"trop_LA"])/10,
            1,
            0.10,
            1,
            1,
            unlist(MatSobol[sam,"theta_LC"]),
            1E9,
            1, 
            0.1,
            unlist(MatSobol[sam,"la_B"]),
            unlist(MatSobol[sam,"ln_T"]),
            unlist(MatSobol[sam,"lm_B"]),
            unlist(MatSobol[sam,"r_B"]),
            unlist(MatSobol[sam,"a_B"]),
            1,
            1,
            unlist(MatSobol[sam,"trop_B"])/10,
            1,
            0.25,
            2,
            2,
            unlist(MatSobol[sam,"theta_B"]),
            1E9,
            1,
            0.1,
            unlist(MatSobol[sam,"la_SBR"]),
            unlist(MatSobol[sam,"ln_T"]),
            1000,
            unlist(MatSobol[sam,"r_SBR"]),
            unlist(MatSobol[sam,"a_SBR"]),
            1,
            1,
            unlist(MatSobol[sam,"trop_SBR"])/10,
            1,
            0.25,
            2,
            2,
            unlist(MatSobol[sam,"theta_SBR"]),
            1E9,
            1)
  val2 <- c(0.01,unlist(MatSobol[sam,"la_T"])*0.1,unlist(MatSobol[sam,"ln_T"])*0.1,0, unlist(MatSobol[sam,"r_T"])*0.1, unlist(MatSobol[sam,"a_T"])*0.1,
            0,rep(NA,4),2,
            MatSobol[sam,"Plc_T"],MatSobol[sam,"theta_T"]*0.1,1000,NA, # Tap root
            0.01,unlist(MatSobol[sam,"la_LC"])*0.1,0,unlist(MatSobol[sam,"lm_LC"])*0.1, unlist(MatSobol[sam,"r_LC"])*0.1, unlist(MatSobol[sam,"a_LC"])*0.1,
            1,rep(NA,4),NA,NA,
            MatSobol[sam,"theta_LC"]*0.1,1000,NA, # Lat C
            0.01,unlist(MatSobol[sam,"la_LA"])*0.1,0,unlist(MatSobol[sam,"lm_LA"])*0.1, unlist(MatSobol[sam,"r_LA"])*0.1, unlist(MatSobol[sam,"a_LA"])*0.1,
            1,rep(NA,4),NA,NA,
            MatSobol[sam,"theta_LC"]*0.1,1000,NA, # Lat A
            0.01,unlist(MatSobol[sam,"la_T"])*0.1,unlist(MatSobol[sam,"ln_T"])*0.1,0, unlist(MatSobol[sam,"r_T"])*0.1, unlist(MatSobol[sam,"a_T"])*0.1,
            0,rep(NA,4),2,
            MatSobol[sam,"Plc_B"],MatSobol[sam,"theta_B"]*0.1,1000,NA, # basal
            0.01,unlist(MatSobol[sam,"la_T"])*0.1,unlist(MatSobol[sam,"ln_T"])*0.1,0, unlist(MatSobol[sam,"r_T"])*0.1, unlist(MatSobol[sam,"a_T"])*0.1,
            1,rep(NA,4),2,
            MatSobol[sam,"Plc_SBR"],MatSobol[sam,"theta_SBR"]*0.1,1000,NA  # SBR
  )
  val3 <- c(rep(NA,6),0,rep(NA,4),3,1-MatSobol[sam,"Plc_T"],rep(NA,3), # Tap root
            rep(NA,6),0,rep(NA,9), # Lat C
            rep(NA,6),1,rep(NA,9), # Lat A
            rep(NA,6),0,rep(NA,4),3,1-MatSobol[sam,"Plc_B"],rep(NA,3), # basal
            rep(NA,6),1,rep(NA,4),3,1-MatSobol[sam, "Plc_SBR"],rep(NA,3)  # SBR
  )
  val4 <- rep(NA,80)
  rparam <- data.frame(type = ty, name = na, param = pp_n, val1, val2, val3, val4 = as.numeric(val4))
  return(rparam)
}

write.atmosph.in<- function(project.path, maxAL, deltaT, atm.bc.data, hCritS = 0, ...){
  
  out.file = "ATMOSPH.IN"
  # default.filename = "ATMOSPH.IN"
  atm_data = readLines(con = paste0(project.path,"ATMOSPH_1.IN"), n = -1L, encoding = "unknown")
  
  if(file.exists(file.path(project.path, out.file))){
    file.remove(file.path(project.path, out.file))
  }
  extinction_ind = grep("Extinction", atm_data)
  
  
  # write(atm_data, file = "ATMOSPH_IN.BAK", append = F)
  
  hcrits_ind = grep("hCritS", atm_data)
  atm_data[hcrits_ind + 1] = sprintf("%7.0f", hCritS)
  
  maxAL_ind = grep("MaxAL", atm_data)
  tAtm_ind = grep(" tAtm", atm_data)
  
  tMax = maxAL*deltaT
  
  atm_data[(maxAL_ind + 1)] = sprintf("%7.0f", maxAL)
  end_line = atm_data[grep("end", atm_data)]
  
  # bc_data = atm_data[(tAtm_ind +1): (end_line - 1)]
  # data_ind = (tMax*(sim_ind-1) + 1):(sim_ind*tMax)
  
  # tAtm = seq(deltaT, tMax, by = deltaT)
  
  bc_data_vars = c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB",
                   "hB", "ht", "RootDepth")
  
  bc_data_new = atm.bc.data[1:maxAL, bc_data_vars]
  # bc_data_new = data.frame(tAtm = seq(deltaT, tMax, deltaT), bc_data_new, row.names = NULL)
  #  bc_data_new = bc_data_new[rep(seq_len(nrow(bc_data_new)), each = 4), ]
  #  bc_data_new$tAtm = seq(deltaT, tMax, by = deltaT)
  row.names(bc_data_new) = NULL
  
  tstep_decimals = hydrusR::get.decimalplaces(deltaT)
  
  fmt_vec = c("%11.0f", "%12.3f", "%12.4f", "%12.4f", "%12.0f", rep("%12.4f",8))
  fmt_vec[1] = sub(pattern = "0", replacement = tstep_decimals, fmt_vec[1])
  
  bc_data_fmt = bc_data_new
  
  for(a in 1:nrow(bc_data_fmt)) {
    bc_data_fmt[a, ] = sprintf(fmt = fmt_vec[1:ncol(bc_data_fmt)], bc_data_new[a, ])
  }
  bc_data_fmt = apply(bc_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
  
  atm_input1 = atm_data[1:tAtm_ind]
  atm_input2 = bc_data_fmt
  atm_input3 = end_line
  
  atmosph_input_new = c(atm_input1, atm_input2, atm_input3)
  atmosph_in_file = file.path(project.path, out.file)
  write(atmosph_input_new, file = atmosph_in_file, append = F)
  
}

convergence <- function(all_roots, conductivities, wheather, tpots){

  t0 <- proc.time()
  soil_glob <- create_soil(wheather)
  t0 <- rbind(t0, soil_1 = proc.time())
  soil <- soil_glob[[1]]
  atm_bc_data <- soil_glob[[2]]
  
  # Soil --------------
  # Take the same time variable as the one that were implemented in CRootBox.
  full_time <- c(1:60)
  time_step = full_time[2]-full_time[1]
  endTime = max(full_time)
  ntimesteps = endTime/time_step
  profile_depth <- 200
  RLD <- all_roots%>%
    mutate(age = ceiling(time))%>%
    dplyr::group_by(age)%>%
    dplyr::summarise(rooting_depth = min(z2))%>%
    ungroup()
  
  RSME <- 10
  k <- 1
  t0 <- rbind(t0, loop_ini = proc.time())
  while(RSME > 10){
    # MARSHAL -----------------
    print(k)
    k <- k + 1
    
    # Computing the root system hydraulic architecture
    # Loop to run MARSHAL with the selected simulation parameters.
    results <- tibble(krs = rep(NA, length(full_time)), 
                      tact = rep(NA, length(full_time)), 
                      tpot = rep(NA, length(full_time)), 
                      tp = tpots,
                      age = full_time,
                      rep = sam)
    t0 <- rbind(t0, loop_HM = proc.time())
    for(ag in full_time){# age of the RS
    print(ag)      
      # Select specific Soil for the simulation
      temp_soil <- soil[soil$value == ag,c(1,2,4)]
      # Select specific root system for the simulation
      temp_root <- all_roots[all_roots$time <= ag,]
      temp_root$time <- temp_root$time- (60 - temp_root$age)
      temp_root$rep <- sam
      temp_root$node1ID <- c(0:(nrow(temp_root)-1))
      temp_root$node2ID <- c(1:nrow(temp_root))
      for(noid in unique(temp_root$branchID)){
        if(noid == 0){next}
        bra <- which(temp_root$branchID == noid)
        fi <- bra[1]
        possi <- which(temp_root$x1 == temp_root$x1[fi] & temp_root$y1 == temp_root$y1[fi] & temp_root$z1 == temp_root$z1[fi] )
        temp_root$branchID[bra] <- possi[1]-1
      }
       
      # Select specific conductivities
      temp_conduct <- conductivities
      
      # -----------------------------
      # Run MARSHAL
      # -----------------------------
      hydraulics <- getSUF(temp_root, 
                           temp_conduct, 
                           temp_soil, 
                           hetero = T, 
                           Psi_collar = tpots)
      
      
      
      # Aggregate output from MARSHAL
      results$krs[results$age == ag] <- hydraulics$krs
      results$tact[results$age== ag] <- hydraulics$tact
      results$tpot[results$age== ag] <- hydraulics$tpot
      t0 <- rbind(t0, marshal = proc.time())
    }
    t0 <- rbind(t0, marshal_end = proc.time())
    k_marshal <- results$tact
    k_hydrus <- atm_bc_data$rRoot/10*1000
    RSME <- sqrt((sum(k_marshal-k_hydrus)^2)/length(k_marshal))
    message(paste0("RSME of: ", RSME))
    atm_bc_data <- data.frame(tAtm = seq(time_step, endTime, by = time_step),
                              # Precipitation (cm/d)
                              Prec = wheather,
                              # Soil evaporation (cm/d)
                              rSoil = rep(0, ntimesteps),
                              # Transpiration (cm/d)
                              rRoot = round(10*results$tact/1000,6), # 100 000p/ha
                              hCritA = rep(15000, ntimesteps),
                              rB = numeric(ntimesteps),
                              hB = numeric(ntimesteps),
                              ht = numeric(ntimesteps),
                              RootDepth = abs(RLD$rooting_depth))
    
    
    # overwrite the atmposheric boundary condition of hydrus.
    write.atmosph.in("Hydrus/1DRAINAG/",
                     maxAL = ntimesteps,
                     deltaT = time_step,
                     atm_bc_data,
                     hCritS = 15000,
                     input.pet = F)
    # setwd("C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx")
    # Run Hydrus1D ---
    system("./h1d_calc")
    
    hydrus <- read.nod_inf(project.path = "Hydrus/1DRAINAG/", 
                         out.file = paste0("Nod_Inf.out"), output = NULL,
                         warning = FALSE)  
    #Hydrus dataset is large, then to optimize computation time, a subset is taken.
    soil_tmp <- hydrus%>%
      filter(Time %in% seq(time_step,endTime,time_step), #Time resolution of the overall exemple
             Depth %in% seq(-profile_depth, 0, by = 0.2))
    
    #formating the soil data to be compatible with MARSHAL
    soil <- soil_tmp%>%
      transmute(id = c(1:nrow(soil_tmp)),
                z = Depth,
                value = Time,
                psi = Head)
    t0 <- rbind(t0, hydrus_end = proc.time())
    no_BUG <- T
    if(is.na(soil$psi[soil$value == 60][1])){
      message("hydrus crash once")
      write.selector.in("Hydrus/1DRAINAG/", "1")
      # Run Hydrus1D ---
    system("./h1d_calc")
      #Load hydrus profile information
      hydrus <- read.nod_inf(project.path = "Hydrus/1DRAINAG/", 
                             out.file = paste0("Nod_Inf.out"), output = NULL,
                             warning = FALSE)
      #Hydrus dataset is large, then to optimize computation time, a subset is taken.
      soil_tmp <- hydrus%>%
        filter(Time %in% seq(time_step,endTime,time_step), #Time resolution of the overall exemple
               Depth %in% seq(-profile_depth, 0, by = 0.2))
      
      #formating the soil data to be compatible with MARSHAL
      soil <- soil_tmp%>%
        transmute(id = c(1:nrow(soil_tmp)),
                  z = Depth,
                  value = Time,
                  psi = Head)
      write.selector.in("Hydrus/1DRAINAG/", "2")
      if(is.na(soil$psi[soil$value == 60][1])){
        message("hydrus crash twice ...")
        no_BUG <- F
        break
      }
    }
  }
  
  if(no_BUG){
    results = new_all_roots <- NULL
    t0 <- rbind(t0, m_last = proc.time())
    for(ag in full_time){# age of the RS
      
      # Select specific Soil for the simulation
      temp_soil <- soil %>% 
        filter(value == ag)%>% 
        select(-value)
      # Select specific root system for the simulation
      temp_root <- all_roots[all_roots$time <= ag,]
      temp_root$time <- temp_root$time- (60 - temp_root$age)
      temp_root$rep <- sam
      temp_root$node1ID <- c(0:(nrow(temp_root)-1))
      temp_root$node2ID <- c(1:nrow(temp_root))
      for(noid in unique(temp_root$branchID)){
        if(noid == 0){next}
        bra <- which(temp_root$branchID == noid)
        fi <- bra[1]
        possi <- which(temp_root$x1 == temp_root$x1[fi] & temp_root$y1 == temp_root$y1[fi] & temp_root$z1 == temp_root$z1[fi] )
        temp_root$branchID[bra] <- possi[1]-1
      }
      # Select specific conductivities
      temp_conduct <- conductivities
      
      # -----------------------------
      # Run MARSHAL
      # -----------------------------
      hydraulics <- getSUF(temp_root, 
                           temp_conduct, 
                           temp_soil, 
                           hetero = T, 
                           Psi_collar = tpots)
      
      # Aggregate output from MARSHAL
      results <- rbind(results, data.frame(krs = hydraulics$krs, 
                                           tact = hydraulics$tact, 
                                           tpot = hydraulics$tpot, 
                                           tp = tpots,
                                           age = ag,
                                           rep = sam))
      # Format dataset to be compatible with MARSHAL output
      first <- temp_root[temp_root$node1ID == 0,]
      nodals_ids <- unique(temp_root$branchID[temp_root$type == 4 | 
                                                temp_root$type == 5])
      for(no in nodals_ids){
        temp <- temp_root%>%
          filter(branchID == no)
        temp <- temp[1,] 
        connection <- data.frame(node1ID = 0,
                                 node2ID = temp$node1ID,
                                 branchID = temp$branchID,
                                 x1 = first$x1, y1 = first$y1, z1 = first$z1,
                                 x2 = temp$x1, y2 = temp$y1, z2 = temp$z1,
                                 radius = temp$radius,
                                 length = sqrt((first$x1-temp$x1)^2 + 
                                                 (first$y1-temp$y1)^2 + 
                                                 (first$z1-temp$z1)^2 ),
                                 R = 0, G = 0, B = 0,
                                 time = temp$time,
                                 type = temp$type,
                                 age = temp$age,
                                 rep = temp$rep)
        new_table = rbind(temp_root, connection)
        temp_root = new_table
        
      }
      temp_root <- temp_root[order(temp_root$node2ID, decreasing = F),]
      
      # Merge output of MARSHAL on specific root segment
      temp_root$suf <- as.vector(hydraulics$suf)
      temp_root$suf1 <- as.vector(hydraulics$suf1)
      temp_root$kx <- as.vector(hydraulics$kx)
      temp_root$kr <- as.vector(hydraulics$kr)
      temp_root$jr <- as.vector(hydraulics$jr)
      temp_root$psi <- as.vector(hydraulics$psi)
      temp_root$jxl <- as.vector(hydraulics$jxl)
      temp_root$psi_soil <- as.vector(hydraulics$psi_soil)
      # Add the simulation specificity to the dataset
      temp_root$rep <- sam
      new_all_roots <- rbind(new_all_roots, temp_root)
      t0 <- rbind(t0, m_last_age = proc.time())
    }
  }
  k_marshal <- results$tact
  k_hydrus <- atm_bc_data$rRoot/10*1000
  RSME <- sqrt((sum(k_marshal-k_hydrus)^2)/length(k_marshal))
  
  message(paste0("RSME of: ", RSME))
  
  dens <- new_all_roots%>%
    mutate(age_RS = ceiling(time))%>%
    dplyr::group_by(type, age_RS, sam = rep, rz1 = round(z1/2)*2)%>%
    dplyr::summarise(root = sum(length),
                     su = sum(suf),
                     sud = sum(suf/length),
                     su1 = sum(suf1),
                     j = sum(jr),
                     jx = sum(jxl),
                     p = sum(psi),
                     kr = sum(kr))%>%
    ungroup()
  t0 <- rbind(t0, END = proc.time())

  new_all_roots <- NULL # clear flash space
  hydrus_marshal <- list(soil, dens, results, t0)
  return(hydrus_marshal)
  
}

create_conductivities <- function(all_roots, MatSobol, sam, rparam){
  anatom = time = hydro <- NULL
  for(typ in unique(all_roots$type)){
    print(typ)
    time <- rbind(time, granar = proc.time())
    tot <- median(all_roots$radius[all_roots$type == typ])*2000
    stele <- (2.6342+(tot/2)*0.04747)^2
    var_stele = stele*(unlist(MatSobol[sam,"stele"])/100)
    stele <- stele + var_stele
    cortex_width = (tot - stele)/2
    nX <- -0.3345+0.034889*stele
    params$value[params$name == "stele" & params$type == "layer_diameter"] <- stele/1000+(stele/1000)*(unlist(MatSobol[sam,"stele"])/100)
    params$value[params$name == "cortex" & params$type == "n_layers"] <- round(((cortex_width)/1000)/params$value[params$name == "cortex" & params$type == "cell_diameter"])-1
    params$value[params$name == "xylem" & params$type == "max_size"] <- sqrt(((0.5767*params$value[params$name == "stele" & params$type == "layer_diameter"])^2)/nX)
    params$value[params$name == "xylem" & params$type == "n_files"] <- round(nX)
    params$value[params$name == "xylem" & params$type == "ratio"] <- (2+0.07456*stele)/nX
    
    if(typ == 2){
      params$value[params$name == "stele" & params$type == "cell_diameter"] <- 0.004
    }else{ params$value[params$name == "stele" & params$type == "cell_diameter"] <- 0.0075}
    
    
    # GRANAR & MECHA ----------------------
    sim <- create_anatomy_2(parameters = params)
    sim_unM <- create_anatomy_2(parameters = params, maturity_x = T)
    
    # save anatomies mature
    write_anatomy_xml(sim = sim,
                      path = "MECHA_GRANAR/cellsetdata/current_root.xml")
    time <- rbind(time, granar_f = proc.time())
    # Run MECHA
    K <- MECHA_R("MECHA_GRANAR/MECHAv4_GRANAR.py", Kx = T, kw = MatSobol[sam,"kw"], km = MatSobol[sam,"km"])
    while(is.na(K$kr1[1])){
      sim <- create_anatomy_2(parameters = params)  # save anatomies mature
      write_anatomy_xml(sim = sim,
                        path = "MECHA_GRANAR/cellsetdata/current_root.xml")
      # Run MECHA
      K <- MECHA_R("MECHA_GRANAR/MECHAv4_GRANAR.py", Kx = T, kw = MatSobol[sam,"kw"], km = MatSobol[sam,"km"])
    }
    K$Mat <- "Y"
    K$Type <- typ
    K$sam <- sam
    time <- rbind(time, mecha_1 = proc.time())
    # save anatomies unmature
    write_anatomy_xml(sim = sim_unM,
                      path = "MECHA_GRANAR/cellsetdata/current_root.xml")
    # Run MECHA
    K_unM <- MECHA_R("MECHA_GRANAR/MECHAv4_GRANAR.py", Kx = T, kw = MatSobol[sam,"kw"], km = MatSobol[sam,"km"])
    while(is.na(K_unM$kr1[1])){
      sim_unM <- create_anatomy_2(parameters = params, maturity_x = T)
      # save anatomies mature
      write_anatomy_xml(sim = sim_unM,
                        path = "MECHA_GRANAR/cellsetdata/current_root.xml")
      # Run MECHA
      K_unM <- MECHA_R("MECHA_GRANAR/MECHAv4_GRANAR.py", Kx = T, kw = MatSobol[sam,"kw"], km = MatSobol[sam,"km"])
    }
    K_unM$Mat <- "N"
    K_unM$Type <- typ
    K_unM$sam <- sam
    time <- rbind(time, mecha_2 = proc.time())
    hydro <- rbind(hydro, K, K_unM)
    
    out <- sim$output
    out$Root_type <- typ
    anatom <- rbind(anatom, out)
    
    # Set conductivities
    cond_tmp <- conductivities%>%filter(order_id == typ)
    
    # Radial 
    cond_tmp[1,6] <- mean(hydro$kr1[hydro$Type == typ])
    cond_tmp[2,6] <- mean(hydro$kr1[hydro$Type == typ])
    cond_tmp[3,6] <- mean(hydro$kr2[hydro$Type == typ])
    cond_tmp[4,6] <- mean(hydro$kr2[hydro$Type == typ])
    cond_tmp[5,6] <- mean(hydro$kr3[hydro$Type == typ])
    cond_tmp[6,6] <- mean(hydro$kr3[hydro$Type == typ])*0.01
    # Where is the transition happpening
    cond_tmp[2,5] <- rparam$val1[rparam$type == typ & rparam$param == "la"]/3
    cond_tmp[3,5] <- cond_tmp[2,5]+rparam$val1[rparam$type == typ & rparam$param == "r"]*unlist(MatSobol[sam,"trans_time_apo"])
    cond_tmp[4,5] <- cond_tmp[3,5]+rparam$val1[rparam$type == typ & rparam$param == "r"]*unlist(MatSobol[sam,"trans_time_apo"])
    cond_tmp[5,5] <- cond_tmp[4,5]+rparam$val1[rparam$type == typ & rparam$param == "r"]*unlist(MatSobol[sam,"trans_time_apo"])
    cond_tmp[6,5] <- cond_tmp[5,5]+rparam$val1[rparam$type == typ & rparam$param == "r"]*3*unlist(MatSobol[sam,"trans_time_apo"])
    
    # Axial
    cond_tmp[7, 6] <- hydro$kx[hydro$Mat == "N" & hydro$Type == typ][1]
    cond_tmp[8, 6] <- hydro$kx[hydro$Mat == "N" & hydro$Type == typ][1]
    cond_tmp[9, 6] <- hydro$kx[hydro$Mat == "Y" & hydro$Type == typ][1]
    cond_tmp[10,6] <- hydro$kx[hydro$Mat == "Y" & hydro$Type == typ][1]
    # Where is the transition happpening
    cond_tmp[7, 5] <- 0
    cond_tmp[8, 5] <- cond_tmp[3,5]+cond_tmp[3,5]*unlist(MatSobol[sam,"gap"])
    cond_tmp[9, 5] <- cond_tmp[8, 5]+rparam$val1[rparam$type == typ & rparam$param == "r"]*unlist(MatSobol[sam,"trans_time_xyl"])
    cond_tmp[10, 5] <- 150
    
    conductivities[conductivities$order_id == typ,] <- cond_tmp
  }
  
  conductivities$y[conductivities$order_id == 0 & conductivities$type == "kr"] <- min(conductivities$y[conductivities$order_id !=0  & conductivities$type == "kr"])
  conductivities$y[conductivities$order_id == 0 & conductivities$type == "kx"] <- max(conductivities$y[conductivities$order_id !=0  & conductivities$type == "kx"])
  
  return(list(conductivities, anatom ,time))
}

write.selector.in<- function(project.path, mesh = "2"){
  
  out.file = "SELECTOR.IN"
  # default.filename = "ATMOSPH.IN"
  selector_data = readLines(con = paste0(project.path,"/SELECTOR_1.IN"), n = -1L, encoding = "unknown")
  
  if(file.exists(file.path(project.path, out.file))){
    file.remove(file.path(project.path, out.file))
  }
  selector_data[30]
  mesh_1 <- selector_data[30]
  selector_data[30]<- paste(replace(str_split(mesh_1, " ")[[1]], which(str_split(mesh_1, " ")[[1]]== "1"), mesh), collapse = " ")
  selector_data[30]<- paste(replace(str_split(mesh_1, " ")[[1]], which(str_split(mesh_1, " ")[[1]]== "2"), mesh), collapse = " ")
  selector_data[30]<- paste(replace(str_split(mesh_1, " ")[[1]], which(str_split(mesh_1, " ")[[1]]== "3"), mesh), collapse = " ")
  
  selector_in_file = file.path(project.path, out.file)
  write(selector_data, file = selector_in_file, append = F)
  
}

write_anatomy_xml <- function(sim = NULL, path = NULL){

  if(is.null(sim)) warning("No simulation found. Please input a GRANAR simulation")
  if(is.null(path)) warning("No path found to save the XML file")

  cellgroups <- data.frame(id_group = c(1, 2, 3, 3, 4, 5, 13, 16, 12, 11),
                           type = c("exodermis", "epidermis", "endodermis", "passage_cell",  "cortex", "stele", "xylem", "pericycle", "companion_cell", "phloem"))

  xml <- '<?xml version="1.0" encoding="utf-8"?>\n'
  xml <- paste0(xml, '<granardata>\n')

  # Write the Metadata
  xml <- paste0(xml, '\t<metadata>\n')
  xml <- paste0(xml, '\t\t<parameters>\n')
  xml <- paste0(xml,paste0('\t\t\t<parameter io="',sim$output$io,'" ',
                            'name="',sim$output$name,'" ',
                            'type="',sim$output$type,'" ',
                            'value="',sim$output$value,'"/>\n', collapse = ""))
  xml <- paste0(xml, '\t\t</parameters>\n')
  xml <- paste0(xml, '\t</metadata>\n')

  # Write the cells information
  xml <- paste0(xml, '\t<cells count="',nrow(sim$cells),'">\n')

  sim$nodes <- merge(sim$nodes, cellgroups, by="type")  %>%
    mutate(id_group = id_group.y)

  temp_wall <- ddply(sim$nodes, .(id_cell, id_group), summarise, walls = paste0('\t\t\t\t<wall id="',
                                                                                paste(id_wall-1, collapse='"/>\n\t\t\t\t<wall id="'),
                                                                                '"/>\n'))
  xml <- paste0(xml, paste0('\t\t<cell id="',temp_wall$id_cell-1, '" group="', temp_wall$id_group, '" truncated="false" >\n',
                            '\t\t\t<walls>\n', temp_wall$walls, '\t\t\t</walls>\n',
                            '\t\t</cell>\n', collapse=""))
  xml <- paste0(xml, '\t</cells>\n')


  # Write the walls information
  xml <- paste0(xml, '\t<walls count="',nrow(sim$walls),'">\n')
  xml <- paste0(xml,paste0('\t\t<wall id="',sim$walls$id_wall-1,'" group="0" edgewall="false" >\n',
                           '\t\t\t<points>\n',
                           '\t\t\t\t<point x="',sim$walls$x1,'" y="',sim$walls$y1,'"/>\n',
                           '\t\t\t\t<point x="',sim$walls$x2,'" y="',sim$walls$y2,'"/>\n',
                           '\t\t\t</points>\n',
                           '\t\t</wall>\n', collapse = ""))
  xml <- paste0(xml, '\t</walls>\n')

  # Write the cell group informations
  print(cellgroups)
  xml <- paste0(xml, '\t<groups>\n')
  xml <- paste0(xml, '\t\t<cellgroups>\n')
  for(i in c(1:nrow(cellgroups))){
    xml <- paste0(xml, '\t\t\t<group id="',cellgroups$id_group[i],'" name="',cellgroups$type[i],'" />\n')
  }
  xml <- paste0(xml, '\t\t</cellgroups>\n')
  xml <- paste0(xml, '\t\t<wallgroups>\n')
  xml <- paste0(xml, '\t\t\t<group id="0" name="unassigned" />\n')
  xml <- paste0(xml, '\t\t</wallgroups>\n')
  xml <- paste0(xml, '\t</groups>\n')

  xml <- paste0(xml, '</granardata>')

  if(!is.null(path)){
    cat(xml, file = path)
    return(TRUE)
  }else{
    return(xml)
  }


}

read_param_xml <- function(path = NULL){

  if( is.null(path) ){
    warning("No path specified")
  }

  input <- read_xml(path)
  params <- NULL

  # Quality checks. Check if all the needed tags are present in the XML file
  to_find <- c("planttype", "randomness", "xylem", "phloem", "stele", "endodermis", "exodermis", "epidermis", "aerenchyma", "pericycle", "cortex")
  for(tf in to_find){
    if (length(xml_find_all(input, paste0("//",tf))) == 0) warning(paste0("Could not find the '",tf,"' tag in the XML file"))
  }

  #Read the file and get the parameters in a table
  for( ch in xml_children(xml_find_all(input, "//*"))){
    att <- xml_attrs(ch)
    for(i in c(1:length(att))){
      params <- rbind(params, data.frame(
        name = xml_name(ch),
        type = names(att)[i],
        value = att[i]
      ))
    }
  }
  row.names(params) <- NULL
  params <- params %>% mutate(value = as.numeric(as.character(value)))

  return(params)
}


write_param_xml <- function(path, params){
  
  if(!is.null(params$param)){
    colnames(params) <- c("name", "type", "value")
  }
  xml <- "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  xml <- paste0(xml, "<granar>\n")
  
  
  for(i in unique(params$name)){
    tmp_type <- params$type[params$name == i]
    tmp_val <- params$value[params$name == i]
    xml <- paste0(xml, paste0("\t<",i))
    for (j in 1:length(tmp_type)){
      xml <- paste0(xml," ",tmp_type[j],"=\"",tmp_val[j], "\"")
    }
    xml <- paste0(xml,"\ />\n")
  }
  xml <- paste0(xml, "</granar>")
  if (!is.null(path)) {
    cat(xml, file = path)
    return(TRUE)
  }  else {
    return(xml)
  }
}

read.nod_inf <- function (project.path, out.file = "Nod_Inf.out", output = NULL, 
  warn = FALSE, ...) 
{
  if (is.null(output) | missing(output)) {
    output = c("Head", "Moisture", "K", "C", "Flux", "Sink", 
      "Kappa", "v/KsTop", "Temp")
  }
  options(warn = -1)
  if (warn == TRUE) 
    options(warn = 0)
  nod_inf = data.table::fread(input = file.path(project.path, 
    out.file), fill = TRUE, blank.lines.skip = FALSE, skip = 9)
  time_lines = nod_inf[grepl("Time:", nod_inf[["Node"]]), 
    ]
  times = c(0, as.numeric(time_lines$Depth))
  for (col in colnames(nod_inf)) set(nod_inf, j = col, value = as.numeric(nod_inf[[col]]))
  nod_inf = na.omit(nod_inf)
  nodes = sort(unique(nod_inf[["Node"]]))
  nod_inf[, `:=`(Time, rep(times, each = length(nodes)))]
  nod_split = split(nod_inf, f = nod_inf$Time)
  nrow_split = sapply(nod_split, nrow)
  extra_index = which(nrow_split > length(nodes))
  for (i in extra_index) {
    nod_split[[i]] = nod_split[[i]][1:length(nodes), ]
  }
  nod_inf = rbindlist(nod_split)
  output_names = intersect(output, colnames(nod_inf))
  output_names = c("Time", "Node", "Depth", output_names)
  nod_out = nod_inf[, .SD, .SDcols = output_names]
  options(warn = 0)
  return(nod_out)
}

write.CPparam <- function(pparam, rparam){
  
  pparam$param <- as.character(pparam$param)%>%
    replace(.,. == "plantingdepth", "seedPos.z")%>%
    replace(.,. == "simtime", "simulationTime")
  
  rparam$param <- as.character(rparam$param)%>%
    replace(.,. == "sigma_tropism", "tropismS")
  
  path <- "CPlantBox-master/modelparameter/rootsystem/Zea_mays_3_Postma_2011.xml"
  require(stringr)
  require(xml2)
  
  x <- read_xml(path)
  for( ch in xml_children(xml_find_all(x, "//seed"))){
    name <- xml_attr(ch, "name")
    if(name %in% pparam$param){
      xml_attr(ch, "value") <- pparam$val1[pparam$param == name]
    }
    if(name == "seedPos.z"){
      xml_attr(ch, "value") <- as.character(- as.numeric(pparam$val1[pparam$param == name]))
    }
  }
  
  k <- 0
  for( ch in xml_children(xml_find_all(x, "//root"))){
    name <- xml_attr(ch, "name")
    if(name=="gf"){
      k <- k + 1
    }
    if(name=="successor"){
      suc_T <- xml_attr(ch, "type")
      if(suc_T == "2"){
        xml_attr(ch, "percentage") <- rparam$val2[rparam$param == "successorP" & rparam$type == k]
      }
      if(suc_T == "3"){
        xml_attr(ch, "percentage") <- rparam$val3[rparam$param == "successorP" & rparam$type == k]
      }
    }
    if(name %in% rparam$param){
      xml_attr(ch, "value") <- rparam$val1[rparam$param == name & rparam$type == k]
      if(!is.na(xml_attr(ch, "dev"))){
        xml_attr(ch, "dev") <- rparam$val2[rparam$param == name & rparam$type == k]
      }
    }
  }
  
  write_xml(x, file = "CPlantBox-master/modelparameter/rootsystem/param.xml")
}


write.options.in <- function(project.path = "./Hydrus_1D_Couvreur/CouvreurV2", krs, kcomp){
  
  out.file <- "/Options.IN"
  # path = "./Hydrus_1D_Couvreur/CouvreurV2/Options.IN"
  op = readLines(con = paste0(project.path,"/Options_1.IN"), n = -1L, encoding = "unknown")
  tmp_krs <- op[10]
  op[10]<- paste(replace(str_split(tmp_krs, " ")[[1]], which(str_split(tmp_krs, " ")[[1]]== "0.0001"), krs), collapse = " ")
  tmp_kcomp <- op[13]
  op[13] <- paste(replace(str_split(tmp_kcomp, " ")[[1]], which(str_split(tmp_kcomp, " ")[[1]]== "0.0001"), kcomp), collapse = " ")
  
  options_in_file = file.path(project.path, out.file)
  write(op, file = options_in_file, append = F)
}


write.profile.dat <- function(project.path = "./Hydrus_1D_Couvreur/CouvreurV2", SSF){
  
  out.file <- "PROFILE.DAT"
  prof = readLines(con = paste0(project.path,"/PROFILE_1.DAT"), n = -1L, encoding = "unknown")
  h_ind = grep("h", prof)
  n <- as.numeric(str_split(prof[h_ind], " ")[[1]][3])
  if(nrow(SSF) != n){error("sum up SSf every two centimeter along the profile")}
  ile = data.table::fread(input = file.path(paste0(project.path,"/PROFILE_1.DAT")),
                              fill = TRUE, 
                              blank.lines.skip = FALSE, 
                              skip = 4)
  nopcol <- rep("na",(length(ile[h_ind+1,])-9))
  for(i in 1:length(nopcol)){
    nopcol[i] <- paste0("nop",i)
  }
  colnames(ile) <- c("id", "z", "h", "Mat", "Lay", "Beta", "Axz", "Bxz", "Dxz", nopcol)
  
  ile <- ile%>%select(-starts_with("nop"))
  ile <- ile[1:101, ]
  ile$h <- SSF$h
  ile$Beta <- SSF$suf
  data_fmt <- ile
  data_fmt = apply(data_fmt, MARGIN = 1, FUN = paste0, collapse = " ")
  
  prof_input1 = prof[1:h_ind]
  prof_input2 = data_fmt
  prof_input3 = prof[(1+h_ind+length(prof_input2)):length(prof)]
  
  prof_input_new = c(prof_input1, prof_input2, prof_input3)
  prof_in_file = file.path(project.path, out.file)
  write(prof_input_new, file = prof_in_file, append = F)
}


HM_RWUM <- function(wheather, all_roots, sam, tpots, conductivities){
  t0 <- proc.time()
  soil_global = new_all_roots <- NULL
  # Initial soil condition
  soil <- data.frame(id=1:101,
                     z = sort(seq(-200,0,2), decreasing = T),
                     value = 1,
                     psi = rep(-100, 101))
  
  temp_conduct <- conductivities
  
  results <- tibble(krs = rep(NA, 60), 
                    tact = rep(NA, 60), 
                    tpot = rep(NA, 60), 
                    tp = tpots,
                    age = c(1:60),
                    rep = sam)
  for(today in 1:60){
    
    # --------- MARSHAL ----------------
    # Computing the root system hydraulic architecture
    t0 <- rbind(t0, loop_HM = proc.time())
    print(today)      
    # Select specific Soil for the simulation
    temp_soil <- soil[soil$value == today,c(1,2,4)]
    # Select specific root system for the simulation
    temp_root <- all_roots[all_roots$time <= today,]
    temp_root$time <- temp_root$time- (60 - temp_root$age)
    temp_root$rep <- sam
    temp_root$node1ID <- c(0:(nrow(temp_root)-1))
    temp_root$node2ID <- c(1:nrow(temp_root))
    for(noid in unique(temp_root$branchID)){
      if(noid == 0){next}
      bra <- which(temp_root$branchID == noid)
      fi <- bra[1]
      possi <- which(temp_root$x1 == temp_root$x1[fi] & temp_root$y1 == temp_root$y1[fi] & temp_root$z1 == temp_root$z1[fi] )
      temp_root$branchID[bra] <- possi[1]-1
    }
    
    
    # -----------------------------
    # Run MARSHAL
    # -----------------------------
    hydraulics <- getSUF(temp_root, 
                         temp_conduct, 
                         temp_soil, 
                         hetero = T, 
                         Psi_collar = tpots)
    
    # Aggregate output from MARSHAL
     results$krs[results$age == today] <- hydraulics$krs
    results$tact[results$age == today] <- hydraulics$tact
    results$tpot[results$age == today] <- hydraulics$tpot
    
    # Format dataset to be compatible with MARSHAL output
    first <- temp_root[temp_root$node1ID == 0,]
    nodals_ids <- unique(temp_root$branchID[temp_root$type == 4 | 
                                              temp_root$type == 5])
    for(no in nodals_ids){
      temp <- temp_root%>%
        filter(branchID == no)
      temp <- temp[1,] 
      connection <- data.frame(node1ID = 0,
                               node2ID = temp$node1ID,
                               branchID = temp$branchID,
                               x1 = first$x1, y1 = first$y1, z1 = first$z1,
                               x2 = temp$x1, y2 = temp$y1, z2 = temp$z1,
                               radius = temp$radius,
                               length = sqrt((first$x1-temp$x1)^2 + 
                                               (first$y1-temp$y1)^2 + 
                                               (first$z1-temp$z1)^2 ),
                               R = 0, G = 0, B = 0,
                               time = temp$time,
                               type = temp$type,
                               age = temp$age,
                               rep = temp$rep)
      new_table = rbind(temp_root, connection)
      temp_root = new_table
      
    }
    temp_root <- temp_root[order(temp_root$node2ID, decreasing = F),]
    
    # Merge output of MARSHAL on specific root segment
    temp_root$suf <- as.vector(hydraulics$suf)
    temp_root$suf1 <- as.vector(hydraulics$suf1)
    temp_root$kx <- as.vector(hydraulics$kx)
    temp_root$kr <- as.vector(hydraulics$kr)
    temp_root$jr <- as.vector(hydraulics$jr)
    temp_root$psi <- as.vector(hydraulics$psi)
    temp_root$jxl <- as.vector(hydraulics$jxl)
    temp_root$psi_soil <- as.vector(hydraulics$psi_soil)
    # Add the simulation specificity to the dataset
    temp_root$rep <- sam
    temp_root$age <- today
    new_all_roots <- rbind(new_all_roots, temp_root)
    RLDWU <- temp_root%>%
      mutate(rz2 = round(z2/2, 1)*2)%>%
      dplyr::group_by(rz2)%>%
      dplyr::summarise(su = sum(suf),
                       ps = sum(psi),
                       jr = sum(jr),
                       jx = sum(jxl))%>%
      ungroup()%>%
      mutate(age = today)
    
    
    t0 <- rbind(t0, marshal = proc.time())
    
    # -------- HYDRUS 1D ------------- 
    atm_bc_data <- data.frame(tAtm = today, Prec = wheather[today], rSoil = 0, 
                              rRoot = hydraulics$tact[1]/75/15, hCritA = 15000,rB = 0, hB = 0, ht = 0, 
                              RootDepth = 0)
    # overwrite the atmposheric boundary condition of hydrus.
    write.atmosph.in("Hydrus/CouvreurV2/",
                     maxAL = 1,
                     deltaT = 1,
                     atm_bc_data,
                     hCritS = 15000,
                     input.pet = F)
    
    Beta <- c(RLDWU$su, rep(0, (101-length(RLDWU$su))))
    
    SSF <- data.frame(suf = Beta, h = temp_soil$psi)
    # overwirte the profile boundary condition
    write.profile.dat(project.path = "./Hydrus_1D_Couvreur/CouvreurV2", SSF)
    krs <- hydraulics$krs[1]
    z_ind <- which(RLDWU$rz2 == min(RLDWU$rz2))
    kcomp <- t(temp_soil$psi[1:z_ind]-RLDWU$ps) * ( (RLDWU$jr/75/15)/RLDWU$su - hydraulics$tact[1]/75/15) / (t(temp_soil$psi[1:z_ind]-RLDWU$ps) * (temp_soil$psi[1:z_ind]-RLDWU$ps))
    write.options.in(project.path = "./Hydrus_1D_Couvreur/CouvreurV2", krs, kcomp)
    # Run Hydrus1D ---
    system("./h1d_calc")
    #Load hydrus profile information
    hydrus <- read.nod_inf(project.path = "Hydrus_1D_Couvreur/CouvreurV2", 
                           out.file = paste0("Nod_Inf.out"))
    soil <- hydrus%>%
      transmute(id = Node,
                z = Depth,
                value = today,
                psi = Head)
    soil_global <- rbind(soil_global, soil)
  }
}
