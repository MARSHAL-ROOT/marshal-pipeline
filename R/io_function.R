# 
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


# FUNCTION TO READ THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME
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




# FUNCTION TO WRITE THE RPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME

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



# FUNCTION TO WRITE THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME

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


# Needs the node table from SmartRoot
# rs = DataFrame with the node data
# order = the order of root for which the sigma needs to be computed
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


#Add info to vtp file
#This function is not yet finish
#root segment will not be connected with the good information

add.info.vtp <- function(path, name, x){
  
  xml <- read_xml(path)
  cell <- xml_find_all(xml, ".//CellData")
  array <- xml_children(cell)
  suf_data <- array[[1]]
  xml_attr(suf_data, "Name") <- name
  type_text <- xml_text(suf_data)
  
  xml_text(suf_data) <- paste0("\n", paste(x[], collapse=" "), "\n")
  xml_add_sibling(array[[2]], suf_data)
  # array <- xml_children(cell)
  # Create new element 
  # type_data <- array[[1]]
  # xml_add_sibling(array[[2]], type_data)
  xml_text(array[[1]]) <- type_text
  xml_attr(array[[1]], "Name") <- "type"
  write_xml(xml , "Hydraulic.vtp")
  
}

#Make a vtp file from MARSHAL output
#This function is not yet finish

#What should be implemented is
#the connectivity and offset

make.marshal.vtp <- function(root_data){
  setwd("C:/Users/heymansad/Documents/Thesis/2018-05 MARSHAL")
  path <- "param.vtp"
  xml <- read_xml(path)
  
  #Find the connector that were added to run MARSHAL
  nodals_ids <- unique(temp_root$branchID[root_data$type == 4 | 
                                            root_data$type == 5])
  link_node = link_time = link_xx2<- NULL
  for(no in nodals_ids){
    temp <- temp_root%>%
      filter(branchID == no)
    temp <- temp[1,]
    node2ID <- temp$node2ID
    link_node <- c(link_node, node2ID)
    timy <- temp$time
    link_time <- c(link_time, timy)
    xx2 <- temp$x2
    link_xx2 <- c(link_xx2, xx2)
  }
  
  #suppresion of the node that connect the basal and the
  #shoot born root the the tap root.
  supr <- NULL
  for(w in 1:length(link_node)){
    tmp_n <- link_node[w]
    tmp_t <- link_time[w]
    tmp_x <- link_xx2[w]
    one <- which(root_data$node2ID[] == tmp_n & root_data$time[] == tmp_t &
                   root_data$x2[] == tmp_x)


   # for(o in 1:length(one)){
   #   for(t in 1:length(two)){
   #       if(one[o] == two[t]){
            supr <- c(supr, one)
           # message(one[o])
            if(length(supr) > length(link_node)){
              message(paste0("error : length ", length(supr), "instead of ", 
                             length(link_node)))
            }
  #      }
  #    }
  #  }
  }
  root_data <- root_data%>%
    mutate(ID = 1:dim(root_data)[1])%>%
    filter(ID %!in% supr)%>%
    select(-ID)
  
  #Info to add to the .vtp file
  suf <- root_data$suf
  suf1 <- root_data$suf1
  kx <- root_data$kx
  kr <- root_data$kr
  jr <- root_data$jr
  psi <- root_data$psi
  jxl <- root_data$jxl
  psi_soil <- root_data$psi_soil
  
  #All coordinates
  x1 <- root_data$x1
  y1 <- root_data$y1
  z1 <- root_data$z1
  x2 <- root_data$x2
  y2 <- root_data$y2
  z2 <- root_data$z2
  pb = txtProgressBar(min = 0, max = length(x1), initial = 0, style = 3)
  k <- 0
  coor = su = su1 = r = x = ps = j = jx = ps_s = connect = off <- NULL
  for(i in 1:length(x1)){
    temp_coor <- paste(x1[i], y1[i], z1[i])
    coor <- c(coor, temp_coor)
    
    connect <- c(connect, k)
    k <- k + 1
    #MARSHAL output on all the coordinates
    temp_suf <- suf[i]
    temp_suf1 <- suf1[i]
    temp_kr <- kr[i]
    temp_kx <- kx[i]
    temp_psi <- psi[i]
    temp_jr <- jr[i]
    temp_jxl <- jxl[i]
    temp_psi_soil <- psi_soil[i]
    
    su <- c(su, temp_suf)
    su1 <- c(su1, temp_suf1)
    r <- c(r, temp_kr)
    x <- c(x, temp_kx)
    ps <- c(ps, temp_psi)
    j <- c(j, temp_jr)
    jx <- c(jx, temp_jxl)
    ps_s <- c(ps_s, temp_psi_soil)
    
    tmp <- root_data$branchID[i]
    if(i < length(x1)){
      if(root_data$branchID[i+1] != tmp){
        temp_coor <- paste(x2[i], y2[i], z2[i])
        coor <- c(coor, temp_coor)
        
        connect <- c(connect, k)
        off <- c(off , k)
        k <- k + 1
        
        su <- c(su, temp_suf)
        su1 <- c(su1, temp_suf1)
        r <- c(r, temp_kr)
        x <- c(x, temp_kx)
        ps <- c(ps, temp_psi)
        j <- c(j, temp_jr)
        jx <- c(jx, temp_jxl)
        ps_s <- c(ps_s, temp_psi_soil)
      }
    }
    else{
      temp_coor <- paste(x2[i], y2[i], z2[i])
      coor <- c(coor, temp_coor)
      
      connect <- c(connect, k)
      off <- c(off, k)
      k <- k + 1
      
      su <- c(su, temp_suf)
      su1 <- c(su1, temp_suf1)
      r <- c(r, temp_kr)
      x <- c(x, temp_kx)
      ps <- c(ps, temp_psi)
      j <- c(j, temp_jr)
      jx <- c(jx, temp_jxl)
      ps_s <- c(ps_s, temp_psi_soil)

    }

    setTxtProgressBar(pb,i)
  }
  #Aggregate the outut 
  output <- cbind(coor, suf = su,
           suf1 = su1,
           kr = r,
           kx = x,
           psi = ps,
           psi_soil = ps_s,
           jr = j,
           jxl = jx)
  posi <- which(output[,2] == "-Inf") 
  output[posi,2] <- -1000
  
  #Additionnal row for the MARSHAL outup in .vtp file
  points <- xml_find_all(xml, ".//PointData")
  array <- xml_children(points)
  for(i in 1:8){
  xml_add_sibling(array[[1]], array[[1]])
  }
  
  #Filling the line with MARSHAL outup
  marshal_out <- c("suf", "suf1", "kr", "kx", "jr", "jxl", "psi", "psi_soil")
  points <- xml_find_all(xml, ".//PointData")
  array <- xml_children(points)
  for(i in 1:8){
    name <- marshal_out[i]
    xml_attr(array[[i+1]], "Name") <- name
    x <- as.vector(output[,i+1])
    xml_text(array[[i+1]]) <- paste0("\n", paste(x[], collapse=" "), "\n")
  }
  
  piece <- xml_parent(xml_parent(array))
  Num_lines <- as.character(length(unique(root_data$branchID)))
  Num_points <- dim(output)
  xml_attr(piece, "NumberOfLines") <- Num_lines
  xml_attr(piece, "NumberOfPoints") <- Num_points
  
  write_xml(xml , "Hydraulic.vtp")
}
  
  

