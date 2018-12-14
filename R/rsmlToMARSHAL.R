# 
# Copyright year 2018, Universitee catholique de Louvain
# All rights reserved.
# 
# Developers: Adrien Heymans
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
#Disclaimer
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.

# Import one rsml file and transmute it into data.frame which can be use with MARSHAL.
# archiDART is required (https://archidart.github.io/) to import rsml into the R environment.

function(path, age = "60", rep = 1){
  require(archiDART)
  Rootsys <- rsmlToTable(inputrsml = "C:/Users/heymansad/Documents/GitHub/marshal-pipeline", 
                       rsml.date = "emergence_time", rsml.connect = T, fitter = T, unitangle = "r")
  branchID <- 0
  k <- 0
  for(i in 2:nrow(Rootsys)){
    if(Rootsys$bran[i] == "true"){
      k <- Rootsys$root[i]
    }
    branchID <- c(branchID,k)
  }
  
  Table_data <- Rootsys%>%
    transmute(node1ID = c(0:(nrow(Rootsys)-1)),
              node2ID = node1ID+1,
              branchID = branchID,
              x1 = x1, y1 = y1, z1 = z1,
              x2 = x2, y2 = y2, z2 = z2,
              radius = (diameter1 + diameter2)/4,
              length = length,
              R = 0, G = 0, B =0,
              time = time,
              type = order,
              age = age,
              rep = rep)
  return(Table_data)
}
