rm(list=ls())
gc()
library(raster)
library(sf)
library(foreach)
library(doParallel)
rasterOptions(tmpdir = "/Users/9oy/Documents/Projects/DSFResearch/Paper1/RstJunk")
num_cores = 8
cl = makeCluster(num_cores)
registerDoParallel(cl)

msa =read_sf("/Users/9oy/Documents/Data/US/Shape/MSA/nhgis0029_shape/nhgis0029_shapefile_tl2020_us_cbsa_2020/US_cbsa_2020.shp")
msa = msa[msa$LSAD == "M1" & !grepl("PR", msa$NAME), ]
allmetros = msa$NAME
allmetros = allmetros[!t(as.data.frame(strsplit(allmetros,", ")))[,2] %in% c("AK","HI")]

allras = list.files("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/FctImp/",pattern="*.tif",full.names = T)

file ="/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/FctImp/Annual_NLCD_FctImp_2023_CU_C1V0.tif"
msa=st_transform(msa,crs(raster(file)))

cnm = function(raster,shp){
  ras = raster::crop(raster, extent(shp))
  ras = raster::mask(ras,shp)
  return(ras)
}

Mout = rep(NA,380)

Mout = foreach(i = 1:380, .combine = 'cbind') %dopar% {
  library(raster)
  library(sf)
  #rasterOptions(tmpdir = "/Users/9oy/Documents/Projects/DSFResearch/Paper1/RstJunk")
  seqe = 1:39
  totalurban = rep(NA,39)
  years = 1985:2023
  for(j in seqe){
    ras = raster(allras[j])
    rassub = cnm(ras,msa[msa$NAME == allmetros[i],])
    rassub[rassub>100] = NA
    out = cellStats(rassub,sum,na.rm=T)
    totalurban[j] = out
  }
  mod = lm(log(totalurban)~years)
  slope = as.numeric(coefficients(mod)[2])
  r2 = summary(mod)$r.squared
  Fout = c(slope,r2)
  write.csv(1,paste("/Users/9oy/Desktop/Monitor/ImperviousProcessed_",as.character(i),".csv",sep=""))
  removeTmpFiles(h=0)
  return(Fout)
}

stopCluster(cl)
Mout = t(Mout)
outdf = data.frame(NAME=allmetros,slope = Mout[,1],Fout=Mout[,2])
write.csv(outdf,"/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/UrbanGrowth_Imp.csv")




