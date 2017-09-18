setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Candidate_Point/Maroc_North")
FILES <- list.files(pattern = ".txt")
#Convert to .csv 
for (i in 1:length(FILES)) {
   FILE=read.csv(file=FILES[i],col.names=c("X", "Y"), sep ="", header = FALSE)
   print(FILE)
   write.csv(FILE,file=paste0("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/TT/",sub(".txt","",FILES[i]),".csv"), row.names=F, quote=F)
  # print(FILES[i])
}

#Groundtruth
#Read file tps
#GROUNDTRUTH
read.tps = function(data) {
  a = readLines(data)
  LM = grep("LM", a)
  ID.ind = grep("ID", a)  
  images = basename(gsub("(IMAGE=)(.*)", "\\2", a[ID.ind - 1]))
  
  skip = LM
  nrows = as.numeric(gsub("(LM=)([0-9])", "\\2", grep("LM", a, value=T)))
  l = length(LM)
  
  landmarks = vector("list", l)
  
  for (i in 1:l) {
    landmarks[i] = list(data.frame(
      read.table(file=data, header=F, skip=LM[i],
                 nrows=nrows[i], col.names=c("X", "Y")),
      IMAGE = images[i],
      ID = read.table(file=data, header=F, skip=ID.ind[i]-1, 
                      nrows=1, sep="=", col.names="ID")[2,],
      Scale = read.table(file=data, header=F, skip=ID.ind[i],
                         nrows=1, sep="=")[,2]))
  }
  do.call(rbind, landmarks)
}

setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/iMorph_data/9. CLIC/0_M_F")
data = read.tps("totlm4_SL_CLIC.TPS")
data

setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/GT")
checkpoint=1
for( i in 1:nrow(data)){
  if ((i %% 16) == 0){
    print(i)
    temp <- sample(data[checkpoint:i,])
    print(temp)
    write.csv(temp,file= paste(toString(data$IMAGE[i]),".csv"),quote=F,row.names=F,col.names=T)
    # write.csv(temp,file= paste("C:/Users/Admin/Documents/Intern/practice/Realdata/Groundtruth/",data$NAME,".csv"),quote=F,row.names=F,col.names=T)
    temp <- data.frame(matrix(ncol = 5,nrow = 15))
    checkpoint=i+1
  }
}

#Self-detect LM
read.tps = function(data) {
  a = readLines(data)
  LM = grep("LM", a)
  ID.ind = grep("ID", a)  
  images = basename(gsub("(IMAGE=)(.*)", "\\2", a[ID.ind - 1]))
  
  skip = LM
  nrows = as.numeric(gsub("(LM=)([0-9])", "\\2", grep("LM", a, value=T)))
  l = length(LM)
  
  landmarks = vector("list", l)
  
  for (i in 1:l) {
    landmarks[i] = list(data.frame(
      read.table(file=data, header=F, skip=LM[i],
                 nrows=nrows[i], col.names=c("X", "Y"))
      #IMAGE = images[i],
      #ID = read.table(file=data, header=F, skip=ID.ind[i]-1, 
      #                nrows=1, sep="=", col.names="ID")[2,],
      #Scale = read.table(file=data, header=F, skip=ID.ind[i],
      #                    nrows=1, sep="=")[,2]
    ))
  }
  do.call(rbind, landmarks)
}


setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Candidate_Point/GT/Maroc_Nord")
FILES <- list.files(pattern = ".TPS")
#Convert to .csv 
for (i in 1:length(FILES)) {
  FILE=read.tps(FILES[i])
  write.csv(FILE,file=paste0("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/GT/",sub(".TPS","",FILES[i]),".tif .csv"), row.names=F, quote=F)
}
