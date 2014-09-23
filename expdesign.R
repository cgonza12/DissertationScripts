
bubLog = read.csv('~/Documents/cbexp/bubLog.csv')


nParticipants = 60
for(k in 1:nParticipants){

seqtype = rep(c("AB","AAB","AAAB"),each=2)
changetype = c("dissappear","size","move")

treatlist = data.frame(expand.grid(seqtype,changetype))
treatlist$rando = runif(nrow(treatlist))

treatlist = treatlist[order(treatlist$rando),]

f1 <- function(x){
  
  seqbatch = unlist(strsplit(as.character(x[[1]]),split = ""))
  preA = rep(sample(changetype[changetype!=x[[2]]],1),length(seqbatch)-1)
  condbatch = c(preA,as.character(x[[2]]))
  return(condbatch)
}


cond = unlist(lapply(split(treatlist,seq(NROW(treatlist))),FUN = f1))

batchcomp = list()
for(j in 1:nrow(treatlist)){
  
seqtypebatch = c() 

for(i in 1:(nchar(as.character(treatlist[j,1]))-1)){
  seqtypebatch[i] = paste(rep("A",i),sep="", collapse="")
  
  
}
batchcomp[[j]] = c(seqtypebatch,as.character(treatlist[j,1]))
}
type = unlist(batchcomp)

batchdat = data.frame(cond,type)
batchdat$type = as.character(batchdat$type)

# combine images with batch sequence
# first 10 always color and are randomized
allColor = seq(1,10,1)
allColor = allColor[order(runif(10))]

sizeImgs =  bubLog$image[bubLog$changeTypes=='size']
dissappearImgs = bubLog$image[bubLog$changeTypes=='dissappear']
moveImgs = bubLog$image[bubLog$changeTypes=='move']

imglist = c()
for(i in 1:nrow(batchdat)){
  
  switch(as.character(batchdat$cond[i]),
         size={
           
         imglist[i] = sample(sizeImgs[!(sizeImgs %in% imglist)] ,1)   
           
         },
         dissappear={
           
         imglist[i] = sample(dissappearImgs[!(dissappearImgs %in% imglist)] ,1)   
         },
         
         move={
           
        imglist[i] = sample(moveImgs[!(moveImgs %in% imglist)] ,1)   
           
         }
         
         
         )

}

imglist = c(allColor,imglist)
cond = c(rep('color',length(allColor)),as.character(batchdat$cond))
type = c(rep('pre',length(allColor)),batchdat$type)

pid = k
treatout = data.frame(pid,cond,type,imglist)

fname = paste0(k,'_treatout.csv')
write.csv(treatout,paste0('~/Documents/cbexp/treatlists/',fname),row.names=T)
}

