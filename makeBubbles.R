# make N change blindness bubble arrays
#   - N/4 color, size, dissappear, move changes 
#   - 4 colors
#   - 4 sizes
#   - 4 moves (x,y + 1,2 SDs)
# 
# trial
#   - make x,y coords and sizes
#   - plot A array
#   - save to file Na.jpg
# 
#   - sample rows size 1
#   - determine change type
#   - if C S or M
#     - determine what attribute the the A bubble has, save
#     - draw AOI box around A stim, save
#     - randomly select one of three remianing 
#     - replace with change, save
# 
#   - plot B array
#   - save to file Nb.jpg
# 
#   - Make A array with 'please click change title'
# 
# BubbleLog
#   - image
#   - change type
#   - imageA file name
#   - A x coord
#   - A y coord
#   - A color
#   - A size
#   - imageB file name
#   - B x coord
#   - B y coord
#   - B color
#   - B size
# 
# 
# 
# global parameters
#   - N bubbles
#   - sd of bubble distribution
#   - image size
#   - size range

library(ggplot2)
library(gtools)
library(gridExtra)
######### Global Parameters ##########
nImg = 190
nBubs = 28
minx = -3*bubSD
maxx =  3*bubSD
miny = -2*bubSD
maxy =  2*bubSD
scaleValx = (900/2)/maxx
scaleValy = (600/2)/maxy
minx = -3*bubSD*scaleValx
maxx =  3*bubSD*scaleValx
miny = -2*bubSD*scaleValy
maxy =  2*bubSD*scaleValy
bubSD = 1.5
imageH = 2
imageW = 3
textsize = 4

sizeRange = c(2,6)
tol=.25
res = 300
nCol = 10
nRem = nImg - nCol
imgWpx = 900
imgHpx = 600

########## Vectors for BubbleLog #########

bubLog = list()

########### Trial ########

changeTypes = c(rep('color',10),rep(c('size', 'dissappear', 'move'),each=nRem/3))
image = seq(1,nImg,1)

for(i in 1:length(image)){
  # make bubdatA
  x = rnorm(nBubs,0,bubSD*1.25)*scaleValx
  x = x[x>(minx-.1) & x<(maxx+.1)]
  
  y = rnorm(nBubs,0,bubSD)*scaleValy
  y = y[y>(miny-.1) & y<(maxy+.1)]
  
  x = x[1:min(length(x),length(y))]
  y = y[1:min(length(x),length(y))]
  
  size = rnorm(nBubs)
  color = rep(c("C1","C2","C3","C4"),each=nBubs/4)
  color = sample(color)[1:length(x)]
  size = quantcut(size,labels=c("S1","S2","S3","S4"))
  size = sample(size)[1:length(x)]
  
  
  
  bubdatA = data.frame(x,y,size,color)
  
  # plot array A
  ggplot(bubdatA,aes(x=x,y=y,size=size,color=color))+
    geom_point()+
    scale_size_discrete(range = sizeRange)+
    guides(size=F,color=F)+
    scale_x_continuous(expand = c(0, 0),limits=c(minx,maxx)) + 
    scale_y_continuous(expand = c(0, 0),limits=c(miny,maxy))+
    theme_minimal()+
    theme(text=element_blank(),
          line=element_blank(),
          title=element_blank(),
          plot.margin=unit(c(0,0,-6,-6),"mm"))
  
  
  Afile = paste0(image[i],'a.jpg')
  
  ggsave(Afile,path = '~/Documents/cbexp/images/',width = imageW,height = imageH,units ='in',dpi=res)
  
  # select row to change
  
  bubdatB = bubdatA
  changeBub = sample(nrow(bubdatA), 1)
  thisChange = changeTypes[i]
  
  switch(thisChange,
         color ={
            bubColors = unique(bubdatA$color)
            thisColor = bubdatB[changeBub,thisChange]
            bubdatB[changeBub,thisChange] <- sample(bubColors[bubColors!=thisColor],size = 1)
           
         },
         size ={
           bubSizes = unique(bubdatA$size)
           thisSize = bubdatB[changeBub,thisChange]
           bubdatB[changeBub,thisChange] <- sample(bubSizes[bubSizes!=thisSize],size = 1)
           
         },
         
         move = {
           
           bubXY = c('x1sd','x2sd','y1sd','y2sd')
           
           changeXY = sample(bubXY,size = 1)
           
               switch(changeXY,
                      x1sd={
                        bubdatB[changeBub,'x'] <- bubdatB[changeBub,'x']+.15*bubSD
                        
                      },
                      x2sd={
                        bubdatB[changeBub,'x'] <- bubdatB[changeBub,'x']-.15*bubSD
                        
                      },
                      y1sd={
                        bubdatB[changeBub,'y'] <- bubdatB[changeBub,'y']+.15*bubSD
                        
                      },
                      y2sd={
                        bubdatB[changeBub,'y'] <- bubdatB[changeBub,'y']-.15*bubSD
                        
                      }
                      )
              
           
           
         },
         dissappear = {
           
           bubdatB[changeBub,] <- c(NA,NA,NA,NA)
           
         }
  )
    
           
       # plot array B
       ggplot(bubdatB,aes(x=x,y=y,size=size,color=color))+
         geom_point()+
         scale_size_discrete(range = sizeRange)+
          guides(size=F,color=F)+
          scale_x_continuous(expand = c(0, 0),limits=c(minx,maxx)) + 
          scale_y_continuous(expand = c(0, 0),limits=c(miny,maxy))+
          theme_minimal()+
          theme(text=element_blank(),
                line=element_blank(),
                title=element_blank(),
                plot.margin=unit(c(0,0,-6,-6),"mm"))
  
       Bfile = paste0(image[i],'b.jpg')
       ggsave(Bfile,path = '~/Documents/cbexp/images/',width = imageW,height = imageH,units ='in',dpi=res)
       
     
  
  # plot instruction array
  xmin = round(bubdatA[changeBub,'x'] - tol*bubSD*scaleValx*1.5,2)
  xmax = round(bubdatA[changeBub,'x'] + tol*bubSD*scaleValx*1.5,2)
  ymin = round(bubdatA[changeBub,'y'] - tol*bubSD*scaleValy,2)
  ymax = round(bubdatA[changeBub,'y'] + tol*bubSD*scaleValy,2)
  
  ggplot(bubdatA,aes(x=x,y=y,size=size,color=color))+
    geom_point()+
    scale_size_discrete(range = sizeRange)+
    guides(size=F,color=F)+
    annotate('rect',xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,color='black',alpha=.01)+
    annotate('text',y=maxy,x=maxx,label='Please click on the change',
             size = textsize,color='red',hjust=1.15,vjust=1.5)+
    annotate('text',x=bubdatA[changeBub,]['x'],y=bubdatA[changeBub,]['y'],label=
               paste0(round(bubdatA[changeBub,]['x']),',', round(bubdatA[changeBub,]['y'])),vjust=1)+
    annotate('point',x=bubdatA[changeBub,]['x'],y=bubdatA[changeBub,]['y'])+
    annotate('point',x=0,y=0,color='black')+
    annotate('point',x=maxx,y=maxy,color='red')+
    annotate('point',x=maxx,y=miny,color='red')+
    annotate('point',x=minx,y=maxy,color='red')+
    annotate('point',x=minx,y=miny,color='red')+
    scale_x_continuous(expand = c(0, 0),limits=c(minx,maxx)) + 
    scale_y_continuous(expand = c(0, 0),limits=c(miny,maxy))+
    theme_minimal()+
    theme(text=element_blank(),
          line=element_blank(),
          title=element_blank(),
          plot.margin=unit(c(0,0,-6,-6),"mm"))

  
  
  instfile = paste0(image[i],'inst.jpg')
  ggsave(instfile,path = '~/Documents/cbexp/images/',width = imageW,height = imageH,units ='in',dpi=res)
  
  ggplot(bubdatA,aes(x=x,y=y,size=size,color=color))+
    geom_point(alpha=0)+
    scale_size_discrete(range = sizeRange)+
    guides(size=F,color=F)+
    scale_x_continuous(expand = c(0, 0),limits=c(minx,maxx)) + 
    scale_y_continuous(expand = c(0, 0),limits=c(miny,maxy))+
    theme_minimal()+
    theme(text=element_blank(),
          line=element_blank(),
          title=element_blank(),
          plot.margin=unit(c(0,0,-6,-6),"mm"))

  
  
  blankfile = paste0(image[i],'blank.jpg')
  ggsave(blankfile,path = '~/Documents/cbexp/images/',width = imageW,height = imageH,units ='in',dpi=res)
  
  
  # save change bub info and aoi 
  # need to convert these into pixels!!, need screen size and resolution
  xpx = round(bubdatA[changeBub,'x'])
  ypx = round(bubdatA[changeBub,'y'])
  xmin = round((bubdatA[changeBub,'x']) - (tol*bubSD*scaleValx*1.5),2)
  xmax = round((bubdatA[changeBub,'x']) + (tol*bubSD*scaleValx*1.5),2)
  ymin = round((bubdatA[changeBub,'y']) - (tol*bubSD*scaleValy),2)
  ymax = round((bubdatA[changeBub,'y']) + (tol*bubSD*scaleValy),2)
  
  
  
  aoi = data.frame(xpx,ypx,xmin,xmax,ymin,ymax)
  thisChangeLog = cbind(bubdatA[changeBub,],bubdatB[changeBub,],aoi)
  
  bubLog[[i]] = thisChangeLog
  
  print(paste0(round(i/nImg*100),"% done!"))
  
}

bubLog = do.call(rbind,bubLog)
bubLog = cbind(image,changeTypes,bubLog)

write.csv(bubLog,'~/Documents/cbexp/bubLog.csv')



