results = read.csv('~/Documents/cbexp/data/60_2014_Sep_19_1550.csv')[1:10,]
bubLog = read.csv('~/Documents/cbexp/bubLog.csv')
results$mousepos = gsub('\\[|\\]','',as.character(results$mousepos))

moslis = strsplit(results$mousepos,' ')

f1 = function(x){
  return(x[x!=""])
}
moslis = lapply(moslis,f1)
results$mosx = as.numeric(lapply(moslis,'[[',1))
results$mosy = as.numeric(lapply(moslis,'[[',2))

results$image = results$im

results = merge(results,bubLog[-1])


results$corr = c()

for(i in 1:nrow(results)){
x = results[i,]

if(x['acc']=='s' &
x['mosx']<x['xmax'] &
  x['mosx']>x['xmin'] &    
  x['mosy']<x['ymax'] &
  x['mosy']>x['ymin']){
  
  results$corr[i] = 1
} else{
  
  results$corr[i] = 0
}

}