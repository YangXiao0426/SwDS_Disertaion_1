setwd("/Users/yang/Desktop/Disertation_1")
data = read.csv('Data.csv',header = T)


#####################visualisation###################
#water
library(ggplot2)

water = data[,c(31:37,88)]

single_water_source = numeric(707)

for(i in 1:707){
  count = 0
  for (j in 1:7) {
    if(water[i,j]==T){
      count = count + 1
    }
  }
  if(count == 1){
    single_water_source[i] = i
  }
}

water_single = water[single_water_source,]
water_single$is_coinf[is.na(water_single$is_coinf)]=0

for(i in 1:694){
  for(j in 1:7){
    if(water_single[i,j] == T){
      water_single$type[i] = colnames(water_single[j])
    }
  }
  if(water_single$is_coinf[i]>=1){
    water_single$coinf[i] = 'co'
  }else{
    water_single$coinf[i] = 'single'
  }
} #obtain the use of each water source



water_n = as.data.frame(data[1,31:37])
water_n= as.data.frame(t(water_n))
for (i in 1:7) {
  water_n[i,1] = sum(water_single[,(i)] == T)
}

set = numeric(7)
water_n[,2] = set

p1<-ggplot(water_single, aes(x=type, y=is_coinf, fill=type)) +
  geom_bar(stat="identity")+theme_minimal() +
  labs(title="Co-infection VS  Water source", 
       x="Water source for patients", y = "Infection Number")+
  scale_fill_discrete(name = "Type")+ylim(0, 400)
p1  # this plot is to show how the single water source influence the coinf

water_n = water_n[-7,] # remove OtherWS

p2<-ggplot(water_n, aes(x=row.names(water_n), y=`1`, fill=row.names(water_n))) +
  geom_bar(stat="identity")+theme_minimal() +
  labs(title="Use number VS  Water source", 
       x="Water source for patients", y = "Use Number")+
  scale_fill_discrete(name = "Type")+ylim(0, 400)
p2

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p2,cols=2) # to draw p1 p2 together


#age
#count the infection number of each virus
data[,56]#start
patienceNumber = as.data.frame(data[1,56:87])
patienceNumber= as.data.frame(t(patienceNumber))

for (i in 1:31) {
  patienceNumber[i,1] = sum(data[,(i+55)] == "1")
}

patienceNumber
# age VS co-inf
ggplot(data, aes(x=Age, y=is_coinf,color=is_coinf)) + geom_point() +  
  labs(title="Co-infentions VS Age", 
       x="Age", y = "Tested Virus Type") + labs(colour = "Tested virus type")


################The risk factors and chi_test####################3

library(nnet)
library(AER)

test_all = data[,c(3,8,9,12,15:41,56:88)]
test_all_use = test_all[,c(-5,-6,-10,-11,-12,-19,-20,-27,-(32:63))]

test_all_use$is_coinf[is.na(test_all_use$is_coinf)]=0

for(i in 1:707){
  if(test_all_use$is_coinf[i] >2 ){
    test_all_use$is_coinf[i] = 'more than 3'
  }else if(test_all_use$is_coinf[i] == 2 ){
    test_all_use$is_coinf[i] = '2'
  }else if(test_all_use$is_coinf[i] == 1 ){
    test_all_use$is_coinf[i] = '1'
  }else{
    test_all_use$is_coinf[i] = '0'
  }
}

test_all_use$is_coinf = as.factor(test_all_use$is_coinf)

test_all_use = test_all_use[,-c(8:13)]

mlm_all = multinom(is_coinf~.,data = test_all_use)
summary(mlm_all)
coeftest(mlm_all)# the model


#anova
#age
test_all_use_n_age = test_all_use[,-1]
mlm_all_n_age = multinom(is_coinf~.,data = test_all_use_n_age)
summary(mlm_all_n_age)
coeftest(mlm_all_n_age)

anova(mlm_all,mlm_all_n_age)
# 3 73.61 <0.001

#site
test_all_use_n_site = test_all_use[,-2]
mlm_all_n_site = multinom(is_coinf~.,data = test_all_use_n_site)
summary(mlm_all_n_site)
coeftest(mlm_all_n_site)

anova(mlm_all,mlm_all_n_site)
#3 11.20 0.01

#gender
test_all_use_n_gender = test_all_use[,-3]
mlm_all_n_gender = multinom(is_coinf~.,data = test_all_use_n_gender)
summary(mlm_all_n_gender)
coeftest(mlm_all_n_gender)

anova(mlm_all,mlm_all_n_gender)
# 3 5.32 0.150

#stay
test_all_use_n_stay = test_all_use[,-4]
mlm_all_n_stay = multinom(is_coinf~.,data = test_all_use_n_stay)
summary(mlm_all_n_stay)
coeftest(mlm_all_n_stay)

anova(mlm_all,mlm_all_n_stay)
#3 1.89 0.597


#ContactDiar
test_all_use_n_contactD = test_all_use[,-5]
mlm_all_n_contactD = multinom(is_coinf~.,data = test_all_use_n_contactD)
summary(mlm_all_n_contactD)
coeftest(mlm_all_n_contactD)

anova(mlm_all,mlm_all_n_contactD)
# 3 5.98 0.113


#bloodstool
test_all_use_n_bloodS = test_all_use[,-6]
mlm_all_n_bloodS = multinom(is_coinf~., data = test_all_use_n_bloodS)
summary(mlm_all_n_bloodS)
coeftest(mlm_all_n_bloodS)

anova(mlm_all,mlm_all_n_bloodS)
#3 2.65 0.449

#mucoidStool 
test_all_use_n_mucoidS = test_all_use[,-7]
mlm_all_n_mucoidS = multinom(is_coinf~.,data = test_all_use_n_mucoidS)
summary(mlm_all_n_mucoidS)
coeftest(mlm_all_n_mucoidS)


anova(mlm_all,mlm_all_n_mucoidS)
#3 , 1.83 , 0.608

#water
test_all_use_n_water = test_all_use[,-c(8:14)]
mlm_all_n_water = multinom(is_coinf~.,data = test_all_use_n_water)
summary(mlm_all_n_water)
coeftest(mlm_all_n_water)

anova(mlm_all,mlm_all_n_water)
#21 34.12 0.035


#keep animal
test_all_use_n_keepAnimal = test_all_use[,-14]
mlm_all_n_keepAnimal = multinom(is_coinf~.,data = test_all_use_n_keepAnimal)
summary(mlm_all_n_keepAnimal)
coeftest(mlm_all_n_keepAnimal)

anova(mlm_all,mlm_all_n_keepAnimal)
#3 0.26 0.968

#kill anmial
test_all_use_n_killAnimal = test_all_use[,-15]
mlm_all_n_killAnimal = multinom(is_coinf~.,data = test_all_use_n_killAnimal)
summary(mlm_all_n_killAnimal)
coeftest(mlm_all_n_killAnimal)

anova(mlm_all,mlm_all_n_killAnimal)
# 3 0.55  0.908


#rawMeat
test_all_use_n_rawMeat = test_all_use[,-16]
mlm_all_n_rawMeat = multinom(is_coinf~.,data = test_all_use_n_rawMeat)
summary(mlm_all_n_rawMeat)
coeftest(mlm_all_n_rawMeat)

anova(mlm_all,mlm_all_n_rawMeat)
#3 0.53 0.912


#if_bact
test_all_use_n_if_bact = test_all_use[,-17]
mlm_all_n_if_bact = multinom(is_coinf~.,data = test_all_use_n_if_bact)
summary(mlm_all_n_if_bact)
coeftest(mlm_all_n_if_bact)

anova(mlm_all,mlm_all_n_if_bact)
#3 4.93 0.177


plot(data$is_coinf,data$KeepAnimal)


test_one_virus = test_all_use[,-18]
#kobu
test_kobu = test_one_virus
test_kobu$Kobuvirus = data$Kobuvirus
mlm_kobu = multinom(Kobuvirus~.,data = test_kobu)
summary(mlm_kobu)
coeftest(mlm_kobu)# the model

#noro
test_noro = test_one_virus
test_noro$Norovirus = data$Norovirus
mlm_noro = multinom(Norovirus~.,data = test_noro)
summary(mlm_noro)
coeftest(mlm_noro)# the model

#sapo
test_sapo = test_one_virus
test_sapo$Sapovirus = data$Sapovirus
mlm_sapo = multinom(Sapovirus~.,data = test_sapo)
summary(mlm_sapo)
coeftest(mlm_sapo)# the model

#mamastro
test_mamastro = test_one_virus
test_mamastro$Mamastrovirus = data$Mamastrovirus
mlm_mamastro = multinom(Mamastrovirus~.,data = test_mamastro)
summary(mlm_mamastro)
coeftest(mlm_mamastro)# the model

#mastadeno
test_mastadeno = test_one_virus
test_mastadeno$Mastadenovirus = data$Mastadenovirus
mlm_mastadeno = multinom(Mastadenovirus~.,data = test_mastadeno)
summary(mlm_mastadeno)
coeftest(mlm_mastadeno)# the model

#rota
test_rota = test_one_virus
test_rota$Rotavirus = data$Rotavirus
mlm_rota = multinom(Rotavirus~.,data = test_rota)
summary(mlm_rota)
coeftest(mlm_rota)# the model

###############################  how coinfections impact the disease severity ######

impacts = test_all[,c(1,4,8:9,10:12,49,51:52,54,59,62,64)]
write.csv(impacts,file = "impact.csv")

#
test_coinf_impact = impacts[,-c(8:13)]

#get coinfection binary data
coinf= as.numeric(data$is_coinf)
coinf[is.na(coinf)]=0

for (i in 1:707) {
  if(coinf[i] <= 1){
    test_coinf_impact$coinf_bi[i] = 0
  }else{
    test_coinf_impact$coinf_bi[i] = 1
  }#get coinf_bi
  
  if(coinf[i] == 1){
    test_coinf_impact$single_inf_bi[i] = 1
  }else{
    test_coinf_impact$single_inf_bi[i] = 0
  }#get single_inf_bi
}

for(i in 1:707){
  if(test_coinf_impact$coinf_bi[i] == 1){
    count = 0
    if(data$Kobuvirus[i] == 1){
      count = count + 1    
    }
    if(data$Mamastrovirus[i] == 1){
      count = count + 1    
    }
    if(data$Mastadenovirus[i] == 1){
      count = count + 1    
    }
    if(data$Rotavirus[i] == 1){
      count = count + 1    
    }
    if(data$Norovirus[i] == 1){
      count = count + 1    
    }
    if(data$Sapovirus[i] == 1){
      count = count + 1    
    }
    if(count == coinf[i]){
      test_coinf_impact$between_common[i] = 1
      test_coinf_impact$between_commo_other[i] = 0
    }else{
      test_coinf_impact$between_common[i] = 0
      test_coinf_impact$between_commo_other[i] = 1
    }
  }else{
    test_coinf_impact$between_common[i] = 0
    test_coinf_impact$between_commo_other[i] = 0
  }
}

#remove the col is_coinf and coinf_bi.  coinf_bi can be explained by singleinf__bi
test_coinf_impact = test_coinf_impact[,-8]

#rm NA or unknown data
test_coinf_impact$NumberDiarEpi[is.na(test_coinf_impact$NumberDiarEpi)]=mean(na.omit(test_coinf_impact$NumberDiarEpi))

#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#apply to the data
for (i in 1:707) {
  if(test_coinf_impact$AbdominalPain[i] == 9){
    test_coinf_impact$AbdominalPain[i] = getmode(test_coinf_impact$AbdominalPain)
  }
  if(test_coinf_impact$ThreeDaysFever[i] == 9){
    test_coinf_impact$ThreeDaysFever[i] = getmode(test_coinf_impact$ThreeDaysFever)
  }
}

# remove the variable Coinfection, because it can be replaced by single infection 
test_coinf_impact=test_coinf_impact[,-9] 

write.csv(test_coinf_impact,file = "impact.csv")

#pre-possessed data, and we fit the kernel ridge in python

########################################
# columns BD-CI
seq=data[,56:87]
dim(seq)
# recode for 32 virus
# 1 is yes, 2 is no
# no is 0, yes for virus 1 is 1, for virus 2 is 2. 
row=nrow(seq); col=ncol(seq)

for(i in 1:row){
  for(j in 1:col)
  {
    if(seq[i,j]==2)
    {
      seq[i,j]=0
    }
    if(seq[i,j]==1)
    {
      seq[i,j]=j
    }
  }
}

matplot(seq,col=1:32) # this plot is slow, be patient to wait
apply(seq,2,sum) 
apply(seq,2,sum)/c(1:32)
plot(apply(seq,2,sum)/c(1:32))
abline(h=10,col='green',lty=2)
abline(h=20,col='orange',lty=2)
abline(h=30,col='pink',lty=2)
abline(h=40,col='lightblue',lty=2)
abline(h=50,col='grey',lty=2)
legend('topleft',c('10','20','30','40','50'),col=c('green','orange','pink','lightblue','grey'),lty=2)
# the plot above is to routhly check the infection information, but too messy to get information


#To make this simple, we set a threshold 40
#Also we includes all common viruses
pcol=which(apply(seq,2,sum)/c(1:32)>40 | names(seq) == "Kobuvirus" | names(seq) =="Mamastrovirus")
pcol 

virusname=names(pcol)
matplot(seq[,pcol],col=1:9) # slow

#matplot is too messy, we choose heatmap
newdata=as.matrix(seq[,pcol])
heatmap(newdata) 
heatmap(newdata,Colv = NA, Rowv = NA) # show raw data

#rearrange
newdata2=newdata/matrix(rep(pcol,row),nrow=row,byrow=TRUE)
range(newdata2)
heatmap(t(newdata2),scale="col")

#count the infection number of each viruses
table(data$is_coinf)
#count the coinfection number of each combination
newdata2 = as.data.frame(newdata2)

#count a particular combination
# count = 0
# for (i in 1:707){
#     if(newdata2$Norovirus[i] == 1 & newdata2$Sapovirus[i] == 1){
#       count = count + 1
#     }
# }   #to check the outcome

count = matrix(0,9,9)
colnames(count) = colnames(newdata2)
row.names(count) = colnames(newdata2)

for (i in 9:2) {
  for (j in (i-1):1) {
    for (k in 1:707) {
      if(newdata2[k,names(newdata2[i])] == 1 & newdata2[k,names(newdata2[j])]){
        count[i,j] = count[i,j] +1
      }
    }
  }
}


