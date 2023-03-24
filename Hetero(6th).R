
#### splitting dataset by row.
n=50
sample_data = data.frame(sample_data)
heta_data = split(sample_data, factor(sort(rank(row.names(sample_data))%%n)))
heta_data

#sub-experiment1
omega=t(rnorm(50))
omega
e1=heta_data[[1]][1,]+omega
e11=heta_data[[1]][2,]+omega
sub1 = rbind(e1,e11)
sub1
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_1=center_colmeans(sub1)

#sub-experiment2
omega2=t(rnorm(50))
omega2
e2=heta_data[[2]][1,]+omega2
e22=heta_data[[2]][2,]+omega2
sub2 = rbind(e2,e22)
sub2
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_2=center_colmeans(sub2)

#sub-experiment3
omega3=t(rnorm(50))
omega3
e3=heta_data[[3]][1,]+omega3
e33=heta_data[[3]][2,]+omega3
sub3 = rbind(e3,e33)
sub3
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_3=center_colmeans(sub3)

#sub-experiment4
omega4=t(rnorm(50))
omega4
e4=heta_data[[4]][1,]+omega4
e44=heta_data[[4]][2,]+omega4
sub4 = rbind(e4,e44)
sub4
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_4=center_colmeans(sub4)

#sub-experiment5
omega5=t(rnorm(50))
omega5
e5=heta_data[[5]][1,]+omega5
e55=heta_data[[5]][2,]+omega5
sub5 = rbind(e5,e55)
sub5
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_5=center_colmeans(sub5)

#sub-experiment6
omega6=t(rnorm(50))
omega6
e6=heta_data[[6]][1,]+omega6
e66=heta_data[[6]][2,]+omega6
sub6 = rbind(e6,e66)
sub6
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_6=center_colmeans(sub6)

#sub-experiment7
omega7=t(rnorm(50))
omega7
e7=heta_data[[7]][1,]+omega7
e77=heta_data[[7]][2,]+omega7
sub7 = rbind(e7,e77)
sub7
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_7=center_colmeans(sub7)

#sub-experiment8
sub8=heta_data[[8]]
sub8
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_8=center_colmeans(sub8)

#sub-experiment 9
omega9=t(rnorm(50))
omega9
e9=heta_data[[9]][1,]+omega9
e99=heta_data[[9]][2,]+omega9
sub9 = rbind(e9,e99)
sub9
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_9=center_colmeans(sub9)

#sub-experiment 10
omega10=t(rnorm(50))
omega10
e10=heta_data[[10]][1,]+omega10
e110=heta_data[[10]][2,]+omega10
sub10 = rbind(e10,e110)
sub10
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_10=center_colmeans(sub10)

#sub-experiment 11
omega11=t(rnorm(50))
omega11
e11=heta_data[[11]][1,]+omega11
e111=heta_data[[11]][2,]+omega11
sub11 = rbind(e11,e111)
sub11
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_11=center_colmeans(sub11)

#sub-experiment 12
omega12=t(rnorm(50))
omega12
e12=heta_data[[12]][1,]+omega12
e112=heta_data[[12]][2,]+omega12
sub12 = rbind(e12,e112)
sub12
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_12=center_colmeans(sub12)

#sub-experiment 13
omega13=t(rnorm(50))
omega13
e13=heta_data[[13]][1,]+omega13
e113=heta_data[[13]][2,]+omega13
sub13 = rbind(e13,e113)
sub13
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_13=center_colmeans(sub13)

#sub-experiment 14
omega14=t(rnorm(50))
omega14
e14=heta_data[[14]][1,]+omega14
e114=heta_data[[14]][2,]+omega14
sub14 = rbind(e14,e114)
sub14
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_14=center_colmeans(sub14)

#sub-experiment 15
omega15=t(rnorm(50))
omega15
e15=heta_data[[15]][1,]+omega15
e115=heta_data[[15]][2,]+omega15
sub15 = rbind(e15,e115)
sub15
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_15=center_colmeans(sub15)

#sub-experiment 16
omega16=t(rnorm(50))
omega16
e16=heta_data[[16]][1,]+omega16
e116=heta_data[[16]][2,]+omega16
sub16 = rbind(e16,e116)
sub16
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_16=center_colmeans(sub16)

#sub-experiment 17
omega17=t(rnorm(50))
omega17
e17=heta_data[[17]][1,]+omega17
e117=heta_data[[17]][2,]+omega17
sub17 = rbind(e17,e117)
sub17
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_17=center_colmeans(sub17)

#sub-experiment 18
omega18=t(rnorm(50))
omega18
e18=heta_data[[18]][1,]+omega18
e118=heta_data[[18]][2,]+omega18
sub18 = rbind(e18,e118)
sub18
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_18=center_colmeans(sub18)

#sub-experiment 19
omega19=t(rnorm(50))
omega19
e19=heta_data[[19]][1,]+omega19
e119=heta_data[[19]][2,]+omega19
sub19 = rbind(e19,e119)
sub19
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_19=center_colmeans(sub19)

#sub-experiment 20
omega20=t(rnorm(50))
omega20
e20=heta_data[[20]][1,]+omega20
e120=heta_data[[20]][2,]+omega20
sub20 = rbind(e20,e120)
sub20
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_20=center_colmeans(sub20)

#sub-experiment 21
omega21=t(rnorm(50))
omega21
e21=heta_data[[21]][1,]+omega21
e121=heta_data[[21]][2,]+omega21
sub21 = rbind(e21,e121)
sub21
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_21=center_colmeans(sub21)

#sub-experiment 22
omega22=t(rnorm(50))
omega22
e22=heta_data[[22]][1,]+omega22
e122=heta_data[[22]][2,]+omega22
sub22 = rbind(e22,e122)
sub22
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_22=center_colmeans(sub22)

#sub-experiment 23
omega23=t(rnorm(50))
omega23
e23=heta_data[[23]][1,]+omega23
e123=heta_data[[23]][2,]+omega23
sub23 = rbind(e23,e123)
sub23
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_23=center_colmeans(sub23)

#sub-experiment 24
omega24=t(rnorm(50))
omega24
e24=heta_data[[24]][1,]+omega24
e124=heta_data[[24]][2,]+omega24
sub24 = rbind(e24,e124)
sub24
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_24=center_colmeans(sub24)

#sub-experiment 25
omega25=t(rnorm(50))
omega25
e25=heta_data[[25]][1,]+omega25
e125=heta_data[[25]][2,]+omega25
sub25 = rbind(e25,e125)
sub25
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_25=center_colmeans(sub25)

#sub-experiment 26
omega26=t(rnorm(50))
omega26
e26=heta_data[[26]][1,]+omega26
e126=heta_data[[26]][2,]+omega26
sub26 = rbind(e26,e126)
sub26
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_26=center_colmeans(sub26)

#sub-experiment 27
omega27=t(rnorm(50))
omega27
e27=heta_data[[27]][1,]+omega27
e127=heta_data[[27]][2,]+omega27
sub27 = rbind(e27,e127)
sub27
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_27=center_colmeans(sub27)

#sub-experiment 28
omega28=t(rnorm(50))
omega28
e28=heta_data[[28]][1,]+omega28
e128=heta_data[[28]][2,]+omega28
sub28 = rbind(e28,e128)
sub28
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_28=center_colmeans(sub28)

#sub-experiment 29
omega29=t(rnorm(50))
omega29
e29=heta_data[[29]][1,]+omega29
e129=heta_data[[29]][2,]+omega29
sub29 = rbind(e29,e129)
sub29
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_29=center_colmeans(sub29)

#sub-experiment 30
omega30=t(rnorm(50))
omega30
e30=heta_data[[30]][1,]+omega30
e130=heta_data[[30]][2,]+omega30
sub30 = rbind(e30,e130)
sub30
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_30=center_colmeans(sub30)

#sub-experiment 31
omega31=t(rnorm(50))
omega31
e31=heta_data[[31]][1,]+omega31
e131=heta_data[[31]][2,]+omega31
sub31 = rbind(e31,e131)
sub31
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_31=center_colmeans(sub31)

#sub-experiment 32
omega32=t(rnorm(50))
omega32
e32=heta_data[[32]][1,]+omega32
e132=heta_data[[32]][2,]+omega32
sub32 = rbind(e32,e132)
sub32
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_32=center_colmeans(sub32)

#sub-experiment 33
omega33=t(rnorm(50))
omega33
e33=heta_data[[33]][1,]+omega33
e133=heta_data[[33]][2,]+omega33
sub33 = rbind(e33,e133)
sub33
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_33=center_colmeans(sub33)

#sub-experiment 34
omega34=t(rnorm(50))
omega34
e34=heta_data[[34]][1,]+omega34
e134=heta_data[[34]][2,]+omega34
sub34 = rbind(e34,e134)
sub34
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_34=center_colmeans(sub34)

#sub-experiment 35
omega35=t(rnorm(50))
omega35
e35=heta_data[[35]][1,]+omega35
e135=heta_data[[35]][2,]+omega35
sub35 = rbind(e35,e135)
sub35
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_35=center_colmeans(sub35)

#sub-experiment 36
omega36=t(rnorm(50))
omega36
e36=heta_data[[36]][1,]+omega36
e136=heta_data[[36]][2,]+omega36
sub36 = rbind(e36,e136)
sub36
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_36=center_colmeans(sub36)

#sub-experiment 37
omega37=t(rnorm(50))
omega37
e37=heta_data[[37]][1,]+omega37
e137=heta_data[[37]][2,]+omega37
sub37 = rbind(e37,e137)
sub37
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_37=center_colmeans(sub37)

#sub-experiment 38
omega38=t(rnorm(50))
omega38
e38=heta_data[[38]][1,]+omega38
e138=heta_data[[38]][2,]+omega38
sub38 = rbind(e38,e138)
sub38
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_38=center_colmeans(sub38)

#sub-experiment 39
omega39=t(rnorm(50))
omega39
e39=heta_data[[39]][1,]+omega39
e139=heta_data[[39]][2,]+omega39
sub39 = rbind(e39,e139)
sub39
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_39=center_colmeans(sub39)

#sub-experiment 40
omega40=t(rnorm(50))
omega40
e40=heta_data[[40]][1,]+omega40
e140=heta_data[[40]][2,]+omega40
sub40 = rbind(e40,e140)
sub40
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_40=center_colmeans(sub40)

#sub-experiment 41
omega41=t(rnorm(50))
omega41
e41=heta_data[[41]][1,]+omega41
e141=heta_data[[41]][2,]+omega41
sub41 = rbind(e41,e141)
sub41
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_41=center_colmeans(sub41)

#sub-experiment 42
omega42=t(rnorm(50))
omega42
e42=heta_data[[42]][1,]+omega42
e142=heta_data[[42]][2,]+omega42
sub42 = rbind(e42,e142)
sub42
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_42=center_colmeans(sub42)

#sub-experiment 43
omega43=t(rnorm(50))
omega43
e43=heta_data[[43]][1,]+omega43
e143=heta_data[[43]][2,]+omega43
sub43 = rbind(e43,e143)
sub43
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_43=center_colmeans(sub43)

#sub-experiment 44
omega44=t(rnorm(50))
omega44
e44=heta_data[[44]][1,]+omega44
e144=heta_data[[44]][2,]+omega44
sub44 = rbind(e44,e144)
sub44
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_44=center_colmeans(sub44)

#sub-experiment 45
omega45=t(rnorm(50))
omega45
e45=heta_data[[45]][1,]+omega45
e145=heta_data[[45]][2,]+omega45
sub45 = rbind(e45,e145)
sub45
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_45=center_colmeans(sub45)

#sub-experiment 46
omega46=t(rnorm(50))
omega46
e46=heta_data[[46]][1,]+omega46
e146=heta_data[[46]][2,]+omega46
sub46 = rbind(e46,e146)
sub46
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_46=center_colmeans(sub46)

#sub-experiment 47
omega47=t(rnorm(50))
omega47
e47=heta_data[[47]][1,]+omega47
e147=heta_data[[47]][2,]+omega47
sub47 = rbind(e47,e147)
sub47
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_47=center_colmeans(sub47)

#sub-experiment 48
omega48=t(rnorm(50))
omega48
e48=heta_data[[48]][1,]+omega48
e148=heta_data[[48]][2,]+omega48
sub48 = rbind(e48,e148)
sub48
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_48=center_colmeans(sub48)

#sub-experiment 49
omega49=t(rnorm(50))
omega49
e49=heta_data[[49]][1,]+omega49
e149=heta_data[[49]][2,]+omega49
sub49 = rbind(e49,e149)
sub49
#centralized
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_49=center_colmeans(sub49)

#sub-experiment 50
omega50=t(rnorm(50))
omega50
e50=heta_data[[50]][1,]+omega50
e150=heta_data[[50]][2,]+omega50
sub50 = rbind(e50,e150)
sub50
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
} 
cen_50=center_colmeans(sub50)

#central_correlation
center_data = rbind(cen_1,cen_2,cen_3,cen_4,cen_5,cen_6,cen_7,cen_8,cen_9,cen_10,cen_11,cen_12,cen_13,
                    cen_14,cen_15,cen_16,cen_17,cen_18,cen_19,cen_20,cen_21,cen_22,cen_23,cen_24,cen_25,
                    cen_26,cen_27,cen_28,cen_29,cen_30,cen_31,cen_32,cen_33,cen_34,cen_35,cen_36,cen_37,cen_38,
                    cen_39,cen_40,cen_41,cen_42,cen_43,cen_44,cen_45,cen_46,cen_47,cen_48,cen_49,cen_50)
center_data
center_corr = cor(center_data,method="pearson")
center_corr
dev.off()

#network
center_corr[center_corr<0.9] <- 0
center_corr[center_corr>=0.9] <- 1
corrplot(center_corr , addCoef.col="pink",number.cex=0.55)
network <- graph_from_adjacency_matrix( center_corr, weighted=T, mode="undirected", diag=F)

dev.off()
par(bg="black", mar=c(0,0,0,0))
plot(network,vertex.size=12,
     vertex.color="green", 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent") 

het_center = cluster_edge_betweenness(network)
membership(het_center)
plot(het_center,network)


#merge all sub experimental data
sub_data = rbind(sub1,sub2,sub3,sub4,sub5,sub6,sub7,sub8,sub9,sub10,sub11,sub12,sub13,sub14,sub16,sub17,sub18
                 ,sub19,sub20,sub21,sub22,sub23,sub24,sub25,sub26,sub27,sub28,sub29,sub30,sub31,sub32,sub33,
                 sub34,sub35,sub36,sub37,sub38,sub39,sub40,sub41,sub42,sub43,sub44,sub45,sub46,sub47,sub48,
                 sub49,sub50)
sub_data

##without centering
wc_corr = cor(sub_data,method="pearson")
wc_corr
wc_corr[wc_corr<0.9] <- 0
wc_corr[wc_corr>=0.9] <- 1
dev.off()
corrplot(wc_corr , addCoef.col="pink",number.cex=0.55)
wc_network <- graph_from_adjacency_matrix( wc_corr, weighted=T, mode="undirected", diag=F)

dev.off()
par(bg="black", mar=c(0,0,0,0))
plot(wc_network,vertex.size=12,
     vertex.color="green", 
     vertex.label.cex=0.7,
     vertex.label.color="blue",
     vertex.frame.color="transparent")

het_non_center = cluster_edge_betweenness(wc_network)
membership(het_non_center)
membership(het_center)
plot(het_non_center,wc_network)
plot(het_center,network)

adj.rand.index(membership(true_member),membership(het_center))
adj.rand.index(membership(true_member),membership(het_non_center))
