#SPISA {psychotree}	R Documentation
#SPIEGEL Studentenpisa Data (Subsample)
#Description
#
#A subsample from the general knowledge quiz ?Studentenpisa? conducted online by the German weekly news magazine SPIEGEL. The data contain the quiz results from 45 questions as well as sociodemographic data for 1075 university students from Bavaria.
#Usage
#
#data("SPISA")
#
#Format
#
#A data frame containing 1075 observations on 6 variables.
#
#spisa
#
#    matrix with 0/1 results from 45 questions in the quiz (indicating wrong/correct answers).
#gender
#
#    factor indicating gender.
#age
#
#    age in years.
#semester
#
#    numeric indicating semester of university enrollment.
#elite
#
#    factor indicating whether the university the student is enrolled in has been granted ?elite? status by the German ?excellence initiative?.
#spon
#
#    ordered factor indicating frequency of accessing the SPIEGEL online (SPON) magazine.
#
#Details
#
#An online quiz for testing one's general knowledge was conducted by the German weekly news magazine SPIEGEL in 2009. Overall, about 700,000 participants answered the quiz and a set of sociodemographic questions. The general knowledge quiz consisted of a total of 45 items from five different topics: politics, history, economy, culture and natural sciences. For each topic, four different sets of nine items were available, that were randomly assigned to the participants. A thorough analysis and discussion of the original data set is provided in Trepte and Verbeet (2010).
#
#Here, we provide the subsample of university students enrolled in the federal state of Bavaria, who had been assigned questionnaire number 20 (so that all subjects have answered the same set of items). Excluding all incomplete records, this subsample contains 1075 observations.
#
#The data are analyzed in Strobl et al. (2010), whose analysis is replicated in vignette("raschtree", package = "psychotree").
#
#The full list of items in questionnaire 20 is given below.
#
#
#
#
#
#

#Politics:

F1="Who determines the rules of action in German politics according to the constitution? ? The Bundeskanzler (federal chancellor)."
F2="What is the function of the second vote in the elections to the German Bundestag (federal parliament)? ? It determines the allocation of seats in the Bundestag.  "
F3="How many people were killed by the RAF (Red Army Faction)? ? 33. "
F4=" Where is Hessen (i.e., the German federal country Hesse) located? ? (Indicate location on a map.)   "
F5=" What is the capital of Rheinland-Pfalz (i.e., the German federal country Rhineland-Palatinate)? ? Mainz. "
F6=" Who is this? ? (Picture of Horst Seehofer.)     "
F7=" Which EU institution is elected in 2009 by the citizens of EU member countries? ? European Parliament.  "
F8=" How many votes does China have in the UNO general assembly? ? 1.    "
F9=" Where is Somalia located? ? (Indicate location on a map.)  "

#History:
 F10="The Roman naval supremacy was established through... ? ... the abolition of Carthage. "
F11="In which century did the Thirty Years' War take place? ? The 17th century.  "
F12="Which form of government is associated with the French King Louis XIV? ? Absolutism. "
F13="What island did Napoleon die on in exile? ? St. Helena.     "
F14="How many percent of the votes did the NSDAP receive in the 1928 elections of the German Reichstag? ? About 3 percent. "
F15="How many Jews were killed by the Nazis during the Holocaust? ? About 6 Million. "
F16="Who is this? ? (Picture of Johannes Rau, former German federal president.)  "
F17="Which of the following countries is not a member of the EU? ? Croatia. "
F18="How did Mao Zedong expand his power in China? ? The Long March.   "

# Economy:
F19="Who is this? ? (Picture of Dieter Zetsche, CEO of Mercedes-Benz.)     "
F20="What is the current full Hartz IV standard rate (part of the social welfare) for adults? ? 351 Euro.   "
F21="What was the average per capita gross national product in Germany in 2007? ? About 29,400 Euro.  "
F22=" What is a CEO? ? A Chief Executive Officer.     "
F23="What is the meaning of the hexagonal ?organic? logo? ? Synthetic pesticides are prohibited.  "
F24="Which company does this logo represent? ? Deutsche Bank.  "
F25="Which German company took over the British automobile manufacturers Rolls-Royce? ? BMW.    "
F26="Which internet company took over the media group Time Warner? ? AOL.  "
F27="What is the historic meaning of manufacturies? ? Manufacturies were the precursors of industrial mass production.  "

# Culture:
F28="Which painter created this painting? ? Andy Warhol. "
F29="What do these four buildings have in common? ? All four were designed by the same architects.  "
F30="Roman numbers: What is the meaning of CLVI? ? 156.    "
F31="What was the German movie with the most viewers since 1990? ? Der Schuh des Manitu. "
F32="In which TV series was the US president portrayed by an African American actor for a long time? ? 24.   "
F33="What is the name of the bestselling novel by Daniel Kehlmann? ? Die Vermessung der Welt (Measuring The World). "
F34="Which city is the setting for the novel ?Buddenbrooks?? ? L?beck.      "
F35="In which city is this building located? ? Paris."
F36="Which one of the following operas is not by Mozart? ? Aida. "

# Natural sciences:
F37="Why does an ice floe not sink in the water? ? Due to the lower density of ice.  "
F38="What is ultrasound not used for? ? Radio.      "
F39="Which sensory cells in the human eye make color vision possible? ? Cones. "
F40="What is also termed Trisomy 21? ? Down syndrome.      "
F41="Which element is the most common in the Earth's atmosphere? ? Nitrogen. "
F42="Which kind of tree does this leaf belong to? ? Maple.    "
F43="Which kind of bird is this? ? Blackbird.     "
F44="Where is the stomach located? ? (Indicate location on a map of the body.) "
F45="What is the sum of interior angles in a triangle? ? 180 degrees.   "




 NAMES=c(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,F25,F26,F27,F28,F29,F30,F31,F32,F33,F34,F35,F36,F37,F38,F39,F40,F41,F42,F43,F44,F45)


library(psychotree)
data(SPISA)



X=SPISA$spisa




wx= SPISA$gender=="male"
wy= SPISA$gender=="female"
v=wx/sum(wx)-wy/sum(wy)

ans=oofos::optimize_on_context_extents(X,objective=v)

 bmax=gurobi::gurobi(ans)
 # 0.3350353
 ans$objval <- bmax$objval
 test <- oofos::compute_extent_optim_test(ans)

NAMES[which(bmax$x[-(1:1075)]>0.5)]
#[1] " Who is this? ? (Picture of Horst Seehofer.)     "
#[2] "Which internet company took over the media group Time Warner? ? AOL.  "



sum((wx/sum(wx))[which(bmax$x[(1:1075)]>0.5)])

#0.5364742

sum((wy/sum(wy))[which(bmax$x[(1:1075)]>0.5)])
#[1] 0.2014388





ans$modelsense="min"
bmin=gurobi(ans)


bmin$objval
# -0.1692433

sum((wx/sum(wx))[which(bmin$x[-(1:45)]>0.5)])
#[1] 0.5957447


 sum((wy/sum(wy))[which(bmin$x[-(1:45)]>0.5)])
#[1] 0.764988




sum((wy/sum(wy))[which(bmax$x[-(1:45)]>0.5)])




 NAMES[which(bmin$x[(1:45)]>0.5)]
#[1] "What is also termed Trisomy 21? ? Down syndrome.      "
#[2] "Which kind of bird is this? ? Blackbird.     "

### Resaampling

Lmax=list()
Lmin=list()

nrep=1000
 set.seed(1234567)
for(k in (1:nrep)){
 Wx=rep(0,1075);Wy=Wx
 i=sample((1:1075),size=658,replace=TRUE)
 Wx[i]=1
 
 j=sample((1:1075),size=417,replace=TRUE)
 Wy[j]=1
 
 V=Wx/sum(Wx)-Wy/sum(Wy)
 ans$obj=c(rep(0,45),V)
 ans$modelsense="max"
 Lmax[[k]]=gurobi(ans,list(outputflag=0))
 ans$modelsense="min"
 Lmin[[k]]=gurobi(ans,list(outputflag=0))
 
 print(Lmax[[k]]$objval)
 print(Lmin[[k]]$objval)
 }
 658
> sum(wy)
[1] 417




#Verteilung der Scores
par(cex=1.5)
pdf("score_distributions.pdf")
  S=rowSums(X)
  Sm=S[which(SPISA$gender=="male")]
  Sf=S[which(SPISA$gender=="female")]
  plot(ecdf(Sm),verticals=TRUE, main="Empirical distribution functions of observed item scores")
  lines(ecdf(Sf),verticals=TRUE,lty=2)
  legend(7.5,0.95,legend=c("male","female"),lty=c(1,2))
  
  dev.off()
  
  
  
  
  
#######
#######

#######

Tamming
  pdf("distr_D_SPISA.pdf")
plot((3:22),rowMeans(statistics[-(1:2),]),ylim=c(0,0.11),pch=16,xlab="VC",main=expression(Expectation~of~D^{"+"}~{"for"}~different~V.C.-dimensions),ylab=expression(E(D^{"+"})))
  dev.off()
  
 rowSds=function(MAT){m=dim(MAT)[1];ans=rep(0,m);for(k in (1:m)){ans[k]=sd(MAT[k,])};return(ans)}
 
   pdf("distr_D_SD_SPISA.pdf")
 
 plot((3:22),rowSds(statistics[-(1:2),]),ylim=c(0,0.02),pch=16,xlab="VC",main=expression(standard~deviation~of~D^{"+"}~~{"for"}~different~V.C.-dimensions),ylab=expression(SD(D^{"+"})))


 dev.off()
 
 
 
 
 ########
 
# DIF
 ans.male=dif.statistic.iter(X,SPISA$gender,"male",n.iter=10000)
 ans.female=dif.statistic.iter(X,SPISA$gender,"male",n.iter=10000)
 
 
 #####
 
 resampling
 
 
 Lmax=list()
Lmin=list()

nrep=10
 set.seed(1234567)
for(k in (1:nrep)){
 Wx=rep(0,1075);Wy=Wx
 i=which(runif(1075)>=417/(1075))#sample((1:1075),size=658,replace=TRUE)
 Wx[i]=1
 
 j=sample((1:1075),size=417,replace=TRUE)
 Wy[-i]=1
 
 Imgender=rep("male",1075)
 Imgender[-i]="female"
 ans.female=dif.statistic.iter(LLL$X[[k]],LLL$group[[k]],"female",n.iter=10000)
 Lmax[[k]]=ans.female
 print(Lmax)}
 
 V=Wx/sum(Wx)-Wy/sum(Wy)
 
  
