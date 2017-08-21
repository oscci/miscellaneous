#http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html

# Based on version done for twins by Alex Wilson
#

#install.packages('DiagrammeR')
library(DiagrammeR)
library(tidyverse)
library(stringr)
#Function to detect if OS is PC or Macintosh and set path accordingly
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
os1=get_os()
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
if(os1=="windows"){
  dir<-"C:\\Users\\wilsona\\Dropbox\\project SCT analysis\\SCT_ASD_analysis\\Project_files\\Data\\"}
main.data <- read.csv(paste0(dir,"SCTData_DATA_2017-08-20_1706.csv"))

#Referral source
nhscases<-c(70, 212, 213, 214, 215, 218, 221, 222, 223, 225, 229, 231,
            233, 234, 237, 239, 240, 241, 247, 251, 252, 254, 255, 257,
            258, 264, 265, 268, 270, 273, 274, 276, 283, 285, 297, 304,
            306, 307, 310, 318, 319, 321, 322, 323, 327, 328, 329, 331,
            334, 335, 336, 337, 340, 342, 346, 348, 351, 352, 355, 357,
            358, 360, 362, 365, 367, 368, 369, 370)
nhsset<-filter(main.data,record_id %in% nhscases)
othset<-setdiff(main.data,nhsset)

y<-table(nhsset$pre_postnatal_diag)
z<-table(othset$pre_postnatal_diag)
y1=y[1]
y2=y[2]
z1=z[1]
z2=z[2]


#Count for zygosity
postnatals<-subset(main.data,pre_postnatal_diag==0)
prenatals<-subset(main.data,pre_postnatal_diag==1)
n.A<-dim(postnatals)[1]
n.B<-dim(prenatals)[1]

#subset by karyotype
xxx2<-subset(postnatals,trisomy==1)
xxy2<-subset(postnatals,trisomy==2)
xyy2<-subset(postnatals,trisomy==3)
xxx1<-subset(prenatals,trisomy==1)
xxy1<-subset(prenatals,trisomy==2)
xyy1<-subset(prenatals,trisomy==3)

n.C<-dim(xxx1)[1]
n.D<-dim(xxy1)[1]
n.E<-dim(xyy1)[1]
n.F<-dim(xxx2)[1]
n.G<-dim(xxy2)[1]
n.H<-dim(xyy2)[1]

#now check DAWBA
n.I=length(which(xxx1$dawba_diagnoses_rater_1_complete>0))
n.J=length(which(xxy1$dawba_diagnoses_rater_1_complete>0))
n.K=length(which(xyy1$dawba_diagnoses_rater_1_complete>0))
n.L=length(which(xxx2$dawba_diagnoses_rater_1_complete>0))
n.M=length(which(xxy2$dawba_diagnoses_rater_1_complete>0))
n.N=length(which(xyy2$dawba_diagnoses_rater_1_complete>0))


#now create flow chart ; TB denotes top to bottom
print(mermaid(paste0("
graph TB


A[Prenatal <br>NHS:N = ",y1,"<br>Other:N = ",y2,"]

B[Postnatal <br>NHS:N = ",z1,"<br>Other:N = ",z2,"]


A-->C[XXY <br>N = ",n.C,"]
A-->D[XXY <br>N = ",n.D,"]
A-->E[XYY <br>N = ",n.E,"]
B-->F[XXX <br>N = ",n.F,"]
B-->G[XXY <br>N = ",n.G,"]
B-->H[XYY <br>N = ",n.H,"]
C-->I[N = ",n.I,"]
D-->J[N = ",n.J,"]
E-->K[N = ",n.K,"]
F-->L[N = ",n.L,"]
G-->M[N = ",n.M,"]
H-->N[N = ",n.N,"]
")))

#At this point I use gui to export diagram to html (filename tempo), and then use:
# webshot::webshot("tempo.html", file="flowout.png", delay=2)


