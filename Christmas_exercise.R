
#ΠΡΟΣΟΧΗ!
#Σβήσε όλες τις μεταβλητές περιβάλλοντος που τυχόν υπάρχουν ήδη. Πηγή: https://community.rstudio.com/t/how-to-clear-the-r-environment/14303
rm(list = ls())


#Φόρτωμα αρχείου
library(readxl)

R_LAB_DATA_NEW_V3<-read_excel("R_LAB_DATA_NEW_V3.xlsx");

########################################################## Ερώτημα Α #####################################


#Για να τυπώσω άδεια γραμμή
cat("########################################################## Ερώτημα Α #####################################","\n\n");


#Α.1)
#Να υπολογίσετε τα κυριότερα μέτρα θέσεως(μέση τιμή, διάμεσος, ελάχιστη και μέγιστη παρατήρηση, πρώτο και τρίτο τεταρτημόριο)
#, μέτρα διασποράς (διασπορά, τυπική απόκλιση και εύρος τιμών) και μέτρα μορφής(συντελεστής ασυμμετρίας) 
#της μεταβλητής  «SMS_BEFORE». 


###### Μέτρα θέσης #####

cat("               Υπολογισμός μέτρων θέσης",'\t\n\n');

cat("                    Ερώτημα Α.1",'\t\n\n');

#μέση τιμή

SMS_BEFORE_MEAN<-mean(R_LAB_DATA_NEW_V3$SMS_BEFORE);

cat(sprintf("Η μέση τιμή είναι των μηνυμάτων x_bar = %s",SMS_BEFORE_MEAN),"\n");



#Διάμεσος

SMS_BEFORE_MEDIAN<-median(R_LAB_DATA_NEW_V3$SMS_BEFORE);


cat(sprintf("Η διάμεσος για το δείγμα είναι m = %s",SMS_BEFORE_MEDIAN),"\n");

# Παρατήρηση: Χρησιμοποιώ τον συμβολισμό m από τη λέξη median για το δείγμα και M για τον πληθυσμό.
# Σύμφωνα με τις συμβάσεις που έχουμε υιοθετήσει με μικρά ελληνικά γράμματα συμβολίζουμε μέτρα για τον πληθυσμό και με αγγλικά 
# για το δείγμα.
# Στο βιβλίο στη σελίδα 72 ο διάμεσος για τον πληθυσμο συμβολίζεται με δ, αλλά και για το δείγμα με δ (σελ 83).


#Επίσης ο διάμεσος μπορεί να προκύψει από 0.5 ποσοστημόριο.



if(SMS_BEFORE_MEDIAN == quantile(R_LAB_DATA_NEW_V3$SMS_BEFORE,0.5)){
  
  
  cat("Ο διάμεσος μπορεί να προκύψει εναλλακτικά από το 0.5 ποσοστημόριο.",'\n\n');
  
}

#Ελάχιστη παρατήρηση 

SMS_BEFORE_MIN<-min(R_LAB_DATA_NEW_V3$SMS_BEFORE);


#Μέγιστη παρατήρηση 

SMS_BEFORE_MAX<-max(R_LAB_DATA_NEW_V3$SMS_BEFORE);


#πρώτο τεταρτημόριο

SMS_BEFORE_MAX_Q1<-quantile(R_LAB_DATA_NEW_V3$SMS_BEFORE,0.25);


#τρίτο τεταρτημόριο

SMS_BEFORE_MAX_Q3<-quantile(R_LAB_DATA_NEW_V3$SMS_BEFORE,0.75);


#Μέτρα διασποράς

cat("             Υπολογισμός μέτρων διασποράς",'\n\n');


#Δειγματική διασπορά s^2

SMS_BEFORE_VARIANCE<-var(R_LAB_DATA_NEW_V3$SMS_BEFORE);

cat(sprintf("Η δειγματική διασπορά s^2 = %s",SMS_BEFORE_VARIANCE),"\n");


#Δειγματική τυπική απόκλιση s

SMS_BEFORE_STD<-sd(R_LAB_DATA_NEW_V3$SMS_BEFORE);

cat(sprintf("Η δειγματική τυπική απόκλιση s = %s",SMS_BEFORE_VARIANCE),"\n");



# Η δειγματική τυπική απόκλιση θα μπορούσε να υπολογιστεί και από την 
# τετραγωνική ρίζα της δειγματικής διασποράς.

if(sqrt(SMS_BEFORE_VARIANCE) == SMS_BEFORE_STD){
  
  
  cat("θα μπορούσε να υπολογιστεί και από την τετραγωνική ρίζα της δειγματικής διασποράς s^2","\n");
  
}

#Εύρος τιμών
SMS_BEFORE_R<-SMS_BEFORE_MAX-SMS_BEFORE_MIN;


cat(sprintf("Το εύρος τιμών R = %s",SMS_BEFORE_R),"\n\n");


#Μέτρα μορφής




cat("             Υπολογισμός μέτρων μορφής",'\n\n');



#Θα χρησιμοποιήσω τον δεύτερο συντελεστή ασσυμετρίας του Pearson
#ο οποίος είναι μη παραμετρικός Πηγή https://en.wikipedia.org/wiki/Skewness

person_second_skewness_coefficient<-function(data){
  
  return(  3*(mean(data)-median(data))/sd(data)  );
  
  
}


SMS_BEFORE_SKEWNESS<-person_second_skewness_coefficient(R_LAB_DATA_NEW_V3$SMS_BEFORE);

if(SMS_BEFORE_SKEWNESS>0){
  
  
  cat( sprintf("Η κατανομή παρουσιάσει θετική ασυμμετρία γ_2 = %s",SMS_BEFORE_SKEWNESS),"\n\n");
  
}else if(SMS_BEFORE_SKEWNESS<0){
  
  cat( sprintf("Η κατανομή παρουσιάσει αρνητική ασυμμετρία γ_2 = %s",SMS_BEFORE_SKEWNESS),"\n\n");
  
}else{
  
  
  cat( sprintf("Η κατανομή είναι συμμετρική γ_2 = %s",SMS_BEFORE_SKEWNESS),"\n\n");
}

# Να κατασκευαστεί το αντίστοιχο ιστόγραμμα
# συχνοτήτων καθως και το θηκόγραμμα. Σχολιάστε.

#Αποθηκευση διαγράμματος
jpeg(file="SMS_BEFORE_boxplot.jpeg");

boxplot(R_LAB_DATA_NEW_V3$SMS_BEFORE,main="SMS_BEFORE boxplot (the red line is mean)");

abline(h=mean(R_LAB_DATA_NEW_V3$SMS_BEFORE),col="red");

dev.off();


#δημιουργία του αντίστοιχου ιστογράμματος

jpeg(file="SMS_BEFORE_histogram.jpeg");

hist(R_LAB_DATA_NEW_V3$SMS_BEFORE,main = "SMS_BEFORE histogram",xlab = "sms sended before campaign");

dev.off();

cat("Σχόλιο: Τόσο από τον συντελεστή ασυμμετρίας αλλά και από το ιστόγραμμα & το θηκόγραμμα","\n");
cat("το δείγμα δεν μοιάζει να προέρχεται από κανονικό πληθυσμό.","\n");
cat("Καλύτερα όμως θα ήταν να βρούμε έναν έλεγχο που θα επιβεβαιώσει ή όχι τον ισχυρισμό.","\n\n\t");
cat("   Εκτελώ το παρακάτω test","\n");

print(shapiro.test(R_LAB_DATA_NEW_V3$SMS_BEFORE));

NORMALITY_TEST_P_VALUE<-shapiro.test(R_LAB_DATA_NEW_V3$SMS_BEFORE)$p.value;

if(NORMALITY_TEST_P_VALUE<0.05){
  
  cat("Σε επίπεδο σημαντικότητας 0.05 απορρίπτω την υπόθεση Η0","\n");
  cat("Ότι δηλαδή ο πληθυσμός από τον οποίο προέρχεται το δείγμα","\n");
  cat("ακολουθεί την κανονική κατανομή και αποδέχομαι την εναλλακτική H1.","\n");
  cat( "Με άλλα λόγια αν ο πληθυσμός ήταν κανονικός","\n");
  cat(sprintf("Η πιθανότητα να πάρω ένα τέτοιο δείγμα είναι %s",NORMALITY_TEST_P_VALUE),"\n");
  
}else{
  
  cat("Σε επίπεδο σημαντικότητας 0.05 αποδέχομαι την υπόθεση Η0","\n");
  cat("Ότι δηλαδή ο πληθυσμός από τον οποίο προέρχεται το δείγμα","\n");
  cat("ακολουθεί την κανονική κατανομή και απορρίπτω την εναλλακτική H1.","\n");
  cat( "Με άλλα λόγια αν ο πληθυσμός είναι κανονικός","\n");
  cat(sprintf("Η πιθανότητα να πάρω ένα τέτοιο δείγμα είναι %f",NORMALITY_TEST_P_VALUE),"\n");
  
}

cat("","\n");


cat("                    Ερώτημα Α.2",'\t\n\n\t');

# Να υπολογίσετε τα κυριότερα μέτρα θέσεως (μέση τιμή, διάμεσος, ελάχιστη και
# μέγιστη παρατήρηση, πρώτο και τρίτο τεταρτημόριο), μέτρα διασποράς (διασπορά,
# τυπική απόκλιση και εύρος τιμών) και μέτρα μορφής (συντελεστής ασυμμετρίας) της
# μεταβλητής «SMS_BEFORE» για κάθε φύλο ξεχωριστά. Να κατασκευαστούν τα
# αντίστοιχα θηκογράμματα. Σχολιάστε.


#Γυναίκες 
SMS_BEFORE_WOMEN<-R_LAB_DATA_NEW_V3$SMS_BEFORE[R_LAB_DATA_NEW_V3$MF == 2]

#Άντρες 
SMS_BEFORE_MEN<-R_LAB_DATA_NEW_V3$SMS_BEFORE[R_LAB_DATA_NEW_V3$MF == 1]


# Σχόλιο: Tο φύλο, είναι μια ονομαστική (nominal) κατηγορική μεταβλητή, οπότε η τιμές
# 1 & 2 δεν υπποδηλώνουν κανενός είδους ιεραρχία.


#Μέσος όρος για τα SMS των γυναικών πριν την διαφημιστική εκστρατεία

cat("Υπολογισμός μέτρων θέσης για τα δυο φύλα.","\n\n");

SMS_BEFORE_WOMEN_MEAN<-mean(SMS_BEFORE_WOMEN);

cat(sprintf("O Μέσος όρος SMS που έχουν στείλει οι γυναίκες πριν την εκστρατεια είναι x_bar_before = %s",SMS_BEFORE_WOMEN_MEAN),"\n")

#Μέσος όρος για τα SMS των αντρών πριν την διαφημιστική εκστρατεία

SMS_BEFORE_MEN_MEAN<-mean(SMS_BEFORE_MEN);

cat(sprintf("O Μέσος όρος SMS που έχουν στείλει οι άντρες πριν την εκστρατεια είναι y_bar_before = %s",SMS_BEFORE_MEN_MEAN),"\n")

# Ελάχιστη παρατήρηση για τις γυναίκες
SMS_BEFORE_WOMEN_ΜΙΝ<-min(SMS_BEFORE_WOMEN);

cat("","\n");

cat(sprintf("Η ελάχιστη παρατήρηση για τις γυναίκες είναι χ_min = %s",SMS_BEFORE_WOMEN_ΜΙΝ),"\n")


# Ελάχιστη παρατήρηση για τους άντρες
SMS_BEFORE_MEN_ΜΙΝ<-min(SMS_BEFORE_MEN);

cat(sprintf("Η ελάχιστη παρατήρηση για τους άντρες είναι y_min = %s",SMS_BEFORE_MEN_ΜΙΝ),"\n")


# Μέγιστη παρατήρηση για τις γυναίκες
SMS_BEFORE_WOMEN_MAX<-max(SMS_BEFORE_WOMEN);

cat("","\n");

cat(sprintf("Η μέγιστη παρατήρηση για τις γυναίκες είναι χ_max = %s",SMS_BEFORE_WOMEN_MAX),"\n")


# Μέγιστη παρατήρηση για τους άντρες
SMS_BEFORE_MEN_ΜΑΧ<-max(SMS_BEFORE_MEN);

cat(sprintf("Η μέγιστη παρατήρηση για τους άντρες είναι y_max = %s",SMS_BEFORE_MEN_ΜΑΧ),"\n")





# Πρώτο τεταρτημόριο για τις γυναίκες
SMS_BEFORE_WOMEN_Q1<-quantile(SMS_BEFORE_WOMEN,probs = 0.25);

cat("","\n");

cat(sprintf("To πρώτο τεταρτημόριο για τις γυναίκες είναι Q1_x = %s",SMS_BEFORE_WOMEN_Q1),"\n")


# Πρώτο τεταρτημόριο για τους άντρες
SMS_BEFORE_MEN_Q1<-quantile(SMS_BEFORE_MEN, probs = 0.25);

cat(sprintf("To πρώτο τεταρτημόριο για τoυς άντρες είναι Q1_y = %s",SMS_BEFORE_MEN_Q1),"\n")






# τρίτο τεταρτημόριο για τις γυναίκες
SMS_BEFORE_WOMEN_Q3<-quantile(SMS_BEFORE_WOMEN,probs = 0.75);

cat("","\n");

cat(sprintf("To τρίτο τεταρτημόριο για τις γυναίκες είναι Q3_x = %s",SMS_BEFORE_WOMEN_Q3),"\n")


# Τρίτο τεταρτημόριο για τους άντρες
SMS_BEFORE_MEN_Q3<-quantile(SMS_BEFORE_MEN, probs = 0.75);

cat(sprintf("To πρώτο τεταρτημόριο για τoυς άντρες είναι Q3_y = %s",SMS_BEFORE_MEN_Q3),"\n\n\t")



cat("Υπολογισμός μέτρων διασποράς για τα δυο φύλα.","\n\n")


# διασπορά για τις γυναίκες


SMS_BEFORE_WOMEN_VARIANCE<-var(SMS_BEFORE_WOMEN);

cat(sprintf("H διασπορά για τις παρατηρήσεις των γυναικών είναι (s_x)^2 = %s",SMS_BEFORE_WOMEN_VARIANCE),"\n")

# διασπορά για τους άντρες


SMS_BEFORE_MEN_VARIANCE<-var(SMS_BEFORE_MEN);

cat(sprintf("H διασπορά για τους άντρες είναι (s_y)^2 = %s",SMS_BEFORE_MEN_VARIANCE),"\n\n")


# τυπική απόκλιση για τις γυναίκες


SMS_BEFORE_WOMEN_STD<-sd(SMS_BEFORE_WOMEN);

cat(sprintf("H τυπική απόκλιση για τις γυναίκες είναι (s_x) = %s",SMS_BEFORE_WOMEN_STD),"\n")

# τυπική απόκλιση για τους άντρες


SMS_BEFORE_MEN_STD<-sd(SMS_BEFORE_MEN);

cat(sprintf("H τυπική απόκλιση για τους άντρες είναι (s_y) = %s",SMS_BEFORE_MEN_STD),"\n\n")




# Εύρος τιμών τις γυναίκες
SMS_BEFORE_WOMEN_RANGE<-SMS_BEFORE_WOMEN_MAX-SMS_BEFORE_WOMEN_ΜΙΝ

cat(sprintf("To εύρος τιμών για τις γυναίκες είναι R_x = %s",SMS_BEFORE_WOMEN_RANGE),"\n");

# Εύρος τιμών για τους άντρες


SMS_BEFORE_MEN_RANGE<- SMS_BEFORE_MEN_ΜΑΧ - SMS_BEFORE_MEN_ΜΙΝ

cat(sprintf("To εύρος τιμών για τους άντρες είναι R_y = %s",SMS_BEFORE_MEN_RANGE),"\n\n")





