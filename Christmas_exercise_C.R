
#ΠΡΟΣΟΧΗ!
#Σβήνει όλες τις μεταβλητές περιβάλλοντος που τυχόν υπάρχουν ήδη. Πηγή: https://community.rstudio.com/t/how-to-clear-the-r-environment/14303
rm(list = ls())

# 
# Β.1) Να διατυπώσετε τον κατάλληλο έλεγχο υποθέσεων για τον ισχυρισμό ότι το
# μέσο μηνιαίο πλήθος απεσταλμένων «SMS_BEFORE» στον αντίστοιχο πληθυσμό
# διαφέρει στατιστικά σημαντικά από τις 100 μονάδες. Σχολιάστε.


#Φόρτωμα αρχείου
library(readxl)



R_LAB_DATA_NEW_V3<-read_excel("R_LAB_DATA_NEW_V3.xlsx");

# 
# 
# Να μελετηθεί η αποτελεσματικότητα της διαφημιστικής εκστρατείας που
# περιγράφεται στο (Β.4) ερώτημα εφαρμόζοντας κατάλληλα το μοντέλο της απλής
# γραμμικής παλινδρόμησης (κατασκευή διαγράμματος διασποράς, εύρεση δειγματικής
# ευθείας ελαχίστων τετραγώνων,  διεξαγωγή ελέγχου υποθέσεως γραμμικής
# συσχέτισης, Χ=SMS_BEFORE, Y=SMS_AFTER)



#Αποθηκευση διαγράμματος
jpeg(file="SMS_BEFORE_AFTER_SCATTER_PLOT.jpeg");

scatter.smooth(R_LAB_DATA_NEW_V3$SMS_BEFORE,R_LAB_DATA_NEW_V3$SMS_AFTER,xlab = "SMS BEFORE campaign",
               ylab = "SMS AFTER campaign",main="SMS AFTER - SMS BEFORE" )

dev.off()



print(lm(R_LAB_DATA_NEW_V3$SMS_AFTER ~ R_LAB_DATA_NEW_V3$SMS_BEFORE))



summary(lm(R_LAB_DATA_NEW_V3$SMS_AFTER ~ R_LAB_DATA_NEW_V3$SMS_BEFORE))






