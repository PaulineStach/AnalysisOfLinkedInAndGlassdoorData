setwd("C:/Users/pauli/Documents/NCI/Programming for Big Data/Project/Dataset1")


#plotting the graphs________________________________________________________________________________________
#barplot with legend
barplot(Switch$Percentage, main="Direction of Job Switch", ylab = "Percentage", 
                 col=c("aquamarine", "chocolate1","darkmagenta", "blue3", "gold", "green1", "olivedrab3", "lightpink1",
                       "ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4",
                       "ivory4","ivory4","ivory4"), cex.lab = 1.5, cex.main = 1.4, beside=TRUE)
legend("topright", Switch$Companies[1:9], col=c("aquamarine", "chocolate1","darkmagenta", "blue3", "gold", "green1", "olivedrab3", "lightpink1",
       "ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4","ivory4", "ivory4","ivory4",
       "ivory4"),lwd=10)


#installing library for treemap
install.packages("treemap")
library(treemap)
#treemap
Treemap <- treemap(Switch,
        
        # data
        index="Companies",
        vSize="Percentage",
        type="index",
        
        # Main
        title="Direction of Job Switch",
        palette="Dark2",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=0.5,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=T                        
        
)
               