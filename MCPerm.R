MCPerm <- function (data,alpha) {
  sig_MCPerm=0  # Initialize a variable to store the number of significant joint correlations.
  for (s_ in 1:s){
    n.sig=0
    ord=order(abs(cor(data[,m],data[,1:(m-1)])),decreasing=TRUE)
    T0=sort(abs(cor(data[,m],data[,1:(m-1)])),decreasing=TRUE)   # T0 represents the observed correlation values.
    for (sim in 1:N_perm){
      data_perm=data
      data_perm[,m]=sample(data_perm[,m]) # Permute (shuffle) the values in the last column (dependent variable, Y).
      T=sort(abs(cor(data_perm[,m],data_perm[,1:(m-1)])),decreasing=TRUE) 
      if(sum (T[1:s_]<=T0[1:s_])==s_) n.sig=n.sig+1
    }
    p.v=(n.sig+1)/(N_perm+1)  # Calculate the p-value for the current 's_'.
    if (p.v>=(1-alpha)) {sig_MCPerm=s_}  else break
  }
  names_MCPerm = ""
  if (sig_MCPerm>0) names_MCPerm=names(data)[ord][1:sig_MCPerm]
  list("Number of joint significant: "= sig_MCPerm, "Significant joint correlations (Y, X_i): "= names_MCPerm )
}

# ---
## Example
data=read.csv2('data.csv') # variables: X1, X2, ..., X_(m-1), Y
m=ncol(data)
N_perm=9999 # Set the number of permutations for the Monte Carlo permutation test.
s=m-1   
alpha=0.05

MCPerm(data,alpha) # Run the MCPerm function with the loaded data and specified alpha.

# ---
## Holm corrected
library(lsr)

# Number of significant findings (Holm corrected)
sum(correlate(data[,m],data[,1:(m-1)],test=TRUE,p.adjust.method = "holm")$p.value<alpha)
