
library(ggplot2)
#close all
#%%%% Set up parameters
alpha = 0.35;
beta = 0.99;
delta = 0.025;
sigma = 2;


#%%%% Set up discretized state space
k_min = 0;
k_max = 45;
num_k = 1000; #% number of points in the grid for k


pi_hh=0.977; pi_hl=1-pi_hh; pi_ll=0.926; pi_lh=1-pi_ll;

pi = c(pi_hh,pi_hl,pi_ll,pi_lh);

A_h=1.1; A_l=0.678;

A = c(A_h, A_l)

k = seq( from = k_min, to = k_max, length.out =  num_k);



k_mat = matrix(rep(k,num_k), num_k, num_k, byrow = FALSE) #; % this will be useful in a bit
               
               #%%%% Set up consumption and return function
               #% 1st dim(rows): k today, 2nd dim (cols): k chosen for tomorrow

cons_h = A[1]*k_mat ^ alpha + (1 - delta) * k_mat - t(k_mat); 

cons_l = A[2]*k_mat ^ alpha + (1 - delta) * k_mat - t(k_mat); 
               
ret_h = cons_h ^ (1 - sigma) / (1 - sigma); #% return function

ret_l = cons_l ^ (1 - sigma) / (1 - sigma);         
               #% negative consumption is not possible -> make it irrelevant by assigning
               #% it very large negative utility
               
ret_h[cons_h < 0] = -Inf; #bing number
ret_l[cons_l < 0] = -Inf          
               #%%%% Iteration


dis = 1; 

tol = .01; #% tolerance for stopping 

v_guess = matrix(rep(0, 2*num_k), 2 ,num_k);
               
               
while (dis > tol){
               #% compute the utility value for all possible combinations of k and k:
value_mat_h = ret_h + beta * (pi[1]* matrix(rep(v_guess[1,],num_k), num_k , num_k , byrow = TRUE) + pi[2] *  matrix(rep(v_guess[2,],num_k), num_k , num_k , byrow = TRUE)   );
        
value_mat_l = ret_l + beta * (pi[3]* matrix(rep(v_guess[1,],num_k), num_k , num_k , byrow = TRUE) + pi[4] *  matrix(rep(v_guess[2,],num_k), num_k , num_k , byrow = TRUE)   );


       
               #% find the optimal k' for every k:
vfn_h = apply(value_mat_h, 1, max);

policy_h<-apply(value_mat_h, 1, which.max)

#vfn_h[vfn_h==-Inf]=0  #necessary adjustment. Infinity is not good to be used in R
             
#vfn = vfn;
           

vfn_l = apply(value_mat_l, 1, max);

policy_l<-apply(value_mat_l, 1, which.max)


#vfn_l[vfn_l==-Inf]=0 
    
               #% what is the distance between current guess and value function
diffrence = abs(vfn_h - v_guess[1,]);  #One is sufficient
               
diffrence[is.na(diffrence)]<-0  #We fix for the infinity bug here

dis<-max(diffrence)
               #% if distance is larger than tolerance, update current guess and
               #% continue, otherwise exit the loop
v_guess[1,] = vfn_h;
v_guess[2,] = vfn_l;
               
               }
               


             
             
               
#The Value Functions               
qplot(x = k, y = vfn_h, color = 'red')+ggtitle('High State')+
  theme(plot.title = element_text(hjust = 0.5))+guides(colour=FALSE)


qplot(x = k, y = vfn_l, color = 'blue1')+ggtitle('Low State')+
  theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = 'blue')+guides(colour=FALSE)


#The Policy Functions

qplot(x = k, y = k[policy_h], color = 'red')+ggtitle('High State')+
  theme(plot.title = element_text(hjust = 0.5))+guides(colour=FALSE)


qplot(x = k, y = k[policy_l], color = 'blue1')+ggtitle('Low State')+
  theme(plot.title = element_text(hjust = 0.5))+scale_colour_manual(values = 'blue')+guides(colour=FALSE)



#Standard Deviation of Y


#Assume first state is of high productivity and k_0 = 10

#probs<-runif(5000)

#A = rep(1, length(probs)+1)


#for (i in 2:length(probs)){

  
 # if(A[i-1]==1){
    
  #  if(probs[i-1]< pi_hh){ A[i]<- 0  }
    
    
  #}
  
  #if(A[i-1]==0){
    
   # if(probs[i-1]< pi_lh){ A[i]<-1  }
    
    
#  }
    #}

A_sers<-rbinom(5000,1,.5)

g<-function(A,kt){
  
  kt= round(kt,2)
  
  k1<-k[policy_matrix[[A]]][which(kt==round(k,2))]
  
  return(k1)
}




Y<-NULL

A_h=1.1

A_l=0.678

GDPsd<-1.8

SDY = 0

while (SDY < GDPsd){

  k_0<-41.66666667

###################################################
alpha = 0.35;
beta = 0.99;
delta = 0.025;
sigma = 2;


#%%%% Set up discretized state space
k_min = 0;
k_max = 45;
num_k = 1000; #% number of points in the grid for k


pi_hh=0.977; pi_hl=1-pi_hh; pi_ll=0.926; pi_lh=1-pi_ll;

pi = c(pi_hh,pi_hl,pi_ll,pi_lh);



A = c(A_h, A_l)

k = seq( from = k_min, to = k_max, length.out =  num_k);



k_mat = matrix(rep(k,num_k), num_k, num_k, byrow = FALSE) #; % this will be useful in a bit

#%%%% Set up consumption and return function
#% 1st dim(rows): k today, 2nd dim (cols): k chosen for tomorrow

cons_h = A[1]*k_mat ^ alpha + (1 - delta) * k_mat - t(k_mat); 

cons_l = A[2]*k_mat ^ alpha + (1 - delta) * k_mat - t(k_mat); 

ret_h = cons_h ^ (1 - sigma) / (1 - sigma); #% return function

ret_l = cons_l ^ (1 - sigma) / (1 - sigma);         
#% negative consumption is not possible -> make it irrelevant by assigning
#% it very large negative utility

ret_h[cons_h < 0] = -Inf; #bing number
ret_l[cons_l < 0] = -Inf          
#%%%% Iteration


dis = 1; 

tol = .5; #% tolerance for stopping 

v_guess = matrix(rep(0, 2*num_k), 2 ,num_k);


while (dis > tol){
  #% compute the utility value for all possible combinations of k and k:
  value_mat_h = ret_h + beta * (pi[1]* matrix(rep(v_guess[1,],num_k), num_k , num_k , byrow = TRUE) + pi[2] *  matrix(rep(v_guess[2,],num_k), num_k , num_k , byrow = TRUE)   );
  
  value_mat_l = ret_l + beta * (pi[3]* matrix(rep(v_guess[1,],num_k), num_k , num_k , byrow = TRUE) + pi[4] *  matrix(rep(v_guess[2,],num_k), num_k , num_k , byrow = TRUE)   );
  
  
  
  #% find the optimal k' for every k:
  vfn_h = apply(value_mat_h, 1, max);
  
  policy_h<-apply(value_mat_h, 1, which.max)
  
  #vfn_h[vfn_h==-Inf]=0  #necessary adjustment. Infinity is not good to be used in R
  
  #vfn = vfn;
  
  
  vfn_l = apply(value_mat_l, 1, max);
  
  policy_l<-apply(value_mat_l, 1, which.max)
  
  
  #vfn_l[vfn_l==-Inf]=0 
  
  #% what is the distance between current guess and value function
  diffrence = abs(vfn_h - v_guess[1,]);  #One is sufficient
  
  diffrence[is.na(diffrence)]<-0  #We fix for the infinity bug here
  
  dis<-max(diffrence)
  #% if distance is larger than tolerance, update current guess and
  #% continue, otherwise exit the loop
  v_guess[1,] = vfn_h;
  v_guess[2,] = vfn_l;
  
}






###################################






A_sers[A_sers==1]<-1
A_sers[A_sers==0]<-2

policy_matrix<-list(policy_h,policy_l)



for (i in 1:5000){
  
  k_0<-g(A_sers[i],k_0)
  
  Y[i]<-AA[A_sers[i]]* (k_0)^alpha
  
  
}


prev_SDY<-SDY
SDY<-sd(Y)

print(SDY)



if(prev_SDY==SDY){break}

else{
A_h<-A_h+100
}


}

print(A_h, SDY )