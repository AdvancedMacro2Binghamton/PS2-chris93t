
vf_iter<-function(A_h, A_l){

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


return(list(v_guess,policy_h,policy_l))

}


