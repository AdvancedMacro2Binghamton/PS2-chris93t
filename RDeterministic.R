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

k = seq( from = k_min, to = k_max, length.out =  num_k);

k_mat = matrix(rep(k,num_k), num_k, num_k, byrow = FALSE) #; % this will be useful in a bit
               
               #%%%% Set up consumption and return function
               #% 1st dim(rows): k today, 2nd dim (cols): k chosen for tomorrow

cons = k_mat ^ alpha + (1 - delta) * k_mat - t(k_mat); 
               
ret = cons ^ (1 - sigma) / (1 - sigma); #% return function
               
               #% negative consumption is not possible -> make it irrelevant by assigning
               #% it very large negative utility
               
ret[cons < 0] = -Inf; #bing number
               
               #%%%% Iteration


dis = 1; 

tol = .0001; #% tolerance for stopping 

v_guess = rep(0, num_k);
               
               
while (dis > tol){
               #% compute the utility value for all possible combinations of k and k:
value_mat = ret + beta * matrix(rep(v_guess,num_k), num_k , num_k , byrow = TRUE);
               
               #% find the optimal k' for every k:
vfn = apply(value_mat, 1, max);

vfn[vfn==-Inf]=0  #necessary adjustment. Infinity is not good to be used in R
             
#vfn = vfn;
               
               #% what is the distance between current guess and value function
dis = max(abs(vfn - v_guess));
               
               #% if distance is larger than tolerance, update current guess and
               #% continue, otherwise exit the loop
v_guess = vfn;
               
               }
               
              #g = k(pol_indx); #% policy function
               
               plot(k,vfn)
               #figure
               #plot(k,g)
               
           
           