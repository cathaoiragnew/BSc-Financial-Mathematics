function [x_final,counter_ca_total,t] = PR(x,acc)
%Polak-Ribiere algorithm implemented on the Rosenbrock function
%16171659
tic
[~,g]=obj(x);
p=-g;
counter=0;

while max(abs(g)) > acc
   %Getting alpha
   [alpha, ] = ls_V2(0, x, p, []);
   %Updating x
   x = x + alpha*p;
   %Updating g
   [~,new_g] = obj(x);
   %Getting beta, breaking into parts so not to have an error with the brackets
   % Beta_top_PR=(new_g.')*(new_g-g);
   % Beta_bottom_PR = ((norm(g).^2));
   % Beta_PR = Beta_top_PR/Beta_bottom_PR;
  Beta_PR = ((new_g.')*(new_g-g) / ((norm(g).^2)) ); 
   %Updating p
   p = -new_g + Beta_PR*p;
   g = new_g;
   counter = counter+1;
end

counter_ca_total=counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)

