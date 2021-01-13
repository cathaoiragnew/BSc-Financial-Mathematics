function [x_final,counter_ca_total,t] = FR(x,acc)
%Fletcher-Reeves algorithm implemented on the Rosenbrock function
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
   [ ~ ,new_g] = obj(x);
   %Getting beta, breaking up to avoid any errors with brackets
   %beta_top_FR=(norm(new_g)).^2;
   %beta_bottom_FR = (norm(g)).^2;
   %beta_FR = beta_top_FR/beta_bottom_FR;
   beta_FR = ( (norm(new_g)).^2 / (norm(g)).^2 );
  
   %Updating p
   p = -new_g + beta_FR*p;
   g = new_g;
   counter = counter+1;
end

counter_ca_total=counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)
end
