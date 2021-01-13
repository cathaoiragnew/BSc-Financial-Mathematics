function [x_final,counter_ca_total,t] = DY(x,acc)
%Dai-Yuan algorithm implemented on the Rosenbrock function
%16171659S
tic
[~,g]=obj(x);
p=-g;
counter=0;

while max(abs(g)) > acc
   
   %Getting alpha
   [alpha, ] = ls_V2(0, x, p, []);
   %Update x
   x = x + alpha*p;
   %Update g
   [~,new_g] = obj(x);
   %Getting beta, breaking into parts so not to have an error with the brackets
   %beta_top_DY= norm(new_g).^2 ;
   %beta_bottom_DY=(p.')*(new_g-g); 
   %beta_DY = beta_top_DY / beta_bottom_DY;
   beta_DY = ( (norm(new_g).^2) / ((p.')*(new_g-g)) );
   %Update p
   p = -new_g + beta_DY*p;
   g = new_g;
   
   counter = counter+1;
end

counter_ca_total=counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)
end
