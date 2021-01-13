function [x_final,counter_ca_total,t] = Hybrid(x,acc)
%Hybrid Method implemented for the Rosenbrock function
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
   
   %Calculating beta DY
   %beta_Top_DY= (norm(new_g).^2);
   %beta_Bottom_DY = ((p.')*(new_g-g)); 
   %beta_DY = beta_Top_DY/beta_Bottom_DY;
   
   beta_DY = ( (norm(new_g).^2) / ((p.')*(new_g-g)) );
   
   %Calculating hestenes and stiefel beta, beta HS
   %beta_Top_HS = ((new_g.')*(new_g-g));
   %beta_Bottom_HS = ((p.')*(new_g-g));
   %beta_HS = beta_Top_HS/beta_Bottom_HS;
   
   beta_HS = ( ((new_g.')*(new_g-g))/((p.')*(new_g-g)));
   
   %Getting hybrid beta, beta HY
   beta_HY = max(0,min(beta_DY,beta_HS));
   %Update p
   p = -new_g + beta_HY*p;
   g = new_g;
   counter = counter+1;
end

counter_ca_total=counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)
end
