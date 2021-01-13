function [x_final,counter_ca_total,t] = SDM(x,acc)
%Steepest Descent Method algorithm implemented on the Rosenbrock function
%16171659
tic
%Gradient of f
[~,g]=obj(x);
%search direction
p=-g;
counter=0;

while max(abs((p.')*g)) > acc
    counter = counter+1;
   %Getting alpha1
    alpha = ls_V2(0,x, p,[]);
   %Updating x
    x = x(:) + alpha*p; 
   %Updating the values of g and p
     [~,g]=obj(x);
      p=-g;
end

counter_ca_total = counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)
end
