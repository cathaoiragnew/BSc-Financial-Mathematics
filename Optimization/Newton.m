function [x_final,counter_ca_total,t] = Newton(x,acc)
%Newtowns Method algorithm implemented on the Rosenbrock function
%16171659
tic
step = 1;
counter=0;

while abs(step) > acc
    %Get gradient of f and gradient squared of f 
    [~,g,h]=obj(x);
    %Calculating eigenvalue of h
    eigen_h = eig(h);
    %Getting min eigenvalue
    min_eigen_h = min(eigen_h);
    
    if min_eigen_h < 0
        H = -(1.5*min_eigen_h*eye(size(h)) + h);
        p = inv(H) *g;
    else
        p = -inv(h)*g;
    end
    
    [alpha, ] = ls_V2(0, x, p, []);
    x = x(:) + alpha*p;
    
    %Updating g and h values with new x
    [ ~ ,gnew,hnew]=obj(x);
    eigen_hnew = eig(hnew);
    min_eigen_hnew = min(eigen_hnew);
    
    %Calculate p+1 and g+1
    if min_eigen_hnew < 0
        H = -(1.5*min_eigen_hnew*eye(size(h)) + hnew);
        p_new = inv(H)*gnew;
    else
        p_new = -inv(hnew)*gnew;
    end
        
    step = (p_new.')*gnew;
    counter = counter+1;
end

counter_ca_total = counter; 
x_final=x;
t=toc;
seconds = num2str(t);
fprintf( '\n \t Total Time Taken in Seconds: \t %s \n' , seconds)
end
