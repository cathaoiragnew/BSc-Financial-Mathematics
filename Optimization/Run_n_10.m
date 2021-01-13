%16171659
%This m file will run the 6 algorithms for n=10 on the rosenbrock problem.

%For loop creates a matrix so that each algorithm is assigned to a specific row
%Row 1: Steepest Decent Method
%Row 2: Newtons Method 
%Row 3: Fletcher-Reeves Method
%Row 4: Polak-Ribiere Method 
%Row 5: Dai-Yuan Method
%Row 6: Hybrid Method
%Each column of the matrix respresents the result of that run of the loop
%Col 1: first run
%Col 2: second run 
%...
%Col 6: sixth run

%Using two matrices: 1)to store run times
%                    2)to store number of iteration

%Setting accuracy to 1e-5 
acc = 1e-5;

%Creating mentioned above Matrices
run_times = zeros(6, 6);
no_iterations = zeros(6,6);

% These are the intial guesses 
x_initial = [ -4 71 46 1.5  26.7 20  ] ;   

for i = 1 :6
    %Outputting the run order, from 1 to 6
    fprintf( '\n <strong> Run %i </strong> \n' , i)
    
    %n=10, creating 10 copies of the initial guess
    x_initial_10= x_initial(i)*ones(1,10);
    
    %Steepest Decent Method
    [x_SDM_10,counter_SDM_10, time_SDM_10] = SDM(x_initial_10, acc);
    run_times(1, i ) = time_SDM_10;
    no_iterations(1, i) = counter_SDM_10;
    
    %Newton's Method 
    [x_Newton_10,counter_Newton_10, time_Newton_10] = Newton(x_initial_10, acc);
    run_times(2, i) = time_Newton_10;
    no_iterations(2,i) = counter_Newton_10;
    
    %Fletcher-Reeves Method 
    [x_FletR_10,counter_FletR_10, time_FletR_10] = FR(x_initial_10, acc);
    run_times(3, i) = time_FletR_10;
    no_iterations(3, i) = counter_FletR_10;
    
    %Polak-Ribiere Method 
    [x_PR_10,counter_PR_10, time_PR_10] = PR(x_initial_10, acc);
    run_times(4, i) = time_PR_10;
    no_iterations(4,i) = counter_PR_10;
    
    %Dai-Yuan Method
    [x_DY_10,counter_DY_10, time_DY_10] = DY(x_initial_10, acc);
    run_times(5, i) = time_DY_10;
    no_iterations(5, i) = counter_DY_10;
    
    %Hybrid Method
    [x_Hyb_10,counter_Hyb_10, time_Hyb_10] = Hybrid(x_initial_10, acc);
    run_times(6, i) = time_Hyb_10;
    no_iterations(6, i) = counter_Hyb_10;
end

%Perf.m to plot Run Times
figure( 'Name' , 'Performance Profile: Time' )
perf(run_times,'Time')
legend( 'SDM' , 'Newton' , 'FR', 'PR' , 'DY' , 'Hybrid')
title( 'Performance Profile: Time' )
xlabel( 'Tau' );
ylabel( 'Performance Profile' );

%Perf.m to plot the number of iterations
figure( 'Name' , 'Performance Profile:Iterations' )
perf(no_iterations,'Iterations' )
legend( 'SDM' , 'Newton' , 'FR', 'PR' , 'DY' , 'Hybrid')
title( 'Performance Profile: Number of Iterations' )
xlabel( 'Tau' );
ylabel( 'Performance Profile' );
