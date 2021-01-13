%16171659
%This m file will run the 6 algorithms for n=2 on the rosenbrock problem
%recording the times taken to run each method
%inital guess will be 15,15 with accuracy level 1e-15

% n = 2
x_initial = [15,15];
acc = 1e-15;

[x_SDM,counter_SDM,time_SDM] = SDM(x_initial, acc);
[x_Newton, counter_Newton,time_Newton] = Newton(x_initial, acc);
[x_FR,counter_FR,time_FR] = FR(x_initial, acc);
[x_PR,counter_PR,time_PR] = PR(x_initial,acc);
[x_DY,counter_DY,time_DY] = DY(x_initial,acc);
[x_Hybrid,counter_Hybrid,time_Hybrid] = Hybrid(x_initial,acc);
