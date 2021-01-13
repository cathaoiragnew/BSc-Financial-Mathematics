% Q2 reading in imag
A = imread('mypic.jpg');
% Q3showing image using image fuction
figure('Name','Original image') 
image(A);
% converting to double precision real
a = double(A);

%breaking into each colour matrix
rk = a(:,:,1); %getting red matrix
gk = a(:,:,2); %getting green matrix
bk = a(:,:,3); %getting blue matrix

%checking to see of all 3 colour matrices create original image
[m,n,p] = size(a);
zk = zeros(m,n,p);
zk(:,:,1) = rk;
zk(:,:,2) = gk;
zk(:,:,3) = bk; 
zk8bit = uint8(zk);
image(zk8bit);

%%%%%%%%%%%%%%%%%%%
%Q4(i)
% using built in svd method
[ur,sr,vr] = svd(rk);
[ug,sg,vg] = svd(gk);
[ub,sb,vb] = svd(bk);

%using usv method 
[ur_usv,sr_usv,vr_usv] = usv(rk);
[ug_usv,sg_usv,vg_usv] = usv(gk);
[ub_usv,sb_usv,vb_usv] = usv(bk);

% using qr method for svd
[ur_qr,sr_qr,vr_qr] = qrsvd(rk);
[ug_qr,sg_qr,vg_qr] = qrsvd(gk);
[ub_qr,sb_qr,vb_qr] = qrsvd(bk);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Comparing || L - USV* || built in method
comp_r_built = norm( (rk) -  ur .* sr .* vr' );
comp_b_built = norm( (bk) -  ub .* sb .* vb' );
comp_g_built = norm( (gk) -  ug .* sg .* vg' );


% Comparing || L - USV* || using usv method
comp_r_usv = norm( (rk) -  ur_usv .* sr_usv  .* vr_usv' );
comp_b_usv = norm( (bk) -  ub_usv .* sb_usv .* vb_usv' );
comp_g_usv = norm( (gk) -  ug_usv .* sg_usv .* vg_usv' );

% Comparing || L - USV* || using qrsvd method
comp_r_qr = norm( (rk) - ur_qr.*sr_qr.*vr_qr' );
comp_b_qr = norm( (bk) - ub_qr.*sb_qr.*vb_qr' );
comp_g_qr = norm( (gk) - ug_qr.*sg_qr.*vg_qr');

%evaluating ||U*U - I|| and ||V*V - I|| using built in svd
% red
norm_built_u_r = norm (ur' .*ur - eye(474));
norm_built_v_r = norm (vr' .*vr - eye(474));
% green
norm_built_u_g = norm (ug' .*ug - eye(474));
norm_built_v_g = norm (vg' .*vg - eye(474));
%blue
norm_built_u_b = norm (ub' .*ub - eye(474));
norm_built_v_b = norm (vb' .*vb - eye(474));


%evaluating norm ||U*U - I|| and ||V*V - I|| using usv
% red
norm_usv_u_r = norm (ur_usv' .* ur_usv - eye(474));   
norm_usv_v_r = norm (vr_usv' .* vr_usv - eye(474));
%blue
norm_usv_u_b = norm (ub_usv' .* ub_usv - eye(474));
norm_usv_v_b = norm (vb_usv' .* vb_usv - eye(474));
%green
norm_usv_u_g = norm (ug_usv' .* ug_usv - eye(474));
norm_usv_v_g = norm (vg_usv' .* vg_usv - eye(474));


%evaluating norm ||U*U - I|| and ||V*V - I|| using qrsvd
% red
norm_sq_u_r = norm (ur_qr' .* ur_qr - eye(474));
norm_sq_v_r = norm (vr_qr' .* vr_qr - eye(474));
% green
norm_sq_u_g = norm (ug_qr' .* ug_qr - eye(474));
norm_sq_v_g = norm (vg_qr' .* vg_qr - eye(474));
%blue
norm_sq_u_b = norm (ub_qr' .* ub_qr - eye(474));
norm_sq_v_b = norm (vb_qr' .* vb_qr - eye(474));


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %Q5 
 
%using usv
%red
figure('Name', 'semi log plot of red')
semilogy(diag(sr_usv));
title('Semi-log Y plot of SVDs red layer')
% blue
figure('Name', 'semi log plot of blue')
semilogy(diag(sb_usv));
title('Semi-log Y plot of SVDs blue layer')
%green
figure('Name', 'semi log plot of green')
semilogy(diag(sg_usv));
title('Semi-log Y plot of SVDs green layer')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%red layer decreases fairly fast so low rank approx will capture most detail
%blue layer decreases fairly fast too 
%same for green

%rank n approx
for  n = 10:35:150
    
vt_r = vr_usv';
vt_b = vb_usv';
vt_g = vg_usv';

%Calculating individual colours of A
outputimageR = ur_usv(:,1:n) * sr_usv(1:n,1:n)  * vt_r(1:n,:);
outputimageB = ub_usv(:,1:n) * sb_usv(1:n,1:n)  * vt_b(1:n,:);
outputimageG = ug_usv(:,1:n) * sg_usv(1:n,1:n)  * vt_g(1:n,:);

%Added all back together to construck rank approx image
constructedimage = cat(3,outputimageR,outputimageG,outputimageB);

%Showing image
figure('Name', 'low rank approx')
image(constructedimage/255)
end