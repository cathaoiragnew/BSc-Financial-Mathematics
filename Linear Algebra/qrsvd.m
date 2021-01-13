
function [u,s,v] = qrsvd(a)
[m,n] = size(a);
p = min(m,n);
if m<n
    error('call with a* as argument')
end;
asa = a' * a;
[v,s2] = eig(asa);
s = sqrt(s2);
ds = diag(s);
%sort the singular values into decreasing order
[dss,is] = sort(ds,1,'descend');
s = diag(dss);
%applying same sort to columns of v
v = v(:,is);
av= a*v;
[q,r] = qr(av,0);
d = diag(r);
dphase= diag(s/r);
s = abs(r); %replacing original s
dphase = diag(dphase);
u = q*dphase;