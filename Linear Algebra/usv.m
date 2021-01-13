function[u,s,v] = usv(a)
[m,n] = size(a);
if m<n
    error('call with a* as argument')
end
as = a' ;
asa = as*a;
[v,s2] = eig(asa);
s = sqrt(s2);
ds = diag(s);
% sort the singular values into descreasing order
[dss,is] = sort(ds,1,'descend');
s = diag(dss);
% and apply the same sort to the column of v
v = v(:,is);
usigma = a*v;
u =usigma/s;