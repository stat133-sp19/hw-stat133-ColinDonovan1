
function [root info] = modifiedbrent3032163642(func, int, params)
info=0;
N = params.maxit;
tol=params.root_tol;
x = zeros(1, N+1); % pre-allocation
x1=int.x1;
x2=int.x2;
for n = 2:N,
if x1 == x2
root = 'f';
return
end
x(1) = x1; x(2) = x2;
x(n+1) = x(n)-((x(n)-x(n-1))/(func(x(n))-func(x(n-1))))* func(x(n));
if abs(x(n+1)- x(n)) < tol,
root = x(n+1);
return
end
end
root = 'f';
if root == 'f'
    info =1;
else
    info =0;
end
end