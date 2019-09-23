function y=chiusura(x)

N=size(x,1);

I = eye(N);
y=x;

for k=1:N
  y = x *(I+y);
  y = y > 0;
end