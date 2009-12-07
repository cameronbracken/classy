function h=zack(q,n)
    %solves the diffusion equation given pumping rates q
    %  n is the number of internal nodes

      dx=1/(n+1);   %distance between nodes
      d=1;          %transmissivity
      xl=1;         %right boundary condition
      xr=1;         %left boundary condition

      a=zeros(n,n);
      b=zeros(n,1);
      
      b(1)=d*xl/(dx^2);
      b(n)=d*xr/(dx^2);

      for  i=1:n
          b(i)=b(i)+q(i)/dx;
      end 

      for  i=1:n
          a(i,i)=d*(+2./(dx^2));
          if (i~=n)
              a(i,i+1)=-d/(dx^2);
          end
          if(i~=1)
              a(i,i-1)=-d/(dx^2);
          end
      end
      
      h=a\b;

end
