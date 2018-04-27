program Pii_Integral_method
implicit none
real(8) ::R,f,p
real(8)::a,b,s,integ,X,pi,y,M
integer::i,N,j
a=0. ; b=1. ; p=4.*atan(1.)
M=f(b)
if (dabs(f(a))>dabs(f(b))) M=f(a)
n=100
print*,'  N     |    Value_of_Pi    |     Erreur en %   ' 
do j=1,10
   n=j*n
   s=0.0d0   
   do i=1,n
      call random_number(r)
       X=a+R*(b-a)
       call random_number(R)
       y=M*R
       if (y<f(x)) S=s+1
   end do
             integ=((b-a)/N)*s
             Pi=4.*integ
             write(*,*)'|',n,'|',pi,'|',dabs(p-pi),'%  |' 
end do
end program
real(8) function F(X)
real(8),intent(in)::X
F=dsqrt(1-x*x)
end function F

