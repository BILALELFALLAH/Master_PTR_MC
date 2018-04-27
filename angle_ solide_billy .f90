program angle_solide_MC 
implicit none
integer*4 :: k  ,i,n      !  j est nombre de succes
real*4  :: x,y,teta,phi,D,R,pi,r1,r2,j  
real*4 :: erreur,omega_exacte,omega_rejet
pi=4.*atan(1.) ;  R=3.      ;   D=10.  
omega_exacte=2*Pi*(1-(D/sqrt(D*D+R*R)))
n=1000
write(*,*)'La valeur exacte pour omega est :',omega_exacte  
write(*,*)'_____________________________________________________________________'
write(*,*)'N_TIRAGE        |   OMEGA_MC         |      ERREUR EN %  '
  do k=1,10
       j=1
       n=k*n              
         do i=1,n
                     
                   call random_number(r1)             
                          teta=acos(2*r1-1) 
                     call random_number(r2)
                           phi=2*pi*r2 
                                
                    x=D*tan(teta)*cos(phi)
                   y=D*tan(teta)*sin(phi)
                
                if (x*x+y*y<=R*R .AND.cos(teta)>=0 )then
                      j=j+1 
                endif 
           enddo
                 omega_rejet=(4*pi*j)/(n)
                 erreur=(abs(omega_exacte-omega_rejet))*100
write(*,*)'_______________________________________________________________________________'                      
write(*,*)'| ',n,'|',omega_rejet,'|',erreur,'%','|'
   end do
end PROGRAM ANGLE_SOLIDE_MC
         
