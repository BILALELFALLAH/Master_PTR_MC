!_____________________________________!
  program  MC_congruenciel_ab 
              implicit none             
              integer*4 :: ntir,j,i
               real*4 :: a,d
               real*4 :: som,r,m,x0,moy,x1
!________________________________________!             
     a=16807         
     m=(2.)**(31)-1
     x0=47  
    open(unit=5,file='res.txt')
    print*,'               N_Tir  |   moy de [-1;1] '    
    
!___________________________________!    
   ntir=1000     
     do j=1,20
     print*,'_________________________________________________________'            
      r=x1/(m-1)
             d=-1+2*r
     ntir=j*ntir
     som=d
        do i=1,ntir
             x1=modulo(a*x0,m)
             x0=x1
             r=x1/(m-1)
             d=-1+2*r
             som=som+d           
        end do
                     moy=som/float(ntir)
               print*,ntir,moy
               write(5,*)ntir,moy  
enddo           
!____________________________________!          
  close(5)
end program 
