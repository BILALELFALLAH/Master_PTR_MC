      program Congruencial_Generator_Moy
      IMPLICIT NONE
      REAL*8 ::x,m, moy,som,r,a
      integer*8 :: j,i,N_tir
      a=16807.
     
      m=((2.)**31)-1
      
      print*,'__________________________________________'
      print*,'            Nbr_Tir         Valeur_Moyenne   '     
      som=0  
      N_tir=1000
        do j=1,20
      print*,'___________________________________________'
                 x=27.
                r=x/(m-1)
                N_tir=N_tir*j
                som=r
             do i=1,N_tir
                x=mod(a*x,m)   
                r=x/(m-1)
               
               som=som+r
                
            enddo  
               moy=som/dfloat(N_tir)
              
                print*,N_tir,moy
        enddo 
  
      end program Congruencial_Generator_Moy

                
