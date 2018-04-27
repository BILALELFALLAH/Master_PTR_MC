PROGRAM    Pi_miss_hit
implicit none
REAL(8)::X, Y, Pi,rr,R,p
INTEGER*8::I,N ,j
Pi = 0.0d0
N=1E3
p=4.*atan(1.)
print*,'             N_Tir          La_valeur_de_Pi          Erreur en %   ' 
do j=1,10
      N=N*j
   DO I = 1,N
         call random_number(r)
             X =(-0.5d0)+R
         call random_number(rr)
            Y=(-0.5d0)+rr
         IF (X*X+Y*Y<=0.25d0)  Pi=Pi+1.0d0
    END DO     
            Pi=4.*Pi/dfloat(N)
            PRINT*,N,Pi,abs(p-pi)*100,'%'
enddo         
END program pi_miss_hit
            
     
       
   
          

         
