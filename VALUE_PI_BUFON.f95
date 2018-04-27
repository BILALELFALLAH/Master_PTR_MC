program Value_Pi_Buffon
implicit none
integer :: Ntirage
real*8:: y,theta,a,Pi0,Pi,D,errr
integer::i,j,Nsucces,Nechec
data D,a,Ntirage/6.,2.,100/
Pi0=4.*atan(1.)
write(*,*)'La valeur exacte du nombre PI est : ' ,Pi0
write(*,*)'___________________________________________________-___'
write(*,*) '    Tirage        ','     Valeur de Pi', '        Erreur en %'
write(*,*)'__________________________________________________________'
do i=1,20
        Nsucces=0; Nechec=0
        do j=1,Ntirage
           y=(D/2)*rand()
           theta=Pi0*rand()
           if(y<=(a/2)*sin(theta)) then ; Nsucces=Nsucces+1
          else  ; Nechec=Nechec
          end if
      enddo
Pi=(2*a*Ntirage)/(D*Nsucces)
errr=(abs(Pi0-Pi))*100
write(*,2)Ntirage,Pi,errr,'%'
Ntirage=Ntirage*2
enddo
          2 format(I10,F25.16,F25.16,A2)
end

