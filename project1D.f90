        program progetto1D
        
        implicit none
        
        integer::it,ix                          !iterator on space and time
        integer,parameter:: Nx=1022            !grid size
        real*8,parameter:: b=0.5, xtot=100.0, eps=5E-1    !phisical dimension and competition
        real*8,parameter:: v=0.01, D=0.005
        real*8:: dx, dt                         !differential
        real*8,dimension(Nx+2)::rho,a,eq,check,a_in        !scalar field
        
        dx=xtot/(Nx+2)
        dt=0.04
        
        rho=1E-3                                !initialize rho
        a=0.0                                   !initialize a
        
        
        open(unit=66,file='random_condition.dat',status='old') 
                read(66,*)a                     !read a from file
        close(66)
        eq=a/b                                  !stable equilibrium
        a_in=a
        
        open(77,file='output1D.dat')
        do it=1,300                             !shit
             
                do ix=1,Nx
                   
                   rho(ix)=rho(ix)*(1+dt*(v/dx-2*D/(dx*dx)))- &
                   & dt*b*rho(ix)*rho(ix)+dt*a(ix)*rho(ix)+ &
                   & dt*(-v/dx+D/(dx*dx))*(rho(ix+1))+ &
                   & dt*D/(dx*dx)*rho(ix-1) !solver
                   !rho(ix)=rho(ix)+dt*(a(ix)*rho(ix)-b*rho(ix)**2.)
                
                end do
                write(77,*)rho(:)
        print*,rho(20)        
        end do
        close(77)
        
        
        
        !print*,abs(eq-a) !check: ---->0 if a>o and constant if a<0
        end program
