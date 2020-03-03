        program progetto1D
        
        !use solver, only: solver1D       
        implicit none
        
        integer::it,ix,iy                          !iterator on space and time
        integer,parameter:: Nx=1022,Nt=600,Ny=254            !grid size
        real*8,parameter:: b=0.1, xtot=100.0, eps=5E-1,ytot=50    !phisical dimension and competition
        real*8,parameter:: vx=1.0,vy=0.0, D=0.2
        real*8:: dx, dt, dy                         !differential
        real*8,dimension(0:Nx+1,0:Ny+1)::rho,a        !scalar field
        
        dx=xtot/(Nx+2)
        dy=dx
        dt=1E-6
        
        rho=1E-5                                !initialize rho
        a=0.1                                   !initialize a
        
         
        open(unit=66,file='random_condition.dat',status='old') 
                read(66,*)a                     !read a from file
        close(66)
        !eq=a/b                                  !stable equilibrium
        !a_in=a
        
        open(44,file='output2D.dat')
        do it=1,100                             !shit
             
        do ix=1,Nx
           do iy=1,Ny

           rho(ix,iy)=rho(ix,iy)*(1+dt/dx*(vx+vy)- &
           & 4*dt/dx/dx*D+a(ix,iy)-b*rho(ix,iy))+ & 
           & rho(ix+1,iy)*(D*dt/dx/dx)+rho(ix-1,iy)*D+dt/dx/dx+ &
           & rho(ix,iy+1)*(-dt/dx*vy+D*dt/dx/dx)+rho(ix,iy-1)*D*dt/dx/dx
                   
                
           end do
                
        end do
        

                write(44,*)rho(:,:)
        print*,rho(500,100),it        
        end do
        close(44)
        
        
        
        !print*,abs(eq-a) !check: ---->0 if a>o and constant if a<0
        end program
