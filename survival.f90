        module survival
        
        contains

        subroutine inizialize(dx,dy,dt,vx,vy,a,b,D,Nx,Ny)
               
        implicit none
        
        integer::it,ix,iy                          !iterator on space and time
        integer::Nx,Ny       
        real*8,parameter:: xtot=100.0, eps=5E-1,ytot=25    !phisical dimension and competition
        real*8,parameter::eta=1d-3
        real*8:: dx, dt, dy, b, D, vx, vy                         !differential
        real*8,dimension(0:Nx+1,0:Ny+1)::a        !scalar field
        
        dx=xtot/(Nx+2)
        dy=dx
        
        vx=8d-1
        vy=sqrt(1d0-vx*vx)
        dt=eta*dx/vx
        b=1d-1                     
        a=1d-1                               
        D=2d-1        


         
        open(unit=66,file='random_condition.dat',status='old') 
                read(66,*)a                     !read a from file
        close(66)
        end subroutine
        
        subroutine euler(rho,dt,dx,dy,vx,vy,a,b,D,Nx,Ny)
        implicit none
        integer,intent(in)::Nx,Ny
        integer::it,ix,iy
        real*8,dimension(0:Nx+1,0:Ny+1)::rho,a
        real*8,intent(in)::dt,dx,dy,vx,vy,b,D

        
        do it=1,100                             
             
        do ix=1,Nx
           do iy=1,Ny

           rho(ix,iy)=rho(ix,iy)*(1+dt/dx*(vx+vy)- &
           & 4*dt/dx/dx*D+a(ix,iy)-b*rho(ix,iy))+ & 
           & rho(ix+1,iy)*(D*dt/dx/dx)+rho(ix-1,iy)*D+dt/dx/dx+ &
           & rho(ix,iy+1)*(-dt/dx*vy+D*dt/dx/dx)+rho(ix,iy-1)*D*dt/dx/dx
                  
                
           end do
                
        end do
        do iy=0,Ny+1
           rho(0,iy)=(rho(1,iy)+rho(1022,Ny+1-iy))/2d0  !possible periodic condition
           rho(1023,iy)=rho(0,Ny+1-iy)
        end do

        do ix=1,Nx+1
           rho(ix,0)=(rho(ix,1)+rho(Nx+1-ix,255))/2d0
           rho(ix,255)=rho(Nx+1-ix,0)
        end do
        open(77,file='output.dat')
                write(77,*)rho(:,:)
        close(77)
        print*,'step=',it
                
        end do
        end subroutine
        
        
        
        
        end module
