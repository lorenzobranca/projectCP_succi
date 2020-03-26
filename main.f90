        program main

        use survival
        implicit none
        real*8::dx,dy,dt,vx,vy,b,D
        integer,parameter::Nx=1022,Ny=254
        real*8,dimension(0:Nx+1,0:Ny+1)::a,rho
               
        call inizialize(dx,dy,dt,vx,vy,a,b,D,Nx,Ny)
           
        print*,dx,dy,dt,vx,vy,b,D
        
        call euler(rho,dt,dx,dy,vx,vy,a,b,D,Nx,Ny)
                
        end program
