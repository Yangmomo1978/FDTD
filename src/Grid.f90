!>  FDTD Coefficients
MODULE Grid
  USE FDTD_Constants
  IMPLICIT NONE
  
  !>  A collection of coefficients used in the FDTD update equations
  !!
  !!  \details Ceze = Ez: coefficient of previous Ez \n
  !!  Cezh = Ez: coefficient of curl(H) \n
  !!  Chxh = Hx: coefficient of previous Hx \n
  !!  Chxe = Hx: coefficient of curl(E) \n
  !!  Chyh = Hy: coefficient of previous Hy \n
  !!  Chye = Hy: coefficient of curl(E) \n
  

  !==================
  ! PRIVATE variables
  !==================
  INTEGER, PARAMETER, PRIVATE :: dp = KIND(1.d0);
  
  !======================
  ! PUBLIC variables
  !======================
  
  !********************
  ! Grid Constants
  !********************
  REAL(dp)  :: freq;
  INTEGER  :: Nlambda;       !Points per wavelength
  
  REAL(dp)  :: dx; !Spatial step
  REAL(dp)  :: dy;
  REAL(dp)  :: dt; !time step
  
  INTEGER :: TotalTime;

  !********************
  ! Grid Sizes
  !********************
  INTEGER  :: TFSF_Size;
  INTEGER  :: PML_Size;
  
  INTEGER  :: SizeX;
  INTEGER  :: SizeY;

  INTEGER  :: TFSF_x0;
  INTEGER  :: TFSF_x1;

  INTEGER  :: TFSF_y0;
  INTEGER  :: TFSF_y1;
  
  !********************
  ! TF/SF
  !********************
  REAL(dp)   :: phi;
  REAL(dp)   :: cosphi;
  REAL(dp)   :: sinphi;
  
  !*******************
  !  Arrays
  !*******************

  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Ceze;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Cezh;
    
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Chxh;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Chxe;

  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Chyh;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Chye;
  
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Ez;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Hy;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Hx;

  REAL(dp), DIMENSION(:), ALLOCATABLE :: den_hy;
  REAL(dp), DIMENSION(:), ALLOCATABLE :: den_hx; 
  REAL(dp), DIMENSION(:), ALLOCATABLE :: den_ex;
  REAL(dp), DIMENSION(:), ALLOCATABLE :: den_ey; 
  
  CONTAINS
    SUBROUTINE GRID_INITIALIZE()
      IMPLICIT NONE
      
      !******************
      ! Grid Parameters
      !******************
      freq    = 1 * (10**9.d0); !1 GHz
      Nlambda = 50;
      dx      = (cc / freq) / Nlambda;
      dy      = dx;
      dt      = dx * 0.99d0 / (DSQRT(2.d0) * cc);

      TotalTime = 2000;
      
      !******************
      ! CPML, TF/SF Size
      !******************
      TFSF_Size = 5 * Nlambda;
      PML_Size  = 11;

      SizeX = Nlambda + TFSF_Size + 2*PML_Size;
      SizeY = SizeX;
    
      TFSF_x0 = Nlambda/2;
      TFSF_x1 = TFSF_x0 + TFSF_Size;

      TFSF_y0 = TFSF_x0;
      TFSF_y1 = TFSF_x1;

      phi = pi / 4;
      cosphi = DCOS(phi);
      sinphi = DSIN(phi);

      !*****************
      ! Array Allocation
      !*****************
      ALLOCATE(Ceze(SizeX,SizeY));
      ALLOCATE(Cezh(SizeX,SizeY)); 
      Ceze(:,:) = 1.d0;  
      Cezh(:,:) = dt / (eps0 * dx);
    
      ALLOCATE(Chxh(SizeX,SizeY-1));
      ALLOCATE(Chxe(SizeX,SizeY-1)); 
      Chxh(:,:) = 1.d0;
      Chxe(:,:) = dt / (mu0 * dx);
      
      ALLOCATE(Chyh(SizeX-1,SizeY));
      ALLOCATE(Chye(SizeX-1,SizeY));
      Chyh(:,:) = 1.d0;
      Chye(:,:) = dt / (mu0 * dx);
      
      ALLOCATE(Ez(SizeX,SizeY));
      ALLOCATE(Hx(SizeX,SizeY-1));
      ALLOCATE(Hy(SizeX-1,SizeY));
      Ez(:,:) = 0.d0;
      Hx(:,:) = 0.d0;
      Hy(:,:) = 0.d0;

      ALLOCATE(den_hy(SizeY-1));
      ALLOCATE(den_hx(SizeX-1));
      ALLOCATE(den_ex(SizeX-1));
      ALLOCATE(den_ey(SizeY-1));
      den_hy(:) = 1.d0; 
      den_hx(:) = 1.d0; 
      den_ex(:) = 1.d0; 
      den_ey(:) = 1.d0; 
  
    END SUBROUTINE
    
    SUBROUTINE GRID_FINALIZE()
      IMPLICIT NONE
      
      DEALLOCATE(Ceze);
      DEALLOCATE(Cezh);
      
      DEALLOCATE(Chxh);
      DEALLOCATE(Chxe);
      
      DEALLOCATE(Chyh);
      DEALLOCATE(Chye);

      DEALLOCATE(Ez);
      DEALLOCATE(Hx);
      DEALLOCATE(Hy);
      
      DEALLOCATE(den_hy);
      DEALLOCATE(den_hx);
      DEALLOCATE(den_ex);
      DEALLOCATE(den_ey);
      
    END SUBROUTINE

END MODULE Grid

























