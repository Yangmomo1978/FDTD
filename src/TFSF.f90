
!> Total Field/Scattered Field Implementation
MODULE TFSF
  USE FDTD_Constants, ONLY :  eps0, mu0                          
  USE GRID

  IMPLICIT NONE
  
  !> Incidenet Fields
  !!
  !! \details The Einc0, Einc1, and Hinc0 fields
  !! are the fields on the incident grid. \n \n
  !! The EzInc, HxInc, and HyInc fields are used
  !! to update the actual FDTD fields.

  !==================
  ! PRIVATE variables
  !==================
  INTEGER, PARAMETER, PRIVATE :: dp = KIND(1.d0);

  !=====================
  ! PUBLIC variables
  !=====================
  REAL(dp), DIMENSION(:), ALLOCATABLE :: Einc0;
  REAL(dp), DIMENSION(:), ALLOCATABLE :: Hinc0;

  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: EzInc;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: HxInc;
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: HyInc;
  CONTAINS  

    !> 1D TF/SF Update Loops
    !! \param Einc0
    !! \param Hinc0
    
    SUBROUTINE TFSF_Initialize()
      IMPLICIT NONE

      ALLOCATE(Einc0(-10:2*SizeX));
      ALLOCATE(Hinc0(-10:2*SizeX));

      ALLOCATE(EzInc(-10:SizeX,-10:SizeY));
      ALLOCATE(HxInc(-10:SizeX,-10:SizeY));
      ALLOCATE(HyInc(-10:SizeX,-10:SizeY));

    END SUBROUTINE


    SUBROUTINE TFSF_Inc()
      IMPLICIT NONE
      REAL(dp) :: Ch, Ce;
      INTEGER :: k;

      Ch = dt / (eps0 * dx);
      Ce = dt / (mu0 * dx); 

      DO k = -10, 2*SizeX-1
        Hinc0(k) = Hinc0(k) - Ce * (Einc0(k+1) - Einc0(k));
      END DO 

      DO k = -9, 2*SizeX
        Einc0(k) = Einc0(k) - Ch * (Hinc0(k) - Hinc0(k - 1));
      END DO

    END SUBROUTINE
    
    !>  Get HxInc and HyInc from Hinc0
    !! \param HxInc
    !! \param HyInc
    !! \param Hinc0

    SUBROUTINE TFSF_UpdateHinc()
      IMPLICIT NONE
      
      !**************
      ! Hx Incidident
      !**************
      INTEGER :: j;
      INTEGER :: i;
      INTEGER :: m0 = 0;
      REAL(dp) :: d;
      REAL(dp) :: d1;

      j = TFSF_y0;
      DO i = TFSF_x0, TFSF_x1
        d = cosphi * (i - TFSF_x0) + sinphi * (j - 0.5d0 - TFSF_y0) + 0.5d0;
        d1 = d - FLOOR(d);    
        HxInc(i, j-1) = (d1 * Hinc0(m0 +  FLOOR(d) ) &
          + (1.d0 - d1) * Hinc0(m0 + FLOOR(d) - 1)) * sinphi; 
      END DO

      j = TFSF_y1;
      DO i = TFSF_x0, TFSF_x1
        d = cosphi * (i - TFSF_x0) + sinphi * (j + 0.5d0 - TFSF_y0) + 0.5d0;
        d1 = d - FLOOR(d);
        HxInc(i, j) = (d1 * Hinc0(m0 +  FLOOR(d) ) &
          + (1.d0 - d1) * Hinc0(m0 + FLOOR(d) - 1)) * sinphi;
      END DO

      !***************
      ! Hy Incident
      !***************
      i = TFSF_x0;
      DO j = TFSF_y0, TFSF_y1
        d = cosphi * (i - 0.5d0 - TFSF_x0) + sinphi * (j - TFSF_y0) + 0.5d0;
        d1 = d - FLOOR(d);
        HyInc(i-1, j) = ((d1 * Hinc0(m0 + FLOOR(d))) &
          + (1.d0 - d1) * Hinc0(m0 + FLOOR(d) - 1)) * (-cosphi);
      END DO
      
      i = TFSF_x1;
      DO j = TFSF_y0, TFSF_y1
        d = cosphi * (i + 0.5d0 - TFSF_x0) + sinphi * (j - TFSF_y0) + 0.5d0;
        d1 = d - FLOOR(d);
        HyInc(i, j) = ((d1 * Hinc0(m0 + FLOOR(d))) &
          + (1.d0 - d1)*Hinc0(m0 + FLOOR(d) - 1)) * (-cosphi);
      END DO

    END SUBROUTINE
    
    !> Update EzInc from Einc0
    !! \param EzInc
    !! \param Einc0
    SUBROUTINE TFSF_UpdateEinc()
      IMPLICIT NONE

      INTEGER :: i;
      INTEGER :: j; 
      INTEGER :: m0 = 0;
      REAL(dp) :: d;
      REAL(dp) :: d1;

      DO j = TFSF_y0, TFSF_y1
        DO i = TFSF_x0, TFSF_x1
          d = cosphi * (i - TFSF_x0) + sinphi * (j - TFSF_y0);
          d1 = d - FLOOR(d);
          EzInc(i,j) = (1.d0 - d1) * Einc0(m0 + FLOOR(d)) &
            + d1 * Einc0(m0 + FLOOR(d) + 1);
        END DO
      END DO

    END SUBROUTINE
    
    SUBROUTINE TFSF_Finalize()
      DEALLOCATE(Einc0);
      DEALLOCATE(Hinc0);

      DEALLOCATE(EzInc);
      DEALLOCATE(HxInc);
      DEALLOCATE(HyInc);

    END SUBROUTINE

END MODULE TFSF















