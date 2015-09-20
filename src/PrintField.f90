!> Prints Ez output to a .csv file
MODULE PrintField
  USE Grid, ONLY : SizeX, SizeY, Ez
  
  IMPLICIT NONE

  CONTAINS

    !> Prints output
    !! \param Ez field

    SUBROUTINE PrintEz()
      IMPLICIT NONE
      INTEGER :: i, j

      OPEN(UNIT=1, FILE="Ez.csv", FORM="FORMATTED", &
                   STATUS="REPLACE", ACTION="WRITE");
      
      DO i = 1, SizeX
        DO j = 1, SizeY
          WRITE(UNIT=1, FMT=*) i,', ',j,', ',Ez(i,j);
        END DO
      END DO
             
      CLOSE(UNIT=1);

    END SUBROUTINE

END MODULE
