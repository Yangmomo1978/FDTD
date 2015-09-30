!> Prints Ez output to a .csv file
MODULE PrintField
  USE Grid, ONLY : SizeX, SizeY, Ez
  
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp = KIND(1.d0);
  REAL(dp) :: Min;
  REAL(dp) :: Max;
  CONTAINS

    !> Prints output
    !! \param Ez field

    SUBROUTINE PrintEz(file)
      IMPLICIT NONE
      INTEGER :: i, j
      CHARACTER (LEN=*), PARAMETER :: dir="output/";
      CHARACTER(LEN=*), PARAMETER :: ext=".csv";
      CHARACTER(LEN=3), INTENT(IN) :: file;
      
      CHARACTER(LEN=14) :: filename;
      filename = dir//file//ext;

      OPEN(UNIT=1, FILE=filename, FORM="FORMATTED", &
                   STATUS="REPLACE", ACTION="WRITE");
      
      DO i = 1, SizeX
        DO j = 1, SizeY
          WRITE(UNIT=1, FMT="(F12.6)",ADVANCE="NO") Ez(i,j);
          WRITE(UNIT=1, FMT="(A2)",ADVANCE="NO") ", ";
        END DO
          WRITE(UNIT=1,FMT=*) 
      END DO
             
      CLOSE(UNIT=1);

    END SUBROUTINE

END MODULE
