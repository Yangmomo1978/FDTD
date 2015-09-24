PROGRAM Main
  USE Grid
  USE FDTD 
  USE TFSF
  USE PrintField
  USE CPML 
  IMPLICIT NONE

  INTEGER :: t;

  CALL GRID_Initialize();
  CALL CPML_Initialize();
  CALL TFSF_Initialize(); 

  DO t = 1, TotalTime
    !==========
    ! 2-D Grid
    !==========
    CALL UpdateH();
    CALL CPML_UpdateHx();
    CALL CPML_UpdateHy();

    IF(SRC_TYPE .EQ. 0) THEN
      CALL TFSF_Hupdate();
    END IF

    CALL UpdateE();
    
    IF(SRC_TYPE .EQ. 0) THEN
      CALL TFSF_Eupdate();
    END IF

    CALL CPML_UpdateEz(); 

    !==========
    ! 1-D Grid
    !==========
    IF(SRC_TYPE .EQ. 0) THEN
    CALL TFSF_Inc();
    Einc0(-2) = Einc0(-2) + Source(t);
    CALL TFSF_UpdateHinc();
    CALL TFSF_UpdateEinc();
    ELSE
      Ez(SizeX/2,SizeY/2) = Ez(SizeX/2,SizeY/2) + Source(t)
    END IF 
  END DO
    
  CALL PrintEz();
  
  CALL TFSF_Finalize();
  CALL CPML_Finalize(); 
  CALL GRID_Finalize();
  
END PROGRAM Main
