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
    CALL TFSF_Hupdate();

    CALL UpdateE();
    CALL TFSF_Eupdate();
    CALL CPML_UpdateEz(); 

    !==========
    ! 1-D Grid
    !==========
    CALL TFSF_Inc();
    Einc0(-2) = Einc0(-2) + Source(t);
    CALL TFSF_UpdateHinc();
    CALL TFSF_UpdateEinc();
 
  END DO
    
  CALL PrintEz();
  
  CALL TFSF_Finalize();
  CALL CPML_Finalize(); 
  CALL GRID_Finalize();
  
END PROGRAM Main
