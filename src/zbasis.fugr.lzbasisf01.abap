*----------------------------------------------------------------------*
***INCLUDE LZBASISF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  DELETE_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NUMBER_ENTRADA  text
*      -->P_NAME_ENTRADA  text
*----------------------------------------------------------------------*
FORM DELETE_JOB  USING    P_NUMBER_ENTRADA TYPE TBTCJOB-JOBCOUNT
                          P_NAME_ENTRADA   TYPE TBTCJOB-JOBNAME.

  CALL FUNCTION 'BP_JOB_DELETE'
    EXPORTING
      JOBCOUNT                 = P_NUMBER_ENTRADA
      JOBNAME                  = P_NAME_ENTRADA
    EXCEPTIONS
      CANT_DELETE_EVENT_ENTRY  = 1
      CANT_DELETE_JOB          = 2
      CANT_DELETE_JOBLOG       = 3
      CANT_DELETE_STEPS        = 4
      CANT_DELETE_TIME_ENTRY   = 5
      CANT_DERELEASE_SUCCESSOR = 6
      CANT_ENQ_PREDECESSOR     = 7
      CANT_ENQ_SUCCESSOR       = 8
      CANT_ENQ_TBTCO_ENTRY     = 9
      CANT_UPDATE_PREDECESSOR  = 10
      CANT_UPDATE_SUCCESSOR    = 11
      COMMIT_FAILED            = 12
      JOBCOUNT_MISSING         = 13
      JOBNAME_MISSING          = 14
      JOB_DOES_NOT_EXIST       = 15
      JOB_IS_ALREADY_RUNNING   = 16
      NO_DELETE_AUTHORITY      = 17
      OTHERS                   = 18.

ENDFORM.
