FUNCTION zmm_job_check.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CPROG) TYPE  SY-CPROG
*"  EXPORTING
*"     REFERENCE(EV_ACTIVE) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA lt_joblist TYPE TABLE OF tbtcjob.

  " INICIA DIZENDO QUE EXISTE JOB
  ev_active = 'X'.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      abap_program_name             = i_cprog
      status                        = 'R' " <-- RUNNING
    TABLES
      joblist                       = lt_joblist
    EXCEPTIONS
      no_jobs_found                 = 1
      program_specification_missing = 2
      invalid_dialog_type           = 3
      job_find_canceled             = 4
      OTHERS                        = 5.

  IF sy-subrc <> 0.
    CLEAR ev_active. " SE DER ERRO SIGNIFICA QUE NAO TEM JOB ATIVO
  ENDIF.

  DATA(lv_lines) = lines( lt_joblist ).

  IF lv_lines = 1.
    CLEAR ev_active. " se for 1 quer dizer que é o proprio
  ENDIF.

ENDFUNCTION.
