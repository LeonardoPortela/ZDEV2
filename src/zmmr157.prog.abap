*&---------------------------------------------------------------------*
*& Report  ZMMR157
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr157.

DATA s_proc_job TYPE  zmmt0122-proc_job VALUE 'X'.

IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    WRITE 'JA EXISTE UM JOB EM EXECUÇÃO'.
    EXIT.
  ENDIF.
ENDIF.

CALL FUNCTION 'ZPROC_ENTR_EST_COMODORO'
  EXPORTING
    p_proc_job = s_proc_job.
