*&---------------------------------------------------------------------*
*& Report Z_1BNFE_CF_GAP_STATUS_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_1BNFE_CF_GAP_STATUS_UPDATE.


PARAMETERS: p_times TYPE i DEFAULT 1,
            p_wait  TYPE i DEFAULT 10.

START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  DO p_times TIMES.

    SUBMIT J_1BNFE_CF_GAP_STATUS_UPDATE AND RETURN.
    WAIT UP TO p_wait SECONDS.

  ENDDO.
