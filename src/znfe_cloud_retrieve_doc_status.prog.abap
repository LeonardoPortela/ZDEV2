*&---------------------------------------------------------------------*
*& Report ZNFE_CLOUD_RETRIEVE_DOC_STATUS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZNFE_CLOUD_RETRIEVE_DOC_STATUS.


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

    SUBMIT NFE_CLOUD_RETRIEVE_DOC_STATUS AND RETURN.
    WAIT UP TO p_wait SECONDS.

  ENDDO.
