*&---------------------------------------------------------------------*
*& Include          ZSDR0197_EVT
*&---------------------------------------------------------------------*


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

  PERFORM f_seleciona_dados.
  PERFORM f_cria_form_lote.
