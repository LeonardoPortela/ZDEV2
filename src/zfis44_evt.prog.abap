*&---------------------------------------------------------------------*
*& Include          ZFIS44_EVT
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF p_conf IS NOT INITIAL.
    PERFORM f_filial_conf.
  ELSEIF p_desc IS NOT INITIAL.
    PERFORM f_filial_descon.
  ELSEIF p_mail IS NOT INITIAL.
    PERFORM f_envia_email.
  ELSEIF p_mail_2 IS NOT INITIAL.
    PERFORM f_envia_email_fiscal.
  ENDIF.
