*&---------------------------------------------------------------------*
*& Report  Z_SD_ZOB_IMOBILIZADO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zob_imobilizado.


DATA: lv_dias             TYPE tvarvc-low,
      lv_erdat_ini        TYPE erdat,
      lv_erdat_fim        TYPE erdat,
      lv_aedat_ini        TYPE erdat,
      lv_aedat_fim        TYPE erdat,
      it_saida            TYPE STANDARD TABLE OF zsde0043,
      it_imobilizados_sel TYPE STANDARD TABLE OF zsde0043,
      it_imobilizados_aux TYPE STANDARD TABLE OF zsde0043,
      ls_imobi            TYPE zsde0043.

DATA: lv_lines           TYPE sy-tabix,
      lv_qtd             TYPE sy-tabix,

      start-of-selection.

IF sy-batch EQ abap_true.
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.
  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SELECT SINGLE low
  FROM tvarvc
  INTO lv_dias
 WHERE name EQ 'ZOB_IMOBILIZADO_DIAS'
   AND type EQ 'P'.

lv_erdat_ini = sy-datum - lv_dias.
lv_erdat_fim = sy-datum.

CALL FUNCTION 'ZAA_GET_DATA_IMOBILIZADO'
  EXPORTING
    i_erdat_ini = lv_erdat_ini
    i_erdat_fim = lv_erdat_fim
    i_aedat_ini = lv_aedat_ini
    i_aedat_fim = lv_aedat_fim
  TABLES
    t_saida     = it_saida.

LOOP AT it_saida INTO DATA(ls_saida).
  MOVE-CORRESPONDING: ls_saida TO ls_imobi.
  APPEND ls_imobi TO it_imobilizados_sel.
  CLEAR ls_imobi.
ENDLOOP.

CLEAR: it_saida.
CLEAR: lv_erdat_ini,
       lv_erdat_fim.

lv_aedat_ini = sy-datum - lv_dias.
lv_aedat_fim = sy-datum.

CALL FUNCTION 'ZAA_GET_DATA_IMOBILIZADO'
  EXPORTING
    i_erdat_ini = lv_erdat_ini
    i_erdat_fim = lv_erdat_fim
    i_aedat_ini = lv_aedat_ini
    i_aedat_fim = lv_aedat_fim
  TABLES
    t_saida     = it_saida.



LOOP AT it_saida INTO ls_saida.
  MOVE-CORRESPONDING: ls_saida TO ls_imobi.
  APPEND ls_imobi TO it_imobilizados_sel.
  CLEAR ls_imobi.
ENDLOOP.

SORT it_imobilizados_sel.
DELETE ADJACENT DUPLICATES FROM it_imobilizados_sel COMPARING ALL FIELDS.


DESCRIBE TABLE it_imobilizados_sel LINES lv_lines.

LOOP AT it_imobilizados_sel ASSIGNING FIELD-SYMBOL(<fs_imo>).


  ADD 1 TO lv_qtd.

  APPEND <fs_imo> TO it_imobilizados_aux.

  IF lv_qtd = 500 OR lv_qtd = lv_lines.

    TRY.
        zcl_int_ob_imobilizado=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = it_imobilizados_aux ).
      CATCH zcx_integracao INTO DATA(ex_integra).
        WRITE: / text-m01.
        EXIT.
      CATCH zcx_error INTO DATA(ex_error).
        WRITE: / text-m01.
        EXIT.
    ENDTRY.


    lv_lines = lv_lines - lv_qtd.
    CLEAR lv_qtd.
    REFRESH: it_imobilizados_aux.

  ENDIF.

ENDLOOP.






END-OF-SELECTION.
