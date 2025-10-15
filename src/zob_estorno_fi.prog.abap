*&---------------------------------------------------------------------*
*& Report  ZOB_COST_CENTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zob_estorno_fi.

CONSTANTS: lc_name TYPE tvarvc-name VALUE 'ZOB_ESTORNO_FI_DIAS'.

DATA: lv_data_ini TYPE erdat.

DATA: lit_docs_estornados  TYPE TABLE OF zfie_doc_contab_estor,
      lit_reverse_document TYPE TABLE OF zfie_reverse_document_ret,
      lit_estorndos_send   TYPE TABLE OF zfit0193.

IF sy-batch EQ abap_true.
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.
  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SELECT *
  FROM tvarvc
  INTO @DATA(lw_tvarv)
  UP TO 1 ROWS
  WHERE name = @lc_name.
ENDSELECT.
IF sy-subrc IS INITIAL.
  lv_data_ini = sy-datum - lw_tvarv-low.
ELSE.
  lv_data_ini = sy-datum - 2.
ENDIF.

CALL FUNCTION 'ZFI_GET_DOC_CTB_ESTORNADO'
  EXPORTING
    i_data_ini        = lv_data_ini
    i_data_fim        = sy-datum
    i_no_send_legados = abap_true
  TABLES
    t_saida           = lit_docs_estornados.

CHECK lit_docs_estornados[] IS NOT INITIAL.

LOOP AT lit_docs_estornados ASSIGNING FIELD-SYMBOL(<fs_doc_estornado>).

  APPEND INITIAL LINE TO lit_reverse_document ASSIGNING FIELD-SYMBOL(<fs_reverse_document>).

  IF <fs_doc_estornado>-awkey IS NOT INITIAL.
    <fs_reverse_document>-obj_key_estorno = <fs_doc_estornado>-awkey.
  ELSE.
    <fs_reverse_document>-obj_key_estorno = <fs_doc_estornado>-bukrs && <fs_doc_estornado>-belnr && <fs_doc_estornado>-gjahr.
  ENDIF.

  <fs_reverse_document>-bukrs  = <fs_doc_estornado>-bukrs.
  <fs_reverse_document>-belnr  = <fs_doc_estornado>-stblg.
  <fs_reverse_document>-gjahr  = <fs_doc_estornado>-gjahr_estorno.

  <fs_reverse_document>-bukrs_e = <fs_doc_estornado>-bukrs.
  <fs_reverse_document>-belnr_e = <fs_doc_estornado>-belnr.
  <fs_reverse_document>-gjahr_e = <fs_doc_estornado>-gjahr.

  <fs_reverse_document>-docnum = <fs_doc_estornado>-docnum.
  <fs_reverse_document>-dt_atualizacao = sy-datum.
  <fs_reverse_document>-hr_atualizacao = sy-uzeit.

  IF <fs_doc_estornado>-origem_fiscal EQ 'S'.
    <fs_reverse_document>-cd_transacao = 'J1B2'.
  ELSE.
    <fs_reverse_document>-cd_transacao = <fs_doc_estornado>-tcode.
  ENDIF.

  APPEND INITIAL LINE TO lit_estorndos_send   ASSIGNING FIELD-SYMBOL(<fs_estornados_send>).

  <fs_estornados_send>-bukrs  = <fs_doc_estornado>-bukrs.
  <fs_estornados_send>-belnr  = <fs_doc_estornado>-belnr.
  <fs_estornados_send>-gjahr  = <fs_doc_estornado>-gjahr.
  <fs_estornados_send>-erdat  = sy-datum.
  <fs_estornados_send>-erzet  = sy-uzeit.

ENDLOOP.

*--> 24.08.2023 18:57:30 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_FI_OUTBOUND_REVERSE'
*    TABLES
*      outreverse = lit_reverse_document.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_REVERSE'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outreverse = lit_reverse_document.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outreverse = lit_reverse_document.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 18:57:30 - Migração S4 – ML – Fim

MODIFY zfit0193 FROM TABLE lit_estorndos_send.

COMMIT WORK.
