FUNCTION z_fi_document_change_ret.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BKPF) LIKE  BKPF STRUCTURE  BKPF
*"     VALUE(I_BSEG) LIKE  BSEG STRUCTURE  BSEG
*"----------------------------------------------------------------------

  TABLES zfit0005.

*> Filtra somente os tipos de documento para retorno
*> através de interface outbound (SIGAM/FOLHA)

  SELECT SINGLE * FROM zfit0005
          WHERE ( blart EQ i_bkpf-blart ).

  CHECK ( sy-subrc EQ 0 ).
  CHECK i_bseg-zfbdt IS NOT INITIAL.  "17/11/2023 em consulta no banco todos documento necessitam de data

  REFRESH it_change.
  CLEAR wa_change.
  wa_change-obj_key        = i_bkpf-awkey.
  wa_change-belnr          = i_bkpf-belnr.
  wa_change-dt_atualizacao = sy-datum.
  wa_change-hr_atualizacao = sy-uzeit.
  wa_change-cd_transacao   = 'FB02'.
  wa_change-zfbdt          = i_bseg-zfbdt.
  wa_change-zuonr          = i_bseg-zuonr.
  wa_change-xblnr          = i_bkpf-xblnr.
  wa_change-bktxt          = i_bkpf-bktxt.
  wa_change-sgtxt          = i_bseg-sgtxt.
  APPEND wa_change TO it_change.

*> Executa a rotina outbound de alteração p/ docto contábil

*--> 22.08.2023 23:52:59 - Migração S4 – ML - Início
*  call function 'Z_FI_OUTBOUND_CHANGE_DOC' in background task
*    destination 'XI_SIGAM_CHANGE_DOC'
*    as separate unit
*    tables
*      outchange = it_change.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_CHANGE_DOC'.

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
        outchange = it_change.
  ELSE.
    CALL FUNCTION c_fm
      DESTINATION 'NONE'
      TABLES
        outchange = it_change.
  ENDIF.
*<-- 22.08.2023 23:52:59 - Migração S4 – ML – Fim

  COMMIT WORK.

ENDFUNCTION.
