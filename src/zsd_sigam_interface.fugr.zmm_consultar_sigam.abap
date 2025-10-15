FUNCTION zmm_consultar_sigam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TCODE) TYPE  SY-TCODE DEFAULT 'ME21N'
*"  EXPORTING
*"     REFERENCE(E_BUKRS) TYPE  BUKRS
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"     REFERENCE(E_SCR_STATUS) TYPE  CHAR1
*"     REFERENCE(ES_PARAM) TYPE  ZSDE0011
*"     REFERENCE(ET_DADOS_LOTE1) TYPE  ZCDE0005
*"     REFERENCE(ET_DADOS_ORDENS1) TYPE  ZCDE0006
*"----------------------------------------------------------------------

  DATA lv_erro TYPE c.

  PERFORM f_refresh_all.

  PERFORM f_define_param USING i_tcode space CHANGING lv_erro.

  es_param = gv_param.

  CHECK lv_erro IS INITIAL.

  CALL SCREEN 9000.

  e_scr_status = gv_screen_status.

ENDFUNCTION.
