FUNCTION z_centro_custo_zfit0218_help .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA: l_tcode TYPE tcode.
  CLEAR: l_tcode.
  GET PARAMETER ID 'TCD' FIELD l_tcode.
  IF l_tcode = 'ZFIR0111'."'ZFI0165'.
    LOOP AT shlp_tab ASSIGNING FIELD-SYMBOL(<_mod>).
      LOOP AT <_mod>-interface ASSIGNING FIELD-SYMBOL(<_mod_interface>).
        CASE <_mod_interface>-shlpfield .
          WHEN 'BUKRS'.
            GET PARAMETER ID 'BUK' FIELD <_mod_interface>-value.
          WHEN 'WERKS'.
            GET PARAMETER ID 'WRK' FIELD <_mod_interface>-value.
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  CLEAR: l_tcode.
  "LIMPA VARIAVEIS DE MEMORIA!
  SET PARAMETER ID 'TCD' FIELD l_tcode.
  SET PARAMETER ID 'BUK' FIELD l_tcode.
  SET PARAMETER ID 'WRK' FIELD l_tcode.

ENDFUNCTION.
