FUNCTION zmm_nfse_popup_cancelamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_EXIBIR) TYPE  C DEFAULT SPACE
*"  CHANGING
*"     REFERENCE(EV_CANC) TYPE  FLAG
*"     REFERENCE(EV_MOTIVO) TYPE  ZMOTIVO_CANCELAR
*"     REFERENCE(EV_DESCR) TYPE  ZMOTIVO_DESCRICAO
*"----------------------------------------------------------------------

  DATA lv_string TYPE string.

  "CLEAR: ev_motivo, ev_canc, ev_descr, gv_9070_canc.

  gv_9070_readonly = iv_exibir.

  gv_9070_motivo = EV_MOTIVO.
  gv_9070_descr  = EV_DESCR.

  CALL SCREEN 9070 STARTING AT 20 1.  "Popup Confirm Cancel NFS-e Process

  ev_canc = gv_9070_canc.

  "/tcsr/t_cancrt

  ev_motivo = /tcsr/t_cancrt-cancreason.

  IF gt_cancr_text IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        i_tabline_length = 100
      IMPORTING
        e_string         = lv_string
      TABLES
        it_table         = gt_cancr_text.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>endian IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>minchar IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>maxchar IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>vertical_tab IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN lv_string WITH space.

    ev_descr = lv_string.

  ENDIF.

ENDFUNCTION.
