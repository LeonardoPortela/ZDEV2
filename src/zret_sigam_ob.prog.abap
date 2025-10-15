*&---------------------------------------------------------------------*
*& Report ZRET_SIGAM_OB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zret_sigam_ob.

TABLES: zret_sigam_algd.
DATA: it_objkey   TYPE STANDARD TABLE OF zret_sigam_algd INITIAL SIZE 0,
      yt_log_mfpf TYPE TABLE OF zfie_ret_document WITH HEADER LINE INITIAL SIZE 0,
      lv_rfc      TYPE rfcdest,
      lv_fm       TYPE rs38l_fnam.

CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

SELECT-OPTIONS: p_objk  FOR zret_sigam_algd-obj_key.

SELECT * FROM zret_sigam_algd
  INTO TABLE @it_objkey
  WHERE obj_key IN @p_objk.


IF it_objkey IS NOT INITIAL.

clear: yt_log_mfpf.

  LOOP AT it_objkey ASSIGNING FIELD-SYMBOL(<it_objkey>).

    yt_log_mfpf-obj_key = <it_objkey>-obj_key.
    yt_log_mfpf-interface = <it_objkey>-interface.
    yt_log_mfpf-dt_atualizacao = <it_objkey>-dt_atualizacao.
    yt_log_mfpf-hr_atualizacao = <it_objkey>-hr_atualizacao.
    yt_log_mfpf-type = <it_objkey>-type.
    yt_log_mfpf-id = <it_objkey>-id.
    yt_log_mfpf-num = <it_objkey>-num.
    yt_log_mfpf-message = <it_objkey>-message.
    yt_log_mfpf-message_v1 = <it_objkey>-message_v1.
    yt_log_mfpf-message_v2 = <it_objkey>-message_v2.
    yt_log_mfpf-message_v3 = <it_objkey>-message_v3.
    yt_log_mfpf-message_v4 = <it_objkey>-message_v4.
    yt_log_mfpf-id_registro = <it_objkey>-id_registro.
    yt_log_mfpf-info_adicional_1 = <it_objkey>-info_adicional_1.
    yt_log_mfpf-info_adicional_2 = <it_objkey>-info_adicional_2.
    yt_log_mfpf-info_adicional_3 = |ZRET_SIGAM_OB|.

    APPEND yt_log_mfpf.

  ENDLOOP.


  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = cc_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION cc_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outreturn = yt_log_mfpf.
  ELSE.
    CALL FUNCTION cc_fm IN BACKGROUND TASK
      TABLES
        outreturn = yt_log_mfpf.
  ENDIF.


ENDIF.

CLEAR: it_objkey.
