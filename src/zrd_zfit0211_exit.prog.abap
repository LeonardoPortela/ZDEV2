*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0165_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0211_exit.

FORM f_exit_zfit0211_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0211 TYPE zfit0211.

  CLEAR: wl_zfit0211.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0211.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zfit0211-hkont
    IMPORTING
      output = wl_zfit0211-hkont.

  IF wl_zfit0211-user_create IS INITIAL.
    wl_zfit0211-date_create = sy-datum.
    wl_zfit0211-time_create = sy-uzeit.
    wl_zfit0211-user_create = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0211 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0211_0002  USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: wl_zfit0211 TYPE zfit0211.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zfit0211.

  IF wl_zfit0211-hkont IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Conta'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
