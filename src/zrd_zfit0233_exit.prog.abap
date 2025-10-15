*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0165_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0233_exit.

FORM f_exit_zfit0233_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0233 TYPE zfit0233.

  CLEAR: wl_zfit0233.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0233.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zfit0233-hkont
    IMPORTING
      output = wl_zfit0233-hkont.

  IF wl_zfit0233-user_create IS INITIAL.
    wl_zfit0233-date_create = sy-datum.
    wl_zfit0233-time_create = sy-uzeit.
    wl_zfit0233-user_create = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0233 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0233_0002  USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: wl_zfit0233 TYPE zfit0233.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zfit0233.

  IF wl_zfit0233-hkont IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Conta'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
