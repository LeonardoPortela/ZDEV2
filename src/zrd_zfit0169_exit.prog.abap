*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0169_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0169_exit.

FORM f_exit_zfit0169_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zfit0169 TYPE zfit0169.
  CLEAR:  wa_zfit0169.

  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0169.

ENDFORM.

FORM f_exit_zfit0169_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfit0169 TYPE zfit0169.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0169.
  wa_zfit0169-usnam = sy-uname.
  wa_zfit0169-dt_atual = sy-datum.
  wa_zfit0169-hr_atual = sy-uzeit.
  MOVE-CORRESPONDING wa_zfit0169 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0169_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfit0169 TYPE zfit0169.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfit0169.
  MOVE-CORRESPONDING wa_zfit0169 TO p_registro_manter.

ENDFORM.
