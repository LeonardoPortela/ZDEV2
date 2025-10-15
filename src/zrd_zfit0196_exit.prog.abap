*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0196_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0196_exit.

FORM f_exit_zfit0196_0001 USING p_registro_manter TYPE any.


  DATA: wl_zfit0196 TYPE zfit0196.

  CLEAR: wl_zfit0196.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0196.

  wl_zfit0196-dt_registro      = sy-datum.
  wl_zfit0196-hr_registro      = sy-uzeit.
  wl_zfit0196-us_registro      = sy-uname.


  MOVE-CORRESPONDING wl_zfit0196 TO p_registro_manter.

ENDFORM.
FORM f_exit_zfit0196_0003 USING p_registro_manter TYPE any.
  SELECT COUNT( * )
         FROM zfit0196.
  IF sy-subrc EQ 0.
    MESSAGE 'Registro já cadastrado!' TYPE 'E'.
  ENDIF.
ENDFORM.
