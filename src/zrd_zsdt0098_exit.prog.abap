*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0150_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRD_ZSDT0098_EXIT.

FORM F_EXIT_ZSDT0098_0003 CHANGING P_REGISTRO_MANTER TYPE ANY.

  DATA: WL_ZSDT0098 TYPE ZSDT0098.

  CLEAR: WL_ZSDT0098.

  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WL_ZSDT0098.

  WL_ZSDT0098-DATA_REGISTRO = SY-DATUM.
  WL_ZSDT0098-HORA_REGISTRO = SY-UZEIT.

  MOVE-CORRESPONDING WL_ZSDT0098 TO P_REGISTRO_MANTER.

ENDFORM.
