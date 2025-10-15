*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0150_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRD_ZSDT0253_EXIT.

FORM F_EXIT_ZSDT0253_0002 USING P_REGISTRO_MANTER TYPE ANY
                       CHANGING P_ERROR.

  DATA: WL_ZSDT0253 TYPE ZSDT0253.

  CLEAR: WL_ZSDT0253.

  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WL_ZSDT0253.

  CLEAR: P_ERROR.

  IF WL_ZSDT0253-BRANCH IS INITIAL.
    P_ERROR = ABAP_TRUE.
    MESSAGE 'Filial Industrializadora é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
