*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0128_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRD_ZMMT0128_EXIT.


FORM F_EXIT_ZMMT0128_0003 CHANGING P_REGISTRO_MANTER TYPE ANY.

  DATA: WA_ZMMT0128 TYPE ZMMT0128.
  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WA_ZMMT0128.
  WA_ZMMT0128-USNAM = SY-UNAME.
  WA_ZMMT0128-ZDT_ATUAL = SY-DATUM.
  WA_ZMMT0128-ZHR_ATUAL = SY-UZEIT.
  MOVE-CORRESPONDING WA_ZMMT0128 TO P_REGISTRO_MANTER.

ENDFORM.
