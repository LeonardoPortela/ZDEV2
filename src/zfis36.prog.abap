*&---------------------------------------------------------------------*
*& Report  ZFIS36
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIS36 MESSAGE-ID ZFI.

TABLES: T001, T042Z, ZFIT0143.

DATA: OK_CODE    TYPE SY-UCOMM,
      GB_SAIR    TYPE CHAR01,
      CK_ALTEROU TYPE CHAR01.

CLEAR: T001, GB_SAIR.

WHILE GB_SAIR EQ ABAP_FALSE.
  CALL SCREEN 9010.
  IF T001-BUKRS IS NOT INITIAL.
    T042Z-LAND1 = T001-LAND1.
    CALL SCREEN 1000.
  ENDIF.
  CLEAR: T001, CK_ALTEROU, T042Z.
ENDWHILE.

INCLUDE ZFIS36_9010.

INCLUDE ZFIS36_9001.

INCLUDE ZFIS36_1000.
