*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0045
*&
*&---------------------------------------------------------------------*
*&
*&  Cadastro de Ausências ou Presenças bloqueantes
*&---------------------------------------------------------------------*
REPORT ZHCMR_PA0045 MESSAGE-ID ZHCM.

TABLES: ZHCMT_PA_0015.

DATA: OK_CODE    TYPE SY-UCOMM,
      CK_ALTEROU TYPE CHAR01.

PERFORM AUSENCIAS_DISPONIVEIS.
PERFORM AUSENCIAS_VINCULADAS.

CALL SCREEN 1000.

INCLUDE ZHCMR_PA0045_1000.
