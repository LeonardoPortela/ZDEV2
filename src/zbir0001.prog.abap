*&---------------------------------------------------------------------*
*& Report  ZBIR0001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZBIR0001.

TABLES: ZBIT0003, ZBIT0002.

PARAMETER: PTECNICO TYPE ZBIT0003-DS_NOME_TECNICO MATCHCODE OBJECT ZBISH_ZBIT0003.
PARAMETER: PACESSO  TYPE ZBIT0002-TP_ACESSO DEFAULT '02' MATCHCODE OBJECT ZBISH_ZBIT0002.

START-OF-SELECTION.

  CALL FUNCTION 'ZBI_CHAMA_REPORT'
    EXPORTING
      I_NM_TECNICO = PTECNICO
      I_TP_ACESSO  = PACESSO
    EXCEPTIONS
      ERRO         = 1
      OTHERS       = 2.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
