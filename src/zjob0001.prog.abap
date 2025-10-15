*&---------------------------------------------------------------------**
*& Report  ZJOB0001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZJOB0001.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*

TYPE-POOLS: ICON.
TYPE-POOLS: SLIS.

TYPES: BEGIN OF L_SAIDA,
        ICON TYPE STRING.
        INCLUDE STRUCTURE ZJOB0001.
TYPES: END OF L_SAIDA.

*----------------------------------------------------------------------*
*                                 VARIÁVEIS                            *
*----------------------------------------------------------------------*

DATA: IT_SAIDA    TYPE table of L_SAIDA,
      WA_SAIDA    TYPE L_SAIDA,
      WA_ZJOB0001 TYPE ZJOB0001,
      BT          TYPE STRING,
      POINT       LIKE SY-TABIX.

DATA: IT_FCAT TYPE SLIS_T_FIELDCAT_ALV.

*----------------------------------------------------------------------*
*                                 SELEÇÃO                              *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM SELECT_TAB. "Performa formulário de seleção de dados da ZJOB0001

END-OF-SELECTION.

  PERFORM PRINT_ALV. "Imprime ALV

*----------------------------------------------------------------------*
*                                 FORMS                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_TAB
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM SELECT_TAB.

  SELECT * FROM ZJOB0001 INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA.

  LOOP AT IT_SAIDA INTO WA_SAIDA. "Loop para completar a tabela IT_SAIDA com os ícones a partir do status do JOB

    IF WA_SAIDA-STATUS = 'A'.
      WA_SAIDA-ICON = ICON_CHECKED.
    ENDIF.
    IF WA_SAIDA-STATUS = 'S'.
      WA_SAIDA-ICON = ICON_MESSAGE_CRITICAL.
    ENDIF.
    MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING ICON.
  ENDLOOP.

ENDFORM.

INCLUDE ZJOB0001_1000. "Include doS formulárioS para a impressão da ALV.
INCLUDE ZJOB0001_USER_COMMAND_1001I01. "Include dos comandos da tela 1001.
