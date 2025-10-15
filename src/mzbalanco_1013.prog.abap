*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1013 .
*----------------------------------------------------------------------*

DATA: WA_ZGLT047 TYPE ZGLT047.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1013_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1013_EXIT INPUT.
  CLEAR: IT_ZGLT047.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_1013_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1013  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1013 OUTPUT.
  SET PF-STATUS 'PF1003'.
  SET TITLEBAR 'TL1014'.

  CLEAR: WA_ZGLT047, WA_ZGLT047_ALTERADO.

  IF IT_ZGLT047-NIVELPAI IS NOT INITIAL.
    SELECT SINGLE * INTO WA_ZGLT047
      FROM ZGLT047
     WHERE VERSN EQ IT_ZGLT047-VERSN
       AND NIVEL EQ IT_ZGLT047-NIVELPAI.
  ENDIF.

  LOOP AT SCREEN.

    IF CK_NIVEL_DEFINIDO = ABAP_TRUE.
      IF ( SCREEN-NAME EQ 'IT_ZGLT047-NIVELPAI' OR
           SCREEN-NAME EQ 'IT_ZGLT047-SQNIVEL'  ).
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF ( SCREEN-NAME EQ 'IT_ZGLT047-VERSN' OR
         SCREEN-NAME EQ 'IT_ZGLT046_ALV-VSTXT' OR
         SCREEN-NAME EQ 'WA_ZGLT047-DESNVL' OR
         SCREEN-NAME EQ 'WA_ZGLT047-SQNIVEL'  ).
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
    IF CK_EDITAR EQ ABAP_TRUE.
      IF SCREEN-NAME EQ 'IT_ZGLT047-NIVEL' OR
         SCREEN-NAME EQ 'IT_ZGLT047-NIVELSUM'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_1013  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1013  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1013 INPUT.

  IF OK_CODE_1013 EQ OK_CONF.

    CLEAR: WA_ZGLT047.

    SELECT SINGLE * INTO WA_ZGLT047
      FROM ZGLT047
     WHERE VERSN EQ IT_ZGLT047-VERSN
       AND NIVEL EQ IT_ZGLT047-NIVEL.

    IF CK_EDITAR EQ ABAP_TRUE.
      "Foi Alterado o pai
      IF ( WA_ZGLT047-NIVELPAI NE IT_ZGLT047-NIVELPAI ).
        PERFORM AJUSTA_NIVELPAI USING IT_ZGLT047 WA_ZGLT047-NIVELPAI IT_ZGLT047-NIVELPAI.
      ELSE.
      "Foi Alterado a Sequência
        IF ( WA_ZGLT047-SQNIVEL NE IT_ZGLT047-SQNIVEL ).
          PERFORM AJUSTA_NIVEL USING IT_ZGLT047.
        ENDIF.
      ENDIF.
    ELSE.

      IF WA_ZGLT047 IS NOT INITIAL.
        MESSAGE W003 WITH IT_ZGLT047-NIVEL IT_ZGLT047-VERSN DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      PERFORM AJUSTA_NIVEL USING IT_ZGLT047.

    ENDIF.

    MOVE-CORRESPONDING IT_ZGLT047 TO WA_ZGLT047_ALTERADO.
    MODIFY ZGLT047 FROM IT_ZGLT047.

    "Verifica Existencia de Nota com Nível.
    "Reconciliação Contábil – Parâmetros Gerais
    IF CK_EDITAR EQ ABAP_FALSE.
      SELECT * INTO TABLE IT_ZGLT041
        FROM ZGLT041
       WHERE COD_CLAS_BAL EQ IT_ZGLT047-NIVEL
       ORDER BY COD_CLAS_BAL COD_CLAS_NOT2.

      IF SY-SUBRC IS INITIAL.
        PERFORM INCLUIR_NOTAS TABLES IT_ZGLT041 USING IT_ZGLT047.
      ENDIF.
    ENDIF.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1013  INPUT


*&---------------------------------------------------------------------*
*&      Form  AJUSTA_NIVELPAI
*&---------------------------------------------------------------------*
*       Ajustar Atribuição de Nível pai
*----------------------------------------------------------------------*
*      -->P_ZGLT047       - Nível
*      -->P_DE_NIVELPAI   - Nível Pai Antigo
*      -->P_PARA_NIVELPAI - Nível Pai Atribuido
*----------------------------------------------------------------------*
FORM AJUSTA_NIVELPAI  USING    P_ZGLT047       TYPE ZGLT047
                               P_DE_NIVELPAI   TYPE ZDENIVELBP
                               P_PARA_NIVELPAI TYPE ZDENIVELBP.

  DATA: IT_ZGLT047_DE TYPE TABLE OF ZGLT047 WITH HEADER LINE,
        LC_POSICAO    TYPE ZDESQNIVEL,
        CK_POS_MEIO(1).

  SELECT * INTO TABLE IT_ZGLT047_DE
    FROM ZGLT047
   WHERE VERSN    EQ P_ZGLT047-VERSN
     AND NIVELPAI EQ P_DE_NIVELPAI
     AND NIVEL    NE P_ZGLT047-NIVEL
   ORDER BY SQNIVEL.

  LC_POSICAO = 0.
  LOOP AT IT_ZGLT047_DE.
    ADD 1 TO LC_POSICAO.
    IT_ZGLT047_DE-SQNIVEL = LC_POSICAO.
    MODIFY ZGLT047 FROM IT_ZGLT047_DE.
  ENDLOOP.

  CLEAR: IT_ZGLT047_DE[].

  SELECT * INTO TABLE IT_ZGLT047_DE
    FROM ZGLT047
   WHERE VERSN    EQ P_ZGLT047-VERSN
     AND NIVELPAI EQ P_PARA_NIVELPAI
   ORDER BY SQNIVEL.

  CLEAR: CK_POS_MEIO.
  LC_POSICAO = 0.
  LOOP AT IT_ZGLT047_DE.
    ADD 1 TO LC_POSICAO.

    "Se posição pai distino igual a posicao da posição do nivel transferido
    IF LC_POSICAO EQ P_ZGLT047-SQNIVEL.
      ADD 1 TO LC_POSICAO.
      CK_POS_MEIO = ABAP_TRUE.
    ENDIF.

    IT_ZGLT047_DE-SQNIVEL = LC_POSICAO.
    MODIFY ZGLT047 FROM IT_ZGLT047_DE.
  ENDLOOP.

  "Se não foi incluido no meio vai resceber a ultima posição da sequencia.
  IF CK_POS_MEIO EQ ABAP_FALSE.
    ADD 1 TO LC_POSICAO.
    P_ZGLT047-SQNIVEL = LC_POSICAO.
  ENDIF.

ENDFORM.                    " AJUSTA_NIVELPAI

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZGLT047         - Nível
*----------------------------------------------------------------------*
FORM AJUSTA_NIVEL  USING  P_ZGLT047 TYPE ZGLT047.

  DATA: IT_ZGLT047_DE TYPE TABLE OF ZGLT047 WITH HEADER LINE,
        LC_POSICAO    TYPE ZDESQNIVEL,
        CK_POS_MEIO(1).

  CLEAR: IT_ZGLT047_DE[].

  SELECT * INTO TABLE IT_ZGLT047_DE
    FROM ZGLT047
   WHERE VERSN    EQ P_ZGLT047-VERSN
     AND NIVELPAI EQ P_ZGLT047-NIVELPAI
     AND NIVEL    NE P_ZGLT047-NIVEL
   ORDER BY SQNIVEL.

  CLEAR: CK_POS_MEIO.
  LC_POSICAO = 0.
  LOOP AT IT_ZGLT047_DE.
    ADD 1 TO LC_POSICAO.

    "Se posição pai distino igual a posicao da posição do nivel transferido
    IF LC_POSICAO EQ P_ZGLT047-SQNIVEL.
      ADD 1 TO LC_POSICAO.
      CK_POS_MEIO = ABAP_TRUE.
    ENDIF.

    IT_ZGLT047_DE-SQNIVEL = LC_POSICAO.
    MODIFY ZGLT047 FROM IT_ZGLT047_DE.
  ENDLOOP.

  "Se não foi incluido no meio vai resceber a ultima posição da sequencia.
  IF CK_POS_MEIO EQ ABAP_FALSE.
    ADD 1 TO LC_POSICAO.
    P_ZGLT047-SQNIVEL = LC_POSICAO.
  ENDIF.

ENDFORM.                    " AJUSTA_NIVEL

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZGLT041  Reconciliação Contábil – Parâmetros Gerais (Notas)
*      -->P_ZGLT047  Nós de estrutura de Balanço Patrimonial
*----------------------------------------------------------------------*
FORM INCLUIR_NOTAS  TABLES P_ZGLT041 STRUCTURE ZGLT041
                    USING  P_ZGLT047 TYPE ZGLT047.

  DATA: ANSWER     TYPE C LENGTH 1,
        WA_ZGLT041 TYPE ZGLT041,
        WA_ZGLT049 TYPE ZGLT049,
        LC_POSICAO TYPE ZDESQNIVEL,
        TEXTLINE1  TYPE STRING.

  CONCATENATE 'O Nível' P_ZGLT047-NIVEL 'possui Nota(s) de Classificação de Balanço!'
         INTO TEXTLINE1 SEPARATED BY SPACE.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL     = 'Atenção!'
      TEXTLINE1 = TEXTLINE1
      TEXTLINE2 = 'Deseja Incluir todas as notas neste Nível?'
    IMPORTING
      ANSWER    = ANSWER.

  IF ANSWER = 'J'.
    LC_POSICAO = 0.
    LOOP AT P_ZGLT041 INTO WA_ZGLT041 .
      CLEAR: WA_ZGLT049.
      ADD 1 TO LC_POSICAO.
      WA_ZGLT049-VERSN        = P_ZGLT047-VERSN.
      WA_ZGLT049-NIVEL        = P_ZGLT047-NIVEL.
      WA_ZGLT049-COD_CLAS_BAL = WA_ZGLT041-COD_CLAS_BAL.
      WA_ZGLT049-COD_CLAS_NOT = WA_ZGLT041-COD_CLAS_NOT2.
      WA_ZGLT049-SQNIVEL      = LC_POSICAO.
      MODIFY ZGLT049 FROM WA_ZGLT049.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " INCLUIR_NOTAS
