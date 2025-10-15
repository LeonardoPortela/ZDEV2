*&---------------------------------------------------------------------*
*&  Include           ZSDT0065_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .
  CLEAR: WG_CADLAN,TG_EDITOR.
  REFRESH:  TG_EDITOR.
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .

  REFRESH TG_MSG_RET.

  IF WG_CADLAN-INSTRUCAO IS INITIAL.
    MOVE:  'WG_CADLAN-INSTRUCAO'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E01 ' Instrução' INTO  TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WG_CADLAN-INSTRUCAO NE '01' AND WG_CADLAN-INSTRUCAO NE '02' AND WG_CADLAN-INSTRUCAO NE '03'.
    MOVE:  'WG_CADLAN-INSTRUCAO'      TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E02 ' Instrução' INTO  TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.


  IF WG_CADLAN-BUKRS IS NOT INITIAL AND WG_CADLAN-BUTXT IS INITIAL.
    MOVE:  'WG_CADLAN-BUKRS'        TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E02 ' Empresa' INTO  TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADLAN-WERKS IS NOT INITIAL AND WG_CADLAN-NAMEW IS INITIAL.
    MOVE:  'WG_CADLAN-WERKS'        TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E02 ' Filial' INTO  TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADLAN-KUNNR IS NOT INITIAL AND WG_CADLAN-NAME1 IS INITIAL.
    MOVE:  'WG_CADLAN-KUNNR'        TO TG_MSG_RET-FIELD.
    CONCATENATE TEXT-E02 ' Cliente' INTO  TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
  "obg_descbox
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    IF TG_EDITOR[] IS INITIAL.
      MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
            'WG_CADLAN-TXT_INSTRUCAO' TO TG_MSG_RET-FIELD.
      CONCATENATE  TG_MSG_RET-MSG 'Texto de Instrução' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_T001       TYPE T001,
        WL_KNA1       TYPE KNA1,
        WL_J_1BBRANCH TYPE J_1BBRANCH,
        WL_ZFIT0048   TYPE ZFIT0048,

        WL_CONT       TYPE SY-TABIX,
        WL_CONT_AUX   TYPE SY-TABIX,
        WL_CONT_AUX2  TYPE SY-TABIX.

  IF WG_CADLAN-KUNNR IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADLAN-KUNNR
      IMPORTING
        OUTPUT = WG_CADLAN-KUNNR.
  ENDIF.

  SELECT SINGLE *
    FROM ZFIT0048
    INTO WL_ZFIT0048
    WHERE INSTRUCAO  = WG_CADLAN-INSTRUCAO
    AND   BUKRS  = WG_CADLAN-BUKRS
    AND   WERKS  = WG_CADLAN-WERKS
    AND   KUNNR  = WG_CADLAN-KUNNR.

  IF SY-SUBRC = 0.
    REFRESH: TG_EDITOR.
*    WG_CADLAN-INSTRUCAO = WL_ZFIT0048-INSTRUCAO.
*    WG_CADLAN-BUKRS     = WL_ZFIT0048-BUKRS.
*    WG_CADLAN-WERKS     = WL_ZFIT0048-WERKS.
*    WG_CADLAN-KUNNR     = WL_ZFIT0048-KUNNR.
    CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
    WL_CONT = STRLEN( WL_ZFIT0048-TXT_INSTRUCAO ).
    WL_CONT_AUX = WL_CONT / 79.

    DO.
      MOVE: WL_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2 TO WG_EDITOR-LINE.
      ADD 80 TO WL_CONT_AUX2.
      APPEND WG_EDITOR TO TG_EDITOR.

      IF WL_CONT_AUX2 GT WL_CONT.
        EXIT.

      ENDIF.
    ENDDO.

    CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
      EXPORTING
        TABLE = TG_EDITOR.
    CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = 1.

  ENDIF.

  IF WG_CADLAN-INSTRUCAO IS NOT INITIAL.
    IF WG_CADLAN-INSTRUCAO = '01'.
      WG_CADLAN-DES_INSTRUCAO = 'Fatura'.
    ELSEIF WG_CADLAN-INSTRUCAO = '02'.
      WG_CADLAN-DES_INSTRUCAO = 'Adiantamento'.
    ELSEIF WG_CADLAN-INSTRUCAO = '03'.
      WG_CADLAN-DES_INSTRUCAO = 'Insumos'.
    ELSE.
      CLEAR WG_CADLAN-DES_INSTRUCAO.
    ENDIF.
  ELSE.
    CLEAR WG_CADLAN-DES_INSTRUCAO.
  ENDIF.

  IF WG_CADLAN-BUKRS IS NOT INITIAL.
    SELECT SINGLE *
      FROM T001
      INTO WL_T001
      WHERE BUKRS = WG_CADLAN-BUKRS.
    IF SY-SUBRC = 0.
      WG_CADLAN-BUTXT = WL_T001-BUTXT.
    ELSE.
      CLEAR WG_CADLAN-BUTXT.
    ENDIF.
  ELSE.
    CLEAR  WG_CADLAN-BUTXT .
  ENDIF.

  IF WG_CADLAN-WERKS IS NOT INITIAL.
    SELECT SINGLE *
      FROM J_1BBRANCH
      INTO WL_J_1BBRANCH
      WHERE BUKRS  = WG_CADLAN-BUKRS
      AND   BRANCH = WG_CADLAN-WERKS.
    IF SY-SUBRC = 0.
      WG_CADLAN-NAMEW = WL_J_1BBRANCH-NAME.
    ELSE.
      CLEAR WG_CADLAN-NAMEW.
    ENDIF.
  ELSE.
    CLEAR  WG_CADLAN-NAMEW.
  ENDIF.

  IF WG_CADLAN-KUNNR IS NOT INITIAL.
    SELECT SINGLE *
      FROM KNA1
      INTO WL_KNA1
      WHERE KUNNR = WG_CADLAN-KUNNR.
    IF SY-SUBRC = 0.
      WG_CADLAN-NAME1 = WL_KNA1-NAME1.
    ELSE.
      CLEAR WG_CADLAN-NAME1.
    ENDIF.
  ELSE.
    CLEAR  WG_CADLAN-NAME1.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: WL_INPUT_ZFIT0048   TYPE ZFIT0048,
        VTXT_INSTRUCAO(480),
        VPOS                TYPE I.

  MOVE: SY-MANDT                TO WL_INPUT_ZFIT0048-MANDT,
        WG_CADLAN-INSTRUCAO     TO WL_INPUT_ZFIT0048-INSTRUCAO,
        WG_CADLAN-BUKRS         TO WL_INPUT_ZFIT0048-BUKRS,
        WG_CADLAN-WERKS         TO WL_INPUT_ZFIT0048-WERKS,
        WG_CADLAN-KUNNR         TO WL_INPUT_ZFIT0048-KUNNR,
        SY-UNAME TO WL_INPUT_ZFIT0048-USNAM,
        SY-DATUM TO WL_INPUT_ZFIT0048-DATA_ATUAL,
        SY-UZEIT TO WL_INPUT_ZFIT0048-HORA_ATUAL.

  REFRESH: TG_EDITOR.
  CLEAR VTXT_INSTRUCAO.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.
    LOOP AT TG_EDITOR INTO WG_EDITOR.
      VPOS = ( SY-TABIX - 1 ) * 80.
      VTXT_INSTRUCAO+VPOS = WG_EDITOR-LINE.
    ENDLOOP.
  ENDIF.
  WL_INPUT_ZFIT0048-TXT_INSTRUCAO = VTXT_INSTRUCAO.


  MODIFY ZFIT0048 FROM       WL_INPUT_ZFIT0048.

  MESSAGE S836(SD) WITH 'Instrução'
                         ', criada/modificada com sucesso!'.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " TRATA_CAMPOS
