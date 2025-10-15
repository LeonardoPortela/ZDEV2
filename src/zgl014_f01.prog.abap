*&---------------------------------------------------------------------*
*&  Include           ZGL014_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS.
  CLEAR:    WG_ZGLT034, X_FIELD.
  REFRESH:  TG_ZGLT034.

  WG_ZGLT034-USNAM = SY-UNAME.
  SELECT SINGLE *
    FROM USR05
    INTO @DATA(_USR05)
    WHERE BNAME = @SY-UNAME
    AND PARID   = 'BUK'.
  IF SY-SUBRC = 0.
    WG_ZGLT034-BUKRS = _USR05-PARVA+0(4).
  ENDIF.
ENDFORM.                    " F_LIMPA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_OBTEM_PROXIMO.
  DATA: VL_NUMBER TYPE I.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZGL_LOTE'
    IMPORTING
      NUMBER                  = VL_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    WG_ZGLT034-LOTE = VL_NUMBER.
  ENDIF.
ENDFORM.                    " F_OBTEM_PROXIMO

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS.
*  CHECK:  WG_ZGLT034-LOTE       IS NOT INITIAL

  CHECK  WG_ZGLT034-DESCR_LOTE IS NOT INITIAL
    AND  WG_ZGLT034-BUKRS      IS NOT INITIAL
    AND  WG_ZGLT034-USNAM      IS NOT INITIAL
    AND  WG_ZGLT034-DEP_RESP   IS NOT INITIAL.

  DATA: WL_INPUT_GLT034 TYPE ZGLT034,
        TL_INPUT_GLT034 TYPE TABLE OF ZGLT034 WITH HEADER LINE.

  DATA: R_GERAR_LOTE    TYPE REF TO ZCL_GERAR_LOTE.
  CREATE OBJECT R_GERAR_LOTE.

  IF P_BUKRS[] IS NOT INITIAL.
    LOOP AT P_BUKRS.
      IF WG_ZGLT034-LOTE IS INITIAL.
        R_GERAR_LOTE->CREATE_LOTE( EXPORTING
                                   I_BUKRS      = P_BUKRS-LOW
                                   I_DESCR_LOTE = WG_ZGLT034-DESCR_LOTE
                                   I_DEP_RESP   = WG_ZGLT034-DEP_RESP
                                   I_USER_RESP  = WG_ZGLT034-USNAM
                                   IMPORTING
                                   E_NUM_LOTE   = WG_ZGLT034-LOTE ).
      ELSE.
        R_GERAR_LOTE->MODIFY_LOTE( EXPORTING
                                   I_NUM_LOTE   = WG_ZGLT034-LOTE
                                   I_BUKRS      = P_BUKRS-LOW
                                   I_DESCR_LOTE = WG_ZGLT034-DESCR_LOTE
                                   I_DEP_RESP   = WG_ZGLT034-DEP_RESP
                                   I_USER_RESP  = WG_ZGLT034-USNAM ).
      ENDIF.
    ENDLOOP.

*      MOVE: WG_ZGLT034-LOTE         TO TL_INPUT_GLT034-LOTE,
*            WG_ZGLT034-DESCR_LOTE   TO TL_INPUT_GLT034-DESCR_LOTE,
*            P_BUKRS-LOW             TO TL_INPUT_GLT034-BUKRS,
*            WG_ZGLT034-USNAM        TO TL_INPUT_GLT034-USNAM,
*            WG_ZGLT034-DEP_RESP     TO TL_INPUT_GLT034-DEP_RESP,
*            SY-DATUM                TO TL_INPUT_GLT034-DATA_ATUAL,
*            SY-UZEIT                TO TL_INPUT_GLT034-HORA_ATUAL,
*            SY-UNAME                TO TL_INPUT_GLT034-USUARIO.

*      APPEND TL_INPUT_GLT034.
*      IF  SY-TABIX LT LINES( P_BUKRS ).
*        PERFORM F_OBTEM_PROXIMO.
*      ENDIF.

*    MODIFY ZGLT034 FROM TABLE TL_INPUT_GLT034.

  ELSE.

    IF WG_ZGLT034-LOTE IS INITIAL.
      R_GERAR_LOTE->CREATE_LOTE( EXPORTING
                                 I_BUKRS      = WG_ZGLT034-BUKRS
                                 I_DESCR_LOTE = WG_ZGLT034-DESCR_LOTE
                                 I_DEP_RESP   = WG_ZGLT034-DEP_RESP
                                 I_USER_RESP  = WG_ZGLT034-USNAM
                                 IMPORTING
                                 E_NUM_LOTE   = WG_ZGLT034-LOTE ).
    ELSE.
      R_GERAR_LOTE->MODIFY_LOTE( EXPORTING
                                 I_NUM_LOTE   = WG_ZGLT034-LOTE
                                 I_BUKRS      = WG_ZGLT034-BUKRS
                                 I_DESCR_LOTE = WG_ZGLT034-DESCR_LOTE
                                 I_DEP_RESP   = WG_ZGLT034-DEP_RESP
                                 I_USER_RESP  = WG_ZGLT034-USNAM ).
    ENDIF.
  ENDIF.
  MESSAGE S836(SD) WITH TEXT-M01  WG_ZGLT034-LOTE TEXT-M02.
  "
  SET PARAMETER ID 'LOT' FIELD WG_ZGLT034-LOTE.
  IF SY-CALLD = 'X'.
    LEAVE PROGRAM.
  ENDIF.
  "CLEAR: WG_ZGLT034.
ENDFORM.                    " F_GRAVA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " F_TRATA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS USING P_NUMERO_LOTE TYPE ZLOTE_NUM.
  DATA: WL_ZGLT034        TYPE ZGLT034,
        WL_ZIMP_CAD_DEPTO TYPE ZIMP_CAD_DEPTO.

  WG_ZGLT034-LOTE = P_NUMERO_LOTE.

  IF WG_ZGLT034-LOTE IS NOT INITIAL.
    SELECT SINGLE * FROM ZGLT034
      INTO WL_ZGLT034
    WHERE LOTE EQ WG_ZGLT034-LOTE.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-E01.
      LEAVE TO SCREEN 100.
*    ELSEIF wl_zglt031-loekz IS NOT INITIAL.
*      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de lote foi eliminado!'.
*      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING WL_ZGLT034 TO WG_ZGLT034.
    ENDIF.
  ENDIF.

  IF WG_ZGLT034-BUKRS IS NOT INITIAL.
    SELECT SINGLE DEP_RESP
    FROM ZIMP_CAD_DEPTO
    INTO WG_ZGLT034-DEP_RESP
    WHERE BUKRS = WG_ZGLT034-BUKRS.
  ENDIF.

  CLEAR WG_ACAO.
  REFRESH: TG_FIELDS.
  PERFORM F_TRATA_CAMPOS USING  SPACE
                                'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0

  PERFORM F_TRATA_CAMPOS USING  SPACE
                                'GR1'
                                C_1         "INPUT 1     NO INPUT 0
                                C_0.        "INVISIBLE 1 VISIBLE 0
ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .
  DATA: WL_ZIMP_CAD_DEPTO TYPE ZIMP_CAD_DEPTO,
        WL_T001           TYPE T001,
        W_MSG(50).
  SELECT SINGLE *
    FROM ZIMP_CAD_DEPTO
    INTO WL_ZIMP_CAD_DEPTO
    WHERE DEP_RESP = WG_ZGLT034-DEP_RESP.

  IF SY-SUBRC NE 0.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-E02.
    WL_ERRO = 'X'.
  ELSEIF WL_ZIMP_CAD_DEPTO-INATIVO = 'S'.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-E06.
    WL_ERRO = 'X'.
  ENDIF.

  IF P_BUKRS[] IS NOT INITIAL.
    LOOP AT P_BUKRS.
      SELECT SINGLE *
        FROM T001
        INTO WL_T001
        WHERE BUKRS = P_BUKRS-LOW.
      IF SY-SUBRC NE 0.
        CONCATENATE TEXT-E03 '' P_BUKRS-LOW INTO W_MSG SEPARATED BY SPACE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH W_MSG.
        WL_ERRO = 'X'.
      ENDIF.
    ENDLOOP.
  ELSE.
    SELECT SINGLE *
     FROM T001
     INTO WL_T001
     WHERE BUKRS = WG_ZGLT034-BUKRS.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E04 '' WG_ZGLT034-BUKRS INTO W_MSG SEPARATED BY SPACE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH W_MSG.
      WL_ERRO = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VERIFICA_ERROS
