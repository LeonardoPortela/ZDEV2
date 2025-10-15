*&---------------------------------------------------------------------*
*&  Include           ZIM02_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA: W_KOSTL TYPE ZIM01_SOL_AP_INV-KOSTL,
        W_ANO   TYPE ZIM01_SOL_AP_INV-ANO.



*  BREAK-POINT.
  CASE SY-TCODE.
    WHEN 'ZIM02'.

      W_TEXTOS = 'Seleção da Solicitação de Aprovação de Investimento'.
      IF ZIM01_SOL_AP_INV-KOSTL IS INITIAL.
        GET PARAMETER ID 'KOS' FIELD ZIM01_SOL_AP_INV-KOSTL.
      ENDIF.

      IF ZIM01_SOL_AP_INV-ANO IS INITIAL.
        GET PARAMETER ID 'GJR' FIELD ZIM01_SOL_AP_INV-ANO.
      ENDIF.
      W_PRI = 'X'.
      SET PF-STATUS '100'.
    WHEN 'ZIM05'.

      W_TEXTOS = 'Seleção da Solicitação Investimento - Class.Contábil'.
      IF ZIM01_SOL_AP_INV-KOSTL IS INITIAL.
        GET PARAMETER ID 'KOS' FIELD ZIM01_SOL_AP_INV-KOSTL.
      ENDIF.

      IF ZIM01_SOL_AP_INV-ANO IS INITIAL.
        GET PARAMETER ID 'GJR' FIELD ZIM01_SOL_AP_INV-ANO.
      ENDIF.

      W_SEG = 'X'.
      DATA FCODE TYPE TABLE OF SY-UCOMM.
      APPEND 'CRIA'  TO FCODE.
      APPEND 'DELE' TO FCODE.
      SET PF-STATUS '100' EXCLUDING  FCODE.

    WHEN OTHERS.
  ENDCASE.

  IF SY-TCODE = 'ZIM02'.
    SET TITLEBAR '100'.
  ELSE.
    SET TITLEBAR '110'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'GRUND'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
  SET PF-STATUS '110'.
  IF SY-TCODE = 'ZIM02'.
    SET TITLEBAR '100'.
  ELSE.
    SET TITLEBAR '110'.
  ENDIF.
  LOOP AT SCREEN.
*BREAK-POINT.
  ENDLOOP.

ENDMODULE.                 " STATUS_0110  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INV'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_INV_CHANGE_TC_ATTR OUTPUT.

  DESCRIBE TABLE T_INV LINES TC_INV-LINES.
  " S_SCRATTR

ENDMODULE.                    "TC_INV_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_INV'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC_INV_GET_LINES OUTPUT.
  G_TC_INV_LINES = SY-LOOPC.

  DATA: W_TIPO(1).

ENDMODULE.                    "TC_INV_GET_LINES OUTPUT
*----------------------------------------------------------------------*
*  MODULE md_change_Screen
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MD_CHANGE_SCREEN OUTPUT.



*  BREAK-POINT.
  LOOP AT SCREEN.
    SCREEN-INPUT = 0.
*    IF tc_inv-current_line = 2.
    IF SCREEN-NAME = 'T_INV-FLAG'.
      SCREEN-INPUT = 1.
    ENDIF.
    IF W_UP = 'X'.

      CASE 'X'.
        WHEN W_PRI.
          IF T_INV-STATUS_CTA EQ '3' OR
             T_INV-STATUS_CTA EQ ''.
            IF SCREEN-GROUP4 = 'NV1'.
              SCREEN-INPUT = 1.
            ENDIF.
          ENDIF.

*          IF T_INV-FASE EQ SPACE.
*            IF SCREEN-GROUP1 = 'NV0'.
*              SCREEN-INPUT = 1.
*            ENDIF.
*          ENDIF.

        WHEN W_SEG.

          IF  T_INV-STATUS_APROV EQ '1'.
            IF SCREEN-GROUP3 = 'NVV'.
              SCREEN-INPUT = 1.
            ENDIF.
          ENDIF.

          IF ( T_INV-STATUS_APROV EQ '2' OR
               T_INV-STATUS_APROV EQ ''     ).
            IF SCREEN-GROUP4 = 'NV2' OR SCREEN-GROUP1 = 'NV5'.
              SCREEN-INPUT = 1.
            ENDIF.
          ENDIF.
        WHEN W_TER.
          IF T_INV-STATUS_CTA EQ '2'.
            IF SCREEN-GROUP4 = 'NV3'.
              SCREEN-INPUT = 1.
            ENDIF.
          ENDIF.
      ENDCASE.

*    ELSE.
* Início Alteração Ricardo Furst.
*      break abap.
*      IF sy-tcode = 'ZIM05' AND screen-group4 = 'NV2'.
*        screen-input = 1.
*      ENDIF.
* Fim Alteração Ricardo Furst.
**        CASE screen-name.,
**          WHEN 'T_INV-TX_USD' OR 'T_INV-TX_EUR' .
**BREAK-POINT.
*        screen-invisible = 0.
*        screen-active = 0.
*        screen-output = 0.
*        screen-input = 0.
**        ENDCASE.
*      ENDIF.
*

      IF T_INV-COD_GPO IS INITIAL.
        CLEAR T_INV-COD_ITEM.
        IF SCREEN-NAME = 'T_INV-COD_ITEM'.
*          screen-input = 0.
        ENDIF.
      ELSE.
        IF ( SCREEN-NAME = 'T_INV-DESCR_ITEM' OR
             SCREEN-NAME = 'T_INV-VLR_UNITARIO' )
          AND SCREEN-INPUT = 1.

          READ TABLE TG_10 WITH KEY COD_GPO = T_INV-COD_GPO BINARY SEARCH.
          IF TG_10-COD_LIB EQ 'X'.
            SCREEN-INPUT = 1.
          ELSE.
            READ TABLE TG_11 WITH KEY T_INV-COD_ITEM BINARY SEARCH.
            IF SY-SUBRC = 0.
              T_INV-DESCR_ITEM = TG_11-DESCR_ITEM.
            ENDIF.
            SCREEN-INPUT = 0.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.


    IF W_CRIA EQ 'X' AND W_UP = 'X'.
      CASE SCREEN-NAME.

        WHEN 'T_INV-OBJETIVO' OR
             'T_INV-MENGE' OR
             'T_INV-DT_INICIO' OR
             'T_INV-DT_FIM' OR
             'T_INV-IZWEK' OR
             'T_INV-FINALIDADE' OR
             'T_INV-DESCR_ITEM'
          .
          SCREEN-INPUT = 1.
        WHEN OTHERS.
      ENDCASE.

    ENDIF.
*ENDIF.
*    ENDIF.
    IF SCREEN-GROUP1 = 'NVT'.
      SCREEN-INPUT = 1.
    ENDIF.
    IF SCREEN-NAME = 'T_INV-COD_ITEM'.
      IF SY-TCODE = 'ZIM02'.
        READ TABLE TG_10 WITH KEY COD_GPO = T_INV-COD_GPO BINARY SEARCH.
        IF TG_10-ITEM_BLOQ EQ 'X'.
          SCREEN-INPUT = 0.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                    "md_change_Screen
*&---------------------------------------------------------------------*
*&      Module  MODIFICA_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFICA_CAMPOS OUTPUT.

  IF TG_10[] IS INITIAL.
    SELECT * FROM ZIM010 INTO TABLE TG_10.
    SORT TG_10 BY COD_GPO.
  ENDIF.

  IF TG_11[] IS INITIAL.
    SELECT * FROM ZIM011 INTO TABLE TG_11.
    SORT TG_11 BY COD_ITEM.
  ENDIF.


  IF NOT W_FIELD IS INITIAL.
    SET CURSOR FIELD W_FIELD LINE W_LINE.
  ENDIF.

  CLEAR: W_PLANEJADO    , W_APROVADO,
         W_PLANEJADO_USD, W_APROVADO_USD,
         W_PLANEJADO_EUR, W_APROVADO_EUR.

  LOOP AT T_INV.
*    IF sy-tcode = 'ZIM02'.
    IF T_INV-APROVADOR IS INITIAL.
      T_INV-APROVADOR  = ZIM02_SOL_AP_CTL-APROVADOR.
    ENDIF.
    MODIFY T_INV.
*    ENDIF.
    T_INV-ANO_FIM_EXEC = T_INV-DT_FIM(4).
    IF T_INV-STATUS_APROV IS INITIAL.
      CLEAR: T_INV-DT_APROVACAO,
             T_INV-APROVADOR.
    ENDIF.
    MODIFY T_INV INDEX SY-TABIX TRANSPORTING ANO_FIM_EXEC DT_APROVACAO APROVADOR.

    ADD T_INV-VLR_TOTAL TO W_PLANEJADO.
    ADD T_INV-VL_USD TO W_PLANEJADO_USD.
    ADD T_INV-VL_EUR TO W_PLANEJADO_EUR.
    IF T_INV-STATUS_APROV = '1'.
      ADD T_INV-VLR_TOTAL TO W_APROVADO.
      ADD T_INV-VL_USD TO W_APROVADO_USD.
      ADD T_INV-VL_EUR TO W_APROVADO_EUR.
    ENDIF.
  ENDLOOP.

  IF SY-TCODE = 'ZIM05'.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN 'TC_INV_INSERT'.
          SCREEN-INPUT = 0.
        WHEN 'TC_INV_DELETE'.
          SCREEN-INPUT = 0.
*        WHEN OTHERS.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  LOOP AT TC_INV-COLS INTO COLS_WA.
**Para ajustar a coluna na posicao 16 todas as colunas posteriores devem ter sua
**posicao aumentada em 1.
*    IF cols_wa-index > 15 .
*      cols_wa-index = cols_wa-index + 1.
*    ENDIF.
**Modificar a posição da coluna de ano fim para 16ªcoluna
*    IF cols_wa-screen-name = 'T_INV-ANO_FIM_EXEC'.
*      cols_wa-index = 16.
*    ENDIF.
    COLS_WA-INDEX = SY-TABIX.
    IF SY-TCODE = 'ZIM02'.
      COLS_WA-INVISIBLE = 0.
      CASE COLS_WA-SCREEN-NAME.
        WHEN 'T_INV-TX_USD' OR 'T_INV-TX_EUR' OR
             'T_INV-VL_USD' OR 'T_INV-VL_EUR' OR
             'T_INV-MOEDA'  OR 'T_INV-ANO_FIM_EXEC' OR
             'T_INV-KNTTP'  OR 'T_INV-KNTTX' OR
             'T_INV-SAKNR'  OR 'T_INV-TXT20'.
          COLS_WA-INVISIBLE = 1.
        WHEN 'T_INV-ANO_FIM_EXEC'.
          COLS_WA-INVISIBLE = 0.
          COLS_WA-SCREEN-INPUT = 0.
          COLS_WA-SCREEN-ACTIVE = 1.
        WHEN 'T_INV-DT_FIM'.
          COLS_WA-INVISIBLE = 0.
          COLS_WA-SCREEN-INPUT = 1.
          COLS_WA-SCREEN-ACTIVE = 1.
          COLS_WA-SCREEN-GROUP4 = 'NV1'.

      ENDCASE.
    ENDIF.
    IF SY-TCODE = 'ZIM05'.
      CASE COLS_WA-SCREEN-NAME.
        WHEN 'T_INV-ANO_FIM_EXEC'.
          COLS_WA-INVISIBLE = 0.
          COLS_WA-SCREEN-INPUT = 0.
        WHEN 'T_INV-DT_FIM'.
          COLS_WA-SCREEN-INPUT = 1.
          COLS_WA-SCREEN-ACTIVE = 1.
          COLS_WA-SCREEN-INVISIBLE = 0.
          COLS_WA-SCREEN-GROUP4 = 'NV2'.
      ENDCASE.
    ENDIF.
    MODIFY TC_INV-COLS FROM COLS_WA.
*  ENDIF.
  ENDLOOP.

  IF W_UP = 'X' AND SY-TCODE = 'ZIM02' AND V_APPEND IS INITIAL.
    V_APPEND = 'X'.
    DO 10 TIMES.
      APPEND INITIAL LINE TO T_INV.
    ENDDO.
  ENDIF.
ENDMODULE.                 " MODIFICA_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCAL_COD_GPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCAL_COD_GPO INPUT.
*  DATA: w_cursor1(10).
*  GET CURSOR LINE w_cursor1.
  DATA: BEGIN OF T_10 OCCURS 0.
          INCLUDE STRUCTURE ZIM010.
  DATA: END OF T_10.
  DATA: L_PVALKEY TYPE DDSHPVKEY.
  DATA: L_T_RETURN TYPE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

*  DATA: BEGIN OF t_fieldtab1 OCCURS 3.
*          INCLUDE STRUCTURE dynpread.
*  DATA: END OF t_fieldtab1.


  TYPES: BEGIN OF TY_GRUPO,
           DESCR_GRUPO TYPE ZIM010-DESCR_GRUPO,
           COD_GPO     TYPE ZIM010-COD_GPO,
         END OF TY_GRUPO.

  DATA: T_GRUPO  TYPE TY_GRUPO OCCURS 0 WITH HEADER LINE.

  SELECT * FROM ZIM010 INTO TABLE T_10
    WHERE STATUS_BLOQ = SPACE.

  CLEAR T_GRUPO. REFRESH T_GRUPO.

  LOOP AT T_10.
    MOVE-CORRESPONDING T_10 TO T_GRUPO.
    APPEND T_GRUPO. CLEAR T_GRUPO.
  ENDLOOP.

  CONCATENATE SY-REPID SY-DYNNR INTO L_PVALKEY  "Note 1038069
  SEPARATED BY '_'.
*break-point.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'T_INV-COD_GPO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'COD_GPO'
      VALUE_ORG       = 'S'
      DISPLAY         = ' '
    TABLES
      VALUE_TAB       = T_GRUPO[] "tg_tipop
      RETURN_TAB      = L_T_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

*
*  IF NOT l_t_return[] IS INITIAL..
*    READ TABLE l_t_return INDEX 1.
*
*    READ TABLE t_10 WITH KEY cod_gpo = l_t_return-fieldval.
*
*
*    MOVE: 'T_INV-COD_GPO'    TO t_fieldtab1-fieldname,
*          t_10-cod_gpo       TO t_fieldtab1-fieldvalue.
**          w_cursor1            TO t_fieldtab1-stepl.
*    APPEND t_fieldtab1.
*  ENDIF.
*
*
*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*    EXPORTING
*      dyname               = sy-repid
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = t_fieldtab1
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      undefind_error       = 7
*      OTHERS               = 8.

ENDMODULE.                 " BUSCAL_COD_GPO  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCAL_COD_ITEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BUSCAL_COD_ITEM INPUT.

  DATA: W_CURSOR(10).
  GET CURSOR LINE W_CURSOR.

  DATA: BEGIN OF T_11 OCCURS 0.
          INCLUDE STRUCTURE ZIM011.
  DATA: END OF T_11.

  DATA: BEGIN OF T_FIELDTAB OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF T_FIELDTAB.
  DATA: WL_CHECK TYPE ZIM010-COD_LIB.
  CLEAR: T_FIELDTAB, WL_CHECK.
  REFRESH T_FIELDTAB.

  TYPES: BEGIN OF TY_ITEM,
           DESCR_ITEM(60), " TYPE ZIM011-DESCR_ITEM,
           COD_ITEM   TYPE ZIM011-COD_ITEM,
*         cod_gpo     TYPE zim011-cod_gpo,
*         descr_grupo TYPE zim011-descr_grupo,
           PRECO_ITEM TYPE ZIM011-PRECO_ITEM,
*         status_bloq TYPE zim011-status_bloq,
*         status_cta  TYPE zim011-status_cta,
*         knttp       TYPE zim011-knttp,
*         knttx       TYPE zim011-knttx,
*         observacoes TYPE zim011-observacoes,
*         saknr       TYPE zim011-saknr,
*         txt20       TYPE zim011-txt20,
*         juncao(132),
         END OF TY_ITEM.


*  TYPES: ty_item TYPE zim011.
  DATA: T_ITEM  TYPE TY_ITEM OCCURS 0 WITH HEADER LINE.


  READ TABLE T_INV INDEX W_CURSOR. " posicionando na linha errada, sempre dar o READ
  "IF sy-subrc NE 0 OR t_inv-cod_gpo IS INITIAL.
  DATA: SET_DISP_FIELD LIKE T130R-FNAME.
  DATA: SET_DISP_LINE  LIKE SY-TABIX.
  DATA: BEGIN OF DYNPFIELDS OCCURS 0. "Hilfsstruktur zum auslesen des akt.
          INCLUDE STRUCTURE DYNPREAD. "Feldwertes vom Dynpro bei >F4<
  DATA: END OF   DYNPFIELDS.


  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  GET CURSOR FIELD SET_DISP_FIELD LINE SET_DISP_LINE.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'T_INV-COD_GPO'.
  DYNPFIELDS-STEPL     = SET_DISP_LINE.
  APPEND DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = SY-REPID
      DYNUMB     = SY-DYNNR
    TABLES
      DYNPFIELDS = DYNPFIELDS
    EXCEPTIONS
      OTHERS     = 01.

  READ TABLE DYNPFIELDS INDEX 1.
  T_INV-COD_GPO = DYNPFIELDS-FIELDVALUE.
  "ENDIF.
  IF T_INV-COD_GPO IS INITIAL.

  ENDIF.

  SELECT * FROM ZIM011 INTO TABLE T_11
    WHERE GJAHR   = ZIM01_SOL_AP_INV-ANO AND
          COD_GPO = T_INV-COD_GPO
      AND STATUS_BLOQ EQ SPACE.

  CLEAR: T_ITEM. REFRESH T_ITEM.

  LOOP AT T_11.
    MOVE-CORRESPONDING T_11 TO T_ITEM.
*    CONCATENATE t_11-cod_item t_11-cod_gpo INTO t_item-juncao SEPARATED BY '|'.
    APPEND T_ITEM.
  ENDLOOP.

*loop at

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'COD_ITEM'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'T_INV-COD_ITEM'
      VALUE_ORG       = 'S'
      DISPLAY         = ' '
      MULTIPLE_CHOICE = ' '
    TABLES
      VALUE_TAB       = T_ITEM[]
      RETURN_TAB      = L_T_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  IF NOT L_T_RETURN[] IS INITIAL..
    READ TABLE L_T_RETURN INDEX 1.

    READ TABLE T_11 WITH KEY COD_ITEM = L_T_RETURN-FIELDVAL
                             COD_GPO  = T_INV-COD_GPO.


    MOVE: 'T_INV-COD_ITEM'    TO T_FIELDTAB-FIELDNAME,
          T_11-COD_ITEM       TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR            TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.

    MOVE: 'T_INV-DESCR_ITEM'    TO T_FIELDTAB-FIELDNAME,
          T_11-DESCR_ITEM       TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR              TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-STATUS_CTA'    TO T_FIELDTAB-FIELDNAME,
          T_11-STATUS_CTA       TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR              TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-KNTTP'         TO T_FIELDTAB-FIELDNAME,
          T_11-KNTTP            TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR              TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-KNTTX'         TO T_FIELDTAB-FIELDNAME,
          T_11-KNTTX            TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR              TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-OBSERVACOES'   TO T_FIELDTAB-FIELDNAME,
          T_11-OBSERVACOES      TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR              TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-SAKNR'    TO T_FIELDTAB-FIELDNAME,
          T_11-SAKNR       TO T_FIELDTAB-FIELDVALUE,
          W_CURSOR         TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.


    MOVE: 'T_INV-TXT20'    TO T_FIELDTAB-FIELDNAME,
           T_11-TXT20      TO T_FIELDTAB-FIELDVALUE,
           W_CURSOR        TO T_FIELDTAB-STEPL.
    APPEND T_FIELDTAB.

** Igor Vilela
*    SELECT SINGLE cod_lib FROM zim010 INTO wl_check
*      WHERE cod_gpo = t_inv-cod_gpo.
*    IF NOT wl_check IS INITIAL.
*      CLEAR t_11-preco_item.
*    ENDIF.
** Igor Vilela

    MOVE: 'T_INV-VLR_UNITARIO' TO T_FIELDTAB-FIELDNAME,
*           t_11-preco_item   TO t_fieldtab-fieldvalue,
           W_CURSOR          TO T_FIELDTAB-STEPL.
    WRITE T_11-PRECO_ITEM   TO T_FIELDTAB-FIELDVALUE.
    SHIFT T_FIELDTAB-FIELDVALUE LEFT DELETING LEADING SPACE.
    APPEND T_FIELDTAB.


    IF W_UP IS INITIAL.
      REFRESH T_FIELDTAB. CLEAR T_FIELDTAB.
    ENDIF.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME               = SY-REPID
        DYNUMB               = SY-DYNNR
      TABLES
        DYNPFIELDS           = T_FIELDTAB
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        UNDEFIND_ERROR       = 7
        OTHERS               = 8.

  ENDIF.
ENDMODULE.                 " BUSCAL_COD_ITEM  INPUT
