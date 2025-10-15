*&---------------------------------------------------------------------*
*& Report  ZPMR0034
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0034.

DEFINE MC_PREENCHE_FIELDCAT.

  clear st_fieldcat.
  st_fieldcat-fieldname     = &1.
  st_fieldcat-datatype      = &2.
  st_fieldcat-reptext_ddic  = &3.
  st_fieldcat-outputlen     = &4.
  st_fieldcat-icon          = &5.
  st_fieldcat-key           = &6.
  st_fieldcat-HOTSPOT       = &7.

  append st_fieldcat to it_fieldcat.

END-OF-DEFINITION.

DEFINE MC_PREENCHE_CLASS.

  vg_i = vg_i + 1.
  clear it_sort.
  it_sort-spos      = vg_i.
  it_sort-fieldname = &1.
  it_sort-group     = &2.
  it_sort-up        = &3.
  it_sort-subtot    = &4.
  append it_sort.

END-OF-DEFINITION.

DEFINE MC_PREENCHE_CABEC.

  clear st_header.
  st_header-typ  = &1.
  st_header-key  = &2.
  st_header-info = &3.
  append st_header to it_header.

END-OF-DEFINITION.

DATA:   V_TABIX     TYPE SY-TABIX        ,  " guardar o índice
        L_FLSTR     LIKE RIHIMRG-PYEAC,
*        l_flstr_aux LIKE rihimrg-pyeac   ,
        V_PERMISSAO TYPE C.

DATA: VG_I              TYPE I,
      ST_FIELDCAT       TYPE KKBLO_FIELDCAT,
      IT_FIELDCAT       TYPE KKBLO_T_FIELDCAT,
      ST_COLINFO        TYPE KKBLO_SPECIALCOL,
      ST_LAYOUT         TYPE KKBLO_LAYOUT,
      ST_PRINT          TYPE SLIS_PRINT_ALV,
      ST_HEADER         TYPE KKBLO_LISTHEADER,
      IT_FIELDCAT_ALV   TYPE SLIS_T_FIELDCAT_ALV,
      IT_SPECIAL_GROUPS TYPE SLIS_T_SP_GROUP_ALV,
      IT_LAYOUT_ALV     TYPE SLIS_LAYOUT_ALV,
      IT_HEADER         TYPE KKBLO_T_LISTHEADER,
      IT_SORT           TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      ST_GRID_SETTINGS  TYPE LVC_S_GLAY,
      DTVINC_I(10)      TYPE C,
      DTVINC_F(10)      TYPE C,
      SDYDO_TEXT_ELEMENT(255),
      P_TEXT_TABLE      TYPE SDYDO_TEXT_TABLE.
INCLUDE <ICON>.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
DATA: O_ALV       TYPE REF TO CL_GUI_ALV_GRID,
      O_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      DEF_VARIANT TYPE DISVARIANT,
      VARIANT     TYPE DISVARIANT,
      V_SAVE(1)   TYPE C VALUE 'A'.

DATA: V_LISTHEADER TYPE SLIS_LISTHEADER,    "Cabeçalho
      V_LAYOUT     TYPE SLIS_LAYOUT_ALV,    "layout para saída
      V_PRINT      TYPE SLIS_PRINT_ALV,     "Ctrl de impressão
      V_VARIANTE   LIKE DISVARIANT.         "Variante de exibiçã

DATA: W_EVENTS     TYPE SLIS_T_EVENT WITH HEADER LINE, " Work-area eventos
      W_ES_VARIANT LIKE DISVARIANT,      " variants.
      W_LAYOUT     TYPE SLIS_LAYOUT_ALV,      " Para layout tipo zebra
      I_EVENTS     TYPE SLIS_T_EVENT,         " Para os eventos
      V_REPID      LIKE SY-REPID,
      V_FLAG,
      V_LNUMT(25)  TYPE C.


CONSTANTS: CC_A        TYPE C VALUE 'A',
           CC_X        TYPE C VALUE 'X',
           CC_I        TYPE C VALUE 'I',
           CC_1        TYPE C VALUE '1',
           CC_2        TYPE C VALUE '2',
           CC_SPRAS(2) TYPE C VALUE 'PT',
           CC_M        TYPE C VALUE 'M'.
* Definições para ALV
TYPE-POOLS: KKBLO.



TABLES: AUFK, VIAUFKST.

TYPES:
  BEGIN OF TY_ZTPARAM,
    PARAM TYPE ZTPARAM-PARAM,
    CONST TYPE ZTPARAM-CONST,
    ZVAL  TYPE ZTPARAM-ZVAL,
  END OF TY_ZTPARAM,

  BEGIN OF TY_ZVAL,
    PARAM TYPE ZTPARAM-PARAM,
    CONST TYPE AUFK-WERKS,
    ZVAL  TYPE AUFK-AUART,
  END OF TY_ZVAL,

  BEGIN OF TY_AUFK,
    BUKRS TYPE AUFK-BUKRS,
    WERKS TYPE AUFK-WERKS,
    KOSTL TYPE AUFK-KOSTL,
    AUFNR TYPE AUFK-AUFNR,
    OBJNR TYPE AUFK-OBJNR,
    AUART TYPE AUFK-AUART,
  END OF TY_AUFK,

  BEGIN OF TY_VIAUFKST,
    EQUNR TYPE VIAUFKST-EQUNR,
    TPLNR TYPE VIAUFKST-TPLNR,
    AUFNR TYPE VIAUFKST-AUFNR,
  END OF TY_VIAUFKST,

  BEGIN OF TY_JEST,
    OBJNR TYPE JEST-OBJNR,
    STAT  TYPE JEST-STAT,
    "   INACT TYPE JEST-INACT,
    TXT04 TYPE TJ02T-TXT04,
  END OF TY_JEST,

  BEGIN OF TY_TJ02T,
    ISTAT TYPE TJ02T-ISTAT,
    TXT04 TYPE TJ02T-TXT04,
    " SPRAS TYPE TJ02T-SPRAS,
  END OF TY_TJ02T,

  BEGIN OF TY_SAIDA,
    BUKRS        TYPE AUFK-BUKRS,
    WERKS        TYPE AUFK-WERKS,
    KOSTL        TYPE AUFK-KOSTL,
    CORRETIVA    TYPE I,
    PREVENTIVA   TYPE I,
    PREDITIVA    TYPE I,
    LUBRIFICACAO TYPE I,
    PROGRAMA     TYPE I,
    MELHORIA     TYPE I,
    TOTAL        TYPE I,
  END OF TY_SAIDA.



DATA: T_ZTPARAM  TYPE  TABLE OF ZTPARAM WITH HEADER LINE,
      T_AUFK     TYPE  TABLE OF TY_AUFK WITH HEADER LINE,
      T_VIAUFKST TYPE TABLE OF TY_VIAUFKST,
      T_JEST     TYPE TABLE OF TY_JEST,
      T_TJ02T    TYPE TABLE OF TY_TJ02T,
      T_SAIDA    TYPE TABLE OF TY_SAIDA,
      T_ZVAL     TYPE TABLE OF TY_ZVAL WITH HEADER LINE,
      W_ZTPARAM  TYPE TY_ZTPARAM,
      W_AUFK     TYPE TY_AUFK,
      W_VIAUFKST TYPE TY_VIAUFKST,
      W_JEST     TYPE TY_JEST,
      W_TJ02T    TYPE TY_TJ02T,
      W_TSAIDA   TYPE TY_SAIDA.

CONSTANTS:
  LC_OR_PREDIT(15)  VALUE 'OR_PREDIT',
  LC_OR_CORRET(15)  VALUE 'OR_CORRET',
  LC_OR_PREVENT(15) VALUE 'OR_PREVENT',
  LC_OR_LUBRIF(15)  VALUE 'OR_LUBRIF',
  LC_OR_PROGRA(15)  VALUE 'OR_PROGRA',
  LC_OR_MELHOR(15)  VALUE 'OR_MELHOR'.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE .

SELECTION-SCREEN COMMENT 1(08) TEXT-004.
SELECTION-SCREEN POSITION 10.
PARAMETERS: P_LIB AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN POSITION 12.
SELECTION-SCREEN COMMENT 15(09) TEXT-005.
PARAMETERS: P_ENC AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 31(08) TEXT-006.
PARAMETERS: P_ABERT AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN POSITION 36.
SELECTION-SCREEN COMMENT 45(13) TEXT-007.
PARAMETERS: P_NEXEC AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B2.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS FOR AUFK-BUKRS OBLIGATORY,
                P_WERKS FOR AUFK-WERKS,
                P_AUFNR FOR AUFK-AUFNR,
                P_AUART FOR AUFK-AUART,
                P_KOSTL FOR AUFK-KOSTL,
                P_EQUNR FOR VIAUFKST-EQUNR,
                P_TPLNR FOR VIAUFKST-TPLNR,
                P_ERDAT FOR AUFK-ERDAT OBLIGATORY.
SELECTION-SCREEN: END  OF BLOCK B1.


START-OF-SELECTION.
  PERFORM: SELECAO,
           TRATATIVA.

  IF T_SAIDA[] IS NOT INITIAL.
    PERFORM F_CABECALHO.
    PERFORM F_CATALOGO.
    PERFORM F_CLASSIFICACAO.
    PERFORM F_LAYOUT.
    PERFORM EVENTOS.
    PERFORM F_RELATORIO TABLES T_SAIDA.
  ELSE.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

*  IF T_SAIDA IS NOT INITIAL.
*    PERFORM CALLSCREEN.
*
*    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM SELECAO.

  FREE: T_ZVAL[].
  SELECT *
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE T_ZTPARAM
    WHERE PARAM IN (LC_OR_PREDIT, LC_OR_CORRET, LC_OR_PREVENT, LC_OR_PROGRA, LC_OR_MELHOR, LC_OR_LUBRIF).
  IF NOT T_ZTPARAM[] IS INITIAL.
    LOOP AT T_ZTPARAM.
      T_ZVAL-PARAM = T_ZTPARAM-PARAM.
      T_ZVAL-ZVAL =  T_ZTPARAM-ZVAL.
      T_ZVAL-CONST = T_ZTPARAM-CONST.
      APPEND T_ZVAL.
    ENDLOOP.
  ENDIF.

  SELECT * FROM AUFK
    INTO CORRESPONDING FIELDS OF TABLE T_AUFK
    FOR ALL ENTRIES IN T_ZVAL
    WHERE BUKRS IN P_BUKRS
      AND WERKS IN P_WERKS
      AND AUFNR IN P_AUFNR
      AND AUART IN P_AUART
      AND KOSTL IN P_KOSTL
      AND ERDAT IN P_ERDAT
      AND AUART EQ T_ZVAL-ZVAL
      AND WERKS EQ T_ZVAL-CONST.


  CHECK T_AUFK[] IS NOT INITIAL. " VERIFICANDO SE A TABELA EXISTE REGISTROS


  SELECT * FROM VIAUFKST
    INTO CORRESPONDING FIELDS OF  TABLE T_VIAUFKST
    FOR ALL ENTRIES IN T_AUFK
    WHERE AUFNR EQ T_AUFK-AUFNR
    AND   EQUNR IN P_EQUNR
    AND   TPLNR IN P_TPLNR.


*  SELECT * FROM JEST
*    INTO CORRESPONDING FIELDS OF TABLE T_JEST
*    FOR ALL ENTRIES IN T_AUFK
*    WHERE OBJNR EQ T_AUFK-OBJNR.
*
*  SELECT * FROM TJ02T
*    INTO CORRESPONDING FIELDS OF TABLE T_TJ02T
*    FOR ALL ENTRIES IN T_JEST
*    WHERE ISTAT EQ T_JEST-STAT.

  IF P_LIB IS NOT INITIAL AND
     P_ENC IS NOT INITIAL AND
     P_ABERT IS NOT INITIAL AND
     P_NEXEC IS NOT INITIAL.

    SELECT JEST~OBJNR JEST~STAT TJ02T~TXT04
      INTO TABLE T_JEST
      FROM JEST
        INNER JOIN TJ02T ON TJ02T~ISTAT = JEST~STAT
          FOR ALL ENTRIES IN T_AUFK
            WHERE JEST~OBJNR  EQ T_AUFK-OBJNR
              AND JEST~INACT  EQ ABAP_FALSE   " Somente os ativos
              AND TJ02T~SPRAS EQ SY-LANGU.
  ELSE.


    SELECT JEST~OBJNR JEST~STAT TJ02T~TXT04
     INTO TABLE T_JEST
     FROM JEST
       INNER JOIN TJ02T ON TJ02T~ISTAT = JEST~STAT
         FOR ALL ENTRIES IN T_AUFK
           WHERE JEST~OBJNR  EQ T_AUFK-OBJNR
             AND JEST~INACT  EQ ABAP_FALSE   " Somente os ativos
             AND TJ02T~SPRAS EQ SY-LANGU.


    "Status Liberada e Encerrada
    IF P_LIB  = 'X' AND
       P_ENC  = 'X' AND
       P_ABERT IS INITIAL AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE  TXT04 NE 'NEXE'
                     AND   TXT04 NE 'ABER'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Liberada, Aberta e Encerrada
    IF P_LIB = 'X' AND
       P_ABERT = 'X' AND
       P_ENC = 'X' AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE  TXT04 NE 'NEXE'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.

*    Deleta as ordens encerradas e abertas.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Liberada e Aberta
    IF P_LIB IS NOT INITIAL AND
       P_ABERT IS NOT INITIAL AND
       P_ENC IS INITIAL AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'ENCE'
                     AND  TXT04 NE 'NEXE'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "Status Liberada e Não Executada
    IF P_LIB IS NOT INITIAL AND
       P_NEXEC IS NOT INITIAL AND
       P_ABERT IS INITIAL AND
       P_ENC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'ENCE'
                     AND  TXT04 NE 'ABER'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Encerrada e Aberta
    IF P_ENC IS NOT INITIAL AND
       P_ABERT IS NOT INITIAL AND
       P_LIB IS INITIAL AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'NEXE'
                     AND  TXT04 NE 'LIB'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Encerrada e Não Executada
    IF P_ENC IS NOT INITIAL AND
       P_NEXEC IS NOT INITIAL AND
       P_LIB IS INITIAL AND
       P_ABERT IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'ABER'
                     AND  TXT04 NE 'LIB'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Aberta e Encerrada
    IF P_ENC IS NOT INITIAL AND
       P_ABERT IS NOT INITIAL AND
       P_LIB IS INITIAL AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'NEXE'
                     AND  TXT04 NE 'LIB'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Aberta e Não Executada
    IF P_NEXEC IS NOT INITIAL AND
       P_ABERT IS NOT INITIAL AND
       P_LIB IS INITIAL AND
       P_ENC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'LIB'
                     AND  TXT04 NE 'ENC'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Status Liberado, Aberto e Não Executado
    IF P_ENC IS INITIAL AND
       P_ABERT IS NOT INITIAL AND
       P_NEXEC IS NOT INITIAL AND
       P_LIB IS NOT INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND  TXT04 NE 'ENCE'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.

    ENDIF.


    IF P_LIB IS NOT INITIAL AND
       P_ABERT IS INITIAL AND
       P_ENC IS INITIAL AND
       P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND   TXT04 NE 'NEXE'
                     AND   TXT04 NE 'ENCE'
                     AND   TXT04 NE 'ABER'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.

*    Deleta as ordens encerradas e abertas.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.


    ELSEIF
      P_ENC IS NOT INITIAL AND
      P_LIB IS INITIAL AND
      P_ABERT IS INITIAL AND
      P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'LIB'
                     AND   TXT04 NE 'ABER'
                     AND   TXT04 NE 'NEXE'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.

*    Deleta as ordens encerradas e abertas.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.



    ELSEIF
      P_ABERT IS NOT INITIAL AND
      P_LIB IS INITIAL AND
      P_ENC IS INITIAL AND
      P_NEXEC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'LIB'
                     AND   TXT04 NE 'NEXE'
                     AND   TXT04 NE 'ENCE'
                     AND   TXT04 NE 'ENTE'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.

*    Deleta as ordens encerradas e abertas.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.

    ELSEIF
      P_NEXEC IS NOT INITIAL AND
      P_LIB IS INITIAL AND
      P_ABERT IS INITIAL AND
      P_ENC IS INITIAL.
      DELETE T_JEST WHERE TXT04 NE 'ENTE'
                     AND   TXT04 NE 'LIB'
                     AND   TXT04 NE 'ENCE'
                     AND   TXT04 NE 'ABER'.
      LOOP AT T_AUFK.
        V_TABIX = SY-TABIX.

*    Deleta as ordens encerradas e abertas.
        IF LINE_EXISTS( T_JEST[ OBJNR = T_AUFK-OBJNR ] ).
          DELETE T_AUFK INDEX V_TABIX. CONTINUE.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRATATIVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRATATIVA.

  LOOP AT  T_AUFK  INTO W_AUFK.
    W_TSAIDA-BUKRS = W_AUFK-BUKRS.
    W_TSAIDA-WERKS = W_AUFK-WERKS.
    W_TSAIDA-KOSTL = W_AUFK-KOSTL.
    READ TABLE T_ZVAL WITH  KEY ZVAL = W_AUFK-AUART
                                CONST = W_AUFK-WERKS.

    IF SY-SUBRC = 0. " VERIFICA SE TEM REGISTRO
      CASE T_ZVAL-PARAM.
        WHEN LC_OR_PREDIT.
          ADD 1 TO W_TSAIDA-PREDITIVA.
        WHEN LC_OR_CORRET.
          ADD 1 TO W_TSAIDA-CORRETIVA.
        WHEN LC_OR_PREVENT.
          ADD 1 TO W_TSAIDA-PREVENTIVA.
        WHEN LC_OR_LUBRIF.
          ADD 1 TO W_TSAIDA-LUBRIFICACAO.
        WHEN LC_OR_PROGRA.
          ADD 1 TO W_TSAIDA-LUBRIFICACAO.
        WHEN LC_OR_PROGRA.
          ADD 1 TO W_TSAIDA-PROGRAMA.
        WHEN LC_OR_MELHOR.
          ADD 1 TO W_TSAIDA-MELHORIA.
      ENDCASE.

    ENDIF.
    COLLECT W_TSAIDA INTO T_SAIDA.   "PASSA OS DADOS PARA TABELA TEMPORARIA E SOMA
    CLEAR W_TSAIDA. " LIMPA A TABELA  PARA REALIZAR O LOOP
    CLEAR T_ZVAL.  " LIMPA A TABELA  PARA REALIZAR O LOOP

  ENDLOOP.

  LOOP AT T_SAIDA INTO W_TSAIDA.
    W_TSAIDA-TOTAL = ( W_TSAIDA-PREVENTIVA + W_TSAIDA-CORRETIVA + W_TSAIDA-PREDITIVA + W_TSAIDA-LUBRIFICACAO + W_TSAIDA-PROGRAMA + W_TSAIDA-MELHORIA ).
    MODIFY T_SAIDA FROM W_TSAIDA.
    CLEAR W_TSAIDA.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALLSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALLSCREEN .

  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'T001'.
  SET TITLEBAR 'T002'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CABECALHO .
  DATA: L_CABEC(100)  TYPE C,
        L_CABEC1(100) TYPE C,
        L_CABEC2(100) TYPE C.

  MC_PREENCHE_CABEC: 'H' ' ' 'Desempenho de manutenção',  " Título do relatório
                     'S' ' ' ' '.                 " Para não exibir barra de rolagem

  IF P_WERKS IS NOT INITIAL.
* Centro
    IF ( P_WERKS-LOW IS NOT INITIAL ) AND ( P_WERKS-HIGH IS NOT INITIAL ).
      CONCATENATE P_WERKS-LOW 'á' P_WERKS-HIGH INTO L_CABEC SEPARATED BY SPACE.
    ELSEIF ( P_WERKS-LOW IS NOT INITIAL ).
      L_CABEC = P_WERKS-LOW.
    ELSE.
*    L_CABEC = 'Todos'.
    ENDIF.
    MC_PREENCHE_CABEC:  'S' 'Centro                 ' L_CABEC.
  ENDIF.

  IF P_EQUNR IS NOT INITIAL.
* Equipamento
    IF ( P_EQUNR-LOW IS NOT INITIAL ) AND ( P_EQUNR-HIGH IS NOT INITIAL ).
      CONCATENATE P_EQUNR-LOW 'á' P_EQUNR-HIGH INTO L_CABEC SEPARATED BY SPACE.
    ELSEIF ( P_EQUNR-LOW IS NOT INITIAL ).
      L_CABEC = P_EQUNR-LOW.
    ELSE.
*    L_CABEC = 'Todos'.
    ENDIF.
    MC_PREENCHE_CABEC:  'S' 'Equipamento            ' L_CABEC.
  ENDIF.


  IF P_ERDAT IS NOT INITIAL.
* Periodo de seleção
    WRITE: P_ERDAT-LOW  TO L_CABEC1,
           P_ERDAT-HIGH TO L_CABEC2.
    CONCATENATE L_CABEC1 'á' L_CABEC2 INTO L_CABEC SEPARATED BY SPACE.
    MC_PREENCHE_CABEC:  'S' 'Período                  ' L_CABEC.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CATALOGO . "Configurando a grid com os dados

*  PERFORM F_LUPA USING 'Preenchendo ALV' SPACE.

  MC_PREENCHE_FIELDCAT:

"                                                      Tam  Sum ID
   'BUKRS'                  'CHAR' 'Empresa'                   '25' ' ' ' ' ' ',
   'WERKS'                  'CHAR' 'Centro'                    '18' ' ' ' ' ' ',
   'KOSTL'                  'CHAR' 'Centro Custo'              '40' ' ' ' ' ' ',
   'CORRETIVA'              'CHAR' 'Corretiva '                '10' ' ' ' ' ' ',
   'PREVENTIVA'             'CHAR' 'Preventiva'                '10' ' ' ' ' ' ',
   'PREDITIVA'              'CHAR' 'Preditiva'                 '18' ' ' ' ' ' ',
   'LUBRIFICACAO'           'CHAR' 'Lubrificação'              '15' ' ' ' ' ' ',
   'PROGRAMA'               'CHAR' 'Programa'                  '37' ' ' ' ' ' ',
   'MELHORIA'               'DATS' 'Melhoria'                  '13' ' ' ' ' ' ',
   'TOTAL'                  'DATS' 'Total'                     '10' ' ' ' ' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CLASSIFICACAO .
  CLEAR VG_I.
  REFRESH IT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LAYOUT .
  ST_LAYOUT-NO_ZEBRA            = ' '.
  ST_LAYOUT-GROUP_BUTTONS       = ' '.
  ST_LAYOUT-TOTALS_ONLY         = ' '.

* Otimizar colunas na tela
  ST_LAYOUT-COLWIDTH_OPTIMIZE   = 'X'.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      IT_FIELDCAT       = IT_FIELDCAT
      IS_LAYOUT         = ST_LAYOUT
    IMPORTING
      ET_FIELDCAT       = IT_FIELDCAT_ALV
      ES_LAYOUT         = IT_LAYOUT_ALV
      ET_SPECIAL_GROUPS = IT_SPECIAL_GROUPS.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EVENTOS .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = I_EVENTS.

  READ TABLE I_EVENTS
       WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
       INTO W_EVENTS.
  IF SY-SUBRC = 0.
    MOVE 'ENCABEZADO' TO W_EVENTS-FORM.
    MODIFY I_EVENTS FROM W_EVENTS INDEX SY-TABIX.
  ENDIF.

ENDFORM.

FORM F_RELATORIO TABLES PT_OUTTAB TYPE TABLE.

*  Não otimizar colunas no preview e nem na impressão
  ST_GRID_SETTINGS-NO_COLWOPT = CC_X.

* Não imprimir o relatório estatístico antes do relatório
  ST_PRINT-NO_PRINT_SELINFOS  = CC_X.
  ST_PRINT-NO_PRINT_LISTINFOS = CC_X.

  V_REPID = SY-REPID.
  CALL FUNCTION 'K_KKB_SAVE_MODE_GET'
    IMPORTING
      E_SAVE = V_SAVE.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPID
      IS_LAYOUT               = IT_LAYOUT_ALV
      IT_FIELDCAT             = IT_FIELDCAT_ALV
      IT_SPECIAL_GROUPS       = IT_SPECIAL_GROUPS
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     i_callback_pf_status_set = 'PF_STATUS_SET'
      I_GRID_SETTINGS         = ST_GRID_SETTINGS
      IT_SORT                 = IT_SORT[]
      I_DEFAULT               = CC_X
      IS_PRINT                = ST_PRINT
      I_SAVE                  = V_SAVE
      IS_VARIANT              = VARIANT
    TABLES
      T_OUTTAB                = PT_OUTTAB
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE CC_I NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

*  FREE: it_relatorio.

ENDFORM.


FORM ORGANIZA_CABECALHO.

  DTVINC_I = |{ P_ERDAT-LOW+6(2) }.{ P_ERDAT-LOW+4(2) }.{ P_ERDAT-LOW+0(4) }|.
  DTVINC_F = |{ P_ERDAT-HIGH+6(2) }.{ P_ERDAT-HIGH+4(2) }.{ P_ERDAT-HIGH+0(4) }|.

*  SDYDO_TEXT_ELEMENT = 'Relatório de Solicitação de Alteração de Preço de Frete: ____________________________________________________'.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  CLEAR: SDYDO_TEXT_ELEMENT.
*
  PERFORM FORMA_CABECALHO USING P_BUKRS P_BUKRS-OPTION  P_BUKRS-LOW  P_BUKRS-HIGH  TEXT-008.
  PERFORM FORMA_CABECALHO USING P_WERKS P_WERKS-OPTION  P_WERKS-LOW  P_WERKS-HIGH  TEXT-009.
  PERFORM FORMA_CABECALHO USING P_AUFNR P_AUFNR-OPTION  P_AUFNR-LOW  P_AUFNR-HIGH  TEXT-010.
  PERFORM FORMA_CABECALHO USING P_AUART P_AUART-OPTION  P_AUART-LOW  P_AUART-HIGH  TEXT-011.
  PERFORM FORMA_CABECALHO USING P_KOSTL P_KOSTL-OPTION  P_KOSTL-LOW  P_KOSTL-HIGH  TEXT-012.
  PERFORM FORMA_CABECALHO USING P_EQUNR P_EQUNR-OPTION  P_EQUNR-LOW  P_EQUNR-HIGH  TEXT-013.
  PERFORM FORMA_CABECALHO USING P_TPLNR P_TPLNR-OPTION  P_TPLNR-LOW  P_TPLNR-HIGH  TEXT-014.
  PERFORM FORMA_CABECALHO USING P_ERDAT P_ERDAT-OPTION  DTVINC_I     DTVINC_F      TEXT-015.


ENDFORM.




FORM FORMA_CABECALHO  USING    P_STATUS
                               P_STATUS_OPTION
                               P_STATUS_LOW
                               P_STATUS_HIGH
                               P_TEXT_006.

  IF P_STATUS IS NOT INITIAL.
    IF P_STATUS_OPTION NE 'EQ' AND P_STATUS_OPTION NE 'BT'.
      SDYDO_TEXT_ELEMENT = P_TEXT_006.
      EXIT.
    ELSEIF P_STATUS_OPTION EQ 'BT'.
      SDYDO_TEXT_ELEMENT = | { P_TEXT_006 } { P_STATUS_LOW } - { P_STATUS_HIGH } |.
    ELSE.
      SDYDO_TEXT_ELEMENT = | { P_TEXT_006 } { P_STATUS_LOW } |.
    ENDIF.
    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
    CLEAR SDYDO_TEXT_ELEMENT.
*  ELSE.
*    SDYDO_TEXT_ELEMENT = P_TEXT_006.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*    CLEAR: SDYDO_TEXT_ELEMENT.
  ENDIF.

ENDFORM.

FORM TOP_OF_PAGE.                                           "#EC CALLED
* Para criar um logotipo, deve-se entrar na transação 0FPM002 e
* preencher:
* - Classe = PICTURES
* - Objeto = OT
* - Item   = Nome do ID da figura

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
      I_LOGO             = 'LOGO_NOVO'.

ENDFORM.                    "top_of_page
