*&---------------------------------------------------------------------*
*& Report  ZAA06
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZAA06.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ANLP, ANLA.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES: BEGIN OF TY_ZIB_CONTABIL.
        INCLUDE STRUCTURE ZIB_CONTABIL.
TYPES:  MARK TYPE C,
        END OF TY_ZIB_CONTABIL.

TYPES:

  " Valores do período do imobilizado
  BEGIN OF TY_ANLP,
    BUKRS   TYPE ANLP-BUKRS,
    GJAHR   TYPE ANLP-GJAHR,
    PERAF   TYPE ANLP-PERAF,
    AFABER  TYPE ANLP-AFABER,
    ANLN1   TYPE ANLP-ANLN1,
    ANLN2   TYPE ANLP-ANLN2,
    NAFAZ   TYPE ANLP-NAFAZ,
    KOSTL   TYPE ANLP-KOSTL,
    GSBER   TYPE ANLP-GSBER,
    BELNR   TYPE ANLP-BELNR,
    FLAG(1) TYPE C,
    OBJ_KEY TYPE ZIB_CONTABIL-OBJ_KEY,
  END OF TY_ANLP,

  " Atribuições de imobilizado com data valor
  BEGIN OF TY_ANLZ,
    BUKRS TYPE ANLZ-BUKRS,
    ANLN1 TYPE ANLZ-ANLN1,
    ANLN2 TYPE ANLZ-ANLN2,
    KOSTL TYPE ANLZ-KOSTL,
  END OF TY_ANLZ,
* Segmento do registro mestre do imobilizado
  BEGIN OF TY_ANLA,
    ANLN1 TYPE ANLA-ANLN1,
    ANLN2 TYPE ANLA-ANLN2,
    TXT50 TYPE ANLA-TXT50,
    TXA50 TYPE ANLA-TXA50,
    ANLKL TYPE ANLA-ANLKL,
  END OF TY_ANLA,

  BEGIN OF TY_ZIB_CONTABIL_ERR,
    OBJ_KEY TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
    NR_ITEM TYPE ZIB_CONTABIL_ERR-NR_ITEM,
    MESSAGE TYPE ZIB_CONTABIL_ERR-MESSAGE,
  END OF TY_ZIB_CONTABIL_ERR,

  BEGIN OF TY_ZIB_CONTABIL_CHV,
    OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
    BELNR   TYPE ZIB_CONTABIL_CHV-BELNR,
    BUKRS   TYPE ZIB_CONTABIL_CHV-BUKRS,
    GJAHR   TYPE ZIB_CONTABIL_CHV-GJAHR,
  END OF TY_ZIB_CONTABIL_CHV,

  BEGIN OF TY_SAIDA,
    ICON(4)  TYPE C,
    ANLN1    TYPE ANLP-ANLN1,
    ANLN2    TYPE ANLP-ANLN2,
    TXT50    TYPE ANLA-TXT50,
    TXA50    TYPE ANLA-TXA50,
    KOSTL    TYPE ANLP-KOSTL,
    GSBER    TYPE ANLP-GSBER,
    NAFAZR   TYPE ANLP-NAFAZ,
    NAFAZU   TYPE ANLP-NAFAZ,
    BELNR    TYPE ZIB_CONTABIL_CHV-BELNR,
    MESSAGE  TYPE ZIB_CONTABIL_ERR-MESSAGE,
    OBJ_KEY  TYPE ZIB_CONTABIL-OBJ_KEY,
    SEQITEMD TYPE ZIB_CONTABIL-SEQITEM,
    BUKRS    TYPE ZIB_CONTABIL_CHV-BUKRS,
    GJAHR    TYPE ZIB_CONTABIL_CHV-GJAHR,
  END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC           TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB       TYPE TABLE OF BDCMSGCOLL,

      IT_ANLP_AUX     TYPE TABLE OF TY_ANLP,
      IT_ANLP         TYPE TABLE OF TY_ANLP,
      IT_ANLP_U       TYPE TABLE OF TY_ANLP,
      IT_ANLA         TYPE TABLE OF TY_ANLA,
      IT_ANLZ         TYPE TABLE OF TY_ANLZ,
      IT_SAIDA        TYPE TABLE OF TY_SAIDA,
      IT_ZIB_CONTABIL TYPE TABLE OF TY_ZIB_CONTABIL,
      IT_COLOR        TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT             TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV              TYPE REF TO CL_GUI_ALV_GRID,
  WA_LAYOUT           TYPE LVC_S_LAYO,

  WA_ANLP             TYPE TY_ANLP,
  WA_ANLP_U           TYPE TY_ANLP,
  WA_ANLA             TYPE TY_ANLA,
  WA_ANLZ             TYPE TY_ANLZ,
  WA_SAIDA            TYPE TY_SAIDA,
  WA_ZIB_CONTABIL     TYPE TY_ZIB_CONTABIL,
  WA_ZIB_CONTABIL_CHV TYPE TY_ZIB_CONTABIL_CHV,
  WA_ZIB_CONTABIL_ERR TYPE TY_ZIB_CONTABIL_ERR,
  WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP      TYPE SLIS_T_LISTHEADER,
  XS_EVENTS  TYPE SLIS_ALV_EVENT,
  EVENTS     TYPE SLIS_T_EVENT,
  GD_LAYOUT  TYPE SLIS_LAYOUT_ALV,
  T_PRINT    TYPE SLIS_PRINT_ALV,
  V_REPORT   LIKE SY-REPID,
  T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
  VG_I       TYPE I.

DEFINE MC_PREENCHE_CLASS.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  T_SORT-UP        = &3.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_BUKRS TYPE ANLP-BUKRS OBLIGATORY,
            P_GJAHR TYPE ANLP-GJAHR OBLIGATORY,
            P_PERAF TYPE ANLP-PERAF OBLIGATORY.
SELECT-OPTIONS: P_ANLKL       FOR  ANLA-ANLKL.

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_INICIAR_VARIAVES, " Cabeçalho
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
*            F_CLASSIFICACAO,
*            F_BUILD_LAYOUT,
            F_IMPRIME_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .
  DATA:
    W_TEXTO1(10),
    W_TEXTO2(10),
    W_TEXTO3(40),

    W_EMPRESA_TEXTO(40),
    W_EXER_TEXTO(40),
    W_PER_TEXTO(40),

    EMPRESA             TYPE C LENGTH 50,
    EXERCICIO           TYPE C LENGTH 50,
    PERIODO             TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Ajuste Fiscal – Área 05 FI-AA'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.

  IF P_BUKRS IS NOT INITIAL.
    W_EMPRESA_TEXTO = 'Empresa    :'.
    CONCATENATE W_EMPRESA_TEXTO P_BUKRS  INTO EMPRESA SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.

  IF P_GJAHR IS NOT INITIAL.
    W_EXER_TEXTO = 'Exercício  :'.
    CONCATENATE W_EXER_TEXTO P_GJAHR  INTO EXERCICIO SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EXERCICIO.
  ENDIF.

  IF P_PERAF IS NOT INITIAL.
    W_EXER_TEXTO = 'Período Depreciação:'.
    CONCATENATE W_EXER_TEXTO P_PERAF  INTO PERIODO  SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' PERIODO.
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0466   text
*      -->P_PERIODO  text
*----------------------------------------------------------------------*


FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Preparando dados'.

  SELECT   BUKRS GJAHR PERAF AFABER ANLN1 ANLN2 NAFAZ KOSTL GSBER BELNR
    FROM ANLP
    INTO TABLE IT_ANLP_AUX
    WHERE BUKRS	 EQ P_BUKRS
    AND   GJAHR	 EQ P_GJAHR
    AND   PERAF	 EQ P_PERAF
    AND   AFABER EQ 5. "REAL

  SELECT   BUKRS GJAHR PERAF AFABER ANLN1 ANLN2 NAFAZ KOSTL GSBER BELNR
    FROM ANLP
    INTO TABLE IT_ANLP_U
    WHERE BUKRS	 EQ P_BUKRS
    AND   GJAHR	 EQ P_GJAHR
    AND   PERAF	 EQ P_PERAF
    AND   AFABER EQ 42 . "USD

* Se não encontrar R$ coloca 0,1.
  SORT IT_ANLP_AUX BY BUKRS GJAHR PERAF ANLN1 ANLN2.
  LOOP AT IT_ANLP_U INTO WA_ANLP.
    READ TABLE IT_ANLP_AUX INTO WA_ANLP WITH KEY BUKRS = WA_ANLP-BUKRS
                                                 GJAHR = WA_ANLP-GJAHR
                                                 PERAF = WA_ANLP-PERAF
                                                 ANLN1 = WA_ANLP-ANLN1
                                                 ANLN2 = WA_ANLP-ANLN2.
    IF SY-SUBRC NE 0.
      WA_ANLP-AFABER = 5.
      WA_ANLP-NAFAZ  = 1 / 100.
      APPEND WA_ANLP TO IT_ANLP_AUX.
    ENDIF.
  ENDLOOP.
  CHECK IT_ANLP_AUX[] IS NOT INITIAL.

  SELECT BUKRS ANLN1 ANLN2 KOSTL
    FROM ANLZ
    INTO TABLE IT_ANLZ
    FOR ALL ENTRIES IN IT_ANLP_AUX
    WHERE BUKRS = IT_ANLP_AUX-BUKRS
      AND ANLN1 = IT_ANLP_AUX-ANLN1
      AND ANLN2 = IT_ANLP_AUX-ANLN2
      AND ( BDATU GE SY-DATUM AND
            ADATU LE SY-DATUM ).

  SELECT ANLN1 ANLN2 TXT50 TXA50 ANLKL
    FROM ANLA
    INTO TABLE IT_ANLA
    FOR ALL ENTRIES IN IT_ANLP_AUX
    WHERE ANLN1 = IT_ANLP_AUX-ANLN1
    AND   ANLN2 = IT_ANLP_AUX-ANLN2.

  SORT IT_ANLA   BY ANLN1 ANLN2.
  LOOP AT IT_ANLP_AUX INTO WA_ANLP.
    READ TABLE IT_ANLA INTO WA_ANLA WITH KEY ANLN1 = WA_ANLP-ANLN1
                                            ANLN2 = WA_ANLP-ANLN2  BINARY SEARCH.

    IF ( ( NOT P_ANLKL IS INITIAL ) AND ( WA_ANLA-ANLKL IN P_ANLKL ) ).
      CONTINUE.
    ENDIF.
    APPEND WA_ANLP TO IT_ANLP.
  ENDLOOP.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Gerando relatório'.

  SORT IT_ANLP_U BY ANLN1 ANLN2.
  SORT IT_ANLZ   BY BUKRS ANLN1 ANLN2.
  SORT IT_ANLA   BY ANLN1 ANLN2.
  SORT IT_ANLP   BY ANLN1 ANLN2 AFABER.
  DATA: VOBJ_KEY LIKE ZIB_CONTABIL-OBJ_KEY.

*  DATA: VBLART TYPE ZIB_CONTABIL-BLART,
*        VCHECK TYPE CHAR1.

  LOOP AT IT_ANLP INTO WA_ANLP.
    READ TABLE IT_ANLA INTO WA_ANLA WITH KEY ANLN1 = WA_ANLP-ANLN1
                                             ANLN2 = WA_ANLP-ANLN2  BINARY SEARCH.

    SELECT SINGLE OBJ_KEY
    FROM ZIB_CONTABIL
    INTO WA_ZIB_CONTABIL_ERR
    WHERE ANLN1 = WA_ANLP-ANLN1
    AND   ANLN2 = WA_ANLP-ANLN2
    AND   GJAHR = WA_ANLP-GJAHR
    AND   MONAT = WA_ANLP-PERAF
    AND   BUKRS = WA_ANLP-BUKRS
    AND   BLART NE 'LM'.

    IF SY-SUBRC = 0.
      VOBJ_KEY = WA_ZIB_CONTABIL_ERR-OBJ_KEY.

*      SELECT SINGLE BLART
*        FROM ZIB_CONTABIL
*        INTO VBLART
*        WHERE OBJ_KEY EQ VOBJ_KEY
*          AND ANLN1 = WA_ANLP-ANLN1
*          AND ANLN2 = WA_ANLP-ANLN2
*          AND GJAHR = WA_ANLP-GJAHR
*          AND MONAT = WA_ANLP-PERAF
*          AND BUKRS = WA_ANLP-BUKRS.
*
*      IF SY-SUBRC IS INITIAL.
*        IF VBLART EQ 'LM'.
*          VCHECK = ABAP_TRUE.
*        ENDIF.
*      ENDIF.

    ELSE.
      CLEAR VOBJ_KEY .
    ENDIF.
    "
    WA_SAIDA-ANLN1   = WA_ANLP-ANLN1.
    WA_SAIDA-ANLN2   = WA_ANLP-ANLN2.
    WA_SAIDA-TXT50   = WA_ANLA-TXT50.
    WA_SAIDA-TXA50   = WA_ANLA-TXA50.
    WA_SAIDA-GSBER   = WA_ANLP-GSBER.
    WA_SAIDA-NAFAZR  = WA_ANLP-NAFAZ.
    WA_SAIDA-OBJ_KEY = VOBJ_KEY.

    READ TABLE IT_ANLZ  INTO WA_ANLZ  WITH KEY BUKRS = WA_ANLP-BUKRS
                                               ANLN1 = WA_ANLP-ANLN1
                                               ANLN2 = WA_ANLP-ANLN2 BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-KOSTL = WA_ANLZ-KOSTL.
    ELSE.
      WA_SAIDA-KOSTL = ''.
    ENDIF.

    READ TABLE IT_ANLP_U  INTO WA_ANLP_U  WITH KEY ANLN1 = WA_ANLP-ANLN1
                                                   ANLN2 = WA_ANLP-ANLN2 BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-NAFAZU = WA_ANLP_U-NAFAZ.
    ELSE.
      WA_SAIDA-NAFAZU = 0.
    ENDIF.

*    IF VBLART NE ABAP_TRUE.
    APPEND WA_SAIDA TO IT_SAIDA.
*    ENDIF.

    CLEAR: WA_SAIDA.", VCHECK.
  ENDLOOP.
  PERFORM ATUALIZA_FLAG.
  PERFORM ATUALIZA_PROCESSAMENTO.
ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Atualiza_flag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ATUALIZA_FLAG.
  DATA VG_TABIX TYPE SY-TABIX.
  SORT IT_ANLP   BY ANLN1 ANLN2.
  LOOP AT IT_SAIDA INTO WA_SAIDA.
    VG_TABIX = SY-TABIX.
    IF WA_SAIDA-OBJ_KEY IS NOT INITIAL.
      WA_SAIDA-ICON  = ICON_ACTIVITY.
      MODIFY IT_SAIDA INDEX VG_TABIX FROM WA_SAIDA TRANSPORTING ICON.
      READ TABLE IT_ANLP INTO WA_ANLP WITH KEY ANLN1 =  WA_SAIDA-ANLN1
                                             ANLN2 =  WA_SAIDA-ANLN2  BINARY SEARCH.
      WA_ANLP-FLAG = 'X'.
      MODIFY IT_ANLP INDEX SY-TABIX FROM WA_ANLP TRANSPORTING FLAG.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "Atualiza_flag

*&---------------------------------------------------------------------*
*&      FORM  ATUALIZA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM ATUALIZA_PROCESSAMENTO.
  DATA    VG_TABIX    TYPE SY-TABIX.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    VG_TABIX = SY-TABIX.

    SELECT  SINGLE OBJ_KEY NR_ITEM MESSAGE
     FROM ZIB_CONTABIL_ERR
     INTO WA_ZIB_CONTABIL_ERR
     WHERE OBJ_KEY = WA_SAIDA-OBJ_KEY.

    IF SY-SUBRC = 0.
      WA_SAIDA-MESSAGE = WA_ZIB_CONTABIL_ERR-MESSAGE.
      WA_SAIDA-ICON    = ICON_MESSAGE_ERROR.
    ENDIF.

    SELECT  SINGLE OBJ_KEY BELNR BUKRS GJAHR
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY = WA_SAIDA-OBJ_KEY.

    IF SY-SUBRC = 0.
      WA_SAIDA-BELNR   = WA_ZIB_CONTABIL_CHV-BELNR.
      WA_SAIDA-BUKRS   = WA_ZIB_CONTABIL_CHV-BUKRS.
      WA_SAIDA-GJAHR   = WA_ZIB_CONTABIL_CHV-GJAHR.
      WA_SAIDA-ICON    = ICON_CHECKED.
    ENDIF.

    MODIFY IT_SAIDA INDEX VG_TABIX FROM WA_SAIDA TRANSPORTING ICON MESSAGE BELNR BUKRS GJAHR.
  ENDLOOP.
ENDFORM.                    "Atualiza_processamento

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .


  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPORT
      IS_LAYOUT                = GD_LAYOUT
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IT_FIELDCAT              = IT_FCAT[]
      IT_SORT                  = T_SORT[]
      I_SAVE                   = 'X'
      IT_EVENTS                = EVENTS
      IS_PRINT                 = T_PRINT
*     IS_VARIANT               = VG_VARIANT
    TABLES
      T_OUTTAB                 = IT_SAIDA.


ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .

  PERFORM ALV_PREENCHE_CAT USING:
             'ICON'        ' '             '02'       ' '     ' '    ' ' , " Icon
             'ANLN1'       TEXT-002        '10'       ' '     ' '    ' ' , " Imobilizado
             'ANLN2'       TEXT-003        '06'       ' '     ' '    ' ' , " Sub-Nro
             'TXT50'       TEXT-004        '20'       ' '     ' '    ' ' , " Denominação 1
             'TXA50'       TEXT-005        '20'       ' '     ' '    ' ' , " Denominação 2
             'KOSTL'       TEXT-006        '15'       ' '     ' '    ' ' , " Centro de Custo.
             'GSBER'       TEXT-007        '10'       ' '     ' '    ' ' , " Divisão
             'NAFAZR'      TEXT-008        '15'       ' '     ' '    ' ' , " Valor R$ (05) Valor USD$(42)
             'NAFAZU'      TEXT-009        '15'       ' '     ' '    ' ' , " Valor R$ (05) Valor USD$(42)
             'BELNR'       TEXT-010        '15'       ' '     ' '    ' ' , " Nro.Documento
             'MESSAGE'     TEXT-011        '99'       ' '     ' '    ' ' .  " Log Erro

ENDFORM.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO  TYPE C
                               P_DESC   TYPE C
                               P_TAM    TYPE C
                               P_HOT    TYPE C
                               P_ZERO   TYPE C
                               P_SOMA   TYPE C.


  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SOMA.
  IF P_CAMPO = 'ICON'.
    WL_FCAT-ICON      = 'X'.
  ENDIF.


  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM           "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LV_FILE    TYPE STRING,
        VNUM(12)   TYPE C,
        VSEQ(12)   TYPE P,
        V_FIRST    LIKE SY-DATUM,
        V_LAST     LIKE SY-DATUM,
        VDATE      LIKE SY-DATUM,
        VDATEC(10) TYPE C,
        VMES(3)    TYPE C,
        VFLAG      TYPE I,
        VG_TABIX   TYPE SY-TABIX,
        VLOOP      TYPE I,
        V_OBJ      TYPE ZIB_CONTABIL-OBJ_KEY,
        VNAFAZ     TYPE ANLP-NAFAZ,
        VNAFAZU    TYPE ANLP-NAFAZ.

  DATA:
    BEGIN OF LS_DATE,
      YEAR(4)  TYPE N,
      MONTH(2) TYPE N,
      DAY(2)   TYPE N,
    END OF LS_DATE.

  DATA: L_DATE_TMP TYPE SY-DATUM.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.

  CASE RS_SELFIELD-FIELDNAME.

    WHEN 'BELNR'.
      SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
      SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
      SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN 'ANLN1'.
      SET PARAMETER ID 'AN1' FIELD WA_SAIDA-ANLN1.
      SET PARAMETER ID 'AN2' FIELD WA_SAIDA-ANLN2.
      SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
      CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
  ENDCASE.
  IF R_UCOMM EQ '&ATUALIZA'.
    PERFORM ATUALIZA_PROCESSAMENTO.
    RS_SELFIELD-REFRESH = 'X'.
  ELSEIF R_UCOMM EQ '&REINICIA'.
    PERFORM REINICIA_PROCESSAMENTO.
    RS_SELFIELD-REFRESH = 'X'.
  ELSEIF R_UCOMM EQ '&PROCESSA'.
    VSEQ = 0.
    VFLAG = 0.
    SORT IT_ANLP   BY ANLN1 ANLN2 AFABER.
    SORT IT_ANLP_U BY ANLN1 ANLN2 AFABER.
    SORT IT_SAIDA  BY ANLN1 ANLN2.
    VLOOP = 1.
    WHILE VLOOP LE 2.
      LOOP AT IT_ANLP INTO WA_ANLP.
        IF WA_ANLP-FLAG EQ 'X'.
          CONTINUE.
        ENDIF.
        VG_TABIX = SY-TABIX.
*       calcula o ultimo dia do periodo.
        MOVE WA_ANLP-PERAF TO VMES.
        CONCATENATE WA_ANLP-GJAHR VMES+1(2) '01' INTO VDATE.

*        CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
*          EXPORTING
*            I_DATE = VDATE
*          IMPORTING
*            E_DATE = V_LAST.

        LS_DATE = VDATE.
        IF LS_DATE-MONTH NE 12.
          LS_DATE-MONTH = LS_DATE-MONTH + 1.
        ELSE.
          LS_DATE-MONTH = 1.
          LS_DATE-YEAR  = LS_DATE-YEAR + 1.
        ENDIF.
        LS_DATE-DAY   = 1.

        L_DATE_TMP = LS_DATE.

*       no special "leap year" algorithm necessary: in SAP NW we trust...
        L_DATE_TMP = L_DATE_TMP - 1.

        V_LAST = L_DATE_TMP.
        CONCATENATE V_LAST+6(2) '.' V_LAST+4(2) '.' V_LAST+0(4) INTO VDATEC.

        IF VLOOP = 1. " Pega a Nova sequência
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR = '01'
              OBJECT      = 'ZID_AA'
            IMPORTING
              NUMBER      = VSEQ.

          VNUM = VSEQ .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = VNUM
            IMPORTING
              OUTPUT = VNUM.

          CONCATENATE 'FIAA' VNUM WA_ANLP-GJAHR  INTO WA_ZIB_CONTABIL-OBJ_KEY.
          READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY  ANLN1   = WA_ANLP-ANLN1
                               ANLN2   = WA_ANLP-ANLN2 BINARY SEARCH.
          WA_SAIDA-OBJ_KEY = WA_ZIB_CONTABIL-OBJ_KEY.
          MODIFY IT_SAIDA INDEX SY-TABIX FROM WA_SAIDA  TRANSPORTING OBJ_KEY.
        ELSE. " Pega a sequência do débito
          READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY  ANLN1   = WA_ANLP-ANLN1
                               ANLN2   = WA_ANLP-ANLN2 BINARY SEARCH.
          WA_ZIB_CONTABIL-OBJ_KEY = WA_SAIDA-OBJ_KEY .
        ENDIF.

        IF VLOOP = 1. " debito
          WA_ZIB_CONTABIL-BSCHL         = 40.
        ELSE.            " credito
          WA_ZIB_CONTABIL-BSCHL         = 50.
        ENDIF.

        WA_ZIB_CONTABIL-SEQITEM       = VLOOP .
        WA_ZIB_CONTABIL-GSBER         = WA_ANLP-GSBER.
        WA_ZIB_CONTABIL-BUKRS         = WA_ANLP-BUKRS.
        WA_ZIB_CONTABIL-INTERFACE     = '53'.   "'35'.  "Modificação CS2016001509
        WA_ZIB_CONTABIL-BLDAT         = VDATEC.
        WA_ZIB_CONTABIL-BUDAT         = VDATEC.
        WA_ZIB_CONTABIL-GJAHR         = WA_ANLP-GJAHR.
        WA_ZIB_CONTABIL-MONAT         = WA_ANLP-PERAF.
        WA_ZIB_CONTABIL-ANLN1         = WA_ANLP-ANLN1.
        WA_ZIB_CONTABIL-ANLN2         = WA_ANLP-ANLN2.

        WA_ZIB_CONTABIL-BLART         = 'AF'.
        IF VLOOP = 1.
          WA_ZIB_CONTABIL-HKONT         = 422457.
        ELSE.
          WA_ZIB_CONTABIL-HKONT         = 422450.
        ENDIF.
        WA_ZIB_CONTABIL-KOSTL         = WA_SAIDA-KOSTL. " WA_ANLP-KOSTL.

        VNAFAZ = WA_ANLP-NAFAZ.
        IF WA_ANLP-NAFAZ <= 0.
          WA_ZIB_CONTABIL-WRBTR         = WA_ANLP-NAFAZ * -1.
          WA_ZIB_CONTABIL-DMBTR         = WA_ANLP-NAFAZ * -1.
        ELSE.
          IF VLOOP = 1.
            WA_ZIB_CONTABIL-HKONT         = 422450.
          ELSE.
            WA_ZIB_CONTABIL-HKONT         = 422457.
          ENDIF.
          WA_ZIB_CONTABIL-WRBTR         = WA_ANLP-NAFAZ.
          WA_ZIB_CONTABIL-DMBTR         = WA_ANLP-NAFAZ.
        ENDIF.

        WA_ZIB_CONTABIL-WAERS         = 'BRL'.
        WA_ZIB_CONTABIL-BUPLA         = WA_ANLP-GSBER.
        CONCATENATE 'Transf. Depreciação Fiscal-' WA_ANLP-ANLN1 '-' WA_ANLP-ANLN2 INTO WA_ZIB_CONTABIL-SGTXT.
        WA_ZIB_CONTABIL-WAERS_I       = 'BRL'.
        WA_ZIB_CONTABIL-WAERS_F       = 'USD'.

        READ TABLE IT_ANLP_U  INTO WA_ANLP_U  WITH KEY ANLN1 = WA_ANLP-ANLN1
                                                                      ANLN2 = WA_ANLP-ANLN2 BINARY SEARCH.
        VNAFAZU = WA_ANLP_U-NAFAZ.
        IF WA_ANLP_U-NAFAZ <= 0.
          WA_ZIB_CONTABIL-DMBE2         = WA_ANLP_U-NAFAZ * -1.
        ELSE.
          WA_ZIB_CONTABIL-DMBE2         = WA_ANLP_U-NAFAZ.
          IF VLOOP = 1.
            WA_ZIB_CONTABIL-HKONT         = 422450.
          ELSE.
            WA_ZIB_CONTABIL-HKONT         = 422457.
          ENDIF.
        ENDIF.

        IF ( VNAFAZU < 0 AND  VNAFAZ > 0 ).
          WA_ZIB_CONTABIL-DMBE2 = 1 / 100.
        ELSEIF ( VNAFAZ < 0 AND  VNAFAZU > 0 ).
          WA_ZIB_CONTABIL-WRBTR         = 1 / 100.
          WA_ZIB_CONTABIL-DMBTR         = 1 / 100.
        ENDIF.

        IF WA_ZIB_CONTABIL-DMBE2 = 0.
          WA_ZIB_CONTABIL-DMBE2 = ( 1 / 100 ).
        ENDIF.
        WA_ZIB_CONTABIL-RG_ATUALIZADO = 'N'.
        INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
        IF SY-SUBRC NE 0.
          ROLLBACK WORK.
        ELSE.
          COMMIT WORK.
        ENDIF.
        IF ( VNAFAZU < 0 AND  VNAFAZ > 0 ).
          IF VLOOP = 1.
            WA_ZIB_CONTABIL-SEQITEM       = 3.
            WA_ZIB_CONTABIL-HKONT         = 422457.
          ELSE.
            WA_ZIB_CONTABIL-SEQITEM       = 4.
            WA_ZIB_CONTABIL-HKONT         = 422450.
          ENDIF.
          WA_ZIB_CONTABIL-WRBTR         = 1 / 100.
          WA_ZIB_CONTABIL-DMBTR         = 1 / 100.
          WA_ZIB_CONTABIL-DMBE2         = VNAFAZU * -1.

          INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
          ELSE.
            COMMIT WORK.
          ENDIF.
        ELSEIF ( VNAFAZ < 0 AND  VNAFAZU > 0 ).
          IF VLOOP = 1.
            WA_ZIB_CONTABIL-SEQITEM       = 3.
            WA_ZIB_CONTABIL-HKONT         = 422457.
          ELSE.
            WA_ZIB_CONTABIL-SEQITEM       = 4.
            WA_ZIB_CONTABIL-HKONT         = 422450.
          ENDIF.
          WA_ZIB_CONTABIL-WRBTR         = VNAFAZ * -1.
          WA_ZIB_CONTABIL-DMBTR         = VNAFAZ * -1.
          WA_ZIB_CONTABIL-DMBE2         = 1 / 100.

          INSERT INTO  ZIB_CONTABIL VALUES WA_ZIB_CONTABIL.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
          ELSE.
            COMMIT WORK.
          ENDIF.

        ENDIF.
        CLEAR: WA_ANLP_U, WA_ZIB_CONTABIL.
      ENDLOOP.
      VLOOP = VLOOP + 1.
    ENDWHILE.
* atualiza icon
    PERFORM ATUALIZA_FLAG.
    PERFORM ATUALIZA_PROCESSAMENTO.
    RS_SELFIELD-REFRESH = 'X'.
    MESSAGE  'Processamento concluído!' TYPE 'I'.
  ENDIF.
ENDFORM.  "User_command

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.        "#EC CALLED
  DESCRIBE TABLE RT_EXTAB. "Avoid Extended Check Warning
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status
*&---------------------------------------------------------------------*
*&      Form  REINICIA_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REINICIA_PROCESSAMENTO .
  DATA:    VG_TABIX  TYPE SY-TABIX,
           V_FLAG(1) TYPE C VALUE 0.

  SORT IT_ANLP  BY ANLN1 ANLN2.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    VG_TABIX = SY-TABIX.
    SELECT  SINGLE OBJ_KEY NR_ITEM MESSAGE
     FROM ZIB_CONTABIL_ERR
     INTO WA_ZIB_CONTABIL_ERR
     WHERE OBJ_KEY = WA_SAIDA-OBJ_KEY.

    IF SY-SUBRC = 0.
      V_FLAG = 1.
      DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = WA_SAIDA-OBJ_KEY.
      DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = WA_SAIDA-OBJ_KEY.
      WA_SAIDA-ICON    = ICON_SYSTEM_UNDO.
      READ TABLE IT_ANLP INTO WA_ANLP WITH KEY ANLN1 =  WA_SAIDA-ANLN1
                                             ANLN2 =  WA_SAIDA-ANLN2  BINARY SEARCH.
      WA_ANLP-FLAG = ' '.
      MODIFY IT_ANLP INDEX SY-TABIX FROM WA_ANLP TRANSPORTING FLAG.
    ENDIF.
    MODIFY IT_SAIDA INDEX VG_TABIX FROM WA_SAIDA TRANSPORTING ICON.
    CLEAR WA_SAIDA.
  ENDLOOP.

ENDFORM.                    " REINICIA_PROCESSAMENTO
