
*&---------------------------------------------------------------------*
*& Report  ZMMR036
*&
*&---------------------------------------------------------------------*
*&TITULO: MAPA de  produção Algodoeira
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 04.07.2013
*TRANSACAO:
*&---------------------------------------------------------------------*
REPORT  ZMMR036.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: CHVW.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*


TYPES:
    BEGIN OF TY_ZPPT0001,
       WERKS        TYPE ZPPT0001-WERKS ,
       TIPO         TYPE ZPPT0001-TIPO  ,
       MATNR        TYPE ZPPT0001-MATNR,
    END OF TY_ZPPT0001,

    BEGIN OF TY_CHVW,
      MATNR         TYPE CHVW-MATNR,
      WERKS         TYPE CHVW-WERKS,
      BUDAT         TYPE CHVW-BUDAT,
      BWART         TYPE CHVW-BWART,
      CHARG         TYPE CHVW-CHARG,
      MENGE         TYPE CHVW-MENGE,
      MBLNR         TYPE CHVW-MBLNR,
      MJAHR         TYPE CHVW-MJAHR,
      ZEILE         TYPE CHVW-ZEILE,
    END OF TY_CHVW,

    BEGIN OF TY_MCHB,
      MATNR        TYPE MCHB-MATNR,
      WERKS        TYPE MCHB-WERKS,
      CLABS        TYPE MCHB-CLABS,
      CHARG        TYPE MCHB-CHARG,
      ERSDA        TYPE MCHB-ERSDA,
    END OF TY_MCHB,

    BEGIN OF TY_ZMMT0025,
      ATINN       TYPE ZMMT0025-ATINN,
      ATNAM       TYPE ZMMT0025-ATNAM,
    END OF TY_ZMMT0025,

    BEGIN OF TY_SAIDA,
      CHARG      TYPE CHVW-CHARG,
      PESO       TYPE CHVW-MENGE,
      BUDAT      TYPE CHVW-BUDAT,
      SAFRA      TYPE AUSP-ATWRT,
      TALHAO     TYPE AUSP-ATWRT,
      VARIEDADE  TYPE AUSP-ATWRT,
      PERIODO    TYPE AUSP-ATWRT,
      PLUMA      TYPE CHVW-MENGE,
      CAROCO     TYPE CHVW-MENGE,
      FIBRILHA   TYPE CHVW-MENGE,
      RENDPLUMA  TYPE ANLP-NAFAZ,
      STATUS(15),
    END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,


      IT_ZMMT0025         TYPE TABLE OF TY_ZMMT0025,
      IT_MCHB             TYPE TABLE OF TY_MCHB,
      IT_CHVW             TYPE TABLE OF TY_CHVW,
      IT_CHVW_CA          TYPE TABLE OF TY_CHVW,
      IT_ZPPT0001         TYPE TABLE OF TY_ZPPT0001,
      IT_ZPPT0001_FARDO   TYPE TABLE OF TY_ZPPT0001,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,

      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*&---------------------------------------------------------------------*
*&      Tabelas/WA/Variaveis
*&---------------------------------------------------------------------*
DATA: TG_MATNR      TYPE TABLE OF ZMME_CL,
      TG_MATNR2     TYPE TABLE OF ZMME_CL,
      TG_RETURN     TYPE TABLE OF ZMME_CL,
      TG_RETURN2    TYPE TABLE OF ZMME_CL,
      WG_RETURN     TYPE ZMME_CL,
      WG_RETURN_AUX TYPE ZMME_CL.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT TYPE LVC_S_LAYO,

      WA_ZMMT0025         TYPE TY_ZMMT0025,
      WA_MCHB             TYPE TY_MCHB,
      WA_CHVW             TYPE TY_CHVW,
      WA_CHVW_CA          TYPE TY_CHVW,
      WA_ZPPT0001         TYPE TY_ZPPT0001,
      WA_ZPPT0001_FARDO   TYPE TY_ZPPT0001,
      WA_SAIDA            TYPE TY_SAIDA,
      WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
     S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
     T_TOP      TYPE SLIS_T_LISTHEADER    ,
     XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
     EVENTS     TYPE SLIS_T_EVENT         ,
     GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
     T_PRINT    TYPE SLIS_PRINT_ALV       ,
     V_REPORT   LIKE SY-REPID             ,
     T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
     IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
     ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
     WA_ESTRUTURA TYPE TY_ESTRUTURA,
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

PARAMETERS:      P_WERKS   TYPE CHVW-WERKS  OBLIGATORY.
SELECT-OPTIONS: P_BUDAT    FOR CHVW-BUDAT  OBLIGATORY,
                P_CHARG    FOR CHVW-CHARG.
PARAMETERS:     P_SAFRA(4)    TYPE C OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_INICIAR_VARIAVES, " Cabeçalho
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM  F_INICIAR_VARIAVES.

  DATA:        W_DATA(10),
               W_HORA(10),
               W_TEXTO(40),

               CABEC        TYPE C LENGTH 200,
               CABEC2       TYPE C LENGTH 200.

  DATA: WA_T001K    TYPE T001K,
        WA_T001     TYPE T001,
        WA_T001W    TYPE T001W.

  V_REPORT = SY-REPID.

  W_TEXTO = 'Algodoeira'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO.

  SELECT SINGLE *
   FROM T001K
   INTO WA_T001K
   WHERE BWKEY = P_WERKS.

  SELECT SINGLE *
   FROM T001W
   INTO WA_T001W
   WHERE WERKS = P_WERKS.

  SELECT SINGLE *
     FROM T001
     INTO WA_T001
     WHERE BUKRS = WA_T001K-BUKRS.
  IF SY-SUBRC  = 0.
    W_TEXTO = 'Empresa    :'.
    CONCATENATE W_TEXTO WA_T001-BUKRS '-' WA_T001-BUTXT    INTO CABEC  SEPARATED BY SPACE.
    W_TEXTO = 'Centro    :'.
    CONCATENATE W_TEXTO WA_T001W-WERKS '-' WA_T001W-NAME1  INTO CABEC2 SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC2.
  ENDIF.

  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO W_DATA.
  W_TEXTO = 'Data    :'.
  CONCATENATE W_TEXTO W_DATA  INTO CABEC SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.

  CONCATENATE SY-UZEIT+0(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2) INTO W_HORA.
  W_TEXTO = 'Hora    :'.
  CONCATENATE W_TEXTO W_HORA  INTO CABEC SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.

ENDFORM.                    " INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.


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
  SELECT  WERKS  TIPO  MATNR
    FROM ZPPT0001
    INTO TABLE IT_ZPPT0001
    WHERE WERKS EQ P_WERKS
    AND TIPO IN ('C','P','S2','S1').

  SELECT  ATINN ATNAM
    FROM  ZMMT0025
    INTO TABLE IT_ZMMT0025.

  SORT IT_ZMMT0025 BY ATNAM.

  "FARDÃO
  IT_ZPPT0001_FARDO[] = IT_ZPPT0001[].
  DELETE IT_ZPPT0001_FARDO WHERE TIPO NE 'C'.
  DELETE IT_ZPPT0001       WHERE TIPO EQ 'C'.

  CHECK IT_ZPPT0001_FARDO[] IS NOT INITIAL.

  SELECT MATNR WERKS BUDAT BWART CHARG MENGE MBLNR MJAHR ZEILE
     FROM CHVW
     INTO TABLE IT_CHVW
    FOR ALL ENTRIES IN IT_ZPPT0001_FARDO
    WHERE MATNR EQ IT_ZPPT0001_FARDO-MATNR
    AND   WERKS EQ IT_ZPPT0001_FARDO-WERKS
    AND   BUDAT IN P_BUDAT
    AND   CHARG IN P_CHARG
    AND   BWART EQ '261'
    AND NOT EXISTS ( SELECT * FROM MSEG
                      WHERE SMBLN = CHVW~MBLNR
                      AND  MJAHR = CHVW~MJAHR ).

  LOOP AT IT_CHVW INTO WA_CHVW.
    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'SAFRA' BINARY SEARCH.
    MOVE: WA_CHVW-MATNR     TO WG_RETURN-MATNR,
          WA_CHVW-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'TALHAO' BINARY SEARCH.
    MOVE: WA_CHVW-MATNR     TO WG_RETURN-MATNR,
          WA_CHVW-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'VARIEDADE' BINARY SEARCH.
    MOVE: WA_CHVW-MATNR     TO WG_RETURN-MATNR,
          WA_CHVW-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'PERIODO' BINARY SEARCH.
    MOVE: WA_CHVW-MATNR     TO WG_RETURN-MATNR,
          WA_CHVW-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR.

  ENDLOOP.
  IF TG_MATNR IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        T_MATNR  = TG_MATNR
        T_RETURN = TG_RETURN
      EXCEPTIONS
        ERRO4    = 1.
*          OTHERS         = 2.
    IF SY-SUBRC <> 0.
*        sy-subrc = 1 "Dados não encontrados
    ELSE.

    ENDIF.
  ENDIF.

  IF IT_CHVW[] IS NOT INITIAL.
    SELECT MATNR WERKS BUDAT BWART CHARG MENGE MBLNR MJAHR ZEILE
      FROM CHVW
      INTO TABLE IT_CHVW_CA
     FOR ALL ENTRIES IN IT_CHVW
     WHERE MBLNR EQ IT_CHVW-MBLNR
     AND   MJAHR EQ IT_CHVW-MJAHR.
  ENDIF.

  " MCHB
  SELECT MATNR WERKS CLABS CHARG ERSDA
  FROM MCHB
  INTO TABLE IT_MCHB
  FOR ALL ENTRIES IN  IT_ZPPT0001_FARDO
  WHERE MATNR EQ IT_ZPPT0001_FARDO-MATNR
  AND   WERKS EQ IT_ZPPT0001_FARDO-WERKS
  AND   CHARG IN P_CHARG
  AND   CLABS GT 0
  and   ERSDA in P_BUDAT.

  LOOP AT IT_MCHB INTO WA_MCHB.
    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'SAFRA' BINARY SEARCH.
    MOVE: WA_MCHB-MATNR     TO WG_RETURN-MATNR,
          WA_MCHB-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR2.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'TALHAO' BINARY SEARCH.
    MOVE: WA_MCHB-MATNR     TO WG_RETURN-MATNR,
          WA_MCHB-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR2.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'VARIEDADE' BINARY SEARCH.
    MOVE: WA_MCHB-MATNR     TO WG_RETURN-MATNR,
          WA_MCHB-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR2.

    READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATNAM = 'PERIODO' BINARY SEARCH.
    MOVE: WA_MCHB-MATNR     TO WG_RETURN-MATNR,
          WA_MCHB-CHARG     TO WG_RETURN-CHARG,
          WA_ZMMT0025-ATINN TO WG_RETURN-ATINN.
    APPEND WG_RETURN TO TG_MATNR2.
  ENDLOOP.
  IF TG_MATNR2 IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        T_MATNR  = TG_MATNR2
        T_RETURN = TG_RETURN2
      EXCEPTIONS
        ERRO4    = 1.
*          OTHERS         = 2.
    IF SY-SUBRC <> 0.
*        sy-subrc = 1 "Dados não encontrados
    ELSE.

    ENDIF.
  ENDIF.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM:  DEFINIR_EVENTOS,
            MONTAR_LAYOUT.

  SORT IT_SAIDA BY CHARG.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
*      I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
*      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.
ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  XTOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  PERFORM MONTAR_ESTRUTURA USING:
    1  ' '      ' '       'IT_SAIDA'  'CHARG'      'Numero Fardao'     '15',
    1  ' '      ' '       'IT_SAIDA'  'PESO'       'Peso'              '15',
    2  ' '      ' '       'IT_SAIDA'  'BUDAT'      'Data lançamento'   '10',
    3  ' '      ' '       'IT_SAIDA'  'SAFRA'      'Safra'             '10',
    4  ' '      ' '       'IT_SAIDA'  'TALHAO'     'Talhão'            '15',
    5  ' '      ' '       'IT_SAIDA'  'VARIEDADE'  'Variedade'         '15',
    6  ' '      ' '       'IT_SAIDA'  'PERIODO'    'Período'           '15',
    7  ' '      ' '       'IT_SAIDA'  'PLUMA'      'Peso Pluma'        '15',
    8  ' '      ' '       'IT_SAIDA'  'CAROCO'     'Caroço'            '15',
    9  ' '      ' '       'IT_SAIDA'  'FIBRILHA'   'Fibrilha'          '15',
   10  ' '      ' '       'IT_SAIDA'  'RENDPLUMA'  'Rend. % Pluma'     '15',
   11  ' '      ' '       'IT_SAIDA'  'STATUS'     'Status'            '20'.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME      = P_FIELD.
  WA_ESTRUTURA-TABNAME        = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME    = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME  = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY            = ' '.
  WA_ESTRUTURA-KEY_SEL        = 'X'.
  WA_ESTRUTURA-COL_POS        = P_COL_POS.
  WA_ESTRUTURA-NO_OUT         = ' '.
  WA_ESTRUTURA-SELTEXT_S      = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M      = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L      = P_SCRTEXT_L.

  IF P_OUTPUTLEN IS INITIAL.
    WA_ESTRUTURA-OUTPUTLEN    = X_CONTADOR.
  ELSE.
    WA_ESTRUTURA-OUTPUTLEN    = P_OUTPUTLEN.
  ENDIF.

  APPEND WA_ESTRUTURA TO ESTRUTURA.
ENDFORM.                    " montar_estrutura


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  SORT: IT_ZPPT0001_FARDO BY WERKS MATNR,
        IT_ZPPT0001       BY WERKS MATNR TIPO,
        IT_CHVW           BY WERKS MATNR,
        IT_CHVW_CA        BY MBLNR MJAHR BWART,
        TG_RETURN         BY MATNR CHARG ATINN,
        TG_RETURN2        BY MATNR CHARG ATINN,
        IT_MCHB           BY WERKS MATNR,
        IT_ZMMT0025       BY ATINN.

  LOOP AT IT_ZPPT0001_FARDO INTO WA_ZPPT0001_FARDO.

    LOOP AT IT_MCHB INTO WA_MCHB WHERE WERKS = WA_ZPPT0001_FARDO-WERKS
                                AND  MATNR = WA_ZPPT0001_FARDO-MATNR.
      WA_SAIDA-CHARG      = WA_MCHB-CHARG.
      WA_SAIDA-PESO       = WA_MCHB-CLABS.
      WA_SAIDA-BUDAT      = WA_MCHB-ERSDA.

      LOOP AT TG_RETURN2 INTO WG_RETURN WHERE MATNR = WA_MCHB-MATNR
                                        AND   CHARG = WA_MCHB-CHARG.
        READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATINN = WG_RETURN-ATINN BINARY SEARCH.
        IF WA_ZMMT0025-ATNAM = 'SAFRA'.
          WA_SAIDA-SAFRA     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'TALHAO'.
          WA_SAIDA-TALHAO     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'VARIEDADE'.
          WA_SAIDA-VARIEDADE     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'PERIODO'.
          WA_SAIDA-PERIODO      = WG_RETURN-ATWRT.
        ENDIF.
      ENDLOOP.
      WA_SAIDA-STATUS = 'DISPONIVEL'.
      IF WA_SAIDA-SAFRA     EQ P_SAFRA.
        APPEND WA_SAIDA TO IT_SAIDA.
      ENDIF.
      CLEAR WA_SAIDA.

    ENDLOOP.

    LOOP AT IT_CHVW INTO WA_CHVW WHERE WERKS = WA_ZPPT0001_FARDO-WERKS
                                  AND  MATNR = WA_ZPPT0001_FARDO-MATNR.
      WA_SAIDA-CHARG      = WA_CHVW-CHARG.
      WA_SAIDA-PESO       = WA_CHVW-MENGE.
      WA_SAIDA-BUDAT      = WA_CHVW-BUDAT.
      LOOP AT TG_RETURN INTO WG_RETURN WHERE MATNR = WA_CHVW-MATNR
                                       AND   CHARG = WA_CHVW-CHARG.
        READ TABLE  IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATINN = WG_RETURN-ATINN BINARY SEARCH.
        IF WA_ZMMT0025-ATNAM = 'SAFRA'.
          WA_SAIDA-SAFRA     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'TALHAO'.
          WA_SAIDA-TALHAO     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'VARIEDADE'.
          WA_SAIDA-VARIEDADE     = WG_RETURN-ATWRT.
        ELSEIF WA_ZMMT0025-ATNAM = 'PERIODO'.
          WA_SAIDA-PERIODO      = WG_RETURN-ATWRT.
        ENDIF.
      ENDLOOP.
      LOOP AT IT_CHVW_CA INTO WA_CHVW_CA  WHERE  MBLNR = WA_CHVW-MBLNR
                                          AND    MJAHR = WA_CHVW-MJAHR.
        IF WA_CHVW_CA-BWART  = '131'. " Pluma
          READ TABLE IT_ZPPT0001  INTO WA_ZPPT0001 WITH KEY WERKS = WA_CHVW_CA-WERKS
                                                            MATNR = WA_CHVW_CA-MATNR
                                                            TIPO  = 'P'.
          IF SY-SUBRC = 0.
            WA_SAIDA-PLUMA = WA_CHVW_CA-MENGE.
          ENDIF.
        ENDIF.
        IF WA_CHVW_CA-BWART  = '531'. " Caroço
          READ TABLE IT_ZPPT0001  INTO WA_ZPPT0001 WITH KEY WERKS = WA_CHVW_CA-WERKS
                                                            MATNR = WA_CHVW_CA-MATNR
                                                            TIPO  = 'S2'.
          IF SY-SUBRC = 0.
            WA_SAIDA-CAROCO = WA_CHVW_CA-MENGE.
          ENDIF.
        ENDIF.
        IF WA_CHVW_CA-BWART  = '531'. " Fibrilha
          READ TABLE IT_ZPPT0001  INTO WA_ZPPT0001 WITH KEY WERKS = WA_CHVW_CA-WERKS
                                                            MATNR = WA_CHVW_CA-MATNR
                                                            TIPO  = 'S1'.
          IF SY-SUBRC = 0.
            WA_SAIDA-FIBRILHA = WA_CHVW_CA-MENGE.
          ENDIF.
        ENDIF.

      ENDLOOP.
      IF WA_SAIDA-PESO GT 0.
        WA_SAIDA-RENDPLUMA = ( WA_SAIDA-PLUMA / WA_SAIDA-PESO ) * 100.
      ELSE.
        WA_SAIDA-RENDPLUMA = 0.
      ENDIF.

      IF WA_SAIDA-RENDPLUMA = 0.
        WA_SAIDA-STATUS = 'DISPONIVEL'.
      ELSE.
        WA_SAIDA-STATUS = 'BENEFICIADO'.
      ENDIF.
      IF WA_SAIDA-SAFRA     EQ P_SAFRA.
        APPEND WA_SAIDA TO IT_SAIDA.
      ENDIF.
      CLEAR WA_SAIDA.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SAIDA
