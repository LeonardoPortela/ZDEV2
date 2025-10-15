************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Projeto "La Expansion"                              *
* Data desenv ...: 17.10.2012                                          *
* Objetivo    ...: Prestamos de stocks                                 *
* Transação   ...: ZMMY0002                                            *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 17.10.2012   Antonio Luiz   Criação       10:52         DEVK924644   *
************************************************************************

REPORT  ZMMY0002.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: MSEG.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF TY_MSEG,
        MBLNR       TYPE MSEG-MBLNR,
        BUDAT_MKPF  TYPE MSEG-BUDAT_MKPF,
        MJAHR       TYPE MSEG-MJAHR,
        MATNR       TYPE MSEG-MATNR,
        WERKS       TYPE MSEG-WERKS,
        LGORT       TYPE MSEG-LGORT,
        CHARG       TYPE MSEG-CHARG,
        LIFNR       TYPE MSEG-LIFNR,
        MENGE       TYPE MSEG-MENGE,
        DMBTR       TYPE MSEG-DMBTR,
        EXBWR       TYPE MSEG-EXBWR,
        XBLNR_MKPF  TYPE MSEG-XBLNR_MKPF,
        BWART       TYPE MSEG-BWART,
        KUNNR       TYPE MSEG-KUNNR,
        PROVEDOR(10) TYPE C,
      END OF TY_MSEG,

       BEGIN OF TY_MSEG_EST,
        SMBLN       TYPE MSEG-SMBLN,
       END OF TY_MSEG_EST,

      BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
      END OF TY_LFA1,

      BEGIN OF TY_KNA1,
         KUNNR TYPE KNA1-KUNNR,
         NAME1 TYPE KNA1-NAME1,
      END OF TY_KNA1,

      BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
      END OF TY_MAKT,

      BEGIN OF TY_T001W,
         WERKS TYPE T001W-WERKS,
         NAME1 TYPE T001W-NAME1,
      END OF TY_T001W,

      BEGIN OF TY_T001L,
         LGORT TYPE T001L-LGORT,
         LGOBE TYPE T001L-LGOBE,
      END OF TY_T001L,

      BEGIN OF TY_SAIDA,
        MAKTX       TYPE MAKT-MAKTX,
        BUDAT_MKPF  TYPE MSEG-BUDAT_MKPF,
        NAME1T      TYPE T001W-NAME1,
        NAME1L      TYPE LFA1-NAME1,
        LGOBE       TYPE T001L-LGOBE,
        XBLNR_MKPF  TYPE MSEG-XBLNR_MKPF,
        OTORGADOK   TYPE MSEG-MENGE,
        DEVOLUCIONK TYPE MSEG-MENGE,
        SALDOK      TYPE MSEG-MENGE,
        OTORGADOV   TYPE MSEG-DMBTR,
        DEVOLUCIONV TYPE MSEG-DMBTR,
        SALDOV      TYPE MSEG-DMBTR,
      END OF TY_SAIDA.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:  IT_MSEG_F      TYPE TABLE OF TY_MSEG ,
       IT_MSEG_C      TYPE TABLE OF TY_MSEG ,
       IT_MSEG        TYPE TABLE OF TY_MSEG ,
       IT_MSEG_AUX    TYPE TABLE OF TY_MSEG ,
       IT_MSEG_EST    TYPE TABLE OF TY_MSEG_EST ,
       IT_LFA1        TYPE TABLE OF TY_LFA1,
       IT_KNA1        TYPE TABLE OF TY_KNA1,
       IT_MAKT        TYPE TABLE OF TY_MAKT,
       IT_T001W       TYPE TABLE OF TY_T001W,
       IT_T001L       TYPE TABLE OF TY_T001L,
       IT_SAIDA       TYPE TABLE OF TY_SAIDA,
       IT_SAIDA_AUX   TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA: WA_MSEG        TYPE TY_MSEG,
      WA_MSEG_AUX    TYPE TY_MSEG,
      WA_MSEG_EST    TYPE TY_MSEG_EST,
      WA_LFA1        TYPE TY_LFA1,
      WA_KNA1        TYPE TY_KNA1,
      WA_MAKT        TYPE TY_MAKT,
      WA_T001W       TYPE TY_T001W,
      WA_T001L       TYPE TY_T001L,
      WA_SAIDA       TYPE TY_SAIDA,
      WA_SAIDA_AUX   TYPE TY_SAIDA,
      WA_CONT        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV         TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT      TYPE LVC_S_LAYO.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

DATA: IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
      T_TOP      TYPE SLIS_T_LISTHEADER     ,
      XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
      EVENTS     TYPE SLIS_T_EVENT         ,
      GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
      T_PRINT    TYPE SLIS_PRINT_ALV       ,
      T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      V_REPORT   LIKE SY-REPID             .

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
        P_MJAHR  FOR MSEG-MJAHR OBLIGATORY,
        P_MATNR  FOR MSEG-MATNR OBLIGATORY,
        P_WERKS  FOR MSEG-WERKS OBLIGATORY,
        P_LGORT  FOR MSEG-LGORT OBLIGATORY,
        P_CHARG  FOR MSEG-CHARG,
        P_LIFNR  FOR MSEG-LIFNR.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: RB1 RADIOBUTTON GROUP RAD1 ,
            RB2 RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_HIST LIKE BSID-UMSKZ AS CHECKBOX USER-COMMAND ACT DEFAULT ' '.
SELECT-OPTIONS P_DATA  FOR MSEG-BUDAT_MKPF MODIF ID A.
SELECTION-SCREEN: END OF BLOCK B3.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF R_HIST = 'X'.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.




*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
            F_CONSTRUIR_CABECALHO ,
            F_SELECIONA_DADOS     , " Form seleciona dados
            F_SAIDA               , " Saida relatorio
            F_IMPRIME_DADOS      .

*  CALL SCREEN 0100.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .


  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_LAYOUT               = GD_LAYOUT
*      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
*      IS_VARIANT              = VG_VARIANT
    TABLES
      T_OUTTAB                = IT_SAIDA.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  IF R_HIST = 'X'.
    IF P_DATA-LOW IS INITIAL.
      MESSAGE 'Dígale al período.'  TYPE 'I'.
      STOP.
    ENDIF.
  ENDIF.
  SELECT  MBLNR BUDAT_MKPF MJAHR MATNR WERKS LGORT CHARG LIFNR MENGE DMBTR EXBWR XBLNR_MKPF BWART KUNNR
    FROM MSEG
    INTO TABLE IT_MSEG_F
    WHERE MJAHR    IN P_MJAHR
    AND   MATNR    IN P_MATNR
    AND   WERKS    IN P_WERKS
    AND   LGORT    IN P_LGORT
    AND   CHARG    IN P_CHARG
    AND   LIFNR    IN P_LIFNR
    AND   MBLNR    NE MSEG-SMBLN
    AND   MBLNR    NE ''
    AND   LIFNR    NE '' ORDER BY PRIMARY KEY .

  SELECT  MBLNR BUDAT_MKPF MJAHR MATNR WERKS LGORT CHARG LIFNR MENGE DMBTR EXBWR XBLNR_MKPF BWART KUNNR
    FROM MSEG
    INTO TABLE IT_MSEG_C
    WHERE MJAHR    IN P_MJAHR
    AND   MATNR    IN P_MATNR
    AND   WERKS    IN P_WERKS
    AND   LGORT    IN P_LGORT
    AND   CHARG    IN P_CHARG
    AND   LIFNR    IN P_LIFNR
    AND   MBLNR    NE MSEG-SMBLN
    AND   MBLNR    NE ''
    AND   KUNNR    NE '' ORDER BY PRIMARY KEY .

  IF R_HIST = 'X'.
    DELETE IT_MSEG_F WHERE  BUDAT_MKPF NOT IN P_DATA.
    DELETE IT_MSEG_C WHERE  BUDAT_MKPF NOT IN P_DATA.
  ENDIF.

*  IF RB1 = 'X'.
*    DELETE IT_MSEG_F WHERE BWART NE 'W01' AND BWART NE 'W03'.
*    DELETE IT_MSEG_C WHERE BWART NE 'W01' AND BWART NE 'W03'.
*  ELSE.
*    DELETE IT_MSEG_F WHERE BWART NE 'W05' AND BWART NE 'W07'.
*    DELETE IT_MSEG_C WHERE BWART NE 'W05' AND BWART NE 'W07'.
*  ENDIF.

  LOOP AT IT_MSEG_F INTO WA_MSEG.
    IF WA_MSEG-LIFNR NE ''.
      WA_MSEG-PROVEDOR = WA_MSEG-LIFNR.
    ELSE.
      WA_MSEG-PROVEDOR = WA_MSEG-KUNNR.
    ENDIF.
    APPEND WA_MSEG TO IT_MSEG.
  ENDLOOP.

  LOOP AT IT_MSEG_C INTO WA_MSEG.
    IF WA_MSEG-LIFNR NE ''.
      WA_MSEG-PROVEDOR = WA_MSEG-LIFNR.
    ELSE.
      WA_MSEG-PROVEDOR = WA_MSEG-KUNNR.
    ENDIF.
    APPEND WA_MSEG TO IT_MSEG.
  ENDLOOP.

  CHECK NOT IT_MSEG IS INITIAL.

* SELECIONA ESTORNADOS
  IT_MSEG_AUX[] = IT_MSEG.
  DELETE ADJACENT DUPLICATES FROM IT_MSEG_AUX COMPARING MBLNR .
  SELECT SMBLN
    FROM MSEG
    INTO TABLE IT_MSEG_EST
    FOR ALL ENTRIES IN IT_MSEG_AUX
    WHERE SMBLN EQ IT_MSEG_AUX-MBLNR.

* ELIMINA DOCS ESTORNADOS
  SORT IT_MSEG BY MBLNR.
  LOOP AT IT_MSEG_EST INTO WA_MSEG_EST.
    DELETE IT_MSEG WHERE MBLNR = WA_MSEG_EST-SMBLN.
  ENDLOOP.

  SELECT LIFNR NAME1
    FROM LFA1
    INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_MSEG
    WHERE LIFNR EQ IT_MSEG-LIFNR ORDER BY PRIMARY KEY .

  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_MSEG
    WHERE KUNNR EQ IT_MSEG-KUNNR ORDER BY PRIMARY KEY .

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_MSEG
    WHERE MATNR EQ IT_MSEG-MATNR.

  SORT IT_MAKT BY MATNR.
SELECT WERKS NAME1
    FROM T001W
    INTO TABLE IT_T001W
    FOR ALL ENTRIES IN IT_MSEG
    WHERE WERKS EQ IT_MSEG-WERKS ORDER BY PRIMARY KEY .

  SELECT LGORT LGOBE
    FROM T001L
    INTO TABLE IT_T001L
    FOR ALL ENTRIES IN IT_MSEG
    WHERE LGORT = IT_MSEG-LGORT.

  SORT IT_T001L BY LGORT.
DELETE ADJACENT DUPLICATES FROM IT_LFA1 COMPARING LIFNR.
  DELETE ADJACENT DUPLICATES FROM IT_KNA1 COMPARING KUNNR.
  DELETE ADJACENT DUPLICATES FROM IT_MAKT COMPARING MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_T001W COMPARING WERKS.
  DELETE ADJACENT DUPLICATES FROM IT_T001L COMPARING LGORT.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .
  IF RB1 = 'X'.
    PERFORM ALV_PREENCHE_CAT USING:
         'MAKTX'            TEXT-003 '30'  '' ''  '', " MATERIAL
         'BUDAT_MKPF'       TEXT-004 '20'  '' ''  '', " FECHA DEL PRESTAMO
         'NAME1T'           TEXT-005 '30' '' ''  '', " PUERTO
         'LGOBE'            TEXT-016 '15' '' ''  '', " ALMÁCEN
         'NAME1L'           TEXT-006 '30'  '' ''  '', " PROVEDOR
         'XBLNR_MKPF'       TEXT-007 '16' '' ''  '', " NUMERO CARTA
         'OTORGADOK'        TEXT-008 '15'  '' ''  '', " OTORGADO KG
         'DEVOLUCIONK'      TEXT-009 '15'  '' ''  '', " DEVOLUCION KG
         'SALDOK'           TEXT-010 '15'  '' ''  '', " SALDO KG
         'OTORGADOV'        TEXT-011 '15'  '' ''  '', " OTORGADO $
         'DEVOLUCIONV'      TEXT-012 '15'  '' ''  '', " DEVOLUCION $
         'SALDOV'           TEXT-013 '15'  '' ''  ''. " SALDO $
  ELSE.
    PERFORM ALV_PREENCHE_CAT USING:
         'MAKTX'            TEXT-003 '30'  '' ''  '', " MATERIAL
         'BUDAT_MKPF'       TEXT-004 '20'  '' ''  '', " FECHA DEL PRESTAMO
         'NAME1T'           TEXT-005 '30' '' ''  '', " PUERTO
         'LGOBE'            TEXT-016 '15' '' ''  '', " ALMÁCEN
         'NAME1L'           TEXT-006 '30'  '' ''  '', " PROVEDOR
         'XBLNR_MKPF'       TEXT-007 '16' '' ''  '', " NUMERO CARTA
         'OTORGADOK'        TEXT-014 '15'  '' ''  '', " RECIBIDO KG
         'DEVOLUCIONK'      TEXT-009 '15'  '' ''  '', " DEVOLUCION KG
         'SALDOK'           TEXT-010 '15'  '' ''  '', " SALDO KG
         'OTORGADOV'        TEXT-015 '15'  '' ''  '', " RECIBIDO $
         'DEVOLUCIONV'      TEXT-012 '15'  '' ''  '', " DEVOLUCION $
         'SALDOV'           TEXT-013 '15'  '' ''  ''. " SALDO $
  ENDIF.
ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_TAM   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C
                               P_SUM   TYPE C.
  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SUM.

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_STATUS OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " Z_STATUS  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  SORT: IT_MSEG BY MATNR WERKS PROVEDOR XBLNR_MKPF LGORT,
        IT_T001W BY WERKS,
        IT_T001L BY LGORT,
        IT_MAKT BY MATNR,
        IT_LFA1 BY LIFNR,
        IT_KNA1 BY KUNNR.

  DATA: VVALOR TYPE MSEG-DMBTR,
        VDEVOL TYPE MSEG-DMBTR,
        VKILO  TYPE MSEG-MENGE,
        VDEVOLK TYPE MSEG-MENGE.

  REFRESH IT_MSEG_AUX.
  IT_MSEG_AUX[] = IT_MSEG.
  DELETE ADJACENT DUPLICATES FROM IT_MSEG_AUX COMPARING MATNR WERKS PROVEDOR XBLNR_MKPF LGORT.
  LOOP AT IT_MSEG_AUX INTO WA_MSEG_AUX.

    READ TABLE IT_MAKT  INTO WA_MAKT WITH KEY MATNR = WA_MSEG_AUX-MATNR BINARY SEARCH.
    IF WA_MSEG_AUX-LIFNR NE ''.
      READ TABLE IT_LFA1  INTO WA_LFA1 WITH KEY LIFNR = WA_MSEG_AUX-LIFNR BINARY SEARCH.
      WA_SAIDA-NAME1L          = WA_LFA1-NAME1.
    ELSE.
      READ TABLE IT_KNA1  INTO WA_KNA1 WITH KEY KUNNR = WA_MSEG_AUX-KUNNR BINARY SEARCH.
      WA_SAIDA-NAME1L          = WA_KNA1-NAME1.
    ENDIF.
    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_MSEG_AUX-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-NAME1T          = WA_T001W-NAME1.
    ELSE.
      CLEAR WA_SAIDA-NAME1T.
    ENDIF.

    READ TABLE IT_T001L INTO WA_T001L WITH KEY LGORT = WA_MSEG_AUX-LGORT BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-LGOBE           = WA_T001L-LGOBE.
    ELSE..
      CLEAR WA_SAIDA-LGOBE.
    ENDIF.

    WA_SAIDA-MAKTX           = WA_MAKT-MAKTX.
    WA_SAIDA-BUDAT_MKPF      = WA_MSEG_AUX-BUDAT_MKPF.
    WA_SAIDA-XBLNR_MKPF      = WA_MSEG_AUX-XBLNR_MKPF.


    CLEAR: VVALOR, VDEVOL, VKILO, VDEVOLK.
    READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MATNR          = WA_MSEG_AUX-MATNR
                                             WERKS          = WA_MSEG_AUX-WERKS
                                             PROVEDOR       = WA_MSEG_AUX-PROVEDOR
                                             XBLNR_MKPF     = WA_MSEG_AUX-XBLNR_MKPF
                                             LGORT          = WA_MSEG_AUX-LGORT.

    LOOP AT IT_MSEG INTO WA_MSEG WHERE MATNR          = WA_MSEG_AUX-MATNR
                                 AND   WERKS          = WA_MSEG_AUX-WERKS
                                 AND   PROVEDOR       = WA_MSEG_AUX-PROVEDOR
                                 AND   XBLNR_MKPF     = WA_MSEG_AUX-XBLNR_MKPF
                                 AND   LGORT          = WA_MSEG_AUX-LGORT.

      IF WA_MSEG-BWART = 'W01' OR WA_MSEG-BWART =  'W05'.
        IF WA_MSEG-EXBWR = 0  OR WA_MSEG-EXBWR IS INITIAL.
          VVALOR     = VVALOR + WA_MSEG-DMBTR.
        ELSE.
          VVALOR     = VVALOR + WA_MSEG-EXBWR.
        ENDIF.
        VKILO      = VKILO + WA_MSEG-MENGE.
      ENDIF.
      IF WA_MSEG-BWART = 'W03' OR WA_MSEG-BWART =  'W07'.
        IF WA_MSEG-EXBWR = 0  OR WA_MSEG-EXBWR IS INITIAL.
          VDEVOL      = VDEVOL + WA_MSEG-DMBTR.
        ELSE.
          VDEVOL      = VDEVOL + WA_MSEG-EXBWR.
        ENDIF.
        VDEVOLK     = VDEVOLK + WA_MSEG-MENGE.
      ENDIF.

    ENDLOOP.


    WA_SAIDA-OTORGADOK       = VKILO.
    WA_SAIDA-DEVOLUCIONK     = VDEVOLK.
    WA_SAIDA-SALDOK          = VKILO - VDEVOLK.
    WA_SAIDA-OTORGADOV       = VVALOR.
    WA_SAIDA-DEVOLUCIONV     = VDEVOL.
    WA_SAIDA-SALDOV          = VVALOR - VDEVOL.

    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR: WA_SAIDA, WA_MAKT, WA_LFA1,WA_KNA1, WA_T001W.
  ENDLOOP.

  IT_SAIDA_AUX[] = IT_SAIDA.

  SORT IT_SAIDA     BY XBLNR_MKPF.
  SORT IT_SAIDA_AUX BY XBLNR_MKPF.
  DELETE ADJACENT DUPLICATES FROM IT_SAIDA_AUX COMPARING  XBLNR_MKPF.

  DATA VSALDO TYPE MSEG-MENGE VALUE 0.

  LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
    VSALDO = 0.
    READ TABLE IT_SAIDA  INTO WA_SAIDA WITH KEY XBLNR_MKPF  = WA_SAIDA_AUX-XBLNR_MKPF.

    LOOP AT IT_SAIDA INTO WA_SAIDA WHERE XBLNR_MKPF  = WA_SAIDA_AUX-XBLNR_MKPF.
      VSALDO = VSALDO + ( WA_SAIDA-OTORGADOK -   WA_SAIDA-DEVOLUCIONK ).
    ENDLOOP.
    IF R_HIST NE 'X' AND VSALDO = 0.
      DELETE  IT_SAIDA WHERE XBLNR_MKPF  = WA_SAIDA_AUX-XBLNR_MKPF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_SAIDA

** TOPO PAGINA
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
*      -->P_1553   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO .
  V_REPORT = SY-REPID.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = 'H'.
  IF RB1 = 'X'.
    LS_LINE-INFO = 'Otorgados'.
  ELSE.
    LS_LINE-INFO = 'Recibidos'.
  ENDIF.
  APPEND LS_LINE TO T_TOP.
  LS_LINE-TYP = 'S'.
  CONCATENATE 'fecha:' SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4)
   INTO LS_LINE-INFO SEPARATED BY '.'.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    "F_CONSTRUIR_CABECALHO
" F_CONSTRUIR_CABECALHO
