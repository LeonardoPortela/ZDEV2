*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 08/10/2013                                              &*
*& Descrição: Consulta Tabela Preços - Insumos                        &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZSDR0034.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_ZSDT0036,
         VAL_ATE      TYPE ZSDT0036-VAL_ATE,
         MATNR        TYPE ZSDT0036-MATNR,
         CULTURA      TYPE ZSDT0036-CULTURA,
         SAFRA        TYPE ZSDT0036-SAFRA,
         MEINS        TYPE ZSDT0036-MEINS,
         WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
         INCO1        TYPE ZSDT0036-INCO1,
         VLR_VENDA    TYPE ZSDT0036-VLR_VENDA,
         WAERK        TYPE ZSDT0036-WAERK,
         LOEKZ        TYPE ZSDT0036-LOEKZ,
         ELIMINADO    TYPE ZSDT0036-ELIMINADO,
         DTVENC       TYPE ZSDT0036-DTVENC,
       END OF TY_ZSDT0036,

       BEGIN OF TY_TVBVK,
         VKGRP  TYPE TVBVK-VKGRP,
         VKBUR  TYPE TVBVK-VKBUR,
       END OF TY_TVBVK,

       BEGIN OF TY_TVKBT,
         VKBUR TYPE TVKBT-VKBUR,
         BEZEI TYPE TVKBT-BEZEI,
       END OF TY_TVKBT,

       BEGIN OF TY_TVGRT,
         VKGRP TYPE TVGRT-VKGRP,
         BEZEI TYPE TVGRT-BEZEI,
       END OF TY_TVGRT,

       BEGIN OF TY_AUX,
         VKGRP      TYPE TVGRT-VKGRP,
         VKGRP_TEXT TYPE TVGRT-BEZEI,
         VKBUR      TYPE TVBVK-VKBUR,
         VKBUR_TEXT TYPE TVKBT-BEZEI,
       END OF TY_AUX,

       BEGIN OF TY_MAKT,
         MATNR  TYPE MAKT-MATNR,
         MAKTX  TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_MARA,
         MATNR  TYPE MARA-MATNR,
         MATKL  TYPE MARA-MATKL,
       END OF TY_MARA,

       BEGIN OF TY_ZSDT0037,
         FILIAL_ORIGEM  TYPE ZSDT0037-FILIAL_ORIGEM,
         FILIAL_DESTINO TYPE ZSDT0037-FILIAL_DESTINO,
         MEINS          TYPE ZSDT0037-MEINS,
         WAERS          TYPE ZSDT0037-WAERS,
         VAL_DE         TYPE ZSDT0037-VAL_DE,
         VAL_ATE        TYPE ZSDT0037-VAL_ATE,
         VLR_FRETE      TYPE ZSDT0037-VLR_FRETE,
       END OF TY_ZSDT0037,

       BEGIN OF TY_SAIDA,
         VAL_ATE      TYPE ZSDT0036-VAL_ATE,
         MATNR        TYPE ZSDT0036-MATNR,
         MAKTX        TYPE MAKT-MAKTX,
         CULTURA      TYPE ZSDT0036-CULTURA,
         SAFRA        TYPE ZSDT0036-SAFRA,
         MEINS        TYPE ZSDT0036-MEINS,
         WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
         INCO1        TYPE ZSDT0036-INCO1,
         MATKL        TYPE MARA-MATKL,
         DTVENC       TYPE ZSDT0036-DTVENC,
         VLR_VENDA    TYPE ZSDT0036-VLR_VENDA,
         WAERK        TYPE ZSDT0036-WAERK,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_0036   TYPE TABLE OF TY_ZSDT0036,
      T_TVBVK  TYPE TABLE OF TY_TVBVK,
      T_TVKBT  TYPE TABLE OF TY_TVKBT,
      T_TVGRT  TYPE TABLE OF TY_TVGRT,
      T_AUX    TYPE TABLE OF TY_AUX,
      T_MAKT   TYPE TABLE OF TY_MAKT,
      T_MARA   TYPE TABLE OF TY_MARA,
      T_0037   TYPE TABLE OF TY_ZSDT0037,
      T_SAIDA  TYPE TABLE OF TY_SAIDA.

DATA: T_SORT   TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_0036   TYPE TY_ZSDT0036,
      WA_TVBVK  TYPE TY_TVBVK,
      WA_TVKBT  TYPE TY_TVKBT,
      WA_TVGRT  TYPE TY_TVGRT,
      WA_AUX    TYPE TY_AUX,
      WA_MAKT   TYPE TY_MAKT,
      WA_MARA   TYPE TY_MARA,
      WA_0037   TYPE TY_ZSDT0037,
      WA_SAIDA  TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      W_PRINT_CTRL TYPE ALV_S_PCTL,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

DATA: IT_RETURN TYPE STANDARD TABLE OF DDSHRETVAL,
      WA_RETURN LIKE LINE OF IT_RETURN,
      WL_VKGRP TYPE TVBVK-VKGRP.


*----------------------------------------------------------------------*
* ESTRUTURAS PDF
*----------------------------------------------------------------------*
DATA: I_T100 TYPE T100 OCCURS 0,
      PDF LIKE TLINE OCCURS 0.
DATA: G_SPOOL TYPE TSP01-RQIDENT,
      G_PROGRAM TYPE SY-REPID VALUE SY-REPID.

*TYPE-POOLS:SLIS.
DATA: W_PRINT TYPE SLIS_PRINT_ALV.
DATA: P_FILE TYPE STRING,
      P_FILE2 TYPE RLGRAP-FILENAME.



*DATA: VARIANTE         LIKE DISVARIANT.
*DATA: GS_VARIANT_C     TYPE DISVARIANT.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_VKGRP TYPE TY_TVGRT-VKGRP OBLIGATORY.                               """ Vendedor
SELECT-OPTIONS: S_VKBUR FOR WA_TVBVK-VKBUR NO INTERVALS NO-EXTENSION OBLIGATORY,      """ Esc. Vendas
                S_WAERK FOR WA_0036-WAERK OBLIGATORY,                                 """ Moeda
                S_CULTU FOR WA_0036-CULTURA OBLIGATORY,                               """ Cultura
                S_SAFRA FOR WA_0036-SAFRA OBLIGATORY,                                 """ Safra
                S_MATNR FOR WA_0036-MATNR,                                            """ Material
                S_MATKL FOR WA_MARA-MATKL.                                            """ Grp. Mercadoria
SELECTION-SCREEN: END OF BLOCK B1.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_VKBUR-LOW.

  PERFORM SEARCH_VKBUR USING 'S_VKBUR-LOW'.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_VKBUR-HIGH.
*
*  PERFORM SEARCH_VKBUR USING 'S_VKBUR-HIGH'.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  SELECT VAL_ATE MATNR CULTURA SAFRA MEINS WERKS_FORNEC INCO1 VLR_VENDA WAERK LOEKZ ELIMINADO DTVENC
    FROM ZSDT0036
      INTO TABLE T_0036
        WHERE CULTURA IN S_CULTU
          AND SAFRA   IN S_SAFRA
          AND MATNR   IN S_MATNR
*          AND (   INCO1 EQ 'FOB'
*               OR INCO1 EQ 'CPT'
*               OR INCO1 EQ 'CIF' )
          AND WAERK IN S_WAERK
          AND LOEKZ NE 'X'
          AND ELIMINADO NE 'X'
          AND VAL_ATE GT SY-DATUM.



  IF SY-SUBRC IS INITIAL.
    SELECT MATNR MAKTX
      FROM MAKT
        INTO TABLE T_MAKT
        FOR ALL ENTRIES IN T_0036
          WHERE MATNR EQ T_0036-MATNR.

    SELECT MATNR MATKL
      FROM MARA
        INTO TABLE T_MARA
        FOR ALL ENTRIES IN T_0036
          WHERE MATNR EQ T_0036-MATNR
            AND MATKL IN S_MATKL.

    SELECT FILIAL_ORIGEM FILIAL_DESTINO MEINS WAERS VAL_DE VAL_ATE VLR_FRETE
      FROM ZSDT0037
        INTO TABLE T_0037
        FOR ALL ENTRIES IN T_0036
          WHERE FILIAL_ORIGEM   EQ T_0036-WERKS_FORNEC
            AND MEINS EQ T_0036-MEINS
            AND WAERS EQ T_0036-WAERK
            AND FILIAL_DESTINO IN S_VKBUR
            AND VAL_DE LE SY-DATUM
            AND VAL_ATE GE SY-DATUM.

  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .
  LOOP AT T_0036 INTO WA_0036.
    READ TABLE T_MAKT INTO WA_MAKT
      WITH KEY MATNR = WA_0036-MATNR.

    READ TABLE T_MARA INTO WA_MARA
      WITH KEY MATNR = WA_0036-MATNR.

    IF SY-SUBRC IS INITIAL.
      IF WA_0036-INCO1 EQ 'FOB'
        OR WA_0036-INCO1 EQ 'CPT'.

        WA_SAIDA-VLR_VENDA = WA_0036-VLR_VENDA.

      ELSEIF WA_0036-INCO1 EQ 'CIF'.

        READ TABLE T_0037 INTO WA_0037
          WITH KEY FILIAL_ORIGEM = WA_0036-WERKS_FORNEC
                   MEINS = WA_0036-MEINS
                   WAERS = WA_0036-WAERK.

        WA_SAIDA-VLR_VENDA = WA_0036-VLR_VENDA + WA_0037-VLR_FRETE.
      ENDIF.


      WA_SAIDA-DTVENC      = WA_0036-DTVENC.
      WA_SAIDA-MATNR        = WA_0036-MATNR.
      WA_SAIDA-CULTURA      = WA_0036-CULTURA.
      WA_SAIDA-SAFRA        = WA_0036-SAFRA.
      WA_SAIDA-MEINS        = WA_0036-MEINS.
      WA_SAIDA-WERKS_FORNEC = WA_0036-WERKS_FORNEC.
      WA_SAIDA-INCO1        = WA_0036-INCO1.
      WA_SAIDA-MAKTX        = WA_MAKT-MAKTX.
      WA_SAIDA-MATKL        = WA_MARA-MATKL.
      WA_SAIDA-WAERK        = WA_0036-WAERK.

      APPEND WA_SAIDA TO T_SAIDA.
    ENDIF.
    CLEAR: WA_SAIDA, WA_0037, WA_MARA, WA_MAKT.
  ENDLOOP.
ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV,
        V_NAME   TYPE STRING,
        V_PATH   TYPE STRING.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
*  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
*  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.
  PERFORM F_ALV_SORT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  T_PRINT-PRINT ='X'.
  T_PRINT-NO_PRINT_LISTINFOS = 'X'.
*  T_PRINT-NO_CHANGE_PRINT_PARAMS = 'X'.
  T_PRINT-print_ctrl-pri_params-pdest = 'Locl'.
  T_PRINT-print_ctrl-pri_params-primm = 'X'.
*  sy-batch = 'X'.
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE           = 'PRIN'
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED = 1.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
     I_CALLBACK_PROGRAM                = V_REPORT
*     IS_VARIANT                        = GS_VARIANT_C
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
     IT_FIELDCAT                       = ESTRUTURA[]
     IS_LAYOUT                         = WL_LAYOUT
     I_SAVE                            = 'A'
     IT_EVENTS                         = EVENTS
     IS_PRINT                          = T_PRINT
     IT_SORT                           = T_SORT[]
    TABLES
      T_OUTTAB                          = T_SAIDA.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*    BREAK-POINT.
    G_SPOOL = SY-SPONO.

    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
      EXPORTING
        SRC_SPOOLID = G_SPOOL
      TABLES
        PDF         = PDF.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
         EXPORTING
           DEFAULT_EXTENSION = 'PDF'
           DEFAULT_FILE_NAME = 'ITAB'
         CHANGING
           FILENAME          = V_NAME              "Filename
           PATH              =  V_PATH                " path without file name
           FULLPATH          =  P_FILE.            " Full path including file name

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          FILENAME = P_FILE
          FILETYPE = 'BIN'
        TABLES
          DATA_TAB = PDF.
      IF SY-SUBRC IS INITIAL.
        CL_GUI_FRONTEND_SERVICES=>EXECUTE( PARAMETER    = P_FILE
                                           APPLICATION  = 'AcroRd32.exe' ).
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ELSE.
*        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*          EXPORTING
*            I_CALLBACK_PROGRAM = G_PROGRAM
*            I_STRUCTURE_NAME   = 'T100'
*          TABLES
*            T_OUTTAB           = i_T100. "is_print = w_print
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
*                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.



ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.

  PERFORM MONTAR_ESTRUTURA USING:
        1  'ZSDT0036'       'DTVENC'             'T_SAIDA' 'DTVENC'          'Data Vencimento'    ' ' ,      "   Data Vencimento
        2  'ZSDT0036'       'MATNR'              'T_SAIDA' 'MATNR'           ' '                  ' ' ,      "   Material
        3  'MAKT'           'MAKTX'              'T_SAIDA' 'MAKTX'           'Descrição Material' ' ' ,      "   Desc. Material
        4  'ZSDT0036'       'CULTURA'            'T_SAIDA' 'CULTURA'         ' '                  ' ' ,      "   Cultura
        5  'ZSDT0036'       'SAFRA'              'T_SAIDA' 'SAFRA'           'Safra'              ' ' ,      "   Safra
        6  'ZSDT0036'       'MEINS'              'T_SAIDA' 'MEINS'           'UM'                 ' ' ,      "   UM
        7  'ZSDT0036'       'WERKS_FORNEC'       'T_SAIDA' 'WERKS_FORNEC'    'Centro Fornec'      ' ' ,      "   Centro Fornec
        8  'ZSDT0036'       'INCO1'              'T_SAIDA' 'INCO1'           'Frete'              ' ' ,      "   Frete
        9  'MARA'           'MATKL'              'T_SAIDA' 'MATKL'           ' '                  ' ' ,      "   Grp. Merc.
       10  'ZSDT0036'       'VLR_VENDA '         'T_SAIDA' 'VLR_VENDA '      'Vlr Venda'          ' ' ,      "   Vlr Venda
       11  'ZSDT0036'       'WAERK'              'T_SAIDA' 'WAERK'           ' '                  ' ' .      "   Moeda

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR: WA_ESTRUTURA.


  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

*  IF P_FIELD EQ 'BELNR'
*    OR P_FIELD EQ 'DOC_VARIACAO'.
*    WA_ESTRUTURA-HOTSPOT = 'X'.
*  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.
  DATA: WL_VKGRP(100),
        WL_VKBUR(100),
        TL_TVGRT TYPE TVGRT,
        TL_TVKBT TYPE TVKBT.

  SELECT SINGLE *
    FROM TVGRT
     INTO TL_TVGRT
      WHERE VKGRP EQ P_VKGRP
        AND SPRAS EQ SY-LANGU.

  SELECT SINGLE *
    FROM TVKBT
    INTO TL_TVKBT
      WHERE VKBUR IN S_VKBUR
        AND SPRAS EQ SY-LANGU.


  CONCATENATE 'Vendedor:' P_VKGRP TL_TVGRT-BEZEI INTO WL_VKGRP SEPARATED BY SPACE.
  CONCATENATE 'Escritório de Vendas:' S_VKBUR-LOW  TL_TVKBT-BEZEI INTO WL_VKBUR SEPARATED BY SPACE.
  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_VKGRP.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_VKBUR.
ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  SEARCH_VKBUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0445   text
*      <--P_S_VKBUR_LOW  text
*----------------------------------------------------------------------*
FORM SEARCH_VKBUR  USING    VALUE(P_0445).
  REFRESH: T_TVBVK, T_TVKBT, T_TVGRT, T_AUX.

  DATA: L_DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
        WL_COND(30).
  REFRESH L_DYNPFIELDS.
  CLEAR   L_DYNPFIELDS.

  L_DYNPFIELDS-FIELDNAME  = 'P_VKGRP'.
  APPEND L_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = SY-REPID
      DYNUMB     = SY-DYNNR
    TABLES
      DYNPFIELDS = L_DYNPFIELDS.
  READ TABLE L_DYNPFIELDS INDEX 1.
  MOVE L_DYNPFIELDS-FIELDVALUE TO WL_VKGRP.

  IF WL_VKGRP NE SPACE.
    WL_COND = 'VKGRP EQ WL_VKGRP'.
  ENDIF.
  SELECT *
    FROM TVBVK
      INTO CORRESPONDING FIELDS OF TABLE T_TVBVK
        WHERE (WL_COND).

  IF SY-SUBRC IS INITIAL.
    SELECT VKBUR BEZEI
      FROM TVKBT
       INTO TABLE T_TVKBT
       FOR ALL ENTRIES IN T_TVBVK
        WHERE VKBUR EQ T_TVBVK-VKBUR
          AND SPRAS EQ SY-LANGU.

    SELECT VKGRP BEZEI
      FROM TVGRT
        INTO TABLE T_TVGRT
       FOR ALL ENTRIES IN T_TVBVK
        WHERE VKGRP EQ T_TVBVK-VKGRP
          AND SPRAS EQ SY-LANGU.


  ENDIF.
*  IF WL_VKBUR IS NOT INITIAL.
*    DELETE T_AUX WHERE VKBUR NE WL_VKBUR.
*  ENDIF.

  LOOP AT T_TVBVK INTO WA_TVBVK.
    READ TABLE T_TVKBT INTO WA_TVKBT
      WITH KEY VKBUR = WA_TVBVK-VKBUR.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_TVGRT INTO WA_TVGRT
        WITH KEY VKGRP = WA_TVBVK-VKGRP.

      WA_AUX-VKGRP      = WA_TVGRT-VKGRP.
      WA_AUX-VKGRP_TEXT = WA_TVGRT-BEZEI.
      WA_AUX-VKBUR      = WA_TVKBT-VKBUR.
      WA_AUX-VKBUR_TEXT = WA_TVKBT-BEZEI.

      APPEND WA_AUX TO T_AUX.
    ENDIF.
    CLEAR: WA_AUX, WA_TVKBT, WA_TVGRT.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = 'EKKO'
      RETFIELD               = 'VKBUR'
*   PVALKEY                = ' '
    DYNPPROG               = SY-REPID
    DYNPNR                 = SY-DYNNR
   DYNPROFIELD            = P_0445
*   STEPL                  = 0
*      WINDOW_TITLE           = 'VKBUR Records'
*   VALUE                  = '001'
      VALUE_ORG              = 'S'
*    MULTIPLE_CHOICE        = 'X'  "allows you select multiple entries from the popup
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
*   MARK_TAB               =
* IMPORTING
*   USER_RESET             = ld_ret
    TABLES
      VALUE_TAB              = T_AUX
*    FIELD_TAB              = lt_field
      RETURN_TAB             = IT_RETURN
*   DYNPFLD_MAPPING        =
   EXCEPTIONS
     PARAMETER_ERROR        = 1
     NO_VALUES_FOUND        = 2
     OTHERS                 = 3.

*  READ TABLE IT_RETURN INT WA_RETURN INDEX 1.
*  P_VKBUR = WA_RETURN-FIELDVAL.O

ENDFORM.                    " SEARCH_VKBUR
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .
  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'MATKL'.
  T_SORT-SUBTOT    = 'X'.
  T_SORT-SPOS      = 1.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.

  CLEAR T_SORT.
  T_SORT-FIELDNAME = 'MAKTX'.
  T_SORT-SUBTOT    = 'X'.
  T_SORT-SPOS      = 2.
  T_SORT-UP        = 'X'.
  APPEND T_SORT.
ENDFORM.                    " F_ALV_SORT
