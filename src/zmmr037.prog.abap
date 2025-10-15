*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 14/10/2013                                              &*
*& Descrição: Estoque de segurança para MRP                           &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZMMR037.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_T001K,
        BUKRS TYPE T001K-BUKRS,
        BWKEY TYPE T001K-BWKEY,
       END OF TY_T001K,

      BEGIN OF TY_MARC,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
        DISMM TYPE MARC-DISMM,
        EISBE TYPE MARC-EISBE,
       END OF TY_MARC,

       BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
       END OF TY_MARA,

       BEGIN OF TY_MAPR,
        MATNR TYPE MAPR-MATNR,
        WERKS TYPE MAPR-WERKS,
        PNUM1 TYPE MAPR-PNUM1,
       END OF TY_MAPR,

       BEGIN OF TY_PROP,
        PNUM1 TYPE PROP-PNUM1,
        PNUM2 TYPE PROP-PNUM2,
        GWERT TYPE PROP-GWERT,
        PRDAT TYPE PROP-PRDAT,
       END OF TY_PROP,

       " Batch input nova estrutura do campo de tabela
       BEGIN OF TY_BDCDATA,
        PROGRAM   TYPE BDCDATA-PROGRAM,  " Pool de módulos BDC
        DYNPRO    TYPE BDCDATA-DYNPRO,   " Número de tela BDC
        DYNBEGIN  TYPE BDCDATA-DYNBEGIN, " Início BDC de uma tela
        FNAM      TYPE BDCDATA-FNAM,     " Nome do campo
        FVAL      TYPE BDCDATA-FVAL,     " Valor do campo BDC
       END OF TY_BDCDATA,

       BEGIN OF TY_MESSAGE,
         MATNR     TYPE MARC-MATNR,         " Código do material
         WERKS     TYPE MARC-WERKS,         " Centro
         EISBE(13) TYPE I,
         MSGTY     TYPE MESSAGE-MSGTY,      " Tipo da mensagem
         MSGNO     TYPE MESSAGE-MSGNO,      " Numero da mensagem
         MSGTX     TYPE MESSAGE-MSGTX,      " Descrição da mensagem
       END OF   TY_MESSAGE,

       BEGIN OF TY_SAIDA,
         MATNR     TYPE MARC-MATNR,
         WERKS     TYPE MARC-WERKS,
         EISBE(13) TYPE I,
         MTART     TYPE MARA-MTART,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.
*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_T001K       TYPE TABLE OF TY_T001K,
      T_MARC        TYPE TABLE OF TY_MARC,
      T_MARA        TYPE TABLE OF TY_MARA,
      T_MAPR        TYPE TABLE OF TY_MAPR,
      T_PROP        TYPE TABLE OF TY_PROP,
      T_BDCDATA     TYPE TABLE OF TY_BDCDATA,
      T_MSG         TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      T_MESSAGE     TYPE TABLE OF TY_MESSAGE,
      T_SAIDA       TYPE TABLE OF TY_SAIDA.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_T001K       TYPE TY_T001K,
      WA_MARC        TYPE TY_MARC,
      WA_MARA        TYPE TY_MARA,
      WA_MAPR        TYPE TY_MAPR,
      WA_PROP        TYPE TY_PROP,
      WA_BDCDATA     TYPE TY_BDCDATA,
      WA_MESSAGE     TYPE TY_MESSAGE,
      WA_PARAMS      TYPE CTU_PARAMS,
      WA_SAIDA       TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: WG_MODE(1) TYPE C VALUE 'N', " informa o Modo do Call Transaction
      WG_S       TYPE C VALUE 'S', " Informa o Update do call Transaction
      MENSG      LIKE MESSAGE VALUE IS INITIAL, " variavel que recebe retorno
      WG_MSGNO   LIKE SY-MSGNO.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

*DATA: VARIANTE         LIKE DISVARIANT.
*DATA: GS_VARIANT_C     TYPE DISVARIANT.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR WA_MARC-MATNR,                                        """ Material
                S_WERKS FOR WA_MARC-WERKS,                                        """ Centro
                S_BUKRS FOR WA_T001K-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION.  """Empresa
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM MONTA_SHDB.
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
  SELECT BUKRS BWKEY
    FROM T001K
      INTO TABLE T_T001K
        WHERE BUKRS IN S_BUKRS
          AND BWKEY IN S_WERKS.

  IF SY-SUBRC IS INITIAL.
    SELECT MATNR WERKS DISMM EISBE
      FROM MARC
        INTO TABLE T_MARC
        FOR ALL ENTRIES IN T_T001K
          WHERE WERKS EQ T_T001K-BWKEY
            AND MATNR IN S_MATNR
            AND DISMM NE SPACE
            AND DISMM NE 'ND'    .

    IF SY-SUBRC IS INITIAL.

      SELECT MATNR MTART
        FROM MARA
          INTO TABLE T_MARA
          FOR ALL ENTRIES IN T_MARC
            WHERE MATNR EQ T_MARC-MATNR.

      SELECT MATNR WERKS PNUM1
        FROM MAPR
          INTO  TABLE T_MAPR
          FOR ALL ENTRIES IN T_MARC
            WHERE MATNR EQ T_MARC-MATNR
              AND WERKS EQ T_MARC-WERKS.

      IF  SY-SUBRC IS INITIAL.
        SELECT PNUM1 PNUM2 GWERT PRDAT
          FROM PROP
            INTO TABLE T_PROP
            FOR ALL ENTRIES IN T_MAPR
              WHERE PNUM1 EQ T_MAPR-PNUM1.

      ENDIF.
    ENDIF.
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
  DATA: WL_EISBE(13) TYPE I.

  SORT: T_PROP BY PRDAT DESCENDING,
        T_MAPR BY MATNR WERKS.

  LOOP AT T_MARC INTO WA_MARC.

    READ TABLE T_T001K INTO WA_T001K
      WITH KEY BWKEY = WA_MARC-WERKS.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_MAPR INTO WA_MAPR
        WITH KEY MATNR = WA_MARC-MATNR
                 WERKS = WA_MARC-WERKS
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        READ TABLE T_PROP INTO WA_PROP
          WITH KEY PNUM1 = WA_MAPR-PNUM1.

        IF SY-SUBRC IS INITIAL.
          READ TABLE T_MARA INTO WA_MARA
            WITH KEY MATNR = WA_MARC-MATNR.
          WA_SAIDA-MTART = WA_MARA-MTART.
          WA_SAIDA-EISBE = WA_PROP-GWERT.
          WA_SAIDA-MATNR = WA_MARC-MATNR.
          WA_SAIDA-WERKS = WA_MARC-WERKS.

          APPEND WA_SAIDA TO T_SAIDA.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: WA_SAIDA, WA_MAPR, WA_PROP, WL_EISBE.
  ENDLOOP.

ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_SHDB .
  DATA: TL_BTCI TYPE STANDARD TABLE OF BDCDATA WITH HEADER LINE,
        WL_KZSEL(20) TYPE C.

  WA_PARAMS-DISMODE = 'N'.
  WA_PARAMS-UPDMODE = 'S'.
  WA_PARAMS-DEFSIZE = 'X'.

  LOOP AT T_SAIDA INTO WA_SAIDA.
    REFRESH: T_BDCDATA, T_MSG, TL_BTCI.
    CLEAR: WL_KZSEL.
    PERFORM Z_PREENCHE_BDC USING:

      'X'    'SAPLMGMM'       '0060',
      ' '    'BDC_OKCODE'     '=ENTR',
      ' '    'RMMG1-MATNR'    WA_SAIDA-MATNR.

    CALL FUNCTION 'MATERIAL_BTCI_SELECTION_NEW'
      EXPORTING
        MATERIAL                  = WA_SAIDA-MATNR
        MATERIALART               = WA_SAIDA-MTART
        SELECTION                 = 'D'
        TCODE                     = 'MM02'
      TABLES
        BTCI_D0070                = TL_BTCI
      EXCEPTIONS
        MATERIAL_NOT_FOUND        = 1
        MATERIAL_NUMBER_MISSING   = 2
        MATERIAL_TYPE_MISSING     = 3
        MATERIAL_TYPE_NOT_FOUND   = 4
        NO_ACTIVE_DYNPRO_SELECTED = 5
        NO_AUTHORITY              = 6
        OTHERS                    = 7.

    IF SY-SUBRC IS NOT INITIAL.
      CALL FUNCTION 'WRITE_MESSAGE'
          EXPORTING
              MSGID         = SY-MSGID
              MSGNO         = SY-MSGNO
              MSGTY         = SY-MSGTY
              MSGV1         = SY-MSGV1
              MSGV2         = SY-MSGV2
              MSGV3         = SY-MSGV3
              MSGV4         = SY-MSGV4
              MSGV5         = ' '
          IMPORTING
*       ERROR         =
             MESSG         = MENSG
*       MSGLN         =
                         .

      WA_MESSAGE-MATNR   = WA_SAIDA-MATNR.
      WA_MESSAGE-WERKS   = WA_SAIDA-WERKS.
      WA_MESSAGE-EISBE   = WA_SAIDA-EISBE.
      WA_MESSAGE-MSGTY   = MENSG-MSGTY.
      WA_MESSAGE-MSGNO   = MENSG-MSGNO.
      WA_MESSAGE-MSGTX   = MENSG-MSGTX.

      " popula a tabela principal de mensagem que será o Log de erro.
      APPEND WA_MESSAGE TO T_MESSAGE.
      CLEAR: WA_MESSAGE.

      CONTINUE.
    ENDIF.

    DELETE TL_BTCI[] WHERE FVAL NE 'X'.

    READ TABLE TL_BTCI INDEX 2.
    CONCATENATE 'MSICHTAUSW-KZSEL' '(' TL_BTCI-FNAM+17(2) ')'  INTO WL_KZSEL.


    PERFORM Z_PREENCHE_BDC USING:

      'X'    'SAPLMGMM'             '0070',
      ' '    'BDC_OKCODE'           '=ENTR',
      ' '    WL_KZSEL                'X'.

    PERFORM Z_PREENCHE_BDC USING:

      'X'    'SAPLMGMM'       '0080',
      ' '    'BDC_OKCODE'     '=ENTR',
      ' '    'RMMG1-WERKS'    WA_SAIDA-WERKS.

    PERFORM Z_PREENCHE_BDC USING:

      'X'    'SAPLMGMM'       '4000',
      ' '    'BDC_OKCODE'     '=BU',
      ' '    'MARC-EISBE'    WA_SAIDA-EISBE.

    CALL TRANSACTION 'MM02' USING  T_BDCDATA
                            OPTIONS FROM WA_PARAMS
                            MESSAGES INTO T_MSG.

    PERFORM Z_IMPRIME_MENSAGEM.

    CLEAR T_BDCDATA.
  ENDLOOP.

ENDFORM.                    " MONTA_SHDB
*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0349   text
*      -->P_0350   text
*      -->P_0351   text
*----------------------------------------------------------------------*
FORM Z_PREENCHE_BDC  USING DYNBEGIN
                           NAME
                           VALUE.
  IF DYNBEGIN = 'X'.
    MOVE: NAME      TO WA_BDCDATA-PROGRAM,
          VALUE     TO WA_BDCDATA-DYNPRO,
          DYNBEGIN  TO WA_BDCDATA-DYNBEGIN.
    APPEND WA_BDCDATA TO T_BDCDATA.
  ELSE.

    MOVE: NAME  TO WA_BDCDATA-FNAM,
          VALUE TO WA_BDCDATA-FVAL.

    CONDENSE WA_BDCDATA-FVAL NO-GAPS.

    APPEND WA_BDCDATA TO T_BDCDATA.
  ENDIF.
  CLEAR: WA_BDCDATA.

ENDFORM.                    " Z_PREENCHE_BDC
*&---------------------------------------------------------------------*
*&      Form  Z_IMPRIME_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_IMPRIME_MENSAGEM .
  LOOP AT T_MSG
    WHERE MSGTYP EQ 'S'
       OR MSGTYP EQ 'E'.
    WG_MSGNO = T_MSG-MSGNR.
    "  Function que faz mostrar a mensagem
    CALL FUNCTION 'WRITE_MESSAGE'
      EXPORTING
        MSGID         = T_MSG-MSGID
        MSGNO         = WG_MSGNO
        MSGTY         = T_MSG-MSGTYP
        MSGV1         = T_MSG-MSGV1
        MSGV2         = T_MSG-MSGV2
        MSGV3         = T_MSG-MSGV3
        MSGV4         = T_MSG-MSGV4
        MSGV5         = ' '
     IMPORTING
*       ERROR         =
        MESSG         = MENSG
*       MSGLN         =
                        .

    WA_MESSAGE-MATNR   = WA_SAIDA-MATNR.
    WA_MESSAGE-WERKS   = WA_SAIDA-WERKS.
    WA_MESSAGE-EISBE   = WA_SAIDA-EISBE.
    WA_MESSAGE-MSGTY   = MENSG-MSGTY.
    WA_MESSAGE-MSGNO   = MENSG-MSGNO.
    WA_MESSAGE-MSGTX   = MENSG-MSGTX.

    " popula a tabela principal de mensagem que será o Log de erro.
    APPEND WA_MESSAGE TO T_MESSAGE.
    CLEAR: WA_MESSAGE.

  ENDLOOP.
*   WRITE: / , WA_SAIDA-MATNR ,
*              mensg-msgtx,
*              mensg-msgty
*           .


ENDFORM.                    " Z_IMPRIME_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
*  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
*  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

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

    TABLES
      T_OUTTAB                          = T_MESSAGE.



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
        1  'MARC'       'MATNR'            'T_MESSAGE' 'MATNR'   ' '                    ' ' ,      "   Material
        2  'MARC'       'WERKS'            'T_MESSAGE' 'WERKS'   ' '                    ' ' ,      "   Centro
        3  ' '          ' '                'T_MESSAGE' 'EISBE'   'Estoque de segurança' ' ' ,      "   Estoque de segurança
        4  'MENSG'      'MSGTY'            'T_MESSAGE' 'MSGTY'   'Tipo da mensagem'     ' ' ,      "   Tipo da mensagem
        5  'MENSG'      'MSGTX'            'T_MESSAGE' 'MSGTX'   'Mensagem'             ' ' .      "   Mensagem


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


  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
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

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.

ENDFORM.                    " INICIAR_VARIAVES
