************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 13.04.2011                                          *
* Objetivo    ...: Relatorio Consulta de Entregas - INSUMOS            *
* Transação   ...: ZMM0021                                             *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 13.04.2011   Victor Hugo            Criação              DEVK915126  *
* 15.04.2011   Victor Hugo            Modificação          DEVK915161  *
* 15.04.2011   Victor Hugo            Modificação          DEVK915237  *
* 18.04.2011   Victor Hugo            Modificação          DEVK915253  *
* 19.04.2011   Victor Hugo            Modificação          DEVK915304  *
* 30.05.2011   Victor Hugo            Modificação          DEVK915404  *
************************************************************************
REPORT  ZMMR0022.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON, SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
       EKKO,
       EKPO,
       RBKP.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:

      " Cabeçalho do documento de compra
      BEGIN OF TY_EKKO,
        EBELN TYPE EKKO-EBELN, " Nº do documento de compras
        LIFNR TYPE EKKO-LIFNR, " Nº conta do fornecedor
        AEDAT TYPE EKKO-AEDAT, " Data de criação do registro
        EKGRP TYPE EKKO-EKGRP, " Grupo de compradores
        BSTYP TYPE EKKO-BSTYP, " Categoria do documento de compras
        BSART TYPE EKKO-BSART, " Tipo de documento de compras
        WAERS TYPE EKKO-WAERS,
      END OF TY_EKKO,

      " Item do documento de compras
      BEGIN OF TY_EKPO,
        EBELN TYPE EKPO-EBELN,  " Nº do documento de compras
        LOEKZ TYPE EKPO-LOEKZ,
        STATU TYPE EKPO-STATU,
        AEDAT TYPE EKPO-AEDAT,
        TXZ01 TYPE EKPO-TXZ01,
        MATNR TYPE EKPO-MATNR,
        EMATN TYPE EKPO-EMATN,
        BUKRS TYPE EKPO-BUKRS,
        WERKS TYPE EKPO-WERKS,
        EBELP TYPE EKPO-EBELP,
      END OF TY_EKPO,

      BEGIN OF TY_ESLL,
           EBELN      TYPE EKPO-EBELN,
           EBELP      TYPE EKPO-EBELP,
           PACKNO     TYPE EKPO-PACKNO,
           SUB_PACKNO TYPE ESLL-SUB_PACKNO,
           SRVPOS     TYPE ESLL-SRVPOS,
           KTEXT1     TYPE ESLL-KTEXT1,
      END OF TY_ESLL,

      " Cabeçalho doc.da fatura recebida
      BEGIN OF TY_RBKP,
        BELNR TYPE RBKP-BELNR, " Nº de um documento de faturamento
        GJAHR TYPE RBKP-GJAHR, " Exercício
        XBLNR TYPE RBKP-XBLNR, " Nº documento de referência
        BLDAT TYPE RBKP-BLDAT, " Data no documento
        BUDAT TYPE RBKP-BUDAT, " Data de lançamento no documento
        RMWWR TYPE RBKP-RMWWR, " Montante bruto de fatura em moedas de documento
        STBLG TYPE RBKP-STBLG,
      END OF TY_RBKP,

      " Centros/filiais
      BEGIN OF TY_T001W,
        NAME1 TYPE T001W-NAME1, " Nome
        WERKS TYPE T001W-WERKS, " Centro
      END OF TY_T001W,

      " Mestre de fornecedores (parte geral)
      BEGIN OF TY_LFA1,
        NAME1 TYPE LFA1-NAME1,                              " Nome 1
        LIFNR TYPE LFA1-LIFNR, " Nº conta do fornecedor
      END OF TY_LFA1,

      BEGIN OF TY_T024,
        EKGRP     TYPE T024-EKGRP,
        EKNAM     TYPE T024-EKNAM,
      END OF TY_T024,

      " Descrição de materiais
      BEGIN OF TY_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END OF TY_MAKT,

      " Estrutura de saída para a ALV.
      BEGIN OF TY_SAIDA,
        WERKS    TYPE EKPO-WERKS,  " Centro
        NAME1_TW TYPE T001W-NAME1, " Nome Centro/Filiais
        EBELN    TYPE EKKO-EBELN,  " Nº do documento de compras
        WAERS    TYPE EKKO-WAERS,  "
        AEDAT    TYPE EKKO-AEDAT,  " Data de criação do registro
        EKGRP    TYPE T024-EKGRP,
        EKNAM    TYPE T024-EKNAM,
        LIFNR    TYPE EKKO-LIFNR,  " Nº conta do fornecedor
        NAME1_LF TYPE LFA1-NAME1,  " Nome de mestre e fornecedores
        BLDAT    TYPE RBKP-BLDAT,  " Data no documento
        BUDAT    TYPE RBKP-BUDAT,  " Data de lançamento no documento
        XBLNR    TYPE RBKP-XBLNR,  " Nº documento de referência
        RMWWR    TYPE RBKP-RMWWR,  " Montante bruto de fatura em moedas de documento
        BELNR    TYPE RBKP-BELNR,  " Miro - Nº de um documento de faturamento
        GJAHR    TYPE RBKP-GJAHR,
        EBELP    TYPE EKPO-EBELP,
        MATNR    TYPE EKPO-MATNR,
        MAKTX    TYPE MAKT-MAKTX,
        LOEKZ    TYPE C LENGTH 20,
        BATXT	   TYPE BATXT,
      END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_RSEG_AUX.
        INCLUDE TYPE RSEG.
TYPES: BUZEI4 TYPE MBLPO.
TYPES: END OF TY_RSEG_AUX.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: IT_EKKO       TYPE TABLE OF TY_EKKO,  " Cabeçalho do documento de compra
      IT_EKPO       TYPE TABLE OF TY_EKPO,  " Item do documento de compras
      IT_EKBE       TYPE TABLE OF EKBE,  " Histórico para o documento de compra
      IT_ESLL       TYPE TABLE OF TY_ESLL,
      IT_ESLL_S     TYPE TABLE OF TY_ESLL,
      IT_RBKP       TYPE TABLE OF RBKP WITH HEADER LINE,  " Cabeçalho doc.da fatura recebida
      IT_RSEG       TYPE TABLE OF TY_RSEG_AUX WITH HEADER LINE,  " Cabeçalho doc.da fatura recebida
      IT_T024       TYPE TABLE OF TY_T024,  " Grupo compra
      IT_T001W      TYPE TABLE OF TY_T001W, " Centros/filiais
      IT_LFA1       TYPE TABLE OF TY_LFA1,  " Mestre de fornecedores (parte geral)
      IT_MAKT       TYPE TABLE OF TY_MAKT,  " Textos breves de material
      IT_SAIDA      TYPE TABLE OF TY_SAIDA, " Tabela Interna para saída da ALV.
      IT_SAIDA_AUX  TYPE TABLE OF TY_SAIDA, " Tabela Interna para saída da ALV.
      T_BDC         TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB     TYPE TABLE OF BDCMSGCOLL,
      IT_T161T      TYPE TABLE OF T161T WITH HEADER LINE. "Tipo de Pedido de Compra

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_EKKO      TYPE TY_EKKO,  " Cabeçalho do documento de compra
      WA_EKPO      TYPE TY_EKPO,  " Item do documento de compras
      WA_EKBE      TYPE EKBE,  " Histórico para o documento de compra
      WA_ESLL      TYPE TY_ESLL,
      WA_ESLL_S    TYPE TY_ESLL,
      WA_EKBE_AUX  TYPE EKBE,  " Histórico para o documento de compra
      WA_RBKP      TYPE RBKP,  " Cabeçalho doc.da fatura recebida
      WA_T024      TYPE TY_T024,  " Grupo compra
      WA_T001W     TYPE TY_T001W, " Centros/filiais
      WA_LFA1      TYPE TY_LFA1,  " Mestre de fornecedores (parte geral)
      WA_MAKT      TYPE TY_MAKT,  " Textos breves de material
      WA_SAIDA     TYPE TY_SAIDA, " Tabela Interna para saída da ALV.
      WA_SAIDA_AUX TYPE TY_SAIDA, " Tabela Interna para saída da ALV.
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT TYPE LVC_S_LAYO,
      WA_T161T     TYPE T161T.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: IT_FCAT TYPE TABLE OF TY_ESTRUTURA,
      S_VARIANT TYPE DISVARIANT         , " Tabela Estrutura co
      T_TOP     TYPE SLIS_T_LISTHEADER  ,
      XS_EVENTS TYPE SLIS_ALV_EVENT     ,
      EVENTS    TYPE SLIS_T_EVENT       ,
      T_PRINT   TYPE SLIS_PRINT_ALV     ,
      V_REPORT  LIKE SY-REPID           ,
      T_SORT    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      ESTRUTURA TYPE TABLE OF TY_ESTRUTURA.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: P_EBELN FOR EKKO-EBELN NO-EXTENSION NO INTERVALS, " Pedido de Compra
                P_BSTYP FOR EKKO-BSTYP NO-DISPLAY DEFAULT 'F', " Tipo de Doc. de Compra
                P_BSART FOR EKKO-BSART, " Tipo de Doc. de Compra
                P_LIFNR FOR EKKO-LIFNR NO-EXTENSION NO INTERVALS, " Fornecedor
                P_WERKS FOR EKPO-WERKS NO-EXTENSION NO INTERVALS, " Centro
                P_AEDAT FOR EKKO-AEDAT NO-EXTENSION,
                P_BUDAT FOR RBKP-BUDAT NO-EXTENSION OBLIGATORY,   " Data do Pedido
                P_BLART FOR RBKP-BLART.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_DATA_M AS CHECKBOX.
SELECTION-SCREEN COMMENT 4(50) TEXT-002.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:  F_INICIAR_VARIAVES,
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  DATA: CK_LIMPAR(1).

  CLEAR: IT_RBKP[],
         IT_RSEG[],
         IT_EKBE.

  CK_LIMPAR = ABAP_FALSE.

  IF P_BSART IS INITIAL.
    P_BSART-SIGN   = 'I'.
    P_BSART-OPTION = 'EQ'.
    P_BSART-LOW    = 'ZNB'.
    P_BSART-HIGH   = 'ZNB'.
    APPEND P_BSART.

    P_BSART-LOW    = 'ZGR'.
    P_BSART-HIGH   = 'ZGR'.
    APPEND P_BSART.

    P_BSART-LOW    = 'ZUB'.
    P_BSART-HIGH   = 'ZUB'.
    APPEND P_BSART.

    P_BSART-LOW    = 'ZGEF'.
    P_BSART-HIGH   = 'ZGEF'.
    APPEND P_BSART.

    CK_LIMPAR = ABAP_TRUE.
  ENDIF.

  IF P_BUDAT IS INITIAL .
    MESSAGE I000(Z01) WITH 'É Obrigatório informar a data de lançamento da Fatura'.
    STOP.
  ENDIF.

  IF P_EBELN IS NOT INITIAL.

    SELECT *
      FROM EKBE
      INTO TABLE IT_EKBE
     WHERE EBELN IN P_EBELN.

    IF IT_EKBE IS NOT INITIAL.
      "Selecionando Faturas
      SELECT * INTO TABLE IT_RBKP
        FROM RBKP
         FOR ALL ENTRIES IN IT_EKBE
       WHERE BUDAT IN P_BUDAT
         AND LIFNR IN P_LIFNR
         AND BELNR EQ IT_EKBE-BELNR
         AND GJAHR EQ IT_EKBE-GJAHR
         AND BLART IN P_BLART.

      CLEAR: IT_EKBE.
    ENDIF.

  ELSEIF P_AEDAT IS NOT INITIAL.
    "Busca Pedidos por Data de criação do registro

    SELECT EBELN LIFNR AEDAT EKGRP BSTYP BSART WAERS
      FROM EKKO
      INTO TABLE IT_EKKO
     WHERE BSTYP IN P_BSTYP
       AND BSART IN P_BSART
       AND AEDAT IN P_AEDAT.

    IF IT_EKKO IS NOT INITIAL.

      SELECT *
        FROM EKBE
        INTO TABLE IT_EKBE
         FOR ALL ENTRIES IN IT_EKKO
       WHERE EBELN EQ IT_EKKO-EBELN.

      IF IT_EKBE IS NOT INITIAL.

        "Selecionando Faturas
        SELECT * INTO TABLE IT_RBKP
          FROM RBKP
           FOR ALL ENTRIES IN IT_EKBE
         WHERE BUDAT IN P_BUDAT
           AND LIFNR IN P_LIFNR
           AND BELNR EQ IT_EKBE-BELNR
           AND GJAHR EQ IT_EKBE-GJAHR
           AND BLART IN P_BLART.

        CLEAR: IT_EKBE.

      ENDIF.

      CLEAR: IT_EKKO.

    ENDIF.

  ELSE.
    "Selecionando Faturas
    SELECT * INTO TABLE IT_RBKP
      FROM RBKP
     WHERE BUDAT IN P_BUDAT
       AND LIFNR IN P_LIFNR
       AND BLART IN P_BLART.
  ENDIF.

  IF IT_RBKP[] IS INITIAL .
    MESSAGE I000(Z01) WITH 'Não foi encontrado Fatura!'.
    STOP.
  ENDIF.

  SORT IT_RBKP BY BELNR GJAHR.

  "Selecionando Itens das Faturas
  SELECT *
    FROM RSEG
    INTO TABLE IT_RSEG
    FOR ALL ENTRIES IN IT_RBKP
  WHERE BELNR EQ IT_RBKP-BELNR
    AND GJAHR EQ IT_RBKP-GJAHR
    AND WERKS IN P_WERKS.

  IF ( P_WERKS IS NOT INITIAL ) AND ( IT_RSEG[] IS INITIAL ).
    MESSAGE I000(Z01) WITH 'Não foi encontrado Fatura p/ Centro Selecionado!'.
    STOP.
  ENDIF.

  SORT IT_RSEG BY BELNR GJAHR EBELN EBELP.

  "Ajustando campo Número do Item de 6 p/ 4.
  LOOP AT IT_RSEG.
    IT_RSEG-BUZEI4 = IT_RSEG-BUZEI.
    MODIFY IT_RSEG INDEX SY-TABIX TRANSPORTING BUZEI4.
  ENDLOOP.

  "Selecionando Histórico de Pedidos das Faturas
  SELECT *
    FROM EKBE
    INTO TABLE IT_EKBE
     FOR ALL ENTRIES IN IT_RSEG
   WHERE EBELN IN P_EBELN
     AND VGABE IN ('2','3')
     AND BEWTP EQ 'Q'
     AND BELNR EQ IT_RSEG-BELNR
     AND GJAHR EQ IT_RSEG-GJAHR
     AND BUZEI EQ IT_RSEG-BUZEI4.

  SORT IT_EKBE BY EBELN EBELP.

  IF ( P_EBELN IS NOT INITIAL ) AND ( IT_EKBE IS INITIAL ).
    MESSAGE I000(Z01) WITH 'Não foi encontrado Fatura p/ Pedido Selecionado!'.
    STOP.
  ENDIF.

  PERFORM LIMPA_EKBE_MOVI_ENVERSO TABLES IT_EKBE.

  IF IT_EKBE IS INITIAL.
    MESSAGE S000(Z01) WITH 'Não foram encontrados dados para os parametros' 'informados' DISPLAY LIKE 'W'.
    STOP.
  ENDIF.

  IF IT_EKBE IS NOT INITIAL.

    "Itens do Documento de Compra
    SELECT EBELN LOEKZ STATU AEDAT TXZ01 MATNR EMATN BUKRS WERKS EBELP
      FROM EKPO
      INTO TABLE IT_EKPO
      FOR ALL ENTRIES IN IT_EKBE
    WHERE EBELN EQ IT_EKBE-EBELN
      AND EBELP EQ IT_EKBE-EBELP.

    IF IT_EKPO IS NOT INITIAL.

      " Cabeçalho do documento de compra
      SELECT EBELN LIFNR AEDAT EKGRP BSTYP BSART WAERS
        FROM EKKO
        INTO TABLE IT_EKKO
         FOR ALL ENTRIES IN IT_EKPO
       WHERE EBELN EQ IT_EKPO-EBELN
         AND BSTYP IN P_BSTYP
         AND BSART IN P_BSART
         AND AEDAT IN P_AEDAT.

      SORT IT_EKKO BY EBELN.

      " Dados de desrição de materiais
      SELECT MATNR MAKTX
        INTO TABLE IT_MAKT
        FROM MAKT
         FOR ALL ENTRIES IN IT_EKPO
       WHERE MATNR = IT_EKPO-MATNR
         AND SPRAS = SY-LANGU.

      SORT IT_MAKT BY MATNR.

      " Centros/filiais
      SELECT NAME1 WERKS
        FROM T001W
        INTO TABLE IT_T001W
         FOR ALL ENTRIES IN IT_EKPO
       WHERE WERKS EQ IT_EKPO-WERKS.

      SORT IT_T001W BY WERKS.

      IF IT_EKKO IS NOT INITIAL.

        SELECT EKGRP EKNAM
          FROM T024
          INTO TABLE IT_T024
          FOR ALL ENTRIES IN IT_EKKO
          WHERE EKGRP EQ IT_EKKO-EKGRP.

        " Mestre de fornecedores (parte geral)
        SELECT NAME1 LIFNR
          FROM LFA1
          INTO TABLE IT_LFA1
          FOR ALL ENTRIES IN IT_EKKO
        WHERE LIFNR EQ IT_EKKO-LIFNR.

        SORT IT_LFA1 BY LIFNR.

        SELECT * INTO TABLE IT_T161T
          FROM T161T
           FOR ALL ENTRIES IN IT_EKKO
         WHERE SPRAS EQ SY-LANGU
           AND BSTYP EQ IT_EKKO-BSTYP
           AND BSART EQ IT_EKKO-BSART.

        SORT IT_T161T BY BSTYP BSART.

        "Busca sub_packno
        SELECT EKPO~EBELN
               EKPO~EBELP
               EKPO~PACKNO
               ESLL~SUB_PACKNO
               ESLL~SRVPOS
               ESLL~KTEXT1
          FROM EKPO
          INNER JOIN ESLL ON ESLL~PACKNO = EKPO~PACKNO
          INTO TABLE IT_ESLL
          FOR ALL ENTRIES IN IT_EKPO
          WHERE EKPO~EBELN = IT_EKPO-EBELN
          AND   EKPO~EBELP = IT_EKPO-EBELP.
        "
        IF IT_ESLL[] IS NOT INITIAL.
          SELECT PACKNO
                 PACKNO
                 PACKNO
                 SUB_PACKNO
                 SRVPOS
                 KTEXT1
        FROM ESLL
        INTO TABLE IT_ESLL_S
        FOR ALL ENTRIES IN IT_ESLL
        WHERE PACKNO = IT_ESLL-SUB_PACKNO.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF IT_EKKO IS INITIAL.
    MESSAGE I000(Z01) WITH 'Não foi encontrado Pedidos para Faturas!'.
    STOP.
  ENDIF.

  IF CK_LIMPAR EQ ABAP_TRUE.
    CLEAR: P_BSART.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM F_SAIDA.

  IF ( P_DATA_M IS INITIAL ).
    PERFORM SAIDA.
  ELSE.
    PERFORM SAIDA_DATA_MENOR.
  ENDIF.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE Z_STATUS OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " Z_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
FORM Z_LAYOUT.
  WA_LAYOUT-ZEBRA = 'X'.
ENDFORM.                    " Z_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_TAM   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C.
  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO   .
  WL_FCAT-SELTEXT_S = P_DESC    .
  WL_FCAT-SELTEXT_M = P_DESC    .
  WL_FCAT-SELTEXT_L = P_DESC    .
  WL_FCAT-HOTSPOT   = P_HOT     .
  WL_FCAT-NO_ZERO   = P_ZERO    .
  WL_FCAT-OUTPUTLEN = P_TAM     .
  WL_FCAT-LOWERCASE = 'X'.

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
            ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING
                E_OBJECT E_INTERACTIVE,

            ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING
                 E_UCOMM.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.  "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE Z_EXIBE_ALV OUTPUT.
  DATA: DTC_LAYOUT TYPE DTC_S_LAYO.

  S_VARIANT-REPORT = SY-REPID.
  IF WA_CONT IS INITIAL.

    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME              = 'CC_ALV'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
  ENDIF.
  IF WA_ALV IS INITIAL AND NOT
    WA_CONT IS INITIAL.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT          = WA_CONT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

  ENDIF.

  IF WA_EVENT IS INITIAL.

    CREATE OBJECT WA_EVENT.
    SET HANDLER: WA_EVENT->ZM_HANDLE_TOOLBAR FOR WA_ALV.
    SET HANDLER: WA_EVENT->ZM_HANDLE_USER_COMMAND FOR WA_ALV.

  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CHECK NOT WA_ALV IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE Z_USER_COMMAND INPUT.
  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM F_ALV .
  PERFORM ALV_PREENCHE_CAT USING:
        'WERKS'        TEXT-010      '6'       ' '  ' '            , " Centro
        'NAME1_TW'     TEXT-011      '30'      ' '  ' '            , " Descrição
        'EBELN'        TEXT-012      '10'      'X'  'X'            , " Pedido
        'WAERS'        TEXT-028      '05'      ' '  ' '            , " Moeda
        'BATXT'        TEXT-027      '20'      ' '  ' '            , " Tipo de documento de compra
        'MATNR'        TEXT-025      '10'      ' '  'X'            , " Material
        'MAKTX'        TEXT-026      '40'      ' '  'X'            , " Descrição
        'EKGRP'        TEXT-023      '10'      ' '  ' '            , " Comprador
        'EKNAM'        TEXT-024      '15'      ' '  ' '            , " Comprador
        'LOEKZ'        TEXT-022      '12'      ' '  ' '            , " Pedido
        'EBELP'        TEXT-021       '4'      ' '  ' '            ,
        'AEDAT'        TEXT-013      '10'      ' '  'X'            , " Dt.Pedido
        'LIFNR'        TEXT-014      '10'      ' '  ' '            , " Código
        'NAME1_LF'     TEXT-015      '35'      ' '  'X'            , " Fornecedor  " Dados da nota fiscal ate aqui.
        'BELNR'        TEXT-020      '10'      'X'  'X'            , " Miro
        'BLDAT'        TEXT-016      '10'      ' '  'X'            , " Dt.Nota
        'BUDAT'        TEXT-017      '10'      ' '  'X'            , " Dt.Lacto
        'XBLNR'        TEXT-018      '16'      ' '  'X'            , " Nro.NF.
        'RMWWR'        TEXT-019      '13'      ' '  'X'            . " Valor R$
ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
FORM F_BDC_FIELD  USING    VALUE(P_FLAG)
                           VALUE(P_FNAM)
                           VALUE(P_FVAL).

  CLEAR T_BDC.
  IF NOT P_FLAG IS INITIAL.
    T_BDC-PROGRAM  = P_FNAM.
    T_BDC-DYNPRO   = P_FVAL.
    T_BDC-DYNBEGIN = 'X'.
  ELSE.
    T_BDC-FNAM = P_FNAM.
    T_BDC-FVAL = P_FVAL.
  ENDIF.
  APPEND T_BDC.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING   P_UCOMM TYPE SYUCOMM.
  CASE P_UCOMM.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR  USING   P_OBJECT  TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                P_INTERACTIVE TYPE CHAR1 .

** Constants for button type
  CONSTANTS:
        C_BUTTON_NORMAL           TYPE I VALUE 0        ,
        C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1        ,
        C_MENU                    TYPE I VALUE 2        ,
        C_SEPARATOR               TYPE I VALUE 3        ,
        C_RADIO_BUTTON            TYPE I VALUE 4        ,
        C_CHECKBOX                TYPE I VALUE 5        ,
        C_MENU_ENTRY              TYPE I VALUE 6        .

  DATA SL_TOOLBAR TYPE STB_BUTTON.

* Append Seperator
  MOVE C_SEPARATOR  TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .

  "Vivemos esperando o dia em que seremos melhores, melhores no amor, melhores na dor, melhores em tudo"

  DATA: W_TEXTO1(10),
        W_TEXTO2(10),
        W_TEXTO3(40),
        W_PEDIDO_TEXTO(40),
        W_FORNECEDOR_TEXTO(40),
        W_CENTRO_TEXTO(40),
        PERIODO_R TYPE C LENGTH 50,
        PERIODO_O TYPE C LENGTH 50,
        PEDIDO TYPE C LENGTH 50,
        FORNECEDOR TYPE C LENGTH 200,
        CENTRO TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Pedidos de Compras X Nota Fiscal'.
  PERFORM F_CONSTRUIR_CABECALHO USING ' ' 'H' W_TEXTO3.

  IF P_EBELN-LOW IS NOT INITIAL.
    W_PEDIDO_TEXTO = 'Pedido de Compra:'.
    PEDIDO = P_EBELN-LOW.
    PERFORM F_CONSTRUIR_CABECALHO USING W_PEDIDO_TEXTO 'S' PEDIDO.
  ENDIF.

  IF P_LIFNR-LOW IS NOT INITIAL.
    W_FORNECEDOR_TEXTO = 'Fornecedor:'.
    FORNECEDOR = P_LIFNR-LOW.
    PERFORM F_CONSTRUIR_CABECALHO USING W_FORNECEDOR_TEXTO 'S' FORNECEDOR.
  ENDIF.

  IF P_WERKS-LOW IS NOT INITIAL.
    W_CENTRO_TEXTO = 'Centro:'.
    CENTRO = P_WERKS-LOW.
    PERFORM F_CONSTRUIR_CABECALHO USING W_CENTRO_TEXTO 'S' CENTRO.
  ENDIF.

  IF P_AEDAT-LOW IS NOT INITIAL.
    CONCATENATE P_AEDAT-LOW+6(2)   '.' P_AEDAT-LOW+4(2)  '.' P_AEDAT-LOW(4)  INTO W_TEXTO1.
    CONCATENATE P_AEDAT-HIGH+6(2)  '.' P_AEDAT-HIGH+4(2) '.' P_AEDAT-HIGH(4) INTO W_TEXTO2.
    W_FORNECEDOR_TEXTO = 'Período Pedido:'.
    CONCATENATE  W_TEXTO1 '-' W_TEXTO2 INTO PERIODO_R SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING W_FORNECEDOR_TEXTO 'S' PERIODO_R.
  ENDIF.

  IF P_BUDAT-LOW IS NOT INITIAL.
    CLEAR: W_TEXTO1, W_TEXTO2, PERIODO_R.
    CONCATENATE P_BUDAT-LOW+6(2)   '.' P_BUDAT-LOW+4(2)  '.' P_BUDAT-LOW(4)  INTO W_TEXTO1.
    CONCATENATE P_BUDAT-HIGH+6(2)  '.' P_BUDAT-HIGH+4(2) '.' P_BUDAT-HIGH(4) INTO W_TEXTO2.
    W_FORNECEDOR_TEXTO = 'Período Fatura:'.
    CONCATENATE  W_TEXTO1 '-' W_TEXTO2 INTO PERIODO_R SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING W_FORNECEDOR_TEXTO 'S' PERIODO_R.
  ENDIF.

  IF P_BSART IS NOT INITIAL.

    SELECT SINGLE * INTO WA_T161T
      FROM T161T
     WHERE SPRAS EQ SY-LANGU
       AND BSTYP IN P_BSTYP
       AND BSART IN P_BSART.

    W_FORNECEDOR_TEXTO = 'Tipo Doc. Compra:'.
    PERIODO_R          = WA_T161T-BATXT.
    PERFORM F_CONSTRUIR_CABECALHO USING W_FORNECEDOR_TEXTO 'S' PERIODO_R.

  ENDIF.

  IF P_DATA_M IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'Critério:' 'S' TEXT-002.
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO   USING KEY TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-KEY = KEY.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO


*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS


*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos


*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .

  IF IT_SAIDA[] IS INITIAL.
    MESSAGE S000(Z01) WITH 'Não foram encontrados dados para os parametros' 'informados' DISPLAY LIKE 'W'.
    STOP.
  ENDIF.
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
FORM F_ALV_SORT .

ENDFORM.                    " F_ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
FORM F_USER_COMMAND USING L_UCOMM  L_SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX L_SELFIELD-TABINDEX.

  CASE L_SELFIELD-FIELDNAME.

    WHEN 'EBELN'.
      SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
      CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'BELNR'.
      SET PARAMETER ID 'RBN' FIELD WA_SAIDA-BELNR.
      SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.


  ENDCASE.
ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  SAIDA_DATA_MENOR
*&---------------------------------------------------------------------*
FORM SAIDA_DATA_MENOR .

  SORT IT_T024 BY EKGRP.

  SORT: IT_ESLL   BY EBELN EBELP,
        IT_ESLL_S BY PACKNO.

  CLEAR: IT_SAIDA, WA_SAIDA.

  LOOP AT IT_EKPO INTO WA_EKPO.

    CLEAR: WA_SAIDA_AUX.

    READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN  BINARY SEARCH.

    IF ( SY-SUBRC EQ 0 ).

      READ TABLE IT_T161T WITH KEY BSTYP = WA_EKKO-BSTYP
                                   BSART = WA_EKKO-BSART.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-BATXT = IT_T161T-BATXT.
      ENDIF.

      WA_SAIDA_AUX-EBELN = WA_EKKO-EBELN.
      WA_SAIDA_AUX-AEDAT = WA_EKKO-AEDAT.
      WA_SAIDA_AUX-LIFNR = WA_EKKO-LIFNR.
      WA_SAIDA_AUX-EBELP = WA_EKPO-EBELP.
      WA_SAIDA_AUX-WERKS = WA_EKPO-WERKS.

      CASE WA_EKPO-LOEKZ.
        WHEN: 'S'.
          WA_SAIDA-LOEKZ = 'Bloqueado'.
        WHEN: 'L'.
          WA_SAIDA-LOEKZ = 'Eliminado'.
        WHEN: ''.
          WA_SAIDA-LOEKZ = 'Ativo'.
      ENDCASE.

      " Descrição de materiais
      IF WA_EKPO-MATNR IS NOT INITIAL.
        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-MATNR = WA_EKPO-MATNR.
          WA_SAIDA-MAKTX = WA_MAKT-MAKTX.
        ENDIF.
      ELSE.
        READ TABLE IT_ESLL INTO WA_ESLL WITH KEY EBELN = WA_EKPO-EBELN
                                                 EBELP = WA_EKPO-EBELP BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE IT_ESLL_S INTO WA_ESLL_S WITH KEY PACKNO = WA_ESLL-SUB_PACKNO BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-MATNR = WA_ESLL_S-SRVPOS.
            WA_SAIDA-MAKTX = WA_ESLL_S-KTEXT1.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR BINARY SEARCH.
      WA_SAIDA_AUX-NAME1_LF =  WA_LFA1-NAME1.

      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKPO-WERKS BINARY SEARCH.
      WA_SAIDA_AUX-NAME1_TW = WA_T001W-NAME1.

      LOOP AT IT_EKBE INTO WA_EKBE_AUX WHERE EBELN = WA_EKPO-EBELN
                                         AND EBELP = WA_EKPO-EBELP.

        READ TABLE IT_RBKP INTO WA_RBKP WITH KEY BELNR = WA_EKBE_AUX-BELNR
                                                 GJAHR = WA_EKBE_AUX-GJAHR
                                                 BINARY SEARCH.
        IF ( SY-SUBRC NE 0 ).
          CONTINUE.
        ENDIF.

        WA_SAIDA_AUX-BLDAT = WA_RBKP-BLDAT.
        WA_SAIDA_AUX-BUDAT = WA_RBKP-BUDAT.
        WA_SAIDA_AUX-XBLNR = WA_RBKP-XBLNR.

        CASE WA_EKBE_AUX-SHKZG.
          WHEN 'S'.
            WA_SAIDA-RMWWR = WA_EKBE_AUX-WRBTR.
          WHEN 'H'.
            WA_SAIDA-RMWWR = WA_EKBE_AUX-WRBTR * -1.
        ENDCASE.

        WA_SAIDA_AUX-BELNR = WA_RBKP-BELNR.
        WA_SAIDA_AUX-GJAHR = WA_RBKP-GJAHR.

        READ TABLE IT_T024 INTO WA_T024 WITH KEY EKGRP = WA_EKKO-EKGRP BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA_AUX-EKGRP = WA_T024-EKGRP.
          WA_SAIDA_AUX-EKNAM = WA_T024-EKNAM.
        ENDIF.

        APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
      ENDLOOP.

    ENDIF.
    CLEAR:
           WA_EKKO,
           WA_EKPO,
           WA_EKBE,
           WA_RBKP,
           WA_T001W,
           WA_LFA1,
           WA_SAIDA_AUX.

  ENDLOOP.

  CLEAR: IT_SAIDA.
  LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
    CLEAR: WA_SAIDA.
    IF  ( WA_SAIDA_AUX-AEDAT > WA_SAIDA_AUX-BUDAT ) AND ( WA_SAIDA_AUX-AEDAT NE WA_SAIDA_AUX-BUDAT ) .
      MOVE-CORRESPONDING WA_SAIDA_AUX TO WA_SAIDA.
      APPEND WA_SAIDA TO IT_SAIDA.
      CLEAR: WA_SAIDA.
    ENDIF.
    CLEAR: WA_SAIDA_AUX.
  ENDLOOP.

  DELETE IT_SAIDA WHERE BLDAT IS INITIAL.
ENDFORM.                    " SAIDA_DATA_MENOR
*&---------------------------------------------------------------------*
*&      Form  SAIDA
*&---------------------------------------------------------------------*
FORM SAIDA.

  SORT IT_T024    BY EKGRP.
  SORT: IT_ESLL   BY EBELN  EBELP,
        IT_ESLL_S BY PACKNO.

  LOOP AT IT_EKPO INTO WA_EKPO.

    READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN BINARY SEARCH.

    IF ( SY-SUBRC EQ 0 ).

      READ TABLE IT_T161T WITH KEY BSTYP = WA_EKKO-BSTYP
                                   BSART = WA_EKKO-BSART.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-BATXT = IT_T161T-BATXT.
      ENDIF.

      WA_SAIDA-EBELN = WA_EKKO-EBELN.
      WA_SAIDA-AEDAT = WA_EKKO-AEDAT.
      WA_SAIDA-LIFNR = WA_EKKO-LIFNR.
      WA_SAIDA-WERKS = WA_EKPO-WERKS.
      WA_SAIDA-EBELP = WA_EKPO-EBELP.
      WA_SAIDA-WAERS = WA_EKKO-WAERS.

      CASE WA_EKPO-LOEKZ.
        WHEN: 'S'.
          WA_SAIDA-LOEKZ = 'Bloqueado'.
        WHEN: 'L'.
          WA_SAIDA-LOEKZ = 'Eliminado'.
        WHEN: ''.
          WA_SAIDA-LOEKZ = 'Ativo'.
      ENDCASE.

      " Descrição de materiais
      IF WA_EKPO-MATNR IS NOT INITIAL.
        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-MATNR = WA_EKPO-MATNR.
          WA_SAIDA-MAKTX = WA_MAKT-MAKTX.
        ENDIF.
      ELSE.
        READ TABLE IT_ESLL INTO WA_ESLL WITH KEY EBELN = WA_EKPO-EBELN
                                                 EBELP = WA_EKPO-EBELP BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE IT_ESLL_S INTO WA_ESLL_S WITH KEY PACKNO = WA_ESLL-SUB_PACKNO BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-MATNR = WA_ESLL_S-SRVPOS.
            WA_SAIDA-MAKTX = WA_ESLL_S-KTEXT1.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR BINARY SEARCH.
      WA_SAIDA-NAME1_LF =  WA_LFA1-NAME1.

      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKPO-WERKS BINARY SEARCH.
      WA_SAIDA-NAME1_TW = WA_T001W-NAME1.

      LOOP AT IT_EKBE INTO WA_EKBE_AUX WHERE EBELN = WA_EKPO-EBELN
                                         AND EBELP = WA_EKPO-EBELP.

        READ TABLE IT_RBKP INTO WA_RBKP WITH KEY BELNR = WA_EKBE_AUX-BELNR
                                                 GJAHR = WA_EKBE_AUX-GJAHR
                                                 BINARY SEARCH.

        IF ( SY-SUBRC NE 0 ).
          CONTINUE.
        ENDIF.
        WA_SAIDA-BLDAT = WA_RBKP-BLDAT.
        WA_SAIDA-BUDAT = WA_RBKP-BUDAT.
        WA_SAIDA-XBLNR = WA_RBKP-XBLNR.

        CASE WA_EKBE_AUX-SHKZG.
          WHEN 'S'.
            WA_SAIDA-RMWWR = WA_EKBE_AUX-WRBTR.
          WHEN 'H'.
            WA_SAIDA-RMWWR = WA_EKBE_AUX-WRBTR * -1.
        ENDCASE.

        WA_SAIDA-BELNR = WA_RBKP-BELNR.
        WA_SAIDA-GJAHR = WA_RBKP-GJAHR.

        READ TABLE IT_T024 INTO WA_T024 WITH KEY EKGRP = WA_EKKO-EKGRP BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-EKGRP = WA_T024-EKGRP.
          WA_SAIDA-EKNAM = WA_T024-EKNAM.
        ENDIF.

        APPEND WA_SAIDA TO IT_SAIDA.

      ENDLOOP.

    ENDIF.
    CLEAR:
           WA_EKKO,
           WA_EKPO,
           WA_EKBE,
           WA_RBKP,
           WA_T001W,
           WA_LFA1,
           WA_SAIDA.

  ENDLOOP.


ENDFORM.                    " SAIDA

*INCLUDE ZMMR0022_01.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_EKBE_MOVI_ENVERSO
*&---------------------------------------------------------------------*
*       Elimina Saídas/Entradas
*----------------------------------------------------------------------*
*      -->P_IT_EKBE  text
*----------------------------------------------------------------------*
FORM LIMPA_EKBE_MOVI_ENVERSO  TABLES IT_EKBE_IN STRUCTURE EKBE.

  DATA: IT_EKBE_ITEM TYPE TABLE OF EKBE,
        WA_EKBE_IN   TYPE EKBE,
        WA_EKBE_ITEM TYPE EKBE,
        WA_EKBE_E    TYPE EKBE,
        WA_EKBE_S    TYPE EKBE,
        VG_SAIDA     TYPE SYSUBRC,
        VG_ENTRADA   TYPE SYSUBRC.

  LOOP AT IT_EKBE_IN INTO WA_EKBE_IN.
    APPEND WA_EKBE_IN TO IT_EKBE_ITEM.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM IT_EKBE_ITEM
         COMPARING EBELN EBELP ZEKKN VGABE MENGE WRBTR WAERS SHKZG.

  "Limpar Saídas
  "H  Crédito
  "S  Débito
  DELETE IT_EKBE_ITEM WHERE SHKZG EQ 'S'.

  DELETE ADJACENT DUPLICATES FROM IT_EKBE_ITEM
         COMPARING EBELN EBELP ZEKKN VGABE MENGE WRBTR WAERS.

  LOOP AT IT_EKBE_ITEM INTO WA_EKBE_ITEM.

    VG_SAIDA   = 0.
    VG_ENTRADA = 0.

    WHILE VG_ENTRADA IS INITIAL AND VG_SAIDA IS INITIAL.

      READ TABLE IT_EKBE_IN INTO WA_EKBE_S
      WITH KEY EBELN = WA_EKBE_ITEM-EBELN
               EBELP = WA_EKBE_ITEM-EBELP
               ZEKKN = WA_EKBE_ITEM-ZEKKN
               VGABE = WA_EKBE_ITEM-VGABE
               MENGE = WA_EKBE_ITEM-MENGE
               WRBTR = WA_EKBE_ITEM-WRBTR
               WAERS = WA_EKBE_ITEM-WAERS
               SHKZG = 'S'.

      VG_SAIDA   = SY-SUBRC.
      VG_ENTRADA = 4.

      IF SY-SUBRC IS INITIAL.

        READ TABLE IT_EKBE_IN INTO WA_EKBE_E
        WITH KEY EBELN = WA_EKBE_ITEM-EBELN
                 EBELP = WA_EKBE_ITEM-EBELP
                 ZEKKN = WA_EKBE_ITEM-ZEKKN
                 VGABE = WA_EKBE_ITEM-VGABE
                 MENGE = WA_EKBE_ITEM-MENGE
                 WRBTR = WA_EKBE_ITEM-WRBTR
                 WAERS = WA_EKBE_ITEM-WAERS
                 SHKZG = 'H'.

        VG_ENTRADA = SY-SUBRC.

        IF VG_ENTRADA IS INITIAL.
          DELETE IT_EKBE_IN WHERE EBELN EQ WA_EKBE_E-EBELN
                              AND EBELP EQ WA_EKBE_E-EBELP
                              AND ZEKKN EQ WA_EKBE_E-ZEKKN
                              AND VGABE EQ WA_EKBE_E-VGABE
                              AND GJAHR EQ WA_EKBE_E-GJAHR
                              AND BELNR EQ WA_EKBE_E-BELNR
                              AND BUZEI EQ WA_EKBE_E-BUZEI.

          DELETE IT_EKBE_IN WHERE EBELN EQ WA_EKBE_S-EBELN
                              AND EBELP EQ WA_EKBE_S-EBELP
                              AND ZEKKN EQ WA_EKBE_S-ZEKKN
                              AND VGABE EQ WA_EKBE_S-VGABE
                              AND GJAHR EQ WA_EKBE_S-GJAHR
                              AND BELNR EQ WA_EKBE_S-BELNR
                              AND BUZEI EQ WA_EKBE_S-BUZEI.
        ENDIF.
      ENDIF.
    ENDWHILE.

  ENDLOOP.

ENDFORM.                    " LIMPA_EKBE_MOVI_ENVERSO
