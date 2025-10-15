************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 20.06.2013                                          *
* Objetivo    ...: Cadastro de Preço - Aquaviário                      *
* Autor       ...: Victor Hugo                                         *
* Transação   ...: ZLES0075                                            *
************************************************************************
REPORT  ZLESR0072.

*------------------------------------------------------------------------
* TABLES
*------------------------------------------------------------------------
TABLES: ZLEST0055.

**------------------------------------------------------------------------
**  TYPES
**------------------------------------------------------------------------
TYPES:
       BEGIN OF TY_T023,
        MATKL    TYPE T023T-MATKL,
        WGBEZ60  TYPE T023T-WGBEZ60,
       END OF TY_T023,

       BEGIN OF TY_SAIDA,
         ZID_CP        TYPE ZLEST0055-ZID_CP,
         STATUS        TYPE C LENGTH 4,
         STATUS_AUX    TYPE C,
         VKORG         TYPE ZLEST0055-VKORG,
         VTWEG         TYPE ZLEST0055-VTWEG,
         SPART         TYPE ZLEST0055-SPART,
         DT_INICIO     TYPE ZLEST0055-DT_INICIO  ,
         DT_FIM        TYPE ZLEST0055-DT_FIM     ,
         AUART         TYPE ZLEST0055-AUART      ,
         MATKL         TYPE ZLEST0055-MATKL      ,
         WGBEZ60       TYPE T023T-WGBEZ60,
         KUNNR         TYPE ZLEST0055-KUNNR      ,
         NAME1         TYPE KNA1-NAME1,
         WAERK         TYPE ZLEST0055-WAERK      ,
         WAERK_FATURA  TYPE ZLEST0055-WAERK_FATURA,
         TP_TRANSGENIA  TYPE ZLEST0055-TP_TRANSGENIA,
         ZTERM         TYPE ZLEST0055-ZTERM,
         ZTERM2        TYPE ZLEST0055-ZTERM2,
         VKAUS         TYPE C LENGTH 23,
         NETPR         TYPE ZLEST0055-NETPR      ,
         UNID_MEDIDA   TYPE ZLEST0055-UNID_MEDIDA,
         USNAM         TYPE SY-UNAME,
         DATA_ATUAL    TYPE SY-DATUM,
         HORA_ATUAL    TYPE SY-UZEIT,
         LINHA_COR     TYPE C LENGTH 4,
         KURST         TYPE TCURR-KURST,
         PO_EMBARQUE   TYPE LIFNR,
         PO_DESTINO    TYPE KUNNR,
         OPERACAO      TYPE ZLEST0055-OPERACAO,
       END OF TY_SAIDA.

*------------------------------------------------------------------------
* VARIABLES
*------------------------------------------------------------------------
DATA: OK_CODE TYPE SY-UCOMM,
      W_EDIT  TYPE C,
      W_VLD_M TYPE C,
      GS_ID   TYPE ZLEST0055-ZID_CP.

"Campos
DATA: W_DT_INICIO  TYPE SY-DATUM,
      W_DT_FIM     TYPE SY-DATUM,
      W_AUART      TYPE ZLEST0055-AUART,
      W_DESC_AUART TYPE TVAKT-BEZEI,
      W_MATKL      TYPE ZLEST0055-MATKL,
      W_DESC_MAKTL TYPE T023T-WGBEZ60,
      W_KUNNR      TYPE ZLEST0055-KUNNR,
      W_DESC_KUNNR TYPE KNA1-NAME1,
      W_VKORG      TYPE ZLEST0055-VKORG,
      W_DESC_VKORG TYPE TVKOT-VTEXT,
      W_VTWEG      TYPE ZLEST0055-VTWEG,
      W_DESC_VTWEG TYPE TVTWT-VTEXT,
      W_SPART      TYPE ZLEST0055-SPART,
      W_DESC_SPART TYPE TSPAT-VTEXT,
      W_WAERK      TYPE ZLEST0055-WAERK,
      W_OPERACAO   TYPE ZLEST0055-OPERACAO,
      W_WAERK_FATURA TYPE ZLEST0055-WAERK,
      W_TP_TRANSGENIA TYPE ZLEST0055-TP_TRANSGENIA,
      W_NETPR      TYPE ZLEST0055-NETPR,
      W_UNID       TYPE ZLEST0055-UNID_MEDIDA,
      W_ZTERM      TYPE ZLEST0055-ZTERM,
      W_ZTERM2     TYPE ZLEST0055-ZTERM2,
      W_VKAUS      TYPE TVLV-ABRVW,
      W_KURST      TYPE TCURR-KURST,
      W_EMBARQUE   TYPE LIFNR,
      W_DESTINO    TYPE KUNNR.

"Campos da Dialog
DATA: WD_DT_INICIO   TYPE SY-DATUM,
      WD_DT_FIM      TYPE SY-DATUM,
      WD_AUART       TYPE ZLEST0055-AUART,
      WD_MATKL       TYPE ZLEST0055-MATKL,
      WD_KUNNR       TYPE ZLEST0055-KUNNR,
      WD_VKORG       TYPE ZLEST0055-VKORG,
      WD_VTWEG       TYPE ZLEST0055-VTWEG,
      WD_SPART       TYPE ZLEST0055-SPART,
      WD_WAERK       TYPE ZLEST0055-WAERK,
      WD_WAERK_FATURA TYPE ZLEST0055-WAERK_FATURA,
      WD_OPERACAO      TYPE ZLEST0055-OPERACAO,
      WD_TP_TRANSGENIA TYPE ZLEST0055-TP_TRANSGENIA,
      WD_NETPR       TYPE ZLEST0055-NETPR,
      WD_UNID        TYPE ZLEST0055-UNID_MEDIDA,
      WD_ZTERM       TYPE ZLEST0055-ZTERM,
      WD_ZTERM2      TYPE ZLEST0055-ZTERM2,
      WD_ID          TYPE NUMC10,
      WD_USUARIO     TYPE SY-UNAME,
      WD_DT_CADASTRO TYPE SY-DATUM,
      WD_HR_CADASTRO TYPE SY-UZEIT.


DATA: IT_SEL_ROWS         TYPE LVC_T_ROW,
      WA_SEL_ROWS         TYPE LVC_S_ROW.

*------------------------------------------------------------------------
* ALV
*------------------------------------------------------------------------
DATA: OBG_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID          TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCATALOG   TYPE LVC_T_FCAT,
      WA_FCATALOG   TYPE LVC_S_FCAT.

DATA:   GS_VARIANT   TYPE DISVARIANT,
        GS_LAYOUT    TYPE LVC_S_LAYO,
        GS_STABLE    TYPE LVC_S_STBL.


DATA: WA_FCODE TYPE SY-UCOMM,
      IT_FCODE LIKE TABLE OF WA_FCODE.

*------------------------------------------------------------------------
* INTERNAL TABLE AND WORK AREA
*------------------------------------------------------------------------
DATA: IT_SAIDA     TYPE TABLE OF TY_SAIDA,
      WA_SAIDA     TYPE TY_SAIDA,
      IT_T023T     TYPE TABLE OF T023T WITH HEADER LINE,
      WA_T023T     TYPE T023T,
      IT_ZLEST0055 TYPE TABLE OF ZLEST0055 WITH HEADER LINE,
      WA_ZLEST0055 TYPE ZLEST0055,
      IT_KNA1      TYPE TABLE OF KNA1 WITH HEADER LINE,
      WA_KNA1      TYPE KNA1,
      IT_TVLV      TYPE TABLE OF TVLV,
      WA_TVLV      TYPE TVLV,
      IT_TVLVT     TYPE TABLE OF TVLVT,
      WA_TVLVT     TYPE TVLVT.

*------------------------------------------------------------------------
* SELECT BEGIN
*------------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_MATKL   FOR ZLEST0055-MATKL, "Grupo de Mercadoria
                P_KUNNR   FOR ZLEST0055-KUNNR, "Cliente
                P_AUART   FOR ZLEST0055-AUART, "Tipo de Ordem
                P_VKORG   FOR ZLEST0055-VKORG, "Organização de venda
                P_EMBA    FOR ZLEST0055-PO_EMBARQUE, "Porto de Embarque
                P_DEST    FOR ZLEST0055-PO_DESTINO. "Porto de Destino.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: R_ATIV RADIOBUTTON GROUP R1 DEFAULT 'X', "Status Ativo.
            R_DESA RADIOBUTTON GROUP R1. "Status Desativado.
SELECTION-SCREEN: END OF BLOCK B2.

START-OF-SELECTION.
  PERFORM: SELECIONAR_DADOS.
  CALL SCREEN 0100.

*----------------------------------------------------------------------*
*  MODULE PBO_0100 OUTPUT
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.



  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

  CASE W_EDIT.

    WHEN: 'X'.
      LOOP AT SCREEN.

        IF ( SCREEN-NAME  = 'W_DT_INICIO') OR
           ( SCREEN-NAME  = 'W_DT_FIM'   ) OR
           ( SCREEN-NAME  = 'W_AUART'    ) OR
           ( SCREEN-NAME  = 'W_MATKL'    ) OR
           ( SCREEN-NAME  = 'W_TP_TRANSGENIA' ) OR
           ( SCREEN-NAME  = 'W_OPERACAO' ) OR
           ( SCREEN-NAME  = 'W_KUNNR'    ) OR
           ( SCREEN-NAME  = 'W_VKORG'    ) OR
           ( SCREEN-NAME  = 'W_VTWEG'    ) OR
           ( SCREEN-NAME  = 'W_SPART'    ).

          SCREEN-OUTPUT = '1'.
          SCREEN-INPUT  = '1'.
          MODIFY SCREEN.
        ELSEIF ( SCREEN-NAME  = 'BTN_ADD'  ) OR
               ( SCREEN-NAME  = 'W_WAERK'  ) OR
               ( SCREEN-NAME  = 'W_WAERK_FATURA' ) OR
               ( SCREEN-NAME  = 'W_NETPR'  ) OR
               ( SCREEN-NAME  = 'W_UNID'   ) OR
               ( SCREEN-NAME  = 'W_ZTERM'  ) OR
               ( SCREEN-NAME  = 'W_ZTERM2' ) OR
               ( SCREEN-NAME  = 'W_KURST'  ).

          SCREEN-OUTPUT = '1'.
          SCREEN-INPUT  = '0  '.

          MODIFY SCREEN.

        ENDIF.


      ENDLOOP.

      CLEAR: W_EDIT.

    WHEN: 'E'.

      IF NOT ( W_WAERK IS INITIAL ) AND ( NOT W_NETPR IS INITIAL ) AND ( NOT W_UNID  IS INITIAL ).

        LOOP AT SCREEN.

          IF ( SCREEN-NAME  = 'W_WAERK'    ) OR
             ( SCREEN-NAME  = 'W_WAERK_FATURA' ) OR
             ( SCREEN-NAME  = 'W_NETPR'    ) OR
             ( SCREEN-NAME  = 'W_UNID'     ) OR
             ( SCREEN-NAME  = 'W_DT_INICIO') OR
             ( SCREEN-NAME  = 'W_DT_FIM'   ) OR
             ( SCREEN-NAME  = 'W_AUART'    ) OR
             ( SCREEN-NAME  = 'W_MATKL'    ) OR
             ( SCREEN-NAME  = 'W_TP_TRANSGENIA' ) OR
             ( SCREEN-NAME  = 'W_KUNNR'    ) OR
             ( SCREEN-NAME  = 'W_VKORG'    ) OR
             ( SCREEN-NAME  = 'W_VTWEG'    ) OR
             ( SCREEN-NAME  = 'W_SPART'    ) OR
             ( SCREEN-NAME  = 'W_OPERACAO' ) OR
             ( SCREEN-NAME  = 'W_ZTERM'    ) OR
             ( SCREEN-NAME  = 'W_ZTERM2'   ) OR
             ( SCREEN-NAME  = 'W_VKAUS'    ) OR
             ( SCREEN-NAME  = 'W_KURST'    ).


            SCREEN-OUTPUT = '1'.
            SCREEN-INPUT  = '0'.
            MODIFY SCREEN.

          ELSEIF ( SCREEN-NAME = 'BTN_ADD' ).
            SCREEN-OUTPUT = '1'.
            SCREEN-INPUT  = '1'.
            MODIFY SCREEN.
          ENDIF.


        ENDLOOP.

      ELSE.

        LOOP AT SCREEN.

          IF ( SCREEN-NAME  = 'BTN_ADD'    ) OR
             ( SCREEN-NAME  = 'W_DT_INICIO') OR
             ( SCREEN-NAME  = 'W_DT_FIM'   ) OR
             ( SCREEN-NAME  = 'W_AUART'    ) OR
             ( SCREEN-NAME  = 'W_MATKL'    ) OR
             ( SCREEN-NAME  = 'W_TP_TRANSGENIA' ) OR
             ( SCREEN-NAME  = 'W_KUNNR'    ) OR
             ( SCREEN-NAME  = 'W_VKORG'    ) OR
             ( SCREEN-NAME  = 'W_OPERACAO' ) OR
             ( SCREEN-NAME  = 'W_VTWEG'    ) OR
             ( SCREEN-NAME  = 'W_SPART'    ) OR
             ( SCREEN-NAME  = 'W_VKAUS'    ) OR
             ( SCREEN-NAME  = 'W_EMBARQUE'    ) OR
             ( SCREEN-NAME  = 'W_DESTINO'    ).

            SCREEN-OUTPUT = '1'.
            SCREEN-INPUT  = '0'.
            MODIFY SCREEN.
          ELSEIF ( SCREEN-NAME  = 'W_WAERK' ) OR
                 ( SCREEN-NAME  = 'W_WAERK_FATURA' ) OR
                 ( SCREEN-NAME  = 'W_NETPR' ) OR
                 ( SCREEN-NAME  = 'W_UNID'  ) OR
                 ( SCREEN-NAME  = 'W_ZTERM' ) OR
                 ( SCREEN-NAME  = 'W_ZTERM2' ) OR
                 ( SCREEN-NAME  = 'W_KURST' ).

            SCREEN-OUTPUT = '1'.
            SCREEN-INPUT  = '1'.
            MODIFY SCREEN.
            W_VLD_M = 'S'.
          ENDIF.


        ENDLOOP.

      ENDIF.

      CLEAR: W_EDIT.
      PERFORM: INCLUIR_DESCRICAO.

    WHEN OTHERS.

      LOOP AT SCREEN.

        IF ( SCREEN-NAME  = 'BTN_ADD'    ) OR
           ( SCREEN-NAME  = 'W_DT_INICIO') OR
           ( SCREEN-NAME  = 'W_DT_FIM'   ) OR
           ( SCREEN-NAME  = 'W_AUART'    ) OR
           ( SCREEN-NAME  = 'W_MATKL'    ) OR
           ( SCREEN-NAME  = 'W_TP_TRANSGENIA' ) OR
           ( SCREEN-NAME  = 'W_KUNNR'    ) OR
           ( SCREEN-NAME  = 'W_VKORG'    ) OR
           ( SCREEN-NAME  = 'W_VTWEG'    ) OR
           ( SCREEN-NAME  = 'W_SPART'    ) OR
           ( SCREEN-NAME  = 'W_OPERACAO' ) OR
           ( SCREEN-NAME  = 'W_WAERK'    ) OR
           ( SCREEN-NAME  = 'W_WAERK_FATURA' ) OR
           ( SCREEN-NAME  = 'W_NETPR'    ) OR
           ( SCREEN-NAME  = 'W_UNID'     ) OR
           ( SCREEN-NAME  = 'W_ZTERM'    ) OR
           ( SCREEN-NAME  = 'W_ZTERM2'   ) OR
           ( SCREEN-NAME  = 'W_VKAUS'    ) OR
           ( SCREEN-NAME  = 'W_KURST'    ) OR
           ( SCREEN-NAME  = 'W_EMBARQUE' ) OR
           ( SCREEN-NAME  = 'W_DESTINO' ).


          SCREEN-OUTPUT = '1'.
          SCREEN-INPUT  = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

  ENDCASE.

  "CLEAR: IT_FCODE[].


ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0100 INPUT.

  CASE OK_CODE.

    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'NOVO'.
      W_EDIT  = 'X'.
      W_VLD_M = ''.
      LEAVE TO SCREEN 0100.
    WHEN: 'ENTER'.
      PERFORM: VALIDACAO_CAMPOS.
      W_EDIT  = 'E'.
      LEAVE TO SCREEN 0100.
    WHEN: 'BTN_ADD'.
      PERFORM: ADICIONAR_LINHA.
      PERFORM: LIMPAR_CAMPOS.
    WHEN: 'SALVAR'.
      PERFORM: SALVAR_REGISTRO.
      PERFORM: SELECIONAR_DADOS.
    WHEN: 'LIMPAR'.
      PERFORM: LIMPAR_DADOS.
    WHEN 'BTN_INATIVAR'.
      PERFORM: INATIVAR_REGISTRO.
      PERFORM: SELECIONAR_DADOS.
  ENDCASE.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  CLEAR: IT_SAIDA[], IT_ZLEST0055[], IT_KNA1[], IT_TVLV[], IT_TVLVT[].

  IF ( R_ATIV EQ 'X' ).
    SELECT * FROM ZLEST0055
     INTO TABLE  IT_ZLEST0055
    WHERE MATKL  IN P_MATKL
      AND KUNNR  IN P_KUNNR
      AND AUART  IN P_AUART
      AND VKORG  IN P_VKORG
      AND PO_EMBARQUE IN P_EMBA
      AND PO_DESTINO  IN P_DEST
      AND STATUS EQ 1.
  ELSE.
    SELECT * FROM ZLEST0055
     INTO TABLE  IT_ZLEST0055
    WHERE MATKL  IN P_MATKL
      AND KUNNR  IN P_KUNNR
      AND AUART  IN P_AUART
      AND VKORG  IN P_VKORG
      AND PO_EMBARQUE IN P_EMBA
      AND PO_DESTINO  IN P_DEST
      AND STATUS EQ 0.
  ENDIF.

  SELECT * FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_ZLEST0055
  WHERE KUNNR EQ IT_ZLEST0055-KUNNR.

  SELECT * FROM TVLV
    INTO TABLE IT_TVLV
    FOR ALL ENTRIES IN IT_ZLEST0055
  WHERE ABRVW EQ IT_ZLEST0055-VKAUS.

  SELECT * FROM TVLVT
    INTO TABLE IT_TVLVT
    FOR ALL ENTRIES IN IT_TVLV
  WHERE ABRVW EQ IT_TVLV-ABRVW.

  PERFORM: AGRUPAMENTO_DADOS.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  AGRUPAMENTO_DADOS
*&---------------------------------------------------------------------*
FORM AGRUPAMENTO_DADOS .

  DATA: WL_T023T TYPE T023T,
        VL_MATKL TYPE C LENGTH 6.

  LOOP AT IT_ZLEST0055 INTO WA_ZLEST0055.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZLEST0055-MATKL
      IMPORTING
        OUTPUT = VL_MATKL.

    SELECT SINGLE * FROM T023T INTO WL_T023T WHERE MATKL EQ VL_MATKL
                                               AND SPRAS EQ 'P'.
    CASE WA_ZLEST0055-STATUS.
      WHEN: '1'.
        WA_SAIDA-STATUS = ICON_LED_GREEN.
      WHEN: '0'.
        WA_SAIDA-STATUS = ICON_LED_RED.
    ENDCASE.


    WA_SAIDA-STATUS_AUX = WA_ZLEST0055-STATUS.

    WA_SAIDA-ZID_CP      = WA_ZLEST0055-ZID_CP.
    WA_SAIDA-DT_INICIO   = WA_ZLEST0055-DT_INICIO.
    WA_SAIDA-DT_FIM      = WA_ZLEST0055-DT_FIM.
    WA_SAIDA-AUART       = WA_ZLEST0055-AUART.
    WA_SAIDA-MATKL       = WA_ZLEST0055-MATKL.
    WA_SAIDA-TP_TRANSGENIA = WA_ZLEST0055-TP_TRANSGENIA.
    WA_SAIDA-ZTERM       = WA_ZLEST0055-ZTERM.
    WA_SAIDA-ZTERM2      = WA_ZLEST0055-ZTERM2.

    READ TABLE IT_TVLV  INTO WA_TVLV  WITH KEY ABRVW = WA_ZLEST0055-VKAUS.
    READ TABLE IT_TVLVT INTO WA_TVLVT WITH KEY ABRVW = WA_TVLV-ABRVW.

    CONCATENATE WA_ZLEST0055-VKAUS '-' WA_TVLVT-BEZEI INTO WA_SAIDA-VKAUS SEPARATED BY SPACE.

    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_ZLEST0055-KUNNR.
    WA_SAIDA-KUNNR       = WA_ZLEST0055-KUNNR.
    WA_SAIDA-NAME1       = WA_KNA1-NAME1.


    WA_SAIDA-WAERK       = WA_ZLEST0055-WAERK.
    WA_SAIDA-WAERK_FATURA = WA_ZLEST0055-WAERK_FATURA.
    WA_SAIDA-VKORG       = WA_ZLEST0055-VKORG.
    WA_SAIDA-VTWEG       = WA_ZLEST0055-VTWEG.
    WA_SAIDA-SPART       = WA_ZLEST0055-SPART.
    WA_SAIDA-WGBEZ60     = WL_T023T-WGBEZ60.
    WA_SAIDA-OPERACAO    = WA_ZLEST0055-OPERACAO.
    WA_SAIDA-NETPR       = WA_ZLEST0055-NETPR.
    WA_SAIDA-UNID_MEDIDA = WA_ZLEST0055-UNID_MEDIDA.
    WA_SAIDA-KURST       = WA_ZLEST0055-KURST.

    WA_SAIDA-PO_EMBARQUE = WA_ZLEST0055-PO_EMBARQUE.
    WA_SAIDA-PO_DESTINO  = WA_ZLEST0055-PO_DESTINO.

    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR: WA_ZLEST0055, WA_T023T, WA_SAIDA.

  ENDLOOP.

ENDFORM.                    " AGRUPAMENTO_DADOS
*&---------------------------------------------------------------------*
*&      Form  ADICIONAR_LINHA
*&---------------------------------------------------------------------*
FORM ADICIONAR_LINHA .


  DATA: WL_T023T TYPE T023T,
        WL_KNA1  TYPE KNA1,
        WL_KUNNR TYPE C LENGTH 10,
        WL_MATKL TYPE C LENGTH 9,
        WL_TP_TRANSGENIA TYPE ZSDT0001-TP_TRANSGENIA,
        WL_TABIX TYPE SY-TABIX,
        WL_TVLV  TYPE TVLV,
        WL_TVLVT TYPE TVLVT.


  CLEAR: WA_SAIDA.

  PERFORM: VALIDACAO_CAMPOS.


  WA_SAIDA-KUNNR        = W_KUNNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_KUNNR
    IMPORTING
      OUTPUT = WL_KUNNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_MATKL
    IMPORTING
      OUTPUT = WL_MATKL.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_EMBARQUE
    IMPORTING
      OUTPUT = W_EMBARQUE.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_DESTINO
    IMPORTING
      OUTPUT = W_DESTINO.

  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE AUART      EQ W_AUART  AND
                                       MATKL      EQ WL_MATKL  AND
                                       TP_TRANSGENIA      EQ W_TP_TRANSGENIA AND
                                       KUNNR      EQ WL_KUNNR AND
                                       WAERK      EQ W_WAERK  AND
                                       WAERK_FATURA  EQ W_WAERK_FATURA AND
                                       VKORG      EQ W_VKORG  AND
                                       VTWEG      EQ W_VTWEG  AND
                                       SPART      EQ W_SPART  AND
                                       OPERACAO   EQ W_OPERACAO  AND
                                       PO_EMBARQUE EQ W_EMBARQUE AND
                                       PO_DESTINO  EQ W_DESTINO AND
                                       STATUS_AUX EQ '1' .


    WL_TABIX = SY-TABIX.
    GS_ID    = WA_SAIDA-ZID_CP.

    WA_SAIDA-STATUS_AUX = 'C'.
    WA_SAIDA-STATUS     = ICON_LED_RED.


    MODIFY IT_SAIDA FROM WA_SAIDA INDEX WL_TABIX.

  ENDLOOP.

  WA_SAIDA-STATUS       = ICON_LED_YELLOW.
  WA_SAIDA-STATUS_AUX   = 'X'.
  WA_SAIDA-VKORG        = W_VKORG.
  WA_SAIDA-VTWEG        = W_VTWEG.
  WA_SAIDA-SPART        = W_SPART.
  WA_SAIDA-OPERACAO     = W_OPERACAO.
  WA_SAIDA-DT_INICIO    = W_DT_INICIO.
  WA_SAIDA-DT_FIM       = W_DT_FIM.
  WA_SAIDA-AUART        = W_AUART.
  WA_SAIDA-MATKL        = W_MATKL.
  WA_SAIDA-TP_TRANSGENIA = W_TP_TRANSGENIA.
  WA_SAIDA-WAERK        = W_WAERK.
  WA_SAIDA-WAERK_FATURA = W_WAERK_FATURA.
  WA_SAIDA-ZTERM        = W_ZTERM.
  WA_SAIDA-ZTERM2       = W_ZTERM2.
  WA_SAIDA-NETPR        = W_NETPR.
  WA_SAIDA-UNID_MEDIDA  = W_UNID.
  WA_SAIDA-KURST        = W_KURST.
  WA_SAIDA-PO_EMBARQUE  = W_EMBARQUE.
  WA_SAIDA-PO_DESTINO   = W_DESTINO.


  SELECT SINGLE * FROM TVLV  INTO WL_TVLV  WHERE ABRVW EQ W_VKAUS.
  SELECT SINGLE * FROM TVLVT INTO WL_TVLVT WHERE ABRVW EQ WL_TVLV-ABRVW.

  CONCATENATE W_VKAUS '-' WL_TVLVT-BEZEI INTO WA_SAIDA-VKAUS SEPARATED BY SPACE.


  SELECT SINGLE * FROM T023T INTO WL_T023T WHERE MATKL EQ W_MATKL.
  WA_SAIDA-WGBEZ60      = WL_T023T-WGBEZ60.



  SELECT SINGLE * FROM KNA1 INTO WL_KNA1 WHERE KUNNR EQ WL_KUNNR.
  WA_SAIDA-NAME1 = WL_KNA1-NAME1.


  WA_SAIDA-USNAM        = SY-UNAME.
  WA_SAIDA-DATA_ATUAL   = SY-DATUM.
  WA_SAIDA-HORA_ATUAL   = SY-UZEIT.

  APPEND WA_SAIDA TO IT_SAIDA.

  IF NOT IT_SAIDA[] IS INITIAL.

    CALL METHOD GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STABLE.

  ENDIF.

ENDFORM.                    " ADICIONAR_LINHA

*&---------------------------------------------------------------------*
*&      Form  VALIDACAO_CAMPOS
*&---------------------------------------------------------------------*
FORM VALIDACAO_CAMPOS .

  IF ( W_DT_INICIO < SY-DATUM ).
    MESSAGE W899(FI) WITH 'Data Inicio Inferior a Data de Hoje.'.
  ENDIF.

  IF ( W_DT_FIM < SY-DATUM ).
    MESSAGE W899(FI) WITH 'Data Fim Inferior a Data de Hoje.'.
  ENDIF.

  IF ( W_DT_FIM <= W_DT_INICIO ).
    MESSAGE W899(FI) WITH 'Data Fim menor/igual Data Inicio.'.
  ENDIF.

  IF ( W_DT_INICIO IS INITIAL ).

    MESSAGE W899(FI) WITH 'Informar Data Inicio'.
  ENDIF.

  IF ( W_DT_FIM IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Data Fim'.
  ENDIF.

  IF ( W_AUART IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Tipo de Ordem'.
  ENDIF.

  IF ( W_MATKL IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Grupo de Mercadoria'.
  ENDIF.

  IF ( W_KUNNR IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Emissor'.
  ENDIF.

  IF ( W_VKAUS IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Utilização'.
  ENDIF.

  IF ( W_OPERACAO IS INITIAL ).
    MESSAGE W899(FI) WITH 'Informar Operação'.
  ENDIF.

  IF ( W_VLD_M EQ 'S' ).

    IF ( W_WAERK IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar a Moeda'.
    ENDIF.

    IF ( W_WAERK_FATURA IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar a Moeda de Fatura'.
    ENDIF.

    IF ( W_ZTERM IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar Cond. Pag.'.
    ENDIF.

    IF ( W_ZTERM2 IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar Cond. Pag.2'.
    ENDIF.

    IF ( W_NETPR IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar o Preço'.
    ENDIF.

    IF ( W_UNID IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar Unidade Medida'.
    ENDIF.

    IF ( W_KURST IS INITIAL ).
      MESSAGE W899(FI) WITH 'Informar a Cotação'.
    ENDIF.

  ENDIF.


ENDFORM.                    " VALIDACAO_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  SALVAR_REGISTRO
*&---------------------------------------------------------------------*
FORM SALVAR_REGISTRO .

  DATA: WL_ZLEST0055 TYPE ZLEST0055,
        WL_TABIX     TYPE SY-TABIX,
        WL_KUNNR     TYPE C LENGTH 10.

  CLEAR: WL_ZLEST0055, WA_SAIDA, WL_TABIX.

*  IF NOT ( GS_ID IS INITIAL ).
*    UPDATE ZLEST0055 SET STATUS = '0' WHERE ZID_CP EQ GS_ID.
*    CLEAR: GS_ID.
*  ENDIF.

  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE STATUS_AUX = 'C' AND ZID_CP IS NOT INITIAL.
    UPDATE ZLEST0055 SET STATUS = '0' WHERE ZID_CP EQ WA_SAIDA-ZID_CP.
    CLEAR: GS_ID.
  ENDLOOP.

  READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY STATUS_AUX = 'X'.

  IF SY-SUBRC NE 0.
    MESSAGE 'Nenhum registro encontrado para salvar!' TYPE 'S'.
    EXIT.
  ENDIF.

  WL_TABIX = SY-TABIX.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZID_CP'
    IMPORTING
      NUMBER      = WL_ZLEST0055-ZID_CP.


  WL_ZLEST0055-STATUS            = 1.
  WL_ZLEST0055-DT_INICIO         = WA_SAIDA-DT_INICIO.
  WL_ZLEST0055-AUART             = WA_SAIDA-AUART.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_SAIDA-MATKL
    IMPORTING
      OUTPUT = WL_ZLEST0055-MATKL.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_SAIDA-KUNNR
    IMPORTING
      OUTPUT = WL_KUNNR.


  WL_ZLEST0055-KUNNR             = WL_KUNNR.


  WL_ZLEST0055-WAERK             = WA_SAIDA-WAERK.
  WL_ZLEST0055-WAERK_FATURA      = WA_SAIDA-WAERK_FATURA.
  WL_ZLEST0055-TP_TRANSGENIA     = WA_SAIDA-TP_TRANSGENIA.
  WL_ZLEST0055-VKORG             = WA_SAIDA-VKORG.
  WL_ZLEST0055-VTWEG             = WA_SAIDA-VTWEG.
  WL_ZLEST0055-SPART             = WA_SAIDA-SPART.
  WL_ZLEST0055-OPERACAO          = WA_SAIDA-OPERACAO.
  WL_ZLEST0055-DT_FIM            = WA_SAIDA-DT_FIM.
  WL_ZLEST0055-NETPR             = WA_SAIDA-NETPR.
  WL_ZLEST0055-ZTERM             = WA_SAIDA-ZTERM.
  WL_ZLEST0055-ZTERM2            = WA_SAIDA-ZTERM2.
  WL_ZLEST0055-UNID_MEDIDA       = WA_SAIDA-UNID_MEDIDA.
  WL_ZLEST0055-VKAUS             = WA_SAIDA-VKAUS(1).
  WL_ZLEST0055-KURST             = WA_SAIDA-KURST.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_SAIDA-PO_EMBARQUE
    IMPORTING
      OUTPUT = WL_ZLEST0055-PO_EMBARQUE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_SAIDA-PO_DESTINO
    IMPORTING
      OUTPUT = WL_ZLEST0055-PO_DESTINO.

  WL_ZLEST0055-USUARIO     = SY-UNAME.
  WL_ZLEST0055-DT_CADASTRO = SY-DATUM.
  WL_ZLEST0055-HR_CADASTRO = SY-UZEIT.

  INSERT INTO ZLEST0055 VALUES WL_ZLEST0055.

  IF ( SY-SUBRC EQ 0 ).

    WA_SAIDA-STATUS = ICON_LED_GREEN.
    WA_SAIDA-STATUS_AUX = SPACE.

    IF WL_TABIX > 0.
      MODIFY IT_SAIDA FROM WA_SAIDA INDEX WL_TABIX.
    ENDIF.

    CALL METHOD GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STABLE.

  ELSE.

  ENDIF.

ENDFORM.                    " SALVAR_REGISTRO

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DESCRICAO
*&---------------------------------------------------------------------*
FORM INCLUIR_DESCRICAO .

  DATA: WL_TVAKT TYPE TVAKT,
        WL_T023T TYPE T023T,
        WL_KNA1  TYPE KNA1,
        WL_TVKOT TYPE TVKOT,
        WL_TVTWT TYPE TVTWT,
        WL_TSPAT TYPE TSPAT.


  DATA: WL_KUNNR TYPE C LENGTH 10.

  "Incluir Descrições
  SELECT SINGLE * FROM TVAKT INTO WL_TVAKT WHERE AUART EQ W_AUART
                                             AND SPRAS EQ 'P'.
  W_DESC_AUART = WL_TVAKT-BEZEI.

  SELECT SINGLE * FROM T023T INTO WL_T023T WHERE MATKL EQ W_MATKL
                                             AND SPRAS EQ 'P'.
  W_DESC_MAKTL = WL_T023T-WGBEZ60.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W_KUNNR
    IMPORTING
      OUTPUT = WL_KUNNR.

  SELECT SINGLE * FROM KNA1 INTO WL_KNA1 WHERE KUNNR EQ WL_KUNNR.
  W_DESC_KUNNR = WL_KNA1-NAME1.

  SELECT SINGLE * FROM TVKOT INTO WL_TVKOT WHERE VKORG EQ W_VKORG
                                             AND SPRAS EQ 'P'.
  W_DESC_VKORG = WL_TVKOT-VTEXT.


  SELECT SINGLE * FROM TVTWT INTO WL_TVTWT WHERE VTWEG EQ W_VTWEG
                                           AND SPRAS EQ 'P'.
  W_DESC_VTWEG = WL_TVTWT-VTEXT.

  SELECT SINGLE * FROM TSPAT INTO WL_TSPAT WHERE SPART EQ W_SPART
                                             AND SPRAS EQ 'P'.
  W_DESC_SPART = WL_TSPAT-VTEXT.



ENDFORM.                    " INCLUIR_DESCRICAO


*&---------------------------------------------------------------------*
*&     CLASSES
*&---------------------------------------------------------------------*
CLASS LCL_ALV_RECEIVER_EVENT DEFINITION.
  PUBLIC SECTION.
    METHODS:
            DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW.
ENDCLASS.                    "LCL_ALV_RECEIVER_EVENT DEFINITION
*----------------------------------------------------------------------*
*       CLASS LCL_ALV_RECEIVER_EVENT IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_ALV_RECEIVER_EVENT IMPLEMENTATION.
  METHOD DOUBLE_CLICK.
    PERFORM: DOUBLE_CLICK USING E_ROW.
  ENDMETHOD.                    "DOUBLE_CLICK

ENDCLASS.                    "LCL_ALV_RECEIVER_EVENT IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM DOUBLE_CLICK  USING  P_ROW TYPE LVC_S_ROW.

  DATA: WL_ZLEST0055 TYPE ZLEST0055.


  IF NOT ( P_ROW IS INITIAL ).
    CLEAR: WA_SAIDA.

    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW-INDEX.

    IF ( WA_SAIDA-STATUS_AUX EQ 'X' ).
      MESSAGE S899(FI) DISPLAY LIKE 'W' WITH 'Registro não salvo!'.
    ELSE.


      SELECT SINGLE * FROM ZLEST0055 INTO WL_ZLEST0055 WHERE ZID_CP  = WA_SAIDA-ZID_CP.

      WD_DT_INICIO   = WL_ZLEST0055-DT_INICIO.
      WD_DT_FIM      = WL_ZLEST0055-DT_FIM.
      WD_AUART       = WL_ZLEST0055-AUART.
      WD_MATKL       = WL_ZLEST0055-MATKL.
      WD_KUNNR       = WL_ZLEST0055-KUNNR.
      WD_VKORG       = WL_ZLEST0055-VKORG.
      WD_VTWEG       = WL_ZLEST0055-VTWEG.
      WD_SPART       = WL_ZLEST0055-SPART.
      WD_OPERACAO    = WL_ZLEST0055-OPERACAO.
      WD_WAERK       = WL_ZLEST0055-WAERK.
      WD_WAERK_FATURA = WL_ZLEST0055-WAERK_FATURA.
      WD_TP_TRANSGENIA = WL_ZLEST0055-TP_TRANSGENIA.
      WD_NETPR       = WL_ZLEST0055-NETPR.
      WD_UNID        = WL_ZLEST0055-UNID_MEDIDA.
      WD_ZTERM       = WL_ZLEST0055-ZTERM.
      WD_ZTERM2      = WL_ZLEST0055-ZTERM2.
      WD_ID          = WL_ZLEST0055-ZID_CP.
      WD_USUARIO     = WL_ZLEST0055-USUARIO.
      WD_DT_CADASTRO = WL_ZLEST0055-DT_CADASTRO.
      WD_HR_CADASTRO = WL_ZLEST0055-HR_CADASTRO.

      CALL SCREEN 0200 STARTING AT 1 1 ENDING AT 75 23.
    ENDIF.

  ENDIF.

ENDFORM.                    " DOUBLE_CLICK



*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETO  OUTPUT
*&---------------------------------------------------------------------*
MODULE CRIAR_OBJETO OUTPUT.

  DATA: WL_VRM_VALUE  TYPE VRM_VALUE,
        IT_VRM_VALUE TYPE STANDARD TABLE OF VRM_VALUE.

  DATA: WL_REPID     TYPE SY-REPID,
        TL_FUNCTION  TYPE UI_FUNCTIONS,
        WL_FUNCTION  LIKE TL_FUNCTION WITH HEADER LINE.


  DATA: OBG_HANDLER TYPE REF TO LCL_ALV_RECEIVER_EVENT.


  IF ( OBG_CONTAINER IS INITIAL ).


    GS_LAYOUT-ZEBRA      = 'X'.
    GS_STABLE-ROW        = 'X'.
    GS_LAYOUT-SEL_MODE   = 'C'.

    CREATE OBJECT OBG_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT GRID
      EXPORTING
        I_PARENT          = OBG_CONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM: CRIAR_CATALOG.

    CREATE OBJECT OBG_HANDLER.

    SET HANDLER OBG_HANDLER->DOUBLE_CLICK FOR GRID.


    CALL METHOD GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING          = TL_FUNCTION
        IS_LAYOUT                     = GS_LAYOUT
        IS_VARIANT                    = GS_VARIANT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = IT_SAIDA[]
        IT_FIELDCATALOG               = IT_FCATALOG
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: IT_VRM_VALUE[].

    WL_VRM_VALUE-KEY  = 'CO'.
    WL_VRM_VALUE-TEXT = 'Convencional'.
    APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

    WL_VRM_VALUE-KEY  = 'R1'.
    WL_VRM_VALUE-TEXT = 'RR'.
    APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

    WL_VRM_VALUE-KEY  = 'R2'.
    WL_VRM_VALUE-TEXT = 'RR2'.
    APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID      = 'W_TP_TRANSGENIA'
        VALUES  = IT_VRM_VALUE
      EXCEPTIONS
        ID_ILLEGAL_NAME = 1
        OTHERS = 2.

  ELSE.
    CALL METHOD GRID->REFRESH_TABLE_DISPLAY.
     " EXPORTING
     "   IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                 " CRIAR_OBJETO  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM CRIAR_CATALOG .

  REFRESH: IT_FCATALOG.

  PERFORM ESTRUTURA_ALV USING:

        0 'ZLEST0055'  ' ' 'IT_SAIDA' 'STATUS'      'Status'              '6'  '' '' ' ' 'C',
        1 'ZLEST0055'  ' ' 'IT_SAIDA' 'VKORG'       'Org. de Vendas'      '8' '' '' ' ' ' ',
        1 'ZLEST0055'  ' ' 'IT_SAIDA' 'VTWEG'       'Canal de Distri.'    '10' '' '' ' ' ' ',
        1 'ZLEST0055'  ' ' 'IT_SAIDA' 'SPART'       'Setor Atividade'     '10' '' '' ' ' ' ',
        1 'ZLEST0055'  ' ' 'IT_SAIDA' 'DT_INICIO'   'Data Inicio'         '10' '' '' ' ' ' ',
        2 'ZLEST0055'  ' ' 'IT_SAIDA' 'DT_FIM'      'Data Fim'            '10' '' '' ' ' ' ',
        3 'ZLEST0055'  ' ' 'IT_SAIDA' 'AUART'       'Tipo de Ordem'       '10' '' '' ' ' ' ',
        4 'ZLEST0055'  ' ' 'IT_SAIDA' 'MATKL'       'Grupo de Mercadoria' '15' '' '' ' ' ' ',
        4 'ZLEST0055'  ' ' 'IT_SAIDA' 'TP_TRANSGENIA' 'Class.Produto'     '14' '' '' ' ' ' ',
        4 'ZLEST0055'  ' ' 'IT_SAIDA' 'OPERACAO'     'Operação'           '08' '' '' ' ' ' ',
        5 'ZLEST0055'  ' ' 'IT_SAIDA' 'WGBEZ60'     'Desc. Grupo Merc.'   '20' '' '' ' ' ' ',
        6 'ZLEST0055'  ' ' 'IT_SAIDA' 'PO_EMBARQUE'  'Porto. Embarque'    '10' '' '' ' ' ' ',
        6 'ZLEST0055'  ' ' 'IT_SAIDA' 'PO_DESTINO'   'Porto. Destino'     '10' '' '' ' ' ' ',
        6 'ZLEST0055'  ' ' 'IT_SAIDA' 'KUNNR'       'Emissor'             '10' '' '' ' ' ' ',
        6 'ZLEST0055'  ' ' 'IT_SAIDA' 'NAME1'       'Desc. Emissor'       '20' '' '' ' ' ' ',
        7 'ZLEST0055'  ' ' 'IT_SAIDA' 'WAERK'       'Moeda'               '5'  '' '' ' ' ' ',
        7 'ZLEST0055'  ' ' 'IT_SAIDA' 'WAERK_FATURA' 'Moeda Fat.'         '10' '' '' ' ' ' ',
        7 'ZLEST0055'  ' ' 'IT_SAIDA' 'ZTERM'       'Cond. Pag.'          '5'  '' '' ' ' ' ',
        7 'ZLEST0055'  ' ' 'IT_SAIDA' 'ZTERM2'      'Cond. Pag.2'         '5'  '' '' ' ' ' ',
        7 'ZLEST0055'  ' ' 'IT_SAIDA' 'VKAUS'       'Utilização'          '8'  '' '' ' ' ' ',
        8 'ZLEST0055'  ' ' 'IT_SAIDA' 'NETPR'       'Preço'               '8'  '' '' ' ' ' ',
        8 'ZLEST0055'  ' ' 'IT_SAIDA' 'KURST'       'Cotação'             '3'  '' '' ' ' ' ',
        9 'ZLEST0055'  ' ' 'IT_SAIDA' 'UNID_MEDIDA' 'Unid. Medida'        '8' '' '' ' ' ' '.

ENDFORM.                    " CRIAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ESTRUTURA_ALV
*&---------------------------------------------------------------------*
FORM ESTRUTURA_ALV   USING VALUE(P_COL_POS)       TYPE I
                           VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                           VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                           VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                           VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                           VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                           VALUE(P_OUTPUTLEN)
                           VALUE(P_EDIT)
                           VALUE(P_SUM)
                           VALUE(P_EMPHASIZE)
                           VALUE(P_JUST).
  CLEAR WA_FCATALOG.

  WA_FCATALOG-FIELDNAME   = P_FIELD.
  WA_FCATALOG-TABNAME     = P_TABNAME.
  WA_FCATALOG-REF_TABLE   = P_REF_TABNAME.
  WA_FCATALOG-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCATALOG-KEY         = ' '.
  WA_FCATALOG-EDIT        = P_EDIT.
  WA_FCATALOG-COL_POS     = P_COL_POS.
  WA_FCATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCATALOG-NO_OUT      = ' '.
  WA_FCATALOG-REPTEXT     = P_SCRTEXT_L.
  WA_FCATALOG-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCATALOG-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCATALOG-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_FCATALOG-JUST        = P_JUST.

  APPEND WA_FCATALOG TO IT_FCATALOG.

ENDFORM.                    " ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_DADOS
*&---------------------------------------------------------------------*
FORM LIMPAR_DADOS .

  DATA: WL_TABIX TYPE SY-TABIX.

  PERFORM: LIMPAR_CAMPOS.

  IF NOT ( IT_SAIDA[] IS INITIAL ).


    IF NOT ( GS_ID IS INITIAL ).

      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ZID_CP = GS_ID.

      WL_TABIX = SY-TABIX.
      WA_SAIDA-STATUS     = ICON_LED_GREEN.
      WA_SAIDA-STATUS_AUX = '1'.

      MODIFY IT_SAIDA FROM WA_SAIDA INDEX WL_TABIX.

      CLEAR: GS_ID.

    ENDIF.

    DELETE IT_SAIDA WHERE STATUS_AUX EQ 'X'.

    CALL METHOD GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STABLE.
  ENDIF.
ENDFORM.                    " LIMPAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_CAMPOS
*&---------------------------------------------------------------------*
FORM LIMPAR_CAMPOS .
  CLEAR: W_DT_INICIO,W_DT_FIM,W_AUART,W_MATKL,W_KUNNR, W_VKORG, W_VTWEG, W_SPART, W_WAERK, W_WAERK_FATURA,W_TP_TRANSGENIA, W_NETPR, W_ZTERM, W_ZTERM2, W_UNID,W_DESC_AUART, W_VKAUS,
         W_DESC_MAKTL, W_DESC_KUNNR, W_DESC_VKORG, W_DESC_VTWEG, W_DESC_SPART, W_KURST, W_DESTINO, W_EMBARQUE, W_OPERACAO.
ENDFORM.                    " LIMPAR_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.

  SET PF-STATUS 'PS0200'.
  SET TITLEBAR  'TB0200'.

  CLEAR: IT_VRM_VALUE[].

  WL_VRM_VALUE-KEY  = 'CO'.
  WL_VRM_VALUE-TEXT = 'Convencional'.
  APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

  WL_VRM_VALUE-KEY  = 'R1'.
  WL_VRM_VALUE-TEXT = 'RR'.
  APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

  WL_VRM_VALUE-KEY  = 'R2'.
  WL_VRM_VALUE-TEXT = 'RR2'.
  APPEND WL_VRM_VALUE TO IT_VRM_VALUE.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID      = 'WD_TP_TRANSGENIA'
      VALUES  = IT_VRM_VALUE
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS = 2.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0200 INPUT.
  CASE SY-UCOMM.
    WHEN: 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT

FORM INATIVAR_REGISTRO.

  DATA: VAR_ANSWER TYPE C.

  CLEAR: IT_SEL_ROWS[].

  CALL METHOD GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente inativar o(s) registro(s) selecionado(s)?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = VAR_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  CHECK VAR_ANSWER EQ '1'.

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SEL_ROWS-INDEX.

    CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA-ZID_CP IS NOT INITIAL ).

    UPDATE ZLEST0055 SET STATUS = '0' WHERE ZID_CP EQ WA_SAIDA-ZID_CP.
  ENDLOOP.

  MESSAGE 'Registros inativados com sucesso!' TYPE 'S'.

ENDFORM.
