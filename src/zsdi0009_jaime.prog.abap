*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT ZSDI0009_JAIME NO STANDARD PAGE HEADING MESSAGE-ID SD.

*----------------------------------------------------------------------*
* Type Pools
*----------------------------------------------------------------------*
TYPE-POOLS ICON.

TABLES: LIKP,
        LV50C,
        VBUK,
        ZSDT0023.
* BBKO/Vagner Santos - Ínício da alteração - 02.10.2010
TABLES LFA1.
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TYPE_MSN,
         NR_ROMANEIO TYPE ZNR_ROMANEIO,
         TP_MSN      TYPE BAPI_MTYPE,
         MESSAGEM    TYPE BAPI_MSG,
       END   OF TYPE_MSN,


       BEGIN OF TYPE_ZSDT0023,
         FORNECIMENTO  TYPE ZSDT0023-VBELN,
         DOC_DISTRI    TYPE ZSDT0023-VBELV,
         CH_REFERENCIA TYPE ZSDT0023-CH_REFERENCIA,
         DOC_MAT_S     TYPE ZSDT0023-MBLNR_S,
         ANO_DOC_MAT_S TYPE ZSDT0023-MJAHR_S,
         DT_SAIDA      TYPE ZSDT0023-DT_SAIDA,
         HS_SAIDA      TYPE ZSDT0023-HS_SAIDA,
       END OF TYPE_ZSDT0023,

       "CS2017000598 22.05.2017
       BEGIN OF TY_ZSDT0001_ITEM,
         POSNR_REM TYPE LIPS-POSNR,
         ITM_LOTE  TYPE I.
         INCLUDE    STRUCTURE ZSDT0001_ITEM.
TYPES END OF TY_ZSDT0001_ITEM.
"Fim CS2017000598 22.05.2017

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: T_ZSDT0001   TYPE TABLE OF ZSDT0001,
      T_ZLEST0002  TYPE TABLE OF ZLEST0002,
      TI_ZLEST0100 TYPE TABLE OF ZLEST0100  WITH HEADER LINE,
      WA_ZLEST0100 TYPE ZLEST0100,
      T_ROMA       TYPE TABLE OF ZSDT0001,
      T_FCAT       TYPE TABLE OF LVC_S_FCAT,
      T_TOOL       TYPE UI_FUNCTIONS,
      T_MSN        TYPE TABLE OF TYPE_MSN,
      SL_MSN       TYPE TYPE_MSN,
      T_BDC        TYPE TABLE OF BDCDATA,
      VG_VSTEL     TYPE VBAP-VSTEL,
      VG_ERDAT     TYPE VBAP-ERDAT,
      VG_TEXTO     TYPE CHAR1,
      VG_DESC      TYPE CHAR600,
      VL_PONTEIRO  TYPE ZLEST0100-CONT,
      T_MESSTAB    TYPE TABLE OF BDCMSGCOLL,
      WA_0023_AUX  TYPE SY-SUBRC.

*-#133089-21.02.2024-JT-inicio
DATA: LC_FATURAMENTO_AUTOMATICO TYPE REF TO ZCL_FATURAMENTO_AUTOMATICO,
      VG_FATURAMENTO_AUTOM      TYPE CHAR01.
*-#133089-12.02.2024-JT-fim

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: V_VTEXT               TYPE TVSTT-VTEXT,
      V_NAME1               TYPE KNA1-NAME1,
      V_BEZEI               TYPE TVAKT-BEZEI,
      V_NAME1_AG            TYPE LFA1-NAME1,
      V_LIFNR               TYPE LFA1-LIFNR,

      V_NR_ROMANEIO         TYPE ZSDT0001-CH_REFERENCIA,
      VL_DELIVERY_C         TYPE BAPISHPDELIVNUMB-DELIV_NUMB,

      VL_MATDOCUMENTYEAR_R  TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
      VL_MAT_DOC_R          TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
      ITEM_TEXT_NR_ROMANEIO TYPE BAPI2017_GM_ITEM_CREATE-ITEM_TEXT.


DATA: LW_PARVW TYPE VBPA-PARVW.
*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: S_VBAK              TYPE VBAK,
      WA_VBAK             TYPE VBAK,
      SL_ZMMT0074         TYPE ZMMT0074,
      S_VBAP              TYPE VBAP,
      "CS2017000598 22.05.2017
      TG_VBAP             TYPE TABLE OF VBAP WITH HEADER LINE,
      T_ZSDT0001_ITEM     TYPE TABLE OF TY_ZSDT0001_ITEM WITH HEADER LINE,
      T_ZSDT0001_ITEM_GRP TYPE TABLE OF TY_ZSDT0001_ITEM WITH HEADER LINE,
      "Fim CS2017000598 22.05.2017
      S_CONT              TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      S_ALV               TYPE REF TO CL_GUI_ALV_GRID,
      S_LAYOUT            TYPE LVC_S_LAYO,
      V_USER              TYPE TVARV_VAL,
      VL_INCO1            TYPE VBKD-INCO1,
      VL_LIFNR            TYPE VBPA-LIFNR,
      V_CHARG             TYPE VBPOK-CHARG,
      V_LGORT             TYPE VBPOK-LGORT,

      T_SADRVB            TYPE TABLE OF SADRVB INITIAL SIZE 0 WITH HEADER LINE,
      T_VBPAVB            TYPE TABLE OF VBPAVB INITIAL SIZE 0 WITH HEADER LINE,

      WA_GOODSMVT_HEADER  TYPE BAPI2017_GM_HEAD_01,
      T_GOODSMVT_ITEM     TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
      WA_GOODSMVT_ITEM    TYPE BAPI2017_GM_ITEM_CREATE,
      T_RETURN            TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      WA_RETURN           TYPE BAPIRET2,
      WA_CODE             TYPE BAPI2017_GM_CODE,
      WA_VBPAVB           TYPE VBPAVB,
      WA_DEPARA           TYPE ZSDT_DEPARA_DEPO.


*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: C_TABLE  TYPE CHAR10 VALUE 'T_ZSDT0001',
           C_04(2)  TYPE C VALUE '04',
           C_F50(3) TYPE C VALUE 'F50',
           C_Z05(3) TYPE C VALUE 'Z05',
           C_ZX1(3) TYPE C VALUE 'ZX1',
           C_05(2)  TYPE C VALUE '05',
           C_E      TYPE C VALUE 'E',
           C_X      TYPE C VALUE 'X'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA S_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING
          E_OBJECT E_INTERACTIVE,

      ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
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
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME.
    SELECT-OPTIONS
      P_VBELN FOR VBUK-VBELN NO INTERVALS NO-EXTENSION OBLIGATORY MATCHCODE OBJECT VMVA.
* BBKO/Vagner Santos - Início da alteração - 02.10.2010
    SELECT-OPTIONS P_LIFNR FOR LFA1-LIFNR NO INTERVALS NO-EXTENSION.
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010
    "parameters p_fat as checkbox default 'X'.
  SELECTION-SCREEN END   OF BLOCK A2.
SELECTION-SCREEN END   OF BLOCK A1.
PARAMETERS:  P_PESO TYPE ZSDT0001-PESO_LIQ_POS_RET NO-DISPLAY.

*-#133089-21.02.2024-JT-inicio
PARAMETERS: P_FATAUT TYPE CHAR01 NO-DISPLAY.
*-#133089-21.02.2024-JT-fim

AT SELECTION-SCREEN OUTPUT.
  IF  NOT ( LW_PARVW IS INITIAL ).
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN: 'P_LIFNR-LOW'.
          SCREEN-INPUT     = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      CASE SCREEN-NAME.
        WHEN: 'P_LIFNR-LOW'.
          SCREEN-INPUT     = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

AT SELECTION-SCREEN ON P_VBELN.

  CLEAR VL_INCO1.
  SELECT SINGLE INCO1
    FROM VBKD
    INTO (VL_INCO1)
  WHERE  VBELN IN P_VBELN
  AND  POSNR NE 0.

  IF ( VL_INCO1 = 'CIF' ) AND ( NOT VL_INCO1 IS INITIAL ).

    LW_PARVW = 'SP'.

    SELECT SINGLE LIFNR
        FROM VBPA
        INTO (P_LIFNR-LOW)
        WHERE VBELN  IN P_VBELN
    AND   PARVW  EQ LW_PARVW.

  ENDIF.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: VL_DELIVERY_C, V_NR_ROMANEIO, VG_FATURAMENTO_AUTOM. "*-#133089-21.02.2024-JT
  IF SY-TCODE = 'ZLES0106' OR
     SY-TCODE = 'ZLES0115' OR
     SY-TCODE = 'ZLES0136' OR
     SY-TCODE = 'ZMM0127'  OR
     SY-BATCH = ABAP_TRUE  OR
     P_FATAUT = ABAP_TRUE.   "*-#133089-21.02.2024-JT
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD V_NR_ROMANEIO.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD VL_DELIVERY_C.
  ENDIF.

*-#133089-21.02.2024-JT-inicio
*-----------------------------------------------
*-verifica se é faturamento automatico
*-----------------------------------------------
  IF P_FATAUT = ABAP_TRUE.
*   SELECT SINGLE ch_referencia
*     INTO @DATA(_ch_ref)
*     FROM zlest0241
*    WHERE ch_referencia = @v_nr_romaneio
*      AND cancelado     = @abap_off.
*   IF sy-subrc = 0.
*     vg_faturamento_autom = abap_true.
*   ENDIF.
    VG_FATURAMENTO_AUTOM = P_FATAUT.
    CREATE OBJECT LC_FATURAMENTO_AUTOMATICO.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  TRY.  "*-#133089-21.02.2024-JT-inicio
* Verifica OV
      PERFORM: Z_VERIFICA_OV   ,
* Verifica Romaneio
               Z_VERIFICA_ROM  ,
*BBKO/Vagner Santos - Início da alteração - 02.10.2010
               Z_VERIFICA_AGENTE_FRETE,
*BBKO/Vagner Santos - Fim da alteração - 02.10.2010
* Verifica Usuário
               Z_SELECIONA_TVARVC,
* Monta FieldCat
               Z_MONTA_FIELDCAT.
    CATCH ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio
      RETURN.        "*-#133089-21.02.2024-JT-inicio
  ENDTRY.            "*-#133089-21.02.2024-JT-inicio

  IF V_NR_ROMANEIO IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    TRY.    "*-#133089-21.02.2024-JT-inicio
        PERFORM Z_HANDLE_COMMAND USING 'REMESSA'.
      CATCH ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio
        RETURN.        "*-#133089-21.02.2024-JT-inicio
    ENDTRY.            "*-#133089-21.02.2024-JT-inicio
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_OV                                            *
*&---------------------------------------------------------------------*
*                                Verifica OV                           *
*----------------------------------------------------------------------*
FORM Z_VERIFICA_OV.

  DATA: LW_LFA1  TYPE LFA1,
        LW_LIFNR TYPE LFA1-LIFNR.

  CLEAR VL_INCO1.
  SELECT SINGLE INCO1
    FROM VBKD
    INTO (VL_INCO1)
  WHERE  VBELN IN P_VBELN
  AND  POSNR NE 0.

  IF ( VL_INCO1 = 'CIF' AND NOT VL_INCO1 IS INITIAL ) AND ( SY-TCODE EQ 'ZOPUS' ).
    SELECT SINGLE LIFNR
        FROM VBPA
        INTO P_LIFNR-LOW
      WHERE  VBELN IN P_VBELN
    AND    PARVW  = 'SP'.
  ENDIF.

  CLEAR: LW_LIFNR, LW_LFA1.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_LIFNR-LOW
    IMPORTING
      OUTPUT = LW_LIFNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LW_LIFNR
    IMPORTING
      OUTPUT = LW_LIFNR.

  SELECT SINGLE * FROM LFA1
    INTO LW_LFA1
  WHERE LIFNR EQ LW_LIFNR.

  V_LIFNR    = LW_LFA1-LIFNR.
  V_NAME1_AG = LW_LFA1-NAME1.



  SELECT SINGLE VBELN
    FROM VBAP
    INTO P_VBELN
  WHERE  VBELN IN P_VBELN.


  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE I836 WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.






ENDFORM.                    " Z_VERIFICA_OV

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_ROM                                           *
*&---------------------------------------------------------------------*
*                            Verifica Romaneio                         *
*----------------------------------------------------------------------*
FORM Z_VERIFICA_ROM RAISING ZCX_ERROR "*-#133089-21.02.2024-JT-inicio
.

  DATA: TL_ZSDT0001    TYPE TABLE OF ZSDT0001,
        WA_TL_ZSDT0001 TYPE ZSDT0001,
        VL_ZLSCH       TYPE VBKD-ZLSCH,
        VL_ZTERM       TYPE VBKD-ZTERM,
        TL_TTEXT       TYPE TABLE OF TTEXT,
        SL_T052        TYPE T052,
        SL_T052U       TYPE T052U,
        SL_TTEXT       TYPE TTEXT.




  REFRESH T_ZSDT0001.
  CLEAR: S_VBAK  ,
         S_VBAP  ,
         "CS2017000598 22.05.2017
         TG_VBAP[],
         WA_TL_ZSDT0001,
         T_ZSDT0001_ITEM[],
         T_ZSDT0001_ITEM_GRP[],
         "Fim CS2017000598 22.05.2017
         VG_VSTEL,
         VG_ERDAT,
         VG_TEXTO.


  SELECT SINGLE *
    FROM VBAK
    INTO S_VBAK
  WHERE  VBELN IN P_VBELN.

  IF ( S_VBAK-KVGR1 NE 'NÃO').
    IF P_LIFNR IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE E899 WITH TEXT-027.
        WHEN ABAP_TRUE.
          MESSAGE E899 WITH TEXT-027 INTO DATA(L_MESG).
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDIF.

  IF V_NR_ROMANEIO IS NOT INITIAL.
    SELECT SINGLE *
      FROM ZSDT0001
      INTO WA_TL_ZSDT0001
    WHERE CH_REFERENCIA = V_NR_ROMANEIO.

    SELECT SINGLE * FROM VBAP INTO S_VBAP
     WHERE VBELN IN P_VBELN
       AND MATNR EQ WA_TL_ZSDT0001-MATNR.

    IF ( WA_TL_ZSDT0001-MATNR IS INITIAL ). "CS2017000598 22.05.2017

      SELECT *
       FROM VBAP INTO TABLE TG_VBAP
      WHERE VBELN IN P_VBELN.

      "Itens Romaneio
      SELECT *
        FROM ZSDT0001_ITEM INTO CORRESPONDING FIELDS OF TABLE T_ZSDT0001_ITEM
      WHERE CH_REFERENCIA = WA_TL_ZSDT0001-CH_REFERENCIA.

      LOOP AT T_ZSDT0001_ITEM.
        IF T_ZSDT0001_ITEM-PART_LOTE IS INITIAL.
          T_ZSDT0001_ITEM-PART_LOTE = T_ZSDT0001_ITEM-CD_ITEM.
        ENDIF.
        MODIFY T_ZSDT0001_ITEM.
      ENDLOOP.

      T_ZSDT0001_ITEM_GRP[] = T_ZSDT0001_ITEM[].

      SORT T_ZSDT0001_ITEM_GRP BY PART_LOTE.
      DELETE ADJACENT DUPLICATES FROM T_ZSDT0001_ITEM_GRP COMPARING PART_LOTE.

      IF ( T_ZSDT0001_ITEM_GRP[] IS INITIAL ).
*-#133089-21.02.2024-JT-inicio
        CASE VG_FATURAMENTO_AUTOM.
          WHEN ABAP_OFF.
            MESSAGE E899 WITH 'Itens do Romaneio não encontrado!'.
            EXIT.
          WHEN ABAP_TRUE.
            MESSAGE E899 WITH 'Itens do Romaneio não encontrado!' INTO L_MESG.
            LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
            LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

    ENDIF.
    "Fim CS2017000598 22.05.2017

  ELSE.
    SELECT SINGLE *
      FROM VBAP
      INTO S_VBAP
    WHERE  VBELN IN P_VBELN.
  ENDIF.

  IF ( WA_TL_ZSDT0001-ID_INTERFACE NE '48' AND
       WA_TL_ZSDT0001-ID_INTERFACE NE '49' AND
       WA_TL_ZSDT0001-ID_INTERFACE NE '51' AND
       WA_TL_ZSDT0001-ID_INTERFACE NE '52' ). "CS2017000598 22.05.2017

    IF NOT S_VBAK-ZPESAGEM EQ '01'.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE I836 WITH TEXT-019.
          LEAVE LIST-PROCESSING.
        WHEN ABAP_TRUE.
          MESSAGE I836 WITH TEXT-019 INTO L_MESG.
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

  ENDIF. "CS2017000598 22.05.2017

  SELECT SINGLE ZLSCH ZTERM
    FROM VBKD
    INTO (VL_ZLSCH, VL_ZTERM)
  WHERE  VBELN IN P_VBELN
  AND  POSNR NE 0.

  IF VL_ZLSCH EQ 'D'.
    VG_TEXTO = 'X'.
    IF NOT VL_ZTERM IS INITIAL.
      SELECT SINGLE *
        FROM T052
        INTO SL_T052
      WHERE  ZTERM EQ VL_ZTERM.

      SELECT SINGLE *
        FROM T052U
        INTO SL_T052U
      WHERE  SPRAS EQ SY-LANGU
      AND  ZTERM EQ VL_ZTERM.

      IF NOT SL_T052 IS INITIAL.
        IF SL_T052U-TEXT1 IS INITIAL.
          CALL FUNCTION 'FI_TEXT_ZTERM'
            EXPORTING
              I_T052  = SL_T052
            TABLES
              T_ZTEXT = TL_TTEXT.
          LOOP AT TL_TTEXT INTO SL_TTEXT.
            IF SY-TABIX EQ 1.
              VG_DESC = SL_TTEXT-TEXT1.
            ELSE.
              CONCATENATE VG_DESC
                          SL_TTEXT-TEXT1
                     INTO VG_DESC SEPARATED BY SPACE.
            ENDIF.
            CLEAR SL_TTEXT.
          ENDLOOP.
        ELSE.
          VG_DESC = SL_T052U-TEXT1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF S_VBAK-AUART EQ 'ZRAG'.
    SELECT *
       FROM ZSDT0001
       INTO TABLE T_ZSDT0001
     WHERE BUKRS        EQ S_VBAK-BUKRS_VF
       AND BRANCH       EQ S_VBAP-WERKS
       AND NR_SAFRA     EQ S_VBAP-CHARG
       AND PARID        EQ S_VBAK-KUNNR
       AND MATNR        EQ S_VBAP-MATNR
       AND TP_MOVIMENTO EQ 'E'
       AND STATUS       NE 'X'.
  ELSE.
    IF ( WA_TL_ZSDT0001-MATNR IS INITIAL ) AND ( WA_TL_ZSDT0001-CH_REFERENCIA IS NOT INITIAL ). "CS2017000598 22.05.2017
      SELECT *
        FROM ZSDT0001
        INTO TABLE T_ZSDT0001
      WHERE CH_REFERENCIA EQ WA_TL_ZSDT0001-CH_REFERENCIA
        AND VBELN         EQ P_VBELN-LOW
        AND TP_MOVIMENTO  EQ 'S'
        AND DOC_REM       EQ SPACE.
      "AND status        NE 'X'.

    ELSE.
      SELECT *
        FROM ZSDT0001
        INTO TABLE T_ZSDT0001
      WHERE VBELN  EQ S_VBAP-VBELN
        AND MATNR  EQ S_VBAP-MATNR
        AND TP_MOVIMENTO EQ 'S'
        AND DOC_REM      EQ SPACE.
      "AND status NE 'X'.

      IF ( T_ZSDT0001[] IS INITIAL ). "Tratativa abaixo para Cenario Fertilizantes - Importado
        SELECT SINGLE *
          FROM ZSDT0001 INTO @DATA(LWA_ZSDT0001_CHECK)
         WHERE CH_REFERENCIA EQ @V_NR_ROMANEIO.

        IF ( SY-SUBRC EQ 0 ) AND ( LWA_ZSDT0001_CHECK-TP_MOVIMENTO = 'S' ) AND ( LWA_ZSDT0001_CHECK-SEQ_LCTO IS NOT INITIAL ).
          SELECT *
            FROM ZSDT0001 INTO TABLE T_ZSDT0001
          WHERE CH_REFERENCIA EQ V_NR_ROMANEIO
            AND VBELN         EQ S_VBAP-VBELN
            AND MATNR         EQ S_VBAP-MATNR
            AND TP_MOVIMENTO  EQ 'S'.
          "status  - Ja vai estar marcado como "X"
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF T_ZSDT0001[] IS INITIAL.
*-#133089-21.02.2024-JT-inicio
    CASE VG_FATURAMENTO_AUTOM.
      WHEN ABAP_OFF.
        MESSAGE I836 WITH TEXT-003.
        LEAVE LIST-PROCESSING.
      WHEN ABAP_TRUE.
        MESSAGE I836 WITH TEXT-003 INTO L_MESG.
        LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
        LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

  SORT T_ZSDT0001 BY CH_REFERENCIA ASCENDING.
  VG_VSTEL = S_VBAP-VSTEL.
  VG_ERDAT = S_VBAP-ERDAT.

  SELECT SINGLE VTEXT
    FROM TVSTT
    INTO V_VTEXT
  WHERE  SPRAS EQ SY-LANGU
  AND  VSTEL EQ VG_VSTEL.

  SELECT SINGLE NAME1
    FROM KNA1
    INTO V_NAME1
  WHERE  KUNNR EQ S_VBAK-KUNNR.

  SELECT SINGLE BEZEI
    FROM TVAKT
    INTO V_BEZEI
  WHERE  SPRAS EQ SY-LANGU
  AND  AUART EQ S_VBAK-AUART.

  TL_ZSDT0001[] = T_ZSDT0001[].
  SORT TL_ZSDT0001 BY PLACA_CAV ASCENDING.
  DELETE ADJACENT DUPLICATES FROM TL_ZSDT0001 COMPARING PLACA_CAV.
  DELETE TL_ZSDT0001 WHERE PLACA_CAV IS INITIAL.
  CHECK NOT TL_ZSDT0001[] IS INITIAL.

  SELECT *
    FROM ZLEST0002
    INTO TABLE T_ZLEST0002
    FOR ALL ENTRIES IN TL_ZSDT0001
  WHERE  PC_VEICULO EQ TL_ZSDT0001-PLACA_CAV.

  SORT T_ZLEST0002 BY PC_VEICULO ASCENDING.

ENDFORM.                    " Z_VERIFICA_ROM

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR USING P_OBJECT      TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                            P_INTERACTIVE TYPE CHAR1.

* Constants for button type
  CONSTANTS:
    C_BUTTON_NORMAL           TYPE I VALUE 0,
    C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1,
    C_MENU                    TYPE I VALUE 2,
    C_SEPARATOR               TYPE I VALUE 3,
    C_RADIO_BUTTON            TYPE I VALUE 4,
    C_CHECKBOX                TYPE I VALUE 5,
    C_MENU_ENTRY              TYPE I VALUE 6.

  DATA SL_TOOLBAR TYPE STB_BUTTON.

* Append Seperator
  MOVE C_SEPARATOR  TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

* Botão Vincular NF's
  CLEAR SL_TOOLBAR.
  MOVE: 'REMESSA'          TO SL_TOOLBAR-FUNCTION ,
         ICON_CREATE       TO SL_TOOLBAR-ICON     ,
         TEXT-004          TO SL_TOOLBAR-QUICKINFO,
         TEXT-004          TO SL_TOOLBAR-TEXT     ,
         SPACE             TO SL_TOOLBAR-DISABLED .
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND USING P_UCOMM TYPE SYUCOMM
                      RAISING ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio

  CASE P_UCOMM.
    WHEN 'REMESSA'.
      " Inicio verificação Camila Brand
      " Verificar o picking.
      DATA: TG_0023         TYPE TABLE OF ZMM0023,
            WA_0023         TYPE ZMM0023,
            "VL_CENTRO_A     TYPE WERKS_D,
            VL_CLABS_F      TYPE LABST,
            VL_CLABS_A      TYPE LABST,
            VL_CLABS_E      TYPE LABST,
            VL_TOTAL        TYPE LABST,
            VL_AUX          TYPE CHAR18,
            VL_MSN1         TYPE CHAR50,
            VL_MSN2         TYPE CHAR50,
            WA_TL_ZSDT0001  TYPE ZSDT0001,
            V_SAFRA_A       TYPE ZSDT0001-NR_SAFRA,
            WA_SETLEAF      TYPE SETLEAF,
            IT_SETLEAF      LIKE TABLE OF WA_SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
            SL_ZSDT0001_ENT TYPE ZSDT0001.

      SELECT *
      FROM ZMM0023
      INTO TABLE TG_0023.

      SORT TG_0023 BY  WERKS ASCENDING MATNR ASCENDING CWERKS DESCENDING. "CS2016001304

      IF V_NR_ROMANEIO IS NOT INITIAL.
        DELETE T_ZSDT0001 WHERE   CH_REFERENCIA NE V_NR_ROMANEIO.
      ENDIF.

      READ TABLE T_ZSDT0001 INTO WA_TL_ZSDT0001 INDEX 1.

      IF WA_TL_ZSDT0001-ID_INTERFACE NE '48' AND
         WA_TL_ZSDT0001-ID_INTERFACE NE '49' AND
         WA_TL_ZSDT0001-ID_INTERFACE NE '51' AND
         WA_TL_ZSDT0001-ID_INTERFACE NE '52'.

        TRY .
            "Verificar Depósito da Ordem de Venda
            ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
                )->GET_DEPOSITO_MATERIAL_FILIAL(
                    EXPORTING
                      I_MATNR      = WA_TL_ZSDT0001-MATNR    " Nº do material
                      I_TP_PRODUTO = CONV #( COND STRING( WHEN WA_TL_ZSDT0001-TP_TRANSGENIA(1) EQ 'C' THEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO ELSE 'RR' ) )    " Tipo de Produto
                      I_BUKRS      = WA_TL_ZSDT0001-BUKRS    " Empresa
                      I_BRANCH     = WA_TL_ZSDT0001-BRANCH    " Local de negócios
                    IMPORTING
                      E_LGORT          = DATA(E_LGORT)    " Depósito
                      E_CENTRO_A_FIXAR = DATA(E_CENTRO_A_FIXAR)
                ).

          CATCH ZCX_DEPOSITO INTO DATA(EX_DEPOSITO).    " .
*-#133089-21.02.2024-JT-inicio
            CASE VG_FATURAMENTO_AUTOM.
              WHEN ABAP_OFF.
                EX_DEPOSITO->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
              WHEN ABAP_TRUE.
                MESSAGE ID EX_DEPOSITO->MSGID TYPE 'S' NUMBER EX_DEPOSITO->MSGNO WITH EX_DEPOSITO->MSGV1 EX_DEPOSITO->MSGV2 EX_DEPOSITO->MSGV3 EX_DEPOSITO->MSGV4 INTO DATA(L_MESG).
                LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
                LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDTRY.
      ELSE.
        CLEAR: E_CENTRO_A_FIXAR.
      ENDIF.

      CLEAR WA_0023.
      READ TABLE TG_0023 INTO WA_0023 WITH KEY WERKS = WA_TL_ZSDT0001-BRANCH
                                               MATNR = WA_TL_ZSDT0001-MATNR. "lê o primeiro - SMC CS2023000120 Urgente - Atualização tela de bloqueio

      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA
      IF SY-SUBRC NE 0.
        SELECT SINGLE MATKL INTO @DATA(_MATKL) FROM MARA WHERE MATNR = @WA_TL_ZSDT0001-MATNR.
        READ TABLE TG_0023 INTO WA_0023 WITH KEY WERKS = WA_TL_ZSDT0001-BRANCH
                                                 MATKL = _MATKL.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      CLEAR WA_0023_AUX.
      IF
        SY-SUBRC NE 0.
        WA_0023_AUX = SY-SUBRC.
      ENDIF.
      ""141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC


      " Se for centro virtual na OV não verifica
      IF ( NOT SY-SUBRC = 0 OR WA_0023-STATUS NE 'A' ) AND ( WA_TL_ZSDT0001-MATNR IS NOT INITIAL ) AND ( S_VBAP-WERKS = WA_TL_ZSDT0001-BRANCH ).
        "ALRS 180118
        SELECT SINGLE *
         FROM VBAP
         INTO S_VBAP
        WHERE VBELN IN P_VBELN
          AND MATNR EQ WA_TL_ZSDT0001-MATNR.

        SELECT SINGLE * INTO @DATA(WA_ZMMT0017)
          FROM ZMMT0017
         WHERE MATNR       EQ @WA_TL_ZSDT0001-MATNR
           AND CENTRO_FIXO EQ @WA_TL_ZSDT0001-BRANCH.

        IF SY-SUBRC IS INITIAL.

          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
          IF WA_0023_AUX IS NOT INITIAL.
            MESSAGE E897(SD) WITH  'Falta parâmetros na ZMM0029. '
                                      'Favor entrar em contato com '
                                       'a área de controladoria e estoque. '.
          ENDIF.
          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

          SELECT SINGLE CLABS
              FROM MCHB
              INTO VL_CLABS_F
            WHERE  MATNR EQ WA_TL_ZSDT0001-MATNR
              AND  WERKS EQ WA_TL_ZSDT0001-BRANCH
              AND  LGORT EQ S_VBAP-LGORT
              AND  CHARG EQ WA_TL_ZSDT0001-NR_SAFRA.

          IF NOT E_CENTRO_A_FIXAR IS INITIAL.
            IF WA_TL_ZSDT0001-NR_SAFRA GT '2019'.
              CONCATENATE WA_TL_ZSDT0001-NR_SAFRA '_' WA_TL_ZSDT0001-BRANCH INTO V_SAFRA_A.
            ELSE.
              V_SAFRA_A = WA_TL_ZSDT0001-NR_SAFRA.
            ENDIF.
            SELECT SINGLE CLABS
              FROM MCHB
              INTO VL_CLABS_A
             WHERE MATNR EQ WA_TL_ZSDT0001-MATNR
               AND WERKS EQ E_CENTRO_A_FIXAR
               AND LGORT EQ S_VBAP-LGORT
               AND CHARG EQ V_SAFRA_A.
          ENDIF.

          "Checa se vai ter entrada de residuo
          "ALRS C
          CLEAR VL_CLABS_E.

          "Conversao S4 Hana 27-07-23
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wa_tl_zsdt0001-matnr
*            IMPORTING
*              output = wa_tl_zsdt0001-matnr.
          "Conversao S4 Hana 27-07-23


          DATA(_BUKRS_MAT_EXC) = ABAP_FALSE.
          PERFORM F_CHECK_EXCECAO_RESIDUO USING WA_TL_ZSDT0001-MATNR
                                                WA_TL_ZSDT0001-BUKRS
                                       CHANGING _BUKRS_MAT_EXC.

          SELECT * INTO TABLE IT_SETLEAF
             FROM SETLEAF
             WHERE SETNAME EQ 'RESIDUO'
             AND VALFROM EQ WA_TL_ZSDT0001-MATNR.

          IF ( SY-SUBRC = 0 ) AND ( _BUKRS_MAT_EXC EQ ABAP_FALSE  ) .
            SELECT SINGLE *
              FROM ZMMT0074
              INTO SL_ZMMT0074
              WHERE WERKS = WA_TL_ZSDT0001-BRANCH
            AND   MATNR = WA_TL_ZSDT0001-MATNR.

            IF SL_ZMMT0074-ENTRADA_ROM = 'S'. "Checa romaneio de entrada
              IF WA_TL_ZSDT0001-ID_CARGA IS NOT INITIAL.

                CLEAR SL_ZSDT0001_ENT-PESO_LIQ.

                SELECT SUM( PESO_LIQ )
                  FROM ZSDT0001
                  INTO SL_ZSDT0001_ENT-PESO_LIQ
                 WHERE TP_MOVIMENTO  EQ 'E'
                   AND ID_CARGA      EQ WA_TL_ZSDT0001-ID_CARGA.

                IF SL_ZSDT0001_ENT-PESO_LIQ IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                  CASE VG_FATURAMENTO_AUTOM.
                    WHEN ABAP_OFF.
                      MESSAGE E897(SD) WITH 'Não foram encontrados todos'
                                            'os romaneios de desmembramento!'.
                    WHEN ABAP_TRUE.
                      MESSAGE E897(SD) WITH 'Não foram encontrados todos' 'os romaneios de desmembramento!' INTO L_MESG.
                      LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
                      LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
                  ENDCASE.
*-#133089-21.02.2024-JT-fim
                ENDIF.

              ELSEIF WA_TL_ZSDT0001-ID_REFERENCIA IS NOT INITIAL.

                DATA(_NR_ROMANEIO) = WA_TL_ZSDT0001-ID_REFERENCIA.

                SELECT SINGLE *
                  FROM ZSDT0001
                  INTO SL_ZSDT0001_ENT
                  WHERE BUKRS         = WA_TL_ZSDT0001-BUKRS
                  AND   BRANCH        = WA_TL_ZSDT0001-BRANCH
                  AND   TP_MOVIMENTO  = 'E'
                  AND   NR_ROMANEIO   = _NR_ROMANEIO
                  "AND   DT_MOVIMENTO  = WA_TL_ZSDT0001-DT_MOVIMENTO
                AND   NR_SAFRA      = WA_TL_ZSDT0001-NR_SAFRA.

                IF SL_ZSDT0001_ENT-CH_REFER_ENT IS NOT INITIAL.

                  DATA(_PESO_ORIG) = SL_ZSDT0001_ENT-PESO_LIQ. "CS2017001903

                  CLEAR SL_ZSDT0001_ENT-PESO_LIQ.
                  SELECT SUM( PESO_LIQ )
                      FROM ZSDT0001
                      INTO SL_ZSDT0001_ENT-PESO_LIQ
                      WHERE BUKRS         = SL_ZSDT0001_ENT-BUKRS
                      AND   BRANCH        = SL_ZSDT0001_ENT-BRANCH
                      AND   TP_MOVIMENTO  = 'E'
                      AND   CH_REFER_ENT  = SL_ZSDT0001_ENT-CH_REFER_ENT
                      "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
                  AND   NR_SAFRA      = SL_ZSDT0001_ENT-NR_SAFRA.


                  IF _PESO_ORIG = SL_ZSDT0001_ENT-PESO_LIQ.
*-#133089-21.02.2024-JT-inicio
                    CASE VG_FATURAMENTO_AUTOM.
                      WHEN ABAP_OFF.
                        MESSAGE E897(SD) WITH 'Não foram encontrados todos'
                                              'os romaneios de desmembramento!'.
                      WHEN ABAP_TRUE.
                        MESSAGE E897(SD) WITH 'Não foram encontrados todos' 'os romaneios de desmembramento!' INTO L_MESG.
                        LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
                        LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
                    ENDCASE.
*-#133089-21.02.2024-JT-fim
                  ENDIF.

                ENDIF.
                IF SY-SUBRC =  0.
                  IF  WA_TL_ZSDT0001-PESO_LIQ GT SL_ZSDT0001_ENT-PESO_LIQ.
                    VL_CLABS_E    =  WA_TL_ZSDT0001-PESO_LIQ - SL_ZSDT0001_ENT-PESO_LIQ.
                  ELSE.
                    "Não gera  doc. entrada
                  ENDIF.

                ENDIF.
              ENDIF.
            ELSE.

              VL_CLABS_E    = WA_TL_ZSDT0001-PESO_LIQ.

            ENDIF.
          ENDIF.
          "ALRS C

          VL_TOTAL = VL_CLABS_A + VL_CLABS_F + VL_CLABS_E.

          IF WA_TL_ZSDT0001-PESO_LIQ GT VL_TOTAL.
            VL_AUX = VL_TOTAL.
            CONDENSE VL_AUX NO-GAPS.
            CONCATENATE 'O total' VL_AUX 'do centro' INTO VL_MSN1 SEPARATED BY SPACE.
            CONCATENATE WA_TL_ZSDT0001-BRANCH 'e' E_CENTRO_A_FIXAR INTO VL_MSN2 SEPARATED BY SPACE.

*-#133089-21.02.2024-JT-inicio
            CASE VG_FATURAMENTO_AUTOM.
              WHEN ABAP_OFF.
                MESSAGE E897(SD) WITH VL_MSN1 VL_MSN2 'é menor que a quantidade do picking'.
              WHEN ABAP_TRUE.
                MESSAGE E897(SD) WITH VL_MSN1 VL_MSN2 'é menor que a quantidade do picking' INTO L_MESG.
                LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
                LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.
        ENDIF.
      ENDIF.
      " Fim verificação Camila Brand
      " Fim verifica picking

*     Gera Remessa
      TRY.  "*-#133089-21.02.2024-JT-inicio
          PERFORM Z_GERA_REMESSA.
        CATCH ZCX_ERROR INTO DATA(EX_ERROR). "*-#133089-21.02.2024-JT-inicio
          MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO L_MESG.
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ). "*-#133089-21.02.2024-JT-inicio
      ENDTRY.            "*-#133089-21.02.2024-JT-inicio

      IF V_NR_ROMANEIO IS INITIAL AND
         VG_FATURAMENTO_AUTOM = ABAP_OFF.  "*-#133089-21.02.2024-JT-inicio
        CALL METHOD S_ALV->REFRESH_TABLE_DISPLAY.
      ENDIF.

  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_GERA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                              Gera Remessa                            *
*----------------------------------------------------------------------*
FORM Z_GERA_REMESSA RAISING ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio.

  DATA: TL_EXEC_COGI   TYPE TABLE OF RGSB4 WITH HEADER LINE,
        TL_ROWS        TYPE LVC_T_ROW,
        SL_ROWS        TYPE LVC_S_ROW,
        SL_ROMA        TYPE ZSDT0001,
        VG_LFIMG       TYPE LFIMG,
        VG_WMENG       TYPE WMENG,
        VG_ROMA        TYPE LFIMG,
        VG_SALDO       TYPE LFIMG,
        ORDEM          TYPE AFPO-AUFNR,
        SL_ZSDT0001    TYPE ZSDT0001,
        VL_SLD_CHECKED TYPE C,
        WA_MSKA        TYPE MSKA.

  DATA:TL_FIELDS TYPE TABLE OF SVAL WITH HEADER LINE,
       LV_RETURN TYPE VBPOK-CHARG,
       V_XCHPF   TYPE MARA-XCHPF,
       WA_MCHB   TYPE MCHB.

  REFRESH: T_ROMA,
           T_MSN ,
           T_MSN .

* Verifica Seleção de Linhas
  IF V_NR_ROMANEIO IS INITIAL.
    CALL METHOD S_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = TL_ROWS.
  ELSE.
    DELETE T_ZSDT0001 WHERE   CH_REFERENCIA NE V_NR_ROMANEIO.
    CLEAR TL_ROWS.
    REFRESH TL_ROWS.
    IF T_ZSDT0001[] IS NOT INITIAL.
      SL_ROWS-INDEX = 1.
      APPEND SL_ROWS TO TL_ROWS.
    ENDIF.
  ENDIF.

  IF TL_ROWS[] IS INITIAL.
    MESSAGE I836 WITH TEXT-016.
    EXIT.
  ENDIF.

  LOOP AT TL_ROWS INTO SL_ROWS.
    READ TABLE T_ZSDT0001 INTO SL_ROMA INDEX SL_ROWS-INDEX.
    APPEND SL_ROMA TO T_ROMA.
*    CLEAR: SL_ROWS, SL_ROMA.
  ENDLOOP.

  VL_SLD_CHECKED = 'X'. "CS2017000598 22.05.2017

  IF ( SL_ROMA-MATNR IS NOT INITIAL ). "CS2017000598 22.05.2017
    "Ler novamente o item OV por material ALRS
    SELECT SINGLE *
      FROM VBAP
      INTO S_VBAP
      WHERE  VBELN IN P_VBELN
    AND    MATNR EQ SL_ROMA-MATNR.

    IF  SY-SUBRC NE 0.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE I836 WITH 'Item do romaneio não encontrado' VG_ROMA.
          EXIT.
        WHEN ABAP_TRUE.
          MESSAGE I836 WITH 'Item do romaneio não encontrado' VG_ROMA INTO DATA(L_MESG).
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

*   Desmenbramento OPUS - Saldo
    SELECT SINGLE SUM( LFIMG ) INTO VG_LFIMG
      FROM LIPS
     WHERE VGBEL EQ S_VBAP-VBELN
    AND   MATNR EQ S_VBAP-MATNR.

* Alteração - RIM-SKM-IR129749-06.03.23 - Inicio
    SELECT SINGLE SUM( WMENG ) INTO VG_WMENG
      FROM VBEP
     WHERE VBELN EQ S_VBAP-VBELN
    AND   POSNR EQ S_VBAP-POSNR.

* *>
**AJUSTE NÃO NECESSÁRIO POIS TRATATIVA REALIZADA NA GERAÇÃO DA OV TRANSAÇÃO ZSDT0044 PARA QUE NA MESMA OV NÃO TENHA ITENS COM MESMO MATERIAL
*    SELECT SINGLE SUM( wmeng ) INTO vg_wmeng
*    FROM vbep AS vb
*     INNER JOIN vbap AS vp ON vp~vbeln = vb~vbeln AND  vp~posnr = vb~posnr
*    WHERE vb~vbeln = s_vbap-vbeln
*     AND  vp~matnr = s_vbap-matnr.
* Alteração - RIM-SKM-IR129749-06.03.23 - Fim

    VG_ROMA = 0.
    LOOP AT T_ROMA INTO SL_ZSDT0001.
      IF SL_ZSDT0001-QTDE_REMESSA IS NOT INITIAL.
        VG_ROMA = VG_ROMA + SL_ZSDT0001-QTDE_REMESSA.
      ELSE.
        VG_ROMA = VG_ROMA + SL_ZSDT0001-PESO_LIQ.
      ENDIF.
    ENDLOOP.

    VG_SALDO = VG_WMENG - VG_LFIMG.
    VG_LFIMG = VG_LFIMG + VG_ROMA.

    IF VG_LFIMG GT VG_WMENG.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE I836 WITH TEXT-023 TEXT-024.
          MESSAGE I836 WITH 'Saldo Atual:' VG_SALDO ' Volume Romaneio(s):' VG_ROMA.
        WHEN ABAP_TRUE.
          L_MESG = 'Sem saldo forneceimento p/ romaneio(s)' && 'Verif. Op. Demembramento OPUS' && 'Saldo Atual:' && VG_SALDO && ' Volume Romaneio(s):' && VG_ROMA.
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      CLEAR: VL_SLD_CHECKED.
    ELSEIF ( S_VBAP-KWMENG <> S_VBAP-NTGEW ).

      IF NOT ( S_VBAP-UMZIZ > 0 AND S_VBAP-UMZIN > 0 AND S_VBAP-UMZIZ NE S_VBAP-UMZIN ). "Não realizar a validação com o faturamento possuir unidade de conversão ( Quantidade OV diferente Peso Liquido )
*-#133089-21.02.2024-JT-inicio
        CASE VG_FATURAMENTO_AUTOM.
          WHEN ABAP_OFF.
            MESSAGE I836 WITH 'Quantidade da Ordem diferente do peso liquido.'.
            MESSAGE I836 WITH 'Entre em contato com a área de execução.'.
          WHEN ABAP_TRUE.
            L_MESG = 'Quantidade da Ordem diferente do peso liquido.' && 'Entre em contato com a área de execução.'.
            LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
            LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CLEAR: VL_SLD_CHECKED.
      ENDIF.

    ENDIF.

  ENDIF.

  IF VL_SLD_CHECKED IS NOT INITIAL. "CS2017000598 22.05.2017

    IF ( SL_ROMA-MATNR IS NOT INITIAL ). "CS2017000598 22.05.2017
      "Cria Remessa
      MESSAGE S836 WITH 'Saldo Atual:' VG_SALDO ' Volume Romaneio(s):' VG_ROMA.
    ENDIF.

    V_CHARG  = SL_ROMA-NR_SAFRA.
    V_LGORT  = S_VBAP-LGORT. "'ARMZ' 180118.

    IF ( S_VBAP-CHARG IS INITIAL ) AND ( SL_ROMA-ID_INTERFACE NE '48' AND
                                         SL_ROMA-ID_INTERFACE NE '49' AND
                                         SL_ROMA-ID_INTERFACE NE '51' AND
                                         SL_ROMA-ID_INTERFACE NE '52' ). "CS2017000598 22.05.2017
      SELECT SINGLE XCHPF
        INTO V_XCHPF
        FROM MARA
      WHERE MATNR = S_VBAP-MATNR.
      IF V_XCHPF = 'X'.

*-#133089-21.02.2024-JT-inicio
        CASE VG_FATURAMENTO_AUTOM.
          WHEN ABAP_OFF.
            CLEAR: TL_FIELDS, TL_FIELDS[].
            TL_FIELDS-TABNAME    = 'VBPOK'.
            TL_FIELDS-FIELDNAME  = 'CHARG'.
            TL_FIELDS-FIELD_OBL  = 'X'.
            APPEND TL_FIELDS.

            CALL FUNCTION 'POPUP_GET_VALUES'
              EXPORTING
                POPUP_TITLE     = 'Informe o Lote da Remessa'
              IMPORTING
                RETURNCODE      = LV_RETURN
              TABLES
                FIELDS          = TL_FIELDS
              EXCEPTIONS
                ERROR_IN_FIELDS = 1
                OTHERS          = 2.
            IF SY-SUBRC IS INITIAL.
              V_CHARG  = TL_FIELDS-VALUE.
            ENDIF.
          WHEN ABAP_TRUE.
            V_CHARG = SY-DATUM(4).
        ENDCASE.
*-#133089-21.02.2024-JT-fim

        SELECT SINGLE *
          FROM MCHB
          INTO WA_MCHB
          WHERE MATNR = S_VBAP-MATNR
          AND   WERKS = S_VBAP-WERKS
          AND   LGORT = S_VBAP-LGORT
          AND   CHARG = V_CHARG.

        IF SY-SUBRC NE 0.
          SELECT SINGLE *
          FROM MSKA
          INTO WA_MSKA
          WHERE MATNR = S_VBAP-MATNR
          AND   WERKS = S_VBAP-WERKS
          AND   LGORT = S_VBAP-LGORT
          AND   CHARG = V_CHARG.

          IF SY-SUBRC NE 0.
            IF WA_MSKA-KALAB LE 0. SY-SUBRC = 4.ENDIF.
          ENDIF.
        ENDIF.

        IF SY-SUBRC = 0.
          SELECT SINGLE *
            FROM MSKA
            INTO @DATA(_MSKA)
             WHERE MATNR  = @S_VBAP-MATNR
              AND   WERKS = @S_VBAP-WERKS
              AND   LGORT = @S_VBAP-LGORT
              AND   CHARG = @V_CHARG
              AND   VBELN = @S_VBAP-VBELN.
          IF _MSKA-KALAB NE SL_ROMA-PESO_LIQ.
*-#133089-21.02.2024-JT-inicio
            CASE VG_FATURAMENTO_AUTOM.
              WHEN ABAP_OFF.
                MESSAGE 'Peso do romaneio difere do peso produzido,ajustar produção!' TYPE 'I'.
                EXIT.
              WHEN ABAP_TRUE.
                L_MESG = 'Peso do romaneio difere do peso produzido,ajustar produção!'.
                LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
                LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.

          V_LGORT = S_VBAP-LGORT.

          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              CLIENT        = SY-MANDT
              SETNR         = 'WERKS_RUN_COGI'
              CLASS         = '0000'
            TABLES
              SET_VALUES    = TL_EXEC_COGI
            EXCEPTIONS
              SET_NOT_FOUND = 1
              OTHERS        = 2.

          TRY.
              TL_EXEC_COGI = TL_EXEC_COGI[ FIELD = 'WERKS' FROM = S_VBAP-WERKS ].

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = V_CHARG
                IMPORTING
                  OUTPUT = ORDEM.

              SELECT SINGLE *
                FROM AFPO
                INTO @DATA(_ORDER_ITEM)
              WHERE AUFNR = @ORDEM.

              IF ( _ORDER_ITEM-WEMNG IS INITIAL ).

                "//Forçar entrada de mercadoria
                SUBMIT CORUAFWP EXPORTING LIST TO MEMORY
                  WITH PAFEHL  EQ 'X'
                  WITH PAWERKS = S_VBAP-WERKS
                  WITH PALGORT = S_VBAP-LGORT
                  AND RETURN.

                CALL FUNCTION 'LIST_FREE_MEMORY'.

                "//Get cogi errors
                SELECT SINGLE *
                  FROM AFFW
                  INTO @DATA(_ERRORS)
                WHERE CHARG = @V_CHARG.

                IF ( SY-SUBRC IS INITIAL ).
                  SUBMIT CORUAFFW
                    WITH S_WERKS  = S_VBAP-WERKS
                    WITH S_LGORT  = S_VBAP-LGORT
                    WITH S_MATNR  = S_VBAP-MATNR
                    WITH P_DISPLY = ABAP_TRUE
                    AND RETURN.

                  EXIT.
                ENDIF.
              ENDIF.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

        ELSE.
*-#133089-21.02.2024-JT-inicio
          CASE VG_FATURAMENTO_AUTOM.
            WHEN ABAP_OFF.
              MESSAGE 'Lote Inválido!' TYPE 'I'.
              EXIT.
            WHEN ABAP_TRUE.
              L_MESG = 'Lote Inválido!'.
              LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
              LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.

      ENDIF.
    ENDIF.


    TRY.  "*-#133089-21.02.2024-JT-inicio
        PERFORM Z_CRIA_REMESSA.
      CATCH ZCX_ERROR INTO DATA(EX_ERROR). "*-#133089-21.02.2024-JT-inicio
        MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO L_MESG. ""*-#133089-21.02.2024-JT-inicio
        LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ). "*-#133089-21.02.2024-JT-inicio
    ENDTRY.            "*-#133089-21.02.2024-JT-inicio

  ENDIF.
ENDFORM.                    " Z_GERA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM Z_MONTA_FIELDCAT.

  REFRESH: T_FCAT,
           T_TOOL.

  DATA(LT_ZSDT0001) = T_ZSDT0001[].
  DELETE LT_ZSDT0001 WHERE QTDE_REMESSA IS INITIAL.
  DESCRIBE TABLE LT_ZSDT0001 LINES DATA(LV_LINES).

* Preenche FieldCat
  PERFORM Z_PREENCHE_FIELDCAT USING:
    C_TABLE 'NR_ROMANEIO'  TEXT-005 12 SPACE,
    C_TABLE 'PARID'        TEXT-006 10 SPACE,
    C_TABLE 'BRANCH'       TEXT-007 06 SPACE,
    C_TABLE 'PLACA_CAV'    TEXT-008 07 SPACE,
    C_TABLE 'DT_MOVIMENTO' TEXT-009 10 SPACE,
    C_TABLE 'PESO_LIQ'     TEXT-010 16 SPACE.

  IF LV_LINES >= 1.
    PERFORM Z_PREENCHE_FIELDCAT USING:
      C_TABLE 'QTDE_REMESSA' TEXT-030 07 SPACE,
      C_TABLE 'UM_REMESSA'   TEXT-031 07 SPACE.
  ENDIF.

  PERFORM Z_PREENCHE_FIELDCAT USING:
    C_TABLE 'TP_MOVIMENTO' TEXT-011 06 SPACE,
    C_TABLE 'MATNR'        TEXT-012 14 'X'  ,
    C_TABLE 'TP_FRETE'     TEXT-013 07 SPACE.


* Monta Layout
  PERFORM Z_LAYOUT.

* Deleta Botões
  PERFORM Z_DELETA_BOT USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM Z_PREENCHE_FIELDCAT USING P_TABLE TYPE C
                               P_FIELD TYPE C
                               P_DESC  TYPE C
                               P_LEN   TYPE N
                               P_ZERO  TYPE C.

  DATA SL_FCAT TYPE LVC_S_FCAT.

  SL_FCAT-TABNAME   = P_TABLE.
  SL_FCAT-FIELDNAME = P_FIELD.
  SL_FCAT-SCRTEXT_L = P_DESC.
  SL_FCAT-SCRTEXT_M = P_DESC.
  SL_FCAT-SCRTEXT_S = P_DESC.
  SL_FCAT-OUTPUTLEN = P_LEN.
  SL_FCAT-NO_ZERO   = P_ZERO.

  APPEND SL_FCAT TO T_FCAT.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM Z_LAYOUT.

  CLEAR S_LAYOUT.

  S_LAYOUT-ZEBRA = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM Z_DELETA_BOT USING P_BOT TYPE C.

  DATA SL_TOOL TYPE UI_FUNC.

  SL_TOOL = P_BOT.
  APPEND SL_TOOL TO T_TOOL.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE ZM_STATUS OUTPUT.

  CASE SY-DYNNR.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

  IF VG_TEXTO IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GP1'.
        SCREEN-OUTPUT    = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF VG_DESC IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GP2'.
        SCREEN-OUTPUT    = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE ZM_OBJ_ALV OUTPUT.

* Instancia Container
  PERFORM: Z_INST_CONT ,
* Instancia Alv
           Z_INST_ALV  ,
* Instancia Eventos
           Z_INST_EVENT,
* Exibe Alv
           Z_EXIBE_ALV .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM Z_INST_CONT.

  CHECK S_CONT IS INITIAL.

  CREATE OBJECT S_CONT
    EXPORTING
      CONTAINER_NAME = 'CC_ALV'.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE I836 WITH TEXT-014.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM Z_INST_ALV.

  CHECK S_ALV IS INITIAL.

  CREATE OBJECT S_ALV
    EXPORTING
      I_PARENT = S_CONT.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE I836 WITH TEXT-015.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM Z_INST_EVENT.

  CHECK S_EVENT IS INITIAL.

  CREATE OBJECT S_EVENT.
  SET HANDLER: S_EVENT->ZM_HANDLE_USER_COMMAND FOR S_ALV,
               S_EVENT->ZM_HANDLE_TOOLBAR      FOR S_ALV.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM Z_EXIBE_ALV.

  DATA VL_INT TYPE INT4.

  CALL METHOD S_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT                     = 'X'
      IS_LAYOUT                     = S_LAYOUT
      IT_TOOLBAR_EXCLUDING          = T_TOOL
    CHANGING
      IT_OUTTAB                     = T_ZSDT0001
      IT_FIELDCATALOG               = T_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

  "CALL METHOD s_alv->set_ready_for_input.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE ZM_USER_COMMAND INPUT.

  CLEAR SY-UCOMM.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE ZM_EXIT_COMMAND INPUT.

  CASE SY-DYNNR.
    WHEN '0100'.
      CASE SY-UCOMM.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR SY-UCOMM.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                                Cria Remessa                          *
*----------------------------------------------------------------------*
FORM Z_CRIA_REMESSA  RAISING ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio..

  DATA: VL_DELIVERY   TYPE BAPISHPDELIVNUMB-DELIV_NUMB,
        V_XBLNR       TYPE VBRK-XBLNR,
        VREFER        TYPE ZSDT0001-CH_REFERENCIA,
        TL_ITEM       TYPE TABLE OF BAPIDLVREFTOSALESORDER,
        SL_ITEM       TYPE BAPIDLVREFTOSALESORDER,
        TL_RETURN     TYPE TABLE OF BAPIRET2,
        SL_ZSDT0001   TYPE ZSDT0001,
        SL_ZLEST0002  TYPE ZLEST0002,
        SL_DATA_REM   TYPE LEDAT,
        SL_SHIP_POINT TYPE VSTEL,
        WA_SETLEAF    TYPE SETLEAF,
        WL_ERRO(1),
        IT_SETLEAF    LIKE TABLE OF WA_SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
        ET_VBKOK      TYPE VBKOK,
        VG_WADAT      TYPE LIKP-WADAT,
        VG_LFDAT      TYPE LIKP-LFDAT,
        W_MSN         TYPE TYPE_MSN,
        MSG_TEXT      TYPE STRING,
        W_RETENT(1).

  CLEAR: T_VBPAVB[], T_SADRVB[], WA_GOODSMVT_ITEM, T_GOODSMVT_ITEM[], T_SADRVB[], T_VBPAVB[], WA_VBPAVB,T_RETURN[], WA_DEPARA.

  IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).

    CALL FUNCTION 'SD_PARTNER_READ'
      EXPORTING
        F_VBELN  = S_VBAP-VBELN
        OBJECT   = 'VBPA'
      TABLES
        I_XVBADR = T_SADRVB
        I_XVBPA  = T_VBPAVB.

    DELETE T_VBPAVB WHERE PARVW NE 'Z1'.

    IF NOT T_VBPAVB[] IS INITIAL.

      READ TABLE T_ROMA INTO SL_ZSDT0001 INDEX 1.
      READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.

*      SELECT SINGLE * INTO WA_DEPARA
*        FROM ZSDT_DEPARA_DEPO
*       WHERE WERKS EQ S_VBAP-VSTEL
*         AND LIFNR EQ WA_VBPAVB-LIFNR.

      DATA(_OPERA) = 'RF'.
      IF  S_VBAK-AUART EQ 'ZIND'.
        _OPERA = 'RI'.
      ENDIF.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255

*      CALL FUNCTION 'Z_BUSCA_DEPARA'
*        EXPORTING
*          i_werks          = s_vbap-vstel
*          i_lifnr          = wa_vbpavb-lifnr
*          i_opera          = _opera
*        IMPORTING
*          zsdt_depara_depo = wa_depara.

      ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
          EXPORTING
            I_WERKS       = S_VBAP-VSTEL
            I_LIFNR       = WA_VBPAVB-LIFNR
            I_OPERACAO    = _OPERA
          IMPORTING
           E_SINGLE_DEPARA          = WA_DEPARA ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM


      IF WA_DEPARA IS NOT INITIAL.
        WA_GOODSMVT_ITEM-MOVE_PLANT = WA_DEPARA-WERKS_V.
      ELSE.
        PERFORM: Z_MONTA_ERRO_DEPARA USING S_VBAP-WERKS SPACE.
      ENDIF.
    ENDIF.

  ENDIF.

  "ALRS
  CLEAR WL_ERRO.
  IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
    LOOP AT T_ROMA INTO SL_ZSDT0001.
      SELECT SINGLE *
        FROM VBAK
        INTO WA_VBAK
      WHERE VBELN = SL_ZSDT0001-VBELN.
      IF ( WA_VBAK-KVGR3 = 'C' AND SL_ZSDT0001-TP_TRANSGENIA = 'RR' ) OR
         ( WA_VBAK-KVGR3 = 'R' AND SL_ZSDT0001-TP_TRANSGENIA = 'CO' ).

*-#133089-21.02.2024-JT-inicio
        CASE VG_FATURAMENTO_AUTOM.
          WHEN ABAP_OFF.
            MESSAGE S836(SD) WITH 'O Tipo de Produto informado no Romaneio :'
                                  SL_ZSDT0001-TP_TRANSGENIA
                                  'esta diferente da Ordem d Venda :'
                                  WA_VBAK-KVGR3.
          WHEN ABAP_TRUE.
            MESSAGE S836(SD) WITH 'O Tipo de Produto informado no Romaneio :' SL_ZSDT0001-TP_TRANSGENIA 'esta diferente da Ordem d Venda :' WA_VBAK-KVGR3 INTO DATA(L_MESG).
            LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
            LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        WL_ERRO = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT T_ROMA INTO SL_ZSDT0001.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0017)
      FROM ZMMT0017
     WHERE MATNR       EQ @SL_ZSDT0001-MATNR
       AND CENTRO_FIXO EQ @SL_ZSDT0001-BRANCH.

    IF SY-SUBRC IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(LWA_SET_TP_OV_VENDA)
     WHERE SETNAME EQ 'TIPO_OV_VENDA'
       AND VALFROM EQ @S_VBAK-AUART.

    CHECK SY-SUBRC IS NOT INITIAL.

    IF SL_ZSDT0001-ID_INTERFACE EQ '48' OR
       SL_ZSDT0001-ID_INTERFACE EQ '49' OR
       SL_ZSDT0001-ID_INTERFACE EQ '51' OR
       SL_ZSDT0001-ID_INTERFACE EQ '52'.
      CONTINUE.
    ENDIF.

    TRY .
        "Verificar Depósito da Ordem de Venda
        ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
            )->GET_DEPOSITO_MATERIAL_FILIAL(
                EXPORTING
                  I_MATNR      = SL_ZSDT0001-MATNR    " Nº do material
                  I_TP_PRODUTO = CONV #( COND STRING( WHEN SL_ZSDT0001-TP_TRANSGENIA(1) EQ 'C' THEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO ELSE 'RR' ) )    " Tipo de Produto
                  I_BUKRS      = SL_ZSDT0001-BUKRS    " Empresa
                  I_BRANCH     = SL_ZSDT0001-BRANCH    " Local de negócios
                IMPORTING
                  E_LGORT      = DATA(E_LGORT)    " Depósito
            ).

      CATCH ZCX_DEPOSITO INTO DATA(EX_DEPOSITO).    " .
*-#133089-21.02.2024-JT-inicio
        CASE VG_FATURAMENTO_AUTOM.
          WHEN ABAP_OFF.
            EX_DEPOSITO->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          WHEN ABAP_TRUE.
            MESSAGE ID EX_DEPOSITO->MSGID TYPE 'S' NUMBER EX_DEPOSITO->MSGNO WITH EX_DEPOSITO->MSGV1 EX_DEPOSITO->MSGV2 EX_DEPOSITO->MSGV3 EX_DEPOSITO->MSGV4 INTO L_MESG.
            LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
            LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        WL_ERRO = 'X'.
    ENDTRY.

    SELECT SINGLE * FROM VBAP INTO @DATA(WA_VBAP) WHERE VBELN EQ @SL_ZSDT0001-VBELN.
    IF WA_VBAP-LGORT NE E_LGORT.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE S011(ZODVENDA) WITH WA_VBAP-LGORT E_LGORT.
        WHEN ABAP_TRUE.
          MESSAGE S011(ZODVENDA) WITH WA_VBAP-LGORT E_LGORT INTO L_MESG.
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      WL_ERRO = 'X'.
    ENDIF.

  ENDLOOP.


  IF WL_ERRO = 'X'.
    EXIT.
  ENDIF.

  IF ( ( ( S_VBAK-AUART NE 'ZRDC' ) AND ( S_VBAK-AUART NE 'ZRFL' ) AND ( S_VBAK-AUART NE 'ZIND' ) ) AND ( WA_DEPARA IS INITIAL ) ) OR
     ( ( ( S_VBAK-AUART EQ 'ZRDC' ) OR  ( S_VBAK-AUART EQ 'ZRFL' ) OR  ( S_VBAK-AUART EQ 'ZIND' ) ) AND ( NOT WA_DEPARA IS INITIAL ) ).

    LOOP AT T_ROMA INTO SL_ZSDT0001.

      CLEAR: IT_SETLEAF[].

      "Conversao S4 Hana 27-07-23
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = sl_zsdt0001-matnr
*        IMPORTING
*          output = sl_zsdt0001-matnr.
      ""Conversao S4 Hana 27-07-23

      IF  S_VBAP-WERKS =  SL_ZSDT0001-BRANCH. "Nao gera residuo centro virtual

        DATA(_BUKRS_MAT_EXC) = ABAP_FALSE.
        PERFORM F_CHECK_EXCECAO_RESIDUO USING SL_ZSDT0001-MATNR
                                              SL_ZSDT0001-BUKRS
                                     CHANGING _BUKRS_MAT_EXC.


        SELECT * INTO TABLE IT_SETLEAF
           FROM SETLEAF
           WHERE SETNAME EQ 'RESIDUO'
        AND VALFROM EQ SL_ZSDT0001-MATNR.

        IF SY-SUBRC = 0 AND _BUKRS_MAT_EXC EQ ABAP_FALSE.
          SELECT SINGLE *
            FROM ZMMT0074
            INTO SL_ZMMT0074
            WHERE WERKS = S_VBAP-WERKS
          AND   MATNR = SL_ZSDT0001-MATNR.
          IF SY-SUBRC EQ 0.
            CLEAR W_RETENT.
            PERFORM Z_ENTRADA_RESIDUO USING SL_ZSDT0001 VL_MAT_DOC_R VL_MATDOCUMENTYEAR_R W_RETENT.
            IF W_RETENT = 'X'.
              EXIT.
            ENDIF.
          ELSE.
            CONCATENATE 'Material obrigatório parâmetro Entrada Residuo, procure Depto Estoque' '!' INTO MSG_TEXT SEPARATED BY SPACE.
            W_MSN-NR_ROMANEIO = SL_ZSDT0001-NR_ROMANEIO.
            W_MSN-MESSAGEM = MSG_TEXT.
            W_MSN-TP_MSN   = 'E'.
            APPEND W_MSN TO T_MSN.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.


      REFRESH: TL_ITEM, TL_RETURN.

      CLEAR: VL_DELIVERY, SL_ITEM, SL_ZLEST0002.

      READ TABLE T_ZLEST0002 INTO SL_ZLEST0002
        WITH KEY PC_VEICULO = SL_ZSDT0001-PLACA_CAV
        BINARY SEARCH.

      IF ( SL_ZSDT0001-MATNR IS INITIAL ). "CS2017000598 22.05.2017
        PERFORM Z_ADD_ITENS_ROM TABLES TL_ITEM
                                 USING SL_ZSDT0001.

        READ TABLE TG_VBAP WITH KEY VBELN = P_VBELN-LOW.

        VG_ERDAT           = SL_ZSDT0001-DT_MOVIMENTO.
        SL_SHIP_POINT      = TG_VBAP-VSTEL.

      ELSE.
        SL_ITEM-REF_DOC    = S_VBAP-VBELN.
        SL_ITEM-REF_ITEM   = S_VBAP-POSNR.

        IF SL_ZSDT0001-QTDE_REMESSA IS NOT INITIAL.
          SL_ITEM-DLV_QTY    = SL_ZSDT0001-QTDE_REMESSA.
        ELSE.
          SL_ITEM-DLV_QTY    = SL_ZSDT0001-PESO_LIQ.
        ENDIF.

        IF P_PESO GT 0.
          SL_ITEM-DLV_QTY  = P_PESO.
        ENDIF.

        SL_ITEM-SALES_UNIT = S_VBAP-VRKME.
        VG_ERDAT           = SL_ZSDT0001-DT_MOVIMENTO.
        SL_SHIP_POINT      = S_VBAP-VSTEL.
        APPEND SL_ITEM TO TL_ITEM.
      ENDIF.

      IF V_USER IS INITIAL.
        SL_DATA_REM = SY-DATUM.
      ELSE.
        SL_DATA_REM = SL_ZSDT0001-DT_MOVIMENTO.

        IF SL_ZSDT0001-FAT_CONTINGENCIA_ECC EQ ABAP_TRUE.
          DATA: LWA_FATURAMENTO_ECC TYPE ZDE_COMPARE_FATURAMENTO.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              I_CH_REFERENCIA         = SL_ZSDT0001-CH_REFERENCIA
              I_GET_DADOS_FAT_ECC     = ABAP_TRUE
            IMPORTING
              E_DADOS_FATURAMENTO_ECC = LWA_FATURAMENTO_ECC.

          IF LWA_FATURAMENTO_ECC-DATA_LCTO_NF IS INITIAL.
            MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
            RETURN.
          ENDIF.

          SL_DATA_REM = LWA_FATURAMENTO_ECC-DATA_LCTO_NF.
        ENDIF.

        PERFORM MEMORIZAR_DT_MOVIMENTO_BADI USING SL_DATA_REM.
      ENDIF.

      "Exporta variavel de romaneio para memoria e exit MV50AFZ1 importar
      VREFER = SL_ZSDT0001-CH_REFERENCIA.
      EXPORT VREFER TO MEMORY ID 'MREFER'.
      "
      " bloqueio lote
      IF ( SL_ZSDT0001-MATNR IS NOT INITIAL ).
        CALL FUNCTION 'ENQUEUE_EMMCH1E'
          EXPORTING
            MODE_MCH1      = 'E'
            MANDT          = SY-MANDT
            MATNR          = SL_ZSDT0001-MATNR
            CHARG          = V_CHARG
            _SCOPE         = '2'
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

        IF SY-SUBRC <> 0.
          VL_DELIVERY_C = '9999999999'. "Erro bloqueio
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD VL_DELIVERY_C.
          EXIT.
        ENDIF.
      ENDIF.
      "
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        EXPORTING
          SHIP_POINT        = SL_SHIP_POINT
          DUE_DATE          = SL_DATA_REM
        IMPORTING
          DELIVERY          = VL_DELIVERY
        TABLES
          SALES_ORDER_ITEMS = TL_ITEM
          RETURN            = TL_RETURN.

      IF VL_DELIVERY IS INITIAL.

        PERFORM F_ESTORNO_RES  CHANGING SL_ZSDT0001.

*     Retorna Erro
        PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                              USING SL_ZSDT0001.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
        "GRAVA REFERENCIA ROMANEIO
        V_XBLNR = SL_ZSDT0001-CH_REFERENCIA.
        DO 100 TIMES.
          SELECT SINGLE *
            FROM LIKP
            INTO @DATA(W_LIKP)
            WHERE VBELN = @VL_DELIVERY.
          IF SY-SUBRC = 0.
            UPDATE LIKP SET XBLNR = V_XBLNR
            WHERE VBELN = VL_DELIVERY.
            COMMIT WORK.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
        " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 1
        IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
          CLEAR: ZSDT0023.
          ZSDT0023-VBELN         = VL_DELIVERY.
          ZSDT0023-VBELV         = S_VBAP-VBELN.
          ZSDT0023-CH_REFERENCIA = SL_ZSDT0001-CH_REFERENCIA.

          ZSDT0023-WERKS_V       = WA_DEPARA-WERKS_V.
          IF S_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL. "ALRS 04.05.2015
            ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_T.
          ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
            ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_F.
          ELSE.
            ZSDT0023-LGORT_V       = WA_DEPARA-LGORT.
          ENDIF.
          ZSDT0023-DT_SAIDA      = SY-DATUM.
          ZSDT0023-HS_SAIDA      = SY-UZEIT.
          INSERT ZSDT0023.
          COMMIT WORK.
        ENDIF.

*     Retorna Sucesso
        PERFORM: Z_MONTA_SUCESSO USING VL_DELIVERY
                                       SL_ZSDT0001
                                       SPACE      ,
*     Salva Textos
                 Z_SALVA_TEXTO   USING VL_DELIVERY
                                       SL_ZSDT0001,
*BBKO/Vagner Santos - Início da alteração - 02.10.2010
                 Z_INCLUIR_AGENTE_FRETE USING VL_DELIVERY
                                              SL_ZSDT0001,
*BBKO/Vagner Santos - Fim da alteração - 02.10.2010
*     Picking
                 Z_PICKING       USING VL_DELIVERY
                                       SL_ZSDT0001
                                       SL_DATA_REM.
*   Registrar SM
*               z_reg_sm        USING vl_delivery.
        DELETE T_ZSDT0001 WHERE CH_REFERENCIA EQ SL_ZSDT0001-CH_REFERENCIA.
        VL_DELIVERY_C = VL_DELIVERY.
        IF V_NR_ROMANEIO IS NOT INITIAL AND SY-CALLD = 'X'.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD VL_DELIVERY_C.
        ENDIF.
      ENDIF.

      IF ( SL_ZSDT0001-MATNR IS NOT INITIAL ).
        CALL FUNCTION 'DEQUEUE_EMMCH1E'
          EXPORTING
            MODE_MCH1 = 'E'
            MANDT     = SY-MANDT
            MATNR     = SL_ZSDT0001-MATNR
            CHARG     = V_CHARG.
      ENDIF.


*      CLEAR SL_ZSDT0001.

    ENDLOOP.
  ENDIF.


  IF NOT T_MSN[] IS INITIAL.
    IF V_NR_ROMANEIO IS NOT INITIAL AND SY-CALLD = 'X'.
      READ TABLE T_MSN INTO SL_MSN WITH KEY TP_MSN = 'E'.
      IF SY-SUBRC = 0.
        REFRESH TI_ZLEST0100.
        CLEAR VL_PONTEIRO.
        SELECT  MAX( CONT )
              FROM ZLEST0100
              INTO VL_PONTEIRO
        WHERE CH_REFERENCIA = SL_ZSDT0001-CH_REFERENCIA.

        IF SY-SUBRC = 0.
          ADD 1 TO VL_PONTEIRO.
        ELSE.
          VL_PONTEIRO = 1.
        ENDIF.
        LOOP AT T_MSN INTO SL_MSN.
          WA_ZLEST0100-MANDT      = SY-MANDT.
          WA_ZLEST0100-CH_REFERENCIA   = SL_ZSDT0001-CH_REFERENCIA.
          WA_ZLEST0100-MSGTYP     = SL_MSN-TP_MSN.
          WA_ZLEST0100-MSGSPRA    = SY-LANGU.
          WA_ZLEST0100-MSGID      = 'OPUS'.
          WA_ZLEST0100-MSGNR      = '000'.
          WA_ZLEST0100-MSGV1      = SL_MSN-MESSAGEM.
          WA_ZLEST0100-DATA       = SY-DATUM.
          WA_ZLEST0100-HORA       = SY-UZEIT.
          WA_ZLEST0100-USUARIO    = SY-UNAME.
          WA_ZLEST0100-CONT       = VL_PONTEIRO.

          APPEND WA_ZLEST0100 TO TI_ZLEST0100.
          ADD 1 TO VL_PONTEIRO.
        ENDLOOP.
        IF TI_ZLEST0100[] IS NOT INITIAL.
          MODIFY ZLEST0100 FROM TABLE TI_ZLEST0100.
        ENDIF.
        CLEAR VL_DELIVERY_C.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD VL_DELIVERY_C.

        IF VG_FATURAMENTO_AUTOM IS INITIAL.  "*-#133089-21.02.2024-JT-inicio
          CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
            TABLES
              TABLE    = T_MSN
            EXCEPTIONS
              FB_ERROR = 1
              OTHERS   = 2.

          IF NOT SY-SUBRC IS INITIAL.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
        ENDIF.
      ENDIF.
*      READ TABLE T_MSN INTO SL_MSN WITH KEY TP_MSN = 'S'.
*      IF SY-SUBRC = 0.
*        EXIT.
*      ENDIF.
    ELSE.
      IF VG_FATURAMENTO_AUTOM IS INITIAL.  "*-#133089-21.02.2024-JT-inicio
        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            TABLE    = T_MSN
          EXCEPTIONS
            FB_ERROR = 1
            OTHERS   = 2.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
    ENDIF.

*-#133089-21.02.2024-JT-inicio
    CASE VG_FATURAMENTO_AUTOM.
      WHEN ABAP_OFF.
      WHEN ABAP_TRUE.
        LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_TAB_ERRO = T_MSN I_STATUS = 'REME' ).
        LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

ENDFORM.                    " Z_CRIA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                             Retorna Erro                             *
*----------------------------------------------------------------------*
FORM Z_MONTA_ERRO TABLES P_RETURN   STRUCTURE BAPIRET2
                   USING P_ZSDT0001 TYPE ZSDT0001.

  DATA: SL_RETURN TYPE BAPIRET2.


  LOOP AT P_RETURN INTO SL_RETURN.
    SL_MSN-NR_ROMANEIO = P_ZSDT0001-NR_ROMANEIO.
    SL_MSN-TP_MSN      = SL_RETURN-TYPE.
    SL_MSN-MESSAGEM    = SL_RETURN-MESSAGE.
    APPEND SL_MSN TO T_MSN.
    CLEAR: SL_RETURN,
           SL_MSN   .
  ENDLOOP.

ENDFORM.                    " Z_MONTA_ERRO
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_SUCESSO                                          *
*&---------------------------------------------------------------------*
*                            Retorna Sucesso                           *
*----------------------------------------------------------------------*
FORM Z_MONTA_SUCESSO USING P_DELIVERY TYPE VBELN_VL
                           P_ZSDT0001 TYPE ZSDT0001
                           P_REM      TYPE CHAR1.

  DATA: SL_MSN  TYPE TYPE_MSN,
        SL_TEXT TYPE CHAR50.

  SL_MSN-NR_ROMANEIO = P_ZSDT0001-NR_ROMANEIO.
  SL_MSN-TP_MSN      = 'S'.

  CASE P_REM.
    WHEN SPACE.
      SL_TEXT = TEXT-017.
    WHEN 'F'.
      SL_TEXT = TEXT-029.
    WHEN 'T'.
      SL_TEXT = TEXT-026.
  ENDCASE.


  IF P_ZSDT0001 IS INITIAL.
    SL_TEXT = TEXT-028.
  ENDIF.

  CONCATENATE SL_TEXT
              P_DELIVERY
         INTO SL_MSN-MESSAGEM SEPARATED BY SPACE.

  APPEND SL_MSN TO T_MSN.

ENDFORM.                    " Z_MONTA_SUCESSO

*&---------------------------------------------------------------------*
*&      Form  Z_PICKING                                                *
*&---------------------------------------------------------------------*
*                                  Picking                             *
*----------------------------------------------------------------------*
FORM Z_PICKING USING P_DELIVERY TYPE VBELN_VL
                     P_ZSDT0001 TYPE ZSDT0001
                     P_DATA_REM TYPE LEDAT.

  DATA: SL_VBKOK_WA          TYPE VBKOK,
        TL_VBPOK             TYPE TABLE OF VBPOK,
        TL_PROT              TYPE TABLE OF PROTT,
        SL_VBPOK             TYPE VBPOK,
        SL_PROT              TYPE PROTT,
        TL_RETURN            TYPE BAPIRET2_T,
        SL_RETURN            TYPE BAPIRET2,
        VL_MSN               TYPE CHAR200,
        VL_AUART             TYPE ZZAUART,
        VL_TKNUM             TYPE VTTK-TKNUM,
        VG_SYSUBRC           TYPE SY-SUBRC,
        VG_ERRO_MAT_S        TYPE C LENGTH 1,
        VG_ERRO_MAT_E        TYPE C LENGTH 1,
        VG_ERRO_PIC          TYPE C LENGTH 1,
        V_PESO_BRUTO         TYPE BRGEW_15,
        V_PESO_LIQ           TYPE NTGEW_15,
        VL_MAT_DOC_S         TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
        VL_MATDOCUMENTYEAR_S TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
        VL_MAT_DOC_E         TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
        VL_MATDOCUMENTYEAR_E TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
        VL_FAT               TYPE VBAK-VBELN,
        WA_SETLEAF           TYPE SETLEAF,
        IT_SETLEAF           LIKE TABLE OF WA_SETLEAF INITIAL SIZE 0 WITH HEADER LINE.

  DATA: T_ROMANEIOS TYPE ZSDT0001_T,
        V_FATURAR	  TYPE CHAR01,
        V_MENSAGEM  TYPE CHAR255.

  DATA: C_R(2) TYPE C.


  CLEAR: T_ROMANEIOS[], V_FATURAR, V_MENSAGEM.

  CALL METHOD ZCL_ROMANEIO=>GET_CK_FATURAR
    EXPORTING
      I_CH_REFERENCIA_SAI = P_ZSDT0001-CH_REFERENCIA
    IMPORTING
      E_ROMANEIOS         = T_ROMANEIOS.

  "CS2017000598 22.05.2017
  IF S_VBAK-AUART = 'ZTER'.
    " Atualiza ZSDT0001
    UPDATE ZSDT0001
       SET STATUS  = 'X'
           DOC_REM = P_DELIVERY
     WHERE CH_REFERENCIA EQ P_ZSDT0001-CH_REFERENCIA.

    CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
      EXPORTING
        CD_REFERENCIA = P_ZSDT0001-CH_REFERENCIA
        TP_BLOQUEIO   = 'X'.

    EXIT.
  ENDIF.
  "Fim CS2017000598 22.05.2017

  IF ( P_ZSDT0001-MATNR IS INITIAL ). "CS2017000598 22.05.2017
    PERFORM Z_INS_LOTE_ITENS TABLES TL_RETURN
                              USING P_DELIVERY.

    IF TL_RETURN[] IS NOT INITIAL.
      PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                            USING P_ZSDT0001.
    ENDIF.
  ELSE.
    IF S_VBAP-CHARG IS INITIAL.
      PERFORM Z_INS_LOTE USING P_DELIVERY V_CHARG.
    ENDIF.
  ENDIF.

  "Determinação Peso Bruto/ Peso Liquido
  CLEAR: V_PESO_BRUTO, V_PESO_LIQ.

  REFRESH:  TL_RETURN.

  IF P_PESO GT 0.
    V_PESO_BRUTO  = P_ZSDT0001-PESO_LIQ.
    V_PESO_LIQ    = P_PESO.

    PERFORM Z_TROCA_PESO       TABLES TL_RETURN
                               USING P_DELIVERY
                                     P_ZSDT0001
                                     V_PESO_BRUTO
                                     V_PESO_LIQ.

    IF TL_RETURN[] IS NOT INITIAL.
      PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                            USING P_ZSDT0001.
    ENDIF.
  ELSEIF ( P_ZSDT0001-MATNR IS NOT INITIAL ).
    READ TABLE T_ROMANEIOS INTO DATA(_WL_ROM) WITH KEY CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
    IF ( SY-SUBRC EQ 0 ) AND ( _WL_ROM-PESO_SUBTOTAL IS NOT INITIAL ) AND ( _WL_ROM-PESO_LIQ IS NOT INITIAL ).
      V_PESO_BRUTO  = _WL_ROM-PESO_SUBTOTAL.
      V_PESO_LIQ    = _WL_ROM-PESO_LIQ.

      PERFORM Z_TROCA_PESO       TABLES TL_RETURN
                                 USING P_DELIVERY
                                       P_ZSDT0001
                                       V_PESO_BRUTO
                                       V_PESO_LIQ.

      IF TL_RETURN[] IS NOT INITIAL.
        PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                              USING P_ZSDT0001.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH: TL_VBPOK ,
           TL_RETURN.
  SL_VBKOK_WA-VBELN_VL  = P_DELIVERY.
  SL_VBKOK_WA-VBELN     = P_DELIVERY.
  SL_VBKOK_WA-WABUC     = 'X'.
  SL_VBKOK_WA-WADAT_IST = P_DATA_REM.

  IF ( P_ZSDT0001-MATNR IS INITIAL ). "CS2017000598 22.05.2017

    LOOP AT T_ZSDT0001_ITEM WHERE CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.

      READ TABLE TG_VBAP WITH KEY VBELN = T_ZSDT0001_ITEM-VBELN
                                  POSNR = T_ZSDT0001_ITEM-POSNR.

      SL_VBPOK-VBELN_VL       = P_DELIVERY.
      SL_VBPOK-POSNR_VL       = T_ZSDT0001_ITEM-ITM_LOTE.
      SL_VBPOK-VBELN          = P_DELIVERY.
      SL_VBPOK-POSNN          = T_ZSDT0001_ITEM-POSNR_REM.
      SL_VBPOK-MATNR          = T_ZSDT0001_ITEM-MATNR.
      SL_VBPOK-PIKMG          = T_ZSDT0001_ITEM-LFIMG.
      SL_VBPOK-CHARG          = T_ZSDT0001_ITEM-CHARG.
      SL_VBPOK-GEWEI          = 'KG'.

*    *************************************************
      SL_VBPOK-LGORT          = TG_VBAP-LGORT.
      SL_VBPOK-BRGEW          = T_ZSDT0001_ITEM-BRGEW.
      SL_VBPOK-NTGEW          = T_ZSDT0001_ITEM-NTGEW.
*    *************************************************
      APPEND SL_VBPOK TO TL_VBPOK.
    ENDLOOP.

  ELSE.

    SL_VBPOK-VBELN_VL       = P_DELIVERY.
    SL_VBPOK-POSNR_VL       = 10.
    SL_VBPOK-VBELN          = P_DELIVERY.
    SL_VBPOK-POSNN          = 10.
    SL_VBPOK-MATNR          = S_VBAP-MATNR.
    IF P_PESO  GT 0.
      SL_VBPOK-PIKMG          = P_PESO .
    ELSE.

      IF P_ZSDT0001-QTDE_REMESSA IS NOT INITIAL.
        SL_VBPOK-PIKMG          = P_ZSDT0001-QTDE_REMESSA.
      ELSE.
        SL_VBPOK-PIKMG          = P_ZSDT0001-PESO_LIQ.
      ENDIF.

    ENDIF.
    SL_VBPOK-CHARG          = V_CHARG .
    SL_VBPOK-GEWEI          = 'KG'.

*  *************************************************
    IF ( P_ZSDT0001-ID_INTERFACE EQ '48' OR
         P_ZSDT0001-ID_INTERFACE EQ '49' OR
         P_ZSDT0001-ID_INTERFACE EQ '51' OR
         P_ZSDT0001-ID_INTERFACE EQ '52' ). "CS2017000598 22.05.2017
      SL_VBPOK-LGORT        = S_VBAP-LGORT.
    ELSE.
      SL_VBPOK-LGORT        = V_LGORT.
    ENDIF.

    IF V_PESO_BRUTO GT 0.
      SL_VBPOK-BRGEW          = V_PESO_BRUTO.
      SL_VBPOK-NTGEW          = V_PESO_LIQ.
    ELSE.
      SL_VBPOK-BRGEW          = P_ZSDT0001-PESO_LIQ.
      SL_VBPOK-NTGEW          = P_ZSDT0001-PESO_LIQ.
    ENDIF.
*  *************************************************

    APPEND SL_VBPOK TO TL_VBPOK.

  ENDIF."CS2017000598 22.05.2017

  CLEAR : VL_MAT_DOC_S, VL_MATDOCUMENTYEAR_S, VL_MAT_DOC_E, VL_MATDOCUMENTYEAR_E.

  CLEAR: TL_PROT[], VG_ERRO_MAT_S, VG_ERRO_MAT_E, VG_ERRO_PIC, VG_SYSUBRC.
  CLEAR: WA_RETURN.

  " Ajustes ZOPUS - Remessa e Armazenagem - CH: 86446.
  REFRESH: IT_SETLEAF[].
  CLEAR: WA_SETLEAF.

  SELECT * INTO TABLE IT_SETLEAF
    FROM SETLEAF
    WHERE SETNAME EQ 'MAGGI_ARMAZENAGEM_VA01'
  AND VALFROM EQ S_VBAK-AUART.

  CLEAR: C_R.
  IF  ( SY-SUBRC EQ 0 ).
    C_R = 'X'.
    PERFORM Z_SAIDA_MERCADORIA USING P_ZSDT0001 SL_VBPOK VG_SYSUBRC VL_MAT_DOC_S VL_MATDOCUMENTYEAR_S C_R P_DATA_REM.

    IF NOT ( VL_MAT_DOC_S IS INITIAL ).

      CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
        EXPORTING
          VBKOK_WA                 = SL_VBKOK_WA
          SYNCHRON                 = 'X'
          IF_ERROR_MESSAGES_SEND_1 = 'X'
        TABLES
          VBPOK_TAB                = TL_VBPOK
          PROT                     = TL_PROT.

      IF ( TL_PROT[] IS INITIAL AND WA_RETURN IS INITIAL ). "AND ( sy-subrc IS INITIAL ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
*        CLEAR: ZSDT0023.
*        ZSDT0023-VBELN         = SL_VBKOK_WA-VBELN.
*        ZSDT0023-VBELV         = S_VBAP-VBELN.
*        ZSDT0023-CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
*        ZSDT0023-MBLNR_S       = VL_MAT_DOC_S.
*        ZSDT0023-MJAHR_S       = VL_MATDOCUMENTYEAR_S.
*        ZSDT0023-MBLNR_E       = VL_MAT_DOC_E.
*        ZSDT0023-MJAHR_E       = VL_MATDOCUMENTYEAR_E.
*        ZSDT0023-WERKS_V       = WA_DEPARA-WERKS_V.
*        IF S_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL. "ALRS 04.05.2015
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_T.
*        ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_F.
*        ELSE.
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT.
*        ENDIF.
*        ZSDT0023-DT_SAIDA      = SY-DATUM.
*        ZSDT0023-HS_SAIDA      = SY-UZEIT.
*        INSERT ZSDT0023.
*        COMMIT WORK.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        VG_ERRO_PIC = 'X'.
      ENDIF.


    ENDIF.

  ELSE.


    IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
      PERFORM Z_SAIDA_MERCADORIA USING P_ZSDT0001 SL_VBPOK VG_SYSUBRC VL_MAT_DOC_S VL_MATDOCUMENTYEAR_S C_R P_DATA_REM .
    ENDIF.

    IF VG_SYSUBRC IS INITIAL.

      IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
        PERFORM GERA_MATERIAL_ENTRADA USING P_DELIVERY VL_MAT_DOC_S VL_MATDOCUMENTYEAR_S VL_MAT_DOC_E VL_MATDOCUMENTYEAR_E VG_ERRO_MAT_E P_DATA_REM P_ZSDT0001 SL_VBPOK..
      ENDIF.

      IF VG_ERRO_MAT_E IS INITIAL.

        CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
          EXPORTING
            VBKOK_WA                 = SL_VBKOK_WA
            SYNCHRON                 = 'X'
            IF_ERROR_MESSAGES_SEND_1 = 'X'
          TABLES
            VBPOK_TAB                = TL_VBPOK
            PROT                     = TL_PROT.

        IF ( TL_PROT[] IS INITIAL AND WA_RETURN IS INITIAL ). "AND ( sy-subrc IS INITIAL ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          VG_ERRO_PIC = 'X'.
        ENDIF.

      ENDIF.

    ELSE.
      VG_ERRO_MAT_S = 'X'.
    ENDIF.

*    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 15.08.2016
*    IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
*      CLEAR: ZSDT0023.
*      ZSDT0023-VBELN         = SL_VBKOK_WA-VBELN.
*      ZSDT0023-VBELV         = S_VBAP-VBELN.
*      ZSDT0023-CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
*      ZSDT0023-MBLNR_S       = VL_MAT_DOC_S.
*      ZSDT0023-MJAHR_S       = VL_MATDOCUMENTYEAR_S.
*      ZSDT0023-MBLNR_E       = VL_MAT_DOC_E.
*      ZSDT0023-MJAHR_E       = VL_MATDOCUMENTYEAR_E.
*      ZSDT0023-WERKS_V       = WA_DEPARA-WERKS_V.
*      IF S_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL. "ALRS 04.05.2015
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_T.
*      ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_F.
*      ELSE.
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT.
*      ENDIF.
*      ZSDT0023-DT_SAIDA      = SY-DATUM.
*      ZSDT0023-HS_SAIDA      = SY-UZEIT.
*      INSERT ZSDT0023.
*      COMMIT WORK.
*    ENDIF.

    IF ( VG_ERRO_PIC IS INITIAL ) AND ( VG_ERRO_MAT_S IS INITIAL ) AND ( VG_ERRO_MAT_E IS INITIAL ).
*   Atualiza ZSDT0001
      UPDATE ZSDT0001
         SET STATUS  = 'X'
             DOC_REM = P_DELIVERY
             DOC_MATERIAL = VL_MAT_DOC_R
             ANO_MATERIAL = VL_MATDOCUMENTYEAR_R
       WHERE CH_REFERENCIA EQ P_ZSDT0001-CH_REFERENCIA.

      IF SY-TCODE NE 'ZLES0106' AND SY-TCODE NE 'ZLES0115' AND SY-TCODE NE 'ZLES0136' AND  SY-TCODE NE 'ZMM0127' AND SY-BATCH NE ABAP_TRUE.
        UPDATE ZSDT0001
                 SET ST_PROC = '99'
               WHERE CH_REFERENCIA EQ P_ZSDT0001-CH_REFERENCIA.
      ENDIF.

      CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
        EXPORTING
          CD_REFERENCIA = P_ZSDT0001-CH_REFERENCIA
          TP_BLOQUEIO   = 'X'.

    ENDIF.

    IF ( NOT VG_ERRO_PIC IS INITIAL ) OR ( NOT VG_ERRO_MAT_S IS INITIAL ) OR ( NOT VG_ERRO_MAT_E IS INITIAL ).
      IF VL_MAT_DOC_E IS INITIAL.
        SELECT SINGLE MBLNR,MJAHR
          FROM MKPF
          INTO ( @VL_MAT_DOC_E, @VL_MATDOCUMENTYEAR_E )
           WHERE BKTXT  = @P_DELIVERY
           AND   TCODE2 = 'MBSU'.
        VG_ERRO_PIC = 'X'.
        REFRESH TL_RETURN.
        WA_RETURN-TYPE    = 'E'.
        WA_RETURN-MESSAGE = |Doc.entrada Recuperado { P_DELIVERY }, { VL_MAT_DOC_E }  |.
        APPEND WA_RETURN TO TL_RETURN.
        PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                           USING P_ZSDT0001.
        REFRESH TL_RETURN.
      ENDIF.
      IF VL_MAT_DOC_S IS INITIAL.
        SELECT SINGLE MBLNR,MJAHR
          FROM MKPF
          INTO ( @VL_MAT_DOC_S, @VL_MATDOCUMENTYEAR_S )
           WHERE BKTXT  = @P_DELIVERY
           AND   TCODE2 = 'MB1B'.
        REFRESH TL_RETURN.
        WA_RETURN-TYPE    = 'E'.
        WA_RETURN-MESSAGE = |Doc.saida Recuperado { P_DELIVERY }, { VL_MAT_DOC_S } |.
        APPEND WA_RETURN TO TL_RETURN.
        PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                           USING P_ZSDT0001.
        REFRESH TL_RETURN.
        VG_ERRO_PIC = 'X'.
      ENDIF.
      IF ( VG_ERRO_PIC IS NOT INITIAL ) OR ( NOT VG_ERRO_MAT_E IS INITIAL ).

        IF VG_ERRO_PIC IS NOT INITIAL AND VL_MAT_DOC_E IS NOT INITIAL.
          "Estorno de material Entrada.
          PERFORM CANCELA_DOC_MATERIAL USING P_DELIVERY  VL_MAT_DOC_E VL_MATDOCUMENTYEAR_E  'E'.
        ENDIF.

        "Estorno de material Saída.
        PERFORM CANCELA_DOC_MATERIAL USING P_DELIVERY VL_MAT_DOC_S VL_MATDOCUMENTYEAR_S 'S'.

        PERFORM F_ESTORNO_RES  CHANGING P_ZSDT0001.
      ENDIF.

* BBKO/Vagner Santos - Início da alteração - 02.10.2010
* Salvar a mensagem de erro
      LOOP AT TL_PROT INTO SL_PROT.
        MESSAGE ID SL_PROT-MSGID
          TYPE SL_PROT-MSGTY
        NUMBER SL_PROT-MSGNO
          WITH SL_PROT-MSGV1
               SL_PROT-MSGV2
               SL_PROT-MSGV3
               SL_PROT-MSGV4
          INTO SL_RETURN-MESSAGE.
        SL_RETURN-TYPE = SL_PROT-MSGTY.
        APPEND SL_RETURN TO TL_RETURN.
        CLEAR: SL_PROT  ,
               SL_RETURN.
      ENDLOOP.
*   Retorna Erro
      PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                            USING P_ZSDT0001.

*   Deleta Delivery Criado
      DATA: SL_HDATA    TYPE BAPIOBDLVHDRCHG,
            SL_HCONT    TYPE BAPIOBDLVHDRCTRLCHG,
            VL_DELIVERY TYPE BAPIOBDLVHDRCHG-DELIV_NUMB,
            TL_BAPIRET2 TYPE BAPIRET2_T.

      SL_HDATA-DELIV_NUMB = P_DELIVERY.
      SL_HCONT-DELIV_NUMB = P_DELIVERY.
      SL_HCONT-DLV_DEL    = 'X'.
      VL_DELIVERY         = P_DELIVERY.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          HEADER_DATA    = SL_HDATA
          HEADER_CONTROL = SL_HCONT
          DELIVERY       = VL_DELIVERY
        TABLES
          RETURN         = TL_BAPIRET2.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      REFRESH TL_RETURN.
      APPEND LINES OF TL_BAPIRET2 TO TL_RETURN.
*   Retorna Erro
      PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                            USING P_ZSDT0001.

    ENDIF.

  ENDIF.

  IF ( C_R EQ 'X' ).
    " Atualiza ZSDT0001
    UPDATE ZSDT0001
       SET STATUS  = 'X'
           DOC_REM = P_DELIVERY
           DOC_MATERIAL = VL_MAT_DOC_R
           ANO_MATERIAL = VL_MATDOCUMENTYEAR_R
     WHERE CH_REFERENCIA EQ P_ZSDT0001-CH_REFERENCIA.

    IF SY-TCODE NE 'ZLES0106' AND SY-TCODE NE 'ZLES0115' AND SY-TCODE NE 'ZMM0127' AND SY-BATCH NE ABAP_TRUE.
      UPDATE ZSDT0001
               SET ST_PROC = '99'
             WHERE CH_REFERENCIA EQ P_ZSDT0001-CH_REFERENCIA.
    ENDIF.

    CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
      EXPORTING
        CD_REFERENCIA = P_ZSDT0001-CH_REFERENCIA
        TP_BLOQUEIO   = 'X'.
  ENDIF.



  CLEAR: C_R.
ENDFORM.                    " Z_PICKING
*&---------------------------------------------------------------------*
*&      Form  Z_SAIDA_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_DELIVERY  text
*      -->P_SL_ZSDT0001  text
*----------------------------------------------------------------------*
FORM Z_SAIDA_MERCADORIA  USING    P_ZSDT0001  TYPE ZSDT0001
                                  SL_VBKOK_WA TYPE VBPOK
                                  LC_SUBRC    TYPE SY-SUBRC
                                  VL_MAT_DOC  TYPE BAPI2017_GM_HEAD_RET-MAT_DOC
                                  VL_MATDOCUMENTYEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR
                                  C_R                TYPE C
                                  P_DATA_REM         TYPE LEDAT.

  DATA: MSG_TEXT TYPE STRING,
        W_MSN    TYPE TYPE_MSN,
        MSG_ID   TYPE T100-ARBGB,
        MSG_NO   TYPE T100-MSGNR,
        MSG_VAR1 TYPE BALM-MSGV1,
        W_RETURN TYPE BAPIRET2.


  DATA: WA_KNA1 TYPE KNA1,
        WA_LFA1 TYPE LFA1.



  CLEAR: WA_GOODSMVT_HEADER, T_GOODSMVT_ITEM.

  LC_SUBRC = 0.
  WA_GOODSMVT_HEADER-PSTNG_DATE = P_DATA_REM.
  WA_GOODSMVT_HEADER-DOC_DATE   = P_DATA_REM.
  WA_GOODSMVT_HEADER-HEADER_TXT = SL_VBKOK_WA-VBELN.
  WA_CODE-GM_CODE               = C_04.
*--> 19.06.2023 - Migration S4 – MIGNOW - Start
  "  wa_goodsmvt_item-material     = s_vbap-matnr.
  DATA(V_LEN2) = STRLEN( S_VBAP-MATNR ).
  IF V_LEN2 > 18.
    WA_GOODSMVT_ITEM-MATERIAL_LONG = S_VBAP-MATNR .
  ELSE.
    WA_GOODSMVT_ITEM-MATERIAL = S_VBAP-MATNR .
  ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
  WA_GOODSMVT_ITEM-PLANT        = S_VBAP-VSTEL.
  WA_GOODSMVT_ITEM-STGE_LOC	    = S_VBAP-LGORT.
  WA_GOODSMVT_ITEM-BATCH        = S_VBAP-CHARG.
  WA_GOODSMVT_ITEM-GR_RCPT      = S_VBAP-VBELN.
  WA_GOODSMVT_ITEM-ITEM_TEXT    = S_VBAP-VBELN .

  CASE S_VBAK-AUART.
    WHEN 'ZIND'.
      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'I50'.
    WHEN OTHERS.
      WA_GOODSMVT_ITEM-MOVE_TYPE    = C_F50.
  ENDCASE.

  WA_GOODSMVT_ITEM-ENTRY_QNT    = SL_VBKOK_WA-PIKMG. "SL_VBKOK_WA-BRGEW.


  IF ( C_R EQ 'X' ).

    SELECT SINGLE * FROM KNA1 INTO WA_KNA1 WHERE KUNNR EQ S_VBAK-KUNNR.

    IF ( SY-SUBRC EQ 0 ).

      SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE STCD1 EQ WA_KNA1-STCD1.

      IF ( SY-SUBRC EQ 0 ).
        WA_GOODSMVT_ITEM-VENDOR    = WA_LFA1-LIFNR.

        CLEAR: WA_GOODSMVT_ITEM-MOVE_TYPE.
        CASE P_ZSDT0001-TP_MOVIMENTO.
          WHEN: 'E'.
            WA_GOODSMVT_ITEM-MOVE_TYPE = '542'.
          WHEN: 'S'.
            WA_GOODSMVT_ITEM-MOVE_TYPE = '541'.
        ENDCASE.

      ELSE.
        MESSAGE I836 WITH 'Fornecedor não cadastrado'.
      ENDIF.

    ENDIF.

  ENDIF.

  APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.

*Exeuta a BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
      GOODSMVT_CODE    = WA_CODE
    IMPORTING
      MATERIALDOCUMENT = VL_MAT_DOC
      MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
    TABLES
      GOODSMVT_ITEM    = T_GOODSMVT_ITEM
      RETURN           = T_RETURN.

  READ TABLE T_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
  IF SY-SUBRC IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LC_SUBRC = 4.

    LOOP AT T_RETURN INTO W_RETURN.
      WRITE W_RETURN-NUMBER TO MSG_NO.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = W_RETURN-ID
          MSG_NO                 = MSG_NO
          MSG_VAR1               = W_RETURN-MESSAGE_V1
          MSG_VAR2               = W_RETURN-MESSAGE_V2
          MSG_VAR3               = W_RETURN-MESSAGE_V3
          MSG_VAR4               = W_RETURN-MESSAGE_V4
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = W_RETURN-TYPE.
      APPEND W_MSN TO T_MSN.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    MSG_ID   = 'M7'.
    MSG_NO   = '060'.
    MSG_VAR1 = VL_MAT_DOC.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        MSG_ID                 = MSG_ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = MSG_VAR1
      IMPORTING
        MSG_TEXT               = MSG_TEXT
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.

    W_MSN-MESSAGEM = MSG_TEXT.
    W_MSN-TP_MSN   = 'S'.
    APPEND W_MSN TO T_MSN.
    "
    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 2 (saida )
    UPDATE ZSDT0023 SET MBLNR_S  = @VL_MAT_DOC,
                        MJAHR_S  = @VL_MATDOCUMENTYEAR
    WHERE VBELN = @SL_VBKOK_WA-VBELN.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " Z_SAIDA_MERCADORIA



*&---------------------------------------------------------------------*
*&      Form  Z_CAP_VEICULO                                            *
*&---------------------------------------------------------------------*
*                          Capacidade Veículo                          *
*----------------------------------------------------------------------*
FORM Z_CAP_VEICULO USING P_ZSDT0001 TYPE ZSDT0001.

  DATA SL_MSN TYPE TYPE_MSN.

  SL_MSN-NR_ROMANEIO = P_ZSDT0001-NR_ROMANEIO.
  SL_MSN-TP_MSN      = 'E'.
  SL_MSN-MESSAGEM    = TEXT-018.
  APPEND SL_MSN TO T_MSN.

ENDFORM.                    " Z_CAP_VEICULO

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_TEXTO                                            *
*&---------------------------------------------------------------------*
*                               Salva Textos                           *
*----------------------------------------------------------------------*
FORM Z_SALVA_TEXTO USING P_DELIVERY TYPE VBELN_VL
                         P_ZSDT0001 TYPE ZSDT0001.

  DATA: SL_HEADER TYPE THEAD,
        SL_LINES  TYPE TLINE,
        VL_TEXT   TYPE CHAR256,
        TL_LINES  TYPE TABLE OF TLINE.

  REFRESH TL_LINES.

  SL_HEADER-TDOBJECT = 'VBBK'.
  SL_HEADER-TDNAME   =  P_DELIVERY.
  SL_HEADER-TDID     = '0001'.
  SL_HEADER-TDSPRAS  = 'P'.

  CONCATENATE 'Pesagem OPUS ChRef:'
              P_ZSDT0001-CH_REFERENCIA
         INTO VL_TEXT SEPARATED BY SPACE.
  SL_LINES-TDFORMAT  = '*'.
  SL_LINES-TDLINE    = VL_TEXT.
  APPEND SL_LINES TO TL_LINES.
  CLEAR: SL_LINES,
         VL_TEXT.
  CONCATENATE 'Número do Romaneio:'
              P_ZSDT0001-NR_ROMANEIO
         INTO VL_TEXT SEPARATED BY SPACE.
  SL_LINES-TDFORMAT  = '*'.
  SL_LINES-TDLINE    = VL_TEXT.
  APPEND SL_LINES TO TL_LINES.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      HEADER          = SL_HEADER
      SAVEMODE_DIRECT = 'X'
    TABLES
      LINES           = TL_LINES
    EXCEPTIONS
      ID              = 1
      LANGUAGE        = 2
      NAME            = 3
      OBJECT          = 4
      OTHERS          = 5.

ENDFORM.                    " Z_SALVA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  Z_REG_SM                                                 *
*&---------------------------------------------------------------------*
*                            Registrar SM                              *
*----------------------------------------------------------------------*
FORM Z_REG_SM USING P_VDELIVERY TYPE VBELN_VL.

  DATA: VL_MODE TYPE C VALUE 'N',
        TL_ITAB TYPE TABLE OF BDCMSGCOLL.

  REFRESH T_BDC.

* Insere BDC
  PERFORM Z_INSERE_BDC USING: 'X' 'SAPMV50A'   '4004'     ,
                              ' ' 'BDC_OKCODE' '=WABU_T'  ,
                              ' ' 'LIKP-VBELN' P_VDELIVERY.

  CALL FUNCTION 'DEQUEUE_ALL'
    EXPORTING
      _SYNCHRON = 'X'.

  WAIT UP TO 2 SECONDS.

  CALL TRANSACTION 'VL02N'
     USING T_BDC
     MODE VL_MODE
     MESSAGES INTO TL_ITAB.

ENDFORM.                    " Z_REG_SM

FORM Z_INS_LOTE USING P_VDELIVERY TYPE VBELN_VL
                      P_CHARG     TYPE VBPOK-CHARG.

  DATA: VL_MODE TYPE C VALUE 'N',
        TL_ITAB TYPE TABLE OF BDCMSGCOLL.

  REFRESH T_BDC.

* Insere BDC
  PERFORM Z_INSERE_BDC USING: 'X' 'SAPMV50A'   '4004'     ,
                              ' ' 'BDC_OKCODE' '/00'  ,
                              ' ' 'LIKP-VBELN' P_VDELIVERY.

  PERFORM Z_INSERE_BDC USING: 'X' 'SAPMV50A'   '1000'     ,
                              ' ' 'BDC_OKCODE' '=SICH_T'  ,
                              ' ' 'LIPS-CHARG(01)' P_CHARG.
  CALL FUNCTION 'DEQUEUE_ALL'
    EXPORTING
      _SYNCHRON = 'X'.

  WAIT UP TO 1 SECONDS.

  CALL TRANSACTION 'VL02N'
     USING T_BDC
     MODE VL_MODE
     MESSAGES INTO TL_ITAB.
  COMMIT WORK.
  WAIT UP TO 2 SECONDS.

ENDFORM.                    " Z_REG_SM

*&---------------------------------------------------------------------*
*&      Form  Z_INSERE_BDC                                             *
*&---------------------------------------------------------------------*
*                             Insere BDC                               *
*----------------------------------------------------------------------*
FORM Z_INSERE_BDC USING P_DYNBEGIN TYPE ANY
                        P_FIELD    TYPE ANY
                        P_VALUE    TYPE ANY.

  DATA SL_BDC TYPE BDCDATA.

  CLEAR SL_BDC.

  IF P_DYNBEGIN EQ 'X'.
    SL_BDC-DYNBEGIN = 'X'.
    SL_BDC-PROGRAM  = P_FIELD.
    SL_BDC-DYNPRO   = P_VALUE.
  ELSE.
    SL_BDC-FNAM = P_FIELD.
    SL_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND SL_BDC TO T_BDC.

ENDFORM.                    " Z_INSERE_BDC
* BBKO/Vagner Santos - Início da alteração - 02.10.2010
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_AGENTE_FRETE
*&---------------------------------------------------------------------*
* Consistir o código do agente de frete informado.
*----------------------------------------------------------------------*
FORM Z_VERIFICA_AGENTE_FRETE RAISING ZCX_ERROR. "*-#133089-21.02.2024-JT-inicio
  .

  IF NOT P_LIFNR IS INITIAL.
    CHECK P_LIFNR-LOW IS NOT INITIAL. "//Quando vazio, é uma fatura agrupada;
    SELECT SINGLE LIFNR INTO LFA1-LIFNR
                        FROM LFA1
    WHERE LIFNR IN P_LIFNR.

    IF NOT SY-SUBRC IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
          MESSAGE I836 WITH TEXT-022.
          LEAVE LIST-PROCESSING.
        WHEN ABAP_TRUE.
          MESSAGE I836 WITH TEXT-022 INTO DATA(L_MESG).
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_MSG = CONV #( L_MESG ) I_STATUS = 'REME' ).
          LC_FATURAMENTO_AUTOMATICO->SET_MENSAGEM( I_COD = '999' I_MESG = CONV #( L_MESG ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_AGENTE_FRETE
*&---------------------------------------------------------------------*
*&      Form  Z_INCLUIR_AGENTE_FRETE
*&---------------------------------------------------------------------*
* Executar a bapi de modificação da remessa
*----------------------------------------------------------------------*
FORM Z_INCLUIR_AGENTE_FRETE  USING    P_DELIVERY TYPE VBELN_VL
                                      P_ZSDT0001 TYPE ZSDT0001.

  DATA: TL_BAPIRET2 TYPE BAPIRET2_T,
        TL_RETURN   TYPE BAPIRET2_T,
        SL_BAPIRET2 TYPE BAPIRET2,
        SL_RETURN   TYPE BAPIRET2.

  DATA: WA_HEADER_DATA    TYPE BAPIOBDLVHDRCHG,
        WA_HEADER_CONTROL TYPE BAPIOBDLVHDRCTRLCHG,
        HEADER_PARTNER    TYPE TABLE OF BAPIDLVPARTNERCHG INITIAL SIZE 0 WITH HEADER LINE.

  READ TABLE P_LIFNR.
  IF NOT P_LIFNR-LOW IS INITIAL.

    WA_HEADER_DATA-DELIV_NUMB    = P_DELIVERY.
    WA_HEADER_CONTROL-DELIV_NUMB = 'X'.

* Incluir o parceiro SP (Agente de frete) na remessa
    IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
      HEADER_PARTNER-UPD_MODE_PARTN = 'U'.
    ELSE.
      HEADER_PARTNER-UPD_MODE_PARTN = 'I'.
    ENDIF.

    HEADER_PARTNER-DELIV_NUMB     = P_DELIVERY.
    HEADER_PARTNER-ITM_NUMBER     = '000010'.
    HEADER_PARTNER-PARTN_ROLE     = 'SP'.
    HEADER_PARTNER-PARTNER_NO     = P_LIFNR-LOW.
    APPEND HEADER_PARTNER.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        HEADER_DATA    = WA_HEADER_DATA
        HEADER_CONTROL = WA_HEADER_CONTROL
        DELIVERY       = P_DELIVERY
      TABLES
        HEADER_PARTNER = HEADER_PARTNER
        RETURN         = TL_BAPIRET2.

    IF TL_BAPIRET2[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ELSE.
      LOOP AT TL_BAPIRET2 INTO SL_BAPIRET2.
        MESSAGE ID SL_BAPIRET2-ID
          TYPE SL_BAPIRET2-TYPE
        NUMBER SL_BAPIRET2-NUMBER
          WITH SL_BAPIRET2-MESSAGE_V1
               SL_BAPIRET2-MESSAGE_V2
               SL_BAPIRET2-MESSAGE_V3
               SL_BAPIRET2-MESSAGE_V4
          INTO SL_RETURN-MESSAGE.
        SL_RETURN-TYPE = SL_BAPIRET2-TYPE.
        APPEND SL_RETURN TO TL_RETURN.
        CLEAR: SL_BAPIRET2,
               SL_RETURN.
      ENDLOOP.

*     Retorna Erro
      PERFORM Z_MONTA_ERRO TABLES TL_RETURN
                            USING P_ZSDT0001.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_INCLUIR_AGENTE_FRETE
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_PARC                                          *
*&---------------------------------------------------------------------*
*                            Verifica Parceiros                        *
*----------------------------------------------------------------------*
FORM Z_VERIFICA_PARC USING P_IN    TYPE VBAK-AUART
                  CHANGING P_AUART TYPE ZZAUART.

  DATA: TL_VBPA    TYPE TABLE OF VBPA,
        SL_VBPA_LR TYPE VBPA,
        SL_VBPA_Z1 TYPE VBPA,
        VL_CNPJ_LR TYPE KNA1-STCD1,
        VL_CNPJ_Z1 TYPE LFA1-STCD1.

  CLEAR P_AUART.

  SELECT *
    FROM VBPA
    INTO TABLE TL_VBPA
  WHERE  VBELN EQ S_VBAK-VBELN.

  DELETE TL_VBPA WHERE PARVW NE 'LR'
                   AND PARVW NE 'Z1'.
  SORT TL_VBPA BY PARVW ASCENDING.

  READ TABLE TL_VBPA: INTO SL_VBPA_LR WITH KEY PARVW = 'LR',
                      INTO SL_VBPA_Z1 WITH KEY PARVW = 'Z1'.

  SELECT SINGLE STCD1
    FROM KNA1
    INTO VL_CNPJ_LR
  WHERE  KUNNR EQ SL_VBPA_LR-KUNNR.

  SELECT SINGLE STCD1
    FROM LFA1
    INTO VL_CNPJ_Z1
  WHERE  LIFNR EQ SL_VBPA_Z1-LIFNR.

  IF VL_CNPJ_LR EQ VL_CNPJ_Z1.
    CASE P_IN.
      WHEN 'ZRFL'.
        P_AUART = 'ZRFL1'.
      WHEN 'ZRDC'.
        P_AUART = 'ZRDC1'.
    ENDCASE.
  ELSE.
    CASE P_IN.
      WHEN 'ZRFL'.
        P_AUART = 'ZRFL2'.
      WHEN 'ZRDC'.
        P_AUART = 'ZRDC2'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_PARC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVARVC                                       *
*&---------------------------------------------------------------------*
*                             Verifica Usuário                         *
*----------------------------------------------------------------------*
FORM Z_SELECIONA_TVARVC.

  DATA: WA_SETLEAF LIKE SETLEAF.

  CLEAR V_USER.

*  SELECT SINGLE LOW
*    FROM TVARVC
*    INTO V_USER
*  WHERE  NAME EQ 'REM_DATA_RETROATIVA'
*    AND  LOW  EQ SY-UNAME.

  SELECT SINGLE *
    FROM SETLEAF
    INTO WA_SETLEAF
   WHERE SETNAME = 'VF01_USUARIO'
  AND VALFROM = SY-UNAME.

  IF SY-SUBRC  IS INITIAL.
    V_USER = SY-UNAME.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_TVARVC
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_MAT_DOC  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM Z_MONTA_SUCESSO_MATERIAL  USING    P_VL_MAT_DOC
                                        P_SPACE.

  DATA: SL_MSN_MAT  TYPE TYPE_MSN,
        SL_TEXT_MAT TYPE CHAR50.
  SL_MSN_MAT-TP_MSN      = 'S'.

  IF NOT P_VL_MAT_DOC IS INITIAL.
    SL_TEXT_MAT = 'Documento de Material Criado: '.
  ENDIF.

  CONCATENATE SL_TEXT_MAT
              P_VL_MAT_DOC
         INTO SL_MSN_MAT-MESSAGEM SEPARATED BY SPACE.
  APPEND SL_MSN_MAT TO T_MSN.


ENDFORM.                    " Z_MONTA_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO_DEPARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LIFNR  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM Z_MONTA_ERRO_DEPARA  USING    P_P_LIFNR
                                   P_SPACE.

  DATA: SL_MSN_DEPARA  TYPE TYPE_MSN,
        SL_TEXT_DEPARA TYPE CHAR100,
        SL_TEXT_FINAL  TYPE CHAR100.

  SL_MSN_DEPARA-TP_MSN      = 'S'.

  IF NOT P_P_LIFNR IS INITIAL.
    SL_TEXT_DEPARA = 'Centro Virtual não encontrado para o centro: '.
    SL_TEXT_FINAL = ' por favor ligar para a Área de Execução.'.
  ENDIF.

  CONCATENATE SL_TEXT_DEPARA
              P_P_LIFNR
              SL_TEXT_FINAL
         INTO SL_MSN_DEPARA-MESSAGEM SEPARATED BY SPACE.
  APPEND SL_MSN_DEPARA TO T_MSN.

ENDFORM.                    " Z_MONTA_ERRO_DEPARA
*&---------------------------------------------------------------------*
*&      Form  Z_ERRO_ESTORNO_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_MAT_DOC  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM Z_ERRO_ESTORNO_MATERIAL  USING    P_VL_MAT_DOC
                                       P_SPACE.

  DATA: SL_MSN_ESTORNO  TYPE TYPE_MSN,
        SL_TEXT_ESTORNO TYPE CHAR100.

  SL_MSN_ESTORNO-TP_MSN      = 'E'.

  IF NOT P_VL_MAT_DOC IS INITIAL.
    SL_TEXT_ESTORNO = 'Ocorreu um erro no estorno no documento de material. '.
  ENDIF.

  CONCATENATE SL_TEXT_ESTORNO
               P_VL_MAT_DOC
         INTO SL_MSN_ESTORNO-MESSAGEM SEPARATED BY SPACE.
  APPEND SL_MSN_ESTORNO TO T_MSN.


ENDFORM.                    " Z_ERRO_ESTORNO_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  GERA_MATERIAL_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERA_MATERIAL_ENTRADA USING VG_REMESSA           TYPE VBELN_VL
                                 VL_MAT_DOC           TYPE BAPI2017_GM_HEAD_RET-MAT_DOC
                                 VL_MATDOCUMENTYEAR   TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR
                                 VL_MAT_DOC_E         TYPE BAPI2017_GM_HEAD_RET-MAT_DOC
                                 VL_MATDOCUMENTYEAR_E TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR
                                 VG_ERRO_MAT_E        TYPE C
                                 P_DATA_REM           TYPE LEDAT
                                 P_ZSDT0001           TYPE ZSDT0001
                                 SL_VBKOK_WA          TYPE VBPOK.


  "-- Projeto S4 Hana - Conversao para bapi - Fim
  DATA: MSG_TEXT TYPE STRING,
        W_MSN    TYPE TYPE_MSN,
        MSG_ID   TYPE T100-ARBGB,
        MSG_NO   TYPE T100-MSGNR,
        MSG_VAR1 TYPE BALM-MSGV1,
        W_RETURN TYPE BAPIRET2.


  DATA: WA_KNA1 TYPE KNA1,
        WA_LFA1 TYPE LFA1.

  CLEAR: WA_GOODSMVT_HEADER, T_GOODSMVT_ITEM, WA_GOODSMVT_ITEM, VG_ERRO_MAT_E.

  WA_GOODSMVT_HEADER-PSTNG_DATE = P_DATA_REM.
  WA_GOODSMVT_HEADER-DOC_DATE   = P_DATA_REM.
  WA_GOODSMVT_HEADER-HEADER_TXT = VG_REMESSA.
  WA_CODE-GM_CODE               = C_05.
  DATA(V_LEN2) = STRLEN( S_VBAP-MATNR ).
  IF V_LEN2 > 18.
    WA_GOODSMVT_ITEM-MATERIAL_LONG = S_VBAP-MATNR .
  ELSE.
    WA_GOODSMVT_ITEM-MATERIAL = S_VBAP-MATNR .
  ENDIF.

  WA_GOODSMVT_ITEM-PLANT        = WA_DEPARA-WERKS_V.
  "wa_goodsmvt_item-move_plant   = s_vbap-vstel.

  IF WA_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL.
    WA_GOODSMVT_ITEM-STGE_LOC	    =  WA_DEPARA-LGORT_T.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort_t.
  ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL.
    WA_GOODSMVT_ITEM-STGE_LOC	    =  WA_DEPARA-LGORT_F.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort_f.
  ELSE.
    WA_GOODSMVT_ITEM-STGE_LOC	    =  WA_DEPARA-LGORT.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort.
  ENDIF.

  WA_GOODSMVT_ITEM-BATCH        = S_VBAP-CHARG.

*  wa_goodsmvt_item-move_type    = 'F52'.

  CASE S_VBAK-AUART.
    WHEN 'ZIND'.
      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'I52'.
    WHEN OTHERS.
      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'F52'.
  ENDCASE.
  WA_GOODSMVT_ITEM-ENTRY_QNT    = SL_VBKOK_WA-PIKMG. "SL_VBKOK_WA-BRGEW.

  APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.

*Exeuta a BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
      GOODSMVT_CODE    = WA_CODE
    IMPORTING
      MATERIALDOCUMENT = VL_MAT_DOC
      MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
    TABLES
      GOODSMVT_ITEM    = T_GOODSMVT_ITEM
      RETURN           = T_RETURN.

  READ TABLE T_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
  IF SY-SUBRC IS INITIAL.
    VG_ERRO_MAT_E = 'X'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    LOOP AT T_RETURN INTO W_RETURN.
      WRITE W_RETURN-NUMBER TO MSG_NO.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = W_RETURN-ID
          MSG_NO                 = MSG_NO
          MSG_VAR1               = W_RETURN-MESSAGE_V1
          MSG_VAR2               = W_RETURN-MESSAGE_V2
          MSG_VAR3               = W_RETURN-MESSAGE_V3
          MSG_VAR4               = W_RETURN-MESSAGE_V4
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = W_RETURN-TYPE.
      APPEND W_MSN TO T_MSN.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    MSG_ID   = 'M7'.
    MSG_NO   = '060'.
    MSG_VAR1 = VL_MAT_DOC.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        MSG_ID                 = MSG_ID
        MSG_NO                 = MSG_NO
        MSG_VAR1               = MSG_VAR1
      IMPORTING
        MSG_TEXT               = MSG_TEXT
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.

    W_MSN-MESSAGEM = MSG_TEXT.
    W_MSN-TP_MSN   = 'S'.
    APPEND W_MSN TO T_MSN.

    UPDATE ZSDT0023 SET MBLNR_E  = @VL_MAT_DOC,
                        MJAHR_E  = @VL_MATDOCUMENTYEAR
    WHERE VBELN = @VG_REMESSA.
    COMMIT WORK.
  ENDIF.

  EXIT.

  "-- Projeto S4 Hana - Conversao para bapi - Fim

  DATA: WA_DATA   TYPE C LENGTH 10,
        VG_TEXTO  TYPE STRING,
*        wa_likp   TYPE likp,
        W_MESSTAB TYPE BDCMSGCOLL.
  "w_msn     TYPE type_msn, "-- Projeto S4 Hana - Conversao para bapi - Fim
  "msg_text  TYPE string.   "-- Projeto S4 Hana - Conversao para bapi - Fim

  CLEAR: VG_ERRO_MAT_E.

  WRITE P_DATA_REM TO WA_DATA.

  CONCATENATE 'Entr Form Lote:' VG_REMESSA INTO VG_TEXTO SEPARATED BY SPACE.

  CLEAR: T_BDC[], T_BDC, T_MESSTAB[], T_MESSTAB.
  "ALRS
  SELECT SINGLE *
         FROM VBAK
         INTO WA_VBAK
  WHERE VBELN = P_ZSDT0001-VBELN.

  IF WA_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL.
    PERFORM Z_INSERE_BDC USING: 'X' 'SAPMM07M'      '0460',
                                ' ' 'BDC_OKCODE'    '/00',
                                ' ' 'MKPF-BUDAT'   WA_DATA,
                                ' ' 'RM07M-MBLNR'  VL_MAT_DOC,
                                ' ' 'RM07M-MJAHR'  VL_MATDOCUMENTYEAR,
                                ' ' 'RM07M-LGORT'  WA_DEPARA-LGORT_T,
                                ' ' 'RM07M-WVERS2'  'X'.
  ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
    PERFORM Z_INSERE_BDC USING: 'X' 'SAPMM07M'      '0460',
                                    ' ' 'BDC_OKCODE'    '/00',
                                    ' ' 'MKPF-BUDAT'   WA_DATA,
                                    ' ' 'RM07M-MBLNR'  VL_MAT_DOC,
                                    ' ' 'RM07M-MJAHR'  VL_MATDOCUMENTYEAR,
                                    ' ' 'RM07M-LGORT'  WA_DEPARA-LGORT_F,
                                    ' ' 'RM07M-WVERS2'  'X'.
  ELSE.
    PERFORM Z_INSERE_BDC USING: 'X' 'SAPMM07M'      '0460',
                                    ' ' 'BDC_OKCODE'    '/00',
                                    ' ' 'MKPF-BUDAT'   WA_DATA,
                                    ' ' 'RM07M-MBLNR'  VL_MAT_DOC,
                                    ' ' 'RM07M-MJAHR'  VL_MATDOCUMENTYEAR,
                                    ' ' 'RM07M-LGORT'  WA_DEPARA-LGORT,
                                    ' ' 'RM07M-WVERS2'  'X'.
  ENDIF.

  PERFORM Z_INSERE_BDC USING: 'X' 'SAPMM07M'    '0421',
                              ' ' 'BDC_OKCODE'  '=SP',
                              ' ' 'BDC_SUBSCR'  'SAPMM07M',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB',
                              ' ' 'DKACB-FMORE' 'X'.

  PERFORM Z_INSERE_BDC USING: 'X' 'SAPLKACB'    '0002',
                              ' ' 'BDC_OKCODE'  '=ENTE',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB'.

  PERFORM Z_INSERE_BDC USING: 'X' 'SAPMM07M'    '0410',
                              ' ' 'BDC_OKCODE'  'BU',
                              ' ' 'BDC_SUBSCR'  'MSEG-SGTXT',
                              ' ' 'MSEG-SGTXT'  VG_TEXTO,
                              ' ' 'BDC_SUBSCR'  'SAPMM07M',
                              ' ' 'BDC_SUBSCR'  'SAPMM07M'.

  PERFORM Z_INSERE_BDC USING: 'X' 'SAPLKACB'    '0002',
                              ' ' 'BDC_OKCODE'  '=ENTE',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB'.

  CALL TRANSACTION 'MBSU' USING T_BDC MODE 'N' UPDATE 'S' MESSAGES INTO T_MESSTAB.

  READ TABLE T_MESSTAB INTO W_MESSTAB WITH KEY MSGTYP = 'S' MSGNR = '060'.

  IF NOT SY-SUBRC IS INITIAL.
    VG_ERRO_MAT_E = 'X'.
    LOOP AT T_MESSTAB INTO W_MESSTAB.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = W_MESSTAB-MSGID
          MSG_NO                 = W_MESSTAB-MSGNR
          MSG_VAR1               = W_MESSTAB-MSGV1(50)
          MSG_VAR2               = W_MESSTAB-MSGV2(50)
          MSG_VAR3               = W_MESSTAB-MSGV3(50)
          MSG_VAR4               = W_MESSTAB-MSGV4(50)
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = W_MESSTAB-MSGTYP.
      APPEND W_MSN TO T_MSN.
    ENDLOOP.
  ELSE.

    VL_MAT_DOC_E         = W_MESSTAB-MSGV1(10).
    VL_MATDOCUMENTYEAR_E = VL_MATDOCUMENTYEAR.

    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 3 (entrada )
    UPDATE ZSDT0023 SET MBLNR_E  = @VL_MAT_DOC_E,
                        MJAHR_E  = @VL_MATDOCUMENTYEAR_E
    WHERE VBELN = @VG_REMESSA.
    COMMIT WORK.

    LOOP AT T_MESSTAB INTO W_MESSTAB.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = W_MESSTAB-MSGID
          MSG_NO                 = W_MESSTAB-MSGNR
          MSG_VAR1               = W_MESSTAB-MSGV1(50)
          MSG_VAR2               = W_MESSTAB-MSGV2(50)
          MSG_VAR3               = W_MESSTAB-MSGV3(50)
          MSG_VAR4               = W_MESSTAB-MSGV4(50)
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = W_MESSTAB-MSGTYP.
      APPEND W_MSN TO T_MSN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GERA_MATERIAL_ENTRADA

*&---------------------------------------------------------------------*
*&      Form  CANCELA_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CANCELA_DOC_MATERIAL  USING VG_REMESSA           TYPE VBELN_VL
                                 VL_MAT_DOC           TYPE BAPI2017_GM_HEAD_RET-MAT_DOC
                                 VL_MATDOCUMENTYEAR   TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR
                                 VG_TIPO              TYPE C.

  DATA: GOODSMVT_HEADRET TYPE BAPI2017_GM_HEAD_RET,
        W_MSN            TYPE TYPE_MSN,
        MSG_TEXT         TYPE STRING,
        V_LOOP           TYPE I,
        VG_BLOQUEADO     TYPE C LENGTH 1.

  DATA: MSG_ID   LIKE  T100-ARBGB,
        MSG_NO   LIKE  T100-MSGNR,
        MSG_VAR1 LIKE  BALM-MSGV1,
        MSG_VAR2 LIKE  BALM-MSGV2,
        MSG_VAR3 LIKE  BALM-MSGV3,
        MSG_VAR4 LIKE  BALM-MSGV4.

  VG_BLOQUEADO = 'X'.
  CLEAR V_LOOP.

  WHILE NOT VG_BLOQUEADO IS INITIAL.
    ADD 1 TO  V_LOOP.
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        MATERIALDOCUMENT    = VL_MAT_DOC
        MATDOCUMENTYEAR     = VL_MATDOCUMENTYEAR
        GOODSMVT_PSTNG_DATE = SY-DATUM
      IMPORTING
        GOODSMVT_HEADRET    = GOODSMVT_HEADRET
      TABLES
        RETURN              = T_RETURN.

    READ TABLE T_RETURN WITH KEY ID = 'M3' NUMBER = 897.
    IF NOT SY-SUBRC IS INITIAL.
      CLEAR: VG_BLOQUEADO.
      "ALRS
      IF V_LOOP GT 1.
        LOOP AT T_RETURN.
          MOVE: T_RETURN-ID         TO MSG_ID  ,
               T_RETURN-NUMBER     TO MSG_NO  ,
               T_RETURN-MESSAGE_V1 TO MSG_VAR1,
               T_RETURN-MESSAGE_V2 TO MSG_VAR2,
               T_RETURN-MESSAGE_V3 TO MSG_VAR3,
               T_RETURN-MESSAGE_V4 TO MSG_VAR4.

          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              MSG_ID                 = MSG_ID
              MSG_NO                 = MSG_NO
              MSG_VAR1               = MSG_VAR1
              MSG_VAR2               = MSG_VAR2
              MSG_VAR3               = MSG_VAR3
              MSG_VAR4               = MSG_VAR4
            IMPORTING
              MSG_TEXT               = MSG_TEXT
            EXCEPTIONS
              FUNCTION_NOT_COMPLETED = 1
              MESSAGE_NOT_FOUND      = 2
              OTHERS                 = 3.

          W_MSN-MESSAGEM = MSG_TEXT.
          W_MSN-TP_MSN   = T_RETURN-TYPE.
          APPEND W_MSN TO T_MSN.
        ENDLOOP.
      ENDIF.
    ELSE.
      MOVE: T_RETURN-ID         TO MSG_ID  ,
            T_RETURN-NUMBER     TO MSG_NO  ,
            T_RETURN-MESSAGE_V1 TO MSG_VAR1,
            T_RETURN-MESSAGE_V2 TO MSG_VAR2,
            T_RETURN-MESSAGE_V3 TO MSG_VAR3,
            T_RETURN-MESSAGE_V4 TO MSG_VAR4.

      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = MSG_ID
          MSG_NO                 = MSG_NO
          MSG_VAR1               = MSG_VAR1
          MSG_VAR2               = MSG_VAR2
          MSG_VAR3               = MSG_VAR3
          MSG_VAR4               = MSG_VAR4
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.

      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = T_RETURN-TYPE.
      APPEND W_MSN TO T_MSN.

      WAIT UP TO 10 SECONDS.

    ENDIF.

  ENDWHILE.


  READ TABLE T_RETURN WITH KEY TYPE = 'E'.
  IF NOT SY-SUBRC IS INITIAL.
    CONCATENATE 'Documento' VL_MAT_DOC 'estornado por documento' GOODSMVT_HEADRET-MAT_DOC 'ano' GOODSMVT_HEADRET-DOC_YEAR '!' INTO MSG_TEXT SEPARATED BY SPACE.
    W_MSN-MESSAGEM = MSG_TEXT.
    W_MSN-TP_MSN   = 'S'.
    APPEND W_MSN TO T_MSN.

    IF   VG_TIPO  = 'S'.
      UPDATE ZSDT0023 SET ES_MBLNR_S = GOODSMVT_HEADRET-MAT_DOC
                          ES_MJAHR_S = GOODSMVT_HEADRET-DOC_YEAR
      WHERE VBELN = VG_REMESSA.
    ELSE.
      UPDATE ZSDT0023 SET ES_MBLNR_E = GOODSMVT_HEADRET-MAT_DOC
                          ES_MJAHR_E = GOODSMVT_HEADRET-DOC_YEAR
      WHERE VBELN = VG_REMESSA.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ELSE.
    "Deu Erro no estorno no documento de material
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM: Z_ERRO_ESTORNO_MATERIAL USING VL_MAT_DOC SPACE.
  ENDIF.


ENDFORM.                    " CANCELA_DOC_MATERIAL

FORM F_ESTORNO_RES  CHANGING P_ZSDT0001 TYPE ZSDT0001.
  DATA: W_ROMANEIO               TYPE ZSDT0001,
        WA_MAT_DOC               TYPE BAPI2017_GM_HEAD_02-MAT_DOC,
        WA_DOC_YEAR              TYPE BAPI2017_GM_HEAD_02-DOC_YEAR,
        WA_PSTNG_DATE            TYPE BAPI2017_GM_HEAD_02-PSTNG_DATE,
        VG_INVOICEDOCNUMBER_MIGO TYPE BAPI2017_GM_HEAD_RET,
        V_BUDAT                  TYPE MKPF-BUDAT,
        W_MSEG                   TYPE MSEG.

  DATA: WA_GOODSMVT_HEADER TYPE BAPI2017_GM_HEAD_01,
        T_GOODSMVT_ITEM    TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
        WA_GOODSMVT_ITEM   TYPE BAPI2017_GM_ITEM_CREATE,
        WA_CODE            TYPE BAPI2017_GM_CODE,
        VL_MAT_DOC         TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
        VL_MATDOCUMENTYEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
        T_RETURN_VT        LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.


  "Estorna entrada de Residuo
  SELECT SINGLE *
    FROM ZSDT0001
    INTO W_ROMANEIO
  WHERE CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.

  IF W_ROMANEIO-DOC_MATERIAL_E IS NOT INITIAL. "doc. material entrada residuo existe
    REFRESH T_RETURN_VT.
    SELECT SINGLE BUDAT INTO V_BUDAT
      FROM MKPF
      WHERE MBLNR = W_ROMANEIO-DOC_MATERIAL_E
    AND   MJAHR = W_ROMANEIO-ANO_MATERIAL_E.
    "
    WA_MAT_DOC      = W_ROMANEIO-DOC_MATERIAL_E.
    WA_DOC_YEAR    	= W_ROMANEIO-ANO_MATERIAL_E.
    WA_PSTNG_DATE   = V_BUDAT.

*    SELECT SINGLE *
*      INTO W_MSEG
*      FROM MSEG
*      WHERE MBLNR = W_ROMANEIO-DOC_MATERIAL_E
*      AND   MJAHR = W_ROMANEIO-ANO_MATERIAL_E
*    AND   BWART = 'ZX1'. "inverte
*
*    IF SY-SUBRC = 0.
*      CLEAR: T_GOODSMVT_ITEM.
*      WA_GOODSMVT_HEADER-PSTNG_DATE = V_BUDAT.
*      WA_GOODSMVT_HEADER-DOC_DATE   = V_BUDAT.
*      WA_GOODSMVT_HEADER-HEADER_TXT = W_ROMANEIO-VBELN.
*
*      WA_CODE-GM_CODE               = '05'.
*
*      WA_GOODSMVT_ITEM-MATERIAL     = W_MSEG-MATNR.
*      WA_GOODSMVT_ITEM-PLANT        = W_MSEG-WERKS.
*      WA_GOODSMVT_ITEM-STGE_LOC      = W_MSEG-LGORT.
*      WA_GOODSMVT_ITEM-BATCH        = W_MSEG-CHARG.
*
*      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'ZX5'. "ALRS 24/05/2017
*      WA_GOODSMVT_ITEM-ENTRY_QNT    = W_MSEG-MENGE.
*      APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*        EXPORTING
*          GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
*          GOODSMVT_CODE    = WA_CODE
*        IMPORTING
*          MATERIALDOCUMENT = VL_MAT_DOC
*          MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
*        TABLES
*          GOODSMVT_ITEM    = T_GOODSMVT_ITEM
*          RETURN           = T_RETURN_VT.
*    ELSE.
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        MATERIALDOCUMENT    = WA_MAT_DOC
        MATDOCUMENTYEAR     = WA_DOC_YEAR
        GOODSMVT_PSTNG_DATE = WA_PSTNG_DATE
      IMPORTING
        GOODSMVT_HEADRET    = VG_INVOICEDOCNUMBER_MIGO
      TABLES
        RETURN              = T_RETURN_VT.
*    ENDIF.

    IF T_RETURN_VT[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = C_X.
      UPDATE ZSDT0001
           SET DOC_MATERIAL_E      = ''
               ANO_MATERIAL_E      = ''
         WHERE CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
    ELSE.
      "gravar log
      READ TABLE T_RETURN_VT WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0.
        REFRESH TI_ZLEST0100.
        CLEAR VL_PONTEIRO.
        SELECT  MAX( CONT )
         FROM ZLEST0100
         INTO VL_PONTEIRO
        WHERE CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.

        IF SY-SUBRC = 0.
          ADD 1 TO VL_PONTEIRO.
        ELSE.
          VL_PONTEIRO = 1.
        ENDIF.
        LOOP AT T_RETURN_VT.
          WA_ZLEST0100-MANDT      = SY-MANDT.
          WA_ZLEST0100-CH_REFERENCIA   = P_ZSDT0001-CH_REFERENCIA.
          WA_ZLEST0100-MSGTYP     = 'E'.
          WA_ZLEST0100-MSGSPRA    = SY-LANGU.
          WA_ZLEST0100-MSGID      = 'LES'.
          WA_ZLEST0100-MSGNR      = '000'.
          WA_ZLEST0100-MSGV1      = T_RETURN_VT-MESSAGE.
          WA_ZLEST0100-DATA       = SY-DATUM.
          WA_ZLEST0100-HORA       = SY-UZEIT.
          WA_ZLEST0100-USUARIO    = SY-UNAME.
          WA_ZLEST0100-CONT       = VL_PONTEIRO.

          APPEND WA_ZLEST0100 TO TI_ZLEST0100.
          ADD 1 TO VL_PONTEIRO.
        ENDLOOP.
        MODIFY ZLEST0100 FROM TABLE TI_ZLEST0100.
      ENDIF.

*-#133089-21.02.2024-JT-inicio
      CASE VG_FATURAMENTO_AUTOM.
        WHEN ABAP_OFF.
        WHEN ABAP_TRUE.
          DATA: T_ERRO2  TYPE BAPIRET2_T.
          T_ERRO2[] = T_RETURN_VT[].
          LC_FATURAMENTO_AUTOMATICO->SET_GRAVAR_MENSAGEM( I_CH_REFERENCIA = V_NR_ROMANEIO I_TYPE = 'E' I_TAB_BAPIRET2 = T_ERRO2 I_STATUS = 'REME' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ENTRADA_RESIDUO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_ENTRADA_RESIDUO USING SL_ZSDT0001        TYPE ZSDT0001
                             VL_MAT_DOC         TYPE BAPI2017_GM_HEAD_RET-MAT_DOC
                             VL_MATDOCUMENTYEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR
                             P_RETENT.

  DATA: SL_ZSDT0001_ENT TYPE ZSDT0001,
        V_NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO,
        W_MSN           TYPE TYPE_MSN,
        MSG_TEXT        TYPE STRING,
        MSG_NO          TYPE T100-MSGNR.


  CHECK SL_ZSDT0001-ID_CARGA IS INITIAL.

  CLEAR: T_GOODSMVT_ITEM, T_RETURN.

  CLEAR: VL_MAT_DOC, VL_MATDOCUMENTYEAR.

  SELECT SINGLE *
    FROM ZSDT0001 INTO @DATA(_WL_0001)
  WHERE CH_REFERENCIA EQ @SL_ZSDT0001-CH_REFERENCIA.

  IF ( SY-SUBRC = 0 ) AND
     ( _WL_0001-DOC_MATERIAL_E IS NOT INITIAL ) AND
     ( _WL_0001-ANO_MATERIAL_E IS NOT INITIAL ).
    VL_MAT_DOC         = _WL_0001-DOC_MATERIAL_E.
    VL_MATDOCUMENTYEAR = _WL_0001-ANO_MATERIAL_E.
    EXIT.
  ENDIF.

  WA_GOODSMVT_HEADER-PSTNG_DATE = SY-DATUM.
  WA_GOODSMVT_HEADER-DOC_DATE   = SY-DATUM.
  WA_GOODSMVT_HEADER-HEADER_TXT = SL_ZSDT0001-VBELN.

  WA_CODE-GM_CODE               = C_05.

*--> 19.06.2023 - Migration S4 – MIGNOW - Start
  "  wa_goodsmvt_item-material     = s_vbap-matnr.
  DATA(V_LEN1) = STRLEN( S_VBAP-MATNR ).
  IF V_LEN1 > 18.
    WA_GOODSMVT_ITEM-MATERIAL_LONG = S_VBAP-MATNR .
  ELSE.
    WA_GOODSMVT_ITEM-MATERIAL = S_VBAP-MATNR .
  ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
  WA_GOODSMVT_ITEM-PLANT        = S_VBAP-VSTEL.
  WA_GOODSMVT_ITEM-STGE_LOC	    = S_VBAP-LGORT.
  WA_GOODSMVT_ITEM-BATCH        = S_VBAP-CHARG.

  WA_GOODSMVT_ITEM-MOVE_TYPE    = SL_ZMMT0074-BWART. "ALRS 22/05/2017
  IF SL_ZMMT0074-ENTRADA_ROM = 'S'. "Checa romaneio de entrada

    IF ( SL_ZSDT0001-ID_REFERENCIA IS INITIAL ) AND ( SL_ZSDT0001-ID_CARGA IS INITIAL ). "
      EXIT. "Não gera  doc. entrada
    ENDIF.

    IF SL_ZSDT0001-ID_CARGA IS NOT INITIAL.
      CLEAR SL_ZSDT0001_ENT-PESO_LIQ.

      SELECT SUM( PESO_LIQ )
        FROM ZSDT0001
        INTO SL_ZSDT0001_ENT-PESO_LIQ
       WHERE TP_MOVIMENTO EQ 'E'
         AND ID_CARGA     EQ SL_ZSDT0001-ID_CARGA.

    ELSE.

      DATA(_NR_ROMANEIO) = SL_ZSDT0001-ID_REFERENCIA.

      CLEAR: SL_ZSDT0001_ENT.

      SELECT SINGLE *
        FROM ZSDT0001
        INTO SL_ZSDT0001_ENT
        WHERE BUKRS         = SL_ZSDT0001-BUKRS
        AND   BRANCH        = SL_ZSDT0001-BRANCH
        AND   TP_MOVIMENTO  = 'E'
        AND   NR_ROMANEIO   = _NR_ROMANEIO
        "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
      AND   NR_SAFRA      = SL_ZSDT0001-NR_SAFRA.

      IF SL_ZSDT0001_ENT-CH_REFER_ENT IS NOT INITIAL.
        CLEAR SL_ZSDT0001_ENT-PESO_LIQ.
        SELECT SUM( PESO_LIQ )
            FROM ZSDT0001
            INTO SL_ZSDT0001_ENT-PESO_LIQ
            WHERE BUKRS         = SL_ZSDT0001_ENT-BUKRS
            AND   BRANCH        = SL_ZSDT0001_ENT-BRANCH
            AND   TP_MOVIMENTO  = 'E'
            AND   CH_REFER_ENT  = SL_ZSDT0001_ENT-CH_REFER_ENT
            "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
        AND   NR_SAFRA      = SL_ZSDT0001_ENT-NR_SAFRA.
      ENDIF.

    ENDIF.

    IF SY-SUBRC = 0.

      IF SL_ZSDT0001-PESO_LIQ_COMERCIAL IS INITIAL.
        CONCATENATE 'Peso liquido comercial do romaneio' SL_ZSDT0001-NR_ROMANEIO 'não encontrado!' INTO MSG_TEXT SEPARATED BY SPACE.
        W_MSN-NR_ROMANEIO = SL_ZSDT0001-NR_ROMANEIO.
        W_MSN-MESSAGEM = MSG_TEXT.
        W_MSN-TP_MSN   = 'E'.
        APPEND W_MSN TO T_MSN.
        P_RETENT = 'X'.
        EXIT.
      ENDIF.

      IF  SL_ZSDT0001-PESO_LIQ GT SL_ZSDT0001-PESO_LIQ_COMERCIAL.  "SL_ZSDT0001_ENT-PESO_LIQ.
        WA_GOODSMVT_ITEM-ENTRY_QNT = SL_ZSDT0001-PESO_LIQ - SL_ZSDT0001-PESO_LIQ_COMERCIAL.
      ELSE.
        EXIT. "Não gera  doc. entrada
      ENDIF.
    ELSE.
      CONCATENATE 'Não foi encontrado o romaneio de entrada, nesta data' '!' INTO MSG_TEXT SEPARATED BY SPACE.
      W_MSN-NR_ROMANEIO = SL_ZSDT0001-NR_ROMANEIO.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = 'E'.
      APPEND W_MSN TO T_MSN.
      P_RETENT = 'X'.
      EXIT.
    ENDIF.

  ELSE.

    WA_GOODSMVT_ITEM-ENTRY_QNT    = SL_ZSDT0001-PESO_LIQ.

  ENDIF.



  APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
      GOODSMVT_CODE    = WA_CODE
    IMPORTING
      MATERIALDOCUMENT = VL_MAT_DOC
      MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
    TABLES
      GOODSMVT_ITEM    = T_GOODSMVT_ITEM
      RETURN           = T_RETURN.

  READ TABLE T_RETURN WITH KEY TYPE = 'E'.

  IF ( NOT SY-SUBRC IS INITIAL ) AND
     ( VL_MAT_DOC IS NOT INITIAL ) AND
     ( VL_MATDOCUMENTYEAR IS NOT INITIAL ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    UPDATE ZSDT0001 SET DOC_MATERIAL_E = VL_MAT_DOC
                        ANO_MATERIAL_E = VL_MATDOCUMENTYEAR
     WHERE CH_REFERENCIA EQ SL_ZSDT0001-CH_REFERENCIA.

    COMMIT WORK.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    LOOP AT T_RETURN INTO DATA(W_RETURN).
      WRITE W_RETURN-NUMBER TO MSG_NO.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          MSG_ID                 = W_RETURN-ID
          MSG_NO                 = MSG_NO
          MSG_VAR1               = W_RETURN-MESSAGE_V1
          MSG_VAR2               = W_RETURN-MESSAGE_V2
          MSG_VAR3               = W_RETURN-MESSAGE_V3
          MSG_VAR4               = W_RETURN-MESSAGE_V4
        IMPORTING
          MSG_TEXT               = MSG_TEXT
        EXCEPTIONS
          FUNCTION_NOT_COMPLETED = 1
          MESSAGE_NOT_FOUND      = 2
          OTHERS                 = 3.
      W_MSN-MESSAGEM = MSG_TEXT.
      W_MSN-TP_MSN   = W_RETURN-TYPE.
      APPEND W_MSN TO T_MSN.
    ENDLOOP.

    P_RETENT = 'X'.

  ENDIF.

ENDFORM.                          " Z_ENTRADA_RESIDUO

*&---------------------------------------------------------------------*
*&      Form  MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SL_DATA_REM  text
*----------------------------------------------------------------------*
FORM MEMORIZAR_DT_MOVIMENTO_BADI  USING P_DATA_REM TYPE LEDAT.

  TYPES:
    BEGIN OF TAB_TYPE,
      PARA TYPE STRING,
      DOBJ TYPE STRING,
    END OF TAB_TYPE.

  DATA: LINE TYPE TAB_TYPE,
        ITAB TYPE STANDARD TABLE OF TAB_TYPE,
        ID   TYPE C LENGTH 10 VALUE 'ROMRETRO'.

  LINE-PARA = 'P1'.
  LINE-DOBJ = 'P_DATA_REM'.
  APPEND LINE TO ITAB.

  EXPORT (ITAB) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI

FORM Z_ADD_ITENS_ROM TABLES P_ITEM     STRUCTURE BAPIDLVREFTOSALESORDER
                      USING P_ZSDT0001 TYPE ZSDT0001.

  DATA: S_ITEM TYPE BAPIDLVREFTOSALESORDER.

  CLEAR: P_ITEM[].

  LOOP AT T_ZSDT0001_ITEM_GRP WHERE CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.

    CLEAR: S_ITEM.

    READ TABLE TG_VBAP WITH KEY VBELN = T_ZSDT0001_ITEM_GRP-VBELN
                                POSNR = T_ZSDT0001_ITEM_GRP-POSNR.

    CHECK SY-SUBRC = 0.

    S_ITEM-REF_DOC    = T_ZSDT0001_ITEM_GRP-VBELN.
    S_ITEM-REF_ITEM   = T_ZSDT0001_ITEM_GRP-POSNR.

    LOOP AT T_ZSDT0001_ITEM WHERE CH_REFERENCIA = T_ZSDT0001_ITEM_GRP-CH_REFERENCIA
                              AND PART_LOTE     = T_ZSDT0001_ITEM_GRP-PART_LOTE.
      ADD T_ZSDT0001_ITEM-LFIMG TO S_ITEM-DLV_QTY.
    ENDLOOP.

    S_ITEM-SALES_UNIT = TG_VBAP-VRKME.

    APPEND S_ITEM TO P_ITEM.

  ENDLOOP.

ENDFORM.

FORM Z_INS_LOTE_ITENS TABLES P_RETURN   STRUCTURE BAPIRET2
                       USING P_DELIVERY TYPE BAPISHPDELIVNUMB-DELIV_NUMB.

  DATA: WA_HEADER_DATA    TYPE BAPIOBDLVHDRCHG,
        WA_HEADER_CONTROL TYPE BAPIOBDLVHDRCTRLCHG,
        HEADER_PARTNER    TYPE TABLE OF BAPIDLVPARTNERCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_DATA         TYPE TABLE OF BAPIOBDLVITEMCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_CONTROL      TYPE TABLE OF BAPIOBDLVITEMCTRLCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_DATA_SPL     TYPE TABLE OF /SPE/BAPIOBDLVITEMCHG INITIAL SIZE 0 WITH HEADER LINE,
        ST_HEADER         TYPE THEAD,
        ST_LINES          TYPE TLINE,
        TG_LIPS_AUX       TYPE TABLE OF LIPS WITH HEADER LINE,
        TG_LIPS           TYPE TABLE OF LIPS WITH HEADER LINE.

  CLEAR: WA_HEADER_DATA, WA_HEADER_CONTROL,
         HEADER_PARTNER, HEADER_PARTNER[],
         ITEM_DATA, ITEM_DATA[],
         ITEM_CONTROL, ITEM_CONTROL[],
         ITEM_DATA_SPL, ITEM_DATA_SPL[],
         P_RETURN[].

  DATA: VL_ITM_LOTE      TYPE I,
        VL_COUNT_ITM     TYPE I,
        VL_PARTICAO_LOTE TYPE C,
        VL_LFIMG         TYPE ZSDT0001_ITEM-LFIMG.

  WA_HEADER_DATA-DELIV_NUMB    = P_DELIVERY.
  WA_HEADER_CONTROL-DELIV_NUMB = 'X'.

  SELECT *
    FROM LIPS INTO TABLE TG_LIPS
  WHERE VBELN = P_DELIVERY.

  TG_LIPS_AUX[] = TG_LIPS[].

  "Itens
  VL_ITM_LOTE = 900001.
  LOOP AT T_ZSDT0001_ITEM_GRP WHERE VBELN  = P_VBELN-LOW.

    CLEAR: VL_PARTICAO_LOTE, VL_COUNT_ITM, TG_LIPS, TG_LIPS_AUX, VL_LFIMG.

    "Check se item deverá ter partição de Lote
    LOOP AT T_ZSDT0001_ITEM WHERE CH_REFERENCIA = T_ZSDT0001_ITEM_GRP-CH_REFERENCIA
                              AND PART_LOTE     = T_ZSDT0001_ITEM_GRP-PART_LOTE.
      ADD T_ZSDT0001_ITEM-LFIMG TO VL_LFIMG.
      ADD 1 TO VL_COUNT_ITM.
    ENDLOOP.

    IF VL_COUNT_ITM > 1.
      VL_PARTICAO_LOTE = 'X'.
    ENDIF.

    "Check Item na remessa criado referente ao item da O.V
    LOOP AT TG_LIPS_AUX WHERE VGBEL  = T_ZSDT0001_ITEM_GRP-VBELN
                          AND VGPOS  = T_ZSDT0001_ITEM_GRP-POSNR
                          AND LFIMG  = VL_LFIMG.
      MOVE-CORRESPONDING TG_LIPS_AUX TO TG_LIPS.
      DELETE TG_LIPS_AUX.
      EXIT.
    ENDLOOP.

    ITEM_DATA-DELIV_NUMB          = P_DELIVERY.
    ITEM_DATA-HIERARITEM          = TG_LIPS-POSNR.
    ITEM_DATA-USEHIERITM          = 1.

    "Lotes
    LOOP AT T_ZSDT0001_ITEM WHERE CH_REFERENCIA = T_ZSDT0001_ITEM_GRP-CH_REFERENCIA
                              AND PART_LOTE     = T_ZSDT0001_ITEM_GRP-PART_LOTE.

      T_ZSDT0001_ITEM-POSNR_REM          = TG_LIPS-POSNR.

      IF VL_PARTICAO_LOTE IS NOT INITIAL.
        T_ZSDT0001_ITEM-ITM_LOTE    = VL_ITM_LOTE.
        ITEM_DATA-DELIV_ITEM        = VL_ITM_LOTE.
      ELSE.
        T_ZSDT0001_ITEM-ITM_LOTE    = TG_LIPS-POSNR.
        ITEM_DATA-DELIV_ITEM        = TG_LIPS-POSNR.
      ENDIF.
      ITEM_DATA-BATCH               = T_ZSDT0001_ITEM-CHARG.
      ITEM_DATA-DLV_QTY             = T_ZSDT0001_ITEM-LFIMG.
      ITEM_DATA-DLV_QTY_IMUNIT      = T_ZSDT0001_ITEM-LFIMG.
      ITEM_DATA-FACT_UNIT_NOM       = 1.
      ITEM_DATA-FACT_UNIT_DENOM     = 1.
      ITEM_DATA-GROSS_WT            = T_ZSDT0001_ITEM-BRGEW. "Peso bruto
      ITEM_DATA-NET_WEIGHT          = T_ZSDT0001_ITEM-NTGEW. "Peso Liquido

*-->   27.07.2023 - Migration S4 – - Start
      "  item_data-material            = t_zsdt0001_item-matnr.
      DATA(V_LEN2) = STRLEN( T_ZSDT0001_ITEM-MATNR ).
      IF V_LEN2 > 18.
        ITEM_DATA-MATERIAL_LONG = T_ZSDT0001_ITEM-MATNR .
      ELSE.
        ITEM_DATA-MATERIAL      = T_ZSDT0001_ITEM-MATNR .
      ENDIF.
*      <-- 19.06.2023 - Migration S4 – End

      APPEND ITEM_DATA.

      ITEM_CONTROL-DELIV_NUMB       = P_DELIVERY.
      IF VL_PARTICAO_LOTE IS NOT INITIAL.
        ITEM_CONTROL-DELIV_ITEM     = T_ZSDT0001_ITEM-ITM_LOTE.
      ELSE.
        ITEM_CONTROL-DELIV_ITEM     = TG_LIPS-POSNR.
      ENDIF.
      ITEM_CONTROL-CHG_DELQTY       = 'X'.
      ITEM_CONTROL-VOLUME_FLG       = 'X'.
      ITEM_CONTROL-NET_WT_FLG       = 'X'.
      ITEM_CONTROL-GROSS_WT_FLG     = 'X'.
      APPEND ITEM_CONTROL.

      IF VL_PARTICAO_LOTE IS NOT INITIAL.
        ADD 1 TO VL_ITM_LOTE.
      ENDIF.

      MODIFY T_ZSDT0001_ITEM.

    ENDLOOP.

  ENDLOOP.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      HEADER_DATA    = WA_HEADER_DATA
      HEADER_CONTROL = WA_HEADER_CONTROL
      DELIVERY       = P_DELIVERY
    TABLES
      HEADER_PARTNER = HEADER_PARTNER
      ITEM_DATA      = ITEM_DATA
      ITEM_CONTROL   = ITEM_CONTROL
      RETURN         = P_RETURN
      ITEM_DATA_SPL  = ITEM_DATA_SPL.

  IF P_RETURN[] IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

ENDFORM.


FORM Z_TROCA_PESO TABLES P_RETURN   STRUCTURE BAPIRET2
                       USING P_DELIVERY   TYPE BAPISHPDELIVNUMB-DELIV_NUMB
                             P_ZSDT0001   TYPE ZSDT0001
                             P_PESO_BRUTO TYPE BRGEW_15
                             P_PESO_LIQ   TYPE NTGEW_15.

  DATA: WA_HEADER_DATA    TYPE BAPIOBDLVHDRCHG,
        WA_HEADER_CONTROL TYPE BAPIOBDLVHDRCTRLCHG,
        HEADER_PARTNER    TYPE TABLE OF BAPIDLVPARTNERCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_DATA         TYPE TABLE OF BAPIOBDLVITEMCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_CONTROL      TYPE TABLE OF BAPIOBDLVITEMCTRLCHG INITIAL SIZE 0 WITH HEADER LINE,
        ITEM_DATA_SPL     TYPE TABLE OF /SPE/BAPIOBDLVITEMCHG INITIAL SIZE 0 WITH HEADER LINE,
        ST_HEADER         TYPE THEAD,
        ST_LINES          TYPE TLINE,
        TG_LIPS           TYPE TABLE OF LIPS WITH HEADER LINE,
        T_SET             TYPE TABLE OF RGSB4,
        W_SET             TYPE RGSB4.

  RANGES: R_MATKL FOR VBAP-MATKL.

  CLEAR: WA_HEADER_DATA, WA_HEADER_CONTROL,
         HEADER_PARTNER, HEADER_PARTNER[],
         ITEM_DATA, ITEM_DATA[],
         ITEM_CONTROL, ITEM_CONTROL[],
         ITEM_DATA_SPL, ITEM_DATA_SPL[],
         P_RETURN[].

  DATA: VL_ITM_LOTE      TYPE I,
        VL_COUNT_ITM     TYPE I,
        VL_PARTICAO_LOTE TYPE C,
        VL_LFIMG         TYPE ZSDT0001_ITEM-LFIMG.

  CLEAR: VL_PARTICAO_LOTE, VL_COUNT_ITM, TG_LIPS, VL_LFIMG.
  WA_HEADER_DATA-DELIV_NUMB    = P_DELIVERY.
  WA_HEADER_CONTROL-DELIV_NUMB = 'X'.

  SELECT *
    FROM LIPS INTO TABLE TG_LIPS
  WHERE VBELN = P_DELIVERY.

*-CS2021000615 - 20.07.2021 - JT - inicio
*------------------------------
*-Grupo de material que gera volume
*------------------------------
  FREE: T_SET, R_MATKL.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS           = '0000'
      SETNR           = 'ZSDI0009_MATKL'
      NO_DESCRIPTIONS = ABAP_FALSE
    TABLES
      SET_VALUES      = T_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  LOOP AT T_SET   INTO W_SET.
    R_MATKL-SIGN   = 'I'.
    R_MATKL-OPTION = 'EQ'.
    R_MATKL-LOW    = W_SET-FROM.
    APPEND R_MATKL.
  ENDLOOP.

*------------------------------
*-item OV
*------------------------------
  SELECT MATKL, VOLEH
    FROM VBAP
    INTO (@DATA(L_MATKL), @DATA(L_VOLEH))
      UP TO 1 ROWS
   WHERE VBELN = @P_ZSDT0001-VBELN
     AND MATNR = @P_ZSDT0001-MATNR.
  ENDSELECT.

  IF ( L_MATKL IN R_MATKL[] AND R_MATKL[] IS NOT INITIAL ) AND ( P_ZSDT0001-MATNR IS NOT INITIAL ).
*-------------------------
*---Qtd embalagens
*-------------------------
    SELECT NM_QTD_EMBALAGENS
      INTO @DATA(L_QTD_EMB)
      FROM ZSDT0001OVRO
        UP TO 1 ROWS
*    WHERE id_carga          = @p_zsdt0001-id_carga
     WHERE NR_ORDEM_VENDA    = @P_ZSDT0001-VBELN
       AND CH_REFERENCIA_SAI = @P_ZSDT0001-CH_REFERENCIA.
    ENDSELECT.

    IF ( SY-SUBRC = 0 ) AND ( L_QTD_EMB IS NOT INITIAL  ).
      ITEM_DATA-VOLUME     = L_QTD_EMB.
      ITEM_DATA-VOLUMEUNIT = L_VOLEH.
    ELSE.
      SELECT SINGLE * INTO @DATA(LWA_ZSDT0001_ITEM)
        FROM ZSDT0001_ITEM
       WHERE CH_REFERENCIA  = @P_ZSDT0001-CH_REFERENCIA
         AND VBELN          = @P_ZSDT0001-VBELN
         AND MATNR          = @P_ZSDT0001-MATNR.

      IF ( SY-SUBRC EQ 0 ) AND ( LWA_ZSDT0001_ITEM-VOLEH IS NOT INITIAL ) AND ( LWA_ZSDT0001_ITEM-VOLUM IS NOT INITIAL ).
        ITEM_DATA-VOLUME     = LWA_ZSDT0001_ITEM-VOLUM.
        ITEM_DATA-VOLUMEUNIT = LWA_ZSDT0001_ITEM-VOLEH.
      ENDIF.
    ENDIF.

  ENDIF.
*-CS2021000615 - 20.07.2021 - JT - fim

  READ TABLE TG_LIPS INDEX 1.

  ITEM_DATA-DELIV_NUMB          = P_DELIVERY.
  ITEM_DATA-HIERARITEM          = TG_LIPS-POSNR.
  ITEM_DATA-USEHIERITM          = 1.
  ITEM_DATA-DELIV_ITEM          = TG_LIPS-POSNR.
  ITEM_DATA-BATCH               = P_ZSDT0001-NR_SAFRA.

  IF ( V_CHARG IS NOT INITIAL ).
    ITEM_DATA-BATCH             = V_CHARG.
  ENDIF.

  IF S_VBAP-CHARG IS NOT INITIAL.
    ITEM_DATA-BATCH = S_VBAP-CHARG.
  ENDIF.

  IF P_ZSDT0001-QTDE_REMESSA IS NOT INITIAL.
    ITEM_DATA-DLV_QTY             = P_ZSDT0001-QTDE_REMESSA.
    ITEM_DATA-DLV_QTY_IMUNIT      = P_ZSDT0001-QTDE_REMESSA. "P_PESO.
    ITEM_DATA-FACT_UNIT_DENOM     = 1.
    ITEM_DATA-FACT_UNIT_NOM       = 1.
  ELSE.
    ITEM_DATA-DLV_QTY             = P_PESO_LIQ.
    ITEM_DATA-DLV_QTY_IMUNIT      = P_PESO_LIQ. "P_PESO.
    ITEM_DATA-FACT_UNIT_NOM       = 1.
    ITEM_DATA-FACT_UNIT_DENOM     = 1.
  ENDIF.

  ITEM_DATA-GROSS_WT            = P_PESO_BRUTO.    "P_ZSDT0001-PESO_LIQ. "Peso bruto
  ITEM_DATA-NET_WEIGHT          = P_PESO_LIQ.       "P_PESO. "Peso Liquido

*-->   27.07.2023 - Migration S4 – - Start
  "  item_data-material            = p_zsdt0001-matnr.
  DATA(V_LEN2) = STRLEN( P_ZSDT0001-MATNR ).
  IF V_LEN2 > 18.
    ITEM_DATA-MATERIAL_LONG = P_ZSDT0001-MATNR .
  ELSE.
    ITEM_DATA-MATERIAL      = P_ZSDT0001-MATNR .
  ENDIF.
*      <-- 19.06.2023 - Migration S4 – End






  APPEND ITEM_DATA.

  ITEM_CONTROL-DELIV_NUMB       = P_DELIVERY.
  ITEM_CONTROL-DELIV_ITEM       = TG_LIPS-POSNR.
  ITEM_CONTROL-CHG_DELQTY       = 'X'.
  ITEM_CONTROL-VOLUME_FLG       = 'X'.
  ITEM_CONTROL-NET_WT_FLG       = 'X'.
  ITEM_CONTROL-GROSS_WT_FLG     = 'X'.
  APPEND ITEM_CONTROL.


  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      HEADER_DATA    = WA_HEADER_DATA
      HEADER_CONTROL = WA_HEADER_CONTROL
      DELIVERY       = P_DELIVERY
    TABLES
      HEADER_PARTNER = HEADER_PARTNER
      ITEM_DATA      = ITEM_DATA
      ITEM_CONTROL   = ITEM_CONTROL
      RETURN         = P_RETURN
      ITEM_DATA_SPL  = ITEM_DATA_SPL.

  IF P_RETURN[] IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ENDIF.

ENDFORM.

FORM F_CHECK_EXCECAO_RESIDUO USING P_MATNR TYPE ZSDT0001-MATNR
                                   P_BUKRS TYPE ZSDT0001-BUKRS
                          CHANGING C_OK    TYPE C.

  CLEAR: C_OK.

  DATA: LVA_MATNR TYPE ZSDT0001-MATNR.

  LVA_MATNR = P_MATNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = LVA_MATNR
    IMPORTING
      OUTPUT = LVA_MATNR.

  CONCATENATE P_BUKRS '-' LVA_MATNR INTO DATA(_BUKRS_MAT_EXC_KEY).

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(_LWA_BUKRS_EXC)
   WHERE SETNAME EQ 'RESIDUO_EXC'
     AND VALFROM EQ @_BUKRS_MAT_EXC_KEY.

  IF SY-SUBRC EQ 0.
    C_OK = ABAP_TRUE.
  ENDIF.

ENDFORM.
