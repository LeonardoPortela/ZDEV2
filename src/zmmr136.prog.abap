*&---------------------------------------------------------------------*
*& Report.: ZMMR136
*& Desc...: MTM Estoque Filial
*& Autor..: Jean Antunes
*&---------------------------------------------------------------------*

REPORT ZMMR136.

TABLES: CKI_DOC_ML, CKMLHD, CKMLPP, CKMLRUNPLANT, MARA, MBEW, MARV, T001K, T030, T134G, MLKEY, SSCRFIELDS.

TYPES: BEGIN OF TY_T001W,
         WERKS TYPE T001W-WERKS,
         NAME1 TYPE T001W-NAME1,
       END OF TY_T001W,

       BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_SAIDA,
         STATUS            TYPE C,
         ICON(4)           TYPE C,
         MONAT             TYPE ZMMT0093-MONAT,
         GJARH             TYPE ZMMT0093-GJARH,
         WERKS             TYPE T001W-WERKS,   "Filial
         NAME1             TYPE T001W-NAME1,   "Nome Filial
         MATNR             TYPE CKMLHD-MATNR,  "Material
         KTEXT             TYPE MAKT-MAKTX,    "Desc.Material
         LBKUM             TYPE CKMLPP-LBKUM,  "ESTOQUE TOTAL
         CUSTO_MEDIO       TYPE DMBTR,
         CUSTO_MEDIO_USD   TYPE DMBTR,
         TOTAL_EST_BRL     TYPE DMBTR,
         TOTAL_EST_USD     TYPE DMBTR,
         CUSTO_MTM_BRL     TYPE DMBTR,
         CUSTO_MTM_USD     TYPE DMBTR,
         TOTAL_MTM_BRL     TYPE DMBTR,
         TOTAL_MTM_USD     TYPE DMBTR,
         VARIACAO_BRL      TYPE DMBTR,
         VARIACAO_USD      TYPE DMBTR,
         DOC_CONTABIL(255) TYPE C, "ZIB_CONTABIL_CHV-BELNR,
         DOC_ESTORNO(255)  TYPE C, "ZIB_CONTABIL_CHV-BELNR,
         OBJ_KEY           TYPE ZMMT0093-OBJ_KEY,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ZIB_LOG,
         OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         BELNR   TYPE ZIB_CONTABIL_CHV-BELNR,
       END OF TY_ZIB_LOG,

       BEGIN OF TY_ZIB_ERR,
         OBJ_KEY    TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
         MESSAGE    TYPE ZIB_CONTABIL_ERR-MESSAGE,
         MESSAGE_V1 TYPE ZIB_CONTABIL_ERR-MESSAGE_V1,
       END OF TY_ZIB_ERR.

DATA: BEGIN OF TG_SAIDA_ZCO0016 OCCURS 0,
        KALNR       TYPE CKMLHD-KALNR,
        MLAST       TYPE CKMLHD-MLAST,
        MATNR       TYPE CKMLHD-MATNR,
        BWKEY       TYPE CKMLHD-BWKEY,
        BWTAR       TYPE CKMLHD-BWTAR,
        SOBKZ       TYPE CKMLHD-SOBKZ,
        VBELN       TYPE CKMLHD-VBELN,
        POSNR       TYPE CKMLHD-POSNR,
        PSPNR       TYPE CKMLHD-PSPNR,
        BDATJ       TYPE CKMLPP-BDATJ,
        POPER       TYPE CKMLPP-POPER,
        STATUS      TYPE CKMLPP-STATUS,
        STATUS_TEXT TYPE DD07V-DDTEXT,
        KONTS       TYPE T030-KONTS,  "Conta razão
        CURTP       TYPE CKMLCR-CURTP,
        BKLAS       TYPE MBEW-BKLAS,
        WERKS       TYPE T001W-WERKS,
        MTART       TYPE MARA-MTART,
        MATKL       TYPE MARA-MATKL,
        SPART       TYPE MARA-SPART,
        KTEXT       TYPE MAKT-MAKTX,
        VPRSV       TYPE CKMLCR-VPRSV,
        LBKUM       TYPE CKMLPP-LBKUM, "quantidade estoque
        MEINS       TYPE MARA-MEINS,
        SALK3       TYPE CKMLCR-SALK3,
        SALK3_U     TYPE CKMLCR-SALK3,
        SALKV       TYPE CKMLCR-SALKV,
        EB_DIF      TYPE CKI_DOC_ML-EB_DIF,
        STPRS       TYPE CKMLCR-STPRS,
        PVPRS       TYPE CKMLCR-PVPRS,
        PRABW_PRZ   TYPE CK_PRABW_PRZ,
        PEINH       TYPE CKMLCR-PEINH,
        WAERS       TYPE WAERS,
        GSBER       TYPE T134G-GSBER.
DATA: END OF TG_SAIDA_ZCO0016.

DATA:  L_DATA            TYPE REF TO DATA,
       L_DATA_LINE       TYPE REF TO DATA,
       L_DATA_DESCR      TYPE REF TO CL_ABAP_DATADESCR,
       L_DATA_LINE_DESCR TYPE REF TO CL_ABAP_DATADESCR,

       TG_T001W          TYPE TABLE OF TY_T001W,
       TG_MAKT           TYPE TABLE OF TY_MAKT,
       TG_ZFIT0049       TYPE TABLE OF ZFIT0049,
       TG_TCURR          TYPE TABLE OF TCURR,
       TG_SAIDA          TYPE TABLE OF TY_SAIDA,
       TG_SAIDA_AUX      TYPE TABLE OF TY_SAIDA,
       TG_SAIDA_GERA     TYPE TABLE OF TY_SAIDA,
       TG_ZMMT0093       TYPE TABLE OF ZMMT0093,
       TG_ZIB_LOG        TYPE TABLE OF TY_ZIB_LOG,
       TG_ZIB_ERR        TYPE TABLE OF TY_ZIB_ERR,
       TG_ZIB_CONT       TYPE TABLE OF ZIB_CONTABIL,

       WG_T001W          TYPE TY_T001W,
       WG_MAKT           TYPE TY_MAKT,
       WG_TCURR          TYPE TCURR,
       WG_SAIDA          TYPE TY_SAIDA,
       WG_ZMMT0093       TYPE ZMMT0093,
       WG_ZIB_LOG        LIKE LINE OF TG_ZIB_LOG,
       WG_ZIB_ERR        LIKE LINE OF TG_ZIB_ERR,
       WG_ZIB_CONT       LIKE LINE OF TG_ZIB_CONT,

       V_SNUM(10)        TYPE C,
       VG_DIA_FIM        TYPE DATS.

FIELD-SYMBOLS: <T_DATA>      TYPE ANY TABLE,
               <T_DATA_LINE> TYPE ANY TABLE,
               <W_DATA>      TYPE ANY,
               <W_DATA_LINE> TYPE ANY,
               <WG_TCURR>    TYPE TCURR,
               <WG_SAIDA>    LIKE LINE OF TG_SAIDA.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: IT_FCAT           TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT2          TYPE TABLE OF LVC_S_FCAT,
      GS_VARIANT_C      TYPE DISVARIANT,
      GS_VARIANT_C2     TYPE DISVARIANT,
      WA_ALV_0001       TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONTAINER_0001 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      IT_SELECTED_ROWS  TYPE LVC_T_ROW,
      WA_SELECTED_ROWS  TYPE LVC_S_ROW,
      WA_LAYOUT         TYPE LVC_S_LAYO.



*----------------------------------------------------------------------*
* Classes
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_REPORT
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT_REPORT.

    PERFORM Z_HOTSPOT_REPORT USING  E_ROW_ID E_COLUMN_ID ES_ROW_NO.

  ENDMETHOD.
ENDCLASS.



*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS FOR MLKEY-BUKRS OBLIGATORY NO-EXTENSION NO INTERVALS,
                P_WERKS FOR MLKEY-WERKS OBLIGATORY,
                P_MATNR FOR CKMLHD-MATNR OBLIGATORY,
                P_MES   FOR CKI_DOC_ML-SL_PERIODE OBLIGATORY NO-EXTENSION NO INTERVALS,
                P_ANO   FOR CKMLRUNPLANT-GJAHR OBLIGATORY NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

****************************************************************
*START-OF-SELECTION
****************************************************************
START-OF-SELECTION.

  PERFORM SELECIONA_GERADOS.        "Busca na ZMMT0093 os materiais que já tiveram MTM gerados no período.
  PERFORM SELECIONA_DADOS.          "Retorna as informações para gerar contabilização.

  CALL SCREEN 0100.
*  PERFORM VALIDA_RETORNO_ZMMT0093.  "Elimina da busca os materiais que já foram gerados.


****************************************************************
*SELECIONA_GERADOS
****************************************************************
FORM SELECIONA_GERADOS.

  DATA: VL_MATNR TYPE MARA-MATNR.

  WRITE P_MATNR-LOW TO VL_MATNR.
  SHIFT VL_MATNR LEFT DELETING LEADING '0'.

  SELECT *
    FROM ZMMT0093
    INTO TABLE TG_ZMMT0093
    WHERE WERKS IN P_WERKS
      AND MONAT IN P_MES
      AND GJARH IN P_ANO
      AND MATNR EQ VL_MATNR
      AND OBJ_KEY IS NOT NULL.

  IF TG_ZMMT0093[] IS NOT INITIAL.
    CLEAR: WG_ZMMT0093.

    LOOP AT TG_ZMMT0093 INTO WG_ZMMT0093.
      CLEAR: WG_SAIDA.

      WG_SAIDA-WERKS            = WG_ZMMT0093-WERKS.
      WG_SAIDA-MATNR            = WG_ZMMT0093-MATNR.
      WG_SAIDA-MONAT            = WG_ZMMT0093-MONAT.
      WG_SAIDA-GJARH            = WG_ZMMT0093-GJARH.

      PERFORM BUSCA_DESCRICAO USING     WG_ZMMT0093-WERKS
                                        WG_ZMMT0093-MATNR
                              CHANGING  WG_SAIDA-NAME1
                                        WG_SAIDA-KTEXT.

      WG_SAIDA-LBKUM            = WG_ZMMT0093-LBKUM.
      WG_SAIDA-CUSTO_MEDIO      = WG_ZMMT0093-CUSTO_MEDIO.
      WG_SAIDA-CUSTO_MEDIO_USD  = WG_ZMMT0093-CUSTO_MEDIO_USD.
      WG_SAIDA-TOTAL_EST_BRL    = WG_ZMMT0093-TOTAL_EST_BRL.
      WG_SAIDA-TOTAL_EST_USD    = WG_ZMMT0093-TOTAL_EST_USD.
      WG_SAIDA-CUSTO_MTM_BRL    = WG_ZMMT0093-CUSTO_MTM_BRL.
      WG_SAIDA-CUSTO_MTM_USD    = WG_ZMMT0093-CUSTO_MTM_USD.
      WG_SAIDA-TOTAL_MTM_BRL    = WG_ZMMT0093-TOTAL_MTM_BRL.
      WG_SAIDA-TOTAL_MTM_USD    = WG_ZMMT0093-TOTAL_MTM_USD.
      WG_SAIDA-VARIACAO_BRL     = WG_ZMMT0093-VARIACAO_BRL.
      WG_SAIDA-VARIACAO_USD     = WG_ZMMT0093-VARIACAO_USD.
      WG_SAIDA-OBJ_KEY          = WG_ZMMT0093-OBJ_KEY.

      PERFORM PESQUISA_DOC_ZIB USING   WG_SAIDA-OBJ_KEY
         CHANGING WG_SAIDA-STATUS WG_SAIDA-DOC_CONTABIL WG_SAIDA-ICON.

      IF ( WG_ZMMT0093-ESTORNAR IS NOT INITIAL ).
        WG_SAIDA-DOC_ESTORNO = WG_SAIDA-DOC_CONTABIL.
        CLEAR WG_SAIDA-DOC_CONTABIL.
      ENDIF.

      APPEND WG_SAIDA TO TG_SAIDA_AUX[].

    ENDLOOP.


  ENDIF.

ENDFORM.


**************************************************************
*VALIDA_RETORNO_ZMMT0093.
****************************************************************
FORM VALIDA_RETORNO_ZMMT0093.

  IF TG_ZMMT0093[] IS NOT INITIAL.
    LOOP AT TG_ZMMT0093[] INTO DATA(WL_ZMMT0093_AUX).
      DELETE P_WERKS[] WHERE LOW EQ WL_ZMMT0093_AUX-WERKS.

    ENDLOOP.
  ENDIF.

ENDFORM.

****************************************************************
*SELECIONA_DADOS
****************************************************************
FORM SELECIONA_DADOS.
  DATA: VL_MESANO(6)     TYPE C,
        VL_DIAMESANO(10) TYPE C,
        FATOR_NEGATIVO   TYPE P VALUE -1,
        VL_GDATU_1       TYPE TCURR-GDATU,
        VL_GDATU_2       TYPE TCURR-GDATU,
        VL_DAY_IN        LIKE SY-DATUM,
        VL_LAST_DAY      LIKE SY-DATUM.

  "Retorna informações da transação ZCO0016
  PERFORM BUSCA_DADOS_ZCOR011.

  IF TG_SAIDA_ZCO0016[] IS NOT INITIAL.

    DELETE TG_SAIDA_ZCO0016[] WHERE LBKUM = 0.

    CHECK ( TG_SAIDA_ZCO0016[] IS NOT INITIAL ).

    CONCATENATE P_MES-LOW+1(2) P_ANO-LOW INTO VL_MESANO.
    CONCATENATE P_ANO-LOW P_MES-LOW+1(2) '01' INTO VL_DIAMESANO.
    VL_DAY_IN = VL_DIAMESANO.

    SELECT WERKS NAME1
      FROM T001W
      INTO TABLE TG_T001W
      FOR ALL ENTRIES IN TG_SAIDA_ZCO0016
      WHERE WERKS EQ TG_SAIDA_ZCO0016-WERKS.

    SELECT MATNR MAKTX
      FROM MAKT
      INTO TABLE TG_MAKT
      FOR ALL ENTRIES IN TG_SAIDA_ZCO0016
      WHERE MATNR EQ TG_SAIDA_ZCO0016-MATNR
        AND SPRAS EQ 'PT'.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = VL_DAY_IN
      IMPORTING
        LAST_DAY_OF_MONTH = VL_LAST_DAY
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.

    VG_DIA_FIM = VL_LAST_DAY.

    CONCATENATE  VL_LAST_DAY+6(2) VL_LAST_DAY+4(2) VL_LAST_DAY+0(4) INTO VL_DAY_IN.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = VL_DAY_IN
      IMPORTING
        OUTPUT = VL_GDATU_1.

    SELECT SINGLE *
      FROM TCURR
      INTO WG_TCURR
      WHERE KURST EQ 'B'
        AND FCURR EQ 'USD'
        AND TCURR EQ 'BRL'
        AND GDATU EQ VL_GDATU_1.

    LOOP AT TG_SAIDA_ZCO0016 INTO DATA(WL_SAIDA_ZCO0016).

      SELECT *
        FROM ZFIT0049
        INTO TABLE @DATA(TL_ZFIT0049)
        WHERE ME_ANO EQ @VL_MESANO
          AND TIPO   IN ('SP','PR')
          AND MATNR  EQ @WL_SAIDA_ZCO0016-MATNR
        ORDER BY DT_REFERENCIA DESCENDING.

      IF TL_ZFIT0049[] IS NOT INITIAL.
        READ TABLE TL_ZFIT0049 INTO DATA(WL_ZFIT0049) INDEX 1.
        APPEND WL_ZFIT0049 TO TG_ZFIT0049[].

        CLEAR: WL_ZFIT0049, TL_ZFIT0049[].
      ENDIF.

      "Monta Saída:
      IF WG_TCURR IS NOT INITIAL.
        CLEAR: WG_T001W, WG_MAKT, WG_SAIDA.

        READ TABLE TG_T001W     INTO WG_T001W     WITH KEY WERKS = WL_SAIDA_ZCO0016-WERKS.
        READ TABLE TG_MAKT      INTO WG_MAKT      WITH KEY MATNR = WL_SAIDA_ZCO0016-MATNR.
        SHIFT WL_SAIDA_ZCO0016-MATNR LEFT DELETING LEADING '0'.
        READ TABLE TG_ZFIT0049  INTO WL_ZFIT0049  INDEX 1.

        WG_SAIDA-STATUS           = ''.
        WG_SAIDA-ICON(4)          = ICON_GENERATE.
        WG_SAIDA-WERKS            = WL_SAIDA_ZCO0016-WERKS.
        WG_SAIDA-NAME1            = WG_T001W-NAME1.
        WG_SAIDA-MATNR            = WL_SAIDA_ZCO0016-MATNR.
        WG_SAIDA-KTEXT            = WG_MAKT-MAKTX.
        WG_SAIDA-MONAT            = P_MES-LOW.
        WG_SAIDA-GJARH            = P_ANO-LOW.
        WG_SAIDA-LBKUM            = WL_SAIDA_ZCO0016-LBKUM.
        WG_SAIDA-CUSTO_MEDIO      = ( WL_SAIDA_ZCO0016-SALK3 / WL_SAIDA_ZCO0016-LBKUM ).
        WG_SAIDA-CUSTO_MEDIO_USD  = ( WL_SAIDA_ZCO0016-SALK3_U / WL_SAIDA_ZCO0016-LBKUM ).
        WG_SAIDA-TOTAL_EST_BRL    = WL_SAIDA_ZCO0016-SALK3.
        WG_SAIDA-TOTAL_EST_USD    = WL_SAIDA_ZCO0016-SALK3_U.
        WG_SAIDA-CUSTO_MTM_BRL    = WL_ZFIT0049-VALOR_COTACAO.
        WG_SAIDA-CUSTO_MTM_USD    = ( WL_ZFIT0049-VALOR_COTACAO / WG_TCURR-UKURS ).
        WG_SAIDA-TOTAL_MTM_BRL    = ( WL_SAIDA_ZCO0016-LBKUM * WG_SAIDA-CUSTO_MTM_BRL ).
        WG_SAIDA-TOTAL_MTM_USD    = ( WL_SAIDA_ZCO0016-LBKUM * WG_SAIDA-CUSTO_MTM_USD ).

        WG_SAIDA-VARIACAO_BRL = ( WG_SAIDA-TOTAL_MTM_BRL - WG_SAIDA-TOTAL_EST_BRL ).
        WG_SAIDA-VARIACAO_USD = ( WG_SAIDA-TOTAL_MTM_USD - WG_SAIDA-TOTAL_EST_USD ).

        "Calcula variação entre Valor Total Estoque MTM e Valor Total Estoque 'BRL'
        IF ( WG_SAIDA-TOTAL_MTM_BRL < WG_SAIDA-TOTAL_EST_BRL ).
          WG_SAIDA-VARIACAO_BRL = ( WG_SAIDA-TOTAL_MTM_BRL - WG_SAIDA-TOTAL_EST_BRL ) * FATOR_NEGATIVO.
        ELSEIF ( WG_SAIDA-TOTAL_MTM_BRL > WG_SAIDA-TOTAL_EST_BRL ).
          WG_SAIDA-VARIACAO_BRL = ( WG_SAIDA-TOTAL_MTM_BRL - WG_SAIDA-TOTAL_EST_BRL ).
        ELSEIF ( WG_SAIDA-TOTAL_MTM_BRL EQ WG_SAIDA-TOTAL_EST_BRL ).
          WG_SAIDA-VARIACAO_BRL = 0.
        ENDIF.

        "Calcula variação entre Valor Total Estoque MTM e Valor Total Estoque 'USD'
        IF ( WG_SAIDA-TOTAL_MTM_USD < WG_SAIDA-TOTAL_EST_USD ).
          WG_SAIDA-VARIACAO_USD = ( WG_SAIDA-TOTAL_MTM_USD - WG_SAIDA-TOTAL_EST_USD ) * FATOR_NEGATIVO.
        ELSEIF ( WG_SAIDA-TOTAL_MTM_USD > WG_SAIDA-TOTAL_EST_USD ).
          WG_SAIDA-VARIACAO_USD = ( WG_SAIDA-TOTAL_MTM_USD - WG_SAIDA-TOTAL_EST_USD ).
        ELSEIF ( WG_SAIDA-TOTAL_MTM_USD EQ WG_SAIDA-TOTAL_EST_USD ).
          WG_SAIDA-VARIACAO_USD = 0.
        ENDIF.

*            WG_SAIDA-DOC_CONTABIL     =

        APPEND WG_SAIDA TO TG_SAIDA[].

      ENDIF.

    ENDLOOP.

    SORT TG_ZFIT0049 BY MATNR.
    SORT TG_SAIDA[] BY MATNR WERKS.

  ENDIF.

ENDFORM.

****************************************************************
*BUSCA_DADOS_ZCOR011
****************************************************************
FORM BUSCA_DADOS_ZCOR011.

  DATA: IT_RSPARAMS TYPE TABLE OF RSPARAMS,
        WA_RSPARAMS TYPE RSPARAMS.

  CLEAR IT_RSPARAMS[].

  "FILIAL
  LOOP AT P_WERKS.
    WA_RSPARAMS-SELNAME = 'S_WERKS'.
    WA_RSPARAMS-KIND    = 'S'.
    WA_RSPARAMS-SIGN    = 'I'.
    WA_RSPARAMS-OPTION  = P_WERKS-OPTION.
    WA_RSPARAMS-LOW     = P_WERKS-LOW.
    WA_RSPARAMS-HIGH    = P_WERKS-HIGH.
    APPEND WA_RSPARAMS TO IT_RSPARAMS.
  ENDLOOP.

  "MATERIAL
  LOOP AT P_MATNR.
    WA_RSPARAMS-SELNAME = 'R_MATNR'.
    WA_RSPARAMS-KIND    = 'S'.
    WA_RSPARAMS-SIGN    = 'I'.
    WA_RSPARAMS-OPTION  = P_MATNR-OPTION.
    WA_RSPARAMS-LOW     = P_MATNR-LOW.
    WA_RSPARAMS-HIGH    = P_MATNR-HIGH.
    APPEND WA_RSPARAMS TO IT_RSPARAMS.
  ENDLOOP.

  "MES
  LOOP AT P_MES.
    WA_RSPARAMS-SELNAME = 'P_POPER'.
    WA_RSPARAMS-KIND    = 'S'.
    WA_RSPARAMS-SIGN    = 'I'.
    WA_RSPARAMS-OPTION  = P_MES-OPTION.
    WA_RSPARAMS-LOW     = P_MES-LOW.
    WA_RSPARAMS-HIGH    = P_MES-HIGH.
    APPEND WA_RSPARAMS TO IT_RSPARAMS.
  ENDLOOP.

  "ANO
  LOOP AT P_ANO.
    WA_RSPARAMS-SELNAME = 'P_BDATJ'.
    WA_RSPARAMS-KIND    = 'S'.
    WA_RSPARAMS-SIGN    = 'I'.
    WA_RSPARAMS-OPTION  = P_ANO-OPTION.
    WA_RSPARAMS-LOW     = P_ANO-LOW.
    WA_RSPARAMS-HIGH    = P_ANO-HIGH.
    APPEND WA_RSPARAMS TO IT_RSPARAMS.
  ENDLOOP.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = SY-TABIX
      TEXT       = 'Extraindo dados da ZCO0016...'.

  PERFORM F_PREPARE_RUN_TIME_INFO.

  SUBMIT ZCOR011
    WITH SELECTION-TABLE IT_RSPARAMS
    WITH R_ST_D EQ 'X'
  AND RETURN.

  PERFORM F_GET_RUNTIME_INFO.

  IF <T_DATA> IS ASSIGNED.
    LOOP AT <T_DATA> ASSIGNING <W_DATA>.
      CLEAR: TG_SAIDA_ZCO0016.
      MOVE-CORRESPONDING <W_DATA> TO TG_SAIDA_ZCO0016.
      APPEND TG_SAIDA_ZCO0016.
    ENDLOOP.
  ENDIF.

ENDFORM.

****************************************************************
*F_PREPARE_RUN_TIME_INFO
****************************************************************
FORM F_PREPARE_RUN_TIME_INFO.

  IF <T_DATA> IS ASSIGNED.
    CLEAR: <T_DATA>[].
  ENDIF.

  IF <T_DATA_LINE>  IS ASSIGNED.
    CLEAR: <T_DATA_LINE>[].
  ENDIF.

  IF <T_DATA> IS ASSIGNED.
    CLEAR: <T_DATA>.
  ENDIF.

  IF <T_DATA_LINE> IS ASSIGNED.
    CLEAR: <T_DATA_LINE>.
  ENDIF.

  FREE: L_DATA, L_DATA_LINE, L_DATA_DESCR, L_DATA_LINE_DESCR.

  CL_SALV_BS_RUNTIME_INFO=>SET( EXPORTING DISPLAY  = ABAP_FALSE
                                          METADATA = ABAP_FALSE
                                          DATA     = ABAP_TRUE ).

ENDFORM.

****************************************************************
*F_GET_RUNTIME_INFO
****************************************************************
FORM F_GET_RUNTIME_INFO.

  TRY.
      CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF(
      IMPORTING R_DATA_DESCR  = L_DATA_DESCR
                R_DATA_LINE_DESCR = L_DATA_LINE_DESCR ).

      CHECK ( L_DATA_DESCR IS NOT INITIAL ) OR ( L_DATA_LINE_DESCR IS  NOT INITIAL ).

      CREATE DATA L_DATA      TYPE HANDLE  L_DATA_DESCR.
      CREATE DATA L_DATA_LINE TYPE HANDLE  L_DATA_LINE_DESCR.

      ASSIGN L_DATA->* TO <T_DATA>.
      ASSIGN L_DATA_LINE->* TO <T_DATA_LINE>.

      CL_SALV_BS_RUNTIME_INFO=>GET_DATA( IMPORTING T_DATA  = <T_DATA>
                                                   T_DATA_LINE = <T_DATA_LINE> ).
    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
  ENDTRY.

  CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ).

  ASSIGN L_DATA->*        TO <W_DATA>.
  ASSIGN L_DATA_LINE->*   TO <W_DATA_LINE>.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICAO
*&---------------------------------------------------------------------*
FORM BUSCA_DESCRICAO  USING    P_WG_ZMMT0093_WERKS
                               P_WG_ZMMT0093_MATNR
                      CHANGING P_WG_SAIDA_NAME1
                               P_WG_SAIDA_KTEXT.

  DATA: VL_MATNR TYPE MAKT-MATNR.
  VL_MATNR = P_WG_ZMMT0093_MATNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VL_MATNR
    IMPORTING
      OUTPUT = VL_MATNR.

  SELECT SINGLE NAME1
    FROM T001W
    INTO (P_WG_SAIDA_NAME1)
    WHERE WERKS EQ P_WG_ZMMT0093_WERKS.

  SELECT SINGLE MAKTX
    FROM MAKT
    INTO (P_WG_SAIDA_KTEXT)
    WHERE MATNR EQ VL_MATNR
      AND SPRAS EQ 'PT'.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'ST001'.
  SET TITLEBAR 'T001'.

  PERFORM ORGANIZA_SAIDA.
  PERFORM CRIA_ALV.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'GERAR' OR 'ESTORNAR'.

      CHECK WA_ALV_0001 IS NOT INITIAL.

      CALL METHOD WA_ALV_0001->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTED_ROWS.

      CLEAR: TG_SAIDA_GERA[].

      LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

        READ TABLE TG_SAIDA INTO WG_SAIDA INDEX WA_SELECTED_ROWS.

        IF ( WG_SAIDA-STATUS EQ ICON_CHECKED ).
          MESSAGE TEXT-005 TYPE 'I'.
          CLEAR: WG_SAIDA, WA_SELECTED_ROWS.
        ELSE.
          APPEND WG_SAIDA TO TG_SAIDA_GERA[].
        ENDIF.

      ENDLOOP.

      CHECK TG_SAIDA_GERA[] IS NOT INITIAL.
      CLEAR: WG_SAIDA.
      "Gerar documento contábil
      PERFORM GERAR_CONTABIL.

      LOOP AT TG_SAIDA_GERA INTO DATA(WG_SAIDA_GERA) WHERE STATUS EQ 'A'.

        LOOP AT TG_SAIDA ASSIGNING <WG_SAIDA> WHERE WERKS EQ WG_SAIDA_GERA-WERKS.

          <WG_SAIDA>-STATUS       = WG_SAIDA_GERA-STATUS.
          <WG_SAIDA>-ICON         = WG_SAIDA_GERA-ICON.
          <WG_SAIDA>-DOC_CONTABIL = WG_SAIDA_GERA-DOC_CONTABIL.

        ENDLOOP.

      ENDLOOP.

      CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.

    WHEN 'REFRESH'.

      LOOP AT TG_SAIDA[] ASSIGNING <WG_SAIDA> WHERE STATUS NE ''.

        SELECT SINGLE OBJ_KEY
          FROM ZMMT0093
          INTO @DATA(VL_OBJ_KEY)
          WHERE WERKS EQ @<WG_SAIDA>-WERKS
            AND MONAT EQ @<WG_SAIDA>-MONAT
            AND GJARH EQ @<WG_SAIDA>-GJARH
            AND MATNR EQ @<WG_SAIDA>-MATNR.

        IF <WG_SAIDA>-DOC_CONTABIL IS NOT INITIAL.

          PERFORM PESQUISA_DOC_ZIB USING    VL_OBJ_KEY
                                 CHANGING <WG_SAIDA>-STATUS <WG_SAIDA>-DOC_CONTABIL <WG_SAIDA>-ICON.

        ELSEIF <WG_SAIDA>-DOC_ESTORNO IS NOT INITIAL.

          PERFORM PESQUISA_DOC_ZIB USING    VL_OBJ_KEY
                                 CHANGING <WG_SAIDA>-STATUS <WG_SAIDA>-DOC_ESTORNO <WG_SAIDA>-ICON.
        ENDIF.

      ENDLOOP.

      CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.

  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_SAIDA
*&---------------------------------------------------------------------*
FORM ORGANIZA_SAIDA .

  IF ( TG_SAIDA_AUX[] IS NOT INITIAL ) AND ( TG_SAIDA[] IS NOT INITIAL ).

    LOOP AT TG_SAIDA[] ASSIGNING FIELD-SYMBOL(<W_SAIDA>).

      READ TABLE TG_SAIDA_AUX[] INTO DATA(WL_SAIDA_AUX) WITH KEY WERKS = <W_SAIDA>-WERKS
                                                                 MATNR = <W_SAIDA>-MATNR.
      IF ( SY-SUBRC = 0 ).

        <W_SAIDA>-STATUS        = WL_SAIDA_AUX-STATUS.
        <W_SAIDA>-ICON          = WL_SAIDA_AUX-ICON.
        <W_SAIDA>-DOC_CONTABIL  = WL_SAIDA_AUX-DOC_CONTABIL.
        <W_SAIDA>-DOC_ESTORNO   = WL_SAIDA_AUX-DOC_ESTORNO.
        <W_SAIDA>-OBJ_KEY       = WL_SAIDA_AUX-OBJ_KEY.

      ENDIF.

    ENDLOOP.

  ENDIF.

  IF TG_SAIDA[] IS INITIAL.
    MESSAGE TEXT-003 TYPE 'I'.
    EXIT.
  ENDIF.

  SORT TG_SAIDA[] BY MATNR WERKS.
  CLEAR: TG_SAIDA_AUX[], WL_SAIDA_AUX.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV
*&---------------------------------------------------------------------*
FORM CRIA_ALV.

  PERFORM CRIA_CALATOGO_0100.
  PERFORM CRIA_ALV_0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_CALATOGO_0100
*&---------------------------------------------------------------------*
FORM CRIA_CALATOGO_0100.

  REFRESH: IT_FCAT[].

  PERFORM ALV_CATALOG_0100  USING :

'TG_SAIDA'        'ICON'                'Status'                      '05'    'X' ''  ''      'MATN1' 'C' ,
'TG_SAIDA'        'WERKS'               'Filial'                      '06'    ''  ''  ''      ''      '' ,
'TG_SAIDA'        'NAME1'               'Nome da Filial'              '25'    ''  ''  'C500'  ''      '' ,
'TG_SAIDA'        'MATNR'               'Material'                    '08'    ''  ''  ''      ''      '' ,
'TG_SAIDA'        'KTEXT'               'Desc.material'               '15'    ''  ''  'C500'  ''      '' ,
'TG_SAIDA'        'LBKUM'               'Vol.Estoque'                 '10'    ''  'X' ''      ''      '' ,
'TG_SAIDA'        'CUSTO_MEDIO'         'Custo Médio BRL'             '13'    ''  ''  ''      ''      '' ,
'TG_SAIDA'        'CUSTO_MEDIO_USD'     'Custo Médio USD'             '14'    ''  ''  ''      ''      '' ,
'TG_SAIDA'        'TOTAL_EST_BRL'       'Total Estoque BRL'           '14'    ''  'X' ''      ''      '' ,
'TG_SAIDA'        'TOTAL_EST_USD'       'Total Estoque USD'           '16'    ''  'X' ''      ''      '' ,
'TG_SAIDA'        'CUSTO_MTM_BRL'       'Custo MTM BRL'               '13'    ''  ''  ''      ''      'C',
'TG_SAIDA'        'CUSTO_MTM_USD'       'Custo MTM USD'               '13'    ''  ''  ''      ''      'C',
'TG_SAIDA'        'TOTAL_MTM_BRL'       'Estoque MTM BRL'             '15'    ''  'X' ''      ''      'C',
'TG_SAIDA'        'TOTAL_MTM_USD'       'Estoque MTM USD'             '15'    ''  'X' ''      ''      'C',
'TG_SAIDA'        'VARIACAO_BRL'        'Variação BRL'                '11'    ''  ''  'C500'  ''      'C',
'TG_SAIDA'        'VARIACAO_USD'        'Variação USD'                '11'    ''  ''  'C500'  ''      'C',
'TG_SAIDA'        'DOC_CONTABIL'        'Doc.Contábil'                '15'    ''  ''  ''      ''      'C',
'TG_SAIDA'        'DOC_ESTORNO'         'Doc.estorno'                 '15'    ''  ''  ''      ''      'C'.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_0100
*&---------------------------------------------------------------------*
FORM ALV_CATALOG_0100  USING   P_TABLE    TYPE C
                               P_CAMPO    TYPE C
                               P_DESC     TYPE C
                               P_TAM      TYPE C
                               P_ZERO     TYPE C
                               P_SUM      TYPE C
                               P_COR      TYPE C
                               P_CONVEXIT TYPE C
                               P_JUST     TYPE C.


  DATA: P_HOT   TYPE C,
        WL_FCAT TYPE LVC_S_FCAT,
        MARCA   TYPE C VALUE 'X'.

  IF P_TABLE EQ 'TG_SAIDA'.
    CASE P_CAMPO.
      WHEN 'DOC_CONTABIL' OR 'DOC_ESTORNO'.
        P_HOT = MARCA.
    ENDCASE.

  ENDIF.

  WL_FCAT-TABNAME   = P_TABLE.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-DO_SUM   =  P_SUM.
  WL_FCAT-EMPHASIZE = P_COR.
  WL_FCAT-CONVEXIT  = P_CONVEXIT.
  WL_FCAT-JUST      = P_JUST.

  APPEND WL_FCAT TO IT_FCAT.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_0100
*&---------------------------------------------------------------------*
FORM CRIA_ALV_0100.

  DATA: WA_EVENT_0001    TYPE REF TO LCL_EVENT_RECEIVER.

  DATA:   G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
          DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
          DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
          DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
          PICTURE            TYPE REF TO CL_GUI_PICTURE,
          DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
          TABLE_ELEMENT      TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN             TYPE REF TO CL_DD_AREA,
          TABLE_ELEMENT2     TYPE REF TO CL_DD_TABLE_ELEMENT,
          COLUMN_1           TYPE REF TO CL_DD_AREA,
          COLUMN_2           TYPE REF TO CL_DD_AREA,
          DG_HTML_CNTRL      TYPE REF TO CL_GUI_HTML_VIEWER.

  IF WA_CONTAINER_0001 IS INITIAL.

    CREATE OBJECT WA_CONTAINER_0001
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_0100'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = WA_CONTAINER_0001
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 13.

    CREATE OBJECT WA_ALV_0001
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    IF WA_EVENT_0001 IS INITIAL.
      CREATE OBJECT WA_EVENT_0001.
      SET HANDLER: WA_EVENT_0001->ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.
    ENDIF.

    WA_LAYOUT-CWIDTH_OPT = ' '.
    WA_LAYOUT-SEL_MODE   = 'A'.

    CALL METHOD WA_ALV_0001->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        IS_VARIANT      = GS_VARIANT_C
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FCAT
        IT_OUTTAB       = TG_SAIDA[].

    CALL METHOD WA_ALV_0001->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR WA_ALV_0001.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.

  ELSE.
    CALL METHOD WA_ALV_0001->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_REPORT
*&---------------------------------------------------------------------*
FORM Z_HOTSPOT_REPORT USING    P_E_ROW_ID    TYPE  LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.


  IF P_E_ROW_ID-ROWTYPE IS INITIAL." ( SY-SUBRC EQ 0 ).
    CASE P_E_COLUMN_ID.

      WHEN: 'DOC_CONTABIL'.

        READ TABLE TG_SAIDA[] INTO WG_SAIDA INDEX P_E_ROW_ID.

        PERFORM ATUALIZA_DOC_CONTABIL USING WG_SAIDA-DOC_CONTABIL.

      WHEN: 'DOC_ESTORNO'.

        READ TABLE TG_SAIDA[] INTO WG_SAIDA INDEX P_E_ROW_ID.

        PERFORM ATUALIZA_DOC_CONTABIL USING WG_SAIDA-DOC_ESTORNO.

    ENDCASE.
  ELSE.
    MESSAGE TEXT-002 TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_SCREEN INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERAR_CONTABIL
*&---------------------------------------------------------------------*
FORM GERAR_CONTABIL.
  DATA: VL_SETID      TYPE SETHIER-SETID,
        TL_SET_VALUES TYPE TABLE OF RGSBV,
        VL_BSCHL      TYPE ZIB_CONTABIL-BSCHL,
        VL_HKONT      TYPE ZIB_CONTABIL-HKONT,
        VL_VALOR_BRL  TYPE ZIB_CONTABIL-WRBTR,
        VL_VALOR_USD  TYPE ZIB_CONTABIL-WRBTR,
        VL_MOEDA_BRL  TYPE C,
        VL_MOEDA_USD  TYPE C.

  CLEAR: WG_ZMMT0093, WG_ZIB_LOG, WG_ZIB_ERR.

  LOOP AT TG_SAIDA_GERA INTO DATA(WL_SAIDA_GERA).

    "Verifica o documento, caso já tenha sido gerado com sucesso, bloquear nova chave.
    "Se documento foi gerado com erro e corrigido, gerar nova chave.
    PERFORM VALIDA_CONTABIL USING WL_SAIDA_GERA-WERKS
                                  WL_SAIDA_GERA-MATNR
                                  WL_SAIDA_GERA-MONAT
                                  WL_SAIDA_GERA-GJARH.

    "Se documento já estiver gerado e SY-UCOMM = 'GERAR' apresentar msg de erro.
    IF ( WG_ZIB_LOG IS NOT INITIAL ) AND ( SY-UCOMM = 'GERAR' ).

      CLEAR: WG_ZIB_LOG, WG_ZIB_ERR, WL_SAIDA_GERA.
      MESSAGE TEXT-001 TYPE 'I'.
      EXIT.

    ELSE.

      CLEAR: V_SNUM.
      PERFORM OBTEM_PROXIMO.

      MOVE-CORRESPONDING WL_SAIDA_GERA TO WG_ZMMT0093.

      WG_ZMMT0093-OBJ_KEY = |ZMM137{ V_SNUM }{ P_ANO-LOW }|.

      IF ( SY-UCOMM EQ 'ESTORNAR' ).
        WG_ZMMT0093-ESTORNAR = ABAP_TRUE.
      ELSEIF ( SY-UCOMM EQ 'GERAR' ).
        WG_ZMMT0093-ESTORNAR = ''.
      ENDIF.

      "Salva documento gerado na tabela ZMMT0093.
      MODIFY ZMMT0093 FROM WG_ZMMT0093.
      COMMIT WORK.


      "BUSCANDO CONTAS NO SET
      CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
        EXPORTING
          SHORTNAME = 'MAGGI_MTM_ESTOQUE'
        IMPORTING
          NEW_SETID = VL_SETID.
      IF ( SY-SUBRC EQ 0 ).
        CALL FUNCTION 'G_SET_FETCH'
          EXPORTING
            SETNR           = VL_SETID
          TABLES
            SET_LINES_BASIC = TL_SET_VALUES.
      ELSE.
        MESSAGE 'Contas não encontradas no set' TYPE 'E'.
      ENDIF.

      IF ( WG_ZMMT0093-VARIACAO_BRL <> 0 ) AND ( WG_ZMMT0093-VARIACAO_BRL > 0 ).
        "Se valor for positivo, lançar nas duas contas abaixo de acordo com o SET.
        READ TABLE TL_SET_VALUES INTO DATA(WL_SET) WITH KEY TITLE = '50 (+)'.

        IF ( SY-UCOMM EQ 'GERAR' ).
          VL_BSCHL = 50.
        ELSEIF ( SY-UCOMM EQ 'ESTORNAR' ).
          VL_BSCHL = 40.
        ENDIF.

        VL_HKONT = WL_SET-FROM.
        VL_VALOR_BRL = WG_ZMMT0093-VARIACAO_BRL.
        VL_VALOR_USD = WG_ZMMT0093-VARIACAO_USD.

        PERFORM GRAVA_ZIB USING VL_BSCHL VL_HKONT VL_VALOR_BRL VL_VALOR_USD.

        READ TABLE TL_SET_VALUES INTO WL_SET WITH KEY TITLE = '40 (+)'.

        IF ( SY-UCOMM EQ 'GERAR' ).
          VL_BSCHL = 40.
        ELSEIF ( SY-UCOMM EQ 'ESTORNAR' ).
          VL_BSCHL = 50.
        ENDIF.

        VL_HKONT = WL_SET-FROM.
        VL_VALOR_BRL = WG_ZMMT0093-VARIACAO_BRL.
        VL_VALOR_USD = WG_ZMMT0093-VARIACAO_USD.

        PERFORM GRAVA_ZIB USING VL_BSCHL VL_HKONT VL_VALOR_BRL VL_VALOR_USD.

        "Após gravar as linhas na ZIB_CONTABIL, pesquisa nas tabelas de log e erro
        IF ( SY-SUBRC = 0 ).

*          PERFORM PESQUISA_DOC_ZIB USING    WG_ZMMT0093-OBJ_KEY
*                                   CHANGING WL_SAIDA_GERA-STATUS WL_SAIDA_GERA-DOC_CONTABIL WL_SAIDA_GERA-ICON.

          WL_SAIDA_GERA-STATUS = 'A'.
          WL_SAIDA_GERA-ICON   = ICON_TIME.

          IF SY-UCOMM EQ 'GERAR'.
            WL_SAIDA_GERA-DOC_CONTABIL = ICON_TIME.
            CLEAR WL_SAIDA_GERA-DOC_ESTORNO.
          ELSE.
            WL_SAIDA_GERA-DOC_ESTORNO = ICON_TIME.
            CLEAR WL_SAIDA_GERA-DOC_CONTABIL.
          ENDIF.


        ENDIF.

      ELSEIF ( WG_ZMMT0093-VARIACAO_BRL <> 0 ) AND ( WG_ZMMT0093-VARIACAO_BRL < 0 ).
        "Se valor for positivo, lançar nas duas contas abaixo de acordo com o SET.
        READ TABLE TL_SET_VALUES INTO WL_SET WITH KEY TITLE = '50 (-)'.
        VL_BSCHL = 50.
        VL_HKONT = WL_SET-FROM.
        VL_VALOR_BRL = WG_ZMMT0093-VARIACAO_BRL.
        VL_VALOR_USD = WG_ZMMT0093-VARIACAO_USD.

        PERFORM GRAVA_ZIB USING VL_BSCHL VL_HKONT VL_VALOR_BRL VL_VALOR_USD.

        READ TABLE TL_SET_VALUES INTO WL_SET WITH KEY TITLE = '40 (-)'.
        VL_BSCHL = 40.
        VL_HKONT = WL_SET-FROM.
        VL_VALOR_BRL = WG_ZMMT0093-VARIACAO_BRL.
        VL_VALOR_USD = WG_ZMMT0093-VARIACAO_USD.

        PERFORM GRAVA_ZIB USING VL_BSCHL VL_HKONT VL_VALOR_BRL VL_VALOR_USD.

        "Após gravar as linhas na ZIB_CONTABIL, pesquisa nas tabelas de log e erro
        IF ( SY-SUBRC = 0 ).

*          PERFORM PESQUISA_DOC_ZIB USING    WG_ZMMT0093-OBJ_KEY
*                                   CHANGING WL_SAIDA_GERA-STATUS WL_SAIDA_GERA-DOC_CONTABIL WL_SAIDA_GERA-ICON.

          WL_SAIDA_GERA-STATUS = 'A'.
          WL_SAIDA_GERA-ICON   = ICON_TIME.
          WL_SAIDA_GERA-DOC_CONTABIL = ICON_TIME.

        ENDIF.

      ENDIF.
    ENDIF.

    MODIFY TG_SAIDA_GERA FROM WL_SAIDA_GERA.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
FORM OBTEM_PROXIMO.

  DATA: VL_NUMBER(10) TYPE C.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZMM137'
    IMPORTING
      NUMBER                  = VL_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    WRITE VL_NUMBER TO V_SNUM.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  VALIDA_CONTABIL
*&---------------------------------------------------------------------*
FORM VALIDA_CONTABIL  USING    P_WL_SAIDA_GERA_WERKS
                               P_WL_SAIDA_GERA_MATNR
                               P_WL_SAIDA_GERA_MONAT
                               P_WL_SAIDA_GERA_GJARH.


  SELECT SINGLE *
    FROM ZMMT0093
    INTO @DATA(WL_ZMMT0093)
    WHERE WERKS EQ @P_WL_SAIDA_GERA_WERKS
      AND MATNR EQ @P_WL_SAIDA_GERA_MATNR
      AND MONAT EQ @P_WL_SAIDA_GERA_MONAT
      AND GJARH EQ @P_WL_SAIDA_GERA_GJARH.

  IF ( WL_ZMMT0093 IS INITIAL ).

    SELECT SINGLE OBJ_KEY BELNR
      FROM ZIB_CONTABIL_CHV
      INTO WG_ZIB_LOG
      WHERE OBJ_KEY EQ WL_ZMMT0093-OBJ_KEY.

  ELSEIF ( WL_ZMMT0093 IS INITIAL ) AND ( SY-UCOMM = 'ESTORNAR' ).

    MESSAGE TEXT-004 TYPE 'I'.
    EXIT.

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GRAVA_ZIB
*&---------------------------------------------------------------------*
FORM GRAVA_ZIB  USING    P_VL_BSCHL
                         P_VL_HKONT
                         P_VL_VALOR_BRL  TYPE ZIB_CONTABIL-WRBTR
                         P_VL_VALOR_USD  TYPE ZIB_CONTABIL-WRBTR.

  DATA: VL_BUDAT_ZIB(10) TYPE C,
        SEQITEM          TYPE ZIB_CONTABIL-SEQITEM.

  VL_BUDAT_ZIB = |{ VG_DIA_FIM+6(2) }.{ VG_DIA_FIM+4(2) }.{ VG_DIA_FIM+0(4) }|.

  SELECT OBJ_KEY, SEQITEM
        FROM ZIB_CONTABIL
        INTO TABLE @DATA(TL_ZIB)
        WHERE OBJ_KEY EQ @WG_ZMMT0093-OBJ_KEY.

  IF ( TL_ZIB IS NOT INITIAL ).

    DESCRIBE TABLE TL_ZIB LINES DATA(COUNT_ZIB).
    READ TABLE TL_ZIB INTO DATA(WL_ZIB) INDEX COUNT_ZIB.
    IF ( WL_ZIB IS NOT INITIAL ).
      SEQITEM = WL_ZIB-SEQITEM + 1.
    ENDIF.
  ELSE.
    SEQITEM = 000001.
  ENDIF.

  WG_ZIB_CONT-OBJ_KEY       = WG_ZMMT0093-OBJ_KEY.
  WG_ZIB_CONT-SEQITEM       = SEQITEM.
  WG_ZIB_CONT-BSCHL         = P_VL_BSCHL.
  WG_ZIB_CONT-GSBER         = WG_ZMMT0093-WERKS.
  WG_ZIB_CONT-BUKRS         = P_BUKRS-LOW.
  WG_ZIB_CONT-INTERFACE     = 0.
  WG_ZIB_CONT-BKTXT         = 'MTM Estoque Filiais'.
  WG_ZIB_CONT-BLDAT         = VL_BUDAT_ZIB.
  WG_ZIB_CONT-BUDAT         = VL_BUDAT_ZIB.
  WG_ZIB_CONT-GJAHR         = WG_ZMMT0093-GJARH.
  WG_ZIB_CONT-MONAT         = WG_ZMMT0093-MONAT.
  WG_ZIB_CONT-BLART         = 'LM'.
  WG_ZIB_CONT-HKONT         = P_VL_HKONT.
  WG_ZIB_CONT-WRBTR         = P_VL_VALOR_BRL.
  WG_ZIB_CONT-WAERS         = 'BRL'.
  WG_ZIB_CONT-SGTXT         = 'Variação MTM Estoque Filiais'.
  WG_ZIB_CONT-WAERS_I       = 'BRL'.
  WG_ZIB_CONT-DMBTR         = P_VL_VALOR_BRL.
  WG_ZIB_CONT-WAERS_F       = 'USD'.
  WG_ZIB_CONT-DMBE2         = P_VL_VALOR_USD.
  WG_ZIB_CONT-RG_ATUALIZADO = 'N'.

  MODIFY ZIB_CONTABIL FROM WG_ZIB_CONT.
  COMMIT WORK.

  CLEAR: WG_ZIB_CONT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_DOC_ZIB
*&---------------------------------------------------------------------*
FORM PESQUISA_DOC_ZIB USING P_WG_ZMMT0093_OBJ_KEY CHANGING P_WL_SAIDA_GERA_STATUS
                                                           P_WL_SAIDA_GERA_DOC_CONTABIL
                                                           P_WL_SAIDA_GERA_ICON.


  DATA: VL_RETURN_ZIB TYPE C,
        VL_COUNT_ZIB  TYPE P DECIMALS 2.

  CLEAR: VL_RETURN_ZIB, VL_COUNT_ZIB.

*  WHILE VL_RETURN_ZIB IS INITIAL .

  SELECT SINGLE OBJ_KEY, BELNR
    FROM ZIB_CONTABIL_CHV
    INTO @DATA(WL_ZIB_DOC)
    WHERE OBJ_KEY EQ @P_WG_ZMMT0093_OBJ_KEY.

  IF ( WL_ZIB_DOC IS INITIAL ).

    SELECT SINGLE OBJ_KEY, MESSAGE, MESSAGE_V1
      FROM ZIB_CONTABIL_ERR
      INTO @DATA(WL_ZIB_ERR)
      WHERE OBJ_KEY EQ @P_WG_ZMMT0093_OBJ_KEY
        AND ID      NE 'RW'.

    IF ( SY-SUBRC NE 0 ).

      SELECT SINGLE OBJ_KEY MESSAGE MESSAGE_V1
        FROM ZIB_CONTABIL_ERR
        INTO WL_ZIB_ERR
        WHERE OBJ_KEY EQ P_WG_ZMMT0093_OBJ_KEY
          AND NR_ITEM EQ 2.

    ENDIF.

    IF ( WL_ZIB_ERR IS NOT INITIAL ).

      P_WL_SAIDA_GERA_STATUS = 'E'.
      P_WL_SAIDA_GERA_DOC_CONTABIL = WL_ZIB_ERR-MESSAGE.
      P_WL_SAIDA_GERA_ICON = ICON_CANCEL.

    ENDIF.

  ELSE.

    P_WL_SAIDA_GERA_STATUS = 'G'.
    P_WL_SAIDA_GERA_DOC_CONTABIL = WL_ZIB_DOC-BELNR.
    P_WL_SAIDA_GERA_ICON = ICON_CHECKED.

  ENDIF.

*    IF ( WL_ZIB_DOC IS INITIAL ) AND ( WL_ZIB_ERR IS INITIAL ).
*      "Enquanto não obtiver retorno do documento...
*      VL_COUNT_ZIB = VL_COUNT_ZIB + 0000000000001.
*
*      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*        EXPORTING
*          PERCENTAGE = VL_COUNT_ZIB
*          TEXT       = 'Gerando Doc Contabil...'.
*    ELSE.
*      "Quando houver retorno, parar de consultar e exibir documento...
*      VL_RETURN_ZIB = ABAP_TRUE.
*      MESSAGE 'Documento(s) gerado(s)!' TYPE 'S'.
*
*    ENDIF.

*  ENDWHILE.




ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DOC_CONTABIL
*&---------------------------------------------------------------------*
FORM ATUALIZA_DOC_CONTABIL  USING    P_WG_SAIDA_DOC_CONTABIL.

  DATA: VL_COUNT_DOC TYPE I.

  IF ( P_WG_SAIDA_DOC_CONTABIL IS NOT INITIAL ).

    VL_COUNT_DOC = STRLEN( P_WG_SAIDA_DOC_CONTABIL ).

    IF ( VL_COUNT_DOC <= 10 ).
      SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO @DATA(WL_VALIDA_ZIB)
      WHERE BELNR EQ @P_WG_SAIDA_DOC_CONTABIL.

      IF ( SY-SUBRC EQ 0 ).

        SET PARAMETER ID 'BLN'   FIELD P_WG_SAIDA_DOC_CONTABIL.
        SET PARAMETER ID 'BUK'   FIELD P_BUKRS-LOW.
        SET PARAMETER ID 'GJAHR' FIELD P_ANO-LOW.
        CALL TRANSACTION 'FB03'  AND SKIP FIRST SCREEN.

      ENDIF.

    ELSE.

      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
          TEXTLINE1 = P_WG_SAIDA_DOC_CONTABIL.

    ENDIF.

  ELSE.
    MESSAGE TEXT-002 TYPE 'I'.
  ENDIF.

ENDFORM.
