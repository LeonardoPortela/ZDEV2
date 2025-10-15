REPORT  ZSDR0021.

" "MIGNOW - Start change  MIGNOW  24.05.2023
 INCLUDE /MIGNOW/I_CONSTANTS_SD.
" "MIGNOW - End change  MIGNOW  24.05.2023

TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBAK.

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
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF TY_VBAK,
          BUKRS_VF TYPE VBAK-BUKRS_VF,
          AUART TYPE VBAK-AUART,
          KUNNR TYPE VBAK-KUNNR,
          VBELN TYPE VBAK-VBELN,
          BSTNK TYPE VBAK-BSTNK,
          WAERK TYPE VBAK-WAERK,
          NETWR TYPE VBAK-NETWR,
          AUDAT TYPE VBAK-AUDAT,
          GUEBG TYPE VBAK-GUEBG,
          GUEEN TYPE VBAK-GUEEN,
          MAHDT TYPE VBAK-MAHDT,
          VBTYP TYPE VBAK-VBTYP,
      END OF TY_VBAK,

       BEGIN OF TY_VBKD,
        VBELN       TYPE VBKD-VBELN,
        PRSDT       TYPE VBKD-PRSDT,
       END OF TY_VBKD,

      BEGIN OF TY_VBAP,
         VBELN  TYPE VBAP-VBELN,
         POSNR  TYPE VBAP-POSNR,
         MATNR  TYPE VBAP-MATNR,
         ARKTX  TYPE VBAP-ARKTX,
         NETPR  TYPE VBAP-NETPR,
         NETWR  TYPE VBAP-NETWR,
         ZMENG  TYPE VBAP-ZMENG,
         KWMENG TYPE VBAP-KWMENG,
         WERKS  TYPE VBAP-WERKS,
      END OF TY_VBAP,

       BEGIN OF TY_VBFA,
          VBELV TYPE VBFA-VBELV,
          POSNV TYPE VBFA-POSNV,
          RFMNG TYPE VBFA-RFMNG,
          RFWRT TYPE VBFA-RFWRT,
          VBELN TYPE VBFA-VBELN,
          POSNN TYPE VBFA-POSNN,
      END OF TY_VBFA,

     BEGIN OF TY_VBRP,
          VBELN TYPE VBRP-VBELN,
          POSNR TYPE VBRP-POSNR,
          NETWR TYPE VBRP-NETWR,
      END OF TY_VBRP,

      BEGIN OF TY_KNA1,
          KUNNR TYPE KNA1-KUNNR,
          NAME1 TYPE KNA1-NAME1,
      END OF TY_KNA1,

      BEGIN OF TY_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
      END OF TY_T001W,

      BEGIN OF TY_SAIDA,
          BUKRS_VF      TYPE VBAK-BUKRS_VF,
          AUART         TYPE VBAK-AUART,
          VBELNC        TYPE VBAK-VBELN,
          BSTNK         TYPE VBAK-BSTNK,
          ST_CTR(4)     TYPE C,
          MST_CTR(45)   TYPE C,
          PRSDT         TYPE VBKD-PRSDT,
          KUNNR         TYPE VBAK-KUNNR,
          NAME1K        TYPE KNA1-NAME1,
          MATNR         TYPE VBAP-MATNR,
          ARKTX         TYPE VBAP-ARKTX,
          WAERK         TYPE VBAK-WAERK,
*          NETPR         TYPE VBAP-NETPR,
*          ZMENG         TYPE VBAP-ZMENG,
*          NETWR         TYPE VBAK-NETWR,
*          ZMENG90       TYPE VBAP-ZMENG,
*          ZMENG105      TYPE VBAP-ZMENG,
          NETPR(15)     TYPE C,
          ZMENG(15)     TYPE C,
          NETWR(15)     TYPE C,
          ZMENG90(15)   TYPE C,
          ZMENG105(15)  TYPE C,
          AUDAT         TYPE VBAK-AUDAT,
*          RFMNG90       TYPE VBFA-RFMNG,
*          RFMNG105      TYPE VBFA-RFMNG,
          RFMNG90(15)   TYPE C,
          RFMNG105(15)  TYPE C,
*          INT_PERI      TYPE I,
          INT_PERI(05)  TYPE C,
          GUEBG         TYPE VBAK-GUEBG,
          GUEEN         TYPE VBAK-GUEEN,
          ST_OV(4)      TYPE C,
          MST_OV(45)    TYPE C,
          VBELNP        TYPE VBAK-VBELN,
          BSTNKC        TYPE VBAK-BSTNK,
          KWMENG        TYPE VBAP-KWMENG,
          MAHDT         TYPE VBAK-MAHDT,
          RFMNG         TYPE VBFA-RFMNG,
          RFWRT         TYPE VBFA-RFWRT,
          BUQUE(15)     TYPE C,
          ADUANA(15)    TYPE C,
          DESTINO(15)   TYPE C,
          FECHA(12)     TYPE C,
          NAME1W        TYPE T001W-NAME1,
      END OF TY_SAIDA.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:  IT_VBAK        TYPE TABLE OF TY_VBAK,
       IT_VBAKC       TYPE TABLE OF TY_VBAK,
       IT_VBKD        TYPE TABLE OF TY_VBKD,
       IT_VBAP        TYPE TABLE OF TY_VBAP,
       IT_VBAPC       TYPE TABLE OF TY_VBAP,
       IT_VBFA        TYPE TABLE OF TY_VBFA,
       IT_VBFAC       TYPE TABLE OF TY_VBFA,
       IT_VBRP        TYPE TABLE OF TY_VBRP,
       IT_KNA1        TYPE TABLE OF TY_KNA1,
       IT_T001W       TYPE TABLE OF TY_T001W,
       IT_SAIDA       TYPE TABLE OF TY_SAIDA.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA: WA_VBAK        TYPE TY_VBAK,
      WA_VBAKC       TYPE TY_VBAK,
      WA_VBKD        TYPE TY_VBKD,
      WA_VBAP        TYPE TY_VBAP,
      WA_VBAPC       TYPE TY_VBAP,
      WA_VBFA        TYPE TY_VBFA,
      WA_VBFAC       TYPE TY_VBFA,
      WA_VBRP        TYPE TY_VBRP,
      WA_KNA1        TYPE TY_KNA1,
      WA_T001W       TYPE TY_T001W,
      WA_SAIDA       TYPE TY_SAIDA,

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
      V_REPORT   LIKE SY-REPID             ,
      VG_I       TYPE I.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_BUKRS TYPE VBAK-BUKRS_VF OBLIGATORY,
           P_VBELN TYPE VBAK-VBELN.
SELECT-OPTIONS: P_KUNNR       FOR  VBAK-KUNNR,
                P_AUART       FOR  VBAK-AUART,
                P_AUDAT       FOR  VBAK-AUDAT NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
            F_INICIALIZAR         ,
            F_SELECIONA_DADOS     , " Form seleciona dados
            F_SAIDA               , " Saida relatorio
            F_CLASSIFICACAO       ,
            F_IMPRIME_DADOS      .


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
     EXPORTING
          INPUT  = P_BUKRS
     IMPORTING
          OUTPUT = P_BUKRS.
  " VBAK Contratos
  SELECT  BUKRS_VF AUART KUNNR VBELN BSTNK WAERK NETWR AUDAT GUEBG GUEEN MAHDT VBTYP
    FROM VBAK
    INTO TABLE IT_VBAK
    WHERE BUKRS_VF EQ P_BUKRS
    AND   KUNNR IN P_KUNNR
    AND   AUDAT IN P_AUDAT
    AND   AUART IN P_AUART
    AND   VBTYP EQ 'G'.

  IF NOT P_VBELN IS INITIAL.
    DELETE IT_VBAK WHERE     VBELN NE P_VBELN.
  ENDIF.
  CHECK NOT IT_VBAK IS INITIAL.

  SELECT VBELN PRSDT
   FROM VBKD
   INTO TABLE IT_VBKD
   FOR ALL ENTRIES IN IT_VBAK
   WHERE VBELN = IT_VBAK-VBELN.

  " item contrato
  SELECT VBELN POSNR MATNR ARKTX NETPR NETWR ZMENG KWMENG WERKS
   FROM VBAP
   INTO TABLE IT_VBAP
   FOR ALL ENTRIES IN IT_VBAK
   WHERE VBELN = IT_VBAK-VBELN
   AND   POSNR = 10.

  " Ordens faturadas
  SELECT VBELV POSNV RFMNG RFWRT VBELN POSNN
    FROM VBFA
    INTO TABLE IT_VBFA
    FOR ALL ENTRIES IN IT_VBAK
   WHERE VBELV   EQ IT_VBAK-VBELN
   AND   VBTYP_N EQ 'C'
   AND   VBTYP_V EQ 'G'
   AND   POSNV NE 0.

  " VBAK ORDENS
  IF NOT IT_VBFA[] IS INITIAL.
    SELECT  BUKRS_VF AUART KUNNR VBELN BSTNK WAERK NETWR AUDAT GUEBG GUEEN MAHDT VBTYP
      FROM VBAK
      INTO TABLE IT_VBAKC
      FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELN = IT_VBFA-VBELN.

    IF NOT IT_VBAKC[] IS INITIAL.
      SELECT VBELV POSNV RFMNG RFWRT VBELN POSNN
      FROM VBFA
        INTO TABLE IT_VBFAC
      FOR ALL ENTRIES IN IT_VBAKC
     WHERE VBELV   EQ IT_VBAKC-VBELN
     AND VBTYP_N = 'M'.

      SELECT VBELN POSNR NETWR
      FROM VBRP
      INTO TABLE IT_VBRP
        FOR ALL ENTRIES IN IT_VBFAC
        WHERE VBELN EQ IT_VBFAC-VBELN.
      " item ordem (pegar Werks)
      SELECT VBELN POSNR MATNR ARKTX NETPR NETWR ZMENG KWMENG WERKS
       FROM VBAP
       INTO TABLE IT_VBAPC
       FOR ALL ENTRIES IN IT_VBAKC
       WHERE VBELN = IT_VBAKC-VBELN
       AND   POSNR NE 0.

      SELECT WERKS NAME1
      FROM T001W
      INTO TABLE IT_T001W
      FOR ALL ENTRIES IN IT_VBAPC
      WHERE WERKS = IT_VBAPC-WERKS.

    ENDIF.
  ENDIF.

  SELECT KUNNR NAME1
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_VBAK
    WHERE KUNNR = IT_VBAK-KUNNR.



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

  DATA: V_DIFF      TYPE I,
        TEXTO(132)  TYPE C,
        VRFMNG      TYPE VBFA-RFMNG,
        TRFMNG      TYPE VBFA-RFMNG,
        VRFWRT      TYPE VBFA-RFWRT,
        VFLAG_VBAP  TYPE C,
        VFLAG_VBFA  TYPE C,
        VVALOR      TYPE P DECIMALS 2,
        VZMENG90    TYPE VBAP-ZMENG,
        VZMENG105   TYPE VBAP-ZMENG,
        VPERIOD     TYPE I.

  DATA: BEGIN OF ITAB_TLINE OCCURS 0.
          INCLUDE STRUCTURE TLINE.
  DATA: END OF ITAB_TLINE.
  DATA: TXID     LIKE THEAD-TDID     VALUE '0001',
        OBJEKT   LIKE THEAD-TDOBJECT VALUE 'VBBK',
        ZNAME    TYPE STXL-TDNAME.


  SORT IT_VBKD BY VBELN.
  SORT IT_KNA1 BY KUNNR.
  SORT IT_VBFA BY VBELV.
  SORT IT_VBFAC BY VBELV POSNV .
  SORT IT_VBAKC BY VBELN.
  SORT IT_VBAP BY VBELN.
  SORT IT_VBAPC BY VBELN POSNR.
  SORT IT_VBRP BY VBELN.

  SORT IT_T001W BY WERKS.
  CLEAR WA_SAIDA.

  LOOP AT IT_VBAK INTO WA_VBAK.

    CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
      EXPORTING
        I_DATUM_BIS             = WA_VBAK-GUEEN
        I_DATUM_VON             = SY-DATUM
      IMPORTING
        E_TAGE                  = V_DIFF
      EXCEPTIONS
        DAYS_METHOD_NOT_DEFINED = 1
        OTHERS                  = 2.

    WA_SAIDA-BUKRS_VF  = WA_VBAK-BUKRS_VF.
    WA_SAIDA-AUART     = WA_VBAK-AUART.
    WA_SAIDA-VBELNC    = WA_VBAK-VBELN.
    WA_SAIDA-BSTNK     = WA_VBAK-BSTNK .
    IF WA_VBAK-AUART = 'YC02'.
      IF V_DIFF LE 15.
        WA_SAIDA-ST_CTR    = ICON_RED_LIGHT.
        WA_SAIDA-MST_CTR   = 'Contrato menor a 15 dias de vencer'.
      ELSEIF V_DIFF LE 30.
        WA_SAIDA-ST_CTR    = ICON_YELLOW_LIGHT.
        WA_SAIDA-MST_CTR   = 'Contrato menor a 30 dias de vencer'.
      ELSEIF V_DIFF GT 30.
        WA_SAIDA-ST_CTR    = ICON_GREEN_LIGHT.
        WA_SAIDA-MST_CTR   = 'Contrato mayor a 30 dias de vencer'.
      ENDIF.
    ENDIF.

    READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-PRSDT     = WA_VBKD-PRSDT.
    ELSE.
      CLEAR WA_SAIDA-PRSDT.
    ENDIF.
    WA_SAIDA-KUNNR     = WA_VBAK-KUNNR.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-NAME1K    = WA_KNA1-NAME1.
    ELSE.
      CLEAR WA_SAIDA-NAME1K.
    ENDIF.

    WA_SAIDA-WAERK     = WA_VBAK-WAERK.
    WA_SAIDA-AUDAT     = WA_VBAK-AUDAT.

    VPERIOD  = WA_VBAK-GUEEN - WA_VBAK-GUEBG.
    WA_SAIDA-INT_PERI = VPERIOD.

    WA_SAIDA-GUEBG     = WA_VBAK-GUEBG.
    WA_SAIDA-GUEEN     = WA_VBAK-GUEEN.
    VFLAG_VBAP = ''.
    TRFMNG = 0.
    VZMENG90 = 0.
    VZMENG105 = 0.
    LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_VBAK-VBELN. " só tem 1 item (eliminar loop)
      VZMENG90 = WA_VBAP-ZMENG + VZMENG90.
      VZMENG105 = WA_VBAP-ZMENG + VZMENG105.
      READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELV = WA_VBAP-VBELN                                   BINARY SEARCH.
      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_VBAP-VBELN.
        READ TABLE IT_VBAPC INTO WA_VBAPC WITH KEY VBELN = WA_VBFA-VBELN
                                                  POSNR = WA_VBFA-POSNN BINARY SEARCH.
        IF SY-SUBRC = 0.
          LOOP AT  IT_VBFAC INTO WA_VBFAC WHERE VBELV = WA_VBAPC-VBELN
                                          AND   POSNV = WA_VBAPC-POSNR.

            TRFMNG = TRFMNG + WA_VBFA-RFMNG.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    VZMENG90 = VZMENG90 * ( 90 / 100 ).
    VZMENG105 = VZMENG105 * ( 105 / 100 ).

    VVALOR   = TRFMNG - VZMENG90.
    IF VVALOR < 0.
      VVALOR = 0.
    ENDIF.
    CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
      EXPORTING
        BETRG                   = VVALOR
        NEW_DECIMAL_SEPARATOR   = ','
        NEW_THOUSANDS_SEPARATOR = '.'
      IMPORTING
        STRING                  = WA_SAIDA-RFMNG90.

    VVALOR  = TRFMNG - VZMENG105.
    IF VVALOR < 0.
      VVALOR = 0.
    ENDIF.
    CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
      EXPORTING
        BETRG                   = VVALOR
        NEW_DECIMAL_SEPARATOR   = ','
        NEW_THOUSANDS_SEPARATOR = '.'
      IMPORTING
        STRING                  = WA_SAIDA-RFMNG105.

    IF TRFMNG > 0.
      IF TRFMNG >= VZMENG105  AND  WA_VBAK-GUEEN GT SY-DATUM .
        WA_SAIDA-ST_CTR    = ICON_CHARACTERISTICS_ACT.
        WA_SAIDA-MST_CTR   = 'Contrato completado al 105%'.
      ELSEIF TRFMNG <= VZMENG90  AND  WA_VBAK-GUEEN GT SY-DATUM .
        WA_SAIDA-ST_CTR    = ICON_CHARACTERISTICS_INA.
        WA_SAIDA-MST_CTR   = 'Contrato completado al 90%'.
      ELSEIF TRFMNG   = WA_VBAP-ZMENG AND  WA_VBAK-GUEEN GT SY-DATUM .
        WA_SAIDA-ST_CTR    = ICON_KEYFIGURE_INA.
        WA_SAIDA-MST_CTR   = 'Contrato completado al 100%'.
      ENDIF.
    ENDIF.

    LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_VBAK-VBELN.
      VFLAG_VBAP = 'X'.
      VZMENG90 = WA_VBAP-ZMENG * ( 90 / 100 ).
      VZMENG105 = WA_VBAP-ZMENG * ( 105 / 100 ).

      WA_SAIDA-MATNR     = WA_VBAP-MATNR.
      WA_SAIDA-ARKTX     = WA_VBAP-ARKTX.
      VVALOR = WA_VBAP-NETWR.
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-NETWR.

      VVALOR = WA_VBAP-NETPR.
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-NETPR.

      VVALOR = WA_VBAP-ZMENG.
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-ZMENG.

      VVALOR = WA_VBAP-ZMENG.
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-ZMENG.

      VVALOR = WA_VBAP-ZMENG * ( 90 / 100 ) .
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-ZMENG90.

      VVALOR = WA_VBAP-ZMENG * ( 105 / 100 ).
      CALL FUNCTION 'HRCM_AMOUNT_TO_STRING_CONVERT'
        EXPORTING
          BETRG                   = VVALOR
          NEW_DECIMAL_SEPARATOR   = ','
          NEW_THOUSANDS_SEPARATOR = '.'
        IMPORTING
          STRING                  = WA_SAIDA-ZMENG105.

      VFLAG_VBFA = ''.
      READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELV = WA_VBAP-VBELN                                   BINARY SEARCH.
      LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_VBAP-VBELN.
        VFLAG_VBFA = 'X'.
        READ TABLE IT_VBAKC INTO WA_VBAKC WITH KEY VBELN = WA_VBFA-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0 .
          MOVE WA_VBAKC-VBELN TO ZNAME.
          WA_SAIDA-VBELNP    = WA_VBAKC-VBELN.
          WA_SAIDA-BSTNKC    = WA_VBAKC-BSTNK.
          WA_SAIDA-MAHDT     = WA_VBAKC-MAHDT.
          CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
            EXPORTING
              I_DATUM_BIS             = SY-DATUM
              I_DATUM_VON             = WA_VBAKC-MAHDT
            IMPORTING
              E_TAGE                  = V_DIFF
            EXCEPTIONS
              DAYS_METHOD_NOT_DEFINED = 1
              OTHERS                  = 2.

          IF WA_VBAKC-AUART = 'YV04'.
            IF V_DIFF <= 5.
              WA_SAIDA-ST_OV     =  ICON_LED_RED.
              WA_SAIDA-MST_OV   = 'Ordem venda menor a 5 dias de vencer'.
            ELSEIF V_DIFF <= 10.
              WA_SAIDA-ST_OV     =  ICON_LED_YELLOW.
              WA_SAIDA-MST_OV   = 'Ordem venda menor a 10 dias de vencer'.
            ELSEIF V_DIFF > 10.
              WA_SAIDA-ST_OV     =  ICON_LED_GREEN.
              WA_SAIDA-MST_OV  = 'Ordem venda mayor  a 10 dias de vencer'.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE IT_VBAPC INTO WA_VBAPC WITH KEY VBELN = WA_VBFA-VBELN
                                                   POSNR = WA_VBFA-POSNN BINARY SEARCH.
        IF SY-SUBRC = 0.

          VRFMNG = 0.
          VRFWRT = 0.
          LOOP AT  IT_VBFAC INTO WA_VBFAC WHERE VBELV = WA_VBAPC-VBELN
                                          AND   POSNV = WA_VBAPC-POSNR.

            VRFMNG = VRFMNG + WA_VBFA-RFMNG.
            TRFMNG = TRFMNG + WA_VBFA-RFMNG.
            READ TABLE IT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_VBFAC-VBELN
                                                       POSNR = WA_VBFAC-POSNN BINARY SEARCH.
            IF SY-SUBRC = 0.
              VRFWRT     = VRFWRT + WA_VBRP-NETWR.
            ENDIF.
          ENDLOOP.
          WA_SAIDA-RFWRT = VRFWRT.

          IF VRFMNG = WA_VBAPC-KWMENG .
            WA_SAIDA-ST_OV     =  ICON_CHECKED.
            WA_SAIDA-MST_OV   = 'Ordem venda completada'.
          ENDIF.

          WA_SAIDA-KWMENG    = WA_VBAPC-KWMENG.

          READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_VBAPC-WERKS BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-NAME1W    = WA_T001W-NAME1.
          ELSE.
            CLEAR WA_SAIDA-NAME1W.
          ENDIF.
        ELSE.
          CLEAR WA_SAIDA-RFWRT.
          CLEAR WA_SAIDA-NAME1W.
        ENDIF.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = TXID
            LANGUAGE                = SY-LANGU
            NAME                    = ZNAME
            OBJECT                  = OBJEKT
          TABLES
            LINES                   = ITAB_TLINE
          EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
        LOOP AT ITAB_TLINE.
          TEXTO = ITAB_TLINE-TDLINE.
          IF TEXTO+0(6) = 'BUQUE:'.
            WA_SAIDA-BUQUE     = TEXTO+7(15).
          ELSEIF TEXTO+0(7) = 'ADUANA:'.
            WA_SAIDA-ADUANA    = TEXTO+8(15).
          ELSEIF TEXTO+0(8) = 'DESTINO:'.
            WA_SAIDA-DESTINO   = TEXTO+9(15).
          ELSEIF TEXTO+0(9) = 'FECHA BL:'.
            WA_SAIDA-FECHA     = TEXTO+10(15).
          ENDIF.
        ENDLOOP.
        "
        VRFMNG = VRFMNG + WA_VBFA-RFMNG.
        WA_SAIDA-RFMNG     = WA_VBFA-RFMNG.
        "WA_SAIDA-RFWRT     = WA_VBFA-RFWRT. trocar por WA_VBRP-NETWR
        APPEND WA_SAIDA TO IT_SAIDA.
      ENDLOOP.

      IF VFLAG_VBFA = ''.
        APPEND WA_SAIDA TO IT_SAIDA.
      ENDIF.
    ENDLOOP.
    IF VFLAG_VBAP = ''.
      APPEND WA_SAIDA TO IT_SAIDA.
    ENDIF.
    CLEAR WA_SAIDA.
  ENDLOOP.

  SORT IT_SAIDA BY BUKRS_VF AUART VBELNC VBELNP.

ENDFORM.                    " F_SAIDA
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
      I_CALLBACK_PROGRAM      = V_REPORT
*      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.


ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
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
*      -->NAME       text
*      -->FORM       text
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
          'BUKRS_VF'     TEXT-002        '10'       ' '     ' '    ' ' , " Company
          'AUART'        TEXT-003        '08'       ' '     ' '    ' ' , " Type Ctr.
          'VBELNC'       TEXT-004        '15'       ' '     ' '    ' ' , " Nr. Ctr.  SAP
          'BSTNK'        TEXT-005        '16'       ' '     ' '    ' ' , " Nr. Ctr. ROE
          'ST_CTR'       TEXT-006        '10'       ' '     ' '    ' ' , " Status Ctr.
          'MST_CTR'      TEXT-007        '35'       ' '     ' '    ' ' , " Motivo Status Ctr.
          'PRSDT'        TEXT-008        '10'       ' '     ' '    ' ' , " Date Ctr.
          'KUNNR'        TEXT-009        '15'       ' '     ' '    ' ' , " Cod. Customer
          'NAME1K'       TEXT-010        '20'       ' '     ' '    ' ' , " Customer
          'MATNR'        TEXT-011        '20'       ' '     ' '    ' ' , " Cod. Commodity
          'ARKTX'        TEXT-012        '25'       ' '     ' '    ' ' , " Descr. Commodity
          'WAERK'        TEXT-013        '08'       ' '     ' '    ' ' , " Currency
          'NETPR'        TEXT-014        '15'       ' '     ' '    ' ' , " Price
          'ZMENG'        TEXT-015        '15'       ' '     ' '    ' ' , " Quantity Ctr.
          'NETWR'        TEXT-016        '15'       ' '     ' '    ' ' , " Amount
          'ZMENG90'      TEXT-017        '15'       ' '     ' '    ' ' , " Quantity 90%
          'ZMENG105'     TEXT-018        '15'       ' '     ' '    ' ' , " Quantity 105%
          'AUDAT'        TEXT-019        '15'       ' '     ' '    ' ' , " Date of document
          'RFMNG90'      TEXT-020        '20'       ' '     ' '    ' ' , " Quantity Open 90 pct
          'RFMNG105'     TEXT-021        '20'       ' '     ' '    ' ' , " Quantity Open 105 pct
          'INT_PERI'     TEXT-022        '20'       ' '     ' '    ' ' , " Shipment Period
          'GUEBG'        TEXT-023        '20'       ' '     ' '    ' ' , " Contract Inicio Date
          'GUEEN'        TEXT-024        '20'       ' '     ' '    ' ' , " Contract Expiry Date
          'ST_OV'        TEXT-025        '10'       ' '     ' '    ' ' , " Status O.V.
          'MST_OV'       TEXT-026        '40'       ' '     ' '    ' ' , " Motivo Status OV.
          'VBELNP'       TEXT-027        '20'       ' '     ' '    ' ' , " Shipping Permission SAP
          'BSTNKC'       TEXT-028        '20'       ' '     ' '    ' ' , " Shipping Permission Nr
          'KWMENG'       TEXT-029        '20'       ' '     ' '    ' ' , " Shipping Permission Qtty
          'MAHDT'        TEXT-030        '20'       ' '     ' '    ' ' , " Shipping Permission Expiry Date
          'RFMNG'        TEXT-031        '15'       ' '     ' '    'X' , " Quantity Applied
          'RFWRT'        TEXT-032        '15'       ' '     ' '    'X' , " Amount Applied
          'BUQUE'        TEXT-033        '15'       ' '     ' '    ' ' , " BUQUE
          'ADUANA'       TEXT-034        '15'       ' '     ' '    ' ' , " ADUANA
          'DESTINO'      TEXT-035        '15'       ' '     ' '    ' ' , " DESTINO
          'FECHA'        TEXT-036        '10'       ' '     ' '    ' ' , " FECHA
          'NAME1W'       TEXT-037        '20'       ' '     ' '    ' ' . " Port of  Origin
ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
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
  IF P_CAMPO = 'ST_CTR' OR P_CAMPO = 'ST_OV'.
    WL_FCAT-ICON      = 'X'.
  ENDIF.
  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.
ENDFORM.                    " F_CONSTRUIR_CABECALHO
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
  REFRESH T_SORT.

  MC_PREENCHE_CLASS: 'BUKRS_VF'    '' 'X' ' ',
                     'AUART'       '' 'X' ' ',
                     'VBELNC'      '' 'X' 'X',
                     'BSTNK'       '' 'X' ' ',
                     'ST_CTR'      '' 'X' ' ',
                     'MST_CTR'     '' 'X' ' ',
                     'PRSDT'       '' 'X' ' ',
                     'KUNNR'       '' 'X' ' ',
                     'NAME1K'      '' 'X' ' ',
                     'MATNR'       '' 'X' ' ',
                     'ARKTX'       '' 'X' ' ',
                     'WAERK'       '' 'X' ' ',
                     'NETPR'       '' 'X' ' ',
                     'ZMENG'       '' 'X' ' ',
                     'NETWR'       '' 'X' ' ',
                     'ZMENG90'     '' 'X' ' ',
                     'ZMENG90'     '' 'X' ' ',
                     'ZMENG105'    '' 'X' ' ',
                     'AUDAT'       '' 'X' ' ',
                     'RFMNG90'     '' 'X' ' ',
                     'RFMNG105'    '' 'X' ' ',
                     'INT_PERI'    '' 'X' ' ',
                     'GUEBG'       '' 'X' ' ',
                     'GUEEN'       '' 'X' ' '.

ENDFORM.                    " F_CLASSIFICACAO
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIALIZAR .
  DATA:
              W_TEXTO1(10),
              W_TEXTO2(10),
              W_TEXTO3(40),

              W_EMPRESA_TEXTO(40),
              W_TIPO_TEXTO(40),
              W_ESC_TEXTO(40),
              W_CLI_TEXTO(40),
              W_OV_TEXTO(40),
              W_DTCRI_TEXTO(40),
              W_DTVE_TEXTO(40),

              EMPRESA      TYPE C LENGTH 50,
              TIPO         TYPE C LENGTH 50,
              DTCRIACAO    TYPE C LENGTH 200.


  V_REPORT = SY-REPID.

  IF P_BUKRS IS NOT INITIAL.
    W_EMPRESA_TEXTO = 'Company   :'.
    CONCATENATE W_EMPRESA_TEXTO P_BUKRS  INTO EMPRESA SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.
  IF P_VBELN IS NOT INITIAL.
    W_EMPRESA_TEXTO = 'Contract   :'.
    CONCATENATE W_EMPRESA_TEXTO P_VBELN  INTO EMPRESA SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.

  IF P_KUNNR IS NOT INITIAL.
    W_TIPO_TEXTO = 'Customer:'.
    IF ( P_KUNNR-LOW IS NOT INITIAL ) AND ( P_KUNNR-HIGH IS NOT INITIAL ).
      CONCATENATE W_TIPO_TEXTO  P_KUNNR-LOW 'á' P_KUNNR-HIGH INTO TIPO SEPARATED BY SPACE.
    ELSEIF ( P_KUNNR-LOW IS NOT INITIAL ).
      CONCATENATE W_TIPO_TEXTO P_KUNNR-LOW  INTO TIPO SEPARATED BY SPACE.
    ELSE.
      CONCATENATE W_TIPO_TEXTO 'Todas'  INTO TIPO SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' TIPO.
  ENDIF.

  IF P_AUART IS NOT INITIAL.
    W_TIPO_TEXTO = 'Type Contract:'.
    IF ( P_AUART-LOW IS NOT INITIAL ) AND ( P_AUART-HIGH IS NOT INITIAL ).
      CONCATENATE W_TIPO_TEXTO  P_AUART-LOW 'á' P_AUART-HIGH INTO TIPO SEPARATED BY SPACE.
    ELSEIF ( P_AUART-LOW IS NOT INITIAL ).
      CONCATENATE W_TIPO_TEXTO P_AUART-LOW  INTO TIPO SEPARATED BY SPACE.
    ELSE.
      CONCATENATE W_TIPO_TEXTO 'Todas'  INTO TIPO SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' TIPO.
  ENDIF.

  IF ( NOT  P_AUDAT IS INITIAL ).
    W_DTCRI_TEXTO = 'Período :'.
    CONCATENATE P_AUDAT-LOW+6(2)   '.' P_AUDAT-LOW+4(2)  '.' P_AUDAT-LOW(4)  INTO W_TEXTO1.
    CONCATENATE P_AUDAT-HIGH+6(2)  '.' P_AUDAT-HIGH+4(2) '.' P_AUDAT-HIGH(4) INTO W_TEXTO2.
    CONCATENATE W_DTCRI_TEXTO W_TEXTO1 ' - ' W_TEXTO2 INTO DTCRIACAO  SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' DTCRIACAO.
  ENDIF.

*                P_AUART       FOR  VBAK-AUART,
*                P_AUDAT
ENDFORM.                    " F_INICIALIZAR
