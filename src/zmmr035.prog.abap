*&---------------------------------------------------------------------*
*& Report  ZMMR035
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMMR035.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.

TYPES: BEGIN OF TY_ZMMT0035,
         SAFRA      TYPE ZMMT0035-SAFRA,
         LIFNR      TYPE LFA1-LIFNR,
         EBELN      TYPE ZMMT0035-EBELN,
         WERKS      TYPE ZMMT0035-WERKS,
         BANFN      TYPE ZMMT0035-BANFN,
         NRO_SOL_CP TYPE ZMMT0035-NRO_SOL_CP,
         PED_FORN   TYPE ZMMT0035-PED_FORN,
         WAERS      TYPE ZMMT0035-WAERS,
       END OF TY_ZMMT0035,

       BEGIN OF TY_EKKO,
         EBELN TYPE EKKO-EBELN,
         BUKRS TYPE EKKO-BUKRS,
         BEDAT TYPE EKKO-BEDAT,
         LIFNR TYPE EKKO-LIFNR,
         WAERS TYPE EKKO-WAERS,
         WKURS TYPE EKKO-WKURS,
         GDATU TYPE TCURR-GDATU,
       END OF TY_EKKO,

       BEGIN OF TY_ZMMT0037,
         NRO_SOL_CP TYPE ZMMT0037-NRO_SOL_CP,
         MATNR      TYPE ZMMT0037-MATNR,
         MWSKZ      TYPE ZMMT0037-MWSKZ,
         EBELP      TYPE ZMMT0037-EBELP,
       END OF TY_ZMMT0037,

       BEGIN OF TY_EKPO,
         EBELN TYPE EKPO-EBELN,
         EBELP TYPE EKPO-EBELP,
         MATNR TYPE EKPO-MATNR,
         TXZ01 TYPE EKPO-TXZ01,
         MATKL TYPE EKPO-MATKL,
         MENGE TYPE EKPO-MENGE,
         NETPR TYPE EKPO-NETPR,
         PEINH TYPE EKPO-PEINH,
         ELIKZ TYPE EKPO-ELIKZ,
         WERKS TYPE EKPO-WERKS,
         MEINS TYPE EKPO-MEINS,
         MWSKZ TYPE EKPO-MWSKZ,
       END OF TY_EKPO,

       BEGIN OF TY_EKBE,
         EBELN TYPE EKBE-EBELN,
         EBELP TYPE EKBE-EBELP,
         VGABE TYPE EKBE-VGABE,
         BELNR TYPE EKBE-BELNR,
         BUZEI TYPE EKBE-BUZEI,
         GJAHR TYPE EKBE-GJAHR,
         MENGE TYPE EKBE-MENGE,
         DMBTR TYPE EKBE-DMBTR,
         WRBTR TYPE EKBE-WRBTR,
         SHKZG TYPE EKBE-SHKZG,
         BUDAT TYPE EKBE-BUDAT,
       END OF TY_EKBE,

       BEGIN OF TY_EKBE_AUX,
         EBELN TYPE EKBE-EBELN,
         EBELP TYPE EKBE-EBELP,
         BUKRS TYPE EKKO-BUKRS,
         GJAHR TYPE EKBE-GJAHR,
         AWKEY TYPE BKPF-AWKEY,
         BELNR TYPE EKBE-BELNR,
         DMBTR TYPE EKBE-DMBTR,
         WRBTR TYPE EKBE-WRBTR,
         SHKZG TYPE EKBE-SHKZG,
         VGABE TYPE EKBE-VGABE,
       END OF TY_EKBE_AUX,

       BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         GJAHR TYPE BKPF-GJAHR,
         AWKEY TYPE BKPF-AWKEY,
         BELNR TYPE BKPF-BELNR,
         KURS2 TYPE BKPF-KURS2,
         EBELN TYPE BSAK-EBELN,
         EBELP TYPE BSAK-EBELP,
       END OF TY_BKPF,

       BEGIN OF TY_BSAK,
         BUKRS    TYPE BSAK-BUKRS,
         BELNR    TYPE BSAK-BELNR,
         GJAHR    TYPE BSAK-GJAHR,
         BSCHL    TYPE BSAK-BSCHL,
         SHKZG    TYPE BSAK-SHKZG,
         UMSKZ    TYPE BSAK-UMSKZ,
         AUGBL    TYPE BSAK-AUGBL,
         DMBE2    TYPE BSAK-DMBE2,
         DMBTR    TYPE BSAK-DMBTR,
         AUGDT    TYPE BSAK-AUGDT,
         BUKRS_BK TYPE BKPF-BUKRS,
         BELNR_BK TYPE BKPF-BELNR,
         GJAHR_BK TYPE BKPF-GJAHR,
         EBELN    TYPE BSAK-EBELN,
         EBELP    TYPE BSAK-EBELP,
         GJAHRE   TYPE BSAK-GJAHR,
       END OF TY_BSAK,

       BEGIN OF TY_BSEG,
         BUKRS   TYPE BSEG-BUKRS,
         BELNR   TYPE BSEG-BELNR,
         GJAHR   TYPE BSEG-GJAHR,
         SHKZG   TYPE BSEG-SHKZG,
         DMBE2   TYPE BSEG-DMBE2,
         DMBTR   TYPE BSEG-DMBTR,
         FLAG(1),
       END OF TY_BSEG,


       BEGIN OF TY_SKB1,
         BUKRS TYPE SKB1-BUKRS,
         SAKNR TYPE SKB1-SAKNR,
         FDLEV TYPE SKB1-FDLEV,
       END OF TY_SKB1,

       BEGIN OF TY_T023T,
         SPRAS   TYPE T023T-SPRAS,
         MATKL   TYPE T023T-MATKL,
         WGBEZ60 TYPE T023T-WGBEZ60,
       END OF TY_T023T,

       BEGIN OF TY_SAIDA,
         NRO_SOL_CP           TYPE ZMMT0035-NRO_SOL_CP,
         SAFRA                TYPE ZMMT0035-SAFRA,
         WERKS                TYPE ZMMT0035-WERKS,
         NAME1_W              TYPE T001W-NAME1,
         LIFNR                TYPE ZMMT0035-LIFNR,
         NAME1_L              TYPE LFA1-NAME1,
         MATNR                TYPE EKPO-MATNR,
         MAKTX                TYPE MAKT-MAKTX,
         MATKL                TYPE EKPO-MATKL,
         WGBEZ60              TYPE T023T-WGBEZ60,
         EBELN                TYPE ZMMT0035-EBELN,
         MWSKZ                TYPE ZMMT0037-MWSKZ,
         WAERS                TYPE ZMMT0035-WAERS,
         BEDAT                TYPE EKKO-BEDAT,
         PEDIDO_VLR           TYPE EKBE-WRBTR,
         PEDIDO_VLR_A         TYPE EKBE-WRBTR,
         PEDIDO_VLR_US        TYPE EKBE-WRBTR,
         PEDIDO_VLRA_U        TYPE EKBE-WRBTR,
         PAGO_VLR             TYPE BSIS-DMBTR,
         PAGO_VLR_US          TYPE BSIS-DMBE2,
         VARI_PAGO            TYPE BSIS-DMBE2,
         RECEBIDO_VLR         TYPE BSIS-DMBTR,
         RECEBIDO_VLR_US      TYPE BSIS-DMBE2,
         SDO_A_PAGAR_VLR      TYPE BSIS-DMBTR,
         SDO_A_PAGAR_VLR_US   TYPE BSIS-DMBE2,
         SDO_A_RECEBER_VLR    TYPE BSIS-DMBTR,
         SDO_A_RECEBER_VLR_US TYPE BSIS-DMBE2,
         ADTO_VLR             TYPE BSIS-DMBTR,
         ADTO_VLR_US          TYPE BSIS-DMBE2,
         DESC_VLR             TYPE BSIS-DMBTR,
         DESC_VLR_US          TYPE BSIS-DMBE2,
       END OF TY_SAIDA,

       BEGIN OF TY_SAIDA_2,
         NRO_SOL_CP TYPE ZMMT0035-NRO_SOL_CP,
         SAFRA      TYPE ZMMT0035-SAFRA,
         WERKS      TYPE ZMMT0035-WERKS,
         NAME1_W    TYPE T001W-NAME1,
         LIFNR      TYPE ZMMT0035-LIFNR,
         NAME1_L    TYPE LFA1-NAME1,
         MATNR      TYPE EKPO-MATNR,
         MEINS      TYPE EKPO-MEINS,
         MAKTX      TYPE MAKT-MAKTX,
         MATKL      TYPE EKPO-MATKL,
         WGBEZ60    TYPE T023T-WGBEZ60,
         EBELN      TYPE ZMMT0035-EBELN,
         MWSKZ      TYPE ZMMT0037-MWSKZ,
         BEDAT      TYPE EKKO-BEDAT,
         WAERS      TYPE ZMMT0035-WAERS,
         BUDAT      TYPE BKPF-BUDAT,
         VLR_TOT    TYPE BSIS-DMBTR,
         VLR_TOT_US TYPE BSIS-DMBE2,
         RECEBIDO   TYPE EKBE-MENGE,
         ARECEBER   TYPE EKBE-MENGE,
         LBLAB      TYPE MSLB-LBLAB,
       END OF TY_SAIDA_2,

       BEGIN OF TY_SAIDA_3,
         NRO_SOL_CP TYPE ZMMT0035-NRO_SOL_CP,
         SAFRA      TYPE ZMMT0035-SAFRA,
         WERKS      TYPE ZMMT0035-WERKS,
         NAME1_W    TYPE T001W-NAME1,
         LIFNR      TYPE ZMMT0035-LIFNR,
         NAME1_L    TYPE LFA1-NAME1,
         MATNR      TYPE EKPO-MATNR,
         MEINS      TYPE EKPO-MEINS,
         MAKTX      TYPE MAKT-MAKTX,
         MATKL      TYPE EKPO-MATKL,
         WGBEZ60    TYPE T023T-WGBEZ60,
         EBELN      TYPE ZMMT0035-EBELN,
         MWSKZ      TYPE ZMMT0037-MWSKZ,
         BEDAT      TYPE EKKO-BEDAT,
         WAERS      TYPE ZMMT0035-WAERS,
         UKURS      TYPE TCURR-UKURS,
         MENGE      TYPE EKBE-MENGE,
         VALOR      TYPE EKBE-DMBTR,
         VALOR_US   TYPE EKBE-WRBTR,
         RECEBIDO   TYPE EKBE-MENGE,
         ARECEBER   TYPE EKBE-MENGE,
       END OF TY_SAIDA_3,

       BEGIN OF TY_EKBE_TOTAL,
         EBELN     TYPE EKBE-EBELN,
         EBELP     TYPE EKBE-EBELP,
         TOT_QUAN  TYPE EKBE-MENGE,
         TOT_DMBTR TYPE EKBE-DMBTR,
         TOT_WRBTR TYPE EKBE-WRBTR,
         TX_CAMBIO TYPE KURSF,
       END OF TY_EKBE_TOTAL,

       BEGIN OF TY_BSIS_TOTAL,
         EBELN     TYPE EKBE-EBELN,
         EBELP     TYPE EKBE-EBELP,
         DMBTR     TYPE BSIS-DMBTR,
         DMBE2     TYPE BSIS-DMBE2,
         DMBE2_VAR TYPE BSIS-DMBE2,
         TX_CAMBIO TYPE KURSF,
       END OF TY_BSIS_TOTAL,

       BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_T001W,
         WERKS TYPE T001W-WERKS,
         NAME1 TYPE T001W-NAME1,
       END OF TY_T001W,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TY_LFA1,

       BEGIN OF TY_TCURR,
         KURST TYPE TCURR-KURST,
         FCURR TYPE TCURR-FCURR,
         TCURR TYPE TCURR-TCURR,
         GDATU TYPE TCURR-GDATU,
         UKURS TYPE TCURR-UKURS,
       END OF TY_TCURR.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: T_0035         TYPE TABLE OF TY_ZMMT0035,
      T_0037         TYPE TABLE OF TY_ZMMT0037,
      T_RBKP         TYPE TABLE OF RBKP,
      T_EKPO         TYPE TABLE OF TY_EKPO,

      T_EKBE         TYPE TABLE OF TY_EKBE,
      T_EKBE_AUX     TYPE TABLE OF TY_EKBE_AUX,
      T_EKBE_GER     TYPE TABLE OF TY_EKBE_AUX,
      T_EKBE_2       TYPE TABLE OF TY_EKBE,
      T_EKBE_4       TYPE TABLE OF TY_EKBE,
      T_BKPF         TYPE TABLE OF TY_BKPF,
      T_BSAK         TYPE TABLE OF TY_BSAK,
      T_BSAK2        TYPE TABLE OF TY_BSAK,
      T_BSAK3        TYPE TABLE OF TY_BSAK,
      T_EKKO         TYPE TABLE OF TY_EKKO,
      T_T023T        TYPE TABLE OF TY_T023T,
      T_SAIDA        TYPE TABLE OF TY_SAIDA,
      T_EKBE_TOTAL   TYPE TABLE OF TY_EKBE_TOTAL,
      T_MAKT         TYPE TABLE OF TY_MAKT,
      T_T001W        TYPE TABLE OF TY_T001W,
      T_LFA1         TYPE TABLE OF TY_LFA1,
      T_TCURR        TYPE TABLE OF TY_TCURR,
      T_EKBE_TOTAL_2 TYPE TABLE OF TY_EKBE_TOTAL,
      T_SAIDA_2      TYPE TABLE OF TY_SAIDA_2,
      T_SAIDA_3      TYPE TABLE OF TY_SAIDA_3.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: WA_0035         TYPE TY_ZMMT0035,
      WA_0037         TYPE TY_ZMMT0037,
      WA_RBKP         TYPE RBKP,
      WA_EKPO         TYPE TY_EKPO,
      WA_EKPO_AUX     TYPE TY_EKPO,
      WA_EKBE         TYPE TY_EKBE,
      WA_EKBE_AUX     TYPE TY_EKBE_AUX,
      WA_EKBE_GER     TYPE TY_EKBE_AUX,
      WA_EKBE_TOT     TYPE TY_EKBE_AUX,
      WA_EKBE_1       TYPE TY_EKBE,
      WA_EKBE_2       TYPE TY_EKBE,
      WA_EKBE_4       TYPE TY_EKBE,
      WA_BSAK         TYPE TY_BSAK,
      WA_BSAK2        TYPE TY_BSAK,
      WA_BSAK3        TYPE TY_BSAK,
      WA_BSAK4        TYPE TY_BSAK,
      WA_BSAK_ORI     TYPE TY_BSAK,
      WA_BSEG         TYPE TY_BSEG,
      WA_BKPF         TYPE TY_BKPF,
      WA_SKB1         TYPE TY_SKB1,
      WA_EKKO         TYPE TY_EKKO,
      WA_T023T        TYPE TY_T023T,
      WA_SAIDA        TYPE TY_SAIDA,
      WA_EKBE_TOTAL   TYPE TY_EKBE_TOTAL,
      WA_EKBE_TOTAL_1 TYPE TY_EKBE_TOTAL,
      WA_EKBE_TOTAL_2 TYPE TY_EKBE_TOTAL,
      WA_EKBE_TOTAL_3 TYPE TY_EKBE_TOTAL,
      WA_BSIS_TOTAL   TYPE TY_BSIS_TOTAL,
      WA_MAKT         TYPE TY_MAKT,
      WA_T001W        TYPE TY_T001W,
      WA_LFA1         TYPE TY_LFA1,
      WA_TCURR        TYPE TY_TCURR,
      WA_SAIDA_2      TYPE TY_SAIDA_2,
      WA_SAIDA_3      TYPE TY_SAIDA_3.


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
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_SAFRA FOR WA_0035-SAFRA OBLIGATORY,
                S_LIFNR FOR WA_0035-LIFNR,
                S_WERKS FOR WA_0035-WERKS,
                S_MATNR FOR WA_0037-MATNR,
                S_EBELN FOR WA_0035-EBELN,
                S_BEDAT FOR WA_EKKO-BEDAT OBLIGATORY,
                S_BUDAT FOR WA_EKKO-BEDAT .
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_RECEB  RADIOBUTTON GROUP G1,
            P_RESU_F RADIOBUTTON GROUP G1 DEFAULT 'X',
            P_PED_PG RADIOBUTTON GROUP G1,
            P_REL_IN RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SAFRA-LOW.
  PERFORM SEARCH_SAFRA USING 'S_SAFRA-LOW'
                       CHANGING S_SAFRA-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SAFRA-HIGH.
  PERFORM SEARCH_SAFRA USING 'S_SAFRA-HIGH'
                       CHANGING S_SAFRA-HIGH.


*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM INICIAR_VARIAVEIS.
  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
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
  DATA: WL_DATA(8),
        RT_LIFNR   TYPE RANGE OF LFA1-LIFNR,
        RA_LIFNR   LIKE LINE OF RT_LIFNR.

  SELECT SAFRA LIFNR EBELN WERKS BANFN NRO_SOL_CP PED_FORN WAERS
    FROM ZMMT0035
    INTO TABLE T_0035
      WHERE SAFRA IN S_SAFRA
        AND LIFNR IN S_LIFNR
        AND EBELN IN S_EBELN
        AND WERKS IN S_WERKS.

  IF S_LIFNR IS NOT  INITIAL.
    LOOP AT S_LIFNR.
      RA_LIFNR-OPTION = S_LIFNR-OPTION.
      RA_LIFNR-SIGN = S_LIFNR-SIGN.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = S_LIFNR-LOW
        IMPORTING
          OUTPUT = RA_LIFNR-LOW.

      IF S_LIFNR-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = S_LIFNR-HIGH
          IMPORTING
            OUTPUT = RA_LIFNR-HIGH.
      ENDIF.
      APPEND RA_LIFNR TO RT_LIFNR.
    ENDLOOP.
    "
    SELECT SAFRA LIFNR EBELN WERKS BANFN NRO_SOL_CP PED_FORN WAERS
        FROM ZMMT0035
        APPENDING TABLE T_0035
          WHERE SAFRA IN S_SAFRA
            AND LIFNR IN RT_LIFNR
            AND EBELN IN S_EBELN
      AND WERKS IN S_WERKS.

*---> 04/07/2023 - Migração S4 - WS
  SORT T_0035 BY NRO_SOL_CP.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM T_0035 COMPARING NRO_SOL_CP.
  ENDIF.


  IF T_0035[] IS NOT INITIAL.
    SELECT WERKS NAME1
       FROM T001W
       INTO TABLE  T_T001W
         FOR ALL ENTRIES IN T_0035
           WHERE WERKS EQ T_0035-WERKS.

    LOOP AT T_0035 INTO WA_0035.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_0035-LIFNR
        IMPORTING
          OUTPUT = WA_0035-LIFNR.

      MODIFY T_0035 FROM WA_0035.
    ENDLOOP.



    SELECT LIFNR NAME1
       FROM LFA1
       INTO TABLE  T_LFA1
         FOR ALL ENTRIES IN T_0035
           WHERE LIFNR EQ T_0035-LIFNR.

    SELECT NRO_SOL_CP MATNR MWSKZ
      FROM ZMMT0037
      INTO TABLE T_0037
        FOR ALL ENTRIES IN T_0035
           WHERE NRO_SOL_CP EQ T_0035-NRO_SOL_CP.


    SELECT EBELN BUKRS BEDAT LIFNR WAERS WKURS
        FROM EKKO
        INTO TABLE T_EKKO
          FOR ALL ENTRIES IN T_0035
              WHERE EBELN EQ T_0035-EBELN
                AND BEDAT IN S_BEDAT
                AND LIFNR IN S_LIFNR.


    IF SY-SUBRC IS INITIAL.
      SELECT EBELN EBELP MATNR TXZ01 MATKL MENGE NETPR PEINH ELIKZ  WERKS MEINS MWSKZ
        FROM EKPO
        INTO TABLE T_EKPO
          FOR ALL ENTRIES IN T_EKKO
            WHERE EBELN EQ T_EKKO-EBELN
            AND LOEKZ	EQ ''.

      IF T_EKPO[] IS NOT INITIAL.
        SELECT MATNR MAKTX
          FROM MAKT
          INTO TABLE  T_MAKT
            FOR ALL ENTRIES IN T_EKPO
              WHERE MATNR EQ T_EKPO-MATNR.



        LOOP AT T_EKKO INTO WA_EKKO.
          CLEAR: WL_DATA.
          CONCATENATE WA_EKKO-BEDAT+6(2) WA_EKKO-BEDAT+4(2)  WA_EKKO-BEDAT(4) INTO WL_DATA.
          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
            EXPORTING
              INPUT  = WL_DATA
            IMPORTING
              OUTPUT = WA_EKKO-GDATU.


          MODIFY T_EKKO FROM WA_EKKO.
        ENDLOOP.


        SELECT KURST FCURR TCURR GDATU UKURS
          FROM TCURR
          INTO TABLE T_TCURR
           FOR ALL ENTRIES IN T_EKKO
            WHERE GDATU EQ T_EKKO-GDATU
              AND KURST EQ 'G'
              AND FCURR EQ 'USD'
              AND TCURR EQ 'BRL'.

        SELECT EBELN EBELP VGABE BELNR BUZEI GJAHR MENGE DMBTR WRBTR SHKZG BUDAT
           FROM EKBE
           INTO TABLE T_EKBE
             FOR ALL ENTRIES IN T_EKPO
               WHERE EBELN EQ T_EKPO-EBELN
                 AND EBELP EQ T_EKPO-EBELP
                 AND (   VGABE EQ '2'
                      OR VGABE EQ '4'
                      OR VGABE EQ 'C' ).
        T_EKBE_2[] = T_EKBE[].
        T_EKBE_4[] = T_EKBE[].
        DELETE T_EKBE_2 WHERE VGABE NE '2'.
        DELETE T_EKBE_4 WHERE VGABE NE '4' AND VGABE NE 'C'.
        DELETE T_EKBE_4 WHERE BUDAT NOT IN S_BUDAT.

        " Somente adiantamento - 19.09.2013 - ALRS
        IF T_EKBE_4[] IS NOT INITIAL.
          LOOP AT T_EKBE_4 INTO WA_EKBE_4.
            CLEAR: WA_EKKO,
                   WA_EKBE_AUX.
            READ TABLE T_EKKO INTO WA_EKKO
               WITH KEY EBELN = WA_EKBE_4-EBELN.

            WA_EKBE_AUX-BUKRS = WA_EKKO-BUKRS.
            WA_EKBE_AUX-GJAHR = WA_EKBE_4-GJAHR.
            WA_EKBE_AUX-EBELN = WA_EKBE_4-EBELN.
            WA_EKBE_AUX-EBELP = WA_EKBE_4-EBELP.
            WA_EKBE_AUX-BELNR = WA_EKBE_4-BELNR.
            WA_EKBE_AUX-DMBTR = WA_EKBE_4-DMBTR.
            WA_EKBE_AUX-WRBTR = WA_EKBE_4-WRBTR.
            WA_EKBE_AUX-SHKZG = WA_EKBE_4-SHKZG.
            WA_EKBE_AUX-VGABE = WA_EKBE_4-VGABE.

            CONCATENATE WA_EKBE_4-BELNR WA_EKBE_4-GJAHR INTO WA_EKBE_AUX-AWKEY.

            APPEND WA_EKBE_AUX TO T_EKBE_AUX.
          ENDLOOP.

          DELETE T_EKBE_AUX  WHERE BELNR+0(2) NE '15' AND BELNR+0(2) NE '20' AND VGABE NE 'C'. "somente pagamentos



        ENDIF.

        IF T_EKBE_2[] IS NOT INITIAL.
          SELECT *
            FROM RBKP
            INTO TABLE T_RBKP
            FOR ALL ENTRIES IN T_EKBE_2
            WHERE BELNR = T_EKBE_2-BELNR
            AND   GJAHR = T_EKBE_2-GJAHR.

          "Seleciona todos os pedidos da MIRO
          SELECT *
            FROM EKBE
            INTO CORRESPONDING FIELDS OF TABLE T_EKBE_GER
            FOR ALL ENTRIES IN T_EKBE_2
            WHERE BELNR = T_EKBE_2-BELNR
            AND   GJAHR = T_EKBE_2-GJAHR.


          LOOP AT T_EKBE_2 INTO WA_EKBE_2.
            CLEAR: WA_EKKO,
                   WA_EKBE_AUX.
            READ TABLE T_EKKO INTO WA_EKKO
               WITH KEY EBELN = WA_EKBE_2-EBELN.

            WA_EKBE_AUX-BUKRS = WA_EKKO-BUKRS.
            WA_EKBE_AUX-GJAHR = WA_EKBE_2-GJAHR.
            WA_EKBE_AUX-EBELN = WA_EKBE_2-EBELN.
            WA_EKBE_AUX-EBELP = WA_EKBE_2-EBELP.
            WA_EKBE_AUX-BELNR = WA_EKBE_2-BELNR.
            WA_EKBE_AUX-DMBTR = 0.
            WA_EKBE_AUX-WRBTR = WA_EKBE_2-WRBTR.
            WA_EKBE_AUX-SHKZG = WA_EKBE_2-SHKZG.

            CONCATENATE WA_EKBE_2-BELNR WA_EKBE_2-GJAHR INTO WA_EKBE_AUX-AWKEY.

            APPEND WA_EKBE_AUX TO T_EKBE_AUX.

          ENDLOOP.


          IF T_EKBE_AUX[] IS NOT INITIAL.
            SELECT BKPF~BUKRS BKPF~GJAHR BKPF~AWKEY BKPF~BELNR BKPF~KURS2
              FROM BKPF
              APPENDING TABLE T_BKPF
                FOR ALL ENTRIES IN T_EKBE_AUX
                    WHERE BUKRS EQ T_EKBE_AUX-BUKRS
                      AND GJAHR EQ T_EKBE_AUX-GJAHR
                      AND AWKEY EQ T_EKBE_AUX-AWKEY
                      AND BLART IN ('IN','RE', 'MC' )
                      AND BUDAT IN S_BUDAT.

            IF T_BKPF[] IS NOT INITIAL.
              " primeira busca MIRO paga
              SELECT BUKRS BELNR GJAHR BSCHL SHKZG UMSKZ AUGBL DMBE2 DMBTR AUGDT BUKRS BELNR GJAHR
                FROM BSAK
                INTO TABLE T_BSAK
                FOR ALL ENTRIES IN T_BKPF
                     WHERE BUKRS EQ T_BKPF-BUKRS
                       AND BELNR EQ T_BKPF-BELNR
                       AND GJAHR EQ T_BKPF-GJAHR.

            ENDIF.
          ENDIF.
        ENDIF.

        PERFORM BUSCA_BSAK.

        SELECT SPRAS MATKL WGBEZ60
            FROM T023T
            INTO TABLE T_T023T
               FOR ALL ENTRIES IN T_EKPO
                 WHERE SPRAS EQ SY-LANGU
                   AND MATKL EQ T_EKPO-MATKL.
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

  DATA: W_VALOR     TYPE KOMP-NETWR,
        V_DMBTR     TYPE BSIS-DMBTR,
        V_DMBE2     TYPE BSIS-DMBE2,

        V_DMBTR_M   TYPE BSIS-DMBTR,
        V_DMBE2_M   TYPE BSIS-DMBE2,

        V_DMBTR_A   TYPE BSIS-DMBTR,
        V_DMBE2_A   TYPE BSIS-DMBE2,

        V_DMBTR_D   TYPE BSIS-DMBTR,
        V_DMBE2_D   TYPE BSIS-DMBE2,

        V_DMBE2_VAR TYPE BSIS-DMBE2,
        V_DMBE2_2   TYPE BSIS-DMBE2,
        V_BUDAT     TYPE BKPF-BUDAT,
        WL_DATA(8),
        XACHOU(1).


  SORT: T_BKPF      BY BUKRS GJAHR AWKEY,
        T_BSAK      BY BUKRS_BK BELNR_BK GJAHR_BK,
        T_RBKP      BY BELNR GJAHR,
        T_EKBE_AUX  BY EBELN EBELP,
        T_EKBE_AUX  BY EBELN EBELP,
        T_EKPO      BY EBELN EBELP,
        T_TCURR     BY GDATU.

  LOOP AT T_EKBE INTO WA_EKBE.
    CLEAR WA_EKBE_TOTAL.
    IF WA_EKBE-VGABE = 'C'.
      CONTINUE.
    ENDIF.
    IF WA_EKBE-SHKZG EQ 'H'.
      MULTIPLY WA_EKBE-MENGE BY -1.
      MULTIPLY WA_EKBE-DMBTR BY -1.
      MULTIPLY WA_EKBE-WRBTR BY -1.
    ENDIF.

    WA_EKBE_TOTAL-EBELN = WA_EKBE-EBELN.
    WA_EKBE_TOTAL-EBELP = WA_EKBE-EBELP.
    WA_EKBE_TOTAL-TOT_QUAN  = WA_EKBE-MENGE.
    WA_EKBE_TOTAL-TOT_DMBTR = WA_EKBE-DMBTR.
    WA_EKBE_TOTAL-TOT_WRBTR = WA_EKBE-WRBTR.

    COLLECT WA_EKBE_TOTAL INTO T_EKBE_TOTAL.

  ENDLOOP.

  LOOP AT T_EKBE_TOTAL INTO WA_EKBE_TOTAL.
    IF WA_EKBE-VGABE = 'C'.
      CONTINUE.
    ENDIF.
    TRY.
        WA_EKBE_TOTAL-TX_CAMBIO = WA_EKBE_TOTAL-TOT_DMBTR / WA_EKBE_TOTAL-TOT_WRBTR.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
    MODIFY T_EKBE_TOTAL FROM WA_EKBE_TOTAL.
  ENDLOOP.

  LOOP AT T_EKBE_2 INTO WA_EKBE_2.
    CLEAR WA_EKBE_TOTAL.
    IF WA_EKBE_2-SHKZG EQ 'H'.
      MULTIPLY WA_EKBE_2-MENGE BY -1.
      MULTIPLY WA_EKBE_2-DMBTR BY -1.
      MULTIPLY WA_EKBE_2-WRBTR BY -1.
    ENDIF.

    WA_EKBE_TOTAL-EBELN = WA_EKBE_2-EBELN.
    WA_EKBE_TOTAL-EBELP = WA_EKBE_2-EBELP.
    WA_EKBE_TOTAL-TOT_QUAN  = WA_EKBE_2-MENGE.
    WA_EKBE_TOTAL-TOT_DMBTR = WA_EKBE_2-DMBTR.
    WA_EKBE_TOTAL-TOT_WRBTR = WA_EKBE_2-WRBTR.

    COLLECT WA_EKBE_TOTAL INTO T_EKBE_TOTAL_2.

  ENDLOOP.

  IF P_RESU_F IS NOT INITIAL.

    LOOP AT T_EKPO INTO WA_EKPO.
      CLEAR: WA_SAIDA, WA_0035, WA_EKBE_TOTAL, WA_BSIS_TOTAL, WA_LFA1, WA_MAKT, WA_T001W, WA_EKBE_TOTAL_2, WA_EKBE_AUX,
             WA_BKPF, WA_BSAK, WA_EKKO.
      READ TABLE T_0035 INTO WA_0035
        WITH KEY EBELN = WA_EKPO-EBELN.

      IF SY-SUBRC IS INITIAL.

        READ TABLE T_MAKT  INTO WA_MAKT
          WITH KEY MATNR = WA_EKPO-MATNR.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_0035-WERKS.

        READ TABLE T_LFA1    INTO WA_LFA1
          WITH KEY LIFNR = WA_0035-LIFNR.

        CLEAR: XACHOU, V_BUDAT.


        PERFORM F_BUSCA_PAGTO USING WA_EKPO-EBELN WA_EKPO-EBELP CHANGING V_DMBE2_2 V_DMBTR   V_DMBTR_D V_DMBE2_D V_DMBTR_M V_DMBE2_M  V_DMBE2_A V_DMBTR_A V_BUDAT.

        READ TABLE T_T023T INTO WA_T023T
          WITH KEY MATKL = WA_EKPO-MATKL.

        READ TABLE T_0037 INTO WA_0037
             WITH KEY NRO_SOL_CP = WA_0035-NRO_SOL_CP.

        WA_SAIDA-NRO_SOL_CP   = WA_0035-NRO_SOL_CP.
        WA_SAIDA-MWSKZ   = WA_EKPO-MWSKZ.

        WA_SAIDA-SAFRA   = WA_0035-SAFRA.
        WA_SAIDA-WERKS   = WA_0035-WERKS.
        WA_SAIDA-WAERS   = WA_0035-WAERS.
        WA_SAIDA-NAME1_W = WA_T001W-NAME1.
        WA_SAIDA-LIFNR   = WA_LFA1-LIFNR.
        WA_SAIDA-NAME1_L = WA_LFA1-NAME1.
        WA_SAIDA-MATNR   = WA_EKPO-MATNR.
        WA_SAIDA-MAKTX   = WA_MAKT-MAKTX.
        WA_SAIDA-MATKL   = WA_EKPO-MATKL.
        WA_SAIDA-WGBEZ60 = WA_T023T-WGBEZ60.
        WA_SAIDA-EBELN   = WA_EKPO-EBELN.
        WA_SAIDA-BEDAT   = WA_EKKO-BEDAT.

        PERFORM R_IMPOSTO_ITEM USING     WA_EKKO-LIFNR
                                         WA_EKPO-WERKS
                                         WA_EKPO-EBELP
                                         WA_EKPO-EBELN
                               CHANGING  W_VALOR .

        WA_SAIDA-DESC_VLR       = V_DMBTR_D.                             "Desconto R$
        WA_SAIDA-DESC_VLR_US    = V_DMBE2_D.                             "Desconto US$

        PERFORM F_CALCULO USING W_VALOR V_DMBE2_2 V_DMBTR V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A  CHANGING WA_SAIDA.


        APPEND WA_SAIDA TO T_SAIDA.

      ENDIF.

    ENDLOOP.
  ELSEIF P_RECEB IS NOT INITIAL.

    LOOP AT T_EKPO INTO WA_EKPO.
      CLEAR: WA_SAIDA, WA_0035, WA_EKBE_TOTAL, WA_LFA1, WA_MAKT, WA_T001W, WA_T023T,
             WA_BKPF, WA_EKKO.
      READ TABLE T_0035 INTO WA_0035
        WITH KEY EBELN = WA_EKPO-EBELN.

      IF SY-SUBRC IS INITIAL.

        READ TABLE T_MAKT  INTO WA_MAKT
          WITH KEY MATNR = WA_EKPO-MATNR.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_0035-WERKS.

        READ TABLE T_LFA1    INTO WA_LFA1
          WITH KEY LIFNR = WA_0035-LIFNR.


        READ TABLE T_EKKO INTO WA_EKKO
          WITH KEY EBELN = WA_EKPO-EBELN.

        READ TABLE T_T023T INTO WA_T023T
          WITH KEY MATKL = WA_EKPO-MATKL.

        READ TABLE T_0037 INTO WA_0037
       WITH KEY NRO_SOL_CP = WA_0035-NRO_SOL_CP.

        WA_SAIDA_2-NRO_SOL_CP   = WA_0035-NRO_SOL_CP.
        WA_SAIDA_2-MWSKZ   = WA_0037-MWSKZ.

        WA_SAIDA_2-SAFRA      = WA_0035-SAFRA.
        WA_SAIDA_2-WERKS      = WA_0035-WERKS.
        WA_SAIDA_2-WAERS      = WA_0035-WAERS.
        WA_SAIDA_2-NAME1_W    = WA_T001W-NAME1.
        WA_SAIDA_2-LIFNR      = WA_LFA1-LIFNR.
        WA_SAIDA_2-NAME1_L    = WA_LFA1-NAME1.
        WA_SAIDA_2-MATNR      = WA_MAKT-MATNR.
        WA_SAIDA_2-MEINS      = WA_EKPO-MEINS.
        WA_SAIDA_2-MAKTX      = WA_MAKT-MAKTX.
        WA_SAIDA_2-MATKL      = WA_T023T-MATKL.
        WA_SAIDA_2-WGBEZ60    = WA_T023T-WGBEZ60.
        WA_SAIDA_2-EBELN      = WA_EKKO-EBELN.
        WA_SAIDA_2-BEDAT      = WA_EKKO-BEDAT.

        CLEAR XACHOU.
        PERFORM F_BUSCA_PAGTO USING WA_EKPO-EBELN WA_EKPO-EBELP CHANGING V_DMBE2_2 V_DMBTR   V_DMBTR_D V_DMBE2_D V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A V_BUDAT.

        PERFORM R_IMPOSTO_ITEM USING     WA_EKKO-LIFNR
                                         WA_EKPO-WERKS
                                         WA_EKPO-EBELP
                                         WA_EKPO-EBELN
                               CHANGING  W_VALOR .



        PERFORM F_CALCULO USING W_VALOR V_DMBE2_2 V_DMBTR V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A  CHANGING WA_SAIDA.

        WA_SAIDA_2-VLR_TOT    = WA_SAIDA-RECEBIDO_VLR.
        WA_SAIDA_2-VLR_TOT_US = WA_SAIDA-RECEBIDO_VLR_US.

        WA_SAIDA_2-RECEBIDO = WA_EKBE_TOTAL-TOT_QUAN.

        IF WA_EKPO-ELIKZ EQ 'X'.
          CLEAR WA_SAIDA_2-ARECEBER.
        ELSE.
          WA_SAIDA_2-ARECEBER = WA_EKPO-MENGE - WA_EKBE_TOTAL-TOT_QUAN.
        ENDIF.

        SELECT SUM( LBLAB )
          FROM  MSLB
          INTO @DATA(W_LBLAB)
          WHERE WERKS = @WA_SAIDA_2-WERKS
          AND   MATNR = @WA_SAIDA_2-MATNR.

        WA_SAIDA_2-LBLAB = W_LBLAB.
        APPEND WA_SAIDA_2 TO T_SAIDA_2.
      ENDIF.
    ENDLOOP.
  ELSEIF P_PED_PG IS NOT INITIAL.

    LOOP AT T_EKPO INTO WA_EKPO.
      CLEAR: WA_SAIDA, WA_0035, WA_BSIS_TOTAL, WA_LFA1, WA_MAKT, WA_T001W, WA_T023T,
             WA_BKPF, WA_EKKO.
      READ TABLE T_0035 INTO WA_0035
        WITH KEY EBELN = WA_EKPO-EBELN.

      IF SY-SUBRC IS INITIAL.

        READ TABLE T_MAKT  INTO WA_MAKT
          WITH KEY MATNR = WA_EKPO-MATNR.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_0035-WERKS.

        READ TABLE T_LFA1    INTO WA_LFA1
          WITH KEY LIFNR = WA_0035-LIFNR.

        READ TABLE T_EKKO INTO WA_EKKO
          WITH KEY EBELN = WA_EKPO-EBELN.

        READ TABLE T_T023T INTO WA_T023T
          WITH KEY MATKL = WA_EKPO-MATKL.


        CLEAR XACHOU.
        CLEAR V_BUDAT.
        PERFORM F_BUSCA_PAGTO USING WA_EKPO-EBELN WA_EKPO-EBELP CHANGING V_DMBE2_2 V_DMBTR   V_DMBTR_D V_DMBE2_D V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A V_BUDAT.

        READ TABLE T_0037 INTO WA_0037
       WITH KEY NRO_SOL_CP = WA_0035-NRO_SOL_CP.

        WA_SAIDA_2-NRO_SOL_CP   = WA_0035-NRO_SOL_CP.
        WA_SAIDA_2-MWSKZ   = WA_0037-MWSKZ.

        WA_SAIDA_2-SAFRA      = WA_0035-SAFRA.
        WA_SAIDA_2-WERKS      = WA_0035-WERKS.
        WA_SAIDA_2-WAERS      = WA_0035-WAERS.
        WA_SAIDA_2-NAME1_W    = WA_T001W-NAME1.
        WA_SAIDA_2-LIFNR      = WA_LFA1-LIFNR.
        WA_SAIDA_2-NAME1_L    = WA_LFA1-NAME1.
        WA_SAIDA_2-MATNR      = WA_MAKT-MATNR.
        WA_SAIDA_2-MEINS      = WA_EKPO-MEINS.
        WA_SAIDA_2-MAKTX      = WA_MAKT-MAKTX.
        WA_SAIDA_2-MATKL      = WA_T023T-MATKL.
        WA_SAIDA_2-WGBEZ60    = WA_T023T-WGBEZ60.
        WA_SAIDA_2-EBELN      = WA_EKKO-EBELN.
        WA_SAIDA_2-BEDAT      = WA_EKKO-BEDAT.
        WA_SAIDA_2-BUDAT      = V_BUDAT.

        PERFORM F_CALCULO USING W_VALOR V_DMBE2_2 V_DMBTR V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A  CHANGING WA_SAIDA.

        WA_SAIDA_2-VLR_TOT    = WA_SAIDA-PAGO_VLR .
        WA_SAIDA_2-VLR_TOT_US = WA_SAIDA-PAGO_VLR_US.

        APPEND WA_SAIDA_2 TO T_SAIDA_2.
      ENDIF.
    ENDLOOP.
  ELSEIF P_REL_IN IS NOT INITIAL.
    LOOP AT T_EKPO INTO WA_EKPO.
      CLEAR: WA_SAIDA, WA_0035, WA_EKBE_TOTAL, WA_BSIS_TOTAL, WA_LFA1, WA_MAKT, WA_T001W, WA_EKBE_TOTAL_2, WA_EKBE_AUX,
             WA_BKPF, WA_EKKO.
      READ TABLE T_0035 INTO WA_0035
        WITH KEY EBELN = WA_EKPO-EBELN.

      IF SY-SUBRC IS INITIAL.

        READ TABLE T_MAKT  INTO WA_MAKT
          WITH KEY MATNR = WA_EKPO-MATNR.

        READ TABLE T_T001W INTO WA_T001W
          WITH KEY WERKS = WA_0035-WERKS.

        READ TABLE T_LFA1    INTO WA_LFA1
          WITH KEY LIFNR = WA_0035-LIFNR.

        READ TABLE T_T023T INTO WA_T023T
          WITH KEY MATKL = WA_EKPO-MATKL.

        READ TABLE T_EKBE_TOTAL INTO WA_EKBE_TOTAL
          WITH KEY EBELN = WA_EKPO-EBELN
                   EBELP = WA_EKPO-EBELP.

        READ TABLE T_EKBE_AUX INTO WA_EKBE_AUX
          WITH KEY EBELN = WA_EKPO-EBELN
                   EBELP = WA_EKPO-EBELP.

        READ TABLE T_EKBE_TOTAL_2 INTO WA_EKBE_TOTAL_2
          WITH KEY EBELN = WA_EKPO-EBELN
                   EBELP = WA_EKPO-EBELP.

        READ TABLE T_EKKO INTO WA_EKKO
          WITH KEY EBELN = WA_EKPO-EBELN.

        READ TABLE T_0037 INTO WA_0037
       WITH KEY NRO_SOL_CP = WA_0035-NRO_SOL_CP.

        WA_SAIDA_3-NRO_SOL_CP   = WA_0035-NRO_SOL_CP.
        WA_SAIDA_3-MWSKZ   = WA_0037-MWSKZ.

        WA_SAIDA_3-SAFRA   = WA_0035-SAFRA.
        WA_SAIDA_3-WERKS   = WA_0035-WERKS.
        WA_SAIDA_3-WAERS   = WA_0035-WAERS.
        WA_SAIDA_3-NAME1_W = WA_T001W-NAME1.
        WA_SAIDA_3-LIFNR   = WA_LFA1-LIFNR.
        WA_SAIDA_3-NAME1_L = WA_LFA1-NAME1.
        WA_SAIDA_3-MATNR   = WA_EKPO-MATNR.
        WA_SAIDA_3-MAKTX   = WA_MAKT-MAKTX.
        WA_SAIDA_3-MATKL   = WA_EKPO-MATKL.
        WA_SAIDA_3-MEINS   = WA_EKPO-MEINS.
        WA_SAIDA_3-WGBEZ60 = WA_T023T-WGBEZ60.
        WA_SAIDA_3-EBELN   = WA_EKPO-EBELN.
        WA_SAIDA_3-BEDAT   = WA_EKKO-BEDAT.

        IF WA_EKBE_TOTAL-TX_CAMBIO IS INITIAL.
          WA_SAIDA_3-UKURS   = WA_TCURR-UKURS.
        ELSE.
          WA_SAIDA_3-UKURS   = WA_EKBE_TOTAL-TX_CAMBIO.
        ENDIF.

        IF WA_EKBE_TOTAL-TOT_QUAN GT WA_EKPO-MENGE
        OR WA_EKPO-ELIKZ EQ 'X'.
          WA_SAIDA_3-MENGE   = WA_EKBE_TOTAL-TOT_QUAN.
          WA_SAIDA_3-VALOR_US = WA_EKBE_TOTAL-TOT_WRBTR.
        ELSE.
          WA_SAIDA_3-MENGE   = WA_EKPO-MENGE.
          WA_SAIDA_3-VALOR_US = ( WA_EKPO-MENGE * ( WA_EKPO-NETPR / WA_EKPO-PEINH ) ).
        ENDIF.

        WA_SAIDA_3-RECEBIDO = WA_EKBE_TOTAL-TOT_QUAN.
        IF WA_EKPO-ELIKZ = 'X'.
          CLEAR WA_SAIDA_3-ARECEBER.
        ELSE.
          WA_SAIDA_3-ARECEBER = WA_EKPO-MENGE - WA_EKBE_TOTAL-TOT_QUAN.
        ENDIF.

        " Pega o valor do pedido com impostos (substitui o calculo acima) ALRS
        PERFORM R_IMPOSTO_ITEM USING    WA_EKKO-LIFNR
                                        WA_EKPO-WERKS
                                        WA_EKPO-EBELP
                                        WA_EKPO-EBELN
                              CHANGING  W_VALOR .

        CLEAR: V_DMBE2_2, V_DMBTR.
        PERFORM F_CALCULO USING W_VALOR V_DMBE2_2 V_DMBTR V_DMBTR_M V_DMBE2_M V_DMBE2_A V_DMBTR_A  CHANGING WA_SAIDA.

        WA_SAIDA_3-VALOR_US  = WA_SAIDA-PEDIDO_VLR_US.
        WA_SAIDA_3-VALOR     = WA_SAIDA-PEDIDO_VLR.

        APPEND WA_SAIDA_3 TO T_SAIDA_3.

      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  IF P_RESU_F EQ 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = V_REPORT
        I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND' "sem 2º click
        IT_FIELDCAT             = ESTRUTURA[]
        I_SAVE                  = 'A'
        IT_EVENTS               = EVENTS
        IS_PRINT                = T_PRINT
      TABLES
        T_OUTTAB                = T_SAIDA.

  ELSEIF P_RECEB  EQ 'X'
      OR P_PED_PG EQ 'X'.
*      OR P_PED_PG EQ 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = V_REPORT
        I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND2' "sem 2º click
        IT_FIELDCAT             = ESTRUTURA[]
        I_SAVE                  = 'A'
        IT_EVENTS               = EVENTS
        IS_PRINT                = T_PRINT
      TABLES
        T_OUTTAB                = T_SAIDA_2.

  ELSEIF P_REL_IN IS NOT INITIAL.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = V_REPORT
        I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND3' "sem 2º click
        IT_FIELDCAT             = ESTRUTURA[]        "I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND' "sem 2º click
        I_SAVE                  = 'A'
        IT_EVENTS               = EVENTS
        IS_PRINT                = T_PRINT
      TABLES
        T_OUTTAB                = T_SAIDA_3.

  ENDIF.

ENDFORM.                    " IMPRIMIR_DADOS


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM XUSER_COMMAND USING R_UCOMM     LIKE SY-UCOMM          "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.


  READ TABLE T_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.

  CASE RS_SELFIELD-FIELDNAME.

    WHEN 'EBELN'.
      SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    "XUSER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  XUSER_COMMAND2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM XUSER_COMMAND2 USING R_UCOMM     LIKE SY-UCOMM         "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.


  READ TABLE T_SAIDA_2 INTO WA_SAIDA_2 INDEX RS_SELFIELD-TABINDEX.

  CASE RS_SELFIELD-FIELDNAME.

    WHEN 'EBELN'.
      SET PARAMETER ID 'BES' FIELD WA_SAIDA_2-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    "XUSER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  XUSER_COMMAND2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM XUSER_COMMAND3 USING R_UCOMM     LIKE SY-UCOMM         "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.


  READ TABLE T_SAIDA_3 INTO WA_SAIDA_3 INDEX RS_SELFIELD-TABINDEX.

  CASE RS_SELFIELD-FIELDNAME.

    WHEN 'EBELN'.
      SET PARAMETER ID 'BES' FIELD WA_SAIDA_3-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    "XUSER_COMMAND

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
* para tira duplo click          SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
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
  IF P_RESU_F EQ 'X'.
    PERFORM MONTAR_ESTRUTURA USING:
          1  'ZMMT0035'   'NRO_SOL_CP' 'T_SAIDA' 'NRO_SOL_CP'  'Nr.Solicitação'  ' ' ,
          1  'ZMMT0035'   'SAFRA'      'T_SAIDA' 'SAFRA'  'Safra'  ' ' ,
          2  'ZMMT0035'   'WERKS' 'T_SAIDA' 'WERKS'  ' '  ' ' ,
          3  'T001W'      'NAME1' 'T_SAIDA' 'NAME1_W'  ' '  ' ' ,
          4  'ZMMT0035'   'LIFNR' 'T_SAIDA' 'LIFNR'  ' '  ' ' ,
          5  'LFA1'       'NAME1' 'T_SAIDA' 'NAME1_L'  ' '  ' ' ,
          6  'EKPO'       'MATNR' 'T_SAIDA' 'MATNR'  ' '  ' ' ,
          7  'MAKT'       'MAKTX' 'T_SAIDA' 'MAKTX'  ' '  ' ' ,
          8  'EKPO'       'MATKL' 'T_SAIDA' 'MATKL'  ' '  ' ' ,
          9  'T023T'      'WGBEZ60' 'T_SAIDA' 'WGBEZ60'            'Grupo de Mercadorias'  ' ' ,
          10 'ZMMT0035'   'EBELN' 'T_SAIDA' 'EBELN'  ' '  ' ' ,
          11 'ZMMT0037'   'MWSKZ' 'T_SAIDA' 'MWSKZ'  'IVA'  ' ' ,
          12 'EKKO'       'BEDAT' 'T_SAIDA' 'BEDAT'       ' '  ' ' ,
          13 'ZMMT0035'   'WAERS' 'T_SAIDA' 'WAERS'  ' '  ' ' ,
          14 'EKBE'       'WRBTR' 'T_SAIDA' 'PEDIDO_VLR'           'Pedido R$'  ' ' ,
          15 'EKBE'       'WRBTR' 'T_SAIDA' 'PEDIDO_VLR_US'        'Pedido US$'   ' ' ,
          16 'EKBE'       'WRBTR' 'T_SAIDA' 'PEDIDO_VLR_A'         'Recebido R$'  ' ' ,
          17 'EKBE'       'WRBTR' 'T_SAIDA' 'PEDIDO_VLRA_U'        'Recebido US$' ' ' ,
          18 'BSIS'       'DMBTR' 'T_SAIDA' 'PAGO_VLR'             'Pago R$'    ' ' ,
          19 'BSIS'       'DMBE2' 'T_SAIDA' 'PAGO_VLR_US'          'Pago US$'  ' ' ,
          20 'BSIS'       'DMBE2' 'T_SAIDA' 'VARI_PAGO'            'Variação R$'  ' ' ,
          21 'BSIS'       'DMBTR' 'T_SAIDA' 'SDO_A_PAGAR_VLR'      'Sdo a Pagar R$'  ' ' ,
          22 'BSIS'       'DMBE2' 'T_SAIDA' 'SDO_A_PAGAR_VLR_US'   'Sdo a Pagar US$'  ' ' ,
          23 'BSIS'       'DMBTR' 'T_SAIDA' 'SDO_A_RECEBER_VLR'    'Sdo a Receber R$'  ' ' ,
          24 'BSIS'       'DMBE2' 'T_SAIDA' 'SDO_A_RECEBER_VLR_US' 'Sdo a Receber US$'  ' ' ,
          25 'BSIS'       'DMBE2' 'T_SAIDA' 'DESC_VLR_US'          'Desconto US$'  ' ' ,
          26 'BSIS'       'DMBTR' 'T_SAIDA' 'DESC_VLR   '          'Desconto RS$'  ' ' ,
          27 'BSIS'       'DMBE2' 'T_SAIDA' 'ADTO_VLR_US'          'Adto US$'  ' ' ,
          28 'BSIS'       'DMBTR' 'T_SAIDA' 'ADTO_VLR   '          'Adto RS$'  ' ' .
  ELSEIF  P_PED_PG EQ 'X'.
    PERFORM MONTAR_ESTRUTURA USING:
          1  'ZMMT0035'   'NRO_SOL_CP' 'T_SAIDA_2' 'NRO_SOL_CP'  'Nr.Solicitação'  ' ' ,
          1  'ZMMT0035'   'SAFRA'   'T_SAIDA_2' 'SAFRA'       'Safra'  ' ' ,
          2  'ZMMT0035'   'WERKS'   'T_SAIDA_2' 'WERKS'       ' '  ' ' ,
          3  'T001W'      'NAME1'   'T_SAIDA_2' 'NAME1_W'     ' '  ' ' ,
          4  'ZMMT0035'   'LIFNR'   'T_SAIDA_2' 'LIFNR'       ' '  ' ' ,
          5  'LFA1'       'NAME1'   'T_SAIDA_2' 'NAME1_L'     ' '  ' ' ,
          6  'EKPO'       'MATNR'   'T_SAIDA_2' 'MATNR'       ' '  ' ' ,
          7  'MAKT'       'MAKTX'   'T_SAIDA_2' 'MAKTX'       ' '  ' ' ,
          8  'EKPO'       'MATKL'   'T_SAIDA_2' 'MATKL'       ' '  ' ' ,
          9  'T023T'      'WGBEZ60' 'T_SAIDA_2' 'WGBEZ60'     'Grupo de Mercadorias'  ' ' ,
          10 'ZMMT0035'   'EBELN'   'T_SAIDA_2' 'EBELN'       ' '  ' ' ,
          10 'ZMMT0037'   'MWSKZ'   'T_SAIDA_2' 'MWSKZ'       'IVA'  ' ' ,
          10 'EKKO'       'BEDAT'   'T_SAIDA_2' 'BEDAT'       ' '  ' ' ,
          11 'ZMMT0035'   'WAERS'   'T_SAIDA_2' 'WAERS'       ' '  ' ' ,
          12 ' '          ' '       'T_SAIDA_2' 'BUDAT'       'Dt.Pagto'  ' ' ,
          12 'BSIS'       'DMBTR'   'T_SAIDA_2' 'VLR_TOT'     'Valor R$'  ' ' ,
          12 'BSIS'       'DMBE2'   'T_SAIDA_2' 'VLR_TOT_US'  'Valor US$'  ' ' .
  ELSEIF P_RECEB  EQ 'X'.
    PERFORM MONTAR_ESTRUTURA USING:
          1  'ZMMT0035'   'NRO_SOL_CP' 'T_SAIDA_2' 'NRO_SOL_CP'  'Nr.Solicitação'  ' ' ,
          1  'ZMMT0035'   'SAFRA'   'T_SAIDA_2' 'SAFRA'       'Safra'  ' ' ,
          2  'ZMMT0035'   'WERKS'   'T_SAIDA_2' 'WERKS'       ' '  ' ' ,
          3  'T001W'      'NAME1'   'T_SAIDA_2' 'NAME1_W'     ' '  ' ' ,
          4  'ZMMT0035'   'LIFNR'   'T_SAIDA_2' 'LIFNR'       ' '  ' ' ,
          5  'LFA1'       'NAME1'   'T_SAIDA_2' 'NAME1_L'     ' '  ' ' ,
          6  'EKPO'       'MATNR'   'T_SAIDA_2' 'MATNR'       ' '  ' ' ,
          7  'MAKT'       'MAKTX'   'T_SAIDA_2' 'MAKTX'       ' '  ' ' ,
          8  'EKPO'       'MATKL'   'T_SAIDA_2' 'MATKL'       ' '  ' ' ,
          9  'T023T'      'WGBEZ60' 'T_SAIDA_2' 'WGBEZ60'     'Grupo de Mercadorias'  ' ' ,
          10 'ZMMT0035'   'EBELN'   'T_SAIDA_2' 'EBELN'       ' '  ' ' ,
          10 'ZMMT0037'   'MWSKZ'   'T_SAIDA_2' 'MWSKZ'       'IVA'  ' ' ,
          10 'EKKO'       'BEDAT'   'T_SAIDA_2' 'BEDAT'       ' '  ' ' ,
          11 'ZMMT0035'   'WAERS'   'T_SAIDA_2' 'WAERS'       ' '  ' ' ,
          12 ' '          ' '       'T_SAIDA_2' 'BUDAT'       'Dt.Pagto'  ' ' ,
          12 'BSIS'       'DMBTR'   'T_SAIDA_2' 'VLR_TOT'     'Valor R$'  ' ' ,
          12 'BSIS'       'DMBE2'   'T_SAIDA_2' 'VLR_TOT_US'  'Valor US$'  ' ' ,
          16 'EKPO'       'MENGE'   'T_SAIDA_2' 'RECEBIDO'    'Recebido'   ' ' ,
          17 'EKPO'       'MENGE'   'T_SAIDA_2' 'ARECEBER'    'A Receber'  ' ' ,
          18 'EKPO'       'MENGE'   'T_SAIDA_2' 'LBLAB'       'Armazenagem'  ' ' .


  ELSEIF P_REL_IN IS NOT INITIAL.
    PERFORM MONTAR_ESTRUTURA USING:
         1  'ZMMT0035'   'NRO_SOL_CP' 'T_SAIDA' 'NRO_SOL_CP'  'Nr.Solicitação'  ' ' ,
         1  'ZMMT0035'   'SAFRA' 'T_SAIDA' 'SAFRA'  'Safra'  ' ' ,
         2  'ZMMT0035'   'WERKS' 'T_SAIDA' 'WERKS'  ' '  ' ' ,
         3  'T001W'      'NAME1' 'T_SAIDA' 'NAME1_W'  ' '  ' ' ,
         4  'ZMMT0035'   'LIFNR' 'T_SAIDA' 'LIFNR'  ' '  ' ' ,
         5  'LFA1'       'NAME1' 'T_SAIDA' 'NAME1_L'  ' '  ' ' ,
         6  'EKPO'       'MATNR' 'T_SAIDA' 'MATNR'  ' '  ' ' ,
         7  'MAKT'       'MAKTX' 'T_SAIDA' 'MAKTX'  ' '  ' ' ,
         8  'EKPO'       'MATKL' 'T_SAIDA' 'MATKL'  ' '  ' ' ,
         9  'T023T'      'WGBEZ60' 'T_SAIDA' 'WGBEZ60'  'Grupo de Mercadorias'  ' ' ,
         10 'ZMMT0035'   'EBELN' 'T_SAIDA' 'EBELN'  ' '  ' ' ,
         10 'ZMMT0037'   'MWSKZ' 'T_SAIDA' 'MWSKZ'  'IVA'  ' ' ,
         10 'EKKO'       'BEDAT' 'T_SAIDA' 'BEDAT'  ' '  ' ' ,
         11 'ZMMT0035'   'WAERS' 'T_SAIDA' 'WAERS'  ' '  ' ' ,
         12 'TCURR'      'UKURS' 'T_SAIDA' 'UKURS'  ' '  ' ' ,
         13 'EKPO'       'MENGE' 'T_SAIDA' 'MENGE'  ' '  ' ' ,
         13 'EKPO'       'MEINS' 'T_SAIDA' 'MEINS'  ' '  ' ' ,
         14 'BSIS'       'DMBE2' 'T_SAIDA' 'VALOR'          'Valor'  ' ' ,
         15 'BSIS'       'DMBTR' 'T_SAIDA' 'VALOR_US'       'Valor US$'  ' ' ,
         16 'EKPO'       'MENGE' 'T_SAIDA' 'RECEBIDO'       'Recebido'   ' ' ,
         17 'EKPO'       'MENGE' 'T_SAIDA' 'ARECEBER'       'A Receber'  ' ' .

  ENDIF.

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

  CLEAR WA_ESTRUTURA.
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
  IF P_FIELD = 'EBELN'.
    WA_ESTRUTURA-HOTSPOT = 'X'.
  ENDIF.

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

  DATA:        W_DATA(10),
               W_HORA(10),
               W_TEXTO(40),

               CABEC        TYPE C LENGTH 100.



  V_REPORT = SY-REPID.

  W_TEXTO = 'Compra de insumos'.
  IF P_PED_PG = 'X'.
    W_TEXTO = 'Resumo Pedido Pagos'.
  ENDIF.
  IF P_RECEB  = 'X'.
    W_TEXTO = 'Recebimento'.
  ENDIF.
  IF P_REL_IN	= 'X'.
    W_TEXTO = 'Relatório de Compras Insumos'.
  ENDIF.
  IF P_RESU_F	= 'X'.
    W_TEXTO = 'Resumo Financeiro'.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO.


  W_TEXTO = 'Safra    :'.
  CONCATENATE W_TEXTO S_SAFRA-LOW    INTO CABEC  SEPARATED BY SPACE.
  IF S_SAFRA-HIGH IS NOT INITIAL.
    CONCATENATE CABEC 'à' S_SAFRA-HIGH  INTO CABEC SEPARATED BY SPACE.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.

  IF S_LIFNR-LOW IS NOT INITIAL.
    W_TEXTO = 'Fornecedor :'.
    CONCATENATE W_TEXTO S_LIFNR-LOW    INTO CABEC  SEPARATED BY SPACE.
    IF S_SAFRA-HIGH IS NOT INITIAL.
      CONCATENATE CABEC 'à' S_LIFNR-HIGH  INTO CABEC SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.


  IF S_WERKS-LOW IS NOT INITIAL.
    W_TEXTO = 'Centro    :'.
    CONCATENATE W_TEXTO S_WERKS-LOW    INTO CABEC  SEPARATED BY SPACE.
    IF S_WERKS-HIGH IS NOT INITIAL.
      CONCATENATE CABEC 'à' S_WERKS-HIGH  INTO CABEC SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.

  IF S_MATNR-LOW IS NOT INITIAL.
    W_TEXTO = 'Material    :'.
    CONCATENATE W_TEXTO S_MATNR-LOW    INTO CABEC  SEPARATED BY SPACE.
    IF S_MATNR-HIGH IS NOT INITIAL.
      CONCATENATE CABEC 'à' S_MATNR-HIGH  INTO CABEC SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.


  IF S_EBELN-LOW IS NOT INITIAL.
    W_TEXTO = 'Doc de compras :'.
    CONCATENATE W_TEXTO S_EBELN-LOW    INTO CABEC  SEPARATED BY SPACE.
    IF S_EBELN-HIGH IS NOT INITIAL.
      CONCATENATE CABEC 'à' S_EBELN-HIGH  INTO CABEC SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.

  IF S_BEDAT-LOW IS NOT INITIAL.
    W_TEXTO = 'Data do documento:'.
    CONCATENATE S_BEDAT-LOW+6(2) '.' S_BEDAT-LOW+4(2) '.' S_BEDAT-LOW+0(4) INTO W_DATA.
    CONCATENATE W_TEXTO W_DATA    INTO CABEC  SEPARATED BY SPACE.
    IF S_BEDAT-HIGH IS NOT INITIAL.
      CONCATENATE S_BEDAT-HIGH+6(2) '.' S_BEDAT-HIGH+4(2) '.' S_BEDAT-HIGH+0(4) INTO W_DATA.
      CONCATENATE CABEC 'à' W_DATA  INTO CABEC SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.


ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  SEARCH_SAFRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1013   text
*      <--P_S_SAFRA_LOW  text
*----------------------------------------------------------------------*
FORM SEARCH_SAFRA  USING    VALUE(P_1013)
                   CHANGING P_SAFRA.

  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE,
        VSAFRA        TYPE ZSDT0044-SAFRA.

  DATA: BEGIN OF TL_SAFRA OCCURS 0,
          SAFRA4 TYPE ZSDT0044-SAFRA,
          SAFRA  TYPE ZMMT0035-SAFRA,
        END OF TL_SAFRA.

  SELECT SAFRA
    FROM ZSDT0044
    INTO TABLE TL_SAFRA
    ORDER BY SAFRA ASCENDING.

  LOOP AT TL_SAFRA.
    VSAFRA = TL_SAFRA-SAFRA4.
    ADD 1 TO VSAFRA .
    CONCATENATE TL_SAFRA-SAFRA4 '/' VSAFRA INTO TL_SAFRA-SAFRA.
    MODIFY TL_SAFRA INDEX SY-TABIX TRANSPORTING SAFRA.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = P_1013
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = P_1013
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_SAFRA
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.

ENDFORM.                    " SEARCH_SAFRA
*&---------------------------------------------------------------------*
*&      Form  R_IMPOSTO_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM R_IMPOSTO_ITEM USING     W_LIFNR
                              W_WERKS
                              W_EBELP
                              W_EBELN
                    CHANGING  W_VALOR .
  DATA: WA_ITE  LIKE MEPOITEM.
  CLEAR WA_ITE.
  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
    EXPORTING
      IM_EBELP = W_EBELP                                    "'00010'
    IMPORTING
      EX_ITEM  = WA_ITE
    EXCEPTIONS
      FAILURE  = 1
      OTHERS   = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  DATA: BEGIN OF T_KONV OCCURS 0.
          INCLUDE STRUCTURE KONV.
        DATA: END OF T_KONV.

  TYPES: TY_KONV TYPE TABLE OF KOMV.

  FIELD-SYMBOLS: <WMWST> TYPE ANY,
                 <LFA1>  TYPE LFA1,
                 <EKPO>  TYPE EKPO,
                 <EK2>   TYPE EKPO,
                 <EKKO>  TYPE EKKO,
                 <VORGA> TYPE ANY,
                 <KONV>  TYPE TY_KONV,
                 <CVA>   TYPE ANY.

  ASSIGN ('(SAPLMEPO)ekpo') TO <EKPO>.
  ASSIGN ('(SAPLMEPO)ekko') TO <EKKO>.
  ASSIGN ('(SAPLMEPO)lfa1') TO <LFA1>.


  SELECT SINGLE * FROM EKPO INTO <EKPO>
    WHERE EBELN = W_EBELN AND
          EBELP = W_EBELP AND
          LOEKZ = SPACE.


  SELECT SINGLE * FROM EKKO INTO <EKKO>
    WHERE EBELN = W_EBELN.

  SELECT SINGLE * FROM LFA1 INTO <LFA1>
    WHERE LIFNR = <EKKO>-LIFNR.

  TRY.

CL_PRC_RESULT_FACTORY=>GET_INSTANCE( )->GET_PRC_RESULT( )->GET_PRICE_ELEMENT_DB(
  EXPORTING IT_SELECTION_ATTRIBUTE = VALUE #(
 ( fieldname = 'KNUMV' value = <EKKO>-KNUMV )
 )
  IMPORTING ET_PRC_ELEMENT_CLASSIC_FORMAT = DATA(ETL1569C2R5737) ).
  T_KONV[] = ETL1569C2R5737.
CATCH CX_PRC_RESULT .
  SY-SUBRC = 4.
ENDTRY.

  ASSIGN ('(SAPLMEPO)fc_vorga') TO <VORGA>.
  ASSIGN ('(SAPLMEPO)cva_en') TO <CVA>.
  ASSIGN ('(SAPLMEPO)tkomv[]') TO <KONV>.

  <VORGA> = <CVA>.

  PERFORM KOND_TAXES(SAPLMEPO) USING 'D' 'X'.

  CHECK <EKPO>-LOEKZ = SPACE.
  ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <WMWST>.


  DATA: W_NETWR  TYPE KOMP-NETWR.

  W_NETWR = <EKPO>-NETWR.

  W_VALOR  = ( W_NETWR + <WMWST> ).

ENDFORM.                    " R_IMPOSTO_ITEM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_BSAK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_BSAK .
  CHECK T_BSAK[] IS NOT INITIAL.
  " Segunda busca em diante (recursivamente)
  T_BSAK2[]    = T_BSAK[].
  LOOP AT T_BSAK INTO WA_BSAK.
    WA_BSAK-GJAHRE = WA_BSAK-AUGDT+0(4).
    MODIFY T_BSAK FROM WA_BSAK INDEX SY-TABIX TRANSPORTING GJAHRE.
  ENDLOOP.
*  "descontos no pagto ou direto na fatura (compensando e dando desconto no doc compensacao) ambos augbl
*  SELECT  BUKRS BELNR GJAHR SHKZG DMBE2 DMBTR
*    FROM BSEG
*    INTO TABLE T_BSEG
*    FOR ALL ENTRIES IN T_BSAK
*    WHERE BUKRS EQ T_BSAK-BUKRS
*    AND   BELNR EQ T_BSAK-AUGBL
*    AND   GJAHR EQ T_BSAK-GJAHRE
*    AND   HKONT IN ( '0000431101', '0000331102' ).

  DATA: XACHOU(1),
        XDESM(1),
        XPAG(1),
        V_BUKRS   TYPE BSAK-BUKRS,
        V_AUGBL   TYPE BSAK-AUGBL,
        V_GJAHR   TYPE BSAK-GJAHR,
        TABIX     TYPE SY-TABIX.

  DELETE T_BSAK2 WHERE AUGBL+0(2) EQ '15' OR  AUGBL+0(2) EQ '20'. " o que for diferente de 15 e 20 fazer outras buscas
  DELETE T_BSAK  WHERE AUGBL+0(2) NE '15' AND AUGBL+0(2) NE '20'. "somente 15 e 20 para buscar na bsis
  LOOP AT T_BSAK2 INTO WA_BSAK2. "BELNR originais BKPF
    XACHOU = ''.
    V_BUKRS = WA_BSAK2-BUKRS.
    V_AUGBL = WA_BSAK2-AUGBL.
    V_GJAHR = WA_BSAK2-AUGDT+0(4).
    REFRESH T_BSAK3.
    WHILE XACHOU = ''.

      SELECT BUKRS BELNR GJAHR BSCHL SHKZG UMSKZ AUGBL DMBE2 DMBTR AUGDT
           FROM BSAK
           INTO TABLE T_BSAK3
           WHERE BUKRS EQ V_BUKRS
           AND BELNR   EQ V_AUGBL
           AND GJAHR   EQ V_GJAHR.
*           AND BSCHL   NOT IN ( '27', '08', '18', '28', '38' ). "compensações

      CLEAR: XPAG, XDESM.
      LOOP AT T_BSAK3 INTO WA_BSAK3.
        IF  WA_BSAK3-BSCHL = '34'.
          XDESM = 'X'.
        ENDIF.
        IF WA_BSAK3-AUGBL+0(2) EQ '15'.
          XPAG = 'X'.
        ENDIF.
      ENDLOOP.

      LOOP AT T_BSAK3 INTO WA_BSAK3.
        TABIX = SY-TABIX.
        IF WA_BSAK3-BELNR = WA_BSAK3-AUGBL.
          DELETE T_BSAK3 INDEX TABIX.
        ENDIF.
        IF WA_BSAK3-UMSKZ IS NOT INITIAL AND XPAG IS INITIAL. "adiantamento não pega o pagamento aqui
          XACHOU = 'X'.
          EXIT.
        ENDIF.
        IF '27_08_18_28_38' CS WA_BSAK3-BSCHL AND XDESM NE 'X'. "compensações não pega exceto desmembramento
          XACHOU = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF XACHOU = 'X'.
        EXIT.
      ENDIF.

      IF T_BSAK3[] IS INITIAL.
        XACHOU = 'X'.
        EXIT.
      ENDIF.

      LOOP AT T_BSAK3 INTO WA_BSAK3.
        TABIX = SY-TABIX.
        IF WA_BSAK3-AUGBL+0(2) EQ '15' OR  WA_BSAK3-AUGBL+0(2) EQ '20'.
          WA_BSAK3-BUKRS_BK = WA_BSAK2-BUKRS_BK.
          WA_BSAK3-BELNR_BK = WA_BSAK2-BELNR_BK.
          WA_BSAK3-GJAHR_BK = WA_BSAK2-GJAHR_BK.
          APPEND WA_BSAK3 TO T_BSAK .
          DELETE T_BSAK3 INDEX TABIX.
        ENDIF.
      ENDLOOP.
      CLEAR V_AUGBL.
      LOOP AT T_BSAK3 INTO WA_BSAK3.
        TABIX = SY-TABIX.
        IF WA_BSAK3-AUGBL+0(2) NE '15' AND  WA_BSAK3-AUGBL+0(2) NE '20'.
          V_BUKRS = WA_BSAK3-BUKRS.
          V_AUGBL = WA_BSAK3-AUGBL.
          V_GJAHR = WA_BSAK3-GJAHR.
          DELETE T_BSAK3 INDEX TABIX.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF V_AUGBL IS INITIAL.
        XACHOU = 'X'.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  LOOP AT T_BSAK INTO WA_BSAK.
    WA_BSAK-GJAHRE = WA_BSAK-AUGDT+0(4).
    MODIFY T_BSAK FROM WA_BSAK INDEX SY-TABIX TRANSPORTING GJAHRE.
  ENDLOOP.

  SORT  T_BSAK BY BUKRS AUGBL GJAHR.

ENDFORM.                    " BUSCA_BSAK
*&---------------------------------------------------------------------*
*&      Form  F_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM F_CALCULO USING  W_VALOR     TYPE KOMP-NETWR
                      V_DMBE2_2   TYPE BSIS-DMBE2
                      V_DMBTR     TYPE BSIS-DMBTR
                      V_DMBTR_M   TYPE BSIS-DMBTR
                      V_DMBE2_M   TYPE BSIS-DMBE2
                      V_DMBE2_A   TYPE BSIS-DMBE2
                      V_DMBTR_A   TYPE BSIS-DMBTR
               CHANGING WA_SAIDA TYPE TY_SAIDA.


  DATA: WL_DATA(8).

  "Adiantamento
  ADD  V_DMBTR_A TO V_DMBTR.
  ADD  V_DMBE2_A TO V_DMBE2_2.

  WA_SAIDA-ADTO_VLR       = V_DMBTR_A.
  WA_SAIDA-ADTO_VLR_US    = V_DMBE2_A.

  WA_SAIDA-PEDIDO_VLR_A   = V_DMBTR_M. "Miro R$
  WA_SAIDA-PEDIDO_VLRA_U  = V_DMBE2_M. "MIRO USD

  IF WA_EKKO-WAERS EQ 'USD'.
    IF WA_EKKO-WKURS = 1.
      CONCATENATE WA_EKKO-BEDAT+6(2) WA_EKKO-BEDAT+4(2)  WA_EKKO-BEDAT(4) INTO WL_DATA.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = WL_DATA
        IMPORTING
          OUTPUT = WA_EKKO-GDATU.
      READ TABLE T_TCURR INTO WA_TCURR  WITH KEY GDATU = WA_EKKO-GDATU BINARY SEARCH.
      WA_EKKO-WKURS = WA_TCURR-UKURS.
    ENDIF.
    WA_SAIDA-PEDIDO_VLR     = W_VALOR * WA_EKKO-WKURS.             "Pedido R$
    WA_SAIDA-PEDIDO_VLR_US  = W_VALOR.                             "Pedido USD
    "
    WA_SAIDA-PAGO_VLR       = V_DMBTR.    "Pago R$
    WA_SAIDA-PAGO_VLR_US    = V_DMBE2_2.  "Pago USD Repete valor da Miro
  ELSE.
    CONCATENATE WA_EKKO-BEDAT+6(2) WA_EKKO-BEDAT+4(2)  WA_EKKO-BEDAT(4) INTO WL_DATA.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = WL_DATA
      IMPORTING
        OUTPUT = WA_EKKO-GDATU.

    CLEAR WA_TCURR.
    READ TABLE T_TCURR INTO WA_TCURR  WITH KEY GDATU = WA_EKKO-GDATU BINARY SEARCH.
    WA_SAIDA-PEDIDO_VLR     = W_VALOR.   "Pedido R$
    IF  WA_TCURR-UKURS NE 0.
      WA_SAIDA-PEDIDO_VLR_US  =  W_VALOR / WA_TCURR-UKURS.         "Pedido USD
    ENDIF.

    WA_SAIDA-PAGO_VLR       = V_DMBTR.             " Pago R$ repete valor MIRO R$
    WA_SAIDA-PAGO_VLR_US    = V_DMBE2_2.
  ENDIF.


  WA_SAIDA-RECEBIDO_VLR    = WA_SAIDA-PEDIDO_VLR_A. "Recebido = vlr miro
  WA_SAIDA-RECEBIDO_VLR_US = WA_SAIDA-PEDIDO_VLRA_U.

  IF WA_EKKO-WAERS EQ 'USD'.
    IF WA_SAIDA-RECEBIDO_VLR_US GE WA_SAIDA-PEDIDO_VLR_US AND WA_EKBE_TOTAL_2-TOT_QUAN GE WA_EKPO-MENGE. "se ja recebeu totalmente, iguala o valor pedido R$ com MIRO
      WA_SAIDA-PEDIDO_VLR  = WA_SAIDA-RECEBIDO_VLR.
    ENDIF.

    WA_SAIDA-SDO_A_PAGAR_VLR      = ABS( WA_SAIDA-PEDIDO_VLR_A )  - ABS( WA_SAIDA-PAGO_VLR ).
    WA_SAIDA-SDO_A_PAGAR_VLR_US   = ABS( WA_SAIDA-PEDIDO_VLRA_U ) - ABS( WA_SAIDA-PAGO_VLR_US ).

  ELSE.
    WA_SAIDA-SDO_A_PAGAR_VLR      = ABS( WA_SAIDA-PEDIDO_VLR_A )  - ABS( WA_SAIDA-PAGO_VLR ).
    IF WA_SAIDA-SDO_A_PAGAR_VLR GT 0.
      WA_SAIDA-SDO_A_PAGAR_VLR_US   = ABS( WA_SAIDA-PEDIDO_VLRA_U ) - ABS( WA_SAIDA-PAGO_VLR_US ).
    ELSE.
      WA_SAIDA-SDO_A_PAGAR_VLR_US = 0.
    ENDIF.
  ENDIF.

  IF WA_SAIDA-PAGO_VLR = 0.
    WA_SAIDA-VARI_PAGO  = 0.
  ELSE.
    WA_SAIDA-VARI_PAGO            = ABS( WA_SAIDA-PEDIDO_VLR_A )  - ABS( WA_SAIDA-PAGO_VLR ).
  ENDIF.

  IF WA_EKKO-WAERS EQ 'USD'.
    IF WA_SAIDA-RECEBIDO_VLR_US = 0.
      WA_SAIDA-VARI_PAGO = 0.
    ENDIF.
    IF  WA_SAIDA-SDO_A_PAGAR_VLR_US LE 0.
      CLEAR: WA_SAIDA-SDO_A_PAGAR_VLR_US, WA_SAIDA-SDO_A_PAGAR_VLR.
    ENDIF.
  ELSE.
    IF WA_SAIDA-RECEBIDO_VLR = 0.
      WA_SAIDA-VARI_PAGO = 0.
    ENDIF.
    IF  WA_SAIDA-SDO_A_PAGAR_VLR LE 0.
      CLEAR: WA_SAIDA-SDO_A_PAGAR_VLR_US, WA_SAIDA-SDO_A_PAGAR_VLR.
    ENDIF.
  ENDIF.

  IF WA_EKPO-ELIKZ EQ 'X' OR WA_EKBE_TOTAL_2-TOT_QUAN GE WA_EKPO-MENGE.
    CLEAR: WA_SAIDA-SDO_A_RECEBER_VLR, WA_SAIDA-SDO_A_RECEBER_VLR_US.

    CLEAR: WA_SAIDA-SDO_A_PAGAR_VLR_US, WA_SAIDA-SDO_A_PAGAR_VLR.
  ELSE.
    WA_SAIDA-SDO_A_RECEBER_VLR    = WA_SAIDA-PEDIDO_VLR - WA_SAIDA-RECEBIDO_VLR.
    WA_SAIDA-SDO_A_RECEBER_VLR_US = WA_SAIDA-PEDIDO_VLR_US - WA_SAIDA-RECEBIDO_VLR_US.
    IF WA_EKKO-WAERS EQ 'USD'.
      WA_SAIDA-SDO_A_RECEBER_VLR = WA_SAIDA-SDO_A_RECEBER_VLR_US * WA_EKKO-WKURS.
    ELSE.
      WA_SAIDA-SDO_A_RECEBER_VLR_US = WA_SAIDA-SDO_A_RECEBER_VLR / WA_TCURR-UKURS.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_PAGTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EKPO_EBELN  text
*      -->P_WA_EKPO_EBELP  text
*      <--P_V_DMBE2_2  text
*      <--P_V_DMBTR  text
*----------------------------------------------------------------------*
FORM F_BUSCA_PAGTO  USING    P_EBELN
                             P_EBELP
                    CHANGING V_DMBE2_2
                             V_DMBTR
                             V_DMBTR_D
                             V_DMBE2_D
                             V_DMBTR_M
                             V_DMBE2_M
                             V_DMBE2_A
                             V_DMBTR_A
                             V_BUDAT.

  SORT T_EKBE_GER BY BELNR GJAHR.

  DATA: XACHOU(1),
        WA_BKPF_PAG   TYPE BKPF,
        VTOTMIRO      TYPE EKBE-WRBTR,
        VTOTPAG       TYPE EKBE-WRBTR,
        V_PERCMIR(14) TYPE P DECIMALS 8,
        V_PERCPAG(14) TYPE P DECIMALS 8,
        WL_DATA(8).

  CLEAR: WA_EKBE_TOTAL, WA_EKBE_TOTAL_2.
  READ TABLE T_EKBE_TOTAL INTO WA_EKBE_TOTAL
        WITH KEY EBELN = P_EBELN
        EBELP = P_EBELP.

  READ TABLE T_EKBE_TOTAL_2 INTO WA_EKBE_TOTAL_2
  WITH KEY EBELN = P_EBELN
           EBELP = P_EBELP.

  READ TABLE T_EKKO INTO WA_EKKO
          WITH KEY EBELN = WA_EKPO-EBELN.

  CLEAR  XACHOU.
  V_DMBE2_2 = 0.
  V_DMBTR   = 0.
  V_DMBE2_A = 0.
  V_DMBTR_A = 0.
  V_DMBE2_D = 0.
  V_DMBTR_D = 0.
  V_DMBTR_M = 0.
  V_DMBE2_M = 0.
  LOOP AT  T_EKBE_AUX INTO WA_EKBE_AUX WHERE EBELN = P_EBELN
                                       AND   EBELP = P_EBELP.

    "totaliza valor de todas as miros que
    CLEAR: V_PERCMIR, VTOTMIRO.
    LOOP AT T_EKBE_GER INTO WA_EKBE_GER WHERE BELNR = WA_EKBE_AUX-BELNR.
      WA_EKBE_TOT-BELNR = WA_EKBE_GER-BELNR.
      WA_EKBE_TOT-WRBTR = WA_EKBE_GER-WRBTR.
      ADD WA_EKBE_TOT-WRBTR TO VTOTMIRO.
    ENDLOOP.
    IF VTOTMIRO NE 0.
      V_PERCMIR = WA_EKBE_AUX-WRBTR / VTOTMIRO.
    ENDIF.
    "
    READ TABLE T_BKPF INTO WA_BKPF
      WITH KEY BUKRS = WA_EKBE_AUX-BUKRS
               GJAHR = WA_EKBE_AUX-GJAHR
               AWKEY = WA_EKBE_AUX-AWKEY.

    IF SY-SUBRC IS INITIAL AND WA_EKBE_AUX-VGABE NE '4' AND WA_EKBE_AUX-VGABE NE 'C'.
      "MIRO
      READ TABLE T_RBKP INTO WA_RBKP WITH KEY BELNR = WA_EKBE_AUX-BELNR
                                              GJAHR = WA_EKBE_AUX-GJAHR BINARY SEARCH.
      IF WA_BKPF-KURS2 LT 0.
        MULTIPLY WA_BKPF-KURS2 BY -1.
      ENDIF.
      IF WA_RBKP-KURSF LT 0.
        MULTIPLY WA_RBKP-KURSF BY -1.
      ENDIF.
      IF SY-SUBRC = 0.
        IF WA_EKKO-WAERS EQ 'USD'.
          IF WA_EKBE_AUX-SHKZG = 'S'.
            V_DMBTR_M =  V_DMBTR_M + ( ( WA_RBKP-RMWWR *  WA_RBKP-KURSF ) * V_PERCMIR ).
            V_DMBE2_M =  V_DMBE2_M + ( WA_RBKP-RMWWR * V_PERCMIR ).
          ELSE.
            V_DMBTR_M =  V_DMBTR_M - ( ( WA_RBKP-RMWWR *  WA_RBKP-KURSF ) * V_PERCMIR ).
            V_DMBE2_M =  V_DMBE2_M - ( WA_RBKP-RMWWR * V_PERCMIR ).
          ENDIF.
        ELSE.
          IF WA_EKBE_AUX-SHKZG = 'S'.
            V_DMBTR_M =  V_DMBTR_M + ( WA_RBKP-RMWWR * V_PERCMIR ).
            V_DMBE2_M =  V_DMBE2_M + ( ( WA_RBKP-RMWWR /  WA_BKPF-KURS2 ) * V_PERCMIR ).
          ELSE.
            V_DMBTR_M =  V_DMBTR_M - ( WA_RBKP-RMWWR * V_PERCMIR ).
            V_DMBE2_M =  V_DMBE2_M - ( ( WA_RBKP-RMWWR / WA_BKPF-KURS2 ) * V_PERCMIR ).
          ENDIF.
        ENDIF.

        CLEAR VTOTPAG.
        LOOP AT T_BSAK INTO WA_BSAK WHERE BUKRS_BK  = WA_BKPF-BUKRS
                                    AND   BELNR_BK  = WA_BKPF-BELNR
                                    AND   GJAHR_BK  = WA_BKPF-GJAHR.
          IF WA_EKKO-WAERS EQ 'USD'.
            IF WA_BSAK-SHKZG = 'S'.
              SUBTRACT WA_BSAK-DMBE2 FROM VTOTPAG.
            ELSE.
              ADD WA_BSAK-DMBE2 TO VTOTPAG.
            ENDIF.
          ELSE.
            IF WA_BSAK-SHKZG = 'S'.
              SUBTRACT  WA_BSAK-DMBTR FROM VTOTPAG.
            ELSE.
              ADD WA_BSAK-DMBTR TO VTOTPAG.
            ENDIF.
          ENDIF.
        ENDLOOP.

*        IF WA_EKBE_AUX-SHKZG = 'S' AND VTOTPAG = 0 AND P_RESU_F IS NOT INITIAL. "Se não houve pagamento ainda da MIRO zera quantidade recebida para dar saldo a pagar
*          WA_EKBE_TOTAL_2-TOT_QUAN = 0.
*        ENDIF.

        LOOP AT T_BSAK INTO WA_BSAK WHERE BUKRS_BK  = WA_BKPF-BUKRS
                                    AND   BELNR_BK  = WA_BKPF-BELNR
                                    AND   GJAHR_BK  = WA_BKPF-GJAHR.
          SELECT SINGLE *
            FROM BKPF
            INTO WA_BKPF_PAG
            WHERE BUKRS = WA_BSAK-BUKRS
            AND   BELNR = WA_BSAK-AUGBL
            AND   GJAHR = WA_BSAK-GJAHRE.

          IF SY-SUBRC = 0.
            V_BUDAT = WA_BKPF_PAG-BUDAT.
            IF VTOTPAG GT 0.
              V_PERCPAG = WA_RBKP-RMWWR / VTOTPAG.
              IF V_PERCPAG LT ( 9 / 10 ). "se pagou menos de 90% tem mais parcelas sem pagar, então pega o valor pago da parcela nominalmente, não em proporção
                IF WA_BSAK-AUGBL+2 = '15'.
                  IF WA_EKKO-WAERS EQ 'USD'.
*---> 14/06/2023 - Migração S4 - JS
*            WA_RBKP-RMWWR = WA_BSAK-DMBE2.
           WA_RBKP-RMWWR = CONV #( WA_BSAK-DMBE2 ).
*<--- 14/06/2023 - Migração S4 - JS
                  ELSE.
*---> 14/06/2023 - Migração S4 - JS
*            WA_RBKP-RMWWR = WA_BSAK-DMBTR.
           WA_RBKP-RMWWR = CONV #( WA_BSAK-DMBTR ).
*<--- 14/06/2023 - Migração S4 - JS
                  ENDIF.
                ENDIF.
                V_PERCPAG = 1.
              ELSE. " senão, pagou tudo
                IF WA_EKKO-WAERS EQ 'USD'.
                  V_PERCPAG = WA_BSAK-DMBE2 / VTOTPAG.
                ELSE.
                  V_PERCPAG =  WA_BSAK-DMBTR / VTOTPAG.
                ENDIF.
              ENDIF.
            ELSE.
              V_PERCPAG = 1.
            ENDIF.
            IF WA_EKKO-WAERS NE 'USD'.
              CONCATENATE WA_BKPF_PAG-BUDAT+6(2) WA_BKPF_PAG-BUDAT+4(2)  WA_BKPF_PAG-BUDAT(4) INTO WL_DATA.
              CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
                EXPORTING
                  INPUT  = WL_DATA
                IMPORTING
                  OUTPUT = WA_EKKO-GDATU.

              SELECT SINGLE KURST FCURR TCURR GDATU UKURS
                FROM TCURR
                INTO WA_TCURR
                  WHERE GDATU EQ WA_EKKO-GDATU
                    AND KURST EQ 'G'
                    AND FCURR EQ 'USD'
                    AND TCURR EQ 'BRL'.

              IF WA_TCURR-UKURS LT 0.
                MULTIPLY WA_TCURR-UKURS BY -1.
              ENDIF.

              IF WA_BSAK-SHKZG = 'S'.
                V_DMBTR =  V_DMBTR - ( WA_RBKP-RMWWR * V_PERCPAG  * V_PERCMIR ).
                V_DMBE2_2 = V_DMBE2_2 -  ( (  WA_RBKP-RMWWR / WA_TCURR-UKURS ) * V_PERCPAG * V_PERCMIR ).
              ELSE.
                V_DMBTR =  V_DMBTR + ( WA_RBKP-RMWWR * V_PERCPAG * V_PERCMIR ).
                V_DMBE2_2 = V_DMBE2_2 +  ( ( WA_RBKP-RMWWR / WA_TCURR-UKURS ) * V_PERCPAG * V_PERCMIR ).
              ENDIF.
            ELSE.
              IF WA_BSAK-SHKZG = 'S'.
                V_DMBTR =  V_DMBTR - ( ( WA_RBKP-RMWWR * WA_BKPF_PAG-KURSF ) * V_PERCPAG * V_PERCMIR ).
                V_DMBE2_2 = V_DMBE2_2 -  ( WA_RBKP-RMWWR * V_PERCPAG * V_PERCMIR ).
              ELSE.
                V_DMBTR =  V_DMBTR + ( ( WA_RBKP-RMWWR * WA_BKPF_PAG-KURSF ) * V_PERCPAG * V_PERCMIR ).
                V_DMBE2_2 = V_DMBE2_2 +  ( WA_RBKP-RMWWR * V_PERCPAG * V_PERCMIR ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.
    ELSE.
      IF WA_EKBE_AUX-DMBTR GT 0.
        SELECT SINGLE *
         FROM BKPF
         INTO WA_BKPF_PAG
         WHERE BUKRS = WA_EKBE_AUX-BUKRS
         AND   BELNR = WA_EKBE_AUX-BELNR
         AND   GJAHR = WA_EKBE_AUX-GJAHR.
        IF SY-SUBRC = 0.
          V_BUDAT = WA_BKPF_PAG-BUDAT.
          IF WA_EKKO-WAERS EQ 'USD'.
            IF WA_EKBE_AUX-SHKZG = 'H'.
              V_DMBTR_A = V_DMBTR_A - ( WA_EKBE_AUX-WRBTR * WA_BKPF_PAG-KURSF ).
              V_DMBE2_A = V_DMBE2_A -  WA_EKBE_AUX-WRBTR.
            ELSE.
              V_DMBTR_A = V_DMBTR_A + (  WA_EKBE_AUX-WRBTR * WA_BKPF_PAG-KURSF ).
              V_DMBE2_A = V_DMBE2_A +  WA_EKBE_AUX-WRBTR.
            ENDIF.
          ELSE.
            CONCATENATE WA_BKPF_PAG-BUDAT+6(2) WA_BKPF_PAG-BUDAT+4(2)  WA_BKPF_PAG-BUDAT(4) INTO WL_DATA.
            CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
              EXPORTING
                INPUT  = WL_DATA
              IMPORTING
                OUTPUT = WA_EKKO-GDATU.

            SELECT SINGLE KURST FCURR TCURR GDATU UKURS
              FROM TCURR
              INTO WA_TCURR
                WHERE GDATU EQ WA_EKKO-GDATU
                  AND KURST EQ 'G'
                  AND FCURR EQ 'USD'
                  AND TCURR EQ 'BRL'.

            IF WA_TCURR-UKURS LT 0.
              MULTIPLY WA_TCURR-UKURS BY -1.
            ENDIF.
            IF WA_EKBE_AUX-SHKZG = 'H'.
              V_DMBTR_A = V_DMBTR_A - WA_EKBE_AUX-WRBTR.
              V_DMBE2_A = V_DMBE2_A -  ( WA_EKBE_AUX-WRBTR / WA_TCURR-UKURS ).
            ELSE.
              V_DMBTR_A = V_DMBTR_A + WA_EKBE_AUX-WRBTR.
              V_DMBE2_A = V_DMBE2_A +  ( WA_EKBE_AUX-WRBTR / WA_TCURR-UKURS ).
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.
