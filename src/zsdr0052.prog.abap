REPORT ZSDR0052.

TABLES: VBAK.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_VKORG FOR VBAK-VKORG OBLIGATORY,
                P_VTWEG FOR VBAK-VTWEG OBLIGATORY,
                P_SPART FOR VBAK-SPART OBLIGATORY,
                P_KUNNR FOR VBAK-KUNNR,
                P_ERDAT FOR VBAK-ERDAT OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

TYPES: BEGIN OF TY_SAIDA,
         AUART         TYPE VBAK-AUART,
         BSTNK         TYPE VBAK-BSTNK,
         VBELNK        TYPE VBAK-VBELN,
         KNUMV         TYPE VBAK-KNUMV,
         KUNNR         TYPE VBAK-KUNNR,
         BNAME         TYPE VBAK-BNAME,
         NAMEK         TYPE KNA1-NAME1,
         STRAS         TYPE KNA1-STRAS,
         ORT02         TYPE KNA1-ORT02,
         ORT01         TYPE KNA1-ORT01,
         ADRNR         TYPE KNA1-ADRNR,
         COUNTRY       TYPE ADRC-COUNTRY,
         REGION        TYPE ADRC-REGION,
         LANDX         TYPE T005T-LANDX,
         BEZEI         TYPE T005U-BEZEI,
         MATNR         TYPE VBAP-MATNR,
         ARKTX         TYPE VBAP-ARKTX,
         KWMENG        TYPE VBAP-KWMENG,
         NETPR         TYPE VBAP-NETPR,
         WAERK         TYPE VBAP-WAERK,
         VGBEL         TYPE VBAP-VGBEL,
         WERKS         TYPE VBAP-WERKS,
         NAMEW         TYPE T001W-NAME1,
         LGORT         TYPE VBAP-LGORT,
         LGOBE         TYPE T001L-LGOBE,
         NETWR         TYPE VBAP-NETWR,
         BSTKD         TYPE VBKD-BSTKD,
         VBELNR        TYPE VBFA-VBELN,
         VBELNF        TYPE VBFA-VBELN,
         WADAT         TYPE LIKP-WADAT,
         BELNR         TYPE BKPF-BELNR,
         BUDAT         TYPE BKPF-BUDAT,
         KBETR         TYPE KONV-KBETR,
         KWERT         TYPE KONV-KWERT,
         XBLNR         TYPE J_1ACAE-XBLNR,
         CAE_STATUS    TYPE J_1ACAE-CAE_STATUS,
         DESC_COD(40),
         CONT_LAND(40),
         REG_BEZ(40),
         LIFNR         TYPE ZSDYT0049-LIFNR,
         COD_MVTO      TYPE ZSDYT0049-COD_MVTO,
         NAMEL         TYPE LFA1-NAME1,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_VBFA_F,
         VBELV TYPE VBELV,
         VBELN TYPE VBELN,
         AWKEY TYPE AWKEY,
       END OF TY_VBFA_F.

FIELD-SYMBOLS: <VBFA> TYPE TY_VBFA_F.

DATA: IT_REMESSA  TYPE RANGE OF VBFA-VBELN,
      WA_REMESSA  LIKE LINE OF IT_REMESSA,
      IT_FATURA   TYPE RANGE OF VBFA-VBELN,
      WA_FATURA   LIKE LINE OF IT_FATURA,
      IT_FATURA_K TYPE RANGE OF BKPF-AWKEY,
      WA_FATURA_K LIKE LINE OF IT_FATURA_K.

DATA: IT_VBAK      TYPE TABLE OF VBAK,
      IT_KNA1      TYPE TABLE OF KNA1,
      IT_ADRC      TYPE TABLE OF ADRC,
      IT_T005T     TYPE TABLE OF T005T,
      IT_T005U     TYPE TABLE OF T005U,
      IT_VBAP      TYPE TABLE OF VBAP,
      IT_T001W     TYPE TABLE OF T001W,
      IT_T001L     TYPE TABLE OF T001L,
      IT_VBKD      TYPE TABLE OF VBKD,
      IT_VBFA_R    TYPE TABLE OF VBFA,
      IT_VBFA_F    TYPE TABLE OF TY_VBFA_F,
      IT_LIKP      TYPE TABLE OF LIKP,
      IT_BKPF_F    TYPE TABLE OF BKPF,
      IT_KONV      TYPE TABLE OF KONV,
      IT_J_1ACAE   TYPE TABLE OF J_1ACAE,
      IT_ZSDYT0049 TYPE TABLE OF ZSDYT0049,
      IT_LFA1      TYPE TABLE OF LFA1,
      IT_SAIDA     TYPE TABLE OF TY_SAIDA,

      WA_VBAK      TYPE VBAK,
      WA_KNA1      TYPE KNA1,
      WA_ADRC      TYPE ADRC,
      WA_T005T     TYPE T005T,
      WA_T005U     TYPE T005U,
      WA_VBAP      TYPE VBAP,
      WA_T001W     TYPE T001W,
      WA_T001L     TYPE T001L,
      WA_VBKD      TYPE VBKD,
      WA_VBFA      TYPE VBFA,
      WA_LIKP      TYPE LIKP,
      WA_BKPF_F    TYPE BKPF,
      WA_VBFA_F    TYPE TY_VBFA_F,
      WA_KONV      TYPE KONV,
      WA_J_1ACAE   TYPE J_1ACAE,
      WA_ZSDYT0049 TYPE ZSDYT0049,
      WA_LFA1      TYPE LFA1,
      WA_SAIDA     TYPE TY_SAIDA.

DATA: IT_FCAT     TYPE LVC_T_FCAT,
      WA_FCAT     TYPE LVC_S_FCAT,
      WA_LAYOUT   TYPE LVC_S_LAYO,
      WA_GRID     TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONT     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_VARIANTE TYPE DISVARIANT.


START-OF-SELECTION.
  CALL SCREEN 0100.

END-OF-SELECTION.


FORM SELECIONA_DADOS.

  SELECT *
    FROM VBAK
      INTO TABLE IT_VBAK
         WHERE VKORG IN P_VKORG
           AND VTWEG IN P_VTWEG
           AND SPART IN P_SPART
           AND KUNNR IN P_KUNNR
           AND ERDAT IN P_ERDAT.

  CHECK NOT IT_VBAK IS INITIAL.

  SELECT *
    FROM KNA1
      INTO TABLE IT_KNA1
         FOR ALL ENTRIES IN IT_VBAK
            WHERE KUNNR EQ IT_VBAK-KUNNR.

  IF NOT IT_KNA1 IS INITIAL.

    SELECT *
      FROM ADRC
        INTO TABLE IT_ADRC
          FOR ALL ENTRIES IN IT_KNA1
              WHERE ADDRNUMBER EQ IT_KNA1-ADRNR.

    SELECT *
      FROM T005T
        INTO TABLE IT_T005T
            FOR ALL ENTRIES IN IT_KNA1
              WHERE LAND1 EQ IT_KNA1-LAND1
                AND SPRAS EQ SY-LANGU.

    SELECT *
      FROM T005U
        INTO TABLE IT_T005U
            FOR ALL ENTRIES IN IT_KNA1
              WHERE BLAND EQ IT_KNA1-REGIO
                AND LAND1 EQ IT_KNA1-LAND1
                AND SPRAS EQ SY-LANGU.

  ENDIF.

  SELECT *
      FROM VBAP
        INTO TABLE IT_VBAP
          FOR ALL ENTRIES IN IT_VBAK
            WHERE VBELN EQ IT_VBAK-VBELN.

  IF NOT IT_VBAP IS INITIAL.

    SELECT *
      FROM T001W
        INTO TABLE IT_T001W
          FOR ALL ENTRIES IN IT_VBAP
    WHERE WERKS EQ IT_VBAP-WERKS.

    SELECT *
      FROM T001L
        INTO TABLE IT_T001L
          FOR ALL ENTRIES IN IT_VBAP
            WHERE LGORT EQ IT_VBAP-LGORT
              AND WERKS EQ IT_VBAP-WERKS.

    SELECT *
      FROM VBKD
         INTO TABLE IT_VBKD
            FOR ALL ENTRIES IN IT_VBAP
                WHERE VBELN EQ IT_VBAP-VBELN.

    SELECT *
      FROM VBFA
        INTO TABLE IT_VBFA_R
           FOR ALL ENTRIES IN IT_VBAP
              WHERE VBELV EQ IT_VBAP-VBELN
              AND VBTYP_N IN ('J').

    LOOP AT IT_VBFA_R INTO WA_VBFA.

      WA_REMESSA-SIGN   = 'I'.
      WA_REMESSA-OPTION = 'EQ'.
      WA_REMESSA-LOW    = WA_VBFA-VBELN.
      WA_REMESSA-HIGH   = WA_VBFA-VBELN.
      APPEND WA_REMESSA TO IT_REMESSA.

    ENDLOOP.

    IF NOT IT_VBFA_R IS INITIAL.
      SELECT *
        FROM LIKP
          INTO TABLE IT_LIKP
            FOR ALL ENTRIES IN IT_VBFA_R
              WHERE VBELN EQ IT_VBFA_R-VBELN.
    ENDIF.

    SELECT VBELV VBELN
      FROM VBFA
        INTO TABLE IT_VBFA_F
           FOR ALL ENTRIES IN IT_VBAP
              WHERE VBELV EQ IT_VBAP-VBELN
                AND VBTYP_N IN ('M').

    LOOP AT IT_VBFA_F INTO WA_VBFA_F.

      WA_FATURA-SIGN   = 'I'.
      WA_FATURA-OPTION = 'EQ'.
      WA_FATURA-LOW    = WA_VBFA_F-VBELN.
      WA_FATURA-HIGH   = WA_VBFA_F-VBELN.
      APPEND WA_FATURA TO IT_FATURA.

    ENDLOOP.

    LOOP AT IT_VBFA_F ASSIGNING <VBFA>.
      <VBFA>-AWKEY = <VBFA>-VBELN.
    ENDLOOP.

    LOOP AT IT_VBFA_F INTO WA_VBFA_F.

      WA_FATURA_K-SIGN   = 'I'.
      WA_FATURA_K-OPTION = 'EQ'.
      WA_FATURA_K-LOW    = WA_VBFA_F-VBELN.
      WA_FATURA_K-HIGH   = WA_VBFA_F-VBELN.
      APPEND WA_FATURA_K TO IT_FATURA_K.

    ENDLOOP.

    IF NOT IT_FATURA_K IS INITIAL.
      SELECT *
        FROM BKPF
          INTO TABLE IT_BKPF_F
            FOR ALL ENTRIES IN IT_VBAK
              WHERE BUKRS EQ IT_VBAK-VKORG
                AND AWKEY IN IT_FATURA_K.
    ENDIF.

  ENDIF.

  SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KNUMV AND KSCHL EQ 'J1AU' INTO CORRESPONDING FIELDS OF TABLE @IT_KONV .

  IF NOT IT_FATURA IS INITIAL.
    SELECT *
        FROM J_1ACAE
          INTO TABLE IT_J_1ACAE
            FOR ALL ENTRIES IN IT_VBAK
              WHERE BUKRS EQ IT_VBAK-VKORG
              AND CAE_REF IN IT_FATURA.

    SELECT *
       FROM ZSDYT0049
          INTO TABLE IT_ZSDYT0049
              WHERE DOC_FAT IN IT_FATURA.

    IF NOT IT_ZSDYT0049 IS INITIAL.

      SELECT *
        FROM LFA1
          INTO TABLE IT_LFA1
             FOR ALL ENTRIES IN IT_ZSDYT0049
                WHERE LIFNR EQ IT_ZSDYT0049-LIFNR.

    ENDIF.

  ENDIF.


ENDFORM.

*&      Form  AGRUPA_DADOS
FORM AGRUPA_DADOS .

  LOOP AT IT_VBAK INTO WA_VBAK.

    WA_SAIDA-AUART     =   WA_VBAK-AUART.
    WA_SAIDA-BSTNK     =   WA_VBAK-BSTNK.
    WA_SAIDA-VBELNK    =   WA_VBAK-VBELN.
    WA_SAIDA-KNUMV     =   WA_VBAK-KNUMV.
    WA_SAIDA-KUNNR     =   WA_VBAK-KUNNR.
    WA_SAIDA-BNAME     =   WA_VBAK-BNAME.

    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR.
    WA_SAIDA-NAMEK     =   WA_KNA1-NAME1.
    WA_SAIDA-STRAS     =   WA_KNA1-STRAS.
    WA_SAIDA-ORT02     =   WA_KNA1-ORT02.
    WA_SAIDA-ORT01     =   WA_KNA1-ORT01.
    WA_SAIDA-ADRNR     =   WA_KNA1-ADRNR.

    READ TABLE IT_ADRC INTO WA_ADRC WITH KEY ADDRNUMBER = WA_KNA1-ADRNR.
    WA_SAIDA-COUNTRY   =   WA_ADRC-COUNTRY.
    WA_SAIDA-REGION    =   WA_ADRC-REGION.

    READ TABLE IT_T005T INTO WA_T005T WITH KEY LAND1 = WA_KNA1-LAND1 SPRAS = SY-LANGU.
    WA_SAIDA-LANDX     =   WA_T005T-LANDX.

    READ TABLE IT_T005U INTO WA_T005U WITH KEY BLAND = WA_KNA1-REGIO LAND1 = WA_KNA1-LAND1 SPRAS = SY-LANGU.
    WA_SAIDA-BEZEI     =   WA_T005U-BEZEI.

    CONCATENATE WA_ADRC-COUNTRY WA_T005T-LANDX INTO WA_SAIDA-CONT_LAND SEPARATED BY '-'.
    CONCATENATE WA_ADRC-REGION WA_T005U-BEZEI  INTO WA_SAIDA-REG_BEZ   SEPARATED BY '-'.


    LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN EQ WA_VBAK-VBELN.

      WA_SAIDA-MATNR     =   WA_VBAP-MATNR.
      WA_SAIDA-ARKTX     =   WA_VBAP-ARKTX.
      WA_SAIDA-KWMENG    =   WA_VBAP-KWMENG.
      WA_SAIDA-NETPR     =   WA_VBAP-NETPR.
      WA_SAIDA-WAERK     =   WA_VBAP-WAERK.
      WA_SAIDA-VGBEL     =   WA_VBAP-VGBEL.

      WA_SAIDA-WERKS     =   WA_VBAP-WERKS.
      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_VBAP-WERKS.
      WA_SAIDA-NAMEW     =   WA_T001W-NAME1.

      WA_SAIDA-LGORT     =   WA_VBAP-LGORT.
      READ TABLE IT_T001L INTO WA_T001L WITH KEY LGORT = WA_VBAP-LGORT.
      WA_SAIDA-LGOBE     =   WA_T001L-LGOBE.

      WA_SAIDA-NETWR     =   WA_VBAP-NETWR.

      READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_VBAP-VBELN.
      WA_SAIDA-BSTKD     =   WA_VBKD-BSTKD.


      READ TABLE IT_VBFA_R INTO WA_VBFA WITH KEY VBELV = WA_VBAP-VBELN.
      WA_SAIDA-VBELNR    =   WA_VBFA-VBELN.

      READ TABLE IT_LIKP INTO WA_LIKP WITH KEY VBELN = WA_VBFA-VBELN.
      WA_SAIDA-WADAT     =   WA_LIKP-WADAT.

      FREE WA_VBFA_F.
      READ TABLE IT_VBFA_F INTO WA_VBFA_F WITH KEY VBELV = WA_VBAP-VBELN.
      WA_SAIDA-VBELNF    =   WA_VBFA_F-VBELN.

      READ TABLE IT_BKPF_F INTO WA_BKPF_F WITH KEY BUKRS = WA_VBAK-VKORG
                                                   AWKEY = WA_VBFA_F-AWKEY.

      WA_SAIDA-BELNR     =   WA_BKPF_F-BELNR.
      WA_SAIDA-BUDAT     =   WA_BKPF_F-BUDAT.

      READ TABLE IT_KONV INTO WA_KONV WITH KEY KNUMV = WA_VBAK-KNUMV
                                               KSCHL = 'J1AU'.

*      WA_SAIDA-KBETR     =   WA_KONV-KBETR.

      WA_SAIDA-KBETR     =   WA_KONV-KBETR / 100.

      WA_SAIDA-KWERT     =   WA_KONV-KWERT.

      READ TABLE IT_J_1ACAE INTO WA_J_1ACAE WITH KEY BUKRS = WA_VBAK-VKORG
                                                   CAE_REF = WA_VBFA_F-VBELN.
      WA_SAIDA-XBLNR     =   WA_J_1ACAE-XBLNR.
      WA_SAIDA-CAE_STATUS =  WA_J_1ACAE-CAE_STATUS.

      CASE WA_J_1ACAE-CAE_STATUS.
        WHEN 'I'. WA_SAIDA-DESC_COD = 'Enviados a AFIP'.
        WHEN 'A'. WA_SAIDA-DESC_COD = 'Autorizados'.
        WHEN 'P'. WA_SAIDA-DESC_COD = 'autorizados con reservas'.
        WHEN 'R'. WA_SAIDA-DESC_COD = 'Rechazados'.
        WHEN 'N'. WA_SAIDA-DESC_COD = 'Doc.nuevo'.
        WHEN 'C'. WA_SAIDA-DESC_COD = 'Contigencia'.
        WHEN 'E'. WA_SAIDA-DESC_COD = 'Error de validación'.
        WHEN 'O'. WA_SAIDA-DESC_COD = 'Observaciones'.
      ENDCASE.

      READ TABLE IT_ZSDYT0049 INTO WA_ZSDYT0049 WITH KEY DOC_FAT = WA_VBFA_F-VBELN.
      WA_SAIDA-LIFNR      =  WA_ZSDYT0049-LIFNR.
      WA_SAIDA-COD_MVTO   =  WA_ZSDYT0049-COD_MVTO.

      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_ZSDYT0049-LIFNR.
      WA_SAIDA-NAMEL      =  WA_LFA1-NAME1.

      APPEND WA_SAIDA TO IT_SAIDA.

    ENDLOOP.

    CLEAR: WA_VBAK, WA_KNA1, WA_ADRC, WA_T005T, WA_T005U, WA_VBAP,
           WA_T001W, WA_T001L, WA_VBKD, WA_VBFA, WA_LIKP, WA_BKPF_F,
           WA_VBFA_F, WA_KONV, WA_J_1ACAE, WA_ZSDYT0049, WA_LFA1, WA_SAIDA.

  ENDLOOP.

ENDFORM.

*&      Form  CRIA_ALV
FORM CRIA_ALV .

  REFRESH IT_FCAT.
  PERFORM MONTAR_ESTRUTURA USING:
    01  'VBAK'      'VBELN'      'IT_SAIDA'   'VBELNK'      'Pedido de Cliente'  ''  ''  ''  ''  '' '',"
    02  'VBAK'      'BNAME'      'IT_SAIDA'   'BNAME'       'Nombre del Buque'  ''  ''  ''  ''  '' '',"
    03  'LIKP'      'WADAT'      'IT_SAIDA'   'WADAT'       'Fecha de BL'  ''  ''  ''  ''  '' '',"
    04  'VBAP'      'WERKS'      'IT_SAIDA'   'WERKS'       'Centro'  ''  ''  ''  ''  '' '',"
    05  'T001W'     'NAME1'      'IT_SAIDA'   'NAMEW'       'Nombre de Ce.'  ''  ''  ''  ''  '' '',"
    06  'VBAP'      'LGORT'      'IT_SAIDA'   'LGORT'       'Almacen'  ''  ''  ''  ''  '' '',"
    07  'T001L'     'LGOBE'      'IT_SAIDA'   'LGOBE'       'Nombre Almacen'  ''  ''  ''  ''  '' '',"
    08  'VBAP'      'MATNR'      'IT_SAIDA'   'MATNR'       'Material'  ''  ''  ''  ''  '' '',"
    09  'VBAP'      'ARKTX'      'IT_SAIDA'   'ARKTX'       'Denominacion'  ''  ''  ''  ''  '' '',"
    10  'VBAP'      'KWMENG'     'IT_SAIDA'   'KWMENG'      'Cantidad de Pedido'  ''  ''  ''  ''  '' '',"
    11  'VBAP'      'NETPR'      'IT_SAIDA'   'NETPR'       'Importe'  ''  ''  ''  ''  '' '',"
    12  'VBAP'      'NETWR'      'IT_SAIDA'   'NETWR'       'Vlr. Liquido'  ''  ''  ''  ''  '' '',"
    13  'KONV'      'KWERT'      'IT_SAIDA'   'KWERT'       'Vlr. IVA'  ''  ''  ''  ''  '' '',"
    14  'KONV'      'KBETR'      'IT_SAIDA'   'KBETR'       'Tx. IVA%'  ''  ''  ''  ''  '' '',"
    15  'VBAP'      'WAERK'      'IT_SAIDA'   'WAERK'       'Moeda'  ''  ''  ''  ''  '' '',"
    16  'VBKD'      'BSTKD'      'IT_SAIDA'   'BSTKD'       'Nro de Pedido de Cliente'  ''  ''  ''  ''  '' '',"
    17  'VBFA'      'VBELN'      'IT_SAIDA'   'VBELNR'      'Nro. Remessa'  ''  ''  ''  ''  '' '',"
    18  ''          ''           'IT_SAIDA'   'DESC_COD'    'Status AFIP'  ''  ''  ''  ''  '' '',"
    19  'VBFA'      'VBELN'      'IT_SAIDA'   'VBELNF'      'Nro. Fatura'  ''  ''  ''  ''  '' '',"
    20  'BKPF'      'BELNR'      'IT_SAIDA'   'BELNR'       'Nro. Documento'  ''  ''  ''  ''  '' '',"
    21  'BKPF'      'BUDAT'      'IT_SAIDA'   'BUDAT'       'Fecha Contab'  ''  ''  ''  ''  '' '',"
    22  'J_1ACAE'   'XBLNR'      'IT_SAIDA'   'XBLNR'       'Nro. Referencia'  ''  ''  ''  ''  '' '',"
    23  'KUNNR'     'VBAK'       'IT_SAIDA'   'KUNNR'       'Deudor'  ''  ''  ''  ''  '' '',"
    24  'KNA1'      'NAME1'      'IT_SAIDA'   'NAMEK'       'Nombre de Solicitante'  ''  ''  ''  ''  '' '',"
    25  'KNA1'      'STRAS'      'IT_SAIDA'   'STRAS'       'Calle/Número'  ''  ''  ''  ''  '' '',"
    26  'KNA1'      'ORT02'      'IT_SAIDA'   'ORT02'       'Distrito'  ''  ''  ''  ''  '' '',"
    27  'KNA1'      'ORT01'      'IT_SAIDA'   'ORT01'       'Población'  ''  ''  ''  ''  '' '',"
    28  ''          ''           'IT_SAIDA'   'CONT_LAND'   'País'  ''  ''  ''  ''  '' '',"
    29  ''          ''           'IT_SAIDA'   'REG_BEZ'     'Region'  ''  ''  ''  ''  '' '',"
    30  'VBAP'      'VGBEL'      'IT_SAIDA'   'VGBEL'       'Pedido Abierto'  ''  ''  ''  ''  '' '',"
    31  'VBAK'      'BSTNK'      'IT_SAIDA'   'BSTNK'       'Nro. Pedido Cliente del pedido abierto'  ''  ''  ''  ''  '' '',"
    32  'VBAK'      'AUART'      'IT_SAIDA'   'AUART'       'Clase de documento de ventas'  ''  ''  ''  ''  '' '',"
    33  'ZSDYT0049' 'LIFNR'      'IT_SAIDA'   'LIFNR'       'Corredor'  ''  ''  ''  ''  '' '',"
    34  'LFA1'       'NAME1'      'IT_SAIDA'  'NAMEL'       'Nome Corredor'  ''  ''  ''  ''  '' '',"
    35  'ZSDYT0049' 'COD_MVTO'   'IT_SAIDA'   'COD_MVTO'    'Cod. Movto'  ''  ''  ''  ''  '' ''."

ENDFORM.

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_OPT)           TYPE C
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE)
                            VALUE(P_F4)
                            VALUE(P_HOTSPOT)
  .

  CLEAR WA_FCAT.
  WA_FCAT-COL_POS    = P_COL_POS.
  WA_FCAT-REF_TABLE  = P_REF_TABNAME.
  WA_FCAT-REF_FIELD  = P_REF_FIELDNAME.
  WA_FCAT-TABNAME    = P_TABNAME.
  WA_FCAT-FIELDNAME  = P_FIELD.
  WA_FCAT-REPTEXT    = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S  = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M  = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L  = P_SCRTEXT_L.
  WA_FCAT-OUTPUTLEN  = P_OUTPUTLEN.
  WA_FCAT-COL_OPT    = P_OPT.
  WA_FCAT-DO_SUM     = P_SUM.
  WA_FCAT-EMPHASIZE  = P_EMPHASIZE.
  WA_FCAT-F4AVAILABL = P_F4.
  WA_FCAT-HOTSPOT    = P_HOTSPOT.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " montar_estrutura

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  SET PF-STATUS 'S0100'.
  SET TITLEBAR 'T0100'.

  PERFORM SELECIONA_DADOS.
  PERFORM AGRUPA_DADOS.
  PERFORM CRIA_ALV.

  WA_LAYOUT-ZEBRA = ABAP_TRUE.

  IF WA_CONT IS INITIAL.

    CREATE OBJECT WA_CONT EXPORTING CONTAINER_NAME = 'C_01'.
    CREATE OBJECT WA_GRID EXPORTING I_PARENT = WA_CONT.

    CALL METHOD WA_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = WA_LAYOUT
        IS_VARIANT      = WA_VARIANTE
        I_SAVE          = 'X'
      CHANGING
        IT_OUTTAB       = IT_SAIDA
        IT_FIELDCATALOG = IT_FCAT.
  ELSE.
    CALL METHOD WA_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
