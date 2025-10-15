FUNCTION z_fi_juros_mi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_EMPRESA) TYPE  STANDARD TABLE
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"     REFERENCE(IT_CLIENTE) TYPE  STANDARD TABLE
*"     REFERENCE(IT_NR_OV) TYPE  STANDARD TABLE
*"     REFERENCE(IT_NR_SOL) TYPE  STANDARD TABLE
*"     REFERENCE(P_OPCAO) TYPE  ZCHAR02
*"     REFERENCE(I_WAERS) TYPE  WAERS OPTIONAL
*"     REFERENCE(P_TIPO) TYPE  ZCHAR02
*"  EXPORTING
*"     REFERENCE(IT_RESULTADO) TYPE  STANDARD TABLE
*"  TABLES
*"      IT_TVAK STRUCTURE  TVAK OPTIONAL
*"----------------------------------------------------------------------

TYPES:

 BEGIN OF TY_SAIDA,
    BUKRS     TYPE BSAD-BUKRS,
    GSBER     TYPE BSAD-GSBER,
    KUNNR     TYPE BSAD-KUNNR,
    NAME1     TYPE KNA1-NAME1,
    TIPO      TYPE C LENGTH 13,
    BELNR     TYPE BSAD-BELNR,
    AUGBL     TYPE BSAD-AUGBL,
    BUDAT     TYPE BSAD-BUDAT,
    AUGDT     TYPE BSAD-AUGDT,
    VBEL2     TYPE BSAD-VBEL2,
    AUART     TYPE VBAK-AUART,
    VBELN     TYPE BSAD-VBELN,
    NR_SOL    TYPE ZSDT0053-NRO_SOL_OV,
    TP_VENDA  TYPE ZSDT0051-TP_VENDA,
    DMBTR     TYPE BSAD-DMBTR,
    DMBE2     TYPE BSAD-DMBE2,
    TX_CAMB   TYPE ZLEST0061-TAX_DOLAR,
    BANCO_LIQ TYPE SKAT-TXT50,
    ZFBDT     TYPE BSAD-ZFBDT,
    BUTXT     TYPE T001-BUTXT,
    BELNR_BX  TYPE BSAD-BELNR,
    BUDAT_BX  TYPE BSAD-BUDAT,
    AUGDT_BX  TYPE BSAD-AUGDT,
    DMBTR_BX  TYPE BSAD-DMBTR,
    DMBE2_BX  TYPE BSAD-DMBE2,
    VBEL2_BX  TYPE BSAD-VBEL2,
    VBELN_BX  TYPE BSAD-VBELN,
    MATNR     TYPE MARA-MATNR,
    MAKTX     TYPE MAKT-MAKTX,
    BUZEI     TYPE BSAD-BUZEI,
    CHARG     TYPE VBAP-CHARG,
    WAERS     TYPE BSAD-WAERS,
    TPSIM     TYPE CHAR2,
    MATKL     TYPE MARA-MATKL,
 END OF TY_SAIDA,

 BEGIN OF TY_BSAD,
    BELNR  TYPE BSAD-BELNR,
    AUGBL  TYPE BSAD-AUGBL,
    BUKRS  TYPE BSAD-BUKRS,
    GJAHR  TYPE BSAD-GJAHR,
    VBEL2  TYPE BSAD-VBEL2,
    GSBER  TYPE BSAD-GSBER,
    KUNNR  TYPE BSAD-KUNNR,
    ZFBDT  TYPE BSAD-ZFBDT,
    ZBD1T  TYPE BSAD-ZBD1T,
    BUDAT  TYPE BSAD-BUDAT,
    AUGDT  TYPE BSAD-AUGDT,
    VBELN  TYPE BSAD-VBELN,
    DMBTR  TYPE BSAD-DMBTR,
    DMBE2  TYPE BSAD-DMBE2,
    UMSKS  TYPE BSAD-UMSKS,
    BUZEI  TYPE BSAD-BUZEI,
    WAERS  TYPE BSAD-WAERS,
    BSCHL  TYPE BSAD-BSCHL,
 END OF TY_BSAD,

 BEGIN OF TY_CONTAS,
    HKONT TYPE BSIS-HKONT,
 END OF TY_CONTAS.



 "===========================================================================
 " Tables
 "===========================================================================--
  DATA: GW_EMPRESA TYPE T001,
        GW_CENTRO  TYPE T001W,
        GW_DATA    TYPE BSAD,
        GW_CLIENTE TYPE BSAD,
        GW_NR_OV   TYPE BSAD,
        GW_NR_SOL  TYPE ZSDT0053.

  DATA: VAR_TABIX TYPE SY-TABIX,
        VAR_SAKNR TYPE C LENGTH 10.

 "===========================================================================
 " Tabelas Internas and WorkAreas
 "===========================================================================

 DATA: GT_BSAD      TYPE TABLE OF BSAD, "Contab.financ.: índice secundário p/clientes (partida liq.)
       GW_BSAD      TYPE BSAD,

       "Ajustes CS2019001627 - Ini
       GT_BSAD_JD   TYPE TABLE OF BSAD WITH HEADER LINE, "Contab.financ.: índice secundário p/clientes (partida liq.)
       GT_BSIS_JD   TYPE TABLE OF BSIS WITH HEADER LINE, "Contab.financ.: índice secundário p/clientes (partida liq.)
       GT_SKA1_JD   TYPE TABLE OF SKA1 WITH HEADER LINE, "Contab.financ.: índice secundário p/clientes (partida liq.)
       "Ajustes CS2019001627 - Fim

       GW_BSAD_AUX  TYPE BSAD,
       GW_VBAP      TYPE VBAP,
       GT_BSIS      TYPE TABLE OF BSIS,    "Contabilidade financ.: índice secundário p/contas do Razão
       GW_BSIS      TYPE BSIS,
       GT_T001      TYPE TABLE OF T001,
       GW_T001      TYPE T001,
       GT_KNA1      TYPE TABLE OF KNA1,
       GW_KNA1      TYPE KNA1,
       GT_ZSDT0053  TYPE TABLE OF ZSDT0053,
       GW_ZSDT0053  TYPE ZSDT0053,
       GW_ZSDT0041  TYPE ZSDT0041,
       GW_ZSDT0040  TYPE ZSDT0040,
       GW_ZSDT0090  TYPE ZSDT0090,
       GT_VBAK      TYPE TABLE OF VBAK,
       GW_VBAK      TYPE VBAK,
       GT_SKAT      TYPE TABLE OF SKAT,
       GW_SKAT      TYPE SKAT,
       GT_ZSDT0051  TYPE TABLE OF ZSDT0051,
       GW_ZSDT0051  TYPE ZSDT0051,
       GT_VBFA      TYPE TABLE OF VBFA,
       GW_VBFA      TYPE VBFA,
       GT_MARA      TYPE TABLE OF MARA,
       GW_MARA      TYPE MARA,
       GT_MAKT      TYPE TABLE OF MAKT,
       GW_MAKT      TYPE MAKT,
       GT_BKPF      TYPE TABLE OF BKPF,
       GW_BKPF      TYPE BKPF,
       GT_SKA1      TYPE TABLE OF SKA1,
       GW_SKA1      TYPE SKA1,
       GT_ZSDT0066  TYPE TABLE OF ZSDT0066,
       GW_ZSDT0066  TYPE ZSDT0066.

 DATA: T_CONTA   TYPE STANDARD TABLE OF RGSB4 WITH HEADER LINE,
       IT_CONTAS TYPE TABLE OF TY_CONTAS,
       WA_CONTAS TYPE TY_CONTAS,
       GT_SAIDA  TYPE TABLE OF TY_SAIDA, "Estrutura de Saída.
       GW_SAIDA  TYPE TY_SAIDA.

 "===========================================================================
 " Váriaveis
 "===========================================================================
 DATA: VAR_LINHAS      TYPE SY-TABIX,
       VAR_DATA_ANTIGA TYPE C,
       VL_MIN_DATA     TYPE DATE,
       VL_WAERS        TYPE BKPF-WAERS.

 "===========================================================================
 " Constante
 "====================================================================
 DATA: CS_TAX_FIX TYPE ZLEST0061-TAX_DOLAR VALUE '3.7870'.

 "===========================================================================
 " Ranges
 "===========================================================================
 RANGES IT_BUKRS   FOR T001-BUKRS.
 RANGES IT_WERKS   FOR T001W-WERKS.
 RANGES IT_AUGDT   FOR BSAD-AUGDT.
 RANGES IT_KUNNR   FOR BSAD-KUNNR.
 RANGES IT_VBEL2   FOR BSAD-VBEL2.
 RANGES IT_NRO_SOL FOR ZSDT0053-NRO_SOL_OV.
 RANGES IT_GJAHR   FOR BSIS-GJAHR.
 RANGES IT_AUART   FOR VBAK-AUART.
 RANGES IT_BSCHL   FOR BSAD-BSCHL.

 "Criar os Ranges.

  DESCRIBE TABLE IT_EMPRESA LINES VAR_LINHAS.
  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_EMPRESA.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 1.
      IT_BUKRS-SIGN   = 'I'.
      IT_BUKRS-OPTION = 'EQ'.
      IF NOT ( GW_EMPRESA-BUKRS IS INITIAL ).
        IT_BUKRS-LOW    = GW_EMPRESA-BUKRS.
        IT_BUKRS-HIGH   = GW_EMPRESA-BUKRS.
        APPEND IT_BUKRS.
      ENDIF.
    WHEN: '2'.
      IT_BUKRS-SIGN   = 'I'.
      IT_BUKRS-OPTION = 'BT'.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 1.
      IT_BUKRS-LOW    = GW_EMPRESA-BUKRS.
      READ TABLE IT_EMPRESA INTO GW_EMPRESA INDEX 2.
      IT_BUKRS-HIGH   = GW_EMPRESA-BUKRS.
      IF NOT ( IT_BUKRS-LOW IS INITIAL  ) AND NOT ( IT_BUKRS-HIGH IS INITIAL  ).
        APPEND IT_BUKRS.
      ENDIF.
  ENDCASE.

  CLEAR: VAR_LINHAS.

  DESCRIBE TABLE IT_DATA LINES VAR_LINHAS.
  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_DATA.
      READ TABLE IT_DATA INTO GW_DATA INDEX 1.
      IT_AUGDT-SIGN   = 'I'.
      IT_AUGDT-OPTION = 'EQ'.
      IT_AUGDT-LOW    = GW_DATA-AUGDT.
      APPEND IT_AUGDT.

      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'EQ'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4).
      APPEND IT_GJAHR.

      IF ( GW_DATA-AUGDT(4) EQ '2013' ) OR ( GW_DATA-AUGDT(4) EQ '2014').
        VAR_DATA_ANTIGA = 'X'.
      ENDIF.

    WHEN: '2'.
      IT_AUGDT-SIGN   = 'I'.
      IT_AUGDT-OPTION = 'BT'.
      CLEAR: GW_DATA.
      READ TABLE IT_DATA INTO GW_DATA INDEX 1.
      IT_AUGDT-LOW    = GW_DATA-AUGDT.


      IT_GJAHR-SIGN   = 'I'.
      IT_GJAHR-OPTION = 'BT'.
      IT_GJAHR-LOW    = GW_DATA-AUGDT(4).

      IF ( GW_DATA-AUGDT(4) EQ '2013' ) OR ( GW_DATA-AUGDT(4) EQ '2014').
        VAR_DATA_ANTIGA = 'X'.
      ENDIF.

      READ TABLE IT_DATA INTO GW_DATA INDEX 2.
      IT_AUGDT-HIGH   = GW_DATA-AUGDT.

      IT_GJAHR-HIGH   = GW_DATA-AUGDT(4).

      IF NOT ( IT_AUGDT-LOW IS INITIAL ) AND NOT ( IT_AUGDT-HIGH IS INITIAL ).
        APPEND IT_AUGDT.
        APPEND IT_GJAHR.
      ENDIF.

  ENDCASE.

  CLEAR: VAR_LINHAS.

  DESCRIBE TABLE IT_CLIENTE LINES VAR_LINHAS.

  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_CLIENTE.
      READ TABLE IT_CLIENTE INTO GW_CLIENTE INDEX 1.
      IT_KUNNR-SIGN   = 'I'.
      IT_KUNNR-OPTION = 'EQ'.
      IT_KUNNR-LOW    = GW_CLIENTE-KUNNR.
      APPEND IT_KUNNR.
    WHEN: '2'.

      IT_KUNNR-SIGN   = 'I'.
      IT_KUNNR-OPTION = 'BT'.

      CLEAR: GW_CLIENTE.
      READ TABLE IT_CLIENTE INTO GW_CLIENTE INDEX 1.
      IT_KUNNR-LOW    = GW_CLIENTE-KUNNR.
      READ TABLE IT_CLIENTE INTO GW_CLIENTE INDEX 2.
      IT_KUNNR-HIGH   = GW_CLIENTE-KUNNR.
      IF NOT ( IT_KUNNR-LOW IS INITIAL ) AND NOT ( IT_KUNNR-HIGH IS INITIAL ).
        APPEND IT_KUNNR.
      ENDIF.

    WHEN OTHERS.

      IT_KUNNR-SIGN   = 'I'.
      IT_KUNNR-OPTION = 'EQ'.

      LOOP AT IT_CLIENTE INTO GW_CLIENTE.
        IT_KUNNR-LOW    = GW_CLIENTE-KUNNR.
        APPEND IT_KUNNR.
      ENDLOOP.

  ENDCASE.

  CLEAR: VAR_LINHAS.
  DESCRIBE TABLE IT_NR_OV LINES VAR_LINHAS.
  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_NR_OV.
      READ TABLE IT_NR_OV INTO GW_NR_OV INDEX 1.
      IT_VBEL2-SIGN   = 'I'.
      IT_VBEL2-OPTION = 'EQ'.
      IT_VBEL2-LOW    = GW_NR_OV-VBEL2.
      IT_VBEL2-HIGH    = GW_NR_OV-VBEL2.
      APPEND IT_VBEL2.
    WHEN: '2'.
      IT_VBEL2-SIGN   = 'I'.
      IT_VBEL2-OPTION = 'BT'.
      READ TABLE IT_NR_OV INTO GW_NR_OV INDEX 1.
      IT_VBEL2-LOW    = GW_NR_OV-VBEL2.
      READ TABLE IT_NR_OV INTO GW_NR_OV INDEX 2.
      IT_VBEL2-HIGH    = GW_NR_OV-VBEL2.
      IF NOT ( IT_VBEL2-LOW IS INITIAL ) AND NOT ( IT_VBEL2-HIGH IS INITIAL ).
        APPEND IT_VBEL2.
      ENDIF.
  ENDCASE.

  CLEAR: VAR_LINHAS.
  DESCRIBE TABLE IT_NR_SOL LINES VAR_LINHAS.
  CASE VAR_LINHAS.
    WHEN: '1'.
      CLEAR: GW_NR_SOL.
      READ TABLE IT_NR_SOL INTO GW_NR_SOL INDEX 1.
      IT_NRO_SOL-SIGN   = 'I'.
      IT_NRO_SOL-OPTION = 'EQ'.
      IT_NRO_SOL-LOW    = GW_NR_SOL-NRO_SOL_OV.
      IT_NRO_SOL-HIGH   = GW_NR_SOL-NRO_SOL_OV.
      APPEND IT_NRO_SOL .
    WHEN: '2'.
      IT_NRO_SOL-SIGN   = 'I'.
      IT_NRO_SOL-OPTION = 'BT'.
      READ TABLE IT_NR_SOL INTO GW_NR_SOL INDEX 1.
      IT_NRO_SOL-LOW = GW_NR_SOL-NRO_SOL_OV.
      READ TABLE IT_NR_SOL INTO GW_NR_SOL INDEX 2.
      IT_NRO_SOL-HIGH = GW_NR_SOL-NRO_SOL_OV.
      IF NOT ( IT_NRO_SOL-LOW IS INITIAL ) AND ( IT_NRO_SOL-HIGH IS INITIAL ).
        APPEND IT_NRO_SOL.
      ENDIF.
  ENDCASE.

  "Define Tipo O.V.
  CLEAR: IT_AUART[].
  IT_AUART-SIGN   = 'I'.
  IT_AUART-OPTION = 'EQ'.

  LOOP AT IT_TVAK INTO DATA(WL_TVAK).
    IT_AUART-LOW   = WL_TVAK-AUART.
    APPEND IT_AUART.
  ENDLOOP.

  CHECK IT_AUART[] IS NOT INITIAL.

  "Define Chave Cliente
  CLEAR: IT_BSCHL[].
  IT_BSCHL-SIGN   = 'I'.
  IT_BSCHL-OPTION = 'EQ'.

  CASE P_TIPO.
    WHEN 'AQ' OR 'PA'.
      IT_BSCHL-LOW = '01'. APPEND IT_BSCHL.
    WHEN OTHERS.
      IT_BSCHL-LOW = '09'. APPEND IT_BSCHL.
      IT_BSCHL-LOW = '19'. APPEND IT_BSCHL.
  ENDCASE.

  CHECK IT_BSCHL[] IS NOT INITIAL.

  REFRESH GT_SAIDA.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_HEDGE_64_JROS'
    TABLES
      SET_VALUES    = T_CONTA
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  SORT T_CONTA BY FROM.

  REFRESH IT_CONTAS.
  CLEAR: WA_CONTAS.

  LOOP AT T_CONTA.
    IF ( T_CONTA-FROM IS NOT INITIAL ).
      WA_CONTAS-HKONT = T_CONTA-FROM(10).
      APPEND WA_CONTAS TO IT_CONTAS.
    ENDIF.
    CLEAR: WA_CONTAS.
  ENDLOOP.

  VL_MIN_DATA = '20160323'.

  IF I_WAERS IS NOT INITIAL.
    VL_WAERS = I_WAERS.
  ELSE.
    VL_WAERS = 'BRL'.
  ENDIF.

  CHECK IT_CONTAS[] IS NOT INITIAL.

  "Contabilidade financ.: índice secundário p/contas do Razão
   SELECT *
     FROM BSIS
     INTO TABLE GT_BSIS
     FOR ALL ENTRIES IN IT_CONTAS
   WHERE BUKRS IN IT_BUKRS
     AND HKONT EQ IT_CONTAS-HKONT
     AND GJAHR IN IT_GJAHR
     AND WAERS EQ VL_WAERS
     AND BUDAT >= VL_MIN_DATA
     AND BUDAT IN IT_AUGDT.

   IF NOT ( GT_BSIS[] IS INITIAL ).

     CASE P_OPCAO.
       WHEN 'TR'.

         CASE VL_WAERS.
           WHEN 'USD'.
             SELECT *
               FROM BSAD AS A
               INTO TABLE GT_BSAD
               FOR ALL ENTRIES IN GT_BSIS
              WHERE BUKRS EQ GT_BSIS-BUKRS
                AND AUGBL EQ GT_BSIS-BELNR
                AND GJAHR EQ GT_BSIS-GJAHR
                AND VBEL2 IN IT_VBEL2
                AND KUNNR IN IT_KUNNR
                AND WAERS EQ 'USD'
                AND ZTERM EQ 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''
                AND EXISTS ( SELECT B~VBELV
                               FROM ZSDT0090 AS B
                              WHERE B~VBELV     EQ A~VBEL2
                                AND B~CATEGORIA EQ 'C'
                                AND B~ESTORNO   EQ '' ).
           WHEN 'BRL'.
             SELECT *
               FROM BSAD
               INTO TABLE GT_BSAD
               FOR ALL ENTRIES IN GT_BSIS
              WHERE BUKRS EQ GT_BSIS-BUKRS
                AND AUGBL EQ GT_BSIS-BELNR
                AND GJAHR EQ GT_BSIS-GJAHR
                AND VBEL2 IN IT_VBEL2
                AND KUNNR IN IT_KUNNR
                AND WAERS EQ 'BRL'
                AND ZTERM EQ 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''.
         ENDCASE.

       WHEN 'VC'.

         CASE VL_WAERS.
           WHEN 'USD'.
             "Contab.financ.: índice secundário p/clientes (partida liq.)
             SELECT *
               FROM BSAD AS A
               INTO TABLE GT_BSAD
               FOR ALL ENTRIES IN GT_BSIS
              WHERE BUKRS EQ GT_BSIS-BUKRS
                AND AUGBL EQ GT_BSIS-BELNR
                AND GJAHR EQ GT_BSIS-GJAHR
                AND VBEL2 IN IT_VBEL2
                AND KUNNR IN IT_KUNNR
                AND WAERS EQ 'USD'
                AND ZTERM NE 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''
                AND EXISTS ( SELECT B~VBELV
                               FROM ZSDT0090 AS B
                              WHERE B~VBELV     EQ A~VBEL2
                                AND B~CATEGORIA EQ 'C'
                                AND B~ESTORNO   EQ '' ).

             "Ajustes CS2019001627 - Ini
             CLEAR: GT_BSAD_JD[], GT_BSAD_JD[], GT_BSIS_JD[], GT_SKA1_JD[].

             SELECT *
               FROM BSAD AS A
               INTO TABLE GT_BSAD
               FOR ALL ENTRIES IN GT_BSIS
              WHERE BUKRS EQ GT_BSIS-BUKRS
                AND BELNR EQ GT_BSIS-BELNR
                AND GJAHR EQ GT_BSIS-GJAHR
                AND VBEL2 IN IT_VBEL2
                AND KUNNR IN IT_KUNNR
                AND WAERS EQ 'USD'
                AND ZTERM NE 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''
                AND EXISTS ( SELECT B~VBELV
                               FROM ZSDT0090 AS B
                              WHERE B~VBELV     EQ A~VBEL2
                                AND B~CATEGORIA EQ 'C'
                                AND B~ESTORNO   EQ '' ).

             IF GT_BSAD_JD[] IS NOT INITIAL.
               SELECT *
                 FROM BSIS INTO TABLE GT_BSIS_JD
                  FOR ALL ENTRIES IN GT_BSAD_JD
                WHERE BUKRS EQ GT_BSAD_JD-BUKRS
                  AND BELNR EQ GT_BSAD_JD-AUGBL
                  AND GJAHR EQ GT_BSAD_JD-GJAHR.

               IF GT_BSIS_JD[] IS NOT INITIAL.
                 SELECT *                           "#EC CI_DB_OPERATION_OK[2431747] "#EC CI_DB_OPERATION_OK[2389136]
                   FROM SKA1 INTO TABLE GT_SKA1_JD  "#EC CI_DB_OPERATION_OK[2431747]
                    FOR ALL ENTRIES IN GT_BSIS_JD   "#EC CI_DB_OPERATION_OK[2389136]
                  WHERE SAKNR EQ GT_BSIS_JD-HKONT
                    AND KTOPL EQ '0050'
                    AND KTOKS EQ 'YB04'.
               ENDIF.
             ENDIF.

             LOOP AT GT_BSAD_JD.
               LOOP AT GT_BSIS_JD WHERE BUKRS = GT_BSAD_JD-BUKRS
                                    AND BELNR = GT_BSAD_JD-AUGBL
                                    AND GJAHR = GT_BSAD_JD-GJAHR.

                READ TABLE GT_SKA1_JD WITH KEY SAKNR = GT_BSIS_JD-HKONT.
                CHECK SY-SUBRC EQ 0.

                APPEND GT_BSAD_JD TO GT_BSAD.
               ENDLOOP.
             ENDLOOP.
             "Ajustes CS2019001627 - Fim


           WHEN 'BRL'.
             "Contab.financ.: índice secundário p/clientes (partida liq.)

             SELECT *
              FROM BSAD
              INTO TABLE GT_BSAD
              FOR ALL ENTRIES IN GT_BSIS
             WHERE BUKRS EQ GT_BSIS-BUKRS
               AND AUGBL EQ GT_BSIS-BELNR
               AND GJAHR EQ GT_BSIS-GJAHR
               AND VBEL2 IN IT_VBEL2
               AND KUNNR IN IT_KUNNR
               AND WAERS EQ 'BRL'
               AND ZTERM NE 'C002'
               AND BSCHL IN IT_BSCHL
               AND VBEL2 NE ''.


             "Ajustes CS2019001627 - Ini
             CLEAR: GT_BSAD_JD[], GT_BSAD_JD[], GT_BSIS_JD[], GT_SKA1_JD[].

             SELECT *
              FROM BSAD
              INTO TABLE GT_BSAD_JD
              FOR ALL ENTRIES IN GT_BSIS
             WHERE BUKRS EQ GT_BSIS-BUKRS
               AND BELNR EQ GT_BSIS-BELNR "Doc. Origem
               AND GJAHR EQ GT_BSIS-GJAHR
               AND VBEL2 IN IT_VBEL2
               AND KUNNR IN IT_KUNNR
               AND WAERS EQ 'BRL'
               AND ZTERM NE 'C002'
               AND BSCHL IN IT_BSCHL
               AND VBEL2 NE ''.

             IF GT_BSAD_JD[] IS NOT INITIAL.
               SELECT *
                 FROM BSIS INTO TABLE GT_BSIS_JD
                  FOR ALL ENTRIES IN GT_BSAD_JD
                WHERE BUKRS EQ GT_BSAD_JD-BUKRS
                  AND BELNR EQ GT_BSAD_JD-AUGBL
                  AND GJAHR EQ GT_BSAD_JD-GJAHR.

               IF GT_BSIS_JD[] IS NOT INITIAL.
                 SELECT *                           "#EC CI_DB_OPERATION_OK[2389136]
                   FROM SKA1 INTO TABLE GT_SKA1_JD  "#EC CI_DB_OPERATION_OK[2431747]
                    FOR ALL ENTRIES IN GT_BSIS_JD
                  WHERE SAKNR EQ GT_BSIS_JD-HKONT
                    AND KTOPL EQ '0050'
                    AND KTOKS EQ 'YB04'.
               ENDIF.
             ENDIF.

             LOOP AT GT_BSAD_JD.
               LOOP AT GT_BSIS_JD WHERE BUKRS = GT_BSAD_JD-BUKRS
                                    AND BELNR = GT_BSAD_JD-AUGBL
                                    AND GJAHR = GT_BSAD_JD-GJAHR.

                READ TABLE GT_SKA1_JD WITH KEY SAKNR = GT_BSIS_JD-HKONT.
                CHECK SY-SUBRC EQ 0.

                APPEND GT_BSAD_JD TO GT_BSAD.
               ENDLOOP.
             ENDLOOP.
             "Ajustes CS2019001627 - Fim

         ENDCASE.



     ENDCASE.

   ENDIF.


*   IF NOT ( GT_BSAD[] IS INITIAL ).

*     SELECT * FROM BKPF
*       INTO TABLE GT_BKPF
*       FOR ALL ENTRIES IN GT_BSAD
*    WHERE BELNR EQ GT_BSAD-AUGBL
*      AND GJAHR EQ GT_BSAD-GJAHR
*      AND BUKRS EQ GT_BSAD-BUKRS
*      AND WAERS EQ 'BRL'.
*
*     "Mestre de clientes (parte geral)
*     SELECT * FROM KNA1
*       INTO TABLE GT_KNA1
*       FOR ALL ENTRIES IN GT_BSAD
*     WHERE KUNNR EQ GT_BSAD-KUNNR.
*
*     "Empresa
*     SELECT * FROM T001
*       INTO TABLE GT_T001
*       FOR ALL ENTRIES IN GT_BSAD
*     WHERE BUKRS EQ GT_BSAD-BUKRS.
*
*     "Mestre de contas do Razão (plano de contas)
*     SELECT * FROM SKA1
*       INTO TABLE GT_SKA1
*       FOR ALL ENTRIES IN GT_BSIS
*     WHERE SAKNR EQ GT_BSIS-HKONT.
*
*
*     SELECT * FROM SKAT
*       INTO TABLE GT_SKAT
*       FOR ALL ENTRIES IN GT_SKA1
*     WHERE SAKNR EQ GT_SKA1-SAKNR
*           AND SPRAS EQ 'P'.
*
*     "Tabela de Solicitação de ordem de venda - MATERIAIS
*     SELECT *
*       FROM ZSDT0053
*       INTO TABLE GT_ZSDT0053
*         FOR ALL ENTRIES IN GT_BSAD
*        WHERE VBELN      EQ GT_BSAD-VBEL2
*          AND NRO_SOL_OV IN IT_NRO_SOL.
*
*     "Tabela de Solicitação Ordem de Venda - Cabeçalho
*     SELECT *
*       FROM ZSDT0051
*       INTO TABLE GT_ZSDT0051
*        FOR ALL ENTRIES IN GT_ZSDT0053
*      WHERE NRO_SOL_OV EQ GT_ZSDT0053-NRO_SOL_OV.
*
*     "Dados gerais de material
*     SELECT *
*       FROM MARA
*       INTO TABLE GT_MARA
*        FOR ALL ENTRIES IN GT_ZSDT0053
*      WHERE MATNR EQ GT_ZSDT0053-MATNR.
*
*      "Tabela de Solicitação de Ordem de Venda – Formação de Lote
*       SELECT *
*         FROM ZSDT0066
*         INTO TABLE GT_ZSDT0066
*         FOR ALL ENTRIES IN GT_BSAD
*       WHERE VBELN EQ GT_BSAD-VBEL2
*         AND NRO_SOL_OV IN IT_NRO_SOL.
*
*       IF ( SY-SUBRC EQ 0 ).
*         "Dados gerais de material
*         SELECT *
*           FROM MARA
*           APPENDING TABLE GT_MARA
*           FOR ALL ENTRIES IN GT_ZSDT0066
*         WHERE MATNR EQ GT_ZSDT0066-MATNR.
*       ENDIF.
*
*       "Descrição do Material
*       SELECT *
*         FROM MAKT
*         INTO TABLE GT_MAKT
*         FOR ALL ENTRIES IN GT_MARA
*       WHERE MATNR EQ GT_MARA-MATNR.
*
*       "Documento de vendas: dados de cabeçalho
*       SELECT *
*         FROM VBAK
*         INTO TABLE GT_VBAK
*         FOR ALL ENTRIES IN GT_BSAD
*       WHERE VBELN EQ GT_BSAD-VBEL2
*         AND AUART IN ('ZCOP','ZCPV','ZMIT','ZREB','ZOFE','ZODF', 'ZREM','ZTRI', 'ZFEX').
*
*       SELECT *
*         FROM VBFA
*         INTO TABLE GT_VBFA
*         FOR ALL ENTRIES IN GT_BSAD
*      WHERE VBELN   EQ GT_BSAD-VBEL2
*        AND VBTYP_N IN ('C','H','L')
*        AND VBTYP_V EQ 'C'.
*
*       IF ( SY-SUBRC EQ 0 ).
*         "Tabela de Solicitação de ordem de venda - MATERIAIS
*         SELECT *
*           FROM ZSDT0053
*           APPENDING TABLE GT_ZSDT0053
*           FOR ALL ENTRIES IN GT_VBFA
*         WHERE VBELN      EQ GT_VBFA-VBELV
*           AND NRO_SOL_OV IN IT_NRO_SOL.
*       ENDIF.

*   ENDIF. "IF NOT ( GT_BSAD[] IS INITIAL ).

   "===========================================================================
   " BSIS (Saída)
   "===========================================================================
   IF ( GT_BSAD[] IS NOT INITIAL ) AND ( GT_BSIS[] IS NOT INITIAL ).
     LOOP AT  GT_BSIS INTO GW_BSIS.

       CLEAR: GW_SAIDA, GW_VBFA, GW_MARA, GW_MAKT, GW_T001, GW_BSAD, GW_BSAD_AUX, GW_KNA1, GW_SKA1, GW_SKAT, GW_ZSDT0053,
              GW_ZSDT0040, GW_ZSDT0041, GW_ZSDT0051, GW_ZSDT0066, GW_VBAK, GW_VBFA.

       VAR_TABIX = SY-TABIX.

       GW_SAIDA-WAERS = GW_BSIS-WAERS.
       GW_SAIDA-BUKRS = GW_BSIS-BUKRS.

       SELECT SINGLE * INTO GW_T001 FROM T001 WHERE BUKRS = GW_BSIS-BUKRS.
       GW_SAIDA-BUTXT = GW_T001-BUTXT.

       GW_SAIDA-GSBER = GW_BSIS-GSBER.

       READ TABLE GT_BSAD INTO GW_BSAD WITH KEY BUKRS = GW_BSIS-BUKRS
                                                AUGBL = GW_BSIS-BELNR
                                                GJAHR = GW_BSIS-GJAHR.

       IF SY-SUBRC <> 0.
         READ TABLE GT_BSAD INTO GW_BSAD WITH KEY BUKRS = GW_BSIS-BUKRS
                                                  BELNR = GW_BSIS-BELNR
                                                  GJAHR = GW_BSIS-GJAHR.
         IF SY-SUBRC <> 0.
           CONTINUE.
         ENDIF.
       ENDIF.

       IF P_OPCAO = 'VC'.


         CASE VL_WAERS.
           WHEN 'USD'.
             CLEAR: GW_BSAD_AUX.
             SELECT SINGLE *
               INTO GW_BSAD_AUX
               FROM BSAD AS A
              WHERE BUKRS = GW_BSIS-BUKRS
                AND GJAHR = GW_BSIS-GJAHR
                AND AUGBL = GW_BSIS-BELNR
                AND WAERS EQ 'USD'
                AND ZTERM EQ 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''
                AND EXISTS ( SELECT B~VBELV
                               FROM ZSDT0090 AS B
                              WHERE B~VBELV     EQ A~VBEL2
                                AND B~CATEGORIA EQ 'C'
                                AND B~ESTORNO   EQ '' ).
             IF SY-SUBRC = 0.
               CONTINUE.
             ENDIF.
           WHEN 'BRL'.
             CLEAR: GW_BSAD_AUX.
             SELECT SINGLE *
               INTO GW_BSAD_AUX
               FROM BSAD
              WHERE BUKRS = GW_BSIS-BUKRS
                AND GJAHR = GW_BSIS-GJAHR
                AND AUGBL = GW_BSIS-BELNR
                AND WAERS EQ 'BRL'
                AND ZTERM EQ 'C002'
                AND BSCHL IN IT_BSCHL
                AND VBEL2 NE ''.
             IF SY-SUBRC = 0.
               CONTINUE.
             ENDIF.
         ENDCASE.

       ENDIF.

       GW_SAIDA-KUNNR = GW_BSAD-KUNNR.

       SELECT SINGLE * INTO GW_KNA1 FROM KNA1 WHERE KUNNR = GW_SAIDA-KUNNR.
       GW_SAIDA-NAME1 = GW_KNA1-NAME1.

       IF ( GW_BSAD-UMSKS IS NOT INITIAL ).
         GW_SAIDA-TIPO = 'Adiantamento'.
       ELSE.
         GW_SAIDA-TIPO = 'Fatura'.
       ENDIF.

       SELECT SINGLE * FROM ZSDT0041 INTO GW_ZSDT0041 WHERE VBELN = GW_BSAD-VBEL2.

       IF SY-SUBRC = 0.

         SELECT SINGLE * INTO GW_VBAK FROM VBAK WHERE VBELN = GW_BSAD-VBEL2.

         CHECK SY-SUBRC = 0.

         IF ( GW_VBAK-AUART IS INITIAL ).
           CONTINUE.
         ENDIF.

         SELECT SINGLE * FROM ZSDT0040 INTO GW_ZSDT0040 WHERE DOC_SIMULACAO = GW_ZSDT0041-DOC_SIMULACAO.

         IF SY-SUBRC = 0.
           GW_SAIDA-TPSIM  = GW_ZSDT0040-TPSIM.
           GW_SAIDA-AUART  = GW_ZSDT0041-AUART.
         ENDIF.

         SELECT SINGLE * INTO GW_MARA FROM MARA WHERE MATNR = GW_ZSDT0041-MATNR.
         IF SY-SUBRC = 0.
           GW_SAIDA-MATNR = GW_MARA-MATNR.
           GW_SAIDA-MATKL = GW_MARA-MATKL.
           SELECT SINGLE * INTO GW_MAKT FROM MAKT WHERE MATNR = GW_MARA-MATNR.
           GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
         ENDIF.

         GW_SAIDA-NR_SOL = GW_ZSDT0041-DOC_SIMULACAO.

       ENDIF.

       IF GW_SAIDA-NR_SOL IS INITIAL.
         SELECT SINGLE * FROM ZSDT0090 INTO GW_ZSDT0090 WHERE VBELV = GW_BSAD-VBEL2.
         IF SY-SUBRC NE 0.
           SELECT SINGLE * FROM ZSDT0090 INTO GW_ZSDT0090 WHERE VBELN = GW_BSAD-VBEL2.
         ENDIF.
         IF ( GW_SAIDA-NR_SOL IS INITIAL ) AND ( SY-SUBRC = 0 ).
           SELECT SINGLE * FROM ZSDT0040 INTO GW_ZSDT0040 WHERE DOC_SIMULACAO = GW_ZSDT0090-DOC_SIMULACAO.
           IF SY-SUBRC = 0.
             GW_SAIDA-NR_SOL = GW_ZSDT0040-DOC_SIMULACAO.
             GW_SAIDA-TPSIM  = GW_ZSDT0040-TPSIM.
           ENDIF.
         ENDIF.
       ENDIF.

       IF GW_SAIDA-NR_SOL IS INITIAL.

         SELECT SINGLE * INTO GW_ZSDT0053 FROM ZSDT0053 WHERE VBELN = GW_BSAD-VBEL2
                                                          AND NRO_SOL_OV IN IT_NRO_SOL.
         IF ( SY-SUBRC NE 0 ).
           SELECT SINGLE *
             INTO GW_ZSDT0066
             FROM ZSDT0066
            WHERE VBELN = GW_BSAD-VBEL2
              AND NRO_SOL_OV   IN IT_NRO_SOL.
           IF ( SY-SUBRC NE 0 ).

             SELECT SINGLE * INTO GW_VBAK FROM VBAK WHERE VBELN = GW_BSAD-VBEL2.

             IF ( GW_VBAK-AUART IS INITIAL ).
               CONTINUE.
             ELSE.
               GW_SAIDA-AUART = GW_VBAK-AUART.
             ENDIF.

             SELECT SINGLE * INTO GW_ZSDT0051 FROM ZSDT0051 WHERE NRO_SOL_OV = GW_ZSDT0053-NRO_SOL_OV.
             GW_SAIDA-TP_VENDA = GW_ZSDT0051-TP_VENDA.

             CASE GW_VBAK-AUART.
               WHEN: 'ZCPV' OR 'ZREB' OR 'ZCOP'.

                 SELECT SINGLE * INTO GW_VBFA FROM VBFA WHERE VBELN = GW_BSAD-VBEL2.

                 SELECT SINGLE * INTO GW_ZSDT0053 FROM ZSDT0053 WHERE VBELN = GW_VBFA-VBELV
                                                                  AND NRO_SOL_OV IN IT_NRO_SOL.

                 GW_SAIDA-NR_SOL = GW_ZSDT0053-NRO_SOL_OV.

                 SELECT SINGLE * INTO GW_ZSDT0051 FROM ZSDT0051 WHERE NRO_SOL_OV = GW_ZSDT0053-NRO_SOL_OV.
                 GW_SAIDA-TP_VENDA = GW_ZSDT0051-TP_VENDA.

                 SELECT SINGLE * INTO GW_MARA FROM MARA WHERE MATNR = GW_ZSDT0053-MATNR.
                 IF SY-SUBRC = 0.
                   GW_SAIDA-MATNR = GW_MARA-MATNR.
                   GW_SAIDA-MATKL = GW_MARA-MATKL.
                   SELECT SINGLE * INTO GW_MAKT FROM MAKT WHERE MATNR = GW_MARA-MATNR.
                   GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
                 ENDIF.
             ENDCASE.

           ENDIF.

           SELECT SINGLE * INTO GW_VBAK FROM VBAK WHERE VBELN = GW_BSAD-VBEL2.

           IF ( GW_VBAK-AUART IS INITIAL ).
             CONTINUE.
           ELSE.
             GW_SAIDA-AUART = GW_VBAK-AUART.
           ENDIF.

           SELECT SINGLE * INTO GW_ZSDT0051 FROM ZSDT0051 WHERE NRO_SOL_OV = GW_ZSDT0053-NRO_SOL_OV.
           GW_SAIDA-TP_VENDA = GW_ZSDT0051-TP_VENDA.

           SELECT SINGLE * INTO GW_MARA FROM MARA WHERE MATNR = GW_ZSDT0066-MATNR.
           IF SY-SUBRC = 0.
             GW_SAIDA-MATNR = GW_MARA-MATNR.
             GW_SAIDA-MATKL = GW_MARA-MATKL.
             SELECT SINGLE * INTO GW_MAKT FROM MAKT WHERE MATNR = GW_MARA-MATNR.
             GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
           ENDIF.

         ELSE.

           SELECT SINGLE * INTO GW_VBAK FROM VBAK WHERE VBELN = GW_BSAD-VBEL2.

           IF ( GW_VBAK-AUART IS INITIAL ).
             CONTINUE.
           ELSE.
             GW_SAIDA-AUART = GW_VBAK-AUART.
           ENDIF.

           SELECT SINGLE * INTO GW_ZSDT0051 FROM ZSDT0051 WHERE NRO_SOL_OV = GW_ZSDT0053-NRO_SOL_OV.
           GW_SAIDA-TP_VENDA = GW_ZSDT0051-TP_VENDA.

           SELECT SINGLE * INTO GW_MARA FROM MARA WHERE MATNR = GW_ZSDT0053-MATNR.
           IF SY-SUBRC = 0.
             GW_SAIDA-MATNR = GW_MARA-MATNR.
             GW_SAIDA-MATKL = GW_MARA-MATKL.
             SELECT SINGLE * INTO GW_MAKT FROM MAKT WHERE MATNR = GW_MARA-MATNR.
             GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
           ENDIF.

           GW_SAIDA-NR_SOL = GW_ZSDT0053-NRO_SOL_OV.
         ENDIF.

       ENDIF.

       IF ( GW_SAIDA-AUART IS INITIAL ) AND ( GW_BSAD-VBEL2 IS NOT INITIAL ).
         SELECT SINGLE * INTO GW_VBAK FROM VBAK WHERE VBELN = GW_BSAD-VBEL2.
         IF SY-SUBRC = 0.
           GW_SAIDA-AUART = GW_VBAK-AUART.
         ENDIF.
       ENDIF.

       IF ( GW_SAIDA-AUART NOT IN IT_AUART ).
         CONTINUE.
       ENDIF.

       GW_SAIDA-BUZEI = GW_BSIS-BUZEI.
       GW_SAIDA-BELNR = GW_BSIS-BELNR.
       GW_SAIDA-AUGBL = GW_BSIS-BELNR.

       GW_SAIDA-BUDAT = GW_BSIS-BUDAT.
       GW_SAIDA-AUGDT = GW_BSIS-AUGDT.
       GW_SAIDA-VBEL2 = GW_BSAD-VBEL2.
       GW_SAIDA-VBELN = GW_BSAD-VBELN.
       GW_SAIDA-BANCO_LIQ = 'Juros\Descontos'.

       IF ( GW_BSIS-DMBTR EQ 0 ) OR ( GW_BSIS-DMBE2 EQ 0 ).
         CONTINUE.
       ENDIF.

       IF ( GW_SAIDA-AUGDT EQ '20150910').
         GW_SAIDA-TX_CAMB = CS_TAX_FIX.
         GW_SAIDA-DMBTR   = GW_BSIS-DMBTR.
         GW_SAIDA-DMBE2   = GW_SAIDA-DMBTR / CS_TAX_FIX.
       ELSE.
         CATCH SYSTEM-EXCEPTIONS ARITHMETIC_ERRORS = 1.
           GW_SAIDA-TX_CAMB = GW_BSIS-DMBTR / GW_BSIS-DMBE2.
         ENDCATCH.
         GW_SAIDA-DMBTR = GW_BSIS-DMBTR.
         GW_SAIDA-DMBE2 = GW_BSIS-DMBE2.
         "PERFORM GET_VLR_DOLAR USING '03' P_OPCAO
         "                   CHANGING GW_SAIDA.
       ENDIF.

       IF GW_BSIS-SHKZG EQ 'H'.
         GW_SAIDA-DMBTR   = GW_SAIDA-DMBTR * -1.
         GW_SAIDA-DMBE2   = GW_SAIDA-DMBE2 * -1.
       ENDIF.

       CLEAR: GW_VBAP, GW_SAIDA-CHARG.
       IF ( GW_SAIDA-VBEL2 IS NOT INITIAL ).
         SELECT SINGLE *
           FROM VBAP INTO GW_VBAP
          WHERE VBELN = GW_SAIDA-VBEL2
            AND CHARG NE ''.

         IF ( SY-SUBRC = 0 ) AND ( SY-TCODE = 'ZFI0064' ).
           GW_SAIDA-CHARG = GW_VBAP-CHARG.
         ENDIF.
       ENDIF.

       IF ( GW_BSAD-VBEL2 IS NOT INITIAL ) AND ( GW_SAIDA-MATNR IS INITIAL OR GW_SAIDA-MATKL IS INITIAL ).
         SELECT SINGLE *
           FROM VBAP INTO GW_VBAP
          WHERE VBELN EQ GW_BSAD-VBEL2
            AND POSNR EQ GW_BSAD-POSN2.
         IF ( SY-SUBRC = 0 ) AND ( GW_VBAP-MATNR IS NOT INITIAL ).
           SELECT SINGLE * INTO GW_MARA FROM MARA WHERE MATNR = GW_VBAP-MATNR.
           IF SY-SUBRC = 0.
             GW_SAIDA-MATNR = GW_MARA-MATNR.
             GW_SAIDA-MATKL = GW_MARA-MATKL.
             SELECT SINGLE * INTO GW_MAKT FROM MAKT WHERE MATNR = GW_MARA-MATNR.
             GW_SAIDA-MAKTX = GW_MAKT-MAKTX.
           ENDIF.
         ENDIF.
       ENDIF.

       APPEND GW_SAIDA TO GT_SAIDA.

     ENDLOOP.

   ENDIF.



  IT_RESULTADO[] = GT_SAIDA[].

ENDFUNCTION.
