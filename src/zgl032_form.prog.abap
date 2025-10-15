*&---------------------------------------------------------------------*
*&  Include           ZGL032_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  DATA: OBJ_ZCL_UTIL_SD TYPE REF TO ZCL_UTIL_SD,
        VG_TX_USD_BRL   TYPE UKURS_CURR,
        VL_GDATU        TYPE GDATU_INV.

  "DATA: TG_SKA1_AUX LIKE TABLE OF TG_SKA1 WITH HEADER LINE.

  CLEAR: VG_NOT_FOUND.
  REFRESH: IT_SAIDA, TG_SKA1, TG_BSIS, TG_BSAD, TG_BSAK,TG_SKAT, TG_BKPF,  TG_BSIS_CBANCO,
           TG_KNA1, TG_T030H, TG_LFA1, TG_EKPO, TG_076, TG_077, TG_VAR_CALC, TG_BSAK_AUX, TG_BSAD_AUX,
           TG_TX_USD_BRL.

  CLEAR: R_BLART_CBANCO[], R_BLART_CBANCO[], R_FDLEV_BANCO[].
*-------------------------------------------------------------------*
*  Monta Range's
*-------------------------------------------------------------------*
  "GJAHR ---------------------------|
  R_GJAHR-SIGN   = 'I'.
  R_GJAHR-OPTION = 'EQ'.
  R_GJAHR-LOW    = S_BUDAT-LOW(4).
  R_GJAHR-HIGH   = S_BUDAT-LOW(4).

  IF S_BUDAT-HIGH IS NOT INITIAL..
    R_GJAHR-OPTION = 'BT'.
    R_GJAHR-HIGH   = S_BUDAT-HIGH(4).
  ENDIF.

  APPEND R_GJAHR.

  "BLART ---------------------------|
  R_BLART_CBANCO-SIGN   = 'I'.
  R_BLART_CBANCO-OPTION = 'EQ'.
  R_BLART_CBANCO-LOW    = 'DZ'.
  APPEND R_BLART_CBANCO.

  R_BLART_CBANCO-LOW    = 'KZ'.
  APPEND R_BLART_CBANCO.

  R_BLART_CBANCO-LOW    = 'ZP'.
  APPEND R_BLART_CBANCO.

  "FDLEV --------------------------|
  R_FDLEV_BANCO-SIGN   = 'I'.
  R_FDLEV_BANCO-OPTION = 'EQ'.
  R_FDLEV_BANCO-LOW    = 'F0'.
  APPEND R_FDLEV_BANCO.

  R_FDLEV_BANCO-LOW    = 'B2'.
  APPEND R_FDLEV_BANCO.

*-------------------------------------------------------------------*
*  Busca de Dados Contas de Variação
*-------------------------------------------------------------------*
  SELECT KTOPL KTOKS SAKNR "#EC CI_DB_OPERATION_OK[2389136]
    FROM SKA1 INTO TABLE TG_SKA1 "#EC CI_DB_OPERATION_OK[2431747]
   WHERE SAKNR IN S_HKTVR
     AND KTOPL = '0050'
*     AND KTOKS IN ( 'YB07','YB08' ) .  "era o grupo de conta de variação cambial ajjutada para yb05 e yb06
      AND KTOKS IN ( 'YB05','YB06' ) .

*  SORT TG_SKA1 BY KTOPL SAKNR.
*  "DELETE ADJACENT DUPLICATES FROM TG_SKA1 COMPARING KTOPL SAKNR.

  IF ( TG_SKA1[] IS INITIAL ).
    VG_NOT_FOUND = 'X'.
    RETURN.
  ENDIF.

*  TG_SKA1_AUX[] = TG_SKA1[].
*  IF S_HKTVR IS NOT INITIAL.
*    SELECT *
*      FROM SKA1 APPENDING CORRESPONDING FIELDS OF TABLE TG_SKA1_AUX
*     WHERE SAKNR IN S_HKTVR
*       AND KTOPL = '0050'
*       AND KTOKS = 'YB08'.
*  ENDIF.

*  SORT TG_SKA1_AUX BY KTOPL SAKNR.
*  DELETE ADJACENT DUPLICATES FROM TG_SKA1_AUX COMPARING KTOPL SAKNR.
*
*  IF ( TG_SKA1_AUX[] IS INITIAL ).
*    VG_NOT_FOUND = 'X'.
*    RETURN.
*  ENDIF.

*-------------------------------------------------------------------*
*  Busca de Dados Documentos contabil Variação
*-------------------------------------------------------------------*

  IF R_TP1 IS NOT INITIAL. "Fornecedor

    SELECT A~BUKRS A~BELNR A~BUZEI A~HKONT A~BUDAT A~WAERS A~WRBTR A~DMBTR A~DMBE2 A~SHKZG
           A~GJAHR A~BLART A~GSBER
      INTO CORRESPONDING FIELDS OF TABLE TG_BSIS
      FROM BSIS AS A
      FOR ALL ENTRIES IN TG_SKA1
     WHERE A~BUKRS IN S_BUKRS
       AND A~HKONT EQ TG_SKA1-SAKNR
       AND A~BUDAT IN S_BUDAT
       AND A~GJAHR IN R_GJAHR
       "AND A~WAERS NE 'BRL'
       AND A~WAERS IN S_WAERS
       AND EXISTS ( SELECT B~BELNR
                      FROM BSAK AS B
                     WHERE B~BUKRS EQ A~BUKRS
                       AND B~AUGBL EQ A~BELNR
                       AND B~HKONT IN S_HKTRC  "Conta de Reconciliação
                       AND B~LIFNR IN S_PARID
                    ).

  ELSEIF R_TP2 IS NOT INITIAL. "Cliente.

    SELECT A~BUKRS A~BELNR A~BUZEI A~HKONT A~BUDAT A~WAERS A~WRBTR A~DMBTR A~DMBE2 A~SHKZG
           A~GJAHR A~BLART A~GSBER
      INTO CORRESPONDING FIELDS OF TABLE TG_BSIS
      FROM BSIS AS A
      FOR ALL ENTRIES IN TG_SKA1
     WHERE A~BUKRS IN S_BUKRS
       AND A~HKONT EQ TG_SKA1-SAKNR
       AND A~BUDAT IN S_BUDAT
       AND A~GJAHR IN R_GJAHR
       "AND A~WAERS NE 'BRL'
       AND A~WAERS IN S_WAERS
       AND EXISTS ( SELECT B~BELNR
                      FROM BSAD AS B
                     WHERE B~BUKRS EQ A~BUKRS
                       AND B~AUGBL EQ A~BELNR
                       AND B~HKONT IN S_HKTRC  "Conta de Reconciliação
                       AND B~KUNNR IN S_PARID
                    ).
  ENDIF.

  "DELETE TG_BSIS WHERE DMBTR EQ 0
  "                 AND DMBE2 EQ 0.


  SORT TG_BSIS BY BUKRS HKONT GJAHR BELNR BUZEI.
  DELETE ADJACENT DUPLICATES FROM TG_BSIS COMPARING BUKRS HKONT GJAHR BELNR BUZEI.

  IF TG_BSIS[] IS NOT INITIAL.

    IF ( R_AR1 IS NOT INITIAL ) OR ( R_AR2 IS NOT INITIAL ). "Analise Calculo Variação

      IF R_TP1 IS NOT INITIAL. "Fornecedor
        "Busca de Dados Lançamentos Fornecedores
        SELECT BUKRS AUGBL GJAHR LIFNR HKONT AUGDT BELNR BUZEI BUDAT BLDAT WAERS
               XBLNR BLART SHKZG GSBER WRBTR DMBTR DMBE2 SGTXT EBELN EBELP
               UMSKS REBZG
          FROM BSAK INTO CORRESPONDING FIELDS OF TABLE TG_BSAK
           FOR ALL ENTRIES IN TG_BSIS
         WHERE BUKRS EQ TG_BSIS-BUKRS
           AND AUGBL EQ TG_BSIS-BELNR
           AND HKONT IN S_HKTRC "Conta de Reconciliação
           AND LIFNR IN S_PARID.
        "AND GJAHR EQ TG_BSIS-GJAHR
      ENDIF.

      IF R_TP2 IS NOT INITIAL. "Cliente

        "Busca de Dados Lançamentos Clientes
        SELECT BUKRS AUGBL GJAHR KUNNR HKONT AUGDT BELNR BUZEI BUDAT BLDAT WAERS
               XBLNR BLART SHKZG GSBER WRBTR DMBTR DMBE2 SGTXT VBELN VBEL2
               UMSKS REBZG
          FROM BSAD INTO CORRESPONDING FIELDS OF TABLE TG_BSAD
          FOR ALL ENTRIES IN TG_BSIS
         WHERE BUKRS EQ TG_BSIS-BUKRS
           AND AUGBL EQ TG_BSIS-BELNR
           AND HKONT IN S_HKTRC "Conta de Reconciliação
           AND KUNNR IN S_PARID.
        "AND GJAHR EQ TG_BSIS-GJAHR
      ENDIF.

    ENDIF.

    "Busca denominação Contas
    SELECT SAKNR TXT50
      FROM SKAT APPENDING TABLE TG_SKAT
      FOR ALL ENTRIES IN TG_BSIS
     WHERE SAKNR EQ TG_BSIS-HKONT
       AND SPRAS EQ SY-LANGU.

  ENDIF.

  IF TG_BSAD[] IS NOT INITIAL.

    LOOP AT TG_BSAD WHERE REBZG = 'V'.
      CLEAR: TG_BSAD-REBZG.
      MODIFY TG_BSAD.
    ENDLOOP.


    "Busca Taxas
*    TG_BSAD_AUX[] = TG_BSAD[].
*    SORT TG_BSAD_AUX BY AUGDT.
*    DELETE ADJACENT DUPLICATES FROM TG_BSAD_AUX COMPARING AUGDT.
*
*    LOOP AT TG_BSAD_AUX.
*
*      CHECK TG_BSAD_AUX-AUGDT IS NOT INITIAL.
*
*      MOVE TG_BSAD_AUX-AUGDT TO VL_GDATU.
*
*      FREE OBJ_ZCL_UTIL_SD.
*      CREATE OBJECT OBJ_ZCL_UTIL_SD.
*
*      OBJ_ZCL_UTIL_SD->SET_KURST('B').
*      OBJ_ZCL_UTIL_SD->SET_WAERK('USD').
*      OBJ_ZCL_UTIL_SD->SET_TCURR('BRL').
*      OBJ_ZCL_UTIL_SD->SET_DATA( VL_GDATU ).
*
*      VG_TX_USD_BRL = ABS( OBJ_ZCL_UTIL_SD->TAXA_CAMBIO( ) ).
*
*      IF VG_TX_USD_BRL <> 0.
*        TG_TX_USD_BRL-DATA = TG_BSAD_AUX-AUGDT.
*        TG_TX_USD_BRL-TAXA = VG_TX_USD_BRL.
*        APPEND TG_TX_USD_BRL.
*      ENDIF.
*    ENDLOOP.

    "Busca de Dados Taxa Cambio (BKPF)
    SELECT BUKRS BELNR GJAHR KURSF KURS2 WAERS STBLG BLART
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE TG_BKPF
      FOR ALL ENTRIES IN TG_BSAD
     WHERE BUKRS EQ TG_BSAD-BUKRS
       AND BELNR EQ TG_BSAD-AUGBL
       AND GJAHR EQ TG_BSAD-AUGDT(4).

    SELECT BSIS~BUKRS BSIS~BELNR BSIS~GJAHR BSIS~HKONT BSIS~DMBTR BSIS~DMBE2
           BSIS~WAERS BSIS~WRBTR
           APPENDING CORRESPONDING FIELDS OF TABLE TG_BSIS_CBANCO
      FROM BSIS INNER JOIN SKB1 ON BSIS~BUKRS = SKB1~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                               AND BSIS~HKONT = SKB1~SAKNR
       FOR ALL ENTRIES IN TG_BSAD
     WHERE BSIS~BUKRS EQ TG_BSAD-BUKRS
       AND BSIS~BELNR EQ TG_BSAD-AUGBL
       AND SKB1~FDLEV IN R_FDLEV_BANCO.

    SELECT BUKRS BELNR GJAHR KURSF KURS2 WAERS STBLG BLART
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE TG_BKPF
      FOR ALL ENTRIES IN TG_BSAD
     WHERE BUKRS EQ TG_BSAD-BUKRS
       AND BELNR EQ TG_BSAD-BELNR
       AND GJAHR EQ TG_BSAD-GJAHR.

    "Busca denominação Contas
    SELECT SAKNR TXT50
      FROM SKAT APPENDING TABLE TG_SKAT
      FOR ALL ENTRIES IN TG_BSAD
     WHERE SAKNR EQ TG_BSAD-HKONT
       AND SPRAS EQ SY-LANGU.

    SELECT KUNNR NAME1
      FROM KNA1 INTO TABLE TG_KNA1
      FOR ALL ENTRIES IN TG_BSAD
     WHERE KUNNR EQ TG_BSAD-KUNNR.

    "Determinação de contas p/diferença de câmbio nas PA
    SELECT HKONT LHREA
      FROM T030H INTO TABLE TG_T030H
      FOR ALL ENTRIES IN TG_BSAD
     WHERE HKONT EQ TG_BSAD-HKONT.

    "Partidas Residuais Liquidadas
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE TG_BSAD_RSD
      FROM BSAD AS A
      FOR ALL ENTRIES IN TG_BSAD
     WHERE A~BUKRS EQ TG_BSAD-BUKRS
       AND A~KUNNR EQ TG_BSAD-KUNNR
       AND A~UMSKS NE 'W'
       AND A~BELNR EQ TG_BSAD-AUGBL
       AND A~BELNR NE A~AUGBL
       AND A~WAERS EQ TG_BSAD-WAERS
       AND A~SHKZG EQ TG_BSAD-SHKZG
       AND ( ( A~REBZG EQ TG_BSAD-REBZG ) OR
             ( A~REBZG EQ TG_BSAD-BELNR ) ).

    "Partidas Residuais em Aberto
    SELECT *
      APPENDING CORRESPONDING FIELDS OF TABLE TG_BSAD_RSD
      FROM BSID AS A
      FOR ALL ENTRIES IN TG_BSAD
     WHERE A~BUKRS EQ TG_BSAD-BUKRS
       AND A~KUNNR EQ TG_BSAD-KUNNR
       AND A~UMSKS NE 'W'
       AND A~BELNR EQ TG_BSAD-AUGBL
       AND A~WAERS EQ TG_BSAD-WAERS
       AND A~SHKZG EQ TG_BSAD-SHKZG
       AND ( ( A~REBZG EQ TG_BSAD-REBZG ) OR
             ( A~REBZG EQ TG_BSAD-BELNR ) ).

  ENDIF.

  IF TG_BSAK[] IS NOT INITIAL.

    LOOP AT TG_BSAK WHERE REBZG = 'V'.
      CLEAR: TG_BSAK-REBZG.
      MODIFY TG_BSAK.
    ENDLOOP.

    "Busca Taxas
*    TG_BSAK_AUX[] = TG_BSAK[].
*    SORT TG_BSAK_AUX BY AUGDT.
*    DELETE ADJACENT DUPLICATES FROM TG_BSAK_AUX COMPARING AUGDT.
*
*    LOOP AT TG_BSAK_AUX.
*      CHECK TG_BSAK_AUX-AUGDT IS NOT INITIAL.
*
*      MOVE TG_BSAK_AUX-AUGDT TO VL_GDATU.
*
*      FREE OBJ_ZCL_UTIL_SD.
*      CREATE OBJECT OBJ_ZCL_UTIL_SD.
*
*      OBJ_ZCL_UTIL_SD->SET_KURST('B').
*      OBJ_ZCL_UTIL_SD->SET_WAERK('USD').
*      OBJ_ZCL_UTIL_SD->SET_TCURR('BRL').
*      OBJ_ZCL_UTIL_SD->SET_DATA( VL_GDATU ).
*
*      VG_TX_USD_BRL = ABS( OBJ_ZCL_UTIL_SD->TAXA_CAMBIO( ) ).
*
*      IF VG_TX_USD_BRL <> 0.
*        TG_TX_USD_BRL-DATA = TG_BSAK_AUX-AUGDT.
*        TG_TX_USD_BRL-TAXA = VG_TX_USD_BRL.
*        APPEND TG_TX_USD_BRL.
*      ENDIF.
*    ENDLOOP.

    "Busca de Dados Taxa Cambio (BKPF)
    SELECT BUKRS BELNR GJAHR KURSF KURS2 WAERS STBLG BLART
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE TG_BKPF
      FOR ALL ENTRIES IN TG_BSAK
     WHERE BUKRS EQ TG_BSAK-BUKRS
       AND BELNR EQ TG_BSAK-AUGBL
       AND GJAHR EQ TG_BSAK-AUGDT(4).

    SELECT BSIS~BUKRS BSIS~BELNR BSIS~GJAHR BSIS~HKONT BSIS~DMBTR BSIS~DMBE2
           BSIS~WAERS BSIS~WRBTR
           APPENDING CORRESPONDING FIELDS OF TABLE TG_BSIS_CBANCO
      FROM BSIS INNER JOIN SKB1 ON BSIS~BUKRS = SKB1~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                               AND BSIS~HKONT = SKB1~SAKNR
       FOR ALL ENTRIES IN TG_BSAK
     WHERE BSIS~BUKRS EQ TG_BSAK-BUKRS
       AND BSIS~BELNR EQ TG_BSAK-AUGBL
       AND SKB1~FDLEV IN R_FDLEV_BANCO.

    SELECT BUKRS BELNR GJAHR KURSF KURS2 WAERS STBLG BLART
      FROM BKPF APPENDING CORRESPONDING FIELDS OF TABLE TG_BKPF
      FOR ALL ENTRIES IN TG_BSAK
     WHERE BUKRS EQ TG_BSAK-BUKRS
       AND BELNR EQ TG_BSAK-BELNR
       AND GJAHR EQ TG_BSAK-GJAHR.

    "Busca denominação Contas
    SELECT SAKNR TXT50
      FROM SKAT APPENDING TABLE TG_SKAT
      FOR ALL ENTRIES IN TG_BSAK
     WHERE SAKNR EQ TG_BSAK-HKONT
       AND SPRAS EQ SY-LANGU.

    SELECT LIFNR NAME1
      FROM LFA1 INTO TABLE TG_LFA1
       FOR ALL ENTRIES IN TG_BSAK
     WHERE LIFNR EQ TG_BSAK-LIFNR.

    "Determinação de contas p/diferença de câmbio nas PA
    SELECT HKONT LHREA
      FROM T030H INTO TABLE TG_T030H
      FOR ALL ENTRIES IN TG_BSAK
     WHERE HKONT EQ TG_BSAK-HKONT.

    "Determinação de contas p/diferença de câmbio nas PA
    SELECT EBELN EBELP MATKL
      FROM EKPO INTO TABLE TG_EKPO
       FOR ALL ENTRIES IN TG_BSAK
     WHERE EBELN EQ TG_BSAK-EBELN.

    IF TG_EKPO[] IS NOT INITIAL.

      SELECT MATKL HKONT
        FROM ZGLT076 INTO TABLE TG_076
        FOR ALL ENTRIES IN TG_EKPO
      WHERE MATKL EQ TG_EKPO-MATKL.

    ENDIF.

    SELECT *
      FROM ZGLT077 INTO TABLE TG_077
       FOR ALL ENTRIES IN TG_BSAK
     WHERE BUKRS EQ TG_BSAK-BUKRS
       AND BELNR EQ TG_BSAK-BELNR
       AND GJAHR EQ TG_BSAK-GJAHR
       AND BUZEI EQ TG_BSAK-BUZEI.


    "Partidas Residuais Liquidadas
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE TG_BSAK_RSD
      FROM BSAK AS A
      FOR ALL ENTRIES IN TG_BSAK
     WHERE A~BUKRS EQ TG_BSAK-BUKRS
       AND A~LIFNR EQ TG_BSAK-LIFNR
       AND A~BELNR EQ TG_BSAK-AUGBL
       AND A~BELNR NE A~AUGBL
       AND A~WAERS EQ TG_BSAK-WAERS
       AND A~SHKZG EQ TG_BSAK-SHKZG
       AND ( ( A~REBZG EQ TG_BSAK-REBZG ) OR
             ( A~REBZG EQ TG_BSAK-BELNR ) ).

    "Partidas Residuais Aberto
    SELECT *
      APPENDING CORRESPONDING FIELDS OF TABLE TG_BSAK_RSD
      FROM BSIK AS A
      FOR ALL ENTRIES IN TG_BSAK
     WHERE A~BUKRS EQ TG_BSAK-BUKRS
       AND A~LIFNR EQ TG_BSAK-LIFNR
       AND A~BELNR EQ TG_BSAK-AUGBL
       AND A~WAERS EQ TG_BSAK-WAERS
       AND A~SHKZG EQ TG_BSAK-SHKZG
       AND ( ( A~REBZG EQ TG_BSAK-REBZG ) OR
             ( A~REBZG EQ TG_BSAK-BELNR ) ).

  ENDIF.

  LOOP AT TG_BSAD_RSD.
    CLEAR: TG_BSAD.
    MOVE-CORRESPONDING TG_BSAD_RSD TO TG_BSAD.
    TG_BSAD-RESIDUAL = 'X'.

    IF TG_BSAD-REBZG = 'V'.
      CLEAR: TG_BSAD-REBZG.
    ENDIF.

    APPEND TG_BSAD.
  ENDLOOP.

  LOOP AT TG_BSAK_RSD.
    CLEAR: TG_BSAK.
    MOVE-CORRESPONDING TG_BSAK_RSD TO TG_BSAK.
    TG_BSAK-RESIDUAL = 'X'.

    IF TG_BSAK-REBZG = 'V'.
      CLEAR: TG_BSAK-REBZG.
    ENDIF.

    APPEND TG_BSAK.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA_DADOS .

  DATA: VL_NOT_VALID TYPE C,
        WL_COLOR     TYPE KKBLO_SPECIALCOL,
        VL_COL       LIKE WL_COLOR-COLOR-COL,
        VL_TX_TMP    LIKE WA_SAIDA-KURSF.

  SORT TG_BSIS_CBANCO BY BUKRS BELNR.

  TG_BSAK_COMP[] = TG_BSAK[].
  TG_BSAD_COMP[] = TG_BSAD[].

  IF ( R_AR1 IS NOT INITIAL ) OR "Analise Calculo Variação
     ( R_AR2 IS NOT INITIAL ).

    IF R_TP1 IS NOT INITIAL. "Fornecedor

      LOOP AT TG_BSAK.

        CLEAR: WA_SAIDA.

        READ TABLE TG_BKPF WITH KEY BUKRS = TG_BSAK-BUKRS
                                    BELNR = TG_BSAK-AUGBL
                                    GJAHR = TG_BSAK-AUGDT(4).

        IF ( SY-SUBRC = 0 ) AND ( TG_BKPF-STBLG IS NOT INITIAL ).
          CONTINUE.
        ENDIF.

        READ TABLE TG_BKPF WITH KEY BUKRS = TG_BSAK-BUKRS
                                    BELNR = TG_BSAK-BELNR
                                    GJAHR = TG_BSAK-GJAHR.

        IF ( SY-SUBRC = 0 ) AND ( TG_BKPF-STBLG IS NOT INITIAL ).
          CONTINUE.
        ENDIF.

        WA_SAIDA-KOART    = 'K'.
        WA_SAIDA-RESIDUAL = TG_BSAK-RESIDUAL.
        WA_SAIDA-REBZG    = TG_BSAK-REBZG.
        WA_SAIDA-BUKRS    = TG_BSAK-BUKRS.
        WA_SAIDA-GJAHR    = TG_BSAK-GJAHR.
        WA_SAIDA-LIFNR    = TG_BSAK-LIFNR.

        PERFORM ATRIB_NAME1 CHANGING WA_SAIDA.

        WA_SAIDA-HKTRC   = TG_BSAK-HKONT.

        PERFORM ATRIB_TXT50 USING WA_SAIDA-HKTRC
                         CHANGING WA_SAIDA-TXT50.

        WA_SAIDA-BELNR = TG_BSAK-BELNR.
        WA_SAIDA-BUZEI = TG_BSAK-BUZEI.
        WA_SAIDA-AUGBL = TG_BSAK-AUGBL.
        WA_SAIDA-BLART = TG_BSAK-BLART.
        WA_SAIDA-WAERS = TG_BSAK-WAERS.
        WA_SAIDA-AUGDT = TG_BSAK-AUGDT.

        IF WA_SAIDA-RESIDUAL IS NOT INITIAL.
          WA_SAIDA-AUGBL = TG_BSAK-BELNR.
          WA_SAIDA-AUGDT = TG_BSAK-BUDAT.
        ENDIF.

        WA_SAIDA-BUDAT = TG_BSAK-BUDAT.
        WA_SAIDA-BLDAT = TG_BSAK-BLDAT.
        WA_SAIDA-WRBTR = TG_BSAK-WRBTR.
        WA_SAIDA-DMBTR = TG_BSAK-DMBTR.
        WA_SAIDA-DMBE2 = TG_BSAK-DMBE2.
        WA_SAIDA-GSBER = TG_BSAK-GSBER.
        WA_SAIDA-SHKZG_PT = TG_BSAK-SHKZG.

        IF TG_BSAK-SHKZG = 'H'.
          WA_SAIDA-WRBTR = ABS( WA_SAIDA-WRBTR ) * -1.
          WA_SAIDA-DMBTR = ABS( WA_SAIDA-DMBTR ) * -1.
          WA_SAIDA-DMBE2 = ABS( WA_SAIDA-DMBE2 ) * -1.
        ENDIF.

        WA_SAIDA-SGTXT = TG_BSAK-SGTXT.

        PERFORM ATRIB_TX_HIS CHANGING WA_SAIDA.

        PERFORM ATRIB_TX_CAMBIO CHANGING WA_SAIDA.

        PERFORM ATRIB_VLR_CALC CHANGING WA_SAIDA.

        PERFORM ATRIB_DADOS_VARIACAO CHANGING WA_SAIDA
                                              VL_NOT_VALID.

        PERFORM ATRIB_DOC_RECLAS CHANGING WA_SAIDA.

        IF VL_NOT_VALID IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        WA_SAIDA-EBELN = TG_BSAK-EBELN.
        WA_SAIDA-EBELP = TG_BSAK-EBELP.

        PERFORM VERIFICA_CLASSIFICACAO CHANGING WA_SAIDA.

        WA_SAIDA-TXHIS_VS   =  WA_SAIDA-TXHIS.
        WA_SAIDA-KURSF_VS   =  WA_SAIDA-KURSF.

        APPEND WA_SAIDA TO IT_SAIDA.

      ENDLOOP.

    ELSEIF R_TP2 IS NOT INITIAL. "Cliente

      LOOP AT TG_BSAD.

        CLEAR: WA_SAIDA.

        READ TABLE TG_BKPF WITH KEY BUKRS = TG_BSAD-BUKRS
                                    BELNR = TG_BSAD-AUGBL
                                    GJAHR = TG_BSAD-AUGDT(4).

        IF ( SY-SUBRC = 0 ) AND ( TG_BKPF-STBLG IS NOT INITIAL ).
          CONTINUE.
        ENDIF.

        READ TABLE TG_BKPF WITH KEY BUKRS = TG_BSAD-BUKRS
                                    BELNR = TG_BSAD-BELNR
                                    GJAHR = TG_BSAD-GJAHR.

        IF ( SY-SUBRC = 0 ) AND ( TG_BKPF-STBLG IS NOT INITIAL ).
          CONTINUE.
        ENDIF.

        WA_SAIDA-KOART    = 'D'.
        WA_SAIDA-RESIDUAL = TG_BSAD-RESIDUAL.
        WA_SAIDA-REBZG    = TG_BSAD-REBZG.
        WA_SAIDA-BUKRS    = TG_BSAD-BUKRS.
        WA_SAIDA-GJAHR    = TG_BSAD-GJAHR.
        WA_SAIDA-KUNNR    = TG_BSAD-KUNNR.

        PERFORM ATRIB_NAME1 CHANGING WA_SAIDA.

        WA_SAIDA-HKTRC   = TG_BSAD-HKONT.

        PERFORM ATRIB_TXT50 USING WA_SAIDA-HKTRC
                         CHANGING WA_SAIDA-TXT50.

        WA_SAIDA-BELNR = TG_BSAD-BELNR.
        WA_SAIDA-BUZEI = TG_BSAD-BUZEI.
        WA_SAIDA-AUGBL = TG_BSAD-AUGBL.
        WA_SAIDA-BLART = TG_BSAD-BLART.
        WA_SAIDA-WAERS = TG_BSAD-WAERS.
        WA_SAIDA-BUDAT = TG_BSAD-BUDAT.
        WA_SAIDA-BLDAT = TG_BSAD-BLDAT.
        WA_SAIDA-AUGDT = TG_BSAD-AUGDT.

        IF WA_SAIDA-RESIDUAL IS NOT INITIAL.
          WA_SAIDA-AUGBL = TG_BSAD-BELNR.
          WA_SAIDA-AUGDT = TG_BSAD-BUDAT.
        ENDIF.

        WA_SAIDA-WRBTR = TG_BSAD-WRBTR.
        WA_SAIDA-DMBTR = TG_BSAD-DMBTR.
        WA_SAIDA-DMBE2 = TG_BSAD-DMBE2.
        WA_SAIDA-GSBER = TG_BSAD-GSBER.
        WA_SAIDA-SHKZG_PT = TG_BSAD-SHKZG.

        IF TG_BSAD-SHKZG = 'H'.
          WA_SAIDA-WRBTR = ABS( WA_SAIDA-WRBTR ) * -1.
          WA_SAIDA-DMBTR = ABS( WA_SAIDA-DMBTR ) * -1.
          WA_SAIDA-DMBE2 = ABS( WA_SAIDA-DMBE2 ) * -1.
        ENDIF.

        WA_SAIDA-SGTXT = TG_BSAD-SGTXT.

        PERFORM ATRIB_TX_HIS CHANGING WA_SAIDA.

        PERFORM ATRIB_TX_CAMBIO CHANGING WA_SAIDA.

        PERFORM ATRIB_VLR_CALC CHANGING WA_SAIDA.

        PERFORM ATRIB_DADOS_VARIACAO CHANGING WA_SAIDA
                                              VL_NOT_VALID.

        IF VL_NOT_VALID IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        WA_SAIDA-VBELN = TG_BSAD-VBELN.
        WA_SAIDA-VBEL2 = TG_BSAD-VBEL2.

        WA_SAIDA-TXHIS_VS   =  WA_SAIDA-TXHIS.
        WA_SAIDA-KURSF_VS   =  WA_SAIDA-KURSF.

        APPEND WA_SAIDA TO IT_SAIDA.

      ENDLOOP.

    ENDIF.

  ENDIF.

  PERFORM CALC_DIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIME_DADOS .

*  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
*
*
*  PERFORM DEFINIR_EVENTOS.
*  PERFORM MONTAR_LAYOUT.
*
*  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  WL_LAYOUT-COLTAB_FIELDNAME = 'COLOR'.
*  WL_LAYOUT-EDIT_MODE = 'A'.
*  "WL_LAYOUT-EDIT = 'X'.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM       = V_REPORT
*      IS_VARIANT               = GS_VARIANT
*      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
*      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*      IT_FIELDCAT              = ESTRUTURA[]
*      IS_LAYOUT                = WL_LAYOUT
*      I_SAVE                   = 'X'
*      IT_EVENTS                = EVENTS
*      IS_PRINT                 = T_PRINT
*    TABLES
*      T_OUTTAB                 = IT_SAIDA.


  CALL SCREEN 0100.



ENDFORM.


FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM INICIAR_VARIAVEIS .

  DATA: WL_BUDAT_LOW(50)  TYPE C,
        WL_BUDAT_HIGH(50) TYPE C,
        WL_LAYOUT1(20),
        WL_LAYOUT2(30),
        WL_LAYOUT3(20),
        WL_LAYOUT4(20),
        WL_LAYOUT5(20),
        WL_LAYOUT6(30),
        WL_DATA,
        WL_SPACE,
        WL_ATE(5),
        WL_BUTXT          TYPE BUTXT.

  WL_LAYOUT1 = TEXT-070.
  WL_LAYOUT2 = TEXT-071.
  WL_LAYOUT3 = TEXT-072.
  WL_LAYOUT4 = TEXT-073.
  WL_LAYOUT5 = TEXT-074.
  WL_LAYOUT6 = TEXT-075.
  WL_DATA    = TEXT-076.
  WL_SPACE   = TEXT-077.
  WL_ATE(3)  = TEXT-078.

  REFRESH: T_TOP.

  SELECT SINGLE BUTXT
    FROM T001 INTO WL_BUTXT
   WHERE BUKRS IN S_BUKRS.

  CONCATENATE WL_LAYOUT1 S_BUKRS+3(4) WL_SPACE WL_BUTXT  INTO WL_BUKRS SEPARATED BY SPACE.

  IF S_BUDAT-HIGH IS NOT INITIAL.

    CONCATENATE S_BUDAT-LOW+6(2) WL_DATA
                S_BUDAT-LOW+4(2) WL_DATA
                S_BUDAT-LOW(4) INTO WL_BUDAT_LOW.

    CONCATENATE S_BUDAT-HIGH+6(2) WL_DATA
                S_BUDAT-HIGH+4(2) WL_DATA
                S_BUDAT-HIGH(4) INTO WL_BUDAT_HIGH.

    CONCATENATE WL_LAYOUT2 WL_BUDAT_LOW WL_ATE WL_BUDAT_HIGH
           INTO WL_BUDAT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE S_BUDAT-LOW+6(2) WL_DATA
                S_BUDAT-LOW+4(2) WL_DATA
                S_BUDAT-LOW(4) INTO WL_BUDAT_LOW.

    CONCATENATE WL_LAYOUT2 WL_BUDAT_LOW INTO WL_BUDAT.
  ENDIF.

  IF ( S_HKTVR-HIGH IS NOT INITIAL ) AND
     ( S_HKTVR-LOW  IS NOT INITIAL ).
    CONCATENATE WL_LAYOUT3 S_HKTVR-LOW  WL_ATE S_HKTVR-HIGH INTO WL_HKTVR SEPARATED BY SPACE.
  ELSEIF S_HKTVR-LOW IS NOT INITIAL.

    LOOP AT S_HKTVR.
      IF S_HKTVR-OPTION EQ 'EQ'.
        IF SY-TABIX EQ 1.
          MOVE S_HKTVR-LOW TO WL_HKTVR.
        ELSE.
          CONCATENATE WL_HKTVR ',' S_HKTVR-LOW INTO WL_HKTVR SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE WL_LAYOUT3 WL_HKTVR INTO WL_HKTVR SEPARATED BY SPACE.

*   CONCATENATE WL_LAYOUT3 S_HKTVR-LOW  INTO WL_HKTVR SEPARATED BY SPACE.

  ELSE.
    WL_HKTVR = WL_LAYOUT3.
  ENDIF.

  IF ( S_HKTRC-HIGH IS NOT INITIAL ) AND
     ( S_HKTRC-LOW  IS NOT INITIAL ).
    CONCATENATE WL_LAYOUT6 S_HKTRC-LOW  WL_ATE S_HKTRC-HIGH INTO WL_HKTRC SEPARATED BY SPACE.
  ELSEIF S_HKTRC-LOW IS NOT INITIAL.

    LOOP AT S_HKTRC.
      IF S_HKTRC-OPTION EQ 'EQ'.
        IF SY-TABIX EQ 1.
          MOVE S_HKTRC-LOW TO WL_HKTRC.
        ELSE.
          CONCATENATE WL_HKTRC ',' S_HKTRC-LOW INTO WL_HKTRC SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE WL_LAYOUT6 WL_HKTRC INTO WL_HKTRC SEPARATED BY SPACE.
*   CONCATENATE WL_LAYOUT6 S_HKTRC-LOW  INTO WL_HKTRC SEPARATED BY SPACE.
  ELSE.
    WL_HKTRC = WL_LAYOUT6.
  ENDIF.

  IF ( S_WAERS-HIGH IS NOT INITIAL ) AND
     ( S_WAERS-LOW  IS NOT INITIAL ).
    CONCATENATE WL_LAYOUT4 S_WAERS-LOW  WL_ATE S_WAERS-HIGH INTO WL_WAERS SEPARATED BY SPACE.
  ELSEIF S_WAERS-LOW IS NOT INITIAL.
    CONCATENATE WL_LAYOUT4 S_WAERS-LOW  INTO WL_WAERS SEPARATED BY SPACE.
  ELSE.
    WL_WAERS = WL_LAYOUT4.
  ENDIF.

  IF ( S_PARID-HIGH IS NOT INITIAL ) AND
     ( S_PARID-LOW  IS NOT INITIAL ).
    CONCATENATE WL_LAYOUT5 S_PARID-LOW  WL_ATE S_PARID-HIGH INTO WL_PARID SEPARATED BY SPACE.
  ELSEIF S_PARID-LOW IS NOT INITIAL.

    LOOP AT S_PARID.
      IF S_PARID-OPTION EQ 'EQ'.
        IF SY-TABIX EQ 1.
          MOVE S_PARID-LOW TO WL_PARID.
        ELSE.
          CONCATENATE WL_PARID ',' S_PARID-LOW INTO WL_PARID SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE WL_LAYOUT5 WL_PARID INTO WL_PARID SEPARATED BY SPACE.

*   CONCATENATE WL_LAYOUT5 S_PARID-LOW  INTO WL_PARID SEPARATED BY SPACE.
  ELSE.
    WL_PARID = WL_LAYOUT5.
  ENDIF.

  IF R_TP1 IS NOT INITIAL. "Fornecedor
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-008.
  ELSEIF R_TP2 IS NOT INITIAL. "Cliente
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-007.
  ENDIF.

  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_BUKRS.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_BUDAT.

  IF WL_HKTVR IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_HKTVR.
  ENDIF.

  IF WL_WAERS IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_WAERS.
  ENDIF.

  IF WL_PARID IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_PARID.
  ENDIF.

  IF WL_HKTRC IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_HKTRC.
  ENDIF.

  V_REPORT = SY-REPID.
  GS_VARIANT-REPORT      = SY-REPID.

  PERFORM CONSTRUIR_NOMES_COL.

ENDFORM.                    " INICIAR_VARIAVEIS

FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE

FORM USER_COMMAND  USING R_UCOMM      LIKE SY-UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'RECLASS'.
      LEAVE TO SCREEN 0.
    WHEN: '&IC1'.

      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'BELNR'.
          CLEAR: WA_SAIDA.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
          SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
          SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
          SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'AUGBL'.
          CLEAR: WA_SAIDA.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
          SET PARAMETER ID 'BLN' FIELD WA_SAIDA-AUGBL.
          SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
          SET PARAMETER ID 'GJR' FIELD WA_SAIDA-AUGDT(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        WHEN 'VBELN'.
          CLEAR: WA_SAIDA.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
          SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELN.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        WHEN 'VBEL2'.
          CLEAR: WA_SAIDA.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
          SET PARAMETER ID 'VF' FIELD WA_SAIDA-VBEL2.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        WHEN 'EBELN'.
          CLEAR: WA_SAIDA.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
          SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDCASE.


  ENDCASE.

ENDFORM.                    "user_command

FORM DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " DEFINIR_EVENTOS

FORM F_CARREGAR_EVENTOS USING NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM MONTAR_LAYOUT .

*  REFRESH:  ESTRUTURA[].
*
*  IF ( R_AR1  IS NOT INITIAL ) OR  "Analise Calculo Variação
*     ( R_AR2  IS NOT INITIAL ).    "Reclassificação de Conta de Variação
*
*    IF R_TP1 IS NOT INITIAL. "Fornecedor
*
*      PERFORM MONTAR_ESTRUTURA USING:
*
*         1   'LFA1'  'LIFNR'   'IT_SAIDA' 'LIFNR'  'Fornecedor'          '10' ''  '',
*         1   ''           ''   'IT_SAIDA' 'NAME1'  'Nome'                '20' ''  '',
*         2   'BSAK'  'HKONT'   'IT_SAIDA' 'HKTRC'  'Cta.Reconc.'         '20' ''  '',
*         3   ''           ''   'IT_SAIDA' 'TXT50'  'Descrição'           '06' ''  '',
*         4   'BSAK'  'BELNR'   'IT_SAIDA' 'BELNR'  'Nro.Documento'       '05' 'X' '',
*         5   'BSAK'  'AUGBL'   'IT_SAIDA' 'AUGBL'  'Doc.Compensação'     '08' 'X' '',
*         6   ''           ''   'IT_SAIDA' 'BLART'  'Tp.Doc.'             '25' ''  '',
*         7   ''           ''   'IT_SAIDA' 'WAERS'  'Moeda'               '06' ''  '',
*         8   ''           ''   'IT_SAIDA' 'BUDAT'  'Dt.Lcto'             '12' ''  '',
*         9   ''           ''   'IT_SAIDA' 'BLDAT'  'Dt.Dcto'             '09' ''  '',
*         10  ''           ''   'IT_SAIDA' 'DMBTR'  'Valor R$'            '07' ''  '',
*         11  ''           ''   'IT_SAIDA' 'DMBE2'  'Valor US$'           '03' ''  '',
*         12  ''           ''   'IT_SAIDA' 'SGTXT'  'Texto Contábil'      '13' ''  '',
*         13  ''           ''   'IT_SAIDA' 'TXHIS'  'Tx.Hist.'            '13' ''  '',
*         14  ''           ''   'IT_SAIDA' 'KURSF'  'Tx.Câmbio'           '13' ''  '',
*         15  ''           ''   'IT_SAIDA' 'VCALC'  'Var.Calculada'       '13' ''  '',
*         16  'BSAK'  'HKONT'   'IT_SAIDA' 'HKTVR'  'Cta.Variação'        '13' ''  '',
*         17  ''           ''   'IT_SAIDA' 'VLANC'  'Var.Lançada'         '13' ''  '',
*         18  ''           ''   'IT_SAIDA' 'VLDIF'  'Vlr.Diferença'       '13' ''  '',
*         21  'BSAK'  'EBELN'   'IT_SAIDA' 'EBELN'  'Ped.Compra'          '13' 'X' '',
*         23  'BSAK'  'HKONT'   'IT_SAIDA' 'HKRCL'  'Cta.Reclas.'         '20' ''  ''.
*
*    ELSEIF R_TP2 IS NOT INITIAL. "Cliente
*
*       PERFORM MONTAR_ESTRUTURA USING:
*
*         1   'KNA1'  'KUNNR'   'IT_SAIDA' 'KUNNR'  'Cliente'             '10' ''   '',
*         1   ''           ''   'IT_SAIDA' 'NAME1'  'Nome'                '20' ''   '',
*         2   'BSAD'  'HKONT'   'IT_SAIDA' 'HKTRC'  'Cta.Reconc.'         '20' ''   '',
*         3   ''           ''   'IT_SAIDA' 'TXT50'  'Descrição'           '06' ''   '',
*         4   'BSAD'  'BELNR'   'IT_SAIDA' 'BELNR'  'Nro.Documento'       '05' 'X'  '',
*         5   'BSAD'  'AUGBL'   'IT_SAIDA' 'AUGBL'  'Doc.Compensação'     '08' 'X'  '',
*         6   ''           ''   'IT_SAIDA' 'BLART'  'Tp.Doc.'             '25' ''   '',
*         7   ''           ''   'IT_SAIDA' 'WAERS'  'Moeda'               '06' ''   '',
*         8   ''           ''   'IT_SAIDA' 'BUDAT'  'Dt.Lcto'             '12' ''   '',
*         9   ''           ''   'IT_SAIDA' 'BLDAT'  'Dt.Dcto'             '09' ''   '',
*         10  ''           ''   'IT_SAIDA' 'DMBTR'  'Valor R$'            '07' ''   '',
*         11  ''           ''   'IT_SAIDA' 'DMBE2'  'Valor US$'           '03' ''   '',
*         12  ''           ''   'IT_SAIDA' 'SGTXT'  'Texto Contábil'      '13' ''   '',
*         13  ''           ''   'IT_SAIDA' 'TXHIS'  'Tx.Hist.'            '13' ''   '',
*         14  ''           ''   'IT_SAIDA' 'KURSF'  'Tx.Câmbio'           '13' ''   '',
*         15  ''           ''   'IT_SAIDA' 'VCALC'  'Var.Calculada'       '13' ''   '',
*         16  'BSAD'  'HKONT'   'IT_SAIDA' 'HKTVR'  'Cta.Variação'        '13' ''   '',
*         17  ''           ''   'IT_SAIDA' 'VLANC'  'Var.Lançada'         '13' ''   '',
*         18  ''           ''   'IT_SAIDA' 'VLDIF'  'Vlr.Diferença'       '13' ''   '',
*         19  'BSAD'  'VBELN'   'IT_SAIDA' 'VBELN'  'Ordem Venda'         '13' 'X'  '',
*         20  'BSAD'  'VBEL2'   'IT_SAIDA' 'VBEL2'  'Fatura'              '13' 'X'  ''.
*
*    ENDIF.
*
*  ENDIF.


ENDFORM.                    " MONTAR_LAYOUT

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_HOTSPOT)
                            VALUE(P_NO_INPUT).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-INPUT         = ' '.
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
  WA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.
  WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  WA_ESTRUTURA-DDICTXT       = 'L'.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM ATRIB_TX_CAMBIO CHANGING P_SAIDA TYPE TY_SAIDA.

  DATA: V_KURSF_BANCO TYPE BKPF-KURSF.

  DATA: V_BELNR_CBANCO TYPE BKPF-BELNR,
        V_ACHOU        TYPE C.

  READ TABLE TG_BKPF WITH KEY BUKRS = P_SAIDA-BUKRS
                              BELNR = P_SAIDA-AUGBL
                              GJAHR = P_SAIDA-AUGDT(4).
  IF SY-SUBRC = 0.
    CASE TG_BKPF-WAERS.
      WHEN 'BRL'.
        WA_SAIDA-KURSF = ABS( TG_BKPF-KURS2 ).
      WHEN OTHERS.
        WA_SAIDA-KURSF = ABS( TG_BKPF-KURSF ).
    ENDCASE.
  ENDIF.

*  PERFORM F_GET_BSIS_CBANCO USING P_SAIDA-BUKRS
*                                  P_SAIDA-AUGBL
*                         CHANGING V_KURSF_BANCO.
*
*  IF V_KURSF_BANCO > 0.
*    WA_SAIDA-KURSF = ABS( V_KURSF_BANCO ).
*  ENDIF.

  "Check se possui documentos contra banco na compensação. Se tiver, utilizar taxa do documento
*  CLEAR: V_ACHOU, V_BELNR_CBANCO.
*  CASE 'X'.
*    WHEN R_TP1. "Fornecedor
*      LOOP AT TG_BSAK WHERE BUKRS EQ P_SAIDA-BUKRS
*                        AND AUGBL EQ P_SAIDA-AUGBL
*                        AND BELNR NE P_SAIDA-AUGBL
*                        AND BLART IN R_BLART_CBANCO.
*
*        V_ACHOU         = 'X'.
*        V_BELNR_CBANCO  = TG_BSAK-BELNR.
*      ENDLOOP.
*    WHEN R_TP2. "Cliente
*      LOOP AT TG_BSAD WHERE BUKRS EQ P_SAIDA-BUKRS
*                        AND AUGBL EQ P_SAIDA-AUGBL
*                        AND BELNR NE P_SAIDA-AUGBL
*                        AND BLART IN R_BLART_CBANCO.
*
*        V_ACHOU         = 'X'.
*        V_BELNR_CBANCO  = TG_BSAK-BELNR.
*      ENDLOOP.
*  ENDCASE.
*
*  IF V_ACHOU IS NOT INITIAL.
*    LOOP AT TG_BKPF WHERE BUKRS = P_SAIDA-BUKRS
*                      AND BELNR = V_BELNR_CBANCO.
*      CASE TG_BKPF-WAERS.
*        WHEN 'BRL'.
*          WA_SAIDA-KURSF = ABS( TG_BKPF-KURS2 ).
*        WHEN OTHERS.
*          WA_SAIDA-KURSF = ABS( TG_BKPF-KURSF ).
*      ENDCASE.
*    ENDLOOP.
*  ENDIF.

ENDFORM.

FORM ATRIB_NAME1  CHANGING P_SAIDA TYPE TY_SAIDA.

  IF P_SAIDA-KUNNR IS NOT INITIAL.
    READ TABLE TG_KNA1 WITH KEY KUNNR = P_SAIDA-KUNNR.

    IF SY-SUBRC = 0.
      P_SAIDA-NAME1 = TG_KNA1-NAME1.
    ENDIF.
  ELSEIF P_SAIDA-LIFNR IS NOT INITIAL.
    READ TABLE TG_LFA1 WITH KEY LIFNR = P_SAIDA-LIFNR.

    IF SY-SUBRC = 0.
      P_SAIDA-NAME1 = TG_LFA1-NAME1.
    ENDIF.
  ENDIF.

ENDFORM.

FORM ATRIB_TXT50  USING P_SAKNR CHANGING P_TXT50.

  READ TABLE TG_SKAT WITH KEY SAKNR = P_SAKNR.

  IF SY-SUBRC = 0.
    P_TXT50 = TG_SKAT-TXT50.
  ENDIF.

ENDFORM.

FORM ATRIB_DADOS_VARIACAO  CHANGING P_SAIDA      TYPE TY_SAIDA
                                    P_NOT_VALID  TYPE C.

  DATA: V_DMBE2 TYPE BSAD-DMBE2,
        V_DMBTR TYPE BSAD-DMBTR.

  CLEAR: P_NOT_VALID.

  READ TABLE TG_T030H WITH KEY HKONT = P_SAIDA-HKTRC.

  IF SY-SUBRC NE 0.
    P_NOT_VALID = 'X'.
    EXIT.
  ENDIF.

  P_SAIDA-HKTVR = TG_T030H-LHREA.

  READ TABLE TG_BSIS WITH KEY BUKRS = P_SAIDA-BUKRS
                              BELNR = P_SAIDA-AUGBL
                              GSBER = P_SAIDA-GSBER
                              HKONT = TG_T030H-LHREA.
  IF SY-SUBRC NE 0.
    "P_NOT_VALID = 'X'.
    EXIT.
  ENDIF.

  P_SAIDA-SHKZG = TG_BSIS-SHKZG.

  IF ( P_SAIDA-BELNR = P_SAIDA-AUGBL ) AND ( P_SAIDA-RESIDUAL IS INITIAL ).

    LOOP AT TG_BSIS WHERE BUKRS = P_SAIDA-BUKRS
                      AND BELNR = P_SAIDA-AUGBL
                      AND GSBER = P_SAIDA-GSBER
                      AND HKONT = TG_T030H-LHREA.

      CLEAR: V_DMBE2, V_DMBTR.

      IF TG_BSIS-SHKZG = 'H'.
        V_DMBE2 = ABS( TG_BSIS-DMBE2 ) * -1.
        V_DMBTR = ABS( TG_BSIS-DMBTR ) * -1.
      ELSE.
        V_DMBE2 = ABS( TG_BSIS-DMBE2 ).
        V_DMBTR = ABS( TG_BSIS-DMBTR ).
      ENDIF.

      IF P_SAIDA-WAERS EQ 'BRL'.
        ADD V_DMBE2 TO P_SAIDA-VLANC_2.
      ENDIF.

      ADD V_DMBTR TO P_SAIDA-VLANC.

      CLEAR: TG_BSIS-DMBTR, TG_BSIS-DMBE2.
      MODIFY TG_BSIS.
    ENDLOOP.

  ENDIF.


ENDFORM.

FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB .
  SET PF-STATUS 'ZSTANDARD_FULLSCREEN'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIAR_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIAR_FIELD_CATALOG .

  FREE: WA_FCAT, IT_FCAT.

  IF ( R_AR1 IS NOT INITIAL ) OR  "Analise Calculo Variação
     ( R_AR2 IS NOT INITIAL ).

    IF R_TP1 IS NOT INITIAL. "Fornecedor

      PERFORM ESTRUTURA_ALV USING:

         1  'LFA1'  'LIFNR'     'IT_SAIDA' 'LIFNR'    TEXT-030                    '10'  ''    '' ' ' ' ' ' ' ' ',
         1  ''           ''     'IT_SAIDA' 'NAME1'    TEXT-031                    '25'  ''    '' ' ' ' ' ' ' ' ',
         2  'BSAK'  'HKONT'     'IT_SAIDA' 'HKTRC'    TEXT-032                    '11'  ''    '' ' ' ' ' ' ' ' ',
         3  ''           ''     'IT_SAIDA' 'TXT50'    TEXT-033                    '20'  ''    '' ' ' ' ' ' ' ' ',
         4  'BSAK'  'BELNR'     'IT_SAIDA' 'BELNR'    TEXT-034                    '15'  ''    '' ' ' ' ' 'X' ' ',
         5  'BSAK'  'AUGBL'     'IT_SAIDA' 'AUGBL'    TEXT-035                    '15'  ''    '' ' ' ' ' 'X' ' ',
         6  ''           ''     'IT_SAIDA' 'BLART'    TEXT-036                    '10'  ''    '' ' ' ' ' ' ' ' ',
         7  ''           ''     'IT_SAIDA' 'WAERS'    TEXT-037                    '06'  ''    '' ' ' ' ' ' ' ' ',
         8  ''           ''     'IT_SAIDA' 'BUDAT'    TEXT-038                    '09'  ''    '' ' ' ' ' ' ' ' ',
         9  ''           ''     'IT_SAIDA' 'BLDAT'    TEXT-039                    '09'  ''    '' ' ' ' ' ' ' ' ',
         9  ''           ''     'IT_SAIDA' 'WRBTR'    TEXT-040                    '12'  ''    '' ' ' ' ' ' ' ' ',
         10 ''           ''     'IT_SAIDA' 'DMBTR'    TEXT-041                    '12'  ''    '' ' ' ' ' ' ' ' ',
         11 ''           ''     'IT_SAIDA' 'DMBE2'    TEXT-042                    '12'  ''    '' ' ' ' ' ' ' ' ',
         12 ''           ''     'IT_SAIDA' 'SGTXT'    TEXT-043                    '18'  ''    '' ' ' ' ' ' ' ' ',
         13 ''           ''     'IT_SAIDA' 'TXHIS_VS' TEXT-044                    '10'  ''    '' ' ' ' ' ' ' ' ',
         14 ''           ''     'IT_SAIDA' 'KURSF_VS' TEXT-045                    '10'  ''    '' ' ' ' ' 'X' ' ',
         15 ''           ''     'IT_SAIDA' 'VCALC'    VG_NCOL_VARCALC             '13'  ''    '' ' ' ' ' ' ' ' ', "'Var.Calculada'
         16 ''           ''     'IT_SAIDA' 'VCALC_2'  TEXT-046                    '13'  ''    '' ' ' ' ' ' ' ' ',
         17 'BSAK'  'HKONT'     'IT_SAIDA' 'HKTVR'    TEXT-047                    '13'  ''    '' ' ' ' ' ' ' ' ',
         18 ''           ''     'IT_SAIDA' 'VLANC'    VG_NCOL_VARLANC             '13'  ''    '' ' ' ' ' ' ' ' ', "'Var.Lançada'
         19 ''           ''     'IT_SAIDA' 'VLDIF'    VG_NCOL_VLRDIFE             '13'  ''    '' ' ' ' ' ' ' ' ', "'Vlr.Diferença'
         20 ''           ''     'IT_SAIDA' 'VLANC_2'  TEXT-048                    '13'  ''    '' ' ' ' ' ' ' ' ',
         21 ''           ''     'IT_SAIDA' 'VLDIF_2'  TEXT-049                    '13'  ''    '' ' ' ' ' ' ' ' ',
         22 'BSAK'  'EBELN'     'IT_SAIDA' 'EBELN'    TEXT-050                    '13'  ''    '' ' ' ' ' 'X' ' ',
         23 'BSAK'  'HKONT'     'IT_SAIDA' 'HKRCL'    TEXT-051                    '13'  ''    '' ' ' ' ' ' ' ' ',
         24 ''           ''     'IT_SAIDA' 'STATUS'   TEXT-052                    '06'  ''    '' ' ' ' ' ' ' ' ',
         25 ''           ''     'IT_SAIDA' 'LOTE'     TEXT-053                    '13'  ''    '' ' ' ' ' 'X' ' ',
         26 ''           ''     'IT_SAIDA' 'DOC_LCTO' TEXT-054                    '13'  ''    '' ' ' ' ' 'X' ' ',
         27 ''           ''     'IT_SAIDA' 'DOC_RCL'  TEXT-055                    '13'  ''    '' ' ' ' ' 'X' ' ',
         28 ''           ''     'IT_SAIDA' 'RESIDUAL' TEXT-056                    '08'  ''    '' ' ' ' ' ' ' ' '.

    ELSEIF R_TP2 IS NOT INITIAL. "Cliente

      PERFORM ESTRUTURA_ALV USING:

         1  'KNA1'  'KUNNR'     'IT_SAIDA' 'KUNNR'    TEXT-057                            '10'  ''    '' ' ' ' ' ' ' ' ',
         1  ''           ''     'IT_SAIDA' 'NAME1'    TEXT-031                            '25'  ''    '' ' ' ' ' ' ' ' ',
         2  'BSAD'  'HKONT'     'IT_SAIDA' 'HKTRC'    TEXT-058                            '11'  ''    '' ' ' ' ' ' ' ' ',
         3  ''           ''     'IT_SAIDA' 'TXT50'    TEXT-059                            '20'  ''    '' ' ' ' ' ' ' ' ',
         4  'BSAD'  'BELNR'     'IT_SAIDA' 'BELNR'    TEXT-034                            '15'  ''    '' ' ' ' ' 'X' ' ',
         5  'BSAD'  'AUGBL'     'IT_SAIDA' 'AUGBL'    TEXT-035                            '15'  ''    '' ' ' ' ' 'X' ' ',
         6  ''           ''     'IT_SAIDA' 'BLART'    TEXT-036                            '10'  ''    '' ' ' ' ' ' ' ' ',
         7  ''           ''     'IT_SAIDA' 'WAERS'    TEXT-037                            '06'  ''    '' ' ' ' ' ' ' ' ',
         8  ''           ''     'IT_SAIDA' 'BUDAT'    TEXT-038                            '09'  ''    '' ' ' ' ' ' ' ' ',
         9  ''           ''     'IT_SAIDA' 'BLDAT'    TEXT-039                            '09'  ''    '' ' ' ' ' ' ' ' ',
         9  ''           ''     'IT_SAIDA' 'WRBTR'    TEXT-040                            '12'  ''    '' ' ' ' ' ' ' ' ',
         10 ''           ''     'IT_SAIDA' 'DMBTR'    TEXT-041                            '12'  ''    '' ' ' ' ' ' ' ' ',
         11 ''           ''     'IT_SAIDA' 'DMBE2'    TEXT-042                            '12'  ''    '' ' ' ' ' ' ' ' ',
         12 ''           ''     'IT_SAIDA' 'SGTXT'    TEXT-043                            '18'  ''    '' ' ' ' ' ' ' ' ',
         13 ''           ''     'IT_SAIDA' 'TXHIS_VS' TEXT-044                            '10'  ''    '' ' ' ' ' ' ' ' ',
         14 ''           ''     'IT_SAIDA' 'KURSF_VS' TEXT-045                            '10'  ''    '' ' ' ' ' 'X' ' ',
         15 ''           ''     'IT_SAIDA' 'VCALC'    VG_NCOL_VARCALC                     '13'  ''    '' ' ' ' ' ' ' ' ', "'Var.Calculada'
         16 ''           ''     'IT_SAIDA' 'VCALC_2'  TEXT-046                            '13'  ''    '' ' ' ' ' ' ' ' ',
         17 'BSAD'  'HKONT'     'IT_SAIDA' 'HKTVR'    TEXT-047                            '13'  ''    '' ' ' ' ' ' ' ' ',
         18 ''           ''     'IT_SAIDA' 'VLANC'    VG_NCOL_VARLANC                     '13'  ''    '' ' ' ' ' ' ' ' ', "'Var.Lançada'
         19 ''           ''     'IT_SAIDA' 'VLDIF'    VG_NCOL_VLRDIFE                     '13'  ''    '' ' ' ' ' ' ' ' ', "'Vlr.Diferença'
         20 ''           ''     'IT_SAIDA' 'VLANC_2'  TEXT-048                            '13'  ''    '' ' ' ' ' ' ' ' ',
         21 ''           ''     'IT_SAIDA' 'VLDIF_2'  TEXT-049                            '13'  ''    '' ' ' ' ' ' ' ' ',
         22 'BSAD'  'VBELN'     'IT_SAIDA' 'VBELN'    TEXT-060                            '13'  ''    '' ' ' ' ' 'X' ' ',
         23 'BSAD'  'VBEL2'     'IT_SAIDA' 'VBEL2'    TEXT-061                            '13'  ''    '' ' ' ' ' 'X' ' ',
         24 'BSAD'  'HKONT'     'IT_SAIDA' 'HKRCL'    TEXT-062                            '13'  ''    '' ' ' ' ' ' ' ' ',
         25 ''           ''     'IT_SAIDA' 'RESIDUAL' TEXT-056                            '08'  ''    '' ' ' ' ' ' ' ' '.



    ENDIF.

  ENDIF.


ENDFORM.

FORM ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                         VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                         VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                         VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                         VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                         VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                         VALUE(P_OUTPUTLEN)
                         VALUE(P_EDIT)
                         VALUE(P_SUM)
                         VALUE(P_EMPHASIZE)
                         VALUE(P_JUST)
                         VALUE(P_HOTSPOT)
                         VALUE(P_F4).

  CASE S_WAERS2-LOW. "Moeda Variação
    WHEN 'BRL'.
      CHECK P_FIELD NE 'VCALC_2' AND
            P_FIELD NE 'VLANC_2' AND
            P_FIELD NE 'VLDIF_2'.
    WHEN 'USD'.
      CHECK P_FIELD NE 'VCALC' AND
            P_FIELD NE 'VLANC' AND
            P_FIELD NE 'VLDIF'.
    WHEN OTHERS.
  ENDCASE.

  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = ' '.
  WA_FCAT-SELTEXT     = P_SCRTEXT_L.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-COLDDICTXT  = 'L'.
  WA_FCAT-SELDDICTXT  = 'L'.
  WA_FCAT-TIPDDICTXT  = 'L'.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_OBJETOS .

  CLEAR: GS_LAYOUT,
         GS_VARIANT,
         IT_SORT[].

  REFRESH: IT_EXCLUDE_FCODE.


ENDFORM.

FORM HANDLE_HOTSPOT_CLICK  USING    I_ROW_ID     TYPE LVC_S_ROW
                                    I_COLUMN_ID  TYPE LVC_S_COL
                                    IS_ROW_NO    TYPE LVC_S_ROID.

  DATA: OPT     TYPE CTU_PARAMS,
        VL_LOTE TYPE ZGLT034-LOTE,
        _GJAHR  TYPE GJAHR.

  CHECK ( I_ROW_ID-INDEX CO '1234567890' ) AND ( I_ROW_ID-ROWTYPE IS INITIAL ).

  CLEAR: WA_SAIDA, _GJAHR.
  CASE I_COLUMN_ID.
    WHEN 'BELNR'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-BELNR IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'AUGBL'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      _GJAHR = WA_SAIDA-AUGDT(4).

      IF ( WA_SAIDA-AUGBL IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-AUGBL.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD _GJAHR. "WA_SAIDA-AUGDT(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'DOC_RCL'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      _GJAHR = WA_SAIDA-DT_LCTO_CTB(4).

      IF ( WA_SAIDA-DOC_RCL IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-DOC_RCL.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD _GJAHR. " WA_SAIDA-DT_LCTO_CTB(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'VBELN'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-VBELN IS NOT INITIAL ).
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'VBEL2'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-VBEL2 IS NOT INITIAL ).
        SET PARAMETER ID 'VF' FIELD WA_SAIDA-VBEL2.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'EBELN'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-EBELN IS NOT INITIAL ).
        SET PARAMETER ID 'BES' FIELD WA_SAIDA-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.


    WHEN 'DOC_LCTO'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-DOC_LCTO IS NOT INITIAL ).
        CLEAR: VL_LOTE.
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-DOC_LCTO.
        SET PARAMETER ID 'LOT' FIELD VL_LOTE.
        CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'LOTE'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( WA_SAIDA-LOTE IS NOT INITIAL ).
        SET PARAMETER ID 'LOT' FIELD WA_SAIDA-LOTE.
        CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'KURSF_VS'.
      CLEAR: WA_SAIDA.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX I_ROW_ID.
      CHECK ( SY-SUBRC = 0 ).

      IF ( SY-SUBRC EQ 0 ) AND
         ( WA_SAIDA-BUKRS IS NOT INITIAL ) AND
         ( WA_SAIDA-AUGBL IS NOT INITIAL ).

        READ TABLE TG_INF_DOC WITH KEY BUKRS = WA_SAIDA-BUKRS
                                     AUGBL = WA_SAIDA-AUGBL.

        IF ( SY-SUBRC = 0 ) AND ( TG_INF_DOC-MSG IS NOT INITIAL ).
          MESSAGE S836(SD) WITH TG_INF_DOC-MSG DISPLAY LIKE 'W'.
        ENDIF.

      ENDIF.


  ENDCASE.

ENDFORM.                    "HANDLE_HOTSPOT_CLICK

FORM VERIFICA_CLASSIFICACAO  CHANGING P_SAIDA TYPE TY_SAIDA.

  DATA: WL_COLOR     TYPE KKBLO_SPECIALCOL.

  IF ( P_SAIDA-EBELN IS NOT INITIAL ) AND
     ( P_SAIDA-EBELP IS NOT INITIAL ).

    READ TABLE TG_EKPO WITH KEY EBELN = P_SAIDA-EBELN
                                EBELP = P_SAIDA-EBELP.

  ELSEIF ( P_SAIDA-EBELN IS NOT INITIAL ).

    READ TABLE TG_EKPO WITH KEY EBELN = P_SAIDA-EBELN.

  ENDIF.

  IF ( SY-SUBRC = 0 ) AND ( TG_EKPO-MATKL IS NOT INITIAL ).

    READ TABLE TG_076 WITH KEY MATKL = TG_EKPO-MATKL.

    IF ( SY-SUBRC = 0 ) AND
       ( TG_076-HKONT IS NOT INITIAL ) AND
       ( P_SAIDA-HKTVR IS NOT INITIAL ). "Conta Variação

      IF TG_076-HKONT <> P_SAIDA-HKTVR.

        IF R_AR2 IS NOT INITIAL. "Reclassificação de Conta de Variação
          P_SAIDA-RECLS = 'X'.
        ENDIF.

        P_SAIDA-HKRCL = TG_076-HKONT.
        WL_COLOR-FIELDNAME = 'HKRCL'.
        WL_COLOR-COLOR-COL = 6.
        WL_COLOR-COLOR-INV = 6.
        APPEND WL_COLOR TO P_SAIDA-COLOR.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM GERAR_DOC_RECLAS .

  TYPES: BEGIN OF TY_LOTE_GER,
           BUKRS TYPE ZGLT034-BUKRS,
           LOTE  TYPE ZGLT034-LOTE,
         END OF TY_LOTE_GER.

  DATA: VAR_ANSWER TYPE C.

  FIELD-SYMBOLS: <SAIDA> TYPE TY_SAIDA.

  DATA: E_NUM_LOTE  TYPE ZLOTE_NUM,
        WL_ZGLT031  TYPE ZGLT031,
        GT_ZGLT032  TYPE TABLE OF ZGLT032,
        WL_ZGLT032  TYPE ZGLT032,
        WL_ZFIT0098 TYPE ZFIT0098,
        GT_ZGLT036  TYPE TABLE OF ZGLT036,
        WL_ZGLT036  TYPE ZGLT036,
        DP_RESP     TYPE CHAR2,
        WL_ZGLT035  TYPE ZGLT035,
        VL_ZUONR    TYPE STRING,
        VL_GSBER    TYPE ZGLT036-GSBER,
        TG_LOTE_GER TYPE TABLE OF TY_LOTE_GER WITH HEADER LINE.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente gerar o documento contábil?'
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

  CLEAR: VL_ZUONR.

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE TG_T030H WITH KEY HKONT = <SAIDA>-HKTRC.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE TG_BSIS WITH KEY BUKRS = <SAIDA>-BUKRS
                                BELNR = <SAIDA>-AUGBL
                                HKONT = TG_T030H-LHREA.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    IF ( <SAIDA>-DOC_RCL IS NOT INITIAL ) OR
       ( TG_BSIS-SHKZG IS INITIAL ) OR
       ( <SAIDA>-HKTVR IS INITIAL ) OR "Conta Variação
       ( <SAIDA>-HKRCL IS INITIAL ) OR "Conta Reclassificação
       ( <SAIDA>-RECLS IS INITIAL ).   "Não Marcado para Reclassificação
      CONTINUE.
    ENDIF.

    VL_ZUONR = TG_BSIS-BELNR.

    CLEAR: TG_077.
    SELECT SINGLE *
      FROM ZGLT077 INTO TG_077
     WHERE BUKRS  = <SAIDA>-BUKRS
       AND BELNR  = <SAIDA>-BELNR
       AND GJAHR  = <SAIDA>-GJAHR
       AND BUZEI  = <SAIDA>-BUZEI.

    IF SY-SUBRC NE 0.
      TG_077-BUKRS  = <SAIDA>-BUKRS.
      TG_077-BELNR  = <SAIDA>-BELNR.
      TG_077-GJAHR  = <SAIDA>-GJAHR.
      TG_077-BUZEI  = <SAIDA>-BUZEI.
      TG_077-HKONT  = <SAIDA>-HKTVR.
      TG_077-HKRCL  = <SAIDA>-HKRCL.
      TG_077-AUGBL  = <SAIDA>-AUGBL.
      TG_077-AUGDT  = <SAIDA>-AUGDT.
      TG_077-BUDAT  = <SAIDA>-BUDAT.
    ELSE.
      IF ( TG_077-LOTE IS NOT INITIAL ).
        CONTINUE.
      ENDIF.
    ENDIF.

    TG_077-USNAM     = SY-UNAME.
    TG_077-DT_ATUAL  = SY-DATUM.
    TG_077-HR_ATUAL  = SY-UZEIT.

    READ TABLE TG_LOTE_GER WITH KEY BUKRS = <SAIDA>-BUKRS.
    IF ( SY-SUBRC = 0 ) AND ( TG_LOTE_GER-LOTE IS NOT INITIAL ).
      E_NUM_LOTE = TG_LOTE_GER-LOTE.
    ELSE.

      DP_RESP = '09'. "Contabilidade

      CALL METHOD ZCL_GERAR_LOTE=>CREATE_LOTE
        EXPORTING
          I_BUKRS      = <SAIDA>-BUKRS
          I_DESCR_LOTE = 'RECLAS.CTA.VAR.'
          I_USER_RESP  = SY-UNAME
          I_DEP_RESP   = DP_RESP
        IMPORTING
          E_NUM_LOTE   = E_NUM_LOTE.

      CLEAR: TG_LOTE_GER.
      TG_LOTE_GER-BUKRS = <SAIDA>-BUKRS.
      TG_LOTE_GER-LOTE  = E_NUM_LOTE.
      APPEND TG_LOTE_GER.

    ENDIF.

    MOVE: E_NUM_LOTE                TO <SAIDA>-LOTE,
          <SAIDA>-LOTE              TO WL_ZGLT035-LOTE,
          <SAIDA>-BUKRS             TO WL_ZGLT035-BUKRS,
          "VL_TIPO_LCTO              TO WL_ZGLT035-TP_LCTO,
          DP_RESP                   TO WL_ZGLT035-DPTO_RESP,
          TG_BSIS-WAERS             TO WL_ZGLT035-MOEDA_DOC,
          "WL_ZGLT031-ST_LC_MOEDA    TO WL_ZGLT035-ST_LC_MOEDA,
          "WL_ZGLT031-MOEDA_INT_HIST TO WL_ZGLT035-MOEDA_INT_HIST,
          "WL_ZGLT031-MOEDA_FT_HIST  TO WL_ZGLT035-MOEDA_FT_HIST,
          "WL_ZGLT031-MOEDA_GP_HIST  TO WL_ZGLT035-MOEDA_GP_HIST,
          TG_BSIS-BLART             TO WL_ZGLT035-BLART,
          'RECLAS.CTA.VAR.'         TO WL_ZGLT035-XBLNR,
          "WL_ZGLT031-BKTXT          TO WL_ZGLT035-BKTXT,
          SY-DATUM                  TO WL_ZGLT035-BUDAT,
          SY-DATUM                  TO WL_ZGLT035-BLDAT,
          SY-DATUM                  TO WL_ZGLT035-DT_LCTO,
          "WL_ZGLT031-PROV_EST       TO WL_ZGLT035-PROV_EST,
          "WL_ZGLT031-ST_AP_FISCAL   TO WL_ZGLT035-ST_AP_FISCAL,
          SY-DATUM+4(2)             TO WL_ZGLT035-MONAT,
          SY-DATUM(4)               TO WL_ZGLT035-GJAHR,
          SY-UNAME                  TO WL_ZGLT035-USNAM,
          SY-DATUM                  TO WL_ZGLT035-DT_ENTRADA,
          SY-UZEIT                  TO WL_ZGLT035-HR_ENTRADA.

    CLEAR: GT_ZGLT036.
    DO 2 TIMES.

      IF <SAIDA>-SHKZG EQ 'H' .
        CASE SY-INDEX.
          WHEN 1.
            WL_ZGLT036-HKONT = <SAIDA>-HKTVR. "Conta Variação
            WL_ZGLT036-BSCHL = '40'.
          WHEN 2.
            WL_ZGLT036-HKONT = <SAIDA>-HKRCL. "Conta Reclassificação
            WL_ZGLT036-BSCHL = '50'.
        ENDCASE.
      ELSE.
        CASE SY-INDEX.
          WHEN 1.
            WL_ZGLT036-HKONT = <SAIDA>-HKRCL. "Conta Reclassificação
            WL_ZGLT036-BSCHL = '40'.
          WHEN 2.
            WL_ZGLT036-HKONT = <SAIDA>-HKTVR. "Conta Variação
            WL_ZGLT036-BSCHL = '50'.
        ENDCASE.
      ENDIF.

      CLEAR: VL_GSBER.
      CONCATENATE <SAIDA>-BUKRS+2(2) '01' VL_GSBER INTO VL_GSBER.

      MOVE: SY-INDEX              TO WL_ZGLT036-SEQITEM,
            VL_ZUONR              TO WL_ZGLT036-ZUONR,
            TG_BSIS-GSBER         TO WL_ZGLT036-GSBER.

      MOVE: ABS( TG_BSIS-DMBTR ) TO WL_ZGLT036-VLR_MOEDA_INT,
            ABS( TG_BSIS-DMBE2 ) TO WL_ZGLT036-VLR_MOEDA_FORTE.

      APPEND WL_ZGLT036 TO GT_ZGLT036.

      CLEAR: WL_ZGLT036, WL_ZGLT032.

    ENDDO.

    CALL METHOD ZCL_GERAR_LOTE=>CONTABILIZAR_LOTE(
      CHANGING
        I_ZGLT036 = GT_ZGLT036
        I_ZGLT035 = WL_ZGLT035 ).

    MOVE: '@B4@'              TO <SAIDA>-STATUS,
          E_NUM_LOTE          TO <SAIDA>-LOTE,
          WL_ZGLT035-DOC_LCTO TO <SAIDA>-DOC_LCTO.

    TG_077-STATUS      = <SAIDA>-STATUS.
    TG_077-LOTE        = E_NUM_LOTE.
    TG_077-DOC_LCTO    = <SAIDA>-DOC_LCTO.
    TG_077-DT_LCTO_CTB = SY-DATUM.

    MODIFY ZGLT077 FROM TG_077.

  ENDLOOP.

  COMMIT WORK.

  LOOP AT TG_LOTE_GER.

    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        P_NUM_LOTE = TG_LOTE_GER-LOTE.

  ENDLOOP.

  CALL METHOD OBJ_ALV->REFRESH_TABLE_DISPLAY.

ENDFORM.

FORM ATRIB_DOC_RECLAS  CHANGING P_SAIDA TYPE TY_SAIDA.

  READ TABLE TG_077 WITH KEY BUKRS = P_SAIDA-BUKRS
                             BELNR = P_SAIDA-BELNR
                             GJAHR = P_SAIDA-GJAHR
                             BUZEI = P_SAIDA-BUZEI.
  IF SY-SUBRC = 0.

    P_SAIDA-LOTE         = TG_077-LOTE.
    P_SAIDA-STATUS       = TG_077-STATUS.
    P_SAIDA-DOC_LCTO     = TG_077-DOC_LCTO.
    P_SAIDA-DT_LCTO_CTB  = TG_077-DT_LCTO_CTB.
    P_SAIDA-DOC_RCL      = TG_077-DOC_RCL.

  ENDIF.

ENDFORM.

FORM REFRESH_DOC_CONTABIL .

  FIELD-SYMBOLS: <SAIDA> TYPE TY_SAIDA.

  DATA: WL_ZGLT034   TYPE ZGLT034,
        WL_ZGLT035   TYPE ZGLT035,
        WL_ZIB_CHAVE TYPE ZIB_CONTABIL_CHV,
        WL_ZIB_ERRO  TYPE ZIB_CONTABIL_ERR.


  CHECK OBJ_ALV IS NOT INITIAL.

  UNASSIGN <SAIDA>.
  CHECK IT_SAIDA IS NOT INITIAL.

  LOOP AT IT_SAIDA ASSIGNING <SAIDA>.

    IF <SAIDA>-DOC_RCL IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR TG_077.
    SELECT SINGLE *
      FROM ZGLT077 INTO TG_077
     WHERE BUKRS       = <SAIDA>-BUKRS
       AND BELNR       = <SAIDA>-BELNR
       AND GJAHR       = <SAIDA>-GJAHR
       AND BUZEI       = <SAIDA>-BUZEI.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CASE <SAIDA>-STATUS.
      WHEN '@B4@' OR '@2K@' OR '@02@'.

        SELECT SINGLE *
          FROM ZGLT034
          INTO WL_ZGLT034
         WHERE BUKRS = <SAIDA>-BUKRS
           AND LOTE  = <SAIDA>-LOTE.

        IF SY-SUBRC NE 0.
          MOVE ABAP_FALSE TO <SAIDA>-LOTE.
          MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
          MOVE ABAP_FALSE TO <SAIDA>-DOC_RCL.
          MOVE '@08@'     TO <SAIDA>-STATUS.

          MOVE ABAP_FALSE TO TG_077-LOTE.
          MOVE ABAP_FALSE TO TG_077-DOC_LCTO.
          MOVE ABAP_FALSE TO TG_077-DOC_RCL.
          MOVE '@08@'     TO TG_077-STATUS.

          MODIFY ZGLT077 FROM TG_077.

          CONTINUE.
        ENDIF.

        "Verifica se Doc. Lcto foi estornado.
        SELECT SINGLE *
          FROM ZGLT035
          INTO WL_ZGLT035
         WHERE BUKRS    = <SAIDA>-BUKRS
           AND DOC_LCTO = <SAIDA>-DOC_LCTO.

        IF ( SY-SUBRC EQ 0 ) AND ( WL_ZGLT035-LOEKZ EQ 'X' ).
          MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
          MOVE ABAP_FALSE TO <SAIDA>-DOC_RCL.
          MOVE ABAP_FALSE TO <SAIDA>-LOTE.
          MOVE '@08@'     TO <SAIDA>-STATUS.

          MOVE ABAP_FALSE TO TG_077-DOC_LCTO.
          MOVE ABAP_FALSE TO TG_077-DOC_RCL.
          MOVE ABAP_FALSE TO TG_077-LOTE.
          MOVE '@08@'     TO TG_077-STATUS.

          MODIFY ZGLT077 FROM TG_077.

          CONTINUE.
        ENDIF.

        PERFORM RETORNA_STATUS_ZIB USING <SAIDA>-DOC_LCTO
                                         WL_ZGLT034-DATA_ATUAL(4)
                                CHANGING WL_ZIB_CHAVE
                                         WL_ZIB_ERRO.

        IF ( WL_ZIB_CHAVE IS NOT INITIAL ).
          MOVE: '@2K@'             TO <SAIDA>-STATUS,
                WL_ZIB_CHAVE-BELNR TO <SAIDA>-DOC_RCL.

          TG_077-STATUS   = <SAIDA>-STATUS.
          TG_077-DOC_RCL  = <SAIDA>-DOC_RCL.
          MODIFY ZGLT077 FROM TG_077.

          MESSAGE S836(SD) WITH TEXT-009.
        ELSEIF ( WL_ZIB_ERRO IS NOT INITIAL ).
          MOVE '@02@' TO <SAIDA>-STATUS.

          TG_077-STATUS = <SAIDA>-STATUS.
          MODIFY ZGLT077 FROM TG_077.

          MESSAGE S836(SD) WITH TEXT-010.
        ENDIF.

    ENDCASE.
  ENDLOOP.

  CALL METHOD OBJ_ALV->REFRESH_TABLE_DISPLAY.

ENDFORM.


FORM RETORNA_STATUS_ZIB USING I_DOC_LCTO TYPE ZGLT077-DOC_LCTO
                              I_ANO_LCTO TYPE ZGLT077-GJAHR
                     CHANGING E_ZIBCHV   TYPE ZIB_CONTABIL_CHV
                              E_ZIBERR   TYPE ZIB_CONTABIL_ERR.

  DATA V_OBJKEY    TYPE CHAR20.
  CLEAR: E_ZIBCHV, E_ZIBERR.

  CONCATENATE 'ZGL17' I_DOC_LCTO I_ANO_LCTO INTO V_OBJKEY.

  SELECT SINGLE *
    FROM ZIB_CONTABIL_CHV
    INTO E_ZIBCHV
   WHERE OBJ_KEY = V_OBJKEY.

  IF ( SY-SUBRC IS NOT INITIAL ).

    SELECT SINGLE *
      FROM ZIB_CONTABIL_ERR
      INTO E_ZIBERR
     WHERE OBJ_KEY = V_OBJKEY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_DOC_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ESTORNAR_DOC_CONT .

  DATA: VAR_ANSWER TYPE C.

  FIELD-SYMBOLS: <SAIDA> TYPE TY_SAIDA.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
  UNASSIGN <SAIDA>.

  DATA: IT_MSG TYPE TABLE OF BDCMSGCOLL, " WITH HEADER LINE,
        WA_MSG TYPE BDCMSGCOLL.

  CALL METHOD OBJ_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK NOT IT_SEL_ROWS IS INITIAL.

  IF ( LINES( IT_SEL_ROWS ) NE 1 ).
    MESSAGE S836(SD) WITH TEXT-011.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente estornar o documento contábil?'
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

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
  READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

  CHECK <SAIDA>-DOC_RCL IS NOT INITIAL.

*  CLEAR: WA_BSAD.
*  SELECT SINGLE *
*    INTO WA_BSAD
*    FROM BSAS
*   WHERE BELNR = <SAIDA>-DOC_RCL
*     AND BUKRS = <SAIDA>-EMPRESA.
*
*  IF SY-SUBRC = 0.
*    MESSAGE 'Documento de Compensação deve ser cancelado primeiro!' TYPE 'S'.
*    RETURN.
*  ENDIF.

  CLEAR TG_077.
  SELECT SINGLE *
    FROM ZGLT077 INTO TG_077
   WHERE BUKRS  = <SAIDA>-BUKRS
     AND BELNR  = <SAIDA>-BELNR
     AND GJAHR  = <SAIDA>-GJAHR
     AND BUZEI  = <SAIDA>-BUZEI.

  IF ( TG_077-DOC_RCL  IS INITIAL ).
    ROLLBACK WORK.
    MESSAGE S836(SD) WITH TEXT-012.
    RETURN.
  ENDIF.

  FREE: IT_DTA.
  DEFINE SHDB.
    CLEAR IT_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  SHDB:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' <SAIDA>-DOC_RCL,
  ' '        ' '    ' '  'BKPF-BUKRS'  <SAIDA>-BUKRS,
  ' '        ' '    ' '  'RF05A-GJAHS' <SAIDA>-DT_LCTO_CTB(4),
  ' '        ' '    ' '  'UF05A-STGRD' '01'.

  OPT-DISMODE = 'E'.
  CALL TRANSACTION 'FB08' USING IT_DTA OPTIONS FROM OPT.

  IF SY-SUBRC IS INITIAL.
    MOVE ABAP_FALSE TO <SAIDA>-LOTE.
    MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
    MOVE ABAP_FALSE TO <SAIDA>-DOC_RCL.
    MOVE '@08@'     TO <SAIDA>-STATUS.

    MOVE ABAP_FALSE TO TG_077-LOTE.
    MOVE ABAP_FALSE TO TG_077-DOC_LCTO.
    MOVE ABAP_FALSE TO TG_077-DOC_RCL.
    MOVE '@08@'     TO TG_077-STATUS.

    MODIFY ZGLT077 FROM TG_077.

    COMMIT WORK.

  ENDIF.

  CALL METHOD OBJ_ALV->REFRESH_TABLE_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_NOMES_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM CONSTRUIR_NOMES_COL.

  CLEAR: VG_WAERS, VG_NCOL_VARCALC, VG_NCOL_VARLANC, VG_NCOL_VLRDIFE.

  SELECT SINGLE WAERS
    FROM T001
    INTO VG_WAERS
   WHERE BUKRS IN S_BUKRS.

  CONCATENATE TEXT-013 VG_WAERS ')' INTO VG_NCOL_VARCALC.
  CONCATENATE TEXT-014 VG_WAERS ')' INTO VG_NCOL_VARLANC.
  CONCATENATE TEXT-015 VG_WAERS ')' INTO VG_NCOL_VLRDIFE.

ENDFORM.

FORM ATRIB_VLR_CALC CHANGING P_SAIDA TYPE TY_SAIDA.

*  CLEAR: TG_T030H, TG_BSIS.
*
*  READ TABLE TG_T030H WITH KEY HKONT = P_SAIDA-HKTRC.
*  IF SY-SUBRC = 0.
*    READ TABLE TG_BSIS WITH KEY BUKRS = P_SAIDA-BUKRS
*                                BELNR = P_SAIDA-AUGBL
*                                HKONT = TG_T030H-LHREA.
*  ENDIF.

  IF ( P_SAIDA-BELNR <> P_SAIDA-AUGBL ) OR ( P_SAIDA-RESIDUAL IS NOT INITIAL ).
    CASE P_SAIDA-WAERS.
      WHEN 'BRL'.

        CHECK P_SAIDA-KURSF IS NOT INITIAL.

        P_SAIDA-VCALC_2 = ( P_SAIDA-DMBTR / ABS( P_SAIDA-KURSF ) ) - P_SAIDA-DMBE2.
      WHEN 'USD'.
        P_SAIDA-VCALC   = ( ABS( P_SAIDA-KURSF ) - ABS( P_SAIDA-TXHIS ) ) * P_SAIDA-DMBE2.
      WHEN OTHERS.
        P_SAIDA-VCALC   = ( ABS( P_SAIDA-KURSF ) - ABS( P_SAIDA-TXHIS ) ) * P_SAIDA-WRBTR.
    ENDCASE.

    P_SAIDA-VCALC    = P_SAIDA-VCALC   * -1.
    P_SAIDA-VCALC_2  = P_SAIDA-VCALC_2 * -1.

    "Atuliazar Variacao Calculada para o documento de compensação/conta reconciliação
    TG_VAR_CALC-BUKRS    = P_SAIDA-BUKRS.
    TG_VAR_CALC-AUGBL    = P_SAIDA-AUGBL.
    TG_VAR_CALC-HKTRC    = P_SAIDA-HKTRC.
    TG_VAR_CALC-GSBER    = P_SAIDA-GSBER.
    TG_VAR_CALC-VCALC    = P_SAIDA-VCALC.
    TG_VAR_CALC-VCALC_2  = P_SAIDA-VCALC_2.
    APPEND TG_VAR_CALC.
  ENDIF.

ENDFORM.

FORM CALC_DIF.

  DATA: VL_OUT_LIMIT TYPE C,
        VL_IN_LIMIT  TYPE C,
        WL_COLOR     TYPE KKBLO_SPECIALCOL,
        VL_MSG       LIKE TG_INF_DOC-MSG.

  DATA: V_CALC_TMP  TYPE BSAD-DMBTR,
        V_CALC2_TMP TYPE BSAD-DMBE2,
        V_TABIX     TYPE SY-TABIX.

  IT_SAIDA_AUX[] = IT_SAIDA[].
  IT_SAIDA_RSD[] = IT_SAIDA[].

  LOOP AT IT_SAIDA INTO WA_SAIDA.

    V_TABIX = SY-TABIX.

    CHECK ( WA_SAIDA-BELNR = WA_SAIDA-AUGBL ) AND ( WA_SAIDA-RESIDUAL IS INITIAL ) .

    DATA(_VAL_01) = ''.   "Check Tx. Cambio x Tx. Historica
    DATA(_VAL_02) = 'X'.  "Check Tx Saldo Residual x Tx Lançamento Original.

    LOOP AT IT_SAIDA_AUX INTO DATA(WA_SAIDA_AUX) WHERE BUKRS EQ WA_SAIDA-BUKRS
                                                   AND AUGBL EQ WA_SAIDA-AUGBL.

      IF ( WA_SAIDA_AUX-BELNR NE WA_SAIDA_AUX-AUGBL ) AND
         ( WA_SAIDA_AUX-TXHIS EQ WA_SAIDA_AUX-KURSF ) AND
         ( WA_SAIDA_AUX-RESIDUAL IS INITIAL     ).
        _VAL_01 = 'X'. "Validou regra 1
      ENDIF.

      IF WA_SAIDA_AUX-RESIDUAL IS NOT INITIAL.
        CASE WA_SAIDA_AUX-KOART.
          WHEN 'D'.
            LOOP AT IT_SAIDA_RSD INTO DATA(WA_SAIDA_RSD) WHERE BUKRS    = WA_SAIDA_AUX-BUKRS
                                                           AND KUNNR    = WA_SAIDA_AUX-KUNNR
                                                           AND AUGBL    = WA_SAIDA_AUX-BELNR
                                                           AND WAERS    = WA_SAIDA_AUX-WAERS
                                                           AND SHKZG    = WA_SAIDA_AUX-SHKZG
                                                           AND RESIDUAL = ''
                                                           AND ( REBZG  = WA_SAIDA_AUX-REBZG OR
                                                                 BELNR  = WA_SAIDA_AUX-REBZG  ).

              CHECK WA_SAIDA_RSD-AUGBL NE WA_SAIDA_RSD-BELNR.
              IF WA_SAIDA_AUX-TXHIS <> WA_SAIDA_RSD-TXHIS. "Possui Tx. Residual divergente do documento de Origem
                _VAL_02 = ''. "Não validou regra do 2
              ENDIF.
            ENDLOOP.
          WHEN 'K'.
            LOOP AT IT_SAIDA_RSD INTO WA_SAIDA_RSD WHERE BUKRS    = WA_SAIDA_AUX-BUKRS
                                                     AND LIFNR    = WA_SAIDA_AUX-LIFNR
                                                     AND AUGBL    = WA_SAIDA_AUX-BELNR
                                                     AND WAERS    = WA_SAIDA_AUX-WAERS
                                                     AND SHKZG    = WA_SAIDA_AUX-SHKZG
                                                     AND RESIDUAL = ''
                                                     AND ( REBZG  = WA_SAIDA_AUX-REBZG OR
                                                           BELNR  = WA_SAIDA_AUX-REBZG  ).

              CHECK WA_SAIDA_RSD-AUGBL NE WA_SAIDA_RSD-BELNR.
              IF WA_SAIDA_AUX-TXHIS <> WA_SAIDA_RSD-TXHIS. "Possui Tx. Residual divergente do documento de Origem
                _VAL_02 = ''. "Não validou regra do 2
              ENDIF.
            ENDLOOP.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    IF ( _VAL_01 IS INITIAL ) OR ( _VAL_02 IS INITIAL ).

      WL_COLOR-FIELDNAME = 'KURSF_VS'.
      WL_COLOR-COLOR-COL = 6.
      WL_COLOR-COLOR-INV = 6.

      LOOP AT IT_SAIDA ASSIGNING FIELD-SYMBOL(<FS_SAIDA>) WHERE BUKRS EQ WA_SAIDA-BUKRS
                                                            AND AUGBL EQ WA_SAIDA-AUGBL.
        READ TABLE <FS_SAIDA>-COLOR WITH KEY FIELDNAME = 'KURSF_VS' TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          APPEND WL_COLOR TO <FS_SAIDA>-COLOR.
        ENDIF.
      ENDLOOP.

      READ TABLE WA_SAIDA-COLOR WITH KEY FIELDNAME = 'KURSF_VS' TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        APPEND WL_COLOR TO WA_SAIDA-COLOR.
      ENDIF.

      TG_INF_DOC-BUKRS = WA_SAIDA-BUKRS.
      TG_INF_DOC-AUGBL = WA_SAIDA-AUGBL.
      IF _VAL_01 IS INITIAL.
        TG_INF_DOC-MSG   = 'Taxa Compensação diferente da Taxa do Documento!'.
      ELSEIF _VAL_02 IS INITIAL.
        TG_INF_DOC-MSG   = 'Taxa Residual diferente do Documento Original!'.
      ENDIF.
      APPEND TG_INF_DOC.
    ENDIF.
    "Fim

    CLEAR: V_CALC_TMP, V_CALC2_TMP.

    LOOP AT TG_VAR_CALC WHERE BUKRS = WA_SAIDA-BUKRS
                          AND AUGBL = WA_SAIDA-AUGBL
                          AND HKTRC = WA_SAIDA-HKTRC
                          AND GSBER = WA_SAIDA-GSBER.

      ADD TG_VAR_CALC-VCALC   TO V_CALC_TMP.
      ADD TG_VAR_CALC-VCALC_2 TO V_CALC2_TMP.
    ENDLOOP.

    IF ( WA_SAIDA-VLANC   <> 0 ) OR
       ( WA_SAIDA-VLANC_2 <> 0 ) OR
       ( V_CALC_TMP       <> 0 ) OR
       ( V_CALC2_TMP      <> 0 ).

      IF ( WA_SAIDA-VLANC  EQ 0 ) AND ( WA_SAIDA-VLANC_2 EQ 0 ).
        CONTINUE.
      ENDIF.

      "WA_SAIDA-VLDIF   = ABS( V_CALC_TMP  ) - ABS( WA_SAIDA-VLANC   ).
      "WA_SAIDA-VLDIF_2 = ABS( V_CALC2_TMP ) - ABS( WA_SAIDA-VLANC_2 ).
      WA_SAIDA-VLDIF   = ( V_CALC_TMP  * -1 ) + ( WA_SAIDA-VLANC   ).
      WA_SAIDA-VLDIF_2 = ( V_CALC2_TMP * -1 ) + ( WA_SAIDA-VLANC_2 ).

      IF ( WA_SAIDA-VLDIF > 0 ).
        IF ( S_LMPOS-LOW > 0 ).
          IF WA_SAIDA-VLDIF > S_LMPOS-LOW. "Verifica se está acima do limite positivo
            VL_OUT_LIMIT = 'X'.
          ELSE.
            VL_IN_LIMIT = 'X'.
          ENDIF.
        ENDIF.
      ELSEIF ( WA_SAIDA-VLDIF < 0 ).
        IF ( S_LMNEG-LOW > 0 ).
          IF ABS( WA_SAIDA-VLDIF ) > S_LMNEG-LOW. "Verifica se está acima do limite negativo
            VL_OUT_LIMIT = 'X'.
          ELSE.
            VL_IN_LIMIT = 'X'.
          ENDIF.
        ENDIF.
      ELSE.
        VL_IN_LIMIT = 'X'.
      ENDIF.

      "Color
      IF VL_OUT_LIMIT IS NOT INITIAL.
        WL_COLOR-FIELDNAME = 'VLDIF'.
        WL_COLOR-COLOR-COL = 6.
        WL_COLOR-COLOR-INV = 6.
        APPEND WL_COLOR TO WA_SAIDA-COLOR.
      ELSEIF VL_IN_LIMIT IS NOT INITIAL.
        WL_COLOR-FIELDNAME = 'VLDIF'.
        WL_COLOR-COLOR-COL = 5.
        WL_COLOR-COLOR-INV = 5.
        APPEND WL_COLOR TO WA_SAIDA-COLOR.
      ENDIF.

    ENDIF.

    IF ( ( R_RC1 IS NOT INITIAL ) OR  "Com Diferença
         ( R_RC2 IS NOT INITIAL ) ).  "Validados

      IF ( VL_IN_LIMIT IS INITIAL ) AND ( VL_OUT_LIMIT IS INITIAL ).
        WA_SAIDA-DEL = 'X'.
      ENDIF.

      IF ( R_RC1 IS NOT INITIAL ) AND  "Com diferença
         ( VL_IN_LIMIT = 'X' ).      "Dentro Limite
        WA_SAIDA-DEL = 'X'.
      ENDIF.

      IF ( R_RC2 IS NOT INITIAL ) AND  "Validados
         ( VL_OUT_LIMIT = 'X' ).      "Fora Limite
        WA_SAIDA-DEL = 'X'.
      ENDIF.

    ENDIF.

    MODIFY IT_SAIDA FROM WA_SAIDA INDEX V_TABIX.

  ENDLOOP.

  DELETE IT_SAIDA WHERE DEL = 'X'.

ENDFORM.

FORM ATRIB_TX_HIS  CHANGING P_SAIDA TYPE TY_SAIDA.

  CASE P_SAIDA-WAERS.
    WHEN 'USD' OR 'BRL'.

      CHECK P_SAIDA-DMBE2 NE 0.

      P_SAIDA-TXHIS = P_SAIDA-DMBTR / P_SAIDA-DMBE2.

    WHEN OTHERS.

      CHECK P_SAIDA-WRBTR NE 0.

      P_SAIDA-TXHIS = P_SAIDA-DMBTR / P_SAIDA-WRBTR.

  ENDCASE.

  P_SAIDA-TXHIS = ABS( P_SAIDA-TXHIS ).

ENDFORM.

FORM F_GET_BSIS_CBANCO USING P_BUKRS     TYPE BSAS-BUKRS
                             P_BELNR     TYPE BSAS-BELNR
                    CHANGING P_KURSF     LIKE BKPF-KURSF.

  CLEAR: TG_BSIS_CBANCO, P_KURSF.

  READ TABLE TG_BSIS_CBANCO WITH KEY BUKRS = P_BUKRS
                                     BELNR = P_BELNR BINARY SEARCH.
  IF ( SY-SUBRC = 0 ) AND
     ( TG_BSIS_CBANCO-DMBTR > 0 ) AND
     ( TG_BSIS_CBANCO-DMBE2 > 0 ).

    TRY.
        P_KURSF = TG_BSIS_CBANCO-DMBTR / TG_BSIS_CBANCO-DMBE2.
      CATCH CX_SY_ARITHMETIC_OVERFLOW.
    ENDTRY.

    IF ( TG_BSIS_CBANCO-WAERS IS NOT INITIAL ) AND
       ( TG_BSIS_CBANCO-WAERS NE 'BRL'       ) AND
       ( TG_BSIS_CBANCO-WAERS NE 'USD'       ) AND
       ( TG_BSIS_CBANCO-WRBTR > 0            ).

      TRY.
          P_KURSF = TG_BSIS_CBANCO-DMBTR / TG_BSIS_CBANCO-WRBTR.
          "P_KURS2 = TG_BSIS_CBANCO-DMBE2 / TG_BSIS_CBANCO-WRBTR.
        CATCH CX_SY_ARITHMETIC_OVERFLOW.
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.
