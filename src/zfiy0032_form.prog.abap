*&---------------------------------------------------------------------*
*&  Include           ZFIY0032_FORM
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

  DATA: VL_TAM_AWKEY TYPE I,
        VL_LAND      TYPE T059U-LAND1,
        VL_LANGU     TYPE SY-LANGU.

  VL_LAND  = 'AR'.
  VL_LANGU = 'S'. "Espanhol

  CLEAR: VG_NOT_FOUND.

*--------------------------------------------------------------------------*
* Seleção Dados REGUP
*--------------------------------------------------------------------------*
  SELECT *
    FROM REGUP INTO CORRESPONDING FIELDS OF TABLE TG_REGUP
   WHERE LAUFD  IN S_LAUFD
     AND ZBUKR  IN S_BUKRS
     AND LIFNR  IN S_LIFNR.

  IF TG_REGUP[] IS INITIAL.
    VG_NOT_FOUND = 'X'.
    EXIT.
  ENDIF.

  "Verifica se para cada TG_REGUP-LAUFI, possui TG_REGUP-XVORL = Branco, caso sim
  "levar esses somente esses registros.
  TG_REGUP_AUX[] = TG_REGUP[].

  SORT TG_REGUP     BY LAUFI.
  SORT TG_REGUP_AUX BY LAUFI XVORL.

  DELETE ADJACENT DUPLICATES FROM TG_REGUP_AUX COMPARING LAUFI XVORL.

  LOOP AT TG_REGUP_AUX WHERE XVORL IS INITIAL.
    LOOP AT TG_REGUP WHERE LAUFI = TG_REGUP_AUX-LAUFI
                       AND XVORL IS NOT INITIAL.
      DELETE TG_REGUP.
    ENDLOOP.
  ENDLOOP.

*--------------------------------------------------------------------------*
* Seleção Dados BKPF
*--------------------------------------------------------------------------*
  SELECT *
    FROM BKPF INTO CORRESPONDING FIELDS OF TABLE TG_BKPF
     FOR ALL ENTRIES IN TG_REGUP
   WHERE BUKRS = TG_REGUP-ZBUKR
     AND BELNR = TG_REGUP-BELNR
     AND GJAHR = TG_REGUP-GJAHR.

  SORT TG_BKPF BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM TG_BKPF COMPARING BUKRS BELNR GJAHR.

  LOOP AT TG_BKPF.
    IF TG_BKPF-AWKEY(1) = 'S'.
      TG_BKPF-XSPLIT = 'X'.
    ENDIF.

    "Pegar a partir do 03 caracter e desprezar os 04 últimos caracteres
    VL_TAM_AWKEY = STRLEN( TG_BKPF-AWKEY ).
    IF VL_TAM_AWKEY > 6.
      VL_TAM_AWKEY   = VL_TAM_AWKEY - 6.
      TG_BKPF-XAWKEY = TG_BKPF-AWKEY+2(VL_TAM_AWKEY).
    ENDIF.

    MODIFY TG_BKPF.
  ENDLOOP.

*--------------------------------------------------------------------------*
* Seleção Dados ZMMT_EE_ZGR_DOCS
*--------------------------------------------------------------------------*
  IF TG_BKPF[] IS NOT INITIAL.

    SELECT *
      FROM ZMMT_EE_ZGR_DOCS INTO CORRESPONDING FIELDS OF TABLE TG_EE_GR_DOC
       FOR ALL ENTRIES IN TG_BKPF
     WHERE OBJ_KEY = TG_BKPF-XAWKEY.

    LOOP AT TG_EE_GR_DOC.
      CONCATENATE TG_EE_GR_DOC-FT_BELNR TG_EE_GR_DOC-FT_GJAHR
             INTO TG_EE_GR_DOC-XAWKEY.

      READ TABLE TG_BKPF WITH KEY XAWKEY = TG_EE_GR_DOC-OBJ_KEY.

      IF SY-SUBRC NE 0.
        DELETE TG_EE_GR_DOC.
      ELSE.
        TG_EE_GR_DOC-BUKRS = TG_BKPF-BUKRS.
        TG_EE_GR_DOC-GJAHR = TG_BKPF-GJAHR.
        TG_EE_GR_DOC-BELNR = TG_BKPF-BELNR.

        SELECT SINGLE TAX_CODE
          FROM ZMMT_EE_ZGR INTO TG_EE_GR_DOC-TAX_CODE
         WHERE OBJ_KEY = TG_EE_GR_DOC-OBJ_KEY.

        MODIFY TG_EE_GR_DOC.
      ENDIF.
    ENDLOOP.

  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados ZMMT_EE_ZGR
*--------------------------------------------------------------------------*
  IF TG_EE_GR_DOC[] IS NOT INITIAL.
    SELECT *
      FROM ZMMT_EE_ZGR INTO CORRESPONDING FIELDS OF TABLE TG_EE_GR
      FOR ALL ENTRIES IN  TG_EE_GR_DOC
     WHERE OBJ_KEY = TG_EE_GR_DOC-OBJ_KEY.

    LOOP AT TG_EE_GR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT     = TG_EE_GR-MATERIAL
        IMPORTING
          OUTPUT    = TG_EE_GR-MATERIAL.

      MODIFY TG_EE_GR.
    ENDLOOP.

  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados MAKT
*--------------------------------------------------------------------------*
  IF TG_EE_GR[] IS NOT INITIAL.
    SELECT *
      FROM MAKT INTO CORRESPONDING FIELDS OF TABLE TG_MAKT
       FOR ALL ENTRIES IN TG_EE_GR
     WHERE MATNR = TG_EE_GR-MATERIAL
       AND SPRAS = VL_LANGU.
  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados IVA
*--------------------------------------------------------------------------*
  IF TG_EE_GR_DOC[] IS NOT INITIAL.

    SELECT *
      FROM BKPF INTO CORRESPONDING FIELDS OF TABLE TG_BKPF_IVA
       FOR ALL ENTRIES IN TG_EE_GR_DOC
     WHERE BUKRS = TG_EE_GR_DOC-BUKRS
       AND GJAHR = TG_EE_GR_DOC-GJAHR
       AND AWKEY = TG_EE_GR_DOC-XAWKEY.

    SORT TG_BKPF_IVA BY BUKRS BELNR GJAHR.
    DELETE ADJACENT DUPLICATES FROM TG_BKPF_IVA COMPARING BUKRS BELNR GJAHR.

    LOOP AT TG_BKPF_IVA.
      LOOP AT TG_EE_GR_DOC WHERE BUKRS  = TG_BKPF_IVA-BUKRS
                             AND GJAHR  = TG_BKPF_IVA-GJAHR
                             AND XAWKEY = TG_BKPF_IVA-AWKEY.
        TG_EE_GR_DOC-IVA_BELNR = TG_BKPF_IVA-BELNR.
        MODIFY TG_EE_GR_DOC.
      ENDLOOP.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados BSET
*--------------------------------------------------------------------------*
  IF TG_EE_GR_DOC[] IS NOT INITIAL.

    SELECT *
      FROM BSET INTO CORRESPONDING FIELDS OF TABLE TG_BSET
       FOR ALL ENTRIES IN TG_EE_GR_DOC
     WHERE BUKRS = TG_EE_GR_DOC-BUKRS
       AND BELNR = TG_EE_GR_DOC-IVA_BELNR
       AND GJAHR = TG_EE_GR_DOC-FT_GJAHR
       AND MWSKZ = TG_EE_GR_DOC-TAX_CODE.

    SORT TG_BSET BY BUKRS BELNR GJAHR BUZEI.
    DELETE ADJACENT DUPLICATES FROM TG_BSET COMPARING BUKRS BELNR GJAHR BUZEI.

  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados REGUPW
*--------------------------------------------------------------------------*
  SELECT *
    FROM REGUPW INTO CORRESPONDING FIELDS OF TABLE TG_REGUPW
     FOR ALL ENTRIES IN TG_REGUP
   WHERE LAUFD  = TG_REGUP-LAUFD
     AND LAUFI  = TG_REGUP-LAUFI
     AND BUKRS  = TG_REGUP-ZBUKR
     AND BELNR  = TG_REGUP-BELNR
     AND GJAHR  = TG_REGUP-GJAHR
     AND BUZEI  = TG_REGUP-BUZEI.

*--------------------------------------------------------------------------*
* Seleção Dados T059P
*--------------------------------------------------------------------------*
  IF TG_REGUPW[] IS NOT INITIAL.

    SELECT *
      FROM T059P INTO CORRESPONDING FIELDS OF TABLE TG_T059P
       FOR ALL ENTRIES IN TG_REGUPW
     WHERE LAND1  = VL_LAND
       AND WITHT  = TG_REGUPW-WITHT.

    SORT TG_T059P BY LAND1 WITHT.
    DELETE ADJACENT DUPLICATES FROM TG_T059P COMPARING LAND1 WITHT.

    LOOP AT TG_T059P.

      SELECT SINGLE TEXT40
        FROM T059U INTO TG_T059P-TEXT40
       WHERE SPRAS = VL_LANGU
         AND LAND1 = VL_LAND
         AND WITHT = TG_T059P-WITHT.

      MODIFY TG_T059P.

      PERFORM ADD_WITHT USING TG_T059P-WITHT.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------------------------------*
* Seleção Dados LFA1
*--------------------------------------------------------------------------*
  SELECT *
    FROM LFA1 INTO CORRESPONDING FIELDS OF TABLE TG_LFA1
    FOR ALL ENTRIES IN TG_REGUP
   WHERE LIFNR = TG_REGUP-LIFNR.

  IF TG_EE_GR[] IS NOT INITIAL.

    SELECT *
      FROM LFA1 APPENDING CORRESPONDING FIELDS OF TABLE TG_LFA1
      FOR ALL ENTRIES IN TG_EE_GR
     WHERE LIFNR = TG_EE_GR-LIFNR.

    SORT TG_LFA1 BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM TG_LFA1 COMPARING LIFNR.

  ENDIF.


ENDFORM.

FORM PROCESSA_DADOS .

  REFRESH: TG_REGUP_AUX.
  TG_REGUP_AUX[] = TG_REGUP[].

  LOOP AT TG_REGUP.

    CLEAR: WA_SAIDA, TG_LFA1, TG_BKPF, TG_EE_GR_DOC, TG_EE_GR, TG_MAKT,
           TG_BKPF_IVA, TG_BSET, TG_REGUPW, TG_T059P.

    "Seleção Dados Cabeçalho Contábil ----------------------------------"
    READ TABLE TG_BKPF WITH KEY BUKRS = TG_REGUP-ZBUKR
                                BELNR = TG_REGUP-BELNR
                                GJAHR = TG_REGUP-GJAHR.

    "Seleção Dados Ent. Grãos Docs. -----------------------------------"
    READ TABLE TG_EE_GR_DOC WITH KEY OBJ_KEY = TG_BKPF-XAWKEY.

    "Seleção Dados Ent. Grãos ------------------------------------------"
    READ TABLE TG_EE_GR WITH KEY OBJ_KEY = TG_EE_GR_DOC-OBJ_KEY.

   "Seleção Material ---------------------------------------------------"
    READ TABLE TG_MAKT WITH KEY MATNR = TG_EE_GR-MATERIAL.

    "Seleção Dados IVA ------------------------------------------------"
    READ TABLE TG_BKPF_IVA WITH KEY BUKRS = TG_EE_GR_DOC-BUKRS
                                    GJAHR = TG_EE_GR_DOC-GJAHR
                                    AWKEY = TG_EE_GR_DOC-XAWKEY.

    "Seleção Dados Fiscais Seg. Doc. ----------------------------------"
    READ TABLE TG_BSET WITH KEY BUKRS = TG_EE_GR_DOC-BUKRS
                                BELNR = TG_EE_GR_DOC-IVA_BELNR
                                GJAHR = TG_EE_GR_DOC-FT_GJAHR
                                MWSKZ = TG_EE_GR_DOC-TAX_CODE.

    "Seleção Dados Categorias de impostos retidos na fonte -----------"
    READ TABLE TG_T059P WITH KEY WITHT = TG_REGUPW-WITHT.


    WA_SAIDA-ZBUKR      = TG_REGUP-ZBUKR.
    WA_SAIDA-GJAHR      = TG_REGUP-GJAHR.
    WA_SAIDA-LAUFI      = TG_REGUP-LAUFI.
    WA_SAIDA-BELNR      = TG_REGUP-BELNR.
    WA_SAIDA-XVORL      = TG_REGUP-XVORL.
    WA_SAIDA-KIDNO      = TG_REGUP-KIDNO.
    WA_SAIDA-FATURA     = TG_REGUP-XBLNR.
    WA_SAIDA-FIN_PAG    = ( TG_EE_GR-VL_CMV * '0.0250' ).
    WA_SAIDA-IVA_PAG    = ( TG_REGUPW-WT_QSSHH  * '0.0250' ).

    WA_SAIDA-PGTO_MERC  = ( ( TG_EE_GR-VL_CMV + TG_BSET-HWSTE ) -
                            ( WA_SAIDA-FIN_PAG + TG_REGUPW-WT_QSSHH + WA_SAIDA-IVA_PAG ) ).

    WA_SAIDA-VLR_MERC  = TG_EE_GR-VL_CMV.
    WA_SAIDA-TOT_IVA   = TG_BSET-HWSTE.
    IF TG_EE_GR-VL_CMV <> 0.
      WA_SAIDA-PERC_IVA  = ( TG_BSET-HWSTE / TG_EE_GR-VL_CMV ) * 100.
    ENDIF.
    WA_SAIDA-TOT_OPERC = ( TG_EE_GR-VL_CMV + TG_BSET-HWSTE ).

    WA_SAIDA-LIFNR     = TG_EE_GR-LIFNR.
    READ TABLE TG_LFA1 WITH KEY LIFNR = WA_SAIDA-LIFNR.
    IF SY-SUBRC = 0.
      WA_SAIDA-CORRETOR = TG_LFA1-NAME1.
    ENDIF.

    IF TG_BKPF-XSPLIT IS INITIAL.
      WA_SAIDA-CD_FORN = TG_REGUP-LIFNR.
    ELSEIF WA_SAIDA-LIFNR IS NOT INITIAL.
      LOOP AT TG_REGUP_AUX WHERE BELNR =  TG_REGUP-BELNR
                             AND LIFNR <> WA_SAIDA-LIFNR.

        WA_SAIDA-CD_FORN = TG_REGUP_AUX-LIFNR.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF WA_SAIDA-CD_FORN IS NOT INITIAL.
      READ TABLE TG_LFA1 WITH KEY LIFNR = WA_SAIDA-CD_FORN.
      IF SY-SUBRC = 0.
        WA_SAIDA-VENDEDOR = TG_LFA1-NAME1.
      ENDIF.
    ENDIF.

    IF TG_REGUPW-WT_QSSHH <> 0.
      WA_SAIDA-ALIQUOTA = (  WA_SAIDA-IVA_PAG / TG_REGUPW-WT_QSSHH ).
    ENDIF.

    PERFORM ATRIB_WITHT USING WA_SAIDA
                              TG_REGUP.

    IF TG_EE_GR-LIFNR IS NOT INITIAL.
      WA_SAIDA-PAGO = TEXT-002.
    ELSE.
      WA_SAIDA-PAGO = TEXT-003.
    ENDIF.

    "WA_SAIDA-TP_COMPROB
    "WA_SAIDA-ORIG_MERC

    WA_SAIDA-PO_NUMBER = TG_EE_GR-PO_NUMBER.
    WA_SAIDA-NRO_MIRO  = TG_EE_GR_DOC-FT_BELNR.
    WA_SAIDA-ENTRY_QNT = TG_EE_GR-ENTRY_QNT.
    WA_SAIDA-MATNR     = TG_EE_GR-MATERIAL.

    READ TABLE TG_MAKT WITH KEY MATNR = WA_SAIDA-MATNR.
    IF SY-SUBRC = 0.
      WA_SAIDA-MAKTX  = TG_MAKT-MAKTX.
    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

ENDFORM.

FORM IMPRIME_DADOS .

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.

  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_VARIANT              = GS_VARIANT
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
      IS_LAYOUT               = WL_LAYOUT
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.

FORM MONTAR_LAYOUT .

  REFRESH:  ESTRUTURA[].

  PERFORM MONTAR_ESTRUTURA USING:


    1   ''  ''   'IT_SAIDA' 'LAUFI'      'Proposta'                 '10' '',
    1   ''  ''   'IT_SAIDA' 'BELNR'      'Doc.Contabil'             '10' 'X',
    1   ''  ''   'IT_SAIDA' 'XVORL'      'Doc.Comp.'                '10' '',
    1   ''  ''   'IT_SAIDA' 'KIDNO'      'Cto.SIGAM'                '10' '',
    1   ''  ''   'IT_SAIDA' 'FATURA'     'Factura'                  '10' '',
    2   ''  ''   'IT_SAIDA' 'PGTO_MERC'  'Pago Mercadoria (CBU)'    '20' '',
    3   ''  ''   'IT_SAIDA' 'VLR_MERC'   'Vlr Mercadoria'           '06' '',
    4   ''  ''   'IT_SAIDA' 'TOT_IVA'    'IVA Total'                '05' '',
    5   ''  ''   'IT_SAIDA' 'PERC_IVA'   '% IVA'                    '08' '',
    6   ''  ''   'IT_SAIDA' 'TOT_OPERC'  'Total Opercion'           '25' '',
    7   ''  ''   'IT_SAIDA' 'FIN_PAG'    'Final a pagar'            '06' '',
    8   ''  ''   'IT_SAIDA' 'LIFNR'      'Cod. Forn.'               '12' '',
    9   ''  ''   'IT_SAIDA' 'CORRETOR'   'Corretor'                 '09' '',
    10  ''  ''   'IT_SAIDA' 'CD_FORN'    'Cod. Forn.'               '07' '',
    11  ''  ''   'IT_SAIDA' 'VENDEDOR'   'Vendedor'                 '03' '',
    12  ''  ''   'IT_SAIDA' 'IVA_PAG'    'IVA Pagar'                '13' '',
    13  ''  ''   'IT_SAIDA' 'ALIQUOTA'   'Alicuota'                 '13' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_01'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_01'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_01'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_02'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_02'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_02'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_03'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_03'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_03'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_04'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_04'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_04'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_05'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_05'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_05'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_06'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_06'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_06'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_07'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_07'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_07'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_08'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_08'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_08'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_09'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_09'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_09'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_10'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_10'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_10'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_11'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_11'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_11'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_12'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_12'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_12'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_13'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_13'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_13'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_14'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_14'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_14'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_15'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_15'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_15'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_16'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_16'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_16'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_17'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_17'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_17'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_18'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_18'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_18'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_19'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_19'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_19'   'Aliq.%'                '08' '',

    14  ''  ''   'IT_SAIDA' 'WT_QSSHH_20'   'Base'                  '13' '',
    14  ''  ''   'IT_SAIDA' 'WT_QBSHH_20'   'Vlr.'                  '08' '',
    14  ''  ''   'IT_SAIDA' 'PER_ALIQ_20'   'Aliq.%'                '08' '',

    18  ''  ''   'IT_SAIDA' 'PAGO'       'Pago'                     '08' '',
    19  ''  ''   'IT_SAIDA' 'TP_COMPROB' 'Tipo Comprobante'         '08' '',
    20  ''  ''   'IT_SAIDA' 'ORIG_MERC'  'Origem Mercadoria'        '13' '',
    21  ''  ''   'IT_SAIDA' 'PO_NUMBER'  'Pedido Compra'            '13' '',
    22  ''  ''   'IT_SAIDA' 'NRO_MIRO'   'Nro.MIRO'                 '12' '',
    23  ''  ''   'IT_SAIDA' 'ENTRY_QNT'  'Quantidade'               '12' '',
    24  ''  ''   'IT_SAIDA' 'MATNR'      'Material'                 '12' '',
    25  ''  ''   'IT_SAIDA' 'MAKTX'      'Desc. Material'           '12' ''.

ENDFORM.                    " MONTAR_LAYOUT

FORM DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " DEFINIR_EVENTOS

FORM F_CARREGAR_EVENTOS USING NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_HOTSPOT).

  DATA: VL_IDX_N  TYPE N LENGTH 2,
        VL_IDX    TYPE C LENGTH 2,
        VL_FIELD  TYPE C LENGTH 10,
        VL_WITH   TYPE T059P-WITHT.

  FIELD-SYMBOLS: <FS_WITHT> TYPE T059P-WITHT.


  DATA: VL_SCRTEXT_AUX TYPE DD03P-SCRTEXT_L,
        VL_SCRTEXT     TYPE DD03P-SCRTEXT_L.

  CLEAR WA_ESTRUTURA.

  CASE P_FIELD(8).

    WHEN 'WT_QSSHH' OR
         'WT_QBSHH' OR
         'PER_ALIQ'.

      CLEAR: VL_FIELD, VL_WITH.
      UNASSIGN  <FS_WITHT>.

      VL_IDX_N = P_FIELD+9(2).
      VL_IDX   = VL_IDX_N.

      CONCATENATE 'WITHT_' VL_IDX INTO VL_FIELD.
      ASSIGN (VL_FIELD) TO <FS_WITHT>.
      IF <FS_WITHT> IS ASSIGNED.
        IF <FS_WITHT> IS INITIAL.
          EXIT.
        ENDIF.

        VL_WITH = <FS_WITHT>.
      ELSE.
        EXIT.
      ENDIF.

      CASE P_FIELD(8).
        WHEN 'WT_QSSHH'.
          PERFORM FORMATA_HEADER USING VL_WITH P_FIELD CHANGING VL_SCRTEXT_AUX.
        WHEN 'WT_QBSHH'.
          PERFORM FORMATA_HEADER USING VL_WITH P_FIELD CHANGING VL_SCRTEXT_AUX.
      ENDCASE.

  ENDCASE.

  IF VL_SCRTEXT_AUX IS NOT INITIAL.
    VL_SCRTEXT = VL_SCRTEXT_AUX.
  ELSE.
    VL_SCRTEXT = P_SCRTEXT_L.
  ENDIF.



  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = VL_SCRTEXT.
  WA_ESTRUTURA-SELTEXT_M     = VL_SCRTEXT.
  WA_ESTRUTURA-SELTEXT_L     = VL_SCRTEXT.
  WA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.

*  CASE P_FIELD(8).
*
*    WHEN 'WT_QSSHH' OR
*         'WT_QBSHH' OR
*         'PER_ALIQ'.
*    WHEN OTHERS.
      WA_ESTRUTURA-DDICTXT  = 'L'.
*  ENDCASE.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM INICIAR_VARIAVEIS .

  DATA: WL_BUKRS(50) TYPE C,
        WL_LAUFD(50) TYPE C,
        WL_LAUFI(50) TYPE C,
        WL_LIFNR(50) TYPE C,

        WL_LAYOUT1(20) VALUE 'Empresa:',
        WL_LAYOUT2(20) VALUE 'Data de pagamento: ',
        WL_LAYOUT3(20) VALUE 'Código Proposta: ',
        WL_LAYOUT4(20) VALUE 'Código Fornecedor: ',

        WL_DATA   VALUE '.',
        WL_SPACE  VALUE '-',
        WL_ATE(3) VALUE 'até',

        WL_BUTXT TYPE BUTXT.

  REFRESH: T_TOP.

  SELECT SINGLE BUTXT
    FROM T001 INTO WL_BUTXT
   WHERE BUKRS IN S_BUKRS.

  CONCATENATE WL_LAYOUT1 S_BUKRS+3(4) WL_SPACE WL_BUTXT  INTO WL_BUKRS SEPARATED BY SPACE.
  CONCATENATE S_LAUFD-LOW+6(2) WL_DATA
              S_LAUFD-LOW+4(2) WL_DATA
              S_LAUFD-LOW(4) INTO WL_LAUFD.

  CONCATENATE WL_LAYOUT2 WL_LAUFD INTO WL_LAUFD SEPARATED BY SPACE.

  IF S_LAUFI-LOW  IS NOT INITIAL AND
     S_LAUFI-HIGH IS NOT INITIAL.
    CONCATENATE WL_LAYOUT3 S_LAUFI-LOW  WL_ATE S_LAUFI-HIGH INTO  WL_LAUFI SEPARATED BY SPACE.
  ENDIF.

  IF S_LAUFI-LOW  IS NOT INITIAL AND
     S_LAUFI-HIGH IS INITIAL.
    CONCATENATE WL_LAYOUT3 S_LAUFI-LOW INTO  WL_LAUFI SEPARATED BY SPACE.
  ENDIF.

  IF S_LIFNR-LOW  IS NOT INITIAL AND
     S_LIFNR-HIGH IS NOT INITIAL.
    CONCATENATE WL_LAYOUT4 S_LIFNR-LOW  WL_ATE S_LIFNR-HIGH INTO WL_LIFNR SEPARATED BY SPACE.
  ENDIF.

  IF S_LIFNR-LOW  IS NOT INITIAL AND
     S_LIFNR-HIGH IS INITIAL.
    CONCATENATE WL_LAYOUT4 S_LIFNR-LOW INTO WL_LIFNR SEPARATED BY SPACE.
  ENDIF.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.

  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_BUKRS.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LAUFD.

  IF WL_LAUFI IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LAUFI.
  ENDIF.

  IF WL_LIFNR IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LIFNR.
  ENDIF.

  V_REPORT = SY-REPID.
  GS_VARIANT-REPORT      = SY-REPID.

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
    WHEN: '&IC1'.

      IF ( RS_SELFIELD-FIELDNAME EQ 'BELNR' ).

        CLEAR: WA_SAIDA.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-ZBUKR.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
  ENDCASE.

ENDFORM.                    "user_command

FORM ADD_WITHT USING P_WITHT TYPE T059P-WITHT.

  DATA: VL_IDX_N  TYPE N LENGTH 2,
        VL_IDX    TYPE C LENGTH 2,
        VL_FIELD  TYPE C LENGTH 10.

  FIELD-SYMBOLS: <FS_WITHT> TYPE T059P-WITHT.

  CHECK P_WITHT IS NOT INITIAL.

  DO 20 TIMES.

    CLEAR: VL_FIELD.
    UNASSIGN  <FS_WITHT>.

    VL_IDX_N = SY-INDEX.
    VL_IDX   = VL_IDX_N.

    CONCATENATE 'WITHT_' VL_IDX INTO VL_FIELD.
    ASSIGN (VL_FIELD) TO <FS_WITHT>.
    IF <FS_WITHT> IS ASSIGNED.
      IF <FS_WITHT> IS INITIAL.
        <FS_WITHT> = P_WITHT.
        EXIT.
      ENDIF.
    ENDIF.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATRIB_WITHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATRIB_WITHT USING P_SAIDA TYPE TY_SAIDA
                       P_REGUP LIKE TG_REGUP.


  "Seleção Dados Info IRF ------------------------------------------"
  LOOP AT TG_REGUPW WHERE LAUFD  = P_REGUP-LAUFD
                      AND LAUFI  = P_REGUP-LAUFI
                      AND BUKRS  = P_REGUP-ZBUKR
                      AND BELNR  = P_REGUP-BELNR
                      AND GJAHR  = P_REGUP-GJAHR
                      AND BUZEI  = P_REGUP-BUZEI.


    CASE TG_REGUPW-WITHT.
      WHEN WITHT_01.

        P_SAIDA-WT_QSSHH_01 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_01 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_01  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_02.

        P_SAIDA-WT_QSSHH_02 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_02 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_02  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_03.

        P_SAIDA-WT_QSSHH_03 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_03 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_03  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_04.

        P_SAIDA-WT_QSSHH_04 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_04 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_04  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_05.

        P_SAIDA-WT_QSSHH_05 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_05 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_05  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_06.

        P_SAIDA-WT_QSSHH_06 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_06 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_06  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_07.

        P_SAIDA-WT_QSSHH_07 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_07 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_07  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_08.

        P_SAIDA-WT_QSSHH_08 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_08 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_08  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_09.

        P_SAIDA-WT_QSSHH_09 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_09 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_09  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_10.

        P_SAIDA-WT_QSSHH_10 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_10 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_10  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_11.

        P_SAIDA-WT_QSSHH_11 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_11 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_11  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_12.

        P_SAIDA-WT_QSSHH_12 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_12 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_12  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_13.

        P_SAIDA-WT_QSSHH_13 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_13 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_13  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_14.

        P_SAIDA-WT_QSSHH_14 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_14 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_14  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_15.

        P_SAIDA-WT_QSSHH_15 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_15 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_15  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_16.

        P_SAIDA-WT_QSSHH_16 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_16 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_16  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_17.

        P_SAIDA-WT_QSSHH_17 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_17 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_17  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_18.

        P_SAIDA-WT_QSSHH_18 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_18 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_18  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_19.

        P_SAIDA-WT_QSSHH_19 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_19 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_19  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

      WHEN WITHT_20.

        P_SAIDA-WT_QSSHH_20 =  TG_REGUPW-WT_QSSHH.
        P_SAIDA-WT_QBSHH_20 =  TG_REGUPW-WT_QBSHH.

        IF TG_REGUPW-WT_QBSHH <> 0.
          WA_SAIDA-PER_ALIQ_20  = ( TG_REGUPW-WT_QSSHH / TG_REGUPW-WT_QBSHH ).
        ENDIF.

    ENDCASE.


  ENDLOOP.





ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORMATA_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WITHT_01  text
*      <--P_VL_SCRTEXT_AUX  text
*----------------------------------------------------------------------*
FORM FORMATA_HEADER  USING    P_WITHT     TYPE T059P-WITHT
                              P_FIELD     TYPE DD03D-FIELDNAME
                     CHANGING P_VL_SCRTEXT_AUX.


  CLEAR: P_VL_SCRTEXT_AUX.

  READ TABLE TG_T059P WITH KEY WITHT = P_WITHT.

  IF ( SY-SUBRC = 0 ) AND ( TG_T059P-TEXT40 IS NOT INITIAL ).

    CASE P_FIELD(8).
      WHEN 'WT_QSSHH'.
        CONCATENATE 'Base -' TG_T059P-TEXT40
               INTO P_VL_SCRTEXT_AUX SEPARATED BY SPACE.
      WHEN 'WT_QBSHH'.
        CONCATENATE 'Vlr -' TG_T059P-TEXT40
               INTO P_VL_SCRTEXT_AUX SEPARATED BY SPACE.
    ENDCASE.

  ENDIF.

ENDFORM.
