*----------------------------------------------------------------------*
***INCLUDE ZGL033_FORM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS .

  DATA: WL_BUKRS(50) TYPE C,
        WL_LAUFD(50) TYPE C,
        WL_LAUFI(50) TYPE C,
        WL_LIFNR(50) TYPE C,

        WL_LAYOUT1(20) VALUE 'Empresa:',
        WL_LAYOUT2(20) VALUE '',
        WL_LAYOUT3(20) VALUE '',
        WL_LAYOUT4(20) VALUE '',

        WL_DATA   VALUE '.',
        WL_SPACE  VALUE '-',
        WL_ATE(3) VALUE 'até',

        WL_BUTXT TYPE BUTXT.

  REFRESH: T_TOP.

  SELECT SINGLE BUTXT
    FROM T001 INTO WL_BUTXT
   WHERE BUKRS IN S_BUKRS.

  CONCATENATE WL_LAYOUT1 S_BUKRS+3(4) WL_SPACE WL_BUTXT  INTO WL_BUKRS SEPARATED BY SPACE.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.

  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_BUKRS.

  V_REPORT = SY-REPID.
  GS_VARIANT-REPORT      = SY-REPID.

ENDFORM.


FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_HOTSPOT).

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
  WA_ESTRUTURA-DDICTXT       = 'L'.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_CARREGAR_EVENTOS USING NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.

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

      IF ( RS_SELFIELD-FIELDNAME EQ 'SEQ_LCTO' ).

        CLEAR: WA_SAIDA.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.

        CHECK WA_SAIDA-SEQ_LCTO IS NOT INITIAL.

        SET PARAMETER ID 'SLCTO' FIELD WA_SAIDA-SEQ_LCTO.
        CALL TRANSACTION 'ZGL047' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  DATA: GT_ZIB_CHV       TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE,
        WL_ZIB_CHV       TYPE ZIB_CONTABIL_CHV,
        VL_OBJKEY        TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
        VL_DT_LCTO_INI  TYPE SY-DATUM,
        VL_DT_LCTO_FIM  TYPE SY-DATUM.

  CLEAR: VG_NOT_FOUND.

  REFRESH: TG_050, TG_067, TG_BSAK, TG_BSAD, TG_LFA1.

*----------------------------------------------------------*
*  Valida Competência
*----------------------------------------------------------*

  PERFORM VALIDA_COMPETENCIA USING P_COMP
                          CHANGING VG_NOT_FOUND
                                   VL_DT_LCTO_INI
                                   VL_DT_LCTO_FIM.

  CHECK VG_NOT_FOUND IS INITIAL.


  PERFORM VALIDA_COMPETENCIA USING P_CPPG
                          CHANGING VG_NOT_FOUND
                                   VG_DT_PGTO_INI
                                   VG_DT_PGTO_FIM.

  CHECK VG_NOT_FOUND IS INITIAL.

*----------------------------------------------------------*
*  Seleção Dados Cabeçalho
*----------------------------------------------------------*
  SELECT *
    FROM ZGLT050 INTO CORRESPONDING FIELDS OF TABLE TG_050
   WHERE BUKRS        IN S_BUKRS
     AND VIG_DE       IN S_VGDE
     AND VIG_ATE      IN S_VGATE
     AND COD_SEGURADORA IN S_LIFNR
     AND SEQ_TIPO     IN S_STIPO
     AND TP_OPR       IN S_TPOPR
     AND SEQ_LCTO     IN S_SLCTO
     AND NRO_APOLICE  IN S_NR_APL
     AND LOEKZ        EQ ''.

  IF TG_050[] IS INITIAL.
    VG_NOT_FOUND = 'X'.
    RETURN.
  ENDIF.

  SORT TG_050 BY SEQ_LCTO.

*----------------------------------------------------------*
*  Seleção de Tipos
*----------------------------------------------------------*
  SELECT *
    FROM ZGLT064 INTO CORRESPONDING FIELDS OF TABLE TG_064
     FOR ALL ENTRIES IN TG_050
   WHERE SEQ_TIPO EQ TG_050-SEQ_TIPO.

*----------------------------------------------------------*
*  Seleção de Fornecedor
*----------------------------------------------------------*

  SELECT *
    FROM LFA1 INTO CORRESPONDING FIELDS OF TABLE TG_LFA1
     FOR ALL ENTRIES IN TG_050
  WHERE LIFNR EQ TG_050-COD_SEGURADORA.

*----------------------------------------------------------*
*  Seleção Dados Pagamento/Recebimento
*----------------------------------------------------------*

  SELECT *
    FROM ZGLT067 INTO CORRESPONDING FIELDS OF TABLE TG_067
     FOR ALL ENTRIES IN TG_050
   WHERE SEQ_LCTO EQ TG_050-SEQ_LCTO
     AND WERKS    IN S_WERKS
     AND DT_VENC  IN S_ZFBDT.

  IF TG_067[] IS INITIAL.
    VG_NOT_FOUND = 'X'.
    RETURN.
  ENDIF.

  LOOP AT TG_067.

    CLEAR: WL_ZIB_CHV.

    READ TABLE TG_050 WITH KEY SEQ_LCTO = TG_067-SEQ_LCTO.

    CHECK SY-SUBRC = 0.

    TG_067-BUKRS = TG_050-BUKRS.

    IF TG_067-DT_LCTO_CTB IS INITIAL.
      TG_067-DT_LCTO_CTB = TG_067-ERDAT.
    ENDIF.

    CONCATENATE 'ZGL17' TG_067-DOC_LCTO TG_067-DT_LCTO_CTB(4) INTO TG_067-OBJ_KEY.

    MODIFY TG_067.

    IF ( VL_DT_LCTO_FIM IS NOT INITIAL ) AND ( TG_067-DT_LCTO_CTB > VL_DT_LCTO_FIM ).
      DELETE TG_067.
    ENDIF.

  ENDLOOP.

  IF TG_067[] IS INITIAL.
    VG_NOT_FOUND = 'X'.
    RETURN.
  ENDIF.

  SELECT *
    FROM ZIB_CONTABIL_CHV INTO TABLE GT_ZIB_CHV
     FOR ALL ENTRIES IN TG_067
   WHERE OBJ_KEY = TG_067-OBJ_KEY.

  SORT GT_ZIB_CHV BY OBJ_KEY.

  LOOP AT TG_067.

    READ TABLE GT_ZIB_CHV WITH KEY OBJ_KEY = TG_067-OBJ_KEY BINARY SEARCH.

    IF ( SY-SUBRC = 0 ) AND ( GT_ZIB_CHV-BELNR IS NOT INITIAL ).
      TG_067-BELNR = GT_ZIB_CHV-BELNR.
    ENDIF.

    MODIFY TG_067.
  ENDLOOP.

  SELECT *
    FROM BSAK INTO TABLE TG_BSAK
     FOR ALL ENTRIES IN TG_067
   WHERE BUKRS = TG_067-BUKRS
     AND BELNR = TG_067-BELNR.

  SELECT *
    FROM BSAD INTO TABLE TG_BSAD
     FOR ALL ENTRIES IN TG_067
   WHERE BUKRS = TG_067-BUKRS
     AND BELNR = TG_067-BELNR.

*----------------------------------------------------------*
*  Seleção Apolices de Baixa e seus bens
*----------------------------------------------------------*
  SELECT *
    FROM ZGLT050 INTO TABLE TG_050_BX
     FOR ALL ENTRIES IN TG_050
   WHERE TP_OPR = 'B'
     AND REF_SEQ_LCTO = TG_050-SEQ_LCTO
     AND LOEKZ        EQ ''.

  SELECT *
    FROM ZGLT067 INTO CORRESPONDING FIELDS OF TABLE TG_067_BX
     FOR ALL ENTRIES IN TG_050_BX
   WHERE SEQ_LCTO EQ TG_050_BX-SEQ_LCTO.

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

  DATA: WL_COLOR    TYPE KKBLO_SPECIALCOL,
        VL_COL      LIKE WL_COLOR-COLOR-COL,
        VL_DT_BAIXA TYPE ZGLT068-DT_BAIXA.

  LOOP AT TG_067.
    CLEAR: WA_SAIDA, TG_050, TG_064, WL_COLOR, VL_COL,VL_DT_BAIXA, TG_BSAD, TG_BSAK, TG_LFA1.

    READ TABLE TG_050 WITH KEY SEQ_LCTO = TG_067-SEQ_LCTO.
    CHECK SY-SUBRC = 0.

    READ TABLE TG_064 WITH KEY SEQ_TIPO = TG_050-SEQ_TIPO.
    CHECK SY-SUBRC = 0.

    READ TABLE TG_LFA1 WITH KEY LIFNR = TG_050-COD_SEGURADORA.

    IF ( TG_067-BUKRS IS NOT INITIAL ) AND ( TG_067-BELNR IS NOT INITIAL ).
      READ TABLE TG_BSAK WITH KEY BUKRS = TG_067-BUKRS
                                  BELNR = TG_067-BELNR.

      READ TABLE TG_BSAD WITH KEY BUKRS = TG_067-BUKRS
                                  BELNR = TG_067-BELNR.
    ENDIF.


    WA_SAIDA-SEQ_LCTO         = TG_050-SEQ_LCTO.
    WA_SAIDA-BUKRS            = TG_050-BUKRS.
    WA_SAIDA-TP_OPR           = TG_050-TP_OPR.
    WA_SAIDA-NRO_APOLICE      = TG_050-NRO_APOLICE.
    WA_SAIDA-REF_SEQ_LCTO     = TG_050-REF_SEQ_LCTO.
    WA_SAIDA-VIG_DE           = TG_050-VIG_DE.
    WA_SAIDA-VIG_ATE          = TG_050-VIG_ATE.
    WA_SAIDA-SEQ_TIPO         = TG_050-SEQ_TIPO.
    WA_SAIDA-DESCR_TIPO       = TG_064-DESCR.
    WA_SAIDA-COD_SEGURADORA   = TG_050-COD_SEGURADORA.
    WA_SAIDA-NAME1            = TG_LFA1-NAME1.
    WA_SAIDA-WAERS            = TG_050-WAERS.
    WA_SAIDA-WKURS            = TG_067-WKURS.
    WA_SAIDA-SEQ_PARC         = TG_050-SEQ_PARC.
    WA_SAIDA-NRO_PARC         = TG_067-NRO_PARC.
    WA_SAIDA-WERKS            = TG_067-WERKS.
    WA_SAIDA-VLR_PREMIO_USD   = TG_067-VLR_PREMIO_USD.
    WA_SAIDA-VLR_PREMIO_BRL   = TG_067-VLR_PREMIO_BRL.
    WA_SAIDA-DT_VENC          = TG_067-DT_VENC.
    WA_SAIDA-HBKID            = TG_067-HBKID.
    WA_SAIDA-BVTYP            = TG_067-BVTYP.
    WA_SAIDA-ZLSPR            = TG_067-ZLSPR.
    WA_SAIDA-ZLSCH            = TG_067-ZLSCH.
    WA_SAIDA-BANKS            = TG_067-BANKS.
    WA_SAIDA-LOTE             = TG_067-LOTE.
    WA_SAIDA-DOC_LCTO         = TG_067-DOC_LCTO.
    WA_SAIDA-DT_LCTO_CTB      = TG_067-DT_LCTO_CTB.
    WA_SAIDA-BELNR            = TG_067-BELNR.

    IF TG_BSAD IS NOT INITIAL.
      WA_SAIDA-AUGBL = TG_BSAD-AUGBL.
      WA_SAIDA-AUGDT = TG_BSAD-AUGDT.
    ELSEIF TG_BSAK IS NOT INITIAL.
      WA_SAIDA-AUGBL = TG_BSAK-AUGBL.
      WA_SAIDA-AUGDT = TG_BSAK-AUGDT.
    ENDIF.

    IF VG_DT_PGTO_FIM IS NOT INITIAL.
      CHECK ( WA_SAIDA-AUGDT IS INITIAL ) OR
            ( WA_SAIDA-AUGDT >= VG_DT_PGTO_FIM ).
    ENDIF.

    WA_SAIDA-ERNAM            = TG_067-ERNAM.
    WA_SAIDA-ERDAT            = TG_067-ERDAT.
    WA_SAIDA-COD_BARRAS       = TG_067-COD_BARRAS.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

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

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.

  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WL_LAYOUT-COLTAB_FIELDNAME = 'COLOR'.
  WL_LAYOUT-EDIT_MODE = 'A'.
  "WL_LAYOUT-EDIT = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPORT
      IS_VARIANT               = GS_VARIANT_C
      "I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IT_FIELDCAT              = ESTRUTURA[]
      IS_LAYOUT                = WL_LAYOUT
      I_SAVE                   = 'X'
      IT_EVENTS                = EVENTS
      IS_PRINT                 = T_PRINT
    TABLES
      T_OUTTAB                 = IT_SAIDA.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .

    PERFORM MONTAR_ESTRUTURA USING:

    01   'ZGLT050'  'SEQ_LCTO'          'IT_SAIDA' 'SEQ_LCTO'        'Seq.Lcto'           '' 'X',
    02   'ZGLT050'  'BUKRS'             'IT_SAIDA' 'BUKRS'           'Empresa'            '' '',
    03   'ZGLT050'  'TP_OPR'            'IT_SAIDA' 'TP_OPR'          'Tipo Opr.'          '' '',
    04   'ZGLT050'  'NRO_APOLICE'       'IT_SAIDA' 'NRO_APOLICE'     'Nro. Apólice'       '' '',
    05   'ZGLT050'  'REF_SEQ_LCTO'      'IT_SAIDA' 'REF_SEQ_LCTO'    'Ref.Seq.Lcto'       '' '',
    06   'ZGLT050'  'VIG_DE'            'IT_SAIDA' 'VIG_DE'          'Dt.Ini.Vig.'        '' '',
    07   'ZGLT050'  'VIG_ATE'           'IT_SAIDA' 'VIG_ATE'         'Dt.Fim.Vig.'        '' '',
    08   'ZGLT050'  'SEQ_TIPO'          'IT_SAIDA' 'SEQ_TIPO'        'Tipo'               '' '',
    09   'ZGLT064'  'DESCR'             'IT_SAIDA' 'DESCR_TIPO'      'Desc.Tp.'           '' '',
    10   'ZGLT050'  'COD_SEGURADORA'    'IT_SAIDA' 'COD_SEGURADORA'  'Seguradora'         '' '',
    11   'LFA1'     'NAME1'             'IT_SAIDA' 'NAME1'           'Desc.Seg.'          '' '',
    10   'ZGLT050'  'WAERS'             'IT_SAIDA' 'WAERS'           'Moeda'              '' '',
    12   'ZGLT068'  'WKURS'             'IT_SAIDA' 'WKURS'           'Tx.Câmbio'          '' '',
    13   'ZGLT050'  'SEQ_PARC'          'IT_SAIDA' 'SEQ_PARC'        'Tot.Parc.'          '' '',
    14   'ZGLT067'  'NRO_PARC'          'IT_SAIDA' 'NRO_PARC'        'Nro.Parc.'          '' '',
    15   'ZGLT068'  'WERKS'             'IT_SAIDA' 'WERKS'           'Filial'             '' '',
    16   'ZGLT067'  'VLR_PREMIO_USD'    'IT_SAIDA' 'VLR_PREMIO_USD'  'Vlr.USD'            '' '',
    17   'ZGLT067'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'VLR_PREMIO_BRL'  'Vlr.BRL'            '' '',
    18   'ZGLT067'  'DT_VENC'           'IT_SAIDA' 'DT_VENC'         'Dt.Venc.'           '' '',
    19   'ZGLT067'  'HBKID'             'IT_SAIDA' 'HBKID'           'Banco Empresa'      '' '',
    20   'ZGLT067'  'BVTYP'             'IT_SAIDA' 'BVTYP'           'Chv.Banco'          '' '',
    21   'ZGLT067'  'ZLSPR'             'IT_SAIDA' 'ZLSPR'           'Bloq.Pgto'          '' '',
    22   'ZGLT067'  'ZLSCH'             'IT_SAIDA' 'ZLSCH'           'Frm.Pgto'           '' '',
    23   'ZGLT067'  'BANKS'             'IT_SAIDA' 'BANKS'           'Cod.País'           '' '',
    24   'ZGLT067'  'LOTE'              'IT_SAIDA' 'LOTE'            'Lote'               '' '',
    25   'ZGLT067'  'DOC_LCTO'          'IT_SAIDA' 'DOC_LCTO'        'Doc.Lcto'           '' '',
    26   'ZGLT067'  'DT_LCTO_CTB'       'IT_SAIDA' 'DT_LCTO_CTB'     'Dt.Lcto.Ctb'        '' '',
    27   'ZGLT067'  'BELNR'             'IT_SAIDA' 'BELNR'           'Doc.Contábil'       '' '',
    28   'BSAK'     'AUGBL'             'IT_SAIDA' 'AUGBL'           'Doc.Compensação'    '' '',
    29   'BSAK'     'AUGDT'             'IT_SAIDA' 'AUGDT'           'Dt.Compensação'     '' '',
    30   'ZGLT067'  'ERNAM'             'IT_SAIDA' 'ERNAM'           'Usuário'            '' '',
    31   'ZGLT067'  'ERDAT'             'IT_SAIDA' 'ERDAT'           'Dt.Registro'        '' '',
    32   'ZGLT067'  'COD_BARRAS'        'IT_SAIDA' 'COD_BARRAS'      'Cod.Barras'         '' ''.


ENDFORM.

FORM HELP_SEARCH USING P_FIELD TYPE CHAR30.

  DATA: VL_DYFIELD  TYPE HELP_INFO-DYNPROFLD,
        VL_RETFIELD TYPE DFIES-FIELDNAME.

  DATA: GT_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        GT_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  CASE P_FIELD.
    WHEN 'S_STIPO-LOW'.

      DATA: BEGIN OF GT_SEQ_TIPO OCCURS 0,
              SEQ_TIPO       TYPE ZGLT064-SEQ_TIPO,
              DESCR        TYPE ZGLT064-DESCR,
            END OF GT_SEQ_TIPO.

      CLEAR GT_SEQ_TIPO[].

      SELECT *
        FROM ZGLT064
        INTO CORRESPONDING FIELDS OF TABLE GT_SEQ_TIPO.

      SORT GT_SEQ_TIPO BY SEQ_TIPO.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'SEQ_TIPO'
          DYNPPROG        = SY-REPID
          DYNPNR          = SY-DYNNR
          DYNPROFIELD     = 'S_STIPO-LOW'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = GT_SEQ_TIPO
          RETURN_TAB      = GT_RETURN_TAB
          DYNPFLD_MAPPING = GT_DSELC.

    WHEN 'S_SLCTO-LOW' OR 'S_SLCTO-HIGH'.

      DATA: BEGIN OF GT_SEQ_LCTO OCCURS 0,
              SEQ_LCTO       TYPE ZGLT050-SEQ_LCTO,
              TP_LCTO        TYPE ZGLT050-TP_LCTO,
              DEP_RESP       TYPE ZGLT050-DEP_RESP,
              BUKRS          TYPE ZGLT050-BUKRS,
              NRO_APOLICE    TYPE ZGLT050-NRO_APOLICE,
              COD_SEGURADORA TYPE ZGLT050-COD_SEGURADORA,
              DT_CRIACAO     TYPE ZGLT050-DT_CRIACAO,
              USNAM          TYPE ZGLT050-USNAM,
            END OF GT_SEQ_LCTO.

      VL_RETFIELD = 'SEQ_LCTO'.
      VL_DYFIELD  = P_FIELD.

      REFRESH GT_SEQ_LCTO.
      CLEAR GT_SEQ_LCTO.

      SELECT SEQ_LCTO TP_LCTO DEP_RESP BUKRS NRO_APOLICE
             COD_SEGURADORA DT_CRIACAO USNAM
        FROM ZGLT050 INTO CORRESPONDING FIELDS OF TABLE GT_SEQ_LCTO
       WHERE LOEKZ      EQ ''.

      SORT GT_SEQ_LCTO BY SEQ_LCTO.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = VL_RETFIELD
          DYNPPROG        = SY-REPID
          DYNPNR          = SY-DYNNR
          DYNPROFIELD     = VL_DYFIELD
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = GT_SEQ_LCTO
          RETURN_TAB      = GT_RETURN_TAB
          DYNPFLD_MAPPING = GT_DSELC.

  ENDCASE.


ENDFORM.

FORM VALIDA_COMPETENCIA  USING    P_COMPETENCIA
                         CHANGING P_RETURN_STATUS
                                  P_DT_INI
                                  P_DT_FIM.

  DATA: VL_MES          TYPE I,
        VL_ANO          TYPE I,
        VL_DT_INI(8)    TYPE C,
        VL_DT_FIM(8)    TYPE C,
        VL_DT_LOW       TYPE SY-DATUM,
        VL_DT_HIGH_IN   TYPE SY-DATUM,
        VL_DT_HIGH_OUT  TYPE SY-DATUM.

  CLEAR: P_RETURN_STATUS, P_DT_INI, P_DT_FIM.

  CHECK P_COMPETENCIA IS NOT INITIAL.

  VL_MES = P_COMPETENCIA(02).
  VL_ANO = P_COMPETENCIA+2(04).

  IF ( VL_MES = 0 ) OR ( VL_MES > 12 ).
    P_RETURN_STATUS = 'X'.
    MESSAGE S836(SD) WITH TEXT-E01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ( VL_ANO = 0 ).
    P_RETURN_STATUS = 'X'.
    MESSAGE S836(SD) WITH TEXT-E02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CONCATENATE P_COMPETENCIA+2(4) P_COMPETENCIA(2) '01' INTO VL_DT_INI.

  CONCATENATE P_COMPETENCIA+2(4) P_COMPETENCIA(2) '01' INTO VL_DT_FIM.

  VL_DT_LOW     = VL_DT_INI.
  VL_DT_HIGH_IN = VL_DT_FIM.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN = VL_DT_HIGH_IN
    IMPORTING
      LAST_DAY_OF_MONTH = VL_DT_HIGH_OUT.

  P_DT_INI  = VL_DT_LOW.
  P_DT_FIM  = VL_DT_HIGH_OUT.

ENDFORM.
