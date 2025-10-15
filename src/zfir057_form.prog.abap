*&---------------------------------------------------------------------*
*&  Include           ZFIR057_FORM
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

  DATA: VL_IDX TYPE SY-TABIX.

  PERFORM LIMPA_DADOS.

  "Dados Parcelas
  SELECT *
    INTO TABLE IT_ZFIT0101
    FROM ZFIT0101
   WHERE EMPRESA     IN P_EMP
     AND LOTEAMENTO  IN P_LOTE
     AND NRO_QUADRA  IN P_QUADRA
     AND NRO_TERRENO IN P_TERR.

  IF IT_ZFIT0101[] IS INITIAL.
    MESSAGE 'Dados não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.

  "Dados Terreno
  SELECT *
    FROM ZFIT0099
    INTO TABLE IT_ZFIT0099
    FOR ALL ENTRIES IN IT_ZFIT0101
   WHERE EMPRESA     EQ IT_ZFIT0101-EMPRESA
     AND LOTEAMENTO  EQ IT_ZFIT0101-LOTEAMENTO
     AND NRO_QUADRA  EQ IT_ZFIT0101-NRO_QUADRA
     AND NRO_TERRENO EQ IT_ZFIT0101-NRO_TERRENO.

  "Dados Loteamento
  SELECT *
    FROM ZFIT0098
    INTO TABLE IT_ZFIT0098
    FOR ALL ENTRIES IN IT_ZFIT0101
  WHERE CODIGO EQ IT_ZFIT0101-LOTEAMENTO.

  "Dados Venda
  SELECT *
    FROM ZFIT0100
    INTO TABLE IT_ZFIT0100
    FOR ALL ENTRIES IN IT_ZFIT0101
   WHERE EMPRESA     EQ IT_ZFIT0101-EMPRESA
     AND LOTEAMENTO  EQ IT_ZFIT0101-LOTEAMENTO
     AND NRO_QUADRA  EQ IT_ZFIT0101-NRO_QUADRA
     AND NRO_TERRENO EQ IT_ZFIT0101-NRO_TERRENO
     AND CLIENTE     IN P_KUNNR.

  IF IT_ZFIT0100[] IS NOT INITIAL.
    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_ZFIT0100
     WHERE KUNNR      EQ IT_ZFIT0100-CLIENTE.

    "Dados Eventos
    SELECT *
      FROM ZFIT0104
      INTO TABLE IT_ZFIT0104
      FOR ALL ENTRIES IN IT_ZFIT0101
     WHERE EMPRESA     EQ IT_ZFIT0101-EMPRESA
       AND LOTEAMENTO  EQ IT_ZFIT0101-LOTEAMENTO
       AND NRO_QUADRA  EQ IT_ZFIT0101-NRO_QUADRA
       AND NRO_TERRENO EQ IT_ZFIT0101-NRO_TERRENO
       AND COD_EV      EQ IT_ZFIT0101-COD_EV.

     IF IT_ZFIT0104[] IS INITIAL.
       MESSAGE 'Dados dos eventos não encontrados!' TYPE 'S'.
       RETURN.
     ENDIF.

     SELECT *
       FROM ZFIT0102
       INTO TABLE IT_ZFIT0102
       FOR ALL ENTRIES IN IT_ZFIT0104
       WHERE COD_EV  EQ IT_ZFIT0104-COD_EV.

     IF IT_ZFIT0102[] IS INITIAL.
       MESSAGE 'Dados dos eventos não encontrados!' TYPE 'S'.
       RETURN.
     ENDIF.


  ELSE.
    MESSAGE 'Dados da venda não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.



ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_DADOS .

  CLEAR: WA_SAIDA,WA_ZFIT0098, WA_ZFIT0099,WA_ZFIT0100,WA_ZFIT0104, WA_KNA1.

  REFRESH: IT_SAIDA,
           IT_ZFIT0098,
           IT_ZFIT0099,
           IT_ZFIT0100,
           IT_ZFIT0104,
           IT_KNA1,
           IT_ZFIT0101.

ENDFORM.                    " LIMPA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA_DADOS .

  CASE 'X'.
    WHEN R_ST1. " Partidas em Aberto
      REFRESH: P_AUGDT, P_DT_ALL.
   WHEN R_ST2. " Partidas Compensadas
      REFRESH: P_BUDAT, P_DT_ALL.
   WHEN R_ST3. " Todas Partidas
      REFRESH: P_BUDAT, P_AUGDT.
  ENDCASE.

  LOOP AT IT_ZFIT0101 INTO WA_ZFIT0101.

    CLEAR: WA_SAIDA, WA_ZFIT0100, WA_KNA1, WA_ZFIT0098, WA_BSID, WA_BSAD, WA_BKPF.

    SELECT SINGLE * INTO WA_BSAD FROM BSAD
       WHERE BELNR = WA_ZFIT0101-BELNR
         AND BUKRS = WA_ZFIT0101-EMPRESA.

    IF P_DT_ALL IS NOT INITIAL. "Todas Partidas

      IF ( WA_ZFIT0101-DATA_VENC NOT IN P_DT_ALL ).
        CONTINUE.
      ENDIF.

    ELSEIF P_BUDAT IS NOT INITIAL.  "Partidas em Aberto

      IF ( WA_BSAD IS NOT INITIAL ).
        CONTINUE.
      ENDIF.

      IF ( WA_ZFIT0101-DATA_VENC > P_BUDAT-LOW ).
        CONTINUE.
      ENDIF.

    ELSEIF P_AUGDT IS NOT INITIAL. "Partidas Compensadas

      IF ( WA_BSAD IS INITIAL ).
        CONTINUE.
      ENDIF.

      IF ( WA_ZFIT0101-DATA_VENC NOT IN P_AUGDT ).
        CONTINUE.
      ENDIF.

    ENDIF.

    WA_SAIDA-STATUS      = ICON_LED_RED.
    WA_SAIDA-EMPRESA     = WA_ZFIT0101-EMPRESA.
    WA_SAIDA-LOTEAMENTO  = WA_ZFIT0101-LOTEAMENTO.
    WA_SAIDA-NRO_QUADRA  = WA_ZFIT0101-NRO_QUADRA.
    WA_SAIDA-NRO_TERRENO = WA_ZFIT0101-NRO_TERRENO.

    READ TABLE IT_ZFIT0098 INTO WA_ZFIT0098 WITH KEY CODIGO = WA_SAIDA-LOTEAMENTO.
    WA_SAIDA-NOME_LOTEAMENTO = WA_ZFIT0098-NOME_LOTEAMENTO.

    READ TABLE IT_ZFIT0100 INTO WA_ZFIT0100 WITH KEY EMPRESA     = WA_SAIDA-EMPRESA
                                                     LOTEAMENTO  = WA_SAIDA-LOTEAMENTO
                                                     NRO_QUADRA  = WA_SAIDA-NRO_QUADRA
                                                     NRO_TERRENO = WA_SAIDA-NRO_TERRENO.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    "Dados Venda
    WA_SAIDA-CLIENTE         = WA_ZFIT0100-CLIENTE.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_SAIDA-CLIENTE.
    WA_SAIDA-DESC_CLIENTE    = WA_KNA1-NAME1.

    READ TABLE IT_ZFIT0104 INTO WA_ZFIT0104 WITH KEY EMPRESA     = WA_SAIDA-EMPRESA
                                                     LOTEAMENTO  = WA_SAIDA-LOTEAMENTO
                                                     NRO_QUADRA  = WA_SAIDA-NRO_QUADRA
                                                     NRO_TERRENO = WA_SAIDA-NRO_TERRENO
                                                     COD_EV      = WA_ZFIT0101-COD_EV.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_ZFIT0102 INTO WA_ZFIT0102 WITH KEY COD_EV = WA_ZFIT0101-COD_EV.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    WA_SAIDA-NOME_EVENTO =  WA_ZFIT0102-NOME_EVENTO.
    WA_SAIDA-COND_PGTO       = WA_ZFIT0104-COND_PGTO.

    CASE WA_SAIDA-COND_PGTO.
      WHEN 'P'.
        WA_SAIDA-DESC_COND_PGTO = 'Parcelado'.
      WHEN 'V'.
        WA_SAIDA-DESC_COND_PGTO = 'A Vista'.
    ENDCASE.

    WA_SAIDA-FORMA_PGTO      = WA_ZFIT0104-FORMA_PGTO.

    CASE  WA_SAIDA-FORMA_PGTO.
      WHEN 'B'.
        WA_SAIDA-DESC_FORMA_PGTO = 'Boleto'.
      WHEN 'C'.
        WA_SAIDA-DESC_FORMA_PGTO = 'Credito em Conta'.
    ENDCASE.

    WA_SAIDA-DATA_VENC       = WA_ZFIT0101-DATA_VENC.
    WA_SAIDA-VALOR           = WA_ZFIT0101-VALOR.


    CLEAR: WA_BKPF.

    WA_SAIDA-BELNR = WA_ZFIT0101-BELNR.

    SELECT SINGLE * INTO WA_BKPF FROM BKPF
         WHERE BELNR = WA_ZFIT0101-BELNR
           AND GJAHR = WA_ZFIT0101-DT_LCTO_CTB(4)
           AND BUKRS = WA_ZFIT0101-EMPRESA.

    IF SY-SUBRC = 0.
      WA_SAIDA-BUDAT   = WA_BKPF-BUDAT.
    ENDIF.

    SELECT SINGLE * INTO WA_BSAD FROM BSAD
         WHERE BELNR = WA_ZFIT0101-BELNR
           AND BUKRS = WA_ZFIT0101-EMPRESA.

    IF SY-SUBRC = 0.
      WA_SAIDA-AUGBL  = WA_BSAD-AUGBL.
      WA_SAIDA-AUGDT  = WA_BSAD-AUGDT.
      WA_SAIDA-STATUS = ICON_LED_GREEN.
    ENDIF.

    IF SY-DATUM > WA_SAIDA-DATA_VENC . "Vencido.
      WA_SAIDA-STV = ICON_ALERT.
    ELSE.
      WA_SAIDA-STV = ICON_RESUBMISSION.
    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR WA_ZFIT0101.

  ENDLOOP.





ENDFORM.                    " PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMI_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .

  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.

  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_VARIANT              = GS_VARIANT
      "I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
      IS_LAYOUT               = WL_LAYOUT
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS .
    PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0328   text
*      -->P_ENDFORM  text
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
FORM MONTAR_LAYOUT .

  REFRESH:  ESTRUTURA[].


  PERFORM MONTAR_ESTRUTURA USING:

      1   ''  ''   'IT_SAIDA' 'STATUS'           'Status'               '04' '',
      1   ''  ''   'IT_SAIDA' 'CLIENTE'          'Cliente'              '08' '',
      2   ''  ''   'IT_SAIDA' 'DESC_CLIENTE'     'Nome Cliente'         '08' '',
      3   ''  ''   'IT_SAIDA' 'LOTEAMENTO'       'Cód. Lot.'            '06' '',
      4   ''  ''   'IT_SAIDA' 'NOME_LOTEAMENTO'  'Loteamento'           '20' '',
      5   ''  ''   'IT_SAIDA' 'NRO_QUADRA'       'Quadra'               '06' '',
      6   ''  ''   'IT_SAIDA' 'NRO_TERRENO'      'Lote'                 '05' '',
      7   ''  ''   'IT_SAIDA' 'NOME_EVENTO'      'Evento Recebimento'   '05' '',
      8   ''  ''   'IT_SAIDA' 'COND_PGTO'        'Cond. Pgto'           '08' '',
      09  ''  ''   'IT_SAIDA' 'DESC_COND_PGTO'   'Cond. Pgto'           '08' '',
      10  ''  ''   'IT_SAIDA' 'FORMA_PGTO'       'Forma. Pgto'          '08' '',
      11  ''  ''   'IT_SAIDA' 'DESC_FORMA_PGTO'  'Forma. Pgto'          '08' '',
      12  ''  ''   'IT_SAIDA' 'DATA_VENC'        'Data Venc.'           '08' '',
      13  ''  ''   'IT_SAIDA' 'STV'              'STV'                  '04' '',
      14  ''  ''   'IT_SAIDA' 'VALOR'            'Valor R$'             '08' '',
      15  ''  ''   'IT_SAIDA' 'BUDAT'            'Dt. Lcto'             '13' '',
      16  ''  ''   'IT_SAIDA' 'AUGDT'            'Dt. Comp.'            '13' '',
      17  ''  ''   'IT_SAIDA' 'BELNR'            'Nro. Documento'       '13' '',
      18  ''  ''   'IT_SAIDA' 'AUGBL'            'Doc. Comp.'           '13' ''.



ENDFORM.                    " MONTAR_LAYOUT

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_HOTSPOT).

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
  WA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.

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
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS .

  DATA: WL_LAYOUT2(50) TYPE C,
        WL_DATA VALUE '.',
        WL_SPACE VALUE '-',
        WL_LAYOUT4(3) VALUE 'até',
        WL_BUKRS(50) TYPE C,
        WL_BUTXT TYPE BUTXT.

  REFRESH: T_TOP.

  CASE 'X'.
    WHEN R_ST1. " Partidas em Aberto
      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.
      PERFORM F_CONSTRUIR_CABECALHO USING 'S' TEXT-006.
    WHEN R_ST2. " Partidas Compensadas
      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.
      PERFORM F_CONSTRUIR_CABECALHO USING 'S' TEXT-007.
    WHEN R_ST3. " Todas Partidas
      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-005.
      PERFORM F_CONSTRUIR_CABECALHO USING 'S' TEXT-008.
  ENDCASE.

  V_REPORT = SY-REPID.
  GS_VARIANT-REPORT      = SY-REPID.

ENDFORM.                    " INICIAR_VARIAVEIS

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDA_CAMPOS .

  CLEAR: VG_IMPRIMIR_REL.


  IF P_EMP IS INITIAL.
    MESSAGE 'Informe a empresa!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF P_LOTE IS INITIAL.
    MESSAGE 'Informe o Loteamento!' TYPE 'S'.
    RETURN.
  ENDIF.

  CASE 'X'.

    WHEN R_ST1. " Partidas em Aberto

      IF P_BUDAT IS INITIAL.
        MESSAGE 'Informe o Periodo para Partidas em Aberto!' TYPE 'S'.
        RETURN.
      ENDIF.

   	WHEN R_ST2. " Partidas Compensadas
      IF P_AUGDT IS INITIAL.
        MESSAGE 'Informe o Periodo para Partidas Compensadas!' TYPE 'S'.
        RETURN.
      ENDIF.

   	WHEN R_ST3. " Todas Partidas
       IF P_DT_ALL IS INITIAL.
        MESSAGE 'Informe o Periodo para Todas as Partidas!' TYPE 'S'.
        RETURN.
      ENDIF.

   ENDCASE.

   VG_IMPRIMIR_REL = 'X'.


ENDFORM.                    " VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  CONFIG_RANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONFIG_RANGES .

   REFRESH: P_EMP,  P_LOTE, P_QUADRA,  P_TERR, P_KUNNR, P_BUDAT, P_AUGDT, P_DT_ALL.

   IF ( P_EMP-LOW  IS NOT INITIAL ) AND
      ( P_EMP-HIGH IS NOT INITIAL ).
     P_EMP-SIGN   = 'I'.
     P_EMP-OPTION = 'BT'.
     APPEND P_EMP.
   ELSEIF ( P_EMP-LOW  IS NOT INITIAL ).
     P_EMP-SIGN   = 'I'.
     P_EMP-OPTION = 'EQ'.
     P_EMP-HIGH = P_EMP-LOW.
     APPEND P_EMP.
   ENDIF.

   IF ( P_LOTE-LOW  IS NOT INITIAL ) AND
      ( P_LOTE-HIGH IS NOT INITIAL ).
     P_LOTE-SIGN   = 'I'.
     P_LOTE-OPTION = 'BT'.
     APPEND P_LOTE.
   ELSEIF ( P_LOTE-LOW  IS NOT INITIAL ).
     P_LOTE-SIGN   = 'I'.
     P_LOTE-OPTION = 'EQ'.
     P_LOTE-HIGH = P_LOTE-LOW.
     APPEND P_LOTE.
   ENDIF.

   IF ( P_QUADRA-LOW  IS NOT INITIAL ) AND
      ( P_QUADRA-HIGH IS NOT INITIAL ).
     P_QUADRA-SIGN   = 'I'.
     P_QUADRA-OPTION = 'BT'.
     APPEND P_QUADRA.
   ELSEIF ( P_QUADRA-LOW IS NOT INITIAL ).
     P_QUADRA-SIGN   = 'I'.
     P_QUADRA-OPTION = 'EQ'.
     P_QUADRA-HIGH = P_QUADRA-LOW.
     APPEND P_QUADRA.
   ENDIF.

   IF ( P_TERR-LOW  IS NOT INITIAL ) AND
      ( P_TERR-HIGH IS NOT INITIAL ).
     P_TERR-SIGN   = 'I'.
     P_TERR-OPTION = 'BT'.
     APPEND P_TERR.
   ELSEIF ( P_TERR-LOW  IS NOT INITIAL ).
     P_TERR-SIGN   = 'I'.
     P_TERR-OPTION = 'EQ'.
     P_TERR-HIGH = P_TERR-LOW.
     APPEND P_TERR.
   ENDIF.

   IF ( P_KUNNR-LOW  IS NOT INITIAL ) AND
      ( P_KUNNR-HIGH IS NOT INITIAL ).
     P_KUNNR-SIGN   = 'I'.
     P_KUNNR-OPTION = 'BT'.
     APPEND P_KUNNR.
   ELSEIF ( P_KUNNR-LOW  IS NOT INITIAL ).
     P_KUNNR-SIGN   = 'I'.
     P_KUNNR-OPTION = 'EQ'.
     P_KUNNR-HIGH = P_KUNNR-LOW.
     APPEND P_KUNNR.
   ENDIF.

   IF ( P_AUGDT-LOW  IS NOT INITIAL ) AND
      ( P_AUGDT-HIGH IS NOT INITIAL ).
     P_AUGDT-SIGN   = 'I'.
     P_AUGDT-OPTION = 'BT'.
     APPEND P_AUGDT.
   ELSEIF ( P_AUGDT-LOW  IS NOT INITIAL ).
     P_AUGDT-SIGN   = 'I'.
     P_AUGDT-OPTION = 'EQ'.
     P_AUGDT-HIGH = P_AUGDT-LOW.
     APPEND P_AUGDT.
   ENDIF.

   IF ( P_DT_ALL-LOW  IS NOT INITIAL ) AND
      ( P_DT_ALL-HIGH IS NOT INITIAL ).
     P_DT_ALL-SIGN   = 'I'.
     P_DT_ALL-OPTION = 'BT'.
     APPEND P_DT_ALL.
   ELSEIF ( P_DT_ALL-LOW  IS NOT INITIAL ).
     P_DT_ALL-SIGN   = 'I'.
     P_DT_ALL-OPTION = 'EQ'.
     P_DT_ALL-HIGH = P_DT_ALL-LOW.
     APPEND P_DT_ALL.
   ENDIF.

ENDFORM.                    " CONFIG_RANGES
*&---------------------------------------------------------------------*
*&      Form  PSQ_QD_LOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PSQ_QD_LOW .
   "TYPES
    TYPES: BEGIN OF TY_QUADRA,
             NRO_QUADRA      TYPE ZFIT0099-NRO_QUADRA,
             NRO_TERRENO     TYPE ZFIT0099-NRO_TERRENO,
             ENDERECO        TYPE ZFIT0099-ENDERECO,
           END OF TY_QUADRA.

    DATA: LT_QUADRA TYPE TABLE OF TY_QUADRA,
          LS_QUADRA TYPE TY_QUADRA,
          LT_MAP    TYPE TABLE OF DSELC,
          LS_MAP    TYPE DSELC,
          LT_RETURN TYPE TABLE OF DDSHRETVAL,
          LS_RETURN TYPE DDSHRETVAL,
          LS_STABLE TYPE LVC_S_STBL.

    IF P_LOTE IS INITIAL.
      MESSAGE 'Selecione um Loteamento!' TYPE 'S'.
      EXIT.
    ENDIF.

    "LOAD F4 DATA
    SELECT NRO_QUADRA NRO_TERRENO ENDERECO
      INTO TABLE LT_QUADRA
      FROM ZFIT0099
     WHERE LOTEAMENTO IN P_LOTE.

    SORT LT_QUADRA BY NRO_QUADRA.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0001'.
    LS_MAP-DYFLDNAME = 'NRO_QUADRA'.
    APPEND LS_MAP TO LT_MAP.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0002'.
    LS_MAP-DYFLDNAME = 'NRO_TERRENO'.
    APPEND LS_MAP TO LT_MAP.

    " CALL SEARCH HELP POPUP FUNCTION
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'NRO_TERRENO'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = LT_QUADRA
        DYNPFLD_MAPPING = LT_MAP
        RETURN_TAB      = LT_RETURN
      EXCEPTIONS
        PARAMETER_ERROR = 1
        NO_VALUES_FOUND = 2
        OTHERS          = 3.


    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
    IF LS_RETURN IS NOT INITIAL.
      P_QUADRA-LOW = LS_RETURN-FIELDVAL.
    ENDIF.

    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0002'.
    IF LS_RETURN IS NOT INITIAL.
      P_TERR-LOW = LS_RETURN-FIELDVAL.
    ENDIF.

ENDFORM.                    " PSQ_QD_LOW
*&---------------------------------------------------------------------*
*&      Form  PSQ_QD_HIGH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PSQ_QD_HIGH .
   "TYPES
    TYPES: BEGIN OF TY_QUADRA,
             NRO_QUADRA      TYPE ZFIT0099-NRO_QUADRA,
             NRO_TERRENO     TYPE ZFIT0099-NRO_TERRENO,
             ENDERECO        TYPE ZFIT0099-ENDERECO,
           END OF TY_QUADRA.

    DATA: LT_QUADRA TYPE TABLE OF TY_QUADRA,
          LS_QUADRA TYPE TY_QUADRA,
          LT_MAP    TYPE TABLE OF DSELC,
          LS_MAP    TYPE DSELC,
          LT_RETURN TYPE TABLE OF DDSHRETVAL,
          LS_RETURN TYPE DDSHRETVAL,
          LS_STABLE TYPE LVC_S_STBL.

    IF P_LOTE IS INITIAL.
      MESSAGE 'Selecione um Loteamento!' TYPE 'S'.
      EXIT.
    ENDIF.

    "LOAD F4 DATA
    SELECT NRO_QUADRA NRO_TERRENO ENDERECO
      INTO TABLE LT_QUADRA
      FROM ZFIT0099
     WHERE LOTEAMENTO IN P_LOTE.

    SORT LT_QUADRA BY NRO_QUADRA.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0001'.
    LS_MAP-DYFLDNAME = 'NRO_QUADRA'.
    APPEND LS_MAP TO LT_MAP.

    "SET RETURN FIELD
    CLEAR LS_MAP.
    LS_MAP-FLDNAME = 'F0002'.
    LS_MAP-DYFLDNAME = 'NRO_TERRENO'.
    APPEND LS_MAP TO LT_MAP.

    " CALL SEARCH HELP POPUP FUNCTION
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'NRO_TERRENO'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = LT_QUADRA
        DYNPFLD_MAPPING = LT_MAP
        RETURN_TAB      = LT_RETURN
      EXCEPTIONS
        PARAMETER_ERROR = 1
        NO_VALUES_FOUND = 2
        OTHERS          = 3.


    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
    IF LS_RETURN IS NOT INITIAL.
      P_QUADRA-HIGH = LS_RETURN-FIELDVAL.
    ENDIF.

    " READ SELECTED F4 VALUE
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0002'.
    IF LS_RETURN IS NOT INITIAL.
      P_TERR-HIGH = LS_RETURN-FIELDVAL.
    ENDIF.
ENDFORM.                    " PSQ_QD_HIGH
