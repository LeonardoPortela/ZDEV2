*----------------------------------------------------------------------*
***INCLUDE ZFIR056_FORM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  PERFORM LIMPA_DADOS.

  "Dados Terreno
  SELECT *
    FROM ZFIT0099
    INTO TABLE IT_ZFIT0099
   WHERE EMPRESA     IN P_EMP
     AND LOTEAMENTO  IN P_LOTE
     AND NRO_QUADRA  IN P_QUADRA
     AND NRO_TERRENO IN P_TERR.

  IF IT_ZFIT0099[] IS INITIAL.
    MESSAGE 'Dados não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.

  "Dados Loteamento
  SELECT *
    FROM ZFIT0098
    INTO TABLE IT_ZFIT0098
    FOR ALL ENTRIES IN IT_ZFIT0099
  WHERE CODIGO EQ IT_ZFIT0099-LOTEAMENTO.

  "Dados Venda
  SELECT *
    FROM ZFIT0100
    INTO TABLE IT_ZFIT0100
    FOR ALL ENTRIES IN IT_ZFIT0099
   WHERE EMPRESA     EQ IT_ZFIT0099-EMPRESA
     AND LOTEAMENTO  EQ IT_ZFIT0099-LOTEAMENTO
     AND NRO_QUADRA  EQ IT_ZFIT0099-NRO_QUADRA
     AND NRO_TERRENO EQ IT_ZFIT0099-NRO_TERRENO.

  IF IT_ZFIT0100[] IS NOT INITIAL.
    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_ZFIT0100
     WHERE KUNNR      EQ IT_ZFIT0100-CLIENTE.

    SELECT A~EMPRESA
           A~LOTEAMENTO
           A~NRO_TERRENO
           A~NRO_QUADRA
           A~COD_EV
           A~VALOR
           A~COND_PGTO
           A~FORMA_PGTO
           A~QTD_PARCELAS
           A~DATA_VENC
      INTO TABLE IT_ZFIT0104
      FROM ZFIT0104 AS A
      INNER JOIN ZFIT0102 AS B ON A~COD_EV = B~COD_EV
       FOR ALL ENTRIES IN IT_ZFIT0100
      WHERE A~EMPRESA     EQ IT_ZFIT0100-EMPRESA
        AND A~LOTEAMENTO  EQ IT_ZFIT0100-LOTEAMENTO
        AND A~NRO_QUADRA  EQ IT_ZFIT0100-NRO_QUADRA
        AND A~NRO_TERRENO EQ IT_ZFIT0100-NRO_TERRENO
        AND B~TIPO = 'CR'.

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
           IT_KNA1.

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

  LOOP AT IT_ZFIT0099 INTO WA_ZFIT0099.

    CLEAR: WA_SAIDA, WA_ZFIT0100, WA_KNA1, WA_ZFIT0098.

    WA_SAIDA-EMPRESA    = WA_ZFIT0099-EMPRESA.
    WA_SAIDA-LOTEAMENTO = WA_ZFIT0099-LOTEAMENTO.

    READ TABLE IT_ZFIT0098 INTO WA_ZFIT0098 WITH KEY CODIGO = WA_SAIDA-LOTEAMENTO.
    WA_SAIDA-NOME_LOTEAMENTO = WA_ZFIT0098-NOME_LOTEAMENTO.

    WA_SAIDA-NRO_QUADRA      = WA_ZFIT0099-NRO_QUADRA.
    WA_SAIDA-NRO_TERRENO     = WA_ZFIT0099-NRO_TERRENO.
    WA_SAIDA-MATRICULA       = WA_ZFIT0099-MATRICULA.
    WA_SAIDA-ENDERECO        = WA_ZFIT0099-ENDERECO.
    WA_SAIDA-NUMERO          = WA_ZFIT0099-NUMERO.
    WA_SAIDA-BAIRRO_LOCALIZ  = WA_ZFIT0099-BAIRRO_LOCALIZ.
    WA_SAIDA-CEP             = WA_ZFIT0099-CEP.
    WA_SAIDA-CIDADE          = WA_ZFIT0099-CIDADE.
    WA_SAIDA-UF              = WA_ZFIT0099-UF.
    WA_SAIDA-VALOR           = WA_ZFIT0099-VALOR.
    WA_SAIDA-VALOR_AGREGADO  = WA_ZFIT0099-VALOR_AGREGADO.
    WA_SAIDA-VALOR_TOTAL     = WA_ZFIT0099-VALOR_TOTAL.
    WA_SAIDA-MET_LADO_DIR    = WA_ZFIT0099-MET_LADO_DIR.
    WA_SAIDA-MET_LADO_ESQ    = WA_ZFIT0099-MET_LADO_ESQ.
    WA_SAIDA-MET_FUNDOS      = WA_ZFIT0099-MET_FUNDOS.
    WA_SAIDA-MET_FRENTE      = WA_ZFIT0099-MET_FRENTE.
    WA_SAIDA-MET_TOT_M2      = WA_ZFIT0099-MET_TOT_M2.

    IF R_ST2 IS NOT INITIAL. "Vendidos

      READ TABLE IT_ZFIT0100 INTO WA_ZFIT0100 WITH KEY EMPRESA     = WA_SAIDA-EMPRESA
                                                       LOTEAMENTO  = WA_SAIDA-LOTEAMENTO
                                                       NRO_QUADRA  = WA_SAIDA-NRO_QUADRA
                                                       NRO_TERRENO = WA_SAIDA-NRO_TERRENO.

      "Dados Venda
      WA_SAIDA-CLIENTE         = WA_ZFIT0100-CLIENTE.
      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_SAIDA-CLIENTE.
      WA_SAIDA-DESC_CLIENTE    = WA_KNA1-NAME1.

      READ TABLE IT_ZFIT0104 INTO WA_ZFIT0104 WITH KEY EMPRESA     = WA_SAIDA-EMPRESA
                                                       LOTEAMENTO  = WA_SAIDA-LOTEAMENTO
                                                       NRO_QUADRA  = WA_SAIDA-NRO_QUADRA
                                                       NRO_TERRENO = WA_SAIDA-NRO_TERRENO.

      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      WA_SAIDA-COND_PGTO       = WA_ZFIT0104-COND_PGTO.
      WA_SAIDA-FORMA_PGTO      = WA_ZFIT0104-FORMA_PGTO.
      WA_SAIDA-QTD_PARCELAS    = WA_ZFIT0104-QTD_PARCELAS.
      WA_SAIDA-DATA_VENC       = WA_ZFIT0104-DATA_VENC.

    ELSEIF R_ST1 IS NOT INITIAL. "Disponivel Venda

      READ TABLE IT_ZFIT0104 INTO WA_ZFIT0104 WITH KEY EMPRESA     = WA_SAIDA-EMPRESA
                                                       LOTEAMENTO  = WA_SAIDA-LOTEAMENTO
                                                       NRO_QUADRA  = WA_SAIDA-NRO_QUADRA
                                                       NRO_TERRENO = WA_SAIDA-NRO_TERRENO.

       IF SY-SUBRC EQ 0.
         CONTINUE.
       ENDIF.

    ENDIF.

    "Dados Inclusão
    WA_SAIDA-DATA_REGISTRO    = WA_ZFIT0099-DATA_REGISTRO.
    WA_SAIDA-HORA_REGISTRO    = WA_ZFIT0099-HORA_REGISTRO.
    WA_SAIDA-USUARIO_REGISTRO = WA_ZFIT0099-USUARIO_REGISTRO.

    APPEND WA_SAIDA TO IT_SAIDA.

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

  IF ( R_ST1 EQ 'X' ). "Layout para Disponivel para Venda
    PERFORM MONTAR_ESTRUTURA USING:

      1   ''  ''   'IT_SAIDA' 'LOTEAMENTO'       'Código'               '06' '',
      2   ''  ''   'IT_SAIDA' 'NOME_LOTEAMENTO'  'Loteamento'           '20' '',
      3   ''  ''   'IT_SAIDA' 'NRO_QUADRA'       'Quadra'               '06' '',
      4   ''  ''   'IT_SAIDA' 'NRO_TERRENO'      'Lote'                 '05' '',
      5   ''  ''   'IT_SAIDA' 'MATRICULA'        'Matricula'            '08' '',
      6   ''  ''   'IT_SAIDA' 'ENDERECO'         'Rua'                  '25' '',
      7   ''  ''   'IT_SAIDA' 'NUMERO'           'Nro.'                 '06' '',
      8   ''  ''   'IT_SAIDA' 'BAIRRO_LOCALIZ'   'Bairro'               '12' '',
      9   ''  ''   'IT_SAIDA' 'CEP'              'CEP'                  '09' '',
      10  ''  ''   'IT_SAIDA' 'CIDADE'           'Cidade'               '07' '',
      11  ''  ''   'IT_SAIDA' 'UF'               'UF'                   '03' '',
      12  ''  ''   'IT_SAIDA' 'VALOR'            'Vlr. Terreno R$'      '13' '',
      13  ''  ''   'IT_SAIDA' 'VALOR_AGREGADO'   'Vlr. Agregado R$'     '13' '',
      14  ''  ''   'IT_SAIDA' 'VALOR_TOTAL'      'Vlr. Total R$'        '13' '',
      15  ''  ''   'IT_SAIDA' 'MET_LADO_DIR'     'Metr. LD'             '08' '',
      16  ''  ''   'IT_SAIDA' 'MET_LADO_ESQ'     'Metr. LE'             '08' '',
      17  ''  ''   'IT_SAIDA' 'MET_FUNDOS'       'Metr. FD'             '08' '',
      18  ''  ''   'IT_SAIDA' 'MET_FRENTE'       'Metr. FT'             '08' '',
      19  ''  ''   'IT_SAIDA' 'MET_TOT_M2'       'Total m2'             '08' '',
      20  ''  ''   'IT_SAIDA' 'DATA_REGISTRO'    'Dt. Inclusão'         '13' '',
      21  ''  ''   'IT_SAIDA' 'HORA_REGISTRO'    'Hr. Inclusão'         '13' '',
      22  ''  ''   'IT_SAIDA' 'USUARIO_REGISTRO' 'Usuário'              '12' ''.

  ELSEIF ( R_ST2 EQ 'X' ). "Layout para Vendidos
    PERFORM MONTAR_ESTRUTURA USING:

      1   ''  ''   'IT_SAIDA' 'LOTEAMENTO'       'Código'               '06' '',
      2   ''  ''   'IT_SAIDA' 'NOME_LOTEAMENTO'  'Loteamento'           '20' '',
      3   ''  ''   'IT_SAIDA' 'NRO_QUADRA'       'Quadra'               '06' '',
      4   ''  ''   'IT_SAIDA' 'NRO_TERRENO'      'Lote'                 '05' '',
      5   ''  ''   'IT_SAIDA' 'MATRICULA'        'Matricula'            '08' '',
      6   ''  ''   'IT_SAIDA' 'ENDERECO'         'Rua'                  '25' '',
      7   ''  ''   'IT_SAIDA' 'NUMERO'           'Nro.'                 '06' '',
      8   ''  ''   'IT_SAIDA' 'BAIRRO_LOCALIZ'   'Bairro'               '12' '',
      9   ''  ''   'IT_SAIDA' 'CEP'              'CEP'                  '09' '',
      10  ''  ''   'IT_SAIDA' 'CIDADE'           'Cidade'               '07' '',
      11  ''  ''   'IT_SAIDA' 'UF'               'UF'                   '03' '',
      14  ''  ''   'IT_SAIDA' 'VALOR_TOTAL'      'Vlr. Total R$'        '13' '',
      15  ''  ''   'IT_SAIDA' 'DATA_VENDA'       'Data Venda'             '08' '',
      16  ''  ''   'IT_SAIDA' 'VALOR'            'Vlr Venda R$'             '08' '',
      17  ''  ''   'IT_SAIDA' 'CLIENTE'          'Cliente'             '08' '',
      17  ''  ''   'IT_SAIDA' 'DESC_CLIENTE'     'Nome Cliente'             '08' '',
      18  ''  ''   'IT_SAIDA' 'COND_PGTO'        'Cond. Pgto'             '08' '',
      19  ''  ''   'IT_SAIDA' 'FORMA_PGTO'       'Forma. Pgto'             '08' '',
      19  ''  ''   'IT_SAIDA' 'QTD_PARCELAS'    'Qtde. Parcelas'             '08' '',
      19  ''  ''   'IT_SAIDA' 'DATA_VENC'        'Dt. 1ª Parc.'             '08' '',
      20  ''  ''   'IT_SAIDA' 'DATA_REGISTRO'    'Dt. Inclusão'         '13' '',
      21  ''  ''   'IT_SAIDA' 'HORA_REGISTRO'    'Hr. Inclusão'         '13' '',
      22  ''  ''   'IT_SAIDA' 'USUARIO_REGISTRO' 'Usuário'              '12' ''.

  ENDIF.

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

  IF R_ST1 IS NOT INITIAL.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.
  ELSE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.
  ENDIF.

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
*&---------------------------------------------------------------------*
*&      Form  CONFIG_RANGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONFIG_RANGES .
  REFRESH: P_EMP,  P_LOTE, P_QUADRA,  P_TERR.

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




ENDFORM.                    " CONFIG_RANGES
