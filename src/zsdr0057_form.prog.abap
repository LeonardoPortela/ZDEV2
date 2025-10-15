*&---------------------------------------------------------------------*
*&  Include           ZSDR0057_FORM
*&---------------------------------------------------------------------*

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPAR_VARIAVEIS.

  "Entradas
  SELECT DT_MOVIMENTO WERKS LIFNR MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0125 INTO TABLE TG_0125_AGRP_MOV
   WHERE DT_MOVIMENTO    IN P_DT_MOV
     AND WERKS           IN P_WERKS
     AND LIFNR           IN P_LIFNR
     AND MATNR           IN P_MATNR
     AND LOEKZ           EQ ''
     AND DEVOLVIDA       EQ ''
  GROUP BY DT_MOVIMENTO WERKS LIFNR MATNR CD_CERTIFICACAO.

  "Saidas Certificadas
  SELECT DT_MOVIMENTO WERKS MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0126 INTO TABLE TG_0126_AGRP_MOV
   WHERE DT_MOVIMENTO     IN P_DT_MOV
     AND WERKS            IN P_WERKS
     AND MATNR            IN P_MATNR
     AND LOEKZ            EQ ''
   GROUP BY DT_MOVIMENTO WERKS MATNR CD_CERTIFICACAO.

  IF ( TG_0125_AGRP_MOV[] IS INITIAL ) AND
     ( TG_0126_AGRP_MOV[] IS INITIAL ).
    MESSAGE 'Nenhuma movimentação encontrada!' TYPE 'S'.
    EXIT.
  ENDIF.

*-------------------------------------------------------------------------*
*  Busca Dados para montagem Resumo
*-------------------------------------------------------------------------*

  "Buscar Saldos Iniciais  ---------------------------------------*

  "Entradas
  SELECT WERKS MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0125 INTO TABLE TG_0125_RES_INI
   WHERE DT_MOVIMENTO    < P_DT_MOV-LOW
     AND WERKS           IN P_WERKS
     AND LIFNR           IN P_LIFNR
     AND MATNR           IN P_MATNR
     AND LOEKZ           EQ ''
     AND DEVOLVIDA       EQ ''
  GROUP BY WERKS MATNR CD_CERTIFICACAO.

  "Saídas
  SELECT WERKS MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0126 INTO TABLE TG_0126_RES_INI
   WHERE DT_MOVIMENTO     < P_DT_MOV-LOW
     AND WERKS            IN P_WERKS
     AND MATNR            IN P_MATNR
     AND LOEKZ            EQ ''
  GROUP BY WERKS MATNR CD_CERTIFICACAO.

  "Buscar Resumo do Periodo --------------------------------------*

  "Entradas Periodo
  SELECT WERKS MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0125 INTO TABLE TG_0125_RES
   WHERE DT_MOVIMENTO    IN P_DT_MOV
     AND WERKS           IN P_WERKS
     AND LIFNR           IN P_LIFNR
     AND MATNR           IN P_MATNR
     AND LOEKZ           EQ ''
     AND DEVOLVIDA       EQ ''
  GROUP BY WERKS MATNR CD_CERTIFICACAO.

  "Saídas Periodo
  SELECT WERKS MATNR CD_CERTIFICACAO SUM( QTDE_CERT )
    FROM ZSDT0126 INTO TABLE TG_0126_RES
   WHERE DT_MOVIMENTO     IN P_DT_MOV
     AND WERKS            IN P_WERKS
     AND MATNR            IN P_MATNR
     AND LOEKZ           EQ ''
  GROUP BY WERKS MATNR CD_CERTIFICACAO.


  LOOP AT TG_0125_RES.
    CLEAR: TG_RES_AUX.
    MOVE-CORRESPONDING TG_0125_RES TO TG_RES_AUX.
    APPEND TG_RES_AUX.
  ENDLOOP.

  LOOP AT TG_0126_RES.
    CLEAR: TG_RES_AUX.
    MOVE-CORRESPONDING TG_0126_RES TO TG_RES_AUX.
    APPEND TG_RES_AUX.
  ENDLOOP.

  SORT TG_RES_AUX BY WERKS MATNR CD_CERTIFICACAO.
  DELETE ADJACENT DUPLICATES FROM TG_RES_AUX COMPARING WERKS MATNR CD_CERTIFICACAO.

  "Busca Fornecedor
  SELECT *
    FROM LFA1 INTO CORRESPONDING FIELDS OF TABLE TG_LFA1
    FOR ALL ENTRIES IN TG_0125_AGRP_MOV
   WHERE LIFNR EQ TG_0125_AGRP_MOV-LIFNR.

  "Busca Filial Entradas
  SELECT *
    FROM J_1BBRANCH APPENDING CORRESPONDING FIELDS OF TABLE TG_J_1BBRANCH
     FOR ALL ENTRIES IN TG_0125_AGRP_MOV
   WHERE BRANCH EQ TG_0125_AGRP_MOV-WERKS.

  "Busca Filial Saidas
  SELECT *
    FROM J_1BBRANCH APPENDING CORRESPONDING FIELDS OF TABLE TG_J_1BBRANCH
     FOR ALL ENTRIES IN TG_0126_AGRP_MOV
   WHERE BRANCH EQ TG_0126_AGRP_MOV-WERKS.

  "Busca Material Entradas
  SELECT *
    FROM MAKT APPENDING CORRESPONDING FIELDS OF TABLE TG_MAKT
     FOR ALL ENTRIES IN TG_0125_AGRP_MOV
   WHERE SPRAS = SY-LANGU
     AND MATNR EQ TG_0125_AGRP_MOV-MATNR.

  "Busca Material Saidas
  SELECT *
    FROM MAKT APPENDING CORRESPONDING FIELDS OF TABLE TG_MAKT
     FOR ALL ENTRIES IN TG_0126_AGRP_MOV
   WHERE SPRAS = SY-LANGU
     AND MATNR EQ TG_0126_AGRP_MOV-MATNR.

  "Busca Certificado Entradas
  SELECT *
    FROM ZSDT0124 APPENDING CORRESPONDING FIELDS OF TABLE TG_0124
     FOR ALL ENTRIES IN TG_0125_AGRP_MOV
   WHERE CD_CERTIFICACAO EQ TG_0125_AGRP_MOV-CD_CERTIFICACAO.

  "Busca Certificado Entradas
  SELECT *
    FROM ZSDT0124 APPENDING CORRESPONDING FIELDS OF TABLE TG_0124
     FOR ALL ENTRIES IN TG_0126_AGRP_MOV
   WHERE CD_CERTIFICACAO EQ TG_0126_AGRP_MOV-CD_CERTIFICACAO.


ENDFORM.

FORM F_PROCESSAR_DADOS.

  "SORT TG_0126 BY DT_MOVIMENTO WERKS.

*-------------------------------------------------------------------------------*
*  Movimentanção agrupada por dia.
*-------------------------------------------------------------------------------*

  "Entradas
  LOOP AT TG_0125_AGRP_MOV.
    CLEAR: WA_SAIDA_0100, TG_LFA1, TG_J_1BBRANCH, TG_MAKT, TG_0124.

    MOVE-CORRESPONDING TG_0125_AGRP_MOV TO WA_SAIDA_0100.

    WA_SAIDA_0100-TP_MOV = 'E'.
    WA_SAIDA_0100-IC_MOV = ICON_INCOMING_OBJECT.

    READ TABLE TG_LFA1 WITH KEY LIFNR = TG_0125_AGRP_MOV-LIFNR.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_FORN = TG_LFA1-NAME1.
    ENDIF.

    READ TABLE TG_J_1BBRANCH WITH KEY BRANCH = TG_0125_AGRP_MOV-WERKS.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_BRANCH = TG_J_1BBRANCH-NAME.
    ENDIF.

    READ TABLE TG_MAKT WITH KEY MATNR = TG_0125_AGRP_MOV-MATNR.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_MATERIAL = TG_MAKT-MAKTX.
    ENDIF.

    READ TABLE TG_0124 WITH KEY CD_CERTIFICACAO = TG_0125_AGRP_MOV-CD_CERTIFICACAO.
    IF SY-SUBRC = 0.
      CONCATENATE TG_0124-CD_CERTIFICACAO '-' TG_0124-DS_CERTIFICADO
             INTO WA_SAIDA_0100-DS_CERTIFICADO SEPARATED BY SPACE.
    ENDIF.

    WA_SAIDA_0100-QTDE_MOV = TG_0125_AGRP_MOV-QTDE_CERT.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

  "Saídas
  LOOP AT TG_0126_AGRP_MOV.
    CLEAR: WA_SAIDA_0100, TG_LFA1, TG_J_1BBRANCH, TG_MAKT, TG_0124.

    MOVE-CORRESPONDING TG_0126_AGRP_MOV TO WA_SAIDA_0100.

    WA_SAIDA_0100-TP_MOV = 'S'.
    WA_SAIDA_0100-IC_MOV = ICON_OUTGOING_OBJECT.

    READ TABLE TG_J_1BBRANCH WITH KEY BRANCH = TG_0126_AGRP_MOV-WERKS.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_BRANCH = TG_J_1BBRANCH-NAME.
    ENDIF.

    READ TABLE TG_MAKT WITH KEY MATNR = TG_0126_AGRP_MOV-MATNR.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_MATERIAL = TG_MAKT-MAKTX.
    ENDIF.

    READ TABLE TG_0124 WITH KEY CD_CERTIFICACAO = TG_0126_AGRP_MOV-CD_CERTIFICACAO.
    IF SY-SUBRC = 0.
      CONCATENATE TG_0124-CD_CERTIFICACAO '-' TG_0124-DS_CERTIFICADO
             INTO WA_SAIDA_0100-DS_CERTIFICADO SEPARATED BY SPACE.
    ENDIF.

    WA_SAIDA_0100-QTDE_MOV = TG_0126_AGRP_MOV-QTDE_CERT.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

  SORT IT_SAIDA_0100 BY DT_MOVIMENTO WERKS CD_CERTIFICACAO.

*-------------------------------------------------------------------------------*
*  Resumo Movimentanção por centro.
*-------------------------------------------------------------------------------*
  LOOP AT TG_RES_AUX.
    CLEAR: WA_SAIDA_0100_RES.

    WA_SAIDA_0100_RES-WERKS            = TG_RES_AUX-WERKS.

    READ TABLE TG_J_1BBRANCH WITH KEY BRANCH = TG_RES_AUX-WERKS.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100_RES-DS_BRANCH = TG_J_1BBRANCH-NAME.
    ENDIF.

    WA_SAIDA_0100_RES-MATNR            = TG_RES_AUX-MATNR.

    READ TABLE TG_MAKT WITH KEY MATNR = TG_RES_AUX-MATNR.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100_RES-DS_MATERIAL = TG_MAKT-MAKTX.
    ENDIF.

    WA_SAIDA_0100_RES-CD_CERTIFICACAO  = TG_RES_AUX-CD_CERTIFICACAO.

    READ TABLE TG_0124 WITH KEY CD_CERTIFICACAO = TG_RES_AUX-CD_CERTIFICACAO.
    IF SY-SUBRC = 0.
      CONCATENATE TG_0124-CD_CERTIFICACAO '-' TG_0124-DS_CERTIFICADO
             INTO WA_SAIDA_0100_RES-DS_CERTIFICADO SEPARATED BY SPACE.
    ENDIF.


    "Compor Saldo Inicial ------------------------------------*

    "Entradas
    READ TABLE TG_0125_RES_INI WITH KEY WERKS            = TG_RES_AUX-WERKS
                                        MATNR            = TG_RES_AUX-MATNR
                                        CD_CERTIFICACAO  = TG_RES_AUX-CD_CERTIFICACAO.

    IF ( SY-SUBRC = 0 ) AND ( TG_0125_RES_INI-QTDE_CERT NE 0 ).
      ADD TG_0125_RES_INI-QTDE_CERT TO WA_SAIDA_0100_RES-SLD_INICIAL.
    ENDIF.

    "Saidas
    READ TABLE TG_0126_RES_INI WITH KEY WERKS            = TG_RES_AUX-WERKS
                                        MATNR            = TG_RES_AUX-MATNR
                                        CD_CERTIFICACAO  = TG_RES_AUX-CD_CERTIFICACAO.

    IF ( SY-SUBRC = 0 ) AND ( TG_0126_RES_INI-QTDE_CERT NE 0 ).
      TG_0126_RES_INI-QTDE_CERT = TG_0126_RES_INI-QTDE_CERT * -1.
      ADD TG_0126_RES_INI-QTDE_CERT TO WA_SAIDA_0100_RES-SLD_INICIAL.
    ENDIF.

    "Compor Entradas ------------------------------------*

    READ TABLE TG_0125_RES WITH KEY WERKS            = TG_RES_AUX-WERKS
                                    MATNR            = TG_RES_AUX-MATNR
                                    CD_CERTIFICACAO  = TG_RES_AUX-CD_CERTIFICACAO.

    IF ( SY-SUBRC = 0 ) AND ( TG_0125_RES-QTDE_CERT NE 0 ).
      ADD TG_0125_RES-QTDE_CERT TO WA_SAIDA_0100_RES-QTDE_ENT.
    ENDIF.

    "Compor Saidas ------------------------------------*
    READ TABLE TG_0126_RES WITH KEY WERKS            = TG_RES_AUX-WERKS
                                    MATNR            = TG_RES_AUX-MATNR
                                    CD_CERTIFICACAO  = TG_RES_AUX-CD_CERTIFICACAO.

    IF ( SY-SUBRC = 0 ) AND ( TG_0126_RES-QTDE_CERT NE 0 ).
      ADD TG_0126_RES-QTDE_CERT TO WA_SAIDA_0100_RES-QTDE_SAI.
    ENDIF.


    "Compor Saldo Final ------------------------------------*
    WA_SAIDA_0100_RES-SLD_FINAL = ( WA_SAIDA_0100_RES-SLD_INICIAL + WA_SAIDA_0100_RES-QTDE_ENT ) -
                                   WA_SAIDA_0100_RES-QTDE_SAI.


    APPEND WA_SAIDA_0100_RES TO IT_SAIDA_0100_RES.

  ENDLOOP.



ENDFORM.


FORM F_LIMPAR_VARIAVEIS .

  CLEAR: IT_SAIDA_0100[],
         IT_SAIDA_0100_RES[],
         TG_0125_AGRP_MOV[],
         TG_0126_AGRP_MOV[],
         TG_0125_RES_INI[],
         TG_0126_RES_INI[],
         TG_RES_AUX[],
         TG_0125_RES[],
         TG_0126_RES[],
         TG_LFA1[],
         TG_J_1BBRANCH[],
         TG_MAKT[],
         TG_0124[].

ENDFORM.

FORM F_IMPRIMIR_DADOS.

  CALL SCREEN 0100.

*  PERFORM F_DEFINIR_EVENTOS.
*  PERFORM F_MONTAR_LAYOUT.
*
*  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM      = V_REPORT
*      IS_VARIANT              = GS_VARIANT_C
*      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*      IT_FIELDCAT             = IT_ESTRUTURA[]
*      IS_LAYOUT               = GS_LAYOUT
*      I_SAVE                  = 'X'
*      IT_EVENTS               = EVENTS
*      IS_PRINT                = T_PRINT
*    TABLES
*      T_OUTTAB                = IT_SAIDA_0100.

ENDFORM.

FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " DEFINIR_EVENTOS

FORM F_CARREGAR_EVENTOS USING NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM F_MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                              VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                              VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                              VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                              VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_OUTPUTLEN)
                              VALUE(P_HOTSPOT)
                              VALUE(P_JUST).

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
  WA_ESTRUTURA-JUST          = P_JUST.
  WA_ESTRUTURA-DDICTXT       = 'L'.
  WA_ESTRUTURA-OUTPUTLEN     = P_OUTPUTLEN.


  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO IT_ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_MONTAR_LAYOUT .

  REFRESH:  IT_ESTRUTURA[].

  PERFORM F_MONTAR_ESTRUTURA USING:


    01  ''            ''          'IT_SAIDA_0100' 'DT_MOVIMENTO'      'Dt.Movimento'             '12' ''    '',
    01  'T001W'       'WERKS'     'IT_SAIDA_0100' 'WERKS'             'Centro'                   '06' ''    '',
    01  'J_1BBRANCH'  'NAME'      'IT_SAIDA_0100' 'DS_BRANCH'         'Desc.Centro'              ''   ''    '',
    02  'LFA1'        'LIFNR'     'IT_SAIDA_0100' 'LIFNR'             'Fornecedor'               '10' ''    '',
    02  'LFA1'        'NAME1'     'IT_SAIDA_0100' 'DS_FORN'           'Desc.Fornecedor'          ''   ''    '',
    03  'MARA'        'MATNR'     'IT_SAIDA_0100' 'MATNR'             'Material'                 '10' ''    '',
    03  'MAKT'        'MAKTX'     'IT_SAIDA_0100' 'DS_MATERIAL'       'Desc.Material'            ''   ''    '',
    04  ''            ''          'IT_SAIDA_0100' 'SLD_INICIAL'       'Saldo Inicial'            '13' ''    '',
    05  ''            ''          'IT_SAIDA_0100' 'ENTRADAS'          'Entradas'                 '13' ''    '',
    06  ''            ''          'IT_SAIDA_0100' 'SAIDAS'            'Saidas'                   '13' ''    '',
    07  ''            ''          'IT_SAIDA_0100' 'SLD_FINAL'         'Saldo Final'              '13' ''    ''.


ENDFORM.                    " MONTAR_LAYOUT

FORM USER_COMMAND  USING R_UCOMM      LIKE SY-UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN: '&IC1'.
*      IF ( RS_SELFIELD-FIELDNAME EQ 'DOC_REM' ).
*
*        CLEAR: WA_SAIDA_0100.
*        READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX RS_SELFIELD-TABINDEX.
*        CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0100-DOC_REM IS NOT INITIAL ).
*        SET PARAMETER ID 'VL'  FIELD WA_SAIDA_0100-DOC_REM.
*        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*
*      ENDIF.
  ENDCASE.

ENDFORM.                    "user_command

FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE

FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM F_INICIAR_VARIAVEIS.

  DATA: WL_DT_MOV_LOW(50)  TYPE C,
        WL_DT_MOV_HIGH(50) TYPE C,
        WL_LAYOUT1(20) VALUE 'Centro:',
        WL_LAYOUT2(30) VALUE 'Data Movimento: ',
        WL_LAYOUT3(20) VALUE 'Material:',
        WL_DATA   VALUE '.',
        WL_SPACE  VALUE '-',
        WL_ATE(3) VALUE 'até',
        VL_DS_CENTRO  TYPE J_1BBRANCH-NAME.

  V_REPORT = SY-REPID.

  REFRESH: T_TOP.

  SELECT SINGLE NAME
    FROM J_1BBRANCH INTO VL_DS_CENTRO
   WHERE BRANCH IN P_WERKS.

  CONCATENATE WL_LAYOUT1 P_WERKS-LOW(4) WL_SPACE VL_DS_CENTRO  INTO WL_CENTRO SEPARATED BY SPACE.

  IF P_DT_MOV-HIGH IS NOT INITIAL.
    CONCATENATE P_DT_MOV-LOW+6(2) WL_DATA
                P_DT_MOV-LOW+4(2) WL_DATA
                P_DT_MOV-LOW(4)  INTO WL_DT_MOV_LOW.

    CONCATENATE P_DT_MOV-HIGH+6(2) WL_DATA
                P_DT_MOV-HIGH+4(2) WL_DATA
                P_DT_MOV-HIGH(4) INTO WL_DT_MOV_HIGH.

    CONCATENATE WL_LAYOUT2 WL_DT_MOV_LOW WL_ATE WL_DT_MOV_HIGH
           INTO WL_DT_MOV SEPARATED BY SPACE.
  ELSE.
    CONCATENATE P_DT_MOV-LOW+6(2) WL_DATA
                P_DT_MOV-LOW+4(2) WL_DATA
                P_DT_MOV-LOW(4) INTO WL_DT_MOV_LOW.

    CONCATENATE WL_LAYOUT2 WL_DT_MOV_LOW INTO WL_DT_MOV.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT        = P_MATNR-LOW
    IMPORTING
      OUTPUT       = P_MATNR-LOW.


  IF ( P_MATNR-HIGH IS NOT INITIAL ) AND
     ( P_MATNR-LOW  IS NOT INITIAL ).
    CONCATENATE WL_LAYOUT3 P_MATNR-LOW  WL_ATE P_MATNR-HIGH INTO WL_MATERIAL SEPARATED BY SPACE.
  ELSEIF P_MATNR-LOW IS NOT INITIAL.
    CONCATENATE WL_LAYOUT3 P_MATNR-LOW  INTO WL_MATERIAL SEPARATED BY SPACE. "#EC CI_FLDEXT_OK[2215424]
  ELSE.
    WL_MATERIAL = WL_LAYOUT3.
  ENDIF.


  "PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-008.

  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_DT_MOV.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_CENTRO.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_MATERIAL.


ENDFORM.
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
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.


ENDFORM.

FORM CRIAR_FIELD_CATALOG USING P_SCREEN.

  CLEAR: WA_FCAT, IT_FCAT, IT_FCAT[] .

  CASE P_SCREEN.
    WHEN '0100'.

      PERFORM ESTRUTURA_ALV USING:

        01 ''             ''                  'IT_SAIDA_0100' 'IC_MOV'         'E/S'                '04'  ''    '' ' ' 'C' ' ' ' ',
        02 ''             ''                  'IT_SAIDA_0100' 'DT_MOVIMENTO'   'Dt.Movimento'       '12'  ''    '' ' ' ' ' ' ' ' ',
        03 ''             ''                  'IT_SAIDA_0100' 'TP_MOV'         'Tp.Mov'             '06'  ''    '' ' ' 'C' ' ' ' ',
        04 'T001W'        'WERKS'             'IT_SAIDA_0100' 'WERKS'          'Centro'             '06'  ''    '' ' ' ' ' ' ' ' ',
        05 'J_1BBRANCH'   'NAME'              'IT_SAIDA_0100' 'DS_BRANCH'      'Desc.Centro'        '20'  ''    '' ' ' ' ' ' ' ' ',
        06 'LFA1'         'LIFNR'             'IT_SAIDA_0100' 'LIFNR'          'Fornecedor'         '10'  ''    '' ' ' ' ' ' ' ' ',
        07 'LFA1'         'NAME1'             'IT_SAIDA_0100' 'DS_FORN'        'Desc.Fornecedor'    '20'  ''    '' ' ' ' ' ' ' ' ',
        08 'MARA'         'MATNR'             'IT_SAIDA_0100' 'MATNR'          'Material'           '10'  ''    '' ' ' ' ' ' ' ' ',
        09 'MAKT'         'MAKTX'             'IT_SAIDA_0100' 'DS_MATERIAL'    'Desc.Material'      '20'  ''    '' ' ' ' ' ' ' ' ',
        10 ''             'DS_CERTIFICADO'    'IT_SAIDA_0100' 'DS_CERTIFICADO' 'Certificado'        ''    ''    '' ' ' ' ' ' ' ' ',
        11 ''             ''                  'IT_SAIDA_0100' 'QTDE_MOV'       'Qtde. Mov'          '13'  ''    '' ' ' ' ' 'X' ' '.

    WHEN '0100_RES'.

      PERFORM ESTRUTURA_ALV USING:

        01 'T001W'        'WERKS'             'IT_SAIDA_0100_RES' 'WERKS'          'Centro'             '06'  ''    '' ' ' ' ' ' ' ' ',
        02 'J_1BBRANCH'   'NAME'              'IT_SAIDA_0100_RES' 'DS_BRANCH'      'Desc.Centro'        '20'  ''    '' ' ' ' ' ' ' ' ',
        03 'MARA'         'MATNR'             'IT_SAIDA_0100_RES' 'MATNR'          'Material'           '10'  ''    '' ' ' ' ' ' ' ' ',
        04 'MAKT'         'MAKTX'             'IT_SAIDA_0100_RES' 'DS_MATERIAL'    'Desc.Material'      '20'  ''    '' ' ' ' ' ' ' ' ',
        05 ''             'DS_CERTIFICADO'    'IT_SAIDA_0100_RES' 'DS_CERTIFICADO' 'Certificado'        ''    ''    '' ' ' ' ' ' ' ' ',
        06 ''             ''                  'IT_SAIDA_0100_RES' 'SLD_INICIAL'    'Saldo Inicial'      '13'  ''    '' ' ' ' ' ' ' ' ',
        07 ''             ''                  'IT_SAIDA_0100_RES' 'QTDE_ENT'       'Entradas'           '13'  ''    '' ' ' ' ' 'X' ' ',
        08 ''             ''                  'IT_SAIDA_0100_RES' 'QTDE_SAI'       'Saidas'             '13'  ''    '' ' ' ' ' 'X' ' ',
        09 ''             ''                  'IT_SAIDA_0100_RES' 'SLD_FINAL'      'Saldo Final'        '13'  ''    '' ' ' ' ' ' ' ' '.

    WHEN '0101_E'.

      PERFORM ESTRUTURA_ALV USING:

        01 'ZSDT0125'     'OBJ_KEY'           'IT_SAIDA_0101' 'OBJ_KEY'           'Chav.Ref.'           '13'  ''    ''    ' ' ' ' ' ' ' ',
        "02 ''             ''                  'IT_SAIDA_0101' 'CD_CERTIFICACAO'   'Cd.Cert.'            '08'  ''    ''    ' ' ' ' ' ' ' ',
        03 'EKKO'         'EBELN'             'IT_SAIDA_0101' 'EBELN'             'Pedido'              '10'  ''    ''    ' ' ' ' ' ' ' ',
        04 ''             ''                  'IT_SAIDA_0101' 'EBELP'             'Itm.Ped.'            '08'  ''    ''    ' ' ' ' ' ' ' ',
        05 ''             ''                  'IT_SAIDA_0101' 'DT_MOVIMENTO'      'Dt.Mov.'             '10'  ''    ''    ' ' ' ' ' ' ' ',
        06 ''             ''                  'IT_SAIDA_0101' 'WERKS'             'Centro'              '06'  ''    ''    ' ' ' ' ' ' ' ',
        07 'LFA1'         'LIFNR'             'IT_SAIDA_0101' 'LIFNR'             'Fornecedor'          '10'  ''    ''    ' ' ' ' ' ' ' ',
        08 'MARA'         'MATNR'             'IT_SAIDA_0101' 'MATNR'             'Material'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        09 ''             ''                  'IT_SAIDA_0101' 'QTDE_MOV'          'Qtde.Origem'         '13'  ''    ''    ' ' ' ' ' ' ' ',
        10 ''             ''                  'IT_SAIDA_0101' 'QTDE_CERT'         'Qtde.Certificada'    '16'  ''    'X'   ' ' ' ' ' ' ' ',
        11 ''             ''                  'IT_SAIDA_0101' 'CENTRO_VIRTUAL'    'C.Virtual'           '10'  ''    ''    ' ' ' ' ' ' ' ',
        12 ''             ''                  'IT_SAIDA_0101' 'LGORT'             'Depósito'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        13 ''             ''                  'IT_SAIDA_0101' 'REF_DOC_NO'        'Ref.Doc.'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        14 ''             ''                  'IT_SAIDA_0101' 'MM_MBLNR'          'Doc.Material'        '10'  ''    ''    ' ' ' ' ' ' ' ',
        15 ''             ''                  'IT_SAIDA_0101' 'MM_MJAHR'          'Ano.Doc.'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        16 ''             ''                  'IT_SAIDA_0101' 'FT_BELNR'          'Doc.Miro'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        17 ''             ''                  'IT_SAIDA_0101' 'FT_GJAHR'          'Ano.Miro'            '10'  ''    ''    ' ' ' ' ' ' ' ',
        18 ''             ''                  'IT_SAIDA_0101' 'DOCNUM'            'Docnum'              '10'  ''    ''    ' ' ' ' 'X' ' ',
        19 ''             ''                  'IT_SAIDA_0101' 'NFENUM'            'Nota Fiscal'         '10'  ''    ''    ' ' ' ' ' ' ' ',
        20 ''             ''                  'IT_SAIDA_0101' 'SERIE'             'Série'               '06'  ''    ''    ' ' ' ' ' ' ' ',
        21 ''             ''                  'IT_SAIDA_0101' 'AV_VBELN'          'Av.Rec.'             '10'  ''    ''    ' ' ' ' ' ' ' ',
        22 ''             ''                  'IT_SAIDA_0101' 'CHARG'             'Safra'               '10'  ''    ''    ' ' ' ' ' ' ' ',
        23 ''             ''                  'IT_SAIDA_0101' 'DT_ATUAL'          'Dt. Registro'        '10'  ''    ''    ' ' ' ' ' ' ' ',
        23 ''             ''                  'IT_SAIDA_0101' 'HR_ATUAL'          'Hr. Registro'        '10'  ''    ''    ' ' ' ' ' ' ' '.

    WHEN '0101_S'.

      PERFORM ESTRUTURA_ALV USING:

        01 'LIKP'         'VBELN'             'IT_SAIDA_0101' 'DOC_REM'           'Remessa'             '10'  ''    ''      ' ' ' ' 'X' ' ',
        "02 ''             ''                  'IT_SAIDA_0101' 'CD_CERTIFICACAO'   'Cd.Cert.'            '08'  ''    ''      ' ' ' ' ' ' ' ',
        05 ''             ''                  'IT_SAIDA_0101' 'DT_MOVIMENTO'      'Dt.Mov.'             '10'  ''    ''      ' ' ' ' ' ' ' ',
        06 ''             ''                  'IT_SAIDA_0101' 'WERKS'             'Centro'              '10'  ''    ''      ' ' ' ' ' ' ' ',
        08 'MARA'         'MATNR'             'IT_SAIDA_0101' 'MATNR'             'Material'            '10'  ''    ''      ' ' ' ' ' ' ' ',
        09 ''             ''                  'IT_SAIDA_0101' 'QTDE_MOV'          'Qtde.Origem'         '13'  ''    ''      ' ' ' ' ' ' ' ',
        10 ''             ''                  'IT_SAIDA_0101' 'QTDE_CERT'         'Qtde.Certificada'    '16'  ''    'X'     ' ' ' ' ' ' ' ',
        10 ''             ''                  'IT_SAIDA_0101' 'KVGR3'             'Classif.'            '08'  ''    ''      ' ' 'C' ' ' ' ',
        18 ''             ''                  'IT_SAIDA_0101' 'DOCNUM'            'Docnum'              '10'  ''    ''      ' ' ' ' 'X' ' ',
        23 ''             ''                  'IT_SAIDA_0101' 'DT_ATUAL'          'Dt. Registro'        '10'  ''    ''      ' ' ' ' ' ' ' ',
        23 ''             ''                  'IT_SAIDA_0101' 'HR_ATUAL'          'Hr. Registro'        '10'  ''    ''      ' ' ' ' ' ' ' '.

  ENDCASE.

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
  WA_FCAT-DO_SUM      = P_SUM.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM HANDLE_HOTSPOT_CLICK  USING    P_SCREEN
                                    I_ROW_ID     TYPE LVC_S_ROW
                                    I_COLUMN_ID  TYPE LVC_S_COL
                                    IS_ROW_NO    TYPE LVC_S_ROID.

  DATA: OPT     TYPE CTU_PARAMS,
        VL_LOTE TYPE ZGLT034-LOTE.

  CASE P_SCREEN.
    WHEN '0100'.

      CLEAR: WA_SAIDA_0100.
      CASE I_COLUMN_ID.
        WHEN 'QTDE_MOV'.
          READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX I_ROW_ID.

          CHECK ( WA_SAIDA_0100-TP_MOV = 'E' ) OR ( WA_SAIDA_0100-TP_MOV = 'S' ).

          CLEAR: IT_SAIDA_0101[].

          CASE WA_SAIDA_0100-TP_MOV.
            WHEN 'E'.

              SELECT *
                FROM ZSDT0125 INTO TABLE TG_0125
               WHERE DT_MOVIMENTO    EQ WA_SAIDA_0100-DT_MOVIMENTO
                 AND WERKS           EQ WA_SAIDA_0100-WERKS
                 AND LIFNR           EQ WA_SAIDA_0100-LIFNR
                 AND MATNR           EQ WA_SAIDA_0100-MATNR
                 AND CD_CERTIFICACAO EQ WA_SAIDA_0100-CD_CERTIFICACAO
                 AND LOEKZ           EQ ''
                 AND DEVOLVIDA       EQ ''.

              LOOP AT TG_0125.
                CLEAR: WA_SAIDA_0101.
                MOVE-CORRESPONDING TG_0125 TO WA_SAIDA_0101.
                APPEND WA_SAIDA_0101 TO IT_SAIDA_0101.
              ENDLOOP.

            WHEN 'S'.

               SELECT *
                FROM ZSDT0126 INTO TABLE TG_0126
               WHERE DT_MOVIMENTO    EQ WA_SAIDA_0100-DT_MOVIMENTO
                 AND WERKS           EQ WA_SAIDA_0100-WERKS
                 AND MATNR           EQ WA_SAIDA_0100-MATNR
                 AND CD_CERTIFICACAO EQ WA_SAIDA_0100-CD_CERTIFICACAO
                 AND LOEKZ           EQ ''.

              LOOP AT TG_0126.
                CLEAR: WA_SAIDA_0101.
                MOVE-CORRESPONDING TG_0126 TO WA_SAIDA_0101.
                APPEND WA_SAIDA_0101 TO IT_SAIDA_0101.
              ENDLOOP.

          ENDCASE.

          VG_LAYOUT_0101 = WA_SAIDA_0100-TP_MOV.

          CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 170 25.

      ENDCASE.


    WHEN '0100_RES'.

      CLEAR: WA_SAIDA_0100.
      CASE I_COLUMN_ID.
        WHEN 'QTDE_ENT'.
          READ TABLE IT_SAIDA_0100_RES INTO WA_SAIDA_0100_RES INDEX I_ROW_ID.

          CHECK SY-SUBRC = 0.

          CLEAR: IT_SAIDA_0101[].

          SELECT *
            FROM ZSDT0125 INTO TABLE TG_0125
           WHERE WERKS           EQ WA_SAIDA_0100_RES-WERKS
             AND MATNR           EQ WA_SAIDA_0100_RES-MATNR
             AND CD_CERTIFICACAO EQ WA_SAIDA_0100_RES-CD_CERTIFICACAO
             AND LOEKZ           EQ ''
             AND DEVOLVIDA       EQ ''.

          LOOP AT TG_0125.
            CLEAR: WA_SAIDA_0101.
            MOVE-CORRESPONDING TG_0125 TO WA_SAIDA_0101.
            APPEND WA_SAIDA_0101 TO IT_SAIDA_0101.
          ENDLOOP.

          VG_LAYOUT_0101 = 'E'.
          CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 170 25.

        WHEN 'QTDE_SAI'.

          READ TABLE IT_SAIDA_0100_RES INTO WA_SAIDA_0100_RES INDEX I_ROW_ID.
          CHECK SY-SUBRC = 0.

          CLEAR: IT_SAIDA_0101[].

          SELECT *
            FROM ZSDT0126 INTO TABLE TG_0126
           WHERE WERKS           EQ WA_SAIDA_0100_RES-WERKS
             AND MATNR           EQ WA_SAIDA_0100_RES-MATNR
             AND CD_CERTIFICACAO EQ WA_SAIDA_0100_RES-CD_CERTIFICACAO
             AND LOEKZ           EQ ''.

          LOOP AT TG_0126.
            CLEAR: WA_SAIDA_0101.
            MOVE-CORRESPONDING TG_0126 TO WA_SAIDA_0101.
            APPEND WA_SAIDA_0101 TO IT_SAIDA_0101.
          ENDLOOP.

          VG_LAYOUT_0101 = 'S'.
          CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 170 25.

      ENDCASE.

    WHEN '0101'.

      CASE I_COLUMN_ID.
        WHEN 'DOC_REM'.
          READ TABLE IT_SAIDA_0101 INTO WA_SAIDA_0101 INDEX I_ROW_ID.

          CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0101-DOC_REM IS NOT INITIAL ).
          SET PARAMETER ID 'VL'  FIELD WA_SAIDA_0101-DOC_REM.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

        WHEN 'DOCNUM'.

          READ TABLE IT_SAIDA_0101 INTO WA_SAIDA_0101 INDEX I_ROW_ID.

          CHECK ( SY-SUBRC = 0 ) AND ( WA_SAIDA_0101-DOCNUM IS NOT INITIAL ).
          SET PARAMETER ID 'JEF'  FIELD WA_SAIDA_0101-DOCNUM.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      ENDCASE.

  ENDCASE.


ENDFORM.                    "HANDLE_HOTSPOT_CLICK

FORM F_PROCESSAR_ENTRADAS .

  DATA: IT_MOV_ESTQ    TYPE TABLE OF ZMMT_EE_ZGR,
        WA_MOV_ESTQ    TYPE ZMMT_EE_ZGR,
        IT_DOC_GERADOS TYPE TABLE OF ZMMT_EE_ZGR_DOCS,
        WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS.

  CHECK P_LIFNR-LOW IS NOT INITIAL.

  DELETE FROM ZSDT0125 WHERE DT_MOVIMENTO IN P_DT_MOV
                         AND MATNR        IN P_MATNR
                         AND LIFNR        EQ P_LIFNR-LOW.

  SELECT *
    FROM ZMMT_EE_ZGR AS A INTO TABLE IT_MOV_ESTQ
   WHERE ZRG_ATLZ      EQ '1'
     AND A~PSTNG_DATE  IN P_DT_MOV
     AND A~MATERIAL    IN P_MATNR
     AND A~TP_OPERACAO IN ('01','02') "Compra/ Devolução de Compra
     AND EXISTS ( SELECT P~LIFNR
                    FROM EKKO AS P
                   WHERE P~EBELN EQ A~PO_NUMBER
                     AND P~LIFNR EQ P_LIFNR-LOW )

     AND EXISTS ( SELECT B~OBJ_KEY
                    FROM ZMMT_EE_ZGR_DOCS AS B
                   WHERE B~OBJ_KEY   EQ A~OBJ_KEY
                     AND B~FT_BELNR  NE ''
                     AND B~MM_MBLNR  NE '' ).

  SELECT *
    FROM ZMMT_EE_ZGR_DOCS AS A INTO TABLE IT_DOC_GERADOS
     FOR ALL ENTRIES IN IT_MOV_ESTQ
   WHERE OBJ_KEY EQ IT_MOV_ESTQ-OBJ_KEY.

  SORT IT_MOV_ESTQ BY PSTNG_DATE.

  LOOP AT IT_MOV_ESTQ INTO WA_MOV_ESTQ.

    READ TABLE IT_DOC_GERADOS INTO WA_DOC_GERADOS WITH KEY OBJ_KEY = WA_MOV_ESTQ-OBJ_KEY.

    CHECK SY-SUBRC = 0.

    CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
       EXPORTING
         I_TP_MOV               = 'E'
         I_WA_MOV_ESTQ          = WA_MOV_ESTQ
         I_WA_DOC_GERADOS       = WA_DOC_GERADOS.

  ENDLOOP.

ENDFORM.
