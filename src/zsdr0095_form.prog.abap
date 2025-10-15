*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_FORM
*&---------------------------------------------------------------------*

FORM F_REFRESH_ALV USING P_ALV.

  CASE P_ALV.
    WHEN '0100'.
      CHECK OBJ_ALV_0100 IS NOT INITIAL.

      CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    WHEN '0110'.

  ENDCASE.

ENDFORM.

FORM F_REFRESH_OBJETOS .

  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.

FORM F_CRIAR_CATALOG USING P_SCREEN.

  FREE: WA_FCAT, IT_FCAT.

  CASE P_SCREEN.
    WHEN '0100'.

      DATA: ZLEST0153 TYPE ZLEST0153.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZSDT0170'             'ID_DUE'                        'IT_SAIDA_0100'  'ID_DUE'                      'Id.DU-e'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       01  'ZSDT0170'             'ID_NOMEACAO_TRAN'              'IT_SAIDA_0100'  'ID_NOMEACAO_TRAN'            'Id.Nomeação'               '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       01  'ZNOM_TRANSPORTE'      'DS_NOME_TRANSPOR'              'IT_SAIDA_0100'  'DS_NOME_TRANSPOR'            'Navio'                     '27'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0170'             'BUKRS'                         'IT_SAIDA_0100'  'BUKRS'                       'Empresa'                   '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0170'             'KUNNR'                         'IT_SAIDA_0100'  'KUNNR'                       'Cliente'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0170'             'PERFORMANCE'                   'IT_SAIDA_0100'  'PERFORMANCE'                 'Performance'               '11'   ' '    ''  ' ' 'C' ' ' ' ' 'X' ,
       "02  'ZSDT0170'             'LCTO_AVULSO'                   'IT_SAIDA_0100'  'LCTO_AVULSO'                 'Lcto.Avulso'               '11'   ' '    ''  ' ' 'C' ' ' ' ' 'X' ,
       02  'ZSDT0170'             'EMB_CONTAINER'                 'IT_SAIDA_0100'  'EMB_CONTAINER'               'Emb.Container'             '13'   ' '    ''  ' ' 'C' ' ' ' ' 'X' ,
       03  'ZSDT0170'             'NUMERO_DUE'                    'IT_SAIDA_0100'  'NUMERO_DUE'                  'Número DU-e'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0170'             'NUMERO_RUC'                    'IT_SAIDA_0100'  'NUMERO_RUC'                  'Número RUC'                '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0170'             'CHAVE_ACESSO'                  'IT_SAIDA_0100'  'CHAVE_ACESSO'                'Chave Acesso'              '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0170'             'CODIGO_URF_DESPACHO'           'IT_SAIDA_0100'  'CODIGO_URF_DESPACHO'         'URF Despacho'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZSDT0170'             'CODIGO_RA_DESPACHO'            'IT_SAIDA_0100'  'CODIGO_RA_DESPACHO'          'RA Despacho'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZSDT0170'             'TP_COD_LOCAL_DESPACHO'         'IT_SAIDA_0100'  'TP_COD_LOCAL_DESPACHO'       'Tp.Loc.Desp.'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZSDT0170'             'CNPJ_CPF_RESP_LOC_DESP'        'IT_SAIDA_0100'  'CNPJ_CPF_RESP_LOC_DESP'      'CNPJ/CPF Resp.Loc.Desp.'   '24'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZSDT0170'             'LOCAL_DESPACHO_LONGITUDE'      'IT_SAIDA_0100'  'LOCAL_DESPACHO_LONGITUDE'    'Loc.Desp.Longitude'        '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZSDT0170'             'LOCAL_DESPACHO_LATITUDE'       'IT_SAIDA_0100'  'LOCAL_DESPACHO_LATITUDE'     'Loc.Desp.Latitude'         '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZSDT0170'             'LOCAL_DESPACHO_END'            'IT_SAIDA_0100'  'LOCAL_DESPACHO_END'          'Loc.Desp.Endereço'         '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       12  'ZSDT0170'             'FORMA_EXPORTACAO'              'IT_SAIDA_0100'  'FORMA_EXPORTACAO'            'Forma Exportação'          '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  'ZSDT0170'             'CASO_ESPECIAL_TRANSPORTE'      'IT_SAIDA_0100'  'CASO_ESPECIAL_TRANSPORTE'    'Caso Esp.Transp.'          '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       14  'ZSDT0170'             'SITUACAO_ESPECIAL'             'IT_SAIDA_0100'  'SITUACAO_ESPECIAL'           'Situação Esp.'             '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       15  'ZSDT0170'             'OBSERVACOES_GERAIS'            'IT_SAIDA_0100'  'OBSERVACOES_GERAIS'          'Observ.Gerais'             '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       16  'ZSDT0170'             'MOEDA_CAMBIO'                  'IT_SAIDA_0100'  'MOEDA_CAMBIO'                'Moeda Neg.'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       17  'ZSDT0170'             'MOTIVO'                        'IT_SAIDA_0100'  'MOTIVO'                      'Motivo'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       18  'ZSDT0170'             'CNPJ_DECLARANTE'               'IT_SAIDA_0100'  'CNPJ_DECLARANTE'             'CNPJ Declarante'           '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       19  'ZSDT0170'             'CODIGO_URF_EMBARQUE'           'IT_SAIDA_0100'  'CODIGO_URF_EMBARQUE'         'URF Embarque'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       20  'ZSDT0170'             'CODIGO_RA_EMBARQUE'            'IT_SAIDA_0100'  'CODIGO_RA_EMBARQUE'          'RA Embarque'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       21  'ZSDT0170'             'TP_DUE'                        'IT_SAIDA_0100'  'TP_DUE'                      'Tp.DU-e'                   '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       22  'ZSDT0170'             'ID_DUE_REF'                    'IT_SAIDA_0100'  'ID_DUE_REF'                  'Id.DU-e.Ref.'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       23  'ZSDT0170'             'STATUS'                        'IT_SAIDA_0100'  'STATUS'                      'Status'                    '07'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       23  ''                     ''                              'IT_SAIDA_0100'  'DS_STATUS'                   'Ds.Status'                 '24'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       24  'ZSDT0170'             'SITUACAO_DUE'                  'IT_SAIDA_0100'  'SITUACAO_DUE'                'Sit.DU-e'                  '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       24  ''                     ''                              'IT_SAIDA_0100'  'DS_SITUACAO_DUE'             'Ds.Situação'               '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       25  'ZSDT0170'             'DT_SITUACAO'                   'IT_SAIDA_0100'  'DT_SITUACAO'                 'Dt.Situação'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       25  'ZSDT0170'             'HR_SITUACAO'                   'IT_SAIDA_0100'  'HR_SITUACAO'                 'Hr.Situação'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       26  'ZSDT0170'             'IND_BLOQUEIO'                  'IT_SAIDA_0100'  'IND_BLOQUEIO'                'Ind.Bloq.'                 '09'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       26  ''                     ''                              'IT_SAIDA_0100'  'DS_IND_BLOQUEIO'             'Ds.Ind.Bloq'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       27  'ZSDT0170'             'SITUACAO_CARGA'                'IT_SAIDA_0100'  'SITUACAO_CARGA'              'Sit.Carga'                 '09'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       27  ''                     ''                              'IT_SAIDA_0100'  'DS_SITUACAO_CARGA'           'Ds.Sit.Carga'              '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       28  'ZSDT0170'             'CONTROLE_ADM'                  'IT_SAIDA_0100'  'CONTROLE_ADM'                'Ctrl.Adm.'                 '09'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       28  ''                     ''                              'IT_SAIDA_0100'  'DS_CONTROLE_ADM'             'Ds.Ctrl.Adm.'              '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       29  'ZSDT0170'             'LIB_LEITURA_OPUS'              'IT_SAIDA_0100'  'LIB_LEITURA_OPUS'            'Lib.Leitura Comex'         '20'   ' '    ''  ' ' 'C' ' ' ' ' 'X',
       30  'ZSDT0170'             'LEITURA_OPUS'                  'IT_SAIDA_0100'  'LEITURA_OPUS'                'Leitura Comex'             '20'   ' '    ''  ' ' 'C' ' ' ' ' 'X',
       31  'ZSDT0170'             'DT_LEITURA_OPUS'               'IT_SAIDA_0100'  'DT_LEITURA_OPUS'             'Dt.Leitura'                '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       32  'ZSDT0170'             'HR_LEITURA_OPUS'               'IT_SAIDA_0100'  'HR_LEITURA_OPUS'             'Hr.Leitura'                '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       33  'ZSDT0170'             'DT_REGISTRO'                   'IT_SAIDA_0100'  'DT_REGISTRO'                 'Dt.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       34  'ZSDT0170'             'HR_REGISTRO'                   'IT_SAIDA_0100'  'HR_REGISTRO'                 'Hr.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       35  'ZSDT0170'             'US_REGISTRO'                   'IT_SAIDA_0100'  'US_REGISTRO'                 'Us.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       36  'ZSDT0170'             'DT_MODIFICACAO'                'IT_SAIDA_0100'  'DT_MODIFICACAO'              'Dt.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       37  'ZSDT0170'             'HR_MODIFICACAO'                'IT_SAIDA_0100'  'HR_MODIFICACAO'              'Hr.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       38  'ZSDT0170'             'US_MODIFICACAO'                'IT_SAIDA_0100'  'US_MODIFICACAO'              'Us.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       39  'ZSDT0170'             'DT_REGISTRO_PORTAL'            'IT_SAIDA_0100'  'DT_REGISTRO_PORTAL'          'Dt.Reg.Portal'             '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       40  'ZSDT0170'             'HR_REGISTRO_PORTAL'            'IT_SAIDA_0100'  'HR_REGISTRO_PORTAL'          'Hr.Reg.Portal'             '13'   ' '    ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0110'.

  ENDCASE.








ENDFORM.

FORM F_ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
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
                           VALUE(P_F4)
                           VALUE(P_CHECK).

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
  WA_FCAT-DO_SUM      = P_SUM.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.
  WA_FCAT-CHECKBOX    = P_CHECK.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM F_EXCLUDE_FCODE USING P_SCREEN.

  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO IT_EXCLUDE_FCODE.

ENDFORM.

FORM F_LIMPA_VARIAVEIS .

  CLEAR: WA_SAIDA_0100,
         IT_SAIDA_0100[],
         TG_T001[],
         TG_0170[],
         TG_ZNOM_TRANSPORTE[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZSDT0170 INTO TABLE TG_0170
   WHERE ID_DUE                    IN P_ID      "Indentificação
     AND BUKRS                     IN P_BUKRS
     AND ID_DUE_REF                IN P_IDREF
     AND ID_NOMEACAO_TRAN          IN P_IDNOM
     AND NUMERO_DUE                IN P_NDUE
     AND NUMERO_RUC                IN P_NRUC
     AND CNPJ_DECLARANTE           IN P_CNPJD
     "Local de Despacho
     AND TP_COD_LOCAL_DESPACHO     IN P_TPLD
     AND CODIGO_URF_DESPACHO       IN P_URFLD
     AND CODIGO_RA_DESPACHO        IN P_RALD
     AND CNPJ_CPF_RESP_LOC_DESP    IN P_CCRPL
     AND LOCAL_DESPACHO_END        IN P_LDEND
     "Informações Basicas
     AND FORMA_EXPORTACAO          IN P_FREXP
     AND CASO_ESPECIAL_TRANSPORTE  IN P_CETRP
     AND SITUACAO_ESPECIAL         IN P_STETRP
     AND MOEDA_CAMBIO              IN P_MOEDAN
     "Local Embarque
     AND CODIGO_URF_EMBARQUE       IN P_URFLE
     AND CODIGO_RA_EMBARQUE        IN P_RALE
     "Status
     AND SITUACAO_DUE              IN P_SITDUE
     AND DT_SITUACAO               IN P_DT_SIT
     AND IND_BLOQUEIO              IN P_INDBLO
     AND SITUACAO_CARGA            IN P_SITCAR
     AND CONTROLE_ADM              IN P_CTADM
     "Administração
     AND DT_REGISTRO               IN P_DTCR
     AND HR_REGISTRO               IN P_HRCR
     AND US_REGISTRO               IN P_USCR
     AND DT_MODIFICACAO            IN P_DTMOD
     AND HR_MODIFICACAO            IN P_HRMOD
     AND US_MODIFICACAO            IN P_USMOD
     AND LOEKZ                     EQ ABAP_FALSE.


  IF TG_0170[] IS NOT INITIAL.

    SELECT *
      FROM ZNOM_TRANSPORTE INTO TABLE TG_ZNOM_TRANSPORTE
       FOR ALL ENTRIES IN TG_0170
     WHERE ID_NOMEACAO_TRAN EQ TG_0170-ID_NOMEACAO_TRAN.

  ENDIF.



  SELECT *
    FROM T001 INTO TABLE TG_T001
   WHERE BUKRS IN P_BUKRS.

ENDFORM.

FORM F_PROCESSA_DADOS .

  SORT TG_0170 BY ID_DUE.

  LOOP AT TG_0170.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0170 TO WA_SAIDA_0100.

    IF TG_0170-ID_NOMEACAO_TRAN IS NOT INITIAL.
      READ TABLE TG_ZNOM_TRANSPORTE WITH KEY ID_NOMEACAO_TRAN = WA_SAIDA_0100-ID_NOMEACAO_TRAN.
      IF SY-SUBRC = 0.
        WA_SAIDA_0100-DS_NOME_TRANSPOR = TG_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR.
      ENDIF.
    ENDIF.

    PERFORM F_ATRIB_DS_DOMINIO USING 'ZDM_STATUS_DUE'
                                      WA_SAIDA_0100-STATUS
                             CHANGING WA_SAIDA_0100-DS_STATUS.

    PERFORM F_ATRIB_DS_DOMINIO USING 'ZDM_SITUACAO_DUE'
                                      WA_SAIDA_0100-SITUACAO_DUE
                             CHANGING WA_SAIDA_0100-DS_SITUACAO_DUE.

    PERFORM F_ATRIB_DS_DOMINIO USING 'ZDM_IND_BLOQ_DUE'
                                      WA_SAIDA_0100-IND_BLOQUEIO
                             CHANGING WA_SAIDA_0100-DS_IND_BLOQUEIO.

    PERFORM F_ATRIB_DS_DOMINIO USING 'ZDM_SITUACAO_CARGA'
                                      WA_SAIDA_0100-SITUACAO_CARGA
                             CHANGING WA_SAIDA_0100-DS_SITUACAO_CARGA.

    PERFORM F_ATRIB_DS_DOMINIO USING 'ZDM_CONTROLE_ADM'
                                      WA_SAIDA_0100-CONTROLE_ADM
                             CHANGING WA_SAIDA_0100-DS_CONTROLE_ADM.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.


ENDFORM.


FORM F_ATUALIZA_TIME.

  DATA: V_UTC         TYPE TZONREF-TZONE VALUE 'UTC',
        LV_DIFF       TYPE TIMEDURA.

  CONVERT DATE SY-DATUM TIME SY-UZEIT INTO TIME STAMP VG_TST_ATUAL TIME ZONE V_UTC.

  IF ( VG_TST_LIM > VG_TST_ATUAL ).
    VG_TST_DIF = VG_TST_LIM - VG_TST_ATUAL.

    CALL METHOD CL_FOEV_TIME_FUNC_BRF=>DIFFERENCE_TIMESTAMP
      EXPORTING
        IV_TIMESTAMP_1 = VG_TST_ATUAL
        IV_TIMESTAMP_2 = VG_TST_LIM
        IV_TIMEZONE_1  = 'CET'
        IV_TIMEZONE_2  = 'CET'
        IV_TIMEUNIT    = 'SECOND'
        IV_FAC_CAL     = 'CH'
      IMPORTING
        EV_DIFFERENCE  = LV_DIFF.

    VG_TIME_LIM = LV_DIFF.
  ENDIF.

ENDFORM.

FORM F_DEFINE_TIME_LIM.

  DATA: V_DATE TYPE ERDAT,
        V_TIME TYPE ERZET.

  DATA: V_UTC TYPE TZONREF-TZONE VALUE 'UTC'.

  V_DATE = SY-DATUM.
  V_TIME = SY-UZEIT + 3600.

  IF SY-UZEIT(2) = '23' AND V_TIME(02) = '00'.
    V_DATE = V_DATE + 1.
  ENDIF.

  CONVERT DATE V_DATE TIME V_TIME INTO TIME STAMP VG_TST_LIM TIME ZONE V_UTC.

ENDFORM.


FORM F_AUTENTICAR.

  DATA: V_TIME_ST_LIM_AUX TYPE TIMESTAMP,
        V_TIME_DIF        TYPE TIMESTAMP,
        V_TIME_FIM        TYPE TIMESTAMP,
        V_TIME_INI        TYPE TIMESTAMP,
        UTC               TYPE TZONREF-TZONE VALUE 'UTC'.

  IF LINES( TG_T001[] ) NE 1. "Se selecionou somente uma empresa, habilita envio Registro DU-e para Portal
    MESSAGE 'Para se autenticar, selecione somente uma Empresa!' TYPE 'I'.
    EXIT.
  ENDIF.

  CREATE OBJECT ZCL_TOKEN_SISCOMEX.

  ZCL_TOKEN_SISCOMEX->ZIF_CADASTRO~NOVO_REGISTRO( ).
  ZCL_TOKEN_SISCOMEX->SET_BUKRS( P_BUKRS-LOW ).
  ZCL_TOKEN_SISCOMEX->SET_ROLE_TYPE( 'IMPEXP' ). "Declarante importador/exportador

  ZCL_TOKEN_SISCOMEX->ZIF_CADASTRO~GRAVAR_REGISTRO( RECEIVING I_GRAVOU = DATA(_GRAVOU) ).

  CHECK _GRAVOU IS NOT INITIAL.

  VG_ST_LOGON = C_CONNECTED.

  PERFORM F_DEFINE_TIME_LIM.
  PERFORM F_ATUALIZA_TIME.

  GO_CLOCK->INTERVAL = VG_TIME_INTERVAL.
  CALL METHOD GO_CLOCK->RUN.

  LEAVE TO SCREEN 0100.


ENDFORM.

FORM F_LOGOUT .

  IF ZCL_TOKEN_SISCOMEX IS NOT INITIAL.
    FREE ZCL_TOKEN_SISCOMEX.
  ENDIF.

  VG_ST_LOGON = C_DISCONNECTED.

  CLEAR: VG_TIME_LIM.

  MESSAGE 'Desconectado com sucesso!' TYPE 'S'.

  LEAVE TO SCREEN 0100.

ENDFORM.

FORM F_VIEW_DUE .

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  IF IT_SEL_ROWS[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF LINES( IT_SEL_ROWS[] ) > 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

  CHECK SY-SUBRC = 0.

  READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

  CHECK ( SY-SUBRC = 0 ).

  FREE ZCL_DUE.
  CREATE OBJECT ZCL_DUE.

  CLEAR: WA_REGISTRO_DUE.
  WA_REGISTRO_DUE-ID_DUE = WA_SAIDA_0100-ID_DUE.
  WA_REGISTRO_DUE-MODO   = C_DUE_VIEW.

  ZCL_DUE->REGISTRO_DUE( WA_REGISTRO_DUE ).

  PERFORM F_REFRESH_ALV USING 0100.

ENDFORM.

FORM F_CHANGE_DUE .

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  IF IT_SEL_ROWS[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF LINES( IT_SEL_ROWS[] ) > 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

  CHECK SY-SUBRC = 0.

  READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

  CHECK ( SY-SUBRC = 0 ).

  FREE ZCL_DUE.
  CREATE OBJECT ZCL_DUE.

  CLEAR: WA_REGISTRO_DUE.
  WA_REGISTRO_DUE-ID_DUE = WA_SAIDA_0100-ID_DUE.
  WA_REGISTRO_DUE-MODO   = C_DUE_CHANGE.

  ZCL_DUE->REGISTRO_DUE( WA_REGISTRO_DUE ).

  PERFORM F_REFRESH_ALV USING 0100.


ENDFORM.

FORM F_TRANSMITIR_DUE .

  DATA: ZCL_DUE TYPE REF TO ZCL_DUE.

  IF ( VG_ST_LOGON = C_DISCONNECTED ) OR ( ZCL_TOKEN_SISCOMEX IS INITIAL ).
    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente Transmitir a(s) DU-e(s) selecionada(s)?'
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

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

    CHECK SY-SUBRC = 0.

    FREE ZCL_DUE.
    CREATE OBJECT ZCL_DUE
      EXPORTING
        I_ID_DUE = WA_SAIDA_0100-ID_DUE.

    ZCL_DUE->SET_TOKEN( ZCL_TOKEN_SISCOMEX ). "Set token para Validação.

    TRY .
      DATA(_ENVIADA) = ZCL_DUE->ENVIAR_DUE( ).
    CATCH ZCX_DUE INTO DATA(EX_DUE).
      CLEAR: _ENVIADA.
      EX_DUE->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
    ENDTRY.

    IF _ENVIADA IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE 'DU-e(s) Transmitida(s) com sucesso!' TYPE 'S'.


ENDFORM.


FORM F_CONSULTAR_STATUS.

  DATA: ZCL_DUE TYPE REF TO ZCL_DUE,
        ZCL_TOKEN_SISCOMEX_0001 TYPE REF TO ZCL_TOKEN_SISCOMEX.

*  IF ( VG_ST_LOGON = C_DISCONNECTED ) OR ( ZCL_TOKEN_SISCOMEX IS INITIAL ).
*    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
*    EXIT.
*  ENDIF.

  IF LINES( TG_T001[] ) NE 1. "
    MESSAGE 'Para consultar, selecione somente uma Empresa!' TYPE 'I'.
    EXIT.
  ENDIF.

  "Checar se Empresa esta parametrizada para consultar DU-es com certificado da Empresa 0001
  FREE ZCL_TOKEN_SISCOMEX_0001.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(_WL_SETLEAF_0001)
   WHERE SETNAME EQ 'DUE_CONSULTA_SIT_EMP0001'
     AND VALFROM EQ @P_BUKRS-LOW.

  IF SY-SUBRC EQ 0.
    CREATE OBJECT ZCL_TOKEN_SISCOMEX_0001.
    ZCL_TOKEN_SISCOMEX_0001->ZIF_CADASTRO~NOVO_REGISTRO( ).
    ZCL_TOKEN_SISCOMEX_0001->SET_BUKRS( '0001' ).
    ZCL_TOKEN_SISCOMEX_0001->SET_ROLE_TYPE( 'IMPEXP' ). "Declarante importador/exportador
    ZCL_TOKEN_SISCOMEX_0001->ZIF_CADASTRO~GRAVAR_REGISTRO( RECEIVING I_GRAVOU = DATA(_GRAVOU) ).
    CHECK _GRAVOU EQ ABAP_TRUE.
  ENDIF.

  IF ( ZCL_TOKEN_SISCOMEX IS INITIAL ) AND ( ZCL_TOKEN_SISCOMEX_0001 IS INITIAL ).
    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'Deseja realmente consultar o Situação da(s) DU-e(s) selecionada(s)?'
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

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

    CHECK SY-SUBRC = 0.

    FREE ZCL_DUE.
    CREATE OBJECT ZCL_DUE
      EXPORTING
        I_ID_DUE = WA_SAIDA_0100-ID_DUE.

    IF ZCL_TOKEN_SISCOMEX_0001 IS NOT INITIAL.
      ZCL_DUE->SET_TOKEN( ZCL_TOKEN_SISCOMEX_0001 ). "Set token para Validação.
    ELSE.
      ZCL_DUE->SET_TOKEN( ZCL_TOKEN_SISCOMEX ). "Set token para Validação.
    ENDIF.

    TRY .
      DATA(_ZSDT0170) = ZCL_DUE->CONSULTAR_DUE( EXPORTING I_ATUALIZA_REGISTRO = ABAP_TRUE ).
    CATCH ZCX_DUE INTO DATA(EX_DUE).
      CLEAR: _ZSDT0170.
      EX_DUE->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
    ENDTRY.

    "IF _ENVIADA IS INITIAL.
    "  RETURN.
    "ENDIF.

  ENDLOOP.

  IF _ZSDT0170 IS NOT INITIAL.
    MESSAGE 'DU-e(s) Consultada(s) com sucesso!' TYPE 'S'.
  ENDIF.

ENDFORM.


FORM F_EXCLUIR_DUE.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK IT_SEL_ROWS[] IS NOT INITIAL.

  IF LINES( IT_SEL_ROWS[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.

  CHECK SY-SUBRC EQ 0.

  READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

  CHECK SY-SUBRC = 0.

  FREE ZCL_DUE.
  CREATE OBJECT ZCL_DUE
    EXPORTING
      I_ID_DUE = WA_SAIDA_0100-ID_DUE.

  TRY.
    ZCL_DUE->ELIMINAR_REGISTRO( ).

    MESSAGE 'DU-e excluida com sucesso!' TYPE 'S'.
  CATCH ZCX_DUE INTO DATA(EX_DUE).
    EX_DUE->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
  ENDTRY.

ENDFORM.

FORM F_LANCAR_DUE .

  FREE ZCL_DUE.
  CREATE OBJECT ZCL_DUE.

  CLEAR: WA_REGISTRO_DUE.
  WA_REGISTRO_DUE-MODO                           = C_DUE_NOVO.  "Novo
  WA_REGISTRO_DUE-FORMA_EXPORTACAO               = '1001'.      "Forma Exportação/Por conta própria
  WA_REGISTRO_DUE-SITUACAO_ESPECIAL              = '2002'.      "Situação Especial/Embarque antecipado
  WA_REGISTRO_DUE-MOEDA_CAMBIO                   = 'USD'.       "Moeda Negociação
  WA_REGISTRO_DUE-TP_DUE                         = '1'.         "Sem NF-e
  WA_REGISTRO_DUE-TP_COD_LOCAL_DESPACHO          = '281'.       "Em Recinto Alfandegado
  WA_REGISTRO_DUE-FATURA_TP_CODIGO               = '388'.       "Nota Fiscal
  WA_REGISTRO_DUE-CODIGO_COND_VENDA              = 'FOB'.       "Código Condição de Venda
  WA_REGISTRO_DUE-CODIGO_ENQUADRAMENTO           = '80000'.     "Código Enquadramento
  WA_REGISTRO_DUE-FATURA_MOTIVO_DISPENSA_NF      = '3004'.      "NF/Embarque antecipado
  WA_REGISTRO_DUE-LCTO_AVULSO                    = ABAP_TRUE.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'DU-e é de Performance?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = VAR_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  IF VAR_ANSWER EQ '1'.
    WA_REGISTRO_DUE-PERFORMANCE = ABAP_TRUE.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmação'
      TEXT_QUESTION         = 'DU-e é com NF-e?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = VAR_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  IF VAR_ANSWER EQ '1'.
    WA_REGISTRO_DUE-TP_DUE   = '2'.         "Com NF-e
  ENDIF.

  ZCL_DUE->REGISTRO_DUE( EXPORTING I_REGISTRO_DUE = WA_REGISTRO_DUE ).


ENDFORM.

FORM F_DOWNLOAD_XML_DUE .

  DATA: ZCL_DUE TYPE REF TO ZCL_DUE.

*  IF ( VG_ST_LOGON = C_DISCONNECTED ) OR ( ZCL_TOKEN_SISCOMEX IS INITIAL ).
*    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
*    EXIT.
*  ENDIF.

  CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  CHECK LINES( IT_SEL_ROWS[] ) NE 0.

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.

    CHECK SY-SUBRC = 0.

    FREE ZCL_DUE.
    CREATE OBJECT ZCL_DUE
      EXPORTING
        I_ID_DUE = WA_SAIDA_0100-ID_DUE.

    ZCL_DUE->SET_TOKEN( ZCL_TOKEN_SISCOMEX ). "Set token para Validação.

    TRY .
      DATA(_XML) = ZCL_DUE->GET_XML_DUE( I_DOWNLOAD = ABAP_TRUE ).
    CATCH ZCX_DUE INTO DATA(EX_DUE).
      CLEAR: _XML.
      EX_DUE->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
    ENDTRY.

  ENDLOOP.

  MESSAGE 'Downloads realizados com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_executar .

    IF GO_CLOCK IS INITIAL.
      CREATE OBJECT: GO_CLOCK.
    ENDIF.

    IF GO_ALARM IS INITIAL.
      CREATE OBJECT: GO_ALARM.
      SET HANDLER GO_ALARM->ON_FINISHED FOR GO_CLOCK.
    ENDIF.

    PERFORM: F_SELECIONAR_DADOS,
             F_PROCESSA_DADOS.

   CALL SCREEN 0100.

ENDFORM.
