*&---------------------------------------------------------------------*
*&  Include           ZLESR0107_FORM
*&---------------------------------------------------------------------*

FORM F_GERAR_LINHAS.

  DATA(_DATA_AUX) = WA_CAB_0100-DT_INI.

  CLEAR: IT_SAIDA_0100[].

  IF WA_CAB_0100-BUKRS IS INITIAL.
    MESSAGE 'Informe a empresa!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-LIFNR IS INITIAL.
    MESSAGE 'Informe o fornecedor!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-DT_INI IS INITIAL.
    MESSAGE 'Informe a data inicial!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-DT_FIM IS INITIAL.
    MESSAGE 'Informe a data final!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-DT_INI > WA_CAB_0100-DT_FIM .
    MESSAGE 'Data inicial não pode ser maior que data final!' TYPE 'S'.
    EXIT.
  ENDIF.

*  IF WA_CAB_0100-VALOR IS INITIAL .
*    MESSAGE 'Informe o Valor do Adiantamento!' TYPE 'S'.
*    EXIT.
*  ENDIF.

  WHILE _DATA_AUX <= WA_CAB_0100-DT_FIM.
    CLEAR: WA_SAIDA_0100.

    WA_SAIDA_0100-NOVO_REG   = 'X'.
    WA_SAIDA_0100-BUKRS      = WA_CAB_0100-BUKRS.
    WA_SAIDA_0100-LIFNR      = WA_CAB_0100-LIFNR.
    WA_SAIDA_0100-DATA       = _DATA_AUX.
    WA_SAIDA_0100-VALOR      = WA_CAB_0100-VALOR.
    WA_SAIDA_0100-OBSERV     = WA_CAB_0100-OBSERV.
    WA_SAIDA_0100-ANEXO      = '@1F@'.
    WA_SAIDA_0100-ANEXO_G    = '@1F@'.
    "WA_SAIDA_0100-ANEXO_LINK = WA_CAB_0100-ANEXO_LINK.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_SAIDA_0100-LIFNR
      IMPORTING
        OUTPUT = WA_SAIDA_0100-LIFNR.

    PERFORM F_DEFINE_COLOR_SAIDA CHANGING WA_SAIDA_0100.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.
    ADD 1 TO _DATA_AUX.

  ENDWHILE.

ENDFORM.

FORM F_REFRESH_OBJETOS .

  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.

FORM F_CRIAR_CATALOG USING P_SCREEN.

  DATA: V_EDIT TYPE C.

  FREE: WA_FCAT, IT_FCAT.

  CASE P_SCREEN.
    WHEN '0100'.

      V_EDIT = ''.

      IF VG_OPERACAO = C_EDIT.
        V_EDIT = 'X'.
      ENDIF.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZLEST0140'      'DATA'        'IT_SAIDA_0100' 'DATA'               'Data'                '12'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZLEST0140'      'VALOR'       'IT_SAIDA_0100' 'VALOR'              'Valor Adto.'         '13'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZLEST0140'      'PERC_GRAOS'  'IT_SAIDA_0100' 'PERC_GRAOS'         '% Grãos'             '10'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZLEST0140'      'PERC_GRAOS'  'IT_SAIDA_0100' 'PERC_GRAOS_REAL'    '% Grãos Real.'       '13'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZLEST0140'      'VALOR'       'IT_SAIDA_0100' 'VL_GRAOS_REAL'      'Vlr.Grãos Real.'     '15'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZLEST0140'      'PERC_FERT'   'IT_SAIDA_0100' 'PERC_FERT'          '% Fert.'             '10'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZLEST0140'      'PERC_FERT'   'IT_SAIDA_0100' 'PERC_FERT_REAL'     '% Fert.Real.'        '13'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZLEST0140'      'VALOR'       'IT_SAIDA_0100' 'VL_FERT_REAL'       'Vlr.Fert.Real.'      '15'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZLEST0140'      'PERC_DEF'    'IT_SAIDA_0100' 'PERC_DEF'           '% Defens.'           '10'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZLEST0140'      'PERC_DEF'    'IT_SAIDA_0100' 'PERC_DEF_REAL'      '% Def.Real.'         '13'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       12  'ZLEST0140'      'VALOR'       'IT_SAIDA_0100' 'VL_DEF_REAL'        'Vlr.Def.Real.'       '15'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       13  'ZLEST0140'      'PERC_SEM'    'IT_SAIDA_0100' 'PERC_SEM'           '% Seme.'             '10'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       14  'ZLEST0140'      'PERC_SEM'    'IT_SAIDA_0100' 'PERC_SEM_REAL'      '% Seme.Real.'        '13'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       15  'ZLEST0140'      'VALOR'       'IT_SAIDA_0100' 'VL_SEM_REAL'        'Vlr.Seme.Real.'      '15'   ''         ''  ' ' ' ' ' ' ' ' '' ,
       16  'ZLEST0140'      'OBSERV'      'IT_SAIDA_0100' 'OBSERV'             'Observação'          '60'   V_EDIT     ''  ' ' ' ' ' ' ' ' '' ,
       17  ''               ''            'IT_SAIDA_0100' 'ANEXO'              'Anexo Ind.'          '12'   ' '        ''  ' ' 'C' 'X' ' ' '' ,
       18  ''               ''            'IT_SAIDA_0100' 'ANEXO_G'            'Anexo Geral'         '12'   ' '        ''  ' ' 'C' 'X' ' ' '' .

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

FORM F_SALVAR_REG .

  DATA: V_DATA_OUT TYPE C LENGTH 15.

  DATA(_REG_SAVE) = ''.
  DATA(_ERROR) = ''.
  FREE: IT_EDITOR.
  clear: WA_EDITOR.

  LOOP AT IT_SAIDA_0100 INTO WA_SAIDA_0100.

    _REG_SAVE = 'X'.

    CLEAR: ZLEST0140.

    MOVE-CORRESPONDING WA_SAIDA_0100 TO ZLEST0140.

    IF ( ZLEST0140-BUKRS IS INITIAL ) OR ( ZLEST0140-DATA IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE | Empresa/Data são campos obrigatórios!| TYPE 'I'.
      _ERROR = ABAP_TRUE.
      EXIT.
    ENDIF.

    V_DATA_OUT = ZLEST0140-DATA+6(2) && '/' && ZLEST0140-DATA+4(2) && '/' && ZLEST0140-DATA(4).

    SELECT SINGLE *
      FROM ZLEST0141 INTO @DATA(_WL_0141_CTB)
     WHERE BUKRS EQ @ZLEST0140-BUKRS
       AND DATA  EQ @ZLEST0140-DATA.

    IF SY-SUBRC EQ 0.
      IF ( _WL_0141_CTB-LOTE IS NOT INITIAL ) OR ( _WL_0141_CTB-SEM_SLD_PGTO IS NOT INITIAL ).
        ROLLBACK WORK.
        MESSAGE | Processamento já realizado na data: { V_DATA_OUT } | TYPE 'I'.
        _ERROR = ABAP_TRUE.
        EXIT.
      ENDIF.
    ENDIF.

    IF ( ( ZLEST0140-PERC_GRAOS +
           ZLEST0140-PERC_FERT  +
           ZLEST0140-PERC_DEF   +
           ZLEST0140-PERC_SEM   ) NE 100 ) AND ZLEST0140-VALOR NE 0.
      ROLLBACK WORK.
      MESSAGE | Empresa: { ZLEST0140-BUKRS } Data: { V_DATA_OUT }, não totaliza 100% nos percentuais de Grãos/Fertilizantes/Defensivos/Sementes!| TYPE 'I'.
      _ERROR = ABAP_TRUE.
      EXIT.
    ENDIF.

    "Para dias não uteis, checar se já houve processamento no ultimo dia anterior Útil.
    DATA(_PROC) = ABAP_FALSE.
    PERFORM F_CHECK_PROC_DIA_ANTERIOR_UTIL USING ZLEST0140-BUKRS
                                                 ZLEST0140-DATA
                                        CHANGING _PROC.
    IF _PROC EQ ABAP_TRUE.
      ROLLBACK WORK.
      _ERROR = ABAP_TRUE.
      EXIT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ZLEST0140-LIFNR
      IMPORTING
        OUTPUT = ZLEST0140-LIFNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = ZLEST0140-BUKRS
      IMPORTING
        OUTPUT = ZLEST0140-BUKRS.

    ZLEST0140-DATA_REG = SY-DATUM.
    ZLEST0140-HORA_REG = SY-UZEIT.
    ZLEST0140-USUARIO  = SY-UNAME.

***    Pegar o valor da saida e verificar se houve alteração valor adiantamento com base na data e empresa.
    READ TABLE T_TEMP ASSIGNING FIELD-SYMBOL(<W_TEMP>) WITH KEY BUKRS = WA_SAIDA_0100-BUKRS
                                                                DATA  = WA_SAIDA_0100-DATA.

    IF SY-SUBRC EQ 0.
**    Verificar se o valor foi alterado.
      IF WA_SAIDA_0100-VALOR NE <W_TEMP>-VALOR.
**      Verifica se o prazo esta dentro da 72 horas.
        CLEAR: DESVIO.
        DESVIO = ( <W_TEMP>-DATA - SY-DATUM ).
        IF DESVIO IS NOT INITIAL.
          CONDENSE DESVIO.
          DESVIO = ( DESVIO * 24 ).
          CONDENSE DESVIO.

***        Verifica se o valor e menor que 72 horas.
          IF DESVIO =< 72.


            W_ZLEST0140 = VALUE #( BUKRS = WA_SAIDA_0100-BUKRS
                                   DATA  = WA_SAIDA_0100-DATA
                                   VALOR_DE = <W_TEMP>-VALOR
                                   VALOR_PARA = WA_SAIDA_0100-VALOR
                                    ).

            TXTOPEN = ABAP_TRUE.
            CALL SCREEN 0300 STARTING AT 05 15 .

            CLEAR WA_EDITOR.
            CALL METHOD EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = IT_EDITOR ).
            IF IT_EDITOR IS INITIAL.
*              MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
              _ERROR = ABAP_TRUE.
              CONTINUE.
            ELSE.

              CLEAR: W_ZLEST0140-OBS.
              LOOP AT IT_EDITOR INTO WA_EDITOR.
                W_ZLEST0140-OBS = |{ W_ZLEST0140-OBS } { WA_EDITOR-LINE }|.
              ENDLOOP.
            ENDIF.

***            Adicionar a justificativa da alteração.
*            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
*              EXPORTING
*                IM_TITLE = 'Observação'
*                IM_DISPLAY_MODE = ' '
*                IM_START_COLUMN = ' '
*                IM_START_ROW    = ' '
*              CHANGING
*                CH_TEXT  = TL_TEXTO.
*
*            IF SY-UCOMM NE 'CX_CONT'.
*              FREE: TL_TEXTO.
*            ENDIF.
*
*            LOOP AT TL_TEXTO INTO DATA(WA).
*              VL_OBSERVACAO = |{ VL_OBSERVACAO } { WA }|.
*            ENDLOOP.
*
*            DATA(TAMANHO_STRING) = STRLEN( VL_OBSERVACAO ).
*
*            IF TAMANHO_STRING >= 200.
*              MESSAGE |Quantidade de caracteres { TAMANHO_STRING } Ultrapassa o Valor Permitido "200"!| TYPE 'I'.
*              EXIT.
*            ENDIF.
*
*            IF VL_OBSERVACAO IS INITIAL.
*              MESSAGE TEXT-001 TYPE 'E' DISPLAY LIKE 'I'.
*            ENDIF.
*
            W_LOG = VALUE #( BUKRS      = WA_SAIDA_0100-BUKRS
                            DATA_MODIF  = SY-DATUM
                            HORA_MODIF  = SY-UZEIT
                            USUARIO     = SY-UNAME
                            DATA        = WA_SAIDA_0100-DATA
                            VALOR_ANT   = <W_TEMP>-VALOR
                            VALOR_ATUAL = WA_SAIDA_0100-VALOR
                            TIPO_OPER   = 'UPDATE'
                            MOTIVO = W_ZLEST0140-OBS ).
*
            APPEND W_LOG TO T_LOG.
*            CLEAR TL_TEXTO.

            MODIFY ZLEST0198 FROM W_LOG.
            COMMIT WORK.
            CLEAR: W_LOG, W_ZLEST0140.
          ENDIF.
        ENDIF.
      ENDIF.


    ENDIF.


    MODIFY ZLEST0140 FROM ZLEST0140.

  ENDLOOP.

  IF ( _REG_SAVE IS NOT INITIAL ) AND ( _ERROR IS INITIAL ).
    COMMIT WORK.
    MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.

    IF ( OBJ_ALV_0100 IS NOT INITIAL ) AND ( VG_OPERACAO NE C_VIEW ).
      CALL METHOD OBJ_ALV_0100->FREE.
      CALL METHOD CL_GUI_CFW=>FLUSH.
      FREE: OBJ_ALV_0100.
    ENDIF.

    VG_OPERACAO = C_VIEW.

    PERFORM: F_SELECIONAR_DADOS,
             F_PROCESSAR_DADOS.
    LEAVE TO SCREEN 0100.
  ELSE.

    MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'.
    CLEAR: _ERROR.
  ENDIF.


ENDFORM.

FORM F_SELECIONAR_DADOS .

  CLEAR: IT_SAIDA_0100[], TG_ZLEST0140[], TG_ZLEST0141_RES[].

  IF WA_CAB_0100-BUKRS IS INITIAL.
    MESSAGE 'Informe a empresa!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-LIFNR IS INITIAL.
    MESSAGE 'Informe o fornecedor!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-DT_INI IS INITIAL.
    MESSAGE 'Informe a data inicial!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF WA_CAB_0100-DT_FIM IS INITIAL.
    WA_CAB_0100-DT_FIM = WA_CAB_0100-DT_INI.
  ENDIF.

  SELECT *
    FROM ZLEST0140 INTO CORRESPONDING FIELDS OF TABLE TG_ZLEST0140
   WHERE BUKRS   = WA_CAB_0100-BUKRS
     AND DATA    >= WA_CAB_0100-DT_INI
     AND DATA    <= WA_CAB_0100-DT_FIM.

  IF TG_ZLEST0140[] IS NOT INITIAL.
    SELECT *
      FROM ZLEST0141_RESUMO INTO TABLE TG_ZLEST0141_RES
       FOR ALL ENTRIES IN TG_ZLEST0140
     WHERE BUKRS = TG_ZLEST0140-BUKRS
       AND DATA  = TG_ZLEST0140-DATA.

    DATA(_DT_PROX_DIA) = WA_CAB_0100-DT_INI.
    IF WA_CAB_0100-DT_FIM IS NOT INITIAL.
      _DT_PROX_DIA = WA_CAB_0100-DT_FIM.
    ENDIF.

    ADD 1 TO _DT_PROX_DIA.

    SELECT *
      FROM ZLEST0141_RESUMO APPENDING TABLE TG_ZLEST0141_RES
     WHERE BUKRS = WA_CAB_0100-BUKRS
       AND DATA  = _DT_PROX_DIA.
  ENDIF.

*  SELECT *
*   FROM SRGBTBREL INTO TABLE TG_ANEXOS
*    FOR ALL ENTRIES IN TG_080
*  WHERE RELTYPE  EQ 'ATTA'
*    AND INSTID_A = TG_080-INSTID_A
*    AND TYPEID_A EQ 'ZLES0144'.


ENDFORM.

FORM F_PROCESSAR_DADOS.

  DATA: VL_OBJ_KEY    TYPE SIBFLPORB-INSTID,
        ANEXOS        TYPE TABLE OF BDN_CON,
        V_DT_PROX_DIA TYPE SY-DATUM.

  LOOP AT TG_ZLEST0140.
    CLEAR: WA_SAIDA_0100, ANEXOS.
    MOVE-CORRESPONDING TG_ZLEST0140 TO WA_SAIDA_0100.

    "Anexos

    "Individual
    CONCATENATE WA_SAIDA_0100-BUKRS WA_SAIDA_0100-LIFNR WA_SAIDA_0100-DATA
           INTO VL_OBJ_KEY.

    PERFORM F_GET_ANEXOS TABLES ANEXOS
                          USING VL_OBJ_KEY
                                'I'. "Geral.

    IF LINES( ANEXOS[] ) > 0.
      WA_SAIDA_0100-ANEXO_LINK = VL_OBJ_KEY.
      WA_SAIDA_0100-ANEXO      = '@1E@'.
    ELSE.
      WA_SAIDA_0100-ANEXO_LINK = VL_OBJ_KEY.
      WA_SAIDA_0100-ANEXO      = '@1F@'.
    ENDIF.

    "Geral
    PERFORM F_GET_ANEXOS TABLES ANEXOS
                          USING WA_SAIDA_0100-ANEXO_REF
                                'G'. "Geral.

    IF LINES( ANEXOS[] ) > 0.
      WA_SAIDA_0100-ANEXO_LINK_G = WA_SAIDA_0100-ANEXO_REF.
      WA_SAIDA_0100-ANEXO_G      = '@1E@'.
    ELSE.
      WA_SAIDA_0100-ANEXO_LINK_G = WA_SAIDA_0100-ANEXO_REF.
      WA_SAIDA_0100-ANEXO_G      = '@1F@'.
    ENDIF.

    V_DT_PROX_DIA = TG_ZLEST0140-DATA + 1.

    LOOP AT TG_ZLEST0141_RES WHERE BUKRS = TG_ZLEST0140-BUKRS
                               AND DATA  = V_DT_PROX_DIA.

      CASE TG_ZLEST0141_RES-MATKL.
        WHEN '658430' OR '658440' OR '700150'. "Fertilizantes

          ADD TG_ZLEST0141_RES-VLR_MOV TO WA_SAIDA_0100-VL_FERT_REAL.

        WHEN '658445'. "Defensivos

          ADD TG_ZLEST0141_RES-VLR_MOV TO WA_SAIDA_0100-VL_DEF_REAL.

        WHEN '700230' OR '700240' OR '658435'. "Sementes

          ADD TG_ZLEST0141_RES-VLR_MOV TO WA_SAIDA_0100-VL_SEM_REAL.

        WHEN OTHERS.

          ADD TG_ZLEST0141_RES-VLR_MOV TO WA_SAIDA_0100-VL_GRAOS_REAL.

      ENDCASE.
    ENDLOOP.

*--------------------------------------------------------------------*
*  Definir Percentuais Realizados
*--------------------------------------------------------------------*

*   IF WA_SAIDA_0100-VALOR > 0.
    IF WA_SAIDA_0100-VALOR > '0.02'.
      "Fertilizantes
      WA_SAIDA_0100-PERC_FERT_REAL  = ( WA_SAIDA_0100-VL_FERT_REAL * 100 ) / WA_SAIDA_0100-VALOR.

      "Defensivos
      WA_SAIDA_0100-PERC_DEF_REAL   = ( WA_SAIDA_0100-VL_DEF_REAL * 100 ) / WA_SAIDA_0100-VALOR.

      "Sementes
      WA_SAIDA_0100-PERC_SEM_REAL   = ( WA_SAIDA_0100-VL_SEM_REAL * 100 ) / WA_SAIDA_0100-VALOR.

      "Grãos
      WA_SAIDA_0100-PERC_GRAOS_REAL = ( WA_SAIDA_0100-VL_GRAOS_REAL * 100 ) / WA_SAIDA_0100-VALOR.
    ENDIF.

*--------------------------------------------------------------------*
*  Definir Colors
*--------------------------------------------------------------------*
    PERFORM F_DEFINE_COLOR_SAIDA CHANGING WA_SAIDA_0100.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.
  ENDLOOP.

ENDFORM.

FORM F_GET_ANEXOS TABLES P_ANEXOS STRUCTURE BDN_CON
                   USING P_OBJ_KEY
                         P_TIPO.

  DATA: V_CLASS TYPE BAPIBDS01-CLASSNAME.
  CLEAR: P_ANEXOS[].

  IF P_TIPO = 'G'.
    V_CLASS = 'ZLES0144-G'.
  ELSE.
    V_CLASS = 'ZLES0144'.
  ENDIF.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      CLASSNAME          = V_CLASS
      OBJKEY             = P_OBJ_KEY
      CLIENT             = SY-MANDT
    TABLES
      GOS_CONNECTIONS    = P_ANEXOS
    EXCEPTIONS
      NO_OBJECTS_FOUND   = 1
      INTERNAL_ERROR     = 2
      INTERNAL_GOS_ERROR = 3
      OTHERS             = 4.

ENDFORM.

FORM F_CRIAR_ANEXO USING P_OBJ_KEY TYPE SIBFLPORB-INSTID
                         P_TIPO    TYPE C.

  DATA: VL_LINES      TYPE I,
        ANEXOS        TYPE TABLE OF BDN_CON,
        VL_IP_MODE    TYPE SGS_RWMOD,
        VL_IP_SERVICE TYPE SGS_SRVNAM,
        WA_BOR        TYPE BORIDENT,
        ANEXO_OBJ     TYPE REF TO CL_GOS_MANAGER.

  CREATE OBJECT ANEXO_OBJ TYPE CL_GOS_MANAGER.

  VL_IP_MODE     = 'E'.
  VL_IP_SERVICE  = 'PCATTA_CREA'.
  WA_BOR-OBJKEY  = P_OBJ_KEY.

  IF P_TIPO = 'G'.
    WA_BOR-OBJTYPE = 'ZLES0144-G'.
  ELSE.
    WA_BOR-OBJTYPE = 'ZLES0144'.
  ENDIF.

  ANEXO_OBJ->SET_RW_MODE( IP_MODE = VL_IP_MODE ).

  ANEXO_OBJ->START_SERVICE_DIRECT(
    EXPORTING
      IP_SERVICE         = VL_IP_SERVICE
      IS_OBJECT          = WA_BOR
    EXCEPTIONS
      NO_OBJECT          = 1
      OBJECT_INVALID     = 2
      EXECUTION_FAILED   = 3
      OTHERS             = 4 ).

  COMMIT WORK.


ENDFORM.

FORM F_DEFINE_COLOR_SAIDA CHANGING P_SAIDA_0100 TYPE TY_SAIDA_0100.

  DATA: WL_COLOR  TYPE KKBLO_SPECIALCOL.

  CLEAR: P_SAIDA_0100-COLOR.

*-----------------------------------------------------------------*
* Fertilizantes
*-----------------------------------------------------------------*
  CLEAR: WL_COLOR.
  WL_COLOR-FIELDNAME = 'PERC_FERT'.
  WL_COLOR-COLOR-COL = 3.
  WL_COLOR-COLOR-INT = 0.
  WL_COLOR-COLOR-INV = 0.

  IF VG_OPERACAO = C_VIEW.
    APPEND WL_COLOR TO P_SAIDA_0100-COLOR.
  ENDIF.

  WL_COLOR-FIELDNAME = 'PERC_FERT_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

  WL_COLOR-FIELDNAME = 'VL_FERT_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

*-----------------------------------------------------------------*
* Defensivos
*-----------------------------------------------------------------*
  CLEAR: WL_COLOR.
  WL_COLOR-FIELDNAME = 'PERC_DEF'.
  WL_COLOR-COLOR-COL = 7.
  WL_COLOR-COLOR-INT = 0.
  WL_COLOR-COLOR-INV = 0.

  IF VG_OPERACAO = C_VIEW.
    APPEND WL_COLOR TO P_SAIDA_0100-COLOR.
  ENDIF.

  WL_COLOR-FIELDNAME = 'PERC_DEF_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

  WL_COLOR-FIELDNAME = 'VL_DEF_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

*-----------------------------------------------------------------*
* Sementes
*-----------------------------------------------------------------*
  CLEAR: WL_COLOR.
  WL_COLOR-FIELDNAME = 'PERC_SEM'.
  WL_COLOR-COLOR-COL = 5.
  WL_COLOR-COLOR-INT = 0.
  WL_COLOR-COLOR-INV = 0.

  IF VG_OPERACAO = C_VIEW.
    APPEND WL_COLOR TO P_SAIDA_0100-COLOR.
  ENDIF.

  WL_COLOR-FIELDNAME = 'PERC_SEM_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

  WL_COLOR-FIELDNAME = 'VL_SEM_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

*-----------------------------------------------------------------*
* Graos
*-----------------------------------------------------------------*
  CLEAR: WL_COLOR.
  WL_COLOR-FIELDNAME = 'PERC_GRAOS'.
  WL_COLOR-COLOR-COL = 1.
  WL_COLOR-COLOR-INT = 0.
  WL_COLOR-COLOR-INV = 0.

  IF VG_OPERACAO = C_VIEW.
    APPEND WL_COLOR TO P_SAIDA_0100-COLOR.
  ENDIF.

  WL_COLOR-FIELDNAME = 'PERC_GRAOS_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.

  WL_COLOR-FIELDNAME = 'VL_GRAOS_REAL'.
  APPEND WL_COLOR TO P_SAIDA_0100-COLOR.



ENDFORM.

FORM F_DIA_ANTERIOR_UTIL CHANGING P_DATA TYPE SY-DATUM.

  DATA: V_DATA_AUX TYPE SY-DATUM.

  CHECK P_DATA IS NOT INITIAL.

  V_DATA_AUX = P_DATA.

  SUBTRACT 1 FROM V_DATA_AUX.

  "Jogar para o dia útil anterior
  ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
    EXPORTING
      I_DATA_BASE = V_DATA_AUX
      I_SIGNUM    = '-'
      I_CK_DATA_ZLES0145 = ABAP_TRUE
    RECEIVING
      R_DATA      = V_DATA_AUX
    EXCEPTIONS
      ERRO        = 1
      OTHERS      = 2 ).

  IF V_DATA_AUX IS NOT INITIAL.
    P_DATA = V_DATA_AUX.
  ENDIF.

ENDFORM.

FORM F_PROXIMO_DIA_UTIL CHANGING P_DATA TYPE SY-DATUM.

  DATA: V_DATA_AUX TYPE SY-DATUM.

  CHECK P_DATA IS NOT INITIAL.

  V_DATA_AUX = P_DATA.

  "Jogar para o dia útil anterior
  ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
    EXPORTING
      I_DATA_BASE = V_DATA_AUX
      I_SIGNUM    = '+'
      I_CK_DATA_ZLES0145 = ABAP_TRUE
    RECEIVING
      R_DATA      = V_DATA_AUX
    EXCEPTIONS
      ERRO        = 1
      OTHERS      = 2 ).

  IF V_DATA_AUX IS NOT INITIAL.
    P_DATA = V_DATA_AUX.
  ENDIF.

ENDFORM.

FORM F_DIA_UTIL USING P_DATA TYPE SY-DATUM
             CHANGING P_DIA_UTIL.

  DATA: V_DT_TMP TYPE SY-DATUM.

  P_DIA_UTIL = ABAP_FALSE.

  V_DT_TMP = P_DATA.

  PERFORM F_PROXIMO_DIA_UTIL CHANGING V_DT_TMP.
  IF V_DT_TMP EQ P_DATA. "É dia Util
    P_DIA_UTIL = ABAP_TRUE.
  ENDIF.

ENDFORM.

FORM F_CHECK_PROC_DIA_ANTERIOR_UTIL USING P_BUKRS TYPE BUKRS
                                          P_DATA  TYPE SY-DATUM
                                 CHANGING P_PROCESSADO.

  DATA: V_DIA_ANT_UTIL TYPE SY-DATUM,
        V_DATA_OUT     TYPE C LENGTH 15.

  P_PROCESSADO = ABAP_FALSE.

  CHECK ( P_BUKRS IS NOT INITIAL ) AND ( P_DATA IS NOT INITIAL ).

  DATA(_DIA_UTIL) = ABAP_FALSE.
  PERFORM F_DIA_UTIL USING P_DATA CHANGING _DIA_UTIL.

  CHECK _DIA_UTIL EQ ABAP_FALSE. "Só fazer checagem se não for dia util.

  "Busca Movimento dia Anterior Util
  CLEAR: V_DIA_ANT_UTIL.
  V_DIA_ANT_UTIL = P_DATA.

  PERFORM F_DIA_ANTERIOR_UTIL CHANGING V_DIA_ANT_UTIL.

  CHECK V_DIA_ANT_UTIL IS NOT INITIAL.

  SELECT SINGLE *
    FROM ZLEST0141 INTO @DATA(WL_0141_MOV_ANT)
   WHERE BUKRS = @P_BUKRS
     AND DATA  = @V_DIA_ANT_UTIL.

  IF ( SY-SUBRC EQ 0 ) AND ( WL_0141_MOV_ANT-LOTE IS NOT INITIAL ) OR ( WL_0141_MOV_ANT-SEM_SLD_PGTO IS NOT INITIAL ).
    P_PROCESSADO = ABAP_TRUE.
    V_DATA_OUT = V_DIA_ANT_UTIL+6(2) && '/' && V_DIA_ANT_UTIL+4(2) && '/' && V_DIA_ANT_UTIL(4).

    MESSAGE | Processamento já realizado na data: { V_DATA_OUT } | TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TIT0200'.

  SORT T_LOG DESCENDING BY DATA_MODIF HORA_MODIF.

  IF OBJ_ALV_0200 IS INITIAL.
    PERFORM F_CRIAR_CAT USING '0200'.

    IF OBJ_CONTAINER_0200 IS INITIAL.
      CREATE OBJECT OBJ_CONTAINER_0200
        EXPORTING
          CONTAINER_NAME = 'CONTAINER_LOG'.
    ENDIF.

    CREATE OBJECT OBJ_ALV_0200
      EXPORTING
        I_PARENT = OBJ_CONTAINER_0200.

    GS_LAYOUT-SEL_MODE    = 'A'.
    GS_LAYOUT-CTAB_FNAME  = 'COLOR'.

    GS_VARIANT-REPORT     = SY-REPID.

    WA_STABLE-ROW         = 'X'.
    WA_STABLE-COL         = 'X'.


    PERFORM F_EXCLUDE_FCODE USING '0200'.

    CALL METHOD OBJ_ALV_0200->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        IS_VARIANT           = GS_VARIANT
      CHANGING
        IT_FIELDCATALOG      = IT_FCA
        IT_OUTTAB            = T_LOG.

    CALL METHOD OBJ_ALV_0200->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD OBJ_ALV_0200->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'ENTER'.
      LEAVE TO SCREEN 0..

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_CRIAR_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1755   text
*----------------------------------------------------------------------*
FORM F_CRIAR_CAT  USING P_SCREEN.

  DATA: V_EDIT TYPE C.

  FREE: WA_FCA, IT_FCA.

  CASE P_SCREEN.
    WHEN '0200'.

*      V_EDIT = ''.

*      IF VG_OPERACAO = C_EDIT.
*        V_EDIT = 'X'.
*      ENDIF.

      PERFORM F_EST_ALV USING:

       01  'ZLEST0198'      'BUKRS      '  '' 'BUKRS      '    'Empresa                    '  '12' ''  '' '' '' '' '' '',
       02  'ZLEST0198'      'DATA_MODIF '  '' 'DATA_MODIF '    'Data de criação do registro'  '25' ''  '' '' '' '' '' '',
       03  'ZLEST0198'      'HORA_MODIF '  '' 'HORA_MODIF '    'Hora modificação registro  '  '25' ''  '' '' '' '' '' '',
       04  'ZLEST0198'      'USUARIO    '  '' 'USUARIO    '    'Nome do usuário            '  '13' ''  '' '' '' '' '' '',
       05  'ZLEST0198'      'TIPO_OPER  '  '' 'TIPO_OPER  '    'Tipo de operação           '  '13' ''  '' '' '' '' '' '',
       06  'ZLEST0198'      'DATA       '  '' 'DATA       '    'Data                       '  '10' ''  '' '' '' '' '' '',
       07  'ZLEST0198'      'VALOR_ANT  '  '' 'VALOR_ANT  '    'Valor anterior             '  '13' ''  '' '' '' '' '' '',
       08  'ZLEST0198'      'VALOR_ATUAL'  '' 'VALOR_ATUAL'    'Valor atual                '  '13' ''  '' '' '' '' '' '',
       09  'ZLEST0198'      'MOTIVO     '  '' 'MOTIVO     '    'Motivo alteração           '  '255' ''  '' '' '' '' '' ''.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOG_PROCESSAMENTO .

  SELECT *
  FROM ZLEST0198
  INTO TABLE T_LOG.

  CALL SCREEN 0200 STARTING AT 5 5.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EST_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_1880   text
*      -->P_1881   text
*      -->P_1882   text
*      -->P_1883   text
*      -->P_1884   text
*      -->P_1885   text
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
*      -->P_1889   text
*      -->P_1890   text
*      -->P_1891   text
*      -->P_1892   text
*----------------------------------------------------------------------*
FORM F_EST_ALV USING VALUE(P_COL_POS)       TYPE I
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

  CLEAR WA_FCA.

  WA_FCA-FIELDNAME   = P_FIELD.
  WA_FCA-TABNAME     = P_TABNAME.
  WA_FCA-REF_TABLE   = P_REF_TABNAME.
  WA_FCA-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCA-KEY         = ' '.
  WA_FCA-EDIT        = P_EDIT.
  WA_FCA-COL_POS     = P_COL_POS.
  WA_FCA-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCA-NO_OUT      = ' '.
  WA_FCA-DO_SUM      = P_SUM.
  WA_FCA-REPTEXT     = P_SCRTEXT_L.
  WA_FCA-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCA-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCA-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCA-EMPHASIZE   = P_EMPHASIZE.
  WA_FCA-STYLE       =
  WA_FCA-JUST        = P_JUST.
  WA_FCA-HOTSPOT     = P_HOTSPOT.
  WA_FCA-F4AVAILABL  = P_F4.
  WA_FCA-CHECKBOX    = P_CHECK.

  APPEND WA_FCA TO IT_FCA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'ST0300'.
  SET TITLEBAR 'TIT0300'.


*  LOOP AT SCREEN.
*    CASE SCREEN-NAME.
*      WHEN 'W_ZLEST0140-OBS' .
*
*        IF NOT BLOCK IS INITIAL.
*          SCREEN-INPUT = 0. "Campo Fechado
*          MODIFY SCREEN.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  CASE SY-UCOMM.
    WHEN 'ENTER'.
***      Verifica se o campo observação foi preenchida.
      CLEAR WA_EDITOR.
      CALL METHOD EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = IT_EDITOR ).
      IF IT_EDITOR IS INITIAL.
        MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
        TXTOPEN = ABAP_TRUE.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVAR_LOG .



ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0400 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  PERFORM CAIXA_TXT_OBS.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAIXA_TXT_OBS .

*  CHECK OK_CODE NE 'EXECUTE'.

  IF NOT C_EDITOR IS INITIAL AND NOT EDITOR IS INITIAL.
    CALL METHOD C_EDITOR->FREE( ).
    CALL METHOD EDITOR->FREE( ).
  ENDIF.

  CREATE OBJECT: C_EDITOR EXPORTING CONTAINER_NAME = 'C_EDITOR', "CONTAINER
                   EDITOR EXPORTING PARENT         = C_EDITOR.

  CALL METHOD EDITOR->SET_TOOLBAR_MODE( TOOLBAR_MODE = EDITOR->FALSE ).
  CALL METHOD EDITOR->SET_STATUSBAR_MODE( STATUSBAR_MODE = EDITOR->FALSE ).

  CASE TXTOPEN.
    WHEN 'X'.
      CALL METHOD EDITOR->SET_READONLY_MODE( READONLY_MODE = EDITOR->FALSE ).
    WHEN OTHERS.
      CALL METHOD EDITOR->SET_READONLY_MODE( READONLY_MODE = EDITOR->TRUE ).
  ENDCASE.

  CALL METHOD EDITOR->SET_TEXT_AS_STREAM
    EXPORTING
      TEXT = IT_EDITOR.


  FREE: TXTOPEN, IT_EDITOR.


ENDFORM.
