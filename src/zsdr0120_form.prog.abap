*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_FORM
*&---------------------------------------------------------------------*

FORM F_LIMPA_VARIAVEIS.

  CLEAR: IT_SAIDA_0100[],
         TG_J_1BNFDOC[],
         TG_ACTIVE[].

ENDFORM.


FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM J_1BNFE_ACTIVE INTO TABLE TG_ACTIVE
   WHERE CREDAT IN P_DOCDAT.

  DELETE TG_ACTIVE WHERE NOT ( ( DOCSTA NE '1' ) AND ( NFNUM9 IS NOT INITIAL ) AND ( FORM IS NOT INITIAL ) AND ( MODEL EQ '55' OR MODEL EQ '57' ) ).

  CHECK TG_ACTIVE[] IS NOT INITIAL.

  SELECT *
    FROM J_1BNFDOC INTO TABLE TG_J_1BNFDOC
     FOR ALL ENTRIES IN TG_ACTIVE
   WHERE DOCNUM EQ TG_ACTIVE-DOCNUM.

ENDFORM.

FORM F_PROCESSA_DADOS.

  LOOP AT TG_ACTIVE.

    CLEAR: WA_SAIDA_0100.

    READ TABLE TG_J_1BNFDOC WITH KEY DOCNUM = TG_ACTIVE-DOCNUM.

    CHECK ( SY-SUBRC EQ 0 ) AND ( TG_J_1BNFDOC-CANDAT IS NOT INITIAL ) AND ( TG_J_1BNFDOC-BUKRS IN P_BUKRS ).

    WA_SAIDA_0100-DOCNUM           =  TG_J_1BNFDOC-DOCNUM.
    WA_SAIDA_0100-BRANCH           =  TG_J_1BNFDOC-BRANCH.
    WA_SAIDA_0100-DOCDAT           =  TG_J_1BNFDOC-DOCDAT.
    WA_SAIDA_0100-CHAVE            =  TG_ACTIVE-REGIO && TG_ACTIVE-NFYEAR && TG_ACTIVE-NFMONTH && TG_ACTIVE-STCD1 && TG_ACTIVE-MODEL && TG_ACTIVE-SERIE && TG_ACTIVE-NFNUM9 && TG_ACTIVE-DOCNUM9 && TG_ACTIVE-CDV.
    WA_SAIDA_0100-STCD1            =  TG_ACTIVE-STCD1.
    WA_SAIDA_0100-NFNUM9           =  TG_ACTIVE-NFNUM9.
    WA_SAIDA_0100-MODEL            =  TG_ACTIVE-MODEL.

    IF ( TG_ACTIVE-SCSSTA EQ '4' OR TG_ACTIVE-SCSSTA EQ 'A'  ).
      WA_SAIDA_0100-ST_INUTILIZACAO  =  ICON_LED_GREEN.
    ELSEIF ( TG_ACTIVE-ACTION_REQU IS INITIAL ).
      WA_SAIDA_0100-ST_INUTILIZACAO  =  ICON_ACTIVITY.
    ELSE.
      WA_SAIDA_0100-ST_INUTILIZACAO  =  ICON_LED_YELLOW.
    ENDIF.

    CASE ABAP_TRUE.
      WHEN P_GER.
        CHECK WA_SAIDA_0100-ST_INUTILIZACAO EQ ICON_LED_GREEN.
      WHEN P_PEN.
        CHECK WA_SAIDA_0100-ST_INUTILIZACAO NE ICON_LED_GREEN.
    ENDCASE.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.


ENDFORM.

FORM F_CALL_ALV.

   CALL SCREEN 0100.

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

      PERFORM F_ESTRUTURA_ALV USING:

       01  'J_1BNFDOC'        'DOCNUM'                   'IT_SAIDA_0100'  'DOCNUM'                        'Documento'          '10'   ' '    ' '  ' ' ' ' 'X' ' ' '' ,
       02  'J_1BNFE_ACTIVE'   'STCD1'                    'IT_SAIDA_0100'  'STCD1'                         'CNPJ Emissor'       '14'   ' '    ' '  ' ' ' ' ' ' ' ' '' ,
       03  'J_1BNFE_ACTIVE'   'NFNUM9'                   'IT_SAIDA_0100'  'NFNUM9'                        'Número'             '10'   ' '    ' '  ' ' ' ' ' ' ' ' '' ,
       03  'J_1BNFE_ACTIVE'   'MODEL'                    'IT_SAIDA_0100'  'MODEL'                         'Modelo'             '06'   ' '    ' '  ' ' 'C' ' ' ' ' '' ,
       04  'J_1BNFDOC'        'BRANCH'                   'IT_SAIDA_0100'  'BRANCH'                        'Filial'             '06'   ' '    ' '  ' ' ' ' ' ' ' ' '' ,
       05  'J_1BNFDOC'        'DOCDAT'                   'IT_SAIDA_0100'  'DOCDAT'                        'Dt.Emissão'         '10'   ' '    ' '  ' ' ' ' ' ' ' ' '' ,
       06  ''                 ''                         'IT_SAIDA_0100'  'CHAVE'                         'Chave'              '44'   ' '    ' '  ' ' ' ' ' ' ' ' '' ,
       07  ''                 ''                         'IT_SAIDA_0100'  'ST_INUTILIZACAO'               'St.Inutilização'    '15'   ' '    ' '  ' ' 'C' ' ' ' ' '' .

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


FORM F_ENVIAR_INUT_GRC.

  DATA: LC_CTE_SKIP_VERSION   TYPE J_1BCTE_INTERNAL_VERSION VALUE '0001',
        LC_NFE_SKIP_VERSION   TYPE J_1BNFE_RFC_VERSION      VALUE '0001'.

  DATA: LT_BAPIRET2     TYPE BAPIRETTAB.

  DATA: LV_MSSTAT       TYPE J_1BNFE_MS_STATUS,
        LV_XNFEACTIVE   TYPE J_1BXNFEACTIVE,
        LV_RFCDEST      TYPE RFCDEST,
        LV_ACCKEY       TYPE C LENGTH 44,
        LV_XJUST        TYPE CHAR255,
        LV_RESEND       TYPE FLAG.

  CLEAR: IT_SEL_ROWS[].

  CALL METHOD OBJ_ALV_0100->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SEL_ROWS.

  IF LINES( IT_SEL_ROWS ) EQ 0.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

    READ TABLE IT_SAIDA_0100 INTO WA_SAIDA_0100 INDEX WA_SEL_ROWS-INDEX.
    CHECK SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM J_1BNFE_ACTIVE INTO @DATA(WL_ACTIVE_MOD)
     WHERE DOCNUM EQ @WA_SAIDA_0100-DOCNUM.

    IF SY-SUBRC NE 0.
      MESSAGE |Dados NF-e(J_1BNFE_ACTIVE) documento: { WA_SAIDA_0100-DOCNUM } não encontrado!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(WL_DOC_MOD)
     WHERE DOCNUM EQ @WA_SAIDA_0100-DOCNUM.

    IF SY-SUBRC NE 0.
      MESSAGE |Dados Documento(J_1BNFDOC) documento: { WA_SAIDA_0100-DOCNUM } não encontrado!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( WL_ACTIVE_MOD-MODEL NE '55' ) AND ( WL_ACTIVE_MOD-MODEL NE '57' ).
      MESSAGE |Modelo: { WL_ACTIVE_MOD-MODEL } Documento: { WL_ACTIVE_MOD-DOCNUM } não previsto para inutilização!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( WL_DOC_MOD-CANDAT IS INITIAL ).
      MESSAGE |Documento: { WL_ACTIVE_MOD-DOCNUM } não esta estornado!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( WL_ACTIVE_MOD-DOCSTA EQ '1' ).
      MESSAGE |Documento: { WL_ACTIVE_MOD-DOCNUM } está autorizado!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( WL_ACTIVE_MOD-SCSSTA  EQ '4'  ) OR  "Autorização para rejeição & inutilização (cancelamento)
       ( WL_ACTIVE_MOD-SCSSTA  EQ 'A'  ).    "Erro de validação & inutilização permitida
      MESSAGE |Documento: { WL_ACTIVE_MOD-DOCNUM } já inutilizado!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( WL_ACTIVE_MOD-DOCSTA      EQ '3'         ) AND  "Documento Rejeitado
       ( WL_ACTIVE_MOD-ACTION_REQU IS NOT INITIAL ).

      WL_ACTIVE_MOD-DOCSTA = '2'. "Documento Recusado
      MODIFY J_1BNFE_ACTIVE FROM WL_ACTIVE_MOD.

      WL_DOC_MOD-DOCSTAT   = '2'. "Documento Recusado
      MODIFY J_1BNFDOC FROM WL_DOC_MOD.

      COMMIT WORK.
    ENDIF.

    IF WL_ACTIVE_MOD-ACTION_REQU IS INITIAL.
      MESSAGE |Documento: { WA_SAIDA_0100-DOCNUM } em processamento! Operação não permitida!  | TYPE 'S'.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
      EXPORTING
        I_BUKRS      = WL_ACTIVE_MOD-BUKRS
        I_BRANCH     = WL_ACTIVE_MOD-BRANCH
        I_MODEL      = WL_ACTIVE_MOD-MODEL
      IMPORTING
        E_RFCDEST    = LV_RFCDEST
        E_XNFEACTIVE = LV_XNFEACTIVE
      EXCEPTIONS
        RFC_ERROR    = 1
        OTHERS       = 2.

    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      CONTINUE.
    ENDIF.

    LV_ACCKEY = WA_SAIDA_0100-CHAVE.
    LV_XJUST  = 'INUTILIZACAO DF-E'.

    CASE WA_SAIDA_0100-MODEL.
      WHEN '55'.

        CALL FUNCTION '/XNFE/OUTNFE_SKIP'
          DESTINATION LV_RFCDEST
          EXPORTING
            IV_RFC_VERSION    = LC_NFE_SKIP_VERSION
            IV_ACCESSKEY      = LV_ACCKEY
            IV_XJUST          = LV_XJUST
            IV_RESEND         = LV_RESEND.

      WHEN '57'.

        CALL FUNCTION '/XNFE/CTE_SKIP'
          DESTINATION LV_RFCDEST
          EXPORTING
            IV_VERSION        = LC_CTE_SKIP_VERSION
            IV_ACCESSKEY      = LV_ACCKEY
            IV_XJUST          = LV_XJUST
          IMPORTING
            EV_ERROR_STATUS   = LV_MSSTAT
            ET_BAPIRET2       = LT_BAPIRET2..

    ENDCASE.

    IF SY-SUBRC EQ 0.

      WL_ACTIVE_MOD-SCSSTA      = '3'.   "Solicitação de rejeição & autorização para inutilização
      WL_ACTIVE_MOD-MSSTAT      = 'C'.   "Solicitação de não utilização recebida pelo SM
      WL_ACTIVE_MOD-ACTION_REQU = SPACE. "Em processamento; nenhuma ação manual necessária

      MODIFY J_1BNFE_ACTIVE FROM WL_ACTIVE_MOD.

      COMMIT WORK.

      MESSAGE 'Documento(s) processado(s) com êxito!' TYPE 'S'.

    ELSE.
      IF ( SY-MSGID IS NOT INITIAL ) AND ( SY-MSGNO IS NOT INITIAL ).
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        MESSAGE 'Houve um erro no processamento!' TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.
