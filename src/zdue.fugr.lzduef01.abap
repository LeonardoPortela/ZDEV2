*----------------------------------------------------------------------*
***INCLUDE LZDUEF01.
*----------------------------------------------------------------------*

FORM f_completa_campos_0110.

  DATA: wl_branch TYPE ty_branch.


  SELECT SINGLE *
                FROM setleaf
                INTO @DATA(wa_setleaf)
               WHERE setname = 'ZALT_CNPJ_DUE'
                 AND valfrom = @sy-uname.

  IF due_control-modo EQ c_due_view.
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due EQ @cab_due-id_due.

    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING _wl_0170 TO cab_due.
    ENDIF.
  ENDIF.

  CHECK due_control-modo NE c_due_view.
  CLEAR: cab_due-codigo_urf_embarque.


  IF wa_setleaf IS INITIAL.

    CLEAR: cab_due-cnpj_declarante.
  ENDIF.


  CASE cab_due-tp_cod_local_despacho.
    WHEN '19' OR "Fora de Recinto Alfandegado - Domiciliar
         '22' .  "Fora de Recinto Alfandegado - Não Domiciliar
    WHEN '281'.  "Recinto Alfandegado
      CLEAR: cab_due-codigo_urf_despacho.
    WHEN OTHERS.
  ENDCASE.

  IF cab_due-performance EQ abap_true.
    CLEAR: cab_due-bukrs.
  ELSE.
    CLEAR: cab_due-kunnr.
  ENDIF.

*------------------------------------------------------------------------------*
* Empresa
*------------------------------------------------------------------------------*
  IF cab_due-bukrs IS NOT INITIAL.
    SELECT SINGLE *
      FROM t001 INTO @DATA(_wl_t001)
     WHERE bukrs = @cab_due-bukrs.

    IF sy-subrc = 0.
      "CAB_DUE-DS_EMPRESA = _WL_T001-BUTXT.

      PERFORM f_get_filial_matriz USING cab_due-bukrs
                               CHANGING wl_branch.

      IF ( wl_branch-cnpj IS NOT INITIAL ) AND ( wa_setleaf IS INITIAL ).
        cab_due-cnpj_declarante = wl_branch-cnpj.
      ENDIF.
    ELSE.
      CLEAR: cab_due-bukrs.
    ENDIF.
  ENDIF.

*------------------------------------------------------------------------------*
* Cliente
*------------------------------------------------------------------------------*
  IF cab_due-kunnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cab_due-kunnr
      IMPORTING
        output = cab_due-kunnr.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(_wl_kna1)
     WHERE kunnr = @cab_due-kunnr.

    IF sy-subrc = 0.
      IF ( _wl_kna1-stcd1 IS NOT INITIAL ).
        cab_due-cnpj_declarante = _wl_kna1-stcd1.
      ENDIF.
    ELSE.
      CLEAR: cab_due-kunnr.
    ENDIF.
  ENDIF.

*------------------------------------------------------------------------------*
* Unidade Receita Federal e Recinto Alfandegado de Despacho
*------------------------------------------------------------------------------*

  IF cab_due-codigo_ra_despacho IS NOT INITIAL.
    "Dados Mestre Recinto Alfandegado
    SELECT SINGLE *
      FROM zsdt0168 INTO @DATA(_wl_0168)
     WHERE codigo_ra      EQ @cab_due-codigo_ra_despacho
       AND local_despacho EQ @abap_true.

    IF sy-subrc = 0.
      "Depara Recinto Alfandegado x Unidade Receita Federal
      SELECT SINGLE *
        FROM zsdt0169 INTO @DATA(_wl_0169)
       WHERE codigo_ra EQ @_wl_0168-codigo_ra.

      IF ( sy-subrc = 0 ) AND ( _wl_0169-codigo_urf IS NOT INITIAL ).
        "Dados Mestre Unidade Receita Federal
        SELECT SINGLE *
          FROM zsdt0167 INTO @DATA(_wl_0167)
         WHERE codigo_urf EQ @_wl_0169-codigo_urf.

        IF sy-subrc NE 0.
          MESSAGE | Cadastro da URF { _wl_0169-codigo_urf } não encontrada! | TYPE 'W'.
          CLEAR: cab_due-codigo_ra_despacho.
        ELSE.
          "CAB_DUE-DS_RA_DESPACHO      = _WL_0168-DS_RA.
          cab_due-codigo_urf_despacho = _wl_0167-codigo_urf.
          "CAB_DUE-DS_URF_DESPACHO     = _WL_0167-DS_URF.
        ENDIF.
      ELSE.
        MESSAGE | Depara para o Recinto { cab_due-codigo_ra_despacho } não encontrado( Transação ZSDT0137)! | TYPE 'W'.
        CLEAR: cab_due-codigo_ra_despacho.
      ENDIF.
    ELSE.
      MESSAGE | O Código { cab_due-codigo_ra_despacho } não se refere à um Recinto Alfandegado de Despacho! | TYPE 'W'.
      CLEAR: cab_due-codigo_ra_despacho.
    ENDIF.
  ENDIF.

  CASE cab_due-tp_cod_local_despacho.
    WHEN '19' OR "Fora de Recinto Alfandegado - Domiciliar
         '22' .  "Fora de Recinto Alfandegado - Não Domiciliar
      IF cab_due-codigo_urf_despacho IS NOT INITIAL.
        "Dados Mestre Unidade Receita Federal
        SELECT SINGLE *
          FROM zsdt0167 INTO _wl_0167
         WHERE codigo_urf EQ cab_due-codigo_urf_despacho.

        IF sy-subrc NE 0.
          CLEAR: cab_due-codigo_urf_despacho.
          MESSAGE | Cadastro da URF { cab_due-codigo_urf_despacho } não encontrada! | TYPE 'W'.
        ELSE.
          "CAB_DUE-DS_URF_DESPACHO  = _WL_0167-DS_URF.
        ENDIF.
      ENDIF.
    WHEN '281'.  "Recinto Alfandegado
    WHEN OTHERS.
  ENDCASE.

*------------------------------------------------------------------------------*
* Unidade Receita Federal e Recinto Alfandegado de Embarque
*------------------------------------------------------------------------------*

  IF cab_due-codigo_ra_embarque IS NOT INITIAL.
    "Dados Mestre Recinto Alfandegado
    SELECT SINGLE *
      FROM zsdt0168 INTO _wl_0168
     WHERE codigo_ra      EQ cab_due-codigo_ra_embarque
       AND local_embarque EQ abap_true.

    IF sy-subrc = 0.
      "Depara Recinto Alfandegado x Unidade Receita Federal
      SELECT SINGLE *
        FROM zsdt0169 INTO _wl_0169
       WHERE codigo_ra EQ _wl_0168-codigo_ra.

      IF ( sy-subrc = 0 ) AND ( _wl_0169-codigo_urf IS NOT INITIAL ).
        "Dados Mestre Unidade Receita Federal
        SELECT SINGLE *
          FROM zsdt0167 INTO _wl_0167
         WHERE codigo_urf EQ _wl_0169-codigo_urf.

        IF sy-subrc NE 0.
          MESSAGE | Cadastro da URF { _wl_0169-codigo_urf } não encontrada! | TYPE 'W'.
          CLEAR: cab_due-codigo_ra_embarque.
        ELSE.
          "CAB_DUE-DS_RA_EMBARQUE      = _WL_0168-DS_RA.
          cab_due-codigo_urf_embarque = _wl_0167-codigo_urf.
          "CAB_DUE-DS_URF_EMBARQUE     = _WL_0167-DS_URF.
        ENDIF.
      ELSE.
        MESSAGE | Depara para o Recinto { cab_due-codigo_ra_embarque } não encontrado( Transação ZSDT0137)! | TYPE 'W'.
        CLEAR: cab_due-codigo_ra_embarque.
      ENDIF.
    ELSE.
      MESSAGE | O Código { cab_due-codigo_ra_embarque } não se refere à um Recinto Alfandegado de Embarque! | TYPE 'W'.
      CLEAR: cab_due-codigo_ra_embarque.
    ENDIF.
  ENDIF.

  CASE cab_due-tp_cod_local_despacho.
    WHEN '19' OR "Fora de Recinto Alfandegado - Domiciliar
         '22' .  "Fora de Recinto Alfandegado - Não Domiciliar
      CLEAR: cab_due-codigo_ra_despacho,
             cab_due-ds_ra_despacho.
    WHEN '281'.  "Recinto Alfandegado
      CLEAR: cab_due-cnpj_cpf_resp_loc_desp,
             cab_due-local_despacho_longitude,
             cab_due-local_despacho_latitude,
             cab_due-local_despacho_end.
    WHEN OTHERS.
      CLEAR: cab_due-codigo_ra_despacho,
             cab_due-ds_ra_despacho,
             cab_due-codigo_urf_despacho,
             cab_due-ds_urf_despacho,
             cab_due-cnpj_cpf_resp_loc_desp,
             cab_due-local_despacho_longitude,
             cab_due-local_despacho_latitude,
             cab_due-local_despacho_end.
  ENDCASE.

ENDFORM.

FORM f_atualiza_textos_0110.

  DATA: wl_branch TYPE ty_branch.

  CLEAR: cab_due-ds_empresa,
         cab_due-ds_cliente,
         cab_due-ds_urf_despacho,
         cab_due-ds_ra_despacho,
         cab_due-ds_urf_embarque,
         cab_due-ds_ra_embarque,
         cab_due-ds_nome_transpor.

*------------------------------------------------------------------------------*
* Empresa
*------------------------------------------------------------------------------*
  IF cab_due-bukrs IS NOT INITIAL.
    SELECT SINGLE *
      FROM t001 INTO @DATA(_wl_t001)
     WHERE bukrs = @cab_due-bukrs.

    IF sy-subrc = 0.
      cab_due-ds_empresa = _wl_t001-butxt.
    ENDIF.
  ENDIF.

*------------------------------------------------------------------------------*
* Cliente
*------------------------------------------------------------------------------*
  IF cab_due-kunnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cab_due-kunnr
      IMPORTING
        output = cab_due-kunnr.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(_wl_kna1)
     WHERE kunnr = @cab_due-kunnr.

    IF sy-subrc = 0.
      cab_due-ds_cliente = _wl_kna1-name1.
    ENDIF.
  ENDIF.

  IF cab_due-id_nomeacao_tran IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cab_due-id_nomeacao_tran
      IMPORTING
        output = cab_due-id_nomeacao_tran.

    SELECT SINGLE ds_nome_transpor
      FROM znom_transporte INTO cab_due-ds_nome_transpor
     WHERE id_nomeacao_tran EQ cab_due-id_nomeacao_tran.

  ENDIF.

*------------------------------------------------------------------------------*
* Unidade Receita Federal e Recinto Alfandegado de Despacho
*------------------------------------------------------------------------------*
  IF cab_due-codigo_urf_despacho IS NOT INITIAL.
    "Dados Mestre Unidade Receita Federal
    SELECT SINGLE *
      FROM zsdt0167 INTO @DATA(_wl_0167)
     WHERE codigo_urf EQ @cab_due-codigo_urf_despacho.

    IF sy-subrc EQ 0.
      cab_due-ds_urf_despacho  = _wl_0167-ds_urf.
    ENDIF.
  ENDIF.

  IF cab_due-codigo_ra_despacho IS NOT INITIAL.
    "Dados Mestre Recinto Alfandegado
    SELECT SINGLE *
      FROM zsdt0168 INTO @DATA(_wl_0168)
     WHERE codigo_ra  EQ @cab_due-codigo_ra_despacho.

    IF sy-subrc EQ 0.
      cab_due-ds_ra_despacho = _wl_0168-ds_ra.
    ENDIF.
  ENDIF.

*------------------------------------------------------------------------------*
* Unidade Receita Federal e Recinto Alfandegado de Embarque
*------------------------------------------------------------------------------*

  IF cab_due-codigo_urf_embarque IS NOT INITIAL.
    "Dados Mestre Unidade Receita Federal
    SELECT SINGLE *
      FROM zsdt0167 INTO _wl_0167
     WHERE codigo_urf EQ cab_due-codigo_urf_embarque.

    IF sy-subrc EQ 0.
      cab_due-ds_urf_embarque  = _wl_0167-ds_urf.
    ENDIF.
  ENDIF.

  IF cab_due-codigo_ra_embarque IS NOT INITIAL.
    "Dados Mestre Recinto Alfandegado
    SELECT SINGLE *
      FROM zsdt0168 INTO _wl_0168
     WHERE codigo_ra  EQ cab_due-codigo_ra_embarque.

    IF sy-subrc EQ 0.
      cab_due-ds_ra_embarque = _wl_0168-ds_ra.
    ENDIF.
  ENDIF.

  PERFORM f_atrib_ds_dominio USING 'ZDM_STATUS_DUE'
                                   cab_due-status
                          CHANGING cab_due-ds_status.

  PERFORM f_atrib_ds_dominio USING 'ZDM_SITUACAO_DUE'
                                   cab_due-situacao_due
                          CHANGING cab_due-ds_situacao_due.

  PERFORM f_atrib_ds_dominio USING 'ZDM_IND_BLOQ_DUE'
                                   cab_due-ind_bloqueio
                          CHANGING cab_due-ds_ind_bloqueio.

  PERFORM f_atrib_ds_dominio USING 'ZDM_SITUACAO_CARGA'
                                   cab_due-situacao_carga
                          CHANGING cab_due-ds_situacao_carga.

  PERFORM f_atrib_ds_dominio USING 'ZDM_CONTROLE_ADM'
                                   cab_due-controle_adm
                          CHANGING cab_due-ds_controle_adm.


ENDFORM.

FORM f_define_listbox_0110.

  DATA: values   TYPE vrm_values WITH HEADER LINE,
        tg_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

**-----------------------------------------------------------*
** Forma Exportação
**-----------------------------------------------------------*
*
*  CLEAR: VALUES[], VALUES, TG_DD07T[].
*
*  SELECT *
*    FROM DD07T INTO TABLE TG_DD07T
*   WHERE DOMNAME    = 'ZDM_FORMA_EXPORTACAO'
*     AND DDLANGUAGE = SY-LANGU.
*
*  IF TG_DD07T[] IS NOT INITIAL.
*    LOOP AT TG_DD07T.
*      VALUES-TEXT = TG_DD07T-DDTEXT.
*      VALUES-KEY  = TG_DD07T-DOMVALUE_L.
*      APPEND VALUES.
*    ENDLOOP.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'CAB_DUE-FORMA_EXPORTACAO'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*  ENDIF.

ENDFORM.

FORM f_control_fields_0110.

*-----------------------------------------------------------------------------*
* Local de Despacho
*-----------------------------------------------------------------------------*

  LOOP AT SCREEN.
    CASE cab_due-tp_cod_local_despacho.
      WHEN '19' OR "Fora de Recinto Alfandegado - Domiciliar
           '22' .  "Fora de Recinto Alfandegado - Não Domiciliar
        CASE screen-group1.
          WHEN 'RD1'.
            screen-input = '0'.
          WHEN 'RD2'.
            screen-input = '1'.
        ENDCASE.
      WHEN '281'.  "Recinto Alfandegado
        CASE screen-group1.
          WHEN 'RD1'.
            screen-input = '1'.
          WHEN 'RD2'.
            screen-input = '0'.
        ENDCASE.
      WHEN OTHERS.
        CASE screen-group1.
          WHEN 'RD1' OR 'RD2'.
            screen-input = '0'.
        ENDCASE.
    ENDCASE.

    IF ( due_control-retificar EQ abap_true ) AND "Retificação
       ( screen-group2         NE 'EX1'     ). "Exceções
      screen-input = '0'.
    ENDIF.

    CASE screen-group1.
      WHEN 'PRO'. "Proprio
        IF cab_due-performance EQ abap_true.
          screen-input = '0'.
        ELSE.
          screen-input = '1'.
        ENDIF.
      WHEN 'TER'. "Terceiro
        IF cab_due-performance EQ abap_true.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      WHEN 'AVS'. "Lançamento Avulso
        IF cab_due-lcto_avulso EQ abap_true.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
    ENDCASE.

    IF due_control-modo EQ c_due_view.
      screen-input = '0'.
    ENDIF.

    CASE screen-group1.
      WHEN 'VW'.
        screen-active = '0'.

        IF ( due_control-modo EQ c_due_view ).
          CASE screen-name.
            WHEN 'BTN_EVENTOS_INTERFACE'.
              screen-input  = '1'.
              screen-active = '1'.
            WHEN 'BTN_LIB_LEITURA_OPUS'.
              screen-input  = '1'.
              screen-active = '1'.
            WHEN 'BTN_INF_DT_REGISTRO'.
              screen-input  = '1'.
              screen-active = '1'.
            WHEN 'BTN_RETRANSMITIR'.
              IF ( cab_due-tp_due EQ '2' ) AND "Com NF-e
                 ( cab_due-status EQ '1' ).    "Autorizada
                screen-input  = '1'.
                screen-active = '1'.
              ENDIF.
            WHEN 'BTN_SOL_MODIFIC_OPUS'.
              screen-input  = '1'.
              screen-active = '1'.
*              IF ( CAB_DUE-LCTO_AVULSO EQ ABAP_FALSE ) AND
*                 ( CAB_DUE-TP_DUE      EQ '1'        ) AND
*                 ( CAB_DUE-ID_DUE_REF  IS INITIAL    ).
*
*              ELSE.
*                READ TABLE TG_PARAMETROS INTO DATA(WL_PARAMETROS) WITH KEY PARID = 'ZMEMO00_BLOQ_DUE'.
*
*                IF ( SY-SUBRC EQ 0 ) OR ( CAB_DUE-LCTO_AVULSO EQ ABAP_TRUE ).
*                  SCREEN-INPUT  = '1'.
*                  SCREEN-ACTIVE = '1'.
*                ENDIF.
*              ENDIF.
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF ( cab_due-performance        EQ abap_true ) AND
       ( cab_due-preenchimento_auto EQ abap_true ).

      CASE screen-name.
        WHEN 'CAB_DUE-ID_NOMEACAO_TRAN'       OR
             'CAB_DUE-KUNNR'                  OR
             'CAB_DUE-ID_DUE_REF'             OR
             'CAB_DUE-NUMERO_RUC'             OR
             'CAB_DUE-REGIO'                  OR
             'CAB_DUE-TP_EXPORTACAO'          OR
             'CAB_DUE-TP_COD_LOCAL_DESPACHO'  OR
             'CAB_DUE-CODIGO_RA_DESPACHO'     OR
             'CAB_DUE-CODIGO_RA_EMBARQUE'     OR
             'CAB_DUE-EMB_CONTAINER'.

          screen-input = '0'.
      ENDCASE.

    ENDIF.

    IF cab_due-lcto_avulso EQ abap_true.
      CASE screen-name.
        WHEN 'CAB_DUE-CIENCIA_ALERTAS_RFB'.
          screen-input = '0'.
      ENDCASE.
    ENDIF.

    CASE screen-name.
      WHEN 'BTN_FILL_DUE'.
        IF ( cab_due-lcto_avulso   EQ abap_true    ) AND

           ( due_control-modo      EQ c_due_novo OR
             due_control-modo      EQ c_due_change ) AND

           ( cab_due-performance   EQ abap_true  ) AND
           ( cab_due-emb_container EQ abap_false ) AND
           ( cab_due-tp_due        EQ '2'        ).

          screen-active = '1'.
          screen-input  = '1'.
        ELSE.
          screen-active = '0'.
          screen-input  = '0'.
        ENDIF.

      WHEN 'BTN_DOWN_XML'.
        IF ( cab_due-lcto_avulso         EQ abap_true  ) AND
           ( due_control-modo            EQ c_due_view ) AND
           ( cab_due-performance         EQ abap_true  ) AND
           ( cab_due-emb_container       EQ abap_false ) AND
           ( cab_due-tp_due              EQ '2'        ) AND
           ( cab_due-preenchimento_auto  EQ abap_true  ).

          screen-active = '1'.
          screen-input  = '1'.
        ELSE.
          screen-active = '0'.
          screen-input  = '0'.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.


FORM f_control_fields_0121.

  LOOP AT SCREEN.
    CASE cab_due-tp_due.
      WHEN '1' .  "Sem NF-e
        CASE screen-group1.
          WHEN 'SNF'.
            screen-active = '1'.
          WHEN 'CNF'.
            screen-active = '0'.
        ENDCASE.
      WHEN '2' .  "Com NF-e
        CASE screen-group1.
          WHEN 'SNF'.
            screen-active = '0'.

            IF ( screen-group2 EQ 'IMP' ) AND ( cab_due-performance EQ abap_true ).
              screen-active = '1'.
            ENDIF.
          WHEN 'CNF'.
            screen-active = '1'.
        ENDCASE.
    ENDCASE.

    IF ( due_control-retificar EQ abap_true ) AND "Retificação
       ( screen-group2         NE 'EX1'     ). "Exceções
      screen-input = '0'.
    ENDIF.

    IF ( cab_due-lcto_avulso EQ abap_true ) AND
       ( cab_due-tp_due      EQ '2' ). "Com NF-e

      IF ( cab_due-performance EQ abap_true ).

        IF screen-name EQ 'ZSDT0172-DOCNUM'.
          screen-input  = '0'.
        ENDIF.

        IF screen-group2 EQ 'MAT'. "Campos Material
          screen-active = '1'.

          IF screen-group3 = 'EDT'.
            screen-input  = '1'.
          ENDIF.
        ENDIF.
      ELSE.
        IF screen-name EQ 'ZSDT0172-FATURA_ID'.
          screen-input  = '0'.
        ENDIF.

        IF screen-group2 EQ 'MAT'. "Campos Material
          screen-active = '1'.
          screen-input  = '0'.
        ENDIF.

      ENDIF.

    ENDIF.

    IF ( cab_due-performance        EQ abap_true ) AND
       ( cab_due-preenchimento_auto EQ abap_true ).

      CASE screen-name.
        WHEN 'ZSDT0172-FATURA_TP_CODIGO'   OR
             'ZSDT0172-FATURA_ID'          OR
             'ZSDT0172-MATNR'              OR
             'ZSDT0172-DESC_COMPLEMENTAR'  OR
             'ZSDT0172-CODIGO_NCM'         OR
             'ZSDT0172-PESO_LIQ_TOTAL'.

          screen-input = '0'.
      ENDCASE.

    ENDIF.

    IF due_control-modo EQ c_due_view.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  DATA: c_edit TYPE c,
        c_edt2 TYPE c.

  FREE: wa_fcat, it_fcat.

  CLEAR: c_edit, c_edt2.

  IF ( due_control-modo EQ c_due_novo   ) OR
     ( due_control-modo EQ c_due_change ).
    c_edit = 'X'.
    c_edt2 = 'X'.
  ENDIF.

  CASE p_screen.
    WHEN '0120'.

      PERFORM f_estrutura_alv USING:

          02  'ZSDT0172'      'ID_DUE_ITEM'                   'IT_SAIDA_0120'  'ID_DUE_ITEM'                  'Id.Item'        '07'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      IF cab_due-tp_due = '1'. "Sem NF-e.

        PERFORM f_estrutura_alv USING:

          03  'ZSDT0172'      'EXPORTADOR_CNPJ'               'IT_SAIDA_0120'  'EXPORTADOR_CNPJ'              'CNPJ Exp.'      '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          04  'ZSDT0172'      'EXPORTADOR_NOME'               'IT_SAIDA_0120'  'EXPORTADOR_NOME'              'Nome Exp.'      '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          05  'ZSDT0172'      'EXPORTADOR_COUNTRY'            'IT_SAIDA_0120'  'EXPORTADOR_COUNTRY'           'País Exp.'      '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          06  'ZSDT0172'      'EXPORTADOR_REGION'             'IT_SAIDA_0120'  'EXPORTADOR_REGION'            'UF Exp.'        '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          07  'ZSDT0172'      'EXPORTADOR_CPL_END'            'IT_SAIDA_0120'  'EXPORTADOR_CPL_END'           'End.Exp.'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      ENDIF.

      IF ( cab_due-tp_due = '1' ) OR "Sem NF-e.
         ( cab_due-tp_due = '2'  AND "Com NF-e
           cab_due-performance EQ abap_true ).

        PERFORM f_estrutura_alv USING:
          07  'ZSDT0172'      'IMPORTADOR_CODIGO'             'IT_SAIDA_0120'  'IMPORTADOR_CODIGO'            'Cod. Imp.'      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          07  'ZSDT0172'      'IMPORTADOR_NOME'               'IT_SAIDA_0120'  'IMPORTADOR_NOME'              'Nome Imp.'      '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          08  'ZSDT0172'      'IMPORTADOR_COUNTRY'            'IT_SAIDA_0120'  'IMPORTADOR_COUNTRY'           'País Imp.'      '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          09  'ZSDT0172'      'IMPORTADOR_CPL_END'            'IT_SAIDA_0120'  'IMPORTADOR_CPL_END'           'End. Imp.'      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      ENDIF.

      PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'      'FATURA_TP_CODIGO'              'IT_SAIDA_0120'  'FATURA_TP_CODIGO'             'Tp.Cd.Fat.'      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .


      IF cab_due-tp_due = '2'. "Com NF-e.

        PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'      'FATURA_ID'                     'IT_SAIDA_0120'  'FATURA_ID'                    'Fatura ID.'      '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'      'DOCNUM'                        'IT_SAIDA_0120'  'DOCNUM'                       'Docnum'          '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      ENDIF.

      IF cab_due-tp_due = '1'. "Sem NF-e.

        PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'       'FATURA_MOTIVO_DISPENSA_NF'     'IT_SAIDA_0120'  'FATURA_MOTIVO_DISPENSA_NF'   'Cd.Disp.NF'      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      ENDIF.

      PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'       'CODIGO_COND_VENDA'             'IT_SAIDA_0120'  'CODIGO_COND_VENDA'           'Cd.Cond.Venda'   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'VLR_LOCAL_EMBARQUE'            'IT_SAIDA_0120'  'VLR_LOCAL_EMBARQUE'          'Vlr.Loc.Emb.'    '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'DESC_COMPLEMENTAR'             'IT_SAIDA_0120'  'DESC_COMPLEMENTAR'           'Ds.Compl.'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'VLR_COND_VENDA'                'IT_SAIDA_0120'  'VLR_COND_VENDA'              'Vlr.Cond.Venda'  '15'   ' '    ''  ' ' ' ' ' ' ' ' '' .

      IF cab_due-tp_due = '1'. "Sem NF-e.

        PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'       'DESC_MERCADORIA'               'IT_SAIDA_0120'  'DESC_MERCADORIA'             'Ds.Merc.'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'CODIGO_NCM'                    'IT_SAIDA_0120'  'CODIGO_NCM'                  'NCM'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'UE_EXPORTADA'                  'IT_SAIDA_0120'  'UE_EXPORTADA'                'UE.Exp.'        '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'QTDE_UE_EXPORTADA'             'IT_SAIDA_0120'  'QTDE_UE_EXPORTADA'           'Qtd.UE.Exp.'    '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'UC_EXPORTADA'                  'IT_SAIDA_0120'  'UC_EXPORTADA'                'Un.Comerc.'     '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'QTDE_UC_EXPORTADA'             'IT_SAIDA_0120'  'QTDE_UC_EXPORTADA'           'Qtd.UC.Exp'     '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'MATNR'                         'IT_SAIDA_0120'  'MATNR'                       'Material'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'ATRIB_CODE'                    'IT_SAIDA_0120'  'ATRIB_CODE'                  'Cod.Atrib.'     '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'       'DESC_ATRIB'                    'IT_SAIDA_0120'  'DESC_ATRIB'                  'Ds.Atrib.'      '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .


      ENDIF.

      PERFORM f_estrutura_alv USING:

          10  'ZSDT0172'      'PESO_LIQ_TOTAL'                 'IT_SAIDA_0120'  'PESO_LIQ_TOTAL'              'Peso Liq.Tot.(KG)'  '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
          10  'ZSDT0172'      'CODIGO_ENQUADRAMENTO'           'IT_SAIDA_0120'  'CODIGO_ENQUADRAMENTO'        'Cod.Enquad.'        '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0122'.

      PERFORM f_estrutura_alv USING:

          01  'T005'          'LAND1'                          'IT_SAIDA_0122'  'DESTINO_COUNTRY'             'País'                           '07'   c_edit    ''  ' ' ' ' ' ' ' ' '' ,
          02  'ZSDT0174'      'PESO_LIQ_TOTAL'                 'IT_SAIDA_0122'  'PESO_LIQ_TOTAL'              'Peso Liq.Tot.(KG)'              '18'   c_edit    ''  ' ' ' ' ' ' ' ' '' ,
          03  'ZSDT0174'      'DESTINO_QTDE_UE_EXPORTADA'      'IT_SAIDA_0122'  'DESTINO_QTDE_UE_EXPORTADA'   'Qtde Un.Estatística Exportada'  '30'   ''        ''  ' ' ' ' ' ' ' ' '' ,
          04  'ZSDT0174'      'UE_EXPORTADA'                   'IT_SAIDA_0122'  'UE_EXPORTADA'                'UE.Exp.'                        '07'   ''        ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0123'.

      IF ( due_control-retificar EQ abap_true ). "Retificação
        CLEAR: c_edit.
      ENDIF.

      IF ( cab_due-performance        EQ abap_true ) AND
         ( cab_due-preenchimento_auto EQ abap_true ).
        CLEAR: c_edit, c_edt2.
      ENDIF.

      IF ( cab_due-lcto_avulso EQ abap_true ) AND ( cab_due-performance EQ abap_false ).

        PERFORM f_estrutura_alv USING:

          01  'ZSDT0173'            'ID_DUE_ITEM'                    'IT_SAIDA_0123'  'ID_DUE_ITEM'                        'Item'                           '04'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          02  'ZSDT0173'            'DOCNUM'                         'IT_SAIDA_0123'  'DOCNUM'                             'Docnum'                         '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          03  'ZSDT0173'            'ID_FATURA_REF'                  'IT_SAIDA_0123'  'ID_FATURA_REF'                      'Id.Fatura'                      '44'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          03  'ZSDT0173'            'ID_FATURA'                      'IT_SAIDA_0123'  'ID_FATURA'                          'Item.NF'                        '07'   c_edt2    ''  ' ' ' ' ' ' ' ' ' ' ,
          04  'ZSDT0173'            'FATURA_TP_CODIGO'               'IT_SAIDA_0123'  'FATURA_TP_CODIGO'                   'Tp.Cod.'                        '07'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          05  'ZSDT0173'            'EMISSOR_CNPJ'                   'IT_SAIDA_0123'  'EMISSOR_CNPJ_CPF'                   'Emissor CNPJ/CPF'               '16'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          06  'ZSDT0173'            'EMISSOR_IE'                     'IT_SAIDA_0123'  'EMISSOR_IE'                         'Emissor IE'                     '10'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          07  'ZSDT0173'            'MODEL'                          'IT_SAIDA_0123'  'MODEL'                              'Model'                          '10'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          08  'ZSDT0173'            'SERIE'                          'IT_SAIDA_0123'  'SERIE'                              'Série'                          '10'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          09  'ZSDT0173'            'NFNUM9'                         'IT_SAIDA_0123'  'NFNUM9'                             'Número'                         '10'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          10  'ZSDT0173'            'PESO_LIQ_TOTAL'                 'IT_SAIDA_0123'  'PESO_LIQ_TOTAL'                     'Peso Liq.Tot.(KG)'              '18'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          11  'ZSDT0173'            'UE_EXPORTADA'                   'IT_SAIDA_0123'  'UE_EXPORTADA'                       'UE.Exp.'                        '07'   ''        ''  ' ' ' ' ' ' ' ' '' ,
          12  'ZSDT0173'            'QTDE_UE_EXPORTADA'              'IT_SAIDA_0123'  'QTDE_UE_EXPORTADA'                  'Qtde.Un.Estatística Exp.'       '24'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          13  'ZSDT0173'            'REGISTRO_CCT'                   'IT_SAIDA_0123'  'REGISTRO_CCT'                       'Registro CCT'                   '12'   c_edit    ''  ' ' ' ' ' ' ' ' 'X' ,
          14  'ZSDT0173'            'NF_PRODUTOR'                    'IT_SAIDA_0123'  'NF_PRODUTOR'                        'NF.Produtor'                    '12'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'ENTRADA_PROPRIA'                'IT_SAIDA_0123'  'ENTRADA_PROPRIA'                    'Entrada.Prop.'                  '13'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'COMPLEMENTO'                    'IT_SAIDA_0123'  'COMPLEMENTO'                        'Complemento'                    '13'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'RFL_TERCEIRO'                   'IT_SAIDA_0123'  'RFL_TERCEIRO'                       'RFL Terc.'                      '09'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          16  'ZIB_NFE_DIST_ITM'    'PROD_NCM'                       'IT_SAIDA_0123'  'NCM_XML'                            'NCM XML'                        '14'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          17  'ZIB_NFE_DIST_ITM'    'PROD_UND_TRIB'                  'IT_SAIDA_0123'  'UTRIB_XML'                          'Un.Trib.XML'                    '12'   ''        ''  ' ' ' ' ' ' ' ' ' ' .

      ELSE.

        PERFORM f_estrutura_alv USING:

          01  'ZSDT0173'            'ID_DUE_ITEM'                    'IT_SAIDA_0123'  'ID_DUE_ITEM'                        'Item'                           '04'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          02  'ZSDT0173'            'DOCNUM'                         'IT_SAIDA_0123'  'DOCNUM'                             'Docnum'                         '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          03  'ZSDT0173'            'ID_FATURA_REF'                  'IT_SAIDA_0123'  'ID_FATURA_REF'                      'Id.Fatura'                      '44'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          03  'ZSDT0173'            'ID_FATURA'                      'IT_SAIDA_0123'  'ID_FATURA'                          'Item.NF'                        '07'   c_edt2    ''  ' ' ' ' ' ' ' ' ' ' ,
          04  'ZSDT0173'            'FATURA_TP_CODIGO'               'IT_SAIDA_0123'  'FATURA_TP_CODIGO'                   'Tp.Cod.'                        '07'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          05  'ZSDT0173'            'EMISSOR_CNPJ'                   'IT_SAIDA_0123'  'EMISSOR_CNPJ_CPF'                   'Emissor CNPJ/CPF'               '16'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          06  'ZSDT0173'            'EMISSOR_IE'                     'IT_SAIDA_0123'  'EMISSOR_IE'                         'Emissor IE'                     '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          07  'ZSDT0173'            'MODEL'                          'IT_SAIDA_0123'  'MODEL'                              'Model'                          '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          08  'ZSDT0173'            'SERIE'                          'IT_SAIDA_0123'  'SERIE'                              'Série'                          '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          09  'ZSDT0173'            'NFNUM9'                         'IT_SAIDA_0123'  'NFNUM9'                             'Número'                         '10'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          10  'ZSDT0173'            'PESO_LIQ_TOTAL'                 'IT_SAIDA_0123'  'PESO_LIQ_TOTAL'                     'Peso Liq.Tot.(KG)'              '18'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          11  'ZSDT0173'            'UE_EXPORTADA'                   'IT_SAIDA_0123'  'UE_EXPORTADA'                       'UE.Exp.'                        '07'   ''        ''  ' ' ' ' ' ' ' ' '' ,
          12  'ZSDT0173'            'QTDE_UE_EXPORTADA'              'IT_SAIDA_0123'  'QTDE_UE_EXPORTADA'                  'Qtde.Un.Estatística Exp.'       '24'   c_edit    ''  ' ' ' ' ' ' ' ' ' ' ,
          13  'ZSDT0173'            'REGISTRO_CCT'                   'IT_SAIDA_0123'  'REGISTRO_CCT'                       'Registro CCT'                   '12'   c_edit    ''  ' ' ' ' ' ' ' ' 'X' ,
          14  'ZSDT0173'            'NF_PRODUTOR'                    'IT_SAIDA_0123'  'NF_PRODUTOR'                        'NF.Produtor'                    '12'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'ENTRADA_PROPRIA'                'IT_SAIDA_0123'  'ENTRADA_PROPRIA'                    'Entrada.Prop.'                  '13'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'COMPLEMENTO'                    'IT_SAIDA_0123'  'COMPLEMENTO'                        'Complemento'                    '13'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          15  'ZSDT0173'            'RFL_TERCEIRO'                   'IT_SAIDA_0123'  'RFL_TERCEIRO'                       'RFL Terc.'                      '09'   c_edit    ''  ' ' 'C' ' ' ' ' 'X' ,
          16  'ZIB_NFE_DIST_ITM'    'PROD_NCM'                       'IT_SAIDA_0123'  'NCM_XML'                            'NCM XML'                        '14'   ''        ''  ' ' ' ' ' ' ' ' ' ' ,
          17  'ZIB_NFE_DIST_ITM'    'PROD_UND_TRIB'                  'IT_SAIDA_0123'  'UTRIB_XML'                          'Un.Trib.XML'                    '12'   ''        ''  ' ' ' ' ' ' ' ' ' ' .


      ENDIF.

    WHEN '0125'.

      PERFORM f_estrutura_alv USING:

          01  'ZSDT0190'            'NR_LPCO'                        'IT_SAIDA_0125'  'NR_LPCO'                            'Nro.LPCO'                       '15'   c_edit    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '1000'.

      PERFORM f_estrutura_alv USING:

           01  ''                    ''                               'IT_SAIDA_1000'  'CODE'                                  'Code'                        '12'    ' '    ''  ' ' ' ' ' ' ' ' '' ,
           02  ''                    ''                               'IT_SAIDA_1000'  'TAG'                                   'Tag'                         '20'    ' '    ''  ' ' ' ' ' ' ' ' '' ,
           03  ''                    ''                               'IT_SAIDA_1000'  'MESSAGE_V1'                            'Mensagem 1'                  '125'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           04  ''                    ''                               'IT_SAIDA_1000'  'MESSAGE_V2'                            'Mensagem 2'                  '125'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           05  ''                    ''                               'IT_SAIDA_1000'  'MESSAGE'                               'Mensagem'                    '125'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0201'.

      PERFORM f_estrutura_alv USING:

         01  'ZSDT0289'                    'NUMERO_RUC'              'IT_SAIDA_0201'  'NUMERO_RUC'                          'Numero RUC'           '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         02  'ZSDT0289'                    'ANO'                     'IT_SAIDA_0201'  'ANO'                                 'Ano'                  '04'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         03  'ZSDT0289'                    'KUNNR_EXP'               'IT_SAIDA_0201'  'KUNNR_EXP'                           'Cliente.Exp.'         '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         04  'ZSDT0289'                    'MATNR'                   'IT_SAIDA_0201'  'MATNR'                               'Material'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         05  'ZSDT0289'                    'CODIGO_RA'               'IT_SAIDA_0201'  'CODIGO_RA'                           'Codigo RA'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         06  'ZSDT0289'                    'ID_NOMEACAO_TRAN'        'IT_SAIDA_0201'  'ID_NOMEACAO_TRAN'                    'Id.Nomeação'          '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         07  'ZSDT0170'                    'ID_DUE'                  'IT_SAIDA_0201'  'ID_DUE_RET'                          'Id.DU-e.Retificação'  '19'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         08  'ZSDT0289'                    'DT_REGISTRO'             'IT_SAIDA_0201'  'DT_REGISTRO'                         'Dt.Registro'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         09  'ZSDT0289'                    'HR_REGISTRO'             'IT_SAIDA_0201'  'HR_REGISTRO'                         'Hr.Registro'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         10  'ZSDT0289'                    'US_REGISTRO'             'IT_SAIDA_0201'  'US_REGISTRO'                         'Us.Registro'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

  ENDCASE.

ENDFORM.

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0120'.
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0122'.
      CALL METHOD obj_alv_0122->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0123'.
      CALL METHOD obj_alv_0123->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0125'.
      CALL METHOD obj_alv_0125->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

  ENDCASE.

ENDFORM.


FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_check).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_check.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  CHECK p_screen <> '0120'.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

FORM f_gravar_due.

  DATA: wl_zsdt0170 TYPE zsdt0170,
        wl_zsdt0172 TYPE zsdt0172,
        wl_zsdt0173 TYPE zsdt0173,
        wl_zsdt0174 TYPE zsdt0174,
        wl_zsdt0190 TYPE zsdt0190.

  DATA: zcl_due TYPE REF TO zcl_due.

  CLEAR: wl_zsdt0170.

  MOVE-CORRESPONDING cab_due TO wl_zsdt0170.

  FREE zcl_due.
  CREATE OBJECT zcl_due.

  "Seta Cabeçalho DU-e
  zcl_due->set_cabecalho( i_zsdt0170 = wl_zsdt0170 ).

  "Seta Itens DU-e
  LOOP AT it_saida_0120 ASSIGNING FIELD-SYMBOL(<fs_saida_0120>).
    CLEAR: wl_zsdt0172.
    MOVE-CORRESPONDING <fs_saida_0120> TO wl_zsdt0172.
    zcl_due->add_item( i_zsdt0172 = wl_zsdt0172 ).
  ENDLOOP.

  "Seta Itens Paises Destino
  LOOP AT it_saida_0122 ASSIGNING FIELD-SYMBOL(<fs_saida_0122>).
    CLEAR: wl_zsdt0174.
    MOVE-CORRESPONDING <fs_saida_0122> TO wl_zsdt0174.
    zcl_due->add_item_pais_destino( i_zsdt0174 = wl_zsdt0174 ).
  ENDLOOP.

  "Seta Itens Faturas Referenciadas
  LOOP AT it_saida_0123 ASSIGNING FIELD-SYMBOL(<fs_saida_0123>).
    CLEAR: wl_zsdt0173.
    MOVE-CORRESPONDING <fs_saida_0123> TO wl_zsdt0173.
    zcl_due->add_item_fatura_ref( i_zsdt0173 = wl_zsdt0173 ).
  ENDLOOP.

  "Seta Itens LPCO
  LOOP AT it_saida_0125 ASSIGNING FIELD-SYMBOL(<fs_saida_0125>).
    CLEAR: wl_zsdt0190.
    MOVE-CORRESPONDING <fs_saida_0125> TO wl_zsdt0190.
    zcl_due->add_item_lpco( i_zsdt0190 = wl_zsdt0190 ).
  ENDLOOP.

  IF cab_due-preenchimento_auto EQ abap_true.
    zcl_due->set_xmls_exportacao( i_xmls_exportacao = cab_due-xmls_exportacao ).
  ENDIF.

  TRY .
      DATA(r_gravou) = zcl_due->gravar_registro( ).
    CATCH zcx_due INTO DATA(ex_due).
      ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  IF r_gravou IS NOT INITIAL.
    PERFORM f_save_zsdt0305 CHANGING  zcl_due->at_due-id_due.
    LEAVE TO SCREEN 0.


  ENDIF.

ENDFORM.

FORM f_lib_leitura_due_comex.

  DATA: zcl_due TYPE REF TO zcl_due.

  FREE zcl_due.
  CREATE OBJECT zcl_due
    EXPORTING
      i_id_due = cab_due-id_due.

  TRY.
      zcl_due->lib_leitura_opus( ).
    CATCH zcx_due INTO DATA(ex_due).
      ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

ENDFORM.

FORM f_sol_modif_due_comex.

  DATA: zcl_due TYPE REF TO zcl_due.

  FREE zcl_due.
  CREATE OBJECT zcl_due
    EXPORTING
      i_id_due = cab_due-id_due.

  TRY.
      zcl_due->sol_modific_opus( ).
    CATCH zcx_due INTO DATA(ex_due).
      ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.


ENDFORM.

FORM f_get_filial_matriz  USING p_bukrs
                       CHANGING p_branch TYPE ty_branch.

  DATA: wl_branch_detail TYPE bapibranch.

  CLEAR: wl_branch_detail, p_branch.

  CHECK p_bukrs IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bbranch INTO @DATA(_wl_branch)
   WHERE bukrs  = @p_bukrs
     AND branch = '0001'. "Matriz

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
      EXPORTING
        company       = _wl_branch-bukrs
        branch        = _wl_branch-branch
      IMPORTING
        branch_detail = wl_branch_detail.

    p_branch-cnpj = wl_branch_detail-cgc_number.
    p_branch-name = wl_branch_detail-name.

    IF wl_branch_detail-adrnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM adrc INTO @DATA(_wl_adrc)
       WHERE addrnumber = @wl_branch_detail-adrnr.

      IF ( sy-subrc = 0 ).
        p_branch-country =  _wl_adrc-country.
        p_branch-region  =  _wl_adrc-region.
        p_branch-street  =  _wl_adrc-street.
        p_branch-city2   =  _wl_adrc-city2.
        p_branch-city1   =  _wl_adrc-city1.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_get_max_id_item  TABLES p_saida_0120  LIKE it_saida_0120
                      CHANGING p_id_due_item TYPE zsdt0172-id_due_item.

  DATA: tg_saida_0120_aux TYPE TABLE OF ty_saida_0120 WITH HEADER LINE.

  tg_saida_0120_aux[] = p_saida_0120[].

  DELETE tg_saida_0120_aux WHERE id_due_item IS INITIAL.

  IF tg_saida_0120_aux[] IS INITIAL.
    p_id_due_item = 1.
  ELSE.
    SORT tg_saida_0120_aux BY id_due_item DESCENDING.
    DELETE ADJACENT DUPLICATES FROM tg_saida_0120_aux COMPARING id_due_item.
    READ TABLE tg_saida_0120_aux INDEX 1.
    IF sy-subrc = 0.
      p_id_due_item = tg_saida_0120_aux-id_due_item + 1.
    ENDIF.
  ENDIF.

ENDFORM.

FORM free_alv USING p_screen.

  CASE p_screen.
    WHEN '0120'.

      IF obj_alv_0120 IS NOT INITIAL.
        CALL METHOD obj_alv_0120->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0120.
      ENDIF.

    WHEN '0122'.

      IF obj_alv_0122 IS NOT INITIAL.
        CALL METHOD obj_alv_0122->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0122.
      ENDIF.

    WHEN '0123'.

      IF obj_alv_0123 IS NOT INITIAL.
        CALL METHOD obj_alv_0123->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0123.
      ENDIF.

    WHEN '0125'.

      IF obj_alv_0125 IS NOT INITIAL.
        CALL METHOD obj_alv_0125->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0125.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETA_CAMPOS_0121
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_completa_campos_0121 .

  DATA: v_retorno_proc TYPE zde_retorno_proc,
        v_chave_nfe    TYPE zde_chave_nfe,
        vmatnr18       TYPE matnr18.

  DATA: v_matnr     TYPE mara-matnr,
        v_ncm       TYPE j1b_nf_xml_item-ncm,
        v_meins_in  TYPE mara-meins,
        v_meins_out TYPE mara-meins,
        v_qtde      TYPE j1b_nf_xml_item-qtrib,
        v_vlr       TYPE zsdt0172-vlr_local_embarque.

  DATA: wl_mara TYPE mara.

  CHECK due_control-modo NE c_due_view.

  CLEAR: zsdt0172-importador_nome,
         zsdt0172-importador_country,
         zsdt0172-importador_cpl_end,
         zsdt0172-desc_mercadoria,
         "ZSDT0172-CODIGO_NCM,
         zsdt0172-ue_exportada,
         zsdt0172-uc_exportada,
         zsdt0172-qtde_ue_exportada,
         zsdt0172-qtde_uc_exportada.

  IF ( cab_due-lcto_avulso EQ abap_true  ) AND
     ( cab_due-tp_due EQ '2'             ) AND
     ( cab_due-performance EQ abap_false ). "Com NF-e

    CLEAR: zsdt0172-matnr,
           zsdt0172-fatura_id.

    IF zsdt0172-docnum IS NOT INITIAL.
      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_lin_doc)
       WHERE docnum = @zsdt0172-docnum.

      IF sy-subrc EQ 0.
        zsdt0172-matnr = _lin_doc-matnr.

        CASE _lin_doc-meins.
          WHEN 'KG'.
            zsdt0172-peso_liq_total = _lin_doc-menge.
          WHEN 'TO'.
            zsdt0172-peso_liq_total = _lin_doc-menge * 1000.
        ENDCASE.

        "Cabeçalho Fatura
        SELECT SINGLE *
          FROM vbrk INTO @DATA(_wl_vbrk)
         WHERE vbeln EQ @_lin_doc-refkey(10).

        IF sy-subrc EQ 0.
          IF ( zsdt0172-peso_liq_total > 0 ) AND ( _wl_vbrk-netwr > 0 ).
            zsdt0172-preco_ton = _wl_vbrk-netwr / ( zsdt0172-peso_liq_total / 1000 ).
            zsdt0172-vlr_local_embarque = _wl_vbrk-netwr.
            zsdt0172-vlr_cond_venda     = _wl_vbrk-netwr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
          EXPORTING
            i_docnum    = zsdt0172-docnum
          IMPORTING
            e_chave_nfe = v_chave_nfe
            e_retorno   = v_retorno_proc.

        IF v_retorno_proc-type EQ 'E'.
          CLEAR: zsdt0172-matnr.
          MESSAGE v_retorno_proc-texto TYPE 'S'.
        ELSEIF v_chave_nfe IS NOT INITIAL.
          zsdt0172-fatura_id = v_chave_nfe.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF zsdt0172-importador_codigo IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zsdt0172-importador_codigo
      IMPORTING
        output = zsdt0172-importador_codigo.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(_wl_kna1)
     WHERE kunnr EQ @zsdt0172-importador_codigo.

    IF sy-subrc NE 0.
      MESSAGE |Cliente Importador { zsdt0172-importador_codigo } não encontrado! | TYPE 'S'.
      CLEAR: zsdt0172-importador_codigo.
    ELSE.
      zsdt0172-importador_nome = _wl_kna1-name1.

      IF _wl_kna1-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc INTO @DATA(_wl_adrc)
         WHERE addrnumber EQ @_wl_kna1-adrnr.

        IF sy-subrc = 0.
          zsdt0172-importador_country  = _wl_adrc-country.
          CONCATENATE _wl_adrc-street _wl_adrc-city2 '/' _wl_adrc-city1
                 INTO zsdt0172-importador_cpl_end SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF zsdt0172-matnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zsdt0172-matnr
      IMPORTING
        output = vmatnr18.

    v_matnr = vmatnr18 .

    CLEAR: wl_mara.
    SELECT SINGLE *
      FROM mara INTO wl_mara
     WHERE matnr EQ v_matnr.

    SELECT SINGLE *
      FROM makt INTO @DATA(_wl_makt)
     WHERE matnr EQ @v_matnr
       AND spras EQ @sy-langu.

    IF ( sy-subrc NE 0 ) OR ( wl_mara IS INITIAL ).
      MESSAGE |Material { zsdt0172-matnr   } não encontrado! | TYPE 'S'.
      CLEAR: zsdt0172-matnr.
    ELSE.

      IF wl_mara-meins IS INITIAL.
        MESSAGE |Material { zsdt0172-matnr   } sem unidade de Medida!! | TYPE 'S'.
        CLEAR: zsdt0172-matnr.
      ELSE.
        zsdt0172-uc_exportada    = wl_mara-meins.
        zsdt0172-desc_mercadoria = _wl_makt-maktx.

        SELECT *
          FROM marc AS a INTO TABLE @DATA(_tg_marc)
         WHERE a~matnr EQ @_wl_makt-matnr
           AND a~steuc NE @space
           AND EXISTS ( SELECT b~centrov_1
                          FROM zsdt_depara_cen AS b
                         WHERE b~centrov_1 EQ a~werks ).

        IF _tg_marc[] IS INITIAL.
          MESSAGE |NCM do Material { zsdt0172-matnr } não encontrado! | TYPE 'S'.
        ELSE.
          READ TABLE _tg_marc INTO DATA(_wl_marc) INDEX 1.
          IF zsdt0172-codigo_ncm IS INITIAL.
            zsdt0172-codigo_ncm = _wl_marc-steuc.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  IF ( zsdt0172-matnr      IS NOT INITIAL ) AND
     ( zsdt0172-codigo_ncm IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zsdt0172-matnr
      IMPORTING
        output = vmatnr18.
    v_matnr = vmatnr18 .
    CLEAR: _tg_marc[].
    SELECT *
      FROM marc AS a INTO TABLE _tg_marc
     WHERE a~matnr EQ v_matnr
       AND a~steuc EQ zsdt0172-codigo_ncm
       AND EXISTS ( SELECT b~centrov_1
                      FROM zsdt_depara_cen AS b
                     WHERE b~centrov_1 EQ a~werks ).

    IF _tg_marc[] IS INITIAL.
      "MESSAGE |NCM do Material { ZSDT0172-MATNR } é inválido! | TYPE 'S'.
      CLEAR: zsdt0172-codigo_ncm.
    ENDIF.

  ELSE.
    CLEAR: zsdt0172-codigo_ncm.
  ENDIF.

*  IF ( ZSDT0172-PESO_LIQ_TOTAL IS INITIAL ).
*
*    V_QTDE  = ZSDT0172-QTDE_UE_EXPORTADA.
*
*    V_MEINS_IN = ZSDT0172-UE_EXPORTADA.
*
*    IF V_MEINS_IN = 'TON'.
*      V_MEINS_IN = 'TO'.
*    ENDIF.
*
*    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*      EXPORTING
*        I_MATNR              = ZSDT0172-MATNR
*        I_IN_ME              = V_MEINS_IN
*        I_OUT_ME             = 'KG'
*        I_MENGE              = V_QTDE
*      IMPORTING
*        E_MENGE              = V_QTDE
*      EXCEPTIONS
*        ERROR_IN_APPLICATION = 1
*        ERROR                = 2
*        OTHERS               = 3.
*
*    IF SY-SUBRC = 0.
*      ZSDT0172-PESO_LIQ_TOTAL = V_QTDE.
*    ENDIF.
*
*  ENDIF.

  PERFORM f_atrib_ue_exp USING zsdt0172-codigo_ncm
                      CHANGING zsdt0172-ue_exportada.

  IF ( zsdt0172-matnr             IS NOT INITIAL ) AND
     ( zsdt0172-peso_liq_total    IS NOT INITIAL ) AND
     ( zsdt0172-ue_exportada      IS NOT INITIAL ) AND
     ( zsdt0172-uc_exportada      IS NOT INITIAL ).

    v_meins_in  = 'KG'.

*------------------------------------------------------*
*   Quantidade Estatistica
*------------------------------------------------------*
    v_qtde      = zsdt0172-peso_liq_total.

    v_meins_out = zsdt0172-ue_exportada.

    IF v_meins_out = 'TON'.
      v_meins_out = 'TO'.
    ENDIF.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = zsdt0172-matnr
        i_in_me              = v_meins_in
        i_out_me             = v_meins_out
        i_menge              = v_qtde
      IMPORTING
        e_menge              = v_qtde
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc = 0.
      zsdt0172-qtde_ue_exportada = v_qtde.
    ENDIF.

*------------------------------------------------------*
*   Quantidade Comercializada
*------------------------------------------------------*
    v_qtde      = zsdt0172-peso_liq_total.

    v_meins_out = zsdt0172-uc_exportada.

    IF v_meins_out = 'TON'.
      v_meins_out = 'TO'.
    ENDIF.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = zsdt0172-matnr
        i_in_me              = v_meins_in
        i_out_me             = v_meins_out
        i_menge              = v_qtde
      IMPORTING
        e_menge              = v_qtde
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc = 0.
      zsdt0172-qtde_uc_exportada = v_qtde.
    ENDIF.

    IF zsdt0172-preco_ton IS NOT INITIAL.
      v_qtde = zsdt0172-peso_liq_total / 1000. "Converter de Kg para Toneladas
      v_vlr  = v_qtde * zsdt0172-preco_ton.

      IF zsdt0172-vlr_local_embarque IS INITIAL.
        zsdt0172-vlr_local_embarque = v_vlr.
      ELSEIF zsdt0172-vlr_local_embarque NE v_vlr.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = |Valor Local Embarque atual: { zsdt0172-vlr_local_embarque } é diferente do valor Calculado: { v_vlr }! Deseja corrigir?|
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.
          zsdt0172-vlr_local_embarque = v_vlr.
        ENDIF.

      ENDIF.

      IF zsdt0172-vlr_cond_venda IS INITIAL.
        zsdt0172-vlr_cond_venda = v_vlr.
      ELSEIF zsdt0172-vlr_cond_venda NE v_vlr.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = |Valor Condição Venda atual: { zsdt0172-vlr_cond_venda } é diferente do valor Calculado: { v_vlr }! Deseja corrigir?|
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.
          zsdt0172-vlr_cond_venda = v_vlr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_dados_retificacao TABLES t_zdoc_exp           STRUCTURE tg_zdoc_exp
                                t_znom_prog_reme     STRUCTURE tg_znom_prog_reme
                                t_j_1bnfdoc_exp      STRUCTURE tg_j_1bnfdoc_exp
                                t_zdoc_nf_produtor   STRUCTURE tg_zdoc_nf_produtor
                                t_zsdt_export        STRUCTURE tg_zsdt_export
                                t_zsdt_retlote       STRUCTURE tg_zsdt_retlote
                                t_zsdt_retlote_ter   STRUCTURE tg_zsdt_retlote_ter
                                t_doc_fat_ref        STRUCTURE tg_doc_fat_ref
                          USING p_id_due             TYPE zde_id_due
                       CHANGING c_due_retificar      TYPE zsdt0170
                                c_ok.

  DATA: wl_zlest0146 TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        lt_zlest0168 TYPE zlest0168_t,
        v_doc_rateio TYPE char01.

  DATA: v_docnum TYPE j_1bnfdoc-docnum.

  c_ok = abap_false.

  CLEAR: t_zdoc_exp[],
         t_znom_prog_reme[],
         t_j_1bnfdoc_exp[],
         t_zdoc_nf_produtor[],
         t_zsdt_export[],
         t_zsdt_retlote[],
         t_zsdt_retlote_ter[],
         t_doc_fat_ref[].

  CLEAR: c_due_retificar.

  "Cabeçalho DU-e Retificar
  SELECT SINGLE *
    FROM zsdt0170 INTO c_due_retificar
   WHERE id_due EQ p_id_due.

  IF sy-subrc NE 0.
    MESSAGE s086 WITH p_id_due.
    RETURN.
  ENDIF.

  "Documento Exportação
  SELECT *
    FROM zdoc_exp AS a INTO TABLE t_zdoc_exp
   WHERE id_due EQ p_id_due
     AND NOT EXISTS ( SELECT id_doc_exp
                        FROM zdoc_exp_recusa AS b
                       WHERE b~id_doc_exp EQ a~id_doc_exp ).

  IF t_zdoc_exp[] IS INITIAL.
    MESSAGE s078.
    RETURN.
  ENDIF.

  "Programação de Nomeações - Remessas
  SELECT *
    FROM znom_prog_reme INTO CORRESPONDING FIELDS OF TABLE t_znom_prog_reme
     FOR ALL ENTRIES IN t_zdoc_exp
   WHERE id_due     EQ t_zdoc_exp-id_due
     AND id_remessa EQ t_zdoc_exp-vbeln.

  IF t_znom_prog_reme[] IS INITIAL.
    MESSAGE s079.
    RETURN.
  ENDIF.

  LOOP AT t_znom_prog_reme ASSIGNING FIELD-SYMBOL(<fs_prog_reme>).
    "Atribuir O.V.
    SELECT SINGLE vgbel INTO <fs_prog_reme>-vgbel
      FROM lips
     WHERE vbeln EQ <fs_prog_reme>-id_remessa.

    IF ( sy-subrc NE 0 ) OR ( <fs_prog_reme>-vgbel IS INITIAL ).
      MESSAGE s081 WITH <fs_prog_reme>-id_remessa.
      RETURN.
    ENDIF.

    "Selecão registro O.V
    SELECT SINGLE *
      FROM vbak INTO @DATA(_wl_vbak)
     WHERE vbeln EQ @<fs_prog_reme>-vgbel.

    IF ( sy-subrc NE 0 ) OR ( _wl_vbak-auart IS INITIAL ).
      MESSAGE s097 WITH <fs_prog_reme>-id_remessa.
      RETURN.
    ENDIF.

    "Recuperar Notas Exportação
    CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
      EXPORTING
        i_vbeln  = <fs_prog_reme>-id_remessa
        i_direct = '2'
      IMPORTING
        e_docnum = v_docnum.

    IF v_docnum IS INITIAL.
      MESSAGE s087 WITH <fs_prog_reme>-id_remessa.
      RETURN.
    ENDIF.

    "Documento Fiscal
    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wl_doc)
     WHERE docnum = @v_docnum.

    IF sy-subrc NE 0.
      MESSAGE s088 WITH v_docnum.
      RETURN.
    ENDIF.

    "Item Documento Fiscal
    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(wl_lin)
     WHERE docnum = @wl_doc-docnum.

    IF sy-subrc NE 0.
      MESSAGE s089 WITH v_docnum.
      RETURN.
    ENDIF.

    "Cabeçalho Fatura
    SELECT SINGLE *
      FROM vbrk INTO @DATA(_wl_vbrk)
     WHERE vbeln EQ @wl_lin-refkey(10).

    IF sy-subrc NE 0.
      MESSAGE s090 WITH v_docnum.
      RETURN.
    ENDIF.

    "Item Fatura
    SELECT SINGLE *
      FROM vbrp INTO @DATA(_wl_vbrp)
     WHERE vbeln EQ @_wl_vbrk-vbeln.

    IF sy-subrc NE 0.
      MESSAGE s091 WITH _wl_vbrk-vbeln.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING wl_doc TO t_j_1bnfdoc_exp.

    t_j_1bnfdoc_exp-nbm       = wl_lin-nbm.
    t_j_1bnfdoc_exp-matnr     = wl_lin-matnr.
    t_j_1bnfdoc_exp-menge     = wl_lin-menge.
    t_j_1bnfdoc_exp-meins     = wl_lin-meins.
    t_j_1bnfdoc_exp-waerk_ft  = _wl_vbrk-waerk.
    t_j_1bnfdoc_exp-netwr     = _wl_vbrk-netwr.
    t_j_1bnfdoc_exp-vgbel     = <fs_prog_reme>-vgbel.
    t_j_1bnfdoc_exp-auart     = _wl_vbak-auart.
    t_j_1bnfdoc_exp-vbeln     = <fs_prog_reme>-id_remessa.

    "Price OV
    SELECT SINGLE FROM v_konv FIELDS * WHERE knumv EQ @_wl_vbak-knumv AND kschl EQ 'PR00' AND waers EQ 'USD' AND kmein EQ 'TO' INTO @DATA(_wl_konv) .






    IF sy-subrc EQ 0.
      t_j_1bnfdoc_exp-preco_ton = _wl_konv-kbetr.
    ENDIF.

    APPEND t_j_1bnfdoc_exp.
  ENDLOOP.

  SORT t_j_1bnfdoc_exp BY docnum.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_exp COMPARING docnum.

  "Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT *
    FROM zdoc_nf_produtor INTO TABLE t_zdoc_nf_produtor
     FOR ALL ENTRIES IN t_znom_prog_reme
   WHERE vbeln EQ t_znom_prog_reme-id_remessa.

*** Stefanini - IR241122 - 24/06/2025 - LAZAROSR - Início de Alteração
  DATA:
    gt_cfop                TYPE TABLE OF zmemo_cfop.

  IF t_zdoc_nf_produtor[] IS NOT INITIAL.

    CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
      EXPORTING
        exp_propria = 'X'
      TABLES
        cfops       = gt_cfop.

    SELECT vbeln,
           vgbel,
           vgpos
      INTO TABLE @DATA(lt_lips)
      FROM lips
      FOR ALL ENTRIES IN @t_zdoc_nf_produtor
     WHERE vbeln EQ @t_zdoc_nf_produtor-vbeln.

    IF lt_lips IS NOT INITIAL.

      SORT lt_lips BY vbeln.

      SELECT vbeln,
             posnr,
             j_1bcfop
        INTO TABLE @DATA(lt_vbap)
        FROM vbap
        FOR ALL ENTRIES IN @lt_lips
       WHERE vbeln EQ @lt_lips-vgbel
         AND posnr EQ @lt_lips-vgpos.

      IF lt_vbap IS NOT INITIAL.

        SORT lt_vbap BY vbeln posnr.

        LOOP AT t_zdoc_nf_produtor INTO DATA(ls_zdoc_nf_prod).

          DATA(vl_tabix) = sy-tabix.

          READ TABLE lt_lips INTO DATA(ls_lips)
                             WITH KEY vbeln = ls_zdoc_nf_prod-vbeln
                                                      BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            READ TABLE lt_vbap INTO DATA(ls_vbap)
                             WITH KEY vbeln = ls_lips-vgbel
                                      posnr = ls_lips-vgpos
                                              BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              READ TABLE gt_cfop TRANSPORTING NO FIELDS
                                 WITH KEY low = ls_vbap-j_1bcfop.

              IF sy-subrc IS NOT INITIAL.

                DELETE t_zdoc_nf_produtor INDEX vl_tabix.
                CONTINUE.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDIF.
*** Stefanini - IR241122 - 24/06/2025 - LAZAROSR - Fim de Alteração

  IF t_zdoc_nf_produtor[] IS INITIAL.
    LOOP AT t_j_1bnfdoc_exp WHERE auart EQ 'ZEXI'.
      MESSAGE s080.
      RETURN.
    ENDLOOP.
  ENDIF.

  "Tabela de Validação de Quantidade Exportada
  SELECT *
    FROM zsdt_export INTO TABLE t_zsdt_export
     FOR ALL ENTRIES IN t_znom_prog_reme
   WHERE ordem = t_znom_prog_reme-vgbel.

  IF t_zsdt_export[] IS INITIAL.
    MESSAGE s082.
    RETURN.
  ENDIF.

  "Tabela Retorno Formação de Lote
  SELECT *
    FROM zsdt_retlote INTO TABLE t_zsdt_retlote
     FOR ALL ENTRIES IN t_zsdt_export
   WHERE docnum_ret = t_zsdt_export-docnum.

  IF t_zsdt_retlote[] IS INITIAL.
    MESSAGE s083.
    RETURN.
  ENDIF.

  "Tabela Retorno Formação de Lote - NF RFL Terceiro
  SELECT *
    FROM zsdt_retlote_ter INTO TABLE t_zsdt_retlote_ter
     FOR ALL ENTRIES IN t_zsdt_export
   WHERE docnum_ret = t_zsdt_export-docnum.

  "Documentos Produtor
  LOOP AT t_zdoc_nf_produtor.
    CLEAR: t_doc_fat_ref.

    READ TABLE t_j_1bnfdoc_exp WITH KEY vbeln = t_zdoc_nf_produtor-vbeln.
    IF sy-subrc NE 0.
      MESSAGE s092 WITH t_zdoc_nf_produtor-vbeln.
      RETURN.
    ENDIF.

    t_doc_fat_ref-docnum_exp      = t_j_1bnfdoc_exp-docnum.
    t_doc_fat_ref-docnum          = t_zdoc_nf_produtor-docnum_prod.
    t_doc_fat_ref-menge           = t_zdoc_nf_produtor-menge.
    t_doc_fat_ref-peso_liq_total  = t_zdoc_nf_produtor-menge.
    t_doc_fat_ref-meins           = 'KG'.
    t_doc_fat_ref-matnr           = t_j_1bnfdoc_exp-matnr.
    t_doc_fat_ref-nf_produtor     = abap_true.

    PERFORM f_atrib_ue_exp USING t_j_1bnfdoc_exp-nbm
                        CHANGING t_doc_fat_ref-ue_exportada.

    IF t_doc_fat_ref-ue_exportada IS INITIAL.
      MESSAGE s099 WITH t_j_1bnfdoc_exp-nbm.
      RETURN.
    ENDIF.

    "Conversão para Unidade Estatistica
    PERFORM f_conv_ue_exp_fat CHANGING t_doc_fat_ref.

    "Check se Documento está registrado no CCT
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = t_doc_fat_ref-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_zlest0168  = lt_zlest0168
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      t_doc_fat_ref-registro_cct     = abap_true.

      READ TABLE lt_zlest0147 INTO DATA(_wl_zlest0147) INDEX 1.
      IF ( sy-subrc EQ 0 ).
        t_doc_fat_ref-complemento      = _wl_zlest0147-complemento.
        t_doc_fat_ref-entrada_propria  = _wl_zlest0147-entrada_propria.
      ENDIF.
    ENDIF.

    APPEND t_doc_fat_ref.
  ENDLOOP.

  "Documento Retorno Formação Lote
  LOOP AT t_zsdt_retlote.
    CLEAR: t_doc_fat_ref.

    READ TABLE t_zsdt_export WITH KEY docnum = t_zsdt_retlote-docnum_ret.
    IF sy-subrc NE 0.
      MESSAGE s093 WITH t_zsdt_retlote-docnum_ret.
      RETURN.
    ENDIF.

    READ TABLE t_j_1bnfdoc_exp WITH KEY vgbel = t_zsdt_export-ordem.
    IF sy-subrc NE 0.
      MESSAGE s094 WITH t_zsdt_export-ordem.
      RETURN.
    ENDIF.

    t_doc_fat_ref-docnum_exp     = t_j_1bnfdoc_exp-docnum.
    t_doc_fat_ref-docnum         = t_zsdt_retlote-docnum.
    t_doc_fat_ref-menge          = t_zsdt_retlote-quant_vinc.
    t_doc_fat_ref-peso_liq_total = t_zsdt_retlote-quant_vinc.
    t_doc_fat_ref-meins          = 'KG'.
    t_doc_fat_ref-matnr          = t_j_1bnfdoc_exp-matnr.

    PERFORM f_atrib_ue_exp USING t_j_1bnfdoc_exp-nbm
                        CHANGING t_doc_fat_ref-ue_exportada.

    IF t_doc_fat_ref-ue_exportada IS INITIAL.
      MESSAGE s099 WITH t_j_1bnfdoc_exp-nbm.
      RETURN.
    ENDIF.

    PERFORM f_conv_ue_exp_fat CHANGING t_doc_fat_ref.

    "Check se Documento está registrado no CCT
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = t_doc_fat_ref-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_zlest0168  = lt_zlest0168
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      t_doc_fat_ref-registro_cct = abap_true.

      READ TABLE lt_zlest0147 INTO _wl_zlest0147 INDEX 1.
      IF ( sy-subrc EQ 0 ).
        t_doc_fat_ref-complemento      = _wl_zlest0147-complemento.
      ENDIF.
    ENDIF.

    APPEND t_doc_fat_ref.
  ENDLOOP.

  "Documento Retorno Formação Lote Terceiro
  LOOP AT t_zsdt_retlote_ter.
    CLEAR: t_doc_fat_ref.

    READ TABLE t_zsdt_export WITH KEY docnum = t_zsdt_retlote_ter-docnum_ret.
    IF sy-subrc NE 0.
      MESSAGE s093 WITH t_zsdt_retlote_ter-docnum_ret.
      RETURN.
    ENDIF.

    READ TABLE t_j_1bnfdoc_exp WITH KEY vgbel = t_zsdt_export-ordem.
    IF sy-subrc NE 0.
      MESSAGE s094 WITH t_zsdt_export-ordem.
      RETURN.
    ENDIF.

    t_doc_fat_ref-docnum_exp     = t_j_1bnfdoc_exp-docnum.
    t_doc_fat_ref-chave_nfe      = t_zsdt_retlote_ter-chave_nfe.
    t_doc_fat_ref-menge          = t_zsdt_retlote_ter-quant_vinc.
    t_doc_fat_ref-peso_liq_total = t_zsdt_retlote_ter-quant_vinc.
    t_doc_fat_ref-registro_cct   = t_zsdt_retlote_ter-registro_cct.
    t_doc_fat_ref-emissor_cnpj   = t_zsdt_retlote_ter-cnpj_emissor.
    t_doc_fat_ref-emissor_cpf    = t_zsdt_retlote_ter-cpf_emissor.
    t_doc_fat_ref-emissor_ie     = t_zsdt_retlote_ter-ie_emissor.
    t_doc_fat_ref-meins          = 'KG'.
    t_doc_fat_ref-matnr          = t_j_1bnfdoc_exp-matnr.
    t_doc_fat_ref-rfl_terceiro   = abap_true.

    PERFORM f_atrib_ue_exp USING t_j_1bnfdoc_exp-nbm
                        CHANGING t_doc_fat_ref-ue_exportada.

    IF t_doc_fat_ref-ue_exportada IS INITIAL.
      MESSAGE s099 WITH t_j_1bnfdoc_exp-nbm.
      RETURN.
    ENDIF.

    PERFORM f_conv_ue_exp_fat CHANGING t_doc_fat_ref.

    APPEND t_doc_fat_ref.
  ENDLOOP.

  c_ok = abap_true.

ENDFORM.

FORM f_atrib_ue_exp  USING p_ncm  TYPE zsdt0172-codigo_ncm
                  CHANGING ue_exp TYPE zsdt0172-ue_exportada.

  DATA: v_ncm TYPE j1b_nf_xml_item-ncm.

  CLEAR: ue_exp.

  CHECK p_ncm IS NOT INITIAL.

  v_ncm = p_ncm.

  REPLACE ALL OCCURRENCES OF '.' IN v_ncm WITH ''.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(_wl_set_ncm_utrib)
   WHERE setname = 'MAGGI_NCM_UTRIB_EXP'
     AND valfrom = @v_ncm.

  IF sy-subrc EQ 0.
    ue_exp = 'TON'.
  ELSE.
    ue_exp = 'KG'.
  ENDIF.

ENDFORM.

FORM f_conv_ue_exp_fat CHANGING p_doc_fat_ref TYPE ty_doc_fat_ref.

  DATA: v_meins_in  TYPE mara-meins,
        v_meins_out TYPE mara-meins,
        v_qtde      TYPE j1b_nf_xml_item-qtrib.

  v_meins_in  = p_doc_fat_ref-meins.
  v_qtde      = p_doc_fat_ref-menge.

  IF v_meins_in EQ p_doc_fat_ref-ue_exportada.
    p_doc_fat_ref-qtde_ue_exportada = v_qtde.
  ELSE.
    IF ( p_doc_fat_ref-matnr         IS NOT INITIAL ) AND
       ( p_doc_fat_ref-ue_exportada  IS NOT INITIAL ) AND
       ( v_meins_in                  IS NOT INITIAL ) AND
       ( v_qtde                      IS NOT INITIAL ).

      IF v_meins_in = 'TON'.
        v_meins_in = 'TO'.
      ENDIF.

      v_meins_out = p_doc_fat_ref-ue_exportada.

      IF v_meins_out = 'TON'.
        v_meins_out = 'TO'.
      ENDIF.

      IF v_meins_in EQ 'KG' AND v_meins_out EQ 'TO'.
        p_doc_fat_ref-qtde_ue_exportada = v_qtde / 1000.
      ELSE.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = p_doc_fat_ref-matnr
            i_in_me              = v_meins_in
            i_out_me             = v_meins_out
            i_menge              = v_qtde
          IMPORTING
            e_menge              = v_qtde
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.

        IF sy-subrc = 0.
          p_doc_fat_ref-qtde_ue_exportada = v_qtde.
        ENDIF.
      ENDIF.


    ENDIF.
  ENDIF.

ENDFORM.


FORM f_conv_qtde_ton USING p_meins     TYPE j_1bnflin-meins
                           p_menge     TYPE j_1bnflin-menge
                           p_matnr     TYPE j_1bnflin-matnr
                  CHANGING c_qtde_ton  TYPE j_1bnflin-menge.


  DATA: v_meins_in  TYPE mara-meins,
        v_meins_out TYPE mara-meins,
        v_qtde      TYPE j1b_nf_xml_item-qtrib.

  CLEAR: c_qtde_ton.

  v_meins_in  = p_meins.
  v_qtde      = p_menge.

  v_meins_out = 'TO'.

  IF ( v_meins_in EQ 'TON' ) OR ( v_meins_in EQ 'TO' ).
    c_qtde_ton = v_qtde.
  ELSE.
    IF ( p_matnr      IS NOT INITIAL ) AND
       ( v_meins_out  IS NOT INITIAL ) AND
       ( v_meins_in   IS NOT INITIAL ) AND
       ( v_qtde       IS NOT INITIAL ).

      IF v_meins_in EQ 'KG' AND v_meins_out EQ 'TO'.
        c_qtde_ton = v_qtde / 1000.
      ELSE.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = p_matnr
            i_in_me              = v_meins_in
            i_out_me             = v_meins_out
            i_menge              = v_qtde
          IMPORTING
            e_menge              = v_qtde
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.

        IF sy-subrc = 0.
          c_qtde_ton = v_qtde.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

FORM f_atrib_ds_dominio USING p_domname    TYPE dd07t-domname
                              p_domvalue_l
                     CHANGING c_ddtext     TYPE dd07t-ddtext.

  DATA: values   TYPE vrm_values WITH HEADER LINE,
        tg_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

  DATA: v_value TYPE dd07t-domvalue_l.

  CLEAR: values[], values, tg_dd07t[], c_ddtext.

  CHECK ( p_domname     IS NOT INITIAL ) AND
        ( p_domvalue_l  IS NOT INITIAL ).

  v_value = CONV #( p_domvalue_l ).

  SELECT SINGLE ddtext
    FROM dd07t INTO c_ddtext
   WHERE domname    = p_domname
     AND ddlanguage = sy-langu
     AND domvalue_l = v_value.


ENDFORM.

FORM f_eventos_interface_due .

  DATA: tg_zsdt0185_out TYPE TABLE OF zsdt0185 WITH HEADER LINE.

  CLEAR: tg_zsdt0185_out[].

  SELECT *
    FROM zsdt0185 INTO TABLE tg_zsdt0185_out
   WHERE id_due = cab_due-id_due.

  CHECK tg_zsdt0185_out[] IS NOT INITIAL.

  SORT tg_zsdt0185_out BY dt_registro hr_registro.

  REFRESH estrutura.

  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_ZSDT0185_OUT' 'ID_LOG'                    'Id.Log'            '06' '' '' '' '',
     02  ''   ''            'TG_ZSDT0185_OUT' 'LEITURA_OPUS'              'Leitura Comex'     '13' '' '' 'C' 'X',
     03  ''   ''            'TG_ZSDT0185_OUT' 'DT_LEITURA_OPUS'           'Dt.Leitura'        '13' '' '' '' '',
     04  ''   ''            'TG_ZSDT0185_OUT' 'HR_LEITURA_OPUS'           'Hr.Leitura'        '13' '' '' '' '',
     05  ''   ''            'TG_ZSDT0185_OUT' 'LIB_LEITURA_OPUS'          'Lib.Leitura'       '13' '' '' 'C' 'X',
     06  ''   ''            'TG_ZSDT0185_OUT' 'SOLIC_MODIFICACAO_OPUS'    'Sol.Modific.Comex' '18' '' '' 'C' 'X',
     07  ''   ''            'TG_ZSDT0185_OUT' 'BLOQ_OPUS'                 'Bloq.Comex'        '13' '' '' 'C' 'X',
     08  ''   ''            'TG_ZSDT0185_OUT' 'VINC_OPUS'                 'Vinc.Comex'        '13' '' '' 'C' 'X',
     09  ''   ''            'TG_ZSDT0185_OUT' 'MSG_OPUS'                  'Msg.Retorno'       '13' '' '' '' '',
     10  ''   ''            'TG_ZSDT0185_OUT' 'SISTEMA'                   'Sistema'           '13' '' '' '' '',
     11  ''   ''            'TG_ZSDT0185_OUT' 'DT_REGISTRO'               'Dt.Registro'       '13' '' '' '' '',
     12  ''   ''            'TG_ZSDT0185_OUT' 'HR_REGISTRO'               'Hr.Registro'       '13' '' '' '' '',
     13  ''   ''            'TG_ZSDT0185_OUT' 'US_REGISTRO'               'Us.Registro'       '13' '' '' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 190
      i_screen_end_line     = 20
    TABLES
      t_outtab              = tg_zsdt0185_out.


ENDFORM.


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_checkbox).


  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  wa_estrutura-just      = p_just.
  wa_estrutura-checkbox  = p_checkbox.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_preencher_nf_item_due_avulso.

  DATA: t_doc_fat_ref  TYPE TABLE OF ty_doc_fat_ref WITH HEADER LINE,
        t_zsdt_retlote TYPE TABLE OF zsdt_retlote WITH HEADER LINE.

  DATA: wl_zlest0146   TYPE zlest0146,
        lt_zlest0147   TYPE zlest0147_t,
        lt_zlest0168   TYPE zlest0168_t,
        v_doc_rateio   TYPE char01,
        v_retorno_proc TYPE zde_retorno_proc,
        v_chave_nfe    TYPE zde_chave_nfe,
        v_chave_nff    TYPE zde_chave_nff.

  CLEAR: it_sel_rows[], wa_sel_rows.

  IF cab_due-tp_due NE '2'.
    MESSAGE 'Opção válida somente para DU-e com NF-e!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF ( cab_due-emb_container EQ abap_false ) AND ( cab_due-performance EQ abap_false ).
    MESSAGE 'Opção válida somente para DU-e de Performance ou Embarque Container!' TYPE 'S'.
    RETURN.
  ENDIF.

  CALL METHOD obj_alv_0120->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX wa_sel_rows-index.

  CHECK sy-subrc = 0.

  CLEAR: t_zsdt_retlote[], vg_docum_exp_venda.

  IF cab_due-emb_container EQ abap_true.

    IF wa_saida_0120-docnum IS INITIAL.
      MESSAGE 'Documento Exportação não foi informado!' TYPE 'S'.
      RETURN.
    ENDIF.

    vg_docum_exp_venda = wa_saida_0120-docnum.

    READ TABLE it_saida_0123 INTO DATA(_wl_0123) WITH KEY id_due_item = wa_saida_0120-id_due_item.
    IF ( sy-subrc EQ 0 ).
      MESSAGE 'Item já possui faturas referenciadas!' TYPE 'S'.
      RETURN.
    ENDIF.

  ELSEIF cab_due-performance EQ abap_true.

    "Informar Documento Venda
    CALL SCREEN 0124 STARTING AT 12 05 ENDING AT 40 02.

    IF vg_docum_exp_venda IS INITIAL.
      MESSAGE 'Documento NF Venda não foi informado!' TYPE 'S'.
      RETURN.
    ENDIF.

    READ TABLE it_saida_0123 INTO DATA(_wl_0143_ft) WITH KEY docnum = vg_docum_exp_venda.
    IF ( sy-subrc EQ 0 ) AND ( _wl_0143_ft-docnum IS NOT INITIAL ).
      MESSAGE 'Documento NF Venda já foi informado!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDIF.

  DATA(lwa_erro_proc) = abap_false.
  PERFORM f_preencher_nf_item_due_n2 USING wa_saida_0120
                                           vg_docum_exp_venda
                                  CHANGING lwa_erro_proc.

  IF lwa_erro_proc IS INITIAL.
    MESSAGE 'Preenchimento das faturas referenciadas concluído!' TYPE 'S'.
  ENDIF.

ENDFORM.

FORM f_preencher_nf_item_due_n2 USING p_saida_0120   TYPE ty_saida_0120
                                      p_docnum_venda TYPE j_1bnfdoc-docnum
                             CHANGING c_erro_proc.

  DATA: t_doc_fat_ref  TYPE TABLE OF ty_doc_fat_ref WITH HEADER LINE,
        t_zsdt_retlote TYPE TABLE OF zsdt_retlote WITH HEADER LINE.

  DATA: wl_zlest0146   TYPE zlest0146,
        lt_zlest0147   TYPE zlest0147_t,
        lt_zlest0168   TYPE zlest0168_t,
        v_doc_rateio   TYPE char01,
        v_retorno_proc TYPE zde_retorno_proc,
        v_chave_nfe    TYPE zde_chave_nfe,
        v_chave_nff    TYPE zde_chave_nff.

  CLEAR: c_erro_proc.


  "Get Item Doc. Exportação Venda
  SELECT SINGLE *
    FROM j_1bnflin INTO @DATA(_wl_lin_exp_venda)
   WHERE docnum EQ @p_docnum_venda.

  IF sy-subrc NE 0.
    c_erro_proc = abap_true.
    MESSAGE |Item Documento Venda/Exportação { p_docnum_venda } não foi encontrado!| TYPE 'S'.
    RETURN.
  ENDIF.

  "Get Item Fatura
  SELECT SINGLE *
    FROM vbrp INTO @DATA(_wl_vbrp)
   WHERE vbeln EQ @_wl_lin_exp_venda-refkey(10).

  IF ( sy-subrc NE 0 ) OR ( _wl_lin_exp_venda-refkey IS INITIAL ).
    c_erro_proc = abap_true.
    MESSAGE |Fatura Documento Venda/Exportação { p_docnum_venda } não foi encontrada!| TYPE 'S'.
    RETURN.
  ENDIF.

  "Get Solicitação Ordem Venda
  SELECT SINGLE *
    FROM zsdt0053 INTO @DATA(_wl_zsdt0053)
   WHERE remessa_exp EQ @_wl_vbrp-vgbel.

  IF ( sy-subrc NE 0 ).
    c_erro_proc = abap_true.
    MESSAGE |Solicitação Ordem Venda(ZSDT0053) Remessa { _wl_vbrp-vgbel } não foi encontrada!| TYPE 'S'.
    RETURN.
  ENDIF.

  IF ( _wl_zsdt0053-docnum_rt IS INITIAL ).
    c_erro_proc = abap_true.
    MESSAGE |Documento Retorno Remessa { _wl_vbrp-vgbel } não foi encontrado!| TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT *
    FROM zsdt_retlote INTO TABLE t_zsdt_retlote
   WHERE docnum_ret  = _wl_zsdt0053-docnum_rt.

  IF t_zsdt_retlote[] IS INITIAL.
    c_erro_proc = abap_true.
    MESSAGE |Nenhuma Rem.Form.Lote vinculada ao Retorno { _wl_zsdt0053-docnum_rt }!| TYPE 'S'.
    RETURN.
  ENDIF.

*--------------------------------------------------------------------------------------*
*  Montar Faturas Referenciadas
*--------------------------------------------------------------------------------------*

  CLEAR: t_doc_fat_ref[].

  "Documento Retorno Formação Lote
  LOOP AT t_zsdt_retlote.
    CLEAR: t_doc_fat_ref.

    t_doc_fat_ref-docnum_exp     = p_saida_0120-docnum.
    t_doc_fat_ref-docnum         = t_zsdt_retlote-docnum.
    t_doc_fat_ref-menge          = t_zsdt_retlote-quant_vinc.
    t_doc_fat_ref-peso_liq_total = t_zsdt_retlote-quant_vinc.
    t_doc_fat_ref-meins          = 'KG'.
    t_doc_fat_ref-matnr          = p_saida_0120-matnr.

    PERFORM f_atrib_ue_exp USING p_saida_0120-codigo_ncm
                        CHANGING t_doc_fat_ref-ue_exportada.

    IF t_doc_fat_ref-ue_exportada IS INITIAL.
      c_erro_proc = abap_true.
      MESSAGE s099 WITH p_saida_0120-codigo_ncm.
      RETURN.
    ENDIF.

    PERFORM f_conv_ue_exp_fat CHANGING t_doc_fat_ref.

    "Check se Documento está registrado no CCT
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = t_doc_fat_ref-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_zlest0168  = lt_zlest0168
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      t_doc_fat_ref-registro_cct = abap_true.
    ENDIF.

    APPEND t_doc_fat_ref.
  ENDLOOP.

  IF cab_due-performance EQ abap_true.

    CLEAR: t_doc_fat_ref.

    t_doc_fat_ref-docnum_exp     = p_saida_0120-docnum.
    t_doc_fat_ref-docnum         = _wl_lin_exp_venda-docnum.
    t_doc_fat_ref-menge          = _wl_lin_exp_venda-menge.
    t_doc_fat_ref-peso_liq_total = _wl_lin_exp_venda-menge.
    t_doc_fat_ref-meins          = _wl_lin_exp_venda-meins.
    t_doc_fat_ref-matnr          = p_saida_0120-matnr.

    PERFORM f_atrib_ue_exp USING p_saida_0120-codigo_ncm
                        CHANGING t_doc_fat_ref-ue_exportada.

    IF t_doc_fat_ref-ue_exportada IS INITIAL.
      c_erro_proc = abap_true.
      MESSAGE s099 WITH p_saida_0120-codigo_ncm.
      RETURN.
    ENDIF.

    PERFORM f_conv_ue_exp_fat CHANGING t_doc_fat_ref.

    "Check se Documento está registrado no CCT
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = t_doc_fat_ref-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_zlest0168  = lt_zlest0168
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      t_doc_fat_ref-registro_cct = abap_true.
    ENDIF.

    APPEND t_doc_fat_ref.

  ENDIF.

  LOOP AT t_doc_fat_ref.
    CLEAR: wa_saida_0123.

    wa_saida_0123-id_due_item   = p_saida_0120-id_due_item.
    wa_saida_0123-ue_exportada  = p_saida_0120-ue_exportada.

    CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
      EXPORTING
        i_docnum             = t_doc_fat_ref-docnum
        i_ck_entrada_propria = abap_true
      IMPORTING
        e_chave_nfe          = v_chave_nfe
        e_chave_nff          = v_chave_nff
        e_emissor_cnpj       = wa_saida_0123-emissor_cnpj
        e_emissor_cpf        = wa_saida_0123-emissor_cpf
        e_emissor_ie         = wa_saida_0123-emissor_ie
        e_regio              = wa_saida_0123-regio
        e_nfyear             = wa_saida_0123-nfyear
        e_nfmonth            = wa_saida_0123-nfmonth
        e_model              = wa_saida_0123-model
        e_serie              = wa_saida_0123-serie
        e_nfnum9             = wa_saida_0123-nfnum9
        e_nfnum              = wa_saida_0123-nfnum
        e_docnum9            = wa_saida_0123-docnum9
        e_cdv                = wa_saida_0123-cdv
        e_retorno            = v_retorno_proc.

    IF v_retorno_proc-type EQ 'E'.
      c_erro_proc = abap_true.
      MESSAGE v_retorno_proc-texto TYPE 'S'.
      RETURN.
    ENDIF.

    IF v_chave_nfe IS NOT INITIAL.
      wa_saida_0123-id_fatura_ref = v_chave_nfe.
    ELSE.
      wa_saida_0123-id_fatura_ref = v_chave_nff.
    ENDIF.

    IF wa_saida_0123-id_fatura_ref IS INITIAL.
      c_erro_proc = abap_true.
      MESSAGE s084.
      RETURN.
    ENDIF.

    wa_saida_0123-id_fatura          = 1.

    IF due_default-fatura_tp_codigo IS NOT INITIAL.
      wa_saida_0123-fatura_tp_codigo = due_default-fatura_tp_codigo.
    ENDIF.

    wa_saida_0123-docnum            = t_doc_fat_ref-docnum.
    wa_saida_0123-registro_cct      = t_doc_fat_ref-registro_cct.
    wa_saida_0123-nf_produtor       = t_doc_fat_ref-nf_produtor.
    wa_saida_0123-entrada_propria   = t_doc_fat_ref-entrada_propria.
    wa_saida_0123-qtde_ue_exportada = t_doc_fat_ref-qtde_ue_exportada.
    wa_saida_0123-peso_liq_total    = t_doc_fat_ref-peso_liq_total.

    IF wa_saida_0123-qtde_ue_exportada IS INITIAL.
      c_erro_proc = abap_true.
      MESSAGE s100 WITH t_doc_fat_ref-docnum.
      RETURN.
    ENDIF.

    IF wa_saida_0123-emissor_cnpj IS NOT INITIAL.
      wa_saida_0123-emissor_cnpj_cpf = wa_saida_0123-emissor_cnpj.
    ELSEIF wa_saida_0123-emissor_cpf IS NOT INITIAL.
      wa_saida_0123-emissor_cnpj_cpf = wa_saida_0123-emissor_cpf.
    ENDIF.

    IF ( wa_saida_0123-nfnum9 IS INITIAL ) AND ( wa_saida_0123-nfnum IS NOT INITIAL ).
      wa_saida_0123-nfnum9 = wa_saida_0123-nfnum.
    ENDIF.

    APPEND wa_saida_0123 TO it_saida_0123.
  ENDLOOP.

ENDFORM.

FORM f_inf_dt_reg_portal .

  IF cab_due-lcto_avulso IS INITIAL.
    MESSAGE 'Operação não permitida para lançamento com transmissão via SAP!' TYPE 'S'.
    EXIT.
  ENDIF.

  vg_dt_reg_portal = cab_due-dt_registro_portal.

  CALL SCREEN 0111 STARTING AT 12 05.

ENDFORM.

FORM f_retransmitir.

  DATA: zcl_token_siscomex TYPE REF TO zcl_token_siscomex,
        zcl_due            TYPE REF TO zcl_due.

  CREATE OBJECT zcl_token_siscomex.

  zcl_token_siscomex->zif_cadastro~novo_registro( ).
  zcl_token_siscomex->set_bukrs( cab_due-bukrs ).
  zcl_token_siscomex->set_role_type( 'IMPEXP' ).

  zcl_token_siscomex->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

  CHECK _gravou IS NOT INITIAL.

  FREE zcl_due.
  CREATE OBJECT zcl_due
    EXPORTING
      i_id_due = cab_due-id_due.

  zcl_due->set_token( zcl_token_siscomex ). "Set token para Validação.

  TRY .
      DATA(_enviada) = zcl_due->enviar_due( i_retransmissao = abap_true  ).
      IF _enviada EQ abap_true.
        MESSAGE 'DU-e(s) Retransmitida(s) com sucesso!' TYPE 'S'.
      ENDIF.
    CATCH zcx_due INTO DATA(ex_due).
      CLEAR: _enviada.
      ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

ENDFORM.

FORM f_confirm_screen_0200.

  IF gwa_nomeacao_sol_ov-id_nomeacao_tran IS INITIAL.
    PERFORM f_confirm_screen_0200_new.
  ELSE.
    PERFORM f_confirm_screen_0200_modify.
  ENDIF.

ENDFORM.

FORM f_confirm_screen_0200_new.

  DATA: lwa_znom_cad_transpt TYPE znom_cad_transpt,
        lwa_znom_transporte  TYPE znom_transporte,
        lva_new_cad_transp   TYPE c.

  CLEAR: lwa_znom_cad_transpt, lwa_znom_transporte, lva_new_cad_transp.

  SHIFT gwa_nomeacao_sol_ov-ds_nome_transpor LEFT DELETING LEADING space.

  IF ( gwa_nomeacao_sol_ov-ds_nome_transpor IS INITIAL ).
    MESSAGE i152.
    EXIT.
  ENDIF.

  IF gwa_nomeacao_sol_ov-id_transporte IS NOT INITIAL.

    SELECT SINGLE *
      FROM znom_cad_transpt INTO lwa_znom_cad_transpt
     WHERE id_transporte EQ gwa_nomeacao_sol_ov-id_transporte.

  ENDIF.

  IF lwa_znom_cad_transpt-id_transporte IS INITIAL.

    SELECT SINGLE *
      FROM znom_cad_transpt INTO lwa_znom_cad_transpt
     WHERE ds_nome_transpor EQ gwa_nomeacao_sol_ov-ds_nome_transpor.

  ENDIF.

  IF lwa_znom_cad_transpt-id_transporte IS INITIAL.

    SELECT MAX( id_transporte )
      FROM znom_cad_transpt INTO @DATA(_id_transporte).

    ADD 1 TO _id_transporte.

    lwa_znom_cad_transpt-id_transporte = _id_transporte.

    lva_new_cad_transp = abap_true.

  ENDIF.

*-------------------------------------------------------------------------------------------*
* Gravar ZNOM_CAD_TRANSPT
*-------------------------------------------------------------------------------------------*

  lwa_znom_cad_transpt-ds_nome_transpor = gwa_nomeacao_sol_ov-ds_nome_transpor.

  IF ( lwa_znom_cad_transpt-id_transporte IS INITIAL ) OR ( lwa_znom_cad_transpt-ds_nome_transpor IS INITIAL ).
    MESSAGE i153.
    EXIT.
  ENDIF.

  IF lva_new_cad_transp EQ abap_true.
    lwa_znom_cad_transpt-tipo             = 'M'.
    lwa_znom_cad_transpt-status           = 'A'.
    lwa_znom_cad_transpt-usnam            = sy-uname.
    lwa_znom_cad_transpt-data_atual       = sy-datum.
    lwa_znom_cad_transpt-hora_atual       = sy-uzeit.

    MODIFY znom_cad_transpt FROM lwa_znom_cad_transpt.
  ENDIF.

*-------------------------------------------------------------------------------------------*
* Gravar ZNOM_TRANSPORTE
*-------------------------------------------------------------------------------------------*

  IF gwa_nomeacao_sol_ov-nr_qtde_nomeada <= 0.
    ROLLBACK WORK.
    MESSAGE i154 WITH gwa_nomeacao_sol_ov-nro_sol_ov.
    EXIT.
  ENDIF.

  IF gwa_nomeacao_sol_ov-vkorg IS INITIAL.
    ROLLBACK WORK.
    MESSAGE i155 WITH gwa_nomeacao_sol_ov-nro_sol_ov.
    EXIT.
  ENDIF.

  IF gwa_nomeacao_sol_ov-werks IS INITIAL.
    ROLLBACK WORK.
    MESSAGE i156 WITH gwa_nomeacao_sol_ov-nro_sol_ov.
    EXIT.
  ENDIF.

  IF gwa_nomeacao_sol_ov-nro_sol_ov IS INITIAL.
    ROLLBACK WORK.
    MESSAGE i157 WITH gwa_nomeacao_sol_ov-nro_sol_ov.
    EXIT.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZID_NOM_TR'
    IMPORTING
      number                  = lwa_znom_transporte-id_nomeacao_tran
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF lwa_znom_transporte-id_nomeacao_tran IS INITIAL.
    ROLLBACK WORK.
    MESSAGE i158 WITH 'ZID_NOM_TR'.
    EXIT.
  ENDIF.

  lwa_znom_transporte-nr_ano            = sy-datum(4).
  lwa_znom_transporte-nr_mes            = sy-datum+4(2).
  lwa_znom_transporte-id_transporte     = lwa_znom_cad_transpt-id_transporte.
  lwa_znom_transporte-ds_nome_transpor  = lwa_znom_cad_transpt-ds_nome_transpor.
  lwa_znom_transporte-nr_qtde_nomeada   = gwa_nomeacao_sol_ov-nr_qtde_nomeada.
  lwa_znom_transporte-bukrs             = gwa_nomeacao_sol_ov-vkorg.
  lwa_znom_transporte-werks             = gwa_nomeacao_sol_ov-werks.
  lwa_znom_transporte-nro_sol_ov        = gwa_nomeacao_sol_ov-nro_sol_ov.
  lwa_znom_transporte-instrucao         = gwa_nomeacao_sol_ov-instrucao.

  MODIFY znom_transporte FROM lwa_znom_transporte.

  LOOP AT git_zsdt0053 INTO DATA(lwa_zsdt0053).
    lwa_zsdt0053-id_nomeacao_tran = lwa_znom_transporte-id_nomeacao_tran.
    MODIFY zsdt0053 FROM lwa_zsdt0053.
  ENDLOOP.

  MESSAGE |Nomeação { lwa_znom_transporte-id_nomeacao_tran } cadastrada com sucesso! |  TYPE 'S'.

  COMMIT WORK.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_confirm_screen_0200_modify.

  DATA: lit_zsdt0170 TYPE TABLE OF zsdt0170,
        lit_zsdt0172 TYPE TABLE OF zsdt0172,
        lit_zsdt0174 TYPE TABLE OF zsdt0174.

  DATA: lva_menge_due TYPE zsdt0172-peso_liq_total.

  CLEAR: lit_zsdt0170[], lit_zsdt0172[], lit_zsdt0174[], lva_menge_due.

  IF ( gwa_nomeacao_sol_ov-id_nomeacao_tran IS INITIAL ).
    MESSAGE i159 WITH 'Id. Nomeação'.
    EXIT.
  ENDIF.

  IF ( gwa_nomeacao_sol_ov-numero_ruc IS INITIAL ).
    MESSAGE i159 WITH 'Numero RUC'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zsdt0170 INTO TABLE lit_zsdt0170
   WHERE numero_ruc EQ gwa_nomeacao_sol_ov-numero_ruc.

  DELETE lit_zsdt0170 WHERE loekz  EQ abap_true.
  DELETE lit_zsdt0170 WHERE status NE '1'.

  IF lit_zsdt0170[] IS INITIAL.
    MESSAGE i160 WITH 'Registro da DU-e com RUC' gwa_nomeacao_sol_ov-numero_ruc.
    EXIT.
  ENDIF.

  READ TABLE lit_zsdt0170 INTO DATA(lwa_zsdt0170) INDEX 1.

  SELECT *
    FROM zsdt0172 INTO TABLE lit_zsdt0172
   WHERE id_due EQ lwa_zsdt0170-id_due.

  IF lit_zsdt0172[] IS INITIAL.
    MESSAGE i160 WITH 'Registro de Item da DU-e com RUC' gwa_nomeacao_sol_ov-numero_ruc.
    EXIT.
  ENDIF.

  LOOP AT lit_zsdt0172 INTO DATA(lwa_zsdt0172).
    ADD lwa_zsdt0172-peso_liq_total TO lva_menge_due.
  ENDLOOP.

  IF lva_menge_due IS INITIAL.
    MESSAGE i160 WITH 'Quantidade DU-e com RUC' gwa_nomeacao_sol_ov-numero_ruc.
    EXIT.
  ENDIF.

  SELECT *
    FROM zsdt0174 INTO TABLE lit_zsdt0174
   WHERE id_due EQ lwa_zsdt0170-id_due.

  IF lit_zsdt0174[] IS INITIAL.
    MESSAGE i160 WITH 'Registro Pais Destino da DU-e com RUC' gwa_nomeacao_sol_ov-numero_ruc.
    EXIT.
  ENDIF.

  READ TABLE lit_zsdt0174 INTO DATA(lwa_zsdt0174) INDEX 1.
  IF NOT ( ( sy-subrc EQ 0 ) AND ( lwa_zsdt0174-destino_country IS NOT INITIAL ) ).
    MESSAGE i160 WITH 'Registro Pais Destino da DU-e com RUC' gwa_nomeacao_sol_ov-numero_ruc.
    EXIT.
  ENDIF.


  IF ( gwa_nomeacao_sol_ov-booking   IS INITIAL ) AND
     ( gwa_nomeacao_sol_ov-dt_data   IS INITIAL ) AND
     ( gwa_nomeacao_sol_ov-nr_conhec IS INITIAL ).
    MESSAGE i161.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM znom_transporte INTO @DATA(lwa_znom_transporte)
   WHERE id_nomeacao_tran EQ @gwa_nomeacao_sol_ov-id_nomeacao_tran.

  IF sy-subrc NE 0.
    MESSAGE i160 WITH 'Registro nomeação' gwa_nomeacao_sol_ov-id_nomeacao_tran.
    EXIT.
  ENDIF.

  lwa_znom_transporte-booking = gwa_nomeacao_sol_ov-booking.
  MODIFY znom_transporte FROM lwa_znom_transporte.

  IF gwa_nomeacao_sol_ov-id_conhec IS NOT INITIAL.

    SELECT SINGLE *
      FROM znom_conhec INTO @DATA(lwa_znom_conhec)
     WHERE id_conhec        EQ @gwa_nomeacao_sol_ov-id_conhec
       AND id_nomeacao_tran EQ @gwa_nomeacao_sol_ov-id_nomeacao_tran.

    IF sy-subrc NE 0.
      MESSAGE i160 WITH 'Registro Conhecimento Transporte' gwa_nomeacao_sol_ov-id_conhec.
      EXIT.
    ENDIF.

  ELSE.

    CLEAR: lwa_znom_conhec.

    lwa_znom_conhec-id_nomeacao_tran = gwa_nomeacao_sol_ov-id_nomeacao_tran.
    lwa_znom_conhec-ds_tipo          = 'B/L'.
    lwa_znom_conhec-nr_qtde          = lva_menge_due.
    lwa_znom_conhec-sg_pais_destino  = lwa_zsdt0174-destino_country.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZID_BL'
      IMPORTING
        number                  = lwa_znom_conhec-id_conhec
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF lwa_znom_conhec-id_conhec IS INITIAL.
      ROLLBACK WORK.
      MESSAGE i158 WITH 'ZID_BL'.
      EXIT.
    ENDIF.

  ENDIF.

  lwa_znom_conhec-dt_data   = gwa_nomeacao_sol_ov-dt_data.
  lwa_znom_conhec-nr_conhec = gwa_nomeacao_sol_ov-nr_conhec.

  MODIFY znom_conhec FROM lwa_znom_conhec.

  MESSAGE |Nomeação { gwa_nomeacao_sol_ov-id_nomeacao_tran } atualizada com sucesso! |  TYPE 'S'.

  COMMIT WORK.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_raise_geral_due USING p_msg.

  RAISE EXCEPTION TYPE zcx_due
    EXPORTING
      textid = VALUE #( msgid = zcx_due=>zcx_erro_geral-msgid
                        msgno = zcx_due=>zcx_erro_geral-msgno
                        attr1 = p_msg )
      msgid  = zcx_due=>zcx_erro_geral-msgid
      msgno  = zcx_due=>zcx_erro_geral-msgno
      msgv1  = p_msg
      msgty  = 'E'.

ENDFORM.

FORM f_conv_ue_siscomex  USING p_ue_siscomex
                      CHANGING c_ue_exportada.

  CLEAR: c_ue_exportada.

  CASE p_ue_siscomex.
    WHEN 'TONEL.METR.LIQ.'.
      c_ue_exportada = 'TON'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_fat_cons_pre_acd.

  TYPES: BEGIN OF ty_chaves,
           chave TYPE c LENGTH 44,
         END OF ty_chaves.

  DATA: lit_chaves  TYPE TABLE OF ty_chaves.

  DATA: it_nfe_cons TYPE zde_chave_doc_e_t,
        wl_nfe_cons TYPE zde_chave_doc_e.

  DATA: l_erro_autenticacao  TYPE char01,
        l_consulta_realizada TYPE char01.

  CLEAR: it_nfe_cons[].

  LOOP AT it_saida_0123 INTO wa_saida_0123.
    CHECK strlen( wa_saida_0123-id_fatura_ref ) = 44.
    wl_nfe_cons = wa_saida_0123-id_fatura_ref.
    APPEND wl_nfe_cons TO it_nfe_cons.
  ENDLOOP.

  CHECK it_nfe_cons[] IS NOT INITIAL.

  CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
    EXPORTING
      i_chaves             = it_nfe_cons
      i_force_consulta     = abap_true
    IMPORTING
      e_erro_autenticacao  = l_erro_autenticacao
      e_consulta_realizada = l_consulta_realizada.

  CHECK l_erro_autenticacao  = abap_false AND
        l_consulta_realizada = abap_true.

  CLEAR: lit_chaves[].
  LOOP AT it_nfe_cons INTO DATA(lwa_chave_cons).
    APPEND VALUE #( chave = lwa_chave_cons ) TO lit_chaves.
  ENDLOOP.

  SORT lit_chaves BY chave.
  DELETE ADJACENT DUPLICATES FROM lit_chaves COMPARING chave.

  SELECT *
    FROM zlest0186 INTO TABLE @DATA(lit_zlest0186)
     FOR ALL ENTRIES IN @lit_chaves
   WHERE chave EQ @lit_chaves-chave.

  CLEAR: it_saida_pre_acd_nf[].

  LOOP AT lit_chaves INTO DATA(lwa_chave).
    CLEAR: wa_saida_pre_acd_nf.

    wa_saida_pre_acd_nf-chave = lwa_chave-chave.

    READ TABLE lit_zlest0186 INTO DATA(lwa_zlest0186) WITH KEY chave = wa_saida_pre_acd_nf-chave.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING lwa_zlest0186 TO wa_saida_pre_acd_nf.
    ENDIF.

    LOOP AT it_saida_0123 INTO wa_saida_0123 WHERE id_fatura_ref = wa_saida_pre_acd_nf-chave.
      ADD wa_saida_0123-qtde_ue_exportada TO wa_saida_pre_acd_nf-qtde_ue_due.
      wa_saida_pre_acd_nf-cct_due = wa_saida_0123-registro_cct.
    ENDLOOP.

    wa_saida_pre_acd_nf-saldo_pos_ret = wa_saida_pre_acd_nf-saldo - wa_saida_pre_acd_nf-qtde_ue_due.

    APPEND wa_saida_pre_acd_nf TO it_saida_pre_acd_nf.

  ENDLOOP.

  REFRESH estrutura.

  PERFORM f_montar_estrutura USING:

      01   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'CHAVE'              'Chave NF-e'             '44' '' '' '' '',
      01   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'CCT_DUE'            'CCT DU-e'               '08' '' '' '' '',
      01   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'CODIGO_URF'         'Codigo URF'             '10' '' '' '' '',
      02   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'CODIGO_RA'          'Codigo RA'              '10' '' '' '' '',
      03   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'LATITUDE'           'Latitude'               '15' '' '' '' '',
      04   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'LONGITUDE'          'Longitude'              '15' '' '' '' '',
      05   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'CNPJ_RESPONSAVEL'   'CNPJ Resp.'             '14' '' '' '' '',
      06   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'DT_RECEPCAO'        'Dt.Recepcão'            '13' '' '' '' '',
      07   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'PESO_AFERIDO'       'Peso Aferido'           '13' '' '' '' '',
      09   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'DT_REGISTRO'        'Dt.Registro'            '12' '' '' '' '',
      10   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'HR_REGISTRO'        'Hr.Registro'            '12' '' '' '' '',
      11   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'US_REGISTRO'        'Us.Registro'            '12' '' '' '' '',
      12   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'SALDO'              'Saldo Fiscal CCT'       '16' '' '' '' '',
      13   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'QTDE_UE_DUE'        'Qtde UE.DU-e'           '13' '' '' '' '',
      14   ''  ''              'IT_SAIDA_PRE_ACD_NF' 'SALDO_POS_RET'      'Saldo Pós Ret.'         '14' '' '' '' ''.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 190
      i_screen_end_line     = 20
    TABLES
      t_outtab              = it_saida_pre_acd_nf.


ENDFORM.


FORM f_selecionar_dados_0201.

  DATA: lit_zsdt0170_ret TYPE TABLE OF zsdt0170.

  RANGES: lra_ano                 FOR zsdt0289-ano,
          lra_kunnr_exp           FOR zsdt0289-kunnr_exp,
          lra_matnr               FOR zsdt0289-matnr,
          lra_codigo_ra           FOR zsdt0289-codigo_ra,
          lra_id_nomeacao_tran    FOR zsdt0289-id_nomeacao_tran.

  CLEAR: it_saida_0201[], lra_ano[], lra_kunnr_exp[],lra_matnr[], lra_codigo_ra[], lit_zsdt0170_ret[].

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_dados_geracao_ruc-kunnr_exp
    IMPORTING
      output = gwa_dados_geracao_ruc-kunnr_exp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_dados_geracao_ruc-matnr
    IMPORTING
      output = gwa_dados_geracao_ruc-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_dados_geracao_ruc-id_nomeacao_tran
    IMPORTING
      output = gwa_dados_geracao_ruc-id_nomeacao_tran.


  IF gwa_dados_geracao_ruc-ano IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gwa_dados_geracao_ruc-ano ) TO lra_ano.
  ENDIF.

  IF gwa_dados_geracao_ruc-kunnr_exp IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gwa_dados_geracao_ruc-kunnr_exp ) TO lra_kunnr_exp.
  ENDIF.

  IF gwa_dados_geracao_ruc-matnr IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gwa_dados_geracao_ruc-matnr ) TO lra_matnr.
  ENDIF.

  IF gwa_dados_geracao_ruc-codigo_ra IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gwa_dados_geracao_ruc-codigo_ra ) TO lra_codigo_ra.
  ENDIF.

  IF gwa_dados_geracao_ruc-id_nomeacao_tran IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gwa_dados_geracao_ruc-id_nomeacao_tran ) TO lra_id_nomeacao_tran.
  ENDIF.

  SELECT *
    FROM zsdt0289 INTO TABLE @DATA(lit_zsdt0289)
   WHERE ano              IN @lra_ano
     AND kunnr_exp        IN @lra_kunnr_exp
     AND matnr            IN @lra_matnr
     AND codigo_ra        IN @lra_codigo_ra
     AND id_nomeacao_tran IN @lra_id_nomeacao_tran.

  IF lit_zsdt0289[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0170 INTO TABLE lit_zsdt0170_ret
      FOR ALL ENTRIES IN lit_zsdt0289
     WHERE numero_ruc EQ lit_zsdt0289-numero_ruc.

    DELETE lit_zsdt0170_ret WHERE NOT ( tp_due EQ '2' AND loekz EQ abap_false ).

  ENDIF.

  LOOP AT lit_zsdt0289 INTO DATA(lwa_zsdt0289).
    CLEAR: wa_saida_0201.
    MOVE-CORRESPONDING lwa_zsdt0289 TO wa_saida_0201.

    READ TABLE lit_zsdt0170_ret INTO DATA(lwa_zsdt0170_ret) WITH KEY numero_ruc = wa_saida_0201-numero_ruc.
    IF sy-subrc EQ 0.
      wa_saida_0201-id_due_ret = lwa_zsdt0170_ret-id_due.
    ENDIF.

    APPEND wa_saida_0201 TO it_saida_0201.
  ENDLOOP.

  IF ck_list_all_0201 EQ abap_false.
    DELETE it_saida_0201 WHERE id_due_ret IS NOT INITIAL. "Deletar RUCs onde a DU-e já foi retificada

  ENDIF.

  SORT it_saida_0201 BY dt_registro hr_registro DESCENDING.


ENDFORM.

FORM f_fill_screen_fields_0201.

  CLEAR: gwa_dados_geracao_ruc-maktx, gwa_dados_geracao_ruc-name1, gwa_dados_geracao_ruc-ds_nome_transpor, gwa_dados_geracao_ruc-ds_ra.

  IF gwa_dados_geracao_ruc-matnr IS NOT INITIAL.
    SELECT SINGLE maktx INTO gwa_dados_geracao_ruc-maktx
      FROM makt
     WHERE matnr EQ gwa_dados_geracao_ruc-matnr
       AND spras EQ sy-langu.
  ENDIF.

  IF gwa_dados_geracao_ruc-kunnr_exp IS NOT INITIAL.
    SELECT SINGLE name1 INTO gwa_dados_geracao_ruc-name1
      FROM kna1
     WHERE kunnr EQ gwa_dados_geracao_ruc-kunnr_exp.
  ENDIF.

  IF gwa_dados_geracao_ruc-codigo_ra IS NOT INITIAL.
    SELECT SINGLE ds_ra INTO gwa_dados_geracao_ruc-ds_ra
      FROM zsdt0168
     WHERE codigo_ra EQ gwa_dados_geracao_ruc-codigo_ra.
  ENDIF.

  IF gwa_dados_geracao_ruc-id_nomeacao_tran IS NOT INITIAL.
    SELECT SINGLE ds_nome_transpor INTO gwa_dados_geracao_ruc-ds_nome_transpor
      FROM znom_transporte
     WHERE id_nomeacao_tran EQ gwa_dados_geracao_ruc-id_nomeacao_tran.
  ENDIF.


ENDFORM.

FORM f_fill_due.

  DATA: lit_xmls_exportacao TYPE znfe_xml_sefaz_auth_t.

  DATA(lva_erro_proc) = abap_false.
  PERFORM f_get_xmls_nf_exportacao TABLES lit_xmls_exportacao
                                 CHANGING lva_erro_proc.

  CHECK lva_erro_proc EQ abap_false.

  PERFORM f_fill_due_n2 TABLES lit_xmls_exportacao
                      CHANGING lva_erro_proc.

  IF lva_erro_proc EQ abap_true.
    CLEAR: it_saida_0120[], it_saida_0122[], it_saida_0123[], it_saida_0125[].
  ENDIF.

ENDFORM.


FORM f_load_file TABLES t_arquivos_xml STRUCTURE tg_arquivos_xml
                  USING p_patch p_file.

  DATA: xline           TYPE string,
        lit_arquivo_xml LIKE TABLE OF xline,
        st_xml          TYPE zexml,
        lva_size        TYPE i,
        lva_path_file   TYPE string.

  DATA : BEGIN OF lit_upl OCCURS 0,
           line(255) TYPE c,
         END OF lit_upl.

  DATA: lva_xml TYPE string.

  CLEAR : lit_arquivo_xml[].

  lva_path_file = p_patch && '\' && p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lva_path_file
      filetype                = 'BIN'
    IMPORTING
      filelength              = lva_size
    TABLES
      data_tab                = lit_upl
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = lva_size
    IMPORTING
      text_buffer  = lva_xml
    TABLES
      binary_tab   = lit_upl
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  APPEND VALUE #( name_file = p_file xml = lva_xml ) TO t_arquivos_xml.


ENDFORM.                    " CARREGA_ARQ


FORM f_get_xmls_nf_exportacao TABLES t_xmls_exportacao TYPE znfe_xml_sefaz_auth_t
                            CHANGING c_erro_proc.

  DATA: lwa_xml_sefaz TYPE znfe_xml_sefaz_auth.

  DATA: lit_arquivos_xml LIKE TABLE OF tg_arquivos_xml.

  DATA: t_element_array TYPE zde_element_array_t.

  CLEAR: c_erro_proc.

  DATA: lva_selected_folder   TYPE string,
        lva_selected_folder_c	TYPE c LENGTH 1000,
        c_mask_loc(6)         TYPE c VALUE '*.xml'.

  DATA: t_dir_loc_f TYPE TABLE OF sdokpath,
        t_dir_local TYPE TABLE OF sdokpath.

  CLEAR: t_xmls_exportacao[], lit_arquivos_xml[], t_dir_loc_f[], t_dir_local[].

  cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Pasta para importar os arquivos XMLs de Importação'
      CHANGING
        selected_folder      = lva_selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4  ).

  lva_selected_folder_c = lva_selected_folder.

  "LVA_SELECTED_FOLDER_C = 'C:\Amaggi\Notas Performance'. "Comentar - Codigo Temporario


  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      directory  = lva_selected_folder_c
      filter     = c_mask_loc
    TABLES
      file_table = t_dir_loc_f
      dir_table  = t_dir_local
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.


  IF sy-subrc <> 0 OR t_dir_loc_f[] IS INITIAL.
    c_erro_proc = abap_true.
    MESSAGE |Diretório Local: { lva_selected_folder } Inválido ou nenhum arquivo encontrado p/o prefixo:  { c_mask_loc } !|  TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT t_dir_loc_f INTO DATA(lwa_file_doc).
    PERFORM f_load_file TABLES lit_arquivos_xml
                         USING lva_selected_folder_c lwa_file_doc-pathname.
  ENDLOOP.

  IF lit_arquivos_xml[] IS INITIAL.
    c_erro_proc = abap_true.
    MESSAGE |Não foi ler os XMLs selecionados!|  TYPE 'I'.
    EXIT.
  ENDIF.


*---------------------------------------------------------------------------------------*
* Converter XMLs para estrutura ABAP
*---------------------------------------------------------------------------------------*

  CLEAR: t_element_array[].

  APPEND 'det' TO t_element_array.
  APPEND 'detPag' TO t_element_array.
  APPEND 'NFref' TO t_element_array.
  APPEND 'DI' TO t_element_array.
  APPEND 'adi' TO t_element_array.
  APPEND 'med' TO t_element_array.
  APPEND 'arma' TO t_element_array.
  APPEND 'comb' TO t_element_array.
  APPEND 'lacres' TO t_element_array.
  APPEND 'dup' TO t_element_array.
  APPEND 'pag' TO t_element_array.
  APPEND 'procRef' TO t_element_array.
  APPEND 'obsCont' TO t_element_array.
  APPEND 'obsFisco' TO t_element_array.
  APPEND 'vol'      TO t_element_array.
  APPEND 'detExport' TO t_element_array.

  LOOP AT lit_arquivos_xml INTO DATA(lwa_arquivo_xml).

    CLEAR: lwa_xml_sefaz.

    DATA(_json) = zcl_string=>xml_to_json( i_xml           = lwa_arquivo_xml-xml
                                           i_element_array = t_element_array ).

    IF _json IS INITIAL.
      MESSAGE |Não foi possivel ler o arquivo { lwa_arquivo_xml-name_file }! |  TYPE 'I'.
      c_erro_proc = abap_true.
      RETURN.
    ENDIF.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = _json
      CHANGING
        data = lwa_xml_sefaz.

    APPEND lwa_xml_sefaz TO t_xmls_exportacao.

  ENDLOOP.


ENDFORM.



FORM f_fill_due_n2  TABLES t_xmls_exportacao TYPE znfe_xml_sefaz_auth_t
                  CHANGING c_erro_proc.

  TYPES: BEGIN OF ty_nfs_venda,
           chave            TYPE zde_chave_doc_e,
           docnum           TYPE j_1bnfdoc-docnum,
           refkey           TYPE j_1bnflin-refkey,
           numero_ruc       TYPE zsdt0053-numero_ruc,
           codigo_ra        TYPE zsdt0053-codigo_ra,
           id_nomeacao_tran TYPE zsdt0053-id_nomeacao_tran,
           matnr            TYPE j_1bnflin-matnr,
           steuc            TYPE marc-steuc,
           ue_exportada     TYPE zsdt0172-ue_exportada,
           uc_exportada     TYPE zsdt0172-uc_exportada,
           kunnr            TYPE kna1-kunnr,
           chave_nfe_exp    TYPE zde_chave_doc_e,
         END OF ty_nfs_venda.

  DATA: lit_nfs_venda       TYPE TABLE OF ty_nfs_venda.


  DATA: lwa_retorno_proc TYPE zde_retorno_proc.

  DATA: lva_quantidade   TYPE j_1bnflin-menge,
        lva_qtde_exp_ind TYPE j_1bnflin-menge,
        lva_docnum_venda TYPE j_1bnflin-docnum,
        lva_docnum       TYPE j_1bnflin-docnum,
        lva_chave_nfe    TYPE zde_chave_doc_e.


  CLEAR: it_saida_0120[],
         it_saida_0122[],
         it_saida_0123[],
         it_saida_0125[].

  CLEAR: cab_due-xmls_exportacao[].

  CLEAR: lit_nfs_venda[].

  LOOP AT t_xmls_exportacao INTO DATA(lwa_xml_exportacao).

    CLEAR: wa_saida_0120.

    "Get ID Item DU-e
    PERFORM f_get_max_id_item TABLES it_saida_0120
                            CHANGING wa_saida_0120-id_due_item.


    "Campos Default
    IF due_default-fatura_tp_codigo IS NOT INITIAL.
      wa_saida_0120-fatura_tp_codigo = due_default-fatura_tp_codigo.
    ENDIF.

    IF due_default-fatura_motivo_dispensa_nf IS NOT INITIAL.
      wa_saida_0120-fatura_motivo_dispensa_nf = due_default-fatura_motivo_dispensa_nf.
    ENDIF.

    IF due_default-codigo_cond_venda IS NOT INITIAL.
      wa_saida_0120-codigo_cond_venda = due_default-codigo_cond_venda.
    ENDIF.

    IF due_default-codigo_enquadramento IS NOT INITIAL.
      wa_saida_0120-codigo_enquadramento = due_default-codigo_enquadramento.
    ENDIF.

    IF strlen( lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe ) NE 44.
      c_erro_proc = abap_true.
      MESSAGE |Existem XMLs com chave NF-e Invalida! protnfe/infprot/chnfe! |  TYPE 'I'.
      RETURN.
    ENDIF.

    wa_saida_0120-fatura_id =  lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe. "CHAVE NF-E EXPORTAÇÃO

    "Preencher Quantidade e Dados Material com base no Material da NF-e Referenciada no XML de exportação
    LOOP AT lwa_xml_exportacao-nfeproc-nfe-infnfe-det INTO DATA(lwa_det).

      CLEAR: lva_quantidade.

      TRANSLATE lwa_det-prod-ucom TO UPPER CASE.

      CASE lwa_det-prod-ucom.
        WHEN 'KG'.
          lva_quantidade = lwa_det-prod-qcom.
        WHEN 'TON' OR 'TO'.
          lva_quantidade = lwa_det-prod-qcom * 1000.
        WHEN OTHERS.
          c_erro_proc = abap_true.
          MESSAGE |Unidade { lwa_det-prod-ucom } do item da NF-e { lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } é desconhecida! |  TYPE 'I'.
          RETURN.
      ENDCASE.

      ADD lva_quantidade TO wa_saida_0120-peso_liq_total.

      LOOP AT lwa_det-prod-detexport INTO DATA(lwa_detexport).

        lva_qtde_exp_ind = lwa_detexport-exportind-qexport.

        IF strlen( lwa_detexport-exportind-chnfe ) NE 44. "NF-e Venda
          c_erro_proc = abap_true.
          MESSAGE |NF-e { lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe } referencia Notas Invalidas em Detalhes Exportação do Item! |  TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM kna1 INTO @DATA(lwa_kna1_emissor)
         WHERE stcd1 EQ @lwa_detexport-exportind-chnfe+6(14)
*** Stefanini - IR256727 - 05/09/2025 - LAZAROSR - Início de Alteração
           AND ktokd EQ 'ZCIC'.
*** Stefanini - IR256727 - 05/09/2025 - LAZAROSR - Fim de Alteração

        IF ( sy-subrc NE 0 ) .
          c_erro_proc = abap_true.
          MESSAGE i171(zdue) WITH lwa_detexport-exportind-chnfe.
          RETURN.
        ENDIF.

        CHECK lwa_kna1_emissor-ktokd EQ 'ZCIC'. "Não validar notas emitidas pelo Teceiro.

        lva_chave_nfe = lwa_detexport-exportind-chnfe.

        CALL FUNCTION 'ZDUE_GET_DOC_FISCAL'
          EXPORTING
            i_chave   = lva_chave_nfe
            i_direct  = '2' "Direção
            i_propria = 'X' "Propria
          IMPORTING
            e_docnum  = lva_docnum.

        IF lva_docnum IS INITIAL.
          c_erro_proc = abap_true.
          MESSAGE |Não localizado no SAP o documento fiscal com chave { lwa_detexport-exportind-chnfe }! |  TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt_retlote INTO @DATA(lwa_zsdt_retlote)
         WHERE docnum EQ @lva_docnum.

        CHECK ( sy-subrc NE 0 ). "Formaçao de Lote deve ignorar..

        lva_docnum_venda = lva_docnum.

        SELECT SINGLE *
          FROM j_1bnflin INTO @DATA(lwa_lin_venda)
         WHERE docnum EQ @lva_docnum_venda.

        IF ( sy-subrc NE 0 ) OR ( lwa_lin_venda-matnr IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Não localizado o item do documento fiscal: { lva_docnum_venda }! |  TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM j_1bnfdoc INTO @DATA(lwa_doc_venda)
         WHERE docnum EQ @lva_docnum_venda.

        IF ( sy-subrc NE 0 ) OR ( lwa_lin_venda-matnr IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Não localizado o cabeçalho do documento fiscal: { lva_docnum_venda }! |  TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM vbrp INTO @DATA(lwa_vbrp)
         WHERE vbeln EQ @lwa_lin_venda-refkey.

        IF ( sy-subrc NE 0 ) OR ( lwa_lin_venda-refkey IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Não localizado a fatura(VF) do documento fiscal: { lwa_lin_venda-docnum }! |  TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt0053 INTO @DATA(lwa_zsdt0053)
         WHERE remessa_exp EQ @lwa_vbrp-vgbel.

        IF ( sy-subrc NE 0 ) OR ( lwa_vbrp-vgbel IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Solicitação Ordem Venda(ZSDT0053) Remessa { lwa_vbrp-vgbel } não foi encontrada!| TYPE 'S'.
          RETURN.
        ENDIF.

        IF ( lwa_zsdt0053-id_nomeacao_tran IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Solicitação Ordem Venda { lwa_zsdt0053-nro_sol_ov } Remessa { lwa_vbrp-vgbel } sem Id. Nomeação!| TYPE 'S'.
          RETURN.
        ENDIF.

        IF ( lwa_zsdt0053-codigo_ra IS INITIAL ).
          c_erro_proc = abap_true.
          MESSAGE |Solicitação Ordem Venda { lwa_zsdt0053-nro_sol_ov } Remessa { lwa_vbrp-vgbel } sem Codigo RA!| TYPE 'S'.
          RETURN.
        ENDIF.


        SELECT SINGLE *
          FROM mara INTO @DATA(wl_mara)
         WHERE matnr EQ @lwa_lin_venda-matnr.

        IF ( sy-subrc NE 0 ).
          c_erro_proc = abap_true.
          MESSAGE |Material { lwa_lin_venda-matnr   } não encontrado! | TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM makt INTO @DATA(wl_makt)
         WHERE matnr EQ @lwa_lin_venda-matnr
           AND spras EQ @sy-langu.

        IF ( sy-subrc NE 0 ).
          c_erro_proc = abap_true.
          MESSAGE |Material { lwa_lin_venda-matnr   } não encontrado! | TYPE 'I'.
          RETURN.
        ENDIF.

        IF wl_mara-meins IS INITIAL.
          c_erro_proc = abap_true.
          MESSAGE |Material { lwa_lin_venda-matnr } sem unidade de Medida!! | TYPE 'I'.
          RETURN.
        ENDIF.

        SELECT SINGLE *
          FROM marc AS a INTO @DATA(wl_marc)
         WHERE a~matnr EQ @wl_mara-matnr
           AND a~steuc NE @space
           AND EXISTS ( SELECT b~centrov_1
                          FROM zsdt_depara_cen AS b
                         WHERE b~centrov_1 EQ a~werks ).

        IF sy-subrc NE 0.
          c_erro_proc = abap_true.
          MESSAGE |NCM do Material { wa_saida_0120-matnr } não encontrado! | TYPE 'S'.
          RETURN.
        ENDIF.

        wa_saida_0120-matnr           = wl_mara-matnr.
        wa_saida_0120-uc_exportada    = wl_mara-meins.
        wa_saida_0120-desc_mercadoria = wl_makt-maktx.
        wa_saida_0120-codigo_ncm      = wl_marc-steuc.

        PERFORM f_atrib_ue_exp USING wa_saida_0120-codigo_ncm
                            CHANGING wa_saida_0120-ue_exportada.

        APPEND VALUE #( chave                = lwa_detexport-exportind-chnfe
                        docnum               = lwa_lin_venda-docnum
                        refkey               = lwa_lin_venda-refkey
                        numero_ruc           = lwa_zsdt0053-numero_ruc
                        codigo_ra            = lwa_zsdt0053-codigo_ra
                        id_nomeacao_tran     = lwa_zsdt0053-id_nomeacao_tran
                        matnr                = wa_saida_0120-matnr
                        steuc                = wa_saida_0120-codigo_ncm
                        ue_exportada         = wa_saida_0120-ue_exportada
                        uc_exportada         = wa_saida_0120-uc_exportada
                        kunnr                = lwa_doc_venda-parid
                        chave_nfe_exp        = lwa_xml_exportacao-nfeproc-protnfe-infprot-chnfe ) TO lit_nfs_venda.

      ENDLOOP.

    ENDLOOP.

    "Converte p/ Quantidade Estatistica
    PERFORM f_conv_quantidade  USING  wa_saida_0120-matnr
                                      wa_saida_0120-peso_liq_total "Peso Origem
                                      'KG'                         "Unid. Peso Origem
                                      wa_saida_0120-ue_exportada   "Unid. Peso Destino
                             CHANGING wa_saida_0120-qtde_ue_exportada "Peso Convertido
                                      c_erro_proc.
    IF c_erro_proc EQ abap_true.
      RETURN.
    ENDIF.

    "Converte p/ Quantidade Comercializada
    PERFORM f_conv_quantidade  USING  wa_saida_0120-matnr
                                      wa_saida_0120-peso_liq_total "Peso Origem
                                      'KG'                         "Unid. Peso Origem
                                      wa_saida_0120-uc_exportada   "Unid. Peso Destino
                             CHANGING wa_saida_0120-qtde_uc_exportada "Peso Convertido
                                      c_erro_proc.
    IF c_erro_proc EQ abap_true.
      RETURN.
    ENDIF.

    wa_saida_0120-importador_codigo = '0000300279'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_0120-importador_codigo
      IMPORTING
        output = wa_saida_0120-importador_codigo.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(_wl_kna1)
     WHERE kunnr EQ @wa_saida_0120-importador_codigo.

    IF sy-subrc NE 0.
      c_erro_proc = abap_true.
      MESSAGE |Cliente Importador { wa_saida_0120-importador_codigo } não encontrado! | TYPE 'S'.
      RETURN.
    ELSE.
      wa_saida_0120-importador_nome = _wl_kna1-name1.

      IF _wl_kna1-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc INTO @DATA(_wl_adrc)
         WHERE addrnumber EQ @_wl_kna1-adrnr.

        IF sy-subrc = 0.
          wa_saida_0120-importador_country  = _wl_adrc-country.
          CONCATENATE _wl_adrc-street _wl_adrc-city2 '/' _wl_adrc-city1
                 INTO wa_saida_0120-importador_cpl_end SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT lit_nfs_venda INTO DATA(lwa_nf_venda) WHERE chave_nfe_exp EQ wa_saida_0120-fatura_id.
      PERFORM f_preencher_nf_item_due_n2 USING  wa_saida_0120
                                                lwa_nf_venda-docnum
                                       CHANGING c_erro_proc.

      IF c_erro_proc EQ abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    APPEND wa_saida_0120 TO it_saida_0120.

  ENDLOOP.


  DATA(lit_nfs_venda_aux) = lit_nfs_venda[].

  SORT lit_nfs_venda     BY numero_ruc codigo_ra matnr steuc ue_exportada uc_exportada id_nomeacao_tran kunnr.
  SORT lit_nfs_venda_aux BY numero_ruc codigo_ra matnr steuc ue_exportada uc_exportada id_nomeacao_tran kunnr.
  DELETE ADJACENT DUPLICATES FROM lit_nfs_venda_aux COMPARING numero_ruc codigo_ra matnr steuc ue_exportada uc_exportada id_nomeacao_tran kunnr .

  IF lines( lit_nfs_venda_aux ) > 1 .
    c_erro_proc = abap_true.
    MESSAGE |DU-e possui NFs de Vendas com caracteristicas diferentes! | TYPE 'I'.

    REFRESH estrutura.

    PERFORM f_montar_estrutura USING:

        01   ''  ''              'LIT_NFS_VENDA' 'CHAVE'              'Chave NF-e'               '44' '' '' '' '',
        01   ''  ''              'LIT_NFS_VENDA' 'DOCNUM'             'Docnum'                   '08' '' '' '' '',
        02   ''  ''              'LIT_NFS_VENDA' 'NUMERO_RUC'         'RUC'                      '10' '' '' '' '',
        03   ''  ''              'LIT_NFS_VENDA' 'CODIGO_RA'          'Codigo RA.'               '15' '' '' '' '',
        04   ''  ''              'LIT_NFS_VENDA' 'MATNR'              'Material'                 '15' '' '' '' '',
        05   ''  ''              'LIT_NFS_VENDA' 'STEUC'              'NCM'                      '14' '' '' '' '',
        06   ''  ''              'LIT_NFS_VENDA' 'UE_EXPORTADA'       'Unid. Estatistica'        '20' '' '' '' '',
        07   ''  ''              'LIT_NFS_VENDA' 'UC_EXPORTADA'       'Unid. Comercializada'     '20' '' '' '' '',
        08   ''  ''              'LIT_NFS_VENDA' 'ID_NOMEACAO_TRAN'   'Id.Nomeação'              '11' '' '' '' '',
        09   ''  ''              'LIT_NFS_VENDA' 'KUNNR'              'Cliente'                  '10' '' '' '' ''.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat           = estrutura[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 150
        i_screen_end_line     = 20
      TABLES
        t_outtab              = lit_nfs_venda.

    RETURN.
  ENDIF.

  READ TABLE lit_nfs_venda_aux INTO lwa_nf_venda INDEX 1.

  SELECT SINGLE *
    FROM zsdt0169 INTO @DATA(lwa_zsdt0169)
   WHERE codigo_ra EQ @lwa_nf_venda-codigo_ra.

  IF ( sy-subrc NE 0 ) OR ( lwa_zsdt0169-codigo_urf IS INITIAL ) OR ( lwa_nf_venda-codigo_ra IS INITIAL  ).
    c_erro_proc = abap_true.
    MESSAGE |Não localizado o Depara do Codigo RA { lwa_nf_venda-codigo_ra } na transação ZSDT0137! | TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM kna1 INTO @DATA(lwa_kna1_venda)
   WHERE kunnr EQ @lwa_nf_venda-kunnr.

  IF ( sy-subrc NE 0 ) .
    c_erro_proc = abap_true.
    MESSAGE |Não localizado o cadastro do cliente { lwa_nf_venda-kunnr } na transação XD03! | TYPE 'S'.
    RETURN.
  ENDIF.

  IF ( lwa_kna1_venda-stcd1 IS INITIAL ) .
    c_erro_proc = abap_true.
    MESSAGE |Não localizado o CNPJ no cadastro do cliente { lwa_nf_venda-kunnr } na transação XD03! | TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM znom_transporte INTO @DATA(lwa_znom_transporte)
   WHERE id_nomeacao_tran EQ @lwa_nf_venda-id_nomeacao_tran.

  IF ( sy-subrc NE 0 ) OR ( lwa_znom_transporte-ds_nome_transpor IS INITIAL ).
    c_erro_proc = abap_true.
    MESSAGE |Não localizado a nomeação com ID { lwa_nf_venda-id_nomeacao_tran }! | TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: cab_due-bukrs, cab_due-numero_due, cab_due-regio, cab_due-tp_exportacao.

  cab_due-id_nomeacao_tran        = lwa_nf_venda-id_nomeacao_tran.
  cab_due-kunnr                   = lwa_nf_venda-kunnr.
  cab_due-cnpj_declarante         = lwa_kna1_venda-stcd1.
  cab_due-numero_ruc              = lwa_nf_venda-numero_ruc.
  cab_due-codigo_ra_despacho      = lwa_nf_venda-codigo_ra.
  cab_due-codigo_ra_embarque      = lwa_nf_venda-codigo_ra.
  cab_due-codigo_urf_despacho     = lwa_zsdt0169-codigo_urf.
  cab_due-codigo_urf_embarque     = lwa_zsdt0169-codigo_urf.
  cab_due-tp_cod_local_despacho   = '281'.
  cab_due-tp_cod_local_despacho   = '281'.
  cab_due-observacoes_gerais      = lwa_znom_transporte-ds_nome_transpor.
  cab_due-preenchimento_auto      = abap_true.
  cab_due-xmls_exportacao[]       = t_xmls_exportacao[].

  MESSAGE |DU-e preenchida com sucesso!|  TYPE 'I'.


ENDFORM.


FORM f_get_documento_fiscal USING p_chave
                                  p_direct
                                  p_nf_prop
                         CHANGING c_docnum  TYPE j_1bnfdoc-docnum.

  DATA: tg_active       TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        lva_chave       TYPE zde_chave_doc_e,
        lva_candat_null TYPE j_1bnfdoc-candat.

  RANGES: r_form FOR j_1bnfdoc-form.

  CLEAR: lva_candat_null, tg_active[], r_form[], c_docnum.

  lva_chave = p_chave.

  CHECK strlen( lva_chave ) = 44.

  IF p_nf_prop EQ abap_true.
    APPEND VALUE #( sign = 'I' option = 'NE' low = space ) TO r_form.
  ELSE.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = space ) TO r_form.
  ENDIF.

  SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @tg_active
    FROM j_1bnfe_active AS a INNER JOIN j_1bnfdoc AS b ON a~docnum = b~docnum
   WHERE a~regio    EQ @lva_chave(2)
     AND a~nfyear   EQ @lva_chave+2(2)
     AND a~nfmonth  EQ @lva_chave+4(2)
     AND a~stcd1    EQ @lva_chave+6(14)
     AND a~model    EQ @lva_chave+20(2)
     AND a~serie    EQ @lva_chave+22(3)
     AND a~nfnum9   EQ @lva_chave+25(9)
     AND a~docnum9  EQ @lva_chave+34(9)
     AND a~cdv      EQ @lva_chave+43(1)
     AND a~direct   EQ @p_direct
     AND a~form     IN @r_form
     AND a~docsta   EQ '1'
     AND a~scssta   NE '2'
     AND b~candat   EQ @lva_candat_null
     AND b~cancel   EQ @space
     AND b~doctyp   IN ('1','2','6').

  DELETE tg_active WHERE docnum IS INITIAL.

  CHECK tg_active[] IS NOT INITIAL.

  READ TABLE tg_active INDEX 1.

  CHECK ( sy-subrc EQ 0 ) AND ( tg_active-docnum IS NOT INITIAL ).

  c_docnum = tg_active-docnum.


ENDFORM.

FORM f_conv_quantidade  USING  p_matnr
                               p_peso_origem
                               p_un_peso_origem
                               p_un_peso_destino
                      CHANGING c_peso_conv
                               c_erro_proc.


  DATA: v_matnr     TYPE mara-matnr,
        v_ncm       TYPE j1b_nf_xml_item-ncm,
        v_meins_in  TYPE mara-meins,
        v_meins_out TYPE mara-meins,
        v_qtde      TYPE j1b_nf_xml_item-qtrib,
        v_vlr       TYPE zsdt0172-vlr_local_embarque.

  CLEAR: c_erro_proc.

  IF ( p_matnr            IS INITIAL ) OR
     ( p_peso_origem      IS INITIAL ) OR
     ( p_un_peso_origem   IS INITIAL ) OR
     ( p_un_peso_destino  IS INITIAL ).
    c_erro_proc = abap_true.
    MESSAGE |Quantidades não podem ser calculadas! | TYPE 'S'.
    RETURN.
  ENDIF.

  v_matnr     = p_matnr.

  v_qtde      = p_peso_origem.
  v_meins_in  = p_un_peso_origem.

  v_meins_out = p_un_peso_destino.

  IF v_meins_out = 'TON'.
    v_meins_out = 'TO'.
  ENDIF.

  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = v_matnr
      i_in_me              = v_meins_in
      i_out_me             = v_meins_out
      i_menge              = v_qtde
    IMPORTING
      e_menge              = v_qtde
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.

  IF sy-subrc = 0.
    c_peso_conv = v_qtde.
  ELSE.
    c_erro_proc = abap_true.
    MESSAGE |Quantidades não podem ser calculadas! | TYPE 'S'.
    RETURN.
  ENDIF.


ENDFORM.

FORM f_download_xml_due .

  DATA: zcl_due TYPE REF TO zcl_due.

  FREE zcl_due.
  CREATE OBJECT zcl_due
    EXPORTING
      i_id_due = cab_due-id_due.

  TRY .
      DATA(_xml) = zcl_due->get_xml_due( i_download = abap_true ).
    CATCH zcx_due INTO DATA(ex_due).
      CLEAR: _xml.
      ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  MESSAGE 'Downloads realizados com sucesso!' TYPE 'S'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_ZSDT0305
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save_zsdt0305 CHANGING lv_due TYPE any.

  " DATA: ls_zsdt0305 TYPE zsdt0305.
  "DATA: t_zsdt0305  TYPE TABLE OF zsdt0305.

*  LOOP AT it_sel_rows INTO DATA(ls_sel_rows).
*    READ TABLE it_saida_0120 INTO DATA(ls_saida_0120) INDEX ls_sel_rows-index.
*    IF sy-subrc IS INITIAL.
*
*      ls_zsdt0305-mandt              = sy-mandt.
*     ls_zsdt0305-id_due             = ls_saida_0120-id_due.
*      ls_zsdt0305-id_due             = lv_due.
*      ls_zsdt0305-id_due_item        = ls_saida_0120-id_due_item.
*      ls_zsdt0305-cod_enquadramento  = zsdt0305-cod_enquadramento.
*      ls_zsdt0305-nr_drawback        = zsdt0305-nr_drawback.
*      ls_zsdt0305-tp_ato             = zsdt0305-tp_ato.
*
*      APPEND ls_zsdt0305 TO t_zsdt0305.
*      CLEAR ls_zsdt0305.
*    ENDIF.
*  ENDLOOP.

  CLEAR: t_zsdt0305.

  LOOP AT it_saida_0120 ASSIGNING FIELD-SYMBOL(<fs_saida_0120>).

    SELECT *
       FROM zsdt0305
       INTO TABLE t_zsdt0305
       WHERE id_due_item = <fs_saida_0120>-id_due_item AND
              fatura_id = <fs_saida_0120>-fatura_id.

    IF sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
      SORT t_zsdt0305 BY id_due_item fatura_id.
*<--- 05/07/2023 - Migração S4 - DL
      READ TABLE t_zsdt0305 INTO ls_zsdt0305 WITH KEY  id_due_item = <fs_saida_0120>-id_due_item
                                                       fatura_id   = <fs_saida_0120>-fatura_id
                                                         BINARY SEARCH.
      ls_zsdt0305-id_due = lv_due.

      " MODIFY t_zsdt0305 FROM ls_zsdt0305 TRANSPORTING id_due where FATURA_ID = <fs_saida_0120>-fatura_id.
      "MODIFY zsdt0305 FROM ls_zsdt0305 TRANSPORTING id_due where FATURA_ID = <fs_saida_0120>-fatura_id.
      UPDATE zsdt0305 SET id_due = ls_zsdt0305-id_due
      WHERE   fatura_id = <fs_saida_0120>-fatura_id.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    "  CLEAR ls_zsdt0305.

  ENDLOOP.


  "IF t_zsdt0305[] IS NOT INITIAL.
*
*    LOOP AT t_zsdt0305 INTO DATA(ls_zsdt0305).
*
*      ls_zsdt0305-id_due = lv_due.
*
*      MODIFY t_zsdt0305 FROM ls_zsdt0305.
*      CLEAR ls_zsdt0305.
*
*    ENDLOOP.

*    IF zsdt0305-cod_enquadramento IS NOT INITIAL AND
*       zsdt0305-nr_drawback       IS NOT INITIAL AND
*       zsdt0305-tp_ato            IS NOT INITIAL.
  " MODIFY zsdt0305 FROM TABLE t_zsdt0305.
  " IF sy-subrc IS INITIAL.
  "    COMMIT WORK.
  "  ENDIF.
*
*        CLEAR: zsdt0305-cod_enquadramento,
*               zsdt0305-nr_drawback,
*               zsdt0305-tp_ato.
*
*        LEAVE TO SCREEN 0.
*      ENDIF.
*    ELSE.
*      DELETE zsdt0305 FROM TABLE t_zsdt0305.
*      IF sy-subrc IS INITIAL.
*        COMMIT WORK.
*      ENDIF.
*    ENDIF.
  " ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_DRAWBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save_drawback .

*  DATA: ls_zsdt0305 TYPE zsdt0305.
*  DATA: t_zsdt0305  TYPE TABLE OF zsdt0305.

  LOOP AT it_sel_rows INTO DATA(ls_sel_rows).
    READ TABLE it_saida_0120 INTO DATA(ls_saida_0120) INDEX ls_sel_rows-index.
    IF sy-subrc IS INITIAL.

      ls_zsdt0305-mandt              = sy-mandt.
      "ls_zsdt0305-id_due             = ls_saida_0120-id_due.
      ls_zsdt0305-fatura_id           = ls_saida_0120-fatura_id.
      ls_zsdt0305-id_due_item        = ls_saida_0120-id_due_item.
      ls_zsdt0305-cod_enquadramento  = zsdt0305-cod_enquadramento.
      ls_zsdt0305-nr_drawback        = zsdt0305-nr_drawback.
      ls_zsdt0305-tp_ato             = zsdt0305-tp_ato.

      APPEND ls_zsdt0305 TO t_zsdt0305.
      CLEAR ls_zsdt0305.
    ENDIF.
  ENDLOOP.


  IF t_zsdt0305[] IS NOT INITIAL.

    IF zsdt0305-cod_enquadramento IS NOT INITIAL AND
       zsdt0305-nr_drawback       IS NOT INITIAL AND
       zsdt0305-tp_ato            IS NOT INITIAL.
      MODIFY zsdt0305 FROM TABLE t_zsdt0305.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.

        CLEAR: zsdt0305-cod_enquadramento,
               zsdt0305-nr_drawback,
               zsdt0305-tp_ato.

        LEAVE TO SCREEN 0.
      ENDIF.
    ELSE.
      DELETE zsdt0305 FROM TABLE t_zsdt0305.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
