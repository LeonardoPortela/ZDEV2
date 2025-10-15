*&---------------------------------------------------------------------*
*&  Include           ZFIR0068_FORM
*&---------------------------------------------------------------------*

FORM config_ranges .

  REFRESH: it_bukrs,
           it_dep_resp,
           it_dt_vcto.

  "----------------------------------------------------
  " Empresa
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_bukrs-low
    IMPORTING
      output = p_bukrs-low.

  IF ( p_bukrs-low  IS NOT INITIAL ) AND
     ( p_bukrs-high IS NOT INITIAL ).
    it_bukrs-sign   = 'I'.
    it_bukrs-option = 'BT'.
    it_bukrs-low  = p_bukrs-low.
    it_bukrs-high = p_bukrs-high .
    APPEND it_bukrs.
  ELSEIF ( p_bukrs-low IS NOT INITIAL ).
    it_bukrs-sign   = 'I'.
    it_bukrs-option = 'EQ'.
    it_bukrs-low  = p_bukrs-low.
    it_bukrs-high = p_bukrs-low.
    APPEND it_bukrs.
  ENDIF.

  "----------------------------------------------------
  " Departamento
  "----------------------------------------------------
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_dep_resp-low
    IMPORTING
      output = p_dep_resp-low.

  IF ( p_dep_resp-low  IS NOT INITIAL ).
    it_dep_resp-sign   = 'I'.
    it_dep_resp-option = 'EQ'.
    it_dep_resp-low  = p_dep_resp-low.
    it_dep_resp-high = p_dep_resp-low.
    APPEND it_dep_resp.
  ENDIF.

  "----------------------------------------------------
  " Data Vencimento
  "----------------------------------------------------
  IF ( p_dt_vcto-low  IS NOT INITIAL ) AND
     ( p_dt_vcto-high IS NOT INITIAL ).
    it_dt_vcto-sign   = 'I'.
    it_dt_vcto-option = 'BT'.
    it_dt_vcto-low  = p_dt_vcto-low.
    it_dt_vcto-high = p_dt_vcto-high.
    APPEND it_dt_vcto.
  ELSEIF ( p_dt_vcto-low IS NOT INITIAL ).
    it_dt_vcto-sign   = 'I'.
    it_dt_vcto-option = 'EQ'.
    it_dt_vcto-low  = p_dt_vcto-low.
    it_dt_vcto-high = p_dt_vcto-low.
    APPEND it_dt_vcto.
  ENDIF.

ENDFORM.                    " CONFIG_RANGES

FORM seleciona_dados.

  CLEAR: vg_ret_consulta.

  PERFORM: limpa_dados,
           config_ranges.

  SELECT *
    FROM zfit0115
    INTO TABLE tg_0115
   WHERE bukrs    IN it_bukrs
     AND dep_resp IN it_dep_resp
     AND dt_vcto  IN it_dt_vcto
     AND tp_prev  IN ('M','S').

  IF tg_0115[] IS INITIAL.
    MESSAGE 'Nenhum registro encontrado!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT *
    FROM t001
    INTO TABLE tg_t001
     FOR ALL ENTRIES IN tg_0115
   WHERE bukrs = tg_0115-bukrs.

  IF tg_t001[] IS INITIAL.
    MESSAGE 'Dados não encontrados!' TYPE 'S'.
    RETURN.
  ENDIF.

  SORT tg_t001 BY bukrs.
  DELETE ADJACENT DUPLICATES FROM tg_t001 COMPARING bukrs.

  "Seleciona Parametros de Fluxo Caixa Previsto
  SELECT *
    FROM zfit0109
    INTO TABLE tg_0109
     FOR ALL ENTRIES IN tg_0115
   WHERE codigo = tg_0115-codigo_flx.

  SORT tg_0109 BY codigo.
  DELETE ADJACENT DUPLICATES FROM tg_0109 COMPARING codigo.

  "Seleciona Cadastro Departamentos
  SELECT *
    FROM zimp_cad_depto
    INTO TABLE tg_cad_depto
     FOR ALL ENTRIES IN tg_0115
   WHERE dep_resp = tg_0115-dep_resp.

  SORT tg_cad_depto BY dep_resp.
  DELETE ADJACENT DUPLICATES FROM tg_cad_depto COMPARING dep_resp.


  vg_ret_consulta = 'X'.

ENDFORM.                    " SELECIONA_DADOS



*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  LOOP AT tg_0115.

    CLEAR: wa_saida_list, tg_t001, tg_0109, tg_cad_depto.

    MOVE-CORRESPONDING tg_0115 TO wa_saida_list.

    READ TABLE tg_t001 WITH KEY bukrs = tg_0115-bukrs.
    IF sy-subrc = 0.
      wa_saida_list-butxt = tg_t001-butxt.
    ENDIF.

    READ TABLE tg_cad_depto WITH KEY dep_resp = tg_0115-dep_resp.
    IF sy-subrc = 0.
      wa_saida_list-dep_resp_desc = tg_cad_depto-dep_resp_desc.
    ENDIF.

    READ TABLE tg_0109 WITH KEY codigo = tg_0115-codigo_flx.
    IF sy-subrc = 0.
      wa_saida_list-descricao = tg_0109-descricao.
      wa_saida_list-clas_flx  = tg_0109-clas_flx.
    ENDIF.

    IF tg_0115-wrbtr IS NOT INITIAL.
      wa_saida_list-valor = tg_0115-wrbtr.
    ELSE.
      "Codigo Abaixo é temporario
      IF tg_0115-dmbtr IS NOT INITIAL.
        wa_saida_list-valor = tg_0115-dmbtr.
      ELSE.
        wa_saida_list-valor = tg_0115-dmbe2.
      ENDIF.
      "Codigo Acima é temporario
    ENDIF.

    wa_saida_list-valor_old    =  wa_saida_list-valor.
    wa_saida_list-dt_vcto_old  =  wa_saida_list-dt_vcto.

    APPEND wa_saida_list TO it_saida_list.

  ENDLOOP.

ENDFORM.                    " PROCESSA_DADOS


*&---------------------------------------------------------------------*
*&      Form  LIMPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_dados .

  REFRESH: tg_0115,
           tg_t001,
           tg_0109,
           tg_cad_depto,
           it_saida_list,
           it_saida_input.

  CLEAR:   tg_0115,
           tg_t001,
           tg_0109,
           tg_cad_depto,
           wa_doc_prev.

ENDFORM.                    " LIMPA_DADOS


FORM criar_field_catalog_0100.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      1  'ZFIT0115'  'CD_PREV'            'IT_SAIDA_LIST' 'CD_PREV'           'Cod.Prev'              '08'  ' '    '' ' ' ' ' ' ' ' ',
      2  'ZFIT0115'  'BUKRS'              'IT_SAIDA_LIST' 'BUKRS'             'Empresa'               '07'  ' '    '' ' ' ' ' ' ' ' ',
      3  'T001'      'BUTXT'              'IT_SAIDA_LIST' 'BUTXT'             'Desc. Empresa'         '20'  ' '    '' ' ' ' ' ' ' ' ',
      4  'ZFIT0115'  'DEP_RESP'           'IT_SAIDA_LIST' 'DEP_RESP'          'Depto'                 '05'  ' '    '' ' ' ' ' ' ' ' ',
      5  'ZFIT0115'  ''                   'IT_SAIDA_LIST' 'DEP_RESP_DESC'     'Desc.Depto.'           '16'  ' '    '' ' ' ' ' ' ' ' ',
      6  'ZFIT0115'  'CODIGO_FLX'         'IT_SAIDA_LIST' 'CODIGO_FLX'        'Cod.Flx.'              '08'  ' '    '' ' ' ' ' ' ' ' ',
      7  'ZFIT0115'  ''                   'IT_SAIDA_LIST' 'DESCRICAO'         'Descrição'             '16'  ' '    '' ' ' ' ' ' ' ' ',
      8  'ZFIT0115'  ''                   'IT_SAIDA_LIST' 'CLAS_FLX'          'Clas.'                 '05'  ' '    '' ' ' ' ' ' ' ' ',
      9  'ZFIT0115'  'DT_VCTO'            'IT_SAIDA_LIST' 'DT_VCTO'           'Dt. Vcto'              '12'  'X'    '' ' ' ' ' ' ' ' ',
     10  'ZFIT0115'  ''                   'IT_SAIDA_LIST' 'WAERS'             'Moeda'                 '07'  ' '    '' ' ' ' ' ' ' ' ',
     11  'ZFIT0115'  'DMBTR'              'IT_SAIDA_LIST' 'VALOR'             'Valor'                 '15'  'X'    '' ' ' ' ' ' ' ' ',
     12  'ZFIT0115'  'VBELN'              'IT_SAIDA_LIST' 'VBELN'             'Ordem de Venda'        '15'  ' '    '' ' ' ' ' ' ' ' ', " #120519 Corrigir erro ao gravar lançamento - PSA
     13  'ZFIT0115'  ''                   'IT_SAIDA_LIST' 'OBSERV'            'Observação'            '40'  ' '    '' ' ' ' ' ' ' ' ',
     14  'ZFIT0115'  'USNAM'              'IT_SAIDA_LIST' 'USNAM'             'Usuário'               '10'  ' '    '' ' ' ' ' ' ' ' ', " #125357  SMC - Equalização ECCxHana
     15  'ZFIT0115'  'DT_ATUAL'           'IT_SAIDA_LIST' 'DT_ATUAL'          'Data'                  '10'  ' '    '' ' ' ' ' ' ' ' ', " #125357  SMC - Equalização ECCxHana
     16  'ZFIT0115'  'HR_ATUAL'           'IT_SAIDA_LIST' 'HR_ATUAL'          'Hora'                  '10'  ' '    '' ' ' ' ' ' ' ' ', " #125357  SMC - Equalização ECCxHana
     17  'ZFIT0115'  'CX_INTERNACIONAL'   'IT_SAIDA_LIST' 'CX_INTERNACIONAL'  'Caixa Internacional'   '15'  ' '    '' ' ' ' ' ' ' ' '.
  "14  'ZFIT0115'  ''           'IT_SAIDA_LIST' 'OBSERV2'       '2ª Observação'     '40'  ' '    '' ' ' ' ' ' ' ' '.


ENDFORM.                    " CRIAR_FIELD_CATALOG_XML

FORM criar_field_catalog_0101.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      1  'T001'     'BUKRS'             'IT_SAIDA_INPUT' 'BUKRS'            'Empresa(A)'           '07'  'X'    '' ' ' ' ' ' ' ' ',
      2  ''         ''                  'IT_SAIDA_INPUT' 'BUTXT'            'Desc. Empresa(B)'     '12'  ' '    '' ' ' ' ' ' ' ' ',
      3  ''         ''                  'IT_SAIDA_INPUT' 'CODIGO_FLX'       'Cod. Flx.(C)'         '10'  'X'    '' ' ' ' ' ' ' 'X',
      4  ''         ''                  'IT_SAIDA_INPUT' 'DESCRICAO'        'Descrição(D)'         '16'  ' '    '' ' ' ' ' ' ' ' ',
      5  ''         ''                  'IT_SAIDA_INPUT' 'CLAS_FLX'         'Classificação(E)'     '12'  ' '    '' ' ' ' ' ' ' ' ',
      6  'ZFIT0115' 'DT_VCTO'           'IT_SAIDA_INPUT' 'DT_VCTO'          'Dt. Vcto(F)'          '15'  'X'    '' ' ' ' ' ' ' ' ',
      7  'ZFIT0115' 'WAERS'             'IT_SAIDA_INPUT' 'WAERS'            'Moeda(G)'             '05'  'X'    '' ' ' ' ' ' ' ' ',
      8  'ZFIT0115' 'DMBTR'             'IT_SAIDA_INPUT' 'VALOR'            'Valor(H)'             '16'  'X'    '' ' ' ' ' ' ' ' ',
      9  'ZFIT0115' 'VBELN'             'IT_SAIDA_INPUT' 'VBELN'            'Ordem de Venda (I)'   '15'  'X'    '' ' ' ' ' ' ' ' ', " 120519 Corrigir erro ao gravar lançamento - PSA
     10  'ZFIT0115' 'OBSERV'            'IT_SAIDA_INPUT' 'OBSERV'           'Observação (J)'       '40'  'X'    '' ' ' ' ' ' ' ' ',
     11  ''         ''                  'IT_SAIDA_INPUT' 'STATUS'           'Status (K)'           '40'  ' '    '' ' ' ' ' ' ' ' ',
     12  'ZFIT0115' 'CX_INTERNACIONAL'  'IT_SAIDA_INPUT' 'CX_INTERNACIONAL' 'Caixa Internacional'  '15'  'X'    '' ' ' ' ' ' ' ' '.

*      1  'T001'     'BUKRS'     'IT_SAIDA_INPUT' 'BUKRS'        'Empresa'           '07'  'X'    '' ' ' ' ' ' ' ' ',
*      2  ''         ''          'IT_SAIDA_INPUT' 'BUTXT'        'Desc. Empresa'     '12'  ' '    '' ' ' ' ' ' ' ' ',
*      3  ''         ''          'IT_SAIDA_INPUT' 'CODIGO_FLX'   'Cod. Flx.'         '10'  'X'    '' ' ' ' ' ' ' 'X',
*      4  ''         ''          'IT_SAIDA_INPUT' 'DESCRICAO'    'Descrição'         '16'  ' '    '' ' ' ' ' ' ' ' ',
*      5  ''         ''          'IT_SAIDA_INPUT' 'CLAS_FLX'     'Classificação'     '12'  ' '    '' ' ' ' ' ' ' ' ',
*      6  'ZFIT0115' 'DT_VCTO'   'IT_SAIDA_INPUT' 'DT_VCTO'      'Dt. Vcto'          '15'  'X'    '' ' ' ' ' ' ' ' ',
*      7  'ZFIT0115' 'WAERS'     'IT_SAIDA_INPUT' 'WAERS'        'Moeda'             '05'  'X'    '' ' ' ' ' ' ' ' ',
*      8  'ZFIT0115' 'DMBTR'     'IT_SAIDA_INPUT' 'VALOR'        'Valor'             '16'  'X'    '' ' ' ' ' ' ' ' ',
*      9  'ZFIT0115' 'OBSERV'    'IT_SAIDA_INPUT' 'OBSERV'       'Observação'        '40'  'X'    '' ' ' ' ' ' ' ' ',
*     10  'ZFIT0115' 'OBSERV2'   'IT_SAIDA_INPUT' 'OBSERV2'      '2ª Observação'     '40'  'X'    '' ' ' ' ' ' ' ' '.


ENDFORM.                    " CRIAR_FIELD_CATALOG_XML

FORM estrutura_alv USING VALUE(p_col_pos)       TYPE i
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
                         VALUE(p_f4).

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
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.

  IF wa_fcat-fieldname EQ 'CX_INTERNACIONAL'.
    wa_fcat-checkbox = abap_true.
  ENDIF.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_objetos .
  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.                    " REFRESH_OBJETOS
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv .
  CALL METHOD obj_alv_list->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS_PREV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_dados_prev.

  DATA: lv_valid TYPE c.

  REFRESH: it_selectedcell.

  CALL METHOD obj_alv_input->get_selected_cells
    IMPORTING
      et_cell = it_selectedcell.

  FREE: itab_erro, wa_msgerros.

  IF obj_alv_input IS NOT INITIAL AND it_selectedcell IS NOT INITIAL. "IF obj_alv_input IS NOT INITIAL.

    LOOP AT it_selectedcell INTO wa_selectedcell.

      READ TABLE it_saida_input INTO wa_saida_input INDEX wa_selectedcell-row_id-index.

      CASE wa_selectedcell-col_id-fieldname.
        WHEN 'BUKRS' OR 'CODIGO_FLX' OR 'VALOR' OR 'WAERS' OR 'VBELN'.

          SELECT SINGLE * INTO tg_t001 FROM t001 WHERE bukrs =  wa_saida_input-bukrs.

          IF sy-subrc = 0.
            wa_saida_input-butxt = tg_t001-butxt.
          ELSE.
            CLEAR: wa_saida_input-butxt.
            wa_msgerros-erro008	= |Empresa Não Informada/Não Encontrada!|.
            APPEND wa_msgerros-erro008 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
          ENDIF.

          SELECT SINGLE * INTO tg_0109
             FROM zfit0109
            WHERE codigo  = wa_saida_input-codigo_flx
              AND tp_prev IN ('M','S').

          IF sy-subrc = 0.
            wa_saida_input-descricao = tg_0109-descricao.
            wa_saida_input-clas_flx  = tg_0109-clas_flx.
            wa_saida_input-tp_prev   = tg_0109-tp_prev.

            IF tg_0109-tp_prev = 'S'. "Saldo Inicial.
              wa_saida_input-dt_vcto = sy-datum.
            ENDIF.

          ELSE.
            CLEAR: wa_saida_input-descricao,
                   wa_saida_input-clas_flx,
                   wa_saida_input-codigo_flx.
            wa_msgerros-erro019  = |Cod. Fluxo Não Informado/Não Encontrado!|.
            APPEND wa_msgerros-erro019 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
          ENDIF.

          IF wa_saida_input-vbeln IS NOT INITIAL.
            CLEAR: aux_vbeln.
            SELECT SINGLE vbeln FROM vbak INTO aux_vbeln WHERE vbeln = wa_saida_input-vbeln. "PSA
            IF aux_vbeln IS INITIAL.
              wa_msgerros-erro009	= |A OV Não Encontrada!|.
              APPEND wa_msgerros-erro009 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
            ENDIF.
          ENDIF.

          IF wa_saida_input-waers IS INITIAL. "continuar psa
            wa_msgerros-erro020	= |Moeda Não Informada/Não Encontrada!|.
            APPEND wa_msgerros-erro020 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
          ENDIF.

          IF wa_saida_input-valor = 0 . "continuar psa
            wa_msgerros-erro021	= |Valor Não Informado!|.
            APPEND wa_msgerros-erro021 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
          ENDIF.

          CLEAR: wa_saida_input-field_style, ls_edit, lt_edit.

          ls_edit-fieldname = 'DT_VCTO'.
          IF tg_0109-tp_prev = 'S'. "Saldo Inicial.
            ls_edit-style = cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            ls_edit-style = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.

          ls_edit-style2 = space.
          ls_edit-style3 = space.
          ls_edit-style4 = space.
          ls_edit-maxlen = 10.
          INSERT ls_edit INTO TABLE lt_edit.

          INSERT LINES OF lt_edit INTO TABLE wa_saida_input-field_style.
          CLEAR: wa_saida_input-status.
          wa_saida_input-status = concat_lines_of( table = itab_erro sep = ';' ).
          MODIFY it_saida_input FROM wa_saida_input INDEX wa_selectedcell-row_id-index.

      ENDCASE.

      CALL METHOD obj_alv_input->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDLOOP.



  ENDIF.

ENDFORM.

FORM gravar_lctos .

  DATA: vl_zfbdt_ini     TYPE zfit0079-zfbdt,
        vl_zfbdt_fim     TYPE zfit0079-zfbdt,
        vl_bukrs_ini     TYPE zfit0079-bukrs,
        vl_bukrs_fim     TYPE zfit0079-bukrs,
        vl_max_versao    TYPE zfit0079-versao,
        vl_max_dt_versao TYPE zfit0079-dt_base_versao,
        vl_max_hora      TYPE zfit0079-hora_versao,
        vl_msg_1         TYPE string,
        vl_msg_2         TYPE string,
        vl_msg_exibir    TYPE string,
        wl_x001          TYPE x001,
        vl_bloq_lcto     TYPE c.

  DATA: var_answer TYPE c,
        vl_gdatu   TYPE gdatu_inv,
        lv_valid   TYPE c.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  CLEAR: vl_zfbdt_ini,wa_msgerros,itab_erro.
  REFRESH: tg_bukrs_rep,itab_erro.

**********************************************************************
  " 120519 Corrigir erro ao gravar lançamento - PSA

  DATA : grid_lines TYPE i. "declare variable
  DESCRIBE TABLE it_selectedcell  LINES grid_lines . "get no of rows

  IF grid_lines = 0.
    wa_msgerros-erro000	= |Favor insira uma linha na Grid!|.
    MESSAGE wa_msgerros-erro000 TYPE 'S'.
    EXIT.
  ELSE.
    CALL METHOD obj_alv_input->check_changed_data
      IMPORTING
        e_valid = lv_valid.
    PERFORM atualiza_dados_prev.
  ENDIF.







**********************************************************************

*  CALL METHOD obj_alv_input->check_changed_data.
*
*  CHECK it_saida_input[] IS NOT INITIAL.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      titlebar              = 'Confirmação'
*      text_question         = 'Deseja realmente gravar o(s) registro(s)?'
*      text_button_1         = 'Sim'
*      text_button_2         = 'Não'
*      default_button        = '1'
*      display_cancel_button = ''
*    IMPORTING
*      answer                = var_answer
*    EXCEPTIONS
*      text_not_found        = 1
*      OTHERS                = 2.
*
*  CHECK var_answer EQ '1'.

*-----------------------------------------------------------------------*
* Validações
*-----------------------------------------------------------------------*

* Busca Taxa Cambio ----------------------------------------------------*
  CREATE OBJECT obj_zcl_util_sd.

  MOVE sy-datum TO vl_gdatu.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_brl = 0.
    wa_msgerros-erro001	= |Taxa de Câmbio não encontrada(USD/BRL)!|.
    "MESSAGE wa_msgerros-erro001 TYPE 'S'.
    APPEND wa_msgerros-erro001 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
    "EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('ARS').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_usd_ars = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_usd_ars = 0.
    wa_msgerros-erro002	= |Taxa de Câmbio não encontrada(USD/ARS)!|.
    "MESSAGE wa_msgerros-erro002 TYPE 'S'.
    APPEND wa_msgerros-erro002 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
    "EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('EURX').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_brl = 0.
    wa_msgerros-erro003	= |Taxa de Câmbio não encontrada(EUR/BRL)!|.
    "MESSAGE wa_msgerros-erro003 TYPE 'S'.
    APPEND wa_msgerros-erro003 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
    "EXIT.
  ENDIF.

  FREE: obj_zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('EUR').
  obj_zcl_util_sd->set_tcurr('USD').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vg_tx_eur_usd = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vg_tx_eur_usd = 0.
    wa_msgerros-erro004	= |Taxa de Câmbio não encontrada(EUR/USD)!|.
    "MESSAGE wa_msgerros-erro004 TYPE 'S'.
    APPEND wa_msgerros-erro004 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
    "EXIT.
  ENDIF.

  IF wa_doc_prev-dep_resp IS INITIAL.
    wa_msgerros-erro005	= |Departamento é um campo obrigatório!|.
    "MESSAGE wa_msgerros-erro005 TYPE 'S'.
    APPEND wa_msgerros-erro005 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
    "RETURN.
  ENDIF.

  LOOP AT it_saida_input INTO wa_saida_input.

    IF wa_saida_input-bukrs IS INITIAL.
      wa_msgerros-erro006	= |Empresa é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro006 TYPE 'S'.
      APPEND wa_msgerros-erro006 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-codigo_flx IS INITIAL.
      wa_msgerros-erro007	= |Codigo do Fluxo é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro007 TYPE 'S'.
      APPEND wa_msgerros-erro007 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    CLEAR: wa_0109_aux.
    SELECT SINGLE *
      INTO wa_0109_aux
      FROM zfit0109
     WHERE codigo = wa_saida_input-codigo_flx.

    IF sy-subrc NE 0.
      wa_msgerros-erro010	= |Código Fluxo não encontrado.!|.
      "MESSAGE wa_msgerros-erro010 TYPE 'S'.
      APPEND wa_msgerros-erro010 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-clas_flx IS INITIAL.
      wa_msgerros-erro011	= |Classificação do Fluxo é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro011 TYPE 'S'.
      APPEND wa_msgerros-erro011 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-tp_prev IS INITIAL.
      wa_msgerros-erro012	= |Tipo de Previsão não encontrada!|.
      "MESSAGE wa_msgerros-erro012 TYPE 'S'.
      APPEND wa_msgerros-erro012 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-dt_vcto IS INITIAL.
      wa_msgerros-erro013	= |Data Vencimento é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro013 TYPE 'S'.
      APPEND wa_msgerros-erro013 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-dt_vcto < sy-datum.
      wa_msgerros-erro014	= |Data Vencimento não pode ser retroativa!|.
      "MESSAGE wa_msgerros-erro014 TYPE 'S'.
      APPEND wa_msgerros-erro014 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_0109_aux-tp_prev = 'S'. "Saldo Inicial.
      IF wa_saida_input-dt_vcto <> sy-datum.
        wa_msgerros-erro015	= |Encontrado Previsão do tipo "Saldo Inicial", com data de vencimento diferente da data atual!|.
        "MESSAGE wa_msgerros-erro015 TYPE 'S'.
        APPEND wa_msgerros-erro015 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
        "RETURN.
      ENDIF.
    ENDIF.


    PERFORM verifica_bloqueio_mov USING wa_saida_input-bukrs
                                        wa_saida_input-dt_vcto
                               CHANGING vl_bloq_lcto.

    IF vl_bloq_lcto IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF wa_saida_input-waers IS INITIAL.
      wa_msgerros-erro016	= |Moeda é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro016 TYPE 'S'.
      APPEND wa_msgerros-erro016 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-valor <= 0.
      wa_msgerros-erro017	= |Valor é um campo obrigatório!|.
      "MESSAGE wa_msgerros-erro017 TYPE 'S'.
      APPEND wa_msgerros-erro017 TO itab_erro. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    IF wa_saida_input-status IS INITIAL.
      wa_saida_input-status = concat_lines_of( table = itab_erro sep = ';' ).
    ELSE.
      wa_saida_input-status = ';' && concat_lines_of( table = itab_erro sep = ';' ).
    ENDIF.

  ENDLOOP.


  REFRESH: p_bukrs,
           p_dep_resp,
           itab_erro.

  p_dep_resp-low    = wa_doc_prev-dep_resp.


**********************************************************************
* Atualiza ALV
  CALL METHOD obj_alv_input->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
***********************************************************************


  IF wa_saida_input-status IS NOT INITIAL. "120519 Corrigir erro ao gravar lançamento - PSA

    MESSAGE 'Existem linhas com Status de Erro!' TYPE 'S'.

    EXIT.

  ELSE.

    CALL METHOD obj_alv_input->check_changed_data.

    CHECK it_saida_input[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Deseja realmente gravar o(s) registro(s)?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = var_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDIF.

  CHECK var_answer EQ '1'.


  LOOP AT it_saida_input INTO wa_saida_input.

    CLEAR: wa_0115, wa_0079.

    "Atribuição de Menor data de Vencimento ---------------------------------*
    IF ( vl_zfbdt_ini IS INITIAL ).
      vl_zfbdt_ini = wa_saida_input-dt_vcto.
    ENDIF.

    IF ( wa_saida_input-dt_vcto < vl_zfbdt_ini ).
      vl_zfbdt_ini = wa_saida_input-dt_vcto.
    ENDIF.

    "Atribuição de Maior data de Vencimento ---------------------------------*
    IF ( vl_zfbdt_fim IS INITIAL ).
      vl_zfbdt_fim = wa_saida_input-dt_vcto.
    ENDIF.

    IF ( wa_saida_input-dt_vcto > vl_zfbdt_fim ).
      vl_zfbdt_fim = wa_saida_input-dt_vcto.
    ENDIF.

    "Atribuição de Menor Empresa ---------------------------------*
    IF ( vl_bukrs_ini IS INITIAL ).
      vl_bukrs_ini = wa_saida_input-bukrs.
    ENDIF.

    IF ( wa_saida_input-bukrs < vl_bukrs_ini ).
      vl_bukrs_ini = wa_saida_input-bukrs.
    ENDIF.

    "Atribuição de Maior Empresa ---------------------------------*
    IF ( vl_bukrs_fim IS INITIAL ).
      vl_bukrs_fim = wa_saida_input-bukrs.
    ENDIF.

    IF ( wa_saida_input-bukrs > vl_bukrs_fim ).
      vl_bukrs_fim = wa_saida_input-bukrs.
    ENDIF.

    SELECT SINGLE *
      FROM zfit0109 INTO wa_0109_aux
     WHERE codigo = wa_saida_input-codigo_flx.

    IF sy-subrc NE 0.
      wa_msgerros-erro018	= |Código Fluxo não encontrado.!|.
      MESSAGE wa_msgerros-erro018 TYPE 'S'.
      CONCATENATE wa_msgerros-erro018 wa_saida_input-status INTO wa_saida_input-status SEPARATED BY '; '. " 120519 Corrigir erro ao gravar lançamento - PSA
      "RETURN.
    ENDIF.

    MOVE: wa_saida_input-bukrs        TO wa_0115-bukrs,
          wa_doc_prev-dep_resp        TO wa_0115-dep_resp,
          wa_saida_input-codigo_flx   TO wa_0115-codigo_flx,
          wa_saida_input-tp_prev      TO wa_0115-tp_prev,
          wa_0109_aux-clas_flx        TO wa_0115-clas_flx,
          wa_saida_input-dt_vcto      TO wa_0115-dt_vcto,
          wa_saida_input-waers        TO wa_0115-waers,
          wa_saida_input-observ       TO wa_0115-observ,
          wa_saida_input-vbeln        TO wa_0115-vbeln, " 120519 Corrigir erro ao gravar lançamento - PSA
         "WA_SAIDA_INPUT-OBSERV2      TO WA_0115-OBSERV2,
          wa_saida_input-valor        TO wa_0115-wrbtr,
          sy-uname                    TO wa_0115-usnam,
          sy-datum                    TO wa_0115-dt_atual,
          sy-uzeit                    TO wa_0115-hr_atual,
          wa_saida_input-cx_internacional   TO wa_0115-cx_internacional.

    "Ini CS2017001994
    CALL FUNCTION 'ZFI_CONV_MOEDA_FLX'
      EXPORTING
        i_bukrs         = wa_saida_input-bukrs
        i_waers_doc     = wa_saida_input-waers
        i_dmbtr         = wa_saida_input-valor
        i_dmbe2         = wa_saida_input-valor
        i_wrbtr         = wa_saida_input-valor
        i_tx_usd_brl    = vg_tx_usd_brl
        i_tx_usd_ars    = vg_tx_usd_ars
        i_tx_eur_brl    = vg_tx_eur_brl
        i_tx_eur_usd    = vg_tx_eur_usd
      CHANGING
        c_dmbtr         = wa_0115-dmbtr
        c_dmbe2         = wa_0115-dmbe2
      EXCEPTIONS
        data_inconplete = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    "Fim CS2017001994

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZFLX_PREV'
      IMPORTING
        number      = wa_0115-cd_prev.
    .
    IF ( sy-subrc <> 0 ) OR ( wa_0115-cd_prev IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar o Numero da Previsão!' TYPE 'S'.
      RETURN.
    ENDIF.

    MODIFY zfit0115 FROM wa_0115.

    IF ( sy-subrc <> 0 ).
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar o documento!' TYPE 'S'.
      RETURN.
    ENDIF.

    "Busca Ultima Versão
    PERFORM get_last_versao USING wa_saida_input-bukrs
                         CHANGING vl_max_dt_versao
                                  vl_max_hora
                                  vl_max_versao.

    IF vl_max_versao IS INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    tg_bukrs_rep-dt_versao   = vl_max_dt_versao.
    tg_bukrs_rep-hora_versao = vl_max_hora.
    tg_bukrs_rep-versao      = vl_max_versao.
    tg_bukrs_rep-bukrs       = wa_saida_input-bukrs.
    APPEND tg_bukrs_rep.

    IF wa_0115-tp_prev <> 'S'. "Diferente de Saldo Inicial

      CLEAR: tg_0109_aux.
      SELECT SINGLE * INTO tg_0109_aux
        FROM zfit0109
       WHERE codigo = wa_0115-codigo_flx.

      IF ( sy-subrc NE 0 ) OR
         ( tg_0109_aux-clas_flx IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE 'Classificação do Fluxo não encontrada!' TYPE 'S'.
        RETURN.
      ENDIF.

      "Grava ZFIT0079
      MOVE: wa_0115-cd_prev          TO wa_0079-cd_prev,
            wa_0115-bukrs            TO wa_0079-bukrs,
            wa_0115-dep_resp         TO wa_0079-dep_resp,
            wa_0115-codigo_flx       TO wa_0079-codigo,
            tg_0109_aux-clas_flx     TO wa_0079-clas_flx,
            tg_0109_aux-tp_prev      TO wa_0079-tp_prev,
            wa_0115-dt_vcto          TO wa_0079-zfbdt,
            tg_0109_aux-st_calc_sdo  TO wa_0079-st_calc_sdo,
            wa_0115-waers            TO wa_0079-waers,
            wa_0115-dmbtr            TO wa_0079-dmbtr,
            wa_0115-dmbe2            TO wa_0079-dmbe2,
            wa_0115-observ           TO wa_0079-sgtxt,
            wa_0115-observ2          TO wa_0079-sgtxt2,
            sy-uname                 TO wa_0079-us_proc,
            wa_0115-usnam            TO wa_0079-usnam,
            vl_max_dt_versao         TO wa_0079-dt_base_versao,
            vl_max_versao            TO wa_0079-versao,
            vl_max_hora              TO wa_0079-hora_versao,
            sy-datum                 TO wa_0079-dt_atual,
            sy-uzeit                 TO wa_0079-hr_atual.

      MODIFY zfit0079 FROM wa_0079.

      IF ( sy-subrc <> 0 ).
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar os documentos(ZFIT0079)!' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF. "IF WA_0115-TP_PREV <> 'S'. "Diferente de Saldo Inicial

  ENDLOOP.

  SORT p_bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM p_bukrs COMPARING low.

  SORT tg_bukrs_rep.
  DELETE ADJACENT DUPLICATES FROM tg_bukrs_rep.


  LOOP AT tg_bukrs_rep.

    CALL FUNCTION 'ZFI_PROC_RESUMO_FLX'
      EXPORTING
        i_data_ini       = vl_zfbdt_ini
        i_bukrs          = tg_bukrs_rep-bukrs
        i_dt_base_versao = tg_bukrs_rep-dt_versao
        i_hora_versao    = tg_bukrs_rep-hora_versao
        i_versao         = tg_bukrs_rep-versao
        i_ref_saldo      = 'X'
      EXCEPTIONS
        m_error          = 1
        OTHERS           = 2.

    IF sy-subrc NE 0.
      MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ENDLOOP.


  COMMIT WORK.
  MESSAGE 'Registros gravados com sucesso!' TYPE 'S'.

  IF sy-tcode EQ 'ZFI0101'.
    LEAVE PROGRAM.
  ELSE.

    p_bukrs-low    = vl_bukrs_ini.
    p_bukrs-high   = vl_bukrs_fim.

    IF p_dt_vcto-low IS INITIAL.
      p_dt_vcto-low = vl_zfbdt_ini.
    ENDIF.

    p_dt_vcto-high = vl_zfbdt_fim.

    PERFORM: seleciona_dados.

    IF vg_ret_consulta IS NOT INITIAL.
      PERFORM: processa_dados,
               refresh_alv.
    ENDIF.

    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_LCTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estornar_lcto .

  DATA: var_answer       TYPE c,
        wa_0115_aux      TYPE zfit0115,
        wa_0079_aux      TYPE zfit0079,
        vl_max_versao    TYPE zfit0079-versao,
        vl_max_dt_versao TYPE zfit0079-dt_base_versao,
        vl_max_hora      TYPE zfit0079-hora_versao,
        vl_msg_1         TYPE string,
        vl_msg_2         TYPE string,
        vl_msg_exibir    TYPE string,
        vl_bloq_lcto     TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_list->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK NOT it_sel_rows IS INITIAL.

  IF ( lines( it_sel_rows ) NE 1 ).
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente excluir o registro?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.


  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  READ TABLE it_saida_list INTO  wa_saida_list INDEX wa_sel_rows-index.

  IF wa_saida_list-cd_prev IS NOT INITIAL.

    PERFORM verifica_bloqueio_mov USING wa_saida_list-bukrs
                                        wa_saida_list-dt_vcto
                               CHANGING vl_bloq_lcto.

    IF vl_bloq_lcto IS NOT INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    PERFORM get_last_versao USING wa_saida_list-bukrs
                         CHANGING vl_max_dt_versao
                                  vl_max_hora
                                  vl_max_versao.

    IF vl_max_versao IS INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    DELETE FROM zfit0115 WHERE cd_prev = wa_saida_list-cd_prev.

    IF sy-subrc NE 0.

      SELECT SINGLE *
        INTO wa_0115_aux
        FROM zfit0115
       WHERE cd_prev = wa_saida_list-cd_prev.

      IF sy-subrc = 0.
        MESSAGE 'Houve um erro ao excluir o(s) registro(s)!' TYPE 'S'.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

    ENDIF.

    IF wa_saida_list-tp_prev <> 'S'. "Diferente de Saldo Inicial

      DELETE FROM zfit0079 WHERE cd_prev        EQ wa_saida_list-cd_prev
                             AND dt_base_versao EQ vl_max_dt_versao
                             AND versao         EQ vl_max_versao.
*        IF SY-SUBRC NE 0.
*
*          SELECT SINGLE *
*            INTO WA_0079_AUX
*            FROM ZFIT0079
*           WHERE CD_PREV = WA_SAIDA_LIST-CD_PREV.
*
*          IF SY-SUBRC = 0.
*            MESSAGE 'Houve um erro ao excluir o(s) registro(s)!' TYPE 'S'.
*            ROLLBACK WORK.
*            RETURN.
*          ENDIF.
*
*        ENDIF.

    ENDIF. "IF WA_SAIDA_LIST-TP_PREV <> 'S'. "Diferente de Saldo Inicial

    CALL FUNCTION 'ZFI_PROC_RESUMO_FLX'
      EXPORTING
        i_data_ini       = wa_saida_list-dt_vcto
        i_bukrs          = wa_saida_list-bukrs
        i_dt_base_versao = vl_max_dt_versao
        i_hora_versao    = vl_max_hora
        i_versao         = vl_max_versao
        i_ref_saldo      = 'X'
      EXCEPTIONS
        m_error          = 1
        OTHERS           = 2.

    IF sy-subrc NE 0.
      MESSAGE 'Houve um erro ao excluir o(s) registro(s)!' TYPE 'S'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ENDIF.

  MESSAGE 'Registro(s) excluido(s) com sucesso!' TYPE 'S'.
  COMMIT WORK.

  PERFORM: seleciona_dados.

  IF vg_ret_consulta IS NOT INITIAL.
    PERFORM: processa_dados.
  ENDIF.

  PERFORM refresh_alv.

ENDFORM.

FORM verifica_bloqueio_mov USING p_bukrs_mov
                                 p_dt_vcto_mov
                        CHANGING p_bloqueio.

  DATA: vl_msg_1      TYPE string,
        vl_msg_2      TYPE string,
        vl_msg_exibir TYPE string.

  DATA: wa_0117_aux TYPE zfit0117.

  CLEAR: p_bloqueio.

  CLEAR: wa_0117_aux.
  SELECT SINGLE *
    INTO wa_0117_aux
    FROM zfit0117
   WHERE bukrs   = p_bukrs_mov
     AND dt_vcto = p_dt_vcto_mov
     AND status  = '@06@'.

  IF sy-subrc = 0.
    p_bloqueio = 'X'.

    vl_msg_1 =  p_bukrs_mov.
    PERFORM formata_data USING p_dt_vcto_mov CHANGING vl_msg_2.

    CONCATENATE 'Movimentação Avulsa Bloqueada na Empresa:' vl_msg_1 'no dia:' vl_msg_2 '!'
           INTO vl_msg_exibir SEPARATED BY space.

    MESSAGE vl_msg_exibir TYPE 'S'.
    RETURN.
  ENDIF.

ENDFORM.

FORM formata_data  USING    p_data
                   CHANGING p_value.

  CONCATENATE p_data+06(2) '.' p_data+04(2) '.' p_data+2(2) INTO p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_ALTERACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_alteracoes .

  DATA: var_answer       TYPE c,
        vl_zfbdt_ini     TYPE zfit0079-zfbdt,
        vl_max_versao    TYPE zfit0079-versao,
        vl_max_dt_versao TYPE zfit0079-dt_base_versao,
        vl_max_hora      TYPE zfit0079-hora_versao,
        vl_msg_1         TYPE string,
        vl_msg_2         TYPE string,
        vl_msg_exibir    TYPE string,
        vl_bloq_lcto     TYPE c,
        vl_get_tx_cambio TYPE c.

  DATA: vl_gdatu   TYPE gdatu_inv.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  FIELD-SYMBOLS: <l_out> TYPE ty_saida_list. " ALV TABLE LINE

  CLEAR: vl_zfbdt_ini, vl_get_tx_cambio.
  REFRESH: tg_bukrs_rep.

  CALL METHOD obj_alv_list->check_changed_data.

  LOOP AT it_saida_list INTO wa_saida_list.

    IF ( ( wa_saida_list-valor   EQ wa_saida_list-valor_old   ) AND
         ( wa_saida_list-dt_vcto EQ wa_saida_list-dt_vcto_old ) ).
      CONTINUE.
    ENDIF.

    CLEAR: wa_0115, wa_0079.

*   Busca Taxa Cambio ----------------------------------------------------*
    IF vl_get_tx_cambio IS INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Confirma modificação do(s) registro(s)?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF var_answer NE '1'.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

      CREATE OBJECT obj_zcl_util_sd.

      MOVE sy-datum TO vl_gdatu.

      obj_zcl_util_sd->set_kurst('B').
      obj_zcl_util_sd->set_waerk('USD').
      obj_zcl_util_sd->set_tcurr('BRL').
      obj_zcl_util_sd->set_data( vl_gdatu ).

      vg_tx_usd_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

      IF vg_tx_usd_brl = 0.
        MESSAGE 'Taxa de Câmbio não encontrada(USD/BRL)!' TYPE 'S'.
        EXIT.
      ENDIF.

      FREE: obj_zcl_util_sd.
      CREATE OBJECT obj_zcl_util_sd.

      obj_zcl_util_sd->set_kurst('B').
      obj_zcl_util_sd->set_waerk('USD').
      obj_zcl_util_sd->set_tcurr('ARS').
      obj_zcl_util_sd->set_data( vl_gdatu ).

      vg_tx_usd_ars = abs( obj_zcl_util_sd->taxa_cambio( ) ).

      IF vg_tx_usd_ars = 0.
        MESSAGE 'Taxa de Câmbio não encontrada(USD/ARS)!' TYPE 'S'.
        EXIT.
      ENDIF.

      FREE: obj_zcl_util_sd.
      CREATE OBJECT obj_zcl_util_sd.

      obj_zcl_util_sd->set_kurst('EURX').
      obj_zcl_util_sd->set_waerk('EUR').
      obj_zcl_util_sd->set_tcurr('BRL').
      obj_zcl_util_sd->set_data( vl_gdatu ).

      vg_tx_eur_brl = abs( obj_zcl_util_sd->taxa_cambio( ) ).

      IF vg_tx_eur_brl = 0.
        MESSAGE 'Taxa de Câmbio não encontrada(EUR/BRL)!' TYPE 'S'.
        EXIT.
      ENDIF.


      FREE: obj_zcl_util_sd.
      CREATE OBJECT obj_zcl_util_sd.

      obj_zcl_util_sd->set_kurst('B').
      obj_zcl_util_sd->set_waerk('EUR').
      obj_zcl_util_sd->set_tcurr('USD').
      obj_zcl_util_sd->set_data( vl_gdatu ).

      vg_tx_eur_usd = abs( obj_zcl_util_sd->taxa_cambio( ) ).

      IF vg_tx_eur_usd = 0.
        MESSAGE 'Taxa de Câmbio não encontrada(EUR/USD)!' TYPE 'S'.
        EXIT.
      ENDIF.

      vl_get_tx_cambio = 'X'.

    ENDIF.

    IF wa_saida_list-dt_vcto IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Data de Vencimento é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF wa_saida_list-dt_vcto < sy-datum.
      ROLLBACK WORK.
      MESSAGE 'Data Vencimento não pode ser retroativa!' TYPE 'S'.
      RETURN.
    ENDIF.

    "Busca ultima versão.
    PERFORM get_last_versao USING wa_saida_list-bukrs
                         CHANGING vl_max_dt_versao
                                  vl_max_hora
                                  vl_max_versao.

    IF vl_max_versao IS INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    "Get Registros para Modificação
    SELECT SINGLE *
      INTO wa_0115
      FROM zfit0115
     WHERE cd_prev = wa_saida_list-cd_prev.

    IF ( sy-subrc NE 0 ).
      ROLLBACK WORK.
      MESSAGE 'Registro não encontrado pra modificação!' TYPE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      INTO wa_0079
      FROM zfit0079
     WHERE cd_prev        = wa_saida_list-cd_prev
       AND dt_base_versao = vl_max_dt_versao
       AND versao         = vl_max_versao.

    IF ( sy-subrc NE 0 ).
      ROLLBACK WORK.
      MESSAGE 'Registro não encontrado pra modificação!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF ( vl_zfbdt_ini IS INITIAL ).
      vl_zfbdt_ini = wa_saida_list-dt_vcto.
    ENDIF.

    IF ( wa_saida_list-dt_vcto_old < vl_zfbdt_ini ).
      vl_zfbdt_ini = wa_saida_list-dt_vcto_old.
    ENDIF.

    IF ( wa_saida_list-dt_vcto < vl_zfbdt_ini ).
      vl_zfbdt_ini = wa_saida_list-dt_vcto.
    ENDIF.



    "Verifica Permissão de Movimento na nova data de Vencimento
    PERFORM verifica_bloqueio_mov USING wa_saida_list-bukrs
                                        wa_saida_list-dt_vcto
                               CHANGING vl_bloq_lcto.

    IF vl_bloq_lcto IS NOT INITIAL.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    IF wa_saida_list-dt_vcto_old <> wa_saida_list-dt_vcto.

      "Verifica Permissão de Movimento na nova data de Vencimento
      PERFORM verifica_bloqueio_mov USING wa_saida_list-bukrs
                                          wa_saida_list-dt_vcto_old
                                 CHANGING vl_bloq_lcto.

      IF vl_bloq_lcto IS NOT INITIAL.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

    ENDIF.

    tg_bukrs_rep-dt_versao   = vl_max_dt_versao.
    tg_bukrs_rep-hora_versao = vl_max_hora.
    tg_bukrs_rep-versao      = vl_max_versao.
    tg_bukrs_rep-bukrs       = wa_saida_list-bukrs.
    APPEND tg_bukrs_rep.

    MOVE: wa_saida_list-dt_vcto  TO wa_0115-dt_vcto,
          wa_saida_list-valor    TO wa_0115-wrbtr.

    "Ini CS2017001994
    CALL FUNCTION 'ZFI_CONV_MOEDA_FLX'
      EXPORTING
        i_bukrs         = wa_saida_list-bukrs
        i_waers_doc     = wa_saida_list-waers
        i_dmbtr         = wa_saida_list-valor
        i_dmbe2         = wa_saida_list-valor
        i_wrbtr         = wa_saida_list-valor
        i_tx_usd_brl    = vg_tx_usd_brl
        i_tx_usd_ars    = vg_tx_usd_ars
        i_tx_eur_brl    = vg_tx_eur_brl
        i_tx_eur_usd    = vg_tx_eur_usd
      CHANGING
        c_dmbtr         = wa_0115-dmbtr
        c_dmbe2         = wa_0115-dmbe2
      EXCEPTIONS
        data_inconplete = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    "Fim CS2017001994

    MODIFY zfit0115 FROM wa_0115.

    IF ( sy-subrc <> 0 ).
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao modificar o(s) documento(s)!' TYPE 'S'.
      RETURN.
    ENDIF.

    MOVE: wa_0115-dt_vcto          TO wa_0079-zfbdt,
          wa_0115-dmbtr            TO wa_0079-dmbtr,
          wa_0115-dmbe2            TO wa_0079-dmbe2,
          sy-uname                 TO wa_0079-us_proc,
          sy-datum                 TO wa_0079-dt_atual,
          sy-uzeit                 TO wa_0079-hr_atual.


    MODIFY zfit0079 FROM wa_0079.

    IF ( sy-subrc <> 0 ).
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao modificar o(s) documento(s)!' TYPE 'S'.
      RETURN.
    ENDIF.

  ENDLOOP.


  IF vl_get_tx_cambio IS NOT INITIAL.

    "Efetua Reprocessamento de Saldos.
    SORT tg_bukrs_rep.
    DELETE ADJACENT DUPLICATES FROM tg_bukrs_rep.

    LOOP AT tg_bukrs_rep.

      CALL FUNCTION 'ZFI_PROC_RESUMO_FLX'
        EXPORTING
          i_data_ini       = vl_zfbdt_ini
          i_bukrs          = tg_bukrs_rep-bukrs
          i_dt_base_versao = tg_bukrs_rep-dt_versao
          i_hora_versao    = tg_bukrs_rep-hora_versao
          i_versao         = tg_bukrs_rep-versao
          i_ref_saldo      = 'X'
        EXCEPTIONS
          m_error          = 1
          OTHERS           = 2.

      IF sy-subrc NE 0.
        MESSAGE 'Houve um erro ao gravar os registros!' TYPE 'S'.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

    ENDLOOP.

    COMMIT WORK.
    MESSAGE 'Registros gravados com sucesso!' TYPE 'S'.

    PERFORM: seleciona_dados.

    IF vg_ret_consulta IS NOT INITIAL.
      PERFORM: processa_dados,
               refresh_alv.
    ENDIF.

  ENDIF.


ENDFORM.

FORM get_last_versao USING p_bukrs
                  CHANGING p_dt_versao
                           p_hr_versao
                           p_versao.

  DATA: vl_msg_1      TYPE string,
        vl_msg_2      TYPE string,
        vl_msg_exibir TYPE string.

  CLEAR: p_dt_versao, p_hr_versao, p_versao.

  SELECT MAX( dt_base_versao )
    INTO (p_dt_versao)
    FROM zfit0111
   WHERE bukrs = p_bukrs.

  IF ( sy-subrc NE 0 ) OR
     ( p_dt_versao IS INITIAL ).

    vl_msg_1 = p_bukrs.
    CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
           INTO vl_msg_exibir SEPARATED BY space.
    MESSAGE vl_msg_exibir TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT MAX( versao )
    INTO (p_versao)
    FROM zfit0111
   WHERE bukrs          = p_bukrs
     AND dt_base_versao = p_dt_versao.

  IF ( sy-subrc NE 0 ) OR
     ( p_versao    IS INITIAL ).

    vl_msg_1 = p_bukrs.
    CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
           INTO vl_msg_exibir SEPARATED BY space.
    MESSAGE vl_msg_exibir TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE hora_versao
    INTO p_hr_versao
    FROM zfit0111
   WHERE bukrs          = p_bukrs
     AND dt_base_versao = p_dt_versao
     AND versao         = p_versao.

  IF ( sy-subrc NE 0 ) OR
     ( p_hr_versao  IS INITIAL ).

    vl_msg_1 = p_bukrs.
    CONCATENATE 'Nenhuma versão de Processamento encontrada para a Empresa('vl_msg_1')!'
    INTO vl_msg_exibir SEPARATED BY space.
    MESSAGE vl_msg_exibir TYPE 'S'.
    RETURN.
  ENDIF.

  IF ( p_dt_versao IS INITIAL ) OR
     ( p_hr_versao IS INITIAL ) OR
     ( p_versao    IS INITIAL ).
    CLEAR: p_dt_versao, p_hr_versao, p_versao.
  ENDIF.

ENDFORM.
