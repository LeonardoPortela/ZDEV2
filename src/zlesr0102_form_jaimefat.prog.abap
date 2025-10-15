*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_FORM
*&---------------------------------------------------------------------*
DATA: results_received(1) TYPE c,
      t_return_goods_cnc  LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
      g_migo_number       TYPE bapi2017_gm_head_ret.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon).                                   "16

  CLEAR wa_afield.
  wa_afield-fieldname   = p_field.
  wa_afield-tabname     = p_tabname.
  wa_afield-ref_table   = p_ref_tabname.
  wa_afield-ref_field   = p_ref_fieldname.
  wa_afield-key         = ' '.
  wa_afield-edit        = p_edit.
  wa_afield-col_pos     = p_col_pos.
  wa_afield-outputlen   = p_outputlen.
  wa_afield-no_out      = p_no_out.
  wa_afield-do_sum      = p_sum.
  wa_afield-reptext     = p_scrtext_l.
  wa_afield-scrtext_s   = p_scrtext_l.
  wa_afield-scrtext_m   = p_scrtext_l.
  wa_afield-scrtext_l   = p_scrtext_l.
  wa_afield-style       = p_style.
  wa_afield-just        = p_just.
  wa_afield-hotspot     = p_hotspot.
  wa_afield-f4availabl  = p_f4.
  wa_afield-checkbox    = p_checkbox.
  wa_afield-icon        = p_icon.
  wa_afield-colddictxt  = 'M'.
  wa_afield-selddictxt  = 'M'.
  wa_afield-tipddictxt  = 'M'.
  wa_afield-col_opt     = 'X'.

*-CS2021000696 - 10.08.2021 - JT - inicio
  IF wa_afield-fieldname = 'ICON'         OR
     wa_afield-fieldname = 'DT_MOVIMENTO' OR
     wa_afield-fieldname = 'NR_ROMANEIO'  OR
     wa_afield-fieldname = 'PLACA_CAV'    OR
     wa_afield-fieldname = 'BUKRS'        OR
     wa_afield-fieldname = 'BRANCH'       OR
     wa_afield-fieldname = 'NRO_CG'.
    wa_afield-fix_column = 'X'.
  ENDIF.
*-CS2021000696 - 10.08.2021 - JT - fim

  APPEND wa_afield TO it_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_atual_frete USING pa_zsdt0001    TYPE ty_zsdt0001
                         p_tipo_chamada TYPE char01
                CHANGING p_saida        TYPE ty_saida.

  DATA: _vbeln         TYPE vbeln,
        _placa_cav     TYPE zplaca,
        _vlr_frete_neg TYPE zvalor_frete,
        _id_ordem      TYPE zde_id_ordem.

  DATA: v_cont_fre TYPE i.

  v_cont_fre = 0.

  IF p_tipo_chamada EQ 'E'.
    APPEND pa_zsdt0001 TO it_zsdt0001.
    APPEND pa_zsdt0001 TO it_zsdt0001_fre.
    PERFORM f_selecao_generica_rom.
    PERFORM f_get_value_set TABLES t_auart  USING 'MAGGI_ARMAZENAGEM_VA01'.
    PERFORM f_pega_frete.
  ENDIF.

  "Vlr Frete
  LOOP AT it_a900 INTO wa_a900 WHERE shtyp = pa_zsdt0001-shtyp
                                 AND tdlnr = pa_zsdt0001-agente_frete
                                 AND route = pa_zsdt0001-route
                                 AND add01 = pa_zsdt0001-add01.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a900-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  LOOP AT it_a910 INTO wa_a910 WHERE shtyp  = pa_zsdt0001-shtyp
                                 AND tdlnr  = pa_zsdt0001-agente_frete
                                 AND lzonea = pa_zsdt0001-lzonea
                                 AND lzonez = pa_zsdt0001-lzonez.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a910-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  LOOP AT it_a911 INTO wa_a911 WHERE shtyp = pa_zsdt0001-shtyp
                                 AND tdlnr = pa_zsdt0001-agente_frete
                                 AND route = pa_zsdt0001-route.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a911-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  LOOP AT it_a915 INTO wa_a915 WHERE shtyp  = pa_zsdt0001-shtyp
                                 AND tdlnr  = pa_zsdt0001-agente_frete
                                 AND lzonea = pa_zsdt0001-lzonea
                                 AND lzonez = pa_zsdt0001-lzonez
                                 AND add01  = pa_zsdt0001-add01.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a915-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  LOOP AT it_a918 INTO wa_a918 WHERE shtyp  = pa_zsdt0001-shtyp
                                 AND tdlnr  = pa_zsdt0001-agente_frete
                                 AND matnr  = pa_zsdt0001-matnr
                                 AND lzonea = pa_zsdt0001-lzonea
                                 AND lzonez = pa_zsdt0001-lzonez
                                 AND add01  = pa_zsdt0001-add01.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a918-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  LOOP AT it_a919 INTO wa_a919 WHERE shtyp  = pa_zsdt0001-shtyp
                                 AND tdlnr  = pa_zsdt0001-agente_frete
                                 AND matnr  = pa_zsdt0001-matnr
                                 AND lzonea = pa_zsdt0001-lzonea
                                 AND lzonez = pa_zsdt0001-lzonez.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a919-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      ADD 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.

  IF wa_konp-krech = 'A'. "Percentual
    p_saida-kbetr = p_saida-kbetr / 10.
  ENDIF.

*---CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
  READ TABLE it_zsdt0001_aux INTO wa_zsdt0001_aux
                             WITH KEY ch_referencia = pa_zsdt0001-ch_referencia.
  LOOP AT it_a942 INTO wa_a942 WHERE shtyp  = wa_zsdt0001_aux-shtyp
                                 AND sdabw  = wa_zsdt0001_aux-sdabw
                                 AND id_viagem = wa_zsdt0001_aux-viagem_id.
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = wa_a942-knumh BINARY SEARCH.
    IF sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
*     ADD  1 TO v_cont_fre.
      MOVE 1 TO v_cont_fre.
    ENDIF.
  ENDLOOP.
*---CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

  "Check Alteração Preço por Solicitação - Transação ZLES0153 - CS2016001693
  IF ( p_saida-tipo = 'O' ) AND ( pa_zsdt0001-vbeln IS NOT INITIAL ).
    _vbeln         = pa_zsdt0001-vbeln.
    _placa_cav     = pa_zsdt0001-placa_cav.
    CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
      EXPORTING
        i_vbeln         = _vbeln
        i_placa_cav     = _placa_cav
        i_id_ordem      = pa_zsdt0001-id_ordem
        i_shtyp         = p_saida-shtyp
      IMPORTING
        e_vlr_frete_neg = _vlr_frete_neg
        e_id_ordem      = _id_ordem.

    IF _vlr_frete_neg > 0.
      p_saida-kbetr    = _vlr_frete_neg. "Atribuir Valor de Frete Negociado
      p_saida-id_ordem = _id_ordem.
    ENDIF.
  ENDIF.

  wa_saida-cont_fre = v_cont_fre.

ENDFORM.                    "f_atual_frete

FORM f_elimina_vt  TABLES tl_itemdata
                 CHANGING p_tipo_chamada TYPE char01
                          p_saida TYPE ty_saida
                          it_tab_bapiret1 TYPE tab_bapiret1.

  DATA: st_headerdata2      TYPE bapishipmentheader,
        st_headerdataaction TYPE bapishipmentheaderaction,
        t_itemdataaction    TYPE TABLE OF bapishipmentitemaction WITH HEADER LINE,
        st_headerdata       TYPE bapishipmentheader,
        v_chv_fat_vt        TYPE zch_ref,
        wa_tab_bapiret1     LIKE LINE OF it_tab_bapiret1.

  CLEAR: st_headerdataaction,
         st_headerdata,
         t_itemdataaction,
         st_headerdata2,
         t_itemdataaction[],
         t_return_vt[].

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_saida-transp+0(10)
    IMPORTING
      output = st_headerdata2-shipment_num.

  st_headerdataaction-shipment_num = 'D'.
  st_headerdataaction-service_agent_id = 'D'.

  LOOP AT tl_itemdata INTO st_itemdata.
    MOVE: 'D' TO t_itemdataaction-delivery,
          'D' TO t_itemdataaction-itenerary.

    APPEND t_itemdataaction.
    CLEAR: t_itemdataaction.
  ENDLOOP.

  REFRESH t_return_vt.
  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = st_headerdata2
      headerdataaction = st_headerdataaction
    TABLES
      itemdata         = tl_itemdata
      itemdataaction   = t_itemdataaction
      return           = t_return_vt.

  IF p_tipo_chamada = 'E'.
    LOOP AT t_return_vt INTO DATA(wa_erros_vt).
      CLEAR: wa_tab_bapiret1.
      MOVE-CORRESPONDING wa_erros_vt TO wa_tab_bapiret1.
      APPEND wa_tab_bapiret1 TO it_tab_bapiret1.
    ENDLOOP.
  ENDIF.

  READ TABLE t_return_vt WITH KEY type = 'E'.
  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(_st_transp_before) = vg_st_transp_before.

    IF vg_cockpit = '04'.
      IF p_saida-tipo = 'P'.
        _st_transp_before = vg_st_aviso_rec.
      ENDIF.
    ENDIF.

    p_saida-st_proc  = _st_transp_before.
    p_saida-transp   = icon_execute_object.

    "Atualiza valor do frete Historico
    LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom) WHERE ch_referencia IS NOT INITIAL.
      UPDATE zsdt0001 SET  kbetr = 0  konwa = '' WHERE ch_referencia = _wl_rom-ch_referencia.


      UPDATE zsdt0001 SET st_proc      = _st_transp_before
                          kbetr        = 0
                          konwa        = ''
                          doc_transp   = ''
       WHERE ch_referencia = _wl_rom-ch_referencia.

      UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = _wl_rom-ch_referencia.

      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
      IF sy-subrc EQ 0.
        <fs_saida_tmp>-st_proc  = _st_transp_before.
        <fs_saida_tmp>-transp   = icon_execute_object.

        CLEAR: v_chv_fat_vt.
        zcl_romaneio=>get_ck_faturar(
          EXPORTING
            i_ch_referencia_sai   = <fs_saida_tmp>-ch_referencia
            i_somente_chv_faturar = abap_true
          IMPORTING
            e_chv_faturar         = v_chv_fat_vt ).

        IF ( v_chv_fat_vt IS NOT INITIAL ) AND ( v_chv_fat_vt NE <fs_saida_tmp>-ch_referencia ).
          <fs_saida_tmp>-transp = icon_icon_list.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ).
      UPDATE zsdt0001 SET agente_frete = ''
       WHERE ch_referencia = p_saida-ch_referencia.
    ENDIF.

  ELSE.
    CASE p_tipo_chamada.
      WHEN 'L'.
        PERFORM f_prepare_return TABLES t_return_vt.
        PERFORM f_grava_log_erro TABLES tg_log_erro USING p_saida.
      WHEN 'E'.
        MESSAGE ID t_return_vt-id TYPE t_return_vt-type NUMBER t_return_vt-number INTO DATA(mtext)
           WITH t_return_vt-message_v1 t_return_vt-message_v2 t_return_vt-message_v3 t_return_vt-message_v4.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( MTEXT ) ).
    ENDCASE.
  ENDIF.
ENDFORM.                    " ELIMINA_VT

FORM f_monta_layout .

  CLEAR it_fieldcat[].

  CASE vg_cockpit. "Field Cat.
    WHEN '01'. "Commodities (Formação Lote, Vendas e Trâsferências Expedidas) - ZLES0106

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' '.

      DATA(lt_saida) = it_saida[].
      DELETE lt_saida WHERE qtde_remessa IS INITIAL.
      DESCRIBE TABLE lt_saida LINES DATA(lv_lines).
      IF lv_lines >= 1.
        PERFORM f_estrutura_alv USING:
        08  ''              ''             'IT_SAIDA' 'QTDE_REMESSA'  'Qtde.Remessa'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'UM_REMESSA'    'U.M. Remessa'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        14  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        15  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        16  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        17  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        21  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        22  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        23  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        24  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        25  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        26  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        27  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        28  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        29  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
      ELSE.
        PERFORM f_estrutura_alv USING:
          08  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          09  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          10  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          10  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          11  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
          11  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          12  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          13  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          14  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          15  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
*----CS2021000508 - 07.06.2021 - JT - inicio
*       16  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
*----CS2021000508 - 07.06.2021 - JT - fim
          17  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          18  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          19  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          20  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          21  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          22  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          23  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          24  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
          25  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim
      ENDIF.

    WHEN '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
    WHEN '03'. " OR '09' . "Troca de notas - Commodities com agrupamento
      PERFORM f_estrutura_alv USING:
          01  ''              ''             'IT_SAIDA' 'ICON'              'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'      'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'       'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          04  ''              ''             'IT_SAIDA' 'PLACA_CAV'         'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          05  ''              ''             'IT_SAIDA' 'REGION'            'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
          06  ''              ''             'IT_SAIDA' 'VBELN'             'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
          07  ''              ''             'IT_SAIDA' 'PESO_LIQ'          'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          08  'ZSDT0001'      'PESO_DESCARGA' 'IT_SAIDA' 'PESO_DESCARGA'     'Peso descarga'       ' '   'X'    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          09  ''              ''             'IT_SAIDA' 'PESO_RETIDO'       'Peso_retenção'       ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          10  ''              ''             'IT_SAIDA' 'PESO_LIQ_POS_RET'  'Peso_Liquido'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          10  ''              ''             'IT_SAIDA' 'CFOP'              'Cfop. Ent.'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          11  ''              ''             'IT_SAIDA' 'INCO1'             'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          12  ''              ''             'IT_SAIDA' 'ROUTE'             'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          13  ''              ''             'IT_SAIDA' 'KBETR'             'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          14  ''              ''             'IT_SAIDA' 'KONWA'             'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          15  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'             'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
          15  ''              ''             'IT_SAIDA' 'SHTYP'             'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          16  ''              ''             'IT_SAIDA' 'REMESSA'           'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          17  ''              ''             'IT_SAIDA' 'FATURA'            'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          18  ''              ''             'IT_SAIDA' 'DANFE'             'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          19  ''              ''             'IT_SAIDA' 'TRANSP'            'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          20  ''              ''             'IT_SAIDA' 'DOCCUS'            'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          21  ''              ''             'IT_SAIDA' 'OVSERV'            'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          22  ''              ''             'IT_SAIDA' 'FATSERV'           'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          23  ''              ''             'IT_SAIDA' 'DACTE'             'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
          24  ''              ''             'IT_SAIDA' 'OPERACAO'          'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          25  ''              ''             'IT_SAIDA' 'MATERIAL'          'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          26  ''              ''             'IT_SAIDA' 'NAME1_C'           'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
          27  ''              ''             'IT_SAIDA' 'NAME1'             'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
          28  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    WHEN '04'. "Fertilizantes (Porto Velho) - ZLES0115

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'BUKRS'         'Empresa'             '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'BRANCH'        'Filial'              '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'EBELN'         'Nro.Pedido'          ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'EBELP'         'Item.Pedido'         ' '   'X'    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'VBELN'         'Ordem/Ped.Trans.'    ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        14  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        14  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        15  'LFA1'          'LIFNR'        'IT_SAIDA' 'PONTO_COLETA'  'Ponto Coleta'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        16  'KNA1'          'KUNNR'        'IT_SAIDA' 'LOCAL_ENTREGA' 'Local Entrega'       ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        17  'ZFIWRT0009'    'NETPR'        'IT_SAIDA' 'NETPR'         'Vlr.Unit Rem'        ' '   'X'    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        18  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        19  ''              ''             'IT_SAIDA' 'SEQ_LCTO'      'Doc.ZNFW'            ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'DANFEZ'        'DANFE ZNFW'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        21  ''              ''             'IT_SAIDA' 'AVISO'         'Aviso Recbto'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',

        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
        22  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        23  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        24  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

        25  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        26  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        27  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        28  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        29  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
*        27  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
*        28  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
*        29  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP


*----CS2021000508 - 07.06.2021 - JT - inicio
        30  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    WHEN '05'. "Insumos - Sementes (Vendas e Trânsferências expedidas )

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        13  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        14  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        15  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        21  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        22  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        23  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        25  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    WHEN '06'. "Insumos - Defensivos (Vendas e Trânsferências expedidas )

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        13  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        14  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        15  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        21  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        22  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        23  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        25  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    WHEN '07'. "Insumos - Fertilizantes (Vendas e Trânsferências expedidas )

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        13  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        14  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        15  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        21  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        22  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        23  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        25  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim


    WHEN '09' OR '10'. "Romaneio de Entrada Completo

      PERFORM f_estrutura_alv USING:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        08  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        13  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        14  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        15  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        21  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        22  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        23  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        25  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

  ENDCASE.

ENDFORM.                    " F_ALV_FIELDCAT

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

FORM f_call_transaction USING p_trans
                              p_saida TYPE ty_saida
                     CHANGING p_erro.

  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  CLEAR: it_msg[], wg_documento, p_erro.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
              MODE wl_mode
     MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.

    p_erro = 'X'.

    PERFORM f_prepare_return3 TABLES it_msg.
    PERFORM f_grava_log_erro TABLES tg_log_erro USING p_saida.

  ELSEIF p_trans EQ 'VL31N'.
    READ TABLE it_msg WITH KEY msgnr  = '311' msgtyp = 'S'.
    IF sy-subrc = 0.
      MOVE it_msg-msgv2 TO wg_documento.
    ENDIF.

    IF  wg_documento IS INITIAL.
      p_erro = 'X'.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_documento
        IMPORTING
          output = wg_documento.
    ENDIF.

  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION

FORM f_estorno_custo CHANGING p_saida TYPE ty_saida.

  DATA vdata(10).
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vdata.

  IF ( p_saida-romaneios_agr[] IS INITIAL ) OR ( p_saida-deliverys[] IS INITIAL ).
    CLEAR: wl_erro.
    PERFORM f_set_romaneios_carga CHANGING p_saida
                                           wl_erro.
    CHECK wl_erro EQ abap_false.

    PERFORM f_set_delivery CHANGING p_saida
                                    wl_erro.
    CHECK wl_erro EQ abap_false.
  ENDIF.

  "Verifica se DACTE foi estornada
  IF ( p_saida-fatserv    IS NOT INITIAL ) AND
     ( p_saida-fatserv(1) NE '@'         ).
    MESSAGE 'Estorne desde a DACTE' TYPE  'S'.
    EXIT.
  ENDIF.

  IF ( ( p_saida-doccus    IS NOT INITIAL ) AND
       ( p_saida-doccus(1) NE '@'         )  )

     AND

     ( ( p_saida-st_proc EQ vg_st_finalizado    ) OR " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
       ( p_saida-st_proc EQ vg_st_aguard_doc_carg ) OR " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
       ( p_saida-st_proc EQ vg_st_fatura_frete  ) OR " Fatura Frete
       ( p_saida-st_proc EQ vg_st_ov_frete      ) OR " OV.Frete
       ( p_saida-st_proc EQ vg_st_custo         ) ).  " Doc.Custo

    "Estornar o documento de custo
    REFRESH ti_bdcdata.
    IF ( p_saida-inco1 NE 'CPT' ) AND ( NOT p_saida-enc_doc_custo = abap_true ).
      "Estornar o documento de custo
      REFRESH ti_bdcdata.
      IF  p_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH',
            ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-POSTX',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH',
                ''          ''      ''   'VFKPD-SLSTOR'     'X'.
      ENDIF.

    ELSE.
      IF  p_saida-shtyp = 'Z001'.
        PERFORM f_bdc_data USING:
            'SAPMV54A'  '0020'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
            ''          ''      ''   'BDC_OKCODE'       '=UEBP',
            ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
            ''          ''      ''   'BDC_OKCODE'       '=PLOE',
            ''          ''      ''   'VIM_MARKED(02)'   'X',

            'SAPLSPO1'  '0100'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=YES',

            'SAPMV54A'  '0030'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
            ''          ''      ''   'BDC_OKCODE'       '=PDET',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=PABR',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=KLAC',
            ''          ''      ''  'VFKPD-SLSTOR'      'X',

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '/00',
            ''          ''      ''   'VFKPD-STDAT'      vdata,

            'SAPMV54A'  '0040'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ELSE.
        PERFORM f_bdc_data USING:
                'SAPMV54A'  '0020'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
                ''          ''      ''   'BDC_OKCODE'       '=UEBP',
                ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

                'SAPMV54A'  '0030'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
                ''          ''      ''   'BDC_OKCODE'       '=PDET',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=PABR',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=KLAC',
                ''          ''      ''  'VFKPD-SLSTOR'      'X',

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '/00',
                ''          ''      ''   'VFKPD-STDAT'      vdata,

                'SAPMV54A'  '0040'  'X'  ''                 ' ',
                ''          ''      ''   'BDC_OKCODE'       '=SICH'.

      ENDIF.
    ENDIF.

    CLEAR wl_erro.
    PERFORM f_call_transaction USING 'VI02'
                                     p_saida
                            CHANGING wl_erro.
    IF wl_erro IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 5 SECONDS.

    ELSE.
      EXIT.
    ENDIF.

    "Eliminar o documento de custo
    REFRESH ti_bdcdata.
    PERFORM f_bdc_data USING:
           'SAPMV54A'  '0020'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
           ''          ''      ''   'BDC_OKCODE'       '=UEBP',
           ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

           'SAPMV54A'  '0030'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/ELOES'.

    CLEAR wl_erro.
    PERFORM f_call_transaction USING 'VI02'
                                     p_saida
                            CHANGING wl_erro.
    IF wl_erro IS INITIAL.

      p_saida-doccus  = icon_icon_list.

      LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom).

        UPDATE zsdt0001 SET st_proc = vg_st_transp
                            fknum   = ''
         WHERE ch_referencia = _wl_rom-ch_referencia.

        COMMIT WORK.

        READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
        IF sy-subrc EQ 0.
          <fs_saida_tmp>-doccus = icon_icon_list.
        ENDIF.
      ENDLOOP.

      WAIT UP TO 2 SECONDS.
    ELSE.
      EXIT.
    ENDIF.
  ENDIF.

*------------------------------------------------------------------*
*  Eliminar VT
*------------------------------------------------------------------*

  IF ( p_saida-st_proc EQ vg_st_finalizado    ) OR " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( p_saida-st_proc EQ vg_st_aguard_doc_carg ) OR " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( p_saida-st_proc EQ vg_st_fatura_frete  ) OR " Fatura Frete
     ( p_saida-st_proc EQ vg_st_ov_frete      ) OR " OV.Frete
     ( p_saida-st_proc EQ vg_st_custo         ) OR " Doc.Custo
     ( p_saida-st_proc EQ vg_st_transp        ).   " Transporte

    REFRESH t_itemdata.
    LOOP AT p_saida-deliverys INTO DATA(_wl_likp).
      CLEAR st_itemdata.
      st_itemdata-delivery  = _wl_likp-vbeln.
      st_itemdata-itenerary = '0010'.
      APPEND st_itemdata TO t_itemdata.
    ENDLOOP.

    DATA(p_tipo_chamada) = 'L'.
    PERFORM f_elimina_vt TABLES t_itemdata
                       CHANGING p_tipo_chamada p_saida t_return[].
  ENDIF.

  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Comentando codigo abaixo
  "Eliminar o aviso
*  IF ( p_saida-st_proc    EQ vg_st_aviso_rec     ) AND
*     ( vg_cockpit         NE '08'                ) AND "Não for Faturamento Frete Paranaguá
*     ( ( p_saida-aviso    IS NOT INITIAL         ) AND
*       ( p_saida-aviso(1) NE '@'                 ) ).
*
*    REFRESH ti_bdcdata.
*    PERFORM f_bdc_data USING:
*           'SAPMV50A'  '4104'  'X'  ''                 ' ',
*           ''          ''      ''   'BDC_OKCODE'       '/00',
*           ''          ''      ''   'LIKP-VBELN'       p_saida-aviso,
*
*           'SAPMV50A'  '1000'  'X'  ''                 ' ',
*           ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.
*
*    CLEAR wl_erro.
*    PERFORM f_call_transaction USING 'VL32N'
*                                     p_saida
*                            CHANGING wl_erro.
*    IF wl_erro IS INITIAL.
*      p_saida-aviso = icon_execute_object.
*      UPDATE zsdt0001 SET st_proc      = vg_st_aviso_rec_before
*                          doc_aviso    = ''
*      WHERE ch_referencia = p_saida-ch_referencia.
*
*      COMMIT WORK.
*
*      WAIT UP TO 2 SECONDS.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDIF.
  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP




ENDFORM.                    " F_ESTORNO_CUSTO

FORM f_estorno_cte CHANGING p_saida TYPE ty_saida.

  CLEAR: wl_erro.

  PERFORM f_set_romaneios_carga CHANGING p_saida
                                         wl_erro.
  CHECK wl_erro EQ abap_false.

  PERFORM f_set_delivery CHANGING p_saida
                                  wl_erro.
  CHECK wl_erro EQ abap_false.

  DATA(_ok) = ''.
  PERFORM f_check_permissao_estorno USING p_saida
                                          '2' "CT-e
                                 CHANGING _ok.

  CHECK _ok IS NOT INITIAL.

  "Cancela fatura
  IF ( p_saida-st_proc EQ vg_st_finalizado    ) OR " Finalizado
     ( p_saida-st_proc EQ vg_st_fatura_frete  ). " Fatura Frete

    PERFORM f_estorno_fatura USING p_saida-fatserv
                                   'X'                "Fatura de frete
                                   p_saida
                          CHANGING wl_erro.

  ENDIF.

  CHECK wl_erro IS INITIAL.

  IF ( p_saida-st_proc EQ vg_st_finalizado    ) OR " Finalizado
     ( p_saida-st_proc EQ vg_st_fatura_frete  ) OR " Fatura Frete
     ( p_saida-st_proc EQ vg_st_ov_frete      ). " OV.Frete

    PERFORM f_estorno_ov_frete USING p_saida-ovserv
                                     p_saida
                            CHANGING wl_erro.

  ENDIF.

  CHECK wl_erro IS INITIAL.

  IF ( p_saida-st_proc EQ vg_st_finalizado    ) OR " Finalizado
     ( p_saida-st_proc EQ vg_st_fatura_frete  ) OR " Fatura Frete
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( p_saida-st_proc EQ vg_st_aguard_doc_carg ) OR " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( p_saida-st_proc EQ vg_st_ov_frete      ) OR " OV.Frete
     ( p_saida-st_proc EQ vg_st_custo         ) OR " Doc.Custo
     ( p_saida-st_proc EQ vg_st_transp        ).   " Transporte


    PERFORM f_estorno_custo CHANGING p_saida.

  ENDIF.

ENDFORM.                    " F_ESTORNO_CTE

FORM f_estorno_nfe CHANGING p_saida TYPE ty_saida.

  DATA(_ok) = ''.
  PERFORM f_check_permissao_estorno USING p_saida
                                          '1' "NF-e
                                 CHANGING _ok.

  CHECK _ok IS NOT INITIAL.

  IF p_saida-operacao+0(4) NE 'ZRDC' AND
     p_saida-operacao+0(4) NE 'ZRFL' AND
     p_saida-operacao+0(4) NE 'ZIND' AND
     p_saida-operacao+0(3) NE 'ZUB'  AND
     p_saida-operacao+0(4) NE 'ZARM' AND
     p_saida-operacao+0(4) NE 'ZRAN'.


    IF ( p_saida-st_proc EQ vg_st_danfe  ) OR
       ( p_saida-st_proc EQ vg_st_fatura ).

      PERFORM f_estorno_fatura USING p_saida-fatura
                                     ''                "Não é frete
                                     p_saida
                            CHANGING wl_erro.
      CHECK wl_erro IS INITIAL.
    ENDIF.

    IF ( p_saida-st_proc EQ vg_st_danfe   ) OR
       ( p_saida-st_proc EQ vg_st_fatura  ) OR
       ( p_saida-st_proc EQ vg_st_remessa ).

      "Estornar Picking
      PERFORM f_estorno_picking_rem USING p_saida-remessa+0(10)
                                 CHANGING wl_erro
                                          p_saida.

      CHECK wl_erro IS INITIAL.

      "Estornar Remessa
      PERFORM f_estorno_remessa USING p_saida-remessa+0(10)
                             CHANGING wl_erro
                                      p_saida.

      CHECK wl_erro IS INITIAL.

      PERFORM f_after_estorno_remessa CHANGING p_saida.

    ENDIF.

    "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Comentando codigo abaixo
    "Eliminar o aviso
    IF ( p_saida-st_proc    EQ vg_st_aviso_rec     ) AND
       ( vg_cockpit         EQ '04'                ) AND
       ( ( p_saida-aviso    IS NOT INITIAL         ) AND
         ( p_saida-aviso(1) NE '@'                 ) ).

      REFRESH ti_bdcdata.
      PERFORM f_bdc_data USING:
             'SAPMV50A'  '4104'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'       '/00',
             ''          ''      ''   'LIKP-VBELN'       p_saida-aviso,

             'SAPMV50A'  '1000'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.

      CLEAR wl_erro.
      PERFORM f_call_transaction USING 'VL32N'
                                       p_saida
                              CHANGING wl_erro.
      IF wl_erro IS INITIAL.
        p_saida-aviso   = icon_execute_object.
        p_saida-st_proc = vg_st_aviso_rec_before.
        UPDATE zsdt0001 SET st_proc      = vg_st_aviso_rec_before
                            doc_aviso    = ''
        WHERE ch_referencia = p_saida-ch_referencia.

        COMMIT WORK.

        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
    "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP


  ELSEIF p_saida-operacao+0(4) EQ 'ZRDC' OR
         p_saida-operacao+0(4) EQ 'ZRFL' OR
         p_saida-operacao+0(4) EQ 'ZIND' OR
         p_saida-operacao+0(4) EQ 'ZRAN'.

    IF ( p_saida-st_proc EQ vg_st_danfe  ) OR
       ( p_saida-st_proc EQ vg_st_fatura ).
      PERFORM f_estorno_fatura USING p_saida-fatura
                                     ''
                                     p_saida
                            CHANGING wl_erro.
      CHECK wl_erro IS INITIAL.
    ENDIF.

    IF ( p_saida-st_proc EQ vg_st_danfe   ) OR
       ( p_saida-st_proc EQ vg_st_fatura  ) OR
       ( p_saida-st_proc EQ vg_st_remessa ).

      "MBST (estorno de migo)
      SELECT SINGLE *
        FROM zsdt0023 INTO @DATA(wa_zsdt0023)
       WHERE vbeln = @p_saida-remessa.

      SELECT SINGLE *
        FROM lips INTO @DATA(wa_lips)
       WHERE vbeln = @p_saida-remessa.

      "Se não existir item exclui somente a remessa
      IF ( sy-subrc NE 0 ) AND ( wa_zsdt0023-es_mblnr_s IS NOT INITIAL ).

        PERFORM f_excluir_remessa USING p_saida-remessa+0(10)
                                        p_saida
                               CHANGING wl_erro.

      ELSE.

        "Estornar Somente Picking
        PERFORM f_estorno_picking_rem USING p_saida-remessa+0(10)
                                   CHANGING wl_erro
                                            p_saida.

        CHECK wl_erro IS INITIAL.

        IF ( wa_zsdt0023 IS NOT INITIAL ). "Fluxo de documento remessa formação de lote

          "BAPI Estorno da MIGO - ENTRADA
          "//Só faz se ainda esta em branco o doc material estorno entrada
          IF wa_zsdt0023-es_mblnr_e IS INITIAL AND wa_zsdt0023-mblnr_e IS NOT INITIAL. " Só faz se ainda esta em branco o doc material estorno entrada
            CLEAR: wl_par_est_migo.
            wl_par_est_migo-mat_doc      = wa_zsdt0023-mblnr_e.
            wl_par_est_migo-doc_year     = wa_zsdt0023-mjahr_e.
            wl_par_est_migo-pstng_date   = wa_zsdt0023-dt_saida.
            wl_par_est_migo-ent_sai      = 'E'.
            wl_par_est_migo-form_lote    = 'X'.
            PERFORM f_estorno_migo USING wl_par_est_migo
                                         p_saida
                                CHANGING wl_erro.

            CHECK wl_erro IS INITIAL.
          ENDIF.

          "BAPI Estorno da MIGO - SAIDA
          "//Só faz se ainda esta em branco o doc material estorno saida
          IF wa_zsdt0023-es_mblnr_s IS INITIAL AND wa_zsdt0023-mblnr_s IS NOT INITIAL.
            CLEAR: wl_par_est_migo.
            wl_par_est_migo-mat_doc      = wa_zsdt0023-mblnr_s.
            wl_par_est_migo-doc_year     = wa_zsdt0023-mjahr_s.
            wl_par_est_migo-pstng_date   = wa_zsdt0023-dt_saida.
            wl_par_est_migo-ent_sai      = 'S'.
            wl_par_est_migo-form_lote    = 'X'.
            PERFORM f_estorno_migo USING wl_par_est_migo
                                         p_saida
                                CHANGING wl_erro.

            CHECK wl_erro IS INITIAL.
          ENDIF.
        ENDIF.

        "Estornar Somente Remessa
        PERFORM f_estorno_remessa USING p_saida-remessa+0(10)
                               CHANGING wl_erro
                                        p_saida.

        CHECK wl_erro IS INITIAL.

        PERFORM f_after_estorno_remessa CHANGING p_saida.

      ENDIF.

    ENDIF.
  ELSEIF p_saida-operacao+0(3) EQ 'ZUB'. "Pedido de transferencia

    IF ( p_saida-st_proc EQ vg_st_danfe   ) OR
       ( p_saida-st_proc EQ vg_st_fatura  ) OR
       ( p_saida-st_proc EQ vg_st_remessa ).

      "Estornar Picking
      PERFORM f_estorno_picking_rem USING p_saida-remessa+0(10)
                                 CHANGING wl_erro
                                          p_saida.

      CHECK wl_erro IS INITIAL.

      "Estornar Remessa
      PERFORM f_estorno_remessa USING p_saida-remessa+0(10)
                             CHANGING wl_erro
                                      p_saida.

      CHECK wl_erro IS INITIAL.

      PERFORM f_after_estorno_remessa CHANGING p_saida.

    ENDIF.

  ENDIF.

*  IF  p_saida-operacao+0(4) EQ 'ZARM'. "US #66690 - WPP
*
*    IF ( p_saida-st_proc EQ vg_st_danfe   ) OR
*       ( p_saida-st_proc EQ vg_st_fatura  ) OR
*       ( p_saida-st_proc EQ vg_st_remessa ).
*
*      "Estornar Movimento Estoque
*      DATA(_estornou_mov_estoque) = zcl_remessa_armazenagem=>zif_remessa_armazenagem~estornar_saida_estoque( i_ch_referencia = p_saida-ch_referencia ).
*
*      CHECK _estornou_mov_estoque IS NOT INITIAL.
*
*      p_saida-fatura = icon_execute_object.
*      p_saida-danfe  = icon_execute_object.
*
*      UPDATE zsdt0001 SET st_proc      = vg_st_fatura_before
*                          fatura_prod  = ''
*                          nro_nf_prod  = ''
*        WHERE ch_referencia = p_saida-ch_referencia.
*
*      "Estornar Remessa
*      PERFORM f_estorno_remessa USING p_saida-remessa+0(10)
*                             CHANGING wl_erro
*                                      p_saida.
*
*      CHECK wl_erro IS INITIAL.
*
*      PERFORM f_after_estorno_remessa CHANGING p_saida.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    " F_ESTORNO_NFE

FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

FORM f_montar_layout_log.
  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     1  ''   ''            'TI_ZLEST0100' 'CONT'     'Seq'         ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGID'    'ID'          ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGV1'    'Menssagem'   '60',
     1  ''   ''            'TI_ZLEST0100' 'DATA'     'Data'        '10',
     1  ''   ''            'TI_ZLEST0100' 'HORA'     'Hora'        '10',
     1  ''   ''            'TI_ZLEST0100' 'USUARIO'  'Usuário'     '15'.

ENDFORM.                    " MONTAR_LAYOUT

FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

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
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

FORM f_memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI

FORM f_lanc_carta_correcao .
  DATA : vl_length          TYPE i,
         vl_id              TYPE zcarta_correcao-id_cc,
         ls_zcarta_correcao TYPE zcarta_correcao,
         pa_docnum          TYPE j_1bdocnum.

*  CLEAR VL_ID.
*  SELECT SINGLE MAX( ID_CC )
*    FROM ZCARTA_CORRECAO
*    INTO VL_ID
*   WHERE DOCNUM      EQ WA_SAIDA-DANFE
*     AND AUTHCODE    EQ SPACE
*     AND NOVO_AGENTE NE SPACE.
*
*  IF VL_ID NE 0.
*    MESSAGE 'Já existe carta de correção enviada, aguarde retorno' TYPE 'I'.
*    EXIT.
*  ENDIF.

  CLEAR: vl_id.
  vl_length = strlen( txt_correc ).

  SELECT SINGLE MAX( id_cc )
    INTO vl_id
    FROM zcarta_correcao
   WHERE docnum EQ wa_saida-danfe.

  IF vl_id IS INITIAL .
    vl_id  = 0.
  ENDIF.

  vl_id = vl_id + 1.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN txt_correc WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN txt_correc WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN txt_correc WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN txt_correc WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN txt_correc WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN txt_correc WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN txt_correc WITH '&#38;'.
  REPLACE ALL OCCURRENCES OF        ''''    IN txt_correc WITH '&#39;'.
  REPLACE ALL OCCURRENCES OF        'º'     IN txt_correc WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'ª'     IN txt_correc WITH 'a' IGNORING CASE.

  SELECT SINGLE * INTO @DATA(lv_doc_fiscal)
    FROM j_1bnfe_active
   WHERE docnum EQ @wa_saida-danfe.

  ls_zcarta_correcao-docnum      = wa_saida-danfe.
  ls_zcarta_correcao-id_cc       = vl_id.
  ls_zcarta_correcao-model       = lv_doc_fiscal-model.
  ls_zcarta_correcao-msg_correc1 = txt_correc(250).
  ls_zcarta_correcao-msg_correc2 = txt_correc+250(250).
  ls_zcarta_correcao-msg_correc3 = txt_correc+500(250).
  ls_zcarta_correcao-msg_correc4 = txt_correc+750(250).
  ls_zcarta_correcao-usuario     = sy-uname.
  ls_zcarta_correcao-novo_agente = wa_ag_frete-transpor2.
  MODIFY zcarta_correcao FROM ls_zcarta_correcao.

  pa_docnum =  wa_saida-danfe.

  DATA: zcl_cce TYPE REF TO zcl_cce.
  FREE: zcl_cce.
  CREATE OBJECT zcl_cce.

  DATA: lc_id_registtro TYPE char12.
  lc_id_registtro = ls_zcarta_correcao-docnum && ls_zcarta_correcao-id_cc.

  zcl_cce->novo_registro( ).
  zcl_cce->zif_cadastro~set_registro( i_id_registro = lc_id_registtro ).
  zcl_cce->enviar( RECEIVING e_enviada = DATA(_enviada) ).
  CLEAR txt_correc.
  CALL METHOD obg_descbox->delete_text.

  IF _enviada EQ abap_false.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  COMMIT WORK.

  MESSAGE 'Carta de Correção enviada com sucesso!' TYPE 'S'.

ENDFORM.                    " LANC_CARTA_CORRECAO

FORM f_chk_estorno_fiscal USING p_saida  TYPE ty_saida
                                p_frete  TYPE c
                       CHANGING p_erro   TYPE c.

  DATA vdocnum_est TYPE j_1bdocnum.

  WAIT UP TO 5 SECONDS.
  CLEAR: vl_docnum,vl_refkey, p_erro.

  CHECK p_frete IS INITIAL.

  IF ( p_saida-tipo = 'P' ) OR ( p_saida-tipo = 'T' ).

    SELECT SINGLE vbeln mjahr
      INTO (vl_vbeln,vl_mjahr)
      FROM vbfa
      WHERE vbelv = p_saida-remessa+0(10)
      AND vbtyp_n  = 'R'
      AND vbtyp_v  = 'J'.

    CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO vl_docnum
      WHERE refkey = vl_refkey.
  ELSE.
    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO vl_docnum
      WHERE refkey = p_saida-fatura.
  ENDIF.

  CHECK sy-subrc = 0.

  CLEAR vcandat.
  SELECT SINGLE  candat
    FROM j_1bnfdoc
    INTO  vcandat
   WHERE docnum     = vl_docnum.

  IF vcandat IS INITIAL. "Documento Fiscal não está estornado ainda....

    "Verificar se documento esta autorizado na SEFAZ
    PERFORM f_check_auth_doc USING vl_docnum.

    IF sy-subrc NE 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          doc_number               = vl_docnum
          ref_type                 = space
          ref_key                  = space
          can_dat                  = sy-datum
        IMPORTING
          doc_number               = vdocnum_est
        EXCEPTIONS
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          nfe_cancel_simulation    = 6
          OTHERS                   = 7.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        p_erro = 'X'.
      ENDIF.
    ELSE.
      p_erro = 'X'.
    ENDIF.

    CHECK p_erro IS NOT INITIAL. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.

    CLEAR: wa_zlest0100, ti_zlest0100[], vl_ponteiro.

    SELECT  MAX( cont )
       FROM zlest0100
       INTO vl_ponteiro
       WHERE ch_referencia = p_saida-ch_referencia.

    IF sy-subrc = 0.
      ADD 1 TO vl_ponteiro.
    ELSE.
      vl_ponteiro = 1.
    ENDIF.

    wa_zlest0100-mandt         = sy-mandt.
    wa_zlest0100-ch_referencia = p_saida-ch_referencia.
    wa_zlest0100-msgtyp     = 'E'.
    wa_zlest0100-msgspra    = sy-langu.
    wa_zlest0100-msgid      = 'LES'.
    wa_zlest0100-msgnr      = '000'.
    wa_zlest0100-msgv1      = 'Danfe, não estornada, automaticamente ==>'.
    CONCATENATE wa_zlest0100-msgv1 vdocnum_est INTO wa_zlest0100-msgv1.
    wa_zlest0100-data       = sy-datum.
    wa_zlest0100-hora       = sy-uzeit.
    wa_zlest0100-usuario    = sy-uname.
    wa_zlest0100-cont       = vl_ponteiro.

    APPEND wa_zlest0100 TO ti_zlest0100.
    ADD 1 TO vl_ponteiro.

    MODIFY zlest0100 FROM TABLE ti_zlest0100.
  ENDIF.

ENDFORM.

FORM f_gerar_vt USING p_tipo
                      p_tipo_chamada TYPE char01
             CHANGING p_erro
                      p_saida TYPE ty_saida
                      it_tab_bapiret1 TYPE tab_bapiret1.

  TYPES: BEGIN OF ty_vbpa,
           parvw TYPE vbpa-parvw,
           kunnr TYPE vbpa-kunnr,
           lifnr TYPE vbpa-lifnr,
         END OF ty_vbpa.

  DATA: wl_tvro         TYPE tvro,
        it_vbpa         TYPE TABLE OF ty_vbpa,
        wl_vbpa         TYPE ty_vbpa,
        wl_a917         TYPE a917,
        v_auart         TYPE vbak-auart,
        v_bsart         TYPE ekko-bsart,
        v_lifnr         TYPE lfa1-lifnr,
        v_ktokk         TYPE lfa1-ktokk,
        v_lzonel        TYPE lfa1-lzone,
        v_lzonek        TYPE kna1-lzone,
        v_kunnr         TYPE vbpa-kunnr,
        v_stcd1         TYPE kna1-stcd1,
        v_route         TYPE trolz-route,
        v_knote         TYPE tvkn-knote,
        it_lfa1_tmp     TYPE TABLE OF lfa1 WITH HEADER LINE,
        it_matnr        TYPE TABLE OF zsdt0001-matnr WITH HEADER LINE,
        wa_tab_bapiret1 LIKE LINE OF it_tab_bapiret1,
        v_msgi(200).


  "Tabelas BAPI

  DATA: st_headerdata       TYPE bapishipmentheader,
        st_stagedata        TYPE bapishipmentstage,
        t_stagedata         TYPE TABLE OF bapishipmentstage,
        t_itemdata          TYPE TABLE OF bapishipmentitem,
        st_itemdata         TYPE bapishipmentitem,
        st_headerdata2      TYPE bapishipmentheader,
        st_headerdataaction TYPE bapishipmentheaderaction,
        st_zlest0002        TYPE zlest0002,
        lc_saida            TYPE ty_saida,
        it_saida_carga      TYPE TABLE OF ty_saida WITH HEADER LINE.

  DATA: tx_msg  TYPE string,
        txterro TYPE string.


  DATA: ws_zlest0135 TYPE zlest0135,
        e_msg_erro   TYPE bapiret2,
        e_status     TYPE sy-subrc.


  CLEAR: v_tknum, p_erro, it_saida_carga[].

  IF ( p_saida-romaneios_agr[] IS INITIAL ) OR ( p_saida-deliverys[] IS INITIAL ).
    CLEAR: p_erro.
    PERFORM f_set_romaneios_carga CHANGING p_saida
                                           p_erro.
    CHECK p_erro EQ abap_false.

    PERFORM f_set_delivery CHANGING p_saida
                                    p_erro.
    CHECK p_erro EQ abap_false.
  ENDIF.

  "Pré Validações
  CLEAR: wa_zsdt0001.
  SELECT SINGLE *
    FROM zsdt0001 INTO wa_zsdt0001
   WHERE ch_referencia = p_saida-ch_referencia.

  IF wa_zsdt0001-fknum GT 0.
    tx_msg = 'Documento atualizado, click em <ATUALIZAR>'.
    CASE p_tipo_chamada.
      WHEN 'L'.
        MESSAGE tx_msg TYPE 'I'.
      WHEN 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    ENDCASE.
    EXIT.
  ENDIF.

  IF sy-tcode NE 'ZLES0136' AND sy-tcode NE 'ZMM0127' AND p_tipo_chamada NE 'E'.
    tx_msg = 'Transação apenas de visualização'.
    CASE p_tipo_chamada.
      WHEN 'L'.
        MESSAGE tx_msg TYPE 'I'.
      WHEN 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    ENDCASE.
    EXIT.
  ENDIF.

  "Validar tipo de veiculo.
  "=======================================================================================
  CLEAR: tx_msg, txterro.
  IF wa_zsdt0001-placa_car1 IS NOT INITIAL.
    CLEAR: st_zlest0002.
    SELECT SINGLE * FROM zlest0002 INTO st_zlest0002
    WHERE pc_veiculo EQ wa_zsdt0001-placa_car1 AND tp_veiculo EQ '0'.
    IF st_zlest0002 IS NOT INITIAL.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car1 } |.
      tx_msg = txterro.
    ENDIF.
    CLEAR txterro.
  ENDIF.

  IF wa_zsdt0001-placa_car2 IS NOT INITIAL.
    CLEAR: st_zlest0002.
    SELECT SINGLE * FROM zlest0002 INTO st_zlest0002
    WHERE pc_veiculo EQ wa_zsdt0001-placa_car2 AND tp_veiculo EQ '0'.
    IF st_zlest0002 IS NOT INITIAL.
      CLEAR: txterro.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car2 } |.
      IF tx_msg IS NOT INITIAL.
        tx_msg = |{ tx_msg } / { wa_zsdt0001-placa_car2 } |.
      ELSE.
        tx_msg = txterro.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wa_zsdt0001-placa_car3 IS NOT INITIAL.
    CLEAR: st_zlest0002.
    SELECT SINGLE * FROM zlest0002 INTO st_zlest0002
    WHERE pc_veiculo EQ wa_zsdt0001-placa_car3 AND tp_veiculo EQ '0'.
    IF st_zlest0002 IS NOT INITIAL.

      CLEAR: txterro.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car3 } |.
      IF tx_msg IS NOT INITIAL.
        tx_msg = |{ tx_msg } / { wa_zsdt0001-placa_car3 } |.
      ELSE.
        tx_msg = txterro.
      ENDIF.
    ENDIF.
  ENDIF.

  IF tx_msg IS NOT INITIAL.
    MESSAGE tx_msg TYPE 'I'.
    EXIT.
  ENDIF.
  "======================================================================

  IF p_saida-fatura IS NOT INITIAL.
    "Verifica Estorno Fatura
    SELECT SINGLE vbeln mjahr
       INTO (vl_vbeln,vl_mjahr)
       FROM vbfa
      WHERE vbelv = p_saida-fatura
        AND vbtyp_n  = 'N'. "estorno
    IF sy-subrc = 0.
      tx_msg = 'O doc de fatura está cancelado. Refazer o lançamento!'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

    CLEAR vl_fksto.
    SELECT SINGLE fksto
      FROM vbrk INTO vl_fksto
     WHERE vbeln = p_saida-fatura.

    IF  vl_fksto = 'X'.
      tx_msg = 'o doc de fatura está cancelado. Refazer o lançamento'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE lifnr name1 dlgrp
    FROM lfa1 INTO wa_lfa1
   WHERE lifnr = p_saida-lifnr.

  IF sy-subrc = 0 AND wa_lfa1-dlgrp NOT IN r_dlgrp AND
     (
       ( p_saida-inco1 NE 'FOB' AND p_saida-inco1 NE 'CFR' ) OR
       ( p_saida-enc_conhecimento EQ abap_true )
     ).

    tx_msg = |Fornecedor { p_saida-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.|.
    CASE p_tipo_chamada.
      WHEN 'L'.
        MESSAGE tx_msg TYPE 'I'.
      WHEN 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    ENDCASE.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @p_saida-shtyp.

  IF ( sy-subrc NE 0 ) OR ( wl_tvtk-abfer IS INITIAL ).
    tx_msg = 'Tipo de Transporte não encontrado!'.
    CASE p_tipo_chamada.
      WHEN 'L'.
        MESSAGE tx_msg TYPE 'I'.
      WHEN 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    ENDCASE.
    EXIT.
  ENDIF.
  "Fim - Pré Validações

  IF ( line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ) ) AND ( p_tipo NE 'T' ).
    EXIT.
  ENDIF.

  IF ( lines( p_saida-romaneios_agr[] ) > 1 ) AND ( p_tipo NE 'T' ).
    EXIT.
  ENDIF.

  IF ( p_saida-inco1 NE 'FOB' AND p_saida-inco1 NE 'CFR' ) OR
     ( p_saida-enc_conhecimento EQ abap_true ).

    IF p_saida-kbetr LE 0.
      tx_msg = 'Não existe valor de frete cadastrado. Solicite à transportadora da sua região'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE i000(z01) WITH 'Não existe valor de frete cadastrado.' 'Solicite à transportadora da sua região'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.
    IF p_saida-cont_fre GT 1.
      tx_msg = 'Existe mais de um valor de frete cadastrado. Solicite a regularização à transportadora da sua região'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE i000(z01) WITH 'Existe mais de um valor de frete cadastrado.' 'Solicite a regularização à transportadora ' 'da sua região'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

    IF p_saida-lifnr IS INITIAL.
      tx_msg = 'Agente de frete não informado!'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

  ENDIF.

  READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = p_saida-ch_referencia BINARY SEARCH.
  CHECK sy-subrc = 0.
  IF wa_zsdt0001-agente_frete IS INITIAL.
    wa_zsdt0001-agente_frete = p_saida-lifnr.
  ENDIF.

  DATA(_error) = abap_false.
  PERFORM f_set_romaneios_carga CHANGING p_saida
                                         _error.
  CHECK _error EQ abap_false.

  PERFORM f_valida_geracao_vt CHANGING p_saida
                                       _error.
  CHECK _error EQ abap_false.

  CLEAR: it_matnr[].

  IF wa_zsdt0001-matnr IS NOT INITIAL.
    it_matnr = wa_zsdt0001-matnr.

    "CS2017002682 - 29.11.2017 - Ini
    IF ( p_saida-tipo = 'O' ) AND  ( vg_cockpit = '04' ).
      READ TABLE it_zsdt0062 INTO wa_zsdt0062 WITH KEY vbeln = p_saida-vbeln
                                                       ebeln = p_saida-ebeln
                                                       ebelp = p_saida-ebelp BINARY SEARCH.
      IF sy-subrc NE 0.
        tx_msg = 'Este pedido/item não pertence a esta Ordem! Caso seja necessário, solicite a vinculação à equipe de insumos.'.
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE i000(z01) WITH 'Este pedido/item não pertence a esta Ordem!' 'Caso seja necessário, solicite a vinculação ' 'à equipe de insumos.'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM ekpo INTO @DATA(_wl_ekpo)
       WHERE ebeln = @p_saida-ebeln
         AND ebelp = @p_saida-ebelp.
      IF ( sy-subrc NE 0 ) OR ( _wl_ekpo-matnr IS INITIAL ).
        tx_msg = 'Pedido de Importação não existe!'.
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.

      it_matnr = _wl_ekpo-matnr.
    ENDIF.
    APPEND it_matnr.
  ELSE.
    LOOP AT tg_zsdt0001_item WHERE ch_referencia = wa_zsdt0001-ch_referencia
                               AND matnr IS NOT INITIAL.
      it_matnr = tg_zsdt0001_item-matnr.
      APPEND it_matnr.
    ENDLOOP.
  ENDIF.

  LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom) WHERE matnr IS NOT INITIAL.
    it_matnr = _wl_rom-matnr.
    APPEND it_matnr.
  ENDLOOP.

  SORT it_matnr.
  DELETE ADJACENT DUPLICATES FROM it_matnr.

  IF it_matnr[] IS INITIAL.
    tx_msg = 'Nenhum material encontrado para a carga!'.
    CASE p_tipo_chamada.
      WHEN 'L'.
        MESSAGE tx_msg TYPE 'I'.
      WHEN 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    ENDCASE.
    EXIT.
  ENDIF.


  IF p_saida-inco1 = 'CIF'.

*** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023.
*    SELECT SINGLE *
*      from tvarvc INTO @DATA(lwa_tvarv_zseg)
*     WHERE name = 'ZLES0136_EXC_ZSEG'
*       AND LOW  = @wa_zsdt0001-agente_frete.

    CLEAR: ws_zlest0135.
    CALL FUNCTION 'Z_LES_EXC_ZSEG'
      EXPORTING
        i_placa       = wa_zsdt0001-placa_cav
        i_ck_consulta = abap_true
      IMPORTING
        e_status      = e_status
        e_msg_erro    = e_msg_erro.



***** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / AOENNING.

    CASE e_status.
      WHEN 1. "Se houve erro na comunicação da API.

        MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
        RETURN.

      WHEN 2. "Check condição tp_transportador "ETC Não equiparado.


        LOOP AT it_matnr.
          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          SELECT SINGLE *
            FROM a917
            INTO wl_a917
           WHERE kappl = 'F'
             AND kschl        = 'ZSEG'
             AND matnr        = it_matnr
             AND tdlnr        = wa_zsdt0001-agente_frete
             AND kfrst        = ''
             AND datbi        GE sy-datum.

          IF sy-subrc NE 0.
            CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
            CASE p_tipo_chamada.
              WHEN 'L'.
                MESSAGE i000(z01) WITH 'Não existe % p/ desc. de Seguro. Mat.:' it_matnr v_msgi.
              WHEN 'E'.
                MESSAGE i000(z01) WITH 'Não existe % p/ desc. de Seguro. Mat.:' it_matnr v_msgi INTO tx_msg.
                "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
            ENDCASE.
            RETURN.
          ENDIF.

          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          SELECT SINGLE *
            FROM a917
            INTO wl_a917
           WHERE kappl = 'F'
             AND kschl = 'ZIOF'
             AND matnr = it_matnr
             AND tdlnr = wa_zsdt0001-agente_frete
             AND kfrst = ''
             AND datbi GE sy-datum.

          IF sy-subrc NE 0.
            CONCATENATE 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' INTO v_msgi SEPARATED BY space.
            CASE p_tipo_chamada.
              WHEN 'L'.
                MESSAGE i000(z01) WITH 'Não existe % p/ desc. de IOF Mat.:' it_matnr v_msgi.
              WHEN 'E'.
                MESSAGE i000(z01) WITH 'Não existe % p/ desc. de IOF Mat.:' it_matnr v_msgi INTO tx_msg.
                "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
            ENDCASE.
            RETURN.
          ENDIF.
        ENDLOOP.


      WHEN 3. "Se não localizou dados, retorna a msg que retornou no JSON da API.

        MESSAGE i000(z01) WITH e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
        RETURN.

      WHEN OTHERS.
    ENDCASE.


  ENDIF.

*-----------------------------------------------------------------------------------*
*  Preenche BAPI
*-----------------------------------------------------------------------------------*

  CLEAR: wa_tvtk.
  SELECT SINGLE shtyp laufk vsart
    FROM tvtk
    INTO wa_tvtk
   WHERE shtyp = wa_zsdt0001-shtyp.

  " Cabeçalho
  CLEAR st_headerdata.
  st_headerdata-service_agent_id        = wa_zsdt0001-agente_frete.

*  st_headerdata-text_1                  = wa_zsdt0001-placa_cav.



  zcl_atribui_rom_doctrans=>set_info_rom_doc_trans(
    EXPORTING
      i_rom      = wa_zsdt0001    " Estrutura para LES de Saída de Carga
    CHANGING
      i_doctrans = st_headerdata    " TransportBAPI, Dados de cabeçalho
  ).

  st_headerdata-service_level           = '1'.
  st_headerdata-shipping_type           = wa_tvtk-vsart.
  st_headerdata-status_plan             = 'X'.
  st_headerdata-status_checkin          = 'X'.
  st_headerdata-status_load_start       = 'X'.
  IF p_saida-krech = 'A'. "Percentual.
    st_headerdata-special_procedure_id 	  = '0003'.
  ELSE.
    st_headerdata-special_procedure_id 	  = '0001'.
  ENDIF.
  st_headerdata-shpmnt_cost_rel         = 'X'.
  st_headerdata-shipment_type           = wa_zsdt0001-shtyp.

  "Início - LES - CS2023000206 - Ajuste contab. CT-e Transf. #107013 RSA
  st_headerdata-trans_plan_pt           = wa_zsdt0001-branch.
  IF wa_zsdt0001-shtyp = 'Z020'.
    CLEAR : st_headerdata-trans_plan_pt.
    st_headerdata-trans_plan_pt         = wa_zsdt0001-id_cli_dest+6(4).
  ENDIF.
  "Fim - LES - CS2023000206 - Ajuste contab. CT-e Transf. #107013 RSA

  IF wa_zsdt0001-id_interface = '49'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_saida-local_entrega
      IMPORTING
        output = p_saida-local_entrega.

    st_headerdata-trans_plan_pt = p_saida-local_entrega+6(4).
  ENDIF.

  v_lifnr = p_saida-lifnr_c.

  SELECT SINGLE lzone ktokk
    FROM lfa1
    INTO (v_lzonel, v_ktokk)
    WHERE lifnr   = v_lifnr.

  CHECK sy-subrc = 0.

  CLEAR v_route.

  v_route = p_saida-route.
  st_headerdata-shipment_route  = v_route.

  SELECT SINGLE *
    FROM tvro
    INTO wl_tvro
    WHERE route    = v_route.

  CHECK sy-subrc = 0.
  IF wl_tvro-traztd GT 24.
    wl_tvro-traztd = wl_tvro-traztd / 24.
  ENDIF.

  st_headerdata-distance        = wl_tvro-distz.
  st_headerdata-distance_unit   = wl_tvro-medst.
  st_headerdata-time_travel     =	wl_tvro-traztd.
  st_headerdata-time_unit       =	'H'.

*ETAPA
  CLEAR st_stagedata.
  REFRESH t_stagedata.
  st_stagedata-stage_cat      = '1'.
  st_stagedata-stage_seq     	= '0001'.
  st_stagedata-service_agent  = wa_zsdt0001-agente_frete.

  "Local de Partida
  IF v_ktokk = 'ZFIC'.
    st_stagedata-org_shipp_dpmnt = v_lifnr+6(4).
  ELSE.
    st_stagedata-org_suppl  = v_lifnr.
  ENDIF.

  v_kunnr = p_saida-local_entrega.

  CHECK sy-subrc = 0.

  st_stagedata-shipping_type  = wa_tvtk-vsart.
  st_stagedata-leg_indicator  = wa_tvtk-laufk. "Código de Percurso

  "Se Código de Percurso  igual a 1:  Percurso preliminar
  IF wa_tvtk-laufk = 1.
    CLEAR v_knote.

    SELECT SINGLE knote
      FROM tvkn
      INTO v_knote
     WHERE kunnr   = v_kunnr.

    IF sy-subrc = 0.
      st_stagedata-dest_point   = v_knote.
    ELSE.
      st_stagedata-dest_point   = v_kunnr.
    ENDIF.

    "Se Código de Percurso  igual a 4:  Percurso direto
  ELSEIF wa_tvtk-laufk = 4.

    IF ( st_headerdata-shipment_type  =  'Z004' ) OR
       ( st_headerdata-shipment_type  =  'Z029' ).

      CLEAR: it_lfa1_tmp[], v_lifnr, v_stcd1.

      SELECT SINGLE stcd1
        FROM kna1
        INTO v_stcd1
       WHERE kunnr = v_kunnr.

      IF v_stcd1 IS NOT INITIAL.
        SELECT *
          FROM lfa1 INTO TABLE it_lfa1_tmp
         WHERE stcd1 = v_stcd1.

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_branch_vt)
         WHERE branch EQ @st_headerdata-trans_plan_pt.

        IF sy-subrc EQ 0.
          "Elimina Fornecedores Bloqueados
          PERFORM f_elimina_lfa1_bloq TABLES it_lfa1_tmp
                                       USING _wl_branch_vt-bukrs.
        ENDIF.

        LOOP AT it_lfa1_tmp.
          v_lifnr = it_lfa1_tmp-lifnr.
          EXIT.
        ENDLOOP.
      ENDIF.

      IF v_lifnr IS INITIAL.
        CONCATENATE 'Não foi possivel encontrar na transação XK03 o parceiro fornecedor na empresa:' _wl_branch_vt-bukrs 'referente ao codigo cliente: ' v_kunnr INTO v_msgi SEPARATED BY space.
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE i000(z01) WITH v_msgi(50) v_msgi+50(50) v_msgi+100(50) .
          WHEN 'E'.
            MESSAGE i000(z01) WITH v_msgi(50) v_msgi+50(50) v_msgi+100(50) INTO tx_msg.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        RETURN.
      ENDIF.

      st_stagedata-dest_suppl  = v_lifnr.

    ELSE.
      st_stagedata-dest_cust    = v_kunnr. "Local de entrega (V_KUNNR)
    ENDIF.

  ENDIF.

  APPEND st_stagedata TO t_stagedata.

*Dados itens
  CLEAR st_itemdata.
  REFRESH t_itemdata.
  LOOP AT p_saida-deliverys INTO DATA(_wl_likp).
    st_itemdata-delivery      = _wl_likp-vbeln.
    st_itemdata-itenerary     = '000010'.
    APPEND st_itemdata TO t_itemdata.
  ENDLOOP.

  CLEAR v_tknum.
  REFRESH t_return_vt.

  "------> Gera o Transporte <------
  CALL FUNCTION 'BAPI_SHIPMENT_CREATE'
    EXPORTING
      headerdata = st_headerdata
    IMPORTING
      transport  = v_tknum
    TABLES
      itemdata   = t_itemdata
      stagedata  = t_stagedata
      return     = t_return_vt.

  IF v_tknum IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    p_erro = 'X'.

    IF p_tipo_chamada = 'E'.
      LOOP AT t_return_vt INTO DATA(wa_erros_vt).
        CLEAR: wa_tab_bapiret1.
        MOVE-CORRESPONDING wa_erros_vt TO wa_tab_bapiret1.
        APPEND wa_tab_bapiret1 TO it_tab_bapiret1.
      ENDLOOP.
    ENDIF.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    CLEAR : st_headerdataaction, st_headerdata2.
    REFRESH: t_return_vt, it_tab_bapiret1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    st_headerdata2-status_load_end       = 'X'.
    st_headerdataaction-status_load_end  = 'C'.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        return           = t_return_vt.

    IF p_tipo_chamada = 'E'.
      LOOP AT t_return_vt INTO wa_erros_vt.
        CLEAR: wa_tab_bapiret1.
        MOVE-CORRESPONDING wa_erros_vt TO wa_tab_bapiret1.
        APPEND wa_tab_bapiret1 TO it_tab_bapiret1.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    IF p_saida-inco1 = 'CIF' AND p_tipo_chamada EQ 'L'.
      CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
        EXPORTING
          p_tknum      = st_headerdata2-shipment_num
        EXCEPTIONS
          adiantamento = 1
          pedagio      = 2
          OTHERS       = 3.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      p_erro = abap_true.

      IF p_tipo_chamada = 'L'.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Estorna VT
      REFRESH t_itemdata.

      LOOP AT p_saida-deliverys INTO _wl_likp.
        CLEAR st_itemdata.
        st_itemdata-delivery  = _wl_likp-vbeln.
        st_itemdata-itenerary = '0010'.
        APPEND st_itemdata TO t_itemdata.
      ENDLOOP.

      MOVE-CORRESPONDING wa_zsdt0001 TO lc_saida.
      lc_saida-transp = st_headerdata2-shipment_num.
      PERFORM f_elimina_vt TABLES t_itemdata CHANGING p_tipo_chamada lc_saida it_tab_bapiret1.

    ELSE.

      st_headerdata2-status_compl             = 'X'.
      st_headerdata2-status_shpmnt_start      = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = st_headerdata2
          headerdataaction = st_headerdataaction
        TABLES
          return           = t_return_vt.
*
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF p_tipo_chamada = 'E'.
        LOOP AT t_return_vt INTO wa_erros_vt.
          CLEAR: wa_tab_bapiret1.
          MOVE-CORRESPONDING wa_erros_vt TO wa_tab_bapiret1.
          APPEND wa_tab_bapiret1 TO it_tab_bapiret1.
        ENDLOOP.
      ENDIF.

      "atualiza valor do frete Historico
      LOOP AT p_saida-romaneios_agr INTO _wl_rom.
        UPDATE zsdt0001 SET kbetr = p_saida-kbetr konwa = p_saida-konwa agente_frete = p_saida-lifnr WHERE ch_referencia = _wl_rom-ch_referencia.
      ENDLOOP.

      IF p_saida-id_ordem IS NOT INITIAL.
        UPDATE zlest0155 SET ch_referencia = p_saida-ch_referencia WHERE id_ordem = p_saida-id_ordem.
      ENDIF.
      p_erro = 'N'.

    ENDIF.
  ENDIF.

  PERFORM f_prepare_return TABLES t_return_vt.
  PERFORM f_grava_log_erro TABLES tg_log_erro USING p_saida.

ENDFORM.                    " F_GERAR_VT

FORM f_pega_frete.

  DATA: v_cont_ped  TYPE i.

  DATA: v_kunnr_lr   TYPE kunnr,
        v_lifnr_z1   TYPE lifnr,
        v_lifnr_sp   TYPE lifnr,
        v_lifnr_ori  TYPE lifnr,
        v_tp_veiculo TYPE zde_tp_prop_veiculo.

  REFRESH: it_a900, it_a910, it_a911, it_a915, it_a918, it_a919, it_konp.

  CHECK it_zsdt0001_fre[] IS NOT INITIAL.

*---> 11.07.2023 10:37:58 - Migração S4 - DL
  SORT it_ekpv BY ebeln.
  SORT it_ekko BY ebeln.
  SORT it_ekpa_pr BY ebeln.
  SORT it_ekpo BY ebeln.
  SORT it_likp BY vbeln.
  SORT it_vbpa BY vbeln parvw.
  SORT it_vbkd BY vbeln.
  SORT it_vbap BY vbeln.
  SORT it_kna1 BY kunnr.
  SORT it_kna1 BY kunnr.
  SORT it_vbkd BY vbeln.
*<--- 11.07.2023 10:37:58 - Migração S4 - DL


  LOOP AT it_zsdt0001_fre ASSIGNING FIELD-SYMBOL(<out_zsdt0001>).

    DATA(tabix) = sy-tabix.

    CLEAR wa_saida.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
    IF sy-subrc = 0.

      CLEAR: wa_vbkd.

      " Exclui Romaneio do SET
      READ TABLE t_auart WITH KEY from = wa_vbak-auart BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "ZONA coleta ordem
      READ TABLE it_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
        <out_zsdt0001>-lzonea = wa_lfa1-lzone.
      ENDIF.
      "Zona Local de entrega
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          <out_zsdt0001>-lzonez = wa_kna1-lzone.
        ENDIF.
      ENDIF.

      "Agregado
      SELECT SINGLE agregado
        FROM zlest0002 INTO @DATA(v_agregado)
       WHERE pc_veiculo = @<out_zsdt0001>-placa_cav.

      IF sy-subrc = 0.
        IF v_agregado = 1.
          <out_zsdt0001>-add01 = '0000000001'.
        ELSE.
          <out_zsdt0001>-add01 = '0000000002'.
        ENDIF.
      ENDIF.

      "Itinerário
      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        <out_zsdt0001>-route = wa_vbap-route.
      ENDIF.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH.

      "Definir Tipo de transporte
      CASE vg_cockpit. "Validar Regra ###
        WHEN '01' OR '05' OR '06' OR '07' OR '09' OR '03'.

          CLEAR: v_kunnr_lr, v_lifnr_z1, v_lifnr_sp.

          IF ( 'ZRDC_ZRFL_ZIND' CS wa_vbak-auart ) AND ( wa_vbak-auart IS NOT INITIAL ).
            "Determinar Local de Entrega
            READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'LR' BINARY SEARCH.
            IF sy-subrc EQ 0.
              v_kunnr_lr = wa_vbpa-kunnr.
            ENDIF.

            "Determinar Terminal Porto
            READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'Z1' BINARY SEARCH.
            IF sy-subrc EQ 0.
              v_lifnr_z1 = wa_vbpa-lifnr.
            ENDIF.

            "Determinar Agente Frete
            READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'SP'
                                                      dlgrp = '0007'. "Multimodal
            IF sy-subrc = 0.

              v_lifnr_sp = wa_vbpa-lifnr.

            ELSEIF ( wa_vbkd-inco1 = 'CIF' ).

              READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                        parvw = 'SP' BINARY SEARCH.
              IF sy-subrc = 0.
                v_lifnr_sp = wa_vbpa-lifnr.
                PERFORM f_troca_agente USING <out_zsdt0001>-placa_cav <out_zsdt0001>-branch CHANGING v_lifnr_sp.
              ENDIF.

            ENDIF.

*            IF ( <OUT_ZSDT0001>-AGENTE_FRETE IS INITIAL ) AND ( V_LIFNR_SP IS NOT INITIAL ).
            IF  ( v_lifnr_sp IS NOT INITIAL ).
              <out_zsdt0001>-agente_frete = v_lifnr_sp.
              PERFORM f_troca_agente USING <out_zsdt0001>-placa_cav <out_zsdt0001>-branch CHANGING <out_zsdt0001>-agente_frete.
            ENDIF.

          ENDIF.

          TRY.
              zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                EXPORTING
                  i_tipo_mov       = CONV #( wa_vbak-tp_movimento )
                  i_vsart          = '01'  "Rodoviario
                  i_tipo_ov        = CONV #( wa_vbak-auart )
                  i_parid_lr       = CONV #( v_kunnr_lr )
                  i_parid_z1       = CONV #( v_lifnr_z1 )
                  i_parid_sp       = CONV #( v_lifnr_sp )
                IMPORTING
                   e_shtyp         = DATA(_shtyp) ).

              <out_zsdt0001>-shtyp = _shtyp.
            CATCH zcx_faturamento.
            CATCH zcx_error.
          ENDTRY.

          "Comentario Final Codigo - 0001 - 18.04.2019

        WHEN '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
        WHEN '03'. "Commodities (Trânsferência Recebidas )
        WHEN '04'.  "Fertilizantes (Porto Velho) - ZLES0115

          IF <out_zsdt0001>-ebeln IS INITIAL.
            CLEAR v_cont_ped.
            LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE vbeln = <out_zsdt0001>-vbeln
                                                   AND matnr = <out_zsdt0001>-matnr. "CS2017002682 - 29.11.2017
              <out_zsdt0001>-ebeln  = wa_zsdt0062-ebeln. "PEDIDO Importação
              <out_zsdt0001>-ebelp  = wa_zsdt0062-ebelp. "CS2017002682 - 29.11.2017
              ADD 1 TO v_cont_ped.
            ENDLOOP.
            IF v_cont_ped GT 1.
              CLEAR: <out_zsdt0001>-ebeln, <out_zsdt0001>-ebelp.
            ENDIF.
          ENDIF.

          "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
          TRY.
              zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                EXPORTING
                  i_tipo_mov       = CONV #( wa_vbak-tp_movimento )
                  i_vsart          = '01'  "Rodoviario
                  i_tipo_ov        = CONV #( wa_vbak-auart )
                IMPORTING
                   e_shtyp         = _shtyp ).

              <out_zsdt0001>-shtyp = _shtyp.
            CATCH zcx_faturamento.
            CATCH zcx_error.
          ENDTRY.

*          IF <out_zsdt0001>-ebeln IS NOT INITIAL.
*            READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = <out_zsdt0001>-ebeln BINARY SEARCH. " Pedidos de importação de fertilizante
*            IF sy-subrc = 0.
*              TRY.
*                  zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
*                    EXPORTING
*                      i_tipo_mov       = CONV #( wa_ekko-tp_movimento )
*                      i_vsart          = '01'  "Rodoviario
*                      i_tipo_pedido    = CONV #( wa_ekko-bsart )
*                    IMPORTING
*                       e_shtyp         = _shtyp ).
*
*                  <out_zsdt0001>-shtyp = _shtyp.
*                CATCH zcx_faturamento.
*                CATCH zcx_error.
*              ENDTRY.
*              "Comentario Final Codigo - 0002 - 18.04.2019
*            ENDIF.
*          ENDIF.
          "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

      ENDCASE.

    ELSE." Pedido de Compra

      CASE vg_cockpit. "Validar Regra ###
        WHEN '01' OR '05' OR '06' OR '07' OR '09'.

          "Itinerário
          READ TABLE it_ekpv INTO wa_ekpv WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedido
          IF sy-subrc = 0.
            <out_zsdt0001>-route = wa_ekpv-route.
          ENDIF.
          READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
          IF sy-subrc = 0 .

            "---> US #66690 - WPP
*            READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
*            IF sy-subrc = 0.
*
*              CASE wa_ekko-bsart.
*                WHEN 'ZARM'.
*
*                  IF <out_zsdt0001>-route IS INITIAL.
*
*                    CLEAR: <out_zsdt0001>-lzonea, <out_zsdt0001>-lzonez.
*
*                    READ TABLE it_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH.
*                    IF sy-subrc = 0.
*                      wa_saida-ponto_coleta = wa_ekpa_pr-lifn2.
*
*                      SELECT SINGLE lifnr name1 dlgrp lzone
*                        FROM lfa1 INTO wa_lfa1
*                       WHERE lifnr = wa_saida-ponto_coleta.
*
*                      IF sy-subrc EQ 0.
*                        <out_zsdt0001>-lzonea = wa_lfa1-lzone.
*                      ENDIF.
*                    ENDIF.
*
*                    SELECT SINGLE lifnr name1 dlgrp lzone regio stcd1
*                      FROM lfa1 INTO wa_lfa1
*                     WHERE lifnr = wa_ekko-lifnr.
*
*                    IF sy-subrc EQ 0.
*                      <out_zsdt0001>-lzonez = wa_lfa1-lzone.
*
*                      SELECT SINGLE *
*                        FROM kna1 INTO @DATA(lwa_kna1_entrega)
*                       WHERE stcd1 EQ @wa_lfa1-stcd1
*                         AND loevm EQ @abap_false.
*
*                      IF sy-subrc EQ 0.
*                        wa_saida-local_entrega =  lwa_kna1_entrega-kunnr.
*                      ENDIF.
*
*                    ENDIF.
*
*                    IF ( <out_zsdt0001>-lzonea IS NOT INITIAL ) AND ( <out_zsdt0001>-lzonez IS NOT INITIAL ).
*                      SELECT SINGLE *
*                        FROM trolz INTO @DATA(wa_trolz_zarm)
*                       WHERE aland = 'BR'
*                         AND azone = @<out_zsdt0001>-lzonea
*                         AND lland = 'BR'
*                         AND lzone = @<out_zsdt0001>-lzonez.
*                      IF sy-subrc = 0.
*                        <out_zsdt0001>-route = wa_trolz_zarm-route.
*                      ENDIF.
*                    ENDIF.
*
*                  ENDIF.
*
*              ENDCASE.
*
*            ENDIF.
            "<--- US #66690 - WPP

            TRY.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  EXPORTING
                    i_tipo_mov       = CONV #( wa_ekko-tp_movimento )
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = CONV #( wa_ekko-bsart )
                  IMPORTING
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              CATCH zcx_faturamento.
              CATCH zcx_error.
            ENDTRY.
            "Comentario Final Codigo - 0002 - 18.04.2019
          ENDIF.

        WHEN '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
        WHEN '03'. "Commodities (Trânsferência Recebidas )
        WHEN '04' OR  "Fertilizantes (Porto Velho) - ZLES0115
             '10'.    "Romaneio de Entrada Completo Transferência

          READ TABLE it_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " pedido
          IF sy-subrc = 0.
            wa_saida-ponto_coleta = wa_ekpa_pr-lifn2.
          ENDIF.

          IF wa_saida-ponto_coleta IS NOT INITIAL.
            SELECT SINGLE lifnr name1 dlgrp lzone
              FROM lfa1 INTO wa_lfa1
             WHERE lifnr = wa_saida-ponto_coleta.
            <out_zsdt0001>-lzonea = wa_lfa1-lzone.
          ENDIF.

          READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
          IF sy-subrc = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_ekpo-werks
              IMPORTING
                output = wa_saida-local_entrega.
          ENDIF.

          IF wa_saida-local_entrega IS NOT INITIAL.
            SELECT SINGLE  kunnr name1 lzone
              FROM kna1
              INTO wa_kna1
              WHERE kunnr = wa_saida-local_entrega.
            <out_zsdt0001>-lzonez = wa_kna1-lzone.
          ENDIF.

          CLEAR: wa_ekko.
          READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de importação de fertilizante / transferencia

          "Itinerário
          IF wa_ekko-bsart NE 'ZUB'. "'T'

            SELECT SINGLE *
              FROM trolz INTO @DATA(wa_trolz)
             WHERE aland = 'BR'
               AND azone = @wa_lfa1-lzone
               AND lland = 'BR'
               AND lzone = @wa_kna1-lzone.
            IF sy-subrc = 0.
              <out_zsdt0001>-route = wa_trolz-route.
            ENDIF.

            TRY.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  EXPORTING
                    i_tipo_mov       = CONV #( wa_ekko-tp_movimento )
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = CONV #( wa_ekko-bsart )
                  IMPORTING
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              CATCH zcx_faturamento.
              CATCH zcx_error.
            ENDTRY.
            "Comentario Final Codigo - 0002 - 18.04.2019

            IF ( <out_zsdt0001>-route IS INITIAL ) AND ( <out_zsdt0001>-doc_aviso IS NOT INITIAL ).
              READ TABLE it_likp INTO DATA(wa_likp) WITH KEY vbeln = <out_zsdt0001>-doc_aviso.
              IF ( sy-subrc = 0 ) AND ( wa_likp-route IS NOT INITIAL ).
                <out_zsdt0001>-route = wa_likp-route.
              ENDIF.
            ENDIF.

          ELSE.                 "Pedido de Transferência

            READ TABLE it_ekpv INTO wa_ekpv WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedido
            IF sy-subrc = 0.
              <out_zsdt0001>-route = wa_ekpv-route.
            ENDIF.

            "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
            TRY.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  EXPORTING
                    i_tipo_mov       = 'S'
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = CONV #( wa_ekko-bsart )
                  IMPORTING
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              CATCH zcx_faturamento.
              CATCH zcx_error.
            ENDTRY.


*            IF <out_zsdt0001>-ebeln IS NOT INITIAL.
*              SELECT SINGLE bsart
*                FROM ekko INTO @DATA(v_bsart)
*               WHERE ebeln = @<out_zsdt0001>-ebeln.
*
*              TRY.
*                  zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
*                    EXPORTING
*                      i_tipo_mov       = 'S'
*                      i_vsart          = '01'  "Rodoviario
*                      i_tipo_pedido    = CONV #( v_bsart )
*                    IMPORTING
*                       e_shtyp         = _shtyp ).
*
*                  <out_zsdt0001>-shtyp = _shtyp.
*                CATCH zcx_faturamento.
*                CATCH zcx_error.
*              ENDTRY.
*              "Comentario Final Codigo - 0003 - 18.04.2019
*            ENDIF.

            "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP



          ENDIF.

      ENDCASE.

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc = 0.
        IF wa_ekpo-inco1 = 'CIF'.
          "Agregado
          SELECT SINGLE agregado
            FROM zlest0002 INTO v_agregado
           WHERE pc_veiculo = <out_zsdt0001>-placa_cav.
          IF sy-subrc = 0.
            IF v_agregado = 1.
              <out_zsdt0001>-add01 = '0000000001'.
            ELSE.
              <out_zsdt0001>-add01 = '0000000002'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "Agente de Frete
    IF <out_zsdt0001>-agente_frete IS INITIAL.
      READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = <out_zsdt0001>-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          IF ( 'ZRFL_ZRDC_ZIND' CS wa_vbak-auart ) AND
             ( wa_vbkd-inco1 = 'CIF'        ) AND
             ( wa_vbak-auart IS NOT INITIAL ).

            READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'SP' BINARY SEARCH.
            <out_zsdt0001>-agente_frete           = wa_vbpa-lifnr.
            PERFORM f_troca_agente USING <out_zsdt0001>-placa_cav <out_zsdt0001>-branch CHANGING <out_zsdt0001>-agente_frete.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE MAX( id_cc )
      FROM zcarta_correcao INTO @DATA(vl_id)
     WHERE docnum        EQ @<out_zsdt0001>-nro_nf_prod
       AND authcode      NE ''
       AND novo_agente   NE ''.

    IF vl_id GT 0.
      SELECT SINGLE *
        FROM zcarta_correcao INTO @DATA(wa_carta)
       WHERE docnum        EQ @<out_zsdt0001>-nro_nf_prod
         AND authcode      NE ''
         AND novo_agente   NE ''
         AND id_cc         EQ @vl_id.
      IF sy-subrc = 0.
        IF wa_carta-novo_agente NE <out_zsdt0001>-agente_frete.
          <out_zsdt0001>-agente_frete = wa_carta-novo_agente.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.


  REFRESH it_konp.
  "Tp.transp./ForncServ./ItinTransp/Contrato/Agregado
  SELECT *
    FROM a900
    INTO TABLE it_a900
     FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
    AND  kschl EQ 'ZFRE'
    AND  shtyp EQ it_zsdt0001_fre-shtyp
    AND  tdlnr EQ it_zsdt0001_fre-agente_frete
    AND  route EQ it_zsdt0001_fre-route
    AND  add01 EQ it_zsdt0001_fre-add01
    AND  kfrst EQ ''
    AND  datab LE sy-datum
    AND  datbi GE sy-datum
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a900~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a900[] IS NOT INITIAL.
    SELECT *
      FROM konp
      INTO TABLE it_konp
      FOR ALL ENTRIES IN it_a900
      WHERE knumh = it_a900-knumh
      AND   loevm_ko EQ ''.
  ENDIF.

  " Tp.transp./ForncServ./Zona part./Zona cheg.
  SELECT *
   FROM a910 INTO TABLE it_a910
    FOR ALL ENTRIES IN it_zsdt0001_fre
  WHERE kappl EQ 'F'
    AND kschl EQ 'ZFRE'
    AND shtyp EQ it_zsdt0001_fre-shtyp
    AND tdlnr EQ it_zsdt0001_fre-agente_frete
    AND lzonea EQ it_zsdt0001_fre-lzonea
    AND lzonez EQ it_zsdt0001_fre-lzonez
    AND kfrst EQ ''
    AND  datab LE sy-datum
    AND  datbi GE sy-datum
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a910~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a910[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a910
     WHERE knumh = it_a910-knumh
       AND loevm_ko EQ ''.
  ENDIF.

  "Tp.transp./ForncServ./ItinTransp/Contrato
  SELECT *
    FROM a911 INTO TABLE it_a911
     FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
     AND kschl EQ 'ZFRE'
     AND shtyp EQ it_zsdt0001_fre-shtyp
     AND tdlnr EQ it_zsdt0001_fre-agente_frete
     AND route EQ it_zsdt0001_fre-route
     AND kfrst EQ ''
     AND  datab LE sy-datum
     AND  datbi GE sy-datum
     AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a911~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a911[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a911
     WHERE knumh = it_a911-knumh
       AND loevm_ko EQ ''.
  ENDIF.

  " Tp.transp./ForncServ./Zona part./Zona cheg./Agregado
  SELECT *
   FROM a915 INTO TABLE it_a915
    FOR ALL ENTRIES IN it_zsdt0001_fre
  WHERE kappl EQ 'F'
    AND kschl EQ 'ZFRE'
    AND shtyp EQ it_zsdt0001_fre-shtyp
    AND tdlnr EQ it_zsdt0001_fre-agente_frete
    AND lzonea EQ it_zsdt0001_fre-lzonea
    AND lzonez EQ it_zsdt0001_fre-lzonez
    AND add01  EQ it_zsdt0001_fre-add01
    AND kfrst EQ ''
    AND  datab LE sy-datum
    AND  datbi GE sy-datum
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a915~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a915[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a915
     WHERE knumh = it_a915-knumh
       AND loevm_ko EQ ''.
  ENDIF.

  " Tp.transp./ForncServ./Material/Zona part./Zona cheg./Suplem.
  SELECT *
   FROM a918 INTO TABLE it_a918
    FOR ALL ENTRIES IN it_zsdt0001_fre
  WHERE kappl EQ 'F'
    AND kschl EQ 'ZFRE'
    AND shtyp EQ it_zsdt0001_fre-shtyp
    AND tdlnr EQ it_zsdt0001_fre-agente_frete
    AND matnr EQ it_zsdt0001_fre-matnr
    AND lzonea EQ it_zsdt0001_fre-lzonea
    AND lzonez EQ it_zsdt0001_fre-lzonez
    AND add01  EQ it_zsdt0001_fre-add01
    AND kfrst EQ ''
    AND  datab LE sy-datum
    AND  datbi GE sy-datum
    AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a918~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a918[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a918
     WHERE knumh = it_a918-knumh
       AND loevm_ko EQ ''.
  ENDIF.

  "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
  SELECT *
    FROM a919 INTO TABLE it_a919
     FOR ALL ENTRIES IN it_zsdt0001_fre
   WHERE kappl EQ 'F'
     AND kschl EQ 'ZFRE'
     AND shtyp EQ it_zsdt0001_fre-shtyp
     AND tdlnr EQ it_zsdt0001_fre-agente_frete
     AND matnr EQ it_zsdt0001_fre-matnr
     AND lzonea EQ it_zsdt0001_fre-lzonea
     AND lzonez EQ it_zsdt0001_fre-lzonez
     AND kfrst EQ ''
     AND  datab LE sy-datum
     AND  datbi GE sy-datum
     AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a919~knumh
                  AND   loevm_ko EQ '' ).

  IF it_a919[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a919
     WHERE knumh = it_a919-knumh
       AND loevm_ko EQ ''.
  ENDIF.

*-CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
  FREE: it_zsdt0001_aux.
  LOOP AT  it_zsdt0001_fre INTO  wa_zsdt0001_fre.
    CLEAR wa_zsdt0001_aux.
    MOVE-CORRESPONDING wa_zsdt0001_fre TO wa_zsdt0001_aux.
    MOVE wa_zsdt0001_fre-add01 TO l_add01.
    MOVE l_add01               TO wa_zsdt0001_aux-sdabw.

    SELECT SINGLE viagem_id
      INTO wa_zsdt0001_aux-viagem_id
      FROM zlest0185
     WHERE id_ordem EQ wa_zsdt0001_fre-id_ordem.

    APPEND wa_zsdt0001_aux   TO it_zsdt0001_aux.
  ENDLOOP.

  "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
  SELECT *
    FROM a942 INTO TABLE it_a942
     FOR ALL ENTRIES IN it_zsdt0001_aux
   WHERE kappl EQ 'F'
     AND kschl EQ 'ZFRE'
     AND shtyp EQ it_zsdt0001_aux-shtyp
     AND sdabw EQ it_zsdt0001_aux-sdabw
     AND id_viagem EQ it_zsdt0001_aux-viagem_id
     AND kfrst EQ ''
     AND  datab LE sy-datum
     AND  datbi GE sy-datum
     AND  EXISTS ( SELECT *
                  FROM  konp
                  WHERE knumh = a942~knumh
                  AND   loevm_ko EQ '' ).
  IF it_a942[] IS NOT INITIAL.
    SELECT *
      FROM konp APPENDING TABLE it_konp
       FOR ALL ENTRIES IN it_a942
     WHERE knumh = it_a942-knumh
       AND loevm_ko EQ ''.
  ENDIF.
*-CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

  SORT: it_a900 BY shtyp tdlnr route add01,
        it_a910 BY shtyp tdlnr lzonea lzonez,
        it_a911 BY shtyp tdlnr route,
        it_a915 BY shtyp tdlnr lzonea lzonez add01,
        it_a918 BY shtyp tdlnr matnr lzonea lzonez add01,
        it_a919 BY shtyp tdlnr matnr lzonea lzonez,
        it_a942 BY shtyp sdabw id_viagem,
        it_konp BY knumh.
ENDFORM.                    "Pega_FRETE

FORM f_saida.

  "Montagem Form de Saida
  IF ( vg_cockpit EQ '01' ) OR
     ( vg_cockpit EQ '03' ) OR "ALRS
     ( vg_cockpit EQ '05' ) OR
     ( vg_cockpit EQ '06' ) OR
     ( vg_cockpit EQ '07' ) OR
     ( vg_cockpit EQ '09' ) OR
     ( vg_cockpit EQ '10' ).
    "Nome Perform para processamento dos dados do Romaneio
    CONCATENATE 'F_SAIDA_' '01' INTO DATA(form_f_saida).
  ELSE.
    "Nome Perform para processamento dos dados do Romaneio
    CONCATENATE 'F_SAIDA_' vg_cockpit INTO form_f_saida.   "Montagem Form de Saida
  ENDIF.

  "Execução Perform
  PERFORM (form_f_saida) IN PROGRAM zlesr0102 IF FOUND.

ENDFORM.

FORM f_saida_01.

  DATA: tabix                TYPE sy-tabix,
        l_erro               TYPE char1,
        v_cont_fre           TYPE i,
        v_cd_uf              TYPE zlest0002-cd_uf,
        vpeso_retido_i       TYPE i,
        v_chv_fat_vt         TYPE zch_ref,
        vmatnr18             TYPE matnr18,
        wa_zsdt0001e         TYPE ty_zsdt0001,
        wa_zlest0019         TYPE zlest0019,
        wa_zlest0041         TYPE zlest0041,
        wa_zlest0087         TYPE zlest0087,
        tl_zmmt_ee_zgr       TYPE TABLE OF zmmt_ee_zgr WITH HEADER LINE,
        tl_zmmt_ee_zgr_docs  TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE,
        tl_zmmt_ee_zgr2      TYPE TABLE OF zmmt_ee_zgr WITH HEADER LINE,
        tl_zmmt_ee_zgr_docs2 TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE.

  SORT: it_vbak     BY vbeln,
        it_tvakt    BY auart,
        it_t161t    BY bsart,
        it_kna1     BY kunnr,
        it_lfa1     BY lifnr,
        it_t001w    BY werks,
        it_vbkd     BY vbeln,
        it_makt     BY matnr,
        it_ekko     BY ebeln,
        it_ekpo     BY ebeln,
        it_vbpa     BY vbeln parvw,
        it_vbpa_cr  BY vbeln,  "Ponto de coleta  REMESSA
        it_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  BY ebeln,  "Ponto de coleta  Pedido
        it_vbap     BY vbeln,  "Itinerário  ORDEM
        it_ekpv     BY ebeln.  "Itinerário  PEDIDO



  SORT: it_zsdt0011_o BY tp_movimento auart,
        it_zsdt0011_p BY tp_movimento bsart.

  "Atualiza variaveis de frete
  it_zsdt0001_fre[] = it_zsdt0001[].
  PERFORM f_pega_frete.
  it_zsdt0001[] = it_zsdt0001_fre[].
  REFRESH it_zsdt0001_fre.

  SELECT *
    FROM zsdt0121
    INTO TABLE t_fatura_agrupada
   WHERE werks = p_branch.

  SORT t_fatura_agrupada BY werks matnr kunnr inco1 cfop.

  SORT it_zsdt0001 BY ch_referencia.
  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    REFRESH: style.
    CLEAR: wa_saida.

    wa_saida-bukrs           = wa_zsdt0001-bukrs.
    wa_saida-branch          = wa_zsdt0001-branch.
    wa_saida-ch_referencia   = wa_zsdt0001-ch_referencia.
    wa_saida-tp_movimento    = wa_zsdt0001-tp_movimento.
    wa_saida-dt_movimento    = wa_zsdt0001-dt_movimento.
    wa_saida-nr_romaneio     = wa_zsdt0001-nr_romaneio.
    wa_saida-nro_cg          = wa_zsdt0001-nro_cg.
    wa_saida-placa_cav       = wa_zsdt0001-placa_cav.
    wa_saida-qtde_remessa    = wa_zsdt0001-qtde_remessa.
    wa_saida-um_remessa      = wa_zsdt0001-um_remessa.

    PERFORM f_set_encerramento_docs CHANGING wa_saida.

*----CS2021000508 - 07.06.2021 - JT - inicio
    PERFORM f_regras_doc_carguero.
*----CS2021000508 - 07.06.2021 - JT - fim

    IF wa_zsdt0001-region IS NOT INITIAL.
      wa_saida-region          = wa_zsdt0001-region.
    ELSE.
      SELECT SINGLE cd_uf
        FROM zlest0002
        INTO v_cd_uf
        WHERE pc_veiculo = wa_zsdt0001-placa_cav.
      IF sy-subrc = 0.
        wa_saida-region          = v_cd_uf.
      ENDIF.
    ENDIF.

    wa_saida-route           = wa_zsdt0001-route.
    wa_saida-st_proc         = wa_zsdt0001-st_proc.
    wa_saida-shtyp           = wa_zsdt0001-shtyp.

    CLEAR wa_saida-icon.
    REFRESH ti_zlest0100.
    SELECT *
      FROM zlest0100
      INTO TABLE ti_zlest0100
      WHERE ch_referencia = wa_saida-ch_referencia.

    IF ti_zlest0100[] IS NOT INITIAL.
      wa_saida-icon = icon_led_red.
    ELSE.
      CLEAR wa_saida-icon.
    ENDIF.

    CLEAR wa_vbak.
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
    IF sy-subrc = 0.
      wa_saida-tipo = 'O'.
      "Ponto de coleta ordem
      READ TABLE it_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_co-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        ENDIF.
      ENDIF.

      "Zona Local de entrega ordem
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-local_entrega = wa_vbpa-kunnr.
      ENDIF.

      READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
      CONCATENATE wa_vbak-auart '-' wa_tvakt-bezei INTO wa_saida-operacao.
      CLEAR wa_kna1.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      wa_saida-kunnr           = wa_kna1-kunnr.
      wa_saida-name1           = wa_kna1-name1.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH.
      IF sy-subrc = 0 .
        wa_saida-inco1           = wa_vbkd-inco1.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

    ELSE.
      wa_saida-tipo = 'P'.
      "Ponto de coleta pedido
      READ TABLE it_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_ekpa_pr-lifn2.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpa_pr-lifn2 BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        ENDIF.
      ENDIF.

      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      wa_saida-bsart = wa_ekko-bsart.

      READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
      CONCATENATE wa_ekko-bsart '-' wa_t161t-batxt  INTO wa_saida-operacao.

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc = 0.
        wa_saida-inco1           = wa_ekpo-inco1.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

      CLEAR wa_lfa1.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpo-lifnr BINARY SEARCH.
      wa_saida-kunnr           = wa_lfa1-lifnr.
      wa_saida-name1           = wa_lfa1-name1.

      "Zona Local de entrega Pedido
      READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_zsdt0001-doc_rem
                                               parvw = 'LR'.  "Local de entrega

      IF ( sy-subrc EQ 0 ) AND ( wa_zsdt0001-doc_rem IS NOT INITIAL ).
        wa_saida-local_entrega = wa_vbpa-kunnr.
      ENDIF.

    ENDIF.

    PERFORM f_atual_frete USING wa_zsdt0001 'L' CHANGING wa_saida.

    " substituivalor do frete pelo valor historico
    IF wa_zsdt0001-kbetr GT 0.
      wa_saida-kbetr = wa_zsdt0001-kbetr.
      wa_saida-konwa = wa_zsdt0001-konwa.
      v_cont_fre = 1.
    ENDIF.

    wa_saida-vbeln           = wa_zsdt0001-vbeln.
    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.

    IF wa_zsdt0001-agente_frete IS NOT INITIAL.
      wa_saida-lifnr = wa_zsdt0001-agente_frete.
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                 parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-kunnr           = wa_kna1-kunnr.
          wa_saida-name1           = wa_kna1-name1.
        ENDIF.
      ENDIF.
    ELSEIF wa_saida-tipo = 'O'.
      IF ( 'ZRFL_ZRDC_ZIND' CS wa_vbak-auart ) AND
         ( wa_vbak-auart IS NOT INITIAL ) AND
         ( wa_vbkd-inco1 = 'CIF'        ).
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'SP' BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-lifnr = wa_vbpa-lifnr.
          PERFORM f_troca_agente USING wa_zsdt0001-placa_cav wa_zsdt0001-branch CHANGING wa_saida-lifnr.
        ENDIF.
      ELSE.
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'LR' BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-kunnr = wa_kna1-kunnr.
            wa_saida-name1 = wa_kna1-name1.
          ENDIF.
        ENDIF.

        IF ( t_fatura_agrupada IS NOT INITIAL ) AND
           ( 'CFR_FOB' CS wa_saida-inco1      ) AND
           ( wa_saida-inco1 IS NOT INITIAL    ).

          IF wa_saida-lifnr IS NOT INITIAL.
            PERFORM f_config_cell USING wa_saida-lifnr  'LIFNR'  cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.

          IF ( wa_saida-region IS NOT INITIAL ) OR (  wa_zsdt0001-placa_cav IS INITIAL  ).
            PERFORM f_config_cell USING wa_saida-region 'REGION' cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    "BLOQUEIA CELULA
    CLEAR: wa_style.
    IF ( wa_saida-region IS NOT INITIAL ) OR (  wa_zsdt0001-placa_cav IS INITIAL  ).
      wa_style-fieldname = 'REGION'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style .
    ENDIF.
    CLEAR: wa_style.
    IF wa_saida-lifnr IS NOT INITIAL.
      wa_style-fieldname = 'LIFNR'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT wa_style INTO TABLE style .
    ENDIF.

    "Busca peso descarga e CFOP (SE agrupamento)
    IF vg_cockpit = '03' OR vg_cockpit = '09' OR vg_cockpit = '10'.
      IF wa_zsdt0001-peso_liq_pos_ret EQ 0.
        SELECT SINGLE *
          FROM zsdt0001
          INTO wa_zsdt0001e
          WHERE tp_movimento =  'E'
          AND   bukrs        =  wa_zsdt0001-bukrs
          AND   branch       =  wa_zsdt0001-branch
          AND   nr_romaneio  =  wa_zsdt0001-id_referencia
          AND   nr_safra     =  wa_zsdt0001-nr_safra.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zlest0087
            INTO wa_zlest0087
            WHERE idinter	=	'L1'
            AND   tp_movi	=	'E'
            AND   tp_reg  = '30'
            AND   bukrs	=	wa_zsdt0001e-bukrs
            AND   werks	=	wa_zsdt0001e-branch
            AND   nr_nf	=	wa_zsdt0001e-nfnum
            AND   serie	=	wa_zsdt0001e-series
            AND   lifnr	=	wa_zsdt0001e-parid
            AND   status  = ''.

          IF sy-subrc = 0.
            wa_saida-peso_descarga  =  wa_zlest0087-descarga_rodo.
            PERFORM f_config_cell USING wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            SELECT SINGLE *
              FROM zlest0041
              INTO wa_zlest0041
              WHERE centro_comprador  = wa_zsdt0001e-branch
              AND   nr_nf             = wa_zsdt0001e-nfnum
              AND   cod_cliente       = wa_zsdt0001e-parid
              AND   serie             = wa_zsdt0001e-series.

            IF sy-subrc = 0.
              SELECT SINGLE *
                FROM zlest0019
                INTO wa_zlest0019
                WHERE idinter  = 'L1'
                AND   tp_movi  = 'E'
                AND   tp_reg   = '30'
                AND   bukrs    = wa_zsdt0001e-bukrs
                AND   branch   = wa_zlest0041-centro_comprador
                AND   nfenum   = wa_zlest0041-nr_nf_propria.
              IF sy-subrc = 0.
                wa_saida-peso_descarga  =  wa_zlest0019-pesodvagao.
                PERFORM f_config_cell USING wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
              ENDIF.

            ENDIF.
          ENDIF.
          "CFOP
          IF wa_zsdt0001e-series+0(1) = '0'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_zsdt0001e-series
              IMPORTING
                output = wa_zsdt0001e-series.
          ENDIF.

          SELECT SINGLE cfop
            INTO wa_saida-cfop
            FROM j_1bnfdoc
            INNER JOIN j_1bnflin
            ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*            AND J_1BNFLIN~ITMNUM = '00010'
            WHERE j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
            AND   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
            AND   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
            AND   j_1bnfdoc~nfenum       =  wa_zsdt0001e-nfnum
            AND   j_1bnfdoc~series       =  wa_zsdt0001e-series.

          IF sy-subrc NE 0.
            wa_zsdt0001e-nfnum2 = wa_zsdt0001e-nfnum.
            SELECT SINGLE cfop
             INTO wa_saida-cfop
             FROM j_1bnfdoc
             INNER JOIN j_1bnflin
             ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*             AND J_1BNFLIN~ITMNUM = '00010'
             WHERE j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
             AND   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
             AND   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
             AND   j_1bnfdoc~nfnum        =  wa_zsdt0001e-nfnum2
             AND   j_1bnfdoc~series       =  wa_zsdt0001e-series.

            IF sy-subrc NE 0.

              SELECT  *
                FROM  zmmt_ee_zgr
                INTO TABLE tl_zmmt_ee_zgr
                WHERE  ch_referencia =  wa_zsdt0001e-ch_referencia.

              IF sy-subrc = 0.

                SELECT *
                   FROM  zmmt_ee_zgr_docs
                   INTO TABLE tl_zmmt_ee_zgr_docs
                   FOR ALL ENTRIES IN tl_zmmt_ee_zgr
                   WHERE  obj_key = tl_zmmt_ee_zgr-obj_key
                  AND docnum NE ' '.

                IF sy-subrc = 0.
                  READ TABLE tl_zmmt_ee_zgr_docs INDEX 1.
                  SELECT SINGLE cfop
                     INTO wa_saida-cfop
                     FROM j_1bnfdoc
                     INNER JOIN j_1bnflin
                    ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*                    AND J_1BNFLIN~ITMNUM = '00010'
                    WHERE j_1bnfdoc~docnum = tl_zmmt_ee_zgr_docs-docnum.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.

          wa_saida-cfop = wa_saida-cfop+0(4).

          READ TABLE t_fatura_agrupada INTO w_fatura_agrupada WITH KEY werks = wa_zsdt0001-branch "RJF
                                                                       matnr = wa_zsdt0001-matnr
                                                                       kunnr = wa_saida-kunnr
                                                                       inco1 = vinco1
                                                                       cfop  = wa_saida-cfop.
          IF sy-subrc = 0.
            wa_saida-perc_ret    = w_fatura_agrupada-perc_ret.

            wa_saida-peso_retido = wa_saida-peso_descarga * ( w_fatura_agrupada-perc_ret / 100 ).
            vpeso_retido_i = wa_saida-peso_retido.
            wa_saida-peso_retido = wa_saida-peso_retido - ( wa_saida-peso_retido - vpeso_retido_i ).


            wa_saida-peso_liq_pos_ret  = wa_saida-peso_descarga - wa_saida-peso_retido.
*            WA_SAIDA-STYLE[] = STYLE[].
          ENDIF.
          "
        ENDIF.
      ELSE.
        wa_saida-peso_descarga    = wa_zsdt0001-peso_descarga.
        wa_saida-perc_ret         = wa_zsdt0001-perc_ret.
        wa_saida-peso_retido      = wa_zsdt0001-peso_retido.
        wa_saida-peso_liq_pos_ret = wa_zsdt0001-peso_liq_pos_ret.
        PERFORM f_config_cell USING wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
*        WA_SAIDA-STYLE[] = STYLE[].
        SELECT SINGLE *
          FROM zsdt0001
          INTO wa_zsdt0001e
          WHERE tp_movimento =  'E'
          AND   bukrs        =  wa_zsdt0001-bukrs
          AND   branch       =  wa_zsdt0001-branch
          AND   nr_romaneio  =  wa_zsdt0001-id_referencia
          AND   nr_safra     =  wa_zsdt0001-nr_safra.

        IF sy-subrc = 0.
          "CFOP
          IF wa_zsdt0001e-series+0(1) = '0'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_zsdt0001e-series
              IMPORTING
                output = wa_zsdt0001e-series.
          ENDIF.

          SELECT SINGLE cfop
            INTO wa_saida-cfop
            FROM j_1bnfdoc
            INNER JOIN j_1bnflin
            ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*            AND J_1BNFLIN~ITMNUM = '00010'
            WHERE j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
            AND   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
            AND   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
            AND   j_1bnfdoc~nfenum       =  wa_zsdt0001e-nfnum
            AND   j_1bnfdoc~series       =  wa_zsdt0001e-series.

          IF sy-subrc NE 0.
            wa_zsdt0001e-nfnum2 = wa_zsdt0001e-nfnum.
            SELECT SINGLE cfop
             INTO wa_saida-cfop
             FROM j_1bnfdoc
             INNER JOIN j_1bnflin
             ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*             AND J_1BNFLIN~ITMNUM = '00010'
             WHERE j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
             AND   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
             AND   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
             AND   j_1bnfdoc~nfnum        =  wa_zsdt0001e-nfnum2
             AND   j_1bnfdoc~series       =  wa_zsdt0001e-series.

            IF sy-subrc NE 0.

              SELECT  *
                FROM     zmmt_ee_zgr
                INTO TABLE tl_zmmt_ee_zgr2
                WHERE  ch_referencia =  wa_zsdt0001e-ch_referencia.

              IF sy-subrc = 0.

                DATA: v_candat TYPE j_1bnfdoc-candat.

                SELECT  *
                   FROM  zmmt_ee_zgr_docs
                   INTO TABLE tl_zmmt_ee_zgr_docs2
                   FOR ALL ENTRIES IN tl_zmmt_ee_zgr2
                   WHERE  obj_key = tl_zmmt_ee_zgr2-obj_key
                   AND docnum NE ' '.

                IF sy-subrc = 0.
                  READ TABLE tl_zmmt_ee_zgr_docs2 INDEX 1.

                  SELECT SINGLE cfop
                     INTO wa_saida-cfop
                     FROM j_1bnfdoc
                     INNER JOIN j_1bnflin
                    ON  j_1bnflin~docnum = j_1bnfdoc~docnum
*                    AND J_1BNFLIN~ITMNUM = '00010'
                    WHERE j_1bnfdoc~docnum = tl_zmmt_ee_zgr_docs2-docnum.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.

          wa_saida-cfop = wa_saida-cfop+0(4).

        ENDIF.

      ENDIF.
    ENDIF.


    wa_saida-style[] = style[].

    IF wa_saida-name1 IS INITIAL .
      CLEAR wa_kna1.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0001-branch  BINARY SEARCH.
      wa_saida-name1           = wa_t001w-name1.
    ENDIF.

    "//Enio Jesus - 17.01.2017
    IF ( wa_zsdt0001-doc_rem IS INITIAL OR  wa_zsdt0001-doc_rem = '' ).

      IF line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 remessa      = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-remessa = icon_execute_object.
        ENDTRY.

      ELSE.
        wa_saida-remessa = icon_execute_object.
      ENDIF.


      "Limite de crédito
      SELECT SINGLE *
        FROM zsdt0151
        INTO wa_zsdt0151
        WHERE ch_referencia = wa_zsdt0001-ch_referencia.
      IF sy-subrc = 0.
        IF wa_zsdt0151-vbeln NE wa_zsdt0001-vbeln.
          DELETE FROM zsdt0151 WHERE ch_referencia = wa_zsdt0001-ch_referencia.
        ELSE.
          IF wa_zsdt0151-status IS INITIAL.
            CONCATENATE icon_workflow_inbox(3) '\QAguardando Aprovação de limite de crédito@' INTO wa_saida-remessa .
          ELSEIF  wa_zsdt0151-status = 'R'.
            CONCATENATE icon_reject(3) '\QLimite de crédito rejeitado@' INTO wa_saida-remessa .
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      wa_saida-remessa = wa_zsdt0001-doc_rem.
      "Ponto de coleta (substitui se ja tiver remessa)
      READ TABLE it_vbpa_cr INTO wa_vbpa_cr WITH KEY vbeln = wa_zsdt0001-doc_rem BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_cr-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_cr-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        ENDIF.
      ENDIF.
    ENDIF.

    "CS2019000400 25.02.2019
    IF wa_saida-lifnr IS INITIAL. "Sugere Agente Frete
      "PERFORM F_TROCA_AGENTE USING WA_ZSDT0001-PLACA_CAV WA_ZSDT0001-PLACA_CAV-BRANCH CHANGING  WA_SAIDA-LIFNR.
      TRY .
          zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
          EXPORTING
            i_bukrs                =    CONV #( wa_zsdt0001-bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
            i_placa                =    CONV #( wa_zsdt0001-placa_cav )
            i_uf_origem_mercadoria =    CONV #( wa_saida-regio_c  )
           IMPORTING
             e_agente_frete         =   wa_saida-lifnr ).

        CATCH zcx_faturamento INTO DATA(_cx_fat).
        CATCH zcx_error INTO DATA(_cx_error).
      ENDTRY.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_prod IS INITIAL OR wa_zsdt0001-fatura_prod = ''.

      IF line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 fatura       = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-fatura = icon_execute_object.
        ENDTRY.

      ELSE.
        wa_saida-fatura = icon_execute_object.
      ENDIF.

    ELSE.
      wa_saida-fatura  = wa_zsdt0001-fatura_prod.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatura
        IMPORTING
          output = wa_saida-fatura.

      "Verfifica se DANFE já está autorizada e muda o campo
      IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.

        PERFORM f_check_aut_doc USING '1'
                                      wa_saida
                                      wa_zsdt0001
                             CHANGING vl_docnum.

        IF vl_docnum IS NOT INITIAL.
          wa_zsdt0001-nro_nf_prod = vl_docnum.
          wa_saida-danfe          = vl_docnum.
          UPDATE zsdt0001 SET st_proc = '03'
                              nro_nf_prod = vl_docnum
          WHERE ch_referencia = wa_saida-ch_referencia.
          wa_saida-st_proc = '03'.
          IF ( wa_saida-inco1 = 'FOB' OR wa_saida-inco1 = 'CFR' ) AND ( NOT wa_saida-enc_conhecimento = abap_true ) . " Finaliza processo com a DANFE autorizada
*----CS2021000508 - 07.06.2021 - JT - inicio
            IF wa_saida-troca_nota            = abap_true AND
               wa_saida-docs_enviado_carguero = abap_false.
              UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe
                                  st_proc     = '98' " Finalizado
              WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '98'.
            ELSE.
              UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe
                                  st_proc     = '99' " Finalizado
            WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '99'.
            ENDIF.

*           UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe
*                               st_proc     = '99' " Finalizado
*           WHERE ch_referencia = wa_saida-ch_referencia.

            wa_saida-transp  = icon_icon_list.
            wa_saida-dacte   = icon_icon_list.
*           wa_saida-st_proc = '99'.
            CLEAR wa_saida-icon.
*----CS2021000508 - 07.06.2021 - JT - fim

          ENDIF.
          IF ( wa_saida-inco1 = 'CPT' ).  " Gerar CPT logo apos aprovação
            CLEAR  wl_erro.
            PERFORM f_gerar_vt USING ''
                                     'L'
                            CHANGING wl_erro
                                     wa_saida
                                     t_return[].
            PERFORM f_check_retorno_vt USING ''
                                             wl_erro
                                    CHANGING wa_saida.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.

      IF line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        TRY.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 danfe        = icon_execute_object
                               ].
          CATCH cx_sy_itab_line_not_found.
            wa_saida-danfe  = icon_execute_object.
        ENDTRY.
      ELSE.
        wa_saida-danfe = icon_execute_object.
      ENDIF.

    ELSE.
      wa_saida-danfe           = wa_zsdt0001-nro_nf_prod.
    ENDIF.

    IF wa_zsdt0001-doc_transp IS INITIAL OR wa_zsdt0001-doc_transp = ''.

      CLEAR: v_chv_fat_vt.
      zcl_romaneio=>get_ck_faturar(
        EXPORTING
          i_ch_referencia_sai   = wa_zsdt0001-ch_referencia
          i_somente_chv_faturar = abap_true
        IMPORTING
          e_chv_faturar         = v_chv_fat_vt ).

      IF NOT 'CFR_FOB' CS wa_saida-inco1.
        wa_saida-transp = icon_execute_object.

        IF ( v_chv_fat_vt IS NOT INITIAL ) AND ( v_chv_fat_vt NE wa_zsdt0001-ch_referencia ) AND ( wa_saida-inco1 = 'CIF' ).
          wa_saida-transp  = icon_icon_list.
        ENDIF.

      ELSE.
        wa_saida-transp  = icon_icon_list.
      ENDIF.
    ELSE.
      wa_saida-transp          = wa_zsdt0001-doc_transp.
    ENDIF.

    IF wa_zsdt0001-fknum IS INITIAL OR wa_zsdt0001-fknum = ''.
      wa_saida-doccus = icon_icon_list.
    ELSE.
      wa_saida-doccus = wa_zsdt0001-fknum.
    ENDIF.

    IF wa_zsdt0001-ov_frete IS INITIAL OR wa_zsdt0001-ov_frete = ''.
      wa_saida-ovserv          = icon_icon_list.
    ELSE.
      wa_saida-ovserv          = wa_zsdt0001-ov_frete.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_frete IS INITIAL OR wa_zsdt0001-fatura_frete = ''.
      wa_saida-fatserv         = icon_icon_list.
    ELSE.
      wa_saida-fatserv         = wa_zsdt0001-fatura_frete.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatserv
        IMPORTING
          output = wa_saida-fatserv.

      IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = '' OR ( wa_saida-st_proc NE '99' AND wa_zsdt0001-nro_nf_frete IS NOT INITIAL ).
        SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
            FROM j_1bnflin
            INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
            INTO (vl_bukrs,vl_docnum)
            WHERE j_1bnflin~refkey = wa_saida-fatserv.

        IF sy-subrc = 0.

          PERFORM f_check_auth_doc USING vl_docnum.

          IF sy-subrc = 0.
*----CS2021000508 - 07.06.2021 - JT - inicio
            IF wa_saida-troca_nota            = abap_true AND
               wa_saida-docs_enviado_carguero = abap_false.
              UPDATE zsdt0001 SET st_proc      = '98'
                                  nro_nf_frete = vl_docnum
              WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '98'.
            ELSE.
              UPDATE zsdt0001 SET st_proc      = '99'
                                  nro_nf_frete = vl_docnum
              WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '99'.
            ENDIF.

            wa_zsdt0001-nro_nf_frete = vl_docnum.
*           UPDATE zsdt0001 SET st_proc      = '99'
*                               nro_nf_frete = vl_docnum
*           WHERE ch_referencia = wa_saida-ch_referencia.
*           wa_saida-st_proc = '99'.
*----CS2021000508 - 07.06.2021 - JT - inicio
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = ''.
      wa_saida-dacte = icon_execute_object.
    ELSE.
      wa_saida-dacte = wa_zsdt0001-nro_nf_frete.
    ENDIF.

    IF wa_zsdt0001-st_proc = '99' .
      IF wa_saida-transp = icon_execute_object.
        wa_saida-transp = icon_icon_list.
      ENDIF.
      IF wa_saida-dacte = icon_execute_object.
        wa_saida-dacte  = icon_icon_list.
      ENDIF.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0001-matnr BINARY SEARCH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = wa_saida-matnr.


    CONCATENATE wa_saida-matnr '-' wa_makt-maktx INTO  wa_saida-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = vmatnr18.

    wa_saida-matnr = vmatnr18 .

    "Checar se há cancelamento de CTE e estorna documentos
    " No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    IF ( wa_saida-dacte   IS NOT INITIAL ) AND ( wa_saida-dacte(1) NE '@' ) AND
       ( wa_saida-inco1   EQ 'CIF' ) AND
       ( wa_saida-st_proc EQ '99' ).

      PERFORM f_check_canc_doc USING wa_saida-dacte.

      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        wa_saida-dacte = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '07'
                            nro_nf_frete = ''
        WHERE ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '07'.
        PERFORM f_estorno_cte CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.

    "Checar se há cancelamento de NFE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    IF ( wa_saida-danfe IS NOT INITIAL ) AND ( wa_saida-danfe(1) NE '@' ) AND
       ( wa_saida-st_proc = '03' OR ( wa_saida-st_proc = '99' AND  'FOB_CPT_CFR' CS wa_saida-inco1  ) ).

      PERFORM f_check_canc_doc USING wa_saida-danfe.

      IF sy-subrc EQ 0.

        REFRESH ti_zlest0100.
        IF ( wa_saida-inco1 = 'CPT' ) OR ( wa_saida-enc_doc_custo EQ abap_true ).
          wa_saida-st_proc = '05'.
          UPDATE zsdt0001 SET st_proc      = '05'
          WHERE ch_referencia = wa_saida-ch_referencia.
          PERFORM f_estorno_custo CHANGING wa_saida.
        ENDIF.

        wa_saida-danfe = icon_execute_object.
        UPDATE zsdt0001 SET   st_proc      = '02'
                              nro_nf_prod  = ''
          WHERE ch_referencia = wa_saida-ch_referencia.

        PERFORM f_estorno_nfe CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.
    "

    IF wa_saida-operacao(4) = 'ZPAR'.
      wa_saida-danfe    = icon_icon_list.
      wa_saida-transp   = icon_icon_list.
      wa_saida-doccus   = icon_icon_list.
      wa_saida-ovserv   = icon_icon_list.
      wa_saida-fatserv  = icon_icon_list.
      wa_saida-dacte    = icon_icon_list.
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING wa_saida.


*-CS2021000218-16.11.2022-#90706-JT-inicio
    PERFORM f_validar_solicitacao_ra  USING wa_zsdt0001-nro_cg
                                            wa_zsdt0001-ch_referencia
                                   CHANGING wa_saida
                                            l_erro.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
    IF vg_cockpit = '01'.   "Pesagem OPUS saida
      PERFORM f_validar_transf_algodao  USING wa_zsdt0001-ch_referencia
                                     CHANGING wa_saida
                                              l_erro.
    ENDIF.
*-CS2023000189-26.05.2023-#108752-JT-fim

    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.

  IF s_lifnr-low IS NOT INITIAL.
    DELETE it_saida WHERE lifnr NE s_lifnr-low.
  ENDIF.

  SORT it_zsdt0001 BY ch_referencia.

  SORT it_saida BY nr_romaneio.

  IF vg_cockpit = '03'. "Agrupamento - Ordernar por CFOP
    SORT it_saida BY cfop remessa DESCENDING.
  ENDIF.

*----CS2021000508 - 07.06.2021 - JT - inicio
  IF cl_grid IS NOT INITIAL.
    PERFORM f_refresh_alv USING '0100'. "Refresh na tela
  ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

ENDFORM.                    " F_SAIDA_01

*----CS2021000508 - 07.06.2021 - JT - inicio
************************************************************
* HABILITAR ENVIO DE ARQUIVOS P/ CARGUERO
************************************************************
FORM f_regras_doc_carguero.

  DATA: l_naotem_doc   TYPE char01.

  FREE: l_naotem_doc.

  wa_zsdt0001-troca_nota = zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
    EXPORTING i_ch_referencia = wa_zsdt0001-ch_referencia ).

  IF wa_zsdt0001-troca_nota = abap_false.
    wa_saida-docs_carguero  = icon_light_out.
  ELSE.

*--------------------------------------------
*-- valida arquivos obrigatorios
*--------------------------------------------
    TRY.
        l_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                         )->get_documentos_obrigatorios( EXPORTING i_ch_referencia = wa_zsdt0001-ch_referencia
                         ).

      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.

    IF wa_zsdt0001-docs_enviado_carguero = abap_false.
      IF l_naotem_doc = abap_true.
        wa_saida-docs_carguero   = icon_red_light.
      ELSE.
        wa_saida-docs_carguero   = icon_yellow_light.
      ENDIF.
    ELSE.
      wa_saida-docs_carguero     = icon_green_light.
    ENDIF.
  ENDIF.

* wa_saida-docs_carguero         = icon_icon_list.
  wa_saida-docs_enviado_carguero = wa_zsdt0001-docs_enviado_carguero.
  wa_saida-troca_nota            = wa_zsdt0001-troca_nota.

ENDFORM.
*----CS2021000508 - 07.06.2021 - JT - fim


FORM f_saida_04.
  DATA: tabix         TYPE sy-tabix,
        v_cont_fre    TYPE i,
        v_cont_ped    TYPE i,
        v_matnr18     TYPE matnr18,
        v_cd_uf       TYPE zlest0002-cd_uf,
        wl_zfiwrt0009 TYPE zfiwrt0009.

  SORT: it_vbak     BY vbeln,
        it_tvakt    BY auart,
        it_t161t    BY bsart,
        it_kna1     BY kunnr,
        it_lfa1     BY lifnr,
        it_t001w    BY werks,
        it_vbkd     BY vbeln,
        it_makt     BY matnr,
        it_ekko     BY ebeln,
        it_ekpo     BY ebeln,
        it_vbpa     BY vbeln parvw,
        it_vbpa_cr  BY vbeln,  "Ponto de coleta  REMESSA
        it_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  BY ebeln,  "Ponto de coleta  Pedido
        it_vbap     BY vbeln,  "Itinerário  ORDEM
        it_ekpv     BY ebeln.  "Itinerário  PEDIDO


  SORT: it_zsdt0011_o BY tp_movimento auart,
        it_zsdt0011_p BY tp_movimento bsart,
        it_zsdt0062   BY vbeln ebeln ebelp.

  "Atualiza variaveis de frete
  it_zsdt0001_fre[] = it_zsdt0001[].
  PERFORM f_pega_frete.
  it_zsdt0001[] = it_zsdt0001_fre[].
  REFRESH it_zsdt0001_fre.


  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    CLEAR: wa_saida.


    wa_saida-bukrs           = wa_zsdt0001-bukrs.
    wa_saida-branch          = wa_zsdt0001-branch.
    wa_saida-nr_safra        = wa_zsdt0001-nr_safra.
    wa_saida-ch_referencia   = wa_zsdt0001-ch_referencia.
    wa_saida-tp_movimento    = wa_zsdt0001-tp_movimento.
    wa_saida-dt_movimento    = wa_zsdt0001-dt_movimento.
    wa_saida-nr_romaneio     = wa_zsdt0001-nr_romaneio.
    wa_saida-nro_cg          = wa_zsdt0001-nro_cg.
    wa_saida-placa_cav       = wa_zsdt0001-placa_cav.
    wa_saida-placa_car1      = wa_zsdt0001-placa_car1.
    wa_saida-placa_car2      = wa_zsdt0001-placa_car2.
    wa_saida-placa_car3      = wa_zsdt0001-placa_car3.
    wa_saida-motorista       = wa_zsdt0001-motorista.
    wa_saida-parid_rom       = wa_zsdt0001-parid.
    wa_saida-id_cli_dest_rom = wa_zsdt0001-id_cli_dest.

    PERFORM f_set_encerramento_docs CHANGING wa_saida.

    IF   wa_zsdt0001-region IS NOT INITIAL.
      wa_saida-region          = wa_zsdt0001-region.
    ELSE.
      SELECT SINGLE cd_uf
        FROM zlest0002
        INTO v_cd_uf
        WHERE pc_veiculo = wa_zsdt0001-placa_cav.
      IF sy-subrc = 0.
        wa_saida-region          = v_cd_uf.
      ENDIF.
    ENDIF.

    wa_saida-route           = wa_zsdt0001-route.
    wa_saida-st_proc         = wa_zsdt0001-st_proc.
    wa_saida-shtyp           = wa_zsdt0001-shtyp.

    CLEAR wa_saida-icon.
    REFRESH ti_zlest0100.
    SELECT *
      FROM zlest0100
      INTO TABLE ti_zlest0100
      WHERE ch_referencia = wa_saida-ch_referencia.

    IF ti_zlest0100[] IS NOT INITIAL.
      wa_saida-icon = icon_led_red.
    ELSE.
      CLEAR wa_saida-icon.
    ENDIF.

    CLEAR wa_vbak.
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
    IF sy-subrc = 0.
      wa_saida-tipo = 'O'.
      "Ponto de coleta ordem
      READ TABLE it_vbpa_co INTO wa_vbpa_co WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_co-lifnr.
        wa_saida-ponto_coleta = wa_vbpa_co-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_co-lifnr BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.

      "Zona Local de entrega ordem
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-local_entrega = wa_vbpa-kunnr.
      ENDIF.

      READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
      CONCATENATE wa_vbak-auart '-' wa_tvakt-bezei INTO wa_saida-operacao.
      CLEAR wa_kna1.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      wa_saida-name1           = wa_kna1-name1.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0001-vbeln BINARY SEARCH.
      IF sy-subrc = 0 .
        wa_saida-inco1           = wa_vbkd-inco1.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

      wa_saida-vbeln           = wa_zsdt0001-vbeln. "ORDEM VENDA

      IF wa_zsdt0001-ebeln IS INITIAL.
        CLEAR v_cont_ped.

        LOOP AT it_zsdt0062 INTO wa_zsdt0062 WHERE vbeln = wa_zsdt0001-vbeln
                                               AND matnr = wa_zsdt0001-matnr. "CS2017002682 - 29.11.2017
          wa_saida-ebeln           = wa_zsdt0062-ebeln. "PEDIDO Importação
          wa_saida-ebelp           = wa_zsdt0062-ebelp. "CS2017002682 - 29.11.2017 - Ini
          ADD 1 TO v_cont_ped.
        ENDLOOP.
        IF v_cont_ped = 0.
          CLEAR: wa_saida-ebeln, wa_saida-ebelp. " CS2017002682 - 29.11.2017
          "CLEAR WA_SAIDA.
          "CONTINUE.
        ELSEIF v_cont_ped GT 1. " se tiver mais de 1, tem que selecionar
          CLEAR: wa_saida-ebeln, wa_saida-ebelp. " CS2017002682 - 29.11.2017
        ENDIF.
      ELSE.
        wa_saida-ebeln           = wa_zsdt0001-ebeln.
        wa_saida-ebelp           = wa_zsdt0001-ebelp. "CS2017002682 - 29.11.2017
      ENDIF.

    ELSE.
      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF wa_ekko-bsart = 'ZUB'.
        wa_saida-tipo = 'T'.
        wa_saida-vbeln           = wa_zsdt0001-vbeln. "Pedido de transferencia
        IF wa_zsdt0001-ebeln IS INITIAL.
          CLEAR: wa_saida-ebeln, wa_saida-ebelp.
        ELSE.
          wa_saida-ebeln = wa_zsdt0001-ebeln.
          wa_saida-ebelp = wa_zsdt0001-ebelp.
        ENDIF.
      ELSE.
        wa_saida-tipo = 'P'.
        CLEAR wa_saida-vbeln.
        wa_saida-ebeln           = wa_zsdt0001-vbeln.
      ENDIF.

      "Ponto de coleta pedido
      READ TABLE it_ekpa_pr INTO wa_ekpa_pr WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_ekpa_pr-lifn2.
        wa_saida-ponto_coleta = wa_ekpa_pr-lifn2.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpa_pr-lifn2 BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.

      READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
      CONCATENATE wa_ekko-bsart '-' wa_t161t-batxt  INTO wa_saida-operacao.

      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zsdt0001-vbeln BINARY SEARCH. " Pedidos de transferencia
      IF sy-subrc = 0.
        wa_saida-inco1           = wa_ekpo-inco1.

        SELECT SINGLE * INTO @DATA(wa_eket)
          FROM eket
         WHERE ebeln EQ @wa_ekpo-ebeln
           AND ebelp EQ @wa_ekpo-ebelp.

        IF sy-subrc IS INITIAL AND wa_eket-charg IS NOT INITIAL.
          wa_saida-nr_safra        = wa_eket-charg.
        ENDIF.

        "local de entrega pedido
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_ekpo-werks
          IMPORTING
            output = wa_saida-local_entrega.
      ELSE.
        CLEAR wa_saida.
        CONTINUE.
      ENDIF.

      CLEAR wa_lfa1.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekpo-lifnr BINARY SEARCH.
      wa_saida-name1           = wa_lfa1-name1.
    ENDIF.

    PERFORM f_atual_frete USING wa_zsdt0001 'L' CHANGING wa_saida.

    " substituivalor do frete pelo valor historico
    IF wa_zsdt0001-kbetr GT 0.
      wa_saida-kbetr = wa_zsdt0001-kbetr.
      wa_saida-konwa = wa_zsdt0001-konwa.
      v_cont_fre = 1.
    ENDIF.

    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.
    wa_saida-peso_fiscal     = wa_zsdt0001-peso_fiscal.

    IF wa_zsdt0001-agente_frete IS NOT INITIAL.
      wa_saida-lifnr = wa_zsdt0001-agente_frete.
      READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                 parvw = 'LR' BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-name1           = wa_kna1-name1.
        ENDIF.
      ENDIF.
    ELSE.
      IF ( 'ZRFL_ZRDC_ZIND' CS wa_vbak-auart ) AND
         ( wa_vbkd-inco1 = 'CIF' ) AND
         ( wa_vbak-auart IS NOT INITIAL ).
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'SP' BINARY SEARCH.
        wa_saida-lifnr           = wa_vbpa-lifnr.
        PERFORM f_troca_agente USING wa_zsdt0001-placa_cav wa_zsdt0001-branch CHANGING wa_saida-lifnr.
      ELSE.
        READ TABLE it_vbpa INTO wa_vbpa  WITH KEY vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'LR' BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbpa-kunnr BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-name1           = wa_kna1-name1.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF wa_zsdt0001-seq_lcto IS INITIAL OR  wa_zsdt0001-seq_lcto = ''.
      wa_saida-seq_lcto        = icon_execute_object.
      wa_saida-danfez          = icon_execute_object.
    ELSE.
      CLEAR wl_zfiwrt0009.
      SELECT SINGLE * FROM zfiwrt0009 INTO wl_zfiwrt0009 WHERE seq_lcto = wa_zsdt0001-seq_lcto.
      wa_saida-netpr           = wl_zfiwrt0009-netpr.
      wa_saida-seq_lcto        = wa_zsdt0001-seq_lcto.
      wa_saida-danfez          = icon_execute_object.
      IF wa_zsdt0001-nro_nf_rem IS INITIAL OR wa_zsdt0001-nro_nf_rem = ''.
        SELECT SINGLE *
          FROM zfiwrt0008
          INTO wa_zfiwrt0008
          WHERE seq_lcto = wa_zsdt0001-seq_lcto.
        IF sy-subrc = 0.
          IF wa_zfiwrt0008-docnum IS NOT INITIAL.

            PERFORM f_check_auth_doc USING wa_zfiwrt0008-docnum.

            IF sy-subrc = 0.
              wa_zsdt0001-nro_nf_rem = wa_zfiwrt0008-docnum.
              wa_saida-danfez        = wa_zfiwrt0008-docnum.
              UPDATE zsdt0001 SET st_proc    = '12'
                                  nro_nf_rem = wa_zfiwrt0008-docnum
              WHERE ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '12'.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        wa_saida-danfez           = wa_zsdt0001-nro_nf_rem.
      ENDIF.
    ENDIF.


    "BLOQUEIA CELULA
    REFRESH: style.

    IF wa_saida-netpr IS NOT INITIAL.
      PERFORM f_config_cell USING wa_saida-netpr  'NETPR'  cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    IF ( wa_saida-region IS NOT INITIAL ) OR (  wa_zsdt0001-placa_cav IS INITIAL  ).
      PERFORM f_config_cell USING wa_saida-region 'REGION' cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    IF wa_saida-lifnr IS NOT INITIAL.
      PERFORM f_config_cell USING wa_saida-lifnr  'LIFNR'  cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    IF wa_saida-ebeln IS NOT INITIAL.
      PERFORM f_config_cell USING wa_saida-ebeln  'EBELN'  cl_gui_alv_grid=>mc_style_disabled.
      PERFORM f_config_cell USING wa_saida-ebelp  'EBELP'  cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    wa_saida-style[] = style[].

    IF wa_saida-name1 IS INITIAL .
      CLEAR wa_kna1.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zsdt0001-branch  BINARY SEARCH.
      wa_saida-name1           = wa_t001w-name1.
    ENDIF.

    IF wa_zsdt0001-doc_aviso IS INITIAL OR  wa_zsdt0001-doc_aviso = ''.
      wa_saida-aviso        = icon_execute_object.
    ELSE.
      wa_saida-aviso       = wa_zsdt0001-doc_aviso.
    ENDIF.

    IF wa_zsdt0001-doc_rem IS INITIAL OR  wa_zsdt0001-doc_rem = ''.
      wa_saida-remessa         = icon_execute_object.
    ELSE.
      wa_saida-remessa         = wa_zsdt0001-doc_rem.
      "Ponto de coleta (substitui se ja tiver remessa)
      READ TABLE it_vbpa_cr INTO wa_vbpa_cr WITH KEY vbeln = wa_zsdt0001-doc_rem BINARY SEARCH. " Ordem
      IF sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_cr-lifnr.
        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa_cr-lifnr BINARY SEARCH.
        wa_saida-name1_c = wa_lfa1-name1.
      ENDIF.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_prod IS INITIAL OR wa_zsdt0001-fatura_prod = ''.
      wa_saida-fatura          = icon_execute_object.
    ELSE.
      wa_saida-fatura          = wa_zsdt0001-fatura_prod.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatura
        IMPORTING
          output = wa_saida-fatura.

*      "Verfifica se DANFE já está autorizada e muda o campo
*      IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.
*        PERFORM f_check_aut_doc USING '1'
*                                      wa_saida
*                                      wa_zsdt0001
*                             CHANGING vl_docnum.
*
*        IF vl_docnum IS NOT INITIAL.
*          wa_zsdt0001-nro_nf_prod = vl_docnum.
*          wa_saida-danfe          = vl_docnum.
*        ENDIF.
*      ENDIF.
    ENDIF.


    IF wa_zsdt0001-nro_nf_prod IS INITIAL OR wa_zsdt0001-nro_nf_prod = ''.
      wa_saida-danfe           = icon_execute_object.
    ELSE.
      wa_saida-danfe           = wa_zsdt0001-nro_nf_prod.
    ENDIF.

    IF wa_zsdt0001-doc_transp IS INITIAL OR wa_zsdt0001-doc_transp = ''.
      wa_saida-transp          = icon_execute_object.
    ELSE.
      wa_saida-transp          = wa_zsdt0001-doc_transp.

      "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
      READ TABLE git_vttk INTO DATA(lwa_vttk) WITH KEY tknum = wa_zsdt0001-doc_transp.
      IF sy-subrc EQ 0 AND wa_zsdt0001-doc_transp IS NOT INITIAL.
        wa_saida-shtyp = lwa_vttk-shtyp.
      ENDIF.
      "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

    ENDIF.

    IF wa_zsdt0001-fknum IS INITIAL OR wa_zsdt0001-fknum = ''.
      wa_saida-doccus          = icon_icon_list.
    ELSE.
      wa_saida-doccus          = wa_zsdt0001-fknum.
    ENDIF.

    IF wa_zsdt0001-ov_frete IS INITIAL OR wa_zsdt0001-ov_frete = ''.
      wa_saida-ovserv          = icon_icon_list.
    ELSE.
      wa_saida-ovserv          = wa_zsdt0001-ov_frete.
    ENDIF.

    CLEAR vl_docnum.
    IF wa_zsdt0001-fatura_frete IS INITIAL OR wa_zsdt0001-fatura_frete = ''.
      wa_saida-fatserv         = icon_icon_list.
    ELSE.
      wa_saida-fatserv         = wa_zsdt0001-fatura_frete.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-fatserv
        IMPORTING
          output = wa_saida-fatserv.

      IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = ''.
        SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
            FROM j_1bnflin
            INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
            INTO (vl_bukrs,vl_docnum)
            WHERE j_1bnflin~refkey = wa_saida-fatserv.

        IF sy-subrc = 0.

          PERFORM f_check_auth_doc USING vl_docnum.

          IF sy-subrc = 0.
            wa_saida-st_proc = '18'.
            wa_zsdt0001-nro_nf_frete = vl_docnum.

            UPDATE zsdt0001 SET st_proc      = '18'
                                nro_nf_frete = vl_docnum
            WHERE ch_referencia = wa_saida-ch_referencia.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_zsdt0001-nro_nf_frete IS INITIAL OR wa_zsdt0001-nro_nf_frete = ''.
      wa_saida-dacte             = icon_execute_object.
    ELSE.
      wa_saida-dacte             = wa_zsdt0001-nro_nf_frete.
    ENDIF.

    IF wa_saida-tipo = 'P'."Se for pedido de importação não prosseguir
      wa_saida-remessa = icon_icon_list.
      wa_saida-fatura  = icon_icon_list.
      wa_saida-danfe   = icon_icon_list.
    ENDIF.

    "Quando o romaneio for sobre um pedido de transferencia ou ordem de venda, será gerada um documento de remessa.
    "Nesse caso, o frete é emitido sobre a remessa da NF de Venda/Transferencia
    IF NOT ( ( wa_saida-aviso IS NOT INITIAL ) AND ( wa_saida-aviso(1) NE '@' ) ).
      IF ( wa_saida-tipo = 'O' ) OR
         ( wa_saida-tipo = 'T' ).
        wa_saida-aviso  = icon_icon_list.
      ENDIF.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0001-matnr BINARY SEARCH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = wa_saida-matnr.

    CONCATENATE wa_saida-matnr '-' wa_makt-maktx INTO  wa_saida-material.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0001-matnr
      IMPORTING
        output = v_matnr18.

    wa_saida-matnr = v_matnr18.
    "Checar se há cancelamento de CTE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    IF ( wa_saida-dacte   IS NOT INITIAL ) AND ( wa_saida-dacte(1) NE '@' ) AND
       ( wa_saida-inco1   EQ 'CIF' ) AND
       ( wa_saida-st_proc EQ '18' OR wa_saida-st_proc EQ '99' ) .

      PERFORM f_check_canc_doc USING wa_saida-dacte.

      IF sy-subrc EQ 0.

        REFRESH ti_zlest0100.
        wa_saida-dacte = icon_execute_object.
        UPDATE zsdt0001 SET st_proc      = '17'
                            nro_nf_frete = ''
        WHERE ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '17'.
        PERFORM f_estorno_cte CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.

    "Checar se há cancelamento de NFE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    IF ( wa_saida-danfe IS NOT INITIAL ) AND ( wa_saida-danfe(1) NE '@' ) AND
       ( wa_saida-st_proc = '21' OR wa_saida-st_proc = '99' ).

      PERFORM f_check_canc_doc USING wa_saida-danfe.

      IF sy-subrc EQ 0.

        REFRESH ti_zlest0100.
        wa_saida-danfe = icon_execute_object.
        UPDATE zsdt0001 SET   st_proc      = '20'
                              nro_nf_prod  = ''
          WHERE ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '20'.
        PERFORM f_estorno_nfe CHANGING wa_saida.
        IF ti_zlest0100[] IS NOT INITIAL.
          wa_saida-icon = icon_led_red.
        ELSE.
          CLEAR wa_saida-icon.
        ENDIF.
      ENDIF.
    ENDIF.

    "Checar se há cancelamento de NF-Remessa e estorna documentos
    "DANFEZ
    CLEAR wa_zfiwrt0008.
    IF ( wa_saida-seq_lcto IS NOT INITIAL ) AND ( wa_saida-seq_lcto(1) NE '@' ).
      SELECT SINGLE *
        FROM zfiwrt0008 INTO wa_zfiwrt0008
       WHERE seq_lcto = wa_saida-seq_lcto.
    ENDIF.
    IF ( ( wa_saida-danfez IS NOT INITIAL AND wa_saida-danfez(1) NE '@' ) OR
         ( wa_saida-seq_lcto IS NOT INITIAL AND wa_saida-seq_lcto(1) NE '@' ) ) AND
       ( wa_zfiwrt0008-docnum IS NOT INITIAL ).

      PERFORM f_check_canc_doc USING wa_zfiwrt0008-docnum.

      IF sy-subrc EQ 0.

        wa_zsdt0001-nro_nf_rem = ''.
        wa_saida-danfez        = icon_execute_object.
        UPDATE zsdt0001 SET st_proc    = '11'
                            nro_nf_rem = ''
        WHERE ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '11'.
        " LIMPA SEQ_LCTO
        wa_zsdt0001-nro_nf_rem = ''.

        IF wa_zfiwrt0008-docs_estornados EQ abap_true.
          wa_saida-seq_lcto        = icon_execute_object.
          UPDATE zsdt0001 SET st_proc      = ''
                              agente_frete = ''
                              seq_lcto     = ''
          WHERE ch_referencia = wa_saida-ch_referencia.

          wa_saida-st_proc = ''.
          wa_saida-lifnr   = ''.
        ENDIF.

      ENDIF.
    ENDIF.

    IF wa_saida-tipo = 'O' AND wa_saida-inco1 = 'FOB'.
      wa_saida-aviso    = icon_icon_list.

      IF wa_saida-enc_conhecimento = abap_false.
        wa_saida-transp   = icon_icon_list.
        wa_saida-dacte    = icon_icon_list.
      ENDIF.
    ENDIF.

    IF wa_saida-emite_conhecimento EQ abap_false.
      wa_saida-ovserv   = icon_icon_list.
      wa_saida-fatserv  = icon_icon_list.
      wa_saida-dacte    = icon_icon_list.
    ENDIF.

*    IF wa_zsdt0001-st_proc = '99' .
*      IF wa_saida-transp          = icon_execute_object.
*        wa_saida-transp          = icon_icon_list.
*      ENDIF.
*      IF wa_saida-dacte          = icon_execute_object.
*        wa_saida-dacte          = icon_icon_list.
*      ENDIF.
*    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING wa_saida.

    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.

  IF s_lifnr-low IS NOT INITIAL.
    DELETE it_saida WHERE lifnr NE s_lifnr-low.
  ENDIF.

  SORT it_zsdt0001 BY ch_referencia.

  SORT it_saida BY nr_romaneio.

ENDFORM.                    " F_SAIDA_04

*FORM F_SAIDA_08.
*
*  DATA:  TABIX      TYPE SY-TABIX,
*         V_CONT_FRE TYPE I,
*         V_CONT_PED TYPE I,
*         V_CD_UF    TYPE ZLEST0002-CD_UF.
*
*  SORT: IT_VBAK     BY VBELN,
*        IT_TVAKT    BY AUART,
*        IT_T161T    BY BSART,
*        IT_KNA1     BY KUNNR,
*        IT_LFA1     BY LIFNR,
*        IT_T001W    BY WERKS,
*        IT_VBKD     BY VBELN,
*        IT_MAKT     BY MATNR,
*        IT_EKKO     BY EBELN,
*        IT_EKPO     BY EBELN,
*        IT_VBPA     BY VBELN PARVW,
*        IT_VBPA_CR  BY VBELN,  "Ponto de coleta  REMESSA
*        IT_VBPA_CO  BY VBELN,  "Ponto de coleta  ORDEM
*        IT_EKPA_PR  BY EBELN,  "Ponto de coleta  Pedido
*        IT_VBAP     BY VBELN,  "Itinerário  ORDEM
*        IT_EKPV     BY EBELN.  "Itinerário  PEDIDO
*
*
*  SORT: IT_ZSDT0011_O BY TP_MOVIMENTO AUART,
*        IT_ZSDT0011_P BY TP_MOVIMENTO BSART,
*        IT_ZSDT0062   BY VBELN EBELN EBELP.
*
*  "Atualiza variaveis de frete
*  IT_ZSDT0001_FRE[] = IT_ZSDT0001[].
*  PERFORM F_PEGA_FRETE.
*  IT_ZSDT0001[] = IT_ZSDT0001_FRE[].
*  REFRESH IT_ZSDT0001_FRE.
*
*
*  LOOP AT IT_ZSDT0001 INTO WA_ZSDT0001.
*    WA_SAIDA-BUKRS           = WA_ZSDT0001-BUKRS.
*    WA_SAIDA-BRANCH          = WA_ZSDT0001-BRANCH.
*    WA_SAIDA-NR_SAFRA        = WA_ZSDT0001-NR_SAFRA.
*    WA_SAIDA-CH_REFERENCIA   = WA_ZSDT0001-CH_REFERENCIA.
*    WA_SAIDA-DT_MOVIMENTO    = WA_ZSDT0001-DT_MOVIMENTO.
*    WA_SAIDA-NR_ROMANEIO     = WA_ZSDT0001-NR_ROMANEIO.
*    WA_SAIDA-NRO_CG          = WA_ZSDT0001-NRO_CG.
*    WA_SAIDA-PLACA_CAV       = WA_ZSDT0001-PLACA_CAV.
*
*    IF WA_ZSDT0001-REGION IS NOT INITIAL.
*      WA_SAIDA-REGION          = WA_ZSDT0001-REGION.
*    ELSE.
*      SELECT SINGLE CD_UF
*        FROM ZLEST0002
*        INTO V_CD_UF
*        WHERE PC_VEICULO = WA_ZSDT0001-PLACA_CAV.
*      IF SY-SUBRC = 0.
*        WA_SAIDA-REGION          = V_CD_UF.
*      ENDIF.
*    ENDIF.
*
*    WA_SAIDA-ROUTE           = WA_ZSDT0001-ROUTE.
*    WA_SAIDA-ST_PROC         = WA_ZSDT0001-ST_PROC.
*    WA_SAIDA-SHTYP           = WA_ZSDT0001-SHTYP.
*
*    CLEAR WA_SAIDA-ICON.
*    REFRESH TI_ZLEST0100.
*    SELECT *
*      FROM ZLEST0100
*      INTO TABLE TI_ZLEST0100
*      WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
*
*    IF TI_ZLEST0100[] IS NOT INITIAL.
*      WA_SAIDA-ICON = ICON_LED_RED.
*    ELSE.
*      CLEAR WA_SAIDA-ICON.
*    ENDIF.
*
*    CLEAR WA_VBAK.
*    READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_ZSDT0001-VBELN BINARY SEARCH. " Ordem
*    IF SY-SUBRC = 0.
*      WA_SAIDA-TIPO = 'O'.
*      "Ponto de coleta ordem
*      READ TABLE IT_VBPA_CO INTO WA_VBPA_CO WITH KEY VBELN = WA_ZSDT0001-VBELN BINARY SEARCH. " Ordem
*      IF SY-SUBRC = 0.
*        WA_SAIDA-LIFNR_C = WA_VBPA_CO-LIFNR.
*        WA_SAIDA-PONTO_COLETA = WA_VBPA_CO-LIFNR.
*        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VBPA_CO-LIFNR BINARY SEARCH.
*        WA_SAIDA-NAME1_C = WA_LFA1-NAME1.
*      ENDIF.
*
*      "Zona Local de entrega ordem
*      READ TABLE IT_VBPA INTO WA_VBPA  WITH KEY VBELN = WA_ZSDT0001-VBELN
*                                                PARVW = 'LR' BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_SAIDA-LOCAL_ENTREGA = WA_VBPA-KUNNR.
*      ENDIF.
*
*      READ TABLE IT_TVAKT INTO WA_TVAKT WITH KEY AUART = WA_VBAK-AUART BINARY SEARCH.
*      CONCATENATE WA_VBAK-AUART '-' WA_TVAKT-BEZEI INTO WA_SAIDA-OPERACAO.
*      CLEAR WA_KNA1.
*      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR BINARY SEARCH.
*      WA_SAIDA-NAME1           = WA_KNA1-NAME1.
*
*      READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_ZSDT0001-VBELN BINARY SEARCH.
*      IF SY-SUBRC = 0 .
*        WA_SAIDA-INCO1           = WA_VBKD-INCO1.
*      ELSE.
*        CLEAR WA_SAIDA.
*        CONTINUE.
*      ENDIF.
*
*      WA_SAIDA-VBELN           = WA_ZSDT0001-VBELN. "ORDEM VENDA
*
*      IF WA_ZSDT0001-EBELN IS INITIAL.
*        CLEAR V_CONT_PED.
*
*        LOOP AT IT_ZSDT0062 INTO WA_ZSDT0062 WHERE VBELN = WA_ZSDT0001-VBELN.
*          WA_SAIDA-EBELN           = WA_ZSDT0062-EBELN. "PEDIDO
*          ADD 1 TO V_CONT_PED.
*        ENDLOOP.
*
*        IF ( V_CONT_PED = 0 ) AND ( WA_SAIDA-OPERACAO(4) NE 'ZTER' ). "Frete
*          CLEAR WA_SAIDA.
*          CONTINUE.
*        ELSEIF V_CONT_PED GT 1. " se tiver mais de 1, tem que selecionar
*          CLEAR WA_SAIDA-EBELN.
*        ENDIF.
*      ELSE.
*        WA_SAIDA-EBELN           = WA_ZSDT0001-EBELN.
*      ENDIF.
*
*    ELSE.
*      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_ZSDT0001-VBELN BINARY SEARCH. " Pedidos de transferencia
*      IF WA_EKKO-BSART = 'ZUB'.
*        WA_SAIDA-TIPO = 'T'.
*        WA_SAIDA-VBELN           = WA_ZSDT0001-VBELN. "Pedido de transferencia
*        IF WA_ZSDT0001-EBELN IS INITIAL.
*          CLEAR WA_SAIDA-EBELN.   "Pedido de importação
*        ELSE.
*          WA_SAIDA-EBELN = WA_ZSDT0001-EBELN.
*        ENDIF.
*      ELSE.
*        WA_SAIDA-TIPO = 'P'.
*        CLEAR WA_SAIDA-VBELN.
*        WA_SAIDA-EBELN           = WA_ZSDT0001-VBELN.
*      ENDIF.
*
*      "Ponto de coleta pedido
*      READ TABLE IT_EKPA_PR INTO WA_EKPA_PR WITH KEY EBELN = WA_ZSDT0001-VBELN BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_SAIDA-LIFNR_C = WA_EKPA_PR-LIFN2.
*        WA_SAIDA-PONTO_COLETA = WA_EKPA_PR-LIFN2.
*        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKPA_PR-LIFN2 BINARY SEARCH.
*        WA_SAIDA-NAME1_C = WA_LFA1-NAME1.
*      ENDIF.
*
*      READ TABLE IT_T161T INTO WA_T161T WITH KEY BSART = WA_EKKO-BSART BINARY SEARCH.
*      CONCATENATE WA_EKKO-BSART '-' WA_T161T-BATXT  INTO WA_SAIDA-OPERACAO.
*
*      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_ZSDT0001-VBELN BINARY SEARCH. " Pedidos de transferencia
*      IF SY-SUBRC = 0.
*        WA_SAIDA-INCO1  = WA_EKPO-INCO1.
*        "local de entrega pedido
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = WA_EKPO-WERKS
*          IMPORTING
*            OUTPUT = WA_SAIDA-LOCAL_ENTREGA.
*      ELSE.
*        CLEAR WA_SAIDA.
*        CONTINUE.
*      ENDIF.
*
*      CLEAR WA_LFA1.
*      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKPO-LIFNR BINARY SEARCH.
*      WA_SAIDA-NAME1           = WA_LFA1-NAME1.
*
*      IF WA_ZSDT0001-DOC_AVISO IS NOT INITIAL.
*        READ TABLE IT_LIKP INTO DATA(WA_LIKP) WITH KEY VBELN = WA_ZSDT0001-DOC_AVISO.
*        IF ( SY-SUBRC = 0 ).
*          IF ( WA_LIKP-INCO1 IS NOT INITIAL ) AND ( WA_SAIDA-INCO1 IS INITIAL ).
*            WA_SAIDA-INCO1 = WA_LIKP-INCO1.
*          ENDIF.
*        ENDIF.
*
*        "Ponto de coleta Aviso
*        READ TABLE IT_VBPA_CR INTO WA_VBPA_CR WITH KEY VBELN = WA_ZSDT0001-DOC_AVISO BINARY SEARCH.
*        IF ( SY-SUBRC = 0 ) AND ( WA_SAIDA-LIFNR_C IS INITIAL ).
*          WA_SAIDA-LIFNR_C = WA_VBPA_CR-LIFNR.
*          WA_SAIDA-PONTO_COLETA = WA_VBPA_CR-LIFNR.
*          READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VBPA_CR-LIFNR BINARY SEARCH.
*          WA_SAIDA-NAME1_C = WA_LFA1-NAME1.
*        ENDIF.
*
*        "Zona Local de entrega
*        READ TABLE IT_VBPA INTO WA_VBPA  WITH KEY VBELN = WA_ZSDT0001-DOC_AVISO
*                                                  PARVW = 'LR' BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          WA_SAIDA-LOCAL_ENTREGA = WA_VBPA-KUNNR.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*    PERFORM F_ATUAL_FRETE USING WA_ZSDT0001 'L' CHANGING WA_SAIDA.
*
*    " substituivalor do frete pelo valor historico
*    IF WA_ZSDT0001-KBETR GT 0.
*      WA_SAIDA-KBETR = WA_ZSDT0001-KBETR.
*      WA_SAIDA-KONWA = WA_ZSDT0001-KONWA.
*      V_CONT_FRE = 1.
*    ENDIF.
*
*    WA_SAIDA-PESO_LIQ        = WA_ZSDT0001-PESO_LIQ.
*    WA_SAIDA-PESO_FISCAL     = WA_ZSDT0001-PESO_FISCAL.
*
*    IF WA_ZSDT0001-AGENTE_FRETE IS NOT INITIAL.
*      WA_SAIDA-LIFNR = WA_ZSDT0001-AGENTE_FRETE.
*      READ TABLE IT_VBPA INTO WA_VBPA  WITH KEY VBELN = WA_ZSDT0001-VBELN
*                                                 PARVW = 'LR' BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA-KUNNR BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          WA_SAIDA-NAME1           = WA_KNA1-NAME1.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      IF ( 'ZRFL_ZRDC' CS WA_VBAK-AUART ) AND
*         ( WA_VBKD-INCO1 = 'CIF' ) AND
*         ( WA_VBAK-AUART IS NOT INITIAL ).
*        READ TABLE IT_VBPA INTO WA_VBPA  WITH KEY VBELN = WA_ZSDT0001-VBELN
*                                                  PARVW = 'SP' BINARY SEARCH.
*        WA_SAIDA-LIFNR           = WA_VBPA-LIFNR.
*      ELSE.
*        READ TABLE IT_VBPA INTO WA_VBPA  WITH KEY VBELN = WA_ZSDT0001-VBELN
*                                                  PARVW = 'LR' BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA-KUNNR BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            WA_SAIDA-NAME1           = WA_KNA1-NAME1.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*    "BLOQUEIA CELULA
*    REFRESH: STYLE.
*
*    IF ( WA_SAIDA-NETPR IS NOT INITIAL ).
*      PERFORM F_CONFIG_CELL USING WA_SAIDA-NETPR  'NETPR'  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    IF ( WA_SAIDA-REGION IS NOT INITIAL ) OR (  WA_ZSDT0001-PLACA_CAV IS INITIAL  ).
*      PERFORM F_CONFIG_CELL USING WA_SAIDA-REGION 'REGION' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    IF ( ( WA_ZSDT0001-DOC_TRANSP IS NOT INITIAL ) AND ( WA_ZSDT0001-DOC_TRANSP(1) NE '@' ) ).
*      PERFORM F_CONFIG_CELL USING WA_SAIDA-LIFNR  'LIFNR'  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    IF WA_SAIDA-EBELN IS NOT INITIAL.
*      PERFORM F_CONFIG_CELL USING WA_SAIDA-EBELN  'EBELN'  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    WA_SAIDA-STYLE[] = STYLE[].
*
*    IF WA_SAIDA-NAME1 IS INITIAL .
*      CLEAR WA_KNA1.
*      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_ZSDT0001-BRANCH  BINARY SEARCH.
*      WA_SAIDA-NAME1           = WA_T001W-NAME1.
*    ENDIF.
*
*    IF WA_ZSDT0001-DOC_AVISO IS INITIAL OR  WA_ZSDT0001-DOC_AVISO = ''.
*      WA_SAIDA-AVISO  = ICON_ICON_LIST.
*    ELSE.
*      WA_SAIDA-AVISO  = WA_ZSDT0001-DOC_AVISO.
*    ENDIF.
*
*    WA_SAIDA-REMESSA = ICON_ICON_LIST.
*    IF WA_SAIDA-TIPO = 'O'.
*      IF WA_ZSDT0001-DOC_REM IS INITIAL OR  WA_ZSDT0001-DOC_REM = ''.
*        WA_SAIDA-REMESSA         = ICON_EXECUTE_OBJECT.
*      ELSE.
*        WA_SAIDA-REMESSA         = WA_ZSDT0001-DOC_REM.
*        "Ponto de coleta (substitui se ja tiver remessa)
*        READ TABLE IT_VBPA_CR INTO WA_VBPA_CR WITH KEY VBELN = WA_ZSDT0001-DOC_REM BINARY SEARCH. " Ordem
*        IF SY-SUBRC = 0.
*          WA_SAIDA-LIFNR_C = WA_VBPA_CR-LIFNR.
*          READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VBPA_CR-LIFNR BINARY SEARCH.
*          WA_SAIDA-NAME1_C = WA_LFA1-NAME1.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF WA_ZSDT0001-DOC_TRANSP IS INITIAL OR WA_ZSDT0001-DOC_TRANSP = ''.
*      WA_SAIDA-TRANSP          = ICON_EXECUTE_OBJECT.
*    ELSE.
*      WA_SAIDA-TRANSP          = WA_ZSDT0001-DOC_TRANSP.
*    ENDIF.
*
*    IF WA_ZSDT0001-FKNUM IS INITIAL OR WA_ZSDT0001-FKNUM = ''.
*      WA_SAIDA-DOCCUS          = ICON_ICON_LIST.
*    ELSE.
*      WA_SAIDA-DOCCUS          = WA_ZSDT0001-FKNUM.
*    ENDIF.
*
*    IF WA_ZSDT0001-OV_FRETE IS INITIAL OR WA_ZSDT0001-OV_FRETE = ''.
*      WA_SAIDA-OVSERV          = ICON_ICON_LIST.
*    ELSE.
*      WA_SAIDA-OVSERV          = WA_ZSDT0001-OV_FRETE.
*    ENDIF.
*
*    CLEAR VL_DOCNUM.
*    IF WA_ZSDT0001-FATURA_FRETE IS INITIAL OR WA_ZSDT0001-FATURA_FRETE = ''.
*      WA_SAIDA-FATSERV         = ICON_ICON_LIST.
*    ELSE.
*      WA_SAIDA-FATSERV         = WA_ZSDT0001-FATURA_FRETE.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_SAIDA-FATSERV
*        IMPORTING
*          OUTPUT = WA_SAIDA-FATSERV.
*
*      IF WA_ZSDT0001-NRO_NF_FRETE IS INITIAL OR WA_ZSDT0001-NRO_NF_FRETE = ''.
*        SELECT SINGLE J_1BNFDOC~BUKRS J_1BNFLIN~DOCNUM
*            FROM J_1BNFLIN
*            INNER JOIN J_1BNFDOC ON J_1BNFDOC~DOCNUM = J_1BNFLIN~DOCNUM
*            INTO (VL_BUKRS,VL_DOCNUM)
*            WHERE J_1BNFLIN~REFKEY = WA_SAIDA-FATSERV.
*
*        IF SY-SUBRC = 0.
*          SELECT SINGLE DOCNUM
*           FROM J_1BNFE_ACTIVE
*           INTO V_DOCNUM
*           WHERE DOCNUM     = VL_DOCNUM
*           AND   CANCEL     = ''
*           AND   DOCSTA     = '1'.
*
*          IF SY-SUBRC = 0.
*            WA_ZSDT0001-NRO_NF_FRETE = VL_DOCNUM.
*            UPDATE ZSDT0001 SET ST_PROC      = '99'
*                                NRO_NF_FRETE = VL_DOCNUM
*            WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
*            WA_SAIDA-ST_PROC = '99'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF WA_ZSDT0001-NRO_NF_FRETE IS INITIAL OR WA_ZSDT0001-NRO_NF_FRETE = ''.
*      WA_SAIDA-DACTE             = ICON_EXECUTE_OBJECT.
*    ELSE.
*      WA_SAIDA-DACTE             = WA_ZSDT0001-NRO_NF_FRETE.
*    ENDIF.
*
*    IF WA_ZSDT0001-ST_PROC = '99' .
*      IF WA_SAIDA-TRANSP          = ICON_EXECUTE_OBJECT.
*        WA_SAIDA-TRANSP          = ICON_ICON_LIST.
*      ENDIF.
*      IF WA_SAIDA-DACTE          = ICON_EXECUTE_OBJECT.
*        WA_SAIDA-DACTE          = ICON_ICON_LIST.
*      ENDIF.
*    ENDIF.
*
*    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZSDT0001-MATNR BINARY SEARCH.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_ZSDT0001-MATNR
*      IMPORTING
*        OUTPUT = WA_SAIDA-MATNR.
*
*    CONCATENATE WA_SAIDA-MATNR '-' WA_MAKT-MAKTX INTO  WA_SAIDA-MATERIAL.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_ZSDT0001-MATNR
*      IMPORTING
*        OUTPUT = WA_SAIDA-MATNR.
*
*    "Checar se há cancelamento de CTE e estorna documentos
*    "DACTE
*    IF ( WA_SAIDA-DACTE IS NOT INITIAL ) AND ( WA_SAIDA-DACTE(1) NE '@' ) AND
*       ( WA_SAIDA-INCO1 EQ 'CIF' ) AND
*       ( WA_SAIDA-ST_PROC EQ '18' OR WA_SAIDA-ST_PROC EQ '99' ) . " no automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
*
*      SELECT SINGLE DOCNUM
*        FROM J_1BNFE_ACTIVE
*        INTO V_DOCNUM
*        WHERE DOCNUM     = WA_SAIDA-DACTE "NRO_NF_FRETE
*        AND   CANCEL       = 'X'.
*
*      IF SY-SUBRC EQ 0.
*        REFRESH TI_ZLEST0100.
*        "LIMPA CTE
*        WA_SAIDA-DACTE           = ICON_EXECUTE_OBJECT.
*        UPDATE ZSDT0001 SET ST_PROC      = '17'
*                            NRO_NF_FRETE = ''
*        WHERE CH_REFERENCIA = WA_SAIDA-CH_REFERENCIA.
*        WA_SAIDA-ST_PROC = '17'.
*        PERFORM F_ESTORNO_CTE CHANGING WA_SAIDA.
*        IF TI_ZLEST0100[] IS NOT INITIAL.
*          WA_SAIDA-ICON = ICON_LED_RED.
*        ELSE.
*          CLEAR WA_SAIDA-ICON.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF WA_SAIDA-TIPO = 'O' AND WA_SAIDA-INCO1 = 'FOB'.
*      WA_SAIDA-AVISO    = ICON_ICON_LIST.
*      WA_SAIDA-TRANSP   = ICON_ICON_LIST.
*      WA_SAIDA-DACTE    = ICON_ICON_LIST.
*    ENDIF.
*    APPEND WA_SAIDA TO IT_SAIDA.
*    CLEAR WA_SAIDA.
*  ENDLOOP.
*
*  IF S_LIFNR-LOW IS NOT INITIAL.
*    DELETE IT_SAIDA WHERE LIFNR NE S_LIFNR-LOW.
*  ENDIF.
*
*  SORT IT_ZSDT0001 BY CH_REFERENCIA.
*
*  SORT IT_SAIDA BY NR_ROMANEIO.
*
*ENDFORM.                    " F_SAIDA_08

FORM f_seleciona_dados.

  "Configura os status de Processamento
  PERFORM f_set_status_proc.

  PERFORM f_refresh_data.

  PERFORM f_get_value_set TABLES t_auart  USING 'MAGGI_ARMAZENAGEM_VA01'.

  PERFORM f_get_value_set TABLES t_usermd USING 'MAGGI_ZLES106_RECUP'.

  PERFORM f_config_ranges.

  IF r_dt_a = 'X'.
    SELECT *
      FROM zsdt0001 INTO TABLE it_zsdt0001
     WHERE bukrs        IN r_bukrs
       AND branch       IN r_branch
       AND vbeln        IN s_vbeln
       AND vbeln        IN s_ebeln
       AND doc_transp   IN s_doc
       AND vbeln        NE ''
       AND dt_movimento IN s_data
       AND parid        IN r_coleta
       AND dt_movimento GE '20141006'
       AND st_proc      NE '99'
       AND tp_movimento IN r_tp_movimento.
  ELSEIF r_dt_f = 'X'.
    SELECT *
      FROM zsdt0001 INTO TABLE it_zsdt0001
     WHERE bukrs        IN r_bukrs
       AND branch       IN r_branch
       AND vbeln        IN s_vbeln
       AND vbeln        IN s_ebeln
       AND doc_transp   IN s_doc
       AND vbeln        NE ''
       AND dt_movimento IN s_data
       AND parid        IN r_coleta
       AND st_proc      EQ '99'
       AND tp_movimento IN r_tp_movimento.
  ELSE.
    SELECT *
      FROM zsdt0001 INTO TABLE it_zsdt0001
     WHERE bukrs         IN r_bukrs
       AND branch        IN r_branch
       AND vbeln         IN s_vbeln
       AND vbeln         IN s_ebeln
       AND doc_transp    IN s_doc
       AND vbeln         NE space
       AND dt_movimento  IN s_data
       AND tp_movimento  IN r_tp_movimento
       AND ch_referencia IN s_chave
       AND id_interface  EQ p_inter.
  ENDIF.

  SORT it_zsdt0001 BY vbeln.

  CHECK ( it_zsdt0001[] IS NOT INITIAL ) AND ( vg_cockpit IS NOT INITIAL ).

  "Seleção de Tabelas genéricas, independente do Processo do Cockipit.
  PERFORM f_selecao_generica_rom.

*-------------------------------------------------------------------------------------*
*  Início tratamento de registros de acordo com o tipo do Cockpit
*-------------------------------------------------------------------------------------*

  "Exceção de Faturamento Fertilizantes
  SELECT *
    INTO TABLE it_zlest0132
    FROM zlest0132.

  SORT: it_zlest0132 BY branch parid.

  LOOP AT it_zsdt0001 ASSIGNING FIELD-SYMBOL(<out_zsdt0001>).
    CLEAR: wa_zlest0132, wa_mara, wa_vbap, wa_vbak.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <out_zsdt0001>-vbeln.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = <out_zsdt0001>-vbeln.

    "Filtro Material
    IF s_matnr IS NOT INITIAL.
      IF <out_zsdt0001>-matnr NOT IN s_matnr.
        CLEAR: tg_zsdt0001_item.
        LOOP AT tg_zsdt0001_item WHERE ch_referencia EQ <out_zsdt0001>-ch_referencia
                                   AND matnr         IN s_matnr.
          EXIT.
        ENDLOOP.
        IF tg_zsdt0001_item IS INITIAL.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <out_zsdt0001>-matnr IS NOT INITIAL.
      READ TABLE it_mara INTO wa_mara WITH KEY matnr = <out_zsdt0001>-matnr BINARY SEARCH.
    ELSE.
      IF ( <out_zsdt0001>-vbeln IS NOT INITIAL ) AND ( wa_vbap-matnr IS NOT INITIAL ).
        READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
      ENDIF.
    ENDIF.

    SELECT *
      FROM zsdt0121
      INTO TABLE t_fatura_agrupada
     WHERE werks = p_branch.

    SORT t_fatura_agrupada BY werks matnr kunnr inco1 cfop.

    "Tabela de Faturamento Fertilizantes
    READ TABLE it_zlest0132 INTO wa_zlest0132 WITH KEY branch = <out_zsdt0001>-branch
                                                       parid  = <out_zsdt0001>-parid BINARY SEARCH.

    CASE vg_cockpit.  "Tratamento de Registros de Romaneio
      WHEN '01'. "Commodities (Formação Lote, Vendas e Trâsferências Expedidas)
        CLEAR w_fatura_agrupada.
        READ TABLE t_fatura_agrupada INTO w_fatura_agrupada WITH KEY werks = <out_zsdt0001>-branch
                                                                     matnr = <out_zsdt0001>-matnr
                                                                     kunnr = <out_zsdt0001>-id_cli_dest BINARY SEARCH.
        IF  ( <out_zsdt0001>-id_interface NE '48' ) AND
            ( <out_zsdt0001>-id_interface NE '49' ) AND
            ( <out_zsdt0001>-id_interface NE '50' ) AND
            ( <out_zsdt0001>-id_interface NE '51' ) AND
            ( <out_zsdt0001>-id_interface NE '52' ) AND
            "( W_FATURA_AGRUPADA-WERKS IS INITIAL ) AND "excluindo romaneios de troca de nota  da  região sul  ( PR, SC e RS)
            ( wa_vbak-spart NE '02' OR
              wa_zlest0132 IS INITIAL ).
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      WHEN '03'. "Troca de notas - Commodities com agrupamento
        CLEAR w_fatura_agrupada.
        READ TABLE t_fatura_agrupada INTO w_fatura_agrupada WITH KEY werks = <out_zsdt0001>-branch
                                                                     matnr = <out_zsdt0001>-matnr
                                                                     kunnr = <out_zsdt0001>-id_cli_dest BINARY SEARCH.
        IF w_fatura_agrupada-werks IS NOT INITIAL .
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      WHEN '04'. "Fertilizantes - Porto Velho
        IF  ( <out_zsdt0001>-id_interface NE '49'            ) AND
            ( <out_zsdt0001>-id_interface NE '51'            ) AND
            ( wa_vbak-spart EQ '02' OR  wa_mara-spart = '02' ) AND
            ( wa_zlest0132 IS NOT INITIAL                    ).
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      WHEN '05'. "Sementes
        IF ( <out_zsdt0001>-id_interface EQ '48' ).
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      WHEN '06'. "Defensivos
        IF ( <out_zsdt0001>-id_interface EQ '52' ).
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
      WHEN '07'. "Fertilizantes
        IF ( <out_zsdt0001>-id_interface EQ '51' ).
          CONTINUE.
        ELSE.
          <out_zsdt0001>-del = 'X'.
        ENDIF.
*      WHEN '08'. "Frete
*        IF ( <out_zsdt0001>-id_interface EQ '49' ).
*          CONTINUE.
*        ELSE.
*          <out_zsdt0001>-del = 'X'.
*        ENDIF.
    ENDCASE.

  ENDLOOP.

  DELETE it_zsdt0001 WHERE del = 'X'.

*-------------------------------------------------------------------------------------*
*  Fim tratamento de registros de acordo com o tipo do Cockpit
*-------------------------------------------------------------------------------------*

  "Define perform para busca especificia de dados auxiliares do Romaneio de acordo com o processo
  CONCATENATE 'F_GET_TABLE_AUX_ROM_' vg_cockpit INTO DATA(form_get_rom_aux).
  "Seleção de Tabelas Específicas do Processo do Cockpit
  PERFORM (form_get_rom_aux) IN PROGRAM zlesr0102 IF FOUND.


ENDFORM.                    " F_SELECIONA_DADOS

FORM f_montar_layout_veic.

  CLEAR: it_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''              ''             'IT_VEIC' 'PLACA'         'Placa'           '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    02  ''              ''             'IT_VEIC' 'TIPO'          'Tipo'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    03  ''              ''             'IT_VEIC' 'COD_PROP'      'Proprietário'    ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    04  ''              ''             'IT_VEIC' 'NOM_PROP'      'Nome'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    05  ''              ''             'IT_VEIC' 'RNTC'          'RNTC'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    06  ''              ''             'IT_VEIC' 'RENAVAM'       'Renavam'         ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    07  ''              ''             'IT_VEIC' 'CNPJ'          'CPF/CNPJ'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    08  ''              ''             'IT_VEIC' 'CIDADE'        'Cidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    09  ''              ''             'IT_VEIC' 'UF'            'UF'              ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT_VEIC

FORM f_alv_header .

  DATA: wl_data(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  IF r_dt_a = 'X'.
    wl_linha = 'Romaneios em aberto'.
  ELSEIF r_dt_f = 'X'.
    wl_linha = 'Romaneios finalizados'.
  ELSE.
    wl_linha = 'Romaneios abertos e finalizados'.
  ENDIF.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  SELECT SINGLE *
    FROM t001 INTO wa_t001
   WHERE bukrs = p_bukrs.

  SELECT SINGLE *
    FROM j_1bbranch INTO wa_j_1bbranch
   WHERE bukrs  = p_bukrs
     AND branch = p_branch.

  CONCATENATE  'Empresa:' p_bukrs '-' wa_t001-butxt
          INTO wl_linha SEPARATED BY space.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_fontsize = cl_dd_area=>list_normal.

  CALL METHOD obj_dyndoc_id->new_line.

  CONCATENATE  'Filial......:' p_branch '-' wa_j_1bbranch-name
         INTO wl_linha SEPARATED BY space.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_fontsize = cl_dd_area=>list_normal.


ENDFORM.                    " ZF_ALV_HEADER

FORM f_imprime_dados .
  PERFORM f_monta_layout.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS

FORM f_get_value_set TABLES p_values STRUCTURE  rgsb4
                      USING p_setnr.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = p_setnr
    TABLES
      set_values    = p_values
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SORT p_values BY from.

ENDFORM.

FORM f_config_ranges.

  CLEAR: r_tp_movimento[], r_bukrs[], r_branch[], r_coleta[].

  r_tp_movimento-sign   = 'I'.
  r_tp_movimento-option = 'EQ'.
  r_tp_movimento-low    = 'S'.
  APPEND r_tp_movimento.

  IF p_bukrs IS NOT INITIAL.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = p_bukrs.
    APPEND r_bukrs.
  ENDIF.

  IF s_branch IS NOT INITIAL. "RJF
    r_branch[] = s_branch[].
  ELSE.
    IF p_branch IS NOT INITIAL.
      r_branch-sign   = 'I'.
      r_branch-option = 'EQ'.
      r_branch-low    = p_branch.
      APPEND r_branch.
    ENDIF.
  ENDIF.

  CLEAR: r_dlgrp.
  r_dlgrp-sign   = 'I'.
  r_dlgrp-option = 'EQ'.
  r_dlgrp-low    = '0001'.
  APPEND r_dlgrp.
  r_dlgrp-low    = '0007'.
  APPEND r_dlgrp.

  CLEAR: r_vsart.
  r_vsart-sign   = 'I'.
  r_vsart-option = 'EQ'.
  r_vsart-low    = '01'. "Rodoviario
  APPEND r_vsart.
  r_vsart-low    = '07'. "Multimodal
  APPEND r_vsart.

  IF vg_cockpit = '04'.
    CLEAR r_coleta.
    r_coleta-sign   = 'I'.
    r_coleta-option = 'EQ'.
    r_coleta-low    = p_coleta.
    APPEND r_coleta.
  ENDIF.

ENDFORM.

FORM f_set_tp_cockpit.

*----------------------------------------------------------------------*
*  01 - Commodities (Formação Lote, Vendas e Trâsferências Expedidas) - ZLES0106
*  02 - Commodities (Armazenagem Enviadas - Remessas e Devoluções )
*  03 - Commodities (Trânsferência Recebidas )
*  04 - Fertilizantes (Porto Velho)                                   - ZLES0115
*  Insumos (Vendas e Trânsferências expedidas )
*  05 - Sementes
*  06 - Defensivos
*  07 - Fertilizantes
*  08 - Frete
*  09 - Romaneio de Entrada Completo

  IF r_cp_01 IS NOT INITIAL.
    vg_cockpit = '01'.
  ELSEIF r_cp_02 IS NOT INITIAL.
    vg_cockpit = '02'.
  ELSEIF r_cp_03 IS NOT INITIAL.
    vg_cockpit = '03'.
  ELSEIF r_cp_04 IS NOT INITIAL.
    vg_cockpit = '04'.
  ELSEIF r_cp_05 IS NOT INITIAL.
    vg_cockpit = '05'.
  ELSEIF r_cp_06 IS NOT INITIAL.
    vg_cockpit = '06'.
  ELSEIF r_cp_07 IS NOT INITIAL.
    vg_cockpit = '07'.
  ELSEIF r_cp_09 IS NOT INITIAL.
    vg_cockpit = '09'.
  ELSEIF r_cp_10 IS NOT INITIAL.
    vg_cockpit = '10'.
  ENDIF.

ENDFORM.

FORM f_refresh_data.

  CLEAR: it_lfa1[],
         t_auart[],
         t_usermd[],
         it_zsdt0001[],
         it_zlest0132[],
         it_mara[],
         it_vbpa_cr[],
         it_vbpa_co[],
         it_ekpa_pr[],
         it_vbap[],
         it_ekpv[],
         it_makt[],
         it_t001w[],
         it_vbak[],
         it_zsdt0011_o[],
         it_kna1[],
         it_tvakt[],
         it_vbkd[],
         it_ekko[],
         git_vttk[],
         it_zsdt0011_p[],
         it_ekpo[],
         it_t161t[],
         it_zsdt0062[],
         tg_zsdt0001_item[],
         r_dlgrp[].

ENDFORM.

FORM f_selecao_generica_rom.

  RANGES: lra_bsart_cockpit_01 FOR ekko-bsart.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_tvarvc_bsart_01)
   WHERE name EQ 'ZLES0136_BSART_COCKPIT_01'.

  IF lit_tvarvc_bsart_01[] IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'ZUB' ) TO lra_bsart_cockpit_01.
  ELSE.
    LOOP AT lit_tvarvc_bsart_01 INTO DATA(lwa_tvarvc_bsart_01).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_tvarvc_bsart_01-low ) TO lra_bsart_cockpit_01.
    ENDLOOP.
  ENDIF.

  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
  LOOP AT it_zsdt0001 ASSIGNING FIELD-SYMBOL(<fs_zsdt0001>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_zsdt0001>-doc_transp
      IMPORTING
        output = <fs_zsdt0001>-doc_transp.

  ENDLOOP.
  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Fim

  "Dados gerais de material
  SELECT matnr spart
    FROM mara APPENDING TABLE it_mara
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE matnr EQ it_zsdt0001-matnr.

  "Busca Pedidos associados a ordem (se existir)
  SELECT vbeln ebeln ebelp matnr status lgort charg qtd_vinc
    FROM zsdt0062 APPENDING TABLE it_zsdt0062
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln  = it_zsdt0001-vbeln.
*---> 10.07.2023 17:05:46 - Migração S4 - DL
*   GROUP BY vbeln ebeln ebelp.
*<--- 10.07.2023 17:05:46 - Migração S4 - DL

  DELETE it_zsdt0062 WHERE status = 'E'.

  DATA(it_zsdt0062_aux) = it_zsdt0062[].

  SORT it_zsdt0062 BY vbeln ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0062 COMPARING vbeln ebeln ebelp.

  LOOP AT it_zsdt0062 ASSIGNING FIELD-SYMBOL(<fs_zsdt0062>).
    CLEAR: <fs_zsdt0062>-qtd_vinc.

    LOOP AT it_zsdt0062_aux INTO DATA(lwa_zsdt0062_aux) WHERE vbeln = <fs_zsdt0062>-vbeln
                                                          AND ebeln = <fs_zsdt0062>-ebeln
                                                          AND ebelp = <fs_zsdt0062>-ebelp.
      ADD lwa_zsdt0062_aux-qtd_vinc TO <fs_zsdt0062>-qtd_vinc.
    ENDLOOP.
  ENDLOOP.


  SELECT *
    FROM likp APPENDING CORRESPONDING FIELDS OF TABLE it_likp
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln  = it_zsdt0001-doc_aviso.

  SELECT *
    FROM lips APPENDING CORRESPONDING FIELDS OF TABLE it_lips
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln  = it_zsdt0001-doc_aviso.

  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
  SELECT *
    FROM vttk APPENDING CORRESPONDING FIELDS OF TABLE git_vttk
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE tknum  = it_zsdt0001-doc_transp.
  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Fim

*-----------------------------------------------------------------------------*
*  Seleção de Pontos de Coleta
*-----------------------------------------------------------------------------*

  PERFORM seleciona_vbpa_co.

  "Remessa
  SELECT vbeln lifnr
    FROM vbpa APPENDING TABLE it_vbpa_cr
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln   = it_zsdt0001-doc_rem
     AND parvw   = 'PC'.

  "Aviso Recebimento
  SELECT vbeln lifnr
    FROM vbpa APPENDING TABLE it_vbpa_cr
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln   = it_zsdt0001-doc_aviso
     AND parvw   = 'PC'.

  IF it_vbpa_cr[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1 APPENDING TABLE it_lfa1
       FOR ALL ENTRIES IN it_vbpa_cr
     WHERE lifnr  = it_vbpa_cr-lifnr.
  ENDIF.

  "Pedido
  SELECT ebeln lifn2
    FROM ekpa APPENDING TABLE it_ekpa_pr
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE ebeln = it_zsdt0001-vbeln
     AND parvw = 'PR'.

  IF it_ekpa_pr[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1 APPENDING TABLE it_lfa1
       FOR ALL ENTRIES IN it_ekpa_pr
     WHERE lifnr  = it_ekpa_pr-lifn2.
  ENDIF.

*-----------------------------------------------------------------------------*
*  Seleção de Itinerário
*-----------------------------------------------------------------------------*

  "Ordem Venda
  SELECT vbeln route matnr posnr
    FROM vbap APPENDING CORRESPONDING FIELDS OF TABLE it_vbap
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln = it_zsdt0001-vbeln.

  "Dados gerais de material
  IF it_vbap[] IS NOT INITIAL.
    SELECT matnr spart
      FROM mara APPENDING CORRESPONDING FIELDS OF TABLE it_mara
       FOR ALL ENTRIES IN it_vbap
     WHERE matnr EQ it_vbap-matnr.

    SELECT matnr maktx
      FROM makt APPENDING TABLE it_makt
       FOR ALL ENTRIES IN it_vbap
     WHERE matnr EQ it_vbap-matnr
       AND spras EQ sy-langu.
  ENDIF.

  IF it_lips[] IS NOT INITIAL.
    SELECT matnr spart
      FROM mara APPENDING CORRESPONDING FIELDS OF TABLE it_mara
       FOR ALL ENTRIES IN it_lips
     WHERE matnr EQ it_lips-matnr.

    SELECT matnr maktx
      FROM makt APPENDING TABLE it_makt
       FOR ALL ENTRIES IN it_lips
     WHERE matnr EQ it_lips-matnr
       AND spras EQ sy-langu.
  ENDIF.

  "Pedido
  SELECT ebeln route
    FROM ekpv APPENDING TABLE it_ekpv
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE ebeln =  it_zsdt0001-vbeln.

  SELECT matnr maktx
    FROM makt APPENDING TABLE it_makt
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE matnr EQ it_zsdt0001-matnr
     AND spras EQ sy-langu.

  SELECT werks name1
    FROM t001w APPENDING TABLE it_t001w
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE werks EQ it_zsdt0001-branch.

  PERFORM seleciona_vbak.

  IF it_vbak[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0011 APPENDING TABLE it_zsdt0011_o
       FOR ALL ENTRIES IN it_vbak
     WHERE tp_movimento = it_vbak-tp_movimento
       AND auart        = it_vbak-auart.

    SELECT kunnr name1 lzone
      FROM kna1 APPENDING TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak
      WHERE kunnr	=	it_vbak-kunnr.

    SELECT auart bezei
      FROM tvakt APPENDING TABLE it_tvakt
       FOR ALL ENTRIES IN it_vbak
     WHERE auart = it_vbak-auart
       AND spras = sy-langu.

    SELECT vbeln inco1
      FROM vbkd APPENDING TABLE it_vbkd
       FOR ALL ENTRIES IN it_zsdt0001
     WHERE vbeln  = it_zsdt0001-vbeln
       AND inco1 IN s_inco1
       AND posnr  = '000000'.
  ENDIF.

  IF vg_cockpit = '01'. "Validar Regra ###

    SELECT ebeln bsart reswk lifnr
      FROM ekko APPENDING TABLE it_ekko
       FOR ALL ENTRIES IN it_zsdt0001
     WHERE ebeln EQ it_zsdt0001-vbeln
       AND ebeln IN s_ebeln
       AND bsart IN lra_bsart_cockpit_01.

  ELSE.

    SELECT ebeln bsart reswk lifnr
      FROM ekko APPENDING TABLE it_ekko
       FOR ALL ENTRIES IN it_zsdt0001
     WHERE ebeln EQ it_zsdt0001-vbeln
       AND ebeln IN s_ebeln.

    IF it_zsdt0062[] IS NOT INITIAL.
      SELECT ebeln bsart reswk
        FROM ekko APPENDING TABLE it_ekko
         FOR ALL ENTRIES IN it_zsdt0062
       WHERE ebeln EQ it_zsdt0062-ebeln
         AND ebeln IN s_ebeln.
    ENDIF.
  ENDIF.

  LOOP AT it_ekko INTO wa_ekko.
    tabix = sy-tabix.
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY vbeln = wa_ekko-ebeln BINARY SEARCH.
    wa_ekko-tp_movimento = wa_zsdt0001-tp_movimento.
    MODIFY it_ekko FROM wa_ekko INDEX tabix TRANSPORTING tp_movimento.
  ENDLOOP.

  IF it_ekko[] IS NOT INITIAL.
    SELECT  *
      FROM zsdt0011 APPENDING TABLE it_zsdt0011_p
       FOR ALL ENTRIES IN it_ekko
     WHERE tp_movimento =  it_ekko-tp_movimento
       AND bsart        =  it_ekko-bsart.

    SELECT ebeln ebelp werks inco1
      FROM ekpo APPENDING TABLE it_ekpo
       FOR ALL ENTRIES IN it_ekko
     WHERE ebeln = it_ekko-ebeln
       AND inco1 IN s_inco1.

    LOOP AT it_ekpo INTO wa_ekpo.
      tabix = sy-tabix .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekpo-werks
        IMPORTING
          output = wa_ekpo-lifnr.
      MODIFY it_ekpo FROM wa_ekpo INDEX tabix TRANSPORTING lifnr.
    ENDLOOP.

    SELECT lifnr name1 dlgrp lzone
      FROM lfa1 APPENDING TABLE it_lfa1
       FOR ALL ENTRIES IN it_ekpo
     WHERE lifnr  = it_ekpo-lifnr.

    SELECT bsart batxt
      FROM t161t APPENDING TABLE it_t161t
       FOR ALL ENTRIES IN it_ekko
     WHERE bsart = it_ekko-bsart
       AND spras = sy-langu.
  ENDIF.

  PERFORM seleciona_vbpa.

  "Itens Romaneio
  SELECT *
    FROM zsdt0001_item INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0001_item
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE ch_referencia EQ it_zsdt0001-ch_referencia.

  SORT: it_mara     BY matnr,
        it_lfa1     BY lifnr,
        it_vbpa_co  BY vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  BY ebeln.  "Ponto de coleta  Pedido

ENDFORM.

FORM f_get_table_aux_rom_01.
ENDFORM.

FORM f_get_table_aux_rom_04.
ENDFORM.


FORM f_exclude_toolbar  USING p_screen.

  CLEAR: tl_function.

  CASE p_screen.
    WHEN '0100'.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_check.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_refresh.
      APPEND wl_function TO tl_function.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_config_layout  USING p_screen.
  CASE p_screen.
    WHEN '0100'.
      CLEAR: wa_layout.
      wa_layout-zebra      = c_x.
      wa_layout-no_rowmark = ''.
      wa_stable-row        = c_x.
      wa_stable-col        = c_x.
      wa_layout-stylefname = 'STYLE'.
      wa_layout-sel_mode   = 'A'.
      wa_layout-cwidth_opt = 'X'.
      wa_layout-col_opt    = 'X'.
      wa_layout-info_fname = 'LINE_COLOR'.
      wa_layout-ctab_fname = 'COLOR_CELL'.
    WHEN '0200'.
  ENDCASE.
ENDFORM.

FORM f_set_status_proc.

  CASE vg_cockpit. "Set status Faturamento
    WHEN '01' OR '05' OR '06' OR '07' OR '09' OR '03' OR '10'.

      vg_st_remessa               = '01'.
      vg_st_fatura                = '02'.
      vg_st_danfe                 = '03'.
      vg_st_transp                = '04'.
      vg_st_custo                 = '05'.
      vg_st_ov_frete              = '06'.
      vg_st_fatura_frete          = '07'.
      vg_st_dacte                 = '08'.

      vg_st_remessa_before        = ''.
      vg_st_fatura_before         = '01'.
      vg_st_danfe_before          = '02'.
      vg_st_transp_before         = '03'.
      vg_st_custo_before          = '04'.
      vg_st_ov_frete_before       = '05'.
      vg_st_fatura_frete_before   = '06'.
      vg_st_dacte_before          = '07'.

    WHEN '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
    WHEN '03'. "Commodities (Trânsferência Recebidas )
    WHEN '04'. "Fertilizantes (Porto Velho) - ZLES0115

      vg_st_znfw                 = '11'.
      vg_st_danfe_znfw           = '12'.
      vg_st_aviso_rec            = '13'.

      vg_st_remessa              = '19'.
      vg_st_fatura               = '20'.
      vg_st_danfe                = '21'.

      vg_st_transp               = '14'.
      vg_st_custo                = '15'.
      vg_st_ov_frete             = '16'.
      vg_st_fatura_frete         = '17'.
      vg_st_dacte                = '18'.


      "Before Status

      vg_st_znfw_before          = ''.
      vg_st_danfe_znfw_before    = '11'.
      vg_st_aviso_rec_before     = '12'.

      vg_st_remessa_before       = '12'.
      vg_st_fatura_before        = '19'.
      vg_st_danfe_before         = '20'.

      vg_st_transp_before        = '21'.
      vg_st_custo_before         = '14'.
      vg_st_ov_frete_before      = '15'.
      vg_st_fatura_frete_before  = '16'.
      vg_st_dacte_before         = '17'.



    WHEN '05'. "Insumos (Vendas e Trânsferências expedidas )


  ENDCASE.

  vg_st_finalizado = '99'.

*----CS2021000508 - 07.06.2021 - JT - inicio
  vg_st_aguard_doc_carg = '98'.
*----CS2021000508 - 07.06.2021 - JT - fim

ENDFORM.

*----CS2021000508 - 07.06.2021 - JT - inicio
FORM f_action_user_docs_carguero USING p_saida            TYPE ty_saida
                                       p_tipo_chamada     TYPE char01
                              CHANGING it_saida_romaneios TYPE zde_les_saida_zsdt0001_t
                                       it_tab_bapiret1    TYPE tab_bapiret1.

  DATA: wa_saida_tn  TYPE zde_les_saida_zsdt0001_tn.

  READ TABLE it_saida_romaneios ASSIGNING FIELD-SYMBOL(<fs_out>)
                                WITH KEY ch_referencia = p_saida-ch_referencia.

  MOVE-CORRESPONDING p_saida TO wa_saida_tn.

  CALL FUNCTION 'ZSD_TROCA_NOTA_UPLOAD'
    EXPORTING
      i_zsdt0001                  = wa_saida_tn
    IMPORTING
      e_st_proc                   = <fs_out>-st_proc
      e_docs_enviado_carguero     = <fs_out>-docs_enviado_carguero
    EXCEPTIONS
      erro_upload                 = 1
      erro_upload_nao_autorizado  = 2
      erro_romaneio_nao_trocanota = 3
      erro_aprovacao_carrega      = 4
      OTHERS                      = 5.

  IF     sy-subrc = 0.
    REFRESH: it_saida.
    PERFORM: f_seleciona_dados, " Form seleciona dados
             f_saida, " Form de saida
             f_refresh_alv USING '0100'. "Refresh na tela
  ELSEIF sy-subrc = 1.
    MESSAGE i024(sd) WITH 'Erro UPLOAD arquivos no Carguero!'.
  ELSEIF sy-subrc = 2.
    MESSAGE i024(sd) WITH 'UPLOAD não autorizado no Carguero!'.
  ELSEIF sy-subrc = 3.
    MESSAGE i024(sd) WITH 'Romaneio não é Troca Nota!'.
  ELSEIF sy-subrc = 4.
*   MESSAGE i024(sd) WITH 'Erro na Aprovacao do Carregamento!'.
  ENDIF.

ENDFORM.
*----CS2021000508 - 07.06.2021 - JT - inicio

*-#133089-12.02.2024-JT-inicio
*******************************************************************
* SELECAO FATURAEMENTO AUTOMATICO
* CHAMADA PELA CLASSE ZCL_AUTOMATIZAR_FATURAMENTO
*******************************************************************
FORM f_selecao_fat_autom USING p_ch_ref
                               p_cockpit.

*---------------------------------------------------
*-SET selecao "Pesagem OPUS saida
*---------------------------------------------------
  vg_cockpit           = p_cockpit.
  vg_faturamento_autom = abap_true.

*---------------------------------------------------
*-Romaneios
*---------------------------------------------------
  SELECT *
    FROM zsdt0001
    INTO TABLE it_zsdt0001
   WHERE ch_referencia = p_ch_ref.

  PERFORM f_selecao_generica_rom.
  PERFORM f_get_value_set TABLES t_auart  USING 'MAGGI_ARMAZENAGEM_VA01'.

ENDFORM.

*******************************************************************
* RECUPERAR DADOS PARA FATURAMENTO AUTOMATICO
* CHAMADA PELA CLASSE ZCL_AUTOMATIZAR_FATURAMENTO
*******************************************************************
FORM f_recuperar_dados  CHANGING p_zsdt0001 TYPE ty_zsdt0001
                                 p_saida    TYPE ty_saida
                                 t_saida    TYPE type zde_les_saida_zsdt0001_t.

  READ TABLE it_zsdt0001 INTO p_zsdt0001  INDEX 1.
  READ TABLE it_saida    INTO p_saida     INDEX 1.
  t_saida[] = it_saida[].

ENDFORM.
*-#133089-12.02.2024-JT-fim

FORM f_action_user_transp  USING p_saida TYPE ty_saida p_tipo_chamada TYPE char01
                           CHANGING it_saida_romaneios TYPE zde_les_saida_zsdt0001_t
                                    it_tab_bapiret1    TYPE tab_bapiret1.

  READ TABLE it_saida_romaneios ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  IF ( <fs_out>-transp = icon_execute_object ) OR ( <fs_out>-transp IS INITIAL AND p_tipo_chamada = 'E' ).

    IF p_tipo_chamada EQ 'L'.
      PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia Romaneio
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    UPDATE zsdt0001
       SET st_proc = <fs_out>-st_proc
     WHERE ch_referencia = <fs_out>-ch_referencia.

    PERFORM f_gerar_vt USING 'T' p_tipo_chamada CHANGING wl_erro <fs_out> it_tab_bapiret1.
    CHECK wl_erro EQ 'N'.
    PERFORM f_check_retorno_vt USING 'T' wl_erro CHANGING <fs_out>.

    IF p_tipo_chamada EQ 'L'.
      PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia Romaneio
      PERFORM f_refresh_alv USING '0100'. "Refresh na Tela
    ENDIF.

  ELSEIF <fs_out>-transp NE icon_icon_list AND <fs_out>-transp+0(4) NE '@11@'.
    IF p_tipo_chamada EQ 'L'.
      SET PARAMETER ID 'TNR' FIELD <fs_out>-transp+0(10).
      CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_action_user_dacte USING p_saida TYPE ty_saida.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  CHECK ( <fs_out>-dacte NE icon_icon_list ).

  IF ( <fs_out>-dacte(1) EQ '@' ).

    CLEAR wa_zsdt0001.
    SELECT SINGLE *
      FROM zsdt0001 INTO wa_zsdt0001
     WHERE ch_referencia = <fs_out>-ch_referencia.

    IF wa_zsdt0001-nro_nf_frete GT 0.
      MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wa_zsdt0001-fat_contingencia_ecc EQ abap_true.

      DATA: lva_ok          TYPE  char01,
            lva_msg_retorno TYPE  string.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia  = p_saida-ch_referencia
          i_check_frete_ok = abap_true
        IMPORTING
          e_ok             = lva_ok
          e_msg_retorno    = lva_msg_retorno.

      IF lva_ok = abap_false.
        MESSAGE lva_msg_retorno TYPE 'I'.
        RETURN.
      ENDIF.

    ENDIF.

    IF sy-tcode NE 'ZLES0136' AND sy-tcode NE 'ZMM0127'.
      MESSAGE 'Transação apenas de visualização' TYPE 'I'.
      EXIT.
    ENDIF.

    IF <fs_out>-fatserv = icon_icon_list.
      MESSAGE 'Gerar a Fatura Frete!' TYPE 'I'.
      EXIT.
    ENDIF.

    PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    UPDATE zsdt0001 SET st_proc       = <fs_out>-st_proc
                  WHERE ch_referencia = <fs_out>-ch_referencia.

    REFRESH it_color.
    MOVE 'REMESSA'   TO wa_color-fname.
    MOVE '5'         TO wa_color-color-col.
    MOVE '1'         TO wa_color-color-int.
    MOVE '1'         TO wa_color-color-inv.
    APPEND wa_color TO it_color.
    <fs_out>-color_cell[] = it_color[].

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_out>-fatserv
      IMPORTING
        output = <fs_out>-fatserv.

    SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
      INTO (vl_bukrs,vl_docnum)
      FROM j_1bnflin INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
     WHERE j_1bnflin~refkey = <fs_out>-fatserv.

    CHECK sy-subrc = 0.

    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_docnum.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
    CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-dacte.

    IF <fs_out>-dacte NE icon_complete.
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      MESSAGE 'Dacte ainda não autorizado pela SEFAZ' TYPE 'I'.
      EXIT.
    ENDIF.

    PERFORM f_check_auth_doc USING vl_docnum.

    IF sy-subrc EQ 0.
      CLEAR <fs_out>-icon.

      <fs_out>-dacte   = vl_docnum.
      <fs_out>-st_proc = vg_st_dacte.

      UPDATE zsdt0001 SET nro_nf_frete = <fs_out>-dacte
                          st_proc      = vg_st_dacte
       WHERE ch_referencia = <fs_out>-ch_referencia.

    ELSE.
      <fs_out>-dacte = icon_execute_object.
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    PERFORM f_refresh_alv USING '0100'.

  ELSE.

    SELECT SINGLE bukrs
      FROM j_1bnfdoc INTO vl_bukrs
     WHERE docnum = <fs_out>-dacte.

    CHECK sy-subrc = 0.

    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-dacte.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_bukrs.
    CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.

  ENDIF.

ENDFORM.

FORM f_action_user_danfe USING p_saida TYPE ty_saida.

  FIELD-SYMBOLS <fs_saida> TYPE ty_saida.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  CHECK ( <fs_out>-danfe NE icon_icon_list ).

  IF ( <fs_out>-danfe(1) EQ '@' ).

    CLEAR wa_zsdt0001.

    SELECT SINGLE *
      FROM zsdt0001 INTO wa_zsdt0001
     WHERE ch_referencia = <fs_out>-ch_referencia.

    IF wa_zsdt0001-nro_nf_prod  GT 0.
      MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wa_zsdt0001-fat_contingencia_ecc EQ abap_true.

      DATA: lva_ok          TYPE  char01,
            lva_msg_retorno TYPE  string.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia  = p_saida-ch_referencia
          i_check_danfe_ok = abap_true
        IMPORTING
          e_ok             = lva_ok
          e_msg_retorno    = lva_msg_retorno.

      IF lva_ok = abap_false.
        MESSAGE lva_msg_retorno TYPE 'I'.
        RETURN.
      ENDIF.

    ENDIF.

    IF sy-tcode NE 'ZLES0136' AND sy-tcode NE 'ZMM0127'.
      MESSAGE 'Transação apenas de visualização' TYPE 'I'.
      EXIT.
    ENDIF.
    IF <fs_out>-fatura = icon_execute_object.
      MESSAGE 'Gerar a Fatura!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF <fs_out>-fatura IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_out>-fatura
        IMPORTING
          output = <fs_out>-fatura.

      "Verifica estorno fatura
      SELECT SINGLE vbeln mjahr
         INTO (vl_vbeln,vl_mjahr)
         FROM vbfa
        WHERE vbelv = <fs_out>-fatura "Fatura que encontrou
          AND vbtyp_n  = 'N'. "Estorno
      IF sy-subrc = 0.
        MESSAGE 'O doc de fatura está cancelado. Refazer o lançamento!' TYPE 'I'.
        EXIT.
      ENDIF.

    ENDIF.

    PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    UPDATE zsdt0001 SET st_proc = <fs_out>-st_proc
     WHERE ch_referencia = <fs_out>-ch_referencia.

    REFRESH it_color.
    MOVE 'REMESSA'   TO wa_color-fname.
    MOVE '5'         TO wa_color-color-col.
    MOVE '1'         TO wa_color-color-int.
    MOVE '1'         TO wa_color-color-inv.
    APPEND wa_color TO it_color.
    <fs_out>-color_cell[] = it_color[].

    IF ( <fs_out>-tipo = 'P' ) AND ( <fs_out>-bsart = 'ZARM') . "US #66690 - WPP

      SELECT SINGLE *
        FROM mkpf INTO @DATA(lwa_mkpf_saida)
       WHERE mblnr EQ @wa_zsdt0001-doc_material.

      CHECK ( sy-subrc EQ 0 ) AND ( wa_zsdt0001-doc_material IS NOT INITIAL ).

      CONCATENATE lwa_mkpf_saida-mblnr lwa_mkpf_saida-mjahr INTO vl_refkey.

      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = vl_refkey.

      CHECK sy-subrc EQ 0.

    ELSEIF ( <fs_out>-tipo = 'P' ) OR ( <fs_out>-tipo = 'T' ).

      SELECT SINGLE vbeln mjahr
        INTO (vl_vbeln,vl_mjahr)
        FROM vbfa
        WHERE vbelv = <fs_out>-remessa
        AND vbtyp_n  = 'R'
        AND vbtyp_v  = 'J'.

      CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
      SELECT SINGLE docnum
        FROM j_1bnflin
        INTO vl_docnum
        WHERE refkey = vl_refkey.
    ELSE.
      SELECT SINGLE docnum
        FROM j_1bnflin
        INTO vl_docnum
        WHERE refkey = <fs_out>-fatura.
    ENDIF.

    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_docnum.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD <fs_out>-bukrs.

    CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-danfe.

    IF ( <fs_out>-danfe NE icon_complete ).
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      MESSAGE 'Danfe ainda não autorizado pela SEFAZ' TYPE 'I'.
      EXIT.
    ENDIF.

    PERFORM f_check_auth_doc USING vl_docnum.

    IF sy-subrc EQ 0.
      <fs_out>-danfe   = vl_docnum.
      <fs_out>-st_proc = vg_st_danfe.

      UPDATE zsdt0001 SET nro_nf_prod = <fs_out>-danfe
                          st_proc      = vg_st_danfe
       WHERE ch_referencia = <fs_out>-ch_referencia.
    ELSE.
      <fs_out>-danfe = icon_execute_object.
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    PERFORM f_refresh_alv USING '0100'.


    ""US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
*    CASE vg_cockpit.
*      WHEN '04'.  "Fertilizantes (Porto Velho) - ZLES0115.
*        LOOP AT <fs_out>-romaneios_agr INTO DATA(_wl_rom).
*
*
*          IF <fs_out>-troca_nota            = abap_true AND
*             <fs_out>-docs_enviado_carguero = abap_false.
*            UPDATE zsdt0001 SET nro_nf_prod = <fs_out>-danfe
*                                st_proc     = vg_st_aguard_doc_carg
*             WHERE ch_referencia = _wl_rom-ch_referencia.
*          ELSE.
*            UPDATE zsdt0001 SET nro_nf_prod = <fs_out>-danfe
*                                st_proc     = vg_st_finalizado
*             WHERE ch_referencia = _wl_rom-ch_referencia.
*          ENDIF.
*
*          COMMIT WORK.
*
*          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
*          IF sy-subrc EQ 0.
*            <fs_saida_tmp>-st_proc = vg_st_finalizado.
*            <fs_saida_tmp>-danfe   = <fs_out>-danfe.
*          ENDIF.
*        ENDLOOP.
*    ENDCASE.
    "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

*    "//18.01.2017 - Enio Jesus (Ajuste p/ faturas agrupadas)
*    LOOP AT IT_SAIDA ASSIGNING <FS_SAIDA> WHERE ( FATURA = <FS_OUT>-FATURA )
*                                            AND ( DANFE  = SPACE           ).
*      <FS_SAIDA>-DANFE   = <FS_OUT>-DANFE.
*      <FS_SAIDA>-ST_PROC = VG_ST_DANFE.
*
*      "//Finaliza processo com a DANFE autorizada
*      UPDATE ZSDT0001 SET NRO_NF_PROD = <FS_SAIDA>-DANFE  ST_PROC = <FS_SAIDA>-ST_PROC
*       WHERE CH_REFERENCIA = <FS_SAIDA>-CH_REFERENCIA.
*
*      CLEAR <FS_SAIDA>-ICON.
*    ENDLOOP.
*
*    UPDATE ZSDT0001 SET NRO_NF_PROD = <FS_OUT>-DANFE ST_PROC = VG_ST_DANFE "Danfe
*     WHERE CH_REFERENCIA = <FS_OUT>-CH_REFERENCIA.
*
*    <FS_OUT>-ST_PROC = VG_ST_DANFE.
*    IF NOT LINE_EXISTS( T_FATURA_AGRUPADA[ WERKS = P_BRANCH KUNNR = <FS_OUT>-KUNNR INCO1 = VINCO1 CFOP = <FS_OUT>-CFOP ] ).
*      CASE <FS_OUT>-INCO1. "Validar Regra ###
*        WHEN 'CPT'. "//Gerar CPT logo apos aprovação
*          CLEAR WL_ERRO.
*          PERFORM F_GERAR_VT CHANGING WL_ERRO.
*          PERFORM F_CHECK_RETORNO_VT USING WL_ERRO <FS_OUT>.
*
*        WHEN 'FOB' OR  'CFR'. "//Finaliza processo com a DANFE autorizada
*
*          UPDATE ZSDT0001 SET NRO_NF_PROD = <FS_OUT>-DANFE ST_PROC = VG_ST_FINALIZADO
*           WHERE CH_REFERENCIA = <FS_OUT>-CH_REFERENCIA.
*
*          <FS_OUT>-TRANSP  = ICON_ICON_LIST.
*          <FS_OUT>-DACTE   = ICON_ICON_LIST.
*          <FS_OUT>-ST_PROC = VG_ST_FINALIZADO.
*
*          CLEAR <FS_OUT>-ICON.
*      ENDCASE.
*    ENDIF.

*    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
*    PERFORM f_refresh_alv USING '0100'.

  ELSE.
    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-danfe.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD <fs_out>-bukrs.
    CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.

FORM f_action_user_icon USING p_saida TYPE ty_saida.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  IF <fs_out>-icon = icon_led_red.

    SELECT *
      FROM zlest0100 INTO TABLE ti_zlest0100
     WHERE ch_referencia = <fs_out>-ch_referencia.

    IF ti_zlest0100[] IS NOT INITIAL.
      PERFORM f_montar_layout_log.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat           = estrutura[]
          i_save                = 'A'
          i_screen_start_column = 3
          i_screen_start_line   = 3
          i_screen_end_column   = 100
          i_screen_end_line     = 13
        TABLES
          t_outtab              = ti_zlest0100.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_lock_rom USING p_status
                      p_ch_referencia.

  CASE p_status.
    WHEN 'B'. "Bloqueio

      CALL FUNCTION 'ENQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia  = p_ch_referencia
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    WHEN 'D'. "Desbloqueio

      CALL FUNCTION 'DEQUEUE_EZSDT0001'
        EXPORTING
          ch_referencia = p_ch_referencia.
  ENDCASE.

ENDFORM.

FORM f_check_retorno_vt USING p_tipo
                              p_erro
                     CHANGING p_saida TYPE ty_saida.

  CLEAR: vl_fknum, vl_ov_frete, vl_fatura_frete.

  CHECK p_saida-ch_referencia IS NOT INITIAL.


  IF line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] )
    AND p_tipo NE 'T'.
    EXIT.
  ENDIF.

  IF ( lines( p_saida-romaneios_agr[] ) > 1 ) AND ( p_tipo NE 'T' ).
    EXIT.
  ENDIF.

  IF ( p_erro EQ 'N' OR p_tipo EQ 'T' ) AND ( v_tknum IS NOT INITIAL ).

    p_saida-transp  = v_tknum.
    p_saida-st_proc = vg_st_transp.

    LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom).

      UPDATE zsdt0001 SET doc_transp = p_saida-transp
                          st_proc    = vg_st_transp "Doc transporte
       WHERE ch_referencia = _wl_rom-ch_referencia.

      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
      IF sy-subrc EQ 0.
        <fs_saida_tmp>-transp  = v_tknum.
        <fs_saida_tmp>-st_proc = vg_st_transp.
      ENDIF.
    ENDLOOP.

    DATA(lva_data_mov_romaneio) = p_saida-dt_movimento.

    IF wa_zsdt0001-fat_contingencia_ecc EQ abap_true.
      DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia         = p_saida-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        IMPORTING
          e_dados_faturamento_ecc = lwa_faturamento_ecc.

      IF lwa_faturamento_ecc-data_lcto_cte IS NOT INITIAL.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_cte.
      ELSEIF lwa_faturamento_ecc-data_lcto_nf IS NOT INITIAL.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_nf.
      ELSEIF lwa_faturamento_ecc-data_lcto_nf_rem IS NOT INITIAL.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_nf_rem.
      ENDIF.
    ENDIF.

    PERFORM f_memorizar_dt_movimento_badi USING lva_data_mov_romaneio.

    PERFORM f_set_encerramento_docs CHANGING p_saida.

    "Gerar custo
    IF ( p_saida-inco1 = 'CPT' ) OR ( p_saida-enc_doc_custo EQ abap_true ).

      DATA(_frota_prop) = abap_false.
      IF p_saida-tipo_veiculo = 'P'.
        _frota_prop = abap_true.
      ENDIF.

      SUBMIT zlesr0013 WITH so_tknum = p_saida-transp
                       WITH p_chave  = p_saida-ch_referencia
                       WITH rb_out   = ''
                       WITH rb_cus   = 'X'
                       WITH rb_dtfat = lva_data_mov_romaneio
                       WITH ckrom    = abap_true
                       WITH ckfprop  = _frota_prop
      AND RETURN.

    ELSE.

      DATA(lc_rb_out) = p_saida-emite_conhecimento.

      SUBMIT zlesr0013 WITH so_tknum = p_saida-transp
                       WITH p_chave  = p_saida-ch_referencia
                       WITH rb_dtfat = lva_data_mov_romaneio
                       WITH ckrom    = abap_true
                       WITH rb_out   = lc_rb_out
                       AND RETURN.
    ENDIF.

    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.
    GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
    GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.

    IF vl_fknum IS NOT INITIAL.

      LOOP AT p_saida-romaneios_agr INTO _wl_rom.

        p_saida-doccus  = vl_fknum.
        p_saida-ovserv  = vl_ov_frete.
        p_saida-fatserv = vl_fatura_frete.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = p_saida-fatserv
          IMPORTING
            output = p_saida-fatserv.

        IF vl_fatura_frete IS NOT INITIAL. "Fatura Frete

          UPDATE zsdt0001 SET st_proc      = vg_st_fatura_frete
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
           WHERE ch_referencia = _wl_rom-ch_referencia.

          p_saida-st_proc = vg_st_fatura_frete.

        ELSEIF vl_ov_frete IS NOT INITIAL. "O.V. Frete

          UPDATE zsdt0001 SET st_proc      = vg_st_ov_frete
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
          WHERE ch_referencia = _wl_rom-ch_referencia.

          p_saida-st_proc = vg_st_ov_frete.

        ELSEIF vl_fknum IS NOT INITIAL. "Doc.Custo

          UPDATE zsdt0001 SET st_proc      = vg_st_custo
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
          WHERE ch_referencia = _wl_rom-ch_referencia.
          p_saida-st_proc = vg_st_custo.

        ENDIF.

        IF ( p_saida-inco1 = 'CPT' ) OR ( p_saida-enc_doc_custo EQ abap_true ). " Finaliza processo com a Fatura serviço gerada

*----CS2021000508 - 07.06.2021 - JT - inicio
          IF p_saida-troca_nota            = abap_true AND
             p_saida-docs_enviado_carguero = abap_false.
            UPDATE zsdt0001 SET st_proc = vg_st_aguard_doc_carg " Finalizado
             WHERE ch_referencia = _wl_rom-ch_referencia.

            p_saida-st_proc = vg_st_aguard_doc_carg.
          ELSE.
            UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
             WHERE ch_referencia = _wl_rom-ch_referencia.

            p_saida-st_proc = vg_st_finalizado.
          ENDIF.

*         UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
*          WHERE ch_referencia = _wl_rom-ch_referencia.

          CLEAR p_saida-icon.

          p_saida-dacte   = icon_icon_list.
*         p_saida-st_proc = vg_st_finalizado.
*----CS2021000508 - 07.06.2021 - JT - fim
        ENDIF.

        READ TABLE it_saida ASSIGNING <fs_saida_tmp> WITH KEY ch_referencia = _wl_rom-ch_referencia.
        IF sy-subrc EQ 0.
          <fs_saida_tmp>-doccus    =  p_saida-doccus.
          <fs_saida_tmp>-ovserv    =  p_saida-ovserv.
          <fs_saida_tmp>-fatserv   =  p_saida-fatserv.
          <fs_saida_tmp>-st_proc   =  p_saida-st_proc.
          <fs_saida_tmp>-dacte     =  p_saida-dacte.
          <fs_saida_tmp>-icon      =  p_saida-icon.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ELSE.
    MESSAGE 'Erro ao gerar transporte!' TYPE 'I'.
  ENDIF.

ENDFORM.

FORM f_refresh_alv USING p_screen.

  CHECK vg_faturamento_autom = abap_off. "*-#133089-21.02.2024-JT

  CASE p_screen.
    WHEN '0100'.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.

ENDFORM.

FORM f_action_user_doc_znfw  USING p_saida TYPE ty_saida.

  DATA: v_ematn TYPE ekpo-matnr. "CS2017002682 - 29.11.2017
  DATA: v_matnr18 TYPE matnr18.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  IF <fs_out>-seq_lcto = icon_execute_object.
    CLEAR: wa_zsdt0001, wa_saida-ematn. "CS2017002682 - 29.11.2017

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
     WHERE ch_referencia   EQ @<fs_out>-ch_referencia
       AND loekz           EQ @abap_false
       AND docs_estornados EQ @abap_false.

    IF sy-subrc = 0.
      MESSAGE i000(z01) WITH 'Já existe nota de remessa'
                             _wl_zfiwrt0008-seq_lcto
                             'para este romaneio.'
                             '<ATUALIZAR DOCUMENTOS>'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001
      INTO wa_zsdt0001
     WHERE ch_referencia = <fs_out>-ch_referencia.

    IF wa_zsdt0001-seq_lcto GT 0.
      MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
      EXIT.
    ENDIF.

    IF <fs_out>-netpr LE 0.
      MESSAGE 'Informe o valor unitario da nota de Remessa!' TYPE 'I'.
      EXIT.
    ENDIF.

    IF <fs_out>-lifnr IS INITIAL.

*-CS2021000218-16.11.2022-#90706-JT-inicio
      IF <fs_out>-inco1 <> 'FOB'.
        MESSAGE 'Informar o agente de frete!' TYPE 'I'.
        EXIT.
      ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim

    ELSEIF ( <fs_out>-region IS INITIAL ) AND ( wa_zsdt0001-placa_cav IS NOT INITIAL ) .
      MESSAGE 'Informar a UF da placa cavalo!' TYPE 'I'.
      EXIT.
    ENDIF.

    "Valida placa veiculo - Transporte Romaneio.
    DATA(_placa_com_erro) = abap_false.
    PERFORM f_valida_placas_faturamento USING  <fs_out>
                                       CHANGING _placa_com_erro.
    CHECK _placa_com_erro IS INITIAL.

    PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF <fs_out>-tipo = 'O'.
      IF <fs_out>-ebeln IS INITIAL.
        MESSAGE 'Informe um pedido da ordem!' TYPE 'I'.
        EXIT.
      ELSE.
        READ TABLE it_zsdt0062 INTO wa_zsdt0062 WITH KEY vbeln = <fs_out>-vbeln
                                                         ebeln = <fs_out>-ebeln
                                                         ebelp = wa_saida-ebelp BINARY SEARCH.
        IF sy-subrc NE 0.
          MESSAGE 'Este pedido não pertence a esta Ordem!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.

      "CS2017002682 - 29.11.2017 - Ini
      CLEAR: v_ematn.
      IF <fs_out>-ebelp IS INITIAL.
        MESSAGE 'Informe o item do pedido da ordem!' TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM ekpo INTO @DATA(_wl_ekpo)
       WHERE ebeln = @<fs_out>-ebeln
         AND ebelp = @<fs_out>-ebelp.
      IF ( sy-subrc NE 0 ) OR ( _wl_ekpo-matnr IS INITIAL ).
        MESSAGE 'Pedido de Importação não existe!' TYPE 'I'.
        EXIT.
      ENDIF.

      v_ematn = _wl_ekpo-matnr.

      SELECT SINGLE *
        FROM makt INTO @DATA(_wl_makt)
       WHERE matnr = @v_ematn
         AND spras = @sy-langu.

      IF sy-subrc NE 0.
        MESSAGE |Descrição do Material { _wl_ekpo-matnr } não encontrada!|  TYPE 'I'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = v_ematn
        IMPORTING
          output = v_ematn.

      CONCATENATE v_ematn '-' _wl_makt-maktx INTO  <fs_out>-material.
      <fs_out>-ematn   = v_ematn.
      <fs_out>-lgort_n = wa_zsdt0062-lgort.
      <fs_out>-charg_n = wa_zsdt0062-charg.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_out>-ematn
        IMPORTING
          output = v_matnr18.

      <fs_out>-ematn = v_matnr18.
      "CS2017002682 - 29.11.2017 - Fim

    ELSEIF <fs_out>-tipo = 'T'.
      IF <fs_out>-ebeln IS INITIAL.
        MESSAGE 'Informe um pedido de Importação!' TYPE 'I'.
        EXIT.
      ELSE.
        SELECT SINGLE *
          FROM ekpo
          INTO CORRESPONDING FIELDS OF wa_ekpo
          WHERE ebeln = <fs_out>-ebeln
          AND   matnr = <fs_out>-matnr.
        IF sy-subrc NE 0.
          MESSAGE 'Pedido de Importação não existe!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vl_seq_lcto.
    PERFORM  f_nota_remessa USING <fs_out>
                         CHANGING vl_seq_lcto.
    IF vl_seq_lcto IS NOT INITIAL.
      <fs_out>-seq_lcto = vl_seq_lcto.
      <fs_out>-st_proc = '11'.

      UPDATE zsdt0001 SET st_proc      = '11' " znfw gravado
                          status       = 'X'
                          seq_lcto     = vl_seq_lcto
                          region       = <fs_out>-region
                          agente_frete = <fs_out>-lifnr
                          ebeln        = <fs_out>-ebeln
                          ebelp        = <fs_out>-ebelp    "CS2017002682 - 29.11.2017
             WHERE ch_referencia = <fs_out>-ch_referencia.

      PERFORM f_refresh_alv USING '0100'.
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

  ELSEIF <fs_out>-seq_lcto  IS NOT INITIAL.
    REFRESH: tl_bdc.
    PERFORM f_preencher_dynpro USING:
           'X' 'ZWRR0002'             '0100',
           ' ' 'P_SEQ_LCTO'           <fs_out>-seq_lcto ,
           ' ' 'BDC_OKCODE'           'SEARCH'.

    opt-dismode = 'E'.
    opt-defsize = ' '.
    CALL TRANSACTION 'ZNFW0002' USING tl_bdc OPTIONS FROM opt.
    CLEAR wa_zfiwrt0008.
    SELECT SINGLE *
         FROM zfiwrt0008
         INTO wa_zfiwrt0008
         WHERE seq_lcto = <fs_out>-seq_lcto.
    IF ( sy-subrc EQ 0 ) AND ( wa_zfiwrt0008-loekz = 'X' ).
      wa_zsdt0001-seq_lcto = ''.
      <fs_out>-seq_lcto        = icon_execute_object.

      "CS2017002682 - 29.11.2017 - Ini
      IF <fs_out>-tipo = 'O'.
        UPDATE zsdt0001 SET st_proc      = ''
                            agente_frete = ''
                            seq_lcto     = ''
                            status       = ''
                            ebeln        = ''
                            ebelp        = 00000
        WHERE ch_referencia = <fs_out>-ch_referencia.
      ELSE. "CS2017002682 - 29.11.2017 - Fim
        UPDATE zsdt0001 SET st_proc      = ''
                            agente_frete = ''
                            seq_lcto     = ''
                            status       = ''
        WHERE ch_referencia = <fs_out>-ch_referencia.
      ENDIF.

      <fs_out>-st_proc = ''.
      <fs_out>-lifnr =  ''.
      <fs_out>-netpr  =  0.
      REFRESH style.
      CLEAR: wa_style.
      IF <fs_out>-netpr IS NOT INITIAL.
        wa_style-fieldname = 'NETPR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
      ENDIF.
      CLEAR: wa_style.
      IF <fs_out>-region IS NOT INITIAL.
        wa_style-fieldname = 'REGION'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
      ENDIF.
      CLEAR: wa_style.
      IF <fs_out>-lifnr IS NOT INITIAL.
        wa_style-fieldname = 'LIFNR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
      ENDIF.
      IF <fs_out>-ebeln IS NOT INITIAL.
        wa_style-fieldname = 'EBELN'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .

        "CS2017002682 - 29.11.2017 - Ini
        wa_style-fieldname = 'EBELP'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        "CS2017002682 - 29.11.2017 - Fim
      ENDIF.
      <fs_out>-style[] = style[].

      PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_monta_impostos TABLES tl_impo STRUCTURE tg_impo
                       USING e_row.
  DATA: BEGIN OF wl_1btxic,
          rate TYPE j_1btxic3-rate,
          base TYPE j_1btxic3-base,
        END OF wl_1btxic.
  DATA: wl_itens     LIKE LINE OF tg_itens,
        wl_1baa      TYPE j_1baa,
        wl_base_aux  TYPE j_1btxic3-base,
        wl_a924      TYPE a924,
        wl_konp      TYPE konp,
        wl_t001w     TYPE t001w,
        wl_1btxsdc   TYPE j_1btxsdc,
        wl_1btxpis   TYPE j_1btxpis,
        wl_1btxcof   TYPE j_1btxcof,
        wl_impo_comp LIKE LINE OF tg_impo_comp.

  READ TABLE tg_itens INTO wl_itens INDEX 1. "E_ROW.

  SELECT SINGLE *
    FROM j_1baa
    INTO wl_1baa
     WHERE nftype EQ wl_0001-nftype.

  IF ( wl_1baa-direct EQ '1' ).
    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
    SELECT SINGLE *
      FROM j_1btxsdc
      INTO wl_1btxsdc
       WHERE taxcode EQ tl_0006-taxcode.

    LOOP AT tg_impo.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF sy-subrc EQ 0 AND  wl_0001-complemento EQ 'S'.
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ 'ICM3'.
          IF tl_0006-opertyp EQ 'T'.
            IF wl_1baa-entrad EQ 'X'.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom  = wg_shipfrom
                   AND shipto    = wg_shipto
                   AND gruop    = '30'
                   AND value    = p_parid
                   AND value2    = wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom  = wg_shipfrom
                     AND shipto    = wg_shipto
                     AND gruop    = '40'
                     AND value    = p_parid.

                IF sy-subrc IS NOT INITIAL.
                  IF p_parvw NE 'BR'
                  AND p_parvw NE 'AG'.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom  = wg_shipfrom
                         AND shipto    = wg_shipto
                         AND matnr    = wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom  = wg_shipfrom
                         AND shipto    = wg_shipto.

                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom  = wg_shipfrom
                   AND shipto    = wg_shipto
                   AND gruop    = '76'
                   AND value    = p_parid
                   AND value2    = wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                IF p_parvw NE 'BR'
                AND p_parvw NE 'AG'.
                  SELECT SINGLE rate base
                    FROM j_1btxic2
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom  = wg_shipfrom
                       AND shipto    = wg_shipto
                       AND matnr    = wl_itens-matnr.
                ENDIF.
                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate
                    FROM j_1btxic1
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom = wg_shipfrom
                       AND shipto   = wg_shipto.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE *
              FROM t001w
              INTO wl_t001w
               WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.
              IF ( wl_1baa-direct NE '1' ).

                SELECT SINGLE *
                  FROM a924
                  INTO wl_a924
                   WHERE kschl    EQ 'ZIVP'
                     AND aland    EQ 'BR'
                     AND txreg_sf EQ wl_t001w-regio
                     AND matnr    EQ wl_itens-matnr
                     AND datab    LE sy-datum
                     AND datbi    GE sy-datum.

                IF sy-subrc IS INITIAL.
                  SELECT SINGLE *
                    FROM konp
                    INTO wl_konp
                     WHERE knumh EQ wl_a924-knumh.

                ENDIF.
              ENDIF.
            ENDIF.
            IF wl_1btxic-base IS INITIAL.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            ELSE.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.

            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wl_0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF tl_0006-opertyp EQ 'I'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wl_0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF tl_0006-opertyp EQ 'N'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wl_0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF wl_1btxsdc-pis EQ 'X'
           AND tg_impo-taxtyp EQ 'IPIS'.

          SELECT SINGLE *
            FROM j_1btxpis
            INTO wl_1btxpis
             WHERE country EQ 'BR'
               AND gruop   EQ '72'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wl_0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ 'X'
           AND tg_impo-taxtyp EQ 'ICOF'.
          SELECT SINGLE *
            FROM j_1btxcof
            INTO wl_1btxcof
             WHERE country EQ 'BR'
               AND gruop   EQ '71'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wl_0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF  tg_impo-taxtyp EQ 'ICS1'.
          SELECT SINGLE *
           FROM j_1baa
           INTO wl_1baa
            WHERE itmtyp EQ wl_0001-itmtyp.

          IF wl_1baa-entrad EQ 'X'.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom  = wg_shipfrom
                 AND shipto    = wg_shipto
                 AND gruop    = '30'
                 AND value    = p_parid
                 AND value2    = wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom  = wg_shipfrom
                   AND shipto    = wg_shipto
                   AND gruop    = '40'
                   AND value    = p_parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom  = wg_shipfrom
                     AND shipto    = wg_shipto.

              ENDIF.

            ENDIF.

          ELSE.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom = wg_shipfrom
                 AND shipto   = wg_shipto
                 AND gruop    = '76'
                 AND value    = p_parid
                 AND value2   = wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate
                FROM j_1btxic1
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom = wg_shipfrom
                   AND shipto   = wg_shipto.
            ENDIF.

          ENDIF.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.

          tl_impo-rate =  wl_1btxic-rate .
          IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          ENDIF.
          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
          ENDIF.

          IF wl_0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wl_0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_MONTA_IMPOSTOS

FORM f_monta_contabil .
  DATA: tl_impo_aux LIKE TABLE OF tg_impo WITH HEADER LINE,
        tl_impo     LIKE TABLE OF tg_impo WITH HEADER LINE,
        tg_tbsl     TYPE TABLE OF tbsl WITH HEADER LINE,
        wl_tabix    TYPE sy-tabix.

  REFRESH: tl_impo, tl_impo_aux.
  CLEAR: tl_impo, tl_impo_aux.

  LOOP AT tg_contab.
    MOVE: 0 TO tg_contab-dmbtr.
    MODIFY tg_contab.
  ENDLOOP.

  tg_tbsl[] = tl_tbsl[].
  LOOP AT tg_itens.
    PERFORM f_monta_impostos TABLES tl_impo_aux
                              USING sy-tabix.

    LOOP AT tl_impo_aux.
      MOVE-CORRESPONDING tl_impo_aux TO tl_impo.
      COLLECT tl_impo.
    ENDLOOP.
    REFRESH: tl_impo_aux.
  ENDLOOP.
  LOOP AT tg_contab.
    wl_tabix = sy-tabix.
    READ TABLE tg_tbsl
      WITH KEY bschl = tg_contab-bschl.

    IF tg_contab-taxtyp IS INITIAL.
      IF wl_0001-complemento = 'S'.
        LOOP AT tl_impo
          WHERE taxtyp EQ 'ICM3'.
          IF tg_tbsl-shkzg EQ 'H'.
            SUBTRACT  tl_impo-taxval FROM tg_contab-dmbtr.
          ELSE.
            ADD tl_impo-taxval TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
      IF wl_0001-energia EQ 'N'.
        LOOP AT tg_itens.
          IF tg_tbsl-shkzg EQ 'H'.
            SUBTRACT  tg_itens-netwr FROM tg_contab-dmbtr.
          ELSE.
            ADD tg_itens-netwr TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ELSEIF wl_0001-energia EQ 'S'.
        LOOP AT tl_impo
          WHERE taxtyp EQ 'ICS1'.
          IF tg_tbsl-shkzg EQ 'H'.
            SUBTRACT  tl_impo-base FROM tg_contab-dmbtr.
          ELSE.
            ADD tl_impo-base TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
    ELSE.
      READ TABLE tl_impo
        WITH KEY taxtyp = tg_contab-taxtyp.
      IF sy-subrc IS INITIAL.
        IF tg_tbsl-shkzg EQ 'H'.
          MOVE: tl_impo-taxval TO tg_contab-dmbtr.
          MULTIPLY tg_contab-dmbtr BY -1.
        ELSE.
          MOVE: tl_impo-taxval TO tg_contab-dmbtr.
        ENDIF.
        MODIFY tg_contab INDEX wl_tabix.
      ENDIF.

    ENDIF.

    CLEAR: wl_tabix, tl_impo, tg_tbsl.
  ENDLOOP.
  SORT tg_contab BY taxtyp bschl.
ENDFORM.                    " MONTA_CONTABIL

FORM f_nota_remessa USING p_saida TYPE ty_saida
                 CHANGING p_seq_lcto TYPE zfiwrt0008-seq_lcto.


  DATA: wl_input_0008 TYPE zfiwrt0008,
        tl_input_0009 TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_input_0010 TYPE TABLE OF zfiwrt0010 WITH HEADER LINE,
        tl_input_0011 TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
        tl_input_0012 TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
        tl_input_0013 TYPE TABLE OF zfiwrt0013 WITH HEADER LINE,
        tl_input_0015 TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
        tl_input_0019 TYPE TABLE OF zfiwrt0019 WITH HEADER LINE,
        tl_impo_aux   LIKE TABLE OF tg_impo WITH HEADER LINE,
        wl_0008_aux   TYPE zfiwrt0008,
        wl_mara       TYPE mara,
        wl_marc       TYPE marc,
        wl_cont       TYPE sy-tabix,
        wl_lin        TYPE sy-tabix,
        int_len       TYPE i,
        vg_anzpk(16).

  CLEAR: wl_0019,
         wl_kna1,
         wl_lfa1,
         wl_t001w,
         wl_t001,
         wl_1bbranch,
         wl_1bad,
         wl_1badt,
         wl_1baa,
         "CS2017002682 - 29.11.2017 - Ini
         wl_mara,
         wl_marc,
         "CS2017002682 - 29.11.2017 - Fim
         wl_0008_aux,
         wl_input_0008.

  REFRESH:  tl_0002,
            tl_0003,
            tl_0004,
            tl_0005,
            tl_0006,
            tl_0007,
            tl_1baj,
            tl_1bajt,
            tl_tbsl,
            tl_skat,

* ---> S4 Migration - 17/07/2023 - CA
*            tl_cskb,
* <--- S4 Migration - 17/07/2023 - CA

            tl_user,
            tl_input_0009 ,
            tl_input_0010,
            tl_input_0011,
            tl_input_0012,
            tl_input_0013,
            tl_input_0015,
            tl_input_0019,
            tl_impo_aux,
            tg_mensagems,
            tl_texto,
            it_lines.


  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
   WHERE ch_referencia = @p_saida-ch_referencia.

  CHECK sy-subrc EQ 0.

  "
  IF p_saida-vbeln IS INITIAL.
    SELECT SINGLE operacao lgort_dest lgort_orig
       INTO ( p_operacao, p_lgort_d, p_lgort_o )
      FROM zlest0117
      WHERE bukrs = p_saida-bukrs
      AND   werks = p_saida-branch
      AND   lifnr_c =  wa_saida-ponto_coleta
      AND   tipo  = 'C'.
  ELSE.
    SELECT SINGLE operacao lgort_dest lgort_orig
       INTO ( p_operacao, p_lgort_d, p_lgort_o )
      FROM zlest0117
      WHERE bukrs = p_saida-bukrs
      AND   werks = p_saida-branch
      AND   lifnr_c =  wa_saida-ponto_coleta
      AND   tipo  = 'R'.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE 'Não existe Operação Nota Writer Cadastrado!' TYPE 'I'.
    EXIT.
  ENDIF.

  "Lê parametros operacao
  SELECT SINGLE *
  FROM zfiwrt0001
  INTO wl_0001
   WHERE operacao EQ p_operacao.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Não existe Operação Nota Writer Cadastrado!' TYPE 'I'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_saida-branch
    IMPORTING
      output = p_parid.

  p_parvw   = wl_0001-parvw.

  SELECT SINGLE *
        FROM t001w
        INTO wl_t001w
         WHERE werks EQ p_saida-branch.
  "
  SELECT SINGLE *
    FROM j_1baa
    INTO wl_1baa
     WHERE nftype EQ wl_0001-nftype.

  IF wl_0001-parvw EQ 'AG'.
    SELECT SINGLE *
      FROM kna1
      INTO wl_kna1
       WHERE kunnr EQ p_parid.

  ELSEIF wl_0001-parvw EQ 'BR'
    OR   wl_0001-parvw EQ 'LF'.
    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
       WHERE lifnr EQ p_parid.

  ENDIF.
  SELECT *
    FROM zfiwrt0002
    INTO TABLE tl_0002
     WHERE operacao EQ p_operacao.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM j_1baj
      INTO TABLE tl_1baj
       FOR ALL ENTRIES IN tl_0002
       WHERE taxtyp EQ tl_0002-taxtyp.

    SELECT *
      FROM j_1bajt
      INTO TABLE tl_1bajt
       FOR ALL ENTRIES IN tl_0002
      WHERE  spras  EQ sy-langu
        AND  taxtyp EQ tl_0002-taxtyp.
  ENDIF.

  SELECT *
    FROM zfiwrt0003
    INTO TABLE tl_0003
     WHERE operacao EQ p_operacao.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM tbsl
      INTO TABLE tl_tbsl
       FOR ALL ENTRIES IN tl_0003
       WHERE bschl EQ tl_0003-bschl.

    SELECT *
      FROM skat
      INTO TABLE tl_skat
       FOR ALL ENTRIES IN tl_0003
        WHERE spras EQ sy-langu
          AND ktopl EQ '0050'
          AND saknr EQ tl_0003-hkont.


* ---> S4 Migration - 17/07/2023 - CA
*    SELECT * "Select não é utilizado
*    FROM cskb
*    INTO TABLE tl_cskb
*     FOR ALL ENTRIES IN tl_0003
*      WHERE kstar EQ tl_0003-hkont
*        AND  ( datbi GE sy-datum
*          AND datab LE sy-datum )
*        AND katyp EQ '01'.
* <--- S4 Migration - 17/07/2023 - CA
  ENDIF.
  SELECT *
  FROM zfiwrt0004
  INTO TABLE tl_0004
   WHERE operacao EQ p_operacao.


  REFRESH: tg_movest.
  LOOP AT tl_0004.
    MOVE: tl_0004-bwart   TO tg_movest-bwart,
          tl_0004-tcode   TO tg_movest-tcode,
          tl_0004-mwskz1  TO tg_movest-mwskz1,
          tl_0004-estorno TO tg_movest-estorno.

    APPEND tg_movest.
    CLEAR: tg_movest.
  ENDLOOP.

  SELECT *
    FROM zfiwrt0005
    INTO TABLE tl_0005
     WHERE operacao EQ p_operacao.

  SELECT *
    FROM zfiwrt0006
    INTO TABLE tl_0006
     WHERE operacao EQ p_operacao.

  SELECT *
     FROM zfiwrt0007
     INTO TABLE tl_0007
      WHERE operacao EQ p_operacao
        AND branch   EQ p_saida-branch
        AND tipo     EQ 'W'.

  IF tl_0007[] IS NOT INITIAL.
    SELECT *
      FROM user_addr
      INTO TABLE tl_user
       FOR ALL ENTRIES IN tl_0007
        WHERE bname EQ tl_0007-usnam.

  ENDIF.

  SELECT SINGLE *
    FROM zfiwrt0019
    INTO wl_0019
  WHERE seq_lcto EQ p_seq_lcto.

  CLEAR: wl_cont, wl_lin.
  LOOP AT tl_0005.
    tg_mensagems-seqnum  = tl_0005-seqnum.
    tg_mensagems-linnum  = tl_0005-linnum.
    tg_mensagems-message = tl_0005-message.
    APPEND tg_mensagems.         " msgs q foram parametrizadas na operacao
    ADD 1 TO wl_cont.
  ENDLOOP.

  "texto
  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title = 'Texto da Nota Remessa'
    CHANGING
      ch_text  = tl_texto.

  IF wl_cont IS INITIAL.
    ADD 1 TO wl_cont.
  ENDIF.

  LOOP AT tl_texto INTO wl_texto.
    tg_mensagems-seqnum  = wl_cont.
    tg_mensagems-linnum  = '01'.
    tg_mensagems-message = wl_texto.
    APPEND tg_mensagems.         " msgs q foram inseridas manualmente
    ADD 1 TO wl_cont.
  ENDLOOP.


  IF p_parvw EQ 'AG'.
    IF wl_kna1-regio EQ wl_t001w-regio.
      wl_indcoper = 'D'.
    ELSE.
      wl_indcoper = 'F'.
    ENDIF.
    IF wl_1baa-direct EQ 1.
      MOVE: wl_kna1-regio TO wg_shipfrom.
    ELSE.
      MOVE: wl_kna1-regio TO  wg_shipto.
    ENDIF.
  ELSEIF p_parvw EQ 'BR'
     OR  p_parvw EQ 'LF'.
    IF wl_lfa1-regio EQ wl_t001w-regio.
      wl_indcoper = 'D'.
    ELSE.
      wl_indcoper = 'F'.
    ENDIF.
    IF wl_1baa-direct EQ 1.
      MOVE: wl_lfa1-regio TO wg_shipfrom.
    ELSE.
      MOVE: wl_lfa1-regio TO wg_shipto.
    ENDIF.
  ENDIF.

  IF wl_1baa-direct EQ 1.
    MOVE: wl_t001w-regio TO wg_shipto.
  ELSE.
    MOVE: wl_t001w-regio TO wg_shipfrom.
  ENDIF.

  PERFORM get_next_number IN PROGRAM zwrr0001 USING  'ZSEQ_LCTO'
                                                     '1'
                                            CHANGING p_seq_lcto.

  wl_input_0008-seq_lcto = p_seq_lcto.

  MOVE:  sy-uname TO wl_input_0008-usnam,
         sy-datum TO wl_input_0008-dt_criacao,
         sy-uzeit TO wl_input_0008-hr_criacao.

*  DELETE FROM zfiwrt0008 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
** ZFIWRT0008

  "CS2017002682 - 29.11.2017 - Ini
  DATA(_matnr) = p_saida-matnr.

  IF ( p_saida-tipo = 'O' ) AND ( p_saida-ematn IS NOT INITIAL ).
    _matnr = p_saida-ematn.
  ENDIF.
  "CS2017002682 - 29.11.2017 - Fim

  SELECT SINGLE *
          FROM mara
          INTO wl_mara
            WHERE matnr EQ _matnr. "CS2017002682 - 29.11.2017

  READ TABLE tl_0006
    WITH KEY indcoper = wl_indcoper.


  vg_anzpk = p_saida-peso_liq.
  REPLACE ALL OCCURRENCES OF '.' IN vg_anzpk WITH ' '.
  REPLACE ALL OCCURRENCES OF ',' IN vg_anzpk WITH ' '.
  CONDENSE vg_anzpk NO-GAPS.
  int_len = strlen( vg_anzpk ).
  IF int_len GT 3.
    int_len = int_len - 3.
  ENDIF.
  vg_anzpk = vg_anzpk+0(int_len).

  MOVE : sy-mandt             TO wl_input_0008-mandt,
         p_operacao           TO wl_input_0008-operacao,
         p_saida-bukrs       TO wl_input_0008-bukrs,
         p_saida-branch      TO wl_input_0008-branch,
         p_parvw              TO wl_input_0008-parvw,
         p_parid              TO wl_input_0008-parid,
         wl_0001-nftype       TO wl_input_0008-nftype,
*         P_SAIDA-BRANCH      TO WL_INPUT_0008-MOVE_PLANT,
         p_lgort_d            TO wl_input_0008-move_stloc,
         wl_0001-ctrl_zrfl    TO wl_input_0008-ctrl_zrfl,
         p_saida-nr_romaneio   TO wl_input_0008-nr_romaneio,
         p_saida-ch_referencia TO wl_input_0008-ch_referencia,
         wl_0001-zpesagem     TO wl_input_0008-zpesagem,
         wl_0001-dias         TO wl_input_0008-dias,
         wl_0001-retorno      TO wl_input_0008-retorno,
         wl_0001-energia      TO wl_input_0008-energia,
         wl_0001-servico      TO wl_input_0008-servico,
         wl_0001-complemento  TO wl_input_0008-complemento,
         p_saida-inco1       TO wl_input_0008-inco1,
         p_saida-inco1       TO wl_input_0008-inco2,
         p_saida-ebeln       TO wl_input_0008-ebeln,
         p_saida-ebelp       TO wl_input_0008-ebelp, "CS2017002682 - 29.11.2017
         wl_0001-referencia   TO wl_input_0008-referencia,
         tl_0006-cfop         TO wl_input_0008-cfop,
         tl_0006-taxlw1       TO wl_input_0008-taxlw1,
         tl_0006-taxlw2       TO wl_input_0008-taxlw2,
         tl_0006-taxlw4       TO wl_input_0008-taxlw4,
         tl_0006-taxlw5       TO wl_input_0008-taxlw5,
         tl_0006-opertyp      TO wl_input_0008-opertyp,
         tl_0006-taxcode      TO wl_input_0008-taxcode,
         sy-uname             TO wl_input_0008-usuario_ult_mod,
         sy-datum             TO wl_input_0008-dt_ult_mod,
         sy-uzeit             TO wl_input_0008-hr_ult_mod,
         sy-datum             TO wl_input_0008-budat,
         sy-datum             TO wl_input_0008-bldat,
         wl_input_0008-seq_lcto TO tl_input_0019-seq_lcto,

         p_saida-lifnr       TO tl_input_0019-lifnr,
         p_saida-placa_cav   TO tl_input_0019-placa,
         p_saida-placa_car1   TO tl_input_0019-placa_car1,
         p_saida-placa_car2   TO tl_input_0019-placa_car2,
         p_saida-placa_car3   TO tl_input_0019-placa_car3,
         p_saida-motorista    TO tl_input_0019-motorista,

         vg_anzpk             TO tl_input_0019-anzpk,
         wl_mara-meins        TO tl_input_0019-shpunt,
         p_saida-peso_liq    TO tl_input_0019-ntgew,
         p_saida-peso_fiscal TO tl_input_0019-brgew,
         p_saida-region      TO tl_input_0019-ufplaca.

  IF lwa_zsdt0001-fat_contingencia_ecc EQ abap_true.
    DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
      EXPORTING
        i_ch_referencia         = lwa_zsdt0001-ch_referencia
        i_get_dados_fat_ecc     = abap_true
      IMPORTING
        e_dados_faturamento_ecc = lwa_faturamento_ecc.

    IF lwa_faturamento_ecc-data_lcto_nf_rem IS INITIAL.
      MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
      RETURN.
    ENDIF.

    wl_input_0008-budat = lwa_faturamento_ecc-data_lcto_nf_rem.
    wl_input_0008-bldat = lwa_faturamento_ecc-data_lcto_nf_rem.
  ENDIF.


** ZFIWRT0009
  DELETE FROM zfiwrt0009 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  DELETE FROM zfiwrt0010 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  " Gravar 1 item

  SELECT SINGLE *
      FROM marc
      INTO wl_marc
       WHERE matnr EQ _matnr. "CS2017002682 - 29.11.2017 - Fim

  CLEAR tg_itens.
  REFRESH tg_itens.
  tg_itens-itmnum = 10.
  tg_itens-matnr  = p_saida-matnr.
  tg_itens-maktx  = p_saida-material.
  tg_itens-cfop   = tl_0006-cfop.
  IF wl_mara-xchpf = 'X'.
    tg_itens-charg  = p_saida-nr_safra.
  ENDIF.
  tg_itens-werks  = p_saida-branch.
  tg_itens-lgort  = p_lgort_o.

  "CS2017002682 - 29.11.2017 - Ini
  IF ( p_saida-tipo = 'O' ) AND ( p_saida-ematn IS NOT INITIAL ).
    tg_itens-matnr  = p_saida-ematn.

    IF wl_mara-xchpf = 'X'..
      tg_itens-charg  = p_saida-charg_n.
    ENDIF.
    tg_itens-lgort  = p_saida-lgort_n.
  ENDIF.
  "CS2017002682 - 29.11.2017 - Fim

  tg_itens-menge  = p_saida-peso_liq.
  tg_itens-meins  = wl_mara-meins.
  tg_itens-netpr  = p_saida-netpr.
  tg_itens-netwr  = p_saida-netpr * p_saida-peso_liq.
  tg_itens-steuc =  wl_marc-steuc.
  APPEND tg_itens.
  LOOP AT tg_itens.
    MOVE:sy-mandt               TO tl_input_0009-mandt,
         wl_input_0008-seq_lcto TO tl_input_0009-seq_lcto,
         tg_itens-itmnum        TO tl_input_0009-itmnum,
         tg_itens-matnr         TO tl_input_0009-matnr,
         tg_itens-cfop          TO tl_input_0009-cfop,
         tg_itens-charg         TO tl_input_0009-charg,
         tg_itens-menge         TO tl_input_0009-menge,
         tg_itens-meins         TO tl_input_0009-meins,
         tg_itens-netpr         TO tl_input_0009-netpr,
         tg_itens-netwr         TO tl_input_0009-netwr,
         wl_0001-itmtyp         TO tl_input_0009-itmtyp,
         tg_itens-werks         TO tl_input_0009-bwkey,
         tg_itens-lgort         TO tl_input_0009-lgort,
         tg_itens-anln1         TO tl_input_0009-anln1,
         tg_itens-anln2         TO tl_input_0009-anln2.

    APPEND tl_input_0009.
***    ZFIWRT0010
    READ TABLE tl_0006
     WITH KEY indcoper = wl_indcoper.

    REFRESH: tg_impo.
    LOOP AT tl_0002.
      READ TABLE tl_1baj
        WITH KEY taxtyp = tl_0002-taxtyp.

      READ TABLE tl_1bajt
        WITH KEY taxtyp = tl_0002-taxtyp.

      MOVE: tl_0002-taxtyp   TO tg_impo-taxtyp,
            tl_1bajt-ttypetxt TO tg_impo-ttypetxt,
            tl_1baj-taxgrp  TO tg_impo-taxgrp.

      IF tl_0002-taxtyp EQ 'ICM3'.
        IF tl_0006-opertyp EQ 'T'.

        ELSEIF tl_0006-opertyp EQ 'I'.

        ELSEIF tl_0006-opertyp EQ 'N'.

        ENDIF.
      ELSE.

      ENDIF.

      APPEND tg_impo.
      CLEAR: tg_impo.
    ENDLOOP.

    PERFORM f_monta_impostos TABLES tl_impo_aux
                              USING sy-tabix.
    LOOP AT tl_impo_aux.
      MOVE: sy-mandt               TO tl_input_0010-mandt,
            wl_input_0008-seq_lcto TO tl_input_0010-seq_lcto,
            tl_input_0009-itmnum   TO tl_input_0010-itmnum,
            tl_impo_aux-taxtyp     TO tl_input_0010-taxtyp,
            tl_impo_aux-base       TO tl_input_0010-base,
            tl_impo_aux-rate       TO tl_input_0010-rate,
            tl_impo_aux-taxval     TO tl_input_0010-taxval,
            tl_impo_aux-excbas     TO tl_input_0010-excbas,
            tl_impo_aux-othbas     TO tl_input_0010-othbas.
      APPEND tl_input_0010.
    ENDLOOP.

    CLEAR: tl_input_0009.
  ENDLOOP.
** ZFIWRT0011
  DELETE FROM zfiwrt0011 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  PERFORM f_monta_contabil.
  LOOP AT tg_contab.
    MOVE: sy-mandt                TO tl_input_0011-mandt,
          wl_input_0008-seq_lcto  TO tl_input_0011-seq_lcto,
          tg_contab-bschl         TO tl_input_0011-bschl,
          tg_contab-hkont         TO tl_input_0011-hkont,
          tg_contab-taxtyp        TO tl_input_0011-taxtyp,
          tg_contab-dmbtr         TO tl_input_0011-dmbtr,
          tg_contab-estorno       TO tl_input_0011-estorno,
          tg_contab-zlsch         TO tl_input_0011-zlsch,
          tg_contab-zfbdt         TO tl_input_0011-zfbdt,
          tg_contab-kostl         TO tl_input_0011-kostl,
          tg_contab-umskz         TO tl_input_0011-umskz.

    tl_input_0011-buzei  = sy-tabix.

    APPEND tl_input_0011.
    CLEAR: tl_input_0011.
  ENDLOOP.

** ZFIWRT0012
  DELETE FROM zfiwrt0012 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_movest.
    MOVE: sy-mandt                TO  tl_input_0012-mandt,
          wl_input_0008-seq_lcto  TO  tl_input_0012-seq_lcto,
          tg_movest-bwart         TO  tl_input_0012-bwart,
          tg_movest-tcode         TO  tl_input_0012-tcode,
          tg_movest-mwskz1        TO  tl_input_0012-mwskz1,
          tg_movest-estorno       TO  tl_input_0012-estorno.

    APPEND tl_input_0012.
    CLEAR: tl_input_0012.
  ENDLOOP.

** ZFIWRT0013
  DELETE FROM zfiwrt0013 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_mensagems.
    MOVE: sy-mandt                 TO tl_input_0013-mandt,
          wl_input_0008-seq_lcto   TO tl_input_0013-seq_lcto,
          tg_mensagems-seqnum  TO tl_input_0013-seqnum,
          tg_mensagems-linnum  TO tl_input_0013-linnum,
          tg_mensagems-message TO tl_input_0013-message.

    APPEND tl_input_0013.
    CLEAR: tl_input_0013.
  ENDLOOP.

** ZFIWRT0015
  REFRESH tg_parc.
  tg_parc-parvw = p_parvw.
  tg_parc-parid = p_parid.
  APPEND tg_parc.

  "Adicionar Parceiro WL
  SELECT SINGLE *
    FROM ekpo INTO @DATA(wa_ekpo)
   WHERE ebeln EQ @p_saida-ebeln.

  IF ( sy-subrc EQ 0 ) AND ( p_saida-ebeln IS NOT INITIAL ).
    CLEAR: tg_parc.
    tg_parc-parvw = 'WL'.
    tg_parc-parid = |{ wa_ekpo-werks ALPHA = IN }|.
    APPEND tg_parc.
  ENDIF.

  IF ( p_saida-parid_rom IS NOT INITIAL ).
    CLEAR: tg_parc.
    tg_parc-parvw = 'PC'.
    tg_parc-parid = |{ p_saida-parid_rom ALPHA = IN }|.
    APPEND tg_parc.
  ENDIF.

  IF ( p_saida-id_cli_dest_rom IS NOT INITIAL ).
    CLEAR: tg_parc.
    tg_parc-parvw = 'LR'.
    tg_parc-parid = |{ p_saida-id_cli_dest_rom ALPHA = IN }|.
    APPEND tg_parc.
  ENDIF.

  DELETE FROM zfiwrt0015 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_parc.
    MOVE: sy-mandt                 TO tl_input_0015-mandt,
          wl_input_0008-seq_lcto   TO tl_input_0015-seq_lcto,
          tg_parc-parvw            TO tl_input_0015-parvw,
          tg_parc-parid            TO tl_input_0015-parid.

    APPEND tl_input_0015.
    CLEAR: tl_input_0015.
  ENDLOOP.

  MODIFY zfiwrt0008 FROM wl_input_0008.
  MODIFY zfiwrt0009 FROM TABLE tl_input_0009.
  MODIFY zfiwrt0010 FROM TABLE tl_input_0010.
  MODIFY zfiwrt0011 FROM TABLE tl_input_0011.
  MODIFY zfiwrt0012 FROM TABLE tl_input_0012.
  MODIFY zfiwrt0013 FROM TABLE tl_input_0013.
  MODIFY zfiwrt0015 FROM TABLE tl_input_0015.
  MODIFY zfiwrt0019 FROM tl_input_0019.

  MESSAGE s836(sd) WITH 'Lançamento'
                         wl_input_0008-seq_lcto
                         ', criado com sucesso!'.
ENDFORM.

FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro

FORM f_get_next_number  USING  p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    wg_flag = c_x.
  ENDIF.

ENDFORM.                    " get_next_number

FORM f_action_user_danfe_znfw USING p_saida TYPE ty_saida.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  CHECK ( <fs_out>-danfez NE icon_icon_list ).

  IF ( <fs_out>-danfez(1) EQ '@' ).

    CLEAR wa_zsdt0001.

    SELECT SINGLE *
      FROM zsdt0001 INTO wa_zsdt0001
     WHERE ch_referencia = <fs_out>-ch_referencia.

    IF wa_zsdt0001-nro_nf_rem GT 0.
      MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wa_zsdt0001-fat_contingencia_ecc EQ abap_true.

      DATA: lva_ok          TYPE  char01,
            lva_msg_retorno TYPE  string.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia       = p_saida-ch_referencia
          i_check_danfe_znfw_ok = abap_true
        IMPORTING
          e_ok                  = lva_ok
          e_msg_retorno         = lva_msg_retorno.

      IF lva_ok = abap_false.
        MESSAGE lva_msg_retorno TYPE 'I'.
        RETURN.
      ENDIF.

    ENDIF.

    IF sy-tcode NE 'ZLES0136' AND sy-tcode NE 'ZMM0127'.
      MESSAGE 'Transação apenas de visualização' TYPE 'I'.
      EXIT.
    ENDIF.

    IF <fs_out>-seq_lcto = icon_execute_object.
      MESSAGE 'Gerar a documento NFW!' TYPE 'I'.
      EXIT.
    ENDIF.

    REFRESH: tl_bdc.
    PERFORM f_preencher_dynpro USING:
            'X' 'ZWRR0004'              '0100',
            ' ' 'P_SEQ_LCTO'            <fs_out>-seq_lcto,
            ' ' 'BDC_OKCODE'            'SEARCH'.

    opt-dismode = 'E'.
    opt-defsize = ' '.
    opt-racommit = 'X'.

    CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

    SELECT SINGLE *
      FROM zfiwrt0008 INTO wa_zfiwrt0008
     WHERE seq_lcto = <fs_out>-seq_lcto.

    CHECK ( sy-subrc = 0 ) AND ( wa_zfiwrt0008-docnum IS NOT INITIAL ).

    PERFORM f_check_auth_doc USING wa_zfiwrt0008-docnum.

    IF sy-subrc = 0.
      wa_zsdt0001-nro_nf_rem = wa_zfiwrt0008-docnum.
      <fs_out>-danfez        = wa_zfiwrt0008-docnum.
      UPDATE zsdt0001 SET st_proc    = '12'
                          nro_nf_rem = wa_zfiwrt0008-docnum
      WHERE ch_referencia = <fs_out>-ch_referencia.
      <fs_out>-st_proc = '12'.

      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ELSE.

      PERFORM f_check_canc_doc USING wa_zfiwrt0008-docnum.

      IF sy-subrc = 0.
        wa_zsdt0001-nro_nf_rem = ''.
        <fs_out>-danfez        = icon_execute_object.
        UPDATE zsdt0001 SET st_proc    = '11'
                            nro_nf_rem = ''
        WHERE ch_referencia = <fs_out>-ch_referencia.
        <fs_out>-st_proc = '11'.

        IF wa_zfiwrt0008-docs_estornados EQ abap_true.
          wa_zsdt0001-seq_lcto = ''.
          <fs_out>-seq_lcto        = icon_execute_object.
          UPDATE zsdt0001 SET st_proc    = ''
                              agente_frete = ''
                              seq_lcto   = ''
          WHERE ch_referencia = <fs_out>-ch_referencia.
          <fs_out>-st_proc = ''.
          <fs_out>-lifnr = ''.
          <fs_out>-netpr  =  0.
        ENDIF.

        REFRESH style.
        CLEAR: wa_style.
        IF <fs_out>-netpr IS NOT INITIAL.
          wa_style-fieldname = 'NETPR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        CLEAR: wa_style.
        IF <fs_out>-region IS NOT INITIAL.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        CLEAR: wa_style.
        IF <fs_out>-lifnr IS NOT INITIAL.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        IF <fs_out>-ebeln IS NOT INITIAL.
          wa_style-fieldname = 'EBELN'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          "CS2017002682 - 29.11.2017 - Ini
          wa_style-fieldname = 'EBELP'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
          "CS2017002682 - 29.11.2017 - Fim
        ENDIF.
        <fs_out>-style[] = style[].

        PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
        PERFORM f_refresh_alv USING '0100'. "Refresh na tela
      ELSE.
        MESSAGE 'Danfe ainda não autorizado pela SEFAZ' TYPE 'I'.
      ENDIF.


    ENDIF.

  ELSE.

    IF <fs_out>-st_proc = '12'. "estorno possivel aqui

      REFRESH: tl_bdc.
      PERFORM f_preencher_dynpro USING:
              'X' 'ZWRR0004'              '0100',
              ' ' 'P_SEQ_LCTO'            <fs_out>-seq_lcto,
              ' ' 'BDC_OKCODE'            'SEARCH'.

      opt-dismode = 'E'.
      opt-defsize = ' '.
      opt-racommit = 'X'.

      CALL TRANSACTION 'ZNFW0005' USING tl_bdc OPTIONS FROM opt.

      SELECT SINGLE *
        FROM zfiwrt0008 INTO wa_zfiwrt0008
       WHERE seq_lcto = <fs_out>-seq_lcto.

      CHECK sy-subrc = 0 AND wa_zfiwrt0008-docnum IS NOT INITIAL.

      PERFORM f_check_canc_doc USING wa_zfiwrt0008-docnum.

      IF sy-subrc = 0.
        wa_zsdt0001-nro_nf_rem = ''.
        <fs_out>-danfez        = icon_execute_object.

        UPDATE zsdt0001 SET st_proc    = '11'
                            nro_nf_rem = ''
        WHERE ch_referencia = <fs_out>-ch_referencia.
        <fs_out>-st_proc = '11'.

        IF wa_zfiwrt0008-docs_estornados EQ abap_true.
          wa_zsdt0001-seq_lcto = ''.
          <fs_out>-seq_lcto        = icon_execute_object.
          UPDATE zsdt0001 SET st_proc    = ''
                              agente_frete = ''
                              seq_lcto   = ''
          WHERE ch_referencia = <fs_out>-ch_referencia.
          <fs_out>-st_proc = ''.
          <fs_out>-lifnr   =  ''.
        ENDIF.

        REFRESH style.
        CLEAR: wa_style.
        IF <fs_out>-netpr IS NOT INITIAL.
          wa_style-fieldname = 'NETPR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        CLEAR: wa_style.
        IF <fs_out>-region IS NOT INITIAL.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        CLEAR: wa_style.
        IF <fs_out>-lifnr IS NOT INITIAL.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        IF <fs_out>-ebeln IS NOT INITIAL.
          wa_style-fieldname = 'EBELN'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          "CS2017002682 - 29.11.2017 - Ini
          wa_style-fieldname = 'EBELP'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
          "CS2017002682 - 29.11.2017 - Fim
        ENDIF.
        <fs_out>-style[] = style[].

        PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
        PERFORM f_refresh_alv USING '0100'. "Refresh na tela
      ENDIF.


    ELSE.
      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-danfez.
      SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD <fs_out>-bukrs.
      CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDIF.


ENDFORM.

FORM f_action_user_aviso USING p_saida TYPE ty_saida.

  DATA: lva_docnum_znfw TYPE j_1bdocnum.

  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  IF <fs_out>-aviso = icon_execute_object.

    SELECT SINGLE *
      FROM likp INTO wa_likp
     WHERE berot         = <fs_out>-ch_referencia
       AND vbtyp         = '7'
       AND spe_loekz     = ''.

    IF sy-subrc EQ 0.
      MESSAGE 'Aviso já foi gerado! Click em <ATUALIZAR DOCUMENTOS>' TYPE 'I'.
      EXIT.
    ENDIF.

    CLEAR wa_zsdt0001.
    SELECT SINGLE *
      FROM zsdt0001
      INTO wa_zsdt0001
       WHERE ch_referencia = <fs_out>-ch_referencia.
    IF wa_zsdt0001-doc_aviso GT 0.
      MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
      EXIT.
    ENDIF.
    IF sy-tcode NE 'ZLES0136' AND sy-tcode NE 'ZMM0127'.
      MESSAGE 'Transação apenas de visualização' TYPE 'I'.
      EXIT.
    ENDIF.

    "Verifica se DANFE da ZNFW foi autorizada
    DATA(lva_danfe_aut) = abap_false.

    IF ( <fs_out>-danfez    IS NOT INITIAL ) AND
       ( <fs_out>-danfez(1) NE '@'         ).

      lva_docnum_znfw = <fs_out>-danfez.

      PERFORM f_check_auth_doc USING lva_docnum_znfw.

      IF sy-subrc NE 0.
        lva_danfe_aut = abap_false.
      ELSE.
        lva_danfe_aut = abap_true.
      ENDIF.
    ELSE.
      lva_danfe_aut = abap_false.
    ENDIF.

    IF lva_danfe_aut = abap_false.
      MESSAGE 'DANFE NFW não autorizada!' TYPE 'I'.
      EXIT.
    ENDIF.

    PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM f_gerar_aviso USING <fs_out>.

    IF wg_documento IS NOT INITIAL.
      <fs_out>-aviso   = wg_documento.
      <fs_out>-st_proc = '13'.
      UPDATE zsdt0001 SET st_proc      = '13' " Aviso
                          doc_aviso    = wg_documento
             WHERE ch_referencia = <fs_out>-ch_referencia.

      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

  ELSEIF <fs_out>-aviso NE icon_icon_list.
    SET PARAMETER ID 'VLM'    FIELD <fs_out>-aviso.
    CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDIF.


ENDFORM.

FORM f_gerar_aviso USING p_saida TYPE ty_saida.

  DATA: zcl_aviso_recebimento TYPE REF TO zcl_aviso_recebimento.

  DATA: vl_nf_serie  TYPE string,
        vg_bolnr(16),
        vg_lfimg(16),
        wl_nfe       TYPE c LENGTH 9,
        wl_serie     TYPE c LENGTH 3,
        vl_lifnr     TYPE ekko-lifnr,
        int_len      TYPE i,
        vl_data(10),
        wl_mara      TYPE mara,
        vl_docnum    TYPE j_1bnfdoc-docnum,
        wl_j_1bnfdoc TYPE j_1bnfdoc,
        i_item       TYPE zde_bapi_remessa_item,
        i_parid	     TYPE j_1bparid,
        i_xblnr	     TYPE xblnr_v1,
        v_wl_parid   TYPE j_1bparid,
        l_serie      TYPE c LENGTH 3.

  FREE: zcl_aviso_recebimento.

  CLEAR: wl_erro, wg_documento, vl_nf_serie, vg_bolnr, vg_lfimg, wl_nfe,
         wl_serie, vl_lifnr, int_len, vl_data, wl_mara, vl_docnum,wl_j_1bnfdoc,
         i_item, i_parid, i_xblnr, l_serie.

  IF p_saida-ponto_coleta IS INITIAL OR
     p_saida-local_entrega IS INITIAL OR
     p_saida-lifnr IS INITIAL.
    MESSAGE 'Parceiros(SP/PC/LR) para gerar Aviso de Recebimento, estão incomplemetos!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE docnum
    FROM zfiwrt0008 INTO vl_docnum
   WHERE seq_lcto = p_saida-seq_lcto.

  IF sy-subrc NE 0.
    MESSAGE 'Registro ZNFW não encontrado!' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
   WHERE ch_referencia = @p_saida-ch_referencia.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO wl_j_1bnfdoc
   WHERE docnum =  vl_docnum.

  IF sy-subrc NE 0.
    MESSAGE 'Documento ZNFW não encontrado!' TYPE 'I'.
    RETURN.
  ENDIF.

  IF vg_cockpit EQ '04'.
    IF ( p_saida-tipo = 'O' ) OR
       ( p_saida-tipo = 'T' ).
      "Quando o romaneio for sobre um pedido de transferencia ou ordem de venda, será gerada um documento de remessa.
      "Nesse caso, o frete é emitido sobre a remessa da NF de Venda/Transferencia
      MESSAGE 'Geração Aviso não permitido!' TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  CLEAR: wl_erro, wg_documento.

  IF 1 = 1. "Gerar Aviso Recebimento por Objeto

    "Pedido de Compra
    SELECT SINGLE *
      FROM ekko INTO @DATA(wa_ekko)
     WHERE ebeln EQ @p_saida-ebeln.

    IF sy-subrc NE 0.
      wl_erro = 'X'.
      MESSAGE 'Pedido não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    "Pedido de Compra - Item
    IF p_saida-tipo = 'O'.
      SELECT SINGLE *
        FROM ekpo INTO @DATA(wa_ekpo)
       WHERE ebeln EQ @p_saida-ebeln
         AND ebelp EQ @p_saida-ebelp.
    ELSE.
      SELECT SINGLE *
        FROM ekpo INTO wa_ekpo
       WHERE ebeln EQ p_saida-ebeln
         AND matnr EQ p_saida-matnr.
    ENDIF.

    IF sy-subrc NE 0.
      wl_erro = 'X'.
      MESSAGE 'Item do Pedido não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    "Divisões de Remessas
    SELECT SINGLE *
      FROM eket INTO @DATA(wa_eket)
     WHERE ebeln EQ @wa_ekpo-ebeln
       AND ebelp EQ @wa_ekpo-ebelp.

    "Material
    SELECT SINGLE *
      FROM mara INTO @DATA(_wl_mara)
     WHERE matnr = @wa_ekpo-matnr.

    IF sy-subrc NE 0.
      wl_erro = 'X'.
      MESSAGE 'Material do Pedido não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    CREATE OBJECT zcl_aviso_recebimento.

    zcl_aviso_recebimento->set_fornecedor( i_lifnr = wa_ekko-lifnr ).
    zcl_aviso_recebimento->set_pedido_compra(   i_ebeln = p_saida-ebeln ).
    zcl_aviso_recebimento->set_route(   i_route = p_saida-route ).

    IF lwa_zsdt0001-fat_contingencia_ecc EQ abap_true.

      DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

      CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
        EXPORTING
          i_ch_referencia         = lwa_zsdt0001-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        IMPORTING
          e_dados_faturamento_ecc = lwa_faturamento_ecc.

      IF lwa_faturamento_ecc-data_lcto_nf_rem IS INITIAL.
        MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
        RETURN.
      ENDIF.

      zcl_aviso_recebimento->set_data_lancamento( i_bldat = CONV #( lwa_faturamento_ecc-data_lcto_nf_rem ) ).

    ELSE.

      zcl_aviso_recebimento->set_data_lancamento( i_bldat = sy-datum ).

    ENDIF.





    i_item-ebeln        = wa_ekpo-ebeln.
    i_item-ebelp        = wa_ekpo-ebelp.
    i_item-vgtyp        = 'V'.
    i_item-quantidade   = p_saida-peso_liq.
    i_item-unidade      = wa_ekpo-meins.
    i_item-material     = wa_ekpo-matnr.
    i_item-traty        = '0001'.
    i_item-tragr        = '0001'.
    i_item-ladgr        = '0003'.
    i_item-mfrgr        = '00000001'.
    i_item-kzbew        = 'B'.
    i_item-plant        = wa_ekpo-werks.
    i_item-stge_loc     = wa_ekpo-lgort.

    IF _wl_mara-xchpf = 'X'.
      i_item-batch  = wa_eket-charg.
      i_item-licha  = wa_eket-charg.
    ENDIF.

    zcl_aviso_recebimento->set_item( i_item = i_item ).

    "Ponto Coleta
    zcl_aviso_recebimento->set_lc_coleta_parid(  i_parid = p_saida-ponto_coleta ).
    zcl_aviso_recebimento->set_lc_coleta_partyp( i_partyp = 'V' ).

    "Agente Frete
    zcl_aviso_recebimento->set_sp_frete_parid( i_parid = p_saida-lifnr ).
    zcl_aviso_recebimento->set_sp_frete_partyp( i_partyp = 'V' ).

    "Local Entrega
    zcl_aviso_recebimento->set_lc_entrega_parid( i_parid = p_saida-local_entrega ).
    zcl_aviso_recebimento->set_lc_entrega_partyp( i_partyp = 'V' ).

    "Fornecedor Mercadoria
    v_wl_parid = |{ wa_ekpo-werks ALPHA = IN }|.
    zcl_aviso_recebimento->set_wl_forn_merc_parid( i_parid = v_wl_parid ).
    zcl_aviso_recebimento->set_wl_forn_merc_partyp( i_partyp = 'V' ).

    DATA(_valor_nf) = wl_j_1bnfdoc-nftot.
    zcl_aviso_recebimento->set_valor_nota( i_valor_nota = _valor_nf ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_j_1bnfdoc-nfenum
      IMPORTING
        output = wl_nfe.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_j_1bnfdoc-series
      IMPORTING
        output = wl_serie.

    CONCATENATE wl_nfe wl_serie INTO i_xblnr SEPARATED BY '-'.

    zcl_aviso_recebimento->set_xblnr( i_xblnr = i_xblnr ).
    zcl_aviso_recebimento->set_ch_referencia( i_ch_referencia = p_saida-ch_referencia ).

    DATA(r_gerou) = zcl_aviso_recebimento->criar_aviso_recebimento( i_particao_lote = abap_true ).

    DATA(r_retorno) = zcl_aviso_recebimento->get_retorno( ).

    IF r_gerou EQ abap_true.
      wg_documento =  zcl_aviso_recebimento->get_nr_remessa( ).
    ELSE.
      wl_erro = 'X'.
      PERFORM f_prepare_return2 TABLES r_retorno.
      PERFORM f_grava_log_erro TABLES tg_log_erro USING p_saida.

      MESSAGE 'Houve um erro ao gerar ao aviso.(Ver Logs)' TYPE 'S'.
    ENDIF.

    CLEAR: zcl_aviso_recebimento.

  ELSE. "Gerar Por SHDB

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_j_1bnfdoc-nfenum
      IMPORTING
        output = wl_nfe.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_j_1bnfdoc-series
      IMPORTING
        output = wl_serie.

    CONCATENATE wl_nfe '-' wl_serie INTO vl_nf_serie.
    CONDENSE vl_nf_serie NO-GAPS.

    CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vl_data .

    vg_lfimg = p_saida-peso_liq.
    CONDENSE vg_lfimg NO-GAPS.
    REPLACE ALL OCCURRENCES OF '.' IN vg_lfimg WITH ','.

    vg_bolnr = wl_j_1bnfdoc-nftot.
    CONDENSE vg_bolnr NO-GAPS.

    SELECT SINGLE lifnr
      FROM ekko
      INTO vl_lifnr
     WHERE ebeln = p_saida-ebeln.

    REFRESH ti_bdcdata.
    PERFORM f_bdc_data USING: 'SAPMV50A' '4007' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '/00',
                              ' '        ''     ' '          'LIKP-LIFNR'	        vl_lifnr,
                              ' '        ''     ' '          'LV50C-BSTNR'        p_saida-ebeln,
                              ' '        ''     ' '          'RV50A-LFDAT_LA'     vl_data,
                              ' '        ''     ' '          'RV50A-LFUHR_LA'     '00:00',
                              ' '        ''     ' '          'RV50A-VERUR_LA'     vl_nf_serie.

    PERFORM f_bdc_data USING: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\01'.

    PERFORM f_bdc_data USING: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\02',
                              ' '        ''     ' '          'LIKP-BLDAT'	        vl_data,
                              ' '        ''     ' '          'RV50A-LFDAT_LA'     vl_data,
                              ' '        ''     ' '          'RV50A-LFUHR_LA'     '00:00',
                              ' '        ''     ' '          'LIPSD-G_LFIMG(01)'  vg_lfimg.


    PERFORM f_bdc_data USING: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '/00',
                              ' '        ''     ' '          'LIPS-BRGEW(01)'	    vg_lfimg,
                              ' '        ''     ' '          'LIKP-ROUTE'	        p_saida-route.

    PERFORM f_bdc_data USING: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=HDET_T'.

    PERFORM f_bdc_data USING: 'SAPMV50A' '2000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\07',
                              ' '        ''     ' '          'LIKP-BOLNR'         vg_bolnr.

    PERFORM f_bdc_data USING: 'SAPMV50A' '2000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=SICH_T',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(02)'         'PC',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(03)'         'LR',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(04)'         'SP',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(02)'       p_saida-ponto_coleta,
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(03)'       p_saida-local_entrega,
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(04)'       p_saida-lifnr.

    PERFORM f_call_transaction USING 'VL31N'
                                     p_saida
                            CHANGING wl_erro.
  ENDIF.

  IF wl_erro IS INITIAL.
    COMMIT WORK.
  ELSE.
    EXIT.
  ENDIF.
ENDFORM.

FORM f_action_user_remessa USING p_saida TYPE ty_saida
                                         RAISING zcx_error. "*-#133089-12.02.2024-JT

  DATA: wl_tvro       TYPE tvro,
        shipment_row  TYPE sy-tabix,
        current_row   TYPE sy-tabix,
        v_werks       TYPE ekpo-werks,
        v_lifnr       TYPE ekko-lifnr,
        v_centro_real TYPE zsdt_depara_cen-centro_real.

  DATA: t_route     TYPE TABLE OF vbap-route WITH HEADER LINE,
        t_romaneios TYPE zsdt0001_t,
        v_faturar	  TYPE char01,
        v_mensagem  TYPE char255,
        v_erro      TYPE char1.  "*-CS2021000218-16.11.2022-#90706-JT

  DATA v_charg      TYPE mch1-charg.
  DATA v_matnr      TYPE mara-matnr.
  DATA v_tenta      TYPE i.
  DATA sperr_user   TYPE sy-msgv1.
  DATA v_instr_referencia TYPE zsdt0066-instrucao.
  DATA v_validar_saldo_faturar TYPE char01.
  DATA v_peso_max TYPE  zsdt0045-peso_max.

  FIELD-SYMBOLS <fs_out> TYPE ty_saida.

*-#133089-21.02.2024-JT-inicio
  CREATE OBJECT lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  READ TABLE it_saida ASSIGNING <fs_out>
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  shipment_row = sy-tabix.
  IF ( p_saida-remessa EQ icon_execute_object ).

    CLEAR: v_tenta.
    WHILE shipment_row IS NOT INITIAL.

      READ TABLE it_saida ASSIGNING <fs_out> INDEX shipment_row.

      current_row = sy-tabix.

      CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

      "Valida placa veiculo - Transporte Romaneio.
      DATA(_placa_com_erro) = abap_false.
      PERFORM f_valida_placas_faturamento USING  <fs_out>
                                       CHANGING _placa_com_erro.
      CHECK _placa_com_erro IS INITIAL.

      "VALIDA SE VAI EXIBIR MENSAGEM DE SALDO À FATURAR.
      SELECT COUNT(*)
      FROM vbak  AS bak
      INNER JOIN vbap AS  bap  ON  bap~vbeln = bak~vbeln
      INTO @DATA(valida_saldo_faturar)
      WHERE auart IN ( 'ZRFL' , 'ZRDC', 'ZIND' )
      AND   bak~vbeln = @<fs_out>-vbeln
      AND   bap~matkl = '700140'.

      IF valida_saldo_faturar IS NOT INITIAL.

        SELECT SINGLE instrucao
          FROM zsdt0066
           INTO v_instr_referencia
          WHERE vbeln = <fs_out>-vbeln.


        SELECT SINGLE *
         FROM zsdt0045
          INTO @DATA(wa_zsdt0045)
         WHERE instrucao = @v_instr_referencia.

        IF wa_zsdt0045-limite_peso = 'S' .

          v_peso_max =   wa_zsdt0045-peso_max.

          "TOTAL REMESSA
          SELECT SUM( vbfa~rfmng )
            FROM zsdt0066 AS  zsdt66
            INNER JOIN vbfa AS vbfa ON vbfa~vbelv = zsdt66~vbeln
            INTO @DATA(total_remessa)
            WHERE instrucao = @v_instr_referencia
            AND   zsdt66~vbeln     <> ' '
            AND   vbfa~vbtyp_n = 'J'
            AND   vbfa~vbtyp_v = 'C'.

          DATA(saldo) = v_peso_max - total_remessa.

          IF <fs_out>-peso_liq > saldo .
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE |A instrução { v_instr_referencia } só possui saldo de { saldo } Kg para faturamento. Verifique o limite cadastrado na instrução.| TYPE 'I'.
                EXIT.
              WHEN abap_true.
                DATA(l_mesg) = |A instrução { v_instr_referencia } só possui saldo de { saldo } Kg para faturamento. Verifique o limite cadastrado na instrução.|.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim

          ENDIF.

        ENDIF.
      ENDIF.

      ADD 1 TO v_tenta.

      CLEAR:  wa_zsdt0001.
      SELECT SINGLE *
        FROM zsdt0001
        INTO wa_zsdt0001
       WHERE ch_referencia = <fs_out>-ch_referencia.

      v_charg = wa_zsdt0001-nr_safra.

      IF  <fs_out>-matnr IS NOT INITIAL.
        CALL FUNCTION 'ENQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1      = 'E'
            mandt          = sy-mandt
            matnr          = <fs_out>-matnr
            charg          = v_charg
            _scope         = '2'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        sperr_user     = sy-msgv1.
        IF sy-subrc <> 0.
          IF v_tenta GE 30.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <fs_out>-matnr
              IMPORTING
                output = v_matnr.
            MESSAGE |Material { v_matnr } para o lote { v_charg }, bloqueado por { sperr_user }.| TYPE 'I'.
            EXIT.
          ENDIF.
          WAIT UP TO 1 SECONDS.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1 = 'E'
            mandt     = sy-mandt
            matnr     = <fs_out>-matnr
            charg     = v_charg.
      ENDIF.

      CLEAR: t_romaneios[], v_faturar, v_mensagem, t_route[].
      CALL METHOD zcl_romaneio=>get_ck_faturar
        EXPORTING
          i_ch_referencia_sai = <fs_out>-ch_referencia
        IMPORTING
          e_romaneios         = t_romaneios
          e_faturar           = v_faturar
          e_mensagem          = v_mensagem.

      IF ( v_faturar IS INITIAL ).
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            IF v_mensagem IS NOT INITIAL.
              MESSAGE v_mensagem TYPE 'I'.
            ELSE.
              MESSAGE 'Doc. Remessa não pode ser gerado(Check Agrupamento VT)!' TYPE 'I'.
            ENDIF.
            RETURN.
          WHEN abap_true.
            IF v_mensagem IS NOT INITIAL.
              l_mesg = v_mensagem.
            ELSE.
              l_mesg = 'Doc. Remessa não pode ser gerado(Check Agrupamento VT)!'.
            ENDIF.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      LOOP AT t_romaneios INTO DATA(_wl_rom) WHERE id_cli_dest IS INITIAL.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE |Romaneio: { _wl_rom-nr_romaneio } sem Cliente Destino!| TYPE 'I'.
            RETURN.
          WHEN abap_true.
            l_mesg = |Romaneio: { _wl_rom-nr_romaneio } sem Cliente Destino!|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDLOOP.
*      LOOP AT T_ROMANEIOS INTO DATA(_WL_ROM).
*        READ TABLE IT_ZSDT0001 INTO DATA(_ROM_TEMP) WITH KEY CH_REFERENCIA = _WL_ROM-CH_REFERENCIA.
*        IF SY-SUBRC EQ 0.
*          T_ROUTE = _ROM_TEMP-ROUTE.
*          APPEND T_ROUTE.
*        ENDIF.
*      ENDLOOP.
*
*      SORT T_ROUTE.
*      DELETE ADJACENT DUPLICATES FROM T_ROUTE.
*
*      IF LINES( T_ROUTE[] ) > 1.
*        MESSAGE |Existe mais de um Itinerátio para a Carga! | TYPE 'S'.
*        RETURN.
*      ENDIF.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = |Criando documento de remessa para o romaneio { CONV i( <fs_out>-nr_romaneio ) }.|.

*----CS2021000508 - 07.06.2021 - JT - inicio
*--------------------------------------------------
*-- Valida status / envio aprovacao
*--------------------------------------------------
      TRY .
          zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
            )->valida_envio_aprovacao(
                 EXPORTING
                   i_ch_referencia = wa_zsdt0001-ch_referencia
                 IMPORTING
                   e_erro          = DATA(l_erro)
                   e_msg_erro      = DATA(l_msg_erro)
            ).

        CATCH zcx_integracao.
        CATCH zcx_error.
      ENDTRY.

      IF l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE l_msg_erro TYPE 'I' .
            EXIT.
          WHEN abap_true.
            l_mesg = l_msg_erro.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

      CLEAR: shipment_row.

      "Valida Centro Emissor O.V/Pedido, com centro emissor do Romaneio.
      CLEAR: v_centro_real,v_werks.
      IF <fs_out>-tipo = 'O'.
        SELECT SINGLE werks
          FROM vbap
          INTO v_werks
          WHERE vbeln = wa_zsdt0001-vbeln.
        IF wa_zsdt0001-branch NE v_werks.
          SELECT SINGLE centro_real
            INTO v_centro_real
            FROM zsdt_depara_cen
          WHERE  centrov_1 = v_werks.
          IF ( sy-subrc = 0 ) AND ( wa_zsdt0001-branch NE v_centro_real ).
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE 'Centro emissor do romaneio diferente do centro emissor da OV' TYPE 'I'.
                EXIT.
              WHEN abap_true.
                l_mesg = 'Centro emissor do romaneio diferente do centro emissor da OV'.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.
        ENDIF.
      ELSE.

        SELECT SINGLE *
          FROM ekko INTO @DATA(lwa_pedido_romaneio)
         WHERE ebeln EQ @wa_zsdt0001-vbeln.

        IF ( sy-subrc = 0 ).

          CASE lwa_pedido_romaneio-bsart.
            WHEN 'ZUB'.
              v_werks = lwa_pedido_romaneio-reswk.
            WHEN OTHERS.
              SELECT SINGLE *
                FROM ekpo INTO @DATA(lwa_item_pedido_romaneio)
               WHERE ebeln EQ @wa_zsdt0001-vbeln.

              IF sy-subrc EQ 0.
                v_werks = lwa_item_pedido_romaneio-werks.
              ENDIF.
          ENDCASE.

          IF wa_zsdt0001-branch NE v_werks.
            SELECT SINGLE *
              FROM zsdt_depara_cen INTO @DATA(lwa_zsdt_depara_cen)
             WHERE centrov_1 EQ @v_werks.

            IF NOT ( ( sy-subrc EQ 0 ) AND ( lwa_zsdt_depara_cen-centro_real EQ wa_zsdt0001-branch  ) ).
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE 'Centro emissor do romaneio diferente do centro emissor do pedido.' TYPE 'I'.
                  EXIT.
                WHEN abap_true.
                  l_mesg = 'Centro emissor do romaneio diferente do centro emissor do pedido.'.
                  lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
              ENDCASE.
*-#133089-21.02.2024-JT-fim
            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.

      IF wa_zsdt0001-doc_rem GT 0.
        MESSAGE 'Documento atualizado, click em <ATUALIZAR>' TYPE 'I'.
        EXIT.
      ENDIF.

      IF <fs_out>-inco1 = 'CIF'.
        IF wa_zsdt0001-motorista IS INITIAL .

*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE 'Motorista não informado, reenviar romaneio' TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = 'Motorista não informado, reenviar romaneio'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim

        ELSEIF wa_zsdt0001-placa_cav IS INITIAL.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE 'Placa cavalo não informada, reenviar romaneio' TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = 'Placa cavalo não informada, reenviar romaneio'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.

*---------------------------------------------------------------------------------*
*     Validações por tipo de Cockpit
*---------------------------------------------------------------------------------*
      CASE vg_cockpit. "Bloqueio antes de Gerar Remessa
        WHEN '03'. " OR '09'.
          IF <fs_out>-peso_liq_pos_ret = 0.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE 'Peso liquido pós retenção não calculado!' TYPE 'I'.
                EXIT.
              WHEN abap_true.
                l_mesg = 'Peso liquido pós retenção não calculado!'.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ELSEIF <fs_out>-peso_descarga GT <fs_out>-peso_liq.
*            MESSAGE 'Peso liquido MENOR que peso descarga !' TYPE 'I'.
*            EXIT.
          ENDIF.
        WHEN '04'.
          IF <fs_out>-danfez = icon_execute_object.
            MESSAGE 'Gerar a Nota de Remessa!' TYPE 'I'.
            EXIT.
          ENDIF.

          CASE <fs_out>-inco1.
            WHEN 'CIF' OR 'CPT'.
              IF ( <fs_out>-tipo     = 'P' ) AND
                 ( <fs_out>-aviso(1) = '@' ).
                MESSAGE 'Gerar aviso de recebimento!' TYPE 'I'.
                EXIT.
              ENDIF.
          ENDCASE.
      ENDCASE.

      v_xblnr = <fs_out>-ch_referencia.
      SELECT SINGLE *
        FROM likp
        INTO wa_likp
       WHERE xblnr = v_xblnr
         AND spe_loekz = ''.

      IF sy-subrc = 0.
        MESSAGE |Já existe a remessa { wa_likp-vbeln } para o romaneio { <fs_out>-nr_romaneio }, estorne.| TYPE 'I'.

        <fs_out>-remessa = wa_likp-vbeln.
        <fs_out>-st_proc = vg_st_remessa.

        UPDATE zsdt0001 SET doc_rem = wa_likp-vbeln
                            st_proc = vg_st_remessa
          WHERE ch_referencia = <fs_out>-ch_referencia.

        PERFORM f_refresh_alv USING '0100'. "Refresh na tela

        EXIT.
      ENDIF.

      IF sy-tcode NE 'ZLES0136'  AND sy-tcode NE 'ZMM0127' AND
         vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
        MESSAGE 'Transação apenas de visualização' TYPE 'I'.
        EXIT.
      ENDIF.

*      VINCO1 = <FS_OUT>-INCO1.
      CLEAR vinco1.


      IF ( <fs_out>-lifnr IS INITIAL ) AND
       NOT  line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ) AND
         ( <fs_out>-inco1 NE 'CFR'     AND
           <fs_out>-inco1 NE 'FOB' ).  "*-CS2021000218-16.11.2022-#90706-JT-
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE 'Informar o agente de frete!' TYPE 'I'.
            EXIT.
          WHEN abap_true.
            l_mesg = 'Informar o agente de frete!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF ( <fs_out>-lifnr IS NOT INITIAL ) AND
        line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE 'Informar o agente de frete, somente na criação do documento de transporte!' TYPE 'I'.
            EXIT.
          WHEN abap_true.
            l_mesg = 'Informar o agente de frete, somente na criação do documento de transporte!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF ( wa_zsdt0001-placa_cav IS NOT INITIAL ) AND ( <fs_out>-region IS INITIAL ) AND ( t_fatura_agrupada IS INITIAL ).
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE 'Informar a UF da placa cavalo!' TYPE 'I'.
            EXIT.
          WHEN abap_true.
            l_mesg = 'Informar a UF da placa cavalo!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      IF ( <fs_out>-region IS NOT INITIAL ).
        SELECT SINGLE *
          FROM t005s
          INTO wa_t005s
          WHERE land1 = 'BR'
          AND   bland = <fs_out>-region.
        IF sy-subrc NE 0.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE |UF { <fs_out>-region } é inválida!| TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = |UF { <fs_out>-region } é inválida!|.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.

      "//Validação para campo "relevância para transporte"
      IF ( 'CPT_CIF' CS <fs_out>-inco1  ) AND ( <fs_out>-inco1 IS NOT INITIAL ).

        CLEAR wl_tvro.
        SELECT SINGLE *
          INTO wl_tvro
          FROM tvro
          WHERE route EQ <fs_out>-route.

        IF wl_tvro-tdiix IS INITIAL.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE |Itinerário { <fs_out>-route } sem relevância para transporte. Solicite regularização para à logística.| TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = |Itinerário { <fs_out>-route } sem relevância para transporte. Solicite regularização para à logística.|.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.

      IF ( <fs_out>-lifnr IS NOT INITIAL ).
        SELECT SINGLE lifnr name1 dlgrp
          FROM lfa1
          INTO wa_lfa1
         WHERE lifnr = <fs_out>-lifnr.
      ENDIF.

      IF ( <fs_out>-lifnr IS NOT INITIAL ) AND wa_lfa1-dlgrp NOT IN r_dlgrp AND
         (
            ( <fs_out>-inco1 NE 'FOB' AND <fs_out>-inco1 NE 'CFR' ) OR
            ( <fs_out>-enc_conhecimento EQ abap_true )
         ).
        CLEAR: <fs_out>-lifnr, <fs_out>-region.

        PERFORM f_refresh_alv USING '0100'. "Refresh na tela

        UPDATE zsdt0001 SET agente_frete = ''
                            region       =  ''
         WHERE ch_referencia = <fs_out>-ch_referencia.

*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE |Fornecedor { <fs_out>-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.| TYPE 'I'.
            EXIT.
          WHEN abap_true.
            l_mesg = |Fornecedor { <fs_out>-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

*-CS2021000218-16.11.2022-#90706-JT-inicio
*      PERFORM f_validar_ag_frete  USING <fs_out>-inco1
*                                        <fs_out>-lifnr
*                               CHANGING v_erro
*                                        v_mensagem.
*      IF v_erro = abap_true.
*        MESSAGE v_mensagem TYPE 'I'.
*        EXIT.
*      ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim

      IF ( <fs_out>-tipo = 'O' AND  <fs_out>-operacao+0(4) = 'ZRDC' ). "DCO
        SELECT SINGLE *
          FROM zdco_produtor
          INTO wa_zdco_produtor
         WHERE vbeln       = <fs_out>-vbeln
           AND cd_material = <fs_out>-matnr
           AND cd_centro   = <fs_out>-branch.

        IF sy-subrc NE 0.
          UPDATE zsdt0001 SET agente_frete = '' region =  ''
           WHERE ch_referencia = <fs_out>-ch_referencia.

          PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE 'DCO não cadastrado para essa Ordem de Venda. Contactar o mercado interno' TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = 'DCO não cadastrado para essa Ordem de Venda. Contactar o mercado interno'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.


*-CS2021000218-16.11.2022-#90706-JT-inicio
      IF vg_cockpit = '06'.  "Insumos
        PERFORM f_validar_solicitacao_ra  USING <fs_out>-nro_cg
                                                <fs_out>-ch_referencia
                                       CHANGING <fs_out>
                                                l_erro.
        IF l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE 'Remessa não foi gerada! Verifique o Log deste Romaneio!' TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
      IF vg_cockpit = '01'.  "Pesagem OPUS saida
        PERFORM f_validar_transf_algodao  USING <fs_out>-ch_referencia
                                       CHANGING <fs_out>
                                                l_erro.
        IF l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE 'Remessa não foi gerada! Verifique o Log deste Romaneio!' TYPE 'I'.
              EXIT.
            WHEN abap_true.
              l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.
*-CS2023000189-26.05.2023-#108752-JT-fim

      PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      UPDATE zsdt0001 SET st_proc = <fs_out>-st_proc region = <fs_out>-region
       WHERE ch_referencia = <fs_out>-ch_referencia.

      <fs_out>-line_color = 'C310'.

      REFRESH it_color.
      MOVE 'REMESSA'  TO wa_color-fname.
      MOVE '5'        TO wa_color-color-col.
      MOVE '1'        TO wa_color-color-int.
      MOVE '1'        TO wa_color-color-inv.
      APPEND wa_color TO it_color.

      <fs_out>-color_cell[] = it_color[].


      CLEAR: vl_delivery.

      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD <fs_out>-ch_referencia.
      IF <fs_out>-tipo = 'O'.
*jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj
        IF sy-batch = abap_true.
          SUBMIT zsdi0009_jaime WITH p_vbeln = <fs_out>-vbeln
                          WITH p_lifnr = <fs_out>-lifnr
                          WITH p_peso  = <fs_out>-peso_liq_pos_ret
          AND RETURN.
        ELSE.
          SUBMIT zsdi0009 WITH p_vbeln = <fs_out>-vbeln
                          WITH p_lifnr = <fs_out>-lifnr
                          WITH p_peso  = <fs_out>-peso_liq_pos_ret
          AND RETURN.
        ENDIF.
*jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj

        GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery.

      ELSEIF <fs_out>-tipo = 'P' AND <fs_out>-bsart = 'ZARM'.

        CALL METHOD zcl_remessa_armazenagem=>zif_remessa_armazenagem~gerar_remessa_com_pesagem_opus
          EXPORTING
            i_ch_ref_romaneio = <fs_out>-ch_referencia
            i_lifnr_sp        = <fs_out>-lifnr
          RECEIVING
            r_delivery        = vl_delivery.

      ELSE.
        SUBMIT ztransf WITH p_ebeln = <fs_out>-vbeln
                       WITH p_lifnr = <fs_out>-lifnr
        AND RETURN.

        GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery.

      ENDIF.

      IF ( vl_delivery IS NOT INITIAL ).
        IF ( vl_delivery = '9999999999' ). "Erro de bloqueio no OPUS
          shipment_row = current_row.
          CONTINUE.
        ENDIF.

        <fs_out>-remessa = vl_delivery. " Já grava em ZSDT0001-DOC_REM

        IF ( <fs_out>-tipo = 'P' ) OR ( <fs_out>-tipo = 'T' ).

          SELECT SINGLE vbeln mjahr
            INTO (vl_vbeln,vl_mjahr)
            FROM vbfa
           WHERE vbelv = vl_delivery
             AND vbtyp_n  = 'R'
             AND vbtyp_v  = 'J'.

          <fs_out>-fatura = vl_vbeln.
        ENDIF.

        IF <fs_out>-lifnr IS NOT INITIAL.
          REFRESH: style.
          CLEAR: wa_style.

          wa_style-fieldname = 'LIFNR'.
          wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.

          DELETE <fs_out>-style WHERE fieldname EQ 'LIFNR'.
          INSERT  wa_style INTO TABLE style .
          <fs_out>-style[] = style[].
        ENDIF.

        IF line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
          LOOP AT it_saida INTO it_saida WHERE ( dt_movimento EQ <fs_out>-dt_movimento ) AND
                                               ( matnr        EQ <fs_out>-matnr        ) AND
                                               ( kunnr        EQ <fs_out>-kunnr        ) AND
                                               ( operacao(4)  EQ <fs_out>-operacao(4)  ) AND
*                                             ( INCO1        EQ VINCO1                ) AND
                                               ( cfop         EQ <fs_out>-cfop         ) AND
                                               ( remessa      IS INITIAL               ).

            shipment_row = sy-tabix.
            EXIT.
          ENDLOOP.
        ENDIF.

        "//Grava vinculo DCO
        IF <fs_out>-tipo = 'O' AND  <fs_out>-operacao+0(4) = 'ZRDC'. "DCO
          SUBMIT zsdi0006 WITH p_vbeln = <fs_out>-remessa
                          WITH p_vinc  = 'X'
          AND RETURN.
        ENDIF.

        IF <fs_out>-tipo = 'O'.
          UPDATE zsdt0001 SET st_proc = vg_st_remessa
                              agente_frete = <fs_out>-lifnr
           WHERE ch_referencia = <fs_out>-ch_referencia.

          IF vg_cockpit = '03' OR vg_cockpit = '09' OR vg_cockpit = '10'.
            UPDATE zsdt0001 SET peso_descarga    = <fs_out>-peso_descarga
                                perc_ret         = <fs_out>-perc_ret
                                peso_retido      = <fs_out>-peso_retido
                                peso_liq_pos_ret = <fs_out>-peso_liq_pos_ret
             WHERE ch_referencia = <fs_out>-ch_referencia.
          ENDIF.

          <fs_out>-st_proc = vg_st_remessa.
        ELSE.
          IF ( <fs_out>-inco1 = 'FOB' OR <fs_out>-inco1 = 'CFR' ) AND ( NOT <fs_out>-enc_conhecimento = abap_true ). " Finaliza processo com a Danfe autorizada

*----CS2021000508 - 07.06.2021 - JT - inicio
            IF <fs_out>-troca_nota            = abap_true AND
               <fs_out>-docs_enviado_carguero = abap_false.
              UPDATE zsdt0001 SET st_proc = vg_st_aguard_doc_carg " Finalizado
                                  fatura_prod  = <fs_out>-fatura
                                  agente_frete = <fs_out>-lifnr
              WHERE ch_referencia = <fs_out>-ch_referencia.

              <fs_out>-st_proc = vg_st_aguard_doc_carg.
            ELSE.
              UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
                                  fatura_prod  = <fs_out>-fatura
                                  agente_frete = <fs_out>-lifnr
              WHERE ch_referencia = <fs_out>-ch_referencia.

              <fs_out>-st_proc = vg_st_finalizado.
            ENDIF.

*           UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
*                               fatura_prod  = <fs_out>-fatura
*                               agente_frete = <fs_out>-lifnr
*           WHERE ch_referencia = <fs_out>-ch_referencia.

            CLEAR <fs_out>-icon.
*           <fs_out>-st_proc = vg_st_finalizado.
*----CS2021000508 - 07.06.2021 - JT - fim

          ELSE.
            UPDATE zsdt0001 SET st_proc      = vg_st_fatura " Danfe mudanã para 02 fatura até aprovação sefaz
                                fatura_prod  = <fs_out>-fatura
                                agente_frete = <fs_out>-lifnr
            WHERE ch_referencia = <fs_out>-ch_referencia.

            <fs_out>-st_proc = vg_st_fatura.
          ENDIF.
        ENDIF.

        IF wa_zsdt0001-id_interface = '51'. "Insumos - Fertilizantes
          PERFORM f_item_text_delivery USING vl_delivery.
        ENDIF.

        IF <fs_out>-operacao(4) = 'ZTER'.
          PERFORM f_atrib_vlr_nf_rem USING vl_delivery
                                           wa_zsdt0001-netwr.
        ENDIF.

      ELSE.
        IF vg_cockpit NE '04'.
          CLEAR: <fs_out>-lifnr, <fs_out>-region.

          UPDATE zsdt0001 SET agente_frete = ''
                              region       = ''
           WHERE ch_referencia = <fs_out>-ch_referencia.
        ENDIF.
      ENDIF.

      PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

      IF <fs_out> IS ASSIGNED.
        UNASSIGN <fs_out>.
      ENDIF.

    ENDWHILE.

    PERFORM f_refresh_alv USING '0100'. "Refresh na tela

*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
      WHEN abap_true.
        IF vl_delivery IS INITIAL.
          l_mesg = abap_off.
          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = l_mesg ).
        ENDIF.
    ENDCASE.
*-#133089-21.02.2024-JT-fim

*-IR 069418 - 13.09.2022 - JT - inicio
  ELSEIF p_saida-remessa IS NOT INITIAL AND p_saida-remessa(10) = '@8Y\QLimit'.
    PERFORM f_reinicia_aprovacao USING p_saida.
    PERFORM f_refresh_alv USING '0100'. "Refresh na tela
*-IR 069418 - 13.09.2022 - JT - fim

  ELSEIF p_saida-remessa IS NOT INITIAL AND p_saida-remessa(1) NE '@'.
    IF p_saida-remessa NE icon_icon_list.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          CLEAR shipment_row.
          SET PARAMETER ID 'VL'    FIELD p_saida-remessa+0(10).
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        WHEN abap_true.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.


*-CS2021000218-16.11.2022-#90706-JT-inicio
  ELSEIF p_saida-remessa IS NOT INITIAL AND p_saida-remessa = icon_led_yellow.
    IF vg_cockpit = '06'.  "Insumos
      PERFORM f_validar_solicitacao_ra  USING p_saida-nro_cg
                                              p_saida-ch_referencia
                                     CHANGING p_saida
                                              l_erro.
      IF l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE 'Remessa não foi gerada! Verifique o Log deste Romaneio!' TYPE 'I'.
          WHEN abap_true.
            l_mesg =  'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ELSE.
        MESSAGE 'Romaneio pronto para gerar Remessa!' TYPE 'I'.
      ENDIF.
      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
    IF vg_cockpit = '01'.  "Pesagem OPUS saida
      PERFORM f_validar_transf_algodao  USING p_saida-ch_referencia
                                     CHANGING p_saida
                                              l_erro.
      IF l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE 'Remessa não foi gerada! Verifique o Log deste Romaneio!' TYPE 'I'.
          WHEN abap_true.
            l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ELSE.
        MESSAGE 'Romaneio pronto para gerar Remessa!' TYPE 'I'.
      ENDIF.
      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ENDIF.
*-CS2023000189-26.05.2023-#108752-JT-fim
  ENDIF.

ENDFORM.

**********************************************************************
* Reinicia aprovacao remessa
**********************************************************************
FORM f_reinicia_aprovacao USING p_saida  TYPE ty_saida.

  DATA: l_resp TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-013
      text_question         = TEXT-010
      text_button_1         = TEXT-011
      text_button_2         = TEXT-012
      display_cancel_button = ''
      default_button        = '2'
    IMPORTING
      answer                = l_resp
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF l_resp = '1'.
    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out2>)
                        WITH KEY ch_referencia = p_saida-ch_referencia.

    DELETE FROM zsdt0151 WHERE ch_referencia = p_saida-ch_referencia.

    IF sy-subrc = 0.
      <fs_out2>-remessa = icon_execute_object.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_action_user_fatura  USING p_saida        TYPE ty_saida
                                 p_tipo_chamada TYPE char01
                           CHANGING it_saida_romaneios TYPE zde_les_saida_zsdt0001_t
                                    it_tab_bapiret1    TYPE tab_bapiret1
                           RAISING zcx_error. "*-#133089-12.02.2024-JT

  DATA wa_zsdt0023  TYPE zsdt0023.

*-#133089-21.02.2024-JT-inicio
  DATA: w_zlest0241  TYPE zlest0241.

  SELECT SINGLE *
    FROM zlest0241
    INTO w_zlest0241
   WHERE ch_referencia = p_saida-ch_referencia
     AND cancelado     = abap_off.
*-#133089-21.02.2024-JT-fim

  DATA: lva_action_vf TYPE sy-tcode.

  "FIELD-SYMBOLS <FS_SAIDA> TYPE TY_SAIDA.

  READ TABLE it_saida_romaneios ASSIGNING FIELD-SYMBOL(<fs_out>)
    WITH KEY ch_referencia = p_saida-ch_referencia.

  CHECK ( sy-subrc = 0 ) AND ( <fs_out> IS ASSIGNED ).

  IF ( p_tipo_chamada = 'L' AND <fs_out>-fatura = icon_execute_object ) OR ( p_tipo_chamada = 'E' AND <fs_out>-fatura IS INITIAL ).
    CLEAR wa_zsdt0001.
    SELECT SINGLE *
      FROM zsdt0001
      INTO wa_zsdt0001
     WHERE ch_referencia = <fs_out>-ch_referencia.

    SELECT SINGLE vbeln mjahr
      INTO (vl_vbeln,vl_mjahr)
      FROM vbfa
     WHERE vbelv = <fs_out>-remessa
       AND vbtyp_n  = 'R'
       AND vbtyp_v  = 'J'.
    IF sy-subrc NE 0.
      DATA(tx_msg) = 'Picking não realizado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
        WHEN abap_true.
          DATA(l_mesg) = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ELSE.
      SELECT SINGLE vbeln mjahr
           INTO (vl_vbeln,vl_mjahr)
           FROM vbfa
          WHERE vbelv = <fs_out>-remessa
            AND vbtyp_n  = 'h'
            AND vbtyp_v  = 'J'.
      IF sy-subrc EQ 0.
        tx_msg = 'Picking estornado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
          WHEN abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.
    ENDIF.

    "Checa o Picking
    IF <fs_out>-operacao+0(4) EQ 'ZRDC' OR
       <fs_out>-operacao+0(4) EQ 'ZRFL' OR
       <fs_out>-operacao+0(4) EQ 'ZIND' .
      "MBST (estorno de migo)
      CLEAR wa_zsdt0023.
      SELECT SINGLE *
          FROM zsdt0023
          INTO wa_zsdt0023
          WHERE vbeln   = <fs_out>-remessa.
      IF wa_zsdt0023-mblnr_s IS INITIAL.
        tx_msg = 'Picking não realizado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
          WHEN abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ELSEIF wa_zsdt0023-es_mblnr_s IS NOT INITIAL.
        tx_msg = 'Picking estornado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
          WHEN abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.
    ENDIF.


    IF wa_zsdt0001-fatura_prod GT 0.
      tx_msg = 'Documento atualizado, click em <ATUALIZAR>'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

    IF sy-tcode NE 'ZLES0136'  AND sy-tcode NE 'ZMM0127' AND p_tipo_chamada NE 'E' AND
       vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT-inicio
      tx_msg = 'Transação apenas de visualização'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

    IF <fs_out>-remessa = icon_execute_object.
      tx_msg = 'Gerar a Remessa!'.
      CASE p_tipo_chamada.
        WHEN 'L'.
          MESSAGE tx_msg TYPE 'I'.
        WHEN 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      ENDCASE.
      EXIT.
    ENDIF.

    IF line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
      TRY.
          it_saida = it_saida_romaneios[ dt_movimento = <fs_out>-dt_movimento
                                         matnr        = <fs_out>-matnr
                                         kunnr        = <fs_out>-kunnr
                                         operacao(4)  = <fs_out>-operacao(4)
                                         cfop         = <fs_out>-cfop
                                         remessa      = icon_execute_object ].

          tx_msg = |Para prosseguir com a fatura agrupada é necessário gerar o doc.remessa do romaneio { it_saida-nr_romaneio }.|.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
            WHEN abap_true.
              l_mesg = tx_msg.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          CASE p_tipo_chamada.
            WHEN 'L'.
              MESSAGE tx_msg TYPE 'I' DISPLAY LIKE 'W'.
            WHEN 'E'.
              "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
          ENDCASE.
          EXIT.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
    IF <fs_out>-tipo = 'O' AND <fs_out>-operacao+0(4) = 'ZRDC'. "DCO
      SELECT SINGLE vbeln
        FROM zdco_vinculo
        INTO vl_vbeln
       WHERE vbeln EQ <fs_out>-remessa.

      IF NOT sy-subrc IS INITIAL.
        tx_msg = 'Remessa sem vinculo com DCO.'.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
          WHEN abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.
    ENDIF.

    IF ( ( <fs_out>-inco1 = 'CPT' )  OR
         ( <fs_out>-enc_doc_custo EQ abap_true ) )
      AND NOT line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
      IF <fs_out>-kbetr LE 0.
        tx_msg = 'Não existe valor de frete cadastrado. Solicite à transportadora da sua região'.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
          WHEN abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        CASE p_tipo_chamada.
          WHEN 'L'.
            MESSAGE tx_msg TYPE 'I'.
          WHEN 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        ENDCASE.
        EXIT.
      ENDIF.
    ENDIF.

    CASE p_tipo_chamada.
      WHEN 'L'.
        PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN 'E'.
    ENDCASE.

    UPDATE zsdt0001 SET st_proc = <fs_out>-st_proc
     WHERE ch_referencia = <fs_out>-ch_referencia.

    <fs_out>-line_color = col_yellow_int.
    <fs_out>-color_cell = VALUE #( ( fname = 'FATURA'
                                     color-col = col_positive
                                     color-int = col_heading
                                     color-inv = col_heading )
                                 ).

    IF ( <fs_out>-tipo = 'P' AND <fs_out>-bsart = 'ZARM' ). "US #66690 - WPP

      zcl_remessa_armazenagem=>zif_remessa_armazenagem~gerar_saida_estoque( EXPORTING i_ch_referencia = <fs_out>-ch_referencia
                                                                            IMPORTING e_mblnr         = DATA(_mblnr_gerado) ).

      IF _mblnr_gerado IS NOT INITIAL.

        <fs_out>-fatura  = _mblnr_gerado.
        <fs_out>-st_proc = vg_st_fatura.

        UPDATE zsdt0001 SET fatura_prod = <fs_out>-fatura
                            st_proc     = <fs_out>-st_proc
         WHERE ch_referencia = <fs_out>-ch_referencia.
      ENDIF.

    ELSE. "US #66690 - WPP

      REFRESH: t_billing, it_lines, t_textdatain, t_return, t_success.

      "//Buscar Texto da ordem venda
      wl_ordemt = <fs_out>-vbeln.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = '0002'
          language                = sy-langu
          name                    = wl_ordemt
          object                  = 'VBBK'
        TABLES
          lines                   = it_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      DATA(ck_dados_zsdt0001ft) = abap_false.

*    IF <FS_OUT>-ID_CARGA IS NOT INITIAL.
*      SELECT SINGLE * INTO @DATA(WA_ZSDT0001FT)
*        FROM ZSDT0001FT
*       WHERE ID_CARGA EQ @<FS_OUT>-ID_CARGA.
*
*      IF SY-SUBRC IS INITIAL.
*        CK_DADOS_ZSDT0001FT = ABAP_TRUE.
*
*        CONCATENATE SY-MANDT <FS_OUT>-ID_CARGA INTO WL_ORDEMT.
*        CALL FUNCTION 'READ_TEXT'
*          EXPORTING
*            ID                      = 'ZFAT'
*            LANGUAGE                = SY-LANGU
*            NAME                    = WL_ORDEMT
*            OBJECT                  = 'ZROMSAIDA'
*          TABLES
*            LINES                   = IT_LINES_ZFAT
*          EXCEPTIONS
*            ID                      = 1
*            LANGUAGE                = 2
*            NAME                    = 3
*            NOT_FOUND               = 4
*            OBJECT                  = 5
*            REFERENCE_CHECK         = 6
*            WRONG_ACCESS_TO_ARCHIVE = 7
*            OTHERS                  = 8.
*      ENDIF.
*    ENDIF.

      IF ck_dados_zsdt0001ft EQ abap_false AND sy-batch EQ abap_false.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Informar texto da fatura?'
            text_button_1         = 'Sim'(100)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(101)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

*-CS2023000189-26.05.2023-#108752-JT-inicio
*-------------------------------------------
*------ informacoes de fardos no texto da fatura
*-------------------------------------------
        SELECT *
          INTO TABLE @DATA(t_0330)
          FROM zsdt0330
         WHERE ch_referencia   = @<fs_out>-ch_referencia
           AND status_estorno <> 'D'
           AND cancelado       = @abap_false.
        IF sy-subrc <> 0.
          FREE t_0330.
        ENDIF.

        DATA(t_0330_lote) = t_0330[].

        SORT t_0330_lote BY lgort.
        DELETE ADJACENT DUPLICATES FROM t_0330_lote
                              COMPARING lgort.
*-CS2023000189-26.05.2023-#108752-JT-fim

        IF w_answer = '1'.
          REFRESH tl_texto.

*-CS2023000189-26.05.2023-#108752-JT-inicio
          LOOP AT t_0330_lote INTO DATA(w_0330_lote).
            DATA(l_lines_330) = 0.
            LOOP AT t_0330    INTO DATA(w_0330) WHERE lgort = w_0330_lote-lgort.
              l_lines_330     = l_lines_330 + 1.
            ENDLOOP.
            wl_texto          = | { 'Lote' } { w_0330_lote-lgort } { 'com' } { l_lines_330 } { 'Fardo(s).' } |.
            APPEND wl_texto  TO tl_texto.
          ENDLOOP.
*-CS2023000189-26.05.2023-#108752-JT-fim

          CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              im_title = 'Texto da Fatura'
            CHANGING
              ch_text  = tl_texto.

          LOOP AT tl_texto INTO wl_texto.
            wa_lines-tdformat = '*'.
            wa_lines-tdline+0(72) = wl_texto.
            APPEND wa_lines TO it_lines.
            CLEAR  wa_lines.
          ENDLOOP.
*-CS2023000189-26.05.2023-#108752-JT-inicio
        ELSE.
          LOOP AT t_0330_lote  INTO w_0330_lote.
            l_lines_330           = 0.
            LOOP AT t_0330     INTO w_0330 WHERE lgort = w_0330_lote-lgort.
              l_lines_330         = l_lines_330 + 1.
            ENDLOOP.
            wa_lines-tdformat     = '*'.
            wa_lines-tdline+0(72) = | { 'Lote' } { w_0330_lote-lgort } { 'com' } { l_lines_330 } { 'Fardo(s).' } |.
            APPEND wa_lines      TO it_lines.
            CLEAR  wa_lines.
          ENDLOOP.
*-CS2023000189-26.05.2023-#108752-JT-fim
        ENDIF.
      ELSEIF ck_dados_zsdt0001ft EQ abap_true.
        LOOP AT it_lines_zfat INTO wa_lines.
          APPEND wa_lines TO it_lines.
        ENDLOOP.
      ENDIF.

      "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
      "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
      SELECT SINGLE *
        FROM setleaf
        INTO wa_setleaf
       WHERE setname = 'VF01_USUARIO'
         AND valfrom = sy-uname.

      IF sy-subrc IS INITIAL.
        w_billing-bill_date = <fs_out>-dt_movimento.
      ELSE.
        w_billing-bill_date = sy-datum.
      ENDIF.

      IF wa_zsdt0001-fat_contingencia_ecc EQ abap_true.
        DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

        CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
          EXPORTING
            i_ch_referencia         = wa_zsdt0001-ch_referencia
            i_get_dados_fat_ecc     = abap_true
          IMPORTING
            e_dados_faturamento_ecc = lwa_faturamento_ecc.

        IF lwa_faturamento_ecc-data_lcto_nf IS INITIAL.
          MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
          RETURN.
        ENDIF.

        w_billing-bill_date = lwa_faturamento_ecc-data_lcto_nf.
      ENDIF.

      lva_action_vf = 'CANCEL'.
      EXPORT lva_action_vf TO MEMORY ID 'ZACTION_VF'.

      "//17.01.2017 - EJ (Adiciona faturas agrupadas)
      IF line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
        LOOP AT it_saida_romaneios ASSIGNING FIELD-SYMBOL(<fs_saida>)
                 WHERE ( dt_movimento EQ <fs_out>-dt_movimento ) AND
                       ( matnr        EQ <fs_out>-matnr        ) AND
                       ( kunnr        EQ <fs_out>-kunnr        ) AND
                       ( operacao(4)  EQ <fs_out>-operacao(4)  ) AND
                       ( cfop         EQ <fs_out>-cfop         ) AND
                       ( fatura       IS INITIAL               ).

          UPDATE zsdt0001 SET st_proc = <fs_saida>-st_proc
           WHERE ch_referencia = <fs_saida>-ch_referencia.

          <fs_saida>-line_color = col_yellow_int.
          <fs_saida>-color_cell = VALUE #( ( fname = 'FATURA'
                                             color-col = col_positive
                                             color-int = col_heading
                                             color-inv = col_heading ) ).
          w_billing-ref_doc    = <fs_saida>-remessa.
          w_billing-ref_doc_ca = 'J'.
          APPEND w_billing TO t_billing.
        ENDLOOP.
      ENDIF.

      w_billing-ref_doc    = <fs_out>-remessa.
      w_billing-ref_doc_ca = 'J'.
      APPEND w_billing TO t_billing.

      REFRESH: ti_bdcdata, t_success.
      CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO DATA(_data).
      PERFORM f_bdc_data USING:
          'SAPMV60A'  '0102'  'X'               ''                 '',
          ' '         ''      ' '               'BDC_CURSOR'       'RV60A-FKDAT',
          ' '         ''      ' '               'BDC_OKCODE'       '=SICH',
          ' '         ''      ' '               'RV60A-FKDAT'      _data,
          ' '         ''      ' '               'KOMFK-VBELN(01)'  <fs_out>-remessa.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(_setleaf_vf)
       WHERE setname = 'MAGGI_GER_VFSHDB'.

      IF ( sy-subrc NE 0 ).
        CLEAR: wl_erro.
        PERFORM f_call_transaction USING 'VF01' <fs_out> CHANGING wl_erro.

        IF wl_erro IS INITIAL.
          WAIT UP TO 6 SECONDS.
          CLEAR: vl_vbeln,vl_mjahr.
          IF ( wa_saida-tipo = 'P' ) OR ( wa_saida-tipo = 'T' ).
            SELECT SINGLE a~vbeln a~mjahr
              FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
             WHERE a~vbelv = <fs_out>-remessa
               AND a~vbtyp_n  = 'R'
               AND a~vbtyp_v  = 'J'
              "Estorno
               AND NOT EXISTS ( SELECT * FROM vbfa AS b WHERE b~vbelv = a~vbeln AND b~vbtyp_n = 'N' ).
          ELSE.
            SELECT SINGLE a~vbeln a~mjahr
              FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
             WHERE a~vbelv = <fs_out>-remessa
               AND a~vbtyp_n  = 'M'
               AND a~vbtyp_v  = 'J'
               "Estorno
               AND NOT EXISTS ( SELECT * FROM vbfa AS b WHERE b~vbelv = a~vbeln AND b~vbtyp_n = 'N' ).
          ENDIF.

          IF sy-subrc = 0.
            w_success-ref_doc  = <fs_out>-remessa.
            w_success-bill_doc = vl_vbeln.
            APPEND w_success TO t_success.
          ENDIF.

        ENDIF.
      ELSE.
        CLEAR: it_tab_bapiret1[].

        CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
          TABLES
            billingdatain = t_billing
            textdatain    = t_textdatain
            return        = it_tab_bapiret1
            success       = t_success.
      ENDIF.

      IF t_success[] IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        WAIT UP TO 5 SECONDS.

        LOOP AT t_success INTO w_success.

          TRY.
              ASSIGN it_saida_romaneios[ remessa = w_success-ref_doc ] TO <fs_saida>.
              <fs_saida>-fatura = w_success-bill_doc.
              <fs_saida>-st_proc = vg_st_fatura.

              IF <fs_saida>-operacao(4) = 'ZPAR'.

                UPDATE zsdt0001 SET fatura_prod = <fs_saida>-fatura
                                    st_proc     = '99'
                 WHERE ch_referencia = <fs_saida>-ch_referencia.

              ELSE.

                UPDATE zsdt0001 SET fatura_prod = <fs_saida>-fatura
                                    st_proc     = <fs_saida>-st_proc
                 WHERE ch_referencia = <fs_saida>-ch_referencia.

              ENDIF.

            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ENDLOOP.

        MESSAGE |Remessa gerada sob. nº { w_success-bill_doc }.| TYPE 'S'.

        "//Texto de cabeçalho
        IF ( it_lines[] IS NOT INITIAL ).
          zid               = '0002'.
          zname             = w_success-bill_doc.
          x_header-tdobject = 'VBBK'.
          x_header-tdname   = zname.
          x_header-tdid     = zid.
          x_header-tdspras  = sy-langu.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = x_header
              savemode_direct = 'X'
            TABLES
              lines           = it_lines
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.
        ENDIF.

      ELSE.
        LOOP AT it_tab_bapiret1 INTO DATA(lwa_bapiret).
          MESSAGE ID lwa_bapiret-id TYPE 'I' NUMBER lwa_bapiret-number WITH lwa_bapiret-message_v1 lwa_bapiret-message_v2 lwa_bapiret-message_v3 lwa_bapiret-message_v4.
        ENDLOOP.
      ENDIF.

    ENDIF.

    IF p_tipo_chamada = 'L'.
      PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ENDIF.

  ELSEIF ( <fs_out>-fatura IS NOT INITIAL ) AND ( <fs_out>-fatura NE icon_icon_list ).
    IF p_tipo_chamada EQ 'L'.
      IF <fs_out>-tipo = 'O'.
        SET PARAMETER ID 'VF'    FIELD <fs_out>-fatura.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ELSE.
        SELECT SINGLE vbeln mjahr
          INTO (vl_vbeln,vl_mjahr)
          FROM vbfa
         WHERE vbelv = <fs_out>-remessa
           AND vbtyp_n  = 'R'
           AND vbtyp_v  = 'J'.


* ---> S4 Migration - 19/07/2023 - LO
*        SET PARAMETER ID 'MBN'    FIELD <fs_out>-fatura.
*        SET PARAMETER ID 'MJA'    FIELD vl_mjahr.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ''
            i_skip_first_screen = 'X'
            i_deadend           = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = <fs_out>-fatura
            i_mjahr             = vl_mjahr
          EXCEPTIONS
            illegal_combination = 1
            OTHERS              = 2.
* <--- S4 Migration - 19/07/2023 - LO













      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_prepare_return TABLES p_return  STRUCTURE bapireturn1.

  CLEAR: tg_log_erro[].

  DELETE p_return WHERE type NE 'E'.

  LOOP AT p_return.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-id.
    tg_log_erro-msgnr  = p_return-number.
    tg_log_erro-msgv1  = p_return-message.
    APPEND tg_log_erro.
  ENDLOOP.

ENDFORM.

FORM f_prepare_return2 TABLES p_return STRUCTURE bapiret2.

  CLEAR: tg_log_erro[].

  DELETE p_return WHERE type NE 'E'.

  LOOP AT p_return.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-id.
    tg_log_erro-msgnr  = p_return-number.
    tg_log_erro-msgv1  = p_return-message.
    APPEND tg_log_erro.
  ENDLOOP.

ENDFORM.

FORM f_prepare_return3 TABLES p_return STRUCTURE bdcmsgcoll.

  DATA: vl_message_return TYPE char600.

  CLEAR: tg_log_erro[].

  DELETE p_return WHERE msgtyp NE 'E'.

  LOOP AT p_return.

    MESSAGE ID     p_return-msgid
            TYPE   p_return-msgtyp
            NUMBER p_return-msgnr
            WITH   p_return-msgv1 p_return-msgv2 p_return-msgv3 p_return-msgv4
            INTO   vl_message_return.

    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-msgid.
    tg_log_erro-msgnr  = p_return-msgnr.
    tg_log_erro-msgv1  = vl_message_return.
    APPEND tg_log_erro.

  ENDLOOP.

ENDFORM.

FORM f_grava_log_erro TABLES p_return STRUCTURE tg_log_erro
                       USING p_saida  TYPE ty_saida.

  CLEAR: ti_zlest0100[], vl_ponteiro.

  SELECT MAX( cont )
    FROM zlest0100 INTO vl_ponteiro
   WHERE ch_referencia = p_saida-ch_referencia.

  IF sy-subrc = 0.
    ADD 1 TO vl_ponteiro.
  ELSE.
    vl_ponteiro = 1.
  ENDIF.

  LOOP AT p_return.
    wa_zlest0100-mandt          = sy-mandt.
    wa_zlest0100-ch_referencia  = p_saida-ch_referencia.
    wa_zlest0100-msgtyp         = p_return-msgtyp.
    wa_zlest0100-msgspra        = sy-langu.
    wa_zlest0100-msgid          = p_return-msgid.
    wa_zlest0100-msgnr          = p_return-msgnr.
    wa_zlest0100-msgv1          = p_return-msgv1.
    wa_zlest0100-data           = sy-datum.
    wa_zlest0100-hora           = sy-uzeit.
    wa_zlest0100-usuario        = sy-uname.
    wa_zlest0100-cont           = vl_ponteiro.

    APPEND wa_zlest0100 TO ti_zlest0100.
    ADD 1 TO vl_ponteiro.
  ENDLOOP.

  MODIFY zlest0100 FROM TABLE ti_zlest0100.


ENDFORM.

FORM f_estorno_fatura USING p_fatura LIKE bapivbrksuccess-bill_doc "bapivbrk-ref_doc
                            p_frete  TYPE c
                            p_saida  TYPE ty_saida
                   CHANGING p_erro.

  CLEAR: t_success[], t_return[], is_cancelled, p_erro.

  IF ( p_fatura  IS INITIAL             ) OR
     ( p_fatura  EQ icon_execute_object ) OR
     ( p_fatura  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da fatura não atribuído!'.
    EXIT.
  ENDIF.

  IF p_frete IS NOT INITIAL.
    SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
      INTO (vl_bukrs,vl_docnum)
      FROM j_1bnflin INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
     WHERE j_1bnflin~refkey = p_saida-fatserv.

    SELECT SINGLE * FROM zcte_ciot INTO wa_zcte_ciot WHERE docnum EQ vl_docnum.
    IF ( sy-subrc EQ 0 ).
      IF ( ( wa_zcte_ciot-st_ciot NE 8 ) AND
           ( wa_zcte_ciot-st_ciot NE 0 ) OR
           ( wa_zcte_ciot-st_ciot NE 0 ) AND
           ( wa_zcte_ciot-st_ciot NE 8 ) ) AND
           ( wa_zcte_ciot-st_ciot NE 9 )  AND
           ( wa_zcte_ciot-st_ciot NE 3 ).
        p_erro = 'X'.
        MESSAGE i000(z01) WITH 'Necessário cancelar a viagem. Documento: ' vl_docnum.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  "*---> 19/07/2023 - Migração S4 - LO
  "//Cancela fatura
*  CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*    EXPORTING
*      billingdoc_number       = p_fatura
*    IMPORTING
*      billingdoc_is_cancelled = is_cancelled.

  DATA lwa_bill_detail      TYPE bapivbrkout.
  DATA lwa_return           TYPE bapireturn1.

  CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
    EXPORTING
      billingdocument       = p_fatura
    IMPORTING
      billingdocumentdetail = lwa_bill_detail
      return                = lwa_return.
  is_cancelled = lwa_bill_detail-cancelled.
  "*<--- 19/07/2023 - Migração S4 - LO


  IF ( is_cancelled IS INITIAL ).
    "Cancela fatura
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = p_fatura
      TABLES
        return          = t_return         " bapireturn1 Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
  ENDIF.

  IF ( t_success[] IS NOT INITIAL ) OR ( is_cancelled IS NOT INITIAL ).

    PERFORM f_chk_estorno_fiscal USING p_saida
                                       p_frete
                              CHANGING p_erro.

    IF p_erro IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      WAIT UP TO 5 SECONDS.

      IF p_frete IS NOT INITIAL.

        p_saida-fatserv  = icon_icon_list.
        p_saida-dacte    = icon_execute_object.

        LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom).

          UPDATE zsdt0001 SET st_proc      = vg_st_fatura_frete_before
                              fatura_frete = ''
                              nro_nf_frete = ''
           WHERE ch_referencia = _wl_rom-ch_referencia.

          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
          IF sy-subrc EQ 0.
            <fs_saida_tmp>-fatserv  = icon_icon_list.
          ENDIF.

        ENDLOOP.

      ELSE.
        p_saida-fatura = icon_execute_object.
        p_saida-danfe  = icon_execute_object.

        UPDATE zsdt0001 SET st_proc      = vg_st_fatura_before
                            fatura_prod  = ''
                            nro_nf_prod  = ''
         WHERE ch_referencia = p_saida-ch_referencia.
      ENDIF.

    ELSE.
      p_erro = 'X'.
      EXIT.
    ENDIF.

  ELSE.
    p_erro = 'X'.
    "Gravar Log Erro
    READ TABLE t_return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      PERFORM f_prepare_return TABLES t_return.
      PERFORM f_grava_log_erro TABLES tg_log_erro
                                USING p_saida.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_estorno_picking_rem USING p_vbeln TYPE likp-vbeln
                        CHANGING p_erro
                                 p_saida TYPE ty_saida.

  DATA: fp_budat TYPE sy-datlo,
        fp_tcode TYPE sy-tcode   VALUE 'VL09',
        fp_vbtyp TYPE likp-vbtyp VALUE 'J'.

  DATA: it_mesg     TYPE STANDARD TABLE OF mesg,
        tl_bapiret2 TYPE bapiret2_t.

  CLEAR: p_erro, it_mesg[].

  CHECK p_saida-operacao(4) NE 'ZTER'.

  IF ( p_vbeln  IS INITIAL             ) OR
     ( p_vbeln  EQ icon_execute_object ) OR
     ( p_vbeln  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da remessa não atribuído!'.
    EXIT.
  ENDIF.

  "Verifica se picking já não foi estornado.
  SELECT SINGLE vbeln mjahr
    FROM vbfa INTO (vl_vbeln,vl_mjahr)
   WHERE vbelv = p_vbeln
     AND vbtyp_n  = 'h'
     AND vbtyp_v  = 'J'.

  CHECK sy-subrc NE 0. "Prosseguir se não encontrou estorno mov. mercadoria

  SELECT SINGLE *
    FROM likp INTO @DATA(_wl_likp)
   WHERE vbeln EQ @p_vbeln.

  CHECK ( sy-subrc EQ 0 ) AND ( _wl_likp-wadat_ist IS NOT INITIAL ). "Existe Picking.

  fp_budat = sy-datlo.

  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
    EXPORTING
      i_vbeln                   = p_vbeln
      i_budat                   = fp_budat
      i_tcode                   = fp_tcode
      i_vbtyp                   = fp_vbtyp
    TABLES
      t_mesg                    = it_mesg
    EXCEPTIONS
      error_reverse_goods_issue = 1
      OTHERS                    = 2.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WAIT UP TO 3 SECONDS.

ENDFORM.

FORM f_estorno_remessa USING p_vbeln TYPE likp-vbeln
                    CHANGING p_erro
                             p_saida TYPE ty_saida.

  DATA: it_mesg     TYPE STANDARD TABLE OF mesg,
        sl_hdata    TYPE bapiobdlvhdrchg,
        sl_hcont    TYPE bapiobdlvhdrctrlchg,
        tl_bapiret2 TYPE bapiret2_t.

  CLEAR: p_erro, it_mesg[].

  IF ( p_vbeln  IS INITIAL             ) OR
     ( p_vbeln  EQ icon_execute_object ) OR
     ( p_vbeln  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da remessa não atribuído!'.
    EXIT.
  ENDIF.

  "Deleta Delivery Criado
  sl_hdata-deliv_numb = p_vbeln.
  sl_hcont-deliv_numb = p_vbeln.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = p_vbeln. "Projeto S4 Hana - WPP

  CLEAR: tl_bapiret2[].

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT tl_bapiret2 INTO DATA(wl_bapiret) WHERE type = 'E'.
    p_erro = 'X'.
    MESSAGE ID wl_bapiret-id TYPE 'I' NUMBER wl_bapiret-number WITH wl_bapiret-message_v1 wl_bapiret-message_v2 wl_bapiret-message_v3 wl_bapiret-message_v4.
    RETURN.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  " FAZ O RESTORNO DO RESÍDUO.
  IF p_saida-ch_referencia IS NOT INITIAL .

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(w_romaneio)
    WHERE ch_referencia = @p_saida-ch_referencia.

    IF w_romaneio-id_carga IS INITIAL.

      PERFORM f_estorno_res CHANGING w_romaneio.


    ENDIF.



  ENDIF.


  WAIT UP TO 3 SECONDS.

  IF p_saida-operacao+0(4) EQ 'ZRDC'.
    SUBMIT zsdi0006 WITH p_vbeln = p_vbeln
                    WITH p_vinc  = ''
                    WITH p_desc  = 'X'
                    AND RETURN.
  ENDIF.

ENDFORM.

FORM f_estorno_res  CHANGING p_zsdt0001 TYPE zsdt0001.
  DATA: w_romaneio               TYPE zsdt0001,
        wa_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
        wa_doc_year              TYPE bapi2017_gm_head_02-doc_year,
        wa_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        v_budat                  TYPE mkpf-budat,
        w_mseg                   TYPE mseg.

  DATA: wa_goodsmvt_header TYPE bapi2017_gm_head_01,
        t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
        wa_code            TYPE bapi2017_gm_code,
        vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year,
        t_return_vt        LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  "Estorna entrada de Residuo
  SELECT SINGLE *
    FROM zsdt0001
    INTO w_romaneio
  WHERE ch_referencia = p_zsdt0001-ch_referencia.

  IF w_romaneio-doc_material_e IS NOT INITIAL. "doc. material entrada re  siduo existe
    REFRESH t_return_vt.
    SELECT SINGLE budat INTO v_budat
      FROM mkpf
      WHERE mblnr = w_romaneio-doc_material_e
    AND   mjahr = w_romaneio-ano_material_e.
    "
    wa_mat_doc      = w_romaneio-doc_material_e.
    wa_doc_year    	= w_romaneio-ano_material_e.
    wa_pstng_date   = v_budat.

*    SELECT SINGLE *
*      INTO W_MSEG
*      FROM MSEG
*      WHERE MBLNR = W_ROMANEIO-DOC_MATERIAL_E
*      AND   MJAHR = W_ROMANEIO-ANO_MATERIAL_E
*    AND   BWART = 'ZX1'. "inverte
*
*    IF SY-SUBRC = 0.
*      CLEAR: T_GOODSMVT_ITEM.
*      WA_GOODSMVT_HEADER-PSTNG_DATE = V_BUDAT.
*      WA_GOODSMVT_HEADER-DOC_DATE   = V_BUDAT.
*      WA_GOODSMVT_HEADER-HEADER_TXT = W_ROMANEIO-VBELN.
*
*      WA_CODE-GM_CODE               = '05'.
*
*      WA_GOODSMVT_ITEM-MATERIAL     = W_MSEG-MATNR.
*      WA_GOODSMVT_ITEM-PLANT        = W_MSEG-WERKS.
*      WA_GOODSMVT_ITEM-STGE_LOC      = W_MSEG-LGORT.
*      WA_GOODSMVT_ITEM-BATCH        = W_MSEG-CHARG.
*
*      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'ZX2'. "ALRS 24/05/2017
*      WA_GOODSMVT_ITEM-ENTRY_QNT    = W_MSEG-MENGE.
*      APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*        EXPORTING
*          GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
*          GOODSMVT_CODE    = WA_CODE
*        IMPORTING
*          MATERIALDOCUMENT = VL_MAT_DOC
*          MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
*        TABLES
*          GOODSMVT_ITEM    = T_GOODSMVT_ITEM
*          RETURN           = T_RETURN_VT.
*    ELSE.
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = wa_mat_doc
        matdocumentyear     = wa_doc_year
        goodsmvt_pstng_date = wa_pstng_date
      IMPORTING
        goodsmvt_headret    = vg_invoicedocnumber_migo
      TABLES
        return              = t_return_vt.
*    ENDIF.

    IF t_return_vt[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
      UPDATE zsdt0001
           SET doc_material_e      = ''
               ano_material_e      = ''
         WHERE ch_referencia = p_zsdt0001-ch_referencia.
    ELSE.
      "gravar log
      READ TABLE t_return_vt WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
         FROM zlest0100
         INTO vl_ponteiro
        WHERE ch_referencia = p_zsdt0001-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.
        LOOP AT t_return_vt.
          wa_zlest0100-mandt      = sy-mandt.
          wa_zlest0100-ch_referencia   = p_zsdt0001-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return_vt-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_excluir_remessa USING p_vbeln TYPE likp-vbeln
                                  p_saida TYPE ty_saida
                         CHANGING p_erro.

  CLEAR: p_erro, ti_bdcdata[].

  IF ( p_vbeln  IS INITIAL             ) OR
     ( p_vbeln  EQ icon_execute_object ) OR
     ( p_vbeln  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da remessa não atribuído!'.
    EXIT.
  ENDIF.

  PERFORM f_bdc_data USING:
      'SAPMV50A'  '4004'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'       '/00',
      ''          ''      ''   'LIKP-VBELN'       p_vbeln,

      'SAPMV50A'  '1000'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.

  PERFORM f_call_transaction USING 'VL02N'
                                   p_saida
                          CHANGING p_erro.
  IF p_erro IS INITIAL.
    COMMIT WORK.

    "Elimina vinculo DCO-remessa
    IF p_saida-operacao+0(4) EQ 'ZRDC'.
      SUBMIT zsdi0006 WITH p_vbeln = p_vbeln
                      WITH p_vinc  = ''
                      WITH p_desc  = 'X'
                      AND RETURN.
    ENDIF.

    PERFORM f_after_estorno_remessa CHANGING p_saida.

  ELSE.
    MESSAGE 'Erro ao estornar, remessa sem item! ' TYPE 'I'.
  ENDIF.

ENDFORM.

FORM f_after_estorno_remessa CHANGING p_saida TYPE ty_saida.

  DATA: v_cd_uf TYPE zlest0002-cd_uf.

  CHECK p_saida IS NOT INITIAL.

  CLEAR p_saida-region.

  SELECT SINGLE cd_uf
    FROM zlest0002 INTO v_cd_uf
   WHERE pc_veiculo = p_saida-placa_cav.

  IF sy-subrc = 0.
    p_saida-region  = v_cd_uf.
  ENDIF.

  CASE vg_cockpit.
    WHEN '04'.
      IF p_saida-tipo = 'O' AND p_saida-inco1 = 'FOB'.
        UPDATE zsdt0001 SET st_proc      = vg_st_aviso_rec_before
                            region       = v_cd_uf
                            doc_rem      = ''
                            status       = ''
                            agente_frete = ''
                            konwa        = ''
                            kbetr        = 0
        WHERE ch_referencia = p_saida-ch_referencia.

        IF p_saida-ch_referencia IS NOT INITIAL.
          UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = p_saida-ch_referencia.
        ENDIF.

      ELSE.
        UPDATE zsdt0001 SET st_proc      = vg_st_remessa_before
                            doc_rem      = ''
                            status       = ''
        WHERE ch_referencia = p_saida-ch_referencia.

        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
*        IF ( p_saida-emite_conhecimento = abap_false ) AND ( p_saida-ovserv(1) = '@' ).
*
*          UPDATE zsdt0001 SET st_proc = vg_st_custo
*           WHERE ch_referencia = p_saida-ch_referencia.
*
*        ENDIF.
        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

      ENDIF.

    WHEN '03' OR '09'.
      UPDATE zsdt0001 SET st_proc  = vg_st_remessa_before
                  region           = v_cd_uf
                  doc_rem          = ''
                  status           = ''
                  agente_frete     = ''
                  konwa            = ''
                  kbetr            = 0
                  peso_descarga    = 0
                  perc_ret         = 0
                  peso_retido      = 0
                  peso_liq_pos_ret = 0
       WHERE ch_referencia = p_saida-ch_referencia.

      IF p_saida-ch_referencia IS NOT INITIAL.
        UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = p_saida-ch_referencia.
      ENDIF.

    WHEN OTHERS.
      UPDATE zsdt0001 SET st_proc      = vg_st_remessa_before
                          region       = v_cd_uf
                          doc_rem      = ''
                          status       = ''
                          agente_frete = ''
                          konwa        = ''
                          kbetr        = 0
      WHERE ch_referencia = p_saida-ch_referencia.

      IF p_saida-ch_referencia IS NOT INITIAL.
        UPDATE zlest0155 SET ch_referencia = space WHERE ch_referencia = p_saida-ch_referencia.
      ENDIF.

  ENDCASE.

  p_saida-st_proc = ''.
  p_saida-remessa = icon_execute_object.
  p_saida-lifnr   = ''.

  IF p_saida-operacao+0(3) EQ 'ZUB'. "Pedido de transferencia
    p_saida-fatura  = icon_execute_object.
    p_saida-danfe   = icon_execute_object.

    UPDATE zsdt0001 SET fatura_prod = ''
                        nro_nf_prod = ''
    WHERE ch_referencia EQ p_saida-ch_referencia.

  ENDIF.

  IF line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ).
    TRY.
        it_saida = it_saida[ dt_movimento = p_saida-dt_movimento
                             kunnr        = p_saida-kunnr
                             operacao(4)  = p_saida-operacao(4)
*                           INCO1        = VINCO1
                             cfop         = p_saida-cfop
                             remessa      = icon_execute_object
                           ].
      CATCH cx_sy_itab_line_not_found.
        p_saida-remessa = icon_execute_object.
    ENDTRY.
  ENDIF.


ENDFORM.

FORM f_estorno_migo  USING p_par_est_migo TYPE ty_par_est_migo
                           p_saida        TYPE ty_saida
                  CHANGING p_erro.

  DATA: t_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: wl_invoicedocnumber_migo TYPE bapi2017_gm_head_ret.

  CLEAR: p_erro.

  IF ( p_par_est_migo-mat_doc IS INITIAL ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número do Migo não atribuído!'.
    EXIT.
  ENDIF.

  "BAPI Estorno da MIGO
  CLEAR: t_return[].

  SELECT SINGLE *
    FROM mseg INTO @DATA(lwa_mseg_estorno)
   WHERE smbln EQ @p_par_est_migo-mat_doc.

  IF sy-subrc EQ 0.

    wl_invoicedocnumber_migo-mat_doc   = lwa_mseg_estorno-mblnr.
    wl_invoicedocnumber_migo-doc_year  = lwa_mseg_estorno-mjahr.

  ELSE.
*--> 13/09/20223 - Migração S4 - DA - Início
*    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*      EXPORTING
*        materialdocument    = p_par_est_migo-mat_doc
*        matdocumentyear     = p_par_est_migo-doc_year
*        goodsmvt_pstng_date = p_par_est_migo-pstng_date
*      IMPORTING
*        goodsmvt_headret    = wl_invoicedocnumber_migo
*      TABLES
*        return              = t_return.
*
*    IF t_return[] IS INITIAL.
    CALL FUNCTION 'ZSD_GOODSMVT_CANCEL'
      STARTING NEW TASK p_par_est_migo-mat_doc
      PERFORMING receive_results_goodsmvt ON END OF TASK
      EXPORTING
        materialdocument    = p_par_est_migo-mat_doc
        matdocumentyear     = p_par_est_migo-doc_year
        goodsmvt_pstng_date = p_par_est_migo-pstng_date
      TABLES
        return              = t_return.

    WAIT UNTIL results_received = 'X' UP TO 60 SECONDS.

    t_return[] = t_return_goods_cnc[].
    wl_invoicedocnumber_migo = g_migo_number.

    FREE: t_return_goods_cnc[], g_migo_number, results_received.

    IF wl_invoicedocnumber_migo-mat_doc IS NOT INITIAL.
*<-- 13/09/20223 - Migração S4 - DA - Fim
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
    ELSE.
      p_erro = 'X'.

      "Gravar Log erro
      READ TABLE t_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        PERFORM f_prepare_return TABLES t_return.
        PERFORM f_grava_log_erro TABLES tg_log_erro USING p_saida.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

  CHECK p_erro IS INITIAL.

  "Tramento Para Remessa Formação de Lote
  IF p_par_est_migo-form_lote IS NOT INITIAL.

    CASE p_par_est_migo-ent_sai.
      WHEN 'E'. "Entrada
        UPDATE zsdt0023 SET es_mblnr_e = wl_invoicedocnumber_migo-mat_doc
                            es_mjahr_e = wl_invoicedocnumber_migo-doc_year
         WHERE vbeln = p_saida-remessa.
      WHEN 'S'. "Saída
        UPDATE zsdt0023 SET es_mblnr_s = wl_invoicedocnumber_migo-mat_doc
                            es_mjahr_s = wl_invoicedocnumber_migo-doc_year
         WHERE vbeln = p_saida-remessa.
    ENDCASE.

  ENDIF.

ENDFORM.

FORM receive_results_goodsmvt USING i_taskname.

  RECEIVE RESULTS FROM FUNCTION 'ZSD_GOODSMVT_CANCEL'
    IMPORTING
      goodsmvt_headret    = g_migo_number
    TABLES
      return              = t_return_goods_cnc.

  results_received = abap_true.


ENDFORM.








FORM f_estorno_ov_frete USING p_ovserv TYPE vbeln_va
                              p_saida  TYPE ty_saida
                     CHANGING p_erro.
  CLEAR: p_erro.

  IF ( p_saida-ovserv  IS INITIAL             ) OR
     ( p_saida-ovserv  EQ icon_execute_object ) OR
     ( p_saida-ovserv  EQ icon_icon_list      ).
    p_erro = 'X'.
    MESSAGE w000(z01) WITH 'Número da OV. de Frete não atribuído!'.
    EXIT.
  ENDIF.

  "Bloqueia Ordem do frete
  CLEAR: wl_orderheaderin, wl_orderheaderinx, tl_bapiparex[], sl_bapiparex, wl_bape_vbak.

  wl_bape_vbak-vbeln           = p_saida-ovserv.
  wl_bape_vbak-tknum           = ''.
  sl_bapiparex-structure       = 'BAPE_VBAK'.
  sl_bapiparex-valuepart1      = wl_bape_vbak.
  APPEND sl_bapiparex TO tl_bapiparex.

  CLEAR sl_bapiparex.
  wl_bape_vbakx-vbeln          = p_saida-ovserv.
  wl_bape_vbakx-tknum          = 'X'.
  sl_bapiparex-structure       = 'BAPE_VBAKX'.
  sl_bapiparex-valuepart1      = wl_bape_vbakx.
  APPEND sl_bapiparex TO tl_bapiparex.

  wl_orderheaderin-bill_block  = '10'.
  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderinx-bill_block = 'X'.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument    = p_ovserv
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    TABLES
      return           = t_return_vt
      extensionin      = tl_bapiparex.

  READ TABLE t_return_vt WITH KEY type = 'E'.

  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    p_saida-ovserv = icon_icon_list.

    LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom).
      UPDATE zsdt0001 SET st_proc      = vg_st_ov_frete_before
                          ov_frete     = ''
       WHERE ch_referencia = _wl_rom-ch_referencia.

      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida_tmp>) WITH KEY ch_referencia = _wl_rom-ch_referencia.
      IF sy-subrc EQ 0.
        <fs_saida_tmp>-ovserv = icon_icon_list.
      ENDIF.
    ENDLOOP.

  ELSE.

    p_erro = 'X'.

    READ TABLE t_return_vt WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      PERFORM f_prepare_return TABLES t_return_vt.
      PERFORM f_grava_log_erro TABLES tg_log_erro
                                USING p_saida.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_call_carta_correcao.

  CLEAR: tl_rows[].

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  IF wa_saida-transp NE icon_execute_object.
    MESSAGE i000(z01) WITH 'Dados de transporte '
                           'deve ser estornado'.
    EXIT.
  ENDIF.

  IF wa_saida-danfe EQ icon_execute_object.
    MESSAGE i000(z01) WITH 'Não existe DANFE'
                           'Autorizado'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(wa_agente)
   WHERE lifnr = @wa_saida-lifnr.

  IF sy-subrc = 0.
    wa_ag_frete-transpor1  = wa_agente-lifnr.
    wa_ag_frete-name1      = wa_agente-name1.
    wa_ag_frete-cnpj1      = wa_agente-stcd1.
    wa_ag_frete-inscr1     = wa_agente-stcd3.
    CALL SCREEN 0300     STARTING AT 020 1
                         ENDING   AT 145 15.
  ENDIF.

ENDFORM.

FORM f_call_dados_transp .

  CLEAR: tl_rows[].

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha apenas' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  IF wa_saida-inco1  NE 'CIF'.
    MESSAGE i000(z01) WITH 'Dados de transporte não disponível '
                           'para tipo de frete:'
                           wa_saida-inco1.
    EXIT.
  ENDIF.

  CLEAR: wa_trans,wa_mot.
  REFRESH it_veic.
  SELECT SINGLE z~motorista la~name1 la~stcd2 la~stcd4 lb~zsabe la~stcd3 lb~eikto
    FROM zsdt0001 AS z
    INNER JOIN lfa1 AS la ON la~lifnr = z~motorista
    INNER JOIN lfb1 AS lb ON lb~lifnr = z~motorista "AND LB~BUKRS = P_BUKRS
  INTO wa_mot
  WHERE ch_referencia = wa_saida-ch_referencia.

  SELECT SINGLE lifnr name1 stcd1
    FROM lfa1
    INTO wa_trans
    WHERE lifnr = wa_saida-lifnr.

  SELECT z1~placa_cav z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
    FROM zsdt0001 AS z1
    INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_cav
    INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
  INTO TABLE it_veic
  WHERE ch_referencia = wa_saida-ch_referencia.

  SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     FROM zsdt0001 AS z1
     INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car1
   INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
   APPENDING TABLE it_veic
   WHERE ch_referencia = wa_saida-ch_referencia.

  SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     FROM zsdt0001 AS z1
     INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car2
   INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
   APPENDING TABLE it_veic
   WHERE ch_referencia = wa_saida-ch_referencia.

  SELECT z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     FROM zsdt0001 AS z1
     INNER JOIN zlest0002 AS z2 ON z2~pc_veiculo  = z1~placa_car3
   INNER JOIN lfa1      AS la ON la~lifnr       = z2~proprietario
   APPENDING TABLE it_veic
   WHERE ch_referencia = wa_saida-ch_referencia.

  CALL SCREEN 0200     STARTING AT 020 1
                       ENDING   AT 140 23.

ENDFORM.

FORM f_call_danfe_dacte USING p_tipo.

  "P_TIPO : 1 = Danfe
  "         2 = Dacte

  DATA: v_imp_doc TYPE j_1bdocnum.

  CLEAR: tl_rows[].

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para impressão' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  CASE p_tipo.
    WHEN '1'. "Danfe.
      IF ( wa_saida-danfe = icon_execute_object ).
        MESSAGE 'Nota não gerada' TYPE 'I'.
        EXIT.
      ENDIF.

      v_imp_doc = wa_saida-danfe.
    WHEN '2'. "Dacte

      IF ( wa_saida-dacte = icon_execute_object ).
        MESSAGE 'CTE não gerado' TYPE 'I'.
        EXIT.
      ENDIF.

      v_imp_doc = wa_saida-dacte.
  ENDCASE.

  PERFORM f_check_auth_doc USING v_imp_doc.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
      EXPORTING
        doc_numero     = v_imp_doc
      EXCEPTIONS
        nao_localizado = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE |Documento: { v_imp_doc } não está autorizado!| TYPE 'I'.
    EXIT.
  ENDIF.


ENDFORM.

FORM f_call_estorno_cte .

  IF sy-tcode NE 'ZLES0136'  AND sy-tcode NE 'ZMM0127'.
    MESSAGE 'Transação apenas de visualização' TYPE 'I'.
    EXIT.
  ENDIF.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para o estorno' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX sl_rows-index.

  CHECK sy-subrc EQ 0.

*----CS2021000508 - 07.06.2021 - JT - inicio
  IF <fs_out>-troca_nota            = abap_true AND
     <fs_out>-docs_enviado_carguero = abap_true.
    MESSAGE 'Remover antes os documentos no Carguero, para estorno!' TYPE  'I'.
    EXIT.
  ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

  " Processo estorno não completo
  IF ( <fs_out>-st_proc EQ vg_st_finalizado AND
       <fs_out>-inco1   NE 'CPT' AND ( NOT <fs_out>-enc_doc_custo EQ abap_true )
     ) OR " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( <fs_out>-st_proc EQ vg_st_aguard_doc_carg ) OR " Aguardando envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( <fs_out>-st_proc EQ vg_st_fatura_frete  ) OR " Fatura Frete
     ( <fs_out>-st_proc EQ vg_st_ov_frete      ) OR " OV.Frete
     ( <fs_out>-st_proc EQ vg_st_custo         ) OR " Doc.Custo
     ( <fs_out>-st_proc EQ vg_st_transp        )." Transporte

    REFRESH ti_zlest0100.
    PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SELECT SINGLE st_proc
      FROM zsdt0001 INTO  <fs_out>-st_proc
     WHERE ch_referencia = <fs_out>-ch_referencia.

    PERFORM f_estorno_cte CHANGING <fs_out>.
    IF ti_zlest0100[] IS NOT INITIAL.
      <fs_out>-icon = icon_led_red.
    ELSE.
      CLEAR <fs_out>-icon.
    ENDIF.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
    PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    PERFORM f_refresh_alv USING '0100'. "Refresh na tela

    CALL METHOD cl_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSEIF ( <fs_out>-st_proc = vg_st_finalizado ) OR
         ( <fs_out>-st_proc = vg_st_dacte ).

    IF ( ( <fs_out>-inco1 = 'CPT' ) OR
         ( <fs_out>-enc_doc_custo EQ abap_true ) ) AND ( <fs_out>-st_proc = vg_st_finalizado ). "Estorna mesmo finalizado se CPT

      PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      UPDATE zsdt0001 SET st_proc = <fs_out>-st_proc
       WHERE ch_referencia = <fs_out>-ch_referencia.

      PERFORM f_estorno_custo CHANGING <fs_out>.

      PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
      PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      PERFORM f_refresh_alv USING '0100'. "Refresh na tela
    ELSE.
      MESSAGE 'Dacte deve ser não autorizada para este processo.' TYPE  'I'.
    ENDIF.

  ELSE.
    MESSAGE 'Não existe documentos de transporte para estorno' TYPE  'I'.
  ENDIF.


ENDFORM.

*----------------------------------------------------------------------
*-CS2021000117 - 28.04.2021 - JT - inicio
* exibir ordem de carregamento
*----------------------------------------------------------------------
FORM f_ordem_carregamento.

  DATA: l_id_ordem     TYPE zde_id_ordem.

  FREE: wa_saida,
        l_id_ordem.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para Exibir Ordem de Carregamento' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows  INTO sl_rows  INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  l_id_ordem = wa_saida-id_ordem.

  IF l_id_ordem IS INITIAL.
    SELECT id_ordem
      INTO l_id_ordem
      FROM zsdt0001
        UP TO 1 ROWS
     WHERE ch_referencia = wa_saida-ch_referencia
       AND tp_movimento  = wa_saida-tp_movimento.
    ENDSELECT.
  ENDIF.

  IF l_id_ordem IS INITIAL.
    MESSAGE 'Ordem de Carregamento não disponível para visualização' TYPE 'I'.
    EXIT.
  ENDIF.

*----------------------------
* exibe ordem de carregamento
*----------------------------
  CALL FUNCTION 'ZSD_EXIBE_ORDEM_CARREGAMENTO'
    EXPORTING
      i_id_ordem = l_id_ordem.

ENDFORM.
*-CS2021000117 - 28.04.2021 - JT - fim

*----------------------------------------------------------------------
*-CS2021000656 - 14.05.2021 - JT - inicio
* imprimit selo
*----------------------------------------------------------------------
FORM f_imprimir_selo.

  DATA: l_id_ordem     TYPE zde_id_ordem.
  DATA: l_nr_safra     TYPE zsdt0001od-nr_safra.
  DATA: l_nr_safra2    TYPE zpmt0054-safra.
  DATA : l_docnum      TYPE j_1bnfdoc-docnum.

  FREE: wa_saida,
        l_nr_safra,
        l_id_ordem.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para Imprimir o Selo' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows  INTO sl_rows  INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  l_docnum   = wa_saida-danfe.
  l_id_ordem = wa_saida-id_ordem.

  IF l_id_ordem IS INITIAL.
    SELECT id_ordem
      INTO l_id_ordem
      FROM zsdt0001
        UP TO 1 ROWS
     WHERE ch_referencia = wa_saida-ch_referencia
       AND tp_movimento  = wa_saida-tp_movimento.
    ENDSELECT.
  ENDIF.

  SELECT nr_safra
    INTO l_nr_safra
    FROM zsdt0001od
      UP TO 1 ROWS
   WHERE id_ordem = l_id_ordem.
  ENDSELECT.

  l_nr_safra2 = l_nr_safra.

  CALL FUNCTION 'ZSD_IMPRIME_SELO'
    EXPORTING
      i_docnum                 = l_docnum
      i_safra                  = l_nr_safra2
      i_imprime_selo           = 'X'
    EXCEPTIONS
      documento_nao_autorizado = 1
      documento_nao_imprimir   = 2
      OTHERS                   = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Selo não pode ser impresso.' TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.
*-CS2021000656 - 14.05.2021 - JT - fim


*-CS2021000218-16.11.2022-#90706-JT-inicio
*----------------------------------------------------------------------
* imprimit RA ASSINADA
*----------------------------------------------------------------------
FORM f_imprimir_ra_assinada.

  FREE: wa_saida.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para Imprimir R.A. assinada' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows  INTO sl_rows  INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = |Verificando documentos. Aguarde...|.

*----------------------------------
* Imprimir RA
*----------------------------------
  CALL FUNCTION 'ZSD_EXIBIR_RA_ASSINADA'
    EXPORTING
      i_nro_cg        = wa_saida-nro_cg
      i_ch_referencia = wa_saida-ch_referencia
    EXCEPTIONS
      pdf_not_found   = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Receituário Agronômico não pode ser impresso.' TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
*----------------------------------------------------------------------
* imprimir Romaneio Algodão
*----------------------------------------------------------------------
FORM f_impr_romaneio_algodao.

  FREE: wa_saida.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para Imprimir Rom.Algodão' TYPE 'I'.
    EXIT.
  ENDIF.

  READ TABLE tl_rows  INTO sl_rows  INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = |Verificando documentos. Aguarde...|.

*----------------------------------
* Imprimir Romaneio Algodão
*----------------------------------
  CALL FUNCTION 'ZSD_IMPRIME_REL_ROMANEIO'
    EXPORTING
      i_ch_referencia           = wa_saida-ch_referencia
      i_imprime                 = abap_true
    EXCEPTIONS
      romaneio_nao_encontrado   = 1
      romaneio_em_estorno       = 2
      erro_impressao_formulario = 3
      OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Romaneio Algodão não pode ser impresso.' TYPE 'I'.
    EXIT.
  ENDIF.

ENDFORM.
*-CS2023000189-26.05.2023-#108752-JT-fim

FORM f_call_estorno_nfe.

  DATA index_estorno TYPE sy-tabix.

  IF sy-tcode NE 'ZLES0136'  AND sy-tcode NE 'ZMM0127'.
    MESSAGE 'Transação apenas de visualização' TYPE 'I'.
    EXIT.
  ENDIF.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para o estorno.' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA(_saida_aux) = it_saida[ tl_rows[ 1 ]-index ].

  IF line_exists( t_fatura_agrupada[ werks = _saida_aux-branch kunnr = _saida_aux-kunnr inco1 = _saida_aux-inco1 cfop = _saida_aux-cfop ] ).

    CLEAR tl_rows[].
    LOOP AT it_saida INTO it_saida WHERE ( dt_movimento EQ _saida_aux-dt_movimento ) AND
                                         ( matnr        EQ _saida_aux-matnr        ) AND
                                         ( kunnr        EQ _saida_aux-kunnr        ) AND
                                         ( operacao(4)  EQ _saida_aux-operacao(4)  ) AND
*                                        ( INCO1        EQ VINCO1                  ) AND
                                         ( cfop         EQ _saida_aux-cfop         ).

      CHECK ( it_saida-remessa IS NOT INITIAL AND it_saida-remessa NE icon_execute_object
         OR   it_saida-fatura  IS NOT INITIAL AND it_saida-fatura  NE icon_execute_object ).

      APPEND VALUE #( index = sy-tabix ) TO tl_rows.
    ENDLOOP.

    IF lines( tl_rows ) > 1.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Estorno de Fatura Agrupada'
          text_question         = 'As faturas e os documentos de remessa serão estornados. Tem certeza que deseja continuar?'
          text_button_1         = 'Sim'(100)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(101)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK w_answer = 1.
    ENDIF.
  ENDIF.


  CLEAR index_estorno.
  LOOP AT tl_rows INTO sl_rows.
    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX sl_rows-index.

    ADD 1 TO index_estorno.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = |Estornando documento(s) { index_estorno } de { lines( tl_rows ) }.|.

*----CS2021000508 - 07.06.2021 - JT - inicio
    IF <fs_out>-troca_nota            = abap_true AND
       <fs_out>-docs_enviado_carguero = abap_true.
      MESSAGE 'Remover antes os documentos no Carguero, para estorno!' TYPE  'I'.
      CONTINUE.
    ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

    IF <fs_out>-operacao(4) = 'ZPAR'.

      IF ( <fs_out>-st_proc EQ vg_st_fatura  ) OR " Fatura
         ( <fs_out>-st_proc EQ vg_st_remessa ) OR " Remessa
         ( <fs_out>-st_proc EQ vg_st_finalizado ) .  " Finalizado

        PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        SELECT SINGLE st_proc
          FROM zsdt0001 INTO  <fs_out>-st_proc
         WHERE ch_referencia = <fs_out>-ch_referencia.

        IF ( <fs_out>-st_proc EQ vg_st_fatura  ) OR " Fatura
           ( <fs_out>-st_proc EQ vg_st_remessa ) OR " Remessa
           ( <fs_out>-st_proc EQ vg_st_finalizado ) .  " Finalizado

          IF <fs_out>-st_proc EQ vg_st_finalizado.
            <fs_out>-st_proc = vg_st_fatura.
          ENDIF.

          PERFORM f_estorno_nfe CHANGING <fs_out>.

          IF ti_zlest0100[] IS NOT INITIAL.
            <fs_out>-icon = icon_led_red.
          ELSE.
            CLEAR <fs_out>-icon.
          ENDIF.
        ENDIF.

        REFRESH style.
        IF <fs_out>-lifnr IS INITIAL.

          IF ( t_fatura_agrupada IS NOT INITIAL ) AND
             ( 'CFR_FOB' CS <fs_out>-inco1      ) AND
             ( <fs_out>-inco1 IS NOT INITIAL ).

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT wa_style INTO TABLE style .
          ELSE.
            DELETE <fs_out>-style WHERE fieldname EQ 'LIFNR'.
          ENDIF.
        ELSE.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.

        IF <fs_out>-region IS INITIAL.
          DELETE <fs_out>-style WHERE fieldname EQ 'REGION'.
        ELSE.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.

        <fs_out>-style = style[].

        PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
        PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio


      ELSE.
        MESSAGE 'Não existem documentos referentes a NF-e para estorno.' TYPE  'I'.
      ENDIF.

    ELSE.

      " Processo estorno não completo.
      IF ( <fs_out>-st_proc EQ vg_st_custo     ) OR " Doc.Custo
         ( <fs_out>-st_proc EQ vg_st_transp    ) OR " Transporte
         ( <fs_out>-st_proc EQ vg_st_fatura    ) OR " Fatura
         ( <fs_out>-st_proc EQ vg_st_aviso_rec ) OR " Aviso
         ( <fs_out>-st_proc EQ vg_st_remessa   ).   " Remessa

        PERFORM f_lock_rom USING 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        SELECT SINGLE st_proc
          FROM zsdt0001 INTO  <fs_out>-st_proc
         WHERE ch_referencia = <fs_out>-ch_referencia.

        REFRESH ti_zlest0100.
        IF ( ( <fs_out>-st_proc EQ vg_st_custo   ) OR " Doc.Custo
             ( <fs_out>-st_proc EQ vg_st_transp  ) ) AND " Transporte

           ( ( <fs_out>-inco1 = 'CPT' ) OR
             ( <fs_out>-enc_doc_custo EQ abap_true ) ).

          <fs_out>-st_proc = vg_st_custo.

          UPDATE zsdt0001 SET st_proc = <fs_out>-st_proc
           WHERE ch_referencia = <fs_out>-ch_referencia.

          PERFORM f_estorno_custo CHANGING <fs_out>.
        ENDIF.

        IF ( <fs_out>-st_proc EQ vg_st_fatura    ) OR " Fatura
           ( <fs_out>-st_proc EQ vg_st_aviso_rec ) OR " Aviso
           ( <fs_out>-st_proc EQ vg_st_remessa   ).   " Remessa

          PERFORM f_estorno_nfe CHANGING <fs_out>.

          IF ti_zlest0100[] IS NOT INITIAL.
            <fs_out>-icon = icon_led_red.
          ELSE.
            CLEAR <fs_out>-icon.
          ENDIF.
        ENDIF.

        REFRESH style.
        IF <fs_out>-lifnr IS INITIAL.
          IF ( t_fatura_agrupada IS NOT INITIAL ) AND
             ( 'CFR_FOB' CS <fs_out>-inco1      ) AND
             ( <fs_out>-inco1 IS NOT INITIAL ).

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT wa_style INTO TABLE style .
          ELSE.
            DELETE <fs_out>-style WHERE fieldname EQ 'LIFNR'.
          ENDIF.
        ELSE.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.

        IF <fs_out>-region IS INITIAL.
          DELETE <fs_out>-style WHERE fieldname EQ 'REGION'.
        ELSE.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.

        <fs_out>-style = style[].

        PERFORM f_repare_docs_romaneio CHANGING <fs_out>.
        PERFORM f_lock_rom USING 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

      ELSE.
        MESSAGE 'Não existem documentos referentes a NF-e para estorno.' TYPE  'I'.
      ENDIF.

    ENDIF.

  ENDLOOP.

  PERFORM f_refresh_alv USING '0100'. "Refresh na tela

ENDFORM. "FORM f_call_estorno_nfe

FORM f_repare_docs_romaneio_sel.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) EQ 0.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX sl_rows-index.

    CHECK sy-subrc = 0.

    PERFORM f_repare_docs_romaneio CHANGING <fs_out>.

  ENDLOOP.

  PERFORM f_refresh_alv USING '0100'. "Refresh na tela

ENDFORM.

FORM f_repare_docs_romaneio CHANGING c_saida TYPE ty_saida.

  CHECK c_saida IS NOT INITIAL.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(lwa_zsdt0001_current)
   WHERE ch_referencia EQ @c_saida-ch_referencia.

  CHECK sy-subrc EQ 0.

  DATA(lwa_zsdt0001_new) = lwa_zsdt0001_current.

*-------------------------------------------------------------------------------------*
*============>                   DOC. ZNFW                               <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_doc_znfw_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                  DANFE ZNFW                               <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_danfe_znfw_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   AVISO                                  <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_aviso_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   REMESSA                                <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_doc_rem_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   FATURA NF-e                            <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_fat_nf_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   DANFE                                  <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_danfe_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               DOC. TRANSPORTE                            <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_tknum_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               DOC. CUSTO                                 <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_fknum_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               OV. SERVIÇO                                <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_ov_serv_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               FATURA. FRETE                              <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_fat_serv_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   DACTE                                  <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_dacte_romaneio CHANGING c_saida lwa_zsdt0001_new.


*-------------------------------------------------------------------------------------*
*============>                   AGENTE FRETE                          <=========== *
*-------------------------------------------------------------------------------------*

  PERFORM f_get_agente_fre_romaneio CHANGING c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>          RECOMPOSIÇÃO DE STATUS.                         <===========  *
*-------------------------------------------------------------------------------------*


  IF ( c_saida-remessa  IS NOT INITIAL AND c_saida-remessa(1) NE '@' ) OR
     ( c_saida-seq_lcto IS NOT INITIAL AND c_saida-seq_lcto(1) NE '@' ).
    lwa_zsdt0001_new-status = abap_true.
  ENDIF.


  CASE vg_cockpit. "Recomposição de Status
    WHEN '01' OR '05' OR '06' OR '07' OR '03' OR '09'.

      PERFORM f_build_status_romaneio_01 USING lwa_zsdt0001_current CHANGING c_saida.

    WHEN '04'. " Fertilizantes (Porto Velho)

      PERFORM f_build_status_romaneio_02 USING lwa_zsdt0001_current CHANGING c_saida.

  ENDCASE.

  IF c_saida-st_proc = '99'.
    IF c_saida-transp = icon_execute_object.
      c_saida-transp = icon_icon_list.
    ENDIF.

    IF c_saida-dacte = icon_execute_object.
      c_saida-dacte = icon_icon_list.
    ENDIF.

    IF c_saida-danfe = icon_execute_object.
      c_saida-danfe = icon_icon_list.
    ENDIF.
  ENDIF.


  lwa_zsdt0001_new-st_proc = c_saida-st_proc.

  IF lwa_zsdt0001_current NE lwa_zsdt0001_new.
    MODIFY zsdt0001 FROM lwa_zsdt0001_new.
  ENDIF.

ENDFORM.

FORM f_item_text_delivery USING p_delivery.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Informar texto nos itens da remessa?'
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF w_answer = '1'.

    SELECT *
      FROM lips INTO TABLE @DATA(tg_lips)
     WHERE vbeln = @p_delivery.

    LOOP AT tg_lips INTO DATA(wl_lips).

      CHECK wl_lips-posnr(1) NE 9.

      CLEAR: tl_texto[].

      IF wl_lips-charg IS NOT INITIAL.
        DATA(_charg) = |/ Lote: { wl_lips-charg } |.
      ENDIF.

      SELECT SINGLE *
        FROM makt INTO @DATA(wl_makt)
       WHERE spras = @sy-langu
         AND matnr = @wl_lips-matnr.

      IF ( sy-subrc = 0 ) AND ( wl_lips-matnr IS NOT INITIAL ).
        DATA(_maktx) = |/ Material: { wl_makt-maktx } |.
      ENDIF.

      DATA(_posnr) = wl_lips-posnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = _posnr
        IMPORTING
          output = _posnr.

      wl_text_header = |Item: { _posnr } { _charg } { _maktx }  |.

      "//Buscar Texto da item remessa
      CONCATENATE wl_lips-vbeln wl_lips-posnr INTO wl_ordemt.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = '0001'
          language                = sy-langu
          name                    = wl_ordemt
          object                  = 'VBBP'
        TABLES
          lines                   = it_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT it_lines INTO wa_lines.
        wl_texto = wa_lines-tdline+0(72).
        APPEND wl_texto TO tl_texto.
      ENDLOOP.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = wl_text_header
        CHANGING
          ch_text  = tl_texto.

      CLEAR: it_lines[], wa_lines.
*      WA_LINES-TDFORMAT = '*'.
*      APPEND WA_LINES TO IT_LINES.

      LOOP AT tl_texto INTO wl_texto.
        wa_lines-tdformat = '*'.
        wa_lines-tdline+0(72) = wl_texto.
        APPEND wa_lines TO it_lines.
        CLEAR  wa_lines.
      ENDLOOP.

      IF ( it_lines[] IS NOT INITIAL ).
        zid               = '0001'.
        CONCATENATE wl_lips-vbeln wl_lips-posnr INTO zname.
        x_header-tdobject = 'VBBP'.
        x_header-tdname   = zname.
        x_header-tdid     = zid.
        x_header-tdspras  = sy-langu.

        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            client          = sy-mandt
            header          = x_header
            savemode_direct = 'X'
          TABLES
            lines           = it_lines
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.
      ENDIF.

    ENDLOOP.

  ENDIF.
  "Fim Textos Itens

ENDFORM.

FORM f_check_aut_doc  USING p_tipo
                            p_saida    TYPE ty_saida
                            p_zsdt0001 TYPE ty_zsdt0001
                   CHANGING p_doc_aut.

  DATA: wl_docnum TYPE j_1bnflin-docnum.

  CLEAR: p_doc_aut, vl_refkey, vl_vbeln, vl_mjahr, vl_docnum, wl_docnum.

  CASE p_tipo.
    WHEN '1'. "Fluxo 1

      IF ( p_saida-tipo = 'P' ) OR ( p_saida-tipo = 'T' ).
        SELECT SINGLE vbeln mjahr
          INTO (vl_vbeln,vl_mjahr)
          FROM vbfa
         WHERE vbelv = p_zsdt0001-doc_rem
           AND vbtyp_n  = 'R'
           AND vbtyp_v  = 'J'.

        IF sy-subrc = 0.
          CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
          SELECT SINGLE docnum
            FROM j_1bnflin
            INTO wl_docnum
            WHERE refkey = vl_refkey.
        ENDIF.
      ELSE.
        SELECT SINGLE docnum
          FROM j_1bnflin
          INTO wl_docnum
          WHERE refkey = p_saida-fatura.
      ENDIF.

      IF ( sy-subrc = 0 ) AND ( wl_docnum IS NOT INITIAL ).

        PERFORM f_check_auth_doc USING wl_docnum.

        IF sy-subrc = 0.
          p_doc_aut = wl_docnum.
        ENDIF.
      ENDIF.

  ENDCASE.



ENDFORM.

FORM f_check_canc_doc USING p_docnum.

  DATA: v_docnum TYPE j_1bnfdoc-docnum.

  v_docnum = p_docnum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_docnum
    IMPORTING
      output = v_docnum.

  IF ( v_docnum IS INITIAL ).
    sy-subrc = 1.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(wl_active_ck)
   WHERE docnum  EQ @v_docnum.

  IF sy-subrc NE 0.
    sy-subrc = 1.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(wl_doc_ck)
   WHERE docnum  EQ @v_docnum.

  IF sy-subrc NE 0.
    sy-subrc = 1.
    RETURN.
  ENDIF.

  IF ( wl_active_ck-cancel EQ abap_true ) OR ( wl_active_ck-scssta EQ '2' ) OR ( wl_doc_ck-candat IS NOT INITIAL ).
    sy-subrc = 0.
  ELSE.
    sy-subrc = 1.
  ENDIF.

ENDFORM.

FORM f_check_auth_doc USING p_docnum.

  DATA: v_docnum TYPE j_1bnfdoc-docnum.

  v_docnum = p_docnum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_docnum
    IMPORTING
      output = v_docnum.

  IF ( v_docnum IS INITIAL ).
    sy-subrc = 1.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(wl_active_doc)
   WHERE docnum     EQ @v_docnum.

  IF ( sy-subrc EQ 0 ) AND ( wl_active_doc-docsta EQ '1' ) AND ( wl_active_doc-scssta NE '2' ) AND ( wl_active_doc-cancel NE abap_true ).
    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(wl_doc_ck)
     WHERE docnum EQ @v_docnum.

    IF wl_doc_ck-candat IS INITIAL.
      sy-subrc = 0.
    ELSE.
      sy-subrc = 1.
    ENDIF.
  ELSE.
    sy-subrc = 1.
  ENDIF.

ENDFORM.

FORM f_config_cell  USING p_value
                          p_fieldname TYPE lvc_s_styl-fieldname
                          p_style     TYPE lvc_s_styl-style.

  CLEAR: wa_style.
  wa_style-fieldname = p_fieldname.
  wa_style-style     = p_style.
  INSERT wa_style INTO TABLE style .

ENDFORM.

FORM f_atrib_vlr_nf_rem  USING p_delivery TYPE likp-vbeln
                           p_netwr    TYPE zsdt0001-netwr.

  DATA: lf_error    TYPE flag,
        lv_upd      TYPE xfeld,
        ls_vbkok    TYPE vbkok,
        ls_logfile  TYPE prott,
        ls_delivery TYPE lxhme_range_c10,
        ls_location TYPE lyloc_wa,
        lt_log_file TYPE TABLE OF prott.

  IF ( p_delivery IS INITIAL ).
    MESSAGE 'Nro. Remessa não informado.(Atribuição Valor NF em Doc. Remessa)' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( p_netwr  IS INITIAL ).
    MESSAGE 'Valor NF não informado no Romaneio.(Atribuição Valor NF em Doc. Remessa)' TYPE 'S'.
    EXIT.
  ENDIF.

  ls_vbkok-vbeln_vl = p_delivery.
  ls_vbkok-bolnr    = p_netwr.

  REPLACE ALL OCCURRENCES OF '.' IN ls_vbkok-bolnr WITH ','.
  CONDENSE ls_vbkok-bolnr NO-GAPS.

  CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
    EXPORTING
      vbkok_wa      = ls_vbkok
      synchron      = 'X'
      commit        = 'X'
      delivery      = ls_vbkok-vbeln_vl
    IMPORTING
      ef_error_any  = lf_error
    TABLES
      prot          = lt_log_file
    EXCEPTIONS
      error_message = 99.

  IF lt_log_file[] IS NOT INITIAL.
    READ TABLE lt_log_file
       WITH KEY msgty = lyrgc_msgty_error
       INTO ls_logfile.

    IF sy-subrc = 0.

      sy-msgty = lyrgc_msgty_error.
      sy-msgid = ls_logfile-msgid.
      sy-msgno = ls_logfile-msgno.
      sy-msgv1 = ls_logfile-msgv1.
      sy-msgv2 = ls_logfile-msgv2.
      sy-msgv3 = ls_logfile-msgv3.
      sy-msgv4 = ls_logfile-msgv4.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING service_failure.

    ENDIF.
  ENDIF.

ENDFORM.

FORM f_check_permissao_estorno  USING p_saida TYPE ty_saida
                                      p_tipo "1 NF-e / 2 CT-e
                             CHANGING p_ok.

*------------------------------------------------------------------*
* P_TIPO: 1 NF-e / 2 CT-e
*
*------------------------------------------------------------------*

  CLEAR: p_ok.

  SELECT SINGLE * FROM zsdt0001 INTO @DATA(wl_0001)
   WHERE ch_referencia = @p_saida-ch_referencia.

  CHECK sy-subrc = 0.

  SELECT SINGLE * FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @p_saida-shtyp.

  IF ( p_tipo EQ '1' ) OR ( p_tipo EQ '2' ).

    IF ( wl_tvtk-abfer        IS NOT INITIAL ) AND
       ( wl_0001-agente_frete IS NOT INITIAL ) AND
       ( wl_0001-branch       IS NOT INITIAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_0001-agente_frete
        IMPORTING
          output = wl_0001-agente_frete.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_0001-branch
        IMPORTING
          output = wl_0001-branch.

      CONCATENATE wl_0001-branch '-' wl_0001-agente_frete INTO DATA(_branch_agente).

      CASE wl_tvtk-abfer.
        WHEN '1' OR '3'. "Saída

        WHEN '2' OR '4'. "Entrada
          SELECT SINGLE *
            FROM setleaf INTO @DATA(wl_setleaf)
           WHERE setname = 'MAGGI_ZLES0136_BLQ_ROM_E'
             AND valfrom = @_branch_agente.

          IF sy-subrc = 0.
            MESSAGE | Estorno não permitido! Romaneios de entrada da Filial/Agente de Frete: { _branch_agente }, parametrizados para modo de visualização(SET MAGGI_ZLES0136_BLQ_ROM_E)!| TYPE 'S'.
            RETURN.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDIF.

  CASE p_tipo.
    WHEN '1'.
    WHEN '2'.

      IF ( wl_0001-nro_nf_frete  IS NOT INITIAL ) AND
         ( wl_0001-id_referencia IS NOT INITIAL ) AND
         ( wl_0001-id_interface  EQ '49' ).
        MESSAGE |Estorno não permitido! Romaneio já referenciado pelo romaneio com Chave: { wl_0001-id_referencia } | TYPE 'S'.
        RETURN.
      ENDIF.

  ENDCASE.

  p_ok = 'X'.

ENDFORM.

FORM f_set_romaneios_carga CHANGING p_saida TYPE ty_saida
                                    p_erro  TYPE c.

  CLEAR: p_saida-romaneios, p_saida-romaneios_agr, p_erro.

  IF p_saida-ch_referencia IS INITIAL.
    p_erro = abap_true.
    MESSAGE 'Ch.Referência não atribuída!!' TYPE 'S'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
   WHERE ch_referencia EQ @p_saida-ch_referencia.

  IF sy-subrc NE 0.
    p_erro = abap_true.
    MESSAGE |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| TYPE 'S' .
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @p_saida-shtyp.

  IF ( sy-subrc NE 0 ) OR ( wl_tvtk-abfer IS INITIAL ).
    p_erro = abap_true.
    MESSAGE 'Tipo de Transporte não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  CASE wl_tvtk-abfer.
    WHEN '1' OR '3'. "Saída

      DATA(_vbtyp_v)  = 'J'.

      IF p_saida-inco1 NE 'CIF'.

        APPEND _wl_0001 TO p_saida-romaneios.
        APPEND _wl_0001 TO p_saida-romaneios_agr.

      ELSE.

        CALL METHOD zcl_romaneio=>get_ck_faturar
          EXPORTING
            i_ch_referencia_sai = p_saida-ch_referencia
          IMPORTING
            e_romaneios         = p_saida-romaneios.


        APPEND _wl_0001 TO p_saida-romaneios.

        LOOP AT p_saida-romaneios INTO DATA(_wl_rom) WHERE id_cli_dest EQ _wl_0001-id_cli_dest.
          APPEND _wl_rom TO p_saida-romaneios_agr.
        ENDLOOP.

      ENDIF.

    WHEN '2' OR '4'. "Entrada

      _vbtyp_v  = '7'.

      APPEND _wl_0001 TO p_saida-romaneios.
      APPEND _wl_0001 TO p_saida-romaneios_agr.

  ENDCASE.

  SORT p_saida-romaneios BY ch_referencia.
  DELETE ADJACENT DUPLICATES FROM p_saida-romaneios COMPARING ch_referencia.

  SORT p_saida-romaneios_agr BY ch_referencia.
  DELETE ADJACENT DUPLICATES FROM p_saida-romaneios_agr COMPARING ch_referencia.

ENDFORM.


FORM f_valida_geracao_vt CHANGING p_saida TYPE ty_saida
                                  p_erro  TYPE c.

  DATA: t_route          TYPE TABLE OF vbap-route WITH HEADER LINE,
        t_romaneios      TYPE zsdt0001_t,
        v_faturar	       TYPE char01,
        v_auart          TYPE vbak-auart,
        v_peso_carga     TYPE zde_nm_peso_subtotal,
        v_peso_romaneios TYPE zde_nm_peso_subtotal,
        v_qtd_embalagens TYPE zde_qt_embalagens,
        v_mensagem       TYPE char255,
        v_chv_fat_vt     TYPE zch_ref.

  CLEAR: t_romaneios[], p_erro, v_faturar, v_peso_carga, v_peso_romaneios,v_qtd_embalagens,v_mensagem, t_route[], v_chv_fat_vt.

  IF p_saida-ch_referencia IS INITIAL.
    p_erro = abap_true.
    MESSAGE 'Ch.Referência não atribuída!!' TYPE 'S'.
    EXIT.
  ENDIF.

  CHECK p_saida-transp EQ icon_execute_object.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
   WHERE ch_referencia EQ @p_saida-ch_referencia.

  IF sy-subrc NE 0.
    p_erro = abap_true.
    MESSAGE |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| TYPE 'S' .
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @p_saida-shtyp.

  IF ( sy-subrc NE 0 ) OR ( wl_tvtk-abfer IS INITIAL ).
    p_erro = abap_true.
    MESSAGE 'Tipo de Transporte não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  CASE wl_tvtk-abfer.
    WHEN '1' OR '3'. "Saída

      DATA(_vbtyp_v)  = 'J'.

      IF p_saida-inco1 EQ 'CIF'.

        CALL METHOD zcl_romaneio=>get_ck_faturar
          EXPORTING
            i_ch_referencia_sai = p_saida-ch_referencia
          IMPORTING
            e_romaneios         = t_romaneios
            e_faturar           = v_faturar
            e_peso_carga        = v_peso_carga
            e_peso_romaneios    = v_peso_romaneios
            e_qtd_embalagens    = v_qtd_embalagens
            e_mensagem          = v_mensagem
            e_chv_faturar       = v_chv_fat_vt.

        IF ( v_faturar IS INITIAL ).
          p_erro = abap_true.
          IF v_mensagem IS NOT INITIAL.
            MESSAGE v_mensagem TYPE 'S'.
          ELSE.
            MESSAGE 'Doc. Transporte não pode ser gerado!(Check Agrupamento VT)' TYPE 'S'.
          ENDIF.
          RETURN.
        ENDIF.

        IF ( v_chv_fat_vt IS NOT INITIAL ) AND ( v_chv_fat_vt NE p_saida-ch_referencia ).
          p_erro = abap_true.
          READ TABLE t_romaneios INTO DATA(_wl_rom) WITH KEY ch_referencia = v_chv_fat_vt.
          MESSAGE |Romaneio Nro: { _wl_rom-nr_romaneio } deve ser faturado! Atualizar Consulta!(Agrupamento CT-e)!  | TYPE 'S'.
          RETURN.
        ENDIF.

      ELSE.
        APPEND _wl_0001 TO t_romaneios.
      ENDIF.

      LOOP AT t_romaneios INTO _wl_rom.

        SELECT SINGLE *
          FROM vbak INTO @DATA(lwa_vbak_exists)
         WHERE vbeln EQ @_wl_rom-vbeln.

        IF sy-subrc NE 0.
          SELECT SINGLE *
            FROM ekko INTO @DATA(lwa_ekko_exists)
           WHERE ebeln EQ @_wl_rom-vbeln.

          CHECK sy-subrc EQ 0.
        ENDIF.

        IF _wl_rom-doc_rem IS INITIAL.
          p_erro = abap_true.
          MESSAGE |Gerar a Remessa Romaneio Nro: { _wl_rom-nr_romaneio } !  | TYPE 'S'.
          RETURN.
        ENDIF.

        CLEAR: v_auart.
        SELECT SINGLE *
          FROM vbak INTO @DATA(_wl_vbak)
         WHERE vbeln = @_wl_rom-vbeln.

        IF sy-subrc EQ 0.
          v_auart = _wl_vbak-auart.
        ENDIF.

        IF ( _wl_rom-nro_nf_prod IS INITIAL ) AND ( v_auart NE 'ZTER' ).
          p_erro = abap_true.
          MESSAGE |Gerar a DANFE Romaneio Nro: { _wl_rom-nr_romaneio } ! | TYPE 'S'.
          RETURN.
        ENDIF.
      ENDLOOP.

*        SORT T_ROUTE.
*        DELETE ADJACENT DUPLICATES FROM T_ROUTE.
*
*        IF LINES( T_ROUTE[] ) > 1.
*          P_ERRO = ABAP_TRUE.
*          MESSAGE |Existe mais de um Itinerátio para a Carga! | TYPE 'S'.
*          RETURN.
*        ENDIF.

    WHEN '2' OR '4'. "Entrada

      _vbtyp_v  = '7'.

      APPEND _wl_0001 TO t_romaneios.

      IF p_saida-aviso = icon_execute_object.
        p_erro = abap_true.
        MESSAGE 'Gerar a Aviso!' TYPE 'S'.
        RETURN.
      ENDIF.


  ENDCASE.

  PERFORM f_set_delivery CHANGING p_saida
                                  p_erro.

  CHECK p_erro EQ abap_false.

  LOOP AT p_saida-deliverys INTO DATA(_delivery).
    SELECT SINGLE vttk~tknum INTO v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                                AND vttk~vsart = wl_tvtk-vsart
     WHERE vbfa~vbelv    = _delivery-vbeln
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = _vbtyp_v.
    IF sy-subrc = 0.
      p_erro = abap_true.
      MESSAGE |Documento de Transporte já gerado para a Remessa: { _delivery-vbeln }. Atualizar linha de documentos!| TYPE 'S'.
      RETURN.
    ENDIF.
  ENDLOOP.

*-CS2021000696 - 16.08.2021 - JT - inicio
  LOOP AT t_romaneios INTO _wl_rom.

    IF zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                EXPORTING i_ch_referencia = _wl_rom-ch_referencia ) = abap_false.

      DATA(l_erro) = zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~set_valida_envio_carregamento(
        EXPORTING
          i_ch_referencia = _wl_rom-ch_referencia ).

      IF l_erro = abap_true.
        p_erro = abap_true.
        MESSAGE |A viagem no carguero deve estar no status Carregamento ou Carregado.|  TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

  ENDLOOP.
*-CS2021000696 - 16.08.2021 - JT - fim

*----CS2021000508 - 07.06.2021 - JT - inicio
*--------------------------------------------------
*-- Valida status / envio aprovacao
*--------------------------------------------------
*  TRY .
*      zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
*        )->valida_envio_aprovacao(
*             EXPORTING
*               i_ch_referencia = _wl_0001-ch_referencia
*             IMPORTING
*               e_erro          = DATA(l_erro)
*               e_msg_erro      = DATA(l_msg_erro)
*        ).
*
*    CATCH zcx_integracao.
*    CATCH zcx_error.
*  ENDTRY.
*
*  IF l_erro = abap_true.
*    p_erro = l_erro.
*    MESSAGE l_msg_erro TYPE 'S' .
*    RETURN.
*  ENDIF.
*----CS2021000508 - 07.06.2021 - JT - fim

*-CS2021001045 - 22.02.2022 - JT - inicio
  DATA(_rem_conta_ordem) = abap_false.
  PERFORM f_check_rem_conta_ordem USING p_saida CHANGING _rem_conta_ordem. "Comentando para subir junto com as melhorias da ZLES0200 US 92467
  IF _rem_conta_ordem EQ abap_true.
    "O docto.Transporte deverá ser gerado pela transação ZLES0200. Romaneio será finalizado!'.
    p_erro = abap_true.
    PERFORM f_repare_docs_romaneio CHANGING p_saida. "Metodo finaliza romaneio
  ENDIF.
*-CS2021001045 - 22.02.2022 - JT - fim

ENDFORM.

*****************************************************************
* valida se simples remessa gerado por terceiro
*****************************************************************
FORM f_check_rem_conta_ordem USING p_saida TYPE ty_saida
                          CHANGING p_rem_conta_ordem.

  DATA: t_set    TYPE TABLE OF rgsb4,
        w_set    TYPE rgsb4,
        t_tvarvc TYPE TABLE OF tvarvc,
        w_tvarvc TYPE tvarvc.

  RANGES:
        r_cfop            FOR j_1bnflin-cfop,
        r_matkl           FOR j_1bnflin-matkl.

  FREE: r_cfop, r_matkl, t_set, p_rem_conta_ordem.

  CHECK p_saida-ch_referencia IS NOT INITIAL.

*---------------------------------
* ler set
*---------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_CFOP_VENDA_IND'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_set INTO w_set.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
  ENDLOOP.

*---------------------------------
* ler TVARVset
*---------------------------------
  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarvc
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_tvarvc INTO w_tvarvc.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_matkl.
  ENDLOOP.

*---------------------------------
* ler OV
*---------------------------------
  SELECT matkl, j_1bcfop
    FROM vbap
    INTO @DATA(w_vbap)
      UP TO 1 ROWS
   WHERE vbeln = @p_saida-vbeln
     AND matnr = @p_saida-matnr.
  ENDSELECT.

  IF sy-subrc         = 0           AND
     w_vbap-j_1bcfop IN r_cfop[]    AND
     w_vbap-matkl    IN r_matkl[]   AND
     r_cfop[]        IS NOT INITIAL AND
     r_matkl[]       IS NOT INITIAL.
    p_rem_conta_ordem = abap_true.
  ENDIF.

ENDFORM.


FORM f_set_delivery CHANGING p_saida TYPE ty_saida
                             p_erro  TYPE c.

  DATA: it_likp TYPE TABLE OF likp WITH HEADER LINE.

  CLEAR: p_saida-deliverys, it_likp[].

  IF p_saida-ch_referencia IS INITIAL.
    p_erro = abap_true.
    MESSAGE 'Ch.Referência não atribuída!!' TYPE 'S'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
   WHERE ch_referencia EQ @p_saida-ch_referencia.

  IF sy-subrc NE 0.
    p_erro = abap_true.
    MESSAGE |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| TYPE 'S' .
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @p_saida-shtyp.

  IF ( sy-subrc NE 0 ) OR ( wl_tvtk-abfer IS INITIAL ).
    p_erro = abap_true.
    MESSAGE 'Tipo de Transporte não encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  CASE wl_tvtk-abfer.
    WHEN '1' OR '3'. "Saída
      IF lines( p_saida-romaneios_agr[] ) EQ 1.
        CLEAR: it_likp.
        it_likp-vbeln = p_saida-remessa+0(10).
        APPEND it_likp.
      ELSE.
        LOOP AT p_saida-romaneios_agr INTO DATA(_wl_rom).
          CLEAR: it_likp.
          it_likp-vbeln = _wl_rom-doc_rem.
          APPEND it_likp.
        ENDLOOP.
      ENDIF.

      DATA(_vbtyp_v)  = 'J'.
    WHEN '2' OR '4'. "Entrada

      CLEAR: it_likp.
      it_likp-vbeln = p_saida-aviso.
      APPEND it_likp.

      _vbtyp_v  = '7'.
  ENDCASE.

  p_saida-deliverys[] = it_likp[].

  IF p_saida-deliverys[] IS INITIAL.
    p_erro = abap_true.
    MESSAGE 'Não foi possível determinar o número da Entrega!' TYPE 'S'.
    EXIT.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBAK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_vbak .

  SELECT vbeln auart kunnr spart vkbur
    FROM vbak APPENDING CORRESPONDING FIELDS OF TABLE it_vbak
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln  = it_zsdt0001-vbeln.

  LOOP AT it_vbak INTO wa_vbak.
    tabix = sy-tabix .
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    wa_vbak-tp_movimento = wa_zsdt0001-tp_movimento.
    MODIFY it_vbak FROM wa_vbak INDEX tabix TRANSPORTING tp_movimento.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBPA_CO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_vbpa_co .

  "Ordem Venda
  SELECT vbeln lifnr
    FROM vbpa APPENDING TABLE it_vbpa_co
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln = it_zsdt0001-vbeln
     AND parvw = 'PC'.

  IF it_vbpa_co[] IS NOT INITIAL.
    SELECT lifnr name1 dlgrp lzone regio
      FROM lfa1 APPENDING TABLE it_lfa1
       FOR ALL ENTRIES IN it_vbpa_co
     WHERE lifnr  = it_vbpa_co-lifnr.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_vbpa .

  SELECT vbeln parvw lifnr kunnr
    FROM vbpa APPENDING TABLE it_vbpa
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln = it_zsdt0001-vbeln
     AND parvw  IN ('LR','SP','Z1').

  SELECT vbeln parvw lifnr kunnr
    FROM vbpa APPENDING TABLE it_vbpa
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln = it_zsdt0001-doc_aviso
     AND parvw  IN ('LR','SP').

  SELECT vbeln parvw lifnr kunnr
    FROM vbpa APPENDING TABLE it_vbpa
     FOR ALL ENTRIES IN it_zsdt0001
   WHERE vbeln = it_zsdt0001-doc_rem
     AND parvw  IN ('LR','SP').

  IF it_vbpa[] IS NOT INITIAL.
    SELECT kunnr name1 lzone
      FROM kna1 APPENDING TABLE it_kna1
       FOR ALL ENTRIES IN it_vbpa
     WHERE kunnr  = it_vbpa-kunnr.
  ENDIF.

  LOOP AT it_vbpa ASSIGNING FIELD-SYMBOL(<fs_vbpa>) WHERE parvw EQ 'SP'
                                        AND lifnr IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1_sp)
     WHERE lifnr EQ @<fs_vbpa>-lifnr.

    IF sy-subrc EQ 0.
      <fs_vbpa>-dlgrp = _wl_lfa1_sp-dlgrp.
    ENDIF.

  ENDLOOP.


ENDFORM.

FORM f_elimina_lfa1_bloq TABLES p_lfa1 STRUCTURE lfa1
                          USING p_bukrs TYPE bukrs.

  LOOP AT p_lfa1 INTO DATA(wl_lfa1).
    DATA(_delete) = ''.
    DATA(_tabix)  = sy-tabix.
    TRY.
        zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wl_lfa1-lifnr
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = p_bukrs ).
      CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
        _delete = 'X'.
    ENDTRY.
    IF _delete IS NOT INITIAL.
      DELETE p_lfa1 INDEX _tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.



"Comentario - 0001 - 18.04.2019 - Ini
*          READ TABLE IT_ZSDT0011_O INTO DATA(WA_ZSDT0011) WITH KEY TP_MOVIMENTO = WA_VBAK-TP_MOVIMENTO
*                                                                   AUART        = WA_VBAK-AUART BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            <OUT_ZSDT0001>-SHTYP = WA_ZSDT0011-SHTYP.
*
*            IF ( 'ZRDC_ZRFL' CS WA_ZSDT0011-AUART ) AND
*               ( WA_ZSDT0011-AUART IS NOT INITIAL ). "exceção para selecionar typo de transporte
*
*              SELECT VBELN PARVW LIFNR KUNNR
*                FROM VBPA
*                INTO TABLE IT_VBPA_2
*                WHERE VBELN  = <OUT_ZSDT0001>-VBELN
*                AND   PARVW  IN ('LR','Z1').
*
*              READ TABLE IT_VBPA_2 INTO WA_VBPA_2 WITH KEY PARVW = 'LR'.
*
*              SELECT SINGLE STCD1
*                FROM KNA1 INTO @DATA(V_STCD1K)
*                WHERE KUNNR   = @WA_VBPA_2-KUNNR.
*
*              READ TABLE IT_VBPA_2 INTO WA_VBPA_2 WITH KEY PARVW = 'Z1'.
*              CHECK SY-SUBRC = 0.
*              SELECT SINGLE STCD1
*                FROM LFA1 INTO @DATA(V_STCD1L)
*               WHERE LIFNR EQ @WA_VBPA_2-LIFNR.
*
*              IF V_STCD1K NE V_STCD1L.
*                <OUT_ZSDT0001>-SHTYP = 'Z001'.
*              ELSE.
*                <OUT_ZSDT0001>-SHTYP = 'Z004'.
*              ENDIF.
*            ENDIF.
*          ENDIF.
"Comentario - 0001 - 18.04.2019 - Fim


"Comentario - 0002 - 18.04.2019 - Inicio

*READ TABLE IT_ZSDT0011_P INTO WA_ZSDT0011 WITH KEY TP_MOVIMENTO = WA_EKKO-TP_MOVIMENTO
*                                                                 BSART        = WA_EKKO-BSART BINARY SEARCH.
*IF SY-SUBRC = 0.
*  <OUT_ZSDT0001>-SHTYP = WA_ZSDT0011-SHTYP.
*ENDIF.

"Comentario - 0002 - 18.04.2019 - Fim


"Comentario - 0003 - 18.04.2019 - Inicio
*SELECT SINGLE *
*  FROM ZSDT0011 INTO WA_ZSDT0011
* WHERE TP_MOVIMENTO = 'S'
*   AND BSART        = V_BSART.
*
*IF SY-SUBRC = 0.
*  <OUT_ZSDT0001>-SHTYP = WA_ZSDT0011-SHTYP.
*ENDIF.
"Comentario - 0003 - 18.04.2019 - Fim
*&---------------------------------------------------------------------*
*&      Form  F_TROCA_AGENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<OUT_ZSDT0001>_PLACA_CAV  text
*      -->P_<OUT_ZSDT0001>_BRANCH  text
*      <--P_V_LIFNR_SP  text
*----------------------------------------------------------------------*
FORM f_troca_agente  USING    p_placa_cav
                              p_branch
                     CHANGING p_lifnr.

  DATA: t_tvarvc    TYPE TABLE OF tvarvc,
        w_tvarvc    TYPE tvarvc,
        v_lifnr_ori TYPE lifnr.

  RANGES: r_bukrs FOR t001-bukrs.

  SELECT SINGLE bukrs
    INTO @DATA(v_bukrs)
    FROM j_1bbranch
    WHERE branch = @p_branch.

*-CS2022000236 - 06.03.2022 - JT - inicio
  FREE: r_bukrs.

*---------------------------------
* ler TVARVset
*---------------------------------
  SELECT *
    FROM tvarvc
    INTO TABLE t_tvarvc
   WHERE name = 'ZLES0136_BUKRS_CHANGE_AGENTE'.

  LOOP AT t_tvarvc INTO w_tvarvc.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_bukrs.
  ENDLOOP.

  CHECK v_bukrs IN r_bukrs[] AND r_bukrs[] IS NOT INITIAL.
* CHECK v_bukrs EQ '0001'. "somente Amaggi
*-CS2022000236 - 06.03.2022 - JT - fim

  TRY.
      zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_veiculo(
      EXPORTING i_placa = p_placa_cav IMPORTING
        e_tipo = DATA(e_tipo)
        e_proprietario = DATA(e_proprietario) ).
    CATCH zcx_faturamento .
    CATCH zcx_error .
  ENDTRY.
  IF e_tipo = 'P'.
*                  V_LIFNR_ORI.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_branch
      IMPORTING
        output = v_lifnr_ori.
    "
    SELECT SINGLE regio
      FROM lfa1
      INTO @DATA(_regio)
      WHERE lifnr = @v_lifnr_ori.
    "
    TRY.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
          EXPORTING
            i_tipo_agente           = '2'               "CS2022000236 - 25.02.2022 - JT - inicio
            i_bukrs                 = CONV #( v_bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
            i_placa                 = CONV #( p_placa_cav )
            i_uf_origem_mercadoria  = CONV #( _regio )
           IMPORTING
             e_agente_frete         = DATA(_agente) ).

        p_lifnr = _agente.
      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.
  ENDIF.

ENDFORM.

FORM f_call_files USING p_salva CHANGING pdf_result TYPE xstring.

  DATA: v_imp_doc          TYPE j_1bdocnum,
        v_url              TYPE zib_nfe,
        v_url_contrato     TYPE zcte_ciot-link_contrato,
        v_url_cpedagio     TYPE zcte_ciot-link_carga_pedagio,
        v_docnum_ref       TYPE zsdt0105-docnum_ref,
        v_url_mdfe         TYPE zsdt0102-url_sefaz,
        v_path             TYPE string,
        v_troca_nota       TYPE char1,
        v_naotem_doc       TYPE char1,
        t_doctos_faltantes TYPE zsdt_doctos_faltantes,
        w_doctos_faltantes TYPE zsde_doctos_faltantes.

  DATA: wa_file_table TYPE file_info,
        it_file_table TYPE STANDARD TABLE OF file_info.

  DATA: t_pdf_files TYPE zsdt_pdf_files,
        w_pdf_files TYPE zsde_pdf_files.

  DATA: lva_declaracao TYPE  xstring.

  CLEAR: tl_rows[].

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para gerar o arquivo' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR: sl_rows, wa_saida.
  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  CHECK sy-subrc = 0.

  FREE: t_pdf_files,
        t_doctos_faltantes,
        merged_document.

*-----------------------------------------
* obtem documentos faturamento
*-----------------------------------------
  TRY.
      t_pdf_files = zcl_faturamento=>zif_faturamento~get_instance(
                      )->get_documentos_faturamento( EXPORTING i_ch_referencia  = wa_saida-ch_referencia
                      ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

*-----------------------------------------
* valida se troca nota
*-----------------------------------------
* v_troca_nota = zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
*                   EXPORTING i_ch_referencia = wa_saida-ch_referencia ).

*--------------------------------------------
*-- valida arquivos obrigatorios
*--------------------------------------------
  TRY.
      v_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                       )->get_documentos_obrigatorios( EXPORTING i_ch_referencia    = wa_saida-ch_referencia
                                                                 t_pdf_files        = t_pdf_files
                                                       IMPORTING t_doctos_faltantes = t_doctos_faltantes
                       ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

  IF v_naotem_doc = abap_true.
    READ TABLE t_doctos_faltantes INTO w_doctos_faltantes INDEX 1.
    MESSAGE |{ w_doctos_faltantes-mensagem } para download| TYPE 'I'.
    EXIT.
  ENDIF.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  TRY.
      merged_document = zcl_faturamento=>zif_faturamento~get_instance(
                          )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                          ).

    CATCH zcx_faturamento.
    CATCH zcx_error.
  ENDTRY.

  "Essa opção de salvar é quando o usuário salva mas não envia e-mail
  IF p_salva = 'X'.

    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder      = v_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc EQ 0.
      PERFORM put_merged_file USING merged_document v_path  wa_saida-dt_movimento wa_saida-placa_cav
                                    wa_saida-ch_referencia  wa_saida-danfe. "-CS2021000218-16.11.2022-#99520-JT
    ENDIF.
  ELSE.
    pdf_result = merged_document.
  ENDIF.

ENDFORM.
FORM f_call_email.

  CLEAR: tl_rows[].
  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) NE 1.
    MESSAGE 'Selecione uma linha para enviar e-mail' TYPE 'I'.
    EXIT.
  ELSE.
    CALL SCREEN 400 STARTING AT 8 5
                     ENDING AT 70 6.
  ENDIF.
ENDFORM.
FORM table2xstring TABLES pt_data STRUCTURE rspolpbi
                   USING  p_data TYPE xstring
                          p_len TYPE i.

  DATA l_rest TYPE i.
  DATA l_chunk TYPE i.

  l_rest = p_len.

  CLEAR p_data.
  LOOP AT pt_data INTO DATA(wa).
    IF l_rest < 128.
      l_chunk = l_rest.
    ELSE.
      l_chunk = 128.
    ENDIF.

    CONCATENATE p_data wa-data(l_chunk) INTO p_data IN BYTE MODE.
    SUBTRACT l_chunk FROM l_rest.
  ENDLOOP.

ENDFORM.

FORM merge_pdfs USING pt_files TYPE t_fileinfotab
                CHANGING pdf_merger TYPE REF TO cl_rspo_pdf_merge
                         merged_document
                         docindex
                         errordoc
                         rc.

  DATA: wa TYPE LINE OF t_fileinfotab.

* Add documents to attribut table of PDF merger
  LOOP AT pt_files INTO wa.
    pdf_merger->add_document( wa-data ).
  ENDLOOP.

* Call kernel method to do the merge of the specified files.
  pdf_merger->merge_documents( IMPORTING merged_document = merged_document rc = rc ).

* Get index of failed document
  IF rc <> 0.
    pdf_merger->get_err_doc_index( IMPORTING index = docindex ).
    pdf_merger->get_document( EXPORTING index = docindex IMPORTING document = errordoc ).
  ENDIF.

  CLEAR pdf_merger.

ENDFORM.
FORM put_merged_file USING merged_document TYPE xstring
                           v_path TYPE string
                           v_dt_movimento TYPE ty_saida-dt_movimento
                           v_placa TYPE ty_saida-placa_cav
                           v_ch_referencia   "-CS2021000218-27.12.2022-#99520-JT
                           v_danfe.          "*-CS2021000218-27.12.2022-#99520-JT


  DATA: bin_tab TYPE STANDARD TABLE OF tabl1024.
  DATA: lo_gui TYPE REF TO cl_gui_frontend_services.
  DATA: path     TYPE string,
        fullpath TYPE string.
  DATA: length TYPE i.
  DATA: l_nrocg  TYPE char10.
  DATA: filter TYPE string, uact TYPE i, name TYPE string.

  CREATE OBJECT lo_gui.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = merged_document
    IMPORTING
      output_length = length
    TABLES
      binary_tab    = bin_tab.


*-CS2021000218-16.05.2023-#112208-JT-inicio
  IF vg_cockpit = '06'.
    IF wa_saida-danfe(1) <> '@'.
      SELECT SINGLE nfenum
        INTO @DATA(l_nfenum)
        FROM j_1bnfdoc
       WHERE docnum = @wa_saida-danfe.
      IF sy-subrc <> 0.
        CLEAR l_nfenum.
      ENDIF.
    ENDIF.

    PACK wa_saida-nro_cg TO l_nrocg.
    PACK l_nfenum        TO l_nfenum.

    CONDENSE: l_nrocg, l_nfenum.

*    fullpath = v_path && '\' && v_dt_movimento+6(2) && '.' && v_dt_movimento+4(2) && '.' && v_dt_movimento(4)
*                      && '_' && l_nrocg && '_' && l_nfenum && '.pdf'.
*-CS2021000218-16.05.2023-#112208-JT-fim

    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- INICIO
    SELECT SINGLE vkbur FROM vbak INTO @DATA(v_vkbur) WHERE vbeln EQ @wa_saida-vbeln.
    "fullpath = v_path && '\' && 'NF' && '_' && l_nfenum  && '_' && wa_saida-name1 && '_' && wa_vbak-vkbur && '.pdf'. "RJF
    fullpath = v_path && '\' && 'NF' && '_' && l_nfenum  && '_' && wa_saida-name1 && '_' && v_vkbur && '.pdf'.
    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- FIM
  ELSE.
    CONCATENATE v_path '\' v_dt_movimento+6(2) '.' v_dt_movimento+4(2) '.' v_dt_movimento(4) '_' v_placa
                       '_' sy-datum  '_' sy-uzeit  "*-CS2021000218-27.12.2022-#99520-JT
                       '.pdf' INTO fullpath.
  ENDIF.

  "CONDENSE fullpath NO-GAPS. Com a entrada do OneDrive por padrão, as pastas sincronizadas ficam com espaço, não salvando o arquivo

  lo_gui->gui_download( EXPORTING  filename     = fullpath
                                   filetype     = 'BIN'
                                   bin_filesize = length
                        CHANGING   data_tab     = bin_tab "). "*-CS2021000218-27.12.2022-#99520-JT-inicio
                        EXCEPTIONS               "*-CS2021000218-27.12.2022-#99520-JT-inicio
                                   OTHERS       = 1 ).        "*-CS2021000218-27.12.2022-#99520-JT-inicio
*-CS2021000218-27.12.2022-#99520-JT-inicio
  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Arquivo não pode ser Gerado: ' fullpath DISPLAY LIKE 'E'.
  ENDIF.
*-CS2021000218-27.12.2022-#99520-JT-fim

ENDFORM.
FORM gera_email .

  DATA : wa_doc_data LIKE sodocchgi1,
         wa_reciever LIKE somlreci1,
         t_reciever  LIKE STANDARD TABLE OF wa_reciever,
         wa_pk_list  LIKE sopcklsti1,
         t_pk_list   LIKE STANDARD TABLE OF wa_pk_list,
         wa_con_txt  LIKE solisti1,
         t_temp      LIKE STANDARD TABLE OF wa_con_txt,
         t_con_txt   LIKE STANDARD TABLE OF wa_con_txt,
         v_date(10),
         v_lin       LIKE sy-tabix,
         wa_fil      TYPE ty_fileinfo.

  CLEAR: sl_rows, wa_saida.
  READ TABLE tl_rows INTO sl_rows INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

  SELECT SINGLE vkbur FROM vbak INTO @DATA(v_vkbur) WHERE vbeln EQ @wa_saida-vbeln.
  CHECK sy-subrc = 0.

  "CRIA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line' <H4 ALIGN=LEFT>Segue em anexo os Documentos de Faturamento referente a viagem abaixo .</H4>'.

  CONCATENATE wa_saida-dt_movimento+6(2)  '.'  wa_saida-dt_movimento+4(2) '.' wa_saida-dt_movimento+0(4) INTO v_date.

  CLEAR: wa_con_txt.
  CONCATENATE '<p>' 'Data Movimento: '  v_date   '.<br>' INTO wa_con_txt SEPARATED BY space.
  new_line   wa_con_txt.

  CLEAR: wa_con_txt.
  CONCATENATE 'Romaneio:  'wa_saida-nr_romaneio '.<br>' INTO  wa_con_txt SEPARATED BY space.
  new_line    wa_con_txt.

  CLEAR: wa_con_txt.
  CONCATENATE 'Placa: '  wa_saida-placa_cav '.<br>' INTO  wa_con_txt SEPARATED BY space.
  new_line    wa_con_txt.

  CLEAR: wa_con_txt.
  CONCATENATE 'Escritório Venda: '  v_vkbur '.<br>' INTO  wa_con_txt SEPARATED BY space.
  new_line    wa_con_txt.

  CLEAR: wa_con_txt.


*-CS2021000218-27.12.2022-#99520-JT-inicio
  IF vg_cockpit = '06'.

    CONCATENATE 'Nro. Documento: ' wa_saida-vbeln '.<br>'  INTO  wa_con_txt SEPARATED BY space.
  ELSE.
    CONCATENATE 'Nro. Documento: ' wa_saida-vbeln '.</p>'  INTO  wa_con_txt SEPARATED BY space.
  ENDIF.
*-CS2021000218-27.12.2022-#99520-JT-fim

  new_line    wa_con_txt.


*-CS2021000218-27.12.2022-#99520-JT-inicio
  IF vg_cockpit = '06'.
    IF wa_saida-danfe(1) <> '@'.
      SELECT SINGLE nfenum
        INTO @DATA(l_nfenum)
        FROM j_1bnfdoc
       WHERE docnum = @wa_saida-danfe.
      IF sy-subrc <> 0.
        CLEAR l_nfenum.
      ENDIF.
    ELSE.
      CLEAR l_nfenum.
    ENDIF.

    SHIFT l_nfenum LEFT DELETING LEADING '0'.
    CONDENSE l_nfenum.

    CLEAR: wa_con_txt.
    CONCATENATE 'Nota Fiscal: ' l_nfenum '.</p>'  INTO  wa_con_txt SEPARATED BY space.
    new_line    wa_con_txt.
  ENDIF.
*-CS2021000218-27.12.2022-#99520-JT-fim



  new_line '</BODY>'.
  new_line '</HTML>'.

  DESCRIBE TABLE t_con_txt LINES v_lin.

  READ TABLE t_con_txt INDEX v_lin INTO wa_con_txt.
  wa_doc_data-doc_size = ( v_lin - 1 ) * 255 + strlen( wa_con_txt ).

  CLEAR wa_pk_list-transf_bin.
  wa_pk_list-head_start = 1.
  wa_pk_list-head_num = 0.
  wa_pk_list-body_start = 1.
  wa_pk_list-body_num = v_lin.
  wa_pk_list-doc_type = 'HTM'.
  APPEND wa_pk_list TO t_pk_list.

  CLEAR: pdf_result.
  PERFORM f_call_files USING '' CHANGING pdf_result.

  IF pdf_result IS NOT INITIAL.


*-CS2021000218-27.12.2022-#99520-JT-inicio
    IF vg_cockpit = '06'.
      "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- INICIO

      CONCATENATE 'Faturamento Defensivos: NF' l_nfenum '-' wa_saida-name1 '-' v_vkbur
      INTO wa_doc_data-obj_descr SEPARATED BY space.
      CONCATENATE 'NF' '_' l_nfenum '_' wa_saida-name1 '_' v_vkbur '.pdf'
      INTO DATA(l_texto).
    ELSE.
      CONCATENATE 'Faturamento da Viagem :'  v_date '_'  wa_saida-placa_cav '.pdf' INTO wa_doc_data-obj_descr.
    ENDIF.

    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- FIM
*-CS2021000218-27.12.2022-#99520-JT-fim



    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = pdf_result
      TABLES
        binary_tab = t_temp.

    DESCRIBE TABLE t_temp LINES v_lin.

    wa_pk_list-transf_bin = 'X'.
    wa_pk_list-head_num   = 1.
    wa_pk_list-body_start = 1.
    wa_pk_list-body_num   = v_lin.
    wa_pk_list-doc_type   = 'PDF'.
    wa_pk_list-obj_name   = 'ATTACHMENT'.
    wa_pk_list-obj_descr  = l_texto.
    wa_pk_list-doc_size   = v_lin * 255.

    APPEND wa_pk_list TO t_pk_list..


    LOOP AT s_email.
      CLEAR: wa_reciever, t_reciever.

      wa_reciever-receiver = s_email-low.
      wa_reciever-rec_type = 'U'.
      wa_reciever-com_type = 'INT'.
      APPEND wa_reciever TO t_reciever.

      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = wa_doc_data
          put_in_outbox              = 'X'
          commit_work                = 'X'
        TABLES
          packing_list               = t_pk_list
          contents_bin               = t_temp
          contents_txt               = t_con_txt
          receivers                  = t_reciever
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.

*----CS2021000508 - 07.06.2021 - JT - inicio
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE 'Email(s) enviado(s) com sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.
    ENDLOOP.
  ELSE.
*   MESSAGE 'Erro a Gerar Arquivo PDF' TYPE 'I'.
    EXIT.
*----CS2021000508 - 07.06.2021 - JT - fim
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_files TABLES it_file_table STRUCTURE file_info USING lva_declaracao.

  DATA : http_client TYPE REF TO if_http_client.
  DATA : url TYPE string.
  DATA : content TYPE xstring.
  DATA : wa_fil TYPE ty_fileinfo.
  DATA : l_dados_selo TYPE xstring.
  DATA : l_docnum     TYPE j_1bnfdoc-docnum.
  DATA: wa_file_table TYPE file_info.
  DATA: l_id_ordem     TYPE zde_id_ordem.
  DATA: l_nr_safra     TYPE zsdt0001od-nr_safra.
  DATA: l_nr_safra2    TYPE zpmt0054-safra.

  FREE: l_id_ordem,
        l_nr_safra.

  CLEAR: it_pdffiles.

  LOOP AT it_file_table INTO wa_file_table.

    url =  wa_file_table-filename.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc = 0.

      http_client->send( ).
      http_client->receive( ).
      content = http_client->response->get_data( ).
      http_client->close( ).

      wa_fil-filename = wa_file_table-filename.
      wa_fil-data = content.
      wa_fil-len = xstrlen( content ).

      APPEND wa_fil TO it_pdffiles.
      CLEAR: wa_fil.
    ENDIF.

  ENDLOOP.

*** Dar o append na carta.
  IF lva_declaracao IS NOT INITIAL.
    wa_fil-filename = 'Declaração do Motorista'.
    wa_fil-data = lva_declaracao.
    wa_fil-len = xstrlen( lva_declaracao ).

    APPEND wa_fil TO it_pdffiles.
    CLEAR: wa_fil.
  ENDIF.
*** fim append.

*-CS2020000656 - 13.05.2021 - JT - inicio
  l_docnum   = wa_saida-danfe.
  l_id_ordem = wa_saida-id_ordem.

  IF l_id_ordem IS INITIAL.
    SELECT id_ordem
      INTO l_id_ordem
      FROM zsdt0001
        UP TO 1 ROWS
     WHERE ch_referencia = wa_saida-ch_referencia
       AND tp_movimento  = wa_saida-tp_movimento.
    ENDSELECT.
  ENDIF.

  SELECT nr_safra
    INTO l_nr_safra
    FROM zsdt0001od
      UP TO 1 ROWS
   WHERE id_ordem = l_id_ordem.
  ENDSELECT.

  l_nr_safra2 = l_nr_safra.

  CALL FUNCTION 'ZSD_IMPRIME_SELO'
    EXPORTING
      i_docnum                 = l_docnum
      i_safra                  = l_nr_safra2
      i_imprime_selo           = ' '
    IMPORTING
      e_xstring_document       = l_dados_selo
    EXCEPTIONS
      documento_nao_autorizado = 1
      documento_nao_imprimir   = 2
      OTHERS                   = 3.

  IF sy-subrc = 0 AND l_dados_selo IS NOT INITIAL.
    wa_fil-filename = 'Selo'.
    wa_fil-data     = l_dados_selo.
    wa_fil-len      = xstrlen( l_dados_selo ).
    APPEND wa_fil  TO it_pdffiles.
    CLEAR: wa_fil.
  ENDIF.
*-CS2020000656 - 13.05.2021 - JT - inicio

ENDFORM.


FORM f_set_encerramento_docs CHANGING p_saida TYPE ty_saida.

  CHECK  p_saida-ch_referencia IS NOT INITIAL.

  IF p_saida-transp IS NOT INITIAL AND ( p_saida-transp(1) NE '@' ).

    TRY.

        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
         EXPORTING
            i_tknum            = CONV #( p_saida-transp )
         IMPORTING
            e_tipo_veiculo     =  p_saida-tipo_veiculo
            e_tipo_remetente   =  p_saida-tipo_remetente
            e_tp_frete         =  p_saida-tp_frete
            e_nota_fiscal      =  p_saida-emite_nota_fiscal
            e_pedagio          =  p_saida-emite_pedagio
            e_doc_custo        =  p_saida-emite_doc_custo
            e_doc_trans        =  p_saida-emite_doc_trans
            e_conhecimento     =  p_saida-emite_conhecimento
            e_pag_frete        =  p_saida-emite_pag_frete
            e_manifesto        =  p_saida-emite_manifesto
            e_seguro_frete     =  p_saida-emite_seguro_frete  ).

      CATCH zcx_faturamento INTO DATA(_zcx_fat).
      CATCH zcx_error       INTO DATA(_zcx_error).
    ENDTRY.

  ELSE.

    TRY.

        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
          EXPORTING
            i_ch_romaneio      =  CONV #( wa_zsdt0001-ch_referencia )
          IMPORTING
            e_tipo_veiculo     =  p_saida-tipo_veiculo
            e_tipo_remetente   =  p_saida-tipo_remetente
            e_tp_frete         =  p_saida-tp_frete
            e_nota_fiscal      =  p_saida-emite_nota_fiscal
            e_pedagio          =  p_saida-emite_pedagio
            e_doc_custo        =  p_saida-emite_doc_custo
            e_doc_trans        =  p_saida-emite_doc_trans
            e_conhecimento     =  p_saida-emite_conhecimento
            e_pag_frete        =  p_saida-emite_pag_frete
            e_manifesto        =  p_saida-emite_manifesto
            e_seguro_frete     =  p_saida-emite_seguro_frete  ).

      CATCH zcx_faturamento INTO _zcx_fat.
      CATCH zcx_error       INTO _zcx_error.
    ENDTRY.

  ENDIF.

  CASE vg_cockpit.
    WHEN '04'.

      CASE p_saida-tp_frete.
        WHEN 'CIF'.

          IF ( p_saida-tipo_veiculo EQ 'P' AND p_saida-tipo_remetente EQ 'P' ).

            "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
            p_saida-enc_doc_custo = abap_true.

*            CASE p_saida-tipo.
*              WHEN 'P'. "Pedido
*                p_saida-enc_doc_custo = abap_true.
*              WHEN 'T' OR "Transferencia
*                   'O'.   "Ordem Venda
*                p_saida-enc_danfe     = abap_true.
*            ENDCASE.

          ENDIF.

        WHEN 'FOB' OR 'CFR'.

*         IF p_saida-TIPO_VEICULO EQ 'P'.
*           p_saida-ENC_CONHECIMENTO = ABAP_TRUE.
*         ENDIF.

      ENDCASE.

    WHEN OTHERS.

      CASE p_saida-tp_frete.
        WHEN 'CIF'.

          IF ( p_saida-tipo_veiculo EQ 'P' AND p_saida-tipo_remetente EQ 'P' ).
            p_saida-enc_doc_custo = abap_true.
          ENDIF.

        WHEN 'FOB' OR 'CFR'.

*      IF p_saida-TIPO_VEICULO EQ 'P'.
*        p_saida-ENC_CONHECIMENTO = ABAP_TRUE.
*      ENDIF.
      ENDCASE.

  ENDCASE.

  IF p_saida-emite_conhecimento EQ abap_true.
    p_saida-enc_doc_custo = abap_false.
  ENDIF.


ENDFORM.
*FORM f_call_nota_parceiro.
*
*  CLEAR: tl_rows[].
*
*  CALL METHOD cl_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = tl_rows.
*
*  IF lines( tl_rows[] ) NE 1.
*    MESSAGE 'Selecione uma linha para exibir Nota Parceiro' TYPE 'I'.
*    EXIT.
*  ENDIF.
*
*  CLEAR: sl_rows, wa_saida.
*  READ TABLE tl_rows INTO sl_rows INDEX 1.
*  READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.
*
*  CHECK sy-subrc = 0.
*
**-----------------------------------------
** Chamar programa para listar Nota Parceiro
**-----------------------------------------
*  SUBMIT zsdr0138
*        WITH p_vbeln = wa_saida-ch_referencia AND RETURN.
*
*ENDFORM.

FORM f_valida_placas_faturamento USING p_saida TYPE ty_saida
                              CHANGING p_error
                               RAISING zcx_error. "*-#133089-12.02.2024-JT

  DATA: v_valida_placa TYPE p.

*-#133089-21.02.2024-JT-inicio
  CREATE OBJECT lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  CLEAR: p_error.

  IF p_saida-placa_cav IS NOT INITIAL.

    CALL FUNCTION 'Z_MASC_PLACA_VEICULO'
      EXPORTING
        i_placa  = p_saida-placa_cav
      IMPORTING
        e_return = v_valida_placa.

    IF v_valida_placa NE 0.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          p_error = abap_true.
          MESSAGE |Formato da placa: { p_saida-placa_cav } é inválido.| TYPE 'I'.
          EXIT.
        WHEN abap_true.
          DATA(l_mesg) = |Formato da placa: { p_saida-placa_cav } é inválido.|.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

  ENDIF.

  IF p_saida-placa_car1 IS NOT INITIAL.

    CALL FUNCTION 'Z_MASC_PLACA_VEICULO'
      EXPORTING
        i_placa  = p_saida-placa_car1
      IMPORTING
        e_return = v_valida_placa.

    IF v_valida_placa NE 0.
      p_error = abap_true.
      MESSAGE |Formato da placa: { p_saida-placa_car1 } é inválido.| TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.

  IF p_saida-placa_car2 IS NOT INITIAL.

    CALL FUNCTION 'Z_MASC_PLACA_VEICULO'
      EXPORTING
        i_placa  = p_saida-placa_car2
      IMPORTING
        e_return = v_valida_placa.

    IF v_valida_placa NE 0.
      p_error = abap_true.
      MESSAGE |Formato da placa: { p_saida-placa_car2 } é inválido.| TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.

  IF p_saida-placa_car3 IS NOT INITIAL.

    CALL FUNCTION 'Z_MASC_PLACA_VEICULO'
      EXPORTING
        i_placa  = p_saida-placa_car3
      IMPORTING
        e_return = v_valida_placa.

    IF v_valida_placa NE 0.
      p_error = abap_true.
      MESSAGE |Formato da placa: { p_saida-placa_car3 } é inválido.| TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.


ENDFORM.

FORM f_get_doc_rem_romaneio CHANGING c_saida        TYPE ty_saida
                                     c_zsdt0001_new TYPE zsdt0001.

  IF c_saida-remessa EQ icon_execute_object OR c_saida-remessa IS INITIAL.

    c_zsdt0001_new-status = abap_false.

    v_xblnr = c_saida-ch_referencia.
    SELECT SINGLE *
      FROM likp INTO wa_likp
     WHERE xblnr = v_xblnr
       AND spe_loekz = ''.
    IF sy-subrc = 0.
      c_saida-remessa        = wa_likp-vbeln.
      c_zsdt0001_new-doc_rem = wa_likp-vbeln.
      c_zsdt0001_new-status  = abap_true.
    ENDIF.

  ELSEIF c_saida-remessa IS NOT INITIAL AND c_saida-remessa(1) NE '@' .

    c_zsdt0001_new-status = abap_true.

    CLEAR: wa_likp.

    DO 5 TIMES.
      SELECT SINGLE *
        FROM likp INTO wa_likp
       WHERE vbeln = c_saida-remessa
         AND spe_loekz = ''.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.
    ENDDO.

    v_xblnr = wa_likp-xblnr+0(20).
    IF ( sy-subrc NE 0 ) OR ( v_xblnr NE c_saida-ch_referencia ).
      IF c_saida-tipo = 'O'.
        c_saida-remessa = icon_execute_object.
      ELSE.
        c_saida-remessa = icon_icon_list.
      ENDIF.

      c_zsdt0001_new-doc_rem = ''.
      c_zsdt0001_new-status  = abap_false.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_get_fat_nf_romaneio CHANGING c_saida        TYPE ty_saida
                                      c_zsdt0001_new TYPE zsdt0001.

  DATA: lva_vbeln_rom TYPE likp-vbeln. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL

  IF ( c_saida-fatura  EQ icon_execute_object OR  c_saida-fatura  IS INITIAL ) AND
     ( c_saida-remessa IS NOT INITIAL AND c_saida-remessa(1) NE '@' ).

    "US #66690 - WPP - Ini
    IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'.

      SELECT SINGLE a~mblnr a~mjahr
        FROM mkpf AS a INTO ( vl_vbeln, vl_mjahr )
       WHERE a~zch_referencia = c_saida-ch_referencia
         AND NOT EXISTS ( SELECT b~mblnr
                            FROM mseg AS b
                           WHERE b~smbln EQ a~mblnr "estorno
                          ).
    ELSE.
      "US #66690 - WPP - Fim

      IF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).
        SELECT SINGLE a~vbeln a~mjahr
          FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
         WHERE a~vbelv = c_saida-remessa
           AND a~vbtyp_n  = 'R'
           AND a~vbtyp_v  = 'J'
           AND NOT EXISTS ( SELECT *
                              FROM mseg AS b
                             WHERE b~smbln EQ a~vbeln "estorno
                           ).
      ELSE.
        SELECT SINGLE a~vbeln a~mjahr
          FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
         WHERE a~vbelv = c_saida-remessa
           AND a~vbtyp_n  = 'M'
           AND a~vbtyp_v  = 'J'
           AND NOT EXISTS ( SELECT *
                              FROM vbfa AS b
                             WHERE b~vbelv   = a~vbeln
                               AND b~vbtyp_n = 'N' "estorno
                           ).
      ENDIF.

    ENDIF.

    IF sy-subrc = 0.
      c_saida-fatura               = vl_vbeln.
      c_zsdt0001_new-fatura_prod = vl_vbeln.

      IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'.
        c_zsdt0001_new-doc_material = vl_vbeln.
        c_zsdt0001_new-ano_material = vl_mjahr.
      ENDIF.

    ELSE.
      c_zsdt0001_new-fatura_prod = ''.

      c_saida-danfe = icon_execute_object.
      c_zsdt0001_new-nro_nf_prod = ''.

      IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'.
        CLEAR: c_zsdt0001_new-doc_material, c_zsdt0001_new-ano_material.
      ENDIF.

    ENDIF.

  ELSEIF ( c_saida-fatura IS NOT INITIAL AND c_saida-fatura(1) NE '@' ).

    "US #66690 - WPP - Ini
    IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'.

      DO 5 TIMES.

        SELECT SINGLE a~mblnr a~mjahr
          FROM mkpf AS a INTO ( vl_vbeln, vl_mjahr )
         WHERE a~zch_referencia = c_saida-ch_referencia
           AND NOT EXISTS ( SELECT b~mblnr
                              FROM mseg AS b
                             WHERE b~smbln EQ a~mblnr "estorno
                            ).
        IF sy-subrc EQ 0.
          EXIT.
        ELSE.
          WAIT UP TO 2 SECONDS.
          sy-subrc = 4.
        ENDIF.
      ENDDO.

    ELSE.
      "US #66690 - WPP - Fim

      IF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).

        DO 5 TIMES.
          SELECT SINGLE *
            FROM mkpf INTO @DATA(wl_mkpf)
           WHERE mblnr EQ @c_saida-fatura.

          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO 2 SECONDS.
            sy-subrc = 4.
          ENDIF.
        ENDDO.

      ELSE.

        DO 5 TIMES.
          SELECT SINGLE fksto
            FROM vbrk INTO vl_fksto
           WHERE vbeln = c_saida-fatura.

          IF sy-subrc EQ 0.
            EXIT.
          ELSE.
            WAIT UP TO 2 SECONDS.
            sy-subrc = 4.
          ENDIF.
        ENDDO.

      ENDIF.

    ENDIF.

    IF sy-subrc NE 0. "Fatura não existe
      c_saida-fatura = icon_execute_object.
      c_zsdt0001_new-fatura_prod = ''.

      IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'.
        CLEAR: c_zsdt0001_new-doc_material, c_zsdt0001_new-ano_material.
      ENDIF.

    ELSE.

      IF c_saida-tipo = 'P' AND c_saida-bsart EQ 'ZARM'. "US #66690 - WPP - Ini
        EXIT.
      ENDIF.

      "Check Estorno Documento
      IF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).
        SELECT SINGLE *
          FROM mseg INTO @DATA(wl_seg_estorno)
         WHERE smbln EQ @c_saida-fatura.
      ELSE.
        SELECT SINGLE vbeln mjahr "se estiver estornada
          FROM vbfa INTO (vl_vbeln,vl_mjahr)
         WHERE vbelv = c_saida-fatura
           AND vbtyp_n  = 'N'. "estorno
      ENDIF.

      IF sy-subrc = 0. "Achou estorno
        CLEAR wl_erro.
        PERFORM f_chk_estorno_fiscal USING c_saida
                                           ''
                                  CHANGING wl_erro.

        IF wl_erro IS INITIAL.
          c_saida-fatura = icon_execute_object.
          c_zsdt0001_new-fatura_prod = ''.
        ENDIF.
      ELSE.

        lva_vbeln_rom = c_saida-remessa.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_vbeln_rom
          IMPORTING
            output = lva_vbeln_rom.


        IF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).
          SELECT SINGLE vbelv mjahr
            FROM vbfa INTO (vl_vbeln,vl_mjahr)
           WHERE vbeln = c_saida-fatura
             AND vbtyp_n  = 'R'
             AND vbtyp_v  = 'J'
             AND vbelv    = lva_vbeln_rom. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL
        ELSE.
          SELECT SINGLE vbelv mjahr
            FROM vbfa INTO (vl_vbeln,vl_mjahr)
           WHERE vbeln = c_saida-fatura
             AND vbtyp_n  = 'M'
             AND vbtyp_v  = 'J'
             AND vbelv    = lva_vbeln_rom. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL
        ENDIF.



        IF sy-subrc NE 0. "fatura é de outra remessa "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL

          c_saida-fatura = icon_execute_object.
          c_zsdt0001_new-fatura_prod = ''.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM f_get_danfe_romaneio CHANGING c_saida        TYPE ty_saida
                                     c_zsdt0001_new TYPE zsdt0001.


  IF ( c_saida-danfe  EQ icon_execute_object OR c_saida-danfe  IS INITIAL ) AND
     ( c_saida-fatura IS NOT INITIAL AND c_saida-fatura(1) NE '@' ) .

    IF ( c_saida-tipo = 'P' ) AND ( c_saida-bsart = 'ZARM') . "US #66690 - WPP

      SELECT SINGLE *
        FROM mkpf INTO @DATA(lwa_mkpf_saida)
       WHERE mblnr EQ @c_zsdt0001_new-doc_material.

      CHECK ( sy-subrc EQ 0 ) AND ( c_zsdt0001_new-doc_material IS NOT INITIAL ).

      CONCATENATE lwa_mkpf_saida-mblnr lwa_mkpf_saida-mjahr INTO vl_refkey.

      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = vl_refkey.

    ELSEIF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).
      CLEAR: vl_vbeln, vl_mjahr.
      SELECT SINGLE vbeln mjahr
        FROM vbfa INTO (vl_vbeln,vl_mjahr)
       WHERE vbelv = c_saida-remessa
         AND vbtyp_n  = 'R'
         AND vbtyp_v  = 'J'.

      CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = vl_refkey.
    ELSE.
      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = c_saida-fatura.
    ENDIF.

    IF sy-subrc = 0.

      PERFORM f_check_auth_doc USING vl_docnum.

      IF sy-subrc = 0.
        c_saida-danfe = vl_docnum.
        c_zsdt0001_new-nro_nf_prod = vl_docnum.
      ENDIF.
    ENDIF.

  ELSEIF ( c_saida-danfe  IS NOT INITIAL AND c_saida-danfe(1)  NE '@' ) AND
         ( c_saida-fatura IS NOT INITIAL AND c_saida-fatura(1) NE '@' ).

    IF ( c_saida-tipo = 'P' ) AND ( c_saida-bsart = 'ZARM') . "US #66690 - WPP

      SELECT SINGLE *
        FROM mkpf INTO lwa_mkpf_saida
       WHERE mblnr EQ c_zsdt0001_new-doc_material.

      CHECK ( sy-subrc EQ 0 ) AND ( c_zsdt0001_new-doc_material IS NOT INITIAL ).

      CONCATENATE lwa_mkpf_saida-mblnr lwa_mkpf_saida-mjahr INTO vl_refkey.

      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = vl_refkey.

    ELSEIF ( c_saida-tipo = 'P' ) OR ( c_saida-tipo = 'T' ).
      CLEAR: vl_vbeln, vl_mjahr.
      SELECT SINGLE vbeln mjahr
        FROM vbfa INTO (vl_vbeln,vl_mjahr)
       WHERE vbelv = c_saida-remessa
         AND vbtyp_n  = 'R'
         AND vbtyp_v  = 'J'.

      CONCATENATE vl_vbeln vl_mjahr INTO vl_refkey.
      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = vl_refkey.
    ELSE.
      SELECT SINGLE docnum
        FROM j_1bnflin INTO vl_docnum
       WHERE refkey = c_saida-fatura.
    ENDIF.

    IF sy-subrc = 0.
      "Danfe cancelada  OU fatura/remessa não é da DANFE.

      PERFORM f_check_canc_doc USING vl_docnum.

      IF ( sy-subrc = 0 ) OR
         ( vl_docnum NE c_saida-danfe ).
        c_saida-danfe = icon_execute_object.
        c_zsdt0001_new-nro_nf_prod = ''.
      ENDIF.
      "WPP - Ajuste Preenchimento Danfe incorreta no Romaneio - 05-07-2023  - Ini
    ELSE.
      c_saida-danfe = icon_execute_object.
      c_zsdt0001_new-nro_nf_prod = ''.
    ENDIF.
  ELSEIF c_saida-fatura EQ icon_execute_object.
    c_saida-danfe = icon_execute_object.
    c_zsdt0001_new-nro_nf_prod = ''.
  ENDIF.

ENDFORM.

FORM f_get_tknum_romaneio CHANGING c_saida        TYPE ty_saida
                                   c_zsdt0001_new TYPE zsdt0001.

  DATA: v_vbeln        TYPE likp-vbeln.

  SELECT SINGLE *
    FROM tvtk INTO @DATA(wl_tvtk)
   WHERE shtyp = @c_saida-shtyp.

  CHECK ( sy-subrc EQ 0 ) OR ( wl_tvtk-abfer IS NOT INITIAL ).

  CLEAR: v_tknum, v_vbeln.

  CASE wl_tvtk-abfer.
    WHEN '1' OR '3'. "Saída
      IF ( c_saida-remessa    IS NOT INITIAL ) AND
         ( c_saida-remessa(1) NE '@'         ).
        v_vbeln         = c_saida-remessa.
        DATA(_vbtyp_v)  = 'J'.
      ENDIF.
    WHEN '2' OR '4'. "Entrada
      IF ( c_saida-aviso      IS NOT INITIAL ) AND
         ( c_saida-aviso(1)   NE '@'         ).
        v_vbeln   = c_saida-aviso.
        _vbtyp_v  = '7'.
      ENDIF.
  ENDCASE.

  IF ( c_saida-transp EQ icon_execute_object ) AND
     ( v_vbeln IS NOT INITIAL ) .
    SELECT SINGLE vttk~tknum INTO v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                                AND vttk~vsart = wl_tvtk-vsart
     WHERE vbfa~vbelv    = v_vbeln
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = _vbtyp_v.
    IF sy-subrc = 0.
      c_saida-transp = v_tknum.
      c_zsdt0001_new-doc_transp = v_tknum.
    ENDIF.
  ELSEIF ( c_saida-transp IS NOT INITIAL AND c_saida-transp(1) NE '@' ).

    DO 5 TIMES.
      SELECT SINGLE vttk~tknum INTO v_tknum
        FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
                                AND vttk~vsart = wl_tvtk-vsart
       WHERE vbfa~vbelv    = v_vbeln
         AND vbfa~vbtyp_n  = '8'
         AND vbfa~vbtyp_v  = _vbtyp_v.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.
    ENDDO.

    IF sy-subrc NE 0.
      c_saida-transp = icon_execute_object.
      c_zsdt0001_new-doc_transp = ''.
    ELSEIF c_saida-transp NE v_tknum.
      c_saida-transp = v_tknum.
      c_zsdt0001_new-doc_transp = v_tknum.
    ENDIF.
  ENDIF.



ENDFORM.

FORM f_get_fknum_romaneio CHANGING c_saida        TYPE ty_saida
                                     c_zsdt0001_new TYPE zsdt0001.

  IF ( c_saida-doccus+0(1) = '@' ) AND
     ( c_saida-transp IS NOT INITIAL AND c_saida-transp(1) NE '@' ).
    SELECT SINGLE fknum
      FROM vfkp INTO vl_fknum
     WHERE rebel = c_saida-transp.
    IF sy-subrc = 0.
      c_saida-doccus = vl_fknum.
      c_zsdt0001_new-fknum = vl_fknum.
    ENDIF.
  ELSEIF ( c_saida-doccus IS NOT INITIAL AND c_saida-doccus(1) NE '@' ).

    DO 5 TIMES.
      SELECT SINGLE fknum
        FROM vfkp INTO vl_fknum
       WHERE rebel = c_saida-transp.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.
    ENDDO.

    IF sy-subrc NE 0.
      c_saida-doccus = icon_icon_list.
      c_zsdt0001_new-fknum = ''.
    ELSEIF c_saida-doccus NE vl_fknum.
      c_saida-doccus = vl_fknum.
      c_zsdt0001_new-fknum = vl_fknum.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_get_ov_serv_romaneio CHANGING c_saida        TYPE ty_saida
                                       c_zsdt0001_new TYPE zsdt0001.

  IF ( c_saida-ovserv+0(1) = '@' ) AND
     ( c_saida-transp IS NOT INITIAL AND c_saida-transp(1) NE '@' ).
    SELECT SINGLE  vbeln auart  kunnr
      FROM vbak INTO wa_vbak
     WHERE tknum = c_saida-transp.
    IF sy-subrc = 0.
      c_saida-ovserv = wa_vbak-vbeln.
      c_zsdt0001_new-ov_frete = wa_vbak-vbeln.
    ENDIF.
  ELSEIF c_saida-ovserv IS NOT INITIAL AND c_saida-ovserv(1) NE '@'.

    DO 5 TIMES.

      SELECT SINGLE  vbeln auart  kunnr
        FROM vbak INTO wa_vbak
       WHERE tknum = c_saida-transp.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.

    ENDDO.

    IF sy-subrc NE 0.
      c_saida-ovserv = icon_icon_list.
      c_zsdt0001_new-ov_frete = ''.
    ELSEIF c_saida-ovserv NE wa_vbak-vbeln.
      c_saida-ovserv            = wa_vbak-vbeln.
      c_zsdt0001_new-ov_frete = wa_vbak-vbeln.
    ENDIF.
  ENDIF.

  IF ( c_saida-ovserv+0(1) = '@' ).
    "Limpar Fatura Serviço e DACTE
    c_saida-fatserv = icon_icon_list.
    c_saida-dacte  = icon_execute_object.

    c_zsdt0001_new-fatura_frete = ''.
    c_zsdt0001_new-nro_nf_frete = ''.
  ENDIF.

ENDFORM.

FORM f_get_fat_serv_romaneio CHANGING c_saida        TYPE ty_saida
                                        c_zsdt0001_new TYPE zsdt0001.

  IF ( c_saida-fatserv+0(1) = '@' ) AND
     ( c_saida-ovserv IS NOT INITIAL AND c_saida-ovserv(1) NE '@' ).

    SELECT SINGLE a~vbeln a~mjahr
      FROM vbfa AS a INTO (vl_vbeln,vl_mjahr)
     WHERE a~vbelv = c_saida-ovserv
       AND a~vbtyp_n  = 'M'
       AND a~vbtyp_v  = 'C'
       AND NOT EXISTS ( SELECT *
                          FROM vbfa AS b
                         WHERE b~vbelv   = a~vbeln
                           AND b~vbtyp_n = 'N' "estorno
                       ).
    IF sy-subrc = 0.
      c_saida-fatserv = vl_vbeln.
      c_zsdt0001_new-fatura_frete = vl_vbeln.
    ELSE.
      c_saida-dacte   = icon_execute_object.
      c_zsdt0001_new-nro_nf_frete = ''.
    ENDIF.

  ELSEIF ( c_saida-fatserv IS NOT INITIAL AND c_saida-fatserv(1) NE '@' ).

    DO 5 TIMES.
      SELECT SINGLE fksto
        FROM vbrk INTO vl_fksto
       WHERE vbeln = c_saida-fatserv.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.
    ENDDO.


    IF sy-subrc NE 0. "Fatura não existe
      c_saida-fatserv = icon_icon_list.
      c_zsdt0001_new-fatura_frete = ''.
    ELSE.
      SELECT SINGLE vbeln mjahr
        FROM vbfa INTO (vl_vbeln,vl_mjahr)
       WHERE vbelv = c_saida-fatserv
         AND vbtyp_n  = 'N'. "estorno
      IF sy-subrc = 0. "Achou estorno
        c_saida-fatserv = icon_icon_list.
        c_zsdt0001_new-fatura_frete = ''.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_get_dacte_romaneio CHANGING c_saida        TYPE ty_saida
                                   c_zsdt0001_new TYPE zsdt0001.

  IF ( c_saida-dacte EQ icon_execute_object ) AND
     ( c_saida-fatserv IS NOT INITIAL AND c_saida-fatserv(1) NE '@' ).
    CLEAR: vl_docnum.

    SELECT SINGLE docnum
      FROM j_1bnflin INTO vl_docnum
     WHERE refkey = c_saida-fatserv.

    IF ( sy-subrc = 0 ) AND ( vl_docnum IS NOT INITIAL ).

      PERFORM f_check_auth_doc USING vl_docnum.

      IF sy-subrc = 0.
        c_saida-dacte  = vl_docnum.
        c_zsdt0001_new-nro_nf_frete = vl_docnum.
      ENDIF.
    ENDIF.
  ELSEIF ( c_saida-dacte   IS NOT INITIAL AND c_saida-dacte(1)   NE '@' ) AND
         ( c_saida-fatserv IS NOT INITIAL AND c_saida-fatserv(1) NE '@' ).

    CLEAR: vl_docnum.
    SELECT SINGLE docnum
      FROM j_1bnflin INTO vl_docnum
     WHERE refkey = c_saida-fatserv.

    PERFORM f_check_canc_doc USING vl_docnum.

    IF  ( sy-subrc = 0  AND vl_docnum IS NOT INITIAL ) OR ( vl_docnum NE c_saida-dacte ). "dacte cancelada  OU fatura não é da DACTE.
      c_saida-dacte  = icon_execute_object.
      c_zsdt0001_new-nro_nf_frete = ''.
    ENDIF.

  ELSEIF ( c_saida-fatserv EQ icon_execute_object ) OR ( c_saida-fatserv EQ icon_icon_list ).
    c_saida-dacte          = icon_execute_object.
    c_zsdt0001_new-nro_nf_frete = ''.
  ENDIF.

ENDFORM.

FORM f_get_doc_znfw_romaneio CHANGING c_saida        TYPE ty_saida
                                      c_zsdt0001_new TYPE zsdt0001.

  DATA: lva_zfiwrt0008_rom TYPE zfiwrt0008.

  CHECK c_saida-dt_movimento >= '20190418'.

  CLEAR: lva_zfiwrt0008_rom.

  IF ( c_saida-seq_lcto(1) EQ '@' ) OR ( c_saida-seq_lcto IS INITIAL ).
    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
     WHERE ch_referencia   EQ @c_saida-ch_referencia
       AND loekz           EQ @abap_false
       AND docs_estornados EQ @abap_false.

    IF sy-subrc = 0.
      c_saida-seq_lcto = _wl_zfiwrt0008-seq_lcto.
      c_zsdt0001_new-seq_lcto = _wl_zfiwrt0008-seq_lcto.
      c_zsdt0001_new-status   = 'X'.
      lva_zfiwrt0008_rom      = _wl_zfiwrt0008.

    ELSE.  "Inclusão condição referente erro em PRD /  IR130107 / AOENNING.
      c_zsdt0001_new-status = abap_false.
    ENDIF.



  ELSE. "Limpa

    DATA(_clear_seq_lcto) = abap_false.

    SELECT SINGLE *
      FROM zfiwrt0008 INTO _wl_zfiwrt0008
     WHERE ch_referencia   EQ c_saida-ch_referencia
       AND loekz           EQ abap_false
       AND docs_estornados EQ abap_false.

    IF ( sy-subrc EQ 0 ) AND ( _wl_zfiwrt0008-seq_lcto EQ c_saida-seq_lcto ).
      lva_zfiwrt0008_rom = _wl_zfiwrt0008.
    ELSE.
      c_saida-seq_lcto        = icon_execute_object.
      c_zsdt0001_new-seq_lcto = ''.
      c_zsdt0001_new-status   = ''.
    ENDIF.
  ENDIF.

  "LES - Inversão Fluxo Fert. PV US #83810 - WPP - Ini
  CASE vg_cockpit. "Recomposição de Status
    WHEN '01' OR '05' OR '06' OR '07' OR '03' OR '09'.

    WHEN '04'. " Fertilizantes (Porto Velho)

      IF ( lva_zfiwrt0008_rom-ebeln IS NOT INITIAL ) AND ( c_zsdt0001_new-ebeln IS INITIAL OR c_zsdt0001_new-ebelp IS INITIAL  ).
        c_zsdt0001_new-ebeln = lva_zfiwrt0008_rom-ebeln.
        c_zsdt0001_new-ebelp = lva_zfiwrt0008_rom-ebelp.
      ENDIF.

  ENDCASE.
  "LES - Inversão Fluxo Fert. PV US #83810 - WPP - Fim


ENDFORM.

FORM f_get_danfe_znfw_romaneio CHANGING c_saida        TYPE ty_saida
                                        c_zsdt0001_new TYPE zsdt0001.

  CHECK c_saida-dt_movimento >= '20190418'.

  IF ( c_saida-danfez(1) EQ '@' ) OR ( c_saida-danfez IS INITIAL ).

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_zfiwrt0008)
     WHERE ch_referencia   EQ @c_saida-ch_referencia
       AND loekz           EQ @abap_false
       AND docs_estornados EQ @abap_false.

    IF ( sy-subrc = 0 ) AND ( _wl_zfiwrt0008-docnum IS NOT INITIAL ).

      PERFORM f_check_auth_doc USING _wl_zfiwrt0008-docnum.

      IF sy-subrc = 0.
        c_saida-danfez   = _wl_zfiwrt0008-docnum.
        c_zsdt0001_new-nro_nf_rem = _wl_zfiwrt0008-docnum.
      ENDIF.

    ENDIF.

  ELSE. "Limpa

    SELECT SINGLE *
      FROM zfiwrt0008 INTO _wl_zfiwrt0008
     WHERE ch_referencia   EQ c_saida-ch_referencia
       AND loekz           EQ abap_false
       AND docs_estornados EQ abap_false.

    IF ( sy-subrc NE 0 ) OR ( _wl_zfiwrt0008-docnum NE c_saida-danfez ).
      c_saida-danfez = icon_execute_object.
      c_zsdt0001_new-nro_nf_rem = ''.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_get_aviso_romaneio CHANGING c_saida        TYPE ty_saida
                                   c_zsdt0001_new TYPE zsdt0001.

  DATA: v_vbeln TYPE likp-vbeln.

  CHECK c_saida-dt_movimento >= '20190418'.

  IF ( c_saida-aviso(1) EQ '@' ) OR ( c_saida-aviso IS INITIAL ).

    SELECT SINGLE *
      FROM likp INTO wa_likp
     WHERE berot         = c_saida-ch_referencia
       AND vbtyp         = '7'
       AND spe_loekz     = ''.

    IF sy-subrc = 0.
      c_saida-aviso              = wa_likp-vbeln.
      c_zsdt0001_new-doc_aviso = wa_likp-vbeln.
    ENDIF.

  ELSEIF c_saida-aviso IS NOT INITIAL AND c_saida-aviso(1) NE  '@'.

    CLEAR: wa_likp.

    v_vbeln = |{ c_saida-aviso ALPHA = IN }|.

    DO 5 TIMES.

      SELECT SINGLE *
        FROM likp INTO wa_likp
       WHERE vbeln     = v_vbeln
         AND spe_loekz = ''.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
        sy-subrc = 4.
      ENDIF.
    ENDDO.

    IF ( sy-subrc NE 0 ) OR ( wa_likp-berot NE c_saida-ch_referencia ).
      c_saida-aviso = icon_execute_object.
      c_zsdt0001_new-doc_aviso = ''.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_build_status_romaneio_01 USING p_zsdt0001_current TYPE zsdt0001
                             CHANGING c_saida            TYPE ty_saida.

  c_saida-st_proc = ''.

  IF ( c_saida-remessa IS NOT INITIAL ) AND ( c_saida-remessa(1) NE '@' ).
    c_saida-st_proc = vg_st_remessa.
  ENDIF.

  IF ( c_saida-fatura IS NOT INITIAL ) AND ( c_saida-fatura(1) NE '@' ).
    c_saida-st_proc = vg_st_fatura.

    IF c_saida-operacao(4) = 'ZPAR'.
      c_saida-st_proc = vg_st_finalizado.
      EXIT.
    ENDIF.
  ENDIF.

  IF ( c_saida-danfe IS NOT INITIAL ) AND ( c_saida-danfe(1) NE '@' ).

    c_saida-st_proc = vg_st_danfe.

    IF ( c_saida-inco1 = 'FOB' OR c_saida-inco1 = 'CFR' ) AND ( NOT c_saida-enc_conhecimento = abap_true ).

      IF c_saida-troca_nota            = abap_true AND
         c_saida-docs_enviado_carguero = abap_false.
        c_saida-st_proc = vg_st_aguard_doc_carg.
      ELSE.
        c_saida-st_proc = vg_st_finalizado.
      ENDIF.

    ENDIF.
  ENDIF.

  IF ( c_saida-transp IS NOT INITIAL ) AND ( c_saida-transp(1) NE '@' ).
    c_saida-st_proc = vg_st_transp.
  ENDIF.

  IF ( c_saida-doccus IS NOT INITIAL ) AND ( c_saida-doccus(1) NE '@' ).
    c_saida-st_proc = vg_st_custo.

    IF ( c_saida-inco1 = 'CPT' ) OR ( c_saida-enc_doc_custo EQ abap_true ).

      IF c_saida-troca_nota            = abap_true AND
         c_saida-docs_enviado_carguero = abap_false.
        c_saida-st_proc = vg_st_aguard_doc_carg.
      ELSE.
        c_saida-st_proc = vg_st_finalizado.
      ENDIF.

    ENDIF.
  ENDIF.

  IF ( c_saida-ovserv IS NOT INITIAL ) AND ( c_saida-ovserv(1) NE '@' ).
    c_saida-st_proc = vg_st_ov_frete.
  ENDIF.

  IF ( c_saida-fatserv IS NOT INITIAL ) AND ( c_saida-fatserv(1) NE '@' ).
    c_saida-st_proc = vg_st_fatura_frete.
  ENDIF.

  IF ( c_saida-dacte IS NOT INITIAL ) AND ( c_saida-dacte(1) NE '@' ) AND
     ( c_saida-inco1 NE 'CPT' ).

    IF c_saida-troca_nota            = abap_true AND
       c_saida-docs_enviado_carguero = abap_false.
      c_saida-st_proc = vg_st_aguard_doc_carg.
    ELSE.
      c_saida-st_proc = vg_st_finalizado.
    ENDIF.

  ENDIF.

  IF ( c_saida-danfe IS NOT INITIAL ) AND ( c_saida-danfe(1) NE '@' ).
    DATA(_rem_conta_ordem) = abap_false.
    PERFORM f_check_rem_conta_ordem USING c_saida CHANGING _rem_conta_ordem.
    IF _rem_conta_ordem EQ abap_true.
      IF p_zsdt0001_current-st_proc NE vg_st_finalizado.
        MESSAGE i000(z01) WITH 'O Docto de Transporte do romaneio:' c_saida-nr_romaneio
                               ', deverá ser gerado '
                               'pela transação ZLES0200. Romaneio será finalizado!'.

      ENDIF.
      c_saida-st_proc = vg_st_finalizado.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_build_status_romaneio_02 USING p_zsdt0001_current TYPE zsdt0001
                             CHANGING c_saida TYPE ty_saida.

  c_saida-st_proc = ''.

  IF ( c_saida-seq_lcto IS NOT INITIAL ) AND ( c_saida-seq_lcto(1) NE '@' ).
    c_saida-st_proc = vg_st_znfw.
  ELSE.
    c_saida-st_proc = ''.
  ENDIF.

  IF ( c_saida-danfez IS NOT INITIAL ) AND ( c_saida-danfez(1) NE '@' ).
    c_saida-st_proc = vg_st_danfe_znfw.
  ENDIF.

  IF ( c_saida-aviso IS NOT INITIAL ) AND ( c_saida-aviso(1) NE '@' ).
    c_saida-st_proc = vg_st_aviso_rec.
  ENDIF.

  IF ( c_saida-remessa IS NOT INITIAL ) AND ( c_saida-remessa(1) NE '@' ).
    c_saida-st_proc = vg_st_remessa.
  ENDIF.

  IF ( c_saida-fatura IS NOT INITIAL ) AND ( c_saida-fatura(1) NE '@' ).
    c_saida-st_proc = vg_st_fatura.
  ENDIF.

  IF ( c_saida-danfe IS NOT INITIAL ) AND ( c_saida-danfe(1) NE '@' ).
    c_saida-st_proc = vg_st_danfe.
  ENDIF.

  IF ( c_saida-transp IS NOT INITIAL ) AND ( c_saida-transp(1) NE '@' ).
    c_saida-st_proc = vg_st_transp.
  ENDIF.

  IF ( c_saida-doccus IS NOT INITIAL ) AND ( c_saida-doccus(1) NE '@' ).
    c_saida-st_proc = vg_st_custo.
  ENDIF.

  IF ( c_saida-ovserv IS NOT INITIAL ) AND ( c_saida-ovserv(1) NE '@' ).
    c_saida-st_proc = vg_st_ov_frete.
  ENDIF.

  IF ( c_saida-fatserv IS NOT INITIAL ) AND ( c_saida-fatserv(1) NE '@' ).
    c_saida-st_proc = vg_st_fatura_frete.
  ENDIF.

  IF ( c_saida-dacte IS NOT INITIAL ) AND ( c_saida-dacte(1) NE '@' ).
    c_saida-st_proc = vg_st_dacte.
  ENDIF.

  DATA(_finaliza_rom) = abap_false.
  DATA(_fat_incompleto)   = abap_false.


  IF NOT ( ( c_saida-danfez IS NOT INITIAL ) AND ( c_saida-danfez(1) NE '@' ) ).
    _fat_incompleto = abap_true.
  ENDIF.

  CASE c_saida-tipo .
    WHEN 'P'.
      DATA(_emite_danfe) = abap_false.
    WHEN 'O' OR 'T'.
      _emite_danfe       = abap_true.
  ENDCASE.

  IF ( _emite_danfe EQ abap_true ) AND  NOT ( ( c_saida-danfe IS NOT INITIAL ) AND ( c_saida-danfe(1) NE '@' ) ).
    _fat_incompleto = abap_true.
  ENDIF.

  CASE c_saida-inco1.
    WHEN 'CIF'.

      CASE c_saida-enc_doc_custo.
        WHEN abap_true.
          IF NOT ( ( c_saida-doccus IS NOT INITIAL ) AND
                   ( c_saida-doccus(1) NE '@'      ) ).
            _fat_incompleto = abap_true.
          ENDIF.
        WHEN abap_false.
          IF NOT ( ( c_saida-dacte IS NOT INITIAL ) AND
                   ( c_saida-dacte(1) NE '@'      ) ).
            _fat_incompleto = abap_true.
          ENDIF.
      ENDCASE.

    WHEN 'CPT'.

      IF NOT ( ( c_saida-doccus IS NOT INITIAL ) AND
               ( c_saida-doccus(1) NE '@'      ) ).
        _fat_incompleto = abap_true.
      ENDIF.

    WHEN 'FOB' OR 'CFR'.


  ENDCASE.

  IF _fat_incompleto EQ abap_false.
    IF c_saida-troca_nota            = abap_true AND
       c_saida-docs_enviado_carguero = abap_false.
      c_saida-st_proc = vg_st_aguard_doc_carg.
    ELSE.
      c_saida-st_proc = vg_st_finalizado.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_get_agente_fre_romaneio CHANGING c_saida        TYPE ty_saida
                                        c_zsdt0001_new TYPE zsdt0001.

  DATA: v_vbeln        TYPE likp-vbeln,
        v_agente_frete TYPE zsdt0001-agente_frete.

  CLEAR: v_vbeln, v_agente_frete.

  IF ( c_saida-lifnr IS INITIAL ).
    CASE vg_cockpit.
      WHEN '04'.

        IF ( c_saida-aviso IS NOT INITIAL ) AND ( c_saida-aviso(1) NE '@' ).
          v_vbeln = |{ c_saida-aviso ALPHA = IN }|.
        ENDIF.

        IF ( c_saida-remessa IS NOT INITIAL ) AND ( c_saida-remessa(1) NE '@' ).
          v_vbeln = |{ c_saida-remessa ALPHA = IN }|.
        ENDIF.

      WHEN OTHERS.

        IF ( c_saida-remessa IS NOT INITIAL ) AND ( c_saida-remessa(1) NE '@' ).
          v_vbeln = |{ c_saida-remessa ALPHA = IN }|.
        ENDIF.

    ENDCASE.

    IF v_vbeln IS NOT INITIAL.
      SELECT SINGLE *
        FROM vbpa INTO @DATA(lwa_vbpa_sp)
       WHERE vbeln EQ @v_vbeln
         AND parvw EQ 'SP'.

      IF sy-subrc EQ 0.
        v_agente_frete = lwa_vbpa_sp-lifnr.
      ENDIF.
    ENDIF.

  ENDIF.

  IF v_agente_frete IS NOT INITIAL.
    c_zsdt0001_new-agente_frete = v_agente_frete.
  ENDIF.


ENDFORM.


*-CS2021000218-16.11.2022-#90706-JT-inicio
************************************************************************************
* Validar Ag.Frete qdo gerar remessa
************************************************************************************
FORM f_validar_ag_frete  USING p_inco1
                               p_lifnr
                      CHANGING p_erro
                               p_mesg.

  FREE: p_erro, p_mesg.

  CHECK p_inco1 = 'FOB'.
  CHECK p_lifnr IS NOT INITIAL.

  SELECT SINGLE ktokk
    INTO @DATA(l_ktokk)
    FROM lfa1
   WHERE lifnr = @p_lifnr.

  IF sy-subrc = 0 AND l_ktokk = 'ZFIC'.
    p_mesg = 'Para Carga FOB o transportador não pode ser Intercompany!'.
    p_erro = abap_true.
  ENDIF.

ENDFORM.

************************************************************************************
* Validar Receituario Agronomico
************************************************************************************
FORM f_validar_solicitacao_ra  USING p_nro_cg
                                     p_ch_referencia
                            CHANGING p_saida              TYPE ty_saida
                                     p_erro.

  DATA: l_chave_assina TYPE i,
        l_pdf_assina   TYPE i,
        l_num_receita  TYPE zsdt0218-numeroreceita.

  FREE: p_erro.

  CHECK p_saida-remessa IS INITIAL OR p_saida-remessa(1) = '@' .

  SELECT SINGLE gera_solicitacao_ra, qtd_solicitacao_ra
    FROM zsdt0302
    INTO @DATA(w_0302)
   WHERE nro_cgd       = @p_nro_cg
     AND ch_referencia = @p_ch_referencia.

  CHECK sy-subrc = 0.
  CHECK w_0302-gera_solicitacao_ra = abap_on.

*-------------------------------
*-- consulta solicitacao receitas
*-------------------------------
* TRY.
*     zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*        )->set_consultar_sol_ra( EXPORTING i_nro_cgd       = p_nro_cg
*                                           i_ch_referencia = p_ch_referencia ).
*
*   CATCH zcx_integracao INTO DATA(ex_integra).
*   CATCH zcx_error INTO DATA(ex_error).
* ENDTRY.

*-------------------------------
*- Receitas geradas
*-------------------------------
  SELECT receitakey, tipo_assinatura, status
    FROM zsdt0298
    INTO TABLE @DATA(t_0298)
   WHERE nro_cgd       = @p_nro_cg
     AND ch_referencia = @p_ch_referencia
     AND cancelado     = @abap_off.

  IF t_0298[] IS INITIAL.
    FREE: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Obrig.Emissão de Sol.Receita.' &&
                         'Ver.se todas as SR foram geradas pelo Rom.na Carga:' && p_nro_cg.
    APPEND tg_log_erro.

    PERFORM f_grava_log_erro TABLES tg_log_erro
                              USING p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_0298 LINES DATA(l_lines_298).

  IF w_0302-qtd_solicitacao_ra <> l_lines_298.
    FREE: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Obrig.Emissão de Sol.Receita.' &&
                         'Ver.se todas as SR foram geradas pelo Rom.na Carga:' && p_nro_cg.
    APPEND tg_log_erro.

    PERFORM f_grava_log_erro TABLES tg_log_erro
                              USING p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    EXIT.
  ENDIF.

  READ TABLE t_0298 INTO DATA(w_0298) INDEX 1.

  CHECK w_0298-tipo_assinatura <> '0'. "manual.

*-------------------------------
* validar status das RAs
*-------------------------------
  LOOP AT t_0298 INTO w_0298.
    IF w_0298-status <> '4'.
      FREE: tg_log_erro.
      tg_log_erro-msgtyp = 'E'.
      tg_log_erro-msgid  = 'AGRIQ'.
      tg_log_erro-msgv1  = 'Existem Receitas não Finalizadas. Verifique no sistema AgriQ.'.
      APPEND tg_log_erro.

      PERFORM f_grava_log_erro TABLES tg_log_erro
                                USING p_saida.
      p_saida-remessa = icon_led_yellow.
      p_saida-icon    = icon_led_red.
      p_erro          = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK p_erro = abap_false.

*-------------------------------
* validar status assinaturas
*-------------------------------
  CLEAR: l_chave_assina, l_pdf_assina, l_num_receita.

  LOOP AT t_0298 INTO w_0298.

    SELECT chave_assinatura, chave_pdf_assinado, numeroreceita
      INTO @DATA(w_0218)
      FROM zsdt0218
        UP TO 1 ROWS
     WHERE receitakey = @w_0298-receitakey
       AND cancelada  = @abap_off.
    ENDSELECT.

    CHECK sy-subrc = 0.

    IF w_0218-chave_assinatura   IS NOT INITIAL.
      l_chave_assina = l_chave_assina + 1.
    ENDIF.
    IF w_0218-chave_pdf_assinado IS NOT INITIAL.
      l_pdf_assina   = l_pdf_assina + 1.
    ELSE.
      l_num_receita  = w_0218-numeroreceita.
    ENDIF.
  ENDLOOP.

  IF     l_chave_assina <> l_lines_298.
    FREE: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Existe(m) Receita(s) pendente de envio para a Assinatura eletrônica'.
    APPEND tg_log_erro.

    PERFORM f_grava_log_erro TABLES tg_log_erro
                              USING p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    EXIT.
  ELSEIF l_pdf_assina <> l_lines_298.
    FREE: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = '"A Receita N°:' && l_num_receita && ', está pendente da Assinatura eletrônica'.
    APPEND tg_log_erro.

    PERFORM f_grava_log_erro TABLES tg_log_erro
                              USING p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    EXIT.
  ENDIF.

  p_saida-remessa = icon_execute_object.

ENDFORM.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
************************************************************************************
* Validar se romaneio tem ZMM0023 efetuada
************************************************************************************
FORM f_validar_transf_algodao  USING p_ch_referencia
                            CHANGING p_saida              TYPE ty_saida
                                     p_erro.

  FREE: p_erro.

  CHECK p_saida-remessa IS INITIAL OR p_saida-remessa(1) = '@' .

* SELECT mblnr
*  INTO @DATA(_mblnr)
*  FROM zsdt0330
*    UP TO 1 ROWS
* WHERE ch_referencia = @p_ch_referencia
*   AND cancelado     = @abap_off.
* ENDSELECT.
*
* IF sy-subrc <> 0.
*   FREE: tg_log_erro.
*   tg_log_erro-msgtyp = 'E'.
*   tg_log_erro-msgid  = 'REMESSA'.
*   tg_log_erro-msgv1  = 'Não foram efetuadas Transferência ' &&
*                        'de todos os fardos na ZMM0023!'.
*   APPEND tg_log_erro.
*
*   PERFORM f_grava_log_erro TABLES tg_log_erro
*                             USING p_saida.
*   p_saida-remessa = icon_led_yellow.
*   p_saida-icon    = icon_led_red.
*   p_erro          = abap_true.
*   EXIT.
* ENDIF.

  SELECT mblnr
   INTO @DATA(_mblnr)
   FROM zsdt0330
     UP TO 1 ROWS
  WHERE ch_referencia = @p_ch_referencia
    AND mblnr         = @abap_off
    AND status_fardo <> '3'
    AND cancelado     = @abap_off.
  ENDSELECT.

  IF sy-subrc = 0.
    FREE: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'REMESSA'.
    tg_log_erro-msgv1  = 'Não foram efetuadas Transferência ' &&
                         'de todos os fardos na ZMM0023!'.
    APPEND tg_log_erro.

    PERFORM f_grava_log_erro TABLES tg_log_erro
                              USING p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
  ENDIF.

ENDFORM.
*-CS2023000189-26.05.2023-#108752-JT-fim

************************************************************************************
************************************************************************************
*&---------------------------------------------------------------------*
*& Form f_check_faturamento_ecc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_check_faturamento_ecc .

  DATA: lva_msg_retorno TYPE string,
        lva_ok          TYPE char01.

  CALL METHOD cl_grid->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF lines( tl_rows[] ) EQ 0.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'I'.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX sl_rows-index.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
     WHERE ch_referencia = @<fs_out>-ch_referencia.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0001-fat_contingencia_ecc = abap_true.

    CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
      EXPORTING
        i_ch_referencia = <fs_out>-ch_referencia
      IMPORTING
        e_ok            = lva_ok
        e_msg_retorno   = lva_msg_retorno.

    IF lva_ok EQ abap_false.
      MESSAGE lva_msg_retorno TYPE 'I'.
    ELSE.
      MESSAGE lva_msg_retorno TYPE 'S'.
    ENDIF.

  ENDLOOP.

  PERFORM f_refresh_alv USING '0100'. "Refresh na tela

ENDFORM.
