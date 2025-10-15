*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_FORM
*&---------------------------------------------------------------------*
data: results_received(1) type c,
      t_return_goods_cnc  like bapiret2 occurs 0 with header line,
      g_migo_number       type bapi2017_gm_head_ret.

form f_estrutura_alv using value(p_col_pos)       type i                    "1
                           value(p_ref_tabname)   like dd02d-tabname        "2
                           value(p_ref_fieldname) like dd03d-fieldname      "3
                           value(p_tabname)       like dd02d-tabname        "4
                           value(p_field)         like dd03d-fieldname      "5
                           value(p_scrtext_l)     like dd03p-scrtext_l      "6
                           value(p_outputlen)                               "7
                           value(p_edit)                                    "8
                           value(p_sum)                                     "9
                           value(p_just)                                    "10
                           value(p_hotspot)                                 "11
                           value(p_f4)                                      "12
                           value(p_checkbox)                                "13
                           value(p_style)                                   "14
                           value(p_no_out)                                  "15
                           value(p_icon).                                   "16

  clear wa_afield.
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
  if wa_afield-fieldname = 'ICON'         or
     wa_afield-fieldname = 'DT_MOVIMENTO' or
     wa_afield-fieldname = 'NR_ROMANEIO'  or
     wa_afield-fieldname = 'PLACA_CAV'    or
     wa_afield-fieldname = 'BUKRS'        or
     wa_afield-fieldname = 'BRANCH'       or
     wa_afield-fieldname = 'NRO_CG'.
    wa_afield-fix_column = 'X'.
  endif.
*-CS2021000696 - 10.08.2021 - JT - fim

  append wa_afield to it_fieldcat.

endform.                    " ESTRUTURA_ALV

form f_atual_frete using pa_zsdt0001    type ty_zsdt0001
                         p_tipo_chamada type char01
                changing p_saida        type ty_saida.

  data: _vbeln         type vbeln,
        _placa_cav     type zplaca,
        _vlr_frete_neg type zvalor_frete,
        _id_ordem      type zde_id_ordem.

  data: v_cont_fre type i.

  v_cont_fre = 0.

  if p_tipo_chamada eq 'E'.
    append pa_zsdt0001 to it_zsdt0001.
    append pa_zsdt0001 to it_zsdt0001_fre.
    perform f_selecao_generica_rom.
    perform f_get_value_set tables t_auart  using 'MAGGI_ARMAZENAGEM_VA01'.
    perform f_pega_frete.
  endif.

  "Vlr Frete
  loop at it_a900 into wa_a900 where shtyp = pa_zsdt0001-shtyp
                                 and tdlnr = pa_zsdt0001-agente_frete
                                 and route = pa_zsdt0001-route
                                 and add01 = pa_zsdt0001-add01.
    read table it_konp into wa_konp with key knumh = wa_a900-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  loop at it_a910 into wa_a910 where shtyp  = pa_zsdt0001-shtyp
                                 and tdlnr  = pa_zsdt0001-agente_frete
                                 and lzonea = pa_zsdt0001-lzonea
                                 and lzonez = pa_zsdt0001-lzonez.
    read table it_konp into wa_konp with key knumh = wa_a910-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  loop at it_a911 into wa_a911 where shtyp = pa_zsdt0001-shtyp
                                 and tdlnr = pa_zsdt0001-agente_frete
                                 and route = pa_zsdt0001-route.
    read table it_konp into wa_konp with key knumh = wa_a911-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  loop at it_a915 into wa_a915 where shtyp  = pa_zsdt0001-shtyp
                                 and tdlnr  = pa_zsdt0001-agente_frete
                                 and lzonea = pa_zsdt0001-lzonea
                                 and lzonez = pa_zsdt0001-lzonez
                                 and add01  = pa_zsdt0001-add01.
    read table it_konp into wa_konp with key knumh = wa_a915-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  loop at it_a918 into wa_a918 where shtyp  = pa_zsdt0001-shtyp
                                 and tdlnr  = pa_zsdt0001-agente_frete
                                 and matnr  = pa_zsdt0001-matnr
                                 and lzonea = pa_zsdt0001-lzonea
                                 and lzonez = pa_zsdt0001-lzonez
                                 and add01  = pa_zsdt0001-add01.
    read table it_konp into wa_konp with key knumh = wa_a918-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  loop at it_a919 into wa_a919 where shtyp  = pa_zsdt0001-shtyp
                                 and tdlnr  = pa_zsdt0001-agente_frete
                                 and matnr  = pa_zsdt0001-matnr
                                 and lzonea = pa_zsdt0001-lzonea
                                 and lzonez = pa_zsdt0001-lzonez.
    read table it_konp into wa_konp with key knumh = wa_a919-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
      add 1 to v_cont_fre.
    endif.
  endloop.

  if wa_konp-krech = 'A'. "Percentual
    p_saida-kbetr = p_saida-kbetr / 10.
  endif.

*---CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
  read table it_zsdt0001_aux into wa_zsdt0001_aux
                             with key ch_referencia = pa_zsdt0001-ch_referencia.

*============================Inicio do ajuste ISSUE #181019 / AOENNING / Fixado condição para pegar o valor do frete se estiver em branco variavel wa_zsdt0001_aux-sdabw.
  if wa_zsdt0001_aux-sdabw eq '0000'.
    wa_zsdt0001_aux-sdabw = '0001'.
  endif.
*============================Fim do ajuste ISSUE #181019 / AOENNING / Fixado condição para pegar o valor do frete se estiver em branco variavel wa_zsdt0001_aux-sdabw.

  loop at it_a942 into wa_a942 where shtyp  = wa_zsdt0001_aux-shtyp
                                 "AND sdabw  = wa_zsdt0001_aux-sdabw ""SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
                                 and id_viagem = wa_zsdt0001_aux-viagem_id.
    read table it_konp into wa_konp with key knumh = wa_a942-knumh binary search.
    if sy-subrc = 0.
      p_saida-kbetr = wa_konp-kbetr.
      p_saida-konwa = wa_konp-konwa.
      p_saida-krech = wa_konp-krech.
*     ADD  1 TO v_cont_fre.
      move 1 to v_cont_fre.
    endif.
  endloop.
*---CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

  "Check Alteração Preço por Solicitação - Transação ZLES0153 - CS2016001693
  if ( p_saida-tipo = 'O' ) and ( pa_zsdt0001-vbeln is not initial ).
    _vbeln         = pa_zsdt0001-vbeln.
    _placa_cav     = pa_zsdt0001-placa_cav.
    call function 'ZLES_VALOR_FRETE_ORDEM_CAR'
      exporting
        i_vbeln         = _vbeln
        i_placa_cav     = _placa_cav
        i_id_ordem      = pa_zsdt0001-id_ordem
        i_shtyp         = p_saida-shtyp
      importing
        e_vlr_frete_neg = _vlr_frete_neg
        e_id_ordem      = _id_ordem.

    if _vlr_frete_neg > 0.
      p_saida-kbetr    = _vlr_frete_neg. "Atribuir Valor de Frete Negociado
      p_saida-id_ordem = _id_ordem.
    endif.
  endif.


  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
  if pa_zsdt0001-id_interface eq '48'. "Sementes
    if pa_zsdt0001-nro_cg is not initial.
      call method zcl_carga_saida_insumos=>busca_dados_carga
        exporting
          i_nr_carga_single = pa_zsdt0001-nro_cg
        importing
          e_romaneios       = data(lit_romaneios_carga_sem).

      read table lit_romaneios_carga_sem into data(lwa_rom_carga) with key ch_referencia = pa_zsdt0001-ch_referencia.
      if sy-subrc eq 0 and lwa_rom_carga-preco_zfre is not initial.
        p_saida-kbetr = lwa_rom_carga-preco_zfre.
        p_saida-krech = 'B'. "Montante Fixo
      endif.
    endif.
  endif.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


  wa_saida-cont_fre = v_cont_fre.

endform.                    "f_atual_frete

form f_elimina_vt  tables tl_itemdata
                 changing p_tipo_chamada type char01
                          p_saida type ty_saida
                          it_tab_bapiret1 type tab_bapiret1
                  raising zcx_error. "*-#133089-12.02.2024-JT

  data: st_headerdata2      type bapishipmentheader,
        st_headerdataaction type bapishipmentheaderaction,
        t_itemdataaction    type table of bapishipmentitemaction with header line,
        st_headerdata       type bapishipmentheader,
        v_chv_fat_vt        type zch_ref,
        wa_tab_bapiret1     like line of it_tab_bapiret1.

  clear: st_headerdataaction,
         st_headerdata,
         t_itemdataaction,
         st_headerdata2,
         t_itemdataaction[],
         t_return_vt[].

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_saida-transp+0(10)
    importing
      output = st_headerdata2-shipment_num.

  st_headerdataaction-shipment_num = 'D'.
  st_headerdataaction-service_agent_id = 'D'.

  loop at tl_itemdata into st_itemdata.
    move: 'D' to t_itemdataaction-delivery,
          'D' to t_itemdataaction-itenerary.

    append t_itemdataaction.
    clear: t_itemdataaction.
  endloop.

  refresh t_return_vt.
  call function 'BAPI_SHIPMENT_CHANGE'
    exporting
      headerdata       = st_headerdata2
      headerdataaction = st_headerdataaction
    tables
      itemdata         = tl_itemdata
      itemdataaction   = t_itemdataaction
      return           = t_return_vt.

  if p_tipo_chamada = 'E'.
    loop at t_return_vt into data(wa_erros_vt).
      clear: wa_tab_bapiret1.
      move-corresponding wa_erros_vt to wa_tab_bapiret1.
      append wa_tab_bapiret1 to it_tab_bapiret1.
    endloop.
  endif.

  read table t_return_vt with key type = 'E'.
  if sy-subrc is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    data(_st_transp_before) = vg_st_transp_before.

    if vg_cockpit = '04'.
      if p_saida-tipo = 'P'.
        _st_transp_before = vg_st_aviso_rec.
      endif.
    endif.

    p_saida-st_proc  = _st_transp_before.
    p_saida-transp   = icon_execute_object.

    "Atualiza valor do frete Historico
    loop at p_saida-romaneios_agr into data(_wl_rom) where ch_referencia is not initial.
      update zsdt0001 set  kbetr = 0  konwa = '' where ch_referencia = _wl_rom-ch_referencia.


      update zsdt0001 set st_proc      = _st_transp_before
                          kbetr        = 0
                          konwa        = ''
                          doc_transp   = ''
       where ch_referencia = _wl_rom-ch_referencia.

      update zlest0155 set ch_referencia = space where ch_referencia = _wl_rom-ch_referencia.

      read table it_saida assigning field-symbol(<fs_saida_tmp>) with key ch_referencia = _wl_rom-ch_referencia.
      if sy-subrc eq 0.
        <fs_saida_tmp>-st_proc  = _st_transp_before.
        <fs_saida_tmp>-transp   = icon_execute_object.

        clear: v_chv_fat_vt.
        zcl_romaneio=>get_ck_faturar(
          exporting
            i_ch_referencia_sai   = <fs_saida_tmp>-ch_referencia
            i_somente_chv_faturar = abap_true
          importing
            e_chv_faturar         = v_chv_fat_vt ).

        if ( v_chv_fat_vt is not initial ) and ( v_chv_fat_vt ne <fs_saida_tmp>-ch_referencia ).
          <fs_saida_tmp>-transp = icon_icon_list.
        endif.
      endif.
    endloop.

    if line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ).
      update zsdt0001 set agente_frete = ''
       where ch_referencia = p_saida-ch_referencia.
    endif.

  else.
    case p_tipo_chamada.
      when 'L'.
        perform f_prepare_return tables t_return_vt.
        perform f_grava_log_erro tables tg_log_erro using p_saida.
      when 'E'.
        message id t_return_vt-id type t_return_vt-type number t_return_vt-number into data(mtext)
           with t_return_vt-message_v1 t_return_vt-message_v2 t_return_vt-message_v3 t_return_vt-message_v4.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( MTEXT ) ).
    endcase.
  endif.
endform.                    " ELIMINA_VT

form f_monta_layout .

  clear it_fieldcat[].

  case vg_cockpit. "Field Cat.
    when '01'. "Commodities (Formação Lote, Vendas e Trâsferências Expedidas) - ZLES0106

      perform f_estrutura_alv using:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' '.

      data(lt_saida) = it_saida[].
      delete lt_saida where qtde_remessa is initial.
      describe table lt_saida lines data(lv_lines).
      if lv_lines >= 1.
        perform f_estrutura_alv using:
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
      else.
        perform f_estrutura_alv using:
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
      endif.

    when '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
    when '03'. " OR '09' . "Troca de notas - Commodities com agrupamento
      perform f_estrutura_alv using:
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

    when '04'. "Fertilizantes (Porto Velho) - ZLES0115

      perform f_estrutura_alv using:
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

    when '05'. "Insumos - Sementes (Vendas e Trânsferências expedidas )

      perform f_estrutura_alv using:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'CFOP_OV'       'CFOP OV'             ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-25.09.2024-#152580-JT
        08  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        14  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        15  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        21  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        22  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        23  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        24  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        25  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        26  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        27  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    when '06'. "Insumos - Defensivos (Vendas e Trânsferências expedidas )

      perform f_estrutura_alv using:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'CFOP_OV'       'CFOP OV'             ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-25.09.2024-#152580-JT
        08  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        14  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        15  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        21  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        22  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        23  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        24  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        25  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        26  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        27  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    when '07'. "Insumos - Fertilizantes (Vendas e Trânsferências expedidas )

      perform f_estrutura_alv using:
        01  ''              ''             'IT_SAIDA' 'ICON'          'Log'                 ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        02  ''              ''             'IT_SAIDA' 'DT_MOVIMENTO'  'Dt.Movimento'        '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NR_ROMANEIO'   'Romaneio'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        03  ''              ''             'IT_SAIDA' 'NRO_CG'        'Carga'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        04  ''              ''             'IT_SAIDA' 'PLACA_CAV'     'Placa'               ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        05  ''              ''             'IT_SAIDA' 'REGION'        'UF Placa'            ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        06  ''              ''             'IT_SAIDA' 'VBELN'         'Nro.Documento'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ',
        07  ''              ''             'IT_SAIDA' 'CFOP_OV'       'CFOP OV'             ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ', "*-CS2024000522-25.09.2024-#152580-JT
        08  ''              ''             'IT_SAIDA' 'PESO_LIQ'      'Quantidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        09  ''              ''             'IT_SAIDA' 'INCO1'         'Tp.Frete'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        10  ''              ''             'IT_SAIDA' 'ROUTE'         'Itinerário'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        11  ''              ''             'IT_SAIDA' 'KBETR'         'Vlr frete'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        12  ''              ''             'IT_SAIDA' 'KONWA'         'Und.Cond.'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        13  'LFA1'          'LIFNR'        'IT_SAIDA' 'LIFNR'         'Agente Frete'        ' '   'X'    ' ' ' ' ' '  'X' ' ' ' ' ' ' ' ',
        14  ''              ''             'IT_SAIDA' 'SHTYP'         'Tp.Transp'           ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        15  ''              ''             'IT_SAIDA' 'REMESSA'       'Remessa'             ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        16  ''              ''             'IT_SAIDA' 'FATURA'        'Nro.Fatura'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        17  ''              ''             'IT_SAIDA' 'DANFE'         'DANFE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        18  ''              ''             'IT_SAIDA' 'TRANSP'        'Doc.Transp.'         ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        19  ''              ''             'IT_SAIDA' 'DOCCUS'        'Doc.Custo'           ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        20  ''              ''             'IT_SAIDA' 'OVSERV'        'OV.Serviço'          ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        21  ''              ''             'IT_SAIDA' 'FATSERV'       'Fatura Serv.'        ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        22  ''              ''             'IT_SAIDA' 'DACTE'         'DACTE'               ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X',
        23  ''              ''             'IT_SAIDA' 'OPERACAO'      'Operação'            '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        24  ''              ''             'IT_SAIDA' 'MATERIAL'      'Produto'             '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        25  ''              ''             'IT_SAIDA' 'NAME1_C'       'Ponto de Coleta'     ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
        26  ''              ''             'IT_SAIDA' 'NAME1'         'Nome do Destinario'  ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
*----CS2021000508 - 07.06.2021 - JT - inicio
        27  ''              ''             'IT_SAIDA' 'DOCS_CARGUERO' 'Docs.Carguero'       ' '   ' '    ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X'.
*----CS2021000508 - 07.06.2021 - JT - fim

    when '09' or '10'. "Romaneio de Entrada Completo

      perform f_estrutura_alv using:
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

  endcase.

endform.                    " F_ALV_FIELDCAT

form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to ti_bdcdata.

endform.                    " F_BDC_DATA

form f_call_transaction using p_trans
                              p_saida type ty_saida
                     changing p_erro.

  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  clear: it_msg[], wg_documento, p_erro.

  wl_mode = 'E'.

  call transaction p_trans using ti_bdcdata
              mode wl_mode
     messages into it_msg.

  read table it_msg with key msgtyp = 'E'.
  if sy-subrc = 0.

    p_erro = 'X'.

    perform f_prepare_return3 tables it_msg.
    perform f_grava_log_erro tables tg_log_erro using p_saida.

  elseif p_trans eq 'VL31N'.
    read table it_msg with key msgnr  = '311' msgtyp = 'S'.
    if sy-subrc = 0.
      move it_msg-msgv2 to wg_documento.
    endif.

    if  wg_documento is initial.
      p_erro = 'X'.
    else.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wg_documento
        importing
          output = wg_documento.
    endif.

  endif.


endform.                    "ZF_CALL_TRANSACTION

form f_estorno_custo changing p_saida type ty_saida
                      raising zcx_error. "*-#133089-12.02.2024-JT

  data vdata(10).
  concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) into vdata.

  if ( p_saida-romaneios_agr[] is initial ) or ( p_saida-deliverys[] is initial ).
    clear: wl_erro.
    perform f_set_romaneios_carga changing p_saida
                                           wl_erro.
    check wl_erro eq abap_false.

    perform f_set_delivery changing p_saida
                                    wl_erro.
    check wl_erro eq abap_false.
  endif.

  "Verifica se DACTE foi estornada
  if ( p_saida-fatserv    is not initial ) and
     ( p_saida-fatserv(1) ne '@'         ).
    message 'Estorne desde a DACTE' type  'S'.
    exit.
  endif.

  if ( ( p_saida-doccus    is not initial ) and
       ( p_saida-doccus(1) ne '@'         )  )

     and

     ( ( p_saida-st_proc eq vg_st_finalizado    ) or " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
       ( p_saida-st_proc eq vg_st_aguard_doc_carg ) or " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
       ( p_saida-st_proc eq vg_st_fatura_frete  ) or " Fatura Frete
       ( p_saida-st_proc eq vg_st_ov_frete      ) or " OV.Frete
       ( p_saida-st_proc eq vg_st_custo         ) ).  " Doc.Custo

    "Estornar o documento de custo
    refresh ti_bdcdata.
    if ( p_saida-inco1 ne 'CPT' ) and ( not p_saida-enc_doc_custo = abap_true ).
      "Estornar o documento de custo
      refresh ti_bdcdata.
      if  p_saida-shtyp = 'Z001'.
        perform f_bdc_data using:
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
      else.
        perform f_bdc_data using:
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
      endif.

    else.
      if  p_saida-shtyp = 'Z001'.
        perform f_bdc_data using:
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

      else.
        perform f_bdc_data using:
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

      endif.
    endif.

    clear wl_erro.
    perform f_call_transaction using 'VI02'
                                     p_saida
                            changing wl_erro.
    if wl_erro is initial.
      commit work.
      wait up to 5 seconds.

    else.
      exit.
    endif.

    "Eliminar o documento de custo
    refresh ti_bdcdata.
    perform f_bdc_data using:
           'SAPMV54A'  '0020'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
           ''          ''      ''   'BDC_OKCODE'       '=UEBP',
           ''          ''      ''   'VFKK-FKNUM'       p_saida-doccus, "fknum

           'SAPMV54A'  '0030'  'X'  ''                 ' ',
           ''          ''      ''   'BDC_OKCODE'       '/ELOES'.

    clear wl_erro.
    perform f_call_transaction using 'VI02'
                                     p_saida
                            changing wl_erro.
    if wl_erro is initial.

      p_saida-doccus  = icon_icon_list.

      loop at p_saida-romaneios_agr into data(_wl_rom).

        update zsdt0001 set st_proc = vg_st_transp
                            fknum   = ''
         where ch_referencia = _wl_rom-ch_referencia.

        commit work.

        read table it_saida assigning field-symbol(<fs_saida_tmp>) with key ch_referencia = _wl_rom-ch_referencia.
        if sy-subrc eq 0.
          <fs_saida_tmp>-doccus = icon_icon_list.
        endif.
      endloop.

      wait up to 2 seconds.
    else.
      exit.
    endif.
  endif.

*------------------------------------------------------------------*
*  Eliminar VT
*------------------------------------------------------------------*

  if ( p_saida-st_proc eq vg_st_finalizado    ) or " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( p_saida-st_proc eq vg_st_aguard_doc_carg ) or " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( p_saida-st_proc eq vg_st_fatura_frete  ) or " Fatura Frete
     ( p_saida-st_proc eq vg_st_ov_frete      ) or " OV.Frete
     ( p_saida-st_proc eq vg_st_custo         ) or " Doc.Custo
     ( p_saida-st_proc eq vg_st_transp        ).   " Transporte

    refresh t_itemdata.
    loop at p_saida-deliverys into data(_wl_likp).
      clear st_itemdata.
      st_itemdata-delivery  = _wl_likp-vbeln.
      st_itemdata-itenerary = '0010'.
      append st_itemdata to t_itemdata.
    endloop.

    data(p_tipo_chamada) = 'L'.
    perform f_elimina_vt tables t_itemdata
                       changing p_tipo_chamada p_saida t_return[].
  endif.

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




endform.                    " F_ESTORNO_CUSTO

form f_estorno_cte changing p_saida type ty_saida.

  clear: wl_erro.

  perform f_set_romaneios_carga changing p_saida
                                         wl_erro.
  check wl_erro eq abap_false.

  perform f_set_delivery changing p_saida
                                  wl_erro.
  check wl_erro eq abap_false.

  data(_ok) = ''.
  perform f_check_permissao_estorno using p_saida
                                          '2' "CT-e
                                 changing _ok.

  check _ok is not initial.

  "Cancela fatura
  if ( p_saida-st_proc eq vg_st_finalizado    ) or " Finalizado
     ( p_saida-st_proc eq vg_st_fatura_frete  ). " Fatura Frete

    perform f_estorno_fatura using p_saida-fatserv
                                   'X'                "Fatura de frete
                                   p_saida
                          changing wl_erro.

  endif.

  check wl_erro is initial.

  if ( p_saida-st_proc eq vg_st_finalizado    ) or " Finalizado
     ( p_saida-st_proc eq vg_st_fatura_frete  ) or " Fatura Frete
     ( p_saida-st_proc eq vg_st_ov_frete      ). " OV.Frete

    perform f_estorno_ov_frete using p_saida-ovserv
                                     p_saida
                            changing wl_erro.

  endif.

  check wl_erro is initial.

  if ( p_saida-st_proc eq vg_st_finalizado    ) or " Finalizado
     ( p_saida-st_proc eq vg_st_fatura_frete  ) or " Fatura Frete
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( p_saida-st_proc eq vg_st_aguard_doc_carg ) or " Aguard envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( p_saida-st_proc eq vg_st_ov_frete      ) or " OV.Frete
     ( p_saida-st_proc eq vg_st_custo         ) or " Doc.Custo
     ( p_saida-st_proc eq vg_st_transp        ).   " Transporte


    perform f_estorno_custo changing p_saida.

  endif.

endform.                    " F_ESTORNO_CTE

form f_estorno_nfe changing p_saida type ty_saida.

  data(_ok) = ''.
  perform f_check_permissao_estorno using p_saida
                                          '1' "NF-e
                                 changing _ok.

  check _ok is not initial.

  if p_saida-operacao+0(4) ne 'ZRDC' and
     p_saida-operacao+0(4) ne 'ZRFL' and
     p_saida-operacao+0(4) ne 'ZIND' and
     p_saida-operacao+0(3) ne 'ZUB'  and
     p_saida-operacao+0(4) ne 'ZARM' and
     p_saida-operacao+0(4) ne 'ZRAN'.


    if ( p_saida-st_proc eq vg_st_danfe  ) or
       ( p_saida-st_proc eq vg_st_fatura ).

      perform f_estorno_fatura using p_saida-fatura
                                     ''                "Não é frete
                                     p_saida
                            changing wl_erro.
      check wl_erro is initial.
    endif.

    if ( p_saida-st_proc eq vg_st_danfe   ) or
       ( p_saida-st_proc eq vg_st_fatura  ) or
       ( p_saida-st_proc eq vg_st_remessa ).

      "Estornar Picking
      perform f_estorno_picking_rem using p_saida-remessa+0(10)
                                 changing wl_erro
                                          p_saida.

      check wl_erro is initial.

      "Estornar Remessa
      perform f_estorno_remessa using p_saida-remessa+0(10)
                             changing wl_erro
                                      p_saida.

      check wl_erro is initial.

      perform f_after_estorno_remessa changing p_saida.

    endif.

    "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Comentando codigo abaixo
    "Eliminar o aviso
    if ( p_saida-st_proc    eq vg_st_aviso_rec     ) and
       ( vg_cockpit         eq '04'                ) and
       ( ( p_saida-aviso    is not initial         ) and
         ( p_saida-aviso(1) ne '@'                 ) ).

      refresh ti_bdcdata.
      perform f_bdc_data using:
             'SAPMV50A'  '4104'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'       '/00',
             ''          ''      ''   'LIKP-VBELN'       p_saida-aviso,

             'SAPMV50A'  '1000'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.

      clear wl_erro.
      perform f_call_transaction using 'VL32N'
                                       p_saida
                              changing wl_erro.
      if wl_erro is initial.
        p_saida-aviso   = icon_execute_object.
        p_saida-st_proc = vg_st_aviso_rec_before.
        update zsdt0001 set st_proc      = vg_st_aviso_rec_before
                            doc_aviso    = ''
        where ch_referencia = p_saida-ch_referencia.

        commit work.

        wait up to 2 seconds.
      else.
        exit.
      endif.
    endif.
    "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP


  elseif p_saida-operacao+0(4) eq 'ZRDC' or
         p_saida-operacao+0(4) eq 'ZRFL' or
         p_saida-operacao+0(4) eq 'ZIND' or
         p_saida-operacao+0(4) eq 'ZRAN'.

    if ( p_saida-st_proc eq vg_st_danfe  ) or
       ( p_saida-st_proc eq vg_st_fatura ).
      perform f_estorno_fatura using p_saida-fatura
                                     ''
                                     p_saida
                            changing wl_erro.
      check wl_erro is initial.
    endif.

    if ( p_saida-st_proc eq vg_st_danfe   ) or
       ( p_saida-st_proc eq vg_st_fatura  ) or
       ( p_saida-st_proc eq vg_st_remessa ).

      "MBST (estorno de migo)
      select single *
        from zsdt0023 into @data(wa_zsdt0023)
       where vbeln = @p_saida-remessa.

      select single *
        from lips into @data(wa_lips)
       where vbeln = @p_saida-remessa.

      "Se não existir item exclui somente a remessa
      if ( sy-subrc ne 0 ) and ( wa_zsdt0023-es_mblnr_s is not initial ).

        perform f_excluir_remessa using p_saida-remessa+0(10)
                                        p_saida
                               changing wl_erro.

      else.

        "Estornar Somente Picking
        perform f_estorno_picking_rem using p_saida-remessa+0(10)
                                   changing wl_erro
                                            p_saida.

        check wl_erro is initial.

        if ( wa_zsdt0023 is not initial ). "Fluxo de documento remessa formação de lote

          "BAPI Estorno da MIGO - ENTRADA
          "//Só faz se ainda esta em branco o doc material estorno entrada
          if wa_zsdt0023-es_mblnr_e is initial and wa_zsdt0023-mblnr_e is not initial. " Só faz se ainda esta em branco o doc material estorno entrada
            clear: wl_par_est_migo.
            wl_par_est_migo-mat_doc      = wa_zsdt0023-mblnr_e.
            wl_par_est_migo-doc_year     = wa_zsdt0023-mjahr_e.
            wl_par_est_migo-pstng_date   = wa_zsdt0023-dt_saida.
            wl_par_est_migo-ent_sai      = 'E'.
            wl_par_est_migo-form_lote    = 'X'.
            perform f_estorno_migo using wl_par_est_migo
                                         p_saida
                                changing wl_erro.

            check wl_erro is initial.
          endif.

          "BAPI Estorno da MIGO - SAIDA
          "//Só faz se ainda esta em branco o doc material estorno saida
          if wa_zsdt0023-es_mblnr_s is initial and wa_zsdt0023-mblnr_s is not initial.
            clear: wl_par_est_migo.
            wl_par_est_migo-mat_doc      = wa_zsdt0023-mblnr_s.
            wl_par_est_migo-doc_year     = wa_zsdt0023-mjahr_s.
            wl_par_est_migo-pstng_date   = wa_zsdt0023-dt_saida.
            wl_par_est_migo-ent_sai      = 'S'.
            wl_par_est_migo-form_lote    = 'X'.
            perform f_estorno_migo using wl_par_est_migo
                                         p_saida
                                changing wl_erro.

            check wl_erro is initial.
          endif.
        endif.

        "Estornar Somente Remessa
        perform f_estorno_remessa using p_saida-remessa+0(10)
                               changing wl_erro
                                        p_saida.

        check wl_erro is initial.

        perform f_after_estorno_remessa changing p_saida.

      endif.

    endif.
  elseif p_saida-operacao+0(3) eq 'ZUB'. "Pedido de transferencia

    if ( p_saida-st_proc eq vg_st_danfe   ) or
       ( p_saida-st_proc eq vg_st_fatura  ) or
       ( p_saida-st_proc eq vg_st_remessa ).

      "Estornar Picking
      perform f_estorno_picking_rem using p_saida-remessa+0(10)
                                 changing wl_erro
                                          p_saida.

      check wl_erro is initial.

      "Estornar Remessa
      perform f_estorno_remessa using p_saida-remessa+0(10)
                             changing wl_erro
                                      p_saida.

      check wl_erro is initial.

      perform f_after_estorno_remessa changing p_saida.

    endif.

  endif.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
  if ( p_saida-remessa  is initial or p_saida-remessa(1) eq '@' ) and
     ( p_saida-seq_lcto is  initial or p_saida-seq_lcto(1) eq '@' ).

    data(_msg_error) = zcl_comercializacao_algodao=>estornar_sobra_perda_romaneio( i_ch_referencia = conv #( p_saida-ch_referencia ) ).
    if _msg_error is not initial.
      message _msg_error type 'I'.
    endif.
  endif.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

endform.                    " F_ESTORNO_NFE

form f_pega_imagem  using    nome_logo
                    changing url.

  refresh graphic_table.
  call method cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    exporting
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    receiving
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  while l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    append graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  endwhile.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  append graphic_table.
  call function 'DP_CREATE_URL'
    exporting
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    tables
      data     = graphic_table
    changing
      url      = url.
endform.                    " F_PEGA_IMAGEM

form f_montar_layout_log.
  refresh estrutura.
  perform f_montar_estrutura using:
     1  ''   ''            'TI_ZLEST0100' 'CONT'     'Seq'         ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGID'    'ID'          ' ',
     1  ''   ''            'TI_ZLEST0100' 'MSGV1'    'Menssagem'   '60',
     1  ''   ''            'TI_ZLEST0100' 'DATA'     'Data'        '10',
     1  ''   ''            'TI_ZLEST0100' 'HORA'     'Hora'        '10',
     1  ''   ''            'TI_ZLEST0100' 'USUARIO'  'Usuário'     '15'.

endform.                    " MONTAR_LAYOUT

form f_montar_estrutura using value(p_col_pos)       type i
                              value(p_ref_tabname)   like dd02d-tabname
                              value(p_ref_fieldname) like dd03d-fieldname
                              value(p_tabname)       like dd02d-tabname
                              value(p_field)         like dd03d-fieldname
                              value(p_scrtext_l)     like dd03p-scrtext_l
                              value(p_outputlen).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

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
  if p_outputlen is initial.
    wa_estrutura-outputlen     = x_contador.
  else.
    wa_estrutura-outputlen     =  p_outputlen.
  endif.

  append wa_estrutura to estrutura.

endform.                    " montar_estrutura

form f_memorizar_dt_movimento_badi  using p_data_rem type ledat.

  types:
    begin of tab_type,
      para type string,
      dobj type string,
    end of tab_type.

  data: line type tab_type,
        itab type standard table of tab_type,
        id   type c length 10 value 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  append line to itab.

  export (itab) to memory id 'ROMRETRO'.

endform.                    " MEMORIZAR_DT_MOVIMENTO_BADI

form f_lanc_carta_correcao .
  data : vl_length          type i,
         vl_id              type zcarta_correcao-id_cc,
         ls_zcarta_correcao type zcarta_correcao,
         pa_docnum          type j_1bdocnum.

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

  clear: vl_id.
  vl_length = strlen( txt_correc ).

  select single max( id_cc )
    into vl_id
    from zcarta_correcao
   where docnum eq wa_saida-danfe.

  if vl_id is initial .
    vl_id  = 0.
  endif.

  vl_id = vl_id + 1.

  replace all occurrences of regex '[áàãâ]' in txt_correc with 'a' ignoring case.
  replace all occurrences of regex '[éê]'   in txt_correc with 'e' ignoring case.
  replace all occurrences of        'í'     in txt_correc with 'i' ignoring case.
  replace all occurrences of regex '[óô]'   in txt_correc with 'o' ignoring case.
  replace all occurrences of regex '[üú]'   in txt_correc with 'u' ignoring case.
  replace all occurrences of regex '[ç]'    in txt_correc with 'c' ignoring case.
  replace all occurrences of        '&'     in txt_correc with '&#38;'.
  replace all occurrences of        ''''    in txt_correc with '&#39;'.
  replace all occurrences of        'º'     in txt_correc with 'o' ignoring case.
  replace all occurrences of        'ª'     in txt_correc with 'a' ignoring case.

  select single * into @data(lv_doc_fiscal)
    from j_1bnfe_active
   where docnum eq @wa_saida-danfe.

  ls_zcarta_correcao-docnum      = wa_saida-danfe.
  ls_zcarta_correcao-id_cc       = vl_id.
  ls_zcarta_correcao-model       = lv_doc_fiscal-model.
  ls_zcarta_correcao-msg_correc1 = txt_correc(250).
  ls_zcarta_correcao-msg_correc2 = txt_correc+250(250).
  ls_zcarta_correcao-msg_correc3 = txt_correc+500(250).
  ls_zcarta_correcao-msg_correc4 = txt_correc+750(250).
  ls_zcarta_correcao-usuario     = sy-uname.
  ls_zcarta_correcao-novo_agente = wa_ag_frete-transpor2.
  modify zcarta_correcao from ls_zcarta_correcao.

  pa_docnum =  wa_saida-danfe.

  data: zcl_cce type ref to zcl_cce.
  free: zcl_cce.
  create object zcl_cce.

  data: lc_id_registtro type char12.
  lc_id_registtro = ls_zcarta_correcao-docnum && ls_zcarta_correcao-id_cc.

  zcl_cce->novo_registro( ).
  zcl_cce->zif_cadastro~set_registro( i_id_registro = lc_id_registtro ).
  zcl_cce->enviar( receiving e_enviada = data(_enviada) ).
  clear txt_correc.
  call method obg_descbox->delete_text.

  if _enviada eq abap_false.
    rollback work.
    return.
  endif.

  commit work.

  message 'Carta de Correção enviada com sucesso!' type 'S'.

endform.                    " LANC_CARTA_CORRECAO

form f_chk_estorno_fiscal using p_saida  type ty_saida
                                p_frete  type c
                       changing p_erro   type c.

  data vdocnum_est type j_1bdocnum.

  wait up to 5 seconds.
  clear: vl_docnum,vl_refkey, p_erro.

  check p_frete is initial.

  if ( p_saida-tipo = 'P' ) or ( p_saida-tipo = 'T' ).

    select single vbeln mjahr
      into (vl_vbeln,vl_mjahr)
      from vbfa
      where vbelv = p_saida-remessa+0(10)
      and vbtyp_n  = 'R'
      and vbtyp_v  = 'J'.

    concatenate vl_vbeln vl_mjahr into vl_refkey.
    select single docnum
      from j_1bnflin
      into vl_docnum
      where refkey = vl_refkey.
  else.
    select single docnum
      from j_1bnflin
      into vl_docnum
      where refkey = p_saida-fatura.
  endif.

  check sy-subrc = 0.

  clear vcandat.
  select single  candat
    from j_1bnfdoc
    into  vcandat
   where docnum     = vl_docnum.

  if vcandat is initial. "Documento Fiscal não está estornado ainda....

    "Verificar se documento esta autorizado na SEFAZ
    perform f_check_auth_doc using vl_docnum.

    if sy-subrc ne 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
      call function 'J_1B_NF_DOCUMENT_CANCEL'
        exporting
          doc_number               = vl_docnum
          ref_type                 = space
          ref_key                  = space
          can_dat                  = sy-datum
        importing
          doc_number               = vdocnum_est
        exceptions
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          nfe_cancel_simulation    = 6
          others                   = 7.
      if sy-subrc eq 0.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.
      else.
        p_erro = 'X'.
      endif.
    else.
      p_erro = 'X'.
    endif.

    check p_erro is not initial. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.

    clear: wa_zlest0100, ti_zlest0100[], vl_ponteiro.

    select  max( cont )
       from zlest0100
       into vl_ponteiro
       where ch_referencia = p_saida-ch_referencia.

    if sy-subrc = 0.
      add 1 to vl_ponteiro.
    else.
      vl_ponteiro = 1.
    endif.

    wa_zlest0100-mandt         = sy-mandt.
    wa_zlest0100-ch_referencia = p_saida-ch_referencia.
    wa_zlest0100-msgtyp     = 'E'.
    wa_zlest0100-msgspra    = sy-langu.
    wa_zlest0100-msgid      = 'LES'.
    wa_zlest0100-msgnr      = '000'.
    wa_zlest0100-msgv1      = 'Danfe, não estornada, automaticamente ==>'.
    concatenate wa_zlest0100-msgv1 vdocnum_est into wa_zlest0100-msgv1.
    wa_zlest0100-data       = sy-datum.
    wa_zlest0100-hora       = sy-uzeit.
    wa_zlest0100-usuario    = sy-uname.
    wa_zlest0100-cont       = vl_ponteiro.

    append wa_zlest0100 to ti_zlest0100.
    add 1 to vl_ponteiro.

    modify zlest0100 from table ti_zlest0100.
  endif.

endform.

form f_gerar_vt using p_tipo
                      p_tipo_chamada type char01
             changing p_erro
                      p_saida type ty_saida
                      it_tab_bapiret1 type tab_bapiret1
              raising zcx_error. "*-#133089-12.02.2024-JT

  types: begin of ty_vbpa,
           parvw type vbpa-parvw,
           kunnr type vbpa-kunnr,
           lifnr type vbpa-lifnr,
         end of ty_vbpa.

  data: wl_tvro         type tvro,
        it_vbpa         type table of ty_vbpa,
        wl_vbpa         type ty_vbpa,
        wl_a917         type a917,
        v_auart         type vbak-auart,
        v_bsart         type ekko-bsart,
        v_lifnr         type lfa1-lifnr,
        v_ktokk         type lfa1-ktokk,
        v_lzonel        type lfa1-lzone,
        v_lzonek        type kna1-lzone,
        v_kunnr         type vbpa-kunnr,
        v_stcd1         type kna1-stcd1,
        v_route         type trolz-route,
        v_knote         type tvkn-knote,
        it_lfa1_tmp     type table of lfa1 with header line,
        it_matnr        type table of zsdt0001-matnr with header line,
        wa_tab_bapiret1 like line of it_tab_bapiret1,
        v_msgi(200).


  "Tabelas BAPI

  data: st_headerdata       type bapishipmentheader,
        st_stagedata        type bapishipmentstage,
        t_stagedata         type table of bapishipmentstage,
        t_itemdata          type table of bapishipmentitem,
        st_itemdata         type bapishipmentitem,
        st_headerdata2      type bapishipmentheader,
        st_headerdataaction type bapishipmentheaderaction,
        st_zlest0002        type zlest0002,
        lc_saida            type ty_saida,
        it_saida_carga      type table of ty_saida with header line.

  data: tx_msg  type string,
        txterro type string.


  data: ws_zlest0135 type zlest0135,
        e_msg_erro   type bapiret2,
        e_status     type sy-subrc.


*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  clear: v_tknum, p_erro, it_saida_carga[].

  if ( p_saida-romaneios_agr[] is initial ) or ( p_saida-deliverys[] is initial ).
    clear: p_erro.
    perform f_set_romaneios_carga changing p_saida
                                           p_erro.
    check p_erro eq abap_false.

    perform f_set_delivery changing p_saida
                                    p_erro.
    check p_erro eq abap_false.
  endif.

  "Pré Validações
  clear: wa_zsdt0001.
  select single *
    from zsdt0001 into wa_zsdt0001
   where ch_referencia = p_saida-ch_referencia.

  if wa_zsdt0001-fknum gt 0 and vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
    tx_msg = 'Documento atualizado, click em <ATUALIZAR>'.
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    endcase.
    exit.
  endif.

  "Duplicidade VI/VT ZLES0136 - IR212606 - Issue 162099 -->>>
  do 2 times.
    select single tknum, id_romaneio
      from vttk into @data(lwa_vttk_exists)
     where id_romaneio eq @p_saida-ch_referencia.

    if sy-subrc eq 0.
      exit.
    else.
      wait up to 1 seconds.
    endif.
  enddo.

  if lwa_vttk_exists is not initial.
    tx_msg = |Ja existe a VT { lwa_vttk_exists-tknum } gerada para o romaneio!'|.
    case vg_faturamento_autom.
      when abap_off.
      when abap_true.
        data(l_mesg) = tx_msg.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
    endcase.
    exit.
  endif.
  "Duplicidade VI/VT ZLES0136 - IR212606 - Issue 162099 <<---

  if sy-tcode ne 'ZLES0136'
     and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
     and sy-tcode ne 'ZMM0127'
     and sy-tcode ne 'ZNFE' and p_tipo_chamada ne 'E' and "*-CS2024000086-26.09.2024-#151423-JT-inicio
     vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT

    tx_msg = 'Transação apenas de visualização'.
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    endcase.
    exit.
  endif.

  "Validar tipo de veiculo.
  "=======================================================================================
  clear: tx_msg, txterro.
  if wa_zsdt0001-placa_car1 is not initial.
    clear: st_zlest0002.
    select single * from zlest0002 into st_zlest0002
    where pc_veiculo eq wa_zsdt0001-placa_car1 and tp_veiculo eq '0'.
    if st_zlest0002 is not initial.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car1 } |.
      tx_msg = txterro.
    endif.
    clear txterro.
  endif.

  if wa_zsdt0001-placa_car2 is not initial.
    clear: st_zlest0002.
    select single * from zlest0002 into st_zlest0002
    where pc_veiculo eq wa_zsdt0001-placa_car2 and tp_veiculo eq '0'.
    if st_zlest0002 is not initial.
      clear: txterro.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car2 } |.
      if tx_msg is not initial.
        tx_msg = |{ tx_msg } / { wa_zsdt0001-placa_car2 } |.
      else.
        tx_msg = txterro.
      endif.
    endif.
  endif.

  if wa_zsdt0001-placa_car3 is not initial.
    clear: st_zlest0002.
    select single * from zlest0002 into st_zlest0002
    where pc_veiculo eq wa_zsdt0001-placa_car3 and tp_veiculo eq '0'.
    if st_zlest0002 is not initial.

      clear: txterro.
      txterro = | Corrigir o tipo de veiculo para Placa: { wa_zsdt0001-placa_car3 } |.
      if tx_msg is not initial.
        tx_msg = |{ tx_msg } / { wa_zsdt0001-placa_car3 } |.
      else.
        tx_msg = txterro.
      endif.
    endif.
  endif.

  if tx_msg is not initial.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message tx_msg type 'I'.
        exit.
      when abap_true.
        l_mesg = tx_msg.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.
  "======================================================================

  if p_saida-fatura is not initial.
    "Verifica Estorno Fatura
    select single vbeln mjahr
       into (vl_vbeln,vl_mjahr)
       from vbfa
      where vbelv = p_saida-fatura
        and vbtyp_n  = 'N'. "estorno
    if sy-subrc = 0.
      tx_msg = 'O doc de fatura está cancelado. Refazer o lançamento!'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

    clear vl_fksto.
    select single fksto
      from vbrk into vl_fksto
     where vbeln = p_saida-fatura.

    if  vl_fksto = 'X'.
      tx_msg = 'o doc de fatura está cancelado. Refazer o lançamento'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.
  endif.

  select single lifnr name1 dlgrp
    from lfa1 into wa_lfa1
   where lifnr = p_saida-lifnr.

  if sy-subrc = 0 and wa_lfa1-dlgrp not in r_dlgrp and
     (
       ( p_saida-inco1 ne 'FOB' and p_saida-inco1 ne 'CFR' ) or
       ( p_saida-enc_conhecimento eq abap_true )
     ).

    tx_msg = |Fornecedor { p_saida-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.|.

    "187202 - bug solto - RGA
    select count(*)
       from lfb1
       where lifnr = p_saida-lifnr
         and bukrs = wa_zsdt0001-bukrs
         and sperr = ''
         and loevm = ''.
    if sy-subrc ne 0.
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.

          concatenate 'Fornecedor' wa_zsdt0001-agente_frete 'não expandido para empresa.' wa_zsdt0001-agente_frete into tx_msg separated by space.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
    endif.
    "187202 - bug solto - RGA - FIM
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
      when abap_true.
        l_mesg = tx_msg.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    endcase.
    exit.
  endif.

  select single *
    from tvtk into @data(wl_tvtk)
   where shtyp = @p_saida-shtyp.

  if ( sy-subrc ne 0 ) or ( wl_tvtk-abfer is initial ).
    tx_msg = 'Tipo de Transporte não encontrado!'.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
      when abap_true.
        l_mesg = tx_msg.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    endcase.
    exit.
  endif.
  "Fim - Pré Validações

  if ( line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ) ) and ( p_tipo ne 'T' ).
    exit.
  endif.

  if ( lines( p_saida-romaneios_agr[] ) > 1 ) and ( p_tipo ne 'T' ).
    exit.
  endif.

  if ( p_saida-inco1 ne 'FOB' and p_saida-inco1 ne 'CFR' ) or
     ( p_saida-enc_conhecimento eq abap_true ).

    if p_saida-kbetr le 0.
      tx_msg = 'Não existe valor de frete cadastrado. Solicite à transportadora da sua região'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message i000(z01) with 'Não existe valor de frete cadastrado.' 'Solicite à transportadora da sua região'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.
    if p_saida-cont_fre gt 1.
      tx_msg = 'Existe mais de um valor de frete cadastrado. Solicite a regularização à transportadora da sua região'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message i000(z01) with 'Existe mais de um valor de frete cadastrado.' 'Solicite a regularização à transportadora ' 'da sua região'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

    if p_saida-lifnr is initial.
      tx_msg = 'Agente de frete não informado!'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          l_mesg = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

  endif.

  read table it_zsdt0001 into wa_zsdt0001 with key ch_referencia = p_saida-ch_referencia binary search.
  check sy-subrc = 0.
  if wa_zsdt0001-agente_frete is initial.
    wa_zsdt0001-agente_frete = p_saida-lifnr.
  endif.

  data(_error) = abap_false.
  perform f_set_romaneios_carga changing p_saida
                                         _error.
  check _error eq abap_false.

  perform f_valida_geracao_vt changing p_saida
                                       _error.
  check _error eq abap_false.

  clear: it_matnr[].

  if wa_zsdt0001-matnr is not initial.
    it_matnr = wa_zsdt0001-matnr.

    "CS2017002682 - 29.11.2017 - Ini
    if ( p_saida-tipo = 'O' ) and  ( vg_cockpit = '04' ).
      read table it_zsdt0062 into wa_zsdt0062 with key vbeln = p_saida-vbeln
                                                       ebeln = p_saida-ebeln
                                                       ebelp = p_saida-ebelp binary search.
      if sy-subrc ne 0.
        tx_msg = 'Este pedido/item não pertence a esta Ordem! Caso seja necessário, solicite a vinculação à equipe de insumos.'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message i000(z01) with 'Este pedido/item não pertence a esta Ordem!' 'Caso seja necessário, solicite a vinculação ' 'à equipe de insumos.'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.

      select single *
        from ekpo into @data(_wl_ekpo)
       where ebeln = @p_saida-ebeln
         and ebelp = @p_saida-ebelp.
      if ( sy-subrc ne 0 ) or ( _wl_ekpo-matnr is initial ).
        tx_msg = 'Pedido de Importação não existe!'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.

      it_matnr = _wl_ekpo-matnr.
    endif.
    append it_matnr.
  else.
    loop at tg_zsdt0001_item where ch_referencia = wa_zsdt0001-ch_referencia
                               and matnr is not initial.
      it_matnr = tg_zsdt0001_item-matnr.
      append it_matnr.
    endloop.
  endif.

  loop at p_saida-romaneios_agr into data(_wl_rom) where matnr is not initial.
    it_matnr = _wl_rom-matnr.
    append it_matnr.
  endloop.

  sort it_matnr.
  delete adjacent duplicates from it_matnr.

  if it_matnr[] is initial.
    tx_msg = 'Nenhum material encontrado para a carga!'.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
      when abap_true.
        l_mesg = tx_msg.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
    case p_tipo_chamada.
      when 'L'.
        message tx_msg type 'I'.
      when 'E'.
        "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
    endcase.
    exit.
  endif.


  if p_saida-inco1 = 'CIF'.

*** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023.
*    SELECT SINGLE *
*      from tvarvc INTO @DATA(lwa_tvarv_zseg)
*     WHERE name = 'ZLES0136_EXC_ZSEG'
*       AND LOW  = @wa_zsdt0001-agente_frete.

    clear: ws_zlest0135.
    call function 'Z_LES_EXC_ZSEG'
      exporting
        i_placa       = wa_zsdt0001-placa_cav
        i_ck_consulta = abap_true
      importing
        e_status      = e_status
        e_msg_erro    = e_msg_erro.



***** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / AOENNING.

    case e_status.
      when 1. "Se houve erro na comunicação da API.

*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message i000(z01) with e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
            return.
          when abap_true.
            message i000(z01) with e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3 into l_mesg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim

      when 2. "Check condição tp_transportador "ETC Não equiparado.


        loop at it_matnr.
          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          select single *
            from a917
            into wl_a917
           where kappl = 'F'
             and kschl        = 'ZSEG'
             and matnr        = it_matnr
             and tdlnr        = wa_zsdt0001-agente_frete
             and kfrst        = ''
             and datbi        ge sy-datum.

          if sy-subrc ne 0.
            concatenate 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' into v_msgi separated by space.
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
              when abap_true.
                message i000(z01) with 'Não existe % p/ desc. de Seguro. Mat.:' it_matnr v_msgi into l_mesg.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
            endcase.
*-#133089-21.02.2024-JT-fim
            case p_tipo_chamada.
              when 'L'.
                message i000(z01) with 'Não existe % p/ desc. de Seguro. Mat.:' it_matnr v_msgi.
              when 'E'.
                message i000(z01) with 'Não existe % p/ desc. de Seguro. Mat.:' it_matnr v_msgi into tx_msg.
                "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
            endcase.
            return.
          endif.

          "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
          select single *
            from a917
            into wl_a917
           where kappl = 'F'
             and kschl = 'ZIOF'
             and matnr = it_matnr
             and tdlnr = wa_zsdt0001-agente_frete
             and kfrst = ''
             and datbi ge sy-datum.

          if sy-subrc ne 0.
            concatenate 'Agente:' wa_zsdt0001-agente_frete 'Solicite ao depto de logística' into v_msgi separated by space.
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
              when abap_true.
                message i000(z01) with 'Não existe % p/ desc. de IOF Mat.:' it_matnr v_msgi into l_mesg.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
            endcase.
*-#133089-21.02.2024-JT-fim
            case p_tipo_chamada.
              when 'L'.
                message i000(z01) with 'Não existe % p/ desc. de IOF Mat.:' it_matnr v_msgi.
              when 'E'.
                message i000(z01) with 'Não existe % p/ desc. de IOF Mat.:' it_matnr v_msgi into tx_msg.
                "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
            endcase.
            return.
          endif.
        endloop.


      when 3. "Se não localizou dados, retorna a msg que retornou no JSON da API.

*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message i000(z01) with e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3.
            return.
          when abap_true.
            message i000(z01) with e_msg_erro-message_v1 e_msg_erro-message_v2 e_msg_erro-message_v3 into l_mesg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim

      when others.
    endcase.

  endif.

*-----------------------------------------------------------------------------------*
*  Preenche BAPI
*-----------------------------------------------------------------------------------*

  clear: wa_tvtk.
  select single shtyp laufk vsart
    from tvtk
    into wa_tvtk
   where shtyp = wa_zsdt0001-shtyp.

  " Cabeçalho
  clear st_headerdata.
  st_headerdata-service_agent_id        = wa_zsdt0001-agente_frete.

*  st_headerdata-text_1                  = wa_zsdt0001-placa_cav.



  zcl_atribui_rom_doctrans=>set_info_rom_doc_trans(
    exporting
      i_rom      = wa_zsdt0001    " Estrutura para LES de Saída de Carga
    changing
      i_doctrans = st_headerdata    " TransportBAPI, Dados de cabeçalho
  ).

  st_headerdata-service_level           = '1'.
  st_headerdata-shipping_type           = wa_tvtk-vsart.
  st_headerdata-status_plan             = 'X'.
  st_headerdata-status_checkin          = 'X'.
  st_headerdata-status_load_start       = 'X'.
  if p_saida-krech = 'A'. "Percentual.
    st_headerdata-special_procedure_id 	  = '0003'.
  elseif p_saida-krech = 'B'. "Montante Fixo.
    st_headerdata-special_procedure_id 	  = '0002'.
  else.
    st_headerdata-special_procedure_id 	  = '0001'.
  endif.
  st_headerdata-shpmnt_cost_rel         = 'X'.
  st_headerdata-shipment_type           = wa_zsdt0001-shtyp.

  "Início - LES - CS2023000206 - Ajuste contab. CT-e Transf. #107013 RSA
  st_headerdata-trans_plan_pt           = wa_zsdt0001-branch.
  if wa_zsdt0001-shtyp = 'Z020'.
    clear : st_headerdata-trans_plan_pt.
    st_headerdata-trans_plan_pt         = wa_zsdt0001-id_cli_dest+6(4).
  endif.
  "Fim - LES - CS2023000206 - Ajuste contab. CT-e Transf. #107013 RSA

  if wa_zsdt0001-id_interface = '49'.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_saida-local_entrega
      importing
        output = p_saida-local_entrega.

    st_headerdata-trans_plan_pt = p_saida-local_entrega+6(4).
  endif.

  v_lifnr = p_saida-lifnr_c.

  select single lzone ktokk
    from lfa1
    into (v_lzonel, v_ktokk)
    where lifnr   = v_lifnr.

  check sy-subrc = 0.

  clear v_route.

  v_route = p_saida-route.
  st_headerdata-shipment_route  = v_route.

  select single *
    from tvro
    into wl_tvro
    where route    = v_route.

  check sy-subrc = 0.
  if wl_tvro-traztd gt 24.
    wl_tvro-traztd = wl_tvro-traztd / 24.
  endif.

  st_headerdata-distance        = wl_tvro-distz.
  st_headerdata-distance_unit   = wl_tvro-medst.
  st_headerdata-time_travel     =	wl_tvro-traztd.
  st_headerdata-time_unit       =	'H'.

*ETAPA
  clear st_stagedata.
  refresh t_stagedata.
  st_stagedata-stage_cat      = '1'.
  st_stagedata-stage_seq     	= '0001'.
  st_stagedata-service_agent  = wa_zsdt0001-agente_frete.

  "Local de Partida
  if v_ktokk = 'ZFIC'.
    st_stagedata-org_shipp_dpmnt = v_lifnr+6(4).
  else.
    st_stagedata-org_suppl  = v_lifnr.
  endif.

  v_kunnr = p_saida-local_entrega.

  check sy-subrc = 0.

  st_stagedata-shipping_type  = wa_tvtk-vsart.
  st_stagedata-leg_indicator  = wa_tvtk-laufk. "Código de Percurso

  "Se Código de Percurso  igual a 1:  Percurso preliminar
  if wa_tvtk-laufk = 1.
    clear v_knote.

    select single knote
      from tvkn
      into v_knote
     where kunnr   = v_kunnr.

    if sy-subrc = 0.
      st_stagedata-dest_point   = v_knote.
    else.
      st_stagedata-dest_point   = v_kunnr.
    endif.

    "Se Código de Percurso  igual a 4:  Percurso direto
  elseif wa_tvtk-laufk = 4.

    if ( st_headerdata-shipment_type  =  'Z004' ) or
       ( st_headerdata-shipment_type  =  'Z029' ).

      clear: it_lfa1_tmp[], v_lifnr, v_stcd1.

      select single stcd1
        from kna1
        into v_stcd1
       where kunnr = v_kunnr.

      if v_stcd1 is not initial.
        select *
          from lfa1 into table it_lfa1_tmp
         where stcd1 = v_stcd1.

        select single *
          from j_1bbranch into @data(_wl_branch_vt)
         where branch eq @st_headerdata-trans_plan_pt.

        if sy-subrc eq 0.
          "Elimina Fornecedores Bloqueados
          perform f_elimina_lfa1_bloq tables it_lfa1_tmp
                                       using _wl_branch_vt-bukrs.
        endif.

        loop at it_lfa1_tmp.
          v_lifnr = it_lfa1_tmp-lifnr.
          exit.
        endloop.
      endif.

      if v_lifnr is initial.
        concatenate 'Não foi possivel encontrar na transação XK03 o parceiro fornecedor na empresa:' _wl_branch_vt-bukrs 'referente ao codigo cliente: ' v_kunnr into v_msgi separated by space.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            message i000(z01) with v_msgi(50) v_msgi+50(50) v_msgi+100(50) into l_mesg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message i000(z01) with v_msgi(50) v_msgi+50(50) v_msgi+100(50) .
          when 'E'.
            message i000(z01) with v_msgi(50) v_msgi+50(50) v_msgi+100(50) into tx_msg.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        return.
      endif.

      st_stagedata-dest_suppl  = v_lifnr.

    else.
      st_stagedata-dest_cust    = v_kunnr. "Local de entrega (V_KUNNR)
    endif.

  endif.

  append st_stagedata to t_stagedata.

*Dados itens
  clear st_itemdata.
  refresh t_itemdata.
  loop at p_saida-deliverys into data(_wl_likp).
    st_itemdata-delivery      = _wl_likp-vbeln.
    st_itemdata-itenerary     = '000010'.
    append st_itemdata to t_itemdata.
  endloop.

  clear v_tknum.
  refresh t_return_vt.

  "------> Gera o Transporte <------
  call function 'BAPI_SHIPMENT_CREATE'
    exporting
      headerdata = st_headerdata
    importing
      transport  = v_tknum
    tables
      itemdata   = t_itemdata
      stagedata  = t_stagedata
      return     = t_return_vt.

  if v_tknum is initial.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    p_erro = 'X'.

    if p_tipo_chamada = 'E'.
      loop at t_return_vt into data(wa_erros_vt).
        clear: wa_tab_bapiret1.
        move-corresponding wa_erros_vt to wa_tab_bapiret1.
        append wa_tab_bapiret1 to it_tab_bapiret1.
      endloop.
    endif.

  else.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    clear : st_headerdataaction, st_headerdata2.
    refresh: t_return_vt, it_tab_bapiret1.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = v_tknum
      importing
        output = st_headerdata2-shipment_num.

    st_headerdata2-status_load_end       = 'X'.
    st_headerdataaction-status_load_end  = 'C'.

    call function 'BAPI_SHIPMENT_CHANGE'
      exporting
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      tables
        return           = t_return_vt.

    if p_tipo_chamada = 'E'.
      loop at t_return_vt into wa_erros_vt.
        clear: wa_tab_bapiret1.
        move-corresponding wa_erros_vt to wa_tab_bapiret1.
        append wa_tab_bapiret1 to it_tab_bapiret1.
      endloop.
    endif.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    if p_saida-inco1 = 'CIF' and p_tipo_chamada eq 'L'.
      call function 'Z_LES_VERIFICA_PED_ADM'
        exporting
          p_tknum      = st_headerdata2-shipment_num
        exceptions
          adiantamento = 1
          pedagio      = 2
          others       = 3.
    endif.

    if sy-subrc is not initial.
      p_erro = abap_true.

      if p_tipo_chamada = 'L'.
        message id sy-msgid type 'I' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      "Estorna VT
      refresh t_itemdata.

      loop at p_saida-deliverys into _wl_likp.
        clear st_itemdata.
        st_itemdata-delivery  = _wl_likp-vbeln.
        st_itemdata-itenerary = '0010'.
        append st_itemdata to t_itemdata.
      endloop.

      move-corresponding wa_zsdt0001 to lc_saida.
      lc_saida-transp = st_headerdata2-shipment_num.
      perform f_elimina_vt tables t_itemdata changing p_tipo_chamada lc_saida it_tab_bapiret1.

    else.

      st_headerdata2-status_compl             = 'X'.
      st_headerdata2-status_shpmnt_start      = 'X'.
      st_headerdataaction-status_compl        = 'C'.
      st_headerdataaction-status_shpmnt_start = 'C'.

      call function 'BAPI_SHIPMENT_CHANGE'
        exporting
          headerdata       = st_headerdata2
          headerdataaction = st_headerdataaction
        tables
          return           = t_return_vt.
*
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      if p_tipo_chamada = 'E'.
        loop at t_return_vt into wa_erros_vt.
          clear: wa_tab_bapiret1.
          move-corresponding wa_erros_vt to wa_tab_bapiret1.
          append wa_tab_bapiret1 to it_tab_bapiret1.
        endloop.
      endif.

      "atualiza valor do frete Historico
      loop at p_saida-romaneios_agr into _wl_rom.
        update zsdt0001 set kbetr = p_saida-kbetr konwa = p_saida-konwa agente_frete = p_saida-lifnr where ch_referencia = _wl_rom-ch_referencia.
      endloop.

      if p_saida-id_ordem is not initial.
        update zlest0155 set ch_referencia = p_saida-ch_referencia where id_ordem = p_saida-id_ordem.
      endif.
      p_erro = 'N'.

    endif.
  endif.

  perform f_prepare_return tables t_return_vt.
  perform f_grava_log_erro tables tg_log_erro using p_saida.

endform.                    " F_GERAR_VT

form f_pega_frete.

  data: v_cont_ped  type i.

  data: v_kunnr_lr   type kunnr,
        v_lifnr_z1   type lifnr,
        v_lifnr_sp   type lifnr,
        v_lifnr_ori  type lifnr,
        v_tp_veiculo type zde_tp_prop_veiculo.

  refresh: it_a900, it_a910, it_a911, it_a915, it_a918, it_a919, it_konp.

  check it_zsdt0001_fre[] is not initial.

*---> 11.07.2023 10:37:58 - Migração S4 - DL
  sort it_ekpv by ebeln.
  sort it_ekko by ebeln.
  sort it_ekpa_pr by ebeln.
  sort it_ekpo by ebeln.
  sort it_likp by vbeln.
  sort it_vbpa by vbeln parvw.
  sort it_vbkd by vbeln.
  sort it_vbap by vbeln.
  sort it_kna1 by kunnr.
  sort it_kna1 by kunnr.
  sort it_vbkd by vbeln.
*<--- 11.07.2023 10:37:58 - Migração S4 - DL


  loop at it_zsdt0001_fre assigning field-symbol(<out_zsdt0001>).

    data(tabix) = sy-tabix.

    clear wa_saida.

    read table it_vbak into wa_vbak with key vbeln = <out_zsdt0001>-vbeln binary search. " Ordem
    if sy-subrc = 0.

      clear: wa_vbkd.

      " Exclui Romaneio do SET
      read table t_auart with key from = wa_vbak-auart binary search.
      if sy-subrc = 0.
        continue.
      endif.
      "ZONA coleta ordem
      read table it_vbpa_co into wa_vbpa_co with key vbeln = <out_zsdt0001>-vbeln binary search. " Ordem
      if sy-subrc = 0.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_vbpa_co-lifnr binary search.
        <out_zsdt0001>-lzonea = wa_lfa1-lzone.
      endif.
      "Zona Local de entrega
      read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                parvw = 'LR' binary search.
      if sy-subrc = 0.
        read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
        if sy-subrc = 0.
          <out_zsdt0001>-lzonez = wa_kna1-lzone.
        endif.
      endif.

      "Agregado
      select single agregado
        from zlest0002 into @data(v_agregado)
       where pc_veiculo = @<out_zsdt0001>-placa_cav.

      if sy-subrc = 0.
        if v_agregado = 1.
          <out_zsdt0001>-add01 = '0000000001'.
        else.
          <out_zsdt0001>-add01 = '0000000002'.
        endif.
      endif.

      "Itinerário
      read table it_vbap into wa_vbap with key vbeln = <out_zsdt0001>-vbeln binary search. " Ordem
      if sy-subrc = 0.
        <out_zsdt0001>-route = wa_vbap-route.
      endif.

      read table it_vbkd into wa_vbkd with key vbeln = <out_zsdt0001>-vbeln binary search.

      "Definir Tipo de transporte
      case vg_cockpit. "Validar Regra ###
        when '01' or '05' or '06' or '07' or '09' or '03'.

          clear: v_kunnr_lr, v_lifnr_z1, v_lifnr_sp.

          if ( 'ZRDC_ZRFL_ZIND' cs wa_vbak-auart ) and ( wa_vbak-auart is not initial ).
            "Determinar Local de Entrega
            read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'LR' binary search.
            if sy-subrc eq 0.
              v_kunnr_lr = wa_vbpa-kunnr.
            endif.

            "Determinar Terminal Porto
            read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'Z1' binary search.
            if sy-subrc eq 0.
              v_lifnr_z1 = wa_vbpa-lifnr.
            endif.

            "Determinar Agente Frete
            read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'SP'
                                                      dlgrp = '0007'. "Multimodal
            if sy-subrc = 0.

              v_lifnr_sp = wa_vbpa-lifnr.

            elseif ( wa_vbkd-inco1 = 'CIF' ).

              read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                        parvw = 'SP' binary search.
              if sy-subrc = 0.
                v_lifnr_sp = wa_vbpa-lifnr.
                perform f_troca_agente using <out_zsdt0001>-placa_cav <out_zsdt0001>-branch changing v_lifnr_sp.
              endif.

            endif.

*            IF ( <OUT_ZSDT0001>-AGENTE_FRETE IS INITIAL ) AND ( V_LIFNR_SP IS NOT INITIAL ).
            if  ( v_lifnr_sp is not initial ).
              <out_zsdt0001>-agente_frete = v_lifnr_sp.
              perform f_troca_agente using <out_zsdt0001>-placa_cav <out_zsdt0001>-branch changing <out_zsdt0001>-agente_frete.
            endif.

          endif.

          try.
              zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                exporting
                  i_tipo_mov       = conv #( wa_vbak-tp_movimento )
                  i_vsart          = '01'  "Rodoviario
                  i_tipo_ov        = conv #( wa_vbak-auart )
                  i_parid_lr       = conv #( v_kunnr_lr )
                  i_parid_z1       = conv #( v_lifnr_z1 )
                  i_parid_sp       = conv #( v_lifnr_sp )
                importing
                   e_shtyp         = data(_shtyp) ).

              <out_zsdt0001>-shtyp = _shtyp.
            catch zcx_faturamento.
            catch zcx_error.
          endtry.

          "Comentario Final Codigo - 0001 - 18.04.2019

        when '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
        when '03'. "Commodities (Trânsferência Recebidas )
        when '04'.  "Fertilizantes (Porto Velho) - ZLES0115

          if <out_zsdt0001>-ebeln is initial.
            clear v_cont_ped.
            loop at it_zsdt0062 into wa_zsdt0062 where vbeln = <out_zsdt0001>-vbeln
                                                   and matnr = <out_zsdt0001>-matnr. "CS2017002682 - 29.11.2017
              <out_zsdt0001>-ebeln  = wa_zsdt0062-ebeln. "PEDIDO Importação
              <out_zsdt0001>-ebelp  = wa_zsdt0062-ebelp. "CS2017002682 - 29.11.2017
              add 1 to v_cont_ped.
            endloop.
            if v_cont_ped gt 1.
              clear: <out_zsdt0001>-ebeln, <out_zsdt0001>-ebelp.
            endif.
          endif.

          "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
          try.
              zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                exporting
                  i_tipo_mov       = conv #( wa_vbak-tp_movimento )
                  i_vsart          = '01'  "Rodoviario
                  i_tipo_ov        = conv #( wa_vbak-auart )
                importing
                   e_shtyp         = _shtyp ).

              <out_zsdt0001>-shtyp = _shtyp.
            catch zcx_faturamento.
            catch zcx_error.
          endtry.

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

      endcase.

    else." Pedido de Compra

      case vg_cockpit. "Validar Regra ###
        when '01' or '05' or '06' or '07' or '09'.

          "Itinerário
          read table it_ekpv into wa_ekpv with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedido
          if sy-subrc = 0.
            <out_zsdt0001>-route = wa_ekpv-route.
          endif.
          read table it_ekko into wa_ekko with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedidos de transferencia
          if sy-subrc = 0 .

            try.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  exporting
                    i_tipo_mov       = conv #( wa_ekko-tp_movimento )
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = conv #( wa_ekko-bsart )
                  importing
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              catch zcx_faturamento.
              catch zcx_error.
            endtry.
            "Comentario Final Codigo - 0002 - 18.04.2019
          endif.

        when '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
        when '03'. "Commodities (Trânsferência Recebidas )
        when '04' or  "Fertilizantes (Porto Velho) - ZLES0115
             '10'.    "Romaneio de Entrada Completo Transferência

          read table it_ekpa_pr into wa_ekpa_pr with key ebeln = <out_zsdt0001>-vbeln binary search. " pedido
          if sy-subrc = 0.
            wa_saida-ponto_coleta = wa_ekpa_pr-lifn2.
          endif.

          if wa_saida-ponto_coleta is not initial.
            select single lifnr name1 dlgrp lzone
              from lfa1 into wa_lfa1
             where lifnr = wa_saida-ponto_coleta.
            <out_zsdt0001>-lzonea = wa_lfa1-lzone.
          endif.

          read table it_ekpo into wa_ekpo with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedidos de transferencia
          if sy-subrc = 0.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wa_ekpo-werks
              importing
                output = wa_saida-local_entrega.
          endif.

          if wa_saida-local_entrega is not initial.
            select single  kunnr name1 lzone
              from kna1
              into wa_kna1
              where kunnr = wa_saida-local_entrega.
            <out_zsdt0001>-lzonez = wa_kna1-lzone.
          endif.

          clear: wa_ekko.
          read table it_ekko into wa_ekko with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedidos de importação de fertilizante / transferencia

          "Itinerário
          if wa_ekko-bsart ne 'ZUB'. "'T'

            select single *
              from trolz into @data(wa_trolz)
             where aland = 'BR'
               and azone = @wa_lfa1-lzone
               and lland = 'BR'
               and lzone = @wa_kna1-lzone.
            if sy-subrc = 0.
              <out_zsdt0001>-route = wa_trolz-route.
            endif.

            try.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  exporting
                    i_tipo_mov       = conv #( wa_ekko-tp_movimento )
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = conv #( wa_ekko-bsart )
                  importing
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              catch zcx_faturamento.
              catch zcx_error.
            endtry.
            "Comentario Final Codigo - 0002 - 18.04.2019

            if ( <out_zsdt0001>-route is initial ) and ( <out_zsdt0001>-doc_aviso is not initial ).
              read table it_likp into data(wa_likp) with key vbeln = <out_zsdt0001>-doc_aviso.
              if ( sy-subrc = 0 ) and ( wa_likp-route is not initial ).
                <out_zsdt0001>-route = wa_likp-route.
              endif.
            endif.

          else.                 "Pedido de Transferência

            read table it_ekpv into wa_ekpv with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedido
            if sy-subrc = 0.
              <out_zsdt0001>-route = wa_ekpv-route.
            endif.

            "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
            try.
                zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_transporte(
                  exporting
                    i_tipo_mov       = 'S'
                    i_vsart          = '01'  "Rodoviario
                    i_tipo_pedido    = conv #( wa_ekko-bsart )
                  importing
                     e_shtyp         = _shtyp ).

                <out_zsdt0001>-shtyp = _shtyp.
              catch zcx_faturamento.
              catch zcx_error.
            endtry.


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



          endif.

      endcase.

      read table it_ekpo into wa_ekpo with key ebeln = <out_zsdt0001>-vbeln binary search. " Pedidos de transferencia
      if sy-subrc = 0.
        if wa_ekpo-inco1 = 'CIF'.
          "Agregado
          select single agregado
            from zlest0002 into v_agregado
           where pc_veiculo = <out_zsdt0001>-placa_cav.
          if sy-subrc = 0.
            if v_agregado = 1.
              <out_zsdt0001>-add01 = '0000000001'.
            else.
              <out_zsdt0001>-add01 = '0000000002'.
            endif.
          endif.
        endif.
      endif.
    endif.

    "Agente de Frete
    if <out_zsdt0001>-agente_frete is initial.
      read table it_vbak into wa_vbak with key vbeln = <out_zsdt0001>-vbeln binary search. " Ordem
      if sy-subrc = 0.
        read table it_vbkd into wa_vbkd with key vbeln = <out_zsdt0001>-vbeln binary search.
        if sy-subrc = 0.
          if ( 'ZRFL_ZRDC_ZIND' cs wa_vbak-auart ) and
             ( wa_vbkd-inco1 = 'CIF'        ) and
             ( wa_vbak-auart is not initial ).

            read table it_vbpa into wa_vbpa  with key vbeln = <out_zsdt0001>-vbeln
                                                      parvw = 'SP' binary search.
            <out_zsdt0001>-agente_frete           = wa_vbpa-lifnr.
            perform f_troca_agente using <out_zsdt0001>-placa_cav <out_zsdt0001>-branch changing <out_zsdt0001>-agente_frete.
          endif.
        endif.
      endif.
    endif.

*-CS2021000253-26.04.2024-#59941-WPP-inicio
    if <out_zsdt0001>-id_ordem is not initial.
      select single agente_frete
        into @data(l_agente_frete)
        from zlest0185
       where id_ordem = @<out_zsdt0001>-id_ordem.

      if sy-subrc = 0 and l_agente_frete is not initial.
        <out_zsdt0001>-agente_frete = l_agente_frete.
      endif.
    endif.
*-CS2021000253-26.04.2024-#59941-WPP-fim

*US-169528 25/07/2025 WBARBOSA INICIO
    if <out_zsdt0001>-id_interface eq '48'. "Sementes
      if <out_zsdt0001>-nro_cg is not initial.
        call method zcl_carga_saida_insumos=>get_agente_frete
          exporting
            i_bukrs  = <out_zsdt0001>-bukrs
            i_branch = <out_zsdt0001>-branch
            i_nro_cg = <out_zsdt0001>-nro_cg
          importing
            e_tdlnr  = <out_zsdt0001>-agente_frete.
      endif.
    endif.
*US-169528 25/07/2025 WBARBOSA FIM

    select single max( id_cc )
      from zcarta_correcao into @data(vl_id)
     where docnum        eq @<out_zsdt0001>-nro_nf_prod
       and authcode      ne ''
       and novo_agente   ne ''.

    if vl_id gt 0.
      select single *
        from zcarta_correcao into @data(wa_carta)
       where docnum        eq @<out_zsdt0001>-nro_nf_prod
         and authcode      ne ''
         and novo_agente   ne ''
         and id_cc         eq @vl_id.
      if sy-subrc = 0.
        if wa_carta-novo_agente ne <out_zsdt0001>-agente_frete.
          <out_zsdt0001>-agente_frete = wa_carta-novo_agente.
        endif.
      endif.
    endif.

  endloop.


  refresh it_konp.
  "Tp.transp./ForncServ./ItinTransp/Contrato/Agregado
  select *
    from a900
    into table it_a900
     for all entries in it_zsdt0001_fre
   where kappl eq 'F'
    and  kschl eq 'ZFRE'
    and  shtyp eq it_zsdt0001_fre-shtyp
    and  tdlnr eq it_zsdt0001_fre-agente_frete
    and  route eq it_zsdt0001_fre-route
    and  add01 eq it_zsdt0001_fre-add01
    and  kfrst eq ''
    and  datab le sy-datum
    and  datbi ge sy-datum
    and  exists ( select *
                  from  konp
                  where knumh = a900~knumh
                  and   loevm_ko eq '' ).

  if it_a900[] is not initial.
    select *
      from konp
      into table it_konp
      for all entries in it_a900
      where knumh = it_a900-knumh
      and   loevm_ko eq ''.
  endif.

  " Tp.transp./ForncServ./Zona part./Zona cheg.
  select *
   from a910 into table it_a910
    for all entries in it_zsdt0001_fre
  where kappl eq 'F'
    and kschl eq 'ZFRE'
    and shtyp eq it_zsdt0001_fre-shtyp
    and tdlnr eq it_zsdt0001_fre-agente_frete
    and lzonea eq it_zsdt0001_fre-lzonea
    and lzonez eq it_zsdt0001_fre-lzonez
    and kfrst eq ''
    and  datab le sy-datum
    and  datbi ge sy-datum
    and  exists ( select *
                  from  konp
                  where knumh = a910~knumh
                  and   loevm_ko eq '' ).

  if it_a910[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a910
     where knumh = it_a910-knumh
       and loevm_ko eq ''.
  endif.

  "Tp.transp./ForncServ./ItinTransp/Contrato
  select *
    from a911 into table it_a911
     for all entries in it_zsdt0001_fre
   where kappl eq 'F'
     and kschl eq 'ZFRE'
     and shtyp eq it_zsdt0001_fre-shtyp
     and tdlnr eq it_zsdt0001_fre-agente_frete
     and route eq it_zsdt0001_fre-route
     and kfrst eq ''
     and  datab le sy-datum
     and  datbi ge sy-datum
     and  exists ( select *
                  from  konp
                  where knumh = a911~knumh
                  and   loevm_ko eq '' ).

  if it_a911[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a911
     where knumh = it_a911-knumh
       and loevm_ko eq ''.
  endif.

  " Tp.transp./ForncServ./Zona part./Zona cheg./Agregado
  select *
   from a915 into table it_a915
    for all entries in it_zsdt0001_fre
  where kappl eq 'F'
    and kschl eq 'ZFRE'
    and shtyp eq it_zsdt0001_fre-shtyp
    and tdlnr eq it_zsdt0001_fre-agente_frete
    and lzonea eq it_zsdt0001_fre-lzonea
    and lzonez eq it_zsdt0001_fre-lzonez
    and add01  eq it_zsdt0001_fre-add01
    and kfrst eq ''
    and  datab le sy-datum
    and  datbi ge sy-datum
    and  exists ( select *
                  from  konp
                  where knumh = a915~knumh
                  and   loevm_ko eq '' ).

  if it_a915[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a915
     where knumh = it_a915-knumh
       and loevm_ko eq ''.
  endif.

  " Tp.transp./ForncServ./Material/Zona part./Zona cheg./Suplem.
  select *
   from a918 into table it_a918
    for all entries in it_zsdt0001_fre
  where kappl eq 'F'
    and kschl eq 'ZFRE'
    and shtyp eq it_zsdt0001_fre-shtyp
    and tdlnr eq it_zsdt0001_fre-agente_frete
    and matnr eq it_zsdt0001_fre-matnr
    and lzonea eq it_zsdt0001_fre-lzonea
    and lzonez eq it_zsdt0001_fre-lzonez
    and add01  eq it_zsdt0001_fre-add01
    and kfrst eq ''
    and  datab le sy-datum
    and  datbi ge sy-datum
    and  exists ( select *
                  from  konp
                  where knumh = a918~knumh
                  and   loevm_ko eq '' ).

  if it_a918[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a918
     where knumh = it_a918-knumh
       and loevm_ko eq ''.
  endif.

  "Tp.transp./ForncServ./Material/Zona part./Zona cheg.
  select *
    from a919 into table it_a919
     for all entries in it_zsdt0001_fre
   where kappl eq 'F'
     and kschl eq 'ZFRE'
     and shtyp eq it_zsdt0001_fre-shtyp
     and tdlnr eq it_zsdt0001_fre-agente_frete
     and matnr eq it_zsdt0001_fre-matnr
     and lzonea eq it_zsdt0001_fre-lzonea
     and lzonez eq it_zsdt0001_fre-lzonez
     and kfrst eq ''
     and  datab le sy-datum
     and  datbi ge sy-datum
     and  exists ( select *
                  from  konp
                  where knumh = a919~knumh
                  and   loevm_ko eq '' ).

  if it_a919[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a919
     where knumh = it_a919-knumh
       and loevm_ko eq ''.
  endif.

*-CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
  free: it_zsdt0001_aux.
  loop at  it_zsdt0001_fre into  wa_zsdt0001_fre.
    clear wa_zsdt0001_aux.
    move-corresponding wa_zsdt0001_fre to wa_zsdt0001_aux.
    move wa_zsdt0001_fre-add01 to l_add01.
    move l_add01               to wa_zsdt0001_aux-sdabw.

    if wa_zsdt0001_fre-id_ordem is not initial.
      select single viagem_id
        into wa_zsdt0001_aux-viagem_id
        from zlest0185
       where id_ordem eq wa_zsdt0001_fre-id_ordem.
    endif.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    if wa_zsdt0001_fre-id_interface = '48'. "Sementes
      select single viagem_id
       into wa_zsdt0001_aux-viagem_id
       from zsdt0133
      where nro_cg eq wa_zsdt0001_fre-nro_cg.
    endif.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    append wa_zsdt0001_aux   to it_zsdt0001_aux.
  endloop.

  "Tp.transp./Contrato/ Id Viagem.
  select *
    from a942 into table it_a942
     for all entries in it_zsdt0001_aux
   where kappl eq 'F'
     and kschl eq 'ZFRE'
     and shtyp eq it_zsdt0001_aux-shtyp
     "AND SDABW EQ '0001' "it_zsdt0001_aux-sdabw / ISSUE 173249 / AOENNING.  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
     and id_viagem eq it_zsdt0001_aux-viagem_id
     and kfrst eq ''
     and  datab le sy-datum
     and  datbi ge sy-datum
     and  exists ( select *
                  from  konp
                  where knumh = a942~knumh
                  and   loevm_ko eq '' ).
  if it_a942[] is not initial.
    select *
      from konp appending table it_konp
       for all entries in it_a942
     where knumh = it_a942-knumh
       and loevm_ko eq ''.
  endif.
*-CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

  sort: it_a900 by shtyp tdlnr route add01,
        it_a910 by shtyp tdlnr lzonea lzonez,
        it_a911 by shtyp tdlnr route,
        it_a915 by shtyp tdlnr lzonea lzonez add01,
        it_a918 by shtyp tdlnr matnr lzonea lzonez add01,
        it_a919 by shtyp tdlnr matnr lzonea lzonez,
        it_a942 by shtyp sdabw id_viagem,
        it_konp by knumh.
endform.                    "Pega_FRETE

form f_saida.

  "Montagem Form de Saida
  if ( vg_cockpit eq '01' ) or
     ( vg_cockpit eq '03' ) or "ALRS
     ( vg_cockpit eq '05' ) or
     ( vg_cockpit eq '06' ) or
     ( vg_cockpit eq '07' ) or
     ( vg_cockpit eq '09' ) or
     ( vg_cockpit eq '10' ).
    "Nome Perform para processamento dos dados do Romaneio
    concatenate 'F_SAIDA_' '01' into data(form_f_saida).
  else.
    "Nome Perform para processamento dos dados do Romaneio
    concatenate 'F_SAIDA_' vg_cockpit into form_f_saida.   "Montagem Form de Saida
  endif.

  "Execução Perform
  perform (form_f_saida) in program zlesr0102 if found.

endform.

form f_saida_01.

  data: tabix                type sy-tabix,
        l_erro               type char1,
        v_cont_fre           type i,
        v_cd_uf              type zlest0002-cd_uf,
        vpeso_retido_i       type i,
        v_chv_fat_vt         type zch_ref,
        vmatnr18             type matnr18,
        wa_zsdt0001e         type ty_zsdt0001,
        wa_zlest0019         type zlest0019,
        wa_zlest0041         type zlest0041,
        wa_zlest0087         type zlest0087,
        tl_zmmt_ee_zgr       type table of zmmt_ee_zgr with header line,
        tl_zmmt_ee_zgr_docs  type table of zmmt_ee_zgr_docs with header line,
        tl_zmmt_ee_zgr2      type table of zmmt_ee_zgr with header line,
        tl_zmmt_ee_zgr_docs2 type table of zmmt_ee_zgr_docs with header line.

  sort: it_vbak     by vbeln,
        it_tvakt    by auart,
        it_t161t    by bsart,
        it_kna1     by kunnr,
        it_lfa1     by lifnr,
        it_t001w    by werks,
        it_vbkd     by vbeln,
        it_makt     by matnr,
        it_ekko     by ebeln,
        it_ekpo     by ebeln,
        it_vbpa     by vbeln parvw,
        it_vbpa_cr  by vbeln,  "Ponto de coleta  REMESSA
        it_vbpa_co  by vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  by ebeln,  "Ponto de coleta  Pedido
        it_vbap     by vbeln,  "Itinerário  ORDEM
        it_ekpv     by ebeln.  "Itinerário  PEDIDO



  sort: it_zsdt0011_o by tp_movimento auart,
        it_zsdt0011_p by tp_movimento bsart.

  "Atualiza variaveis de frete
  it_zsdt0001_fre[] = it_zsdt0001[].
  perform f_pega_frete.
  it_zsdt0001[] = it_zsdt0001_fre[].
  refresh it_zsdt0001_fre.

  select *
    from zsdt0121
    into table t_fatura_agrupada
   where werks = p_branch.

  sort t_fatura_agrupada by werks matnr kunnr inco1 cfop.

  sort it_zsdt0001 by ch_referencia.
  loop at it_zsdt0001 into wa_zsdt0001.
    refresh: style.
    clear: wa_saida.

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

    perform f_set_encerramento_docs changing wa_saida.

*----CS2021000508 - 07.06.2021 - JT - inicio
    perform f_regras_doc_carguero.
*----CS2021000508 - 07.06.2021 - JT - fim

    if wa_zsdt0001-region is not initial.
      wa_saida-region          = wa_zsdt0001-region.
    else.
      select single cd_uf
        from zlest0002
        into v_cd_uf
        where pc_veiculo = wa_zsdt0001-placa_cav.
      if sy-subrc = 0.
        wa_saida-region          = v_cd_uf.
      endif.
    endif.

    wa_saida-route           = wa_zsdt0001-route.
    wa_saida-st_proc         = wa_zsdt0001-st_proc.
    wa_saida-shtyp           = wa_zsdt0001-shtyp.

    clear wa_saida-icon.
    refresh ti_zlest0100.
    select *
      from zlest0100
      into table ti_zlest0100
      where ch_referencia = wa_saida-ch_referencia.

    if ti_zlest0100[] is not initial.
      wa_saida-icon = icon_led_red.
    else.
      clear wa_saida-icon.
    endif.

*-#158056-11.11.2024-JT-inicio
    select single ch_faturamento
      into @data(_ch_faturamento)
      from zlest0241
     where ch_referencia = @wa_saida-ch_referencia
       and cancelado     = @abap_false.

    if sy-subrc = 0.
      select single status_msg
        into @data(_status_msg)
        from zlest0242
       where ch_faturamento = @_ch_faturamento
         and status_msg     = 'E'.

      if sy-subrc = 0.
        wa_saida-icon = icon_led_red.
      endif.
    endif.
*-#158056-11.11.2024-JT-fim

    clear wa_vbak.
    read table it_vbak into wa_vbak with key vbeln = wa_zsdt0001-vbeln binary search. " Ordem
    if sy-subrc = 0.
      wa_saida-tipo = 'O'.
      "Ponto de coleta ordem
      read table it_vbpa_co into wa_vbpa_co with key vbeln = wa_zsdt0001-vbeln binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_co-lifnr.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_vbpa_co-lifnr binary search.
        if sy-subrc eq 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        endif.
      endif.

      "Zona Local de entrega ordem
      read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                parvw = 'LR' binary search.
      if sy-subrc = 0.
        wa_saida-local_entrega = wa_vbpa-kunnr.
      endif.

      read table it_tvakt into wa_tvakt with key auart = wa_vbak-auart binary search.
      concatenate wa_vbak-auart '-' wa_tvakt-bezei into wa_saida-operacao.
      clear wa_kna1.
      read table it_kna1 into wa_kna1 with key kunnr = wa_vbak-kunnr binary search.
      wa_saida-kunnr           = wa_kna1-kunnr.
      wa_saida-name1           = wa_kna1-name1.

      read table it_vbkd into wa_vbkd with key vbeln = wa_zsdt0001-vbeln binary search.
      if sy-subrc = 0 .
        wa_saida-inco1           = wa_vbkd-inco1.
      else.
        clear wa_saida.
        continue.
      endif.

    else.
      wa_saida-tipo = 'P'.
      "Ponto de coleta pedido
      read table it_ekpa_pr into wa_ekpa_pr with key ebeln = wa_zsdt0001-vbeln binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_ekpa_pr-lifn2.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_ekpa_pr-lifn2 binary search.
        if sy-subrc eq 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        endif.
      endif.

      read table it_ekko into wa_ekko with key ebeln = wa_zsdt0001-vbeln binary search. " Pedidos de transferencia
      if sy-subrc ne 0.
        continue.
      endif.

      wa_saida-bsart = wa_ekko-bsart.

      read table it_t161t into wa_t161t with key bsart = wa_ekko-bsart binary search.
      concatenate wa_ekko-bsart '-' wa_t161t-batxt  into wa_saida-operacao.

      read table it_ekpo into wa_ekpo with key ebeln = wa_zsdt0001-vbeln binary search. " Pedidos de transferencia
      if sy-subrc = 0.
        wa_saida-inco1           = wa_ekpo-inco1.
      else.
        clear wa_saida.
        continue.
      endif.

      clear wa_lfa1.
      read table it_lfa1 into wa_lfa1 with key lifnr = wa_ekpo-lifnr binary search.
      wa_saida-kunnr           = wa_lfa1-lifnr.
      wa_saida-name1           = wa_lfa1-name1.

      "Zona Local de entrega Pedido
      read table it_vbpa into wa_vbpa with key vbeln = wa_zsdt0001-doc_rem
                                               parvw = 'LR'.  "Local de entrega

      if ( sy-subrc eq 0 ) and ( wa_zsdt0001-doc_rem is not initial ).
        wa_saida-local_entrega = wa_vbpa-kunnr.
      endif.

    endif.

    perform f_atual_frete using wa_zsdt0001 'L' changing wa_saida.

    " substituivalor do frete pelo valor historico
    if wa_zsdt0001-kbetr gt 0.
      wa_saida-kbetr = wa_zsdt0001-kbetr.
      wa_saida-konwa = wa_zsdt0001-konwa.
      v_cont_fre = 1.
    endif.

    wa_saida-vbeln           = wa_zsdt0001-vbeln.
    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.

    if wa_zsdt0001-agente_frete is not initial.
      wa_saida-lifnr = wa_zsdt0001-agente_frete.
      read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                 parvw = 'LR' binary search.
      if sy-subrc = 0.
        read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
        if sy-subrc = 0.
          wa_saida-kunnr           = wa_kna1-kunnr.
          wa_saida-name1           = wa_kna1-name1.
        endif.
      endif.
    elseif wa_saida-tipo = 'O'.
      if ( 'ZRFL_ZRDC_ZIND' cs wa_vbak-auart ) and
         ( wa_vbak-auart is not initial ) and
         ( wa_vbkd-inco1 = 'CIF'        ).
        read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'SP' binary search.
        if sy-subrc = 0.
          wa_saida-lifnr = wa_vbpa-lifnr.
          perform f_troca_agente using wa_zsdt0001-placa_cav wa_zsdt0001-branch changing wa_saida-lifnr.
        endif.
      else.
        read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'LR' binary search.
        if sy-subrc = 0.
          read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
          if sy-subrc = 0.
            wa_saida-kunnr = wa_kna1-kunnr.
            wa_saida-name1 = wa_kna1-name1.
          endif.
        endif.

        if ( t_fatura_agrupada is not initial ) and
           ( 'CFR_FOB' cs wa_saida-inco1      ) and
           ( wa_saida-inco1 is not initial    ).

          if wa_saida-lifnr is not initial.
            perform f_config_cell using wa_saida-lifnr  'LIFNR'  cl_gui_alv_grid=>mc_style_disabled.
          endif.

          if ( wa_saida-region is not initial ) or (  wa_zsdt0001-placa_cav is initial  ).
            perform f_config_cell using wa_saida-region 'REGION' cl_gui_alv_grid=>mc_style_disabled.
          endif.

        endif.
      endif.
    endif.


*US-169528 25/07/2025 WBARBOSA INICIO
    if wa_zsdt0001-id_interface eq '48'. "Sementes
      if wa_zsdt0001-nro_cg is not initial.
        call method zcl_carga_saida_insumos=>get_agente_frete
          exporting
            i_bukrs  = wa_zsdt0001-bukrs
            i_branch = wa_zsdt0001-branch
            i_nro_cg = wa_zsdt0001-nro_cg
          importing
            e_inco1  = data(lv_inco1).

        wa_saida-tp_frete = conv #( lv_inco1 ).
        wa_saida-inco1 = conv #( lv_inco1 ).
      endif.
    endif.
*US-169528 25/07/2025 WBARBOSA FIM

    "BLOQUEIA CELULA
    clear: wa_style.
    if ( wa_saida-region is not initial ) or (  wa_zsdt0001-placa_cav is initial  ).
      wa_style-fieldname = 'REGION'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      insert  wa_style into table style .
    endif.
    clear: wa_style.
    if wa_saida-lifnr is not initial.
      wa_style-fieldname = 'LIFNR'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      insert wa_style into table style .
    endif.

    "Busca peso descarga e CFOP (SE agrupamento)
    if vg_cockpit = '03' or vg_cockpit = '09' or vg_cockpit = '10'.
      if wa_zsdt0001-peso_liq_pos_ret eq 0.
        select single *
          from zsdt0001
          into wa_zsdt0001e
          where tp_movimento =  'E'
          and   bukrs        =  wa_zsdt0001-bukrs
          and   branch       =  wa_zsdt0001-branch
          and   nr_romaneio  =  wa_zsdt0001-id_referencia
          and   nr_safra     =  wa_zsdt0001-nr_safra.

        if sy-subrc = 0.
          select single *
            from zlest0087
            into wa_zlest0087
            where idinter	=	'L1'
            and   tp_movi	=	'E'
            and   tp_reg  = '30'
            and   bukrs	=	wa_zsdt0001e-bukrs
            and   werks	=	wa_zsdt0001e-branch
            and   nr_nf	=	wa_zsdt0001e-nfnum
            and   serie	=	wa_zsdt0001e-series
            and   lifnr	=	wa_zsdt0001e-parid
            and   status  = ''.

          if sy-subrc = 0.
            wa_saida-peso_descarga  =  wa_zlest0087-descarga_rodo.
            perform f_config_cell using wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
          else.
            select single *
              from zlest0041
              into wa_zlest0041
              where centro_comprador  = wa_zsdt0001e-branch
              and   nr_nf             = wa_zsdt0001e-nfnum
              and   cod_cliente       = wa_zsdt0001e-parid
              and   serie             = wa_zsdt0001e-series.

            if sy-subrc = 0.
              select single *
                from zlest0019
                into wa_zlest0019
                where idinter  = 'L1'
                and   tp_movi  = 'E'
                and   tp_reg   = '30'
                and   bukrs    = wa_zsdt0001e-bukrs
                and   branch   = wa_zlest0041-centro_comprador
                and   nfenum   = wa_zlest0041-nr_nf_propria.
              if sy-subrc = 0.
                wa_saida-peso_descarga  =  wa_zlest0019-pesodvagao.
                perform f_config_cell using wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
              endif.

            endif.
          endif.
          "CFOP
          if wa_zsdt0001e-series+0(1) = '0'.
            call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
              exporting
                input  = wa_zsdt0001e-series
              importing
                output = wa_zsdt0001e-series.
          endif.

          select single cfop
            into wa_saida-cfop
            from j_1bnfdoc
            inner join j_1bnflin
            on  j_1bnflin~docnum = j_1bnfdoc~docnum
*            AND J_1BNFLIN~ITMNUM = '00010'
            where j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
            and   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
            and   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
            and   j_1bnfdoc~nfenum       =  wa_zsdt0001e-nfnum
            and   j_1bnfdoc~series       =  wa_zsdt0001e-series.

          if sy-subrc ne 0.
            wa_zsdt0001e-nfnum2 = wa_zsdt0001e-nfnum.
            select single cfop
             into wa_saida-cfop
             from j_1bnfdoc
             inner join j_1bnflin
             on  j_1bnflin~docnum = j_1bnfdoc~docnum
*             AND J_1BNFLIN~ITMNUM = '00010'
             where j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
             and   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
             and   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
             and   j_1bnfdoc~nfnum        =  wa_zsdt0001e-nfnum2
             and   j_1bnfdoc~series       =  wa_zsdt0001e-series.

            if sy-subrc ne 0.

              select  *
                from  zmmt_ee_zgr
                into table tl_zmmt_ee_zgr
                where  ch_referencia =  wa_zsdt0001e-ch_referencia.

              if sy-subrc = 0.

                select *
                   from  zmmt_ee_zgr_docs
                   into table tl_zmmt_ee_zgr_docs
                   for all entries in tl_zmmt_ee_zgr
                   where  obj_key = tl_zmmt_ee_zgr-obj_key
                  and docnum ne ' '.

                if sy-subrc = 0.
                  read table tl_zmmt_ee_zgr_docs index 1.
                  select single cfop
                     into wa_saida-cfop
                     from j_1bnfdoc
                     inner join j_1bnflin
                    on  j_1bnflin~docnum = j_1bnfdoc~docnum
*                    AND J_1BNFLIN~ITMNUM = '00010'
                    where j_1bnfdoc~docnum = tl_zmmt_ee_zgr_docs-docnum.
                endif.
              endif.
            endif.

          endif.

          wa_saida-cfop = wa_saida-cfop+0(4).

          read table t_fatura_agrupada into w_fatura_agrupada with key werks = wa_zsdt0001-branch "RJF
                                                                       matnr = wa_zsdt0001-matnr
                                                                       kunnr = wa_saida-kunnr
                                                                       inco1 = vinco1
                                                                       cfop  = wa_saida-cfop.
          if sy-subrc = 0.
            wa_saida-perc_ret    = w_fatura_agrupada-perc_ret.

            wa_saida-peso_retido = wa_saida-peso_descarga * ( w_fatura_agrupada-perc_ret / 100 ).
            vpeso_retido_i = wa_saida-peso_retido.
            wa_saida-peso_retido = wa_saida-peso_retido - ( wa_saida-peso_retido - vpeso_retido_i ).


            wa_saida-peso_liq_pos_ret  = wa_saida-peso_descarga - wa_saida-peso_retido.
*            WA_SAIDA-STYLE[] = STYLE[].
          endif.
          "
        endif.
      else.
        wa_saida-peso_descarga    = wa_zsdt0001-peso_descarga.
        wa_saida-perc_ret         = wa_zsdt0001-perc_ret.
        wa_saida-peso_retido      = wa_zsdt0001-peso_retido.
        wa_saida-peso_liq_pos_ret = wa_zsdt0001-peso_liq_pos_ret.
        perform f_config_cell using wa_saida-peso_descarga  'PESO_DESCARGA'  cl_gui_alv_grid=>mc_style_disabled.
*        WA_SAIDA-STYLE[] = STYLE[].
        select single *
          from zsdt0001
          into wa_zsdt0001e
          where tp_movimento =  'E'
          and   bukrs        =  wa_zsdt0001-bukrs
          and   branch       =  wa_zsdt0001-branch
          and   nr_romaneio  =  wa_zsdt0001-id_referencia
          and   nr_safra     =  wa_zsdt0001-nr_safra.

        if sy-subrc = 0.
          "CFOP
          if wa_zsdt0001e-series+0(1) = '0'.
            call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
              exporting
                input  = wa_zsdt0001e-series
              importing
                output = wa_zsdt0001e-series.
          endif.

          select single cfop
            into wa_saida-cfop
            from j_1bnfdoc
            inner join j_1bnflin
            on  j_1bnflin~docnum = j_1bnfdoc~docnum
*            AND J_1BNFLIN~ITMNUM = '00010'
            where j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
            and   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
            and   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
            and   j_1bnfdoc~nfenum       =  wa_zsdt0001e-nfnum
            and   j_1bnfdoc~series       =  wa_zsdt0001e-series.

          if sy-subrc ne 0.
            wa_zsdt0001e-nfnum2 = wa_zsdt0001e-nfnum.
            select single cfop
             into wa_saida-cfop
             from j_1bnfdoc
             inner join j_1bnflin
             on  j_1bnflin~docnum = j_1bnfdoc~docnum
*             AND J_1BNFLIN~ITMNUM = '00010'
             where j_1bnfdoc~bukrs        =  wa_zsdt0001e-bukrs
             and   j_1bnfdoc~branch       =  wa_zsdt0001e-branch
             and   j_1bnfdoc~parid        =  wa_zsdt0001e-parid
             and   j_1bnfdoc~nfnum        =  wa_zsdt0001e-nfnum2
             and   j_1bnfdoc~series       =  wa_zsdt0001e-series.

            if sy-subrc ne 0.

              select  *
                from     zmmt_ee_zgr
                into table tl_zmmt_ee_zgr2
                where  ch_referencia =  wa_zsdt0001e-ch_referencia.

              if sy-subrc = 0.

                data: v_candat type j_1bnfdoc-candat.

                select  *
                   from  zmmt_ee_zgr_docs
                   into table tl_zmmt_ee_zgr_docs2
                   for all entries in tl_zmmt_ee_zgr2
                   where  obj_key = tl_zmmt_ee_zgr2-obj_key
                   and docnum ne ' '.

                if sy-subrc = 0.
                  read table tl_zmmt_ee_zgr_docs2 index 1.

                  select single cfop
                     into wa_saida-cfop
                     from j_1bnfdoc
                     inner join j_1bnflin
                    on  j_1bnflin~docnum = j_1bnfdoc~docnum
*                    AND J_1BNFLIN~ITMNUM = '00010'
                    where j_1bnfdoc~docnum = tl_zmmt_ee_zgr_docs2-docnum.
                endif.
              endif.
            endif.

          endif.

          wa_saida-cfop = wa_saida-cfop+0(4).

        endif.

      endif.
    endif.


    wa_saida-style[] = style[].

    if wa_saida-name1 is initial .
      clear wa_kna1.
      read table it_t001w into wa_t001w with key werks = wa_zsdt0001-branch  binary search.
      wa_saida-name1           = wa_t001w-name1.
    endif.

    "//Enio Jesus - 17.01.2017
    if ( wa_zsdt0001-doc_rem is initial or  wa_zsdt0001-doc_rem = '' ).

      if line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        try.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 remessa      = icon_execute_object
                               ].
          catch cx_sy_itab_line_not_found.
            wa_saida-remessa = icon_execute_object.
        endtry.

      else.
        wa_saida-remessa = icon_execute_object.
      endif.


      "Limite de crédito
      select single *
        from zsdt0151
        into wa_zsdt0151
        where ch_referencia = wa_zsdt0001-ch_referencia.
      if sy-subrc = 0.
        if wa_zsdt0151-vbeln ne wa_zsdt0001-vbeln.
          delete from zsdt0151 where ch_referencia = wa_zsdt0001-ch_referencia.
        else.
          if wa_zsdt0151-status is initial.
            concatenate icon_workflow_inbox(3) '\QAguardando Aprovação de limite de crédito@' into wa_saida-remessa .
          elseif  wa_zsdt0151-status = 'R'.
            concatenate icon_reject(3) '\QLimite de crédito rejeitado@' into wa_saida-remessa .
          endif.
        endif.
      endif.
    else.
      wa_saida-remessa = wa_zsdt0001-doc_rem.
      "Ponto de coleta (substitui se ja tiver remessa)
      read table it_vbpa_cr into wa_vbpa_cr with key vbeln = wa_zsdt0001-doc_rem binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_cr-lifnr.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_vbpa_cr-lifnr binary search.
        if sy-subrc eq 0.
          wa_saida-name1_c = wa_lfa1-name1.
          wa_saida-regio_c = wa_lfa1-regio.
        endif.
      endif.
    endif.

    "CS2019000400 25.02.2019
    if wa_saida-lifnr is initial. "Sugere Agente Frete
      "PERFORM F_TROCA_AGENTE USING WA_ZSDT0001-PLACA_CAV WA_ZSDT0001-PLACA_CAV-BRANCH CHANGING  WA_SAIDA-LIFNR.
      try .
          zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
          exporting
            i_bukrs                =    conv #( wa_zsdt0001-bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
            i_placa                =    conv #( wa_zsdt0001-placa_cav )
            i_uf_origem_mercadoria =    conv #( wa_saida-regio_c  )
           importing
             e_agente_frete         =   wa_saida-lifnr ).

        catch zcx_faturamento into data(_cx_fat).
        catch zcx_error into data(_cx_error).
      endtry.
    endif.

    clear vl_docnum.
    if wa_zsdt0001-fatura_prod is initial or wa_zsdt0001-fatura_prod = ''.

      if line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        try.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 fatura       = icon_execute_object
                               ].
          catch cx_sy_itab_line_not_found.
            wa_saida-fatura = icon_execute_object.
        endtry.

      else.
        wa_saida-fatura = icon_execute_object.
      endif.

    else.
      wa_saida-fatura  = wa_zsdt0001-fatura_prod.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_saida-fatura
        importing
          output = wa_saida-fatura.

      "Verfifica se DANFE já está autorizada e muda o campo
      if wa_zsdt0001-nro_nf_prod is initial or wa_zsdt0001-nro_nf_prod = ''.

        perform f_check_aut_doc using '1'
                                      wa_saida
                                      wa_zsdt0001
                             changing vl_docnum.

        if vl_docnum is not initial.
          wa_zsdt0001-nro_nf_prod = vl_docnum.
          wa_saida-danfe          = vl_docnum.
          update zsdt0001 set st_proc = '03'
                              nro_nf_prod = vl_docnum
          where ch_referencia = wa_saida-ch_referencia.
          wa_saida-st_proc = '03'.
          if ( wa_saida-inco1 = 'FOB' or wa_saida-inco1 = 'CFR' ) and ( not wa_saida-enc_conhecimento = abap_true ) . " Finaliza processo com a DANFE autorizada
*----CS2021000508 - 07.06.2021 - JT - inicio
            if wa_saida-troca_nota            = abap_true and
               wa_saida-docs_enviado_carguero = abap_false.
              update zsdt0001 set nro_nf_prod = wa_saida-danfe
                                  st_proc     = '98' " Finalizado
              where ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '98'.
            else.
              update zsdt0001 set nro_nf_prod = wa_saida-danfe
                                  st_proc     = '99' " Finalizado
            where ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '99'.
            endif.

*           UPDATE zsdt0001 SET nro_nf_prod = wa_saida-danfe
*                               st_proc     = '99' " Finalizado
*           WHERE ch_referencia = wa_saida-ch_referencia.

            wa_saida-transp  = icon_icon_list.
            wa_saida-dacte   = icon_icon_list.
*           wa_saida-st_proc = '99'.
            clear wa_saida-icon.
*----CS2021000508 - 07.06.2021 - JT - fim

          endif.
          "LES-260924-CS2024000086-ZLES0136-VT/VI Frota Prop#133287 - WPP - Ini
*          IF ( wa_saida-inco1 = 'CPT' ).  " Gerar CPT logo apos aprovação
*            CLEAR  wl_erro.
*            PERFORM f_gerar_vt USING ''
*                                     'L'
*                            CHANGING wl_erro
*                                     wa_saida
*                                     t_return[].
*            PERFORM f_check_retorno_vt USING ''
*                                             wl_erro
*                                    CHANGING wa_saida.
*          ENDIF.
          "LES-260924-CS2024000086-ZLES0136-VT/VI Frota Prop#133287 - WPP - Fim
        endif.
      endif.
    endif.

    if wa_zsdt0001-nro_nf_prod is initial or wa_zsdt0001-nro_nf_prod = ''.

      if line_exists( t_fatura_agrupada[ werks = wa_saida-branch kunnr = wa_saida-kunnr inco1 = vinco1 cfop = wa_saida-cfop ] ).
        try.
            it_saida = it_saida[ dt_movimento = wa_saida-dt_movimento
                                 matnr        = wa_zsdt0001-matnr
                                 kunnr        = wa_saida-kunnr
                                 operacao(4)  = wa_saida-operacao(4)
*                               INCO1        = VINCO1
                                 cfop         = wa_saida-cfop
                                 danfe        = icon_execute_object
                               ].
          catch cx_sy_itab_line_not_found.
            wa_saida-danfe  = icon_execute_object.
        endtry.
      else.
        wa_saida-danfe = icon_execute_object.
      endif.

    else.
      wa_saida-danfe           = wa_zsdt0001-nro_nf_prod.
    endif.

    if wa_zsdt0001-doc_transp is initial or wa_zsdt0001-doc_transp = ''.

      clear: v_chv_fat_vt.
      zcl_romaneio=>get_ck_faturar(
        exporting
          i_ch_referencia_sai   = wa_zsdt0001-ch_referencia
          i_somente_chv_faturar = abap_true
        importing
          e_chv_faturar         = v_chv_fat_vt ).

      if not 'CFR_FOB' cs wa_saida-inco1.
        wa_saida-transp = icon_execute_object.

        if ( v_chv_fat_vt is not initial ) and ( v_chv_fat_vt ne wa_zsdt0001-ch_referencia ) and ( wa_saida-inco1 = 'CIF' ).
          wa_saida-transp  = icon_icon_list.
        endif.

      else.
        wa_saida-transp  = icon_icon_list.
      endif.
    else.
      wa_saida-transp          = wa_zsdt0001-doc_transp.
    endif.

    if wa_zsdt0001-fknum is initial or wa_zsdt0001-fknum = ''.
      wa_saida-doccus = icon_icon_list.
    else.
      wa_saida-doccus = wa_zsdt0001-fknum.
    endif.

    if wa_zsdt0001-ov_frete is initial or wa_zsdt0001-ov_frete = ''.
      wa_saida-ovserv          = icon_icon_list.
    else.
      wa_saida-ovserv          = wa_zsdt0001-ov_frete.
    endif.

    clear vl_docnum.
    if wa_zsdt0001-fatura_frete is initial or wa_zsdt0001-fatura_frete = ''.
      wa_saida-fatserv         = icon_icon_list.
    else.
      wa_saida-fatserv         = wa_zsdt0001-fatura_frete.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_saida-fatserv
        importing
          output = wa_saida-fatserv.

      if wa_zsdt0001-nro_nf_frete is initial or wa_zsdt0001-nro_nf_frete = '' or ( wa_saida-st_proc ne '99' and wa_zsdt0001-nro_nf_frete is not initial ).
        select single j_1bnfdoc~bukrs j_1bnflin~docnum
            from j_1bnflin
            inner join j_1bnfdoc on j_1bnfdoc~docnum = j_1bnflin~docnum
            into (vl_bukrs,vl_docnum)
            where j_1bnflin~refkey = wa_saida-fatserv.

        if sy-subrc = 0.

          perform f_check_auth_doc using vl_docnum.

          if sy-subrc = 0.
*----CS2021000508 - 07.06.2021 - JT - inicio
            if wa_saida-troca_nota            = abap_true and
               wa_saida-docs_enviado_carguero = abap_false.
              update zsdt0001 set st_proc      = '98'
                                  nro_nf_frete = vl_docnum
              where ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '98'.
            else.
              update zsdt0001 set st_proc      = '99'
                                  nro_nf_frete = vl_docnum
              where ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '99'.
            endif.

            wa_zsdt0001-nro_nf_frete = vl_docnum.
*           UPDATE zsdt0001 SET st_proc      = '99'
*                               nro_nf_frete = vl_docnum
*           WHERE ch_referencia = wa_saida-ch_referencia.
*           wa_saida-st_proc = '99'.
*----CS2021000508 - 07.06.2021 - JT - inicio
          endif.
        endif.
      endif.
    endif.

    if wa_zsdt0001-nro_nf_frete is initial or wa_zsdt0001-nro_nf_frete = ''.
      wa_saida-dacte = icon_execute_object.
    else.
      wa_saida-dacte = wa_zsdt0001-nro_nf_frete.
    endif.

    if wa_zsdt0001-st_proc = '99' .
      if wa_saida-transp = icon_execute_object.
        wa_saida-transp = icon_icon_list.
      endif.
      if wa_saida-dacte = icon_execute_object.
        wa_saida-dacte  = icon_icon_list.
      endif.
    endif.

    read table it_makt into wa_makt with key matnr = wa_zsdt0001-matnr binary search.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_zsdt0001-matnr
      importing
        output = wa_saida-matnr.


    concatenate wa_saida-matnr '-' wa_makt-maktx into  wa_saida-material.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zsdt0001-matnr
      importing
        output = vmatnr18.

    wa_saida-matnr = vmatnr18 .

    "Checar se há cancelamento de CTE e estorna documentos
    " No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    if ( wa_saida-dacte   is not initial ) and ( wa_saida-dacte(1) ne '@' ) and
       ( wa_saida-inco1   eq 'CIF' ) and
       ( wa_saida-st_proc eq '99' ).

      perform f_check_canc_doc using wa_saida-dacte.

      if sy-subrc eq 0.
        refresh ti_zlest0100.
        wa_saida-dacte = icon_execute_object.
        update zsdt0001 set st_proc      = '07'
                            nro_nf_frete = ''
        where ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '07'.
        perform f_estorno_cte changing wa_saida.
        if ti_zlest0100[] is not initial.
          wa_saida-icon = icon_led_red.
        else.
          clear wa_saida-icon.
        endif.
      endif.
    endif.

    "Checar se há cancelamento de NFE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    if ( wa_saida-danfe is not initial ) and ( wa_saida-danfe(1) ne '@' ) and
       ( wa_saida-st_proc = '03' or ( wa_saida-st_proc = '99' and  'FOB_CPT_CFR' cs wa_saida-inco1  ) ).

      perform f_check_canc_doc using wa_saida-danfe.

      if sy-subrc eq 0.

        refresh ti_zlest0100.
        if ( wa_saida-inco1 = 'CPT' ) or ( wa_saida-enc_doc_custo eq abap_true ).
          wa_saida-st_proc = '05'.
          update zsdt0001 set st_proc      = '05'
          where ch_referencia = wa_saida-ch_referencia.
          perform f_estorno_custo changing wa_saida.
        endif.

        wa_saida-danfe = icon_execute_object.
        update zsdt0001 set   st_proc      = '02'
                              nro_nf_prod  = ''
          where ch_referencia = wa_saida-ch_referencia.

        perform f_estorno_nfe changing wa_saida.
        if ti_zlest0100[] is not initial.
          wa_saida-icon = icon_led_red.
        else.
          clear wa_saida-icon.
        endif.
      endif.
    endif.
    "

    if wa_saida-operacao(4) = 'ZPAR'.
      wa_saida-danfe    = icon_icon_list.
      wa_saida-transp   = icon_icon_list.
      wa_saida-doccus   = icon_icon_list.
      wa_saida-ovserv   = icon_icon_list.
      wa_saida-fatserv  = icon_icon_list.
      wa_saida-dacte    = icon_icon_list.
    endif.

    perform f_repare_docs_romaneio changing wa_saida.


*-CS2021000218-16.11.2022-#90706-JT-inicio
    perform f_validar_solicitacao_ra  using wa_zsdt0001-nro_cg
                                            wa_zsdt0001-ch_referencia
                                   changing wa_saida
                                            l_erro.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
    if vg_cockpit = '01'.   "Pesagem OPUS saida
      perform f_validar_transf_algodao  using wa_zsdt0001-ch_referencia
                                     changing wa_saida
                                              l_erro.
    endif.
*-CS2023000189-26.05.2023-#108752-JT-fim

*-CS2021000253-26.04.2024-#59941-JT-inicio
*    SELECT SINGLE agente_frete
*      INTO @DATA(l_agente_frete)
*      FROM zlest0185
*     WHERE id_ordem = @wa_zsdt0001-id_ordem.
*
*    IF sy-subrc = 0 AND l_agente_frete IS NOT INITIAL.
*      wa_saida-lifnr = l_agente_frete.
*
*      READ TABLE wa_saida-style INTO DATA(_style) WITH KEY fieldname = 'LIFNR'
*                                                           style     = cl_gui_alv_grid=>mc_style_disabled.
*      IF sy-subrc <> 0.
*        wa_style-fieldname = 'LIFNR'.
*        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT wa_style INTO TABLE wa_saida-style[].
*      ENDIF.
*    ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim

*-CS2024000522-25.09.2024-#152580-JT-inicio
    if vg_cockpit = '05' or "Insumos - Sementes (Vendas e Trânsferências expedidas )
       vg_cockpit = '06' or "Insumos - Defensivos (Vendas e Trânsferências expedidas )
       vg_cockpit = '07'.   "Insumos - Fertilizantes (Vendas e Trânsferências expedidas )
      if wa_zsdt0001-matnr is initial.
        select single matnr, posnr
          into @data(_zsdt0001_item)
          from zsdt0001_item
         where ch_referencia = @wa_zsdt0001-ch_referencia
           and vbeln         = @wa_zsdt0001-vbeln.

        if sy-subrc = 0.
          select single j_1bcfop
            into @data(_j_1bcfop)
            from vbap
           where vbeln = @wa_zsdt0001-vbeln
             and posnr = @_zsdt0001_item-posnr
             and matnr = @_zsdt0001_item-matnr.

          if sy-subrc = 0.
            wa_saida-cfop_ov = _j_1bcfop.
          endif.
        endif.
      else.
        select single j_1bcfop
          into _j_1bcfop
          from vbap
         where vbeln = wa_zsdt0001-vbeln
           and matnr = wa_zsdt0001-matnr.

        if sy-subrc = 0.
          wa_saida-cfop_ov = _j_1bcfop.
        endif.
      endif.
    endif.
*-CS2024000522-25.09.2024-#152580-JT-fim

    append wa_saida to it_saida.
    clear wa_saida.
  endloop.

  if s_lifnr-low is not initial.
    delete it_saida where lifnr ne s_lifnr-low.
  endif.

  sort it_zsdt0001 by ch_referencia.

  sort it_saida by nr_romaneio.

  if vg_cockpit = '03'. "Agrupamento - Ordernar por CFOP
    sort it_saida by cfop remessa descending.
  endif.

*----CS2021000508 - 07.06.2021 - JT - inicio
  if cl_grid is not initial.
    perform f_refresh_alv using '0100'. "Refresh na tela
  endif.
*----CS2021000508 - 07.06.2021 - JT - fim

endform.                    " F_SAIDA_01

*----CS2021000508 - 07.06.2021 - JT - inicio
************************************************************
* HABILITAR ENVIO DE ARQUIVOS P/ CARGUERO
************************************************************
form f_regras_doc_carguero.

  data: l_naotem_doc   type char01.

  free: l_naotem_doc.

  wa_zsdt0001-troca_nota = zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
    exporting
      i_ch_referencia = wa_zsdt0001-ch_referencia ).

  if wa_zsdt0001-troca_nota = abap_false.
    wa_saida-docs_carguero  = icon_light_out.
  else.

*--------------------------------------------
*-- valida arquivos obrigatorios
*--------------------------------------------
    try.
        l_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                         )->get_documentos_obrigatorios( exporting i_ch_referencia = wa_zsdt0001-ch_referencia
                         ).

      catch zcx_faturamento.
      catch zcx_error.
    endtry.

    if wa_zsdt0001-docs_enviado_carguero = abap_false.
      if l_naotem_doc = abap_true.
        wa_saida-docs_carguero   = icon_red_light.
      else.
        wa_saida-docs_carguero   = icon_yellow_light.
      endif.
    else.
      wa_saida-docs_carguero     = icon_green_light.
    endif.
  endif.

* wa_saida-docs_carguero         = icon_icon_list.
  wa_saida-docs_enviado_carguero = wa_zsdt0001-docs_enviado_carguero.
  wa_saida-troca_nota            = wa_zsdt0001-troca_nota.

endform.
*----CS2021000508 - 07.06.2021 - JT - fim


form f_saida_04.
  data: tabix         type sy-tabix,
        v_cont_fre    type i,
        v_cont_ped    type i,
        v_matnr18     type matnr18,
        v_cd_uf       type zlest0002-cd_uf,
        wl_zfiwrt0009 type zfiwrt0009.

  sort: it_vbak     by vbeln,
        it_tvakt    by auart,
        it_t161t    by bsart,
        it_kna1     by kunnr,
        it_lfa1     by lifnr,
        it_t001w    by werks,
        it_vbkd     by vbeln,
        it_makt     by matnr,
        it_ekko     by ebeln,
        it_ekpo     by ebeln,
        it_vbpa     by vbeln parvw,
        it_vbpa_cr  by vbeln,  "Ponto de coleta  REMESSA
        it_vbpa_co  by vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  by ebeln,  "Ponto de coleta  Pedido
        it_vbap     by vbeln,  "Itinerário  ORDEM
        it_ekpv     by ebeln.  "Itinerário  PEDIDO


  sort: it_zsdt0011_o by tp_movimento auart,
        it_zsdt0011_p by tp_movimento bsart,
        it_zsdt0062   by vbeln ebeln ebelp.

  "Atualiza variaveis de frete
  it_zsdt0001_fre[] = it_zsdt0001[].
  perform f_pega_frete.
  it_zsdt0001[] = it_zsdt0001_fre[].
  refresh it_zsdt0001_fre.


  loop at it_zsdt0001 into wa_zsdt0001.
    clear: wa_saida.


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

    perform f_set_encerramento_docs changing wa_saida.

    if   wa_zsdt0001-region is not initial.
      wa_saida-region          = wa_zsdt0001-region.
    else.
      select single cd_uf
        from zlest0002
        into v_cd_uf
        where pc_veiculo = wa_zsdt0001-placa_cav.
      if sy-subrc = 0.
        wa_saida-region          = v_cd_uf.
      endif.
    endif.

    wa_saida-route           = wa_zsdt0001-route.
    wa_saida-st_proc         = wa_zsdt0001-st_proc.
    wa_saida-shtyp           = wa_zsdt0001-shtyp.

    clear wa_saida-icon.
    refresh ti_zlest0100.
    select *
      from zlest0100
      into table ti_zlest0100
      where ch_referencia = wa_saida-ch_referencia.

    if ti_zlest0100[] is not initial.
      wa_saida-icon = icon_led_red.
    else.
      clear wa_saida-icon.
    endif.

    clear wa_vbak.
    read table it_vbak into wa_vbak with key vbeln = wa_zsdt0001-vbeln binary search. " Ordem
    if sy-subrc = 0.
      wa_saida-tipo = 'O'.
      "Ponto de coleta ordem
      read table it_vbpa_co into wa_vbpa_co with key vbeln = wa_zsdt0001-vbeln binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_co-lifnr.
        wa_saida-ponto_coleta = wa_vbpa_co-lifnr.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_vbpa_co-lifnr binary search.
        wa_saida-name1_c = wa_lfa1-name1.
      endif.

      "Zona Local de entrega ordem
      read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                parvw = 'LR' binary search.
      if sy-subrc = 0.
        wa_saida-local_entrega = wa_vbpa-kunnr.
      endif.

      read table it_tvakt into wa_tvakt with key auart = wa_vbak-auart binary search.
      concatenate wa_vbak-auart '-' wa_tvakt-bezei into wa_saida-operacao.
      clear wa_kna1.
      read table it_kna1 into wa_kna1 with key kunnr = wa_vbak-kunnr binary search.
      wa_saida-name1           = wa_kna1-name1.

      read table it_vbkd into wa_vbkd with key vbeln = wa_zsdt0001-vbeln binary search.
      if sy-subrc = 0 .
        wa_saida-inco1           = wa_vbkd-inco1.
      else.
        clear wa_saida.
        continue.
      endif.

      wa_saida-vbeln           = wa_zsdt0001-vbeln. "ORDEM VENDA

      if wa_zsdt0001-ebeln is initial.
        clear v_cont_ped.

        loop at it_zsdt0062 into wa_zsdt0062 where vbeln = wa_zsdt0001-vbeln
                                               and matnr = wa_zsdt0001-matnr. "CS2017002682 - 29.11.2017
          wa_saida-ebeln           = wa_zsdt0062-ebeln. "PEDIDO Importação
          wa_saida-ebelp           = wa_zsdt0062-ebelp. "CS2017002682 - 29.11.2017 - Ini
          add 1 to v_cont_ped.
        endloop.
        if v_cont_ped = 0.
          clear: wa_saida-ebeln, wa_saida-ebelp. " CS2017002682 - 29.11.2017
          "CLEAR WA_SAIDA.
          "CONTINUE.
        elseif v_cont_ped gt 1. " se tiver mais de 1, tem que selecionar
          clear: wa_saida-ebeln, wa_saida-ebelp. " CS2017002682 - 29.11.2017
        endif.
      else.
        wa_saida-ebeln           = wa_zsdt0001-ebeln.
        wa_saida-ebelp           = wa_zsdt0001-ebelp. "CS2017002682 - 29.11.2017
      endif.

    else.
      read table it_ekko into wa_ekko with key ebeln = wa_zsdt0001-vbeln binary search. " Pedidos de transferencia
      if wa_ekko-bsart = 'ZUB'.
        wa_saida-tipo = 'T'.
        wa_saida-vbeln           = wa_zsdt0001-vbeln. "Pedido de transferencia
        if wa_zsdt0001-ebeln is initial.
          clear: wa_saida-ebeln, wa_saida-ebelp.
        else.
          wa_saida-ebeln = wa_zsdt0001-ebeln.
          wa_saida-ebelp = wa_zsdt0001-ebelp.
        endif.
      else.
        wa_saida-tipo = 'P'.
        clear wa_saida-vbeln.
        wa_saida-ebeln           = wa_zsdt0001-vbeln.
      endif.

      "Ponto de coleta pedido
      read table it_ekpa_pr into wa_ekpa_pr with key ebeln = wa_zsdt0001-vbeln binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_ekpa_pr-lifn2.
        wa_saida-ponto_coleta = wa_ekpa_pr-lifn2.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_ekpa_pr-lifn2 binary search.
        wa_saida-name1_c = wa_lfa1-name1.
      endif.

      read table it_t161t into wa_t161t with key bsart = wa_ekko-bsart binary search.
      concatenate wa_ekko-bsart '-' wa_t161t-batxt  into wa_saida-operacao.

      read table it_ekpo into wa_ekpo with key ebeln = wa_zsdt0001-vbeln binary search. " Pedidos de transferencia
      if sy-subrc = 0.
        wa_saida-inco1           = wa_ekpo-inco1.

        select single * into @data(wa_eket)
          from eket
         where ebeln eq @wa_ekpo-ebeln
           and ebelp eq @wa_ekpo-ebelp.

        if sy-subrc is initial and wa_eket-charg is not initial.
          wa_saida-nr_safra        = wa_eket-charg.
        endif.

        "local de entrega pedido
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wa_ekpo-werks
          importing
            output = wa_saida-local_entrega.
      else.
        clear wa_saida.
        continue.
      endif.

      clear wa_lfa1.
      read table it_lfa1 into wa_lfa1 with key lifnr = wa_ekpo-lifnr binary search.
      wa_saida-name1           = wa_lfa1-name1.
    endif.

    perform f_atual_frete using wa_zsdt0001 'L' changing wa_saida.

    " substituivalor do frete pelo valor historico
    if wa_zsdt0001-kbetr gt 0.
      wa_saida-kbetr = wa_zsdt0001-kbetr.
      wa_saida-konwa = wa_zsdt0001-konwa.
      v_cont_fre = 1.
    endif.

    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.
    wa_saida-peso_fiscal     = wa_zsdt0001-peso_fiscal.

    if wa_zsdt0001-agente_frete is not initial.
      wa_saida-lifnr = wa_zsdt0001-agente_frete.
      read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                 parvw = 'LR' binary search.
      if sy-subrc = 0.
        read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
        if sy-subrc = 0.
          wa_saida-name1           = wa_kna1-name1.
        endif.
      endif.
    else.
      if ( 'ZRFL_ZRDC_ZIND' cs wa_vbak-auart ) and
         ( wa_vbkd-inco1 = 'CIF' ) and
         ( wa_vbak-auart is not initial ).
        read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'SP' binary search.
        wa_saida-lifnr           = wa_vbpa-lifnr.
        perform f_troca_agente using wa_zsdt0001-placa_cav wa_zsdt0001-branch changing wa_saida-lifnr.
      else.
        read table it_vbpa into wa_vbpa  with key vbeln = wa_zsdt0001-vbeln
                                                  parvw = 'LR' binary search.
        if sy-subrc = 0.
          read table it_kna1 into wa_kna1 with key kunnr = wa_vbpa-kunnr binary search.
          if sy-subrc = 0.
            wa_saida-name1           = wa_kna1-name1.
          endif.
        endif.
      endif.

    endif.

    if wa_zsdt0001-seq_lcto is initial or  wa_zsdt0001-seq_lcto = ''.
      wa_saida-seq_lcto        = icon_execute_object.
      wa_saida-danfez          = icon_execute_object.
    else.
      clear wl_zfiwrt0009.
      select single * from zfiwrt0009 into wl_zfiwrt0009 where seq_lcto = wa_zsdt0001-seq_lcto.
      wa_saida-netpr           = wl_zfiwrt0009-netpr.
      wa_saida-seq_lcto        = wa_zsdt0001-seq_lcto.
      wa_saida-danfez          = icon_execute_object.
      if wa_zsdt0001-nro_nf_rem is initial or wa_zsdt0001-nro_nf_rem = ''.
        select single *
          from zfiwrt0008
          into wa_zfiwrt0008
          where seq_lcto = wa_zsdt0001-seq_lcto.
        if sy-subrc = 0.
          if wa_zfiwrt0008-docnum is not initial.

            perform f_check_auth_doc using wa_zfiwrt0008-docnum.

            if sy-subrc = 0.
              wa_zsdt0001-nro_nf_rem = wa_zfiwrt0008-docnum.
              wa_saida-danfez        = wa_zfiwrt0008-docnum.
              update zsdt0001 set st_proc    = '12'
                                  nro_nf_rem = wa_zfiwrt0008-docnum
              where ch_referencia = wa_saida-ch_referencia.
              wa_saida-st_proc = '12'.
            endif.
          endif.
        endif.
      else.
        wa_saida-danfez           = wa_zsdt0001-nro_nf_rem.
      endif.
    endif.


    "BLOQUEIA CELULA
    refresh: style.

    if wa_saida-netpr is not initial.
      perform f_config_cell using wa_saida-netpr  'NETPR'  cl_gui_alv_grid=>mc_style_disabled.
    endif.

    if ( wa_saida-region is not initial ) or (  wa_zsdt0001-placa_cav is initial  ).
      perform f_config_cell using wa_saida-region 'REGION' cl_gui_alv_grid=>mc_style_disabled.
    endif.

    if wa_saida-lifnr is not initial.
      perform f_config_cell using wa_saida-lifnr  'LIFNR'  cl_gui_alv_grid=>mc_style_disabled.
    endif.

    if wa_saida-ebeln is not initial.
      perform f_config_cell using wa_saida-ebeln  'EBELN'  cl_gui_alv_grid=>mc_style_disabled.
      perform f_config_cell using wa_saida-ebelp  'EBELP'  cl_gui_alv_grid=>mc_style_disabled.
    endif.

    wa_saida-style[] = style[].

    if wa_saida-name1 is initial .
      clear wa_kna1.
      read table it_t001w into wa_t001w with key werks = wa_zsdt0001-branch  binary search.
      wa_saida-name1           = wa_t001w-name1.
    endif.

    if wa_zsdt0001-doc_aviso is initial or  wa_zsdt0001-doc_aviso = ''.
      wa_saida-aviso        = icon_execute_object.
    else.
      wa_saida-aviso       = wa_zsdt0001-doc_aviso.
    endif.

    if wa_zsdt0001-doc_rem is initial or  wa_zsdt0001-doc_rem = ''.
      wa_saida-remessa         = icon_execute_object.
    else.
      wa_saida-remessa         = wa_zsdt0001-doc_rem.
      "Ponto de coleta (substitui se ja tiver remessa)
      read table it_vbpa_cr into wa_vbpa_cr with key vbeln = wa_zsdt0001-doc_rem binary search. " Ordem
      if sy-subrc = 0.
        wa_saida-lifnr_c = wa_vbpa_cr-lifnr.
        read table it_lfa1 into wa_lfa1 with key lifnr = wa_vbpa_cr-lifnr binary search.
        wa_saida-name1_c = wa_lfa1-name1.
      endif.
    endif.

    clear vl_docnum.
    if wa_zsdt0001-fatura_prod is initial or wa_zsdt0001-fatura_prod = ''.
      wa_saida-fatura          = icon_execute_object.
    else.
      wa_saida-fatura          = wa_zsdt0001-fatura_prod.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_saida-fatura
        importing
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
    endif.


    if wa_zsdt0001-nro_nf_prod is initial or wa_zsdt0001-nro_nf_prod = ''.
      wa_saida-danfe           = icon_execute_object.
    else.
      wa_saida-danfe           = wa_zsdt0001-nro_nf_prod.
    endif.

    if wa_zsdt0001-doc_transp is initial or wa_zsdt0001-doc_transp = ''.
      wa_saida-transp          = icon_execute_object.
    else.
      wa_saida-transp          = wa_zsdt0001-doc_transp.

      "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
      read table git_vttk into data(lwa_vttk) with key tknum = wa_zsdt0001-doc_transp.
      if sy-subrc eq 0 and wa_zsdt0001-doc_transp is not initial.
        wa_saida-shtyp = lwa_vttk-shtyp.
      endif.
      "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

    endif.

    if wa_zsdt0001-fknum is initial or wa_zsdt0001-fknum = ''.
      wa_saida-doccus          = icon_icon_list.
    else.
      wa_saida-doccus          = wa_zsdt0001-fknum.
    endif.

    if wa_zsdt0001-ov_frete is initial or wa_zsdt0001-ov_frete = ''.
      wa_saida-ovserv          = icon_icon_list.
    else.
      wa_saida-ovserv          = wa_zsdt0001-ov_frete.
    endif.

    clear vl_docnum.
    if wa_zsdt0001-fatura_frete is initial or wa_zsdt0001-fatura_frete = ''.
      wa_saida-fatserv         = icon_icon_list.
    else.
      wa_saida-fatserv         = wa_zsdt0001-fatura_frete.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_saida-fatserv
        importing
          output = wa_saida-fatserv.

      if wa_zsdt0001-nro_nf_frete is initial or wa_zsdt0001-nro_nf_frete = ''.
        select single j_1bnfdoc~bukrs j_1bnflin~docnum
            from j_1bnflin
            inner join j_1bnfdoc on j_1bnfdoc~docnum = j_1bnflin~docnum
            into (vl_bukrs,vl_docnum)
            where j_1bnflin~refkey = wa_saida-fatserv.

        if sy-subrc = 0.

          perform f_check_auth_doc using vl_docnum.

          if sy-subrc = 0.
            wa_saida-st_proc = '18'.
            wa_zsdt0001-nro_nf_frete = vl_docnum.

            update zsdt0001 set st_proc      = '18'
                                nro_nf_frete = vl_docnum
            where ch_referencia = wa_saida-ch_referencia.
          endif.
        endif.
      endif.
    endif.

    if wa_zsdt0001-nro_nf_frete is initial or wa_zsdt0001-nro_nf_frete = ''.
      wa_saida-dacte             = icon_execute_object.
    else.
      wa_saida-dacte             = wa_zsdt0001-nro_nf_frete.
    endif.

    if wa_saida-tipo = 'P'."Se for pedido de importação não prosseguir
      wa_saida-remessa = icon_icon_list.
      wa_saida-fatura  = icon_icon_list.
      wa_saida-danfe   = icon_icon_list.
    endif.

    "Quando o romaneio for sobre um pedido de transferencia ou ordem de venda, será gerada um documento de remessa.
    "Nesse caso, o frete é emitido sobre a remessa da NF de Venda/Transferencia
    if not ( ( wa_saida-aviso is not initial ) and ( wa_saida-aviso(1) ne '@' ) ).
      if ( wa_saida-tipo = 'O' ) or
         ( wa_saida-tipo = 'T' ).
        wa_saida-aviso  = icon_icon_list.
      endif.
    endif.

    read table it_makt into wa_makt with key matnr = wa_zsdt0001-matnr binary search.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_zsdt0001-matnr
      importing
        output = wa_saida-matnr.

    concatenate wa_saida-matnr '-' wa_makt-maktx into  wa_saida-material.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zsdt0001-matnr
      importing
        output = v_matnr18.

    wa_saida-matnr = v_matnr18.
    "Checar se há cancelamento de CTE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    if ( wa_saida-dacte   is not initial ) and ( wa_saida-dacte(1) ne '@' ) and
       ( wa_saida-inco1   eq 'CIF' ) and
       ( wa_saida-st_proc eq '18' or wa_saida-st_proc eq '99' ) .

      perform f_check_canc_doc using wa_saida-dacte.

      if sy-subrc eq 0.

        refresh ti_zlest0100.
        wa_saida-dacte = icon_execute_object.
        update zsdt0001 set st_proc      = '17'
                            nro_nf_frete = ''
        where ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '17'.
        perform f_estorno_cte changing wa_saida.
        if ti_zlest0100[] is not initial.
          wa_saida-icon = icon_led_red.
        else.
          clear wa_saida-icon.
        endif.
      endif.
    endif.

    "Checar se há cancelamento de NFE e estorna documentos
    "No automatico somente se estiver finalizado (SEFAZ), senão fazer pelo botão EST_CTE
    if ( wa_saida-danfe is not initial ) and ( wa_saida-danfe(1) ne '@' ) and
       ( wa_saida-st_proc = '21' or wa_saida-st_proc = '99' ).

      perform f_check_canc_doc using wa_saida-danfe.

      if sy-subrc eq 0.

        refresh ti_zlest0100.
        wa_saida-danfe = icon_execute_object.
        update zsdt0001 set   st_proc      = '20'
                              nro_nf_prod  = ''
          where ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '20'.
        perform f_estorno_nfe changing wa_saida.
        if ti_zlest0100[] is not initial.
          wa_saida-icon = icon_led_red.
        else.
          clear wa_saida-icon.
        endif.
      endif.
    endif.

    "Checar se há cancelamento de NF-Remessa e estorna documentos
    "DANFEZ
    clear wa_zfiwrt0008.
    if ( wa_saida-seq_lcto is not initial ) and ( wa_saida-seq_lcto(1) ne '@' ).
      select single *
        from zfiwrt0008 into wa_zfiwrt0008
       where seq_lcto = wa_saida-seq_lcto.
    endif.
    if ( ( wa_saida-danfez is not initial and wa_saida-danfez(1) ne '@' ) or
         ( wa_saida-seq_lcto is not initial and wa_saida-seq_lcto(1) ne '@' ) ) and
       ( wa_zfiwrt0008-docnum is not initial ).

      perform f_check_canc_doc using wa_zfiwrt0008-docnum.

      if sy-subrc eq 0.

        wa_zsdt0001-nro_nf_rem = ''.
        wa_saida-danfez        = icon_execute_object.
        update zsdt0001 set st_proc    = '11'
                            nro_nf_rem = ''
        where ch_referencia = wa_saida-ch_referencia.
        wa_saida-st_proc = '11'.
        " LIMPA SEQ_LCTO
        wa_zsdt0001-nro_nf_rem = ''.

        if wa_zfiwrt0008-docs_estornados eq abap_true.
          wa_saida-seq_lcto        = icon_execute_object.
          update zsdt0001 set st_proc      = ''
                              agente_frete = ''
                              seq_lcto     = ''
          where ch_referencia = wa_saida-ch_referencia.

          wa_saida-st_proc = ''.
          wa_saida-lifnr   = ''.
        endif.

      endif.
    endif.

    if wa_saida-tipo = 'O' and wa_saida-inco1 = 'FOB'.
      wa_saida-aviso    = icon_icon_list.

      if wa_saida-enc_conhecimento = abap_false.
        wa_saida-transp   = icon_icon_list.
        wa_saida-dacte    = icon_icon_list.
      endif.
    endif.

    if wa_saida-emite_conhecimento eq abap_false.
      wa_saida-ovserv   = icon_icon_list.
      wa_saida-fatserv  = icon_icon_list.
      wa_saida-dacte    = icon_icon_list.
    endif.

*    IF wa_zsdt0001-st_proc = '99' .
*      IF wa_saida-transp          = icon_execute_object.
*        wa_saida-transp          = icon_icon_list.
*      ENDIF.
*      IF wa_saida-dacte          = icon_execute_object.
*        wa_saida-dacte          = icon_icon_list.
*      ENDIF.
*    ENDIF.

    perform f_repare_docs_romaneio changing wa_saida.

    append wa_saida to it_saida.
    clear wa_saida.
  endloop.

  if s_lifnr-low is not initial.
    delete it_saida where lifnr ne s_lifnr-low.
  endif.

  sort it_zsdt0001 by ch_referencia.

  sort it_saida by nr_romaneio.

endform.                    " F_SAIDA_04

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

form f_seleciona_dados.

  "Configura os status de Processamento
  perform f_set_status_proc.

  perform f_refresh_data.

  perform f_get_value_set tables t_auart  using 'MAGGI_ARMAZENAGEM_VA01'.

  perform f_get_value_set tables t_usermd using 'MAGGI_ZLES106_RECUP'.

  perform f_config_ranges.

  if r_dt_a = 'X'.
    select *
      from zsdt0001 into table it_zsdt0001
     where bukrs        in r_bukrs
       and branch       in r_branch
       and vbeln        in s_vbeln
       and vbeln        in s_ebeln
       and doc_transp   in s_doc
       and vbeln        ne ''
       and dt_movimento in s_data
       and parid        in r_coleta
       and dt_movimento ge '20141006'
       and st_proc      ne '99'
       and nr_romaneio  in s_roman  "*-CS2024000090-28.05.2024-#133805-JT
       and tp_movimento in r_tp_movimento.
  elseif r_dt_f = 'X'.
    select *
      from zsdt0001 into table it_zsdt0001
     where bukrs        in r_bukrs
       and branch       in r_branch
       and vbeln        in s_vbeln
       and vbeln        in s_ebeln
       and doc_transp   in s_doc
       and vbeln        ne ''
       and dt_movimento in s_data
       and parid        in r_coleta
       and st_proc      eq '99'
       and nr_romaneio  in s_roman  "*-CS2024000090-28.05.2024-#133805-JT
       and tp_movimento in r_tp_movimento.
  else.
    select *
      from zsdt0001 into table it_zsdt0001
     where bukrs         in r_bukrs
       and branch        in r_branch
       and vbeln         in s_vbeln
       and vbeln         in s_ebeln
       and doc_transp    in s_doc
       and vbeln         ne space
       and dt_movimento  in s_data
       and tp_movimento  in r_tp_movimento
       and ch_referencia in s_chave
       and nr_romaneio   in s_roman  "*-CS2024000090-28.05.2024-#133805-JT
       and id_interface  eq p_inter.
  endif.

  sort it_zsdt0001 by vbeln.

  check ( it_zsdt0001[] is not initial ) and ( vg_cockpit is not initial ).

  "Seleção de Tabelas genéricas, independente do Processo do Cockipit.
  perform f_selecao_generica_rom.

*-------------------------------------------------------------------------------------*
*  Início tratamento de registros de acordo com o tipo do Cockpit
*-------------------------------------------------------------------------------------*

  "Exceção de Faturamento Fertilizantes
  select *
    into table it_zlest0132
    from zlest0132.

  sort: it_zlest0132 by branch parid.

  loop at it_zsdt0001 assigning field-symbol(<out_zsdt0001>).
    clear: wa_zlest0132, wa_mara, wa_vbap, wa_vbak.

    read table it_vbak into wa_vbak with key vbeln = <out_zsdt0001>-vbeln.

    read table it_vbap into wa_vbap with key vbeln = <out_zsdt0001>-vbeln.

    "Filtro Material
    if s_matnr is not initial.
      if <out_zsdt0001>-matnr not in s_matnr.
        clear: tg_zsdt0001_item.
        loop at tg_zsdt0001_item where ch_referencia eq <out_zsdt0001>-ch_referencia
                                   and matnr         in s_matnr.
          exit.
        endloop.
        if tg_zsdt0001_item is initial.
          <out_zsdt0001>-del = 'X'.
        endif.
      endif.
    endif.

    if <out_zsdt0001>-matnr is not initial.
      read table it_mara into wa_mara with key matnr = <out_zsdt0001>-matnr binary search.
    else.
      if ( <out_zsdt0001>-vbeln is not initial ) and ( wa_vbap-matnr is not initial ).
        read table it_mara into wa_mara with key matnr = wa_vbap-matnr binary search.
      endif.
    endif.

    select *
      from zsdt0121
      into table t_fatura_agrupada
     where werks = p_branch.

    sort t_fatura_agrupada by werks matnr kunnr inco1 cfop.

    "Tabela de Faturamento Fertilizantes
    read table it_zlest0132 into wa_zlest0132 with key branch = <out_zsdt0001>-branch
                                                       parid  = <out_zsdt0001>-parid binary search.

    case vg_cockpit.  "Tratamento de Registros de Romaneio
      when '01'. "Commodities (Formação Lote, Vendas e Trâsferências Expedidas)
        clear w_fatura_agrupada.
        read table t_fatura_agrupada into w_fatura_agrupada with key werks = <out_zsdt0001>-branch
                                                                     matnr = <out_zsdt0001>-matnr
                                                                     kunnr = <out_zsdt0001>-id_cli_dest binary search.
        if  ( <out_zsdt0001>-id_interface ne '48' ) and
            ( <out_zsdt0001>-id_interface ne '49' ) and
            ( <out_zsdt0001>-id_interface ne '50' ) and
            ( <out_zsdt0001>-id_interface ne '51' ) and
            ( <out_zsdt0001>-id_interface ne '52' ) and
            "( W_FATURA_AGRUPADA-WERKS IS INITIAL ) AND "excluindo romaneios de troca de nota  da  região sul  ( PR, SC e RS)
            ( wa_vbak-spart ne '02' or
              wa_zlest0132 is initial ).
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
      when '03'. "Troca de notas - Commodities com agrupamento
        clear w_fatura_agrupada.
        read table t_fatura_agrupada into w_fatura_agrupada with key werks = <out_zsdt0001>-branch
                                                                     matnr = <out_zsdt0001>-matnr
                                                                     kunnr = <out_zsdt0001>-id_cli_dest binary search.
        if w_fatura_agrupada-werks is not initial .
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
      when '04'. "Fertilizantes - Porto Velho
        if  ( <out_zsdt0001>-id_interface ne '49'            ) and
            ( <out_zsdt0001>-id_interface ne '51'            ) and
            ( wa_vbak-spart eq '02' or  wa_mara-spart = '02' ) and
            ( wa_zlest0132 is not initial                    ).
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
      when '05'. "Sementes
        if ( <out_zsdt0001>-id_interface eq '48' ).
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
      when '06'. "Defensivos
        if ( <out_zsdt0001>-id_interface eq '52' ).
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
      when '07'. "Fertilizantes
        if ( <out_zsdt0001>-id_interface eq '51' ).
          continue.
        else.
          <out_zsdt0001>-del = 'X'.
        endif.
*      WHEN '08'. "Frete
*        IF ( <out_zsdt0001>-id_interface EQ '49' ).
*          CONTINUE.
*        ELSE.
*          <out_zsdt0001>-del = 'X'.
*        ENDIF.
    endcase.

  endloop.

  delete it_zsdt0001 where del = 'X'.

*-------------------------------------------------------------------------------------*
*  Fim tratamento de registros de acordo com o tipo do Cockpit
*-------------------------------------------------------------------------------------*

  "Define perform para busca especificia de dados auxiliares do Romaneio de acordo com o processo
  concatenate 'F_GET_TABLE_AUX_ROM_' vg_cockpit into data(form_get_rom_aux).
  "Seleção de Tabelas Específicas do Processo do Cockpit
  perform (form_get_rom_aux) in program zlesr0102 if found.


endform.                    " F_SELECIONA_DADOS

form f_montar_layout_veic.

  clear: it_fieldcat[].

  perform f_estrutura_alv using:
    01  ''              ''             'IT_VEIC' 'PLACA'         'Placa'           '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    02  ''              ''             'IT_VEIC' 'TIPO'          'Tipo'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    03  ''              ''             'IT_VEIC' 'COD_PROP'      'Proprietário'    ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    04  ''              ''             'IT_VEIC' 'NOM_PROP'      'Nome'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    05  ''              ''             'IT_VEIC' 'RNTC'          'RNTC'            ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    06  ''              ''             'IT_VEIC' 'RENAVAM'       'Renavam'         ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    07  ''              ''             'IT_VEIC' 'CNPJ'          'CPF/CNPJ'        ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    08  ''              ''             'IT_VEIC' 'CIDADE'        'Cidade'          ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ',
    09  ''              ''             'IT_VEIC' 'UF'            'UF'              ' '   ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' '.


endform.                    " MONTAR_LAYOUT_VEIC

form f_alv_header .

  data: wl_data(10),
        wl_hora(8),
        wl_linha(60),
        wl_text type sdydo_text_element.

  data: wa_t001       type t001,
        wa_j_1bbranch type j_1bbranch.

  if r_dt_a = 'X'.
    wl_linha = 'Romaneios em aberto'.
  elseif r_dt_f = 'X'.
    wl_linha = 'Romaneios finalizados'.
  else.
    wl_linha = 'Romaneios abertos e finalizados'.
  endif.

  wl_text = wl_linha.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  select single *
    from t001 into wa_t001
   where bukrs = p_bukrs.

  select single *
    from j_1bbranch into wa_j_1bbranch
   where bukrs  = p_bukrs
     and branch = p_branch.

  concatenate  'Empresa:' p_bukrs '-' wa_t001-butxt
          into wl_linha separated by space.

  wl_text = wl_linha.
  call method obj_dyndoc_id->new_line.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_fontsize = cl_dd_area=>list_normal.

  call method obj_dyndoc_id->new_line.

  concatenate  'Filial......:' p_branch '-' wa_j_1bbranch-name
         into wl_linha separated by space.

  wl_text = wl_linha.
  call method obj_dyndoc_id->new_line.

  call method obj_dyndoc_id->add_text
    exporting
      text         = wl_text
      sap_fontsize = cl_dd_area=>list_normal.


endform.                    " ZF_ALV_HEADER

form f_imprime_dados .
  perform f_monta_layout.

  call screen 0100.
endform.                    " F_IMPRIME_DADOS

form f_get_value_set tables p_values structure  rgsb4
                      using p_setnr.

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = p_setnr
    tables
      set_values    = p_values
    exceptions
      set_not_found = 1
      others        = 2.

  sort p_values by from.

endform.

form f_config_ranges.

  clear: r_tp_movimento[], r_bukrs[], r_branch[], r_coleta[].

  r_tp_movimento-sign   = 'I'.
  r_tp_movimento-option = 'EQ'.
  r_tp_movimento-low    = 'S'.
  append r_tp_movimento.

  if p_bukrs is not initial.
    r_bukrs-sign   = 'I'.
    r_bukrs-option = 'EQ'.
    r_bukrs-low    = p_bukrs.
    append r_bukrs.
  endif.

  if s_branch is not initial. "RJF
    r_branch[] = s_branch[].
  else.
    if p_branch is not initial.
      r_branch-sign   = 'I'.
      r_branch-option = 'EQ'.
      r_branch-low    = p_branch.
      append r_branch.
    endif.
  endif.

  clear: r_dlgrp.
  r_dlgrp-sign   = 'I'.
  r_dlgrp-option = 'EQ'.
  r_dlgrp-low    = '0001'.
  append r_dlgrp.
  r_dlgrp-low    = '0007'.
  append r_dlgrp.

  clear: r_vsart.
  r_vsart-sign   = 'I'.
  r_vsart-option = 'EQ'.
  r_vsart-low    = '01'. "Rodoviario
  append r_vsart.
  r_vsart-low    = '07'. "Multimodal
  append r_vsart.

  if vg_cockpit = '04'.
    clear r_coleta.
    r_coleta-sign   = 'I'.
    r_coleta-option = 'EQ'.
    r_coleta-low    = p_coleta.
    append r_coleta.
  endif.

endform.

form f_set_tp_cockpit.

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

  if r_cp_01 is not initial.
    vg_cockpit = '01'.
  elseif r_cp_02 is not initial.
    vg_cockpit = '02'.
  elseif r_cp_03 is not initial.
    vg_cockpit = '03'.
  elseif r_cp_04 is not initial.
    vg_cockpit = '04'.
  elseif r_cp_05 is not initial.
    vg_cockpit = '05'.
  elseif r_cp_06 is not initial.
    vg_cockpit = '06'.
  elseif r_cp_07 is not initial.
    vg_cockpit = '07'.
  elseif r_cp_09 is not initial.
    vg_cockpit = '09'.
  elseif r_cp_10 is not initial.
    vg_cockpit = '10'.
  endif.

endform.

form f_refresh_data.

  clear: it_lfa1[],
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

endform.

form f_selecao_generica_rom.

  ranges: lra_bsart_cockpit_01 for ekko-bsart.

  select *
    from tvarvc into table @data(lit_tvarvc_bsart_01)
   where name eq 'ZLES0136_BSART_COCKPIT_01'.

  if lit_tvarvc_bsart_01[] is initial.
    append value #( sign = 'I' option = 'EQ' low = 'ZUB' ) to lra_bsart_cockpit_01.
  else.
    loop at lit_tvarvc_bsart_01 into data(lwa_tvarvc_bsart_01).
      append value #( sign = 'I' option = 'EQ' low = lwa_tvarvc_bsart_01-low ) to lra_bsart_cockpit_01.
    endloop.
  endif.

  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
  loop at it_zsdt0001 assigning field-symbol(<fs_zsdt0001>).

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_zsdt0001>-doc_transp
      importing
        output = <fs_zsdt0001>-doc_transp.

  endloop.
  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Fim

  "Dados gerais de material
  select matnr spart
    from mara appending table it_mara
     for all entries in it_zsdt0001
   where matnr eq it_zsdt0001-matnr.

  "Busca Pedidos associados a ordem (se existir)
  select vbeln ebeln ebelp matnr status lgort charg qtd_vinc
    from zsdt0062 appending table it_zsdt0062
     for all entries in it_zsdt0001
   where vbeln  = it_zsdt0001-vbeln.
*---> 10.07.2023 17:05:46 - Migração S4 - DL
*   GROUP BY vbeln ebeln ebelp.
*<--- 10.07.2023 17:05:46 - Migração S4 - DL

  delete it_zsdt0062 where status = 'E'.

  data(it_zsdt0062_aux) = it_zsdt0062[].

  sort it_zsdt0062 by vbeln ebeln ebelp.
  delete adjacent duplicates from it_zsdt0062 comparing vbeln ebeln ebelp.

  loop at it_zsdt0062 assigning field-symbol(<fs_zsdt0062>).
    clear: <fs_zsdt0062>-qtd_vinc.

    loop at it_zsdt0062_aux into data(lwa_zsdt0062_aux) where vbeln = <fs_zsdt0062>-vbeln
                                                          and ebeln = <fs_zsdt0062>-ebeln
                                                          and ebelp = <fs_zsdt0062>-ebelp.
      add lwa_zsdt0062_aux-qtd_vinc to <fs_zsdt0062>-qtd_vinc.
    endloop.
  endloop.


  select *
    from likp appending corresponding fields of table it_likp
     for all entries in it_zsdt0001
   where vbeln  = it_zsdt0001-doc_aviso.

  select *
    from lips appending corresponding fields of table it_lips
     for all entries in it_zsdt0001
   where vbeln  = it_zsdt0001-doc_aviso.

  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
  select *
    from vttk appending corresponding fields of table git_vttk
     for all entries in it_zsdt0001
   where tknum  = it_zsdt0001-doc_transp.
  "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP - Fim

*-----------------------------------------------------------------------------*
*  Seleção de Pontos de Coleta
*-----------------------------------------------------------------------------*

  perform seleciona_vbpa_co.

  "Remessa
  select vbeln lifnr
    from vbpa appending table it_vbpa_cr
     for all entries in it_zsdt0001
   where vbeln   = it_zsdt0001-doc_rem
     and parvw   = 'PC'.

  "Aviso Recebimento
  select vbeln lifnr
    from vbpa appending table it_vbpa_cr
     for all entries in it_zsdt0001
   where vbeln   = it_zsdt0001-doc_aviso
     and parvw   = 'PC'.

  if it_vbpa_cr[] is not initial.
    select lifnr name1 dlgrp lzone regio
      from lfa1 appending table it_lfa1
       for all entries in it_vbpa_cr
     where lifnr  = it_vbpa_cr-lifnr.
  endif.

  "Pedido
  select ebeln lifn2
    from ekpa appending table it_ekpa_pr
     for all entries in it_zsdt0001
   where ebeln = it_zsdt0001-vbeln
     and parvw = 'PR'.

  if it_ekpa_pr[] is not initial.
    select lifnr name1 dlgrp lzone regio
      from lfa1 appending table it_lfa1
       for all entries in it_ekpa_pr
     where lifnr  = it_ekpa_pr-lifn2.
  endif.

*-----------------------------------------------------------------------------*
*  Seleção de Itinerário
*-----------------------------------------------------------------------------*

  "Ordem Venda
  select vbeln route matnr posnr
    from vbap appending corresponding fields of table it_vbap
     for all entries in it_zsdt0001
   where vbeln = it_zsdt0001-vbeln.

  "Dados gerais de material
  if it_vbap[] is not initial.
    select matnr spart
      from mara appending corresponding fields of table it_mara
       for all entries in it_vbap
     where matnr eq it_vbap-matnr.

    select matnr maktx
      from makt appending table it_makt
       for all entries in it_vbap
     where matnr eq it_vbap-matnr
       and spras eq sy-langu.
  endif.

  if it_lips[] is not initial.
    select matnr spart
      from mara appending corresponding fields of table it_mara
       for all entries in it_lips
     where matnr eq it_lips-matnr.

    select matnr maktx
      from makt appending table it_makt
       for all entries in it_lips
     where matnr eq it_lips-matnr
       and spras eq sy-langu.
  endif.

  "Pedido
  select ebeln route
    from ekpv appending table it_ekpv
     for all entries in it_zsdt0001
   where ebeln =  it_zsdt0001-vbeln.

  select matnr maktx
    from makt appending table it_makt
     for all entries in it_zsdt0001
   where matnr eq it_zsdt0001-matnr
     and spras eq sy-langu.

  select werks name1
    from t001w appending table it_t001w
     for all entries in it_zsdt0001
   where werks eq it_zsdt0001-branch.

  perform seleciona_vbak.

  if it_vbak[] is not initial.
    select *
      from zsdt0011 appending table it_zsdt0011_o
       for all entries in it_vbak
     where tp_movimento = it_vbak-tp_movimento
       and auart        = it_vbak-auart.

    select kunnr name1 lzone
      from kna1 appending table it_kna1
      for all entries in it_vbak
      where kunnr	=	it_vbak-kunnr.

    select auart bezei
      from tvakt appending table it_tvakt
       for all entries in it_vbak
     where auart = it_vbak-auart
       and spras = sy-langu.

    select vbeln inco1
      from vbkd appending table it_vbkd
       for all entries in it_zsdt0001
     where vbeln  = it_zsdt0001-vbeln
       and inco1 in s_inco1
       and posnr  = '000000'.
  endif.

  if vg_cockpit = '01'. "Validar Regra ###

    select ebeln bsart reswk lifnr
      from ekko appending table it_ekko
       for all entries in it_zsdt0001
     where ebeln eq it_zsdt0001-vbeln
       and ebeln in s_ebeln
       and bsart in lra_bsart_cockpit_01.

  else.

    select ebeln bsart reswk lifnr
      from ekko appending table it_ekko
       for all entries in it_zsdt0001
     where ebeln eq it_zsdt0001-vbeln
       and ebeln in s_ebeln.

    if it_zsdt0062[] is not initial.
      select ebeln bsart reswk
        from ekko appending table it_ekko
         for all entries in it_zsdt0062
       where ebeln eq it_zsdt0062-ebeln
         and ebeln in s_ebeln.
    endif.
  endif.

  loop at it_ekko into wa_ekko.
    tabix = sy-tabix.
    read table it_zsdt0001 into wa_zsdt0001 with key vbeln = wa_ekko-ebeln binary search.
    wa_ekko-tp_movimento = wa_zsdt0001-tp_movimento.
    modify it_ekko from wa_ekko index tabix transporting tp_movimento.
  endloop.

  if it_ekko[] is not initial.
    select  *
      from zsdt0011 appending table it_zsdt0011_p
       for all entries in it_ekko
     where tp_movimento =  it_ekko-tp_movimento
       and bsart        =  it_ekko-bsart.

    select ebeln ebelp werks inco1
      from ekpo appending table it_ekpo
       for all entries in it_ekko
     where ebeln = it_ekko-ebeln
       and inco1 in s_inco1.

    loop at it_ekpo into wa_ekpo.
      tabix = sy-tabix .
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_ekpo-werks
        importing
          output = wa_ekpo-lifnr.
      modify it_ekpo from wa_ekpo index tabix transporting lifnr.
    endloop.

    select lifnr name1 dlgrp lzone
      from lfa1 appending table it_lfa1
       for all entries in it_ekpo
     where lifnr  = it_ekpo-lifnr.

    select bsart batxt
      from t161t appending table it_t161t
       for all entries in it_ekko
     where bsart = it_ekko-bsart
       and spras = sy-langu.
  endif.

  perform seleciona_vbpa.

  "Itens Romaneio
  select *
    from zsdt0001_item into corresponding fields of table tg_zsdt0001_item
     for all entries in it_zsdt0001
   where ch_referencia eq it_zsdt0001-ch_referencia.

  sort: it_mara     by matnr,
        it_lfa1     by lifnr,
        it_vbpa_co  by vbeln,  "Ponto de coleta  ORDEM
        it_ekpa_pr  by ebeln.  "Ponto de coleta  Pedido

endform.

form f_get_table_aux_rom_01.
endform.

form f_get_table_aux_rom_04.
endform.


form f_exclude_toolbar  using p_screen.

  clear: tl_function.

  case p_screen.
    when '0100'.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_check.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_refresh.
      append wl_function to tl_function.
    when others.
  endcase.

endform.

form f_config_layout  using p_screen.
  case p_screen.
    when '0100'.
      clear: wa_layout.
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
    when '0200'.
  endcase.
endform.

form f_set_status_proc.

  case vg_cockpit. "Set status Faturamento
    when '01' or '05' or '06' or '07' or '09' or '03' or '10'.

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

    when '02'. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
    when '03'. "Commodities (Trânsferência Recebidas )
    when '04'. "Fertilizantes (Porto Velho) - ZLES0115

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



    when '05'. "Insumos (Vendas e Trânsferências expedidas )


  endcase.

  vg_st_finalizado = '99'.

*----CS2021000508 - 07.06.2021 - JT - inicio
  vg_st_aguard_doc_carg = '98'.
*----CS2021000508 - 07.06.2021 - JT - fim

endform.

*----CS2021000508 - 07.06.2021 - JT - inicio
form f_action_user_docs_carguero using p_saida            type ty_saida
                                       p_tipo_chamada     type char01
                              changing it_saida_romaneios type zde_les_saida_zsdt0001_t
                                       it_tab_bapiret1    type tab_bapiret1.

  data: wa_saida_tn  type zde_les_saida_zsdt0001_tn.

  read table it_saida_romaneios assigning field-symbol(<fs_out>)
                                with key ch_referencia = p_saida-ch_referencia.

  move-corresponding p_saida to wa_saida_tn.

  call function 'ZSD_TROCA_NOTA_UPLOAD'
    exporting
      i_zsdt0001                  = wa_saida_tn
    importing
      e_st_proc                   = <fs_out>-st_proc
      e_docs_enviado_carguero     = <fs_out>-docs_enviado_carguero
    exceptions
      erro_upload                 = 1
      erro_upload_nao_autorizado  = 2
      erro_romaneio_nao_trocanota = 3
      erro_aprovacao_carrega      = 4
      others                      = 5.

  if     sy-subrc = 0.
    refresh: it_saida.
    perform: f_seleciona_dados, " Form seleciona dados
             f_saida, " Form de saida
             f_refresh_alv using '0100'. "Refresh na tela
  elseif sy-subrc = 1.
    message i024(sd) with 'Erro UPLOAD arquivos no Carguero!'.
  elseif sy-subrc = 2.
    message i024(sd) with 'UPLOAD não autorizado no Carguero!'.
  elseif sy-subrc = 3.
    message i024(sd) with 'Romaneio não é Troca Nota!'.
  elseif sy-subrc = 4.
*   MESSAGE i024(sd) WITH 'Erro na Aprovacao do Carregamento!'.
  endif.

endform.
*----CS2021000508 - 07.06.2021 - JT - inicio

*-#133089-12.02.2024-JT-inicio
*******************************************************************
* SELECAO FATURAEMENTO AUTOMATICO
* CHAMADA PELA CLASSE ZCL_AUTOMATIZAR_FATURAMENTO
*******************************************************************
form f_selecao_fat_autom using p_ch_ref
                               p_cockpit.

  free: it_saida.

  perform f_refresh_data.

*---------------------------------------------------
*-SET selecao "Pesagem OPUS saida
*---------------------------------------------------
  vg_cockpit           = p_cockpit.
  vg_faturamento_autom = abap_true.

*---------------------------------------------------
*-Romaneios
*---------------------------------------------------
  select *
    from zsdt0001
    into table it_zsdt0001
   where ch_referencia = p_ch_ref.

  perform f_set_status_proc.
  perform f_selecao_generica_rom.
  perform f_get_value_set tables t_auart  using 'MAGGI_ARMAZENAGEM_VA01'.
  perform f_get_value_set tables t_usermd using 'MAGGI_ZLES106_RECUP'.
  perform f_config_ranges.

endform.

*******************************************************************
* RECUPERAR DADOS PARA FATURAMENTO AUTOMATICO
* CHAMADA PELA CLASSE ZCL_AUTOMATIZAR_FATURAMENTO
*******************************************************************
form f_recuperar_dados  changing t_saida    type zde_les_saida_zsdt0001_t
                                 p_zsdt0001 type ty_zsdt0001
                                 p_saida    type ty_saida.

  read table it_zsdt0001 into p_zsdt0001  index 1.
  read table it_saida    into p_saida     index 1.
  t_saida[] = it_saida[].

endform.
*-#133089-12.02.2024-JT-fim

*-CS2024000086-26.09.2024-#151423-JT-inicio
*******************************************************************
* SETAR TIPO DE FATURAMENTO (MANUAL / AUTOMATICO )
*******************************************************************
form f_set_tipo_faturamento using p_tipo_fat.

  vg_faturamento_autom = p_tipo_fat.
  vg_no_grid           = abap_true.

endform.
"*-CS2024000086-26.09.2024-#151423-JT-fim

form f_action_user_transp  using p_saida type ty_saida p_tipo_chamada type char01
                           changing it_saida_romaneios type zde_les_saida_zsdt0001_t
                                    it_tab_bapiret1    type tab_bapiret1
                           raising zcx_error. "*-#133089-12.02.2024-JT

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  read table it_saida_romaneios assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  if ( <fs_out>-transp = icon_execute_object ) or ( <fs_out>-transp is initial and p_tipo_chamada = 'E' ).

    if p_tipo_chamada eq 'L'.
      perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia Romaneio
      if sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          when abap_true.
            data(l_mesg) = |Romaneio bloqueado por { sy-msgv1 }. Processamento Paralizado.|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.
    endif.

    update zsdt0001
       set st_proc = <fs_out>-st_proc
     where ch_referencia = <fs_out>-ch_referencia.

    perform f_gerar_vt using 'T' p_tipo_chamada changing wl_erro <fs_out> it_tab_bapiret1.
    check wl_erro eq 'N'.
    perform f_check_retorno_vt using 'T' wl_erro changing <fs_out>.

    if p_tipo_chamada eq 'L'.
      perform f_repare_docs_romaneio changing <fs_out>.
      perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia Romaneio
      perform f_refresh_alv using '0100'. "Refresh na Tela
    endif.

  elseif <fs_out>-transp ne icon_icon_list and <fs_out>-transp+0(4) ne '@11@'.
    if p_tipo_chamada eq 'L' and vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
      set parameter id 'TNR' field <fs_out>-transp+0(10).
      call transaction 'VT03N' and skip first screen.
    endif.
  endif.

endform.

form f_action_user_dacte using p_saida type ty_saida
                       raising zcx_error. "*-#133089-12.02.2024-JT

  data wa_active_mod type j_1bnfe_active.     "*-#133089-12.02.2024-JT

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  check ( <fs_out>-dacte ne icon_icon_list ).

  if ( <fs_out>-dacte(1) eq '@' ).

    clear wa_zsdt0001.
    select single *
      from zsdt0001 into wa_zsdt0001
     where ch_referencia = <fs_out>-ch_referencia.

    if wa_zsdt0001-nro_nf_frete gt 0 and
       vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
      exit.
    endif.

    if wa_zsdt0001-fat_contingencia_ecc eq abap_true.

      data: lva_ok          type  char01,
            lva_msg_retorno type  string.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia  = p_saida-ch_referencia
          i_check_frete_ok = abap_true
        importing
          e_ok             = lva_ok
          e_msg_retorno    = lva_msg_retorno.

      if lva_ok = abap_false.
        message lva_msg_retorno type 'I'.
        return.
      endif.

    endif.

    if sy-tcode ne 'ZLES0136'
      and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      and sy-tcode ne 'ZMM0127'
      and vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Transação apenas de visualização' type 'I'.
      exit.
    endif.

    if <fs_out>-fatserv = icon_icon_list and
      vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT.
      message 'Gerar a Fatura Frete!' type 'I'.
      exit.
    endif.

    perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    if sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        when abap_true.
          data(l_mesg) = |Romaneio bloqueado por { sy-msgv1 }. Processamento Paralizado.|.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
      endcase.
*-#133089-21.02.2024-JT-fim
    endif.

    update zsdt0001 set st_proc       = <fs_out>-st_proc
                  where ch_referencia = <fs_out>-ch_referencia.

    refresh it_color.
    move 'REMESSA'   to wa_color-fname.
    move '5'         to wa_color-color-col.
    move '1'         to wa_color-color-int.
    move '1'         to wa_color-color-inv.
    append wa_color to it_color.
    <fs_out>-color_cell[] = it_color[].

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = <fs_out>-fatserv
      importing
        output = <fs_out>-fatserv.

    select single j_1bnfdoc~bukrs j_1bnflin~docnum
      into (vl_bukrs,vl_docnum)
      from j_1bnflin inner join j_1bnfdoc on j_1bnfdoc~docnum = j_1bnflin~docnum
     where j_1bnflin~refkey = <fs_out>-fatserv.

    check sy-subrc = 0.

*-#133089-21.02.2024-JT-inicio
    if vg_faturamento_autom = abap_true.
*---------------------------
*---- autorizacao na classe ZCL_FATURAMENTO_AUTOMATICO
*---------------------------
    else.
*-#133089-21.02.2024-JT-fim
      set parameter id 'Z_MY_PARAMETER_1' field vl_docnum.
      set parameter id 'Z_MY_PARAMETER_2' field vl_bukrs.
      call transaction 'ZCTE' and skip first screen.
      get parameter id 'Z_MY_PARAMETER_1' field <fs_out>-dacte.

      if <fs_out>-dacte ne icon_complete.
        perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
        message 'Dacte ainda não autorizado pela SEFAZ' type 'I'.
        exit.
      endif.
    endif. "*-#133089-21.02.2024-JT

    perform f_check_auth_doc using vl_docnum.

    if sy-subrc eq 0.
      clear <fs_out>-icon.

      <fs_out>-dacte   = vl_docnum.
      <fs_out>-st_proc = vg_st_dacte.

      update zsdt0001 set nro_nf_frete = <fs_out>-dacte
                          st_proc      = vg_st_dacte
       where ch_referencia = <fs_out>-ch_referencia.

    else.
      <fs_out>-dacte = icon_execute_object.
    endif.

    perform f_repare_docs_romaneio changing <fs_out>.
    perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    perform f_refresh_alv using '0100'.

  else.
    if vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
      select single bukrs
        from j_1bnfdoc into vl_bukrs
       where docnum = <fs_out>-dacte.

      check sy-subrc = 0.

      set parameter id 'Z_MY_PARAMETER_1' field <fs_out>-dacte.
      set parameter id 'Z_MY_PARAMETER_2' field vl_bukrs.
      call transaction 'ZCTE' and skip first screen.
    endif.
  endif.

endform.

form f_action_user_danfe using p_saida type ty_saida
                         raising zcx_error. "*-#133089-12.02.2024-JT

  data: wa_active_mod  type j_1bnfe_active,  "*-#133089-12.02.2024-JT
        lc_gera_transp type char01.         "*-CS2024000086-25.09.2024-#133287-JT-inicio

  field-symbols <fs_saida> type ty_saida.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  check ( <fs_out>-danfe ne icon_icon_list ).

  if ( <fs_out>-danfe(1) eq '@' ).

    clear wa_zsdt0001.

    select single *
      from zsdt0001 into wa_zsdt0001
     where ch_referencia = <fs_out>-ch_referencia.

    if wa_zsdt0001-nro_nf_prod  gt 0 and
      vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
      exit.
    endif.

    "Ajustes Geração Frete CPT 27/02/24 - Ini
    case vg_cockpit.
      when '01' or '05' or '06' or '07'.

        if <fs_out>-danfe = icon_execute_object. " AND ( <fs_out>-inco1 = 'CPT' ). " OR lc_gera_transp = abap_true ). "*-CS2024000086-25.09.2024-#133287-JT
          "*-CS2024000086-25.09.2024-#133287-JT-inicio
          if <fs_out>-inco1 = 'CPT'.
            if <fs_out>-lifnr is initial.
              case vg_faturamento_autom.
                when abap_off.
                  message 'Agente de frete nao informado!' type 'I'.
                  exit.
                when abap_true.
                  data(l_mesg) = 'Agente de frete nao informado!'.
                  lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
              endcase.
            endif.

            select single *
              from lfa1 into @data(lwa_lfa1_check)
             where lifnr = @<fs_out>-lifnr.

            if sy-subrc ne 0.
              case vg_faturamento_autom.
                when abap_off.
                  message |Agente de frete { <fs_out>-lifnr } não encontrado!| type 'I'.
                  exit.
                when abap_true.
                  l_mesg = |Agente de frete { <fs_out>-lifnr } não encontrado!|.
                  lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
              endcase.
            endif.
          endif.
          "*-CS2024000086-25.09.2024-#133287-JT-fim

*-CS2024000086-26.09.2024-#151423-JT-inicio
          try.
              data(lc_gerou_vt) = zcl_faturamento=>zif_faturamento~get_instance( )->get_status_gerou_vt_vi( <fs_out>-ch_referencia ).

            catch zcx_error into data(_zcx_error).
              lc_gerou_vt = abap_false.
          endtry.
*-CS2024000086-26.09.2024-#151423-JT-fim

          if lc_gerou_vt = abap_false.
*-CS2024000086-26.09.2024-#151423-JT-inicio
*          DATA(_gerou_vt_vi) = abap_false.

*          DATA(_rem_conta_ordem) = abap_false.
*          PERFORM f_check_rem_conta_ordem USING <fs_out> CHANGING _rem_conta_ordem.
*
*          IF ( lwa_lfa1_check-ktokk NE 'ZFIC' AND _rem_conta_ordem = abap_false ) OR lc_gera_transp = abap_true. "*-CS2024000086-25.09.2024-#133287-JT
*
*            DATA(_gerou_vt_vi) = abap_false.
*
*            IF wa_zsdt0001-fknum IS INITIAL.
*-CS2024000086-26.09.2024-#151423-JT-fim
*
            call function 'SAPGUI_PROGRESS_INDICATOR'
              exporting
                text = |Criando documentos de transporte e custo para o romaneio { conv i( <fs_out>-nr_romaneio ) }.|.

            clear  wl_erro.
            perform f_gerar_vt using ''
                                     'L'
                            changing wl_erro
                                     <fs_out>
                                     t_return[].
            perform f_check_retorno_vt using ''
                                             wl_erro
                                    changing <fs_out>.
*            ENDIF.

            perform f_repare_docs_romaneio changing <fs_out>.

*-CS2024000086-26.09.2024-#151423-JT-inicio
            try.
                data(_gerou_vt_vi) = zcl_faturamento=>zif_faturamento~get_instance(
                                      )->get_status_gerou_vt_vi( exporting i_ch_referencia    = <fs_out>-ch_referencia
                                                                           i_check_doc_gerado = abap_true ).
              catch zcx_error into _zcx_error.
                _gerou_vt_vi = abap_false.
            endtry.
*-CS2024000086-26.09.2024-#151423-JT-fim

*-CS2024000086-26.09.2024-#151423-JT-inicio
*            SELECT SINGLE *
*              FROM zsdt0001 INTO @DATA(wa_zsdt0001_check)
*             WHERE ch_referencia = @<fs_out>-ch_referencia.
*
*            IF sy-subrc EQ 0 AND wa_zsdt0001_check-fknum IS NOT INITIAL.
*
*              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                EXPORTING
*                  input  = wa_zsdt0001_check-fknum
*                IMPORTING
*                  output = wa_zsdt0001_check-fknum.
*
*              DO 5 TIMES.
*
*                SELECT SINGLE *
*                  FROM vfkp INTO @DATA(lwa_vfkp)
*                 WHERE fknum = @wa_zsdt0001_check-fknum
*                   AND fkpty = 'Z001'.
*
*                IF sy-subrc EQ 0.
*
*                  IF lwa_vfkp-netwr = 0.
**-#133089-21.02.2024-JT-inicio
*                    CASE vg_faturamento_autom.
*                      WHEN abap_off.
*                        MESSAGE |Documento de Custo { wa_zsdt0001_check-fknum } criado sem valor!| TYPE 'I'.
*                        RETURN.
*                      WHEN abap_true.
*                        l_mesg = |Documento de Custo { wa_zsdt0001_check-fknum } criado sem valor!|.
*                        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = CONV #( l_mesg ) ).
*                    ENDCASE.
**-#133089-21.02.2024-JT-fim
*                  ELSE.
*                    _gerou_vt_vi = abap_true.
*                  ENDIF.
*
*                  EXIT.
*                ENDIF.
*
*                WAIT UP TO 2 SECONDS.
*              ENDDO.
*            ENDIF.
*-CS2024000086-26.09.2024-#151423-JT-inicio

            if _gerou_vt_vi eq abap_false.
*-#133089-21.02.2024-JT-inicio
              case vg_faturamento_autom.
                when abap_off.
                  message |Documento de custo não gerado para o romaneio {  <fs_out>-nr_romaneio }!| type 'I'.
                  exit.
                when abap_true.
                  l_mesg = |Documento de custo não gerado para o romaneio {  <fs_out>-nr_romaneio }!|.
                  lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
              endcase.
*-#133089-21.02.2024-JT-fim
            endif.

          endif.

        endif.

    endcase.

    "Ajustes Geração Frete CPT 27/02/24 - Fim

    if wa_zsdt0001-fat_contingencia_ecc eq abap_true.

      data: lva_ok          type  char01,
            lva_msg_retorno type  string.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia  = p_saida-ch_referencia
          i_check_danfe_ok = abap_true
        importing
          e_ok             = lva_ok
          e_msg_retorno    = lva_msg_retorno.

      if lva_ok = abap_false.
        message lva_msg_retorno type 'I'.
        return.
      endif.

    endif.

    if sy-tcode ne 'ZLES0136'
      and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      and sy-tcode ne 'ZMM0127'
      and vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Transação apenas de visualização' type 'I'.
      exit.
    endif.
    if <fs_out>-fatura = icon_execute_object and
      vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Gerar a Fatura!' type 'I'.
      exit.
    endif.

    if <fs_out>-fatura is not initial.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = <fs_out>-fatura
        importing
          output = <fs_out>-fatura.

      "Verifica estorno fatura
      select single vbeln mjahr
         into (vl_vbeln,vl_mjahr)
         from vbfa
        where vbelv = <fs_out>-fatura "Fatura que encontrou
          and vbtyp_n  = 'N'. "Estorno
      if sy-subrc = 0.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'O doc de fatura está cancelado. Refazer o lançamento!' type 'I'.
            exit.
          when abap_true.
            l_mesg = 'O doc de fatura está cancelado. Refazer o lançamento!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

    endif.

    perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    if sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        when abap_true.
          data(l_mesg2) = |Romaneio bloqueado por { sy-msgv1 }. Processamento Paralizado.|.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg2 ).
      endcase.
*-#133089-21.02.2024-JT-fim
    endif.

    update zsdt0001 set st_proc = <fs_out>-st_proc
     where ch_referencia = <fs_out>-ch_referencia.

    refresh it_color.
    move 'REMESSA'   to wa_color-fname.
    move '5'         to wa_color-color-col.
    move '1'         to wa_color-color-int.
    move '1'         to wa_color-color-inv.
    append wa_color to it_color.
    <fs_out>-color_cell[] = it_color[].

    if ( <fs_out>-tipo = 'P' ) or ( <fs_out>-tipo = 'T' ).

      select single vbeln mjahr
        into (vl_vbeln,vl_mjahr)
        from vbfa
        where vbelv = <fs_out>-remessa
        and vbtyp_n  = 'R'
        and vbtyp_v  = 'J'.

      concatenate vl_vbeln vl_mjahr into vl_refkey.
      select single docnum
        from j_1bnflin
        into vl_docnum
        where refkey = vl_refkey.
    else.
      select single docnum
        from j_1bnflin
        into vl_docnum
        where refkey = <fs_out>-fatura.
    endif.

*-#133089-21.02.2024-JT-inicio
    if vg_faturamento_autom = abap_true.
*---------------------------
*---- autorizacao na classe ZCL_FATURAMENTO_AUTOMATICO
*---------------------------
    else.
*-#133089-21.02.2024-JT-fim
      set parameter id 'Z_MY_PARAMETER_1' field vl_docnum.
      set parameter id 'Z_MY_PARAMETER_2' field <fs_out>-bukrs.

      call transaction 'ZNFE' and skip first screen.
      get parameter id 'Z_MY_PARAMETER_1' field <fs_out>-danfe.

      if ( <fs_out>-danfe ne icon_complete ).
        perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
        message 'Danfe ainda não autorizado pela SEFAZ' type 'I'.
        exit.
      endif.
    endif. "*-#133089-21.02.2024-JT

    perform f_check_auth_doc using vl_docnum.

    if sy-subrc eq 0.
      <fs_out>-danfe   = vl_docnum.
      <fs_out>-st_proc = vg_st_danfe.

      update zsdt0001 set nro_nf_prod = <fs_out>-danfe
                          st_proc      = vg_st_danfe
       where ch_referencia = <fs_out>-ch_referencia.
    else.
      <fs_out>-danfe = icon_execute_object.
    endif.

    perform f_repare_docs_romaneio changing <fs_out>.
    perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    perform f_refresh_alv using '0100'.


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

  else.
    if vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
      set parameter id 'Z_MY_PARAMETER_1' field <fs_out>-danfe.
      set parameter id 'Z_MY_PARAMETER_2' field <fs_out>-bukrs.
      call transaction 'ZNFE' and skip first screen.
    endif.
  endif.

endform.

form f_action_user_icon using p_saida type ty_saida.

  data: wa_zlest0100 type zlest0100.  "*-#158056-11.11.2024-JT-inicio

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

*-#158056-11.11.2024-JT-inicio
  free: ti_zlest0100.

  select single ch_faturamento
    into @data(_ch_faturamento)
    from zlest0241
   where ch_referencia = @p_saida-ch_referencia
     and cancelado     = @abap_false.

  if sy-subrc = 0.
    select *
      into table @data(t_zlest0242)
      from zlest0242
     where ch_faturamento = @_ch_faturamento.

    if sy-subrc = 0.
      loop at t_zlest0242 into data(w_zlest0242) where status_msg = 'E'.
        wa_zlest0100-msgv1   = w_zlest0242-mensagem.
        wa_zlest0100-data    = w_zlest0242-data_reg.
        wa_zlest0100-hora    = w_zlest0242-hora_reg.
        wa_zlest0100-usuario = w_zlest0242-user_reg.
        append wa_zlest0100 to ti_zlest0100.
      endloop.
    endif.
  endif.

  if it_zlest0100[] is not initial.
    <fs_out>-icon = icon_led_red.
  endif.
*-#158056-11.11.2024-JT-fim

  if <fs_out>-icon = icon_led_red.

    select *
*     FROM zlest0100 INTO TABLE ti_zlest0100
      from zlest0100 appending table ti_zlest0100  "*-#158056-11.11.2024-JT
     where ch_referencia = <fs_out>-ch_referencia.

    if ti_zlest0100[] is not initial.
      perform f_montar_layout_log.
      call function 'REUSE_ALV_GRID_DISPLAY'
        exporting
          it_fieldcat           = estrutura[]
          i_save                = 'A'
          i_screen_start_column = 3
          i_screen_start_line   = 3
          i_screen_end_column   = 100
          i_screen_end_line     = 13
        tables
          t_outtab              = ti_zlest0100.
    endif.

  endif.

endform.

form f_lock_rom using p_status
                      p_ch_referencia.

  case p_status.
    when 'B'. "Bloqueio

      call function 'ENQUEUE_EZSDT0001'
        exporting
          ch_referencia  = p_ch_referencia
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.

    when 'D'. "Desbloqueio

      call function 'DEQUEUE_EZSDT0001'
        exporting
          ch_referencia = p_ch_referencia.
  endcase.

endform.

form f_check_retorno_vt using p_tipo
                              p_erro
                     changing p_saida type ty_saida
                      raising zcx_error. "*-#133089-12.02.2024-JT

  clear: vl_fknum, vl_ov_frete, vl_fatura_frete.

  check p_saida-ch_referencia is not initial.


  if line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] )
    and p_tipo ne 'T'.
    exit.
  endif.

  if ( lines( p_saida-romaneios_agr[] ) > 1 ) and ( p_tipo ne 'T' ).
    exit.
  endif.

  if ( p_erro eq 'N' or p_tipo eq 'T' ) and ( v_tknum is not initial ).

    p_saida-transp  = v_tknum.
    p_saida-st_proc = vg_st_transp.

    loop at p_saida-romaneios_agr into data(_wl_rom).

      update zsdt0001 set doc_transp = p_saida-transp
                          st_proc    = vg_st_transp "Doc transporte
       where ch_referencia = _wl_rom-ch_referencia.

      read table it_saida assigning field-symbol(<fs_saida_tmp>) with key ch_referencia = _wl_rom-ch_referencia.
      if sy-subrc eq 0.
        <fs_saida_tmp>-transp  = v_tknum.
        <fs_saida_tmp>-st_proc = vg_st_transp.
      endif.
    endloop.

    data(lva_data_mov_romaneio) = p_saida-dt_movimento.

    if wa_zsdt0001-fat_contingencia_ecc eq abap_true.
      data: lwa_faturamento_ecc type zde_compare_faturamento.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia         = p_saida-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        importing
          e_dados_faturamento_ecc = lwa_faturamento_ecc.

      if lwa_faturamento_ecc-data_lcto_cte is not initial.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_cte.
      elseif lwa_faturamento_ecc-data_lcto_nf is not initial.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_nf.
      elseif lwa_faturamento_ecc-data_lcto_nf_rem is not initial.
        lva_data_mov_romaneio = lwa_faturamento_ecc-data_lcto_nf_rem.
      endif.
    endif.

    perform f_memorizar_dt_movimento_badi using lva_data_mov_romaneio.

    perform f_set_encerramento_docs changing p_saida.

    "Gerar custo
    if ( p_saida-inco1 = 'CPT' ) or ( p_saida-enc_doc_custo eq abap_true ).

      data(_frota_prop) = abap_false.
      if p_saida-tipo_veiculo = 'P'.
        _frota_prop = abap_true.
      endif.

      submit zlesr0013 with so_tknum = p_saida-transp
                       with p_chave  = p_saida-ch_referencia
                       with rb_out   = ''
                       with rb_cus   = 'X'
                       with rb_dtfat = lva_data_mov_romaneio
                       with ckrom    = abap_true
                       with ckfprop  = _frota_prop
                       with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
      and return.

    else.

      data(lc_rb_out) = p_saida-emite_conhecimento.

      submit zlesr0013 with so_tknum = p_saida-transp
                       with p_chave  = p_saida-ch_referencia
                       with rb_dtfat = lva_data_mov_romaneio
                       with ckrom    = abap_true
                       with rb_out   = lc_rb_out
                       with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
                       and return.
    endif.

    get parameter id 'Z_MY_PARAMETER_1' field vl_fknum.
    get parameter id 'Z_MY_PARAMETER_2' field vl_ov_frete.
    get parameter id 'Z_MY_PARAMETER_3' field vl_fatura_frete.

    if vl_fknum is not initial.

      loop at p_saida-romaneios_agr into _wl_rom.

        p_saida-doccus  = vl_fknum.
        p_saida-ovserv  = vl_ov_frete.
        p_saida-fatserv = vl_fatura_frete.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = p_saida-fatserv
          importing
            output = p_saida-fatserv.

        if vl_fatura_frete is not initial. "Fatura Frete

          update zsdt0001 set st_proc      = vg_st_fatura_frete
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
           where ch_referencia = _wl_rom-ch_referencia.

          p_saida-st_proc = vg_st_fatura_frete.

        elseif vl_ov_frete is not initial. "O.V. Frete

          update zsdt0001 set st_proc      = vg_st_ov_frete
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
          where ch_referencia = _wl_rom-ch_referencia.

          p_saida-st_proc = vg_st_ov_frete.

        elseif vl_fknum is not initial. "Doc.Custo

          update zsdt0001 set st_proc      = vg_st_custo
                              fknum        = vl_fknum
                              ov_frete     = vl_ov_frete
                              fatura_frete = vl_fatura_frete
          where ch_referencia = _wl_rom-ch_referencia.
          p_saida-st_proc = vg_st_custo.

        endif.

        if ( p_saida-inco1 = 'CPT' ) or ( p_saida-enc_doc_custo eq abap_true ). " Finaliza processo com a Fatura serviço gerada

*----CS2021000508 - 07.06.2021 - JT - inicio
          if p_saida-troca_nota            = abap_true and
             p_saida-docs_enviado_carguero = abap_false.
            update zsdt0001 set st_proc = vg_st_aguard_doc_carg " Finalizado
             where ch_referencia = _wl_rom-ch_referencia.

            p_saida-st_proc = vg_st_aguard_doc_carg.
          else.
            update zsdt0001 set st_proc = vg_st_finalizado " Finalizado
             where ch_referencia = _wl_rom-ch_referencia.

            p_saida-st_proc = vg_st_finalizado.
          endif.

*         UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
*          WHERE ch_referencia = _wl_rom-ch_referencia.

          clear p_saida-icon.

          p_saida-dacte   = icon_icon_list.
*         p_saida-st_proc = vg_st_finalizado.
*----CS2021000508 - 07.06.2021 - JT - fim
        endif.

        read table it_saida assigning <fs_saida_tmp> with key ch_referencia = _wl_rom-ch_referencia.
        if sy-subrc eq 0.
          <fs_saida_tmp>-doccus    =  p_saida-doccus.
          <fs_saida_tmp>-ovserv    =  p_saida-ovserv.
          <fs_saida_tmp>-fatserv   =  p_saida-fatserv.
          <fs_saida_tmp>-st_proc   =  p_saida-st_proc.
          <fs_saida_tmp>-dacte     =  p_saida-dacte.
          <fs_saida_tmp>-icon      =  p_saida-icon.
        endif.

      endloop.

    endif.

  else.
    if vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
      message 'Erro ao gerar transporte!' type 'I'.
    endif.
  endif.

endform.

form f_refresh_alv using p_screen.

  check vg_faturamento_autom = abap_off. "*-#133089-21.02.2024-JT
  check vg_no_grid           = abap_off. "*-CS2024000086-26.09.2024-#151423-JT-inicio

  case p_screen.
    when '0100'.
      call method cl_grid->refresh_table_display
        exporting
          is_stable = wa_stable.
  endcase.

endform.

form f_action_user_doc_znfw  using p_saida type ty_saida.

  data: v_ematn type ekpo-matnr. "CS2017002682 - 29.11.2017
  data: v_matnr18 type matnr18.

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  if <fs_out>-seq_lcto = icon_execute_object.
    clear: wa_zsdt0001, wa_saida-ematn. "CS2017002682 - 29.11.2017

    select single *
      from zfiwrt0008 into @data(_wl_zfiwrt0008)
     where ch_referencia   eq @<fs_out>-ch_referencia
       and loekz           eq @abap_false
       and docs_estornados eq @abap_false.

    if sy-subrc = 0.
      message i000(z01) with 'Já existe nota de remessa'
                             _wl_zfiwrt0008-seq_lcto
                             'para este romaneio.'
                             '<ATUALIZAR DOCUMENTOS>'.
      exit.
    endif.

    select single *
      from zsdt0001
      into wa_zsdt0001
     where ch_referencia = <fs_out>-ch_referencia.

    if wa_zsdt0001-seq_lcto gt 0.
      message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
      exit.
    endif.

    if <fs_out>-netpr le 0.
      message 'Informe o valor unitario da nota de Remessa!' type 'I'.
      exit.
    endif.

    if <fs_out>-lifnr is initial.

*-CS2021000218-16.11.2022-#90706-JT-inicio
      if <fs_out>-inco1 <> 'FOB'.
        message 'Informar o agente de frete!' type 'I'.
        exit.
      endif.
*-CS2021000218-16.11.2022-#90706-JT-fim

    elseif ( <fs_out>-region is initial ) and ( wa_zsdt0001-placa_cav is not initial ) .
      message 'Informar a UF da placa cavalo!' type 'I'.
      exit.
    endif.

    "Valida placa veiculo - Transporte Romaneio.
    data(_placa_com_erro) = abap_false.
    perform f_valida_placas_faturamento using  <fs_out>
                                       changing _placa_com_erro.
    check _placa_com_erro is initial.

    perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if <fs_out>-tipo = 'O'.
      if <fs_out>-ebeln is initial.
        message 'Informe um pedido da ordem!' type 'I'.
        exit.
      else.
        read table it_zsdt0062 into wa_zsdt0062 with key vbeln = <fs_out>-vbeln
                                                         ebeln = <fs_out>-ebeln
                                                         ebelp = wa_saida-ebelp binary search.
        if sy-subrc ne 0.
          message 'Este pedido não pertence a esta Ordem!' type 'I'.
          exit.
        endif.
      endif.

      "CS2017002682 - 29.11.2017 - Ini
      clear: v_ematn.
      if <fs_out>-ebelp is initial.
        message 'Informe o item do pedido da ordem!' type 'I'.
        exit.
      endif.

      select single *
        from ekpo into @data(_wl_ekpo)
       where ebeln = @<fs_out>-ebeln
         and ebelp = @<fs_out>-ebelp.
      if ( sy-subrc ne 0 ) or ( _wl_ekpo-matnr is initial ).
        message 'Pedido de Importação não existe!' type 'I'.
        exit.
      endif.

      v_ematn = _wl_ekpo-matnr.

      select single *
        from makt into @data(_wl_makt)
       where matnr = @v_ematn
         and spras = @sy-langu.

      if sy-subrc ne 0.
        message |Descrição do Material { _wl_ekpo-matnr } não encontrada!|  type 'I'.
        exit.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = v_ematn
        importing
          output = v_ematn.

      concatenate v_ematn '-' _wl_makt-maktx into  <fs_out>-material.
      <fs_out>-ematn   = v_ematn.
      <fs_out>-lgort_n = wa_zsdt0062-lgort.
      <fs_out>-charg_n = wa_zsdt0062-charg.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = <fs_out>-ematn
        importing
          output = v_matnr18.

      <fs_out>-ematn = v_matnr18.
      "CS2017002682 - 29.11.2017 - Fim

    elseif <fs_out>-tipo = 'T'.
      if <fs_out>-ebeln is initial.
        message 'Informe um pedido de Importação!' type 'I'.
        exit.
      else.
        select single *
          from ekpo
          into corresponding fields of wa_ekpo
          where ebeln = <fs_out>-ebeln
          and   matnr = <fs_out>-matnr.
        if sy-subrc ne 0.
          message 'Pedido de Importação não existe!' type 'I'.
          exit.
        endif.
      endif.
    endif.

    clear vl_seq_lcto.
    perform  f_nota_remessa using <fs_out>
                         changing vl_seq_lcto.
    if vl_seq_lcto is not initial.
      <fs_out>-seq_lcto = vl_seq_lcto.
      <fs_out>-st_proc = '11'.

      update zsdt0001 set st_proc      = '11' " znfw gravado
                          status       = 'X'
                          seq_lcto     = vl_seq_lcto
                          region       = <fs_out>-region
                          agente_frete = <fs_out>-lifnr
                          ebeln        = <fs_out>-ebeln
                          ebelp        = <fs_out>-ebelp    "CS2017002682 - 29.11.2017
             where ch_referencia = <fs_out>-ch_referencia.

      perform f_refresh_alv using '0100'.
    endif.

    perform f_repare_docs_romaneio changing <fs_out>.
    perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

  elseif <fs_out>-seq_lcto  is not initial.
    refresh: tl_bdc.
    perform f_preencher_dynpro using:
           'X' 'ZWRR0002'             '0100',
           ' ' 'P_SEQ_LCTO'           <fs_out>-seq_lcto ,
           ' ' 'BDC_OKCODE'           'SEARCH'.

    opt-dismode = 'E'.
    opt-defsize = ' '.
    call transaction 'ZNFW0002' using tl_bdc options from opt.
    clear wa_zfiwrt0008.
    select single *
         from zfiwrt0008
         into wa_zfiwrt0008
         where seq_lcto = <fs_out>-seq_lcto.
    if ( sy-subrc eq 0 ) and ( wa_zfiwrt0008-loekz = 'X' ).
      wa_zsdt0001-seq_lcto = ''.
      <fs_out>-seq_lcto        = icon_execute_object.

      "CS2017002682 - 29.11.2017 - Ini
      if <fs_out>-tipo = 'O'.
        update zsdt0001 set st_proc      = ''
                            agente_frete = ''
                            seq_lcto     = ''
                            status       = ''
                            ebeln        = ''
                            ebelp        = 00000
        where ch_referencia = <fs_out>-ch_referencia.
      else. "CS2017002682 - 29.11.2017 - Fim
        update zsdt0001 set st_proc      = ''
                            agente_frete = ''
                            seq_lcto     = ''
                            status       = ''
        where ch_referencia = <fs_out>-ch_referencia.
      endif.

      <fs_out>-st_proc = ''.
      <fs_out>-lifnr =  ''.
      <fs_out>-netpr  =  0.
      refresh style.
      clear: wa_style.
      if <fs_out>-netpr is not initial.
        wa_style-fieldname = 'NETPR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .
      endif.
      clear: wa_style.
      if <fs_out>-region is not initial.
        wa_style-fieldname = 'REGION'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .
      endif.
      clear: wa_style.
      if <fs_out>-lifnr is not initial.
        wa_style-fieldname = 'LIFNR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .
      endif.
      if <fs_out>-ebeln is not initial.
        wa_style-fieldname = 'EBELN'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .

        "CS2017002682 - 29.11.2017 - Ini
        wa_style-fieldname = 'EBELP'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        insert  wa_style into table style .
        "CS2017002682 - 29.11.2017 - Fim
      endif.
      <fs_out>-style[] = style[].

      perform f_repare_docs_romaneio changing <fs_out>.
      perform f_refresh_alv using '0100'. "Refresh na tela
    endif.
  endif.


endform.

form f_monta_impostos tables tl_impo structure tg_impo
                       using e_row.
  data: begin of wl_1btxic,
          rate type j_1btxic3-rate,
          base type j_1btxic3-base,
        end of wl_1btxic.
  data: wl_itens     like line of tg_itens,
        wl_1baa      type j_1baa,
        wl_base_aux  type j_1btxic3-base,
        wl_a924      type a924,
        wl_konp      type konp,
        wl_t001w     type t001w,
        wl_1btxsdc   type j_1btxsdc,
        wl_1btxpis   type j_1btxpis,
        wl_1btxcof   type j_1btxcof,
        wl_impo_comp like line of tg_impo_comp.

  read table tg_itens into wl_itens index 1. "E_ROW.

  select single *
    from j_1baa
    into wl_1baa
     where nftype eq wl_0001-nftype.

  if ( wl_1baa-direct eq '1' ).
    clear: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
    select single *
      from j_1btxsdc
      into wl_1btxsdc
       where taxcode eq tl_0006-taxcode.

    loop at tg_impo.
      read table tg_impo_comp into wl_impo_comp with key itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp binary search.
      if sy-subrc eq 0 and  wl_0001-complemento eq 'S'.
        move-corresponding: wl_impo_comp to tl_impo.
        move :  tg_impo-ttypetxt  to tl_impo-ttypetxt,
                tg_impo-taxgrp    to tl_impo-taxgrp.
        append tl_impo.
      elseif tg_impo[] is not initial.
        if tg_impo-taxtyp eq 'ICM3'.
          if tl_0006-opertyp eq 'T'.
            if wl_1baa-entrad eq 'X'.
              select single rate base
                from j_1btxic3
                into wl_1btxic
                 where land1    = 'BR'
                   and shipfrom  = wg_shipfrom
                   and shipto    = wg_shipto
                   and gruop    = '30'
                   and value    = p_parid
                   and value2    = wl_itens-matnr.

              if sy-subrc is not initial.
                select single rate base
                  from j_1btxic3
                  into wl_1btxic
                   where land1    = 'BR'
                     and shipfrom  = wg_shipfrom
                     and shipto    = wg_shipto
                     and gruop    = '40'
                     and value    = p_parid.

                if sy-subrc is not initial.
                  if p_parvw ne 'BR'
                  and p_parvw ne 'AG'.
                    select single rate base
                      from j_1btxic2
                      into wl_1btxic
                       where land1    = 'BR'
                         and shipfrom  = wg_shipfrom
                         and shipto    = wg_shipto
                         and matnr    = wl_itens-matnr.
                  endif.
                  if sy-subrc is not initial.
                    select single rate
                      from j_1btxic1
                      into wl_1btxic
                       where land1    = 'BR'
                         and shipfrom  = wg_shipfrom
                         and shipto    = wg_shipto.

                  endif.
                endif.
              endif.

            else.
              select single rate base
                from j_1btxic3
                into wl_1btxic
                 where land1    = 'BR'
                   and shipfrom  = wg_shipfrom
                   and shipto    = wg_shipto
                   and gruop    = '76'
                   and value    = p_parid
                   and value2    = wl_itens-matnr.

              if sy-subrc is not initial.
                if p_parvw ne 'BR'
                and p_parvw ne 'AG'.
                  select single rate base
                    from j_1btxic2
                    into wl_1btxic
                     where land1    = 'BR'
                       and shipfrom  = wg_shipfrom
                       and shipto    = wg_shipto
                       and matnr    = wl_itens-matnr.
                endif.
                if sy-subrc is not initial.
                  select single rate
                    from j_1btxic1
                    into wl_1btxic
                     where land1    = 'BR'
                       and shipfrom = wg_shipfrom
                       and shipto   = wg_shipto.
                endif.
              endif.
            endif.
            move-corresponding: tg_impo to tl_impo.
            select single *
              from t001w
              into wl_t001w
               where werks eq wl_itens-werks.
            if sy-subrc is initial.
              if ( wl_1baa-direct ne '1' ).

                select single *
                  from a924
                  into wl_a924
                   where kschl    eq 'ZIVP'
                     and aland    eq 'BR'
                     and txreg_sf eq wl_t001w-regio
                     and matnr    eq wl_itens-matnr
                     and datab    le sy-datum
                     and datbi    ge sy-datum.

                if sy-subrc is initial.
                  select single *
                    from konp
                    into wl_konp
                     where knumh eq wl_a924-knumh.

                endif.
              endif.
            endif.
            if wl_1btxic-base is initial.
              if wl_konp-kbetr gt wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              endif.
              tl_impo-base   = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            else.
              if wl_konp-kbetr gt wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              endif.
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.

            endif.
            tl_impo-rate = wl_1btxic-rate.
            if wl_0001-complemento eq 'S'.
              clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            endif.
            append tl_impo.
            clear: tl_impo.
          elseif tl_0006-opertyp eq 'I'.
            move-corresponding: tg_impo to tl_impo.
            move: wl_itens-netwr to tl_impo-excbas.
            if wl_0001-complemento eq 'S'.
              clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            endif.
            append tl_impo.
            clear: tl_impo.
          elseif tl_0006-opertyp eq 'N'.
            move-corresponding: tg_impo to tl_impo.
            move: wl_itens-netwr to tl_impo-othbas.
            if wl_0001-complemento eq 'S'.
              clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            endif.
            append tl_impo.
            clear: tl_impo.
          endif.
        elseif wl_1btxsdc-pis eq 'X'
           and tg_impo-taxtyp eq 'IPIS'.

          select single *
            from j_1btxpis
            into wl_1btxpis
             where country eq 'BR'
               and gruop   eq '72'
               and value   eq wl_itens-werks.

          move-corresponding: tg_impo to tl_impo.
          if sy-subrc is initial.
            tl_impo-base   = wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          else.
            move: wl_itens-netwr to tl_impo-othbas.
          endif.
          if wl_0001-complemento eq 'S'.
            clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          endif.
          append tl_impo.
          clear: tl_impo, wl_1btxpis.

        elseif wl_1btxsdc-cofins eq 'X'
           and tg_impo-taxtyp eq 'ICOF'.
          select single *
            from j_1btxcof
            into wl_1btxcof
             where country eq 'BR'
               and gruop   eq '71'
               and value   eq wl_itens-werks.

          move-corresponding: tg_impo to tl_impo.
          if sy-subrc is initial.
            tl_impo-base   = wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            if  tl_impo-base > 0 and wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            endif.
            tl_impo-othbas = 0.
          else.
            move: wl_itens-netwr to tl_impo-othbas.
          endif.

          if wl_0001-complemento eq 'S'.
            clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          endif.
          append tl_impo.
          clear: tl_impo, wl_1btxcof.

        elseif  tg_impo-taxtyp eq 'ICS1'.
          select single *
           from j_1baa
           into wl_1baa
            where itmtyp eq wl_0001-itmtyp.

          if wl_1baa-entrad eq 'X'.
            select single rate base
              from j_1btxic3
              into wl_1btxic
               where land1    = 'BR'
                 and shipfrom  = wg_shipfrom
                 and shipto    = wg_shipto
                 and gruop    = '30'
                 and value    = p_parid
                 and value2    = wl_itens-matnr.

            if sy-subrc is not initial.
              select single rate base
                from j_1btxic3
                into wl_1btxic
                 where land1    = 'BR'
                   and shipfrom  = wg_shipfrom
                   and shipto    = wg_shipto
                   and gruop    = '40'
                   and value    = p_parid.

              if sy-subrc is not initial.
                select single rate
                  from j_1btxic1
                  into wl_1btxic
                   where land1    = 'BR'
                     and shipfrom  = wg_shipfrom
                     and shipto    = wg_shipto.

              endif.

            endif.

          else.
            select single rate base
              from j_1btxic3
              into wl_1btxic
               where land1    = 'BR'
                 and shipfrom = wg_shipfrom
                 and shipto   = wg_shipto
                 and gruop    = '76'
                 and value    = p_parid
                 and value2   = wl_itens-matnr.

            if sy-subrc is not initial.
              select single rate
                from j_1btxic1
                into wl_1btxic
                 where land1    = 'BR'
                   and shipfrom = wg_shipfrom
                   and shipto   = wg_shipto.
            endif.

          endif.
          move-corresponding: tg_impo to tl_impo.

          tl_impo-rate =  wl_1btxic-rate .
          if wl_1btxic-base > 0 and  wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          endif.
          if tl_impo-base > 0 and  tl_impo-rate > 0.
            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
          endif.

          if wl_0001-complemento eq 'S'.
            clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          endif.

          append tl_impo.
          clear: tl_impo, wl_1btxic.
        else.

**        Aqui outros impostos
          move-corresponding: tg_impo to tl_impo.
          move: wl_itens-netwr to tl_impo-othbas.

          if wl_0001-complemento eq 'S'.
            clear: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          endif.

          append tl_impo.
          clear: tl_impo.
        endif.
      endif.
    endloop.

  endif.

endform.                    " F_MONTA_IMPOSTOS

form f_monta_contabil .
  data: tl_impo_aux like table of tg_impo with header line,
        tl_impo     like table of tg_impo with header line,
        tg_tbsl     type table of tbsl with header line,
        wl_tabix    type sy-tabix.

  refresh: tl_impo, tl_impo_aux.
  clear: tl_impo, tl_impo_aux.

  loop at tg_contab.
    move: 0 to tg_contab-dmbtr.
    modify tg_contab.
  endloop.

  tg_tbsl[] = tl_tbsl[].
  loop at tg_itens.
    perform f_monta_impostos tables tl_impo_aux
                              using sy-tabix.

    loop at tl_impo_aux.
      move-corresponding tl_impo_aux to tl_impo.
      collect tl_impo.
    endloop.
    refresh: tl_impo_aux.
  endloop.
  loop at tg_contab.
    wl_tabix = sy-tabix.
    read table tg_tbsl
      with key bschl = tg_contab-bschl.

    if tg_contab-taxtyp is initial.
      if wl_0001-complemento = 'S'.
        loop at tl_impo
          where taxtyp eq 'ICM3'.
          if tg_tbsl-shkzg eq 'H'.
            subtract  tl_impo-taxval from tg_contab-dmbtr.
          else.
            add tl_impo-taxval to tg_contab-dmbtr.
          endif.
        endloop.
      endif.
      modify tg_contab index wl_tabix.
      if wl_0001-energia eq 'N'.
        loop at tg_itens.
          if tg_tbsl-shkzg eq 'H'.
            subtract  tg_itens-netwr from tg_contab-dmbtr.
          else.
            add tg_itens-netwr to tg_contab-dmbtr.
          endif.
        endloop.
      elseif wl_0001-energia eq 'S'.
        loop at tl_impo
          where taxtyp eq 'ICS1'.
          if tg_tbsl-shkzg eq 'H'.
            subtract  tl_impo-base from tg_contab-dmbtr.
          else.
            add tl_impo-base to tg_contab-dmbtr.
          endif.
        endloop.
      endif.
      modify tg_contab index wl_tabix.
    else.
      read table tl_impo
        with key taxtyp = tg_contab-taxtyp.
      if sy-subrc is initial.
        if tg_tbsl-shkzg eq 'H'.
          move: tl_impo-taxval to tg_contab-dmbtr.
          multiply tg_contab-dmbtr by -1.
        else.
          move: tl_impo-taxval to tg_contab-dmbtr.
        endif.
        modify tg_contab index wl_tabix.
      endif.

    endif.

    clear: wl_tabix, tl_impo, tg_tbsl.
  endloop.
  sort tg_contab by taxtyp bschl.
endform.                    " MONTA_CONTABIL

form f_nota_remessa using p_saida type ty_saida
                 changing p_seq_lcto type zfiwrt0008-seq_lcto.


  data: wl_input_0008 type zfiwrt0008,
        tl_input_0009 type table of zfiwrt0009 with header line,
        tl_input_0010 type table of zfiwrt0010 with header line,
        tl_input_0011 type table of zfiwrt0011 with header line,
        tl_input_0012 type table of zfiwrt0012 with header line,
        tl_input_0013 type table of zfiwrt0013 with header line,
        tl_input_0015 type table of zfiwrt0015 with header line,
        tl_input_0019 type table of zfiwrt0019 with header line,
        tl_impo_aux   like table of tg_impo with header line,
        wl_0008_aux   type zfiwrt0008,
        wl_mara       type mara,
        wl_marc       type marc,
        wl_cont       type sy-tabix,
        wl_lin        type sy-tabix,
        int_len       type i,
        vg_anzpk(16).

  clear: wl_0019,
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

  refresh:  tl_0002,
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


  select single *
    from zsdt0001 into @data(lwa_zsdt0001)
   where ch_referencia = @p_saida-ch_referencia.

  check sy-subrc eq 0.

  "
  if p_saida-vbeln is initial.
    select single operacao lgort_dest lgort_orig
       into ( p_operacao, p_lgort_d, p_lgort_o )
      from zlest0117
      where bukrs = p_saida-bukrs
      and   werks = p_saida-branch
      and   lifnr_c =  wa_saida-ponto_coleta
      and   tipo  = 'C'.
  else.
    select single operacao lgort_dest lgort_orig
       into ( p_operacao, p_lgort_d, p_lgort_o )
      from zlest0117
      where bukrs = p_saida-bukrs
      and   werks = p_saida-branch
      and   lifnr_c =  wa_saida-ponto_coleta
      and   tipo  = 'R'.
  endif.

  if sy-subrc ne 0.
    message 'Não existe Operação Nota Writer Cadastrado!' type 'I'.
    exit.
  endif.

  "Lê parametros operacao
  select single *
  from zfiwrt0001
  into wl_0001
   where operacao eq p_operacao.

  if sy-subrc is not initial.
    message 'Não existe Operação Nota Writer Cadastrado!' type 'I'.
    exit.
  endif.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_saida-branch
    importing
      output = p_parid.

  p_parvw   = wl_0001-parvw.

  select single *
        from t001w
        into wl_t001w
         where werks eq p_saida-branch.
  "
  select single *
    from j_1baa
    into wl_1baa
     where nftype eq wl_0001-nftype.

  if wl_0001-parvw eq 'AG'.
    select single *
      from kna1
      into wl_kna1
       where kunnr eq p_parid.

  elseif wl_0001-parvw eq 'BR'
    or   wl_0001-parvw eq 'LF'.
    select single *
      from lfa1
      into wl_lfa1
       where lifnr eq p_parid.

  endif.
  select *
    from zfiwrt0002
    into table tl_0002
     where operacao eq p_operacao.

  if sy-subrc is initial.
    select *
      from j_1baj
      into table tl_1baj
       for all entries in tl_0002
       where taxtyp eq tl_0002-taxtyp.

    select *
      from j_1bajt
      into table tl_1bajt
       for all entries in tl_0002
      where  spras  eq sy-langu
        and  taxtyp eq tl_0002-taxtyp.
  endif.

  select *
    from zfiwrt0003
    into table tl_0003
     where operacao eq p_operacao.

  if sy-subrc is initial.
    select *
      from tbsl
      into table tl_tbsl
       for all entries in tl_0003
       where bschl eq tl_0003-bschl.

    select *
      from skat
      into table tl_skat
       for all entries in tl_0003
        where spras eq sy-langu
          and ktopl eq '0050'
          and saknr eq tl_0003-hkont.


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
  endif.
  select *
  from zfiwrt0004
  into table tl_0004
   where operacao eq p_operacao.


  refresh: tg_movest.
  loop at tl_0004.
    move: tl_0004-bwart   to tg_movest-bwart,
          tl_0004-tcode   to tg_movest-tcode,
          tl_0004-mwskz1  to tg_movest-mwskz1,
          tl_0004-estorno to tg_movest-estorno.

    append tg_movest.
    clear: tg_movest.
  endloop.

  select *
    from zfiwrt0005
    into table tl_0005
     where operacao eq p_operacao.

  select *
    from zfiwrt0006
    into table tl_0006
     where operacao eq p_operacao.

  select *
     from zfiwrt0007
     into table tl_0007
      where operacao eq p_operacao
        and branch   eq p_saida-branch
        and tipo     eq 'W'.

  if tl_0007[] is not initial.
    select *
      from user_addr
      into table tl_user
       for all entries in tl_0007
        where bname eq tl_0007-usnam.

  endif.

  select single *
    from zfiwrt0019
    into wl_0019
  where seq_lcto eq p_seq_lcto.

  clear: wl_cont, wl_lin.
  loop at tl_0005.
    tg_mensagems-seqnum  = tl_0005-seqnum.
    tg_mensagems-linnum  = tl_0005-linnum.
    tg_mensagems-message = tl_0005-message.
    append tg_mensagems.         " msgs q foram parametrizadas na operacao
    add 1 to wl_cont.
  endloop.

  "texto
  call function 'CATSXT_SIMPLE_TEXT_EDITOR'
    exporting
      im_title = 'Texto da Nota Remessa'
    changing
      ch_text  = tl_texto.

  if wl_cont is initial.
    add 1 to wl_cont.
  endif.

  loop at tl_texto into wl_texto.
    tg_mensagems-seqnum  = wl_cont.
    tg_mensagems-linnum  = '01'.
    tg_mensagems-message = wl_texto.
    append tg_mensagems.         " msgs q foram inseridas manualmente
    add 1 to wl_cont.
  endloop.


  if p_parvw eq 'AG'.
    if wl_kna1-regio eq wl_t001w-regio.
      wl_indcoper = 'D'.
    else.
      wl_indcoper = 'F'.
    endif.
    if wl_1baa-direct eq 1.
      move: wl_kna1-regio to wg_shipfrom.
    else.
      move: wl_kna1-regio to  wg_shipto.
    endif.
  elseif p_parvw eq 'BR'
     or  p_parvw eq 'LF'.
    if wl_lfa1-regio eq wl_t001w-regio.
      wl_indcoper = 'D'.
    else.
      wl_indcoper = 'F'.
    endif.
    if wl_1baa-direct eq 1.
      move: wl_lfa1-regio to wg_shipfrom.
    else.
      move: wl_lfa1-regio to wg_shipto.
    endif.
  endif.

  if wl_1baa-direct eq 1.
    move: wl_t001w-regio to wg_shipto.
  else.
    move: wl_t001w-regio to wg_shipfrom.
  endif.

  perform get_next_number in program zwrr0001 using  'ZSEQ_LCTO'
                                                     '1'
                                            changing p_seq_lcto.

  wl_input_0008-seq_lcto = p_seq_lcto.

  move:  sy-uname to wl_input_0008-usnam,
         sy-datum to wl_input_0008-dt_criacao,
         sy-uzeit to wl_input_0008-hr_criacao.

*  DELETE FROM zfiwrt0008 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
** ZFIWRT0008

  "CS2017002682 - 29.11.2017 - Ini
  data(_matnr) = p_saida-matnr.

  if ( p_saida-tipo = 'O' ) and ( p_saida-ematn is not initial ).
    _matnr = p_saida-ematn.
  endif.
  "CS2017002682 - 29.11.2017 - Fim

  select single *
          from mara
          into wl_mara
            where matnr eq _matnr. "CS2017002682 - 29.11.2017

  read table tl_0006
    with key indcoper = wl_indcoper.


  vg_anzpk = p_saida-peso_liq.
  replace all occurrences of '.' in vg_anzpk with ' '.
  replace all occurrences of ',' in vg_anzpk with ' '.
  condense vg_anzpk no-gaps.
  int_len = strlen( vg_anzpk ).
  if int_len gt 3.
    int_len = int_len - 3.
  endif.
  vg_anzpk = vg_anzpk+0(int_len).

  move : sy-mandt             to wl_input_0008-mandt,
         p_operacao           to wl_input_0008-operacao,
         p_saida-bukrs       to wl_input_0008-bukrs,
         p_saida-branch      to wl_input_0008-branch,
         p_parvw              to wl_input_0008-parvw,
         p_parid              to wl_input_0008-parid,
         wl_0001-nftype       to wl_input_0008-nftype,
*         P_SAIDA-BRANCH      TO WL_INPUT_0008-MOVE_PLANT,
         p_lgort_d            to wl_input_0008-move_stloc,
         wl_0001-ctrl_zrfl    to wl_input_0008-ctrl_zrfl,
         p_saida-nr_romaneio   to wl_input_0008-nr_romaneio,
         p_saida-ch_referencia to wl_input_0008-ch_referencia,
         wl_0001-zpesagem     to wl_input_0008-zpesagem,
         wl_0001-dias         to wl_input_0008-dias,
         wl_0001-retorno      to wl_input_0008-retorno,
         wl_0001-energia      to wl_input_0008-energia,
         wl_0001-servico      to wl_input_0008-servico,
         wl_0001-complemento  to wl_input_0008-complemento,
         p_saida-inco1       to wl_input_0008-inco1,
         p_saida-inco1       to wl_input_0008-inco2,
         p_saida-ebeln       to wl_input_0008-ebeln,
         p_saida-ebelp       to wl_input_0008-ebelp, "CS2017002682 - 29.11.2017
         wl_0001-referencia   to wl_input_0008-referencia,
         tl_0006-cfop         to wl_input_0008-cfop,
         tl_0006-taxlw1       to wl_input_0008-taxlw1,
         tl_0006-taxlw2       to wl_input_0008-taxlw2,
         tl_0006-taxlw4       to wl_input_0008-taxlw4,
         tl_0006-taxlw5       to wl_input_0008-taxlw5,
         tl_0006-opertyp      to wl_input_0008-opertyp,
         tl_0006-taxcode      to wl_input_0008-taxcode,
         sy-uname             to wl_input_0008-usuario_ult_mod,
         sy-datum             to wl_input_0008-dt_ult_mod,
         sy-uzeit             to wl_input_0008-hr_ult_mod,
         sy-datum             to wl_input_0008-budat,
         sy-datum             to wl_input_0008-bldat,
         wl_input_0008-seq_lcto to tl_input_0019-seq_lcto,

         p_saida-lifnr       to tl_input_0019-lifnr,
         p_saida-placa_cav   to tl_input_0019-placa,
         p_saida-placa_car1   to tl_input_0019-placa_car1,
         p_saida-placa_car2   to tl_input_0019-placa_car2,
         p_saida-placa_car3   to tl_input_0019-placa_car3,
         p_saida-motorista    to tl_input_0019-motorista,

         vg_anzpk             to tl_input_0019-anzpk,
         wl_mara-meins        to tl_input_0019-shpunt,
         p_saida-peso_liq    to tl_input_0019-ntgew,
         p_saida-peso_fiscal to tl_input_0019-brgew,
         p_saida-region      to tl_input_0019-ufplaca.

  if lwa_zsdt0001-fat_contingencia_ecc eq abap_true.
    data: lwa_faturamento_ecc type zde_compare_faturamento.

    call function 'ZLES_FAT_CONTINGENCIA_0002'
      exporting
        i_ch_referencia         = lwa_zsdt0001-ch_referencia
        i_get_dados_fat_ecc     = abap_true
      importing
        e_dados_faturamento_ecc = lwa_faturamento_ecc.

    if lwa_faturamento_ecc-data_lcto_nf_rem is initial.
      message 'Data Lacto NF-e não encontrado no ECC'  type 'E'.
      return.
    endif.

    wl_input_0008-budat = lwa_faturamento_ecc-data_lcto_nf_rem.
    wl_input_0008-bldat = lwa_faturamento_ecc-data_lcto_nf_rem.
  endif.


** ZFIWRT0009
  delete from zfiwrt0009 where seq_lcto eq wl_input_0008-seq_lcto.
  delete from zfiwrt0010 where seq_lcto eq wl_input_0008-seq_lcto.
  " Gravar 1 item

  select single *
      from marc
      into wl_marc
       where matnr eq _matnr. "CS2017002682 - 29.11.2017 - Fim

  clear tg_itens.
  refresh tg_itens.
  tg_itens-itmnum = 10.
  tg_itens-matnr  = p_saida-matnr.
  tg_itens-maktx  = p_saida-material.
  tg_itens-cfop   = tl_0006-cfop.
  if wl_mara-xchpf = 'X'.
    tg_itens-charg  = p_saida-nr_safra.
  endif.
  tg_itens-werks  = p_saida-branch.
  tg_itens-lgort  = p_lgort_o.

  "CS2017002682 - 29.11.2017 - Ini
  if ( p_saida-tipo = 'O' ) and ( p_saida-ematn is not initial ).
    tg_itens-matnr  = p_saida-ematn.

    if wl_mara-xchpf = 'X'..
      tg_itens-charg  = p_saida-charg_n.
    endif.
    tg_itens-lgort  = p_saida-lgort_n.
  endif.
  "CS2017002682 - 29.11.2017 - Fim

  tg_itens-menge  = p_saida-peso_liq.
  tg_itens-meins  = wl_mara-meins.
  tg_itens-netpr  = p_saida-netpr.
  tg_itens-netwr  = p_saida-netpr * p_saida-peso_liq.
  tg_itens-steuc =  wl_marc-steuc.
  append tg_itens.
  loop at tg_itens.
    move:sy-mandt               to tl_input_0009-mandt,
         wl_input_0008-seq_lcto to tl_input_0009-seq_lcto,
         tg_itens-itmnum        to tl_input_0009-itmnum,
         tg_itens-matnr         to tl_input_0009-matnr,
         tg_itens-cfop          to tl_input_0009-cfop,
         tg_itens-charg         to tl_input_0009-charg,
         tg_itens-menge         to tl_input_0009-menge,
         tg_itens-meins         to tl_input_0009-meins,
         tg_itens-netpr         to tl_input_0009-netpr,
         tg_itens-netwr         to tl_input_0009-netwr,
         wl_0001-itmtyp         to tl_input_0009-itmtyp,
         tg_itens-werks         to tl_input_0009-bwkey,
         tg_itens-lgort         to tl_input_0009-lgort,
         tg_itens-anln1         to tl_input_0009-anln1,
         tg_itens-anln2         to tl_input_0009-anln2.

    append tl_input_0009.
***    ZFIWRT0010
    read table tl_0006
     with key indcoper = wl_indcoper.

    refresh: tg_impo.
    loop at tl_0002.
      read table tl_1baj
        with key taxtyp = tl_0002-taxtyp.

      read table tl_1bajt
        with key taxtyp = tl_0002-taxtyp.

      move: tl_0002-taxtyp   to tg_impo-taxtyp,
            tl_1bajt-ttypetxt to tg_impo-ttypetxt,
            tl_1baj-taxgrp  to tg_impo-taxgrp.

      if tl_0002-taxtyp eq 'ICM3'.
        if tl_0006-opertyp eq 'T'.

        elseif tl_0006-opertyp eq 'I'.

        elseif tl_0006-opertyp eq 'N'.

        endif.
      else.

      endif.

      append tg_impo.
      clear: tg_impo.
    endloop.

    perform f_monta_impostos tables tl_impo_aux
                              using sy-tabix.
    loop at tl_impo_aux.
      move: sy-mandt               to tl_input_0010-mandt,
            wl_input_0008-seq_lcto to tl_input_0010-seq_lcto,
            tl_input_0009-itmnum   to tl_input_0010-itmnum,
            tl_impo_aux-taxtyp     to tl_input_0010-taxtyp,
            tl_impo_aux-base       to tl_input_0010-base,
            tl_impo_aux-rate       to tl_input_0010-rate,
            tl_impo_aux-taxval     to tl_input_0010-taxval,
            tl_impo_aux-excbas     to tl_input_0010-excbas,
            tl_impo_aux-othbas     to tl_input_0010-othbas.
      append tl_input_0010.
    endloop.

    clear: tl_input_0009.
  endloop.
** ZFIWRT0011
  delete from zfiwrt0011 where seq_lcto eq wl_input_0008-seq_lcto.
  perform f_monta_contabil.
  loop at tg_contab.
    move: sy-mandt                to tl_input_0011-mandt,
          wl_input_0008-seq_lcto  to tl_input_0011-seq_lcto,
          tg_contab-bschl         to tl_input_0011-bschl,
          tg_contab-hkont         to tl_input_0011-hkont,
          tg_contab-taxtyp        to tl_input_0011-taxtyp,
          tg_contab-dmbtr         to tl_input_0011-dmbtr,
          tg_contab-estorno       to tl_input_0011-estorno,
          tg_contab-zlsch         to tl_input_0011-zlsch,
          tg_contab-zfbdt         to tl_input_0011-zfbdt,
          tg_contab-kostl         to tl_input_0011-kostl,
          tg_contab-umskz         to tl_input_0011-umskz.

    tl_input_0011-buzei  = sy-tabix.

    append tl_input_0011.
    clear: tl_input_0011.
  endloop.

** ZFIWRT0012
  delete from zfiwrt0012 where seq_lcto eq wl_input_0008-seq_lcto.
  loop at tg_movest.
    move: sy-mandt                to  tl_input_0012-mandt,
          wl_input_0008-seq_lcto  to  tl_input_0012-seq_lcto,
          tg_movest-bwart         to  tl_input_0012-bwart,
          tg_movest-tcode         to  tl_input_0012-tcode,
          tg_movest-mwskz1        to  tl_input_0012-mwskz1,
          tg_movest-estorno       to  tl_input_0012-estorno.

    append tl_input_0012.
    clear: tl_input_0012.
  endloop.

** ZFIWRT0013
  delete from zfiwrt0013 where seq_lcto eq wl_input_0008-seq_lcto.
  loop at tg_mensagems.
    move: sy-mandt                 to tl_input_0013-mandt,
          wl_input_0008-seq_lcto   to tl_input_0013-seq_lcto,
          tg_mensagems-seqnum  to tl_input_0013-seqnum,
          tg_mensagems-linnum  to tl_input_0013-linnum,
          tg_mensagems-message to tl_input_0013-message.

    append tl_input_0013.
    clear: tl_input_0013.
  endloop.

** ZFIWRT0015
  refresh tg_parc.
  tg_parc-parvw = p_parvw.
  tg_parc-parid = p_parid.
  append tg_parc.

  "Adicionar Parceiro WL
  select single *
    from ekpo into @data(wa_ekpo)
   where ebeln eq @p_saida-ebeln.

  if ( sy-subrc eq 0 ) and ( p_saida-ebeln is not initial ).
    clear: tg_parc.
    tg_parc-parvw = 'WL'.
    tg_parc-parid = |{ wa_ekpo-werks alpha = in }|.
    append tg_parc.
  endif.

  if ( p_saida-parid_rom is not initial ).
    clear: tg_parc.
    tg_parc-parvw = 'PC'.
    tg_parc-parid = |{ p_saida-parid_rom alpha = in }|.
    append tg_parc.
  endif.

  if ( p_saida-id_cli_dest_rom is not initial ).
    clear: tg_parc.
    tg_parc-parvw = 'LR'.
    tg_parc-parid = |{ p_saida-id_cli_dest_rom alpha = in }|.
    append tg_parc.
  endif.

  delete from zfiwrt0015 where seq_lcto eq wl_input_0008-seq_lcto.
  loop at tg_parc.
    move: sy-mandt                 to tl_input_0015-mandt,
          wl_input_0008-seq_lcto   to tl_input_0015-seq_lcto,
          tg_parc-parvw            to tl_input_0015-parvw,
          tg_parc-parid            to tl_input_0015-parid.

    append tl_input_0015.
    clear: tl_input_0015.
  endloop.

  modify zfiwrt0008 from wl_input_0008.
  modify zfiwrt0009 from table tl_input_0009.
  modify zfiwrt0010 from table tl_input_0010.
  modify zfiwrt0011 from table tl_input_0011.
  modify zfiwrt0012 from table tl_input_0012.
  modify zfiwrt0013 from table tl_input_0013.
  modify zfiwrt0015 from table tl_input_0015.
  modify zfiwrt0019 from tl_input_0019.

  message s836(sd) with 'Lançamento'
                         wl_input_0008-seq_lcto
                         ', criado com sucesso!'.
endform.

form f_preencher_dynpro using l_start type c l_name type c l_value.

  move l_start to wl_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to wl_bdc-program,
  l_value to wl_bdc-dynpro.
  else.
    move:
      l_name  to wl_bdc-fnam,
      l_value to wl_bdc-fval.
  endif.
  append wl_bdc to tl_bdc.
  clear: wl_bdc.

endform.                    " f_preencher_dynpro

form f_get_next_number  using  p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      changing p_number.

  clear p_number.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = p_nr_range
      object                  = p_object
    importing
      number                  = p_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc ne 0.
    clear: p_number.
    message e836(sd) with 'O intervalo de numeração,'
                      'não foi encontrado!'.
  else.
    wg_flag = c_x.
  endif.

endform.                    " get_next_number

form f_action_user_danfe_znfw using p_saida type ty_saida.

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  check ( <fs_out>-danfez ne icon_icon_list ).

  if ( <fs_out>-danfez(1) eq '@' ).

    clear wa_zsdt0001.

    select single *
      from zsdt0001 into wa_zsdt0001
     where ch_referencia = <fs_out>-ch_referencia.

    if wa_zsdt0001-nro_nf_rem gt 0.
      message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
      exit.
    endif.

    if wa_zsdt0001-fat_contingencia_ecc eq abap_true.

      data: lva_ok          type  char01,
            lva_msg_retorno type  string.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia       = p_saida-ch_referencia
          i_check_danfe_znfw_ok = abap_true
        importing
          e_ok                  = lva_ok
          e_msg_retorno         = lva_msg_retorno.

      if lva_ok = abap_false.
        message lva_msg_retorno type 'I'.
        return.
      endif.

    endif.

    if sy-tcode ne 'ZLES0136'
      and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      and sy-tcode ne 'ZMM0127'.
      message 'Transação apenas de visualização' type 'I'.
      exit.
    endif.

    if <fs_out>-seq_lcto = icon_execute_object.
      message 'Gerar a documento NFW!' type 'I'.
      exit.
    endif.

    refresh: tl_bdc.
    perform f_preencher_dynpro using:
            'X' 'ZWRR0004'              '0100',
            ' ' 'P_SEQ_LCTO'            <fs_out>-seq_lcto,
            ' ' 'BDC_OKCODE'            'SEARCH'.

    opt-dismode = 'E'.
    opt-defsize = ' '.
    opt-racommit = 'X'.

    call transaction 'ZNFW0005' using tl_bdc options from opt.

    select single *
      from zfiwrt0008 into wa_zfiwrt0008
     where seq_lcto = <fs_out>-seq_lcto.

    check ( sy-subrc = 0 ) and ( wa_zfiwrt0008-docnum is not initial ).

    perform f_check_auth_doc using wa_zfiwrt0008-docnum.

    if sy-subrc = 0.
      wa_zsdt0001-nro_nf_rem = wa_zfiwrt0008-docnum.
      <fs_out>-danfez        = wa_zfiwrt0008-docnum.
      update zsdt0001 set st_proc    = '12'
                          nro_nf_rem = wa_zfiwrt0008-docnum
      where ch_referencia = <fs_out>-ch_referencia.
      <fs_out>-st_proc = '12'.

      perform f_refresh_alv using '0100'. "Refresh na tela
    else.

      perform f_check_canc_doc using wa_zfiwrt0008-docnum.

      if sy-subrc = 0.
        wa_zsdt0001-nro_nf_rem = ''.
        <fs_out>-danfez        = icon_execute_object.
        update zsdt0001 set st_proc    = '11'
                            nro_nf_rem = ''
        where ch_referencia = <fs_out>-ch_referencia.
        <fs_out>-st_proc = '11'.

        if wa_zfiwrt0008-docs_estornados eq abap_true.
          wa_zsdt0001-seq_lcto = ''.
          <fs_out>-seq_lcto        = icon_execute_object.
          update zsdt0001 set st_proc    = ''
                              agente_frete = ''
                              seq_lcto   = ''
          where ch_referencia = <fs_out>-ch_referencia.
          <fs_out>-st_proc = ''.
          <fs_out>-lifnr = ''.
          <fs_out>-netpr  =  0.
        endif.

        refresh style.
        clear: wa_style.
        if <fs_out>-netpr is not initial.
          wa_style-fieldname = 'NETPR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        clear: wa_style.
        if <fs_out>-region is not initial.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        clear: wa_style.
        if <fs_out>-lifnr is not initial.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        if <fs_out>-ebeln is not initial.
          wa_style-fieldname = 'EBELN'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          "CS2017002682 - 29.11.2017 - Ini
          wa_style-fieldname = 'EBELP'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
          "CS2017002682 - 29.11.2017 - Fim
        endif.
        <fs_out>-style[] = style[].

        perform f_repare_docs_romaneio changing <fs_out>.
        perform f_refresh_alv using '0100'. "Refresh na tela
      else.
        message 'Danfe ainda não autorizado pela SEFAZ' type 'I'.
      endif.


    endif.

  else.

    if <fs_out>-st_proc = '12'. "estorno possivel aqui

      refresh: tl_bdc.
      perform f_preencher_dynpro using:
              'X' 'ZWRR0004'              '0100',
              ' ' 'P_SEQ_LCTO'            <fs_out>-seq_lcto,
              ' ' 'BDC_OKCODE'            'SEARCH'.

      opt-dismode = 'E'.
      opt-defsize = ' '.
      opt-racommit = 'X'.

      call transaction 'ZNFW0005' using tl_bdc options from opt.

      select single *
        from zfiwrt0008 into wa_zfiwrt0008
       where seq_lcto = <fs_out>-seq_lcto.

      check sy-subrc = 0 and wa_zfiwrt0008-docnum is not initial.

      perform f_check_canc_doc using wa_zfiwrt0008-docnum.

      if sy-subrc = 0.
        wa_zsdt0001-nro_nf_rem = ''.
        <fs_out>-danfez        = icon_execute_object.

        update zsdt0001 set st_proc    = '11'
                            nro_nf_rem = ''
        where ch_referencia = <fs_out>-ch_referencia.
        <fs_out>-st_proc = '11'.

        if wa_zfiwrt0008-docs_estornados eq abap_true.
          wa_zsdt0001-seq_lcto = ''.
          <fs_out>-seq_lcto        = icon_execute_object.
          update zsdt0001 set st_proc    = ''
                              agente_frete = ''
                              seq_lcto   = ''
          where ch_referencia = <fs_out>-ch_referencia.
          <fs_out>-st_proc = ''.
          <fs_out>-lifnr   =  ''.
        endif.

        refresh style.
        clear: wa_style.
        if <fs_out>-netpr is not initial.
          wa_style-fieldname = 'NETPR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        clear: wa_style.
        if <fs_out>-region is not initial.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        clear: wa_style.
        if <fs_out>-lifnr is not initial.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.
        if <fs_out>-ebeln is not initial.
          wa_style-fieldname = 'EBELN'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .

          "CS2017002682 - 29.11.2017 - Ini
          wa_style-fieldname = 'EBELP'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
          "CS2017002682 - 29.11.2017 - Fim
        endif.
        <fs_out>-style[] = style[].

        perform f_repare_docs_romaneio changing <fs_out>.
        perform f_refresh_alv using '0100'. "Refresh na tela
      endif.


    else.
      set parameter id 'Z_MY_PARAMETER_1' field <fs_out>-danfez.
      set parameter id 'Z_MY_PARAMETER_2' field <fs_out>-bukrs.
      call transaction 'ZNFE' and skip first screen.
    endif.

  endif.


endform.

form f_action_user_aviso using p_saida type ty_saida.

  data: lva_docnum_znfw type j_1bdocnum.

  read table it_saida assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  if <fs_out>-aviso = icon_execute_object.

    select single *
      from likp into wa_likp
     where berot         = <fs_out>-ch_referencia
       and vbtyp         = '7'
       and spe_loekz     = ''.

    if sy-subrc eq 0.
      message 'Aviso já foi gerado! Click em <ATUALIZAR DOCUMENTOS>' type 'I'.
      exit.
    endif.

    clear wa_zsdt0001.
    select single *
      from zsdt0001
      into wa_zsdt0001
       where ch_referencia = <fs_out>-ch_referencia.
    if wa_zsdt0001-doc_aviso gt 0.
      message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
      exit.
    endif.
    if sy-tcode ne 'ZLES0136'
      and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      and sy-tcode ne 'ZMM0127'.
      message 'Transação apenas de visualização' type 'I'.
      exit.
    endif.

    "Verifica se DANFE da ZNFW foi autorizada
    data(lva_danfe_aut) = abap_false.

    if ( <fs_out>-danfez    is not initial ) and
       ( <fs_out>-danfez(1) ne '@'         ).

      lva_docnum_znfw = <fs_out>-danfez.

      perform f_check_auth_doc using lva_docnum_znfw.

      if sy-subrc ne 0.
        lva_danfe_aut = abap_false.
      else.
        lva_danfe_aut = abap_true.
      endif.
    else.
      lva_danfe_aut = abap_false.
    endif.

    if lva_danfe_aut = abap_false.
      message 'DANFE NFW não autorizada!' type 'I'.
      exit.
    endif.

    perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    perform f_gerar_aviso using <fs_out>.

    if wg_documento is not initial.
      <fs_out>-aviso   = wg_documento.
      <fs_out>-st_proc = '13'.
      update zsdt0001 set st_proc      = '13' " Aviso
                          doc_aviso    = wg_documento
             where ch_referencia = <fs_out>-ch_referencia.

      perform f_refresh_alv using '0100'. "Refresh na tela
    endif.

    perform f_repare_docs_romaneio changing <fs_out>.
    perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

  elseif <fs_out>-aviso ne icon_icon_list.
    set parameter id 'VLM'    field <fs_out>-aviso.
    call transaction 'VL33N' and skip first screen.
  endif.


endform.

form f_gerar_aviso using p_saida type ty_saida.

  data: zcl_aviso_recebimento type ref to zcl_aviso_recebimento.

  data: vl_nf_serie  type string,
        vg_bolnr(16),
        vg_lfimg(16),
        wl_nfe       type c length 9,
        wl_serie     type c length 3,
        vl_lifnr     type ekko-lifnr,
        int_len      type i,
        vl_data(10),
        wl_mara      type mara,
        vl_docnum    type j_1bnfdoc-docnum,
        wl_j_1bnfdoc type j_1bnfdoc,
        i_item       type zde_bapi_remessa_item,
        i_parid	     type j_1bparid,
        i_xblnr	     type xblnr_v1,
        v_wl_parid   type j_1bparid,
        l_serie      type c length 3.

  free: zcl_aviso_recebimento.

  clear: wl_erro, wg_documento, vl_nf_serie, vg_bolnr, vg_lfimg, wl_nfe,
         wl_serie, vl_lifnr, int_len, vl_data, wl_mara, vl_docnum,wl_j_1bnfdoc,
         i_item, i_parid, i_xblnr, l_serie.

  if p_saida-ponto_coleta is initial or
     p_saida-local_entrega is initial or
     p_saida-lifnr is initial.
    message 'Parceiros(SP/PC/LR) para gerar Aviso de Recebimento, estão incomplemetos!' type 'I'.
    exit.
  endif.

  select single docnum
    from zfiwrt0008 into vl_docnum
   where seq_lcto = p_saida-seq_lcto.

  if sy-subrc ne 0.
    message 'Registro ZNFW não encontrado!' type 'I'.
    return.
  endif.

  select single *
    from zsdt0001 into @data(lwa_zsdt0001)
   where ch_referencia = @p_saida-ch_referencia.

  check sy-subrc eq 0.

  select single *
    from j_1bnfdoc into wl_j_1bnfdoc
   where docnum =  vl_docnum.

  if sy-subrc ne 0.
    message 'Documento ZNFW não encontrado!' type 'I'.
    return.
  endif.

  if vg_cockpit eq '04'.
    if ( p_saida-tipo = 'O' ) or
       ( p_saida-tipo = 'T' ).
      "Quando o romaneio for sobre um pedido de transferencia ou ordem de venda, será gerada um documento de remessa.
      "Nesse caso, o frete é emitido sobre a remessa da NF de Venda/Transferencia
      message 'Geração Aviso não permitido!' type 'I'.
      return.
    endif.
  endif.

  clear: wl_erro, wg_documento.

  if 1 = 1. "Gerar Aviso Recebimento por Objeto

    "Pedido de Compra
    select single *
      from ekko into @data(wa_ekko)
     where ebeln eq @p_saida-ebeln.

    if sy-subrc ne 0.
      wl_erro = 'X'.
      message 'Pedido não encontrado!' type 'S'.
      return.
    endif.

    "Pedido de Compra - Item
    if p_saida-tipo = 'O'.
      select single *
        from ekpo into @data(wa_ekpo)
       where ebeln eq @p_saida-ebeln
         and ebelp eq @p_saida-ebelp.
    else.
      select single *
        from ekpo into wa_ekpo
       where ebeln eq p_saida-ebeln
         and matnr eq p_saida-matnr.
    endif.

    if sy-subrc ne 0.
      wl_erro = 'X'.
      message 'Item do Pedido não encontrado!' type 'S'.
      return.
    endif.

    "Divisões de Remessas
    select single *
      from eket into @data(wa_eket)
     where ebeln eq @wa_ekpo-ebeln
       and ebelp eq @wa_ekpo-ebelp.

    "Material
    select single *
      from mara into @data(_wl_mara)
     where matnr = @wa_ekpo-matnr.

    if sy-subrc ne 0.
      wl_erro = 'X'.
      message 'Material do Pedido não encontrado!' type 'S'.
      return.
    endif.

    create object zcl_aviso_recebimento.

    zcl_aviso_recebimento->set_fornecedor( i_lifnr = wa_ekko-lifnr ).
    zcl_aviso_recebimento->set_pedido_compra( i_ebeln = p_saida-ebeln ).
    zcl_aviso_recebimento->set_route( i_route = p_saida-route ).

    if lwa_zsdt0001-fat_contingencia_ecc eq abap_true.

      data: lwa_faturamento_ecc type zde_compare_faturamento.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia         = lwa_zsdt0001-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        importing
          e_dados_faturamento_ecc = lwa_faturamento_ecc.

      if lwa_faturamento_ecc-data_lcto_nf_rem is initial.
        message 'Data Lacto NF-e não encontrado no ECC'  type 'E'.
        return.
      endif.

      zcl_aviso_recebimento->set_data_lancamento( i_bldat = conv #( lwa_faturamento_ecc-data_lcto_nf_rem ) ).

    else.

      zcl_aviso_recebimento->set_data_lancamento( i_bldat = sy-datum ).

    endif.





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

    if _wl_mara-xchpf = 'X'.
      i_item-batch  = wa_eket-charg.
      i_item-licha  = wa_eket-charg.
    endif.

    zcl_aviso_recebimento->set_item( i_item = i_item ).

    "Ponto Coleta
    zcl_aviso_recebimento->set_lc_coleta_parid( i_parid = p_saida-ponto_coleta ).
    zcl_aviso_recebimento->set_lc_coleta_partyp( i_partyp = 'V' ).

    "Agente Frete
    zcl_aviso_recebimento->set_sp_frete_parid( i_parid = p_saida-lifnr ).
    zcl_aviso_recebimento->set_sp_frete_partyp( i_partyp = 'V' ).

    "Local Entrega
    zcl_aviso_recebimento->set_lc_entrega_parid( i_parid = p_saida-local_entrega ).
    zcl_aviso_recebimento->set_lc_entrega_partyp( i_partyp = 'V' ).

    "Fornecedor Mercadoria
    v_wl_parid = |{ wa_ekpo-werks alpha = in }|.
    zcl_aviso_recebimento->set_wl_forn_merc_parid( i_parid = v_wl_parid ).
    zcl_aviso_recebimento->set_wl_forn_merc_partyp( i_partyp = 'V' ).

    data(_valor_nf) = wl_j_1bnfdoc-nftot.
    zcl_aviso_recebimento->set_valor_nota( i_valor_nota = _valor_nf ).

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_j_1bnfdoc-nfenum
      importing
        output = wl_nfe.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_j_1bnfdoc-series
      importing
        output = wl_serie.

    concatenate wl_nfe wl_serie into i_xblnr separated by '-'.

    zcl_aviso_recebimento->set_xblnr( i_xblnr = i_xblnr ).
    zcl_aviso_recebimento->set_ch_referencia( i_ch_referencia = p_saida-ch_referencia ).

    data(r_gerou) = zcl_aviso_recebimento->criar_aviso_recebimento( i_particao_lote = abap_true ).

    data(r_retorno) = zcl_aviso_recebimento->get_retorno( ).

    if r_gerou eq abap_true.
      wg_documento =  zcl_aviso_recebimento->get_nr_remessa( ).
    else.
      wl_erro = 'X'.
      perform f_prepare_return2 tables r_retorno.
      perform f_grava_log_erro tables tg_log_erro using p_saida.

      message 'Houve um erro ao gerar ao aviso.(Ver Logs)' type 'S'.
    endif.

    clear: zcl_aviso_recebimento.

  else. "Gerar Por SHDB

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_j_1bnfdoc-nfenum
      importing
        output = wl_nfe.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wl_j_1bnfdoc-series
      importing
        output = wl_serie.

    concatenate wl_nfe '-' wl_serie into vl_nf_serie.
    condense vl_nf_serie no-gaps.

    concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) into vl_data .

    vg_lfimg = p_saida-peso_liq.
    condense vg_lfimg no-gaps.
    replace all occurrences of '.' in vg_lfimg with ','.

    vg_bolnr = wl_j_1bnfdoc-nftot.
    condense vg_bolnr no-gaps.

    select single lifnr
      from ekko
      into vl_lifnr
     where ebeln = p_saida-ebeln.

    refresh ti_bdcdata.
    perform f_bdc_data using: 'SAPMV50A' '4007' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '/00',
                              ' '        ''     ' '          'LIKP-LIFNR'	        vl_lifnr,
                              ' '        ''     ' '          'LV50C-BSTNR'        p_saida-ebeln,
                              ' '        ''     ' '          'RV50A-LFDAT_LA'     vl_data,
                              ' '        ''     ' '          'RV50A-LFUHR_LA'     '00:00',
                              ' '        ''     ' '          'RV50A-VERUR_LA'     vl_nf_serie.

    perform f_bdc_data using: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\01'.

    perform f_bdc_data using: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\02',
                              ' '        ''     ' '          'LIKP-BLDAT'	        vl_data,
                              ' '        ''     ' '          'RV50A-LFDAT_LA'     vl_data,
                              ' '        ''     ' '          'RV50A-LFUHR_LA'     '00:00',
                              ' '        ''     ' '          'LIPSD-G_LFIMG(01)'  vg_lfimg.


    perform f_bdc_data using: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '/00',
                              ' '        ''     ' '          'LIPS-BRGEW(01)'	    vg_lfimg,
                              ' '        ''     ' '          'LIKP-ROUTE'	        p_saida-route.

    perform f_bdc_data using: 'SAPMV50A' '1000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=HDET_T'.

    perform f_bdc_data using: 'SAPMV50A' '2000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=T\07',
                              ' '        ''     ' '          'LIKP-BOLNR'         vg_bolnr.

    perform f_bdc_data using: 'SAPMV50A' '2000' 'X'          ''                   '' ,
                              ' '        ''     ' '          'BDC_OKCODE'         '=SICH_T',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(02)'         'PC',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(03)'         'LR',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARVW(04)'         'SP',
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(02)'       p_saida-ponto_coleta,
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(03)'       p_saida-local_entrega,
                              ' '        ''     ' '          'GVS_TC_DATA-REC-PARTNER(04)'       p_saida-lifnr.

    perform f_call_transaction using 'VL31N'
                                     p_saida
                            changing wl_erro.
  endif.

  if wl_erro is initial.
    commit work.
  else.
    exit.
  endif.
endform.

form f_action_user_remessa using p_saida type ty_saida
                                         raising zcx_error. "*-#133089-12.02.2024-JT

  data: wl_tvro       type tvro,
        shipment_row  type sy-tabix,
        current_row   type sy-tabix,
        v_werks       type ekpo-werks,
        v_lifnr       type ekko-lifnr,
        v_centro_real type zsdt_depara_cen-centro_real.

  data: t_route     type table of vbap-route with header line,
        t_romaneios type zsdt0001_t,
        v_faturar	  type char01,
        v_mensagem  type char255,
        v_erro      type char1.  "*-CS2021000218-16.11.2022-#90706-JT

  data v_charg      type mch1-charg.
  data v_matnr      type mara-matnr.
  data v_tenta      type i.
  data sperr_user   type sy-msgv1.
  data v_instr_referencia type zsdt0066-instrucao.
  data v_validar_saldo_faturar type char01.
  data v_peso_max type  zsdt0045-peso_max.
  data: matkl_algodao_pluma type range of matkl, "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG
        v_lifnrr            type lifnr, "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG
        v_kunnr             type kunnr. "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG

  field-symbols <fs_out> type ty_saida.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  read table it_saida assigning <fs_out>
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  shipment_row = sy-tabix.
  if ( p_saida-remessa eq icon_execute_object ).

    clear: v_tenta.
    while shipment_row is not initial.

      read table it_saida assigning <fs_out> index shipment_row.

      current_row = sy-tabix.

      check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

      "Valida placa veiculo - Transporte Romaneio.
      data(_placa_com_erro) = abap_false.
      perform f_valida_placas_faturamento using  <fs_out>
                                       changing _placa_com_erro.
      check _placa_com_erro is initial.

      "VALIDA SE VAI EXIBIR MENSAGEM DE SALDO À FATURAR.
      select count(*)
      from vbak  as bak
      inner join vbap as  bap  on  bap~vbeln = bak~vbeln
      into @data(valida_saldo_faturar)
      where auart in ( 'ZRFL' , 'ZRDC', 'ZIND', 'ZFEX' ) "Adicionando ZFEX 10-07-2024 - Projeto Algodao 2024 - WPP
      and   bak~vbeln = @<fs_out>-vbeln
      and   bap~matkl = '700140'.

      if valida_saldo_faturar is not initial.

        select single instrucao
          from zsdt0066
           into v_instr_referencia
          where vbeln = <fs_out>-vbeln.

        if sy-subrc ne 0. "ZFEX 23-08-2024 - Projeto Algodao 2024 - WPP - Issue 149489
          select single instrucao
          from zsdt0053
           into v_instr_referencia
          where vbeln = <fs_out>-vbeln.
        endif.

        select single *
         from zsdt0045
          into @data(wa_zsdt0045)
         where instrucao = @v_instr_referencia.

        if wa_zsdt0045-limite_peso = 'S' .

          v_peso_max =   wa_zsdt0045-peso_max.

          "TOTAL REMESSA
          select sum( vbfa~rfmng )
            from zsdt0066 as  zsdt66
            inner join vbfa as vbfa on vbfa~vbelv = zsdt66~vbeln
            into @data(total_remessa)
            where instrucao = @v_instr_referencia
            and   zsdt66~vbeln     <> ' '
            and   vbfa~vbtyp_n = 'J'
            and   vbfa~vbtyp_v = 'C'.

          data(saldo) = v_peso_max - total_remessa.

          if <fs_out>-peso_liq > saldo .
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
                message |A instrução { v_instr_referencia } só possui saldo de { saldo } Kg para faturamento. Verifique o limite cadastrado na instrução.| type 'I'.
                exit.
              when abap_true.
                data(l_mesg) = |A instrução { v_instr_referencia } só possui saldo de { saldo } Kg para faturamento. Verifique o limite cadastrado na instrução.|.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            endcase.
*-#133089-21.02.2024-JT-fim

          endif.

        endif.
      endif.

      add 1 to v_tenta.

      clear:  wa_zsdt0001.
      select single *
        from zsdt0001
        into wa_zsdt0001
       where ch_referencia = <fs_out>-ch_referencia.

      v_charg = wa_zsdt0001-nr_safra.

      if  <fs_out>-matnr is not initial.
        call function 'ENQUEUE_EMMCH1E'
          exporting
            mode_mch1      = 'E'
            mandt          = sy-mandt
            matnr          = <fs_out>-matnr
            charg          = v_charg
            _scope         = '2'
          exceptions
            foreign_lock   = 1
            system_failure = 2
            others         = 3.

        sperr_user     = sy-msgv1.
        if sy-subrc <> 0.
          if v_tenta ge 30.
            call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
              exporting
                input  = <fs_out>-matnr
              importing
                output = v_matnr.
            message |Material { v_matnr } para o lote { v_charg }, bloqueado por { sperr_user }.| type 'I'.
            exit.
          endif.
          wait up to 1 seconds.
          continue.
        endif.

        call function 'DEQUEUE_EMMCH1E'
          exporting
            mode_mch1 = 'E'
            mandt     = sy-mandt
            matnr     = <fs_out>-matnr
            charg     = v_charg.
      endif.

      clear: t_romaneios[], v_faturar, v_mensagem, t_route[].
      call method zcl_romaneio=>get_ck_faturar
        exporting
          i_ch_referencia_sai = <fs_out>-ch_referencia
        importing
          e_romaneios         = t_romaneios
          e_faturar           = v_faturar
          e_mensagem          = v_mensagem.

      if ( v_faturar is initial ).
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            if v_mensagem is not initial.
*              CALL FUNCTION 'S_AUT_POPUP_TO_DISPLAY_TEXT_LO'
*                EXPORTING
*                  iv_titel        = 'Erro Integração OPUS!'
*                  iv_textline1    = v_mensagem(65)
*                  iv_textline2    = v_mensagem+65(65)
*                  iv_textline3    = v_mensagem+130(65)
*                  iv_start_column = 20
*                  iv_start_row    = 10.
*              MESSAGE v_mensagem TYPE 'I'.
              message i024(sd) with v_mensagem(50) v_mensagem+50(50) v_mensagem+100(50) v_mensagem+150(50).
            else.
              call function 'S_AUT_POPUP_TO_DISPLAY_TEXT_LO'
                exporting
                  iv_titel        = 'Erro Integração OPUS!'
                  iv_textline1    = 'Dados adicionais do romaneio não integrados no SAP pelo OPUS!'
                  iv_textline2    = 'Realizar a ação de Alterar e Gravar no OPUS. '
                  iv_textline3    = 'Caso o problema persista abrir IR para suporte OPUS!'
                  iv_start_column = 20
                  iv_start_row    = 10.
*             MESSAGE 'Doc. Remessa não pode ser gerado(Check Agrupamento VT)!' TYPE 'I'.
            endif.
            return.
          when abap_true.
            if v_mensagem is not initial.
              l_mesg = v_mensagem.
            else.
              l_mesg = 'Dados adicionais do romaneio não integrados no SAP pelo OPUS.' &&
                       'Realizar a ação de Altera e Grava no OPUS. ' &&
                       'Caso o problema persista abrir IR para suporte OPUS.'.
*             l_mesg = 'Doc. Remessa não pode ser gerado(Check Agrupamento VT)!'.
            endif.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

      "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG -- INICIO
      select *
      from tvarvc
      into table @data(t_matkl_pluma)
      where name = 'MAGGI_GR_ALGODAO_PLUMA'.

      if sy-subrc = 0.
        loop at t_matkl_pluma into data(w_matkl_pluma).
          append value #( sign = 'I' option = 'EQ' low = w_matkl_pluma-low ) to matkl_algodao_pluma.
        endloop.
      endif.

      loop at t_romaneios into data(_wl_rom).
        select single matkl from mara into @data(v_matkl) where matnr = @wa_zsdt0001-matnr.

        if  v_matkl in matkl_algodao_pluma[]  and matkl_algodao_pluma[]  is not initial.
          select single lifnr, kunnr
            from vbpa
            into @data(wa_vbpa)
            where  vbeln  =  @_wl_rom-vbeln
            and parvw  in ( 'PC ', 'LR' ).

          if v_kunnr is initial.
            v_kunnr = wa_vbpa-kunnr.
          else.
            if v_kunnr eq wa_vbpa-kunnr.
              continue.
            else.
              message 'Parceiros PC e  LR  divergentes nas OVs. Faturamento não permitido!' type 'I'.
              return.
            endif.
          endif.

          if v_lifnr is initial.
            v_lifnr = wa_vbpa-lifnr.
          else.
            if v_lifnr eq wa_vbpa-lifnr.
              continue.
            else.
              message 'Parceiros PC e  LR  divergentes nas OVs. Faturamento não permitido!' type 'I'.
              return.
            endif.
          endif.
        endif.

      endloop.
      "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG  --  FIM

      loop at t_romaneios into data(wl_rom) where id_cli_dest is initial.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message |Romaneio: { wl_rom-nr_romaneio } sem Cliente Destino!| type 'I'.
            return.
          when abap_true.
            l_mesg = |Romaneio: { wl_rom-nr_romaneio } sem Cliente Destino!|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endloop.
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

      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = sy-tabix
          text       = |Criando documento de remessa para o romaneio { conv i( <fs_out>-nr_romaneio ) }.|.

*----CS2021000508 - 07.06.2021 - JT - inicio
*--------------------------------------------------
*-- Valida status / envio aprovacao
*--------------------------------------------------
      try .
          zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
            )->valida_envio_aprovacao(
                 exporting
                   i_ch_referencia = wa_zsdt0001-ch_referencia
                 importing
                   e_erro          = data(l_erro)
                   e_msg_erro      = data(l_msg_erro)
            ).

        catch zcx_integracao.
        catch zcx_error.
      endtry.

      if l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message l_msg_erro type 'I' .
            exit.
          when abap_true.
            l_mesg = l_msg_erro.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.
*----CS2021000508 - 07.06.2021 - JT - fim

      clear: shipment_row.

      "Valida Centro Emissor O.V/Pedido, com centro emissor do Romaneio.
      clear: v_centro_real,v_werks.
      if <fs_out>-tipo = 'O'.
        select single werks
          from vbap
          into v_werks
          where vbeln = wa_zsdt0001-vbeln.
        if wa_zsdt0001-branch ne v_werks.
          select single centro_real
            into v_centro_real
            from zsdt_depara_cen
          where  centrov_1 = v_werks.
          if ( sy-subrc = 0 ) and ( wa_zsdt0001-branch ne v_centro_real ).
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
                message 'Centro emissor do romaneio diferente do centro emissor da OV' type 'I'.
                exit.
              when abap_true.
                l_mesg = 'Centro emissor do romaneio diferente do centro emissor da OV'.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            endcase.
*-#133089-21.02.2024-JT-fim
          endif.
        endif.
      else.

        select single *
          from ekko into @data(lwa_pedido_romaneio)
         where ebeln eq @wa_zsdt0001-vbeln.

        if ( sy-subrc = 0 ).

          case lwa_pedido_romaneio-bsart.
            when 'ZUB'.
              v_werks = lwa_pedido_romaneio-reswk.
            when others.
              select single *
                from ekpo into @data(lwa_item_pedido_romaneio)
               where ebeln eq @wa_zsdt0001-vbeln.

              if sy-subrc eq 0.
                v_werks = lwa_item_pedido_romaneio-werks.
              endif.
          endcase.

          if wa_zsdt0001-branch ne v_werks.
            select single *
              from zsdt_depara_cen into @data(lwa_zsdt_depara_cen)
             where centrov_1 eq @v_werks.

            if not ( ( sy-subrc eq 0 ) and ( lwa_zsdt_depara_cen-centro_real eq wa_zsdt0001-branch  ) ).
*-#133089-21.02.2024-JT-inicio
              case vg_faturamento_autom.
                when abap_off.
                  message 'Centro emissor do romaneio diferente do centro emissor do pedido.' type 'I'.
                  exit.
                when abap_true.
                  l_mesg = 'Centro emissor do romaneio diferente do centro emissor do pedido.'.
                  lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
              endcase.
*-#133089-21.02.2024-JT-fim
            endif.
          endif.

        endif.

      endif.

      if wa_zsdt0001-doc_rem gt 0.
        message 'Documento atualizado, click em <ATUALIZAR>' type 'I'.
        exit.
      endif.

      if <fs_out>-inco1 = 'CIF'.
        if wa_zsdt0001-motorista is initial .

*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message 'Motorista não informado, reenviar romaneio' type 'I'.
              exit.
            when abap_true.
              l_mesg = 'Motorista não informado, reenviar romaneio'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim

        elseif wa_zsdt0001-placa_cav is initial.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message 'Placa cavalo não informada, reenviar romaneio' type 'I'.
              exit.
            when abap_true.
              l_mesg = 'Placa cavalo não informada, reenviar romaneio'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.

*---------------------------------------------------------------------------------*
*     Validações por tipo de Cockpit
*---------------------------------------------------------------------------------*
      case vg_cockpit. "Bloqueio antes de Gerar Remessa
        when '03'. " OR '09'.
          if <fs_out>-peso_liq_pos_ret = 0.
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
                message 'Peso liquido pós retenção não calculado!' type 'I'.
                exit.
              when abap_true.
                l_mesg = 'Peso liquido pós retenção não calculado!'.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
            endcase.
*-#133089-21.02.2024-JT-fim
          elseif <fs_out>-peso_descarga gt <fs_out>-peso_liq.
*            MESSAGE 'Peso liquido MENOR que peso descarga !' TYPE 'I'.
*            EXIT.
          endif.
        when '04'.
          if <fs_out>-danfez = icon_execute_object.
            message 'Gerar a Nota de Remessa!' type 'I'.
            exit.
          endif.

          case <fs_out>-inco1.
            when 'CIF' or 'CPT'.
              if ( <fs_out>-tipo     = 'P' ) and
                 ( <fs_out>-aviso(1) = '@' ).
                message 'Gerar aviso de recebimento!' type 'I'.
                exit.
              endif.
          endcase.
      endcase.

      v_xblnr = <fs_out>-ch_referencia.
      select single *
        from likp
        into wa_likp
       where xblnr = v_xblnr
         and spe_loekz = ''.

      if sy-subrc = 0.
        message |Já existe a remessa { wa_likp-vbeln } para o romaneio { <fs_out>-nr_romaneio }, estorne.| type 'I'.

        <fs_out>-remessa = wa_likp-vbeln.
        <fs_out>-st_proc = vg_st_remessa.

        update zsdt0001 set doc_rem = wa_likp-vbeln
                            st_proc = vg_st_remessa
          where ch_referencia = <fs_out>-ch_referencia.

        perform f_refresh_alv using '0100'. "Refresh na tela

        exit.
      endif.

      if sy-tcode ne 'ZLES0136'
        and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        and sy-tcode ne 'ZMM0127' and
        vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT
        message 'Transação apenas de visualização' type 'I'.
        exit.
      endif.

*      VINCO1 = <FS_OUT>-INCO1.
      clear vinco1.


      if ( <fs_out>-lifnr is initial ) and
       not  line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ) and
         ( <fs_out>-inco1 ne 'CFR'     and
           <fs_out>-inco1 ne 'FOB' ).  "*-CS2021000218-16.11.2022-#90706-JT-
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Informar o agente de frete!' type 'I'.
            exit.
          when abap_true.
            l_mesg = 'Informar o agente de frete!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

      if ( <fs_out>-lifnr is not initial ) and
        line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Informar o agente de frete, somente na criação do documento de transporte!' type 'I'.
            exit.
          when abap_true.
            l_mesg = 'Informar o agente de frete, somente na criação do documento de transporte!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

      "187202 - bug solto - RGA
      if  <fs_out>-lifnr is not initial.

        select count(*)
           from lfb1
           where lifnr = <fs_out>-lifnr
             and bukrs = <fs_out>-bukrs
             and sperr = ''
             and loevm = ''.
        if sy-subrc ne 0.
          case vg_faturamento_autom.
            when abap_off.

              concatenate 'Fornecedor' <fs_out>-lifnr 'não expandido para empresa.' <fs_out>-bukrs into data(vmessa) separated by space.
              message vmessa type 'I'.
              exit.

            when abap_true.
              concatenate 'Fornecedor' <fs_out>-lifnr 'não expandido para empresa.' <fs_out>-bukrs into l_mesg.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
        endif.
      endif.
      "187202 - bug solto - RGA - FIM

      if ( wa_zsdt0001-placa_cav is not initial ) and ( <fs_out>-region is initial ) and ( t_fatura_agrupada is initial ).
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Informar a UF da placa cavalo!' type 'I'.
            exit.
          when abap_true.
            l_mesg = 'Informar a UF da placa cavalo!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

      if ( <fs_out>-region is not initial ).
        select single *
          from t005s
          into wa_t005s
          where land1 = 'BR'
          and   bland = <fs_out>-region.
        if sy-subrc ne 0.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message |UF { <fs_out>-region } é inválida!| type 'I'.
              exit.
            when abap_true.
              l_mesg = |UF { <fs_out>-region } é inválida!|.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.

      "//Validação para campo "relevância para transporte"
      if ( 'CPT_CIF' cs <fs_out>-inco1  ) and ( <fs_out>-inco1 is not initial ).

        clear wl_tvro.
        select single *
          into wl_tvro
          from tvro
          where route eq <fs_out>-route.

        if wl_tvro-tdiix is initial.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message |Itinerário { <fs_out>-route } sem relevância para transporte. Solicite regularização para à logística.| type 'I'.
              exit.
            when abap_true.
              l_mesg = |Itinerário { <fs_out>-route } sem relevância para transporte. Solicite regularização para à logística.|.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.

      if ( <fs_out>-lifnr is not initial ).
        select single lifnr name1 dlgrp
          from lfa1
          into wa_lfa1
         where lifnr = <fs_out>-lifnr.
      endif.

      if ( <fs_out>-lifnr is not initial ) and wa_lfa1-dlgrp not in r_dlgrp and
         (
            ( <fs_out>-inco1 ne 'FOB' and <fs_out>-inco1 ne 'CFR' ) or
            ( <fs_out>-enc_conhecimento eq abap_true )
         ).
        clear: <fs_out>-lifnr, <fs_out>-region.

        perform f_refresh_alv using '0100'. "Refresh na tela

        update zsdt0001 set agente_frete = ''
                            region       =  ''
         where ch_referencia = <fs_out>-ch_referencia.

*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message |Fornecedor { <fs_out>-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.| type 'I'.
            exit.
          when abap_true.
            l_mesg = |Fornecedor { <fs_out>-lifnr } não configurado como agente de frete. Solicite ajuste à central de cadastro.|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

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

      if ( <fs_out>-tipo = 'O' and  <fs_out>-operacao+0(4) = 'ZRDC' ). "DCO
        select single *
          from zdco_produtor
          into wa_zdco_produtor
         where vbeln       = <fs_out>-vbeln
           and cd_material = <fs_out>-matnr
           and cd_centro   = <fs_out>-branch.

        if sy-subrc ne 0.
          update zsdt0001 set agente_frete = '' region =  ''
           where ch_referencia = <fs_out>-ch_referencia.

          perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message 'DCO não cadastrado para essa Ordem de Venda. Contactar o mercado interno' type 'I'.
              exit.
            when abap_true.
              l_mesg = 'DCO não cadastrado para essa Ordem de Venda. Contactar o mercado interno'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.


*-CS2021000218-16.11.2022-#90706-JT-inicio
      if vg_cockpit = '06'.  "Insumos
        perform f_validar_solicitacao_ra  using <fs_out>-nro_cg
                                                <fs_out>-ch_referencia
                                       changing <fs_out>
                                                l_erro.
        if l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message 'Remessa não foi gerada! Verifique o Log deste Romaneio!' type 'I'.
              exit.
            when abap_true.
              l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
      if vg_cockpit = '01'.  "Pesagem OPUS saida
        perform f_validar_transf_algodao  using <fs_out>-ch_referencia
                                       changing <fs_out>
                                                l_erro.
        if l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message 'Remessa não foi gerada! Verifique o Log deste Romaneio!' type 'I'.
              exit.
            when abap_true.
              l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      endif.
*-CS2023000189-26.05.2023-#108752-JT-fim

      zcl_comercializacao_algodao=>ck_ajuste_sobra_perda_pendente( i_chave_referencia_rom = <fs_out>-ch_referencia ). "SD - Ganho Peso Automatico Algodao US #145369 - WPP

      perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
      if sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          when abap_true.
            l_mesg = |Romaneio bloqueado por { sy-msgv1 }. Processamento Paralizado.|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.

      update zsdt0001 set st_proc = <fs_out>-st_proc region = <fs_out>-region
       where ch_referencia = <fs_out>-ch_referencia.

      <fs_out>-line_color = 'C310'.

      refresh it_color.
      move 'REMESSA'  to wa_color-fname.
      move '5'        to wa_color-color-col.
      move '1'        to wa_color-color-int.
      move '1'        to wa_color-color-inv.
      append wa_color to it_color.

      <fs_out>-color_cell[] = it_color[].


      clear: vl_delivery.

      set parameter id 'Z_MY_PARAMETER_1' field <fs_out>-ch_referencia.
      if <fs_out>-tipo = 'O'.
        submit zsdi0009 with p_vbeln = <fs_out>-vbeln
                        with p_lifnr  = <fs_out>-lifnr
                        with p_peso   = <fs_out>-peso_liq_pos_ret
                        with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
        and return.

        get parameter id 'Z_MY_PARAMETER_2' field vl_delivery.

      elseif <fs_out>-tipo = 'P' and <fs_out>-bsart = 'ZARM'.

        call method zcl_remessa_armazenagem=>zif_remessa_armazenagem~gerar_remessa_com_pesagem_opus
          exporting
            i_ch_ref_romaneio = <fs_out>-ch_referencia
            i_lifnr_sp        = <fs_out>-lifnr
          receiving
            r_delivery        = vl_delivery.

      else.
        submit ztransf with p_ebeln  = <fs_out>-vbeln
                       with p_lifnr  = <fs_out>-lifnr
                       with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
                       with p_cockp  = vg_cockpit            "*-#144450-28.06.2024-JT
        and return.

        get parameter id 'Z_MY_PARAMETER_2' field vl_delivery.

      endif.

      if ( vl_delivery is not initial ).
        if ( vl_delivery = '9999999999' ). "Erro de bloqueio no OPUS
          shipment_row = current_row.
          continue.
        endif.

        <fs_out>-remessa = vl_delivery. " Já grava em ZSDT0001-DOC_REM

        if ( <fs_out>-tipo = 'P' ) or ( <fs_out>-tipo = 'T' ).

          select single vbeln mjahr
            into (vl_vbeln,vl_mjahr)
            from vbfa
           where vbelv = vl_delivery
             and vbtyp_n  = 'R'
             and vbtyp_v  = 'J'.

          <fs_out>-fatura = vl_vbeln.
        endif.

        if <fs_out>-lifnr is not initial.
          refresh: style.
          clear: wa_style.

          wa_style-fieldname = 'LIFNR'.
          wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.

          delete <fs_out>-style where fieldname eq 'LIFNR'.
          insert  wa_style into table style .
          <fs_out>-style[] = style[].
        endif.

        if line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
          loop at it_saida into it_saida where ( dt_movimento eq <fs_out>-dt_movimento ) and
                                               ( matnr        eq <fs_out>-matnr        ) and
                                               ( kunnr        eq <fs_out>-kunnr        ) and
                                               ( operacao(4)  eq <fs_out>-operacao(4)  ) and
*                                             ( INCO1        EQ VINCO1                ) AND
                                               ( cfop         eq <fs_out>-cfop         ) and
                                               ( remessa      is initial               ).

            shipment_row = sy-tabix.
            exit.
          endloop.
        endif.

        "//Grava vinculo DCO
        if <fs_out>-tipo = 'O' and  <fs_out>-operacao+0(4) = 'ZRDC'. "DCO
          submit zsdi0006 with p_vbeln  = <fs_out>-remessa
                          with p_vinc   = 'X'
                          with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
          and return.
        endif.

        if <fs_out>-tipo = 'O'.
          update zsdt0001 set st_proc = vg_st_remessa
                              agente_frete = <fs_out>-lifnr
           where ch_referencia = <fs_out>-ch_referencia.

          if vg_cockpit = '03' or vg_cockpit = '09' or vg_cockpit = '10'.
            update zsdt0001 set peso_descarga    = <fs_out>-peso_descarga
                                perc_ret         = <fs_out>-perc_ret
                                peso_retido      = <fs_out>-peso_retido
                                peso_liq_pos_ret = <fs_out>-peso_liq_pos_ret
             where ch_referencia = <fs_out>-ch_referencia.
          endif.

          <fs_out>-st_proc = vg_st_remessa.
        else.
          if ( <fs_out>-inco1 = 'FOB' or <fs_out>-inco1 = 'CFR' ) and ( not <fs_out>-enc_conhecimento = abap_true ). " Finaliza processo com a Danfe autorizada

*----CS2021000508 - 07.06.2021 - JT - inicio
            if <fs_out>-troca_nota            = abap_true and
               <fs_out>-docs_enviado_carguero = abap_false.
              update zsdt0001 set st_proc = vg_st_aguard_doc_carg " Finalizado
                                  fatura_prod  = <fs_out>-fatura
                                  agente_frete = <fs_out>-lifnr
              where ch_referencia = <fs_out>-ch_referencia.

              <fs_out>-st_proc = vg_st_aguard_doc_carg.
            else.
              update zsdt0001 set st_proc = vg_st_finalizado " Finalizado
                                  fatura_prod  = <fs_out>-fatura
                                  agente_frete = <fs_out>-lifnr
              where ch_referencia = <fs_out>-ch_referencia.

              <fs_out>-st_proc = vg_st_finalizado.
            endif.

*           UPDATE zsdt0001 SET st_proc = vg_st_finalizado " Finalizado
*                               fatura_prod  = <fs_out>-fatura
*                               agente_frete = <fs_out>-lifnr
*           WHERE ch_referencia = <fs_out>-ch_referencia.

            clear <fs_out>-icon.
*           <fs_out>-st_proc = vg_st_finalizado.
*----CS2021000508 - 07.06.2021 - JT - fim

          else.
            update zsdt0001 set st_proc      = vg_st_fatura " Danfe mudanã para 02 fatura até aprovação sefaz
                                fatura_prod  = <fs_out>-fatura
                                agente_frete = <fs_out>-lifnr
            where ch_referencia = <fs_out>-ch_referencia.

            <fs_out>-st_proc = vg_st_fatura.
          endif.
        endif.

        if wa_zsdt0001-id_interface = '51'. "Insumos - Fertilizantes
          perform f_item_text_delivery using vl_delivery.
        endif.

        if <fs_out>-operacao(4) = 'ZTER'.
          perform f_atrib_vlr_nf_rem using vl_delivery
                                           wa_zsdt0001-netwr.
        endif.

      else.
        if vg_cockpit ne '04'.
          clear: <fs_out>-lifnr, <fs_out>-region.

          update zsdt0001 set agente_frete = ''
                              region       = ''
           where ch_referencia = <fs_out>-ch_referencia.
        endif.
      endif.

      perform f_repare_docs_romaneio changing <fs_out>.
      perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

      if <fs_out> is assigned.
        unassign <fs_out>.
      endif.

    endwhile.

    perform f_refresh_alv using '0100'. "Refresh na tela

*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
      when abap_true.
        if vl_delivery is initial.
          l_mesg = abap_off.
          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = l_mesg ).
        endif.
    endcase.
*-#133089-21.02.2024-JT-fim

*-IR 069418 - 13.09.2022 - JT - inicio
  elseif p_saida-remessa is not initial and p_saida-remessa(10) = '@8Y\QLimit'.
    perform f_reinicia_aprovacao using p_saida.
    perform f_refresh_alv using '0100'. "Refresh na tela
*-IR 069418 - 13.09.2022 - JT - fim

  elseif p_saida-remessa is not initial and p_saida-remessa(1) ne '@'.
    if p_saida-remessa ne icon_icon_list.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
          clear shipment_row.
          set parameter id 'VL'    field p_saida-remessa+0(10).
          call transaction 'VL03N' and skip first screen.
        when abap_true.
      endcase.
*-#133089-21.02.2024-JT-fim
    endif.


*-CS2021000218-16.11.2022-#90706-JT-inicio
  elseif p_saida-remessa is not initial and p_saida-remessa = icon_led_yellow.
    if vg_cockpit = '06'.  "Insumos
      perform f_validar_solicitacao_ra  using p_saida-nro_cg
                                              p_saida-ch_referencia
                                     changing p_saida
                                              l_erro.
      if l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Remessa não foi gerada! Verifique o Log deste Romaneio!' type 'I'.
          when abap_true.
            l_mesg =  'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      else.
        message 'Romaneio pronto para gerar Remessa!' type 'I'.
      endif.
      perform f_refresh_alv using '0100'. "Refresh na tela
    endif.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
    if vg_cockpit = '01'.  "Pesagem OPUS saida
      perform f_validar_transf_algodao  using p_saida-ch_referencia
                                     changing p_saida
                                              l_erro.
      if l_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Remessa não foi gerada! Verifique o Log deste Romaneio!' type 'I'.
          when abap_true.
            l_mesg = 'Remessa não foi gerada! Verifique o Log deste Romaneio!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
        endcase.
*-#133089-21.02.2024-JT-fim
      else.
        message 'Romaneio pronto para gerar Remessa!' type 'I'.
      endif.
      perform f_refresh_alv using '0100'. "Refresh na tela
    endif.
*-CS2023000189-26.05.2023-#108752-JT-fim
  endif.

endform.

**********************************************************************
* Reinicia aprovacao remessa
**********************************************************************
form f_reinicia_aprovacao using p_saida  type ty_saida.

  data: l_resp type c.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = text-013
      text_question         = text-010
      text_button_1         = text-011
      text_button_2         = text-012
      display_cancel_button = ''
      default_button        = '2'
    importing
      answer                = l_resp
    exceptions
      text_not_found        = 1
      others                = 2.

  if l_resp = '1'.
    read table it_saida assigning field-symbol(<fs_out2>)
                        with key ch_referencia = p_saida-ch_referencia.

    delete from zsdt0151 where ch_referencia = p_saida-ch_referencia.

    if sy-subrc = 0.
      <fs_out2>-remessa = icon_execute_object.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = c_x.
    endif.
  endif.

endform.

form f_action_user_fatura  using p_saida        type ty_saida
                                 p_tipo_chamada type char01
                           changing it_saida_romaneios type zde_les_saida_zsdt0001_t
                                    it_tab_bapiret1    type tab_bapiret1
                           raising zcx_error. "*-#133089-12.02.2024-JT

  data wa_zsdt0023  type zsdt0023.

*-#133089-21.02.2024-JT-inicio
  data: w_zlest0241  type zlest0241.

  select single *
    from zlest0241
    into w_zlest0241
   where ch_referencia = p_saida-ch_referencia
     and cancelado     = abap_off.

  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  data: lva_action_vf type sy-tcode.

  "FIELD-SYMBOLS <FS_SAIDA> TYPE TY_SAIDA.

  read table it_saida_romaneios assigning field-symbol(<fs_out>)
    with key ch_referencia = p_saida-ch_referencia.

  check ( sy-subrc = 0 ) and ( <fs_out> is assigned ).

  if ( p_tipo_chamada = 'L' and <fs_out>-fatura = icon_execute_object ) or ( p_tipo_chamada = 'E' and <fs_out>-fatura is initial ).
    clear wa_zsdt0001.
    select single *
      from zsdt0001
      into wa_zsdt0001
     where ch_referencia = <fs_out>-ch_referencia.

    select single vbeln mjahr
      into (vl_vbeln,vl_mjahr)
      from vbfa
     where vbelv = <fs_out>-remessa
       and vbtyp_n  = 'R'
       and vbtyp_v  = 'J'.
    if sy-subrc ne 0.
      data(tx_msg) = 'Picking não realizado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
        when abap_true.
          data(l_mesg) = tx_msg.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
      endcase.
*-#133089-21.02.2024-JT-fim
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    else.
      select single vbeln mjahr
           into (vl_vbeln,vl_mjahr)
           from vbfa
          where vbelv = <fs_out>-remessa
            and vbtyp_n  = 'h'
            and vbtyp_v  = 'J'.
      if sy-subrc eq 0.
        tx_msg = 'Picking estornado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.
    endif.

    "Checa o Picking
    if <fs_out>-operacao+0(4) eq 'ZRDC' or
       <fs_out>-operacao+0(4) eq 'ZRFL' or
       <fs_out>-operacao+0(4) eq 'ZIND' .
      "MBST (estorno de migo)
      clear wa_zsdt0023.
      select single *
          from zsdt0023
          into wa_zsdt0023
          where vbeln   = <fs_out>-remessa.
      if wa_zsdt0023-mblnr_s is initial.
        tx_msg = 'Picking não realizado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      elseif wa_zsdt0023-es_mblnr_s is not initial.
        tx_msg = 'Picking estornado, estorne a remessa'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.
    endif.


    if wa_zsdt0001-fatura_prod gt 0 and vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
      tx_msg = 'Documento atualizado, click em <ATUALIZAR>'.
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

    if sy-tcode ne 'ZLES0136'
      and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      and sy-tcode ne 'ZMM0127' and p_tipo_chamada ne 'E'
      and vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT-inicio

      tx_msg = 'Transação apenas de visualização'.
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

    if <fs_out>-remessa = icon_execute_object and vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT.
      tx_msg = 'Gerar a Remessa!'.
      case p_tipo_chamada.
        when 'L'.
          message tx_msg type 'I'.
        when 'E'.
          "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
      endcase.
      exit.
    endif.

    if line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
      try.
          it_saida = it_saida_romaneios[ dt_movimento = <fs_out>-dt_movimento
                                         matnr        = <fs_out>-matnr
                                         kunnr        = <fs_out>-kunnr
                                         operacao(4)  = <fs_out>-operacao(4)
                                         cfop         = <fs_out>-cfop
                                         remessa      = icon_execute_object ].

          tx_msg = |Para prosseguir com a fatura agrupada é necessário gerar o doc.remessa do romaneio { it_saida-nr_romaneio }.|.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
            when abap_true.
              l_mesg = tx_msg.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
          endcase.
*-#133089-21.02.2024-JT-fim
          case p_tipo_chamada.
            when 'L'.
              message tx_msg type 'I' display like 'W'.
            when 'E'.
              "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
          endcase.
          exit.
        catch cx_sy_itab_line_not_found.
      endtry.
    endif.

    if <fs_out>-tipo = 'O' and <fs_out>-operacao+0(4) = 'ZRDC'. "DCO
      select single vbeln
        from zdco_vinculo
        into vl_vbeln
       where vbeln eq <fs_out>-remessa.

      if not sy-subrc is initial.
        tx_msg = 'Remessa sem vinculo com DCO.'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.
    endif.

    if ( ( <fs_out>-inco1 = 'CPT' )  or
         ( <fs_out>-enc_doc_custo eq abap_true ) )
      and not line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
      if <fs_out>-kbetr le 0.
        tx_msg = 'Não existe valor de frete cadastrado. Solicite à transportadora da sua região'.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
          when abap_true.
            l_mesg = tx_msg.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
        case p_tipo_chamada.
          when 'L'.
            message tx_msg type 'I'.
          when 'E'.
            "ZCL_CARGA_SAIDA=>ZIF_CARGA~GERA_ERRO_GERAL( I_TEXTO = CONV #( TX_MSG ) ).
        endcase.
        exit.
      endif.
    endif.

    case p_tipo_chamada.
      when 'L'.
        perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        if sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            when abap_true.
              data(l_mesg2) = |Romaneio bloqueado por { sy-msgv1 }. Processamento Paralizado.|.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg2 ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.
      when 'E'.
    endcase.

    update zsdt0001 set st_proc = <fs_out>-st_proc
     where ch_referencia = <fs_out>-ch_referencia.

    <fs_out>-line_color = col_yellow_int.
    <fs_out>-color_cell = value #( ( fname = 'FATURA'
                                     color-col = col_positive
                                     color-int = col_heading
                                     color-inv = col_heading )
                                 ).


    refresh: t_billing, it_lines, t_textdatain, t_return, t_success.

    "//Buscar Texto da ordem venda
    wl_ordemt = <fs_out>-vbeln.
    call function 'READ_TEXT'
      exporting
        id                      = '0002'
        language                = sy-langu
        name                    = wl_ordemt
        object                  = 'VBBK'
      tables
        lines                   = it_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    data(ck_dados_zsdt0001ft) = abap_false.

*  IF <FS_OUT>-ID_CARGA IS NOT INITIAL.
*    SELECT SINGLE * INTO @DATA(WA_ZSDT0001FT)
*      FROM ZSDT0001FT
*     WHERE ID_CARGA EQ @<FS_OUT>-ID_CARGA.
*
*    IF SY-SUBRC IS INITIAL.
*      CK_DADOS_ZSDT0001FT = ABAP_TRUE.
*
*      CONCATENATE SY-MANDT <FS_OUT>-ID_CARGA INTO WL_ORDEMT.
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          ID                      = 'ZFAT'
*          LANGUAGE                = SY-LANGU
*          NAME                    = WL_ORDEMT
*          OBJECT                  = 'ZROMSAIDA'
*        TABLES
*          LINES                   = IT_LINES_ZFAT
*        EXCEPTIONS
*          ID                      = 1
*          LANGUAGE                = 2
*          NAME                    = 3
*          NOT_FOUND               = 4
*          OBJECT                  = 5
*          REFERENCE_CHECK         = 6
*          WRONG_ACCESS_TO_ARCHIVE = 7
*          OTHERS                  = 8.
*    ENDIF.
*  ENDIF.

    if ck_dados_zsdt0001ft eq abap_false and sy-batch eq abap_false.

*-#133089-21.02.2024-JT-inicio
      if vg_faturamento_autom = abap_false.
        call function 'POPUP_TO_CONFIRM'
          exporting
            text_question         = 'Informar texto da fatura?'
            text_button_1         = 'Sim'(100)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(101)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 25
            start_row             = 6
          importing
            answer                = w_answer
          exceptions
            text_not_found        = 1
            others                = 2.

        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      else.
        w_answer = '1'.
      endif.
*-#133089-21.02.2024-JT-fim

*-CS2023000189-26.05.2023-#108752-JT-inicio
*-------------------------------------------
*------ informacoes de fardos no texto da fatura
*-------------------------------------------
      select *
        into table @data(t_0330)
        from zsdt0330
       where ch_referencia   = @<fs_out>-ch_referencia
         and status_estorno <> 'D'
         and cancelado       = @abap_false.
      if sy-subrc <> 0.
        free t_0330.
      endif.

      data(t_0330_lote) = t_0330[].

      sort t_0330_lote by lgort.
      delete adjacent duplicates from t_0330_lote
                            comparing lgort.
*-CS2023000189-26.05.2023-#108752-JT-fim

      if w_answer = '1'.
        refresh tl_texto.

*-CS2023000189-26.05.2023-#108752-JT-inicio
        loop at t_0330_lote into data(w_0330_lote).
          data(l_lines_330) = 0.
          loop at t_0330    into data(w_0330) where lgort = w_0330_lote-lgort.
            l_lines_330     = l_lines_330 + 1.
          endloop.
          wl_texto          = | { 'Lote' } { w_0330_lote-lgort } { 'com' } { l_lines_330 } { 'Fardo(s).' } |.
          append wl_texto  to tl_texto.
        endloop.
*-CS2023000189-26.05.2023-#108752-JT-fim

*** Inicio - Rubenilson - 10.09.24 - #140377
        data: lv_name  type thead-tdname,
              lt_lines type table of tline.

        select *
          from zsdt0138
          into @data(ls_zsdt0138)
          up to 1 rows
          where ch_referencia = @<fs_out>-ch_referencia.
        endselect.
        if sy-subrc is initial.
          lv_name = ls_zsdt0138-seq_cam && ls_zsdt0138-nro_sol && ls_zsdt0138-seq && ls_zsdt0138-filial_resp.
        endif.

        call function 'READ_TEXT'
          exporting
            id                      = 'ST'
            language                = sy-langu
            name                    = lv_name
            object                  = 'TEXT'
          tables
            lines                   = lt_lines
          exceptions
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            others                  = 8.
        if sy-subrc is initial.
          append initial line to tl_texto assigning field-symbol(<fs_texto>).
          loop at lt_lines assigning field-symbol(<fs_lines>).
            append initial line to tl_texto assigning <fs_texto>.

            <fs_texto> = <fs_lines>-tdline.
          endloop.
        endif.

*** Fim - Rubenilson - 10.09.24 - #140377

*-#133089-21.02.2024-JT-inicio
        if vg_faturamento_autom = abap_false.
          call function 'CATSXT_SIMPLE_TEXT_EDITOR'
            exporting
              im_title = 'Texto da Fatura'
            changing
              ch_text  = tl_texto.
        endif.
*-#133089-21.02.2024-JT-fim

        loop at tl_texto into wl_texto.
          wa_lines-tdformat = '*'.
          wa_lines-tdline+0(72) = wl_texto.
          append wa_lines to it_lines.
          clear  wa_lines.
        endloop.
*-CS2023000189-26.05.2023-#108752-JT-inicio
      else.
        loop at t_0330_lote  into w_0330_lote.
          l_lines_330           = 0.
          loop at t_0330     into w_0330 where lgort = w_0330_lote-lgort.
            l_lines_330         = l_lines_330 + 1.
          endloop.
          wa_lines-tdformat     = '*'.
          wa_lines-tdline+0(72) = | { 'Lote' } { w_0330_lote-lgort } { 'com' } { l_lines_330 } { 'Fardo(s).' } |.
          append wa_lines      to it_lines.
          clear  wa_lines.
        endloop.
*-CS2023000189-26.05.2023-#108752-JT-fim
      endif.
    elseif ck_dados_zsdt0001ft eq abap_true.
      loop at it_lines_zfat into wa_lines.
        append wa_lines to it_lines.
      endloop.
    endif.

*-#133089-21.02.2024-JT-inicio
    data vpos  type i.
    data vqtde type i.
    vpos = 0.
    data(vlen) = strlen(  w_zlest0241-texto_fatura ).
    if w_zlest0241-texto_fatura is not initial and vg_faturamento_autom = abap_true.
      do 3 times.
        if vlen lt 132.
          vqtde = vlen.
        else.
          vqtde = 132.
        endif.
        if w_zlest0241-texto_fatura+vpos(vqtde) is not initial.
          wa_lines-tdformat     = '*'.
          wa_lines-tdline = w_zlest0241-texto_fatura+vpos(vqtde). "-#191707 - 24.09.2025 -ALRS"
          append wa_lines      to it_lines.
          if vlen lt 132.
            exit.
          else.
            subtract 132 from vlen.
            add 132 to vpos.
          endif.
        endif.
      enddo.
    endif.
*-#133089-21.02.2024-JT-fim

    "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
    "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
    select single *
      from setleaf
      into wa_setleaf
     where setname = 'VF01_USUARIO'
       and valfrom = sy-uname.

    if sy-subrc is initial.
      w_billing-bill_date = <fs_out>-dt_movimento.
    else.
      w_billing-bill_date = sy-datum.
    endif.

    if wa_zsdt0001-fat_contingencia_ecc eq abap_true.
      data: lwa_faturamento_ecc type zde_compare_faturamento.

      call function 'ZLES_FAT_CONTINGENCIA_0002'
        exporting
          i_ch_referencia         = wa_zsdt0001-ch_referencia
          i_get_dados_fat_ecc     = abap_true
        importing
          e_dados_faturamento_ecc = lwa_faturamento_ecc.

      if lwa_faturamento_ecc-data_lcto_nf is initial.
        message 'Data Lacto NF-e não encontrado no ECC'  type 'E'.
        return.
      endif.

      w_billing-bill_date = lwa_faturamento_ecc-data_lcto_nf.
    endif.

    lva_action_vf = 'CANCEL'.
    export lva_action_vf to memory id 'ZACTION_VF'.

    "//17.01.2017 - EJ (Adiciona faturas agrupadas)
    if line_exists( t_fatura_agrupada[ werks = <fs_out>-branch kunnr = <fs_out>-kunnr inco1 = vinco1 cfop = <fs_out>-cfop ] ).
      loop at it_saida_romaneios assigning field-symbol(<fs_saida>)
               where ( dt_movimento eq <fs_out>-dt_movimento ) and
                     ( matnr        eq <fs_out>-matnr        ) and
                     ( kunnr        eq <fs_out>-kunnr        ) and
                     ( operacao(4)  eq <fs_out>-operacao(4)  ) and
                     ( cfop         eq <fs_out>-cfop         ) and
                     ( fatura       is initial               ).

        update zsdt0001 set st_proc = <fs_saida>-st_proc
         where ch_referencia = <fs_saida>-ch_referencia.

        <fs_saida>-line_color = col_yellow_int.
        <fs_saida>-color_cell = value #( ( fname = 'FATURA'
                                           color-col = col_positive
                                           color-int = col_heading
                                           color-inv = col_heading ) ).
        w_billing-ref_doc    = <fs_saida>-remessa.
        w_billing-ref_doc_ca = 'J'.
        append w_billing to t_billing.
      endloop.
    endif.

    w_billing-ref_doc    = <fs_out>-remessa.
    w_billing-ref_doc_ca = 'J'.
    append w_billing to t_billing.

    refresh: ti_bdcdata, t_success.
    concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) into data(_data).
    perform f_bdc_data using:
        'SAPMV60A'  '0102'  'X'               ''                 '',
        ' '         ''      ' '               'BDC_CURSOR'       'RV60A-FKDAT',
        ' '         ''      ' '               'BDC_OKCODE'       '=SICH',
        ' '         ''      ' '               'RV60A-FKDAT'      _data,
        ' '         ''      ' '               'KOMFK-VBELN(01)'  <fs_out>-remessa.

    select single *
      from setleaf into @data(_setleaf_vf)
     where setname = 'MAGGI_GER_VFSHDB'.

    if ( sy-subrc ne 0 ).
      clear: wl_erro.
      perform f_call_transaction using 'VF01' <fs_out> changing wl_erro.

      if wl_erro is initial.
        wait up to 6 seconds.
        clear: vl_vbeln,vl_mjahr.
        if ( wa_saida-tipo = 'P' ) or ( wa_saida-tipo = 'T' ).
          select single a~vbeln a~mjahr
            from vbfa as a into (vl_vbeln,vl_mjahr)
           where a~vbelv = <fs_out>-remessa
             and a~vbtyp_n  = 'R'
             and a~vbtyp_v  = 'J'
            "Estorno
             and not exists ( select * from vbfa as b where b~vbelv = a~vbeln and b~vbtyp_n = 'N' ).
        else.
          select single a~vbeln a~mjahr
            from vbfa as a into (vl_vbeln,vl_mjahr)
           where a~vbelv = <fs_out>-remessa
             and a~vbtyp_n  = 'M'
             and a~vbtyp_v  = 'J'
             "Estorno
             and not exists ( select * from vbfa as b where b~vbelv = a~vbeln and b~vbtyp_n = 'N' ).
        endif.

        if sy-subrc = 0.
          w_success-ref_doc  = <fs_out>-remessa.
          w_success-bill_doc = vl_vbeln.
          append w_success to t_success.
        endif.

      endif.
    else.
      clear: it_tab_bapiret1[].

      call function 'BAPI_BILLINGDOC_CREATEMULTIPLE'
        tables
          billingdatain = t_billing
          textdatain    = t_textdatain
          return        = it_tab_bapiret1
          success       = t_success.
    endif.

    if t_success[] is not initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.

      wait up to 5 seconds.

      loop at t_success into w_success.

        try.
            assign it_saida_romaneios[ remessa = w_success-ref_doc ] to <fs_saida>.
            <fs_saida>-fatura = w_success-bill_doc.
            <fs_saida>-st_proc = vg_st_fatura.

            if <fs_saida>-operacao(4) = 'ZPAR'.

              update zsdt0001 set fatura_prod = <fs_saida>-fatura
                                  st_proc     = '99'
               where ch_referencia = <fs_saida>-ch_referencia.

            else.

              update zsdt0001 set fatura_prod = <fs_saida>-fatura
                                  st_proc     = <fs_saida>-st_proc
               where ch_referencia = <fs_saida>-ch_referencia.

            endif.

          catch cx_sy_itab_line_not_found.
        endtry.

      endloop.

      message |Remessa gerada sob. nº { w_success-bill_doc }.| type 'S'.

      "//Texto de cabeçalho
      if ( it_lines[] is not initial ).
        zid               = '0002'.
        zname             = w_success-bill_doc.
        x_header-tdobject = 'VBBK'.
        x_header-tdname   = zname.
        x_header-tdid     = zid.
        x_header-tdspras  = sy-langu.

        call function 'SAVE_TEXT'
          exporting
            client          = sy-mandt
            header          = x_header
            savemode_direct = 'X'
          tables
            lines           = it_lines
          exceptions
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            others          = 5.
      endif.

    else.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
          loop at it_tab_bapiret1 into data(lwa_bapiret).
            message id lwa_bapiret-id type 'I' number lwa_bapiret-number with lwa_bapiret-message_v1 lwa_bapiret-message_v2 lwa_bapiret-message_v3 lwa_bapiret-message_v4.
          endloop.
        when abap_true.
          l_mesg = abap_off.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = p_saida-ch_referencia i_type = 'E' i_tab_bapiret1 = it_tab_bapiret1 i_status = 'FATU' ).
      endcase.
*-#133089-21.02.2024-JT-fim
    endif.

    if p_tipo_chamada = 'L'.
      perform f_repare_docs_romaneio changing <fs_out>.
      perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      perform f_refresh_alv using '0100'. "Refresh na tela
    endif.

  elseif ( <fs_out>-fatura is not initial ) and ( <fs_out>-fatura ne icon_icon_list ) and
           vg_faturamento_autom = abap_off. "*-#133089-21.02.2024-JT
    if p_tipo_chamada eq 'L'.
      if <fs_out>-tipo = 'O'.
        set parameter id 'VF'    field <fs_out>-fatura.
        call transaction 'VF03' and skip first screen.
      else.
        select single vbeln mjahr
          into (vl_vbeln,vl_mjahr)
          from vbfa
         where vbelv = <fs_out>-remessa
           and vbtyp_n  = 'R'
           and vbtyp_v  = 'J'.


* ---> S4 Migration - 19/07/2023 - LO
*        SET PARAMETER ID 'MBN'    FIELD <fs_out>-fatura.
*        SET PARAMETER ID 'MJA'    FIELD vl_mjahr.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        call function 'MIGO_DIALOG'
          exporting
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ''
            i_skip_first_screen = 'X'
            i_deadend           = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = <fs_out>-fatura
            i_mjahr             = vl_mjahr
          exceptions
            illegal_combination = 1
            others              = 2.
* <--- S4 Migration - 19/07/2023 - LO

      endif.
    endif.
  endif.

endform.

form f_prepare_return tables p_return  structure bapireturn1.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  clear: tg_log_erro[].

  delete p_return where type ne 'E'.

  loop at p_return.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-id.
    tg_log_erro-msgnr  = p_return-number.
    tg_log_erro-msgv1  = p_return-message.
    append tg_log_erro.
  endloop.

*-#133089-21.02.2024-JT-inicio
  if vg_faturamento_autom = abap_true.
    data: t_ret type table of bapiret2.
    t_ret[] = t_return_vt[].
    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_zsdt0001-ch_referencia i_type = 'E' i_tab_bapiret2 = t_ret i_status = 'TRAN' ).
  endif.
*-#133089-21.02.2024-JT-fim

endform.

form f_prepare_return2 tables p_return structure bapiret2.

  clear: tg_log_erro[].

  delete p_return where type ne 'E'.

  loop at p_return.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-id.
    tg_log_erro-msgnr  = p_return-number.
    tg_log_erro-msgv1  = p_return-message.
    append tg_log_erro.
  endloop.

endform.

form f_prepare_return3 tables p_return structure bdcmsgcoll.

  data: vl_message_return type char600.

  clear: tg_log_erro[].

  delete p_return where msgtyp ne 'E'.

  loop at p_return.

    message id     p_return-msgid
            type   p_return-msgtyp
            number p_return-msgnr
            with   p_return-msgv1 p_return-msgv2 p_return-msgv3 p_return-msgv4
            into   vl_message_return.

    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid	 = p_return-msgid.
    tg_log_erro-msgnr  = p_return-msgnr.
    tg_log_erro-msgv1  = vl_message_return.
    append tg_log_erro.

  endloop.

endform.

form f_grava_log_erro tables p_return structure tg_log_erro
                       using p_saida  type ty_saida.

  clear: ti_zlest0100[], vl_ponteiro.

  select max( cont )
    from zlest0100 into vl_ponteiro
   where ch_referencia = p_saida-ch_referencia.

  if sy-subrc = 0.
    add 1 to vl_ponteiro.
  else.
    vl_ponteiro = 1.
  endif.

  loop at p_return.
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

    append wa_zlest0100 to ti_zlest0100.
    add 1 to vl_ponteiro.
  endloop.

  modify zlest0100 from table ti_zlest0100.


endform.

form f_estorno_fatura using p_fatura like bapivbrksuccess-bill_doc "bapivbrk-ref_doc
                            p_frete  type c
                            p_saida  type ty_saida
                   changing p_erro.

  clear: t_success[], t_return[], is_cancelled, p_erro.

  if ( p_fatura  is initial             ) or
     ( p_fatura  eq icon_execute_object ) or
     ( p_fatura  eq icon_icon_list      ).
    p_erro = 'X'.
    message w000(z01) with 'Número da fatura não atribuído!'.
    exit.
  endif.

  if p_frete is not initial.
    select single j_1bnfdoc~bukrs j_1bnflin~docnum
      into (vl_bukrs,vl_docnum)
      from j_1bnflin inner join j_1bnfdoc on j_1bnfdoc~docnum = j_1bnflin~docnum
     where j_1bnflin~refkey = p_saida-fatserv.

    select single * from zcte_ciot into wa_zcte_ciot where docnum eq vl_docnum.
    if ( sy-subrc eq 0 ).
      if ( ( wa_zcte_ciot-st_ciot ne 8 ) and
           ( wa_zcte_ciot-st_ciot ne 0 ) or
           ( wa_zcte_ciot-st_ciot ne 0 ) and
           ( wa_zcte_ciot-st_ciot ne 8 ) ) and
           ( wa_zcte_ciot-st_ciot ne 9 )  and
           ( wa_zcte_ciot-st_ciot ne 3 ).
        p_erro = 'X'.
        message i000(z01) with 'Necessário cancelar a viagem. Documento: ' vl_docnum.
        return.
      endif.
    endif.
  endif.

  "*---> 19/07/2023 - Migração S4 - LO
  "//Cancela fatura
*  CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*    EXPORTING
*      billingdoc_number       = p_fatura
*    IMPORTING
*      billingdoc_is_cancelled = is_cancelled.

  data lwa_bill_detail      type bapivbrkout.
  data lwa_return           type bapireturn1.

  call function 'BAPI_BILLINGDOC_GETDETAIL'
    exporting
      billingdocument       = p_fatura
    importing
      billingdocumentdetail = lwa_bill_detail
      return                = lwa_return.
  is_cancelled = lwa_bill_detail-cancelled.
  "*<--- 19/07/2023 - Migração S4 - LO


  if ( is_cancelled is initial ).
    "Cancela fatura
    call function 'ZBAPI_BILLINGDOC_CANCEL1'
      exporting
        billingdocument = p_fatura
      tables
        return          = t_return         " bapireturn1 Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
  endif.

  if ( t_success[] is not initial ) or ( is_cancelled is not initial ).

    perform f_chk_estorno_fiscal using p_saida
                                       p_frete
                              changing p_erro.

    if p_erro is initial.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = c_x.

      wait up to 5 seconds.

      if p_frete is not initial.

        p_saida-fatserv  = icon_icon_list.
        p_saida-dacte    = icon_execute_object.

        loop at p_saida-romaneios_agr into data(_wl_rom).

          update zsdt0001 set st_proc      = vg_st_fatura_frete_before
                              fatura_frete = ''
                              nro_nf_frete = ''
           where ch_referencia = _wl_rom-ch_referencia.

          read table it_saida assigning field-symbol(<fs_saida_tmp>) with key ch_referencia = _wl_rom-ch_referencia.
          if sy-subrc eq 0.
            <fs_saida_tmp>-fatserv  = icon_icon_list.
          endif.

        endloop.

      else.
        p_saida-fatura = icon_execute_object.
        p_saida-danfe  = icon_execute_object.

        update zsdt0001 set st_proc      = vg_st_fatura_before
                            fatura_prod  = ''
                            nro_nf_prod  = ''
         where ch_referencia = p_saida-ch_referencia.
      endif.

    else.
      p_erro = 'X'.
      exit.
    endif.

  else.
    p_erro = 'X'.
    "Gravar Log Erro
    read table t_return with key type = 'E'.
    if sy-subrc eq 0.
      perform f_prepare_return tables t_return.
      perform f_grava_log_erro tables tg_log_erro
                                using p_saida.
      exit.
    endif.
  endif.

endform.

form f_estorno_picking_rem using p_vbeln type likp-vbeln
                        changing p_erro
                                 p_saida type ty_saida.

  data: fp_budat     type sy-datlo,
        fp_tcode     type sy-tcode   value 'VL09',
        fp_vbtyp     type likp-vbtyp value 'J',
        lc_biodiesel type char01.        "*-#155161-21.10.2024-JT-inicio - Comentado codigo 24/12/2024

  data: it_mesg     type standard table of mesg,
        tl_bapiret2 type bapiret2_t.

  clear: p_erro, it_mesg[].

  check p_saida-operacao(4) ne 'ZTER'.

  if ( p_vbeln  is initial             ) or
     ( p_vbeln  eq icon_execute_object ) or
     ( p_vbeln  eq icon_icon_list      ).
    p_erro = 'X'.
    message w000(z01) with 'Número da remessa não atribuído!'.
    exit.
  endif.

  "Verifica se picking já não foi estornado.
  select single vbeln mjahr
    from vbfa into (vl_vbeln,vl_mjahr)
   where vbelv = p_vbeln
     and vbtyp_n  = 'h'
     and vbtyp_v  = 'J'.

  check sy-subrc ne 0. "Prosseguir se não encontrou estorno mov. mercadoria

  select single *
    from likp into @data(_wl_likp)
   where vbeln eq @p_vbeln.

  check ( sy-subrc eq 0 ) and ( _wl_likp-wadat_ist is not initial ). "Existe Picking.

  fp_budat = sy-datlo.

  call function 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
    exporting
      i_vbeln                   = p_vbeln
      i_budat                   = fp_budat
      i_tcode                   = fp_tcode
      i_vbtyp                   = fp_vbtyp
    tables
      t_mesg                    = it_mesg
    exceptions
      error_reverse_goods_issue = 1
      others                    = 2.

  if sy-subrc ne 0.
    p_erro = 'X'.
    message id sy-msgid type 'I' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.

  call function 'BAPI_TRANSACTION_COMMIT'
    exporting
      wait = 'X'.

  wait up to 3 seconds.

*-#155161-21.10.2024-JT-inicio - Comentado codigo 24/12/2024
*---------------------------------------------------
*-Estornar Doc.Material Biodiesel
*---------------------------------------------------
  try.
      lc_biodiesel = zcl_faturamento=>zif_faturamento~get_instance(
                  )->set_estornar_biodiesel_frota( p_saida-ch_referencia
                  ).
    catch zcx_error into data(lc_error).
      p_erro = 'X'.
      message id lc_error->msgid type 'I' number lc_error->msgno with lc_error->msgv1 lc_error->msgv2 lc_error->msgv3 lc_error->msgv4.
      exit.
  endtry.
*-#155161-21.10.2024-JT-fim

endform.

form f_estorno_remessa using p_vbeln type likp-vbeln
                    changing p_erro
                             p_saida type ty_saida.

  data: it_mesg     type standard table of mesg,
        sl_hdata    type bapiobdlvhdrchg,
        sl_hcont    type bapiobdlvhdrctrlchg,
        tl_bapiret2 type bapiret2_t.

  clear: p_erro, it_mesg[].

  if ( p_vbeln  is initial             ) or
     ( p_vbeln  eq icon_execute_object ) or
     ( p_vbeln  eq icon_icon_list      ).
    p_erro = 'X'.
    message w000(z01) with 'Número da remessa não atribuído!'.
    exit.
  endif.

  "Deleta Delivery Criado
  sl_hdata-deliv_numb = p_vbeln.
  sl_hcont-deliv_numb = p_vbeln.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = p_vbeln. "Projeto S4 Hana - WPP

  clear: tl_bapiret2[].

  call function 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
    exporting
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    tables
      return         = tl_bapiret2.

  if sy-subrc ne 0.
    p_erro = 'X'.
    message id sy-msgid type 'I' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.

  loop at tl_bapiret2 into data(wl_bapiret) where type = 'E'.
    p_erro = 'X'.
    message id wl_bapiret-id type 'I' number wl_bapiret-number with wl_bapiret-message_v1 wl_bapiret-message_v2 wl_bapiret-message_v3 wl_bapiret-message_v4.
    return.
  endloop.

  call function 'BAPI_TRANSACTION_COMMIT'
    exporting
      wait = 'X'.

  " FAZ O RESTORNO DO RESÍDUO.
  if p_saida-ch_referencia is not initial .

    select single *
      from zsdt0001 into @data(w_romaneio)
    where ch_referencia = @p_saida-ch_referencia.

    if w_romaneio-id_carga is initial.

      perform f_estorno_res changing w_romaneio.


    endif.



  endif.


  wait up to 3 seconds.

  if p_saida-operacao+0(4) eq 'ZRDC'.
    submit zsdi0006 with p_vbeln = p_vbeln
                    with p_vinc  = ''
                    with p_desc  = 'X'
                    with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
                    and return.
  endif.

endform.

form f_estorno_res  changing p_zsdt0001 type zsdt0001.
  data: w_romaneio               type zsdt0001,
        wa_mat_doc               type bapi2017_gm_head_02-mat_doc,
        wa_doc_year              type bapi2017_gm_head_02-doc_year,
        wa_pstng_date            type bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo type bapi2017_gm_head_ret,
        v_budat                  type mkpf-budat,
        w_mseg                   type mseg.

  data: wa_goodsmvt_header type bapi2017_gm_head_01,
        t_goodsmvt_item    type table of bapi2017_gm_item_create,
        wa_goodsmvt_item   type bapi2017_gm_item_create,
        wa_code            type bapi2017_gm_code,
        vl_mat_doc         type bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear type bapi2017_gm_head_ret-doc_year,
        t_return_vt        like bapiret2 occurs 0 with header line.


  "Estorna entrada de Residuo
  select single *
    from zsdt0001
    into w_romaneio
  where ch_referencia = p_zsdt0001-ch_referencia.

  if w_romaneio-doc_material_e is not initial. "doc. material entrada re  siduo existe
    refresh t_return_vt.
    select single budat into v_budat
      from mkpf
      where mblnr = w_romaneio-doc_material_e
    and   mjahr = w_romaneio-ano_material_e.
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
    call function 'BAPI_GOODSMVT_CANCEL'
      exporting
        materialdocument    = wa_mat_doc
        matdocumentyear     = wa_doc_year
        goodsmvt_pstng_date = wa_pstng_date
      importing
        goodsmvt_headret    = vg_invoicedocnumber_migo
      tables
        return              = t_return_vt.
*    ENDIF.

    if t_return_vt[] is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = c_x.
      update zsdt0001
           set doc_material_e      = ''
               ano_material_e      = ''
         where ch_referencia = p_zsdt0001-ch_referencia.
    else.
      "gravar log
      read table t_return_vt with key type = 'E'.
      if sy-subrc eq 0.
        refresh ti_zlest0100.
        clear vl_ponteiro.
        select  max( cont )
         from zlest0100
         into vl_ponteiro
        where ch_referencia = p_zsdt0001-ch_referencia.

        if sy-subrc = 0.
          add 1 to vl_ponteiro.
        else.
          vl_ponteiro = 1.
        endif.
        loop at t_return_vt.
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

          append wa_zlest0100 to ti_zlest0100.
          add 1 to vl_ponteiro.
        endloop.
        modify zlest0100 from table ti_zlest0100.
      endif.
    endif.
  endif.
endform.

form f_excluir_remessa using p_vbeln type likp-vbeln
                                  p_saida type ty_saida
                         changing p_erro.

  clear: p_erro, ti_bdcdata[].

  if ( p_vbeln  is initial             ) or
     ( p_vbeln  eq icon_execute_object ) or
     ( p_vbeln  eq icon_icon_list      ).
    p_erro = 'X'.
    message w000(z01) with 'Número da remessa não atribuído!'.
    exit.
  endif.

  perform f_bdc_data using:
      'SAPMV50A'  '4004'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'       '/00',
      ''          ''      ''   'LIKP-VBELN'       p_vbeln,

      'SAPMV50A'  '1000'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'       '/ELOES_T'.

  perform f_call_transaction using 'VL02N'
                                   p_saida
                          changing p_erro.
  if p_erro is initial.
    commit work.

    "Elimina vinculo DCO-remessa
    if p_saida-operacao+0(4) eq 'ZRDC'.
      submit zsdi0006 with p_vbeln = p_vbeln
                      with p_vinc  = ''
                      with p_desc  = 'X'
                      with p_fataut = vg_faturamento_autom  "*-#133089-21.02.2024-JT
                      and return.
    endif.

    perform f_after_estorno_remessa changing p_saida.

  else.
    message 'Erro ao estornar, remessa sem item! ' type 'I'.
  endif.

endform.

form f_after_estorno_remessa changing p_saida type ty_saida.

  data: v_cd_uf type zlest0002-cd_uf.

  check p_saida is not initial.

  clear p_saida-region.

  select single cd_uf
    from zlest0002 into v_cd_uf
   where pc_veiculo = p_saida-placa_cav.

  if sy-subrc = 0.
    p_saida-region  = v_cd_uf.
  endif.

  case vg_cockpit.
    when '04'.
      if p_saida-tipo = 'O' and p_saida-inco1 = 'FOB'.
        update zsdt0001 set st_proc      = vg_st_aviso_rec_before
                            region       = v_cd_uf
                            doc_rem      = ''
                            status       = ''
                            agente_frete = ''
                            konwa        = ''
                            kbetr        = 0
        where ch_referencia = p_saida-ch_referencia.

        if p_saida-ch_referencia is not initial.
          update zlest0155 set ch_referencia = space where ch_referencia = p_saida-ch_referencia.
        endif.

      else.
        update zsdt0001 set st_proc      = vg_st_remessa_before
                            doc_rem      = ''
                            status       = ''
        where ch_referencia = p_saida-ch_referencia.

        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
*        IF ( p_saida-emite_conhecimento = abap_false ) AND ( p_saida-ovserv(1) = '@' ).
*
*          UPDATE zsdt0001 SET st_proc = vg_st_custo
*           WHERE ch_referencia = p_saida-ch_referencia.
*
*        ENDIF.
        "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP

      endif.

    when '03' or '09'.
      update zsdt0001 set st_proc  = vg_st_remessa_before
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
       where ch_referencia = p_saida-ch_referencia.

      if p_saida-ch_referencia is not initial.
        update zlest0155 set ch_referencia = space where ch_referencia = p_saida-ch_referencia.
      endif.

    when others.
      update zsdt0001 set st_proc      = vg_st_remessa_before
                          region       = v_cd_uf
                          doc_rem      = ''
                          status       = ''
                          agente_frete = ''
                          konwa        = ''
                          kbetr        = 0
      where ch_referencia = p_saida-ch_referencia.

      if p_saida-ch_referencia is not initial.
        update zlest0155 set ch_referencia = space where ch_referencia = p_saida-ch_referencia.
      endif.

  endcase.

  p_saida-st_proc = ''.
  p_saida-remessa = icon_execute_object.
  p_saida-lifnr   = ''.

  if p_saida-operacao+0(3) eq 'ZUB'. "Pedido de transferencia
    p_saida-fatura  = icon_execute_object.
    p_saida-danfe   = icon_execute_object.

    update zsdt0001 set fatura_prod = ''
                        nro_nf_prod = ''
    where ch_referencia eq p_saida-ch_referencia.

  endif.

  if line_exists( t_fatura_agrupada[ werks = p_saida-branch kunnr = p_saida-kunnr inco1 = vinco1 cfop = p_saida-cfop ] ).
    try.
        it_saida = it_saida[ dt_movimento = p_saida-dt_movimento
                             kunnr        = p_saida-kunnr
                             operacao(4)  = p_saida-operacao(4)
*                           INCO1        = VINCO1
                             cfop         = p_saida-cfop
                             remessa      = icon_execute_object
                           ].
      catch cx_sy_itab_line_not_found.
        p_saida-remessa = icon_execute_object.
    endtry.
  endif.


endform.

form f_estorno_migo  using p_par_est_migo type ty_par_est_migo
                           p_saida        type ty_saida
                  changing p_erro.

  data: t_return like bapiret2 occurs 0 with header line.

  data: wl_invoicedocnumber_migo type bapi2017_gm_head_ret.

  clear: p_erro.

  if ( p_par_est_migo-mat_doc is initial ).
    p_erro = 'X'.
    message w000(z01) with 'Número do Migo não atribuído!'.
    exit.
  endif.

  "BAPI Estorno da MIGO
  clear: t_return[].

  select single *
    from mseg into @data(lwa_mseg_estorno)
   where smbln eq @p_par_est_migo-mat_doc.

  if sy-subrc eq 0.

    wl_invoicedocnumber_migo-mat_doc   = lwa_mseg_estorno-mblnr.
    wl_invoicedocnumber_migo-doc_year  = lwa_mseg_estorno-mjahr.

  else.
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
    call function 'ZSD_GOODSMVT_CANCEL'
      starting new task p_par_est_migo-mat_doc
      performing receive_results_goodsmvt on end of task
      exporting
        materialdocument    = p_par_est_migo-mat_doc
        matdocumentyear     = p_par_est_migo-doc_year
        goodsmvt_pstng_date = p_par_est_migo-pstng_date
      tables
        return              = t_return.

    wait until results_received = 'X' up to 60 seconds.

    t_return[] = t_return_goods_cnc[].
    wl_invoicedocnumber_migo = g_migo_number.

    free: t_return_goods_cnc[], g_migo_number, results_received.

    if wl_invoicedocnumber_migo-mat_doc is not initial.
*<-- 13/09/20223 - Migração S4 - DA - Fim
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = c_x.
    else.
      p_erro = 'X'.

      "Gravar Log erro
      read table t_return with key type = 'E'.
      if sy-subrc eq 0.
        perform f_prepare_return tables t_return.
        perform f_grava_log_erro tables tg_log_erro using p_saida.
        exit.
      endif.
    endif.

  endif.

  check p_erro is initial.

  "Tramento Para Remessa Formação de Lote
  if p_par_est_migo-form_lote is not initial.

    case p_par_est_migo-ent_sai.
      when 'E'. "Entrada
        update zsdt0023 set es_mblnr_e = wl_invoicedocnumber_migo-mat_doc
                            es_mjahr_e = wl_invoicedocnumber_migo-doc_year
         where vbeln = p_saida-remessa.
      when 'S'. "Saída
        update zsdt0023 set es_mblnr_s = wl_invoicedocnumber_migo-mat_doc
                            es_mjahr_s = wl_invoicedocnumber_migo-doc_year
         where vbeln = p_saida-remessa.
    endcase.

  endif.

endform.

form receive_results_goodsmvt using i_taskname.

  receive results from function 'ZSD_GOODSMVT_CANCEL'
    importing
      goodsmvt_headret    = g_migo_number
    tables
      return              = t_return_goods_cnc.

  results_received = abap_true.


endform.








form f_estorno_ov_frete using p_ovserv type vbeln_va
                              p_saida  type ty_saida
                     changing p_erro.
  clear: p_erro.

  if ( p_saida-ovserv  is initial             ) or
     ( p_saida-ovserv  eq icon_execute_object ) or
     ( p_saida-ovserv  eq icon_icon_list      ).
    p_erro = 'X'.
    message w000(z01) with 'Número da OV. de Frete não atribuído!'.
    exit.
  endif.

  "Bloqueia Ordem do frete
  clear: wl_orderheaderin, wl_orderheaderinx, tl_bapiparex[], sl_bapiparex, wl_bape_vbak.

  wl_bape_vbak-vbeln           = p_saida-ovserv.
  wl_bape_vbak-tknum           = ''.
  sl_bapiparex-structure       = 'BAPE_VBAK'.
  sl_bapiparex-valuepart1      = wl_bape_vbak.
  append sl_bapiparex to tl_bapiparex.

  clear sl_bapiparex.
  wl_bape_vbakx-vbeln          = p_saida-ovserv.
  wl_bape_vbakx-tknum          = 'X'.
  sl_bapiparex-structure       = 'BAPE_VBAKX'.
  sl_bapiparex-valuepart1      = wl_bape_vbakx.
  append sl_bapiparex to tl_bapiparex.

  wl_orderheaderin-bill_block  = '10'.
  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderinx-bill_block = 'X'.

  call function 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    exporting
      salesdocument    = p_ovserv
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    tables
      return           = t_return_vt
      extensionin      = tl_bapiparex.

  read table t_return_vt with key type = 'E'.

  if sy-subrc ne 0.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_x.

    p_saida-ovserv = icon_icon_list.

    loop at p_saida-romaneios_agr into data(_wl_rom).
      update zsdt0001 set st_proc      = vg_st_ov_frete_before
                          ov_frete     = ''
       where ch_referencia = _wl_rom-ch_referencia.

      read table it_saida assigning field-symbol(<fs_saida_tmp>) with key ch_referencia = _wl_rom-ch_referencia.
      if sy-subrc eq 0.
        <fs_saida_tmp>-ovserv = icon_icon_list.
      endif.
    endloop.

  else.

    p_erro = 'X'.

    read table t_return_vt with key type = 'E'.
    if sy-subrc eq 0.
      perform f_prepare_return tables t_return_vt.
      perform f_grava_log_erro tables tg_log_erro
                                using p_saida.
      exit.
    endif.

  endif.

endform.

form f_call_carta_correcao.

  clear: tl_rows[].

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha apenas' type 'I'.
    exit.
  endif.

  read table tl_rows into sl_rows index 1.
  read table it_saida into wa_saida index sl_rows-index.

  if wa_saida-transp ne icon_execute_object.
    message i000(z01) with 'Dados de transporte '
                           'deve ser estornado'.
    exit.
  endif.

  if wa_saida-danfe eq icon_execute_object.
    message i000(z01) with 'Não existe DANFE'
                           'Autorizado'.
    exit.
  endif.

  select single *
    from lfa1 into @data(wa_agente)
   where lifnr = @wa_saida-lifnr.

  if sy-subrc = 0.
    wa_ag_frete-transpor1  = wa_agente-lifnr.
    wa_ag_frete-name1      = wa_agente-name1.
    wa_ag_frete-cnpj1      = wa_agente-stcd1.
    wa_ag_frete-inscr1     = wa_agente-stcd3.
    call screen 0300     starting at 020 1
                         ending   at 145 15.
  endif.

endform.

form f_call_dados_transp .

  clear: tl_rows[].

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha apenas' type 'I'.
    exit.
  endif.

  read table tl_rows into sl_rows index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  if wa_saida-inco1  ne 'CIF'.
    message i000(z01) with 'Dados de transporte não disponível '
                           'para tipo de frete:'
                           wa_saida-inco1.
    exit.
  endif.

  clear: wa_trans,wa_mot.
  refresh it_veic.
  select single z~motorista la~name1 la~stcd2 la~stcd4 lb~zsabe la~stcd3 lb~eikto
    from zsdt0001 as z
    inner join lfa1 as la on la~lifnr = z~motorista
    inner join lfb1 as lb on lb~lifnr = z~motorista "AND LB~BUKRS = P_BUKRS
  into wa_mot
  where ch_referencia = wa_saida-ch_referencia.

  select single lifnr name1 stcd1
    from lfa1
    into wa_trans
    where lifnr = wa_saida-lifnr.

  select z1~placa_cav z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
    from zsdt0001 as z1
    inner join zlest0002 as z2 on z2~pc_veiculo  = z1~placa_cav
    inner join lfa1      as la on la~lifnr       = z2~proprietario
  into table it_veic
  where ch_referencia = wa_saida-ch_referencia.

  select z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     from zsdt0001 as z1
     inner join zlest0002 as z2 on z2~pc_veiculo  = z1~placa_car1
   inner join lfa1      as la on la~lifnr       = z2~proprietario
   appending table it_veic
   where ch_referencia = wa_saida-ch_referencia.

  select z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     from zsdt0001 as z1
     inner join zlest0002 as z2 on z2~pc_veiculo  = z1~placa_car2
   inner join lfa1      as la on la~lifnr       = z2~proprietario
   appending table it_veic
   where ch_referencia = wa_saida-ch_referencia.

  select z2~pc_veiculo z2~tp_veiculo z2~proprietario la~name1 la~bahns z2~cd_renavam la~stcd1 la~stcd2 z2~cd_cidade z2~cd_uf
     from zsdt0001 as z1
     inner join zlest0002 as z2 on z2~pc_veiculo  = z1~placa_car3
   inner join lfa1      as la on la~lifnr       = z2~proprietario
   appending table it_veic
   where ch_referencia = wa_saida-ch_referencia.

  call screen 0200     starting at 020 1
                       ending   at 140 23.

endform.

form f_call_danfe_dacte using p_tipo.

  "P_TIPO : 1 = Danfe
  "         2 = Dacte

  data: v_imp_doc type j_1bdocnum.

  clear: tl_rows[].

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para impressão' type 'I'.
    exit.
  endif.

  read table tl_rows into sl_rows index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  case p_tipo.
    when '1'. "Danfe.
      if ( wa_saida-danfe = icon_execute_object ).
        message 'Nota não gerada' type 'I'.
        exit.
      endif.

      v_imp_doc = wa_saida-danfe.
    when '2'. "Dacte

      if ( wa_saida-dacte = icon_execute_object ).
        message 'CTE não gerado' type 'I'.
        exit.
      endif.

      v_imp_doc = wa_saida-dacte.
  endcase.

  perform f_check_auth_doc using v_imp_doc.

  if sy-subrc eq 0.
    call function 'Z_SD_PRINT_NFE_CTE'
      exporting
        doc_numero     = v_imp_doc
      exceptions
        nao_localizado = 1
        others         = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  else.
    message |Documento: { v_imp_doc } não está autorizado!| type 'I'.
    exit.
  endif.


endform.

form f_call_estorno_cte .

  if sy-tcode ne 'ZLES0136'
    and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    and sy-tcode ne 'ZMM0127'.
    message 'Transação apenas de visualização' type 'I'.
    exit.
  endif.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para o estorno' type 'I'.
    exit.
  endif.

  read table tl_rows into sl_rows index 1.
  read table it_saida assigning field-symbol(<fs_out>) index sl_rows-index.

  check sy-subrc eq 0.

*----CS2021000508 - 07.06.2021 - JT - inicio
  if <fs_out>-troca_nota            = abap_true and
     <fs_out>-docs_enviado_carguero = abap_true.
    message 'Remover antes os documentos no Carguero, para estorno!' type  'I'.
    exit.
  endif.
*----CS2021000508 - 07.06.2021 - JT - fim

  " Processo estorno não completo
  if ( <fs_out>-st_proc eq vg_st_finalizado and
       <fs_out>-inco1   ne 'CPT' and ( not <fs_out>-enc_doc_custo eq abap_true )
     ) or " Finalizado
*----CS2021000508 - 07.06.2021 - JT - inicio
     ( <fs_out>-st_proc eq vg_st_aguard_doc_carg ) or " Aguardando envio carguero
*----CS2021000508 - 07.06.2021 - JT - fim
     ( <fs_out>-st_proc eq vg_st_fatura_frete  ) or " Fatura Frete
     ( <fs_out>-st_proc eq vg_st_ov_frete      ) or " OV.Frete
     ( <fs_out>-st_proc eq vg_st_custo         ) or " Doc.Custo
     ( <fs_out>-st_proc eq vg_st_transp        )." Transporte

    refresh ti_zlest0100.
    perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    select single st_proc
      from zsdt0001 into  <fs_out>-st_proc
     where ch_referencia = <fs_out>-ch_referencia.

    perform f_estorno_cte changing <fs_out>.
    if ti_zlest0100[] is not initial.
      <fs_out>-icon = icon_led_red.
    else.
      clear <fs_out>-icon.
    endif.

    perform f_repare_docs_romaneio changing <fs_out>.
    perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
    perform f_refresh_alv using '0100'. "Refresh na tela

    call method cl_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

  elseif ( <fs_out>-st_proc = vg_st_finalizado ) or
         ( <fs_out>-st_proc = vg_st_dacte ).

    if ( ( <fs_out>-inco1 = 'CPT' ) or
         ( <fs_out>-enc_doc_custo eq abap_true ) ) and ( <fs_out>-st_proc = vg_st_finalizado ). "Estorna mesmo finalizado se CPT

      perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      update zsdt0001 set st_proc = <fs_out>-st_proc
       where ch_referencia = <fs_out>-ch_referencia.

      perform f_estorno_custo changing <fs_out>.

      perform f_repare_docs_romaneio changing <fs_out>.
      perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio
      perform f_refresh_alv using '0100'. "Refresh na tela
    else.
      message 'Dacte deve ser não autorizada para este processo.' type  'I'.
    endif.

  else.
    message 'Não existe documentos de transporte para estorno' type  'I'.
  endif.


endform.

*----------------------------------------------------------------------
*-CS2021000117 - 28.04.2021 - JT - inicio
* exibir ordem de carregamento
*----------------------------------------------------------------------
form f_ordem_carregamento.

  data: l_id_ordem     type zde_id_ordem.

  free: wa_saida,
        l_id_ordem.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para Exibir Ordem de Carregamento' type 'I'.
    exit.
  endif.

  read table tl_rows  into sl_rows  index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  l_id_ordem = wa_saida-id_ordem.

  if l_id_ordem is initial.
    select id_ordem
      into l_id_ordem
      from zsdt0001
        up to 1 rows
     where ch_referencia = wa_saida-ch_referencia
       and tp_movimento  = wa_saida-tp_movimento.
    endselect.
  endif.

  if l_id_ordem is initial.
    message 'Ordem de Carregamento não disponível para visualização' type 'I'.
    exit.
  endif.

*----------------------------
* exibe ordem de carregamento
*----------------------------
  call function 'ZSD_EXIBE_ORDEM_CARREGAMENTO'
    exporting
      i_id_ordem = l_id_ordem.

endform.
*-CS2021000117 - 28.04.2021 - JT - fim

*----------------------------------------------------------------------
*-CS2021000656 - 14.05.2021 - JT - inicio
* imprimit selo
*----------------------------------------------------------------------
form f_imprimir_selo.

  data: l_id_ordem     type zde_id_ordem.
  data: l_nr_safra     type zsdt0001od-nr_safra.
  data: l_nr_safra2    type zpmt0054-safra.
  data : l_docnum      type j_1bnfdoc-docnum.

  free: wa_saida,
        l_nr_safra,
        l_id_ordem.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para Imprimir o Selo' type 'I'.
    exit.
  endif.

  read table tl_rows  into sl_rows  index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  l_docnum   = wa_saida-danfe.
  l_id_ordem = wa_saida-id_ordem.

  if l_id_ordem is initial.
    select id_ordem
      into l_id_ordem
      from zsdt0001
        up to 1 rows
     where ch_referencia = wa_saida-ch_referencia
       and tp_movimento  = wa_saida-tp_movimento.
    endselect.
  endif.

  select nr_safra
    into l_nr_safra
    from zsdt0001od
      up to 1 rows
   where id_ordem = l_id_ordem.
  endselect.

  l_nr_safra2 = l_nr_safra.

  call function 'ZSD_IMPRIME_SELO'
    exporting
      i_docnum                 = l_docnum
      i_safra                  = l_nr_safra2
      i_imprime_selo           = 'X'
    exceptions
      documento_nao_autorizado = 1
      documento_nao_imprimir   = 2
      others                   = 3.

  if sy-subrc <> 0.
    message 'Selo não pode ser impresso.' type 'I'.
    exit.
  endif.

endform.
*-CS2021000656 - 14.05.2021 - JT - fim


*-CS2021000218-16.11.2022-#90706-JT-inicio
*----------------------------------------------------------------------
* imprimit RA ASSINADA
*----------------------------------------------------------------------
form f_imprimir_ra_assinada.

  free: wa_saida.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para Imprimir R.A. assinada' type 'I'.
    exit.
  endif.

  read table tl_rows  into sl_rows  index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 50
      text       = |Verificando documentos. Aguarde...|.

*----------------------------------
* Imprimir RA
*----------------------------------
  call function 'ZSD_EXIBIR_RA_ASSINADA'
    exporting
      i_nro_cg        = wa_saida-nro_cg
      i_ch_referencia = wa_saida-ch_referencia
    exceptions
      pdf_not_found   = 1
      others          = 2.

  if sy-subrc <> 0.
    message 'Receituário Agronômico não pode ser impresso.' type 'I'.
    exit.
  endif.

endform.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
*----------------------------------------------------------------------
* imprimir Romaneio Algodão
*----------------------------------------------------------------------
form f_impr_romaneio_algodao.

  free: wa_saida.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para Imprimir Rom.Algodão' type 'I'.
    exit.
  endif.

  read table tl_rows  into sl_rows  index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 50
      text       = |Verificando documentos. Aguarde...|.

*----------------------------------
* Imprimir Romaneio Algodão
*----------------------------------
  call function 'ZSD_IMPRIME_REL_ROMANEIO'
    exporting
      i_ch_referencia           = wa_saida-ch_referencia
      i_imprime                 = abap_true
    exceptions
      romaneio_nao_encontrado   = 1
      romaneio_em_estorno       = 2
      erro_impressao_formulario = 3
      others                    = 4.

  if sy-subrc <> 0.
    message 'Romaneio Algodão não pode ser impresso.' type 'I'.
    exit.
  endif.

endform.
*-CS2023000189-26.05.2023-#108752-JT-fim

form f_call_estorno_nfe.

  data index_estorno type sy-tabix.

  if sy-tcode ne 'ZLES0136'
    and sy-tcode ne 'ZSDT0112' "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    and sy-tcode ne 'ZMM0127'.
    message 'Transação apenas de visualização' type 'I'.
    exit.
  endif.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para o estorno.' type 'I'.
    exit.
  endif.

  data(_saida_aux) = it_saida[ tl_rows[ 1 ]-index ].

  if line_exists( t_fatura_agrupada[ werks = _saida_aux-branch kunnr = _saida_aux-kunnr inco1 = _saida_aux-inco1 cfop = _saida_aux-cfop ] ).

    clear tl_rows[].
    loop at it_saida into it_saida where ( dt_movimento eq _saida_aux-dt_movimento ) and
                                         ( matnr        eq _saida_aux-matnr        ) and
                                         ( kunnr        eq _saida_aux-kunnr        ) and
                                         ( operacao(4)  eq _saida_aux-operacao(4)  ) and
*                                        ( INCO1        EQ VINCO1                  ) AND
                                         ( cfop         eq _saida_aux-cfop         ).

      check ( it_saida-remessa is not initial and it_saida-remessa ne icon_execute_object
         or   it_saida-fatura  is not initial and it_saida-fatura  ne icon_execute_object ).

      append value #( index = sy-tabix ) to tl_rows.
    endloop.

    if lines( tl_rows ) > 1.
      call function 'POPUP_TO_CONFIRM'
        exporting
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
        importing
          answer                = w_answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check w_answer = 1.
    endif.
  endif.


  clear index_estorno.
  loop at tl_rows into sl_rows.
    read table it_saida assigning field-symbol(<fs_out>) index sl_rows-index.

    add 1 to index_estorno.


*-------------------------------------------------------------------------------------* BUG PRD / AOENNING.
*============>          RECUPERA DOC MATERIAL VINC.REMESSA.  <===========  *
*-------------------------------------------------------------------------------------*
    perform f_get_doc_mat_rem_zsdt0023 changing <fs_out>.


    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = |Estornando documento(s) { index_estorno } de { lines( tl_rows ) }.|.

*----CS2021000508 - 07.06.2021 - JT - inicio
    if <fs_out>-troca_nota            = abap_true and
       <fs_out>-docs_enviado_carguero = abap_true.
      message 'Remover antes os documentos no Carguero, para estorno!' type  'I'.
      continue.
    endif.
*----CS2021000508 - 07.06.2021 - JT - fim

    if <fs_out>-operacao(4) = 'ZPAR'.

      if ( <fs_out>-st_proc eq vg_st_fatura  ) or " Fatura
         ( <fs_out>-st_proc eq vg_st_remessa ) or " Remessa
         ( <fs_out>-st_proc eq vg_st_finalizado ) .  " Finalizado

        perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.

        select single st_proc
          from zsdt0001 into  <fs_out>-st_proc
         where ch_referencia = <fs_out>-ch_referencia.

        if ( <fs_out>-st_proc eq vg_st_fatura  ) or " Fatura
           ( <fs_out>-st_proc eq vg_st_remessa ) or " Remessa
           ( <fs_out>-st_proc eq vg_st_finalizado ) .  " Finalizado

          if <fs_out>-st_proc eq vg_st_finalizado.
            <fs_out>-st_proc = vg_st_fatura.
          endif.

          perform f_estorno_nfe changing <fs_out>.

          if ti_zlest0100[] is not initial.
            <fs_out>-icon = icon_led_red.
          else.
            clear <fs_out>-icon.
          endif.
        endif.

        refresh style.
        if <fs_out>-lifnr is initial.

          if ( t_fatura_agrupada is not initial ) and
             ( 'CFR_FOB' cs <fs_out>-inco1      ) and
             ( <fs_out>-inco1 is not initial ).

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            insert wa_style into table style .
          else.
            delete <fs_out>-style where fieldname eq 'LIFNR'.
          endif.
        else.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.

        if <fs_out>-region is initial.
          delete <fs_out>-style where fieldname eq 'REGION'.
        else.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.

        <fs_out>-style = style[].

        perform f_repare_docs_romaneio changing <fs_out>.
        perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio


      else.
        message 'Não existem documentos referentes a NF-e para estorno.' type  'I'.
      endif.

    else.

      " Processo estorno não completo.
      if ( <fs_out>-st_proc eq vg_st_custo     ) or " Doc.Custo
         ( <fs_out>-st_proc eq vg_st_transp    ) or " Transporte
         ( <fs_out>-st_proc eq vg_st_fatura    ) or " Fatura
         ( <fs_out>-st_proc eq vg_st_aviso_rec ) or " Aviso
         ( <fs_out>-st_proc eq vg_st_remessa   ).   " Remessa

        perform f_lock_rom using 'B' <fs_out>-ch_referencia. "Bloqueia romaneio
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.

        select single st_proc
          from zsdt0001 into  <fs_out>-st_proc
         where ch_referencia = <fs_out>-ch_referencia.

        refresh ti_zlest0100.
        if ( ( <fs_out>-st_proc eq vg_st_custo   ) or " Doc.Custo
             ( <fs_out>-st_proc eq vg_st_transp  ) ) and " Transporte

           ( ( <fs_out>-inco1 = 'CPT' ) or
             ( <fs_out>-enc_doc_custo eq abap_true ) ).

          <fs_out>-st_proc = vg_st_custo.

          update zsdt0001 set st_proc = <fs_out>-st_proc
           where ch_referencia = <fs_out>-ch_referencia.

          perform f_estorno_custo changing <fs_out>.
        endif.

        if ( <fs_out>-st_proc eq vg_st_fatura    ) or " Fatura
           ( <fs_out>-st_proc eq vg_st_aviso_rec ) or " Aviso
           ( <fs_out>-st_proc eq vg_st_remessa   ).   " Remessa

          perform f_estorno_nfe changing <fs_out>.

          if ti_zlest0100[] is not initial.
            <fs_out>-icon = icon_led_red.
          else.
            clear <fs_out>-icon.
          endif.
        endif.

        refresh style.
        if <fs_out>-lifnr is initial.
          if ( t_fatura_agrupada is not initial ) and
             ( 'CFR_FOB' cs <fs_out>-inco1      ) and
             ( <fs_out>-inco1 is not initial ).

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            insert wa_style into table style .
          else.
            delete <fs_out>-style where fieldname eq 'LIFNR'.
          endif.
        else.
          wa_style-fieldname = 'LIFNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.

        if <fs_out>-region is initial.
          delete <fs_out>-style where fieldname eq 'REGION'.
        else.
          wa_style-fieldname = 'REGION'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          insert  wa_style into table style .
        endif.

        <fs_out>-style = style[].

        perform f_repare_docs_romaneio changing <fs_out>.
        perform f_lock_rom using 'D' <fs_out>-ch_referencia. "Desbloqueia romaneio

      else.
        message 'Não existem documentos referentes a NF-e para estorno.' type  'I'.
      endif.

    endif.

  endloop.

  perform f_refresh_alv using '0100'. "Refresh na tela

endform. "FORM f_call_estorno_nfe

form f_repare_docs_romaneio_sel.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) eq 0.
    message 'Selecione pelo menos uma linha!' type 'I'.
    exit.
  endif.

  loop at tl_rows into sl_rows.

    read table it_saida assigning field-symbol(<fs_out>) index sl_rows-index.

    check sy-subrc = 0.

    perform f_repare_docs_romaneio changing <fs_out>.

  endloop.

  perform f_refresh_alv using '0100'. "Refresh na tela

endform.

form f_repare_docs_romaneio changing c_saida type ty_saida.

  check c_saida is not initial.

  select single *
    from zsdt0001 into @data(lwa_zsdt0001_current)
   where ch_referencia eq @c_saida-ch_referencia.

  check sy-subrc eq 0.

  data(lwa_zsdt0001_new) = lwa_zsdt0001_current.

*-------------------------------------------------------------------------------------*
*============>                   DOC. ZNFW                               <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_doc_znfw_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                  DANFE ZNFW                               <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_danfe_znfw_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   AVISO                                  <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_aviso_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   REMESSA                                <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_doc_rem_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   FATURA NF-e                            <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_fat_nf_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   DANFE                                  <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_danfe_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               DOC. TRANSPORTE                            <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_tknum_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               DOC. CUSTO                                 <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_fknum_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               OV. SERVIÇO                                <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_ov_serv_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>               FATURA. FRETE                              <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_fat_serv_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>                   DACTE                                  <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_dacte_romaneio changing c_saida lwa_zsdt0001_new.


*-------------------------------------------------------------------------------------*
*============>                   AGENTE FRETE                          <=========== *
*-------------------------------------------------------------------------------------*

  perform f_get_agente_fre_romaneio changing c_saida lwa_zsdt0001_new.

*-------------------------------------------------------------------------------------*
*============>          RECOMPOSIÇÃO DE STATUS.                         <===========  *
*-------------------------------------------------------------------------------------*


  if ( c_saida-remessa  is not initial and c_saida-remessa(1) ne '@' ) or
     ( c_saida-seq_lcto is not initial and c_saida-seq_lcto(1) ne '@' ).
    lwa_zsdt0001_new-status = abap_true.
  endif.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
  zcl_comercializacao_algodao=>get_mov_sobra_perda_romaneio(
    exporting
      i_ch_referencia = conv #( lwa_zsdt0001_new-ch_referencia )
    importing
      e_mblnr         = data(e_mblnr_perda_sobra) ).
  if e_mblnr_perda_sobra is not initial.
    lwa_zsdt0001_new-status = abap_true.
  endif.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

  case vg_cockpit. "Recomposição de Status
    when '01' or '05' or '06' or '07' or '03' or '09'.

      perform f_build_status_romaneio_01 using lwa_zsdt0001_current changing c_saida.

    when '04'. " Fertilizantes (Porto Velho)

      perform f_build_status_romaneio_02 using lwa_zsdt0001_current changing c_saida.

  endcase.

  if c_saida-st_proc = '99'.
    if c_saida-transp = icon_execute_object.
      c_saida-transp = icon_icon_list.
    endif.

    if c_saida-dacte = icon_execute_object.
      c_saida-dacte = icon_icon_list.
    endif.

    if c_saida-danfe = icon_execute_object.
      c_saida-danfe = icon_icon_list.
    endif.
  endif.


  lwa_zsdt0001_new-st_proc = c_saida-st_proc.

  if lwa_zsdt0001_current ne lwa_zsdt0001_new.
    modify zsdt0001 from lwa_zsdt0001_new.
  endif.


*-------------------------------------------------------------------------------------* BUG PRD / AOENNING.
*============>          RECUPERA DOC MATERIAL VINC.REMESSA.  <===========  *
*-------------------------------------------------------------------------------------*
  perform f_get_doc_mat_rem_zsdt0023 changing c_saida.

endform.

form f_item_text_delivery using p_delivery.

  call function 'POPUP_TO_CONFIRM'
    exporting
      text_question         = 'Informar texto nos itens da remessa?'
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    importing
      answer                = w_answer
    exceptions
      text_not_found        = 1
      others                = 2.

  if w_answer = '1'.

    select *
      from lips into table @data(tg_lips)
     where vbeln = @p_delivery.

    loop at tg_lips into data(wl_lips).

      check wl_lips-posnr(1) ne 9.

      clear: tl_texto[].

      if wl_lips-charg is not initial.
        data(_charg) = |/ Lote: { wl_lips-charg } |.
      endif.

      select single *
        from makt into @data(wl_makt)
       where spras = @sy-langu
         and matnr = @wl_lips-matnr.

      if ( sy-subrc = 0 ) and ( wl_lips-matnr is not initial ).
        data(_maktx) = |/ Material: { wl_makt-maktx } |.
      endif.

      data(_posnr) = wl_lips-posnr.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = _posnr
        importing
          output = _posnr.

      wl_text_header = |Item: { _posnr } { _charg } { _maktx }  |.

      "//Buscar Texto da item remessa
      concatenate wl_lips-vbeln wl_lips-posnr into wl_ordemt.
      call function 'READ_TEXT'
        exporting
          id                      = '0001'
          language                = sy-langu
          name                    = wl_ordemt
          object                  = 'VBBP'
        tables
          lines                   = it_lines
        exceptions
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          others                  = 8.

      loop at it_lines into wa_lines.
        wl_texto = wa_lines-tdline+0(72).
        append wl_texto to tl_texto.
      endloop.

      call function 'CATSXT_SIMPLE_TEXT_EDITOR'
        exporting
          im_title = wl_text_header
        changing
          ch_text  = tl_texto.

      clear: it_lines[], wa_lines.
*      WA_LINES-TDFORMAT = '*'.
*      APPEND WA_LINES TO IT_LINES.

      loop at tl_texto into wl_texto.
        wa_lines-tdformat = '*'.
        wa_lines-tdline+0(72) = wl_texto.
        append wa_lines to it_lines.
        clear  wa_lines.
      endloop.

      if ( it_lines[] is not initial ).
        zid               = '0001'.
        concatenate wl_lips-vbeln wl_lips-posnr into zname.
        x_header-tdobject = 'VBBP'.
        x_header-tdname   = zname.
        x_header-tdid     = zid.
        x_header-tdspras  = sy-langu.

        call function 'SAVE_TEXT'
          exporting
            client          = sy-mandt
            header          = x_header
            savemode_direct = 'X'
          tables
            lines           = it_lines
          exceptions
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            others          = 5.
      endif.

    endloop.

  endif.
  "Fim Textos Itens

endform.

form f_check_aut_doc  using p_tipo
                            p_saida    type ty_saida
                            p_zsdt0001 type ty_zsdt0001
                   changing p_doc_aut.

  data: wl_docnum type j_1bnflin-docnum.

  clear: p_doc_aut, vl_refkey, vl_vbeln, vl_mjahr, vl_docnum, wl_docnum.

  case p_tipo.
    when '1'. "Fluxo 1

      if ( p_saida-tipo = 'P' ) or ( p_saida-tipo = 'T' ).
        select single vbeln mjahr
          into (vl_vbeln,vl_mjahr)
          from vbfa
         where vbelv = p_zsdt0001-doc_rem
           and vbtyp_n  = 'R'
           and vbtyp_v  = 'J'.

        if sy-subrc = 0.
          concatenate vl_vbeln vl_mjahr into vl_refkey.
          select single docnum
            from j_1bnflin
            into wl_docnum
            where refkey = vl_refkey.
        endif.
      else.
        select single docnum
          from j_1bnflin
          into wl_docnum
          where refkey = p_saida-fatura.
      endif.

      if ( sy-subrc = 0 ) and ( wl_docnum is not initial ).

        perform f_check_auth_doc using wl_docnum.

        if sy-subrc = 0.
          p_doc_aut = wl_docnum.
        endif.
      endif.

  endcase.



endform.

form f_check_canc_doc using p_docnum.

  data: v_docnum type j_1bnfdoc-docnum.

  v_docnum = p_docnum.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = v_docnum
    importing
      output = v_docnum.

  if ( v_docnum is initial ).
    sy-subrc = 1.
    return.
  endif.

  select single *
    from j_1bnfe_active into @data(wl_active_ck)
   where docnum  eq @v_docnum.

  if sy-subrc ne 0.
    sy-subrc = 1.
    return.
  endif.

  select single *
    from j_1bnfdoc into @data(wl_doc_ck)
   where docnum  eq @v_docnum.

  if sy-subrc ne 0.
    sy-subrc = 1.
    return.
  endif.

  if ( wl_active_ck-cancel eq abap_true ) or ( wl_active_ck-scssta eq '2' ) or ( wl_doc_ck-candat is not initial ).
    sy-subrc = 0.
  else.
    sy-subrc = 1.
  endif.

endform.

form f_check_auth_doc using p_docnum.

  data: v_docnum type j_1bnfdoc-docnum.

  v_docnum = p_docnum.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = v_docnum
    importing
      output = v_docnum.

  if ( v_docnum is initial ).
    sy-subrc = 1.
    return.
  endif.

  select single *
    from j_1bnfe_active into @data(wl_active_doc)
   where docnum     eq @v_docnum.

  if ( sy-subrc eq 0 ) and ( wl_active_doc-docsta eq '1' ) and ( wl_active_doc-scssta ne '2' ) and ( wl_active_doc-cancel ne abap_true ).
    select single *
      from j_1bnfdoc into @data(wl_doc_ck)
     where docnum eq @v_docnum.

    if wl_doc_ck-candat is initial.
      sy-subrc = 0.
    else.
      sy-subrc = 1.
    endif.
  else.
    sy-subrc = 1.
  endif.

endform.

form f_config_cell  using p_value
                          p_fieldname type lvc_s_styl-fieldname
                          p_style     type lvc_s_styl-style.

  clear: wa_style.
  wa_style-fieldname = p_fieldname.
  wa_style-style     = p_style.
  insert wa_style into table style .

endform.

form f_atrib_vlr_nf_rem  using p_delivery type likp-vbeln
                           p_netwr    type zsdt0001-netwr.

  data: lf_error    type flag,
        lv_upd      type xfeld,
        ls_vbkok    type vbkok,
        ls_logfile  type prott,
        ls_delivery type lxhme_range_c10,
        ls_location type lyloc_wa,
        lt_log_file type table of prott.

  if ( p_delivery is initial ).
    message 'Nro. Remessa não informado.(Atribuição Valor NF em Doc. Remessa)' type 'S'.
    exit.
  endif.

  if ( p_netwr  is initial ).
    message 'Valor NF não informado no Romaneio.(Atribuição Valor NF em Doc. Remessa)' type 'S'.
    exit.
  endif.

  ls_vbkok-vbeln_vl = p_delivery.
  ls_vbkok-bolnr    = p_netwr.

  replace all occurrences of '.' in ls_vbkok-bolnr with ','.
  condense ls_vbkok-bolnr no-gaps.

  call function 'WS_DELIVERY_UPDATE_2'
    exporting
      vbkok_wa      = ls_vbkok
      synchron      = 'X'
      commit        = 'X'
      delivery      = ls_vbkok-vbeln_vl
    importing
      ef_error_any  = lf_error
    tables
      prot          = lt_log_file
    exceptions
      error_message = 99.

  if lt_log_file[] is not initial.
    read table lt_log_file
       with key msgty = lyrgc_msgty_error
       into ls_logfile.

    if sy-subrc = 0.

      sy-msgty = lyrgc_msgty_error.
      sy-msgid = ls_logfile-msgid.
      sy-msgno = ls_logfile-msgno.
      sy-msgv1 = ls_logfile-msgv1.
      sy-msgv2 = ls_logfile-msgv2.
      sy-msgv3 = ls_logfile-msgv3.
      sy-msgv4 = ls_logfile-msgv4.

      message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         raising service_failure.

    endif.
  endif.

endform.

form f_check_permissao_estorno  using p_saida type ty_saida
                                      p_tipo "1 NF-e / 2 CT-e
                             changing p_ok.

*------------------------------------------------------------------*
* P_TIPO: 1 NF-e / 2 CT-e
*
*------------------------------------------------------------------*

  clear: p_ok.

  select single * from zsdt0001 into @data(wl_0001)
   where ch_referencia = @p_saida-ch_referencia.

  check sy-subrc = 0.

  select single * from tvtk into @data(wl_tvtk)
   where shtyp = @p_saida-shtyp.

  if ( p_tipo eq '1' ) or ( p_tipo eq '2' ).

    if ( wl_tvtk-abfer        is not initial ) and
       ( wl_0001-agente_frete is not initial ) and
       ( wl_0001-branch       is not initial ).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wl_0001-agente_frete
        importing
          output = wl_0001-agente_frete.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wl_0001-branch
        importing
          output = wl_0001-branch.

      concatenate wl_0001-branch '-' wl_0001-agente_frete into data(_branch_agente).

      case wl_tvtk-abfer.
        when '1' or '3'. "Saída

        when '2' or '4'. "Entrada
          select single *
            from setleaf into @data(wl_setleaf)
           where setname = 'MAGGI_ZLES0136_BLQ_ROM_E'
             and valfrom = @_branch_agente.

          if sy-subrc = 0.
            message | Estorno não permitido! Romaneios de entrada da Filial/Agente de Frete: { _branch_agente }, parametrizados para modo de visualização(SET MAGGI_ZLES0136_BLQ_ROM_E)!| type 'S'.
            return.
          endif.
      endcase.
    endif.

  endif.

  case p_tipo.
    when '1'.
    when '2'.

      if ( wl_0001-nro_nf_frete  is not initial ) and
         ( wl_0001-id_referencia is not initial ) and
         ( wl_0001-id_interface  eq '49' ).
        message |Estorno não permitido! Romaneio já referenciado pelo romaneio com Chave: { wl_0001-id_referencia } | type 'S'.
        return.
      endif.

  endcase.

  p_ok = 'X'.

endform.

form f_set_romaneios_carga changing p_saida type ty_saida
                                    p_erro  type c
                            raising zcx_error. "*-#133089-12.02.2024-JT

  clear: p_saida-romaneios, p_saida-romaneios_agr, p_erro.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  if p_saida-ch_referencia is initial.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Ch.Referência não atribuída!!' type 'S'.
        exit.
      when abap_true.
        data(l_mesg) = 'Ch.Referência não atribuída!!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  select single *
    from zsdt0001 into @data(_wl_0001)
   where ch_referencia eq @p_saida-ch_referencia.

  if sy-subrc ne 0.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| type 'S' .
        exit.
      when abap_true.
        l_mesg = |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!|.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  select single *
    from tvtk into @data(wl_tvtk)
   where shtyp = @p_saida-shtyp.

  if ( sy-subrc ne 0 ) or ( wl_tvtk-abfer is initial ).
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Tipo de Transporte não encontrado!' type 'S'.
        exit.
      when abap_true.
        l_mesg = 'Tipo de Transporte não encontrado!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  case wl_tvtk-abfer.
    when '1' or '3'. "Saída

      data(_vbtyp_v)  = 'J'.

      if p_saida-inco1 ne 'CIF'.

        append _wl_0001 to p_saida-romaneios.
        append _wl_0001 to p_saida-romaneios_agr.

      else.

        call method zcl_romaneio=>get_ck_faturar
          exporting
            i_ch_referencia_sai = p_saida-ch_referencia
          importing
            e_romaneios         = p_saida-romaneios.


        append _wl_0001 to p_saida-romaneios.

        loop at p_saida-romaneios into data(_wl_rom) where id_cli_dest eq _wl_0001-id_cli_dest.
          append _wl_rom to p_saida-romaneios_agr.
        endloop.

      endif.

    when '2' or '4'. "Entrada

      _vbtyp_v  = '7'.

      append _wl_0001 to p_saida-romaneios.
      append _wl_0001 to p_saida-romaneios_agr.

  endcase.

  sort p_saida-romaneios by ch_referencia.
  delete adjacent duplicates from p_saida-romaneios comparing ch_referencia.

  sort p_saida-romaneios_agr by ch_referencia.
  delete adjacent duplicates from p_saida-romaneios_agr comparing ch_referencia.

endform.


form f_valida_geracao_vt changing p_saida type ty_saida
                                  p_erro  type c
                          raising zcx_error. "*-#133089-12.02.2024-JT

  data: t_route          type table of vbap-route with header line,
        t_romaneios      type zsdt0001_t,
        v_faturar	       type char01,
        v_auart          type vbak-auart,
        v_peso_carga     type zde_nm_peso_subtotal,
        v_peso_romaneios type zde_nm_peso_subtotal,
        v_qtd_embalagens type zde_qt_embalagens,
        v_mensagem       type char255,
        v_chv_fat_vt     type zch_ref,
        lc_gera_transp   type char01.  "*-CS2024000086-25.09.2024-#133287-JT-inicio

  clear: t_romaneios[], p_erro, v_faturar, v_peso_carga, v_peso_romaneios,v_qtd_embalagens,v_mensagem, t_route[], v_chv_fat_vt.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  if p_saida-ch_referencia is initial.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Ch.Referência não atribuída!!' type 'S'.
        exit.
      when abap_true.
        data(l_mesg) = 'Ch.Referência não atribuída!!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  check p_saida-transp eq icon_execute_object.

  select single *
    from zsdt0001 into @data(_wl_0001)
   where ch_referencia eq @p_saida-ch_referencia.

  if sy-subrc ne 0.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| type 'S' .
        exit.
      when abap_true.
        l_mesg = |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!|.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  select single *
    from tvtk into @data(wl_tvtk)
   where shtyp = @p_saida-shtyp.

  if ( sy-subrc ne 0 ) or ( wl_tvtk-abfer is initial ).
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Tipo de Transporte não encontrado!' type 'S'.
        exit.
      when abap_true.
        l_mesg = 'Tipo de Transporte não encontrado!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  case wl_tvtk-abfer.
    when '1' or '3'. "Saída

      data(_vbtyp_v)  = 'J'.

      if p_saida-inco1 eq 'CIF'.

        call method zcl_romaneio=>get_ck_faturar
          exporting
            i_ch_referencia_sai = p_saida-ch_referencia
          importing
            e_romaneios         = t_romaneios
            e_faturar           = v_faturar
            e_peso_carga        = v_peso_carga
            e_peso_romaneios    = v_peso_romaneios
            e_qtd_embalagens    = v_qtd_embalagens
            e_mensagem          = v_mensagem
            e_chv_faturar       = v_chv_fat_vt.

        if ( v_faturar is initial ).
          p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              if v_mensagem is not initial.
                message v_mensagem type 'S'.
              else.
                message 'Doc. Transporte não pode ser gerado!(Check Agrupamento VT)' type 'S'.
              endif.
              return.
            when abap_true.
              if v_mensagem is not initial.
                l_mesg = v_mensagem.
              else.
                l_mesg = 'Doc. Transporte não pode ser gerado!(Check Agrupamento VT)'.
              endif.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.

        if ( v_chv_fat_vt is not initial ) and ( v_chv_fat_vt ne p_saida-ch_referencia ).
          p_erro = abap_true.
          read table t_romaneios into data(_wl_rom) with key ch_referencia = v_chv_fat_vt.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message |Romaneio Nro: { _wl_rom-nr_romaneio } deve ser faturado! Atualizar Consulta!(Agrupamento CT-e)!  | type 'S'.
              return.
            when abap_true.
              l_mesg = |Romaneio Nro: { _wl_rom-nr_romaneio } deve ser faturado! Atualizar Consulta!(Agrupamento CT-e)! |.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.

      else.
        append _wl_0001 to t_romaneios.
      endif.

      loop at t_romaneios into _wl_rom.

        select single *
          from vbak into @data(lwa_vbak_exists)
         where vbeln eq @_wl_rom-vbeln.

        if sy-subrc ne 0.
          select single *
            from ekko into @data(lwa_ekko_exists)
           where ebeln eq @_wl_rom-vbeln.

          check sy-subrc eq 0.
        endif.

        if _wl_rom-doc_rem is initial.
          p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
          case vg_faturamento_autom.
            when abap_off.
              message |Gerar a Remessa Romaneio Nro: { _wl_rom-nr_romaneio } !  | type 'S'.
              return.
            when abap_true.
              l_mesg = |Gerar a Remessa Romaneio Nro: { _wl_rom-nr_romaneio } !  |.
              lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
          endcase.
*-#133089-21.02.2024-JT-fim
        endif.

        clear: v_auart.
        select single *
          from vbak into @data(_wl_vbak)
         where vbeln = @_wl_rom-vbeln.

        if sy-subrc eq 0.
          v_auart = _wl_vbak-auart.
        endif.

*-CS2024000086-25.09.2024-#133287-JT-inicio
        lc_gera_transp =  zcl_faturamento=>zif_faturamento~get_instance( )->get_gera_vt_frota_propria( _wl_rom-ch_referencia ).

*       PERFORM f_check_gera_vt_frota_propria    USING _wl_rom-ch_referencia
*                                             CHANGING lc_gera_transp.
*-CS2024000086-25.09.2024-#133287-JT-fim

        if ( p_saida-inco1  = 'CPT' ) and ( v_auart ne 'ZTER' ) and "Ajustes Geração Frete CPT 27/02/24
           ( lc_gera_transp = abap_false ).  "*-CS2024000086-25.09.2024-#133287-JT-inicio
          if ( _wl_rom-fatura_prod is initial ).
            p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
                message |Gerar a Fatura Romaneio Nro: { _wl_rom-nr_romaneio } ! | type 'S'.
                return.
              when abap_true.
                l_mesg = |Gerar a Fatura Romaneio Nro: { _wl_rom-nr_romaneio } ! |.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
            endcase.
*-#133089-21.02.2024-JT-fim
          endif.
        else.
          if ( _wl_rom-nro_nf_prod is initial ) and ( v_auart ne 'ZTER' ) and
             ( lc_gera_transp       = abap_false ).  "*-CS2024000086-25.09.2024-#133287-JT-inicio
            p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
            case vg_faturamento_autom.
              when abap_off.
                message |Gerar a DANFE Romaneio Nro: { _wl_rom-nr_romaneio } ! | type 'S'.
                return.
              when abap_true.
                l_mesg = |Gerar a DANFE Romaneio Nro: { _wl_rom-nr_romaneio } ! |.
                lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
            endcase.
*-#133089-21.02.2024-JT-fim
          endif.
        endif.
      endloop.

*        SORT T_ROUTE.
*        DELETE ADJACENT DUPLICATES FROM T_ROUTE.
*
*        IF LINES( T_ROUTE[] ) > 1.
*          P_ERRO = ABAP_TRUE.
*          MESSAGE |Existe mais de um Itinerátio para a Carga! | TYPE 'S'.
*          RETURN.
*        ENDIF.

    when '2' or '4'. "Entrada

      _vbtyp_v  = '7'.

      append _wl_0001 to t_romaneios.

      if p_saida-aviso = icon_execute_object.
        p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message 'Gerar a Aviso!' type 'S'.
            return.
          when abap_true.
            l_mesg = 'Gerar a Aviso!'.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.


  endcase.

  perform f_set_delivery changing p_saida
                                  p_erro.

  check p_erro eq abap_false.

  loop at p_saida-deliverys into data(_delivery).
    select single vttk~tknum into v_tknum
      from vbfa inner join vttk on  vttk~tknum = vbfa~vbeln
                                and vttk~vsart = wl_tvtk-vsart
     where vbfa~vbelv    = _delivery-vbeln
       and vbfa~vbtyp_n  = '8'
       and vbfa~vbtyp_v  = _vbtyp_v.
    if sy-subrc = 0 and vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT-fim
      p_erro = abap_true.
      message |Documento de Transporte já gerado para a Remessa: { _delivery-vbeln }. Atualizar linha de documentos!| type 'S'.
      return.
    endif.
  endloop.

*-CS2021000696 - 16.08.2021 - JT - inicio
  loop at t_romaneios into _wl_rom.

    if zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                exporting i_ch_referencia = _wl_rom-ch_referencia ) = abap_false.

      data(l_erro) = zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~set_valida_envio_carregamento(
        exporting
          i_ch_referencia = _wl_rom-ch_referencia ).

      if l_erro = abap_true.
        p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
        case vg_faturamento_autom.
          when abap_off.
            message |A viagem no carguero deve estar no status Carregamento ou Carregado.|  type 'S' display like 'E'.
            return.
          when abap_true.
            l_mesg = |A viagem no carguero deve estar no status Carregamento ou Carregado.|.
            lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
        endcase.
*-#133089-21.02.2024-JT-fim
      endif.
    endif.

  endloop.
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
  data(_rem_conta_ordem) = abap_false.
  perform f_check_rem_conta_ordem using _wl_0001      "p_saida *-CS2024000086-25.09.2024-#133287-JT-inicio
                               changing _rem_conta_ordem. "Comentando para subir junto com as melhorias da ZLES0200 US 92467
  if _rem_conta_ordem eq abap_true.
    "O docto.Transporte deverá ser gerado pela transação ZLES0200. Romaneio será finalizado!'.
    p_erro = abap_true.
    perform f_repare_docs_romaneio changing p_saida. "Metodo finaliza romaneio
  endif.
*-CS2021001045 - 22.02.2022 - JT - fim

endform.

*****************************************************************
* valida se simples remessa gerado por terceiro
*****************************************************************
form f_check_rem_conta_ordem using p_saida type zsdt0001 "ty_saida  *-CS2024000086-25.09.2024-#133287-JT-inicio
                          changing p_rem_conta_ordem.


*-CS2024000086-25.09.2024-#133287-JT-inicio
  p_rem_conta_ordem =  zcl_faturamento=>zif_faturamento~get_instance( )->get_check_rem_conta_ordem( p_saida ).

*  DATA: t_set    TYPE TABLE OF rgsb4,
*        w_set    TYPE rgsb4,
*        t_tvarvc TYPE TABLE OF tvarvc,
*        w_tvarvc TYPE tvarvc.
*
*  RANGES:
*        r_cfop            FOR j_1bnflin-cfop,
*        r_matkl           FOR j_1bnflin-matkl.
*
*  FREE: r_cfop, r_matkl, t_set, p_rem_conta_ordem.
*
*  CHECK p_saida-ch_referencia IS NOT INITIAL.
*
**---------------------------------
** ler set
**---------------------------------
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      class         = '0000'
*      setnr         = 'MAGGI_CFOP_VENDA_IND'
*    TABLES
*      set_values    = t_set
*    EXCEPTIONS
*      set_not_found = 1
*      OTHERS        = 2.
*
*  LOOP AT t_set INTO w_set.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_cfop.
*  ENDLOOP.
*
**---------------------------------
** ler TVARVset
**---------------------------------
*  SELECT *
*    FROM tvarvc
*    INTO TABLE t_tvarvc
*   WHERE name = 'MAGGI_GR_FERTILIZANTES'.
*
*  LOOP AT t_tvarvc INTO w_tvarvc.
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) TO r_matkl.
*  ENDLOOP.
*
**---------------------------------
** ler OV
**---------------------------------
*  SELECT matkl, j_1bcfop
*    FROM vbap
*    INTO @DATA(w_vbap)
*      UP TO 1 ROWS
*   WHERE vbeln = @p_saida-vbeln
*     AND matnr = @p_saida-matnr.
*  ENDSELECT.
*
*  IF sy-subrc         = 0           AND
*     w_vbap-j_1bcfop IN r_cfop[]    AND
*     w_vbap-matkl    IN r_matkl[]   AND
*     r_cfop[]        IS NOT INITIAL AND
*     r_matkl[]       IS NOT INITIAL.
*    p_rem_conta_ordem = abap_true.
*  ENDIF.
*-CS2024000086-25.09.2024-#133287-JT-fim

endform.


form f_set_delivery changing p_saida type ty_saida
                             p_erro  type c
                     raising zcx_error. "*-#133089-12.02.2024-JT

  data: it_likp type table of likp with header line.

  clear: p_saida-deliverys, it_likp[].

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  if p_saida-ch_referencia is initial.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Ch.Referência não atribuída!!' type 'S'.
        exit.
      when abap_true.
        data(l_mesg) = 'Ch.Referência não atribuída!!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  select single *
    from zsdt0001 into @data(_wl_0001)
   where ch_referencia eq @p_saida-ch_referencia.

  if sy-subrc ne 0.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!| type 'S' .
        exit.
      when abap_true.
        l_mesg = |Registro Rom. Chv.Referência: { p_saida-ch_referencia } não encontrado!!|.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  select single *
    from tvtk into @data(wl_tvtk)
   where shtyp = @p_saida-shtyp.

  if ( sy-subrc ne 0 ) or ( wl_tvtk-abfer is initial ).
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Tipo de Transporte não encontrado!' type 'S'.
        exit.
      when abap_true.
        l_mesg = 'Tipo de Transporte não encontrado!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

  case wl_tvtk-abfer.
    when '1' or '3'. "Saída
      if lines( p_saida-romaneios_agr[] ) eq 1.
        clear: it_likp.
        it_likp-vbeln = p_saida-remessa+0(10).
        append it_likp.
      else.
        loop at p_saida-romaneios_agr into data(_wl_rom).
          clear: it_likp.
          it_likp-vbeln = _wl_rom-doc_rem.
          append it_likp.
        endloop.
      endif.

      data(_vbtyp_v)  = 'J'.
    when '2' or '4'. "Entrada

      clear: it_likp.
      it_likp-vbeln = p_saida-aviso.
      append it_likp.

      _vbtyp_v  = '7'.
  endcase.

  p_saida-deliverys[] = it_likp[].

  if p_saida-deliverys[] is initial.
    p_erro = abap_true.
*-#133089-21.02.2024-JT-inicio
    case vg_faturamento_autom.
      when abap_off.
        message 'Não foi possível determinar o número da Entrega!' type 'S'.
        exit.
      when abap_true.
        l_mesg = 'Não foi possível determinar o número da Entrega!'.
        lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = conv #( l_mesg ) ).
    endcase.
*-#133089-21.02.2024-JT-fim
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBAK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_vbak .

  select vbeln auart kunnr spart vkbur
    from vbak appending corresponding fields of table it_vbak
     for all entries in it_zsdt0001
   where vbeln  = it_zsdt0001-vbeln.

  loop at it_vbak into wa_vbak.
    tabix = sy-tabix .
    read table it_zsdt0001 into wa_zsdt0001 with key vbeln = wa_vbak-vbeln binary search.
    wa_vbak-tp_movimento = wa_zsdt0001-tp_movimento.
    modify it_vbak from wa_vbak index tabix transporting tp_movimento.
  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBPA_CO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_vbpa_co .

  "Ordem Venda
  select vbeln lifnr
    from vbpa appending table it_vbpa_co
     for all entries in it_zsdt0001
   where vbeln = it_zsdt0001-vbeln
     and parvw = 'PC'.

  if it_vbpa_co[] is not initial.
    select lifnr name1 dlgrp lzone regio
      from lfa1 appending table it_lfa1
       for all entries in it_vbpa_co
     where lifnr  = it_vbpa_co-lifnr.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_VBPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_vbpa .

  select vbeln parvw lifnr kunnr
    from vbpa appending table it_vbpa
     for all entries in it_zsdt0001
   where vbeln = it_zsdt0001-vbeln
     and parvw  in ('LR','SP','Z1').

  select vbeln parvw lifnr kunnr
    from vbpa appending table it_vbpa
     for all entries in it_zsdt0001
   where vbeln = it_zsdt0001-doc_aviso
     and parvw  in ('LR','SP').

  select vbeln parvw lifnr kunnr
    from vbpa appending table it_vbpa
     for all entries in it_zsdt0001
   where vbeln = it_zsdt0001-doc_rem
     and parvw  in ('LR','SP').

  if it_vbpa[] is not initial.
    select kunnr name1 lzone
      from kna1 appending table it_kna1
       for all entries in it_vbpa
     where kunnr  = it_vbpa-kunnr.
  endif.

  loop at it_vbpa assigning field-symbol(<fs_vbpa>) where parvw eq 'SP'
                                        and lifnr is not initial.
    select single *
      from lfa1 into @data(_wl_lfa1_sp)
     where lifnr eq @<fs_vbpa>-lifnr.

    if sy-subrc eq 0.
      <fs_vbpa>-dlgrp = _wl_lfa1_sp-dlgrp.
    endif.

  endloop.


endform.

form f_elimina_lfa1_bloq tables p_lfa1 structure lfa1
                          using p_bukrs type bukrs.

  loop at p_lfa1 into data(wl_lfa1).
    data(_delete) = ''.
    data(_tabix)  = sy-tabix.
    try.
        zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = wl_lfa1-lifnr
        )->ck_ativo(
        )->ck_ativo_empresa( i_empresa = p_bukrs ).
      catch zcx_parceiros into data(ex_parceiros_k).
        _delete = 'X'.
    endtry.
    if _delete is not initial.
      delete p_lfa1 index _tabix.
    endif.
  endloop.

endform.



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
form f_troca_agente  using    p_placa_cav
                              p_branch
                     changing p_lifnr.

  data: t_tvarvc    type table of tvarvc,
        w_tvarvc    type tvarvc,
        v_lifnr_ori type lifnr.

  ranges: r_bukrs for t001-bukrs.

  select single bukrs
    into @data(v_bukrs)
    from j_1bbranch
    where branch = @p_branch.

*-CS2022000236 - 06.03.2022 - JT - inicio
  free: r_bukrs.

*---------------------------------
* ler TVARVset
*---------------------------------
  select *
    from tvarvc
    into table t_tvarvc
   where name = 'ZLES0136_BUKRS_CHANGE_AGENTE'.

  loop at t_tvarvc into w_tvarvc.
    append value #( sign = 'I' option = 'EQ' low = w_tvarvc-low ) to r_bukrs.
  endloop.

  check v_bukrs in r_bukrs[] and r_bukrs[] is not initial.
* CHECK v_bukrs EQ '0001'. "somente Amaggi
*-CS2022000236 - 06.03.2022 - JT - fim

  try.
      zcl_faturamento=>zif_faturamento~get_instance( )->get_tipo_veiculo(
      exporting i_placa = p_placa_cav importing
        e_tipo = data(e_tipo)
        e_proprietario = data(e_proprietario) ).
    catch zcx_faturamento .
    catch zcx_error .
  endtry.
  if e_tipo = 'P'.
*                  V_LIFNR_ORI.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_branch
      importing
        output = v_lifnr_ori.
    "
    select single regio
      from lfa1
      into @data(_regio)
      where lifnr = @v_lifnr_ori.
    "
    try.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_agente_frete(
          exporting
            i_tipo_agente           = '2'               "CS2022000236 - 25.02.2022 - JT - inicio
            i_bukrs                 = conv #( v_bukrs ) "CS2022000236 - 25.02.2022 - JT - inicio
            i_placa                 = conv #( p_placa_cav )
            i_uf_origem_mercadoria  = conv #( _regio )
           importing
             e_agente_frete         = data(_agente) ).

        p_lifnr = _agente.
      catch zcx_faturamento.
      catch zcx_error.
    endtry.
  endif.

endform.

form f_call_files using p_salva changing pdf_result type xstring.

  data: v_imp_doc          type j_1bdocnum,
        v_url              type zib_nfe,
        v_url_contrato     type zcte_ciot-link_contrato,
        v_url_cpedagio     type zcte_ciot-link_carga_pedagio,
        v_docnum_ref       type zsdt0105-docnum_ref,
        v_url_mdfe         type zsdt0102-url_sefaz,
        v_path             type string,
        v_troca_nota       type char1,
        v_naotem_doc       type char1,
        t_doctos_faltantes type zsdt_doctos_faltantes,
        w_doctos_faltantes type zsde_doctos_faltantes.

  data: wa_file_table type file_info,
        it_file_table type standard table of file_info.

  data: t_pdf_files type zsdt_pdf_files,
        w_pdf_files type zsde_pdf_files.

  data: lva_declaracao type  xstring.

  clear: tl_rows[].

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para gerar o arquivo' type 'I'.
    exit.
  endif.

  clear: sl_rows, wa_saida.
  read table tl_rows into sl_rows index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  free: t_pdf_files,
        t_doctos_faltantes,
        merged_document.

*-----------------------------------------
* obtem documentos faturamento
*-----------------------------------------
  try.
      t_pdf_files = zcl_faturamento=>zif_faturamento~get_instance(
                      )->get_documentos_faturamento( exporting i_ch_referencia  = wa_saida-ch_referencia
                      ).

    catch zcx_faturamento.
    catch zcx_error.
  endtry.

*-----------------------------------------
* valida se troca nota
*-----------------------------------------
* v_troca_nota = zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
*                   EXPORTING i_ch_referencia = wa_saida-ch_referencia ).

*--------------------------------------------
*-- valida arquivos obrigatorios
*--------------------------------------------
  try.
      v_naotem_doc = zcl_faturamento=>zif_faturamento~get_instance(
                       )->get_documentos_obrigatorios( exporting i_ch_referencia    = wa_saida-ch_referencia
                                                                 t_pdf_files        = t_pdf_files
                                                       importing t_doctos_faltantes = t_doctos_faltantes
                       ).

    catch zcx_faturamento.
    catch zcx_error.
  endtry.

  if v_naotem_doc = abap_true.
    read table t_doctos_faltantes into w_doctos_faltantes index 1.
    message |{ w_doctos_faltantes-mensagem } para download| type 'I'.
    exit.
  endif.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
  try.
      merged_document = zcl_faturamento=>zif_faturamento~get_instance(
                          )->get_merge_pdf( exporting t_pdf_files = t_pdf_files
                          ).

    catch zcx_faturamento.
    catch zcx_error.
  endtry.

  "Essa opção de salvar é quando o usuário salva mas não envia e-mail
  if p_salva = 'X'.

    call method cl_gui_frontend_services=>directory_browse
      changing
        selected_folder      = v_path
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4.

    if sy-subrc eq 0.
      perform put_merged_file using merged_document v_path  wa_saida-dt_movimento wa_saida-placa_cav
                                    wa_saida-ch_referencia  wa_saida-danfe. "-CS2021000218-16.11.2022-#99520-JT
    endif.
  else.
    pdf_result = merged_document.
  endif.

endform.
form f_call_email.

  clear: tl_rows[].
  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) ne 1.
    message 'Selecione uma linha para enviar e-mail' type 'I'.
    exit.
  else.
    "ZLES0136 - envio email p mais de um dest. PT3 - BG #144498 - INICIO
    if vg_cockpit = '06'.  "Defensivos.
      perform gera_email.
    else.
      call screen 400 starting at 8 5
                       ending at 70 6.
    endif.
    "ZLES0136 - envio email p mais de um dest. PT3 - BG #144498 -
  endif.
endform.
form table2xstring tables pt_data structure rspolpbi
                   using  p_data type xstring
                          p_len type i.

  data l_rest type i.
  data l_chunk type i.

  l_rest = p_len.

  clear p_data.
  loop at pt_data into data(wa).
    if l_rest < 128.
      l_chunk = l_rest.
    else.
      l_chunk = 128.
    endif.

    concatenate p_data wa-data(l_chunk) into p_data in byte mode.
    subtract l_chunk from l_rest.
  endloop.

endform.

form merge_pdfs using pt_files type t_fileinfotab
                changing pdf_merger type ref to cl_rspo_pdf_merge
                         merged_document
                         docindex
                         errordoc
                         rc.

  data: wa type line of t_fileinfotab.

* Add documents to attribut table of PDF merger
  loop at pt_files into wa.
    pdf_merger->add_document( wa-data ).
  endloop.

* Call kernel method to do the merge of the specified files.
  pdf_merger->merge_documents( importing merged_document = merged_document rc = rc ).

* Get index of failed document
  if rc <> 0.
    pdf_merger->get_err_doc_index( importing index = docindex ).
    pdf_merger->get_document( exporting index = docindex importing document = errordoc ).
  endif.

  clear pdf_merger.

endform.
form put_merged_file using merged_document type xstring
                           v_path type string
                           v_dt_movimento type ty_saida-dt_movimento
                           v_placa type ty_saida-placa_cav
                           v_ch_referencia   "-CS2021000218-27.12.2022-#99520-JT
                           v_danfe.          "*-CS2021000218-27.12.2022-#99520-JT


  data: bin_tab type standard table of tabl1024.
  data: lo_gui type ref to cl_gui_frontend_services.
  data: path     type string,
        fullpath type string.
  data: length type i.
  data: l_nrocg  type char10.
  data: filter type string, uact type i, name type string.

  create object lo_gui.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = merged_document
    importing
      output_length = length
    tables
      binary_tab    = bin_tab.


*-CS2021000218-16.05.2023-#112208-JT-inicio
  if vg_cockpit = '06'.
    if wa_saida-danfe(1) <> '@'.
      select single nfenum
        into @data(l_nfenum)
        from j_1bnfdoc
       where docnum = @wa_saida-danfe.
      if sy-subrc <> 0.
        clear l_nfenum.
      endif.
    endif.

    pack wa_saida-nro_cg to l_nrocg.
    pack l_nfenum        to l_nfenum.

    condense: l_nrocg, l_nfenum.

*    fullpath = v_path && '\' && v_dt_movimento+6(2) && '.' && v_dt_movimento+4(2) && '.' && v_dt_movimento(4)
*                      && '_' && l_nrocg && '_' && l_nfenum && '.pdf'.
*-CS2021000218-16.05.2023-#112208-JT-fim

    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- INICIO
    fullpath = v_path && '\' && 'NF' && '_' && l_nfenum  && '_' && wa_saida-name1 && '_' && wa_saida-branch && '.pdf'.
    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- FIM
  else.
    concatenate v_path '\' v_dt_movimento+6(2) '.' v_dt_movimento+4(2) '.' v_dt_movimento(4) '_' v_placa
                       '_' sy-datum  '_' sy-uzeit  "*-CS2021000218-27.12.2022-#99520-JT
                       '.pdf' into fullpath.
  endif.

  "CONDENSE fullpath NO-GAPS. Com a entrada do OneDrive por padrão, as pastas sincronizadas ficam com espaço, não salvando o arquivo

  lo_gui->gui_download( exporting  filename     = fullpath
                                   filetype     = 'BIN'
                                   bin_filesize = length
                        changing   data_tab     = bin_tab "). "*-CS2021000218-27.12.2022-#99520-JT-inicio
                        exceptions               "*-CS2021000218-27.12.2022-#99520-JT-inicio
                                   others       = 1 ).        "*-CS2021000218-27.12.2022-#99520-JT-inicio
*-CS2021000218-27.12.2022-#99520-JT-inicio
  if sy-subrc <> 0.
    message s024(sd) with 'Arquivo não pode ser Gerado: ' fullpath display like 'E'.
  endif.
*-CS2021000218-27.12.2022-#99520-JT-fim

endform.
form gera_email .

  data : wa_doc_data  like sodocchgi1,
         wa_reciever  like somlreci1,
         t_reciever   like standard table of wa_reciever,
         it_receivers type table of somlreci1,
         wa_pk_list   like sopcklsti1,
         t_pk_list    like standard table of wa_pk_list,
         wa_con_txt   like solisti1,
         t_temp       like standard table of wa_con_txt,
         t_con_txt    like standard table of wa_con_txt,
         v_date(10),
         v_lin        like sy-tabix,
         wa_fil       type ty_fileinfo,
         it_zmail     type table of zmail, "ZLES0136 - ENVIO EMAIL P MAIS DE UM DEST. PT3 - BG #144498
         v_check(1). "ZLES0136 - ENVIO EMAIL P MAIS DE UM DEST. PT3 - BG #144498

  clear: sl_rows, wa_saida.
  read table tl_rows into sl_rows index 1.
  read table it_saida into wa_saida index sl_rows-index.

  check sy-subrc = 0.

  select  single * from vbak into @data(w_vbak) where vbeln eq @wa_saida-vbeln. "ZLES0136 - ENVIO EMAIL P MAIS DE UM DEST. PT3 - BG #144498

  "CRIA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line' <H4 ALIGN=LEFT>Segue em anexo os Documentos de Faturamento referente a viagem abaixo .</H4>'.

  concatenate wa_saida-dt_movimento+6(2)  '.'  wa_saida-dt_movimento+4(2) '.' wa_saida-dt_movimento+0(4) into v_date.

  clear: wa_con_txt.
  concatenate '<p>' 'Data Movimento: '  v_date   '.<br>' into wa_con_txt separated by space.
  new_line   wa_con_txt.

  clear: wa_con_txt.
  concatenate 'Romaneio:  'wa_saida-nr_romaneio '.<br>' into  wa_con_txt separated by space.
  new_line    wa_con_txt.

  clear: wa_con_txt.
  concatenate 'Placa: '  wa_saida-placa_cav '.<br>' into  wa_con_txt separated by space.
  new_line    wa_con_txt.

  clear: wa_con_txt.
  concatenate 'Escritório Venda: '  w_vbak-vkbur '.<br>' into  wa_con_txt separated by space.  "ZLES0136 - ENVIO EMAIL P MAIS DE UM DEST. PT3 - BG #144498
  new_line    wa_con_txt.

  clear: wa_con_txt.


*-CS2021000218-27.12.2022-#99520-JT-inicio
  if vg_cockpit = '06'.

    concatenate 'Nro. Documento: ' wa_saida-vbeln '.<br>'  into  wa_con_txt separated by space.
  else.
    concatenate 'Nro. Documento: ' wa_saida-vbeln '.</p>'  into  wa_con_txt separated by space.
  endif.
*-CS2021000218-27.12.2022-#99520-JT-fim

  new_line    wa_con_txt.


*-CS2021000218-27.12.2022-#99520-JT-inicio
  if vg_cockpit = '06'.
    if wa_saida-danfe(1) <> '@'.
      select single nfenum
        into @data(l_nfenum)
        from j_1bnfdoc
       where docnum = @wa_saida-danfe.
      if sy-subrc <> 0.
        clear l_nfenum.
      endif.
    else.
      clear l_nfenum.
    endif.

    shift l_nfenum left deleting leading '0'.
    condense l_nfenum.

    clear: wa_con_txt.
    concatenate 'Nota Fiscal: ' l_nfenum '.</p>'  into  wa_con_txt separated by space.
    new_line    wa_con_txt.
  endif.
*-CS2021000218-27.12.2022-#99520-JT-fim



  new_line '</BODY>'.
  new_line '</HTML>'.

  describe table t_con_txt lines v_lin.

  read table t_con_txt index v_lin into wa_con_txt.
  wa_doc_data-doc_size = ( v_lin - 1 ) * 255 + strlen( wa_con_txt ).

  clear wa_pk_list-transf_bin.
  wa_pk_list-head_start = 1.
  wa_pk_list-head_num = 0.
  wa_pk_list-body_start = 1.
  wa_pk_list-body_num = v_lin.
  wa_pk_list-doc_type = 'HTM'.
  append wa_pk_list to t_pk_list.

  clear: pdf_result.
  perform f_call_files using '' changing pdf_result.

  if pdf_result is not initial.


*-CS2021000218-27.12.2022-#99520-JT-inicio
    if vg_cockpit = '06'.
      "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- INICIO

      concatenate 'Faturamento Defensivos: NF' l_nfenum '-' wa_saida-name1 '-' wa_saida-branch
      into wa_doc_data-obj_descr separated by space.
      concatenate 'NF' '_' l_nfenum '_' wa_saida-name1 '_' wa_saida-branch '.pdf'
      into data(l_texto).
    else.
      concatenate 'Faturamento da Viagem :'  v_date '_'  wa_saida-placa_cav '.pdf' into wa_doc_data-obj_descr.
    endif.

    "BUG 133071 - CS2023000707 - erro ao buscar a filial para preencher no nome do arquivo pdf - BG -- FIM
*-CS2021000218-27.12.2022-#99520-JT-fim



    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer     = pdf_result
      tables
        binary_tab = t_temp.

    describe table t_temp lines v_lin.

    wa_pk_list-transf_bin = 'X'.
    wa_pk_list-head_num   = 1.
    wa_pk_list-body_start = 1.
    wa_pk_list-body_num   = v_lin.
    wa_pk_list-doc_type   = 'PDF'.
    wa_pk_list-obj_name   = 'ATTACHMENT'.
    wa_pk_list-obj_descr  = l_texto.
    wa_pk_list-doc_size   = v_lin * 255.

    append wa_pk_list to t_pk_list..
    "ZLES0136 - envio email p mais de um dest. PT3 - BG #144498 - inicio
    if vg_cockpit = '06'.

      if sy-subrc is initial and w_vbak-vkgrp is not initial.

        select *
        from zmail
        into table it_zmail
        where id_processo  = 'FAT_DEF'
          and tcode        =  'ZLES0136'
          and bukrs        =  wa_saida-bukrs
          and vkbur        =  w_vbak-vkbur.

        if sy-subrc is not initial.
          concatenate 'Não existe e-mail parametrizado para o escritório de venda'  w_vbak-vkbur into data(v_msg_vkbur) separated by space.
          message v_msg_vkbur type 'I' display like 'W'.
        endif.

        select *
         from zmail
         appending  corresponding fields of table it_zmail
         where id_processo  = 'FAT_DEF'
           and tcode        = 'ZLES0136'
           and bukrs        =  wa_saida-bukrs
           and werks        =  wa_saida-branch.
        if sy-subrc is not initial.
          concatenate 'Não existe e-mail parametrizado para o centro emissor'  wa_saida-branch into data(v_msg_werks) separated by space.
          message v_msg_werks type 'I' display like 'W'.
        endif.

        "----------------------------------------------------------------------
        if it_zmail[] is not initial.
          loop at it_zmail into data(wa_zmail).
            clear: wa_reciever, t_reciever.

            wa_reciever-receiver = wa_zmail-email.
            wa_reciever-rec_type = 'U'.
            wa_reciever-com_type = 'INT'.
            append wa_reciever to it_receivers.

          endloop.

          call function 'SO_DOCUMENT_SEND_API1'
            exporting
              document_data              = wa_doc_data
              put_in_outbox              = 'X'
              sender_address             = ''
              sender_address_type        = ''
              commit_work                = 'X'
            tables
              packing_list               = t_pk_list
              contents_bin               = t_temp
              contents_txt               = t_con_txt
              receivers                  = it_receivers
            exceptions
              too_many_receivers         = 1
              document_not_sent          = 2
              document_type_not_exist    = 3
              operation_no_authorization = 4
              parameter_error            = 5
              x_error                    = 6
              enqueue_error              = 7
              others                     = 8.

          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          else.
            message 'Email(s) enviado(s) com sucesso!' type 'I' display like 'S'.
          endif.
        else.
          concatenate 'Não existe e-mail parametrizado para o escritório de venda' w_vbak-vkbur ' e centro emissor ' wa_saida-branch into data(v_mensagem) separated by space.
          message v_mensagem type 'I' display like 'W'.
        endif.

        "-------------------------------------------------------------------------



*        IF IT_ZMAIL[] IS NOT INITIAL.
*          LOOP AT IT_ZMAIL INTO DATA(WA_ZMAIL).
*            CLEAR: WA_RECIEVER, T_RECIEVER.
*
*            WA_RECIEVER-RECEIVER = WA_ZMAIL-EMAIL.
*            WA_RECIEVER-REC_TYPE = 'U'.
*            WA_RECIEVER-COM_TYPE = 'INT'.
*            APPEND WA_RECIEVER TO T_RECIEVER.
*            ENDLOOP.
*            CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
*              EXPORTING
*                DOCUMENT_DATA              = WA_DOC_DATA
*                PUT_IN_OUTBOX              = 'X'
*                COMMIT_WORK                = 'X'
*              TABLES
*                PACKING_LIST               = T_PK_LIST
*                CONTENTS_BIN               = T_TEMP
*                CONTENTS_TXT               = T_CON_TXT
*                RECEIVERS                  = T_RECIEVER
*              EXCEPTIONS
*                TOO_MANY_RECEIVERS         = 1
*                DOCUMENT_NOT_SENT          = 2
*                DOCUMENT_TYPE_NOT_EXIST    = 3
*                OPERATION_NO_AUTHORIZATION = 4
*                PARAMETER_ERROR            = 5
*                X_ERROR                    = 6
*                ENQUEUE_ERROR              = 7
*                OTHERS                     = 8.
*
*            IF SY-SUBRC <> 0.
*              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*            ENDIF.
*          ENDLOOP.
*          IF V_CHECK IS INITIAL.
*            MESSAGE 'Email(s) enviado(s) com sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
*          ENDIF.
*        ELSE.
*          "CONCATENATE 'Não existe e-mail parametrizado para o escritório de venda' W_VBAK-VKBUR ' e centro emissor ' WA_SAIDA-BRANCH INTO DATA(V_MENSAGEM) SEPARATED BY SPACE.
*          "MESSAGE V_MENSAGEM TYPE 'I' DISPLAY LIKE 'W'.
*        ENDIF.
      endif.
    else.
      loop at s_email.
        clear: wa_reciever, t_reciever.

        wa_reciever-receiver = s_email-low.
        wa_reciever-rec_type = 'U'.
        wa_reciever-com_type = 'INT'.
        append wa_reciever to t_reciever.

        call function 'SO_DOCUMENT_SEND_API1'
          exporting
            document_data              = wa_doc_data
            put_in_outbox              = 'X'
            commit_work                = 'X'
          tables
            packing_list               = t_pk_list
            contents_bin               = t_temp
            contents_txt               = t_con_txt
            receivers                  = t_reciever
          exceptions
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            others                     = 8.

*----CS2021000508 - 07.06.2021 - JT - inicio
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        else.
          message 'Email(s) enviado(s) com sucesso!' type 'I' display like 'S'.
        endif.
      endloop.
    endif.
    "ZLES0136 - ENVIO EMAIL P MAIS DE UM DEST. PT3 - BG #144498
  else.
*   MESSAGE 'Erro a Gerar Arquivo PDF' TYPE 'I'.
    exit.
*----CS2021000508 - 07.06.2021 - JT - fim
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_files tables it_file_table structure file_info using lva_declaracao.

  data : http_client type ref to if_http_client.
  data : url type string.
  data : content type xstring.
  data : wa_fil type ty_fileinfo.
  data : l_dados_selo type xstring.
  data : l_docnum     type j_1bnfdoc-docnum.
  data: wa_file_table type file_info.
  data: l_id_ordem     type zde_id_ordem.
  data: l_nr_safra     type zsdt0001od-nr_safra.
  data: l_nr_safra2    type zpmt0054-safra.

  free: l_id_ordem,
        l_nr_safra.

  clear: it_pdffiles.

  loop at it_file_table into wa_file_table.

    url =  wa_file_table-filename.

    call method cl_http_client=>create_by_url
      exporting
        url                = url
      importing
        client             = http_client
      exceptions
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        others             = 4.

    if sy-subrc = 0.

      http_client->send( ).
      http_client->receive( ).
      content = http_client->response->get_data( ).
      http_client->close( ).

      wa_fil-filename = wa_file_table-filename.
      wa_fil-data = content.
      wa_fil-len = xstrlen( content ).

      append wa_fil to it_pdffiles.
      clear: wa_fil.
    endif.

  endloop.

*** Dar o append na carta.
  if lva_declaracao is not initial.
    wa_fil-filename = 'Declaração do Motorista'.
    wa_fil-data = lva_declaracao.
    wa_fil-len = xstrlen( lva_declaracao ).

    append wa_fil to it_pdffiles.
    clear: wa_fil.
  endif.
*** fim append.

*-CS2020000656 - 13.05.2021 - JT - inicio
  l_docnum   = wa_saida-danfe.
  l_id_ordem = wa_saida-id_ordem.

  if l_id_ordem is initial.
    select id_ordem
      into l_id_ordem
      from zsdt0001
        up to 1 rows
     where ch_referencia = wa_saida-ch_referencia
       and tp_movimento  = wa_saida-tp_movimento.
    endselect.
  endif.

  select nr_safra
    into l_nr_safra
    from zsdt0001od
      up to 1 rows
   where id_ordem = l_id_ordem.
  endselect.

  l_nr_safra2 = l_nr_safra.

  call function 'ZSD_IMPRIME_SELO'
    exporting
      i_docnum                 = l_docnum
      i_safra                  = l_nr_safra2
      i_imprime_selo           = ' '
    importing
      e_xstring_document       = l_dados_selo
    exceptions
      documento_nao_autorizado = 1
      documento_nao_imprimir   = 2
      others                   = 3.

  if sy-subrc = 0 and l_dados_selo is not initial.
    wa_fil-filename = 'Selo'.
    wa_fil-data     = l_dados_selo.
    wa_fil-len      = xstrlen( l_dados_selo ).
    append wa_fil  to it_pdffiles.
    clear: wa_fil.
  endif.
*-CS2020000656 - 13.05.2021 - JT - inicio

endform.


form f_set_encerramento_docs changing p_saida type ty_saida.

  check  p_saida-ch_referencia is not initial.

  if p_saida-transp is not initial and ( p_saida-transp(1) ne '@' ).

    try.

        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
         exporting
            i_tknum            = conv #( p_saida-transp )
         importing
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

      catch zcx_faturamento into data(_zcx_fat).
      catch zcx_error       into data(_zcx_error).
    endtry.

  else.

    try.

        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
          exporting
            i_ch_romaneio      =  conv #( wa_zsdt0001-ch_referencia )
          importing
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

      catch zcx_faturamento into _zcx_fat.
      catch zcx_error       into _zcx_error.
    endtry.

  endif.

  case vg_cockpit.
    when '04'.

      case p_saida-tp_frete.
        when 'CIF'.

          if ( p_saida-tipo_veiculo eq 'P' and p_saida-tipo_remetente eq 'P' ).

            "US 83810 - Inversão Fluxo Faturamento Porto Velho - WPP
            p_saida-enc_doc_custo = abap_true.

*            CASE p_saida-tipo.
*              WHEN 'P'. "Pedido
*                p_saida-enc_doc_custo = abap_true.
*              WHEN 'T' OR "Transferencia
*                   'O'.   "Ordem Venda
*                p_saida-enc_danfe     = abap_true.
*            ENDCASE.

          endif.

        when 'FOB' or 'CFR'.

*         IF p_saida-TIPO_VEICULO EQ 'P'.
*           p_saida-ENC_CONHECIMENTO = ABAP_TRUE.
*         ENDIF.

      endcase.

    when others.

      case p_saida-tp_frete.
        when 'CIF'.

          if ( p_saida-tipo_veiculo eq 'P' and p_saida-tipo_remetente eq 'P' ).
            p_saida-enc_doc_custo = abap_true.
          endif.

        when 'FOB' or 'CFR'.

*      IF p_saida-TIPO_VEICULO EQ 'P'.
*        p_saida-ENC_CONHECIMENTO = ABAP_TRUE.
*      ENDIF.
      endcase.

  endcase.

  if p_saida-emite_conhecimento eq abap_true.
    p_saida-enc_doc_custo = abap_false.
  endif.


endform.
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

form f_valida_placas_faturamento using p_saida type ty_saida
                              changing p_error
                               raising zcx_error. "*-#133089-12.02.2024-JT

  data: v_valida_placa type p.

*-#133089-21.02.2024-JT-inicio
  create object lc_faturamento_automatico.
*-#133089-21.02.2024-JT-fim

  clear: p_error.

  if p_saida-placa_cav is not initial.

    call function 'Z_MASC_PLACA_VEICULO'
      exporting
        i_placa  = p_saida-placa_cav
      importing
        e_return = v_valida_placa.

    if v_valida_placa ne 0.
*-#133089-21.02.2024-JT-inicio
      case vg_faturamento_autom.
        when abap_off.
          p_error = abap_true.
          message |Formato da placa: { p_saida-placa_cav } é inválido.| type 'I'.
          exit.
        when abap_true.
          data(l_mesg) = |Formato da placa: { p_saida-placa_cav } é inválido.|.
          lc_faturamento_automatico->set_mensagem( i_cod = '99' i_mesg = l_mesg ).
      endcase.
*-#133089-21.02.2024-JT-fim
    endif.

  endif.

  if p_saida-placa_car1 is not initial.

    call function 'Z_MASC_PLACA_VEICULO'
      exporting
        i_placa  = p_saida-placa_car1
      importing
        e_return = v_valida_placa.

    if v_valida_placa ne 0.
      p_error = abap_true.
      message |Formato da placa: { p_saida-placa_car1 } é inválido.| type 'I'.
      exit.
    endif.

  endif.

  if p_saida-placa_car2 is not initial.

    call function 'Z_MASC_PLACA_VEICULO'
      exporting
        i_placa  = p_saida-placa_car2
      importing
        e_return = v_valida_placa.

    if v_valida_placa ne 0.
      p_error = abap_true.
      message |Formato da placa: { p_saida-placa_car2 } é inválido.| type 'I'.
      exit.
    endif.

  endif.

  if p_saida-placa_car3 is not initial.

    call function 'Z_MASC_PLACA_VEICULO'
      exporting
        i_placa  = p_saida-placa_car3
      importing
        e_return = v_valida_placa.

    if v_valida_placa ne 0.
      p_error = abap_true.
      message |Formato da placa: { p_saida-placa_car3 } é inválido.| type 'I'.
      exit.
    endif.

  endif.


endform.

form f_get_doc_rem_romaneio changing c_saida        type ty_saida
                                     c_zsdt0001_new type zsdt0001.

  if c_saida-remessa eq icon_execute_object or c_saida-remessa is initial.

    c_zsdt0001_new-status = abap_false.

    v_xblnr = c_saida-ch_referencia.
    select single *
      from likp into wa_likp
     where xblnr = v_xblnr
       and spe_loekz = ''.
    if sy-subrc = 0.
      c_saida-remessa        = wa_likp-vbeln.
      c_zsdt0001_new-doc_rem = wa_likp-vbeln.
      c_zsdt0001_new-status  = abap_true.
    endif.

  elseif c_saida-remessa is not initial and c_saida-remessa(1) ne '@' .

    c_zsdt0001_new-status = abap_true.

    clear: wa_likp.

    do 5 times.
      select single *
        from likp into wa_likp
       where vbeln = c_saida-remessa
         and spe_loekz = ''.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.
    enddo.

    v_xblnr = wa_likp-xblnr+0(20).
    if ( sy-subrc ne 0 ) or ( v_xblnr ne c_saida-ch_referencia ).
      if c_saida-tipo = 'O'.
        c_saida-remessa = icon_execute_object.
      else.
        c_saida-remessa = icon_icon_list.
      endif.

      c_zsdt0001_new-doc_rem = ''.
      c_zsdt0001_new-status  = abap_false.
    endif.

  endif.

endform.

form f_get_fat_nf_romaneio changing c_saida        type ty_saida
                                      c_zsdt0001_new type zsdt0001.

  data: lva_vbeln_rom type likp-vbeln. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL

  if ( c_saida-fatura  eq icon_execute_object or  c_saida-fatura  is initial ) and
     ( c_saida-remessa is not initial and c_saida-remessa(1) ne '@' ).

    if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).
      select single a~vbeln a~mjahr
        from vbfa as a into (vl_vbeln,vl_mjahr)
       where a~vbelv = c_saida-remessa
         and a~vbtyp_n  = 'R'
         and a~vbtyp_v  = 'J'
         and not exists ( select *
                            from mseg as b
                           where b~smbln eq a~vbeln "estorno
                         ).
    else.
      select single a~vbeln a~mjahr
        from vbfa as a into (vl_vbeln,vl_mjahr)
       where a~vbelv = c_saida-remessa
         and a~vbtyp_n  = 'M'
         and a~vbtyp_v  = 'J'
         and not exists ( select *
                            from vbfa as b
                           where b~vbelv   = a~vbeln
                             and b~vbtyp_n = 'N' "estorno
                         ).
    endif.

    if sy-subrc = 0.
      c_saida-fatura             = vl_vbeln.
      c_zsdt0001_new-fatura_prod = vl_vbeln.

    else.
      c_zsdt0001_new-fatura_prod = ''.

      c_saida-danfe = icon_execute_object.
      c_zsdt0001_new-nro_nf_prod = ''.

    endif.

  elseif ( c_saida-fatura is not initial and c_saida-fatura(1) ne '@' ).

    if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).

      do 5 times.
        select single *
          from mkpf into @data(wl_mkpf)
         where mblnr eq @c_saida-fatura.

        if sy-subrc eq 0.
          exit.
        else.
          wait up to 2 seconds.
          sy-subrc = 4.
        endif.
      enddo.

    else.

      do 5 times.
        select single fksto
          from vbrk into vl_fksto
         where vbeln = c_saida-fatura.

        if sy-subrc eq 0.
          exit.
        else.
          wait up to 2 seconds.
          sy-subrc = 4.
        endif.
      enddo.

    endif.


    if sy-subrc ne 0. "Fatura não existe
      c_saida-fatura = icon_execute_object.
      c_zsdt0001_new-fatura_prod = ''.

    else.

      "Check Estorno Documento
      if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).
        select single *
          from mseg into @data(wl_seg_estorno)
         where smbln eq @c_saida-fatura.
      else.
        select single vbeln mjahr "se estiver estornada
          from vbfa into (vl_vbeln,vl_mjahr)
         where vbelv = c_saida-fatura
           and vbtyp_n  = 'N'. "estorno
      endif.

      if sy-subrc = 0. "Achou estorno
        clear wl_erro.
        perform f_chk_estorno_fiscal using c_saida
                                           ''
                                  changing wl_erro.

        if wl_erro is initial.
          c_saida-fatura = icon_execute_object.
          c_zsdt0001_new-fatura_prod = ''.
        endif.
      else.

        lva_vbeln_rom = c_saida-remessa.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lva_vbeln_rom
          importing
            output = lva_vbeln_rom.


        if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).
          select single vbelv mjahr
            from vbfa into (vl_vbeln,vl_mjahr)
           where vbeln = c_saida-fatura
             and vbtyp_n  = 'R'
             and vbtyp_v  = 'J'
             and vbelv    = lva_vbeln_rom. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL
        else.
          select single vbelv mjahr
            from vbfa into (vl_vbeln,vl_mjahr)
           where vbeln = c_saida-fatura
             and vbtyp_n  = 'M'
             and vbtyp_v  = 'J'
             and vbelv    = lva_vbeln_rom. "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL
        endif.



        if sy-subrc ne 0. "fatura é de outra remessa "WPP 28/04/2023 - Correçao Recuperação Fatura Agrupada - SUL

          c_saida-fatura = icon_execute_object.
          c_zsdt0001_new-fatura_prod = ''.
        endif.
      endif.

    endif.

  endif.

endform.

form f_get_danfe_romaneio changing c_saida        type ty_saida
                                     c_zsdt0001_new type zsdt0001.


  if ( c_saida-danfe  eq icon_execute_object or c_saida-danfe  is initial ) and
     ( c_saida-fatura is not initial and c_saida-fatura(1) ne '@' ) .

    if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).
      clear: vl_vbeln, vl_mjahr.
      select single vbeln mjahr
        from vbfa into (vl_vbeln,vl_mjahr)
       where vbelv = c_saida-remessa
         and vbtyp_n  = 'R'
         and vbtyp_v  = 'J'.

      concatenate vl_vbeln vl_mjahr into vl_refkey.
      select single docnum
        from j_1bnflin into vl_docnum
       where refkey = vl_refkey.
    else.
      select single docnum
        from j_1bnflin into vl_docnum
       where refkey = c_saida-fatura.
    endif.

    if sy-subrc = 0.

      perform f_check_auth_doc using vl_docnum.

      if sy-subrc = 0.
        c_saida-danfe = vl_docnum.
        c_zsdt0001_new-nro_nf_prod = vl_docnum.
      endif.
    endif.

  elseif ( c_saida-danfe  is not initial and c_saida-danfe(1)  ne '@' ) and
         ( c_saida-fatura is not initial and c_saida-fatura(1) ne '@' ).

    if ( c_saida-tipo = 'P' ) or ( c_saida-tipo = 'T' ).
      clear: vl_vbeln, vl_mjahr.
      select single vbeln mjahr
        from vbfa into (vl_vbeln,vl_mjahr)
       where vbelv = c_saida-remessa
         and vbtyp_n  = 'R'
         and vbtyp_v  = 'J'.

      concatenate vl_vbeln vl_mjahr into vl_refkey.
      select single docnum
        from j_1bnflin into vl_docnum
       where refkey = vl_refkey.
    else.
      select single docnum
        from j_1bnflin into vl_docnum
       where refkey = c_saida-fatura.
    endif.

    if sy-subrc = 0.
      "Danfe cancelada  OU fatura/remessa não é da DANFE.

      perform f_check_canc_doc using vl_docnum.

      if ( sy-subrc = 0 ) or
         ( vl_docnum ne c_saida-danfe ).
        c_saida-danfe = icon_execute_object.
        c_zsdt0001_new-nro_nf_prod = ''.
      endif.
      "WPP - Ajuste Preenchimento Danfe incorreta no Romaneio - 05-07-2023  - Ini
    else.
      c_saida-danfe = icon_execute_object.
      c_zsdt0001_new-nro_nf_prod = ''.
    endif.
  elseif c_saida-fatura eq icon_execute_object.
    c_saida-danfe = icon_execute_object.
    c_zsdt0001_new-nro_nf_prod = ''.
  endif.

endform.

form f_get_tknum_romaneio changing c_saida        type ty_saida
                                   c_zsdt0001_new type zsdt0001.

  data: v_vbeln        type likp-vbeln.

  select single *
    from tvtk into @data(wl_tvtk)
   where shtyp = @c_saida-shtyp.

  check ( sy-subrc eq 0 ) or ( wl_tvtk-abfer is not initial ).

  clear: v_tknum, v_vbeln.

  case wl_tvtk-abfer.
    when '1' or '3'. "Saída
      if ( c_saida-remessa    is not initial ) and
         ( c_saida-remessa(1) ne '@'         ).
        v_vbeln         = c_saida-remessa.
        data(_vbtyp_v)  = 'J'.
      endif.
    when '2' or '4'. "Entrada
      if ( c_saida-aviso      is not initial ) and
         ( c_saida-aviso(1)   ne '@'         ).
        v_vbeln   = c_saida-aviso.
        _vbtyp_v  = '7'.
      endif.
  endcase.

  if ( c_saida-transp eq icon_execute_object ) and
     ( v_vbeln is not initial ) .
    select single vttk~tknum into v_tknum
      from vbfa inner join vttk on  vttk~tknum = vbfa~vbeln
                                and vttk~vsart = wl_tvtk-vsart
     where vbfa~vbelv    = v_vbeln
       and vbfa~vbtyp_n  = '8'
       and vbfa~vbtyp_v  = _vbtyp_v.
    if sy-subrc = 0.
      c_saida-transp = v_tknum.
      c_zsdt0001_new-doc_transp = v_tknum.
    endif.
  elseif ( c_saida-transp is not initial and c_saida-transp(1) ne '@' ).

    do 5 times.
      select single vttk~tknum into v_tknum
        from vbfa inner join vttk on  vttk~tknum = vbfa~vbeln
                                and vttk~vsart = wl_tvtk-vsart
       where vbfa~vbelv    = v_vbeln
         and vbfa~vbtyp_n  = '8'
         and vbfa~vbtyp_v  = _vbtyp_v.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.
    enddo.

    if sy-subrc ne 0.
      c_saida-transp = icon_execute_object.
      c_zsdt0001_new-doc_transp = ''.
    elseif c_saida-transp ne v_tknum.
      c_saida-transp = v_tknum.
      c_zsdt0001_new-doc_transp = v_tknum.
    endif.
  endif.



endform.

form f_get_fknum_romaneio changing c_saida        type ty_saida
                                     c_zsdt0001_new type zsdt0001.

  if ( c_saida-doccus+0(1) = '@' ) and
     ( c_saida-transp is not initial and c_saida-transp(1) ne '@' ).
    select single fknum
      from vfkp into vl_fknum
     where rebel = c_saida-transp.
    if sy-subrc = 0.
      c_saida-doccus = vl_fknum.
      c_zsdt0001_new-fknum = vl_fknum.
    endif.
  elseif ( c_saida-doccus is not initial and c_saida-doccus(1) ne '@' ).

    do 5 times.
      select single fknum
        from vfkp into vl_fknum
       where rebel = c_saida-transp.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.
    enddo.

    if sy-subrc ne 0.
      c_saida-doccus = icon_icon_list.
      c_zsdt0001_new-fknum = ''.
    elseif c_saida-doccus ne vl_fknum.
      c_saida-doccus = vl_fknum.
      c_zsdt0001_new-fknum = vl_fknum.
    endif.
  endif.

endform.

form f_get_ov_serv_romaneio changing c_saida        type ty_saida
                                       c_zsdt0001_new type zsdt0001.

  if ( c_saida-ovserv+0(1) = '@' ) and
     ( c_saida-transp is not initial and c_saida-transp(1) ne '@' ).
    select single  vbeln auart  kunnr
      from vbak into wa_vbak
     where tknum = c_saida-transp.
    if sy-subrc = 0.
      c_saida-ovserv = wa_vbak-vbeln.
      c_zsdt0001_new-ov_frete = wa_vbak-vbeln.
    endif.
  elseif c_saida-ovserv is not initial and c_saida-ovserv(1) ne '@'.

    do 5 times.

      select single  vbeln auart  kunnr
        from vbak into wa_vbak
       where tknum = c_saida-transp.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.

    enddo.

    if sy-subrc ne 0.
      c_saida-ovserv = icon_icon_list.
      c_zsdt0001_new-ov_frete = ''.
    elseif c_saida-ovserv ne wa_vbak-vbeln.
      c_saida-ovserv            = wa_vbak-vbeln.
      c_zsdt0001_new-ov_frete = wa_vbak-vbeln.
    endif.
  endif.

  if ( c_saida-ovserv+0(1) = '@' ).
    "Limpar Fatura Serviço e DACTE
    c_saida-fatserv = icon_icon_list.
    c_saida-dacte  = icon_execute_object.

    c_zsdt0001_new-fatura_frete = ''.
    c_zsdt0001_new-nro_nf_frete = ''.
  endif.

endform.

form f_get_fat_serv_romaneio changing c_saida        type ty_saida
                                        c_zsdt0001_new type zsdt0001.

  if ( c_saida-fatserv+0(1) = '@' ) and
     ( c_saida-ovserv is not initial and c_saida-ovserv(1) ne '@' ).

    select single a~vbeln a~mjahr
      from vbfa as a into (vl_vbeln,vl_mjahr)
     where a~vbelv = c_saida-ovserv
       and a~vbtyp_n  = 'M'
       and a~vbtyp_v  = 'C'
       and not exists ( select *
                          from vbfa as b
                         where b~vbelv   = a~vbeln
                           and b~vbtyp_n = 'N' "estorno
                       ).
    if sy-subrc = 0.
      c_saida-fatserv = vl_vbeln.
      c_zsdt0001_new-fatura_frete = vl_vbeln.
    else.
      c_saida-dacte   = icon_execute_object.
      c_zsdt0001_new-nro_nf_frete = ''.
    endif.

  elseif ( c_saida-fatserv is not initial and c_saida-fatserv(1) ne '@' ).

    do 5 times.
      select single fksto
        from vbrk into vl_fksto
       where vbeln = c_saida-fatserv.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.
    enddo.


    if sy-subrc ne 0. "Fatura não existe
      c_saida-fatserv = icon_icon_list.
      c_zsdt0001_new-fatura_frete = ''.
    else.
      select single vbeln mjahr
        from vbfa into (vl_vbeln,vl_mjahr)
       where vbelv = c_saida-fatserv
         and vbtyp_n  = 'N'. "estorno
      if sy-subrc = 0. "Achou estorno
        c_saida-fatserv = icon_icon_list.
        c_zsdt0001_new-fatura_frete = ''.
      endif.
    endif.
  endif.

endform.

form f_get_dacte_romaneio changing c_saida        type ty_saida
                                   c_zsdt0001_new type zsdt0001.

  if ( c_saida-dacte eq icon_execute_object ) and
     ( c_saida-fatserv is not initial and c_saida-fatserv(1) ne '@' ).
    clear: vl_docnum.

    select single docnum
      from j_1bnflin into vl_docnum
     where refkey = c_saida-fatserv.

    if ( sy-subrc = 0 ) and ( vl_docnum is not initial ).

      perform f_check_auth_doc using vl_docnum.

      if sy-subrc = 0.
        c_saida-dacte  = vl_docnum.
        c_zsdt0001_new-nro_nf_frete = vl_docnum.
      endif.
    endif.
  elseif ( c_saida-dacte   is not initial and c_saida-dacte(1)   ne '@' ) and
         ( c_saida-fatserv is not initial and c_saida-fatserv(1) ne '@' ).

    clear: vl_docnum.
    select single docnum
      from j_1bnflin into vl_docnum
     where refkey = c_saida-fatserv.

    perform f_check_canc_doc using vl_docnum.

    if  ( sy-subrc = 0  and vl_docnum is not initial ) or ( vl_docnum ne c_saida-dacte ). "dacte cancelada  OU fatura não é da DACTE.
      c_saida-dacte  = icon_execute_object.
      c_zsdt0001_new-nro_nf_frete = ''.
    endif.

  elseif ( c_saida-fatserv eq icon_execute_object ) or ( c_saida-fatserv eq icon_icon_list ).
    c_saida-dacte          = icon_execute_object.
    c_zsdt0001_new-nro_nf_frete = ''.
  endif.

endform.

form f_get_doc_znfw_romaneio changing c_saida        type ty_saida
                                      c_zsdt0001_new type zsdt0001.

  data: lva_zfiwrt0008_rom type zfiwrt0008.

  check c_saida-dt_movimento >= '20190418'.

  clear: lva_zfiwrt0008_rom.

  if ( c_saida-seq_lcto(1) eq '@' ) or ( c_saida-seq_lcto is initial ).
    select single *
      from zfiwrt0008 into @data(_wl_zfiwrt0008)
     where ch_referencia   eq @c_saida-ch_referencia
       and loekz           eq @abap_false
       and docs_estornados eq @abap_false.

    if sy-subrc = 0.
      c_saida-seq_lcto = _wl_zfiwrt0008-seq_lcto.
      c_zsdt0001_new-seq_lcto = _wl_zfiwrt0008-seq_lcto.
      c_zsdt0001_new-status   = 'X'.
      lva_zfiwrt0008_rom      = _wl_zfiwrt0008.

    else.  "Inclusão condição referente erro em PRD /  IR130107 / AOENNING.
      c_zsdt0001_new-status = abap_false.
    endif.



  else. "Limpa

    data(_clear_seq_lcto) = abap_false.

    select single *
      from zfiwrt0008 into _wl_zfiwrt0008
     where ch_referencia   eq c_saida-ch_referencia
       and loekz           eq abap_false
       and docs_estornados eq abap_false.

    if ( sy-subrc eq 0 ) and ( _wl_zfiwrt0008-seq_lcto eq c_saida-seq_lcto ).
      lva_zfiwrt0008_rom = _wl_zfiwrt0008.
    else.
      c_saida-seq_lcto        = icon_execute_object.
      c_zsdt0001_new-seq_lcto = ''.
      c_zsdt0001_new-status   = ''.
    endif.
  endif.

  "LES - Inversão Fluxo Fert. PV US #83810 - WPP - Ini
  case vg_cockpit. "Recomposição de Status
    when '01' or '05' or '06' or '07' or '03' or '09'.

    when '04'. " Fertilizantes (Porto Velho)

      if ( lva_zfiwrt0008_rom-ebeln is not initial ) and ( c_zsdt0001_new-ebeln is initial or c_zsdt0001_new-ebelp is initial  ).
        c_zsdt0001_new-ebeln = lva_zfiwrt0008_rom-ebeln.
        c_zsdt0001_new-ebelp = lva_zfiwrt0008_rom-ebelp.
      endif.

  endcase.
  "LES - Inversão Fluxo Fert. PV US #83810 - WPP - Fim


endform.

form f_get_danfe_znfw_romaneio changing c_saida        type ty_saida
                                        c_zsdt0001_new type zsdt0001.

  check c_saida-dt_movimento >= '20190418'.

  if ( c_saida-danfez(1) eq '@' ) or ( c_saida-danfez is initial ).

    select single *
      from zfiwrt0008 into @data(_wl_zfiwrt0008)
     where ch_referencia   eq @c_saida-ch_referencia
       and loekz           eq @abap_false
       and docs_estornados eq @abap_false.

    if ( sy-subrc = 0 ) and ( _wl_zfiwrt0008-docnum is not initial ).

      perform f_check_auth_doc using _wl_zfiwrt0008-docnum.

      if sy-subrc = 0.
        c_saida-danfez   = _wl_zfiwrt0008-docnum.
        c_zsdt0001_new-nro_nf_rem = _wl_zfiwrt0008-docnum.
      endif.

    endif.

  else. "Limpa

    select single *
      from zfiwrt0008 into _wl_zfiwrt0008
     where ch_referencia   eq c_saida-ch_referencia
       and loekz           eq abap_false
       and docs_estornados eq abap_false.

    if ( sy-subrc ne 0 ) or ( _wl_zfiwrt0008-docnum ne c_saida-danfez ).
      c_saida-danfez = icon_execute_object.
      c_zsdt0001_new-nro_nf_rem = ''.
    endif.

  endif.

endform.

form f_get_aviso_romaneio changing c_saida        type ty_saida
                                   c_zsdt0001_new type zsdt0001.

  data: v_vbeln type likp-vbeln.

  check c_saida-dt_movimento >= '20190418'.

  if ( c_saida-aviso(1) eq '@' ) or ( c_saida-aviso is initial ).

    select single *
      from likp into wa_likp
     where berot         = c_saida-ch_referencia
       and vbtyp         = '7'
       and spe_loekz     = ''.

    if sy-subrc = 0.
      c_saida-aviso              = wa_likp-vbeln.
      c_zsdt0001_new-doc_aviso = wa_likp-vbeln.
    endif.

  elseif c_saida-aviso is not initial and c_saida-aviso(1) ne  '@'.

    clear: wa_likp.

    v_vbeln = |{ c_saida-aviso alpha = in }|.

    do 5 times.

      select single *
        from likp into wa_likp
       where vbeln     = v_vbeln
         and spe_loekz = ''.

      if sy-subrc eq 0.
        exit.
      else.
        wait up to 2 seconds.
        sy-subrc = 4.
      endif.
    enddo.

    if ( sy-subrc ne 0 ) or ( wa_likp-berot ne c_saida-ch_referencia ).
      c_saida-aviso = icon_execute_object.
      c_zsdt0001_new-doc_aviso = ''.
    endif.

  endif.

endform.

form f_build_status_romaneio_01 using p_zsdt0001_current type zsdt0001
                             changing c_saida            type ty_saida.

  c_saida-st_proc = ''.

  if ( c_saida-remessa is not initial ) and ( c_saida-remessa(1) ne '@' ).
    c_saida-st_proc = vg_st_remessa.
  endif.

  if ( c_saida-fatura is not initial ) and ( c_saida-fatura(1) ne '@' ).
    c_saida-st_proc = vg_st_fatura.

    if c_saida-operacao(4) = 'ZPAR'.
      c_saida-st_proc = vg_st_finalizado.
      exit.
    endif.
  endif.

  if ( c_saida-danfe is not initial ) and ( c_saida-danfe(1) ne '@' ).

    c_saida-st_proc = vg_st_danfe.

    if ( c_saida-inco1 = 'FOB' or c_saida-inco1 = 'CFR' ) and ( not c_saida-enc_conhecimento = abap_true ).

      if c_saida-troca_nota            = abap_true and
         c_saida-docs_enviado_carguero = abap_false.
        c_saida-st_proc = vg_st_aguard_doc_carg.
      else.
        c_saida-st_proc = vg_st_finalizado.
      endif.

    endif.
  endif.

  if ( c_saida-transp is not initial ) and ( c_saida-transp(1) ne '@' ).
    c_saida-st_proc = vg_st_transp.
  endif.

  if ( c_saida-doccus is not initial ) and ( c_saida-doccus(1) ne '@' ).
    c_saida-st_proc = vg_st_custo.

    if ( c_saida-inco1 = 'CPT' ) or ( c_saida-enc_doc_custo eq abap_true ).

      if ( c_saida-danfe is not initial ) and ( c_saida-danfe(1) ne '@' ).  "Ajustes Geração Frete CPT 27/02/24
        if c_saida-troca_nota            = abap_true and
           c_saida-docs_enviado_carguero = abap_false.
          c_saida-st_proc = vg_st_aguard_doc_carg.
        else.
          c_saida-st_proc = vg_st_finalizado.
        endif.
      endif.

    endif.
  endif.

  if ( c_saida-ovserv is not initial ) and ( c_saida-ovserv(1) ne '@' ).
    c_saida-st_proc = vg_st_ov_frete.
  endif.

  if ( c_saida-fatserv is not initial ) and ( c_saida-fatserv(1) ne '@' ).
    c_saida-st_proc = vg_st_fatura_frete.
  endif.

  if ( c_saida-dacte is not initial ) and ( c_saida-dacte(1) ne '@' ) and
     ( c_saida-inco1 ne 'CPT' ).

    if c_saida-troca_nota            = abap_true and
       c_saida-docs_enviado_carguero = abap_false.
      c_saida-st_proc = vg_st_aguard_doc_carg.
    else.
      c_saida-st_proc = vg_st_finalizado.
    endif.

  endif.

  if ( c_saida-danfe is not initial ) and ( c_saida-danfe(1) ne '@' ).
    data(_rem_conta_ordem) = abap_false.
    perform f_check_rem_conta_ordem using p_zsdt0001_current  "c_saida *-CS2024000086-25.09.2024-#133287-JT-inicio
                                 changing _rem_conta_ordem.
    if _rem_conta_ordem eq abap_true.
      if p_zsdt0001_current-st_proc ne vg_st_finalizado.
        message i000(z01) with 'O Docto de Transporte do romaneio:' c_saida-nr_romaneio
                               ', deverá ser gerado '
                               'pela transação ZLES0200. Romaneio será finalizado!'.

      endif.
      c_saida-st_proc = vg_st_finalizado.
    endif.
  endif.

endform.

form f_build_status_romaneio_02 using p_zsdt0001_current type zsdt0001
                             changing c_saida type ty_saida.

  c_saida-st_proc = ''.

  if ( c_saida-seq_lcto is not initial ) and ( c_saida-seq_lcto(1) ne '@' ).
    c_saida-st_proc = vg_st_znfw.
  else.
    c_saida-st_proc = ''.
  endif.

  if ( c_saida-danfez is not initial ) and ( c_saida-danfez(1) ne '@' ).
    c_saida-st_proc = vg_st_danfe_znfw.
  endif.

  if ( c_saida-aviso is not initial ) and ( c_saida-aviso(1) ne '@' ).
    c_saida-st_proc = vg_st_aviso_rec.
  endif.

  if ( c_saida-remessa is not initial ) and ( c_saida-remessa(1) ne '@' ).
    c_saida-st_proc = vg_st_remessa.
  endif.

  if ( c_saida-fatura is not initial ) and ( c_saida-fatura(1) ne '@' ).
    c_saida-st_proc = vg_st_fatura.
  endif.

  if ( c_saida-danfe is not initial ) and ( c_saida-danfe(1) ne '@' ).
    c_saida-st_proc = vg_st_danfe.
  endif.

  if ( c_saida-transp is not initial ) and ( c_saida-transp(1) ne '@' ).
    c_saida-st_proc = vg_st_transp.
  endif.

  if ( c_saida-doccus is not initial ) and ( c_saida-doccus(1) ne '@' ).
    c_saida-st_proc = vg_st_custo.
  endif.

  if ( c_saida-ovserv is not initial ) and ( c_saida-ovserv(1) ne '@' ).
    c_saida-st_proc = vg_st_ov_frete.
  endif.

  if ( c_saida-fatserv is not initial ) and ( c_saida-fatserv(1) ne '@' ).
    c_saida-st_proc = vg_st_fatura_frete.
  endif.

  if ( c_saida-dacte is not initial ) and ( c_saida-dacte(1) ne '@' ).
    c_saida-st_proc = vg_st_dacte.
  endif.

  data(_finaliza_rom) = abap_false.
  data(_fat_incompleto)   = abap_false.


  if not ( ( c_saida-danfez is not initial ) and ( c_saida-danfez(1) ne '@' ) ).
    _fat_incompleto = abap_true.
  endif.

  case c_saida-tipo .
    when 'P'.
      data(_emite_danfe) = abap_false.
    when 'O' or 'T'.
      _emite_danfe       = abap_true.
  endcase.

  if ( _emite_danfe eq abap_true ) and  not ( ( c_saida-danfe is not initial ) and ( c_saida-danfe(1) ne '@' ) ).
    _fat_incompleto = abap_true.
  endif.

  case c_saida-inco1.
    when 'CIF'.

      case c_saida-enc_doc_custo.
        when abap_true.
          if not ( ( c_saida-doccus is not initial ) and
                   ( c_saida-doccus(1) ne '@'      ) ).
            _fat_incompleto = abap_true.
          endif.
        when abap_false.
          if not ( ( c_saida-dacte is not initial ) and
                   ( c_saida-dacte(1) ne '@'      ) ).
            _fat_incompleto = abap_true.
          endif.
      endcase.

    when 'CPT'.

      if not ( ( c_saida-doccus is not initial ) and
               ( c_saida-doccus(1) ne '@'      ) ).
        _fat_incompleto = abap_true.
      endif.

    when 'FOB' or 'CFR'.


  endcase.

  if _fat_incompleto eq abap_false.
    if c_saida-troca_nota            = abap_true and
       c_saida-docs_enviado_carguero = abap_false.
      c_saida-st_proc = vg_st_aguard_doc_carg.
    else.
      c_saida-st_proc = vg_st_finalizado.
    endif.
  endif.


endform.

form f_get_agente_fre_romaneio changing c_saida        type ty_saida
                                        c_zsdt0001_new type zsdt0001.

  data: v_vbeln        type likp-vbeln,
        v_agente_frete type zsdt0001-agente_frete.

  clear: v_vbeln, v_agente_frete.

  if ( c_saida-lifnr is initial ).
    case vg_cockpit.
      when '04'.

        if ( c_saida-aviso is not initial ) and ( c_saida-aviso(1) ne '@' ).
          v_vbeln = |{ c_saida-aviso alpha = in }|.
        endif.

        if ( c_saida-remessa is not initial ) and ( c_saida-remessa(1) ne '@' ).
          v_vbeln = |{ c_saida-remessa alpha = in }|.
        endif.

      when others.

        if ( c_saida-remessa is not initial ) and ( c_saida-remessa(1) ne '@' ).
          v_vbeln = |{ c_saida-remessa alpha = in }|.
        endif.

    endcase.

    if v_vbeln is not initial.
      select single *
        from vbpa into @data(lwa_vbpa_sp)
       where vbeln eq @v_vbeln
         and parvw eq 'SP'.

      if sy-subrc eq 0.
        v_agente_frete = lwa_vbpa_sp-lifnr.
      endif.
    endif.

  endif.

  if v_agente_frete is not initial.
    c_zsdt0001_new-agente_frete = v_agente_frete.
  endif.


endform.


*-CS2021000218-16.11.2022-#90706-JT-inicio
************************************************************************************
* Validar Ag.Frete qdo gerar remessa
************************************************************************************
form f_validar_ag_frete  using p_inco1
                               p_lifnr
                      changing p_erro
                               p_mesg.

  free: p_erro, p_mesg.

  check p_inco1 = 'FOB'.
  check p_lifnr is not initial.

  select single ktokk
    into @data(l_ktokk)
    from lfa1
   where lifnr = @p_lifnr.

  if sy-subrc = 0 and l_ktokk = 'ZFIC'.
    p_mesg = 'Para Carga FOB o transportador não pode ser Intercompany!'.
    p_erro = abap_true.
  endif.

endform.

************************************************************************************
* Validar Receituario Agronomico
************************************************************************************
form f_validar_solicitacao_ra  using p_nro_cg
                                     p_ch_referencia
                            changing p_saida              type ty_saida
                                     p_erro.

  data: l_chave_assina type i,
        l_pdf_assina   type i,
        l_num_receita  type zsdt0218-numeroreceita.

  free: p_erro.

  check p_saida-remessa is initial or p_saida-remessa(1) = '@' .

  select single gera_solicitacao_ra, qtd_solicitacao_ra
    from zsdt0302
    into @data(w_0302)
   where nro_cgd       = @p_nro_cg
     and ch_referencia = @p_ch_referencia.

  check sy-subrc = 0.
  check w_0302-gera_solicitacao_ra = abap_on.

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
  select receitakey, tipo_assinatura, status
    from zsdt0298
    into table @data(t_0298)
   where nro_cgd       = @p_nro_cg
     and ch_referencia = @p_ch_referencia
     and cancelado     = @abap_off.

  if t_0298[] is initial.
    free: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Obrig.Emissão de Sol.Receita.' &&
                         'Ver.se todas as SR foram geradas pelo Rom.na Carga:' && p_nro_cg.
    append tg_log_erro.

    perform f_grava_log_erro tables tg_log_erro
                              using p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    exit.
  endif.

  describe table t_0298 lines data(l_lines_298).

  if w_0302-qtd_solicitacao_ra <> l_lines_298.
    free: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Obrig.Emissão de Sol.Receita.' &&
                         'Ver.se todas as SR foram geradas pelo Rom.na Carga:' && p_nro_cg.
    append tg_log_erro.

    perform f_grava_log_erro tables tg_log_erro
                              using p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    exit.
  endif.

  read table t_0298 into data(w_0298) index 1.

  check w_0298-tipo_assinatura <> '0'. "manual.

*-------------------------------
* validar status das RAs
*-------------------------------
  loop at t_0298 into w_0298.
    if w_0298-status <> '4'.
      free: tg_log_erro.
      tg_log_erro-msgtyp = 'E'.
      tg_log_erro-msgid  = 'AGRIQ'.
      tg_log_erro-msgv1  = 'Existem Receitas não Finalizadas. Verifique no sistema AgriQ.'.
      append tg_log_erro.

      perform f_grava_log_erro tables tg_log_erro
                                using p_saida.
      p_saida-remessa = icon_led_yellow.
      p_saida-icon    = icon_led_red.
      p_erro          = abap_true.
      exit.
    endif.
  endloop.

  check p_erro = abap_false.

*-------------------------------
* validar status assinaturas
*-------------------------------
  clear: l_chave_assina, l_pdf_assina, l_num_receita.

  loop at t_0298 into w_0298.

    select chave_assinatura, chave_pdf_assinado, numeroreceita
      into @data(w_0218)
      from zsdt0218
        up to 1 rows
     where receitakey = @w_0298-receitakey
       and cancelada  = @abap_off.
    endselect.

    check sy-subrc = 0.

    if w_0218-chave_assinatura   is not initial.
      l_chave_assina = l_chave_assina + 1.
    endif.
    if w_0218-chave_pdf_assinado is not initial.
      l_pdf_assina   = l_pdf_assina + 1.
    else.
      l_num_receita  = w_0218-numeroreceita.
    endif.
  endloop.

  if     l_chave_assina <> l_lines_298.
    free: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = 'Existe(m) Receita(s) pendente de envio para a Assinatura eletrônica'.
    append tg_log_erro.

    perform f_grava_log_erro tables tg_log_erro
                              using p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    exit.
  elseif l_pdf_assina <> l_lines_298.
    free: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'AGRIQ'.
    tg_log_erro-msgv1  = '"A Receita N°:' && l_num_receita && ', está pendente da Assinatura eletrônica'.
    append tg_log_erro.

    perform f_grava_log_erro tables tg_log_erro
                              using p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
    exit.
  endif.

  p_saida-remessa = icon_execute_object.

endform.
*-CS2021000218-16.11.2022-#90706-JT-fim


*-CS2023000189-26.05.2023-#108752-JT-inicio
************************************************************************************
* Validar se romaneio tem ZMM0023 efetuada
************************************************************************************
form f_validar_transf_algodao  using p_ch_referencia
                            changing p_saida              type ty_saida
                                     p_erro.

  free: p_erro.

  check p_saida-remessa is initial or p_saida-remessa(1) = '@' .

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

  select mblnr
   into @data(_mblnr)
   from zsdt0330
     up to 1 rows
  where ch_referencia = @p_ch_referencia
    and mblnr         = @abap_off
    and status_fardo <> '3'
    and cancelado     = @abap_off.
  endselect.

  if sy-subrc = 0.
    free: tg_log_erro.
    tg_log_erro-msgtyp = 'E'.
    tg_log_erro-msgid  = 'REMESSA'.
    tg_log_erro-msgv1  = 'Não foram efetuadas Transferência ' &&
                         'de todos os fardos na ZMM0023!'.
    append tg_log_erro.

    perform f_grava_log_erro tables tg_log_erro
                              using p_saida.
    p_saida-remessa = icon_led_yellow.
    p_saida-icon    = icon_led_red.
    p_erro          = abap_true.
  endif.

endform.
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
form f_check_faturamento_ecc .

  data: lva_msg_retorno type string,
        lva_ok          type char01.

  call method cl_grid->get_selected_rows
    importing
      et_index_rows = tl_rows.

  if lines( tl_rows[] ) eq 0.
    message 'Selecione pelo menos uma linha!' type 'I'.
    exit.
  endif.

  loop at tl_rows into sl_rows.

    read table it_saida assigning field-symbol(<fs_out>) index sl_rows-index.

    check sy-subrc = 0.

    select single *
      from zsdt0001 into @data(lwa_zsdt0001)
     where ch_referencia = @<fs_out>-ch_referencia.

    check sy-subrc eq 0 and lwa_zsdt0001-fat_contingencia_ecc = abap_true.

    call function 'ZLES_FAT_CONTINGENCIA_0002'
      exporting
        i_ch_referencia = <fs_out>-ch_referencia
      importing
        e_ok            = lva_ok
        e_msg_retorno   = lva_msg_retorno.

    if lva_ok eq abap_false.
      message lva_msg_retorno type 'I'.
    else.
      message lva_msg_retorno type 'S'.
    endif.

  endloop.

  perform f_refresh_alv using '0100'. "Refresh na tela

endform.
*&---------------------------------------------------------------------*
*& Form f_get_doc_material_remessa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- C_SAIDA
*&      <-- LWA_ZSDT0001_NEW
*&---------------------------------------------------------------------*
form f_get_doc_mat_rem_zsdt0023  changing c_saida        type ty_saida.
*                                          c_zsdt0001_new TYPE zsdt0001.

  data: zcheck_update type char01.
  clear: zcheck_update.

  if c_saida-ch_referencia is not initial.

    "Verifica os documentos tabela ZSDT0023 com base na remessa.
    select single *
    from zsdt0023
    into @data(wa_zsdt0023)
    where ch_referencia eq @c_saida-ch_referencia
      and vbeln         eq @c_saida-remessa   "#181497-22.05.2024-JT
      and es_mblnr_s eq @space.

    if sy-subrc eq 0.
      "Verifica se tem documentos de material gerados vinculado a remessa.
      select a~mblnr, a~mjahr, a~bwart  from mseg as a
        inner join mkpf as b on b~mblnr eq a~mblnr
        into table @data(gt_mkpf)
      where b~bktxt eq @c_saida-remessa
        and a~smbln eq @space.
      if sy-subrc eq 0.
        read table gt_mkpf into data(ws_mkpf_s) with key bwart = 'F50'.
        read table gt_mkpf into data(ws_mkpf_e) with key bwart = 'F52'.

        if wa_zsdt0023-mblnr_s is initial and ws_mkpf_s-mblnr is not initial.
          wa_zsdt0023-mblnr_s = ws_mkpf_s-mblnr.
          wa_zsdt0023-mjahr_s = ws_mkpf_s-mjahr.
          zcheck_update = abap_true.
        endif.

        if wa_zsdt0023-mblnr_e is initial and ws_mkpf_e-mblnr is not initial.
          wa_zsdt0023-mblnr_e = ws_mkpf_e-mblnr.
          wa_zsdt0023-mjahr_e = ws_mkpf_e-mjahr.
          zcheck_update = abap_true.
        endif.
      endif.
    endif.

    if zcheck_update eq abap_true.
      modify zsdt0023 from wa_zsdt0023.
      commit work.
    endif.
  endif.

  clear: ws_mkpf_s, ws_mkpf_e, wa_zsdt0023.
  free : gt_mkpf.
endform.

*-CS2024000086-25.09.2024-#133287-JT-inicio
*&---------------------------------------------------------------------*
*& valida geracao VT frota propria
*&---------------------------------------------------------------------*
form f_check_gera_vt_frota_propria    using p_ch_ref
                                   changing p_gerar_vt.

  free: p_gerar_vt.

  try.
      zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
        exporting
          i_ch_romaneio      = conv #( p_ch_ref )
        importing
          e_tipo_veiculo     = data(_tipo_veiculo)
          e_tipo_remetente   = data(_tipo_remetente)
          e_tp_frete         = data(_tp_frete) ).

    catch zcx_faturamento into data(_zcx_fat).
      return.
    catch zcx_error       into data(_zcx_error).
      return.
  endtry.

  if _tipo_veiculo  = 'P ' and _tipo_remetente = 'P' and _tp_frete = 'CIF'.
    p_gerar_vt = abap_true.
  endif.

endform.
*-CS2024000086-25.09.2024-#133287-JT-fim

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
