*----------------------------------------------------------------------*
***INCLUDE ZFIR0043_O01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: fcode type table of sy-ucomm.
  refresh: fcode.

  if wg_acao is initial.
    append c_save to fcode.
    append c_deldoc to fcode.
  endif.

  read table t_usermd with key from = sy-uname.
  if sy-subrc ne 0.
    xbotao = 'N'.
  else.
    xbotao = 'S'.
  endif.

  clear     wg_cadpro-mensagem.
  if g_tab_tela-subtela = '0210'.
    if xbloqueio = 'X' or xestorno = 'X' or  xbotao = 'N'.
      append c_add      to fcode.
      append c_modif    to fcode.
      append 'IMPORTAR' to fcode.
    elseif xadd = 'X'.
      append c_add      to fcode.
    endif.
    append 'CALCULAR' to fcode.
    append 'GERAR'    to fcode.
    append 'ATUALIZAR' to fcode.
    append c_deldoc   to fcode.
    append c_save     to fcode.
    wg_cadpro-mensagem = 'Projeção BM&F'.
  elseif g_tab_tela-subtela = '0211'.
    if xbloqueio = 'X' or xestorno = 'X' or  xbotao = 'N'.
      append 'CALCULAR' to fcode.
    endif.
    append 'IMPORTAR' to fcode.
    append 'GERAR'    to fcode.
    append 'ATUALIZAR' to fcode.
    append c_deldoc   to fcode.
    append c_add      to fcode.
    append c_modif    to fcode.
    append c_displa   to fcode.
    append c_save     to fcode.
    wg_cadpro-mensagem = 'Calculo Taxas Interpolação'.
  elseif g_tab_tela-subtela = '0212'.
    append 'CALCULAR' to fcode.
    append 'GERAR'    to fcode.
    append 'ATUALIZAR' to fcode.
    append 'IMPORTAR' to fcode.
    append c_deldoc   to fcode.
    append c_add      to fcode.
    append c_modif    to fcode.
    append c_displa   to fcode.
    append c_save     to fcode.
    if xcod_oper_nav = 'N'. " NDF
      wg_cadpro-mensagem = 'Valorização Operação - NDF'.
    elseif xcod_oper_nav = 'S'. " Swap S
      wg_cadpro-mensagem = 'Valorização Operação - SWAP Fluxo C. T.V.'.
    elseif xcod_oper_nav = 'V'. " Swap V
      wg_cadpro-mensagem = 'Valorização Operação - SWAP Fluxo C. T.F.'.
    elseif xcod_oper_nav = 'H'. " Swap H
      wg_cadpro-mensagem = 'Valorização Operação - SWAP Vanila'.
    endif.
  elseif g_tab_tela-subtela = '0213'.
    append 'CALCULAR' to fcode.
    if xbloqueio = 'X' or xestorno = 'X' or  xbotao = 'N'.
      append 'GERAR'    to fcode.
    else.
      append 'ATUALIZAR' to fcode.
    endif.
    append 'IMPORTAR' to fcode.
    append c_deldoc   to fcode.
    append c_add      to fcode.
    append c_modif    to fcode.
    append c_displa   to fcode.
    append c_save     to fcode.
    if  xestorno_botao = 'X'.
      wg_cadpro-mensagem = 'Estorno Contabilização anterior'.
    elseif xcod_oper_nav = 'N'. " NDF.
      wg_cadpro-mensagem = 'Resumo Contabilização - NDF'.
    elseif xcod_oper_nav = 'S'. " SWAP FLUXO CAIXA
      wg_cadpro-mensagem = 'Resumo Contabilização - SWAP Fluxo C. T.F.'.
    elseif xcod_oper_nav = 'V'. " SWAP FLUXO CAIXA
      wg_cadpro-mensagem = 'Resumo Contabilização - SWAP Fluxo C. T.V.'.
    elseif xcod_oper_nav = 'H'. " SWAP VANILA
      wg_cadpro-mensagem = 'Resumo Contabilização - SWAP VANILA'.
    endif.
  elseif g_tab_tela-subtela = '0214'.
    if xcalculo = 'X'  or  xbotao = 'N'. "GRAVOU NA ZIB_CONTABIL ou não está autorizado a gravar
      append c_save to fcode.
      append c_deldoc to fcode.
    endif.
    append 'CALCULAR' to fcode.
    append 'GERAR'    to fcode.
    append 'ATUALIZAR' to fcode.
    append 'IMPORTAR' to fcode.
    append c_add      to fcode.
    append c_modif    to fcode.
    append c_displa   to fcode.
    wg_cadpro-mensagem = 'Fim do Processo & Gravação dos cálculos'.
  endif.

  if wg_acao is initial or wg_acao = c_displa.
    append c_save to fcode.
    append c_deldoc to fcode.
    if xmodif = 'X' or wg_acao is initial .
      append c_modif to fcode.
    endif.
  elseif xmodif = 'X' .
    append c_modif to fcode.
  endif.

  set pf-status 'Z001' excluding fcode.
  call method cl_gui_cfw=>dispatch.
  set titlebar '0200'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.

  perform  f_cria_objetos.

endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIT0060'       'SEQ'               'TG_ITENS' 'SEQ'               'Seq.'             '10' ' ' ' ' 'X',
        2 'ZFIT0060'       'DIAS_CORRIDOS'     'TG_ITENS' 'DIAS_CORRIDOS'     'Dias Corridos'    '10' 'X' ' ' 'X',
        3 'ZFIT0060'       'TX_PROJ_ANO_BAN'   'TG_ITENS' 'TX_PROJ_ANO_BAN'   'Tx. Bancária'     '10' 'X' ' ' 'X',
        4 'ZFIT0060'       'TX_PROJ_ANO_COM'   'TG_ITENS' 'TX_PROJ_ANO_COM'   'Tx. Comercial'    '10' 'X' ' ' 'X',
        5 'ZFIT0060'       'TX_PAR_DOLAR'      'TG_ITENS' 'TX_PAR_DOLAR'      'Tx. Paridade'     '10' 'X' ' ' 'X',
        6 'ZFIT0060'       'TX_CUPOM_CAMB'     'TG_ITENS' 'TX_CUPOM_CAMB'     'Cupom Cambial'    '10' 'X' ' ' 'X',
        7 'ZFIT0060'       'DT_BASE_BMF'       'TG_ITENS' 'DT_BASE_BMF'       'Dt.Base – dias corridos'   '20' ' ' ' ' 'X',
        8 'ZFIT0060'       'DIAS_UTEIS'        'TG_ITENS' 'DIAS_UTEIS'        'Dias Uteis'       '10' ' ' ' ' 'X'.

endform.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_layout2 .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIT0062'       'DT_VCTO_CTO'         'TG_ITENS2' 'DT_VCTO_CTO'       'Data Vct. Contrato'       '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'SEQ'                 'TG_ITENS2' 'SEQ'               'Sequência'                '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_C_MTM_CTO'      'TG_ITENS2' 'DIAS_C_MTM_CTO'    'Dias_C_MTM_Cto'           '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_C_PRI_INTER'    'TG_ITENS2' 'DIAS_C_PRI_INTER'  'Dias_C_Pri_Inter'         '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_C_SEQ_INTER'    'TG_ITENS2' 'DIAS_C_SEQ_INTER'  'Dias_C_Seq_Inter'         '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_U_MTM_CTO'      'TG_ITENS2' 'DIAS_U_MTM_CTO'    'Dias_U_MTM_Cto'           '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_U_PRI_INTER'    'TG_ITENS2' 'DIAS_U_PRI_INTER'  'Dias_U_Pri_Inter'         '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'DIAS_U_SEQ_INTER'    'TG_ITENS2' 'DIAS_U_SEQ_INTER'  'Dias_U_Seq_Inter'         '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_BCO_TP'           'TG_ITENS2' 'TX_BCO_TP'         'Tx_Bco_T+1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_BCO_TM'           'TG_ITENS2' 'TX_BCO_TM'         'Tx_Bco_T-1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_BCO_INTERP'       'TG_ITENS2' 'TX_BCO_INTERP'     'Tx_Bco_Inter252'          '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_COM_T1P'          'TG_ITENS2' 'TX_COM_T1P'        'Tx_Com_T+1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_COM_T1M'          'TG_ITENS2' 'TX_COM_T1M'        'Tx_Com_T-1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_COM_INTERP'       'TG_ITENS2' 'TX_COM_INTERP'     'Tx_Com_Interp360'         '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_CB_TP'            'TG_ITENS2' 'TX_CB_TP'          'Tx_Cb_Tp+1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_CB_TM'            'TG_ITENS2' 'TX_CB_TM'          'Tx_Cb_Tp-1'               '10' ' ' ' ' 'X',
        2 'ZFIT0062'       'TX_CB_INTERP'        'TG_ITENS2' 'TX_CB_INTERP'      'Tx_Cb_InterpUSD'          '10' ' ' ' ' 'X'.

endform.                    " MONTAR_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_edit)
                            value(p_sum)
                            value(p_emphasize).

  clear w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  if p_outputlen is not initial.
    w_fieldcatalog-outputlen      = p_outputlen.
  endif.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  if p_field eq 'ESTORNO'.
    w_fieldcatalog-checkbox = c_x.
  endif.

  if p_field eq 'HKONT' or p_field eq 'UMSKZ'.
    w_fieldcatalog-f4availabl = c_x.
  endif.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module inicializa_tela output.
  if wg_acao is initial.
    xcod_oper_nav = wg_cadpro-cod_oper.

    if g_tab_tela-subtela is initial.
      g_tab_tela-subtela   = '0210'.
    endif.

    refresh: tg_fields.
    perform trata_campos using space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0

    " Usuários que podem mudar o Status
    call function 'G_SET_GET_ALL_VALUES'
      exporting
        class         = '0000'
        setnr         = 'MAGGI_ZFI0043_USER'
      tables
        set_values    = t_usermd
      exceptions
        set_not_found = 1
        others        = 2.
    if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
    sort t_usermd by from.

  endif.
endmodule.                 " INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields output.

* layout exclusivo divida bullet
  loop at screen.
    if ( screen-name eq 'WG_CADPRO-P_DIVIDA' ).
      if  g_tab_tela-subtela eq '0212' and xcod_oper_nav eq 'H'. " Swap Vanila
        screen-active    = 1.
        screen-invisible = 0.
        modify screen.
      else.
        screen-active    = 0.
        screen-invisible = 1.
        modify screen.
      endif.
      exit.
    endif.
  endloop.
  loop at tg_fields.
    loop at screen.
      if screen-name eq tg_fields-campo
      or screen-group1 eq tg_fields-group1.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        modify screen.
      endif.

      if g_tab_tela-subtela ne '0213'.
        if screen-name eq 'TXTDOC_CTB' or
          screen-name eq 'WG_CADPRO-BELNR' or
          screen-name eq 'WG_CADPRO-BELNR2' or
          screen-name eq 'BTN_REI'.
          screen-active    = 0.
          screen-invisible = 1.
          modify screen.
        endif.
      endif.

      if  ( g_tab_tela-subtela ne '0212' and g_tab_tela-subtela ne '0213' ) or wg_cadpro-cod_oper ne 'T'.
        if ( screen-name eq 'BTN_A' or  screen-name eq 'BTN_P' ).
          screen-active    = 0.
          screen-invisible = 1.
          modify screen.
        endif.
      endif.

      if g_tab_tela-subtela eq '0212' or
         g_tab_tela-subtela eq '0213' or
         g_tab_tela-subtela eq '0214'.

        if g_tab_tela-subtela eq '0213'.
          if screen-name eq 'BTN_REI'.
            if  wg_cadpro-belnr eq icon_message_warning_small or
                wg_cadpro-belnr eq icon_alert or "Estorno não concluido ZIB
                xestorno_botao = 'X' or
                xbotao ne 'S'.
              screen-active    = 0.
              screen-invisible = 1.
              modify screen.
            elseif wg_cadpro-belnr eq icon_incomplete.
              btn_rei = 'Reinicializar'.
            else.
              btn_rei = 'Estornar'.
            endif.
          endif.
        endif.

        if g_tab_tela-subtela eq '0214'.
          if screen-name eq 'BTN_PROX'.
            screen-input    = 0.
            modify screen.
          endif.
          if screen-name eq 'WG_COLAPS'.
            screen-active    = 0.
            screen-invisible = 1.
            modify screen.
          endif.
        endif.
      elseif g_tab_tela-subtela eq '0210'.
        if screen-name eq 'BTN_ANT'.
          screen-input    = 0.
          modify screen.
        endif.
      endif.
    endloop.
  endloop.

  if not 'H_V_T' cs wg_cadpro-cod_oper.
    loop at screen.
      if screen-name eq 'WG_CADPRO-PTAX' or
       screen-name eq 'TXTPTAX'.
        screen-input     = 0.
        screen-invisible = 1.
        clear wg_cadpro-ptax.
        modify screen.
      endif.
    endloop.
  endif.

  if wg_colaps eq '@K2@'.
    loop at screen.
      if screen-group4 eq 'B1'.
        screen-active    = 0.
        screen-invisible = 1.
        modify screen.
      endif.
    endloop.
  else.
    loop at screen.
      if screen-group4 eq 'B1'.
        screen-active    = 1.
        screen-invisible = 0.
        modify screen.
      endif.
    endloop.
  endif.


  if x_field is not initial.
    set cursor field x_field.
  endif.
endmodule.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0110 output.
  set pf-status 'Z002'.
  set titlebar '0110'.
endmodule.                 " STATUS_0110  OUTPUT
