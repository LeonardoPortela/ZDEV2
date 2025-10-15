*----------------------------------------------------------------------*
***INCLUDE ZFIR0025_FORMS.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_cpa .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 ' LIFNR'           'LFA1'        'TG_PAGAR' 'LIFNR'           'Fornecedor'(006)          '12' ' ' ' ' ' ',
        2 ' NAME1'           'LFA1'        'TG_PAGAR' 'NAME1'           'Nome Fornecedor'(007)     '30' ' ' ' ' ' ',
        3 ' '                ' '           'TG_PAGAR' 'BUDAT'           'Dt.Lcto'(005)             '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_PAGAR' 'BELNR'           'Documento'(202)           '12' ' ' ' ' ' ',
        5 ' '                ' '           'TG_PAGAR' 'INVOICE'         'Invoice'(007)             '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_PAGAR' 'VLR_PGTO'        'Valor Documento'(205)     '15' ' ' 'X' ' ',
        7 'BSID'             'DMBE2'       'TG_PAGAR' 'VLR_COMP'        'Vlr.Compensação'(291)     '15' 'X' 'X' ' ',
        8 ' '                ' '           'TG_PAGAR' 'VLR_SLD'         'Saldo Residual'(209)      '15' ' ' 'X' ' '.
endform.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ADTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_adta .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        3 ' '                ' '           'TG_ADIANT' 'BUDAT'           'Dt.Lcto'(005)             '10' ' ' ' ' ' ',
        4 ' '                ' '           'TG_ADIANT' 'BELNR'           'Documento'(202)           '12' ' ' ' ' ' ',
*        5 ' '                ' '           'TG_ADIANT' 'INVOICE'         'Sol.OV.'             '12' ' ' ' ' ' ',
        6 ' '                ' '           'TG_ADIANT' 'VLR_PGTO'        'Valor Documento'(205)     '15' ' ' ' ' ' ',
        7 'BSID'             'DMBE2'       'TG_ADIANT' 'VLR_COMP'        'Vlr.Compensação'(291)     '15' 'X' ' ' ' ',
        8 ' '                ' '           'TG_ADIANT' 'VLR_SLD'         'Saldo Residual'(209)      '15' ' ' ' ' ' '.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_IVAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_PAGAR  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
form f_shdb_ivad  changing p_alv like wg_pagar p_erro.
  data: vdata(10),
        vcampo(15),
        cnum_seq(2),
        vnum_seq    type i,
        vsaldo      type bsid-dmbe2,
        wl_vlr(16),
        wl_vlra(16),
        wl_vlrk(16).
  " Verifica se o saldo é ADIANTAMENTO
  vtotal_adt = 0.
  loop at tg_adiant into wg_adiant.
    if wg_adiant-vlr_comp gt 0 .
      add wg_adiant-vlr_sld to vtotal_adt.
    endif.
  endloop.

  " Verifica se o saldo é no contas a pagar
  vtotal_cp = 0.
  loop at tg_pagar into wg_pagar.
    if wg_pagar-vlr_comp gt 0  and wg_pagar-vlr_sld gt 0.
      add wg_pagar-vlr_sld to vtotal_cp.
    endif.
  endloop.

  refresh ti_bdcdata.
  concatenate  wg_cadcom-dt_pgto+6(2) wg_cadcom-dt_pgto+4(2) wg_cadcom-dt_pgto(4) into vdata  separated by '.'.

  clear wl_vlrk.
  perform f_bdc_data using:

    'SAPMF05A'  '0122'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=SL',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'SI',
    ''          ''      ''   'BKPF-BUKRS'       p_bukrs,
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       wg_cadcom-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       wg_cadinvo-waers, "'USD',
    ''          ''      ''   'BKPF-KURSF'       wl_vlrk,
    ''          ''      ''   'BKPF-WWERT'       vdata,

    'SAPMF05A'  '0710'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA',
    ''          ''      ''   'RF05A-AGBUK'      p_bukrs,
    ''          ''      ''   'RF05A-AGKON'      p_alv-lifnr,
    ''          ''      ''   'RF05A-AGKOA'      'K',
    ''          ''      ''   'RF05A-AGUMS'      'A',
    ''          ''      ''   'RF05A-XNOPS'      'X',
    ''          ''      ''   'RF05A-XPOS1(01)'  ' ',
    ''          ''      ''   'RF05A-XPOS1(03)'  'X',
    'SAPMF05A'  '0731'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'	      '=PA'.

  vtotal_cp = vtotal_cp * -1.
  write: vtotal_cp to wl_vlr.
  condense wl_vlr no-gaps.
  "
  write: vtotal_adt to wl_vlra.
  condense wl_vlra no-gaps.

  vnum_seq = 0.
  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cp.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cp.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cp.

  endloop.

  tg_compe_aux[] = tg_compe[].
  sort tg_compe_aux by belnr_cr.
  delete adjacent duplicates from tg_compe_aux comparing belnr_cr.
  loop at tg_compe_aux into wg_compe.
    add 1 to vnum_seq.
    cnum_seq = vnum_seq.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = cnum_seq
      importing
        output = cnum_seq.
    concatenate 'RF05A-SEL01(' cnum_seq ')' into vcampo.
    perform f_bdc_data using:
      ''          ''      ''   vcampo    wg_compe-belnr_cr.
  endloop.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=REST',
         ''          ''      ''   'RF05A-ABPOS'      '1'.
  " Posicionar no documento que ficará com saldo

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '/00',
         ''          ''      ''   'RF05A-ABPOS'      '1',
         ''          ''      ''   'DF05B-PSDIF(01)'  wl_vlr,
         ''          ''      ''   'DF05B-PSDIF(02)'  wl_vlra.

  perform f_bdc_data using:
         'SAPDF05X'  '3100'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=AB',
         ''          ''      ''   'RF05A-ABPOS'      '1',

         'SAPMF05A'  '0700'  'X'  ''                 ' ',
         ''          ''      ''   'BDC_OKCODE'        '=BU'.
  "ENDLOOP.


  clear p_erro.
  perform zf_call_transaction using 'F-51' changing p_erro.
endform.
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_DADOS_MODELO
*&---------------------------------------------------------------------*
form gravar_dados_modelo.

  data: wa_186 type zfit0186,
        lt_187 type table of zfit0187,
        lt_188 type table of zfit0188.

  data: returncode,
        lt_fields  type table of sval.

  call function 'SAPGUI_SET_FUNCTIONCODE'
    exporting
      functioncode           = '=ENT'
    exceptions
      function_not_supported = 1
      others                 = 2.

  append initial line to lt_fields assigning field-symbol(<fs_fields>).
  <fs_fields>-tabname   = 'ZFIT0186'.
  <fs_fields>-fieldname = 'DS_MODELO'.

  call function 'POPUP_GET_VALUES'
    exporting
*     NO_VALUE_CHECK  = ' '
      popup_title     = 'Informe uma descrição para o modelo'
      start_column    = '5'
      start_row       = '5'
    importing
      returncode      = returncode
    tables
      fields          = lt_fields
    exceptions
      error_in_fields = 1
      others          = 2.

  if returncode is initial.

    wa_186 = corresponding #( wg_cadger mapping operacao   = tp_operacao
                                                moeda_pgto = waers
                            ). "Cabeçalho

    read table lt_fields into data(wa_ds_modelo) with key fieldname = 'DS_MODELO'.
    if sy-subrc = 0.
      wa_186-ds_modelo = wa_ds_modelo-value.
    endif.

    select single * from zfit0186 into @data(wa)
      where ds_modelo = @wa_ds_modelo-value.
    if sy-subrc = 0.

      data: lv_answer.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Gravar Modelo'
          text_question         = 'Descrição do modelo já existe, deseja sobrescrever?'
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ' '
          popup_type            = 'ICON_MESSAGE_ERROR'
        importing
          answer                = lv_answer.

      check lv_answer = 1.
    endif.

    loop at tg_conta assigning field-symbol(<fs_conta>). "Conta bancária
      append initial line to lt_187 assigning field-symbol(<fs_187>).
      <fs_187> = corresponding #( <fs_conta> ).
      <fs_187>-ds_modelo = wa_186-ds_modelo.
    endloop.

    loop at tg_criaadt assigning field-symbol(<fs_adiant>). "Adiantamento
      clear: zva_tabix.
      zva_tabix = sy-tabix.
      append initial line to lt_188 assigning field-symbol(<fs_188>).

      <fs_188> = corresponding #( <fs_adiant> ).
      <fs_188>-ds_modelo = wa_186-ds_modelo.
      <fs_188>-item = zva_tabix.
      <fs_188>-wrbtr = ' '.
    endloop.

    modify zfit0186 from wa_186.       "Cabeçalho
    modify zfit0187 from table lt_187. "Conta bancária
    modify zfit0188 from table lt_188. "Adiantamento

    if sy-subrc = 0.
      message s274(lr). "O modelo foi gravado com êxito
    endif.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  BUSCAR_DADOS_MODELO
*&---------------------------------------------------------------------*
form buscar_dados_modelo.

  data: linha_selecionada type slis_selfield.
  data: _exit             type c.
  clear: wg_cadger, tg_conta[], tg_criaadt[].

  select *
    from zfit0186
    into table @data(lt_186). "Cabeçalho

  if sy-subrc <> 0.
    clear lt_186.
    message s147(k7) display like 'E'.
  endif.

  check sy-subrc = 0.

  data(tl_fieldcat) = value slis_t_fieldcat_alv(
*   ( fieldname = ''                 seltext_m = ''  outputlen = '' )
   ( fieldname = 'DS_MODELO'        seltext_m = 'Descrição do modelo  '  outputlen = '100' )
  ).

  call function 'REUSE_ALV_POPUP_TO_SELECT'
    exporting
      i_title     = 'Selecionar Modelo'
      i_selection = 'X'
      i_tabname   = 'LT_186'
      i_zebra     = 'X'
      it_fieldcat = tl_fieldcat
    importing
      es_selfield = linha_selecionada
      e_exit      = _exit
    tables
      t_outtab    = lt_186.

  if sy-subrc = 0.

    read table lt_186 index linha_selecionada-tabindex into data(wa_186).
    if sy-subrc = 0.

      wg_cadger = corresponding #( wa_186 mapping tp_operacao = operacao
                                                  waers = moeda_pgto
                                 ). "Cabeçalho

      select * from zfit0187 "Conta bancária
        into table @data(lt_187)
        where ds_modelo = @wa_186-ds_modelo.
      if sy-subrc <> 0.
        clear lt_187.
      endif.

      select * from zfit0188 "Adiantamento
        into table @data(lt_188)
        where ds_modelo = @wa_186-ds_modelo.
      if sy-subrc <> 0.
        clear lt_188.
      endif.

      loop at lt_187 assigning field-symbol(<fs_187>).
        append initial line to tg_conta assigning field-symbol(<fs_conta>).
        <fs_conta> = corresponding #( <fs_187> ).
      endloop.

      loop at lt_188 assigning field-symbol(<fs_188>).
        append initial line to tg_criaadt assigning field-symbol(<fs_adiant>).
        <fs_adiant> = corresponding #( <fs_188> ).
      endloop.

      call function 'SAPGUI_SET_FUNCTIONCODE'
        exporting
          functioncode           = '=ENT'
        exceptions
          function_not_supported = 1
          others                 = 2.

    endif.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  DELETAR_MODELO
*&---------------------------------------------------------------------*
form deletar_modelo .

  data: linha_selecionada type slis_selfield.
  data: _exit             type c.

  clear: wg_cadger, tg_conta[], tg_criaadt[].

  select *
    from zfit0186
    into table @data(lt_186). "Cabeçalho

  if sy-subrc <> 0.
    clear lt_186.
    message s147(k7) display like 'E'.
  endif.

  check sy-subrc = 0.

  data(tl_fieldcat) = value slis_t_fieldcat_alv(
      ( fieldname = 'DS_MODELO'        seltext_m = 'Descrição  '  outputlen = '100' )
  ).

  call function 'REUSE_ALV_POPUP_TO_SELECT'
    exporting
      i_title     = 'Selecionar Modelo'
      i_selection = 'X'
      i_tabname   = 'LT_186'
      i_zebra     = 'X'
      it_fieldcat = tl_fieldcat
    importing
      es_selfield = linha_selecionada
      e_exit      = _exit
    tables
      t_outtab    = lt_186.

  if sy-subrc = 0.

    read table lt_186 index linha_selecionada-tabindex into data(wa_186).
    if sy-subrc = 0.

      concatenate 'Excluir Modelo: ' wa_186-ds_modelo into data(lv_question) separated by space.
      data: lv_answer.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Deletar Modelo'
          text_question         = lv_question
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ' '
          popup_type            = 'ICON_MESSAGE_ERROR'
        importing
          answer                = lv_answer.
      if lv_answer = 1.

        delete from zfit0188 where ds_modelo = wa_186-ds_modelo.
        delete from zfit0187 where ds_modelo = wa_186-ds_modelo.
        delete from zfit0186 where ds_modelo = wa_186-ds_modelo.

        if sy-subrc = 0.
          clear: wg_cadger, tg_conta[], tg_criaadt[].
          message s015(cotpl) with wa_186-ds_modelo. "O modelo &1 foi eliminado
        else.
          message s037(pq) display like 'E'. "Ocorreu um erro ao eliminar
        endif.
      endif.
    endif.
  endif.

endform.
