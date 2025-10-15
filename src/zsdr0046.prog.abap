*/===========================================================================\*
*      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' *
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Izyan Nascimento ( izyan.nascimento@grupomaggi.com.br )              |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Paulo Quevedo ( paulo.quevedo@grupomaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Solicitação da Ordem de Venda - HEDGE                                     |*
*/===========================================================================\*

report  zsdr0046.

type-pools: vrm.
*=============================================================================*
*Tabela                                                                       *
*=============================================================================*

tables: zsdt0094, zsdt0051, sscrfields..

*=============================================================================*
*Estrutura                                                                     *
*=============================================================================*
types: begin of ty_saida,
         data_registro           type zsdt0094-data_registro,
         hora_registro           type zsdt0094-hora_registro,
         matnr                   type mara-matnr,
         maktx                   type makt-maktx,
         tpsim                   type zsdt0040-tpsim,
         tp_venda                type zsdt0051-tp_venda,
         zterm                   type zsdt0052-zterm,
         data_progr              type zsdt0055-data_progr,
         dtde_logist             type zsdt0051-dtde_logist,
         formula                 type kurrf,
         dmbtr                   type zsdt0053-dmbtr,
         pmein                   type zsdt0053-pmein,
         programa                type zsdt0094-programa,
         nro_sol_ov              type zsdt0094-nro_sol_ov,
         data_venc               type zsdt0094-data_venc,
         data_lib                type zsdt0094-data_lib,
         inco1                   type zsdt0051-inco1,
         cadencia_qte            type zsdt0094-cadencia_qte,
         zieme                   type zsdt0094-zieme,
         total_proporc           type zsdt0094-total_proporc,
         total_proporc_usd       type zsdt0094-total_proporc,
         total_proporc_usd_curva type zsdt0094-total_proporc,
         taxa_curva              type zsdt0094-taxa_curva,
         frete_cif               type zsdt0094-total_proporc,
         frete_porto             type zsdt0094-total_proporc,
         tipo                    type zsdt0094-tipo,
         tipo_taxa               type zsdt0094-tipo_taxa,
         fixacao                 type zsdt0094-fixacao,
         bezei                   type zsdt0094-bezei,
         charg                   type zsdt0053-charg,
         waerk                   type zsdt0051-waerk,
         vbeln                   type zsdt0094-vbeln,
         vkorg                   type vkorg,
         kunnr                   type kunnr,
         cli_forn                type name1,
         ktokd                   type ktokd,
         intercompany            type zsdt0094-intercompany,
       end of ty_saida.

*=============================================================================*
*Tabela_Interna                                                               *
*=============================================================================*
data:
  tg_0094  type table of zsdt0094,
  tg_saida type table of ty_saida.

data: gw_saida type ty_saida.
data: gt_bdc type table of bdcdata,
      gw_bdc type bdcdata.


*=============================================================================*
*Work_Area                                                                    *
*=============================================================================*
data:
  wg_0094   type zsdt0094,
  wg_saida  type ty_saida,
  wa_cont   type ref to cl_gui_custom_container,
  wa_alv    type ref to  cl_gui_alv_grid,
  wa_layout type lvc_s_layo.

data c_function  type c length 18.

data: var_opcao type c length 3.

*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
data:it_fcat    type table of lvc_s_fcat.
data:it_list    type vrm_values,
     list_value type vrm_values.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
data: variante   like disvariant,
      vg_repid   like sy-repid,
      vg_variant type disvariant.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
selection-screen:  begin of block b1 with frame title text-001.
  select-options: p_nr_sol  for zsdt0094-nro_sol_ov,
                  p_dt_ven  for zsdt0094-data_venc no-extension,
                  p_dt_lib  for zsdt0094-data_lib no-extension,
                  p_vkorg   for zsdt0051-vkorg.

  selection-screen begin of line.
    selection-screen comment 1(10) text-001.
  selection-screen end of line.

  parameters: p_varia like disvariant-variant.

  selection-screen begin of line.
    selection-screen comment 1(10) text-001.
  selection-screen end of line.

  parameters: r_mi radiobutton group gp2 default 'X' user-command p_0.
  parameters: r_in radiobutton group gp2.

  selection-screen begin of line.
    parameters: r_aq radiobutton group gp2.
    selection-screen comment 2(50) text-005 for field r_aq.
  selection-screen end of line.

  parameters: r_tp radiobutton group gp2.

  selection-screen begin of line.
    selection-screen comment 1(10) text-001.
  selection-screen end of line.

  selection-screen begin of line.
    parameters: r_venda  as checkbox default 'X' user-command p_1.
    selection-screen comment 2(50) text-002 for field r_venda.
  selection-screen end of line.

  selection-screen begin of line.
    parameters: r_frete  as checkbox user-command p_2.
    selection-screen comment 2(50) text-003 for field r_frete.
  selection-screen end of line.

  selection-screen begin of line.
    parameters: r_compra  as checkbox user-command p_3.
    selection-screen comment 2(50) text-004 for field r_compra.
  selection-screen end of line.

*PARAMETERS: R_VENDA RADIOBUTTON GROUP GP1 DEFAULT 'X',
*            R_FRETE RADIOBUTTON GROUP GP1.


selection-screen: end of block b1.
data: gv_ucomm         type sy-ucomm.

at selection-screen.
  gv_ucomm = sscrfields-ucomm.

  if variante is initial.
    variante-report   = sy-repid.
    variante-variant = p_varia.
  endif.

at selection-screen output.

  case gv_ucomm.
    when 'P_0'.
      case abap_true.
        when r_aq or r_tp. " OR R_TR OR R_SP.
          r_venda = r_compra = abap_false.
          r_frete = abap_true.
      endcase.
    when 'P_1'.
      case abap_true.
        when r_aq or r_tp. " OR R_TR OR R_SP.
          r_venda = r_compra = abap_false.
          r_frete = abap_true.
        when others.
          r_frete = r_compra = abap_false.
      endcase.
    when 'P_2'.
      case abap_true.
        when r_aq or r_tp. " OR R_TR OR R_SP.
          r_venda = r_compra = abap_false.
          r_frete = abap_true.
        when others.
          r_venda = r_compra = abap_false.
      endcase.
    when 'P_3'.
      case abap_true.
        when r_aq or r_tp. " OR R_TR OR R_SP.
          r_venda = r_compra = abap_false.
          r_frete = abap_true.
        when others.
          r_venda = r_frete  = abap_false.
      endcase.
  endcase.

at selection-screen on value-request for p_varia.

  vg_repid          = sy-repid.
  variante-report   = vg_repid.

  if ( not p_varia is initial ).
    vg_variant-variant = p_varia.

  endif.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = variante
      i_save        = 'A'
    importing
      es_variant    = variante
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if ( sy-subrc ne 0 ).
    message s000(z01) with 'Não existe variante'.
    stop.
  else.
    move variante-variant to p_varia.
  endif.

*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
start-of-selection.

  data: var_visao type zde_visao_zsdt0090.

  case abap_true.
    when r_mi.
      var_visao = '01'.
    when r_in.
      var_visao = '02'.
    when r_aq.
      var_visao = '03'.
    when r_tp.
      var_visao = '04'.
  endcase.

  "Check de permissão de visão
  authority-check object 'ZSDT0090'
    id 'ZVS_000003' field var_visao.

  if sy-subrc <> 0.
    select single *
      from user_addrp into @data(wl_user)
     where bname = @sy-uname.

    if ( sy-subrc = 0 ) and ( wl_user-name_first is not initial ).
      message | { wl_user-name_first }, seu perfil está sem acesso ao tipo de visão selecionada! | type 'S'.
    else.
      message | Perfil do usuário sem acesso ao tipo de visão! | type 'S'.
    endif.

    exit.
  endif.

  perform:
           f_seleciona_dados     , " Form selecionar dado
           f_alv                 . " ALV

  call screen 0100.

end-of-selection.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
form f_seleciona_dados.

  data: var_nr_sol_low type zsdt0094-nro_sol_ov,
        var_dt_ven_low type zsdt0094-data_venc,
        var_dt_lib_low type zsdt0094-data_lib,
        var_vkorg_low  type zsdt0051-vkorg.

  data: var_nr_sol_high type zsdt0094-nro_sol_ov,
        var_dt_ven_high type zsdt0094-data_venc,
        var_dt_lib_high type zsdt0094-data_lib,
        var_vkorg_high  type zsdt0051-vkorg.

  data: gt_nro_sol   type table of zsdt0094,
        gt_data_venc type table of zsdt0094,
        gt_data_lib  type table of zsdt0094,
        gt_vkorg     type table of zsdt0051.

  data: gw_nro_sol   type zsdt0094,
        gw_data_venc type zsdt0094,
        gw_data_lib  type zsdt0094,
        gw_vkorg     type zsdt0051.

  if not ( p_nr_sol-low is initial ).

    if ( p_nr_sol-high is initial ).
      loop at p_nr_sol.
        var_nr_sol_low = p_nr_sol-low.
        gw_nro_sol-nro_sol_ov = var_nr_sol_low.
        append gw_nro_sol to gt_nro_sol.
      endloop.
    else.

      var_nr_sol_low = p_nr_sol-low.
      gw_nro_sol-nro_sol_ov = var_nr_sol_low.
      append gw_nro_sol to gt_nro_sol.

      var_nr_sol_high = p_nr_sol-high.
      gw_nro_sol-nro_sol_ov = var_nr_sol_high.
      append gw_nro_sol to gt_nro_sol.

    endif.
  endif.

  if not ( p_dt_ven-low is initial ).
    var_dt_ven_low = p_dt_ven-low.
    gw_data_venc-data_venc = var_dt_ven_low.
    append gw_data_venc to gt_data_venc.

    if not ( p_dt_ven-high is initial ).
      var_dt_ven_high        = p_dt_ven-high.
      gw_data_venc-data_venc = var_dt_ven_high.
      append gw_data_venc to gt_data_venc.
    endif.
  endif.

  if not ( p_dt_lib-low is initial ).
    var_dt_lib_low = p_dt_lib-low.
    gw_data_lib-data_lib = var_dt_lib_low.
    append gw_data_lib to gt_data_lib.

    if not ( p_dt_lib-high is initial ).
      var_dt_lib_high = p_dt_lib-high.
      gw_data_lib-data_lib = var_dt_lib_high.
      append gw_data_lib to gt_data_lib.
    endif.
  endif.

  if not ( p_vkorg-low is initial ).

    if ( p_vkorg-high is initial ).

      loop at p_vkorg.
        var_vkorg_low = p_vkorg-low.
        gw_vkorg-vkorg = var_vkorg_low.
        append gw_vkorg to gt_vkorg.
      endloop.

    else.
      var_vkorg_low = p_vkorg-low.
      gw_vkorg-vkorg = var_vkorg_low.
      append gw_vkorg to gt_vkorg.

      var_vkorg_high = p_vkorg-high.
      gw_vkorg-vkorg = var_vkorg_high.
      append gw_vkorg to gt_vkorg.
    endif.
  endif.

  case abap_true.
    when r_mi.
      case abap_true.
        when r_venda. var_opcao = 'VDA'.
        when r_frete. var_opcao = 'FRE'.
        when r_compra.
          message 'Não há dados para esta Seleção!' type 'I'.
          exit.
      endcase.
      c_function = 'Z_SOLIC_TAXA_CURVA'.

    when r_in.
      case abap_true.
        when r_venda.
          var_opcao = 'VDI'.
          c_function = 'Z_SOLIC_T_CURVA_IN'.
        when r_frete.
          var_opcao = 'FRI'.
          c_function = 'Z_SOLIC_T_CURVA_IN'.
        when r_compra.
          var_opcao = 'PDI'.
          c_function = 'Z_SOLIC_T_CURVA_PD'.
      endcase.

    when r_aq or r_tp. "OR R_TR  OR R_SP.

      case abap_true.
        when r_aq. var_opcao = 'AQV'.
*        WHEN R_TR. VAR_OPCAO = 'TBO'.
        when r_tp. var_opcao = 'TBP'.
*        WHEN R_SP. VAR_OPCAO = 'SPT'.
      endcase.

      c_function = 'Z_SOLIC_T_CURVA_AQ'.

  endcase.

  call function c_function
    exporting
      i_opcao      = var_opcao
    importing
      it_resultado = tg_saida
    tables
      it_nr_ov     = gt_nro_sol
      it_data_venc = gt_data_venc
      it_data_lib  = gt_data_lib
      it_vkorg     = gt_vkorg.

endform.                    "F_SELECIONA_DADOS

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.
    methods: handle_hotspot_click for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_handler implementation.

  method handle_hotspot_click.
    perform handle_hotspot_click using e_row_id e_column_id es_row_no.
  endmethod.                    "handle_hotspot_click


endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION


*=============================================================================*
*Form F_Alv
*=============================================================================*
form f_alv.

  if ( r_venda eq abap_true ).
    perform alv_preenche_cat using:

          'DATA_REGISTRO'           'Data Registro'    '12'  '' ''   '',
          'HORA_REGISTRO'           'Hora Registro'    '12'  '' ''   '',
          'PROGRAMA'                'Programa'         '10'  '' ''   '',

          'MATNR'                   'Cod. Material'    '10'  '' 'X'   '',
          'MAKTX'                   'Descri. Material' '40'  '' ''   ''.

    case abap_true.
      when r_mi.
        perform alv_preenche_cat using:
            'TP_VENDA'                'Tipo de Venda'    '07'  '' ''   ''.
      when r_in.
        perform alv_preenche_cat using:
            'TPSIM'                   'Tipo de Venda'    '07'  '' ''   ''.
    endcase.

    perform alv_preenche_cat using:
          'ZTERM'                   'Cond. Pagto.'        '10'  '' ''   '',
          'DATA_PROGR'              'Data Cadência'       '12'  '' ''   '',
          'DTDE_LOGIST'             'Data Inic. Logis.'   '12'  '' ''   '',
          'VKORG     '              'Organização venda'   '10'  '' ''   '',
          'NRO_SOL_OV'              'Nr. Sol.OV'          '10'  'X' 'X'  '',
          'DATA_VENC'               'Data Venc.'          '12'  '' ''   '',
          'DATA_LIB'                'Data Lib.'           '12'  '' ''   '',
          'CADENCIA_QTE'            'Cadencia'            '15'  '' ''   '',
          'DMBTR'                   'Preço Venda Liq'     '10'  '' ''   '',
          'ZIEME'                   'UM'                  '05'  '' ''   '',
          'TOTAL_PROPORC'           'Vlr. Reais'          '15'  '' ''   '',
          'TOTAL_PROPORC_USD'       'Vlr. Dolar Câmbio'   '20'  '' ''   '',
          'TOTAL_PROPORC_USD_CURVA' 'Vlr. Dolar Curva'    '20'  '' ''   '',

          'TAXA_CURVA'              'Taxa Curva'          '10'  '' ''   '',
          'TIPO_TAXA'               'Tipo Taxa'           '05'  '' ''   '',
          'FORMULA'                 'Taxa Cambio'         '10'  '' ''   '',
          'FRETE_CIF'               'Frete CIF'           '10'  '' ''   '',
          'FRETE_PORTO'             'Frete Porto'         '10'  '' ''   '',
          'TIPO'                    'Tipo'                '05'  '' ''   '',
          'FIXACAO'                 'Fixação'             '10'  '' ''   '',
          'BEZEI'                   'Bezei'               '15'  '' ''   '',
          'CHARG'                   'Safra'               '05'  '' ''   '',
          'WAERK'                   'Moeda'               '05'  '' ''   '',
          'VBELN'                   'Ordem Venda'         '15'  '' ''   ''.

  else.
    perform alv_preenche_cat using:

          'DATA_REGISTRO'            'Data Registro'      '12'  '' ''   '',
          'HORA_REGISTRO'            'Hora Registro'      '12'  '' ''   '',
          'PROGRAMA'                 'Programa'           '10'  '' ''   '',
          'MATNR'                    'Cod. Material'      '10'  '' 'X'   '',
          'MAKTX'                    'Desc. Material'     '40'  '' ''   ''.

    case abap_true.
      when r_mi.
        perform alv_preenche_cat using:
            'TP_VENDA'                'Tipo de Venda'     '07'  '' ''   ''.
      when r_in.
        perform alv_preenche_cat using:
            'TPSIM'                   'Tipo de Venda'     '07'  '' ''   ''.
    endcase.

    perform alv_preenche_cat using:
          'ZTERM'                    'Cond. Pagto.'         '10'  '' ''   '',
          'DATA_PROGR'               'Data Cadência'        '12'  '' ''   '',
          'DTDE_LOGIST'              'Data Inic. Logis.'    '12'  '' ''   '',
          'VKORG      '              'Organização venda'    '10'  '' ''   '',
          'NRO_SOL_OV'               'Nr. Sol.OV'           '10'  'X' 'X'  '',
          'DATA_VENC'                'Data Venc.'           '12'  '' ''   '',
          'DATA_LIB'                 'Data Lib.'            '12'  '' ''   '',
          'INCO1'                    'Frete CIF/FOB'        '05'  '' ''   '',
          'CADENCIA_QTE'             'Cadencia'             '12'  '' ''   '',
          'DMBTR'                    'Preço Venda Liq'      '10'  '' ''   '',
          'ZIEME'                    'UM'                   '05'  '' ''   '',
          'TOTAL_PROPORC'            'Vlr. Reais'           '15'  '' ''   '',
          'TOTAL_PROPORC_USD'        'Vlr. Dolar Câmbio'    '20'  '' ''   '',
          'TOTAL_PROPORC_USD_CURVA'  'Vlr. Dolar Curva'     '20'  '' ''   '',
          'TAXA_CURVA'               'Taxa Curva'           '10'  '' ''   '',
          'TIPO_TAXA'                'Tipo Taxa'            '05'  '' ''   '',
          'FORMULA'                  'Taxa Cambio'          '10'  '' ''   '',
          'FRETE_CIF'                'Frete CIF'            '10'  '' ''   '',
          'FRETE_PORTO'              'Frete Porto'          '10'  '' ''   '',
          'TIPO'                     'Tipo'                 '05'  '' ''   '',
          'FIXACAO'                  'Fixação'              '10'  '' ''   '',
          'BEZEI'                    'Bezei'                '15'  '' ''   '',
          'CHARG'                    'Safra'                '05'  '' ''   '',
          'WAERK'                    'Moeda'                '05'  '' ''   ''.

  endif.

  case abap_true.
    when r_aq or r_tp." OR R_TR OR R_SP.
      loop at it_fcat assigning field-symbol(<fcat>)
        where fieldname eq 'CHARG'
           or fieldname eq 'FIXACAO'
           or fieldname eq 'FRETE_PORTO'
           or fieldname eq 'FRETE_CIF'
           or fieldname eq 'FRETE_PORTO'
           or fieldname eq 'DATA_PROGR'
           or fieldname eq 'DTDE_LOGIST'
           or fieldname eq 'NRO_SOL_OV'.

        if <fcat>-fieldname eq 'NRO_SOL_OV'.
          <fcat>-scrtext_l  = <fcat>-scrtext_m  = <fcat>-scrtext_s  = 'Doc. fatura'.
        else.
          <fcat>-no_out = abap_true.
        endif.

      endloop.

      perform alv_preenche_cat using:
          'CLI_FORN'                 'Fornecedor/Cliente'     '30'  '' ''   '',
          'INTERCOMPANY'             'Interc.'                '06'  '' ''   ''.

  endcase.


endform.                    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
form alv_preenche_cat   using   p_campo type c
                                p_desc  type c
                                p_tam   type c
                                p_hot   type c
                                p_zero  type c
                                p_sum   type c.
  data: wl_fcat type lvc_s_fcat.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    =  p_sum.
  wl_fcat-outputlen =  p_tam.


  append wl_fcat to it_fcat.

endform.                    " ALV_PREENCHE_CAT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status '0100'.

  case abap_true.
    when r_mi.
      set titlebar  '0100_01'.
    when r_in.
      set titlebar  '0100_02'.
    when r_aq.
      set titlebar  '0100_03'.
    when r_tp.
      set titlebar  '0100_04'.
  endcase.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exibe alv  OUTPUT
*&---------------------------------------------------------------------*
module exibe_alv output.

*  DATA: WL_VARIANT  TYPE DISVARIANT.
  data: gr_event_handler type ref to lcl_event_handler.

  if wa_cont is initial.

    create object wa_cont
      exporting
        container_name              = 'OBJ_0046'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    create object wa_alv
      exporting
        i_parent          = wa_cont
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

*  WL_VARIANT-REPORT   = SY-REPID.
*  WL_VARIANT-USERNAME = SY-UNAME.

    create object gr_event_handler.
    set handler gr_event_handler->handle_hotspot_click for wa_alv.

    call method wa_alv->set_table_for_first_display
      exporting
        is_layout                     = wa_layout
        i_save                        = 'A'
        is_variant                    = variante
      changing
        it_outtab                     = tg_saida
        it_fieldcatalog               = it_fcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  else.
    call method wa_alv->refresh_table_display.
  endif.
endmodule.                 " exibe alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
module z_user_command input.
  if sy-dynnr eq '0100'.
    case sy-ucomm.
      when 'BACK' or
           'CANC' or
           'EXIT'  .
        leave to screen 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      when 'REFRESH'.
        perform: f_seleciona_dados.
    endcase.
  endif.
endmodule.                 " Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
form handle_hotspot_click  using    i_row_id     type lvc_s_row
                                    i_column_id  type lvc_s_col
                                    is_row_no    type lvc_s_roid.
  data opt type ctu_params.
  free gt_bdc.

  case i_column_id.
    when: 'NRO_SOL_OV'.
      read table tg_saida into gw_saida index i_row_id.

      case abap_true.
        when r_compra.

          set parameter id 'BES' field gw_saida-nro_sol_ov.
          call transaction 'ME23N' and skip first screen.

        when others.

          case abap_true.
            when r_in.

              perform f_preencher_dynpro using:
                    'X' 'ZSDR016'                      '0100',
                    ' ' 'WG_HEADER-DOC_SIMULACAO'      gw_saida-nro_sol_ov,
                    ' ' 'BDC_OKCODE'                   'ATUAL'.

              opt-dismode = 'E'.
              opt-defsize = ' '.

              call transaction 'ZSDT0044' using gt_bdc options from opt.

            when r_mi.

              perform f_preencher_dynpro using:
                    'X' 'ZSDR0022'                      '0050',
                    ' ' 'WG_HEADER-NRO_SOL_OV'          gw_saida-nro_sol_ov,
                    ' ' 'BDC_OKCODE'                    'ATUAL'.

              opt-dismode = 'E'.
              opt-defsize = ' '.

              call transaction 'ZSDT0062' using gt_bdc options from opt.

            when r_aq or r_tp. " OR R_SP OR R_TR.

              set parameter id 'VF' field gw_saida-nro_sol_ov.
              call transaction 'VF03' and skip first screen.

          endcase.
      endcase.
  endcase.


endform.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1141   text
*      -->P_1142   text
*      -->P_1143   text
*----------------------------------------------------------------------*
form f_preencher_dynpro   using l_start type c l_name type c l_value.

  move l_start to gw_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to gw_bdc-program,
  l_value to gw_bdc-dynpro.
  else.
    move:
      l_name  to gw_bdc-fnam,
      l_value to gw_bdc-fval.
  endif.
  append gw_bdc to gt_bdc.
  clear: gw_bdc.

endform.                    " F_PREENCHER_DYNPRO
