*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviário                                              &*
*& Data.....: 25/11/2013                                              &*
*& Descrição: Frete Aquaviário - Entrada                              &*
*&--------------------------------------------------------------------&*
report  zlesr0077 message-id zaquaviario.

*----------------------------------------------------------------------*
* INCLUDES.
* INCLUDES.
*----------------------------------------------------------------------*
include: <icon>.
*----------------------------------------------------------------------*
* TABLES"
*----------------------------------------------------------------------*
tables: zlest0061.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
types: begin of ty_saida,
         status         type c length 4,
         auart          type zlest0061-auart,
         dt_fatura      type zlest0061-dt_fatura,
         peso_vinculado type db20199vp,
         vlr_usd        type zlest0061-vlr_usd,
         vlr_brl        type zlest0061-vlr_brl,
         cte_serie      type c length 13,
         cl_codigo      type zlest0061-cl_codigo,
         tknum          type zlest0061-tknum,
         fknum          type zlest0061-fknum,
         bukrs          type zlest0061-bukrs,
         werks          type zlest0061-werks,
         ano_viagem     type zlest0061-ano_viagem,
         nr_viagem      type zlest0061-nr_viagem,
         nome_emb       type zlest0061-nome_emb,
         nr_dco         type zlest0061-nr_dco,
         safra          type zlest0061-safra,
         docnum         type j_1bnfdoc-docnum,
         docnum_entr    type zlest0032-docnum,
         belnr          type zlest0032-belnr,
         shtyp          type zsdt0011-shtyp,
         tax_dolar      type zlest0061-tax_dolar,
         data_taxa      type zlest0061-data_taxa,
         waerk          type zlest0061-waerk,
         waerk_fatura   type zlest0061-waerk_fatura,
         frkrl_ump      type tvtk-frkrl_ump,
         estilo_cell    type lvc_t_scol,
         centro_fat	    type werks_d,
         operacao       type zlest0061-operacao,
         rm_codigo      type zlest0061-rm_codigo,
         vbeln          type vttp-vbeln, "aviso
       end of ty_saida,

       begin of ty_saida_message,
         type    type bapi_mtype,
         message type bapi_msg,
       end of ty_saida_message,

       begin of ty_aviso,
         po_number     type zmmt_ee_zgr-po_number,
         p_nfnum       type zlest0060-nfnum,
         p_series      type zlest0060-series,
         po_embarque   type zlest0056-po_embarque,
         po_destino    type zlest0056-po_destino,
         nftot         type j_1bnfdoc-nftot,
         ch_referencia type zsdt0001-ch_referencia,
         quantity      type zmmt_ee_zgr-quantity,
       end of ty_aviso,

       begin of ty_aviso_aquav,
         po_number     type zmmt_ee_zgr-po_number,
         po_item       type zmmt_ee_zgr-po_item,
         po_embarque   type zlest0056-po_embarque,
         po_destino    type zlest0056-po_destino,
         entry_qnt     type zmmt_ee_zgr-entry_qnt,
         ref_doc_no    type zmmt_ee_zgr-ref_doc_no,
         ch_referencia type zsdt0001-ch_referencia,
         "P_NFNUM       TYPE ZLEST0060-NFNUM,
         "P_SERIES      TYPE ZLEST0060-SERIES,
         dt_fatura     type zlest0061-dt_fatura,
         meins         type zmmt_ee_zgr-meins,
         material      type zmmt_ee_zgr-material,
         nftot         type j_1bnfdoc-nftot,
       end of ty_aviso_aquav,

       begin of ty_tp_mov,
         tp_movimento type zsdt0011-tp_movimento,
         docnum       type zlest0061-docnum,
       end of ty_tp_mov,

       begin of ty_dados_compl,
         docnum        type zlest0060-docnum,
         dt_fatura     type zlest0061-dt_fatura,
         rm_codigo     type zlest0060-rm_codigo,
         nfnum         type zlest0060-nfnum,
         series        type zlest0060-series,
         docnum_nf     type zlest0060-docnum,
         aviso_rec(10) type c,
       end of ty_dados_compl,

       begin of ty_dados_aviso,
         aviso_rec(10) type c,
       end of ty_dados_aviso.

*----------------------------------------------------------------------*
* Internal Tables
*----------------------------------------------------------------------*
data: gt_saida         type table of ty_saida,
      gt_zlest0061     type table of zlest0061,
      gt_kna1          type table of kna1,
      gt_j_1bnfdoc     type table of j_1bnfdoc,
      gt_zlest0060     type table of zlest0060,
      gt_zsdt0001      type table of zsdt0001,
      gt_zlest0032     type table of zlest0032,
      gt_zsdt0011      type table of zsdt0011,
      gw_zsdt0011      type zsdt0011,
      gt_tvtk          type table of tvtk,
      gw_tvtk          type tvtk,
      obj_custom       type ref to cl_gui_custom_container,
      obj_grid         type ref to cl_gui_alv_grid,
      obj_custom_msg   type ref to cl_gui_custom_container,
      obj_grid_msg     type ref to cl_gui_alv_grid,
      gt_fcat          type lvc_t_fcat,
      gt_fcat_msg      type lvc_t_fcat,
      gt_bdc           type table of bdcdata,
      gt_msg           type table of bdcmsgcoll,
      gt_saida_message type table of ty_saida_message,
      gt_zlest0056     type table of zlest0056,
      gt_tp_mov        type table of ty_tp_mov,
      gt_dados_compl   type table of ty_dados_compl,
      gt_dados_aviso   type table of ty_dados_aviso,
      gt_lfa1          type table of lfa1,
      gt_vttp          type table of vttp.

*----------------------------------------------------------------------*
* Work Area
*----------------------------------------------------------------------*
data: gw_saida         type ty_saida,
      gw_zlest0061     type zlest0061,
      gw_kna1          type kna1,
      gw_j_1bnfdoc     type j_1bnfdoc,
      gw_zlest0060     type zlest0060,
      gw_zsdt0001      type zsdt0001,
      gw_bdc           type bdcdata,
      gw_msg           type bdcmsgcoll,
      gw_saida_message type ty_saida_message,
      gw_zlest0032     type zlest0032,
      gw_fcat          type lvc_s_fcat,
      gw_fcat_msg      type lvc_s_fcat,
      gw_zlest0056     type zlest0056,
      gw_aviso         type ty_aviso,
      gw_aviso_aquav   type ty_aviso_aquav,
      gw_tp_mov        type ty_tp_mov,
      gw_dados_compl   type ty_dados_compl,
      gw_vttp          type vttp.

data: vl_estorno     type c length 2,
      vr_aviso_aquav.


data: zcl_aviso_recebimento type ref to zcl_aviso_recebimento.
data: i_item       type zde_bapi_remessa_item.
*----------------------------------------------------------------------*
* Objetos
*----------------------------------------------------------------------*
data: obj_zcl_util type ref to zcl_util.

"Seleção.
selection-screen: begin of block  b1 with frame title text-001.
  select-options: p_ano    for zlest0061-ano_viagem no intervals no-extension,
                  p_werks  for zlest0061-werks no intervals no-extension,
                  p_viagem for zlest0061-nr_viagem  no intervals,
                  p_fatura for zlest0061-dt_fatura.

  parameters: r1_cte  radiobutton group rad1  default 'X',
              r2_nfps radiobutton group rad1.

  parameters: pdocnum type j_1bdocnum no-display,
              pestorn type char01 no-display.

selection-screen: end of block b1.
*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
initialization.

start-of-selection.
  perform: selecionar_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
form selecionar_dados .
  if ( r1_cte eq 'X' ).
    perform: dados_cte.
  elseif ( r2_nfps eq 'X' ).
    perform: dados_nfps.
  endif.
endform.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DADOS_CTE
*&---------------------------------------------------------------------*
form dados_cte .

  data: tl_estilo_cell type lvc_t_scol with header line,
        wl_estilo_cell type lvc_s_scol.

  data: rg_docnum type range of j_1bdocnum.

  data: tp_mov type range of zsdt0011-tp_movimento,
        wp_mov like line of tp_mov.

  if pdocnum is not initial.
    rg_docnum = value #( sign = 'I' option = 'EQ' ( low = pdocnum high = pdocnum ) ).
  endif.

  "Frete Aquaviário - Ordem de Venda
  select * from zlest0061
    into table gt_zlest0061
 where ano_viagem in p_ano
   and nr_viagem  in p_viagem
   and dt_fatura  in p_fatura
   and werks      in p_werks
   and docnum     in rg_docnum.

  check not gt_zlest0061[] is initial.

  select * from zlest0060
    into table @data(it_zlest0060)
    for all entries in @gt_zlest0061
  where docnum eq @gt_zlest0061-docnum.

  if sy-subrc = 0.

    select * from lfa1
      into table @data(it_lfa1)
     for all entries in @it_zlest0060
      where lifnr eq @it_zlest0060-rm_codigo
      and   land1 eq 'BR'.

    loop at it_zlest0060 into data(wa_zlest0060).

      select single * from j_1bnfdoc into @data(wa_doc)
        where ( ( nfnum  eq @wa_zlest0060-nfnum+3(6) ) or ( nfenum eq @wa_zlest0060-nfnum ) )
          and     docdat eq @wa_zlest0060-docdat
          and     series eq @wa_zlest0060-series+0(1)
          and     branch eq @wa_zlest0060-rm_codigo+6(4).
*         AND     PARID  EQ @WA_ZLEST0060-RM_CODIGO.

      if wa_doc-direct eq '1'.

        gw_tp_mov-tp_movimento  = 'E'.
        gw_tp_mov-docnum        = wa_zlest0060-docnum.
        append  gw_tp_mov to gt_tp_mov.
        clear gw_tp_mov.

      else.
        gw_tp_mov-tp_movimento  = 'S'.
        gw_tp_mov-docnum        = wa_zlest0060-docnum.
        append  gw_tp_mov to gt_tp_mov.
        clear gw_tp_mov.
      endif.

      clear : wa_zlest0060, wa_doc.

    endloop.
  endif.


  select * from zlest0056
    into table gt_zlest0056
    for all entries in gt_zlest0061
   where ano_viagem eq gt_zlest0061-ano_viagem
     and nr_viagem  eq gt_zlest0061-nr_viagem
     and werks      eq gt_zlest0061-werks
     and bukrs      eq gt_zlest0061-bukrs.

  "Depara Tipo Odem/Tipo Transporte
  select * from zsdt0011
    into table gt_zsdt0011
    for all entries in gt_zlest0061
  where auart        eq gt_zlest0061-auart.


  "Mestre de clientes (parte geral)
  select * from kna1
    into table gt_kna1
    for all entries in gt_zlest0061
 where kunnr eq gt_zlest0061-cl_codigo
   and ktokd = 'ZCIC'.

  select * from j_1bnfdoc
   into table gt_j_1bnfdoc
   for all entries in gt_zlest0061
  where docnum eq gt_zlest0061-docnum.

  select * from zlest0032
    into table gt_zlest0032
    for all entries in gt_zlest0061
  where tknum eq gt_zlest0061-tknum.

  select * from tvtk
    into table gt_tvtk
    for all entries in gt_zsdt0011
 where shtyp eq gt_zsdt0011-shtyp.

  select * from vttp
    into table gt_vttp
   for all entries in gt_zlest0061
   where tknum  eq gt_zlest0061-tknum.

  loop at gt_zlest0061 into gw_zlest0061.

    clear: gw_j_1bnfdoc, gw_kna1.

    read table gt_j_1bnfdoc into gw_j_1bnfdoc with key docnum = gw_zlest0061-docnum.
    if ( sy-subrc eq 0 ).

      read table gt_kna1 into gw_kna1 with key kunnr = gw_zlest0061-cl_codigo.

      if ( sy-subrc eq 0 ).

        refresh: tl_estilo_cell.
        clear: wl_estilo_cell.


        gw_saida-docnum       = gw_j_1bnfdoc-docnum.
        gw_saida-bukrs        = gw_zlest0061-bukrs.
        gw_saida-werks        = gw_zlest0061-werks.
        gw_saida-ano_viagem   = gw_zlest0061-ano_viagem.
        gw_saida-nr_viagem    = gw_zlest0061-nr_viagem.
        gw_saida-nome_emb     = gw_zlest0061-nome_emb.
        gw_saida-nr_dco       = gw_zlest0061-nr_dco.
        gw_saida-safra        = gw_zlest0061-safra.
        gw_saida-data_taxa    = gw_zlest0061-data_taxa.
        gw_saida-waerk        = gw_zlest0061-waerk.
        gw_saida-waerk_fatura = gw_zlest0061-waerk_fatura.
        gw_saida-centro_fat	  = gw_zlest0061-centro_fat.
        gw_saida-operacao     = gw_zlest0061-operacao.

        if gw_saida-waerk_fatura is initial.
          gw_saida-waerk_fatura = gw_saida-waerk.
        endif.

        if not ( gw_zlest0061-tknum is initial ).
          gw_saida-status = icon_led_green.

          wl_estilo_cell-fname = 'TKNUM'.
          wl_estilo_cell-color-col   = '5'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to tl_estilo_cell.
          insert lines of tl_estilo_cell into table gw_saida-estilo_cell.

          wl_estilo_cell-fname = 'FKNUM'.
          wl_estilo_cell-color-col   = '5'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to tl_estilo_cell.
          insert lines of tl_estilo_cell into table gw_saida-estilo_cell.
        else.
          wl_estilo_cell-fname = 'TKNUM'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to tl_estilo_cell.
          insert lines of tl_estilo_cell into table gw_saida-estilo_cell.

          wl_estilo_cell-fname = 'FKNUM'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to tl_estilo_cell.
          insert lines of tl_estilo_cell into table gw_saida-estilo_cell.

          gw_saida-status = icon_led_red.
        endif.


        gw_saida-auart          = gw_zlest0061-auart.


        read table gt_tp_mov into gw_tp_mov with key docnum = gw_zlest0061-docnum.

        read table gt_zsdt0011 into gw_zsdt0011 with key auart        = gw_zlest0061-auart
                                                         tp_movimento = gw_tp_mov-tp_movimento.
        gw_saida-shtyp          = gw_zsdt0011-shtyp.


        gw_saida-dt_fatura      = gw_zlest0061-dt_fatura.
        gw_saida-peso_vinculado = gw_zlest0061-peso_vinculado.
        gw_saida-vlr_usd        = gw_zlest0061-vlr_usd.
        gw_saida-vlr_brl        = gw_zlest0061-vlr_brl.

        if not ( gw_j_1bnfdoc-nfenum is initial ).
          concatenate gw_j_1bnfdoc-nfenum '-' gw_j_1bnfdoc-series into gw_saida-cte_serie.
        endif.

        if not ( gw_zlest0061-tknum is initial ).
          read table gt_zlest0032 into gw_zlest0032 with key tknum = gw_zlest0061-tknum.
          gw_saida-belnr       = gw_zlest0032-belnr.
          gw_saida-docnum_entr = gw_zlest0032-docnum.
        endif.

        gw_saida-cl_codigo = gw_zlest0061-cl_codigo.
        gw_saida-rm_codigo = gw_zlest0061-rm_codigo.
        gw_saida-tax_dolar = gw_zlest0061-tax_dolar.
        gw_saida-tknum = gw_zlest0061-tknum.
        gw_saida-fknum = gw_zlest0061-fknum.

        read table gt_tvtk into gw_tvtk with key shtyp = gw_zsdt0011-shtyp.
        gw_saida-frkrl_ump = gw_tvtk-frkrl_ump.

        read table gt_vttp into gw_vttp with key tknum = gw_zlest0061-tknum.
        if sy-subrc eq 0.
          gw_saida-vbeln = gw_vttp-vbeln.
        endif.

        append gw_saida to gt_saida.

      endif.
    endif.

    clear: gw_saida, gw_zlest0061, gw_tvtk, gw_zlest0032, gw_kna1, gw_j_1bnfdoc, gw_tp_mov.
  endloop.

  check not gt_saida[] is initial.

  if pdocnum is not initial.
    case pestorn.
      when abap_false.
        perform: gerar_vt_vi using 1.
      when abap_true.
        perform: estornar using 1.
    endcase.
  else.
    perform: criar_alv_saida.
    call screen 0100.
  endif.


endform.                    " DADOS_CTE
*&---------------------------------------------------------------------*
*&      Form  DADOS_NFPS
*&---------------------------------------------------------------------*
form dados_nfps .

endform.                    " DADOS_NFPS
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
module pbo_0100 output.
  set pf-status 'PF0100'. "Status da Tela 0100.
  set titlebar  'TB0100'. "Title da Tela 0100
endmodule.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
module pai_0100 input.

  data: vl_tknum type zlest0061-tknum,
        vl_fknum type zlest0061-fknum,
        wl_vfkp  type vfkp.


  case sy-ucomm.
    when: 'BACK'. "Botão para Voltar a Tela anterior.
      leave to screen 0.
    when: 'CANC'. "Botão para Voltar a Tela anterior.
      leave to screen 0.
    when: 'EXIT'. "Botão para sair do programa corrente.
      leave program.
    when: 'GERAR'.
      perform: gerar_vt_vi using 0.
    when: 'ESTORNAR'.
      perform: estornar using 0.
  endcase.
endmodule.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_SAIDA
*&---------------------------------------------------------------------*
form criar_alv_saida .

  data: wl_layout  type lvc_s_layo,
        wl_variant type disvariant.

  if ( obj_custom is initial ).

    perform: criar_catalog.

    create object obj_custom
      exporting
        container_name              = 'CONTAINER_PRINCIPAL'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


    create object obj_grid
      exporting
        i_parent          = obj_custom
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    wl_layout-sel_mode   = 'A'.
    wl_layout-ctab_fname = 'ESTILO_CELL'.
    wl_variant-report   = sy-repid.
    wl_variant-username = sy-uname.

    call method obj_grid->set_table_for_first_display
      exporting
        is_layout                     = wl_layout
        i_save                        = 'A'
        is_variant                    = wl_variant
      changing
        it_outtab                     = gt_saida[]
        it_fieldcatalog               = gt_fcat[]
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

  endif.


endform.                    " CRIAR_ALV_SAIDA
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
form criar_catalog .

  refresh: gt_fcat[].

  perform montar_catalog using:
     'STATUS'         'Status'           '10'  '' '' '' 'C' '' '' '' ''  '' '',
     'AUART'          'Tipo. O.V.'       '12'  '' '' '' '' '' '' '' ''  '' '',
     'WERKS'          'Centro'           '05'  '' '' '' '' '' '' '' ''  '' '',
     'NR_VIAGEM'      'Número Viagem'    '05'  '' '' '' '' '' '' '' ''  '' '',
     'DT_FATURA'      'Dt.Fatura'        '10'  '' '' '' '' '' '' '' ''  '' '',
     'PESO_VINCULADO' 'Peso Vinculado'   '15'   '' '' '' '' 'X' '' '' ''  '' '',
     'VLR_USD'        'Vlr. USD'         '15'  '' '' '' '' 'X' '' '' ''  '' '',
     'TAX_DOLAR'      'Tax. Dolar'       '10'  '' '' '' '' 'X' '' '' ''  '' '',
     'VLR_BRL'        'Vlr. BRL'         '15'  '' '' '' '' 'X' '' '' ''  '' '',
     'CTE_SERIE'      'Ct-e'             '9'   'X' '' '' '' '' '' '' ''  '' '',
     'CL_CODIGO'      'Centro Tomador' '10'  'X' '' '' '' '' '' '' ''  '' '',
     'TKNUM'          'VT'               '10'  'X' '' '' '' '' '' '' ''  '' '',
     'FKNUM'          'VI'               '10'  'X' '' '' '' '' '' '' ''  '' '',
     'BELNR'          'MIRO'             '10'  'X' '' '' '' '' '' '' ''  '' '',
     'OPERACAO'       'Operação'         '03'  ' ' '' '' '' '' '' '' ''  '' '',
     'DOCNUM_ENTR'    'Doc. Entrada'     '10'  'X' '' '' '' '' '' '' ''  '' ''.

endform.                    " CRIAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ROMANEIO
*&---------------------------------------------------------------------*
form montar_catalog using  value(p_fieldname)
                           value(p_desc)
                           value(p_tam)
                           value(p_no_zero)
                           value(p_hotspot)
                           value(p_cor)
                           value(p_just)
                           value(p_sum)
                           value(p_edit)
                           value(p_ref_tabname)   like dd02d-tabname
                           value(p_ref_fieldname) like dd03d-fieldname
                           value(p_tabname)       like dd02d-tabname
                           value(p_check).

  clear: gw_fcat.

  gw_fcat-fieldname = p_fieldname.
  gw_fcat-ref_table = p_ref_tabname..
  gw_fcat-ref_field = p_ref_fieldname.
  gw_fcat-tabname   = p_tabname.
  gw_fcat-scrtext_l = p_desc.
  gw_fcat-scrtext_m = p_desc.
  gw_fcat-scrtext_s = p_desc.
  gw_fcat-outputlen = p_tam.
  gw_fcat-no_zero   = p_no_zero.
  gw_fcat-hotspot   = p_hotspot.
  gw_fcat-emphasize = p_cor.
  gw_fcat-just      = p_just.
  gw_fcat-do_sum    = p_sum.
  gw_fcat-edit      = p_edit.
  gw_fcat-checkbox  = p_check.

  append gw_fcat  to gt_fcat.

endform.                    " MONTAR_CATALOG_ROMANEIO
*&---------------------------------------------------------------------*
*&      Form  GERAR_VT_VI
*&---------------------------------------------------------------------*
form gerar_vt_vi using p_index type i.

  data: tl_rows                 type lvc_t_row,
        sl_rows                 type lvc_s_row,
        lt_headerdata           type bapishipmentheader,
        lt_itemdata             type table of bapishipmentitem,
        lt_stagedata            type table of bapishipmentstage,
        wl_stagedata            type bapishipmentstage,
        wl_itemdata             type bapishipmentitem,
        lt_return               like bapiret2 occurs 0 with header line,
        lt_return_aux           type table of bapiret2,
        wl_return               type bapiret2,

        lt_headerdata_change    type bapishipmentheader,
        lt_headerdataaction     type bapishipmentheaderaction,
        wl_vfkp                 type vfkp,
        lt_konv                 type table of konv,
        wl_konv                 type konv,
        wl_zsdt0001             type zsdt0001,

        wl_j_1bnfe_active       type j_1bnfe_active,
        wl_j_1bnflin            type j_1bnflin,

        wl_vbfa                 type vbfa,
        wl_zfiwrt0008           type zfiwrt0008,
        wl_ekes                 type ekes,
        lw_lfa1                 type lfa1,
        lw_kna1                 type kna1,
        wl_zlest0104            type zlest0104,
        wa_zib_cte_dist_n55     type zib_cte_dist_n55,

        wl_maxdif               type setleaf-valfrom,

        lt_estilo_cell          type lvc_t_scol with header line,
        wl_estilo_cell          type lvc_s_scol,

        lwa_j_1bnfdoc_pr        type j_1bnfdoc,
        lwa_zmmt_ee_zgr_pr      type zmmt_ee_zgr,
        lwa_zmmt_ee_zgr_docs_pr type zmmt_ee_zgr_docs,
        lwa_zsdt0001_pr         type zsdt0001,
        lva_ent_pr_found        type c,


        vl_tknum                type vttk-tknum,
        vl_fknum                type vfkp-fknum,

        vl_data_fix             type c length 10,
        vl_data_liq             type c length 10,
        vl_mode                 type c length 1,
        wl_stable               type lvc_s_stbl,
        vl_error                type c length 2,
        vl_chave_acesso         type zib_cte_dist_n55-n55_chave_acesso,
        wl_item_data            type bapishipmentitem,
        lva_show_message_erro   type c,
        lva_chave_elet          type c.


  data: lt_vbpa type table of vbpa,
        wl_vbpa type vbpa,
        lt_tvkn type table of tvkn,
        wl_tvkn type tvkn.

  data: vl_total       type konv-kbetr,
        vl_vlr_dif     type netwr_p,
        vl_maxdiv      type netwr_p,
        vl_obj_key     type zfiwrt0008-obj_key,
        vl_nota        type string,
        vl_serie       type string,
        vl_nota_ekes   type string,
        lva_ref_doc_no type zmmt_ee_zgr-ref_doc_no,
        lva_nf_number9 type j_1bnfdoc-nfenum,
        lva_nf_number  type j_1bnfdoc-nfnum,
        lva_series     type j_1bnfdoc-series.


  data: var_docnum_correcao type j_1bnfdoc-docnum.
  data: lw_carta_correcao type zcarta_correcao,
        it_carta_correcao type table of zcarta_correcao. "<<<<< Ajuste BUG SOLTO 159013 / AOENNING >>>>>>>>

  data: vl_org_point type bapishipmentstage-org_point,
        vl_org_cust  type bapishipmentstage-org_cust.

  data: vl_msg        type bapi_msg,
        vl_vlr_fatura type c  length 15,
        vl_vlr_custo  type c length 15.

  if p_index is initial.
    call method obj_grid->get_selected_rows
      importing
        et_index_rows = tl_rows.
  else.
    sl_rows-index = p_index.
    append sl_rows to tl_rows.
  endif.

  refresh: gt_saida_message[].

  clear: vl_org_point,vl_org_cust,  lva_show_message_erro, lva_chave_elet.

  clear: gt_dados_aviso[].

  if not ( tl_rows[] is initial ).

    free obj_zcl_util.
    clear: gw_saida.


    loop at tl_rows into sl_rows.

      clear: gw_saida.

      read table gt_saida into gw_saida index sl_rows-index.

      if ( sy-subrc eq 0 ).

        select single * into @data(wa_cte_ter)
          from zib_cte_dist_ter
         where docnum_cte_sub eq @gw_saida-docnum.

        if sy-subrc is initial.

          select single * into @data(wa_cte_n55)
            from zib_cte_dist_n55
           where cd_chave_cte eq @wa_cte_ter-cd_chave_cte
             and tknum ne @space.

          if sy-subrc is initial.
            gw_saida_message-message = |Existe VT gerada para a CT-e: { gw_saida-cte_serie } VT: { wa_cte_n55-tknum }, verificar na ZMM0079|.
            gw_saida_message-type = 'W'.
            append gw_saida_message to gt_saida_message.
            clear: gw_saida_message.

            vl_tknum = wa_cte_n55-tknum.
            vl_fknum = wa_cte_n55-fknum.

            update zlest0061 set tknum = vl_tknum
                                 fknum = vl_fknum
             where docnum     eq gw_saida-docnum
               and nr_viagem  eq gw_saida-nr_viagem
               and cl_codigo  eq gw_saida-cl_codigo
               and ano_viagem eq gw_saida-ano_viagem.

            commit work.

            gw_saida-tknum  = vl_tknum.
            gw_saida-fknum  = vl_fknum.
            gw_saida-status = icon_led_green.

            clear: gw_saida-estilo_cell.

            wl_estilo_cell-fname = 'TKNUM'.
            wl_estilo_cell-color-col   = '5'.
            wl_estilo_cell-color-int   = '0'.
            wl_estilo_cell-color-inv   = '0'.
            append wl_estilo_cell to lt_estilo_cell.
            insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

            wl_estilo_cell-fname = 'FKNUM'.
            wl_estilo_cell-color-col   = '5'.
            wl_estilo_cell-color-int   = '0'.
            wl_estilo_cell-color-inv   = '0'.
            append wl_estilo_cell to lt_estilo_cell.
            insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

            modify gt_saida from gw_saida index sl_rows-index transporting tknum fknum status estilo_cell.

            continue.
          endif.
        endif.

        if not ( gw_saida-tknum is initial ) or not ( gw_saida-fknum is initial ).

          concatenate 'Existe VT/VI gerado para a Ct-e: ' gw_saida-cte_serie ' VT: ' gw_saida-tknum ' VI: ' gw_saida-fknum into gw_saida_message-message separated by space.
          gw_saida_message-type = 'W'.
          append gw_saida_message to gt_saida_message.
          clear: gw_saida_message.
          continue.
        endif.

        refresh: gt_zlest0060.
        clear: lt_headerdata.

        lt_headerdata-shipment_type  = gw_saida-shtyp.

        if gw_saida-centro_fat is initial.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = gw_saida-werks
            importing
              output = lt_headerdata-service_agent_id.
        else.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = gw_saida-centro_fat
            importing
              output = lt_headerdata-service_agent_id.
        endif.

        lt_headerdata-trans_plan_pt     = gw_saida-cl_codigo+6(4).

        lt_headerdata-service_level        = '1'.
        lt_headerdata-shipping_type        = '03'.
        lt_headerdata-external_id_1        = gw_saida-cte_serie.
        lt_headerdata-status_plan          = 'X'.
        lt_headerdata-status_checkin       = 'X'.
        lt_headerdata-status_load_start    = 'X'.
        lt_headerdata-time_travel          = '30'.
        lt_headerdata-time_total           = '1'.
        lt_headerdata-time_unit            = 'H'.
        lt_headerdata-special_procedure_id = '0001'.
        lt_headerdata-shpmnt_cost_rel      = 'X'.

        select * from zlest0060
            into table gt_zlest0060
          where bukrs       eq gw_saida-bukrs
            and werks       eq gw_saida-werks
            and ano_viagem  eq gw_saida-ano_viagem
            and nr_viagem   eq gw_saida-nr_viagem
            and nome_emb    eq gw_saida-nome_emb
            and cl_codigo   eq gw_saida-cl_codigo
            and rm_codigo   eq gw_saida-rm_codigo
            and nr_dco      eq gw_saida-nr_dco
            and safra       eq gw_saida-safra
            and operacao    eq gw_saida-operacao.

        check not gt_zlest0060[] is initial.


        select * from lfa1 into table gt_lfa1
          for all entries in gt_zlest0060
         where lifnr  eq gt_zlest0060-rm_codigo
          and  land1  eq 'BR' .


        refresh: lt_itemdata[].

*----------------------------------------------------------------------------------------------------------------*
*       Pre validações - Ini
*----------------------------------------------------------------------------------------------------------------*
        data(lva_found_error_nf) = abap_false.

        loop at gt_zlest0060 into gw_zlest0060.

          read table gt_lfa1 into data(lwa_lfa1) with key lifnr = gw_zlest0060-rm_codigo.
          if lwa_lfa1-ktokk = 'ZFIC'.

          else.

            perform f_get_doc_fiscal_entrada using gw_zlest0060
                                          changing lwa_j_1bnfdoc_pr
                                                   lwa_zmmt_ee_zgr_pr
                                                   lwa_zmmt_ee_zgr_docs_pr
                                                   lwa_zsdt0001_pr
                                                   lva_ent_pr_found.
            if lva_ent_pr_found eq abap_false.

              lva_found_error_nf = abap_true.
              clear gw_saida_message.
              concatenate 'Não encontrada NF de entrada ' gw_zlest0060-nfnum  'Fonecedor: ' gw_zlest0060-rm_codigo  into gw_saida_message-message separated by space.
              gw_saida_message-type = 'W'.
              append gw_saida_message to gt_saida_message.
              clear: gw_saida_message.
              continue.

            else.

              read table gt_zsdt0011 into gw_zsdt0011 with key auart        = gw_saida-auart
                                                               tp_movimento = 'E'.

              if not ( sy-subrc eq 0 and gw_zsdt0011-shtyp is not initial ).
                lva_found_error_nf = abap_true.
                concatenate 'Tipo de OV: ' gw_saida-auart ' e Tipo de movimento E, não parametrizado na transação ZSDT0022!' into gw_saida_message-message separated by space.
                gw_saida_message-type = 'W'.
                append gw_saida_message to gt_saida_message.
                clear: gw_saida_message.
                continue.
              endif.

            endif.

          endif. "IF GW_LFA1-KTOKK = 'ZFIC'.

        endloop.

        if lva_found_error_nf eq abap_true.
          continue.
        endif.
*----------------------------------------------------------------------------------------------------------------*
*       Pre validações - Fim
*----------------------------------------------------------------------------------------------------------------*

        clear: lva_found_error_nf.

        clear: var_docnum_correcao.
        loop at gt_zlest0060 into gw_zlest0060.

          clear: wl_zlest0104.

          read table gt_lfa1 into data(gw_lfa1) with key lifnr = gw_zlest0060-rm_codigo.
          if gw_lfa1-ktokk = 'ZFIC' .

            select single * from zlest0104 into wl_zlest0104 where emissor = gw_zlest0060-werks.

            select single * from zsdt0001 into wl_zsdt0001 where bukrs       eq wl_zlest0104-bukrs
                                                             and branch      eq wl_zlest0104-branch
                                                             and nr_safra    eq gw_zlest0060-safra
                                                             and parid       eq gw_zlest0060-rm_codigo
                                                             and nr_romaneio eq gw_zlest0060-nr_romaneio.

            if ( sy-subrc eq 0 ) and not ( wl_zsdt0001-nr_romaneio is initial ) and ( wl_zsdt0001-doc_rem is not initial ) .

              wl_itemdata-delivery = wl_zsdt0001-doc_rem.
              append wl_itemdata to lt_itemdata.

              if vl_org_point is initial.

                if wl_zsdt0001-local_descarga = '76'.
                  vl_org_cust = '0000001003'.
                elseif wl_zsdt0001-local_descarga = '257'.
                  vl_org_cust = '0000000161'.
                elseif wl_zsdt0001-local_descarga = '278'.
                  vl_org_cust = '0000003904'.
                endif.

                select single *
                  from tvkn into wl_tvkn
                 where kunnr eq vl_org_cust.

                if sy-subrc = 0.
                  vl_org_point = wl_tvkn-knote.
                endif.

              endif.

            elseif ( sy-subrc ne 0 ) and ( wl_zsdt0001-nr_romaneio is initial ).

              select single * from j_1bnfe_active
                into wl_j_1bnfe_active
                where regio   eq gw_zlest0060-chave_nfe(2)
                  and nfyear  eq gw_zlest0060-chave_nfe+2(2)
                  and nfmonth eq gw_zlest0060-chave_nfe+4(2)
                  and stcd1   eq gw_zlest0060-chave_nfe+6(14)
                  and model   eq gw_zlest0060-chave_nfe+20(2)
                  and serie   eq gw_zlest0060-chave_nfe+22(3)
                  and nfnum9  eq gw_zlest0060-chave_nfe+25(9).

              if ( sy-subrc eq 0 ).

                clear: wl_itemdata-delivery.

                select single * from j_1bnflin
                  into wl_j_1bnflin
                where docnum eq wl_j_1bnfe_active-docnum.


                case wl_j_1bnflin-reftyp.
                  when: 'BI'.

                    select single * from vbfa
                      into wl_vbfa
                     where vbeln   eq wl_j_1bnflin-refkey
                      and  vbtyp_v eq 'J'
                      and  vbtyp_n eq 'M'.

                    wl_itemdata-delivery = wl_vbfa-vbelv.
                    append wl_itemdata to lt_itemdata.

                  when: 'ZW'.

                    clear: vl_obj_key.
                    concatenate 'ZG0' wl_j_1bnflin-refkey wl_j_1bnfe_active-credat(4) into vl_obj_key.

                    select single * from zfiwrt0008
                      into wl_zfiwrt0008
                    where obj_key eq vl_obj_key.

                    clear: vl_nota_ekes, vl_nota, vl_serie.

                    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      exporting
                        input  = gw_zlest0060-nfnum
                      importing
                        output = vl_nota.

                    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      exporting
                        input  = gw_zlest0060-series
                      importing
                        output = vl_serie.

                    concatenate vl_nota '-' vl_serie into vl_nota_ekes.
                    condense vl_nota_ekes no-gaps.

                    select single * from ekes
                      into wl_ekes
                    where ebeln eq wl_zfiwrt0008-ebeln
                      and xblnr eq vl_nota_ekes.

                    if sy-subrc is not initial.
                      append value #(
                                      type = 'E'
                                      message = |Para o pedido { wl_zfiwrt0008-ebeln } não foi encontrado o aviso de recebimento para a nota { vl_nota_ekes } |
                                    ) to lt_return_aux.
                    endif.

                    wl_itemdata-delivery = wl_ekes-vbeln.
                    append wl_itemdata to lt_itemdata.

                    select single route
                      from likp
                      into @data(vl_route)
                      where vbeln eq @wl_ekes-vbeln.

                    if vl_route is initial.
                      append value #(
                                      type = 'E'
                                      message = |Não Encontrado Itinerario na Remessa/Aviso { wl_ekes-vbeln }|
                                    ) to lt_return_aux.
                    endif.

                endcase.

                if ( gw_zlest0060-id_frete_aqua_nf is not initial ) and ( wl_itemdata-delivery is not initial ).
                  update zlest0060 set vbeln_aviso_ref = wl_itemdata-delivery
                   where id_frete_aqua_nf eq gw_zlest0060-id_frete_aqua_nf.
                endif.

              endif. "IF ( SY-SUBRC EQ 0 ).

            elseif ( sy-subrc eq 0 ) and ( wl_zsdt0001-nr_romaneio is not initial ) and ( wl_zsdt0001-doc_rem is initial ).

*            PERFORM F_GET_REMESSA_NF TABLES LT_ITEMDATA
*                                      USING GW_ZLEST0060.

              select single * from zib_cte_dist_ter into @data(wa_ter)
                where docnum_cte_sub eq @gw_zlest0060-docnum.

              select single * from zib_cte_dist_n01 into @data(wa_n01)
                where cd_chave_cte eq @wa_ter-cd_chave_cte.


              select single * from  zsdt0001 into @data(wa_0001)
                where tp_movimento  eq 'E'
                and   branch        eq @gw_zlest0060-dt_codigo+6(4)   " (4 ÚLTIMOS CARACTERES))
                and   nr_safra      eq @gw_zlest0060-safra
                and   docdat        eq @gw_zlest0060-docdat
                and   nfnum         eq @gw_zlest0060-nfnum
                and  ( ( series     eq @gw_zlest0060-series+2(1) ) or  ( series  eq @gw_zlest0060-series ) )
                and   matnr         eq @gw_zlest0060-matnr.

              if sy-subrc = 0.

                select single * from zmmt_ee_zgr into @data(wa_zmmt_ee_zgr)
                  where ch_referencia eq @wa_0001-ch_referencia.


                select single * from zmmt_ee_zgr_docs into @data(wa_zmmt_ee_zgr_docs)
                  where obj_key eq @wa_zmmt_ee_zgr-obj_key.


                select single * from j_1bnfe_active into @data(wa_active)
                 where docnum eq @wa_zmmt_ee_zgr_docs-docnum.


                select single * from j_1bnfdoc into @data(wa_doc)
                  where docnum eq @wa_zmmt_ee_zgr_docs-docnum.

                concatenate   wa_active-regio    wa_active-nfyear
                              wa_active-nfmonth  wa_active-stcd1
                              wa_active-model    wa_active-serie
                              wa_active-nfnum9   wa_active-docnum9  wa_active-cdv   into vl_chave_acesso.


                if wa_n01-docnum_nf = '0000000000'.

                  wa_zib_cte_dist_n55-mandt             = sy-mandt.
                  wa_zib_cte_dist_n55-cd_chave_cte      = wa_ter-cd_chave_cte.
                  wa_zib_cte_dist_n55-n55_chave_acesso  = vl_chave_acesso.
                  wa_zib_cte_dist_n55-docnum_nfe        = wa_active-docnum .
                  wa_zib_cte_dist_n55-bukrs             = wa_doc-bukrs.
                  wa_zib_cte_dist_n55-branch            = wa_doc-branch.
                  wa_zib_cte_dist_n55-parvw             = wa_doc-parvw.
                  wa_zib_cte_dist_n55-parid             = wa_doc-parid.
                  wa_zib_cte_dist_n55-zvlr_mercadoria   = wa_doc-nftot.
                  wa_zib_cte_dist_n55-vbeln_re          = wa_zmmt_ee_zgr_docs-ft_belnr.
                  wa_zib_cte_dist_n55-gjahr             = wa_zmmt_ee_zgr_docs-ft_gjahr.
                  wa_zib_cte_dist_n55-mblnr             = wa_zmmt_ee_zgr_docs-mm_mblnr.
                  wa_zib_cte_dist_n55-mjahr             = wa_zmmt_ee_zgr_docs-mm_mjahr.
                  wa_zib_cte_dist_n55-zvlr_frete        = wa_ter-valor_receber.

                  modify zib_cte_dist_n55 from wa_zib_cte_dist_n55.
                  commit work.
                endif.
              endif.
            endif.

            select single * from zlest0056 into @data(wa_zlest0056)
              where bukrs       eq @gw_zlest0060-bukrs
              and   werks       eq @gw_zlest0060-werks
              and   nr_viagem   eq @gw_zlest0060-nr_viagem
              and   ano_viagem  eq @gw_zlest0060-ano_viagem.

            wl_stagedata-org_suppl = wa_zlest0056-po_embarque.
            wl_stagedata-dest_cust = wa_zlest0056-po_destino.
            append wl_stagedata to lt_stagedata.


            gw_aviso-po_number      = wa_zmmt_ee_zgr-po_number.
            gw_aviso-p_nfnum        = gw_zlest0060-nfnum.
            gw_aviso-p_series       = gw_zlest0060-series.
            gw_aviso-po_embarque    = wa_zlest0056-po_embarque.
            gw_aviso-po_destino     = wa_zlest0056-po_destino.
            gw_aviso-nftot          = wa_doc-nftot.
            gw_aviso-ch_referencia  = wa_0001-ch_referencia.
            gw_aviso-quantity       = wa_zmmt_ee_zgr-quantity.

            perform gera_aviso_rec using gw_aviso
                                         changing  wl_itemdata-delivery.
            append wl_itemdata to lt_itemdata.

            var_docnum_correcao = wl_j_1bnfe_active-docnum.
            clear: wl_itemdata, wl_zsdt0001, wl_j_1bnfe_active, wl_j_1bnflin, wl_vbfa, wl_zfiwrt0008, wl_ekes, vl_chave_acesso.

          else.

            clear: wl_stagedata, vr_aviso_aquav, wa_cte_n55, lva_ref_doc_no,  lva_nf_number9.

            vr_aviso_aquav = abap_true.

*            SELECT SINGLE * INTO @DATA(WA_CTE_N01)
*               FROM  ZIB_CTE_DIST_N01
*              WHERE CD_CHAVE_CTE EQ @WA_CTE_TER-CD_CHAVE_CTE
*              AND   TKNUM        EQ @SPACE.
*
*
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                INPUT  = WA_CTE_N01-N01_NUMR_SERIE
*              IMPORTING
*                OUTPUT = WA_CTE_N01-N01_NUMR_SERIE.
*
*
*            SELECT SINGLE * INTO WA_CTE_N55
*              FROM ZIB_CTE_DIST_N55
*             WHERE CD_CHAVE_CTE EQ WA_CTE_TER-CD_CHAVE_CTE
*               AND TKNUM        EQ SPACE.


            read table gt_zlest0061 into gw_zlest0061 with key docnum =  gw_zlest0060-docnum.

            select single * from zlest0056 into @data(gw_zlest0056)
              where bukrs       eq @gw_zlest0061-bukrs
              and   werks       eq @gw_zlest0061-werks
              and   nr_viagem   eq @gw_zlest0061-nr_viagem
              and   ano_viagem  eq @gw_zlest0061-ano_viagem.

            if sy-subrc eq 0.
              wl_stagedata-org_suppl = gw_zlest0056-po_embarque.
              wl_stagedata-dest_cust = gw_zlest0056-po_destino.
              wl_stagedata-stage_cat    = '1'.
              wl_stagedata-stage_seq    = '0001'.
              append wl_stagedata to lt_stagedata.
            endif.

            perform f_get_doc_fiscal_entrada using gw_zlest0060
                                          changing lwa_j_1bnfdoc_pr
                                                   lwa_zmmt_ee_zgr_pr
                                                   lwa_zmmt_ee_zgr_docs_pr
                                                   lwa_zsdt0001_pr
                                                   lva_ent_pr_found.

            if lva_ent_pr_found eq abap_false.

              lva_found_error_nf = abap_true.

              clear gw_saida_message.
              concatenate 'Não encontrada NF de entrada ' gw_zlest0060-nfnum  'Fonecedor: ' gw_zlest0060-rm_codigo  into gw_saida_message-message separated by space.
              gw_saida_message-type = 'W'.
              append gw_saida_message to gt_saida_message.
              clear: gw_saida_message.
              continue.

            else.

              clear: lva_nf_number, lva_nf_number9, lva_series.

              if gw_zlest0060-chave_nfe+0(1) eq 'F'. "Formulario
                lva_nf_number  = gw_zlest0060-nfnum.
              else.
                lva_nf_number9 = gw_zlest0060-nfnum.
              endif.

              lva_series = gw_zlest0060-series.

              call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                exporting
                  input  = lva_nf_number9
                importing
                  output = lva_nf_number9.

              call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                exporting
                  input  = lva_series
                importing
                  output = lva_series.

              call function 'J_1B_NF_NUMBER_CONDENSE'
                exporting
                  nf_number  = lva_nf_number
                  series     = lva_series
                  nf_number9 = lva_nf_number9
                importing
                  ref_number = lva_ref_doc_no.


              gw_aviso_aquav-po_number      = lwa_zmmt_ee_zgr_pr-po_number.
              gw_aviso_aquav-po_item        = lwa_zmmt_ee_zgr_pr-po_item.
              gw_aviso_aquav-po_embarque    = gw_zlest0056-po_embarque.
              gw_aviso_aquav-po_destino     = gw_zlest0056-po_destino.
              gw_aviso_aquav-entry_qnt      = gw_zlest0060-peso_liq_ret.
              "GW_AVISO_AQUAV-REF_DOC_NO     = LWA_ZMMT_EE_ZGR_PR-REF_DOC_NO.
              gw_aviso_aquav-ref_doc_no     = lva_ref_doc_no.
              gw_aviso_aquav-ch_referencia  = lwa_zmmt_ee_zgr_pr-ch_referencia.
              gw_aviso_aquav-dt_fatura      = gw_zlest0061-dt_fatura.
              gw_aviso_aquav-meins          = lwa_zmmt_ee_zgr_pr-meins.
              gw_aviso_aquav-material       = lwa_zmmt_ee_zgr_pr-material.

              if ( gw_zlest0060-vbeln_aviso is not initial ) and ( gw_zlest0060-id_frete_aqua_nf is not initial ).
                select single *
                  from likp into @data(likp)
                 where vbeln eq @gw_zlest0060-vbeln_aviso.

                if sy-subrc ne 0.
                  clear: gw_zlest0060-vbeln_aviso.
                  update zlest0060 set vbeln_aviso = gw_zlest0060-vbeln_aviso
                   where id_frete_aqua_nf eq gw_zlest0060-id_frete_aqua_nf.

                  commit work.

                endif.
              endif.

              clear: wl_itemdata-delivery.

              if gw_zlest0060-vbeln_aviso is not initial.
                wl_itemdata-delivery = gw_zlest0060-vbeln_aviso.
              else.
                perform gera_aviso_aquav  using gw_aviso_aquav
                                       changing wl_itemdata-delivery.

                if ( wl_itemdata-delivery is not initial ) and ( gw_zlest0060-id_frete_aqua_nf is not initial ).
                  update zlest0060 set vbeln_aviso = wl_itemdata-delivery
                   where id_frete_aqua_nf eq gw_zlest0060-id_frete_aqua_nf.

                  commit work.
                endif.
              endif.

              append wl_itemdata to lt_itemdata.

              read table gt_zsdt0011 into gw_zsdt0011 with key auart  = gw_saida-auart
                                                               tp_movimento = 'E'.
              if sy-subrc eq 0 and gw_zsdt0011-shtyp is not initial.
                lt_headerdata-shipment_type  = gw_zsdt0011-shtyp.
              else.

                lva_found_error_nf = abap_true.

                concatenate 'Tipo de OV: ' gw_saida-auart ' e Tipo de movimento E, não parametrizado na transação ZSDT0022!' into gw_saida_message-message separated by space.
                gw_saida_message-type = 'W'.
                append gw_saida_message to gt_saida_message.
                clear: gw_saida_message.
                continue.
              endif.

              gw_dados_compl-docnum        =  gw_zlest0060-docnum.
              gw_dados_compl-dt_fatura     =  gw_zlest0061-dt_fatura.
              gw_dados_compl-rm_codigo     =  gw_zlest0060-rm_codigo.
              gw_dados_compl-nfnum         =  gw_zlest0060-nfnum.
              gw_dados_compl-series        =  gw_zlest0060-series.
              gw_dados_compl-docnum_nf     =  lwa_j_1bnfdoc_pr-docnum.
              gw_dados_compl-aviso_rec     =  wl_itemdata-delivery.
              append gw_dados_compl to gt_dados_compl.
              clear gw_dados_compl.

              append value #( aviso_rec = wl_itemdata-delivery ) to gt_dados_aviso.

            endif.
          endif.
        endloop.

        if lva_found_error_nf eq abap_true.
          continue.
        endif.

        refresh: lt_vbpa[], lt_tvkn[].

*        IF PDOCNUM IS NOT INITIAL.
*          DATA(LC_TEXTO1) = 'CPROG=' && SY-CPROG && ',XPROG=' && SY-XPROG.
*          DATA(LC_TEXTO2) = ',XFORM=' && SY-XFORM && ',TCODE=' && SY-TCODE.
*          DATA(LC_TEXTO3) = ',REPI2=' && SY-REPI2.
*          MESSAGE S000 WITH LC_TEXTO1 LC_TEXTO2 LC_TEXTO3.
*        ENDIF.

        case gw_saida-frkrl_ump.
          when: 'X'.

            select vgbel
              from lips
              into table @data(t_vgbel)
                 for all entries in @lt_itemdata
            where vbeln eq @lt_itemdata-delivery.

            select count(*)
              from ekpo
                for all entries in @t_vgbel
           where ebeln eq @t_vgbel-vgbel.

            if sy-subrc is initial.

              select *
                from vbpa
                 into table lt_vbpa
               for all entries in lt_itemdata
                 where vbeln eq lt_itemdata-delivery
                   and parvw in ('LR','WE').

            else.

              select *
                from vbpa
                 into table lt_vbpa
               for all entries in lt_itemdata
                 where vbeln eq lt_itemdata-delivery
                   and parvw in ('LR','Z1').

            endif.

            check not lt_vbpa[] is initial.

            select * from tvkn
              into table lt_tvkn
              for all entries in lt_vbpa
            where kunnr eq lt_vbpa-kunnr
               or lifnr eq lt_vbpa-lifnr.

            check not lt_tvkn[] is initial.


            if vr_aviso_aquav eq abap_false.

              refresh: lt_stagedata[].
              clear: wl_stagedata.

              read table gt_zlest0056 into gw_zlest0056 index 1.
              if ( sy-subrc eq 0 ).
               select single * from lfa1 into lw_lfa1 where lifnr eq gw_zlest0056-po_embarque.
               select single * from kna1 into lw_kna1 where stcd1 eq lw_lfa1-stcd1 and stcd3 eq lw_lfa1-stcd3. "BUG SOLTO 160255 / AOENNING / 19-03-2025
              endif.

              loop at lt_vbpa into wl_vbpa.

                case wl_vbpa-parvw.
                  when: 'LR'.

                    if ( vl_org_point is not initial ) and
                       ( vl_org_cust  is not initial ).
                      wl_stagedata-org_point = vl_org_point.
                      wl_stagedata-org_cust  = vl_org_cust.
                    else.
                      read table lt_tvkn into wl_tvkn with key kunnr = wl_vbpa-kunnr.
                      if wl_vbpa-kunnr eq lw_kna1-kunnr.
                        wl_stagedata-org_point = wl_tvkn-knote.
                      else.
                        wl_stagedata-org_cust  = lw_kna1-kunnr.
                      endif.
                    endif.

                  when: 'Z1'.
                                                            "US156376

                    clear var_docnum_correcao.
                    read table gt_zlest0060 into gw_zlest0060 with key doc_rem = wl_vbpa-vbeln.
                    if sy-subrc = 0.
                      var_docnum_correcao = gw_zlest0060-docnum_rem.
                    endif.                                                            "US156376.

*                    <<<<< Inicio ajuste BUG SOLTO 159013 / AOENNING >>>>>>>>
*                    select single * from zcarta_correcao into lw_carta_correcao where docnum = var_docnum_correcao and authcode ne ' ' and novo_terminal ne ''.
                    free: it_carta_correcao.
                    clear: lw_carta_correcao.
                    select * from zcarta_correcao into table it_carta_correcao where docnum = var_docnum_correcao and authcode ne ' ' and novo_terminal ne ''.
                    sort it_carta_correcao descending by id_cc.
                    read table it_carta_correcao into lw_carta_correcao index 1.
*                   <<<<< Fim ajuste BUG SOLTO 159013 / AOENNING >>>>>>>>
                    if ( sy-subrc eq 0 ) and ( lw_carta_correcao-docnum is not initial ).
                      wl_stagedata-dest_suppl = lw_carta_correcao-novo_terminal.
                    else.
                      read table lt_tvkn into wl_tvkn with key kunnr = wl_vbpa-lifnr.
                      wl_stagedata-dest_suppl = wl_vbpa-lifnr.
                    endif.

                  when: 'WE'.
*                  <<<<< Inicio ajuste BUG SOLTO 159013 / AOENNING >>>>>>>>
*                    select single * from zcarta_correcao into lw_carta_correcao where docnum = var_docnum_correcao.
                    free: it_carta_correcao.
                    clear: lw_carta_correcao.
                    select * from zcarta_correcao into table it_carta_correcao where docnum = var_docnum_correcao.
                    sort it_carta_correcao descending by id_cc.
                    read table it_carta_correcao into lw_carta_correcao index 1.
*                    <<<<< Fim ajuste BUG SOLTO 159013 / AOENNING >>>>>>>>
                    if ( sy-subrc eq 0 ) and ( lw_carta_correcao-docnum is not initial ).
                      wl_stagedata-dest_suppl = lw_carta_correcao-novo_terminal.
                    else.

                      data: vl_stcd1 type kna1-stcd1,
                            vl_stcd3 type kna1-stcd3.

                      select single stcd1, stcd3
                        from kna1
                        into (@vl_stcd1,@vl_stcd3)
                        where kunnr eq @wl_vbpa-kunnr.

                      data(obj) = new zcl_fornecedores( ).

                      obj->zif_parceiros~set_parceiro_cnpj_cpf_ie(
                        exporting
                          i_cnpj             = conv #( vl_stcd1 )
                          i_insc_estatual    = vl_stcd3
*                         I_KTOKK            = I_KTOKK
                          i_agnorar_bloqueio = abap_true
                                               )->get_id_parceiro( importing e_parceiro = wl_stagedata-dest_suppl ).

                    endif.
                endcase.

                clear: wl_vbpa, wl_tvkn.
              endloop.

              wl_stagedata-stage_cat    = '1'.
              wl_stagedata-stage_seq    = '0001'.
              append  wl_stagedata to lt_stagedata.

            endif.

            clear: vl_tknum, vl_fknum, vl_error, vl_data_fix, vl_data_liq.
            refresh: lt_return[].

            call function 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
              exporting
                headerdata = lt_headerdata
              importing
                transport  = vl_tknum
              tables
                itemdata   = lt_itemdata
                stagedata  = lt_stagedata
                return     = lt_return.

          when others.

            refresh: lt_stagedata[].
            clear: wl_stagedata, vl_tknum.

            if line_exists( lt_return_aux[ type = 'E' ] ).
              append lines of lt_return_aux to lt_return.
            else.
              call function 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
                exporting
                  headerdata = lt_headerdata
                importing
                  transport  = vl_tknum
                tables
                  itemdata   = lt_itemdata
                  stagedata  = lt_stagedata
                  return     = lt_return.
            endif.
        endcase.

*        IF PDOCNUM IS NOT INITIAL.
*          LC_TEXTO1 = 'CPROG=' && SY-CPROG && ',XPROG=' && SY-XPROG.
*          LC_TEXTO2 = ',XFORM=' && SY-XFORM && ',TCODE=' && SY-TCODE.
*          LC_TEXTO3 = ',REPI2=' && SY-REPI2.
*          MESSAGE S000 WITH LC_TEXTO1 LC_TEXTO2 LC_TEXTO3.
*        ENDIF.

        if not ( vl_tknum is initial ).

          if pdocnum is not initial.
            "  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            loop at lt_return into data(wa_retorno).
              message id wa_retorno-id type 'S' number wa_retorno-number with wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
            endloop.
          endif.

          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.
          "ENDIF.

          clear: lt_headerdata_change,
                 lt_headerdataaction.

          refresh: lt_return.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = vl_tknum
            importing
              output = lt_headerdata_change-shipment_num.


          lt_headerdata_change-status_load_end     = 'X'.
          lt_headerdata_change-status_compl        = 'X'.
          lt_headerdata_change-status_shpmnt_start = 'X'.
          lt_headerdata_change-status_shpmnt_end   = 'X'.

          lt_headerdataaction-status_load_end     = 'C'.
          lt_headerdataaction-status_compl        = 'C'.
          lt_headerdataaction-status_shpmnt_start = 'C'.
          lt_headerdataaction-status_shpmnt_end   = 'C'.

          call function 'BAPI_SHIPMENT_CHANGE'
            exporting
              headerdata       = lt_headerdata_change
              headerdataaction = lt_headerdataaction
            tables
              return           = lt_return.

          if not ( lt_return[] is initial ).

            if pdocnum is not initial.

              loop at lt_return into wa_retorno.
                message id wa_retorno-id type 'S' number wa_retorno-number with wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
              endloop.

              call function 'BAPI_TRANSACTION_COMMIT'.
              wait up to 5 seconds.
            else.
              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.
            endif.

            "Criar objeto da classe de utilitarios.
            create object obj_zcl_util.

            obj_zcl_util->conv_data_us_br( exporting i_data  = gw_saida-dt_fatura
                                                     i_opcao = '.'
                                           receiving e_data  = vl_data_fix ).


            if ( gw_saida-data_taxa+4(2) ne sy-datum+4(2) ).
              gw_saida-data_taxa = sy-datum.
            endif.


            obj_zcl_util->conv_data_us_br( exporting i_data  = gw_saida-data_taxa
                                                     i_opcao = '.'
                                           receiving e_data  = vl_data_liq ).

            free obj_zcl_util.

            refresh: gt_bdc[].

            perform zf_bdc using: 'X' 'SAPMV54A'   '0010',
                                  ' ' 'BDC_OKCODE' '=UEBP',
                                  ' ' 'VTTK-TKNUM' vl_tknum,
                                  ' ' 'VFKK-PRSDT' vl_data_fix,
                                  ' ' 'VFKK-BUDAT' vl_data_liq.

            perform zf_bdc using: 'X' 'SAPMV54A'   '0030',
                                  ' ' 'BDC_CURSOR' 'VFKP-FKPOS(01)',
                                  ' ' 'BDC_OKCODE' '=PDET'.

            if ( gw_saida-waerk_fatura ne 'BRL' ).
              "Desmarca Transferência
              perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                                    ' ' 'BDC_OKCODE' '=PABR'.

              perform zf_bdc using: 'X' 'SAPMV54A'     '0040',
                                    ' ' 'BDC_OKCODE'   '=PPRI',
                                    ' ' 'VFKPD-SLFREI' ' '.
            endif.

            perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                                  ' ' 'BDC_OKCODE' '/00',
                                  ' ' 'VFKP-POSTX' vl_tknum,
                                  ' ' 'VFKP-PRSDT' vl_data_fix,
                                  ' ' 'VFKP-KURST' 'B'.

            if ( gw_saida-waerk_fatura ne 'BRL' ).

              perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                                    ' ' 'BDC_OKCODE' '/00',
                                    ' ' 'VFKP-WAERS' gw_saida-waerk_fatura.

              "Marca Transferencia
              perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                                    ' ' 'BDC_OKCODE' '=PABR'.

              perform zf_bdc using: 'X' 'SAPMV54A'     '0040',
                                    ' ' 'BDC_OKCODE' '=SICH',
                                    ' ' 'VFKPD-SLFREI' 'X'.
            else.
              perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                                    ' ' 'BDC_OKCODE' '=SICH',
                                    ' ' 'VFKP-POSTX' vl_tknum,
                                    ' ' 'VFKP-PRSDT' vl_data_fix.
            endif.

            vl_mode = 'N'.
            refresh: gt_msg.

            call transaction 'VI01' using gt_bdc mode vl_mode update 'S' messages into gt_msg.
            if pdocnum is not initial.
              commit work.
              loop at gt_msg into data(wa_msg).
                message id wa_msg-msgid type 'S' number wa_msg-msgnr with wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4.
              endloop.
            else.
              commit work and wait.
            endif.

            clear: vl_fknum.
            loop at gt_msg into gw_msg where ( msgtyp eq 'S' )
                                         and ( msgv1 is not initial )
                                         and ( env   is not initial )
                                         and ( dyname eq 'SAPMV54A' )
                                         and ( msgid eq 'VY' ).
              vl_fknum = gw_msg-msgv1.
            endloop.

            if vl_fknum is not initial.
              clear: gt_msg[].
            else.
              loop at gt_msg into gw_msg.
                message id gw_msg-msgid type 'S'
                 number gw_msg-msgnr
                   with gw_msg-msgv1 gw_msg-msgv2 gw_msg-msgv3 gw_msg-msgv4
                   into data(i_texto).

                gw_saida_message-type    = gw_msg-msgtyp.
                gw_saida_message-message = i_texto.
                append gw_saida_message to gt_saida_message.
                vl_error = 'VI'.
              endloop.
            endif.


*              CASE GW_MSG-MSGTYP.
*                WHEN: 'S'.
*                  IF NOT ( GW_MSG-MSGV1 IS INITIAL ) AND NOT ( GW_MSG-ENV IS INITIAL ) AND ( GW_MSG-DYNAME EQ 'SAPMV54A' ) AND ( GW_MSG-MSGID EQ 'VY' ).
*                    VL_FKNUM = GW_MSG-MSGV1.
*                  ENDIF.
*
*                WHEN: 'E'.
*
*                  MESSAGE ID GW_MSG-MSGID TYPE 'S'
*                   NUMBER GW_MSG-MSGNR
*                     WITH GW_MSG-MSGV1 GW_MSG-MSGV2 GW_MSG-MSGV3 GW_MSG-MSGV4
*                     INTO DATA(I_TEXTO).
*
*                  GW_SAIDA_MESSAGE-TYPE    = GW_MSG-MSGTYP.
*                  GW_SAIDA_MESSAGE-MESSAGE = I_TEXTO.
*                  APPEND GW_SAIDA_MESSAGE TO GT_SAIDA_MESSAGE.
*                  VL_ERROR = 'VI'.
*
*              ENDCASE.



            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = vl_fknum
              importing
                output = vl_fknum.


            if ( vl_error is initial ) and not ( vl_fknum is initial ).

              clear: wl_vfkp, vl_estorno.
              refresh: lt_konv[].

              select single * from vfkp
                into wl_vfkp
               where fknum eq vl_fknum.

              check not wl_vfkp is initial.

              try.

                  cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                    exporting it_selection_attribute = value #(
                   ( fieldname = 'KNUMV' value = wl_vfkp-knumv )
                   ( fieldname = 'KAPPL' value = 'F' )
                   ( fieldname = 'KSCHL' value = 'ZFRE' )
                   )
                    importing et_prc_element_classic_format = lt_konv ).
                catch cx_prc_result .
                  sy-subrc = 4.
              endtry.

              if ( sy-subrc eq 0 ).

                clear: vl_vlr_dif, wl_maxdif, vl_maxdiv, vl_total.

                loop at lt_konv into wl_konv.
                  add wl_konv-kwert to vl_total.
                endloop.

                case gw_saida-waerk_fatura.
                  when: 'BRL'.
                    vl_vlr_dif = abs( gw_saida-vlr_brl - vl_total ).
                  when others.
                    vl_vlr_dif = abs( gw_saida-vlr_usd - vl_total ).
                endcase.


                select single valfrom
                 into wl_maxdif
                 from setleaf
                where setname eq 'ZMAXDIF'.

                move wl_maxdif to vl_maxdiv.

                if ( vl_vlr_dif > vl_maxdiv ).

                  clear: vl_estorno.

                  perform: estornar_vt_vi tables lt_itemdata gt_dados_aviso using vl_tknum vl_fknum wl_vfkp changing vl_estorno.

                  if not ( vl_estorno is initial ).

                    clear: vl_vlr_fatura, vl_vlr_custo.


                    case gw_saida-waerk_fatura.
                      when: 'BRL'.
                        vl_vlr_fatura = gw_saida-vlr_brl.
                      when others.
                        vl_vlr_fatura = gw_saida-vlr_usd.
                    endcase.

                    vl_vlr_custo  = vl_total.

                    if pdocnum is not initial.
                      message e013 with gw_saida-cte_serie vl_vlr_fatura vl_vlr_custo.
                    else.
                      concatenate  'Vlr. fatura difere vlr. custo, Cte' ':' gw_saida-cte_serie ' ;Vlr. Fat.: ' vl_vlr_fatura ';Vlr. Custo: ' vl_vlr_custo into vl_msg.
                      clear: gw_saida_message.
                      gw_saida_message-message = vl_msg.
                      append gw_saida_message to gt_saida_message.
                    endif.

                  else.
                    "Mensagem de Documento não estornado.
                  endif.

                else.
                  "Gravar a VT/VI na tabela ZLEST0061
                  update zlest0061 set tknum = vl_tknum
                                       fknum = vl_fknum where docnum     eq gw_saida-docnum
                                                          and nr_viagem  eq gw_saida-nr_viagem
                                                          and cl_codigo  eq gw_saida-cl_codigo
                                                          and ano_viagem eq gw_saida-ano_viagem.

                  commit work.

                  gw_saida-tknum  = vl_tknum.
                  gw_saida-fknum  = vl_fknum.
                  gw_saida-status = icon_led_green.

                  clear: gw_saida-estilo_cell.

                  wl_estilo_cell-fname = 'TKNUM'.
                  wl_estilo_cell-color-col   = '5'.
                  wl_estilo_cell-color-int   = '0'.
                  wl_estilo_cell-color-inv   = '0'.
                  append wl_estilo_cell to lt_estilo_cell.
                  insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

                  wl_estilo_cell-fname = 'FKNUM'.
                  wl_estilo_cell-color-col   = '5'.
                  wl_estilo_cell-color-int   = '0'.
                  wl_estilo_cell-color-inv   = '0'.
                  append wl_estilo_cell to lt_estilo_cell.
                  insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

*                  IF LVA_CHAVE_ELET EQ 'N'.
*                    WA_CTE_N01-TKNUM = VL_TKNUM.
*                    WA_CTE_N01-FKNUM = VL_FKNUM.
*                    MODIFY  ZIB_CTE_DIST_N01 FROM   WA_CTE_N01.
*                  ELSE.
*                    WA_CTE_N55-TKNUM = VL_TKNUM.
*                    WA_CTE_N55-FKNUM = VL_FKNUM.
*                    MODIFY ZIB_CTE_DIST_N55 FROM   WA_CTE_N55.
*                  ENDIF.

                  modify gt_saida from gw_saida index sl_rows-index transporting tknum fknum status estilo_cell.
                endif.
              else.
                gw_saida_message-message = 'Não existe Tarifa de Frete cadastrado!'.
                gw_saida_message-type = 'E'.
                append gw_saida_message to gt_saida_message.
                clear: gw_saida_message.
                "Estornar a VT/VI
                perform: estornar_vt_vi tables lt_itemdata gt_dados_aviso using vl_tknum vl_fknum wl_vfkp changing vl_estorno.
              endif.

            else.
              "Estornar a VT
              call function 'BAPI_TRANSACTION_ROLLBACK'.
              clear: vl_estorno.
              perform: estornar_vt_vi tables lt_itemdata gt_dados_aviso using vl_tknum vl_fknum wl_vfkp changing vl_estorno.
            endif.

          else.
            call function 'BAPI_TRANSACTION_ROLLBACK'.
            perform: estornar_vt_vi tables lt_itemdata gt_dados_aviso using vl_tknum vl_fknum wl_vfkp changing vl_estorno.
          endif.

        else.
          "Caso a BAPI da VT não execute corretamente.
          call function 'BAPI_TRANSACTION_ROLLBACK'.

          refresh: gt_saida_message[].

          check not lt_return[] is initial.

          loop at lt_return into wl_return.


            case wl_return-type.
              when 'E'.
                gw_saida_message-type      = wl_return-type.
                gw_saida_message-message   = wl_return-message.
                append gw_saida_message to gt_saida_message.
            endcase.

            clear: gw_saida_message, wl_return.

          endloop.

          if ( pdocnum is initial ).
            lva_show_message_erro = abap_true.
            call screen 1200 starting at 055 2 ending   at 173 30.

          endif.

        endif.

*        "Caso a BAPI da VT não execute corretamente.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      else.

      endif.
    endloop.

  else.
    message s000 display like 'W' with 'Selecionar uma linha.'.
  endif.

  if not ( gt_saida_message[] is initial ) and ( pdocnum is initial ) and ( lva_show_message_erro is initial ).
    call screen 1200 starting at 055 2 ending   at 173 30.
  endif.

  if pdocnum is initial.
    wl_stable-row = 'X'.
    wl_stable-col = 'X'.
    call method obj_grid->refresh_table_display
      exporting
        is_stable = wl_stable.
  endif.

endform.                    " GERAR_VT_VI
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR
*&---------------------------------------------------------------------*
form estornar using p_index type i.

  data: tl_rows     type lvc_t_row,
        sl_rows     type lvc_s_row,
        wl_zsdt0001 type zsdt0001,
        lt_itemdata type table of bapishipmentitem,
        wl_itemdata type bapishipmentitem,
        wl_vfkp     type vfkp,
        lt_konv     type table of konv,
        wl_konv     type konv,
        vl_tknum    type vttk-tknum,
        vl_fknum    type vfkp-fknum.

  data: lt_estilo_cell type lvc_t_scol with header line,
        wl_estilo_cell type lvc_s_scol.


  data: wl_stable      type lvc_s_stbl.

  if p_index is initial.
    call method obj_grid->get_selected_rows
      importing
        et_index_rows = tl_rows.
  else.
    sl_rows-index = p_index.
    append sl_rows to tl_rows.
  endif.

  clear: gt_dados_aviso[].

  if not ( tl_rows[] is initial ).

    loop at tl_rows into sl_rows.

      read table gt_saida into gw_saida index sl_rows-index.

      if ( sy-subrc eq 0 ).

        select * from zlest0060
          into table gt_zlest0060
        where bukrs       eq gw_saida-bukrs
          and werks       eq gw_saida-werks
          and ano_viagem  eq gw_saida-ano_viagem
          and nr_viagem   eq gw_saida-nr_viagem
          and nome_emb    eq gw_saida-nome_emb
          and cl_codigo   eq gw_saida-cl_codigo
          and rm_codigo   eq gw_saida-rm_codigo
          and nr_dco      eq gw_saida-nr_dco
          and safra       eq gw_saida-safra
          and operacao    eq gw_saida-operacao.

        loop at gt_zlest0060 into gw_zlest0060.
          select single * from zsdt0001 into wl_zsdt0001 where bukrs       eq gw_zlest0060-bukrs
                                                           and branch      eq gw_zlest0060-dt_codigo
                                                           and nr_safra    eq gw_zlest0060-safra
                                                           and parid       eq gw_zlest0060-rm_codigo
                                                           and nr_romaneio eq gw_zlest0060-nr_romaneio.
          if ( sy-subrc eq 0 ).
            wl_itemdata-delivery = wl_zsdt0001-doc_rem.
            append wl_itemdata to lt_itemdata.
          endif.

          if gw_zlest0060-vbeln_aviso is not initial.
            append value #( aviso_rec = gw_zlest0060-vbeln_aviso ) to gt_dados_aviso.
          endif.

        endloop.

        vl_tknum = gw_saida-tknum.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = gw_saida-fknum
          importing
            output = vl_fknum.

        select single * from vfkp
          into wl_vfkp
         where fknum eq vl_fknum.

        data(lc_vl_tknum) = vl_tknum.
        data(lc_vl_fknum) = vl_fknum.

        perform: estornar_vt_vi tables lt_itemdata gt_dados_aviso using vl_tknum vl_fknum wl_vfkp changing vl_estorno.

        if not ( vl_estorno is initial ).

          clear: vl_tknum, vl_fknum.

          update zlest0061 set tknum = vl_tknum
                               fknum = vl_fknum
                               where docnum     eq gw_saida-docnum
                                 and nr_viagem  eq gw_saida-nr_viagem
                                 and cl_codigo  eq gw_saida-cl_codigo
                                 and ano_viagem eq gw_saida-ano_viagem.

          update zib_cte_dist_n55
             set tknum = space
                 fknum = space
                 lblni = space
                 ebeln = space
                 ebelp = space
           where tknum eq lc_vl_tknum
             and tknum ne space.

          commit work.

          gw_saida-status = icon_led_red.

          clear: gw_saida-estilo_cell, gw_saida-tknum, gw_saida-fknum.

          wl_estilo_cell-fname = 'TKNUM'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to lt_estilo_cell.
          insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

          wl_estilo_cell-fname = 'FKNUM'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          append wl_estilo_cell to lt_estilo_cell.
          insert lines of lt_estilo_cell into table gw_saida-estilo_cell.

          modify gt_saida from gw_saida index sl_rows-index transporting tknum fknum status estilo_cell.

        endif.

      endif.

    endloop.

  endif.

  if pdocnum is initial.
    wl_stable-row = 'X'.
    wl_stable-col = 'X'.
    call method obj_grid->refresh_table_display
      exporting
        is_stable = wl_stable.
  endif.

endform.                    " ESTORNAR

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_VT_VI
*&---------------------------------------------------------------------*
form estornar_vt_vi tables pt_itemdata structure bapishipmentitem
                           t_avisos_rec like gt_dados_aviso
                     using p_tknum     type zlest0061-tknum
                           p_fknum     type zlest0061-fknum
                           w_vfkp      structure vfkp
                           changing vl_estorno type c.


  data: vl_mode             type c length 1,
        lt_itemdataaction   type table of bapishipmentitemaction with header line,
        lt_headerdataaction type bapishipmentheaderaction,
        wa_itemdata         type bapishipmentitem,
        wa_itemdataaction   type bapishipmentitemaction,
        it_itemdataaction   type table of bapishipmentitemaction,
        it_itemdata         type table of bapishipmentitem,
        lt_headerdata       type bapishipmentheader,
        wl_itemdata         type bapishipmentitem,
        lt_return           like bapiret2 occurs 0 with header line,
        wl_return           type bapiret2,
        wa_vttp             type vttp.

  data: wl_stable      type lvc_s_stbl.

  "Estornar a VI
  if not ( p_fknum is initial ).

    refresh: gt_bdc[].


    "Bloquear como Estorno a VI
    perform zf_bdc using: 'X' 'SAPMV54A'   '0020',
                      ' ' 'BDC_OKCODE' '=UEBP',
                      ' ' 'VFKK-FKNUM' p_fknum.

    perform zf_bdc using: 'X' 'SAPMV54A'   '0030',
                          ' ' 'BDC_CURSOR' 'VFKP-FKPOS(01)',
                          ' ' 'BDC_OKCODE' '=PDET'.

    perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                          ' ' 'BDC_OKCODE' '=PABR',
                          ' ' 'VFKP-POSTX' w_vfkp-postx.

    perform zf_bdc using: 'X' 'SAPMV54A'   '0040',
                          ' ' 'BDC_OKCODE' '=SICH',
                          ' ' 'VFKP-POSTX' w_vfkp-postx,
                          ' ' 'VFKPD-SLSTOR' 'X'.
    vl_mode = 'N'.
    call transaction 'VI02' using  gt_bdc mode vl_mode update 'S' messages into gt_msg.

    if pdocnum is not initial.
      commit work.
      loop at gt_msg into data(wa_msg).
        message id wa_msg-msgid type 'S' number wa_msg-msgnr with wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4.
      endloop.
    else.
      commit work and wait.
    endif.

    "Eliminar a VI
    refresh: gt_bdc[].
    perform zf_bdc using:  'X' 'SAPMV54A'   '0020',
                           ' ' 'BDC_OKCODE' '=UEBP',
                           ' ' 'VFKK-FKNUM' p_fknum.

    perform zf_bdc using: 'X' 'SAPMV54A' '0030',
                          ' ' 'BDC_OKCODE' '/ELOES'.

    refresh: gt_msg[].

    call transaction 'VI02' using gt_bdc mode vl_mode update 'S' messages into gt_msg.

    if pdocnum is not initial.
      commit work.
      loop at gt_msg into wa_msg.
        message id wa_msg-msgid type 'S' number wa_msg-msgnr with wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4.
      endloop.
    else.
      commit work and wait.
    endif.

    vl_estorno = 'X'.
  endif.

  "Estornar a VT
  if not ( p_tknum is initial ).

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_tknum
      importing
        output = lt_headerdata-shipment_num.

    lt_headerdataaction-shipment_num     = 'D'.
    lt_headerdataaction-service_agent_id = 'D'.

    select * into wa_vttp
      from vttp
     where tknum eq lt_headerdata-shipment_num.

      wa_itemdata-delivery  = wa_vttp-vbeln.
      wa_itemdata-itenerary = wa_vttp-tpnum.
      append wa_itemdata to it_itemdata.

      wa_itemdataaction-delivery  = 'D'.
      wa_itemdataaction-itenerary = 'D'.
      append wa_itemdataaction to it_itemdataaction.

    endselect.

    call function 'BAPI_SHIPMENT_CHANGE'
      exporting
        headerdata       = lt_headerdata
        headerdataaction = lt_headerdataaction
      tables
        itemdata         = it_itemdata
        itemdataaction   = it_itemdataaction
        return           = lt_return.

    if pdocnum is not initial.
      call function 'BAPI_TRANSACTION_COMMIT'.
      commit work.
      loop at lt_return into data(wa_retorno).
        message id wa_retorno-id type 'S' number wa_retorno-number with wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.
      endloop.
    else.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
      commit work and wait.
    endif.
  endif.


  loop at t_avisos_rec into data(lwa_aviso_rec) where aviso_rec is not initial.

    data: it_mesg     type standard table of mesg,
          sl_hdata    type bapiobdlvhdrchg,
          sl_hcont    type bapiobdlvhdrctrlchg,
          tl_bapiret2 type bapiret2_t,
          vl_delivery type bapishpdelivnumb-deliv_numb.

    " CLEAR: P_ERRO, IT_MESG[].

    clear vl_delivery.

    "Deleta Delivery Criado
    sl_hdata-deliv_numb = lwa_aviso_rec-aviso_rec.
    sl_hcont-deliv_numb = lwa_aviso_rec-aviso_rec.
    sl_hcont-dlv_del    = 'X'.


    get parameter id 'Z_MY_PARAMETER_2' field vl_delivery.

    clear: tl_bapiret2[].

    call function 'BAPI_OUTB_DELIVERY_CHANGE' " (VL02N)
      exporting
        header_data    = sl_hdata
        header_control = sl_hcont
        delivery       = vl_delivery
      tables
        return         = tl_bapiret2.

*    IF SY-SUBRC NE 0.
*      P_ERRO = 'X'.
*      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      EXIT.
*    ENDIF.

    loop at tl_bapiret2 into data(wl_bapiret) where type = 'E'.
      "P_ERRO = 'X'.
      message id wl_bapiret-id type 'I' number wl_bapiret-number with wl_bapiret-message_v1 wl_bapiret-message_v2 wl_bapiret-message_v3 wl_bapiret-message_v4.
      return.
    endloop.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    select single *
      from likp into @data(lwa_likp_aviso)
     where vbeln eq @lwa_aviso_rec-aviso_rec.

    if ( sy-subrc ne 0 ) and ( lwa_aviso_rec-aviso_rec is not initial ).
      update zlest0060 set vbeln_aviso = space
       where vbeln_aviso = lwa_aviso_rec-aviso_rec.

      commit work.
    endif.

  endloop.

endform.                    " ESTORNAR_VT_VI
*&---------------------------------------------------------------------*
*&      Form  ZF_BDC
*&---------------------------------------------------------------------*
form zf_bdc  using p_dynbegin type any
                   p_name     type any
                   p_value    type any.


  if p_dynbegin eq 'X'.

    gw_bdc-program  = p_name.
    gw_bdc-dynpro   = p_value.
    gw_bdc-dynbegin = p_dynbegin.

    append gw_bdc to gt_bdc.
  else.
    gw_bdc-fnam = p_name.
    gw_bdc-fval = p_value.

    append gw_bdc to gt_bdc.
  endif.

  clear gw_bdc.

endform.                    " ZF_BDC
*&---------------------------------------------------------------------*
*&      Module  PBO_1200  OUTPUT
*&---------------------------------------------------------------------*
module pbo_1200 output.
  set pf-status 'PF1200'. "Status da Tela 01200.
  set titlebar  'TB1200'. "Title da Tela 01200.
  perform: tela_message_erro.
endmodule.                 " PBO_1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_1200  INPUT
*&---------------------------------------------------------------------*

module pbo_1200 input.
  case sy-ucomm.
    when: 'FECHAR'. "Botão para Voltar a Tela anterior.
      leave to screen 0.
  endcase.
endmodule.                 " PBO_1200  INPUT
*&---------------------------------------------------------------------*
*&      Form  TELA_MESSAGE_ERRO
*&---------------------------------------------------------------------*
form tela_message_erro .

  data: wl_layout  type lvc_s_layo,
        wl_variant type disvariant.


  data: wl_stable      type lvc_s_stbl.

  if ( obj_custom_msg is initial ).

    clear: wl_layout, wl_variant.

    perform: criar_catalog_message.

    create object obj_custom_msg
      exporting
        container_name              = 'CONTAINER_MESSAGE'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    create object obj_grid_msg
      exporting
        i_parent          = obj_custom_msg
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    wl_layout-sel_mode   = 'A'.
    wl_variant-report   = sy-repid.
    wl_variant-username = sy-uname.

    call method obj_grid_msg->set_table_for_first_display
      exporting
        is_layout                     = wl_layout
        i_save                        = 'A'
        is_variant                    = wl_variant
      changing
        it_outtab                     = gt_saida_message[]
        it_fieldcatalog               = gt_fcat_msg[]
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

  else.

    call method obj_grid_msg->refresh_table_display
      exporting
        is_stable = wl_stable.


  endif.



endform.                    " TELA_MESSAGE_ERRO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_MESSAGE
*&---------------------------------------------------------------------*
form criar_catalog_message .

  perform montar_catalog_message using:
     'TYPE'           'Status'           '10'  '' '' '' '' '' '' '' ''  '' '',
     'MESSAGE'        'Mensagem'         '70'  '' '' '' '' '' '' '' ''  '' ''.

endform.                    " CRIAR_CATALOG_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_MESSAGE
*&---------------------------------------------------------------------*
form montar_catalog_message  using  value(p_fieldname)
                             value(p_desc)
                             value(p_tam)
                             value(p_no_zero)
                             value(p_hotspot)
                             value(p_cor)
                             value(p_just)
                             value(p_sum)
                             value(p_edit)
                             value(p_ref_tabname)   like dd02d-tabname
                             value(p_ref_fieldname) like dd03d-fieldname
                             value(p_tabname)       like dd02d-tabname
                             value(p_check).

  clear: gw_fcat_msg.

  gw_fcat_msg-fieldname = p_fieldname.
  gw_fcat_msg-ref_table = p_ref_tabname..
  gw_fcat_msg-ref_field = p_ref_fieldname.
  gw_fcat_msg-tabname   = p_tabname.
  gw_fcat_msg-scrtext_l = p_desc.
  gw_fcat_msg-scrtext_m = p_desc.
  gw_fcat_msg-scrtext_s = p_desc.
  gw_fcat_msg-outputlen = p_tam.
  gw_fcat_msg-no_zero   = p_no_zero.
  gw_fcat_msg-hotspot   = p_hotspot.
  gw_fcat_msg-emphasize = p_cor.
  gw_fcat_msg-just      = p_just.
  gw_fcat_msg-do_sum    = p_sum.
  gw_fcat_msg-edit      = p_edit.
  gw_fcat_msg-checkbox  = p_check.

  append gw_fcat_msg  to gt_fcat_msg.


endform.                    " MONTAR_CATALOG_MESSAGE

form f_get_remessa_nf  tables p_lt_itemdata structure bapishipmentitem
                        using p_zlest0060 type zlest0060.

  ranges: r_nr_nf           for zlest0041-nr_nf,
          r_serie           for zlest0041-serie,
          r_cod_cliente     for zlest0041-cod_cliente.

  data: wl_active    type j_1bnfe_active,
        wl_lin       type j_1bnflin,
        wl_item_data type bapishipmentitem,
        tg_zlest0041 type table of zlest0041 with header line,
        w_werks      type zlest0060-werks.

  if  gw_zlest0060-chave_nfe+0(1) = 'F'.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gw_zlest0060-cl_codigo
      importing
        output = w_werks.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = w_werks
      importing
        output = w_werks.
    select *
        from zlest0041 into table tg_zlest0041
       where nr_nf             eq gw_zlest0060-nfnum
         and cod_cliente       eq gw_zlest0060-rm_codigo
         and centro_comprador  eq w_werks.

    read table tg_zlest0041 index 1.

    check ( sy-subrc eq 0 ) and ( tg_zlest0041[] is not initial ) and ( tg_zlest0041-docnum is not initial  ) .

    select single *
      from j_1bnflin into @data(wl_nflin_prop2)
     where docnum eq @tg_zlest0041-docnum.

    check sy-subrc eq 0 and wl_nflin_prop2-reftyp = 'BI'.

    select single *
      from vbrp into @data(_wl_vbrp2)
     where vbeln = @wl_nflin_prop2-refkey.

    check sy-subrc eq 0.

    select single *
      from likp into @data(_wl_likp2)
     where vbeln = @_wl_vbrp2-vgbel.

    check sy-subrc eq 0.

    wl_item_data-delivery = _wl_likp2-vbeln.
    append wl_item_data to p_lt_itemdata.

  else.
    select single *
      from j_1bnfe_active into wl_active
     where regio   eq gw_zlest0060-chave_nfe(2)
       and nfyear  eq gw_zlest0060-chave_nfe+2(2)
       and nfmonth eq gw_zlest0060-chave_nfe+4(2)
       and stcd1   eq gw_zlest0060-chave_nfe+6(14)
       and model   eq gw_zlest0060-chave_nfe+20(2)
       and serie   eq gw_zlest0060-chave_nfe+22(3)
       and nfnum9  eq gw_zlest0060-chave_nfe+25(9).

    check ( sy-subrc eq 0 ).

    select single *
      from j_1bnflin into wl_lin
     where docnum eq wl_active-docnum.

    check ( sy-subrc eq 0 ).

    select single *
      from j_1bnfdoc into @data(_wl_doc)
     where docnum eq @wl_active-docnum.

    check ( sy-subrc eq 0 ).

    case wl_lin-reftyp.
      when: 'LI'.

        check wl_active-direct eq '1'.

        clear: r_nr_nf[], tg_zlest0041[], r_cod_cliente[].

        "Parceiro
        if ( _wl_doc-partyp = 'V' ).
          r_cod_cliente-sign   = 'I'.
          r_cod_cliente-option = 'EQ'.
          r_cod_cliente-low    = _wl_doc-parid.
          append r_cod_cliente.
        endif.

        check r_cod_cliente[] is not initial.

        "Numero NF
        r_nr_nf-sign   = 'I'. r_nr_nf-option = 'EQ'.
        if _wl_doc-nfenum is not initial.
          r_nr_nf-low    = |{ _wl_doc-nfenum alpha = in  }|. append r_nr_nf.
          r_nr_nf-low    = |{ _wl_doc-nfenum alpha = out }|. append r_nr_nf.
        else.
          r_nr_nf-low    = |{ _wl_doc-nfnum alpha = in  }|. append r_nr_nf.
          r_nr_nf-low    = |{ _wl_doc-nfnum alpha = out }|. append r_nr_nf.
        endif.

        select *
          from zlest0041 into table tg_zlest0041
         where nr_nf            in r_nr_nf
           and cod_cliente      in r_cod_cliente.

        delete tg_zlest0041 where centro_comprador ne _wl_doc-branch.
        read table tg_zlest0041 index 1.

        check ( sy-subrc eq 0 ) and ( tg_zlest0041[] is not initial ) and ( tg_zlest0041-docnum is not initial  ) .

        select single *
          from j_1bnflin into @data(wl_nflin_prop)
         where docnum eq @tg_zlest0041-docnum.

        check sy-subrc eq 0 and wl_nflin_prop-reftyp = 'BI'.

        select single *
          from vbrp into @data(_wl_vbrp)
         where vbeln = @wl_nflin_prop-refkey.

        check sy-subrc eq 0.

        select single *
          from likp into @data(_wl_likp)
         where vbeln = @_wl_vbrp-vgbel.

        check sy-subrc eq 0.

        wl_item_data-delivery = _wl_likp-vbeln.
        append wl_item_data to p_lt_itemdata.

    endcase.
  endif.


endform.


form gera_aviso_rec using wa_aviso type ty_aviso
                    changing r_vbeln type bapishipmentitem-delivery .

  data: s_xblnr         type xblnr1,
        s_nf            type zlest0060-nfnum,
        s_serie         type c,
        v_wl_parid      type j_1bparid,
        r_documento(10).



  s_nf    = wa_aviso-p_nfnum.
  s_serie = wa_aviso-p_series+2(1).

  concatenate s_nf '-' s_serie  into s_xblnr.


  select single *
    from ekes into @data(wa_ekes)
   where ebeln eq  @wa_aviso-po_number
    and  ebelp eq  '00010'
    and  xblnr eq  @s_xblnr.

  if sy-subrc = 4.

    select single * from ekko into @data(wa_ekko)
      where ebeln eq @wa_aviso-po_number.

    if sy-subrc = 0.

      select single * from ekpo into @data(wa_ekpo)
        where ebeln eq @wa_ekko-ebeln.

      select  single * from lfa1 into @data(wa_lfa1)
        where lifnr eq @wa_aviso-po_embarque
        and   land1 eq 'BR'.


      select single * from kna1 into @data(wa_kna1)
        where kunnr eq @wa_aviso-po_destino
        and   land1 eq 'BR'.


      select single * from trolz into @data(wa_trolz)
        where  aland 	eq 'BR'
          and  azone  eq @wa_lfa1-lzone
          and  lland  eq 'BR'
          and  lzone  eq @wa_kna1-lzone.


      select single * from eket into @data(wa_eket)
          where ebeln eq @wa_ekpo-ebeln
            and ebelp eq @wa_ekpo-ebelp.


      select single *  from mara into @data(wa_mara)
       where matnr eq @wa_ekpo-matnr.


      create object zcl_aviso_recebimento.

      zcl_aviso_recebimento->set_fornecedor( i_lifnr = wa_ekko-lifnr ).
      zcl_aviso_recebimento->set_pedido_compra( i_ebeln = wa_ekko-ebeln ).
      zcl_aviso_recebimento->set_route( i_route = wa_trolz-route ).
      zcl_aviso_recebimento->set_data_lancamento( i_bldat = sy-datum ).


      i_item-ebeln        = wa_ekpo-ebeln.
      i_item-ebelp        = wa_ekpo-ebelp.
      i_item-vgtyp        = 'V'.
      i_item-quantidade   = wa_aviso-quantity.
      i_item-unidade      = wa_ekpo-meins.
      i_item-material     = wa_ekpo-matnr.
      i_item-traty        = '0001'.
      i_item-tragr        = '0001'.
      i_item-ladgr        = '0003'.
      i_item-mfrgr        = '00000001'.
      i_item-kzbew        = 'B'.
      i_item-plant        = wa_ekpo-werks.
      i_item-stge_loc     = wa_ekpo-lgort.

      if wa_mara-xchpf = 'X'.
        i_item-batch = wa_eket-charg.
        i_item-licha = wa_eket-charg.
      endif.

      zcl_aviso_recebimento->set_item( i_item = i_item ).
      zcl_aviso_recebimento->set_lc_coleta_parid( i_parid = wa_aviso-po_embarque ).
      zcl_aviso_recebimento->set_lc_coleta_partyp( i_partyp = 'V' ).

      zcl_aviso_recebimento->set_lc_entrega_parid( i_parid = wa_aviso-po_destino ).
      zcl_aviso_recebimento->set_lc_entrega_partyp( i_partyp = 'V' ).

      data(valor_nf) = wa_aviso-nftot.
      zcl_aviso_recebimento->set_valor_nota( i_valor_nota = valor_nf ).

      zcl_aviso_recebimento->set_xblnr( i_xblnr = s_xblnr ).
      zcl_aviso_recebimento->set_ch_referencia( i_ch_referencia = wa_aviso-ch_referencia ).

      data(r_gerou) = zcl_aviso_recebimento->criar_aviso_recebimento( i_particao_lote = abap_true ).
      data(r_retorno) = zcl_aviso_recebimento->get_retorno( ).

      if r_gerou eq abap_true.
        r_documento = zcl_aviso_recebimento->get_nr_remessa( ).
        r_vbeln = r_documento.
      else.
        data(wl_erro) = 'X'.
        message 'Hove um erro ao gerar o aviso! ' type 'S'.
      endif.
    endif.

    clear: zcl_aviso_recebimento.
  else.
    r_vbeln = wa_ekes-vbeln.
  endif.

endform.

form gera_aviso_aquav using wa_aviso_aquav type ty_aviso_aquav
                     changing vr_vbeln type bapishipmentitem-delivery.

  data vr_documento(10).

  clear: i_item, vr_vbeln.

  if wa_aviso_aquav is not initial .

    select single * from lfa1 into @data(gw_lfa1)
      where lifnr eq @wa_aviso_aquav-po_embarque
      and   land1 eq 'BR'.

    select single * from kna1 into @data(gw_kna1)
      where kunnr eq @wa_aviso_aquav-po_destino
      and   land1 eq 'BR'.

    select single * from trolz into @data(gw_trolz)
      where  aland 	eq 'BR'
        and  azone  eq @gw_lfa1-lzone
        and  lland  eq 'BR'
        and  lzone  eq @gw_kna1-lzone.

    select single * from ekko into @data(gw_ekko)
      where ebeln eq @wa_aviso_aquav-po_number.

    select single * from ekpo into @data(gw_ekpo)
      where ebeln eq @wa_aviso_aquav-po_number
      and   ebelp eq @wa_aviso_aquav-po_item.

    select single * from eket into @data(gw_eket)
      where ebeln eq @wa_aviso_aquav-po_number
      and   ebelp eq @wa_aviso_aquav-po_item.

    select single * from mara into @data(gw_mara)
     where matnr eq @gw_ekpo-matnr.


    create object zcl_aviso_recebimento.
    zcl_aviso_recebimento->set_fornecedor( i_lifnr = gw_ekko-lifnr ).
    zcl_aviso_recebimento->set_pedido_compra( i_ebeln = gw_ekko-ebeln ).
    zcl_aviso_recebimento->set_route( i_route = gw_trolz-route ).
    zcl_aviso_recebimento->set_data_lancamento( i_bldat = wa_aviso_aquav-dt_fatura ).

    i_item-ebeln      = wa_aviso_aquav-po_number.
    i_item-ebelp      = wa_aviso_aquav-po_item.
    i_item-vgtyp      = 'V'.
    i_item-quantidade = wa_aviso_aquav-entry_qnt.
    i_item-unidade    = wa_aviso_aquav-meins.
    i_item-material   = wa_aviso_aquav-material.
    i_item-traty      = '0001'.
    i_item-tragr      = '0001'.
    i_item-ladgr      = '0003'.
    i_item-mfrgr      = '00000001'.
    i_item-kzbew      = 'B'.
    i_item-plant      = gw_ekpo-werks.
    i_item-stge_loc   = gw_ekpo-lgort.

    if gw_mara-xchpf eq 'X'.
      i_item-batch = gw_eket-charg.
      i_item-licha = gw_eket-licha.
    endif.

    zcl_aviso_recebimento->set_item( i_item = i_item ).
    zcl_aviso_recebimento->set_lc_coleta_parid( i_parid = wa_aviso_aquav-po_embarque ).
    zcl_aviso_recebimento->set_lc_coleta_partyp( i_partyp = 'V' ).

    zcl_aviso_recebimento->set_lc_entrega_parid( i_parid = wa_aviso_aquav-po_destino ).
    zcl_aviso_recebimento->set_lc_entrega_partyp( i_partyp = 'V' ).

    zcl_aviso_recebimento->set_valor_nota( i_valor_nota = 0 ).

    zcl_aviso_recebimento->set_xblnr( i_xblnr = wa_aviso_aquav-ref_doc_no ).
    zcl_aviso_recebimento->set_ch_referencia( i_ch_referencia = wa_aviso_aquav-ch_referencia ).

    data(r_gerou) = zcl_aviso_recebimento->criar_aviso_recebimento( i_particao_lote = abap_true ).
    data(r_retorno) = zcl_aviso_recebimento->get_retorno( ).

    if r_gerou eq abap_true.
      vr_documento = zcl_aviso_recebimento->get_nr_remessa( ).
      vr_vbeln     = vr_documento.
    else.
      clear: gw_saida_message.
      gw_saida_message-message = 'Houve um erro ao gerar o aviso!'.
      gw_saida_message-type = 'E'.
      append gw_saida_message to gt_saida_message.
      clear: gw_saida_message.
    endif.

    clear zcl_aviso_recebimento.
  endif.
endform.

form f_get_doc_fiscal_entrada  using p_zlest0060         type zlest0060
                            changing c_j_1bnfdoc         type j_1bnfdoc
                                     c_zmmt_ee_zgr       type zmmt_ee_zgr
                                     c_zmmt_ee_zgr_docs  type zmmt_ee_zgr_docs
                                     c_zsdt0001          type zsdt0001
                                     c_ent_found         type c.


  data: lva_candat_null type j_1bnfdoc-candat.

  ranges: lra_series for j_1bnfdoc-series.

  data: lva_series_aux type zlest0060-series.

  "Monta Range serie para filtro documento
  clear: lra_series[], lva_candat_null, c_ent_found.

  clear: c_j_1bnfdoc, c_zsdt0001, c_zmmt_ee_zgr,  c_zmmt_ee_zgr_docs.

  lva_series_aux = p_zlest0060-series.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = lva_series_aux
    importing
      output = lva_series_aux.

  append value #( sign = 'I' option = 'EQ' low = lva_series_aux ) to lra_series.

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = lva_series_aux
    importing
      output = lva_series_aux.

  append value #( sign = 'I' option = 'EQ' low = lva_series_aux ) to lra_series.

  if p_zlest0060-chave_nfe+0(1) eq 'F'.

    select single * from j_1bnfdoc into @data(lwa_j_1bnfdoc)
     where docdat eq @p_zlest0060-docdat
       and series in @lra_series
       and parid  eq @p_zlest0060-rm_codigo
       and branch eq @p_zlest0060-dt_codigo+6(4)
       and nfnum  eq @p_zlest0060-nfnum+3(6)
       and direct eq '1'
       and doctyp ne '5'
       and cancel ne 'X'
       and candat eq @lva_candat_null.

  else.

    select single * from j_1bnfdoc into lwa_j_1bnfdoc
     where docdat eq p_zlest0060-docdat
       and series in lra_series
       and parid  eq p_zlest0060-rm_codigo
       and branch eq p_zlest0060-dt_codigo+6(4)
       and nfenum eq p_zlest0060-nfnum
       and direct eq '1'
       and doctyp ne '5'
       and cancel ne 'X'
       and candat eq lva_candat_null.

  endif.

  if sy-subrc eq 0.
    select single *
      from zmmt_ee_zgr_docs into @data(lwa_zmmt_ee_zgr_docs)
     where docnum eq @lwa_j_1bnfdoc-docnum.

    if sy-subrc eq 0.
      select single *
        from zmmt_ee_zgr into @data(lwa_zmmt_ee_zgr)
       where obj_key eq @lwa_zmmt_ee_zgr_docs-obj_key.

      if sy-subrc eq 0 .
        c_j_1bnfdoc        = lwa_j_1bnfdoc.
        c_zmmt_ee_zgr      = lwa_zmmt_ee_zgr.
        c_zmmt_ee_zgr_docs = lwa_zmmt_ee_zgr_docs.
        c_ent_found        = abap_true.
        return.
      endif.
    endif.
  endif.

*------------------------------------------------------------------------------*
*  Buscar por Fluxo de Entrada Propria
*------------------------------------------------------------------------------*

  "Localizar Romaneio de Entrada
  select single * from zsdt0001 into @data(lwa_zsdt0001)
   where tp_movimento eq 'E'
     and docdat       eq @p_zlest0060-docdat
     and series       in @lra_series
     and parid        eq @p_zlest0060-rm_codigo
     and branch       eq @p_zlest0060-dt_codigo+6(4)
     and nfnum        eq @p_zlest0060-nfnum.

  if sy-subrc eq 0.

    select *
      from zmmt_ee_zgr into table @data(lit_zmm_ee_zgr)
     where ch_referencia eq @lwa_zsdt0001-ch_referencia.

    if lit_zmm_ee_zgr[] is not initial.

      select *
        from zmmt_ee_zgr_docs into table @data(lit_zmmt_ee_zgr_docs)
         for all entries in @lit_zmm_ee_zgr
       where obj_key eq @lit_zmm_ee_zgr-obj_key.

    endif.

    loop at lit_zmmt_ee_zgr_docs into lwa_zmmt_ee_zgr_docs where docnum is not initial.

      read table lit_zmm_ee_zgr into lwa_zmmt_ee_zgr with key obj_key = lwa_zmmt_ee_zgr_docs-obj_key.

      check sy-subrc eq 0.

      select single *
        from j_1bnfdoc into lwa_j_1bnfdoc
       where docnum   eq lwa_zmmt_ee_zgr_docs-docnum
         and candat   eq lva_candat_null
         and cancel   eq space
         and doctyp   in ('1','2','6').

      check ( sy-subrc eq 0 ) and ( lwa_j_1bnfdoc-entrad eq abap_true ). "Entrada Propria.

      c_j_1bnfdoc        = lwa_j_1bnfdoc.
      c_zmmt_ee_zgr      = lwa_zmmt_ee_zgr.
      c_zmmt_ee_zgr_docs = lwa_zmmt_ee_zgr_docs.
      c_zsdt0001         = lwa_zsdt0001.
      c_ent_found = abap_true.
      return.

    endloop.

  endif.



endform.
