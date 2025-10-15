*----------------------------------------------------------------------*
***INCLUDE LZSAPMZPLANCOMPF03 .
*----------------------------------------------------------------------*

type-pools: icon.


data: plan_gs_layout         type lvc_s_layo,
      plan_prim_recusa       type c length 1,
      plan_alv_recusa        type ref to cl_gui_alv_grid,
      plan_container_recusa  type ref to cl_gui_custom_container,
      plan_catalogo_recusa   type lvc_t_fcat,
      plan_scroll_col        type lvc_s_col,
      plan_scroll_row        type lvc_s_roid.

data: plan_catalogo_remetente_re   type lvc_t_fcat,
      plan_prim_remetente_re       type c length 1,
      plan_container_remetente_re  type ref to cl_gui_custom_container,
      plan_alv_remetente_re        type ref to cl_gui_alv_grid,
      c_alv_toolbar_remessa_re     type ref to cl_alv_grid_toolbar_manager.

data: it_znom_remetente_alv type table of zplac_vinc_produtor with header line,
      vg_a_qtd_recusar      type ntgew_ap.

constants: c_grid_color_c300 type c length 04 value 'C300',
           c_grid_color_c400 type c length 04 value 'C400'.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_RECUSAS_NF_RECUSA
*&---------------------------------------------------------------------*
*       Busca Recusas por nota fiscal de entrada de recusa
*----------------------------------------------------------------------*
form busca_recusas_nf_recusa  tables   it_vbrp structure vbrp
                                       it_zdoc_exp_recusa structure zdoc_exp_recusa
                              using    p_docnum_rec type j_1bdocnum.

  data: wa_j_1bnflin      type j_1bnflin,
        wa_j_1bnfdoc      type j_1bnfdoc,
        wa_j_1bnflin_ex   type j_1bnflin,
        wa_j_1bnfdoc_ex   type j_1bnfdoc,
        wa_j_1bnfe_active type j_1bnfe_active,
        vg_fatura         type vbeln_vf,
        vg_posnr          type posnr_vf,
        wa_vbrp           type vbrp,
        wa_vbrk           type vbrk,
        wa_vbak           type vbak,
        wa_vbap           type vbap,
        wa_lips_ex        type lips,
        wa_vbrp_ex        type vbrp,
        vg_refkey         type j_1brefkey,
        wa_znom_prog_reme type znom_prog_reme,
        wa_zdoc_exp       type zdoc_exp,
        wa_zdoc_exp_recusa type zdoc_exp_recusa,
        wa_zdoc_memo_nf_exp type zdoc_memo_nf_exp,
        wa_kna1            type kna1.

  select single * into wa_j_1bnflin
    from j_1bnflin
   where docnum eq p_docnum_rec.

  check sy-subrc is initial.

  select single * into wa_j_1bnfdoc
    from j_1bnfdoc
   where docnum eq p_docnum_rec.

  check sy-subrc is initial.

  if wa_j_1bnfdoc-direct ne '1'.
    message e035 with p_docnum_rec raising direcao_nota.
  endif.

  if ( wa_j_1bnfdoc-doctyp ne '1' ) and ( wa_j_1bnfdoc-doctyp ne '6' ).
    message e036 with p_docnum_rec raising tipo_nota.
  endif.

  if wa_j_1bnflin-reftyp ne 'BI'.
    message e034 raising sem_fatura_ref.
  endif.

  vg_fatura = wa_j_1bnflin-refkey(10).
  vg_posnr  = wa_j_1bnflin-refitm.

  select single * into wa_vbrp
    from vbrp
   where vbeln eq vg_fatura
     and posnr eq vg_posnr.

  check sy-subrc is initial.

  "Verificar nota já incluida dados de recusa
  select single * into wa_zdoc_exp_recusa
    from zdoc_exp_recusa
   where vbeln_ft_rec eq wa_vbrp-vbeln
     and posnr_ft_rec eq wa_vbrp-posnr.

  if sy-subrc is initial.
    message e049 with wa_vbrp-vbeln raising fatura_recusada.
  endif.

  "Fatura da Nota Fiscal de Retorno/Devolução
  select single * into wa_vbrk
    from vbrk
   where vbeln eq vg_fatura.

  check sy-subrc is initial.

  if wa_vbrk-fksto eq 'X'.
    message e037 with vg_fatura raising fatura_estornada.
  endif.

  if not wa_vbrk-sfakn is initial.
    message e038 with vg_fatura wa_vbrk-sfakn raising fatura_estornada.
  endif.

  "Ordem de venda da Fatura da Nota Fiscal de Retorno/Devolução
  select single * into wa_vbak
    from vbak
   where vbeln eq wa_vbrp-aubel.

  check sy-subrc is initial.

  if ( wa_vbak-vbtyp ne 'H' ) or ( wa_vbak-vgbel is initial ).
    message e039 with wa_vbrp-aubel raising ordem_errada.
  endif.

  "Item da Ordem de Venda da Fatura da Nota Fiscal de Retorno/Devolução
  select single * into wa_vbap
    from vbap
   where vbeln eq wa_vbrp-aubel
     and posnr eq wa_vbrp-aupos.

  check sy-subrc is initial.

  vg_fatura = wa_vbap-vgbel.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = vg_fatura
    importing
      output = vg_fatura.

  vg_refkey = vg_fatura(10).

  select single * into wa_j_1bnflin_ex
    from j_1bnflin
   where reftyp eq 'BI'
     and refkey eq vg_refkey
     and refitm eq wa_vbap-vgpos.

  if not sy-subrc is initial.
    message e040 raising nota_fiscal_exp.
  endif.

  select single * into wa_j_1bnfdoc_ex
    from j_1bnfdoc
   where docnum eq wa_j_1bnflin_ex-docnum.

  if ( not sy-subrc is initial ) or ( not wa_j_1bnfdoc_ex-cancel is initial ).
    message e040 raising nota_fiscal_exp.
  endif.

  if wa_j_1bnfdoc_ex-nfe eq 'X'.

    select single * into wa_j_1bnfe_active
      from j_1bnfe_active
     where docnum eq wa_j_1bnflin_ex-docnum.

    if ( not sy-subrc is initial ) or
       ( not wa_j_1bnfe_active-cancel is initial ) or
       ( wa_j_1bnfe_active-docsta ne '1' ).
      message e040 raising nota_fiscal_exp.
    endif.

  endif.

  select single * into wa_zdoc_memo_nf_exp
    from zdoc_memo_nf_exp
   where docnum eq wa_j_1bnflin_ex-docnum.

  if sy-subrc is initial.
    message e050 with wa_j_1bnflin_ex-docnum raising nota_fiscal_compro.
  endif.

  select single * into wa_vbrp_ex
    from vbrp
   where vbeln eq wa_vbap-vgbel
     and posnr eq wa_vbap-vgpos.

  check sy-subrc is initial.

  select single * into wa_znom_prog_reme
    from znom_prog_reme
   where id_remessa eq wa_vbrp_ex-vgbel.

  if not sy-subrc is initial.
    message e041 with wa_vbrp_ex-vgbel raising nao_planejada.
  endif.

  select single * into wa_zdoc_exp
    from zdoc_exp
   where vbeln            eq wa_znom_prog_reme-id_remessa
     and id_registro_expo eq wa_znom_prog_reme-id_registro_expo.

  if not sy-subrc is initial.
    message e041 with wa_vbrp_ex-vgbel raising nao_planejada.
  endif.

  select single * into wa_lips_ex
    from lips
   where vbeln eq wa_vbrp_ex-vgbel
     and posnr eq wa_vbrp_ex-vgpos.

*"-- Recusa/Devolução --------------------------------------------------------------------------------
*    "Documento de vendas recusa/devolução
*    wa_vbap-vbeln.
*    "Item do documento de vendas recusa/devolução
*    wa_vbap-posnr.
*    "Volume da Ordem de Recusa/Devolução Un/Qtd
*    wa_vbap-GEWEI wa_vbap-NTGEW
*    "Fatura da Recusa/Devolução
*    wa_vbrp-vbeln.
*    "Item da Fatura da Recusa/Devolução
*    wa_vbrp-posnr.
*    "Nota fiscal de Recusa/Devolução
*    p_docnum_rec
*"----------------------------------------------------------------------------------------------------

*"-- Exportaçção Efetiva -----------------------------------------------------------------------------
*    "Fatura da Exportação
*    wa_vbap-vgbel.
*    "Item da  Fatura da Exportação
*    wa_vbap-vgpos.
*    "Nota Fiscal da Exportação
*    wa_j_1bnflin_ex-docnum.
*    "item Nota Fiscal da Exportação
*    wa_j_1bnflin_ex-itmnum.
*    "Remessa da Exportação
*    wa_vbrp_ex-vgbel.
*    "item Remessa da Exportação
*    wa_vbrp_ex-vgpos.
*    "Volume da Remessa Exportação Un/Qtd
*    wa_lips_ex-GEWEI wa_lips_ex-NTGEW
*    "Documento de vendas exportação
*    wa_vbrp_ex-VBELV
*    "item do Documento de vendas exportação
*    wa_vbrp_ex-POSNV
*"----------------------------------------------------------------------------------------------------

*"-- Vinculo telas de controle de compromisso de exportação ------------------------------------------
*    "Identificador de exportação
*    wa_zdoc_exp-id_doc_exp
*"----------------------------------------------------------------------------------------------------

  "Fatura da Recusa/Devolução
  append wa_vbrp to it_vbrp.

  wa_zdoc_exp_recusa_tela-id_doc_exp   = wa_zdoc_exp-id_doc_exp.

  if wa_lips_ex-gewei ne 'KG'.
    call function 'ME_CONVERSION_MEINS'
      exporting
        i_matnr             = wa_lips_ex-matnr
        i_mein1             = wa_lips_ex-gewei
        i_meins             = 'KG'
        i_menge             = wa_lips_ex-ntgew
      importing
        menge               = wa_lips_ex-ntgew
      exceptions
        error_in_conversion = 1
        no_success          = 2
        others              = 3.
    if not sy-subrc is initial.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      wa_lips_ex-gewei = 'KG'.
    endif.
  endif.

  if wa_vbap-gewei ne 'KG'.
    call function 'ME_CONVERSION_MEINS'
      exporting
        i_matnr             = wa_vbap-matnr
        i_mein1             = wa_vbap-gewei
        i_meins             = 'KG'
        i_menge             = wa_vbap-ntgew
      importing
        menge               = wa_vbap-ntgew
      exceptions
        error_in_conversion = 1
        no_success          = 2
        others              = 3.
    if not sy-subrc is initial.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      wa_vbap-gewei = 'KG'.
    endif.
  endif.

  select single * into wa_kna1
    from kna1
   where kunnr eq wa_vbak-kunnr.

  wa_zdoc_exp_recusa_tela-icone          = icon_storno.
  wa_zdoc_exp_recusa_tela-id_doc_exp     = wa_zdoc_exp-id_doc_exp.
  wa_zdoc_exp_recusa_tela-vbeln_re_exp   = wa_vbrp_ex-vgbel.
  wa_zdoc_exp_recusa_tela-posnr_re_exp   = wa_vbrp_ex-vgpos.
  wa_zdoc_exp_recusa_tela-docnum_exp     = wa_j_1bnflin_ex-docnum.
  wa_zdoc_exp_recusa_tela-itmnum_exp     = wa_j_1bnflin_ex-itmnum.
  wa_zdoc_exp_recusa_tela-vbeln_ft_exp   = wa_vbap-vgbel.
  wa_zdoc_exp_recusa_tela-posnr_ft_exp   = wa_vbap-vgpos.
  wa_zdoc_exp_recusa_tela-vbeln_ov_exp   = wa_vbrp_ex-vbelv.
  wa_zdoc_exp_recusa_tela-posnr_ov_exp   = wa_vbrp_ex-posnv.
  wa_zdoc_exp_recusa_tela-id_und_exp     = wa_lips_ex-gewei.
  wa_zdoc_exp_recusa_tela-nm_qtd_exp     = wa_lips_ex-ntgew.
  wa_zdoc_exp_recusa_tela-vbeln_ov_rec   = wa_vbap-vbeln.
  wa_zdoc_exp_recusa_tela-posnr_ov_rec   = wa_vbap-posnr.
  wa_zdoc_exp_recusa_tela-vbeln_ft_rec   = wa_vbrp-vbeln.
  wa_zdoc_exp_recusa_tela-posnr_ft_rec   = wa_vbrp-posnr.
  wa_zdoc_exp_recusa_tela-data_ov_rec    = wa_vbak-audat.
  wa_zdoc_exp_recusa_tela-cliente_ov_rec = wa_kna1-kunnr.
  wa_zdoc_exp_recusa_tela-nome_cl_ov_rec = wa_kna1-name1.
  wa_zdoc_exp_recusa_tela-id_und_rec     = wa_vbap-gewei.
  wa_zdoc_exp_recusa_tela-nm_qtd_rec     = wa_vbap-ntgew.
  wa_zdoc_exp_recusa_tela-werks_ov_rec   = wa_vbap-werks.
  append wa_zdoc_exp_recusa_tela to it_zdoc_exp_recusa_tela.

  wa_zdoc_exp_recusa-id_doc_exp    = wa_zdoc_exp-id_doc_exp.
  wa_zdoc_exp_recusa-vbeln_re_exp  = wa_vbrp_ex-vgbel.
  wa_zdoc_exp_recusa-posnr_re_exp  = wa_vbrp_ex-vgpos.
  wa_zdoc_exp_recusa-vbeln_ov_rec  = wa_vbap-vbeln.
  wa_zdoc_exp_recusa-posnr_ov_rec  = wa_vbap-posnr.
  wa_zdoc_exp_recusa-vbeln_ft_rec  = wa_vbrp-vbeln.
  wa_zdoc_exp_recusa-posnr_ft_rec  = wa_vbrp-posnr.
  wa_zdoc_exp_recusa-id_unidade    = wa_vbap-gewei.
  wa_zdoc_exp_recusa-nm_quantidade = wa_vbap-ntgew.
  append wa_zdoc_exp_recusa to it_zdoc_exp_recusa.

endform.                    " BUSCA_RECUSAS_NF_RECUSA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_RECUSAS_NF_EXPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form busca_recusas_nf_exportacao tables it_vbrp structure vbrp
                                        it_zdoc_exp_recusa structure zdoc_exp_recusa
                                 using  p_docnum_exp type j_1bdocnum.

  data: wa_j_1bnflin_ex    type j_1bnflin,
        wa_j_1bnfdoc_ex    type j_1bnfdoc,
        wa_j_1bnfe_active  type j_1bnfe_active,
        wa_vbrp_ex         type vbrp,
        wa_lips_ex         type lips,
        it_vbap            type table of vbap with header line,
        it_vbak            type table of vbak with header line,
        it_kna1            type table of kna1 with header line,
        it_vbrp_rec        type table of vbrp with header line,
        wa_znom_prog_reme  type znom_prog_reme,
        wa_zdoc_exp        type zdoc_exp,
        wa_zdoc_exp_recusa type zdoc_exp_recusa,
        wa_zdoc_memo_nf_exp type zdoc_memo_nf_exp.

  select single * into wa_j_1bnflin_ex
    from j_1bnflin
   where docnum eq p_docnum_exp.

  if ( not sy-subrc is initial ) or ( wa_j_1bnflin_ex-reftyp ne 'BI' ).
    message e040 raising nota_fiscal_exp.
  endif.

  select single * into wa_j_1bnfdoc_ex
    from j_1bnfdoc
   where docnum eq wa_j_1bnflin_ex-docnum.

  if ( not sy-subrc is initial ) or ( not wa_j_1bnfdoc_ex-cancel is initial ).
    message e040 raising nota_fiscal_exp.
  endif.

  if wa_j_1bnfdoc_ex-nfe eq 'X'.
    select single * into wa_j_1bnfe_active
      from j_1bnfe_active
     where docnum eq wa_j_1bnflin_ex-docnum.

    if ( not sy-subrc is initial ) or
       ( not wa_j_1bnfe_active-cancel is initial ) or
       ( wa_j_1bnfe_active-docsta ne '1' ).
      message e040 raising nota_fiscal_exp.
    endif.
  endif.

  select single * into wa_zdoc_memo_nf_exp
    from zdoc_memo_nf_exp
   where docnum eq wa_j_1bnflin_ex-docnum.

  if sy-subrc is initial.
    message e050 with wa_j_1bnflin_ex-docnum raising nota_fiscal_compro.
  endif.

  wa_vbrp_ex-vbeln = wa_j_1bnflin_ex-refkey(10).
  wa_vbrp_ex-posnr = wa_j_1bnflin_ex-refitm.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = wa_vbrp_ex-vbeln
    importing
      output = wa_vbrp_ex-vbeln.

  select single * into wa_vbrp_ex
    from vbrp
   where vbeln eq wa_vbrp_ex-vbeln
     and posnr eq wa_vbrp_ex-posnr.

  check sy-subrc is initial.

  select single * into wa_lips_ex
    from lips
   where vbeln eq wa_vbrp_ex-vgbel
     and posnr eq wa_vbrp_ex-vgpos.

  check sy-subrc is initial.

  select single * into wa_znom_prog_reme
    from znom_prog_reme
   where id_remessa eq wa_vbrp_ex-vgbel.

  if not sy-subrc is initial.
    message e041 with wa_vbrp_ex-vgbel raising nao_planejada.
  endif.

  select single * into wa_zdoc_exp
    from zdoc_exp
   where vbeln            eq wa_znom_prog_reme-id_remessa
     and id_registro_expo eq wa_znom_prog_reme-id_registro_expo.

  if not sy-subrc is initial.
    message e041 with wa_vbrp_ex-vgbel raising nao_planejada.
  endif.

  select * into table it_vbap
    from vbap as p
   where p~vgbel eq wa_vbrp_ex-vbeln
     and p~vgpos eq wa_vbrp_ex-posnr
     and exists ( select * from vbak as k where k~vbeln eq p~vbeln and k~vbtyp eq 'H' ).

  if not sy-subrc is initial.
    message e042 raising ordem_errada.
  endif.

  select * into table it_vbrp_rec
    from vbrp as p
     for all entries in it_vbap
   where p~aubel eq it_vbap-vbeln
     and p~aupos eq it_vbap-posnr
     and not exists ( select *
                        from zdoc_exp_recusa as r
                       where r~vbeln_ft_rec eq p~vbeln
                         and r~posnr_ft_rec eq p~posnr ) AND DRAFT = SPACE .

  if not sy-subrc is initial.
    message e042 raising ordem_errada.
  endif.

  select * into table it_vbak
    from vbak
     for all entries in it_vbap
   where vbeln eq it_vbap-vbeln.

  select * into table it_kna1
    from kna1
    for all entries in it_vbak
   where kunnr eq it_vbak-kunnr.

  wa_zdoc_exp_recusa_tela-icone        = icon_storno.
  wa_zdoc_exp_recusa_tela-id_doc_exp   = wa_zdoc_exp-id_doc_exp.
  wa_zdoc_exp_recusa_tela-docnum_exp   = wa_j_1bnflin_ex-docnum.
  wa_zdoc_exp_recusa_tela-itmnum_exp   = wa_j_1bnflin_ex-itmnum.
  wa_zdoc_exp_recusa_tela-vbeln_ft_exp = wa_vbrp_ex-vbeln.
  wa_zdoc_exp_recusa_tela-posnr_ft_exp = wa_vbrp_ex-posnr.
  wa_zdoc_exp_recusa_tela-vbeln_ov_exp = wa_vbrp_ex-aubel.
  wa_zdoc_exp_recusa_tela-posnr_ov_exp = wa_vbrp_ex-aupos.
  wa_zdoc_exp_recusa_tela-vbeln_re_exp = wa_vbrp_ex-vgbel.
  wa_zdoc_exp_recusa_tela-posnr_re_exp = wa_vbrp_ex-vgpos.
  wa_zdoc_exp_recusa_tela-id_und_exp   = wa_lips_ex-gewei.
  wa_zdoc_exp_recusa_tela-nm_qtd_exp   = wa_lips_ex-ntgew.
  wa_zdoc_exp_recusa-id_doc_exp        = wa_zdoc_exp-id_doc_exp.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = wa_vbrp_ex-vgbel
    importing
      output = wa_zdoc_exp_recusa-vbeln_re_exp.

  wa_zdoc_exp_recusa-posnr_re_exp      = wa_vbrp_ex-vgpos.

  loop at it_vbap.

    loop at it_vbrp_rec where aubel eq it_vbap-vbeln
                          and aupos eq it_vbap-posnr.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = it_vbap-vbeln
        importing
          output = wa_zdoc_exp_recusa_tela-vbeln_ov_rec.

      wa_zdoc_exp_recusa_tela-posnr_ov_rec = it_vbap-posnr.
      wa_zdoc_exp_recusa_tela-vbeln_ft_rec = it_vbrp_rec-vbeln.
      wa_zdoc_exp_recusa_tela-posnr_ft_rec = it_vbrp_rec-posnr.
      wa_zdoc_exp_recusa_tela-id_und_rec   = it_vbap-gewei.
      wa_zdoc_exp_recusa_tela-nm_qtd_rec   = it_vbap-ntgew.
      wa_zdoc_exp_recusa_tela-werks_ov_rec = it_vbap-werks.

      read table it_vbak with key vbeln = it_vbap-vbeln.
      if sy-subrc is initial.

        wa_zdoc_exp_recusa_tela-data_ov_rec  = it_vbak-audat.

        read table it_kna1 with key kunnr = it_vbak-kunnr.
        if sy-subrc is initial.
          wa_zdoc_exp_recusa_tela-cliente_ov_rec = it_kna1-kunnr.
          wa_zdoc_exp_recusa_tela-nome_cl_ov_rec = it_kna1-name1.
        endif.

      endif.

      append wa_zdoc_exp_recusa_tela to it_zdoc_exp_recusa_tela.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = it_vbap-vbeln
        importing
          output = wa_zdoc_exp_recusa-vbeln_ov_rec.

      wa_zdoc_exp_recusa-posnr_ov_rec      = it_vbap-posnr.
      wa_zdoc_exp_recusa-vbeln_ft_rec      = it_vbrp_rec-vbeln.
      wa_zdoc_exp_recusa-posnr_ft_rec      = it_vbrp_rec-posnr.
      wa_zdoc_exp_recusa-id_unidade        = it_vbap-gewei.
      wa_zdoc_exp_recusa-nm_quantidade     = it_vbap-ntgew.
      append wa_zdoc_exp_recusa to it_zdoc_exp_recusa.
    endloop.

  endloop.

endform.                    " BUSCA_RECUSAS_NF_EXPORTACAO

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1001 input.

  data: vg_verifica type sy-subrc.

  case ok_code_1001.
    when ok_sel.
      perform verifica_selecao_recusa using vg_verifica.
      if vg_verifica is initial.
        leave to screen 0.
      endif.
  endcase.

endmodule.                 " USER_COMMAND_1001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1001 output.

  set pf-status 'PF1001'.
  set titlebar 'TL1001'.

  perform plan_cria_recusas_alv.

endmodule.                 " STATUS_1001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1001_exit input.
  leave to screen 0.
endmodule.                 " USER_COMMAND_1001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_RECUSAS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form plan_cria_recusas_alv .

  constants: tabela_recusa_re type string value 'IT_ZDOC_EXP_RECUSA_TELA'.

  data: text_n000 type c length 50 value 'O.V. Recusa',
        text_n001 type c length 50 value 'Unid.',
        text_n002 type c length 50 value 'Quantidade',
        text_n003 type c length 50 value 'Id.Cliente',
        text_n004 type c length 50 value 'Nome Cliente',
        text_n005 type c length 50 value 'Centro'.

  if plan_prim_recusa is initial.

    create object plan_container_recusa
      exporting
        container_name = 'CTN_RECUSA'.

    create object plan_alv_recusa
      exporting
        i_parent = plan_container_recusa.

    perform z_estrutura_fieldcat tables plan_catalogo_recusa using:
        tabela_recusa_re 'ICONE'          ''        'X'   01 03 space space space 'X'   space space space,
        tabela_recusa_re 'WERKS_OV_REC'   text_n005 space 02 04 space space space space space space space,
        tabela_recusa_re 'VBELN_OV_REC'   text_n000 space 03 10 space space space space space space space,
        tabela_recusa_re 'CLIENTE_OV_REC' text_n003 space 04 10 space space space space space space space,
        tabela_recusa_re 'NOME_CL_OV_REC' text_n004 space 05 30 space space space space space space space,
        tabela_recusa_re 'ID_UND_REC'     text_n001 space 06 04 space space space space space space space,
        tabela_recusa_re 'NM_QTD_REC'     text_n002 space 07 15 space space space space space c_grid_color_c300 space.

    clear: plan_gs_layout.
    plan_gs_layout-zebra      = 'X'.
    plan_gs_layout-sel_mode   = space.

    call method plan_alv_recusa->set_table_for_first_display
      exporting
        i_default       = space
        is_layout       = plan_gs_layout
      changing
        it_fieldcatalog = plan_catalogo_recusa
        it_outtab       = it_zdoc_exp_recusa_tela[].

    plan_prim_recusa = 'X'.
  endif.

  call method plan_alv_recusa->refresh_table_display.

  call method plan_alv_recusa->set_scroll_info_via_id
    exporting
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

endform.                    " PLAN_CRIA_RECUSAS_ALV


*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_RECUSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_VERIFICA  text
*----------------------------------------------------------------------*
form verifica_selecao_recusa  using  p_verifica type sy-subrc.

  data: it_selected_rows type lvc_t_row,
        wa_selected_rows type lvc_s_row.

  call method plan_alv_recusa->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  loop at it_selected_rows into wa_selected_rows.
    read table it_zdoc_exp_recusa_tela into wa_zdoc_exp_recusa_tela index wa_selected_rows-index.
  endloop.

  if not wa_zdoc_exp_recusa_tela is initial.
    p_verifica = 0.
  else.
    p_verifica = 1.
  endif.

endform.                    " VERIFICA_SELECAO_RECUSA

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTAS_DA_EXPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form consulta_notas_da_exportacao  tables it_zdoc_nf_produtor structure zdoc_nf_produtor
                                   using  p_recusa_tela    type zdoc_exp_recusa_tela
                                          p_znom_remetente type znom_remetente.

  data: wa_zdoc_exp         type zdoc_exp,
        it_zdoc_exp_rec_nf  type table of zdoc_exp_rec_nf with header line,
        it_zdoc_exp_recusa  type table of zdoc_exp_recusa with header line,
        wa_znom_prog_reme   type znom_prog_reme,
        wa_zdoc_nf_produtor type zdoc_nf_produtor.

  "Notas de Produtor Vinculada a Exportação
  select * into table it_zdoc_nf_produtor
    from zdoc_nf_produtor
   where vbeln eq p_recusa_tela-vbeln_re_exp.

  "Busca notas de produtor recusadas/devolvidas dentro do mesmo processo de exportação
  select single * into wa_zdoc_exp
    from zdoc_exp
   where vbeln eq p_recusa_tela-vbeln_re_exp.

  if not wa_zdoc_exp is initial.

    "Para Parte Empresa recusada
    select * into table it_zdoc_exp_recusa
      from zdoc_exp_recusa
     where vbeln_re_exp eq wa_zdoc_exp-vbeln.

    "Para Parte Produtor recusada
    select * into table it_zdoc_exp_rec_nf
      from zdoc_exp_rec_nf
     where vbeln_re_exp eq wa_zdoc_exp-vbeln.

    if ( it_zdoc_exp_rec_nf[] is initial ) and ( it_zdoc_nf_produtor[] is initial ).
      "Busca parte Empresa Vinculada
      select single * into wa_znom_prog_reme
        from znom_prog_reme
       where id_remessa eq wa_zdoc_exp-vbeln.

      if sy-subrc is initial.

        select single * into p_znom_remetente
          from znom_remetente
         where id_nomeacao_tran eq wa_znom_prog_reme-id_nomeacao_tran
           and id_empresa       eq wa_znom_prog_reme-id_empresa
           and id_filial        eq wa_znom_prog_reme-id_filial
           and id_material      eq wa_znom_prog_reme-id_material
           and id_remetente     eq space.

        if sy-subrc is initial.
          "Parte Empresa possivel a ser recusada/devolvida do processo
          loop at it_zdoc_exp_recusa.
            p_znom_remetente-nr_parte_empresa = p_znom_remetente-nr_parte_empresa - it_zdoc_exp_recusa-nm_quantidade.
          endloop.
        endif.

      endif.
    else.
      "Busca parte Produtores
      loop at it_zdoc_exp_rec_nf.
        read table it_zdoc_nf_produtor
              into wa_zdoc_nf_produtor
          with key docnum_prod = it_zdoc_exp_rec_nf-docnum_prod
                   itmnum_prod = it_zdoc_exp_rec_nf-itmnum_prod.
        if sy-subrc is initial.
          wa_zdoc_nf_produtor-menge = wa_zdoc_nf_produtor-menge - it_zdoc_exp_rec_nf-menge.
          modify it_zdoc_nf_produtor from wa_zdoc_nf_produtor index sy-tabix transporting menge.
        endif.
      endloop.
    endif.

  endif.

endform.                    " CONSULTA_NOTAS_DA_EXPORTACAO


*&---------------------------------------------------------------------*
*&      Form  VINCULA_NF_PRODUTOR_RECUSA
*&---------------------------------------------------------------------*
*       Vincula Notas Livres em Recusa
*----------------------------------------------------------------------*
form vincula_nf_produtor_recusa  tables   p_nf_produtor structure zdoc_nf_produtor
                                          p_nf_recusa   structure zdoc_exp_rec_nf
                                 using    p_recusa_tela type zdoc_exp_recusa_tela.

  data: it_znom_reme_notas    type table of znom_reme_notas with header line,
        it_znom_remetente     type table of znom_remetente with header line,
        it_znom_remetente_aux type table of znom_remetente with header line,
        it_lfa1               type table of lfa1 with header line,
        wa_nf_produtor        type zdoc_nf_produtor,
        wa_nf_recusa          type zdoc_exp_rec_nf,
        wa_adrc               type adrc,
        cidade                type j_1btxjurt,
        text_cliente_cgc      type c length 18,
        text_cliente_cpf      type c length 14,
        vg_qtd_recusar        type ntgew_ap.

  vg_qtd_recusar   = p_recusa_tela-nm_qtd_rec.
  vg_a_qtd_recusar = vg_qtd_recusar.

  "Selecionar Grupo Planejamento
  select SINGLE * from lips INTO @DATA(lwa_lips_exp) WHERE vbeln eq @p_recusa_tela-vbeln_re_exp.
  CHECK sy-subrc is INITIAL.

  select SINGLE * from zsdt_export INTO @DATA(lwa_zsdt_export) where ordem eq @lwa_lips_exp-vgbel.
  CHECK sy-subrc is INITIAL.

  select SINGLE * from znom_remetente INTO @DATA(lwa_znom_remetente) where docnum_rt eq @lwa_zsdt_export-docnum.
  CHECK sy-subrc is INITIAL.

  "Selecionar todos as notas vinculadas na exportação
  select * into table it_znom_reme_notas
    from znom_reme_notas as n
     for all entries in p_nf_produtor
   where n~docnum      eq p_nf_produtor-docnum_prod
     and n~itmnum      eq p_nf_produtor-itmnum_prod
     AND n~grp_retorno eq lwa_znom_remetente-grp_retorno
     and exists ( select *
                    from znom_prog_reme as r
                   where r~id_nomeacao_tran eq n~id_nomeacao_tran
                     and r~id_empresa       eq n~id_empresa
                     and r~id_filial        eq n~id_filial
                     and r~id_material      eq n~id_material
                     and r~id_remessa       eq p_recusa_tela-vbeln_re_exp ).

  check sy-subrc is initial.

  "Seleciona todos os remetentes
  select * into table it_znom_remetente
    from znom_remetente
     for all entries in it_znom_reme_notas
   where id_nomeacao_tran  eq it_znom_reme_notas-id_nomeacao_tran
     and id_empresa        eq it_znom_reme_notas-id_empresa
     and id_filial         eq it_znom_reme_notas-id_filial
     and id_material       eq it_znom_reme_notas-id_material
     and id_remetente      eq it_znom_reme_notas-id_remetente
     and grp_retorno       eq it_znom_reme_notas-grp_retorno.

  check sy-subrc is initial.

  move it_znom_remetente[] to it_znom_remetente_aux[].
  sort it_znom_remetente_aux by id_remetente.
  delete adjacent duplicates from it_znom_remetente_aux comparing id_remetente.
  delete it_znom_remetente_aux where id_remetente eq space.

  if not it_znom_remetente_aux[] is initial.
    select * into table it_lfa1
      from lfa1
       for all entries in it_znom_remetente_aux
     where lifnr eq it_znom_remetente_aux-id_remetente.
  endif.

  clear: it_znom_remetente_alv[].

  loop at it_znom_remetente.

    clear: it_znom_remetente_alv.
    move-corresponding it_znom_remetente to it_znom_remetente_alv.

    it_znom_remetente_alv-icone = icon_customer.

    read table it_lfa1 with key lifnr = it_znom_remetente-id_remetente.
    if sy-subrc is initial.
      it_znom_remetente_alv-name1 = it_lfa1-name1.
      if ( it_lfa1-stkzn is initial ) and ( it_lfa1-stcd1 is not initial ).
        call function 'CONVERSION_EXIT_CGCBR_OUTPUT'
          exporting
            input  = it_lfa1-stcd1
          importing
            output = text_cliente_cgc.
        it_znom_remetente_alv-stcd1 = text_cliente_cgc.
      elseif ( it_lfa1-stkzn is not initial ) and ( it_lfa1-stcd2 is not initial ).
        call function 'CONVERSION_EXIT_CPFBR_OUTPUT'
          exporting
            input  = it_lfa1-stcd2
          importing
            output = text_cliente_cpf.
        it_znom_remetente_alv-stcd1 = text_cliente_cpf.
      endif.

      select single * into wa_adrc
        from adrc
       where addrnumber eq it_lfa1-adrnr.

      if sy-subrc is initial.
        it_znom_remetente_alv-uf = wa_adrc-taxjurcode(3).

        select single * into cidade
          from j_1btxjurt
         where spras      = wa_adrc-langu
           and country    = wa_adrc-country
           and taxjurcode = wa_adrc-taxjurcode.

        if sy-subrc is initial.
          it_znom_remetente_alv-munic = cidade-text.
        endif.
      endif.
    endif.

    loop at it_znom_reme_notas where id_remetente = it_znom_remetente-id_remetente.
      loop at p_nf_produtor into wa_nf_produtor where docnum_prod eq it_znom_reme_notas-docnum
                                                  and itmnum_prod eq it_znom_reme_notas-itmnum.
        it_znom_remetente_alv-nr_qtd_possivel = it_znom_remetente_alv-nr_qtd_possivel + wa_nf_produtor-menge.
      endloop.
      if it_znom_remetente_alv-nr_qtd_possivel gt vg_qtd_recusar.
        it_znom_remetente_alv-nr_qtd_vincular = vg_qtd_recusar.
      else.
        it_znom_remetente_alv-nr_qtd_vincular = it_znom_remetente_alv-nr_qtd_possivel.
      endif.
      vg_qtd_recusar = vg_qtd_recusar - it_znom_remetente_alv-nr_qtd_vincular.
    endloop.

    append it_znom_remetente_alv.
  endloop.

  call screen 1002 starting at 07 05 ending at 100 15.

  "Vincular notas livres de remetente até o volume a recusar do mesmo
  if not it_znom_remetente_alv[] is initial.
    "Notas do remtemte
    loop at it_znom_remetente_alv.
      loop at it_znom_reme_notas where id_remetente eq it_znom_remetente_alv-id_remetente.
        loop at p_nf_produtor into wa_nf_produtor where docnum_prod eq it_znom_reme_notas-docnum
                                                    and itmnum_prod eq it_znom_reme_notas-itmnum.
          if it_znom_remetente_alv-nr_qtd_vincular gt 0.
            wa_nf_recusa-id_doc_exp   = p_recusa_tela-id_doc_exp.

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = p_recusa_tela-vbeln_re_exp
              importing
                output = wa_nf_recusa-vbeln_re_exp.

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = p_recusa_tela-vbeln_ov_rec
              importing
                output = wa_nf_recusa-vbeln_ov_rec.

            wa_nf_recusa-posnr_re_exp = p_recusa_tela-posnr_re_exp.
            wa_nf_recusa-posnr_ov_rec = p_recusa_tela-posnr_ov_rec.

            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wa_nf_produtor-docnum_prod
              importing
                output = wa_nf_recusa-docnum_prod.

            wa_nf_recusa-itmnum_prod  = wa_nf_produtor-itmnum_prod.
            wa_nf_recusa-id_unidade   = it_znom_remetente_alv-id_unidade.
            if wa_nf_produtor-menge gt it_znom_remetente_alv-nr_qtd_vincular.
              wa_nf_recusa-menge = it_znom_remetente_alv-nr_qtd_vincular.
            else.
              wa_nf_recusa-menge = wa_nf_produtor-menge.
            endif.
            wa_nf_recusa-id_remetente = it_znom_reme_notas-id_remetente.
            append wa_nf_recusa to p_nf_recusa.
            it_znom_remetente_alv-nr_qtd_vincular = it_znom_remetente_alv-nr_qtd_vincular - wa_nf_recusa-menge.
          endif.
        endloop.
      endloop.
    endloop.
  endif.

endform.                    " VINCULA_NF_PRODUTOR_RECUSA

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_PRODUTORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form excluir_produtores .

  data: it_selected_rows  type lvc_t_row,
        wa_selected_rows  type lvc_s_row,
        it_prod_remessa_c type table of zplac_vinc_produtor with header line.

  clear: it_prod_remessa_c[].

  call method plan_alv_remetente_re->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  move it_znom_remetente_alv[] to it_prod_remessa_c[].

  loop at it_selected_rows into wa_selected_rows.
    read table it_prod_remessa_c index wa_selected_rows-index.
    if sy-subrc is initial.
      delete it_znom_remetente_alv index sy-tabix.
    endif.
  endloop.

endform.                    " EXCLUIR_PRODUTORES

*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1002 output.

  set pf-status 'PF1001'.
  set titlebar 'TL1002'.

  constants: tabela_remetente_re type string value 'IT_ZNOM_REMETENTE_ALV'.

  data: text_n000 type c length 50 value 'Remetente',
        text_n001 type c length 50 value 'Remetente',
        text_n002 type c length 50 value 'Qtd. Disponível',
        text_n003 type c length 50 value 'CNPJ/CPF',
        text_n004 type c length 50 value 'UF',
        text_n006 type c length 50 value 'Qtd. Vinculada',
        it_exclude_fcode type ui_functions,
        wa_exclude_fcode like line of it_exclude_fcode.

  if plan_prim_remetente_re is initial.

    create object plan_container_remetente_re
      exporting
        container_name = 'CTN_REMETENTE'.

    call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      importing
        e_grid = plan_alv_remetente_re.

    create object plan_alv_remetente_re
      exporting
        i_parent = plan_container_remetente_re.

    perform z_estrutura_fieldcat tables plan_catalogo_remetente_re using:
        tabela_remetente_re 'ICONE'           text_n000 'X'   01 03 space space space 'X'   space space             space,
        tabela_remetente_re 'NR_QTD_POSSIVEL' text_n002 space 02 15 space space space space space c_grid_color_c400 space,
        tabela_remetente_re 'NR_QTD_VINCULAR' text_n006 space 03 15 space space space space space c_grid_color_c300 'X',
        tabela_remetente_re 'NAME1'           text_n001 space 04 35 space space space space 'X'   space             space,
        tabela_remetente_re 'STCD1'           text_n003 space 05 18 space space space space 'X'   space             space,
        tabela_remetente_re 'UF'              text_n004 space 06 03 space space space space space space             space.

    clear: plan_gs_layout.
    plan_gs_layout-zebra      = 'X'.
    plan_gs_layout-sel_mode   = space.
    plan_gs_layout-edit_mode  = 'X'.

    wa_exclude_fcode = '&LOCAL&CUT'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&INSERT_ROW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&MOVE_ROW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE_NEW_ROW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&UNDO'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&VARI_ADMIN'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&APPEND'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY_ROW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&VLOTUS'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&AQW'.
    append wa_exclude_fcode to it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_SUM'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_EXPORT'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&MB_FILTER'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&GRAPH'.
    append wa_exclude_fcode to it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    append wa_exclude_fcode to it_exclude_fcode.

    call method plan_alv_remetente_re->set_table_for_first_display
      exporting
        i_default            = space
        is_layout            = plan_gs_layout
        it_toolbar_excluding = it_exclude_fcode
      changing
        it_fieldcatalog      = plan_catalogo_remetente_re
        it_outtab            = it_znom_remetente_alv[].

    plan_prim_remetente_re = 'X'.
  endif.

  call method plan_alv_remetente_re->refresh_table_display.

  call method plan_alv_remetente_re->set_scroll_info_via_id
    exporting
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

endmodule.                 " STATUS_1002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1002 input.

  data: vg_qtd_recusada type ntgew_ap.

  case ok_code_1001.
    when ok_sel.
      call method plan_alv_remetente_re->check_changed_data.
      vg_qtd_recusada = 0.
      loop at it_znom_remetente_alv.
        vg_qtd_recusada = vg_qtd_recusada + it_znom_remetente_alv-nr_qtd_vincular.
      endloop.
      if vg_qtd_recusada ne vg_a_qtd_recusar.
        message s045.
      else.
        leave to screen 0.
      endif.
  endcase.

endmodule.                 " USER_COMMAND_1002  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1002_exit input.
  clear: it_znom_remetente_alv[].
  leave to screen 0.
endmodule.                 " USER_COMMAND_1002_EXIT  INPUT
