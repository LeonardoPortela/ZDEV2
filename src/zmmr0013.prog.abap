*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZMMR0013                                                                             *
*& Chamado        : USER STORY 156553                                                                    *
*& Data           : 09/12/2024                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data      | Request    | Autor         | Alteração                                                   *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 09/12/2024 | DEVK9A2BIM | NSEGATIN      | Desenvolver um relatório de suporte para a EFD (Dev Inicial)*
*--------------------------------------------------------------------------------------------------------*
report zmmr0013.

tables: zfiwrt0008, j_1bnflin.

*--------------------------------------------------------------------*
* T Y P E S                                                          *
*--------------------------------------------------------------------*
types: begin of ty_out,
         bukrs      type bukrs,
         branch     type j_1bbranc_,
         name       type name1,
         finalidade type val_text,
         matnr      type matnr,
         maktx      type maktx,
         docnum_qbr type j_1bdocnum,
         cfop_qbr   type j_1bcfop,
         budat_qbr  type budat,
         month_qbr  type month,
         nfenum_qbr type j_1bnfnum9,
         menge      type j_1bnetqty,
         netwrt     type j_1bnfnett,
         taxval     type j_1btaxval,
         rate       type j_1btxrate,
         dias_corr  type p,
         ac_key_qbr type j_1b_nfe_access_key_dtel44,
         nfenum_rfl type j_1bnfnum9,
         cfop_rfl   type j_1bcfop,
         docnum_rfl type j_1bdocnum,
         budat_rfl  type budat,
         month_rfl  type month,
         quant_vinc type j_1bnetqty,
         vlr_total  type j_1bnetval,
         unitario   type zunita,
         vlicms_qbr type j_1bnetval,
         ac_key_rfl type j_1b_nfe_access_key_dtel44,
         prazo(9),
       end   of ty_out,

       begin of ty_select,
         bukrs      type bukrs,
         branch     type j_1bbranc_,
         finalidade type val_text,
         matnr      type matnr,
         docnum_qbr type j_1bdocnum,
         cfop       type j_1bcfop,
         budat_qbr  type budat,
         menge      type j_1bnetqty,
         netwrt     type j_1bnfnett,
         taxval     type j_1btaxval,
         rate       type j_1btxrate,
         docnum_rfl type j_1bdocnum,
         quant_vinc type j_1bnetqty,
         vlr_total  type j_1bnetval,
         unitario   type zunita,
         matkl      type mara-matkl,
       end   of ty_select,

       begin of ty_nfdoc,
         docnum type j_1bdocnum,
         pstdat type j_1bpstdat,
         nfenum type j_1bnfnum9,
       end   of ty_nfdoc,

       begin of ty_nflin,
         docnum type j_1bdocnum,
         itmnum type j_1bitmnum,
         matnr  type matnr,
         cfop   type j_1bcfop,
       end   of ty_nflin,

       begin of ty_active,
         docnum  type j_1bdocnum,
         regio   type j_1bregio,
         nfyear  type j_1byear,
         nfmonth type j_1bmonth,
         stcd1   type j_1bstcd1,
         model   type j_1bmodel,
         serie   type j_1bseries,
         nfnum9  type j_1bnfnum9,
         docnum9 type j_1bdocnum9,
         cdv     type j_1bcheckdigit,
       end   of ty_active,

       begin of ty_makt,
         matnr type matnr,
         maktx type maktx,
       end   of ty_makt,

       begin of ty_zsdt0283,
         bukrs type zsdt0283-bukrs,
         matkl type zsdt0283-matkl,
         dias  type zsdt0283-dias,
       end   of ty_zsdt0283,

       begin of ty_brancht,
         bukrs  type bukrs,
         branch type j_1bbranc_,
         name   type name1,
       end   of ty_brancht,

       begin of ty_dd07t,
         ddtext     type val_text,
         domvalue_l type domvalue_l,
       end   of ty_dd07t.

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
data: tg_select     type table of ty_select,
      tg_nfdoc_qbr  type table of ty_nfdoc,
      tg_nfdoc_rfl  type table of ty_nfdoc,
      tg_active_qbr type table of ty_active,
      tg_active_rfl type table of ty_active,
      tg_nlin_rfl   type table of ty_nflin,
      tg_makt       type table of ty_makt,
      tg_brancht    type table of ty_brancht,
      tg_out        type table of ty_out,
      tg_dd07t      type table of ty_dd07t,
      tg_zsdt0283   type table of ty_zsdt0283.

*--------------------------------------------------------------------*
* C L A S S   D E F I N I T I O N                                    *
*--------------------------------------------------------------------*
class clg_event_handler definition.
  public section.
* To define link click in ALV Grid.
    methods: zm_link_click for event link_click of cl_salv_events_table
      importing row
                column.

endclass.

*--------------------------------------------------------------------*
* C L A S S   I M P L E M E N T A T I O N S                          *
*--------------------------------------------------------------------*
class clg_event_handler implementation.
* To implement link click in ALV Grid.
  method zm_link_click.
    perform zf_link_click using row
                                column.

  endmethod.                    "ZM_LINK_CLICK

endclass.                    "CLG_EVENT_HANDLER IMPLEMENTATION
*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
* Tela de Seleção
selection-screen begin of block b1 with frame title text-001.
  parameters:     p_bukrs  type bukrs.
  select-options: s_branch for zfiwrt0008-branch obligatory no intervals,
                  s_matnr  for j_1bnflin-matnr no intervals,
                  s_docnum for j_1bnflin-docnum no intervals,
                  s_budat  for zfiwrt0008-budat obligatory no-extension,
                  s_ac_key for zfiwrt0008-access_key no intervals.
  selection-screen skip.
* Tipo de Layout de relatório.
  selection-screen begin of block b2 with frame title text-002.
    parameters: p_layout type slis_vari.

  selection-screen end of block b2.
selection-screen end of block b1.
*--------------------------------------------------------------------*
* I N I T I A L I Z A T I O N                                        *
*--------------------------------------------------------------------*
initialization.
* Restringe as opções da tela de seleção
  perform zf_limit_select_option.

*--------------------------------------------------------------------*
* A T - S E L E C T I O N  S C R E E N                               *
*--------------------------------------------------------------------*
* O evento é executado quando clicado no botão de ajuda da Variante de Layout.
at selection-screen on value-request for p_layout.
* Pocessamento da varinate de Layout do ALV Grid.
  perform zf_variant_prg using p_layout.

*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
start-of-selection.
  perform: zf_select_data, "Selecionar dados para o processamento
           zf_proces_data, "Processa dados selecionados
           zf_show_data.   "Exibe os dados selecionados
*&---------------------------------------------------------------------*
*&      Form  ZF_VARIANT_PRG
*&---------------------------------------------------------------------*
*       Pocessamento da varinate de Layout do ALV Grid
*----------------------------------------------------------------------*
*      -->UV_LAYOUT  Valor do nome do Layaout de exibição
*----------------------------------------------------------------------*
form zf_variant_prg using uv_layout.

  data: el_variant  type disvariant,
        el_variantx type disvariant.

  data: vl_save type c,
        vl_exit type c.

  clear: el_variantx, el_variant, vl_save, vl_exit.

  vl_save           = sy-abcde(1). "A
  el_variant-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = el_variant
      i_save     = vl_save
    importing
      e_exit     = vl_exit
      es_variant = el_variantx
    exceptions
      not_found  = 2.

  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  else.
    if vl_exit = space.
      uv_layout = el_variantx-variant.

    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       Selecionar dados para o processamento
*----------------------------------------------------------------------*
form zf_select_data.

  types: begin of ty_ac_key,
           docnum     type j_1bdocnum,
           ac_key_qbr type j_1b_nfe_access_key_dtel44,
         end   of ty_ac_key.

  data: tl_ac_key type table of ty_ac_key,
        tl_stg    type table of char13.

  data: rl_finalidade type range of zfin_export,
        rl_docnum_qbr type range of j_1bdocnum.

  constants: cl_icms    type j_1btaxgrp value 'ICMS',
             cl_ieq     type char3      value 'IEQ',
             cl_domname type domname    value 'ZFIN_EXPORT_D'.

  rl_finalidade = value #( sign = cl_ieq(1) option = cl_ieq+1(2) ( low = sy-abcde+16(1) high = space )    "Q - Quebra Porto
                                                                 ( low = sy-abcde+18(1) high = space )    "S - Sinistro Parcial
                                                                 ( low = sy-abcde+24(1) high = space ) ). "Y - Sinistro Total

  select a~bukrs a~branch f~finalidade b~matnr a~docnum b~cfop a~budat
         b~menge b~netwrt c~taxval c~rate e~docnum e~quant_vinc e~vlr_total
         e~unitario  g~matkl
  from zfiwrt0008 as a
   inner join j_1bnflin as b
    on a~docnum eq b~docnum
   inner join j_1bnfstx as c
    on  b~docnum eq c~docnum and
        b~itmnum eq c~itmnum
   inner join zfiwrt0020 as d
    on a~seq_lcto eq d~seq_lcto
   inner join zsdt_retlote as e
    on d~docnum eq e~docnum_ret
   inner join zsdt_export as f
    on  e~docnum_ret eq f~docnum
   inner join mara as g
    on g~matnr = b~matnr
   into table tg_select
  where a~bukrs      eq p_bukrs
    and a~branch     in s_branch
    and a~budat      in s_budat
    and b~matnr      in s_matnr
    and c~taxgrp     eq cl_icms
    and f~finalidade in rl_finalidade.

  if sy-subrc is initial.
    sort tg_select by docnum_qbr.
    data(tl_select) = tg_select.
    delete adjacent duplicates from tl_select comparing docnum_qbr.

    select docnum pstdat nfenum
      from j_1bnfdoc
      into table tg_nfdoc_qbr
      for all entries in tl_select
    where docnum eq tl_select-docnum_qbr.

    if sy-subrc is initial.
      select docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
        from j_1bnfe_active
        into table tg_active_qbr
        for all entries in tg_nfdoc_qbr
      where docnum eq tg_nfdoc_qbr-docnum.
* Verifica se a chave de quebra foi preenchida.
      if     sy-subrc   is initial and
         not s_ac_key[] is initial.
* Trata os dados com a chave de quebra informada.
        tl_ac_key = tg_active_qbr.
        delete tl_ac_key where ac_key_qbr not in s_ac_key.

        if tl_ac_key[] is initial.
* Não encontrados dados para esta seleção
          message s114(pt) display like sy-abcde+4(1). "E
          leave list-processing.

        else.
          tl_stg = tl_ac_key.
          replace all occurrences of regex '\<' in table tl_stg with cl_ieq in character mode respecting case.
          rl_docnum_qbr = tl_stg.
* Elimina s dados que não pertencem a chave de quebra informada.
          delete tg_select where docnum_qbr not in rl_docnum_qbr.

          if tg_select[] is initial.
* Não encontrados dados para esta seleção
            message s114(pt) display like sy-abcde+4(1). "E
            leave list-processing.

          endif.

          delete tg_nfdoc_qbr where docnum not in rl_docnum_qbr.

        endif.

      endif.

    endif.

    check tg_select[] is not initial.

    tl_select = tg_select.
    sort tl_select by docnum_rfl.
    delete adjacent duplicates from tl_select comparing docnum_rfl.

    select docnum pstdat nfenum
      from j_1bnfdoc
      into table tg_nfdoc_rfl
      for all entries in tl_select
    where docnum eq tl_select-docnum_rfl.

    if sy-subrc is initial.
      select docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
        from j_1bnfe_active
        into table tg_active_rfl
        for all entries in tg_nfdoc_rfl
      where docnum eq tg_nfdoc_rfl-docnum.

      select docnum itmnum matnr cfop
        from j_1bnflin
        into table tg_nlin_rfl
        for all entries in tg_nfdoc_rfl
      where docnum eq tg_nfdoc_rfl-docnum.

    endif.

    tl_select = tg_select.
    sort tl_select by matnr.
    delete adjacent duplicates from tl_select comparing matnr.

    select matnr maktx from makt
      into table tg_makt
      for all entries in tl_select
    where spras eq sy-langu
      and matnr eq tl_select-matnr.

    tl_select = tg_select.
    sort tl_select by bukrs branch.
    delete adjacent duplicates from tl_select comparing bukrs branch.

    select bukrs branch name
      from j_1bbrancht
      into table tg_brancht
      for all entries in tl_select
    where bukrs    eq tl_select-bukrs
      and branch   eq tl_select-branch
      and language eq sy-langu.

    tl_select = tg_select.
    sort tl_select by finalidade.
    delete adjacent duplicates from tl_select comparing finalidade.

    select ddtext domvalue_l
      from dd07t
      into table tg_dd07t
      for all entries in tl_select
    where domname    eq cl_domname
      and ddlanguage eq sy-langu
      and domvalue_l eq tl_select-finalidade(10).

    select distinct bukrs matkl dias
      from zsdt0283
      into table tg_zsdt0283
      for all entries in tg_select
      where bukrs = tg_select-bukrs
      and   matkl = tg_select-matkl.

  else.
* Não encontrados dados para esta seleção
    message s114(pt) display like sy-abcde+4(1). "E
    leave list-processing.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_DATA
*&---------------------------------------------------------------------*
*       Processa dados selecionados
*----------------------------------------------------------------------*
form zf_proces_data.

* Carrega a tabela de saída para exibir o relatório.
  tg_out = value #( for el_select     in tg_select
                    for el_nfdoc_qbr  in tg_nfdoc_qbr  where ( docnum     eq el_select-docnum_qbr )
                    for el_active_qbr in tg_active_qbr where ( docnum     eq el_select-docnum_qbr )
                    for el_nfdoc_rfl  in tg_nfdoc_rfl  where ( docnum     eq el_select-docnum_rfl )
                    for el_active_rfl in tg_active_rfl where ( docnum     eq el_select-docnum_rfl )
                    for el_nflin_rfl  in tg_nlin_rfl   where ( docnum     eq el_select-docnum_rfl )
                    for el_makt       in tg_makt       where ( matnr      eq el_select-matnr )
                    for el_brancht    in tg_brancht    where ( branch     eq el_select-branch )
                    for el_dd07t      in tg_dd07t      where ( domvalue_l eq el_select-finalidade )
                    ( bukrs      = el_select-bukrs
                      branch     = el_select-branch
                      name       = el_brancht-name
                      finalidade = | { el_select-finalidade } { sy-uline(1) } { el_dd07t-ddtext } |
                      matnr      = el_select-matnr
                      maktx      = el_makt-maktx
                      docnum_qbr = el_select-docnum_qbr
                      cfop_qbr   = el_select-cfop
                      budat_qbr  = el_select-budat_qbr
                      month_qbr  = el_select-budat_qbr+4(2)
                      nfenum_qbr = el_nfdoc_qbr-nfenum
                      menge      = el_select-menge
                      netwrt     = el_select-netwrt
                      taxval     = el_select-taxval
                      rate       = el_select-rate
                      dias_corr  = el_select-budat_qbr - el_nfdoc_rfl-pstdat
                      ac_key_qbr = el_active_qbr-regio && el_active_qbr-nfyear && el_active_qbr-nfmonth && el_active_qbr-stcd1 && el_active_qbr-model &&
                                   el_active_qbr-serie && el_active_qbr-nfnum9 && el_active_qbr-docnum9 && el_active_qbr-cdv
                      nfenum_rfl = el_nfdoc_rfl-nfenum
                      docnum_rfl = el_select-docnum_rfl
                      cfop_rfl   = el_nflin_rfl-cfop
                      budat_rfl  = el_nfdoc_rfl-pstdat
                      month_rfl  = el_nfdoc_rfl-pstdat+4(2)
                      quant_vinc = el_select-quant_vinc
                      vlr_total  = el_select-vlr_total
                      unitario   = el_select-unitario
                      vlicms_qbr = el_select-vlr_total * ( el_select-rate / 100 )
                      ac_key_rfl = el_active_rfl-regio && el_active_rfl-nfyear && el_active_rfl-nfmonth && el_active_rfl-stcd1 && el_active_rfl-model &&
                                   el_active_rfl-serie && el_active_rfl-nfnum9 && el_active_rfl-docnum9 && el_active_rfl-cdv
                      prazo      = el_select-matkl
                    )
                  ).

  loop at tg_out assigning field-symbol(<out>).
    read table tg_zsdt0283 into data(wg_zsdt0283) with key bukrs = <out>-bukrs matkl = <out>-prazo.
    if sy-subrc = 0.
      if <out>-dias_corr gt wg_zsdt0283-dias.
        <out>-prazo = 'FORA'.
      else.
        <out>-prazo = 'DENTRO'.
      endif.
    endif.
  endloop.

  delete tg_out where nfenum_qbr is initial.

endform.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_DATA
*&---------------------------------------------------------------------*
*       Exibe os dados selecionados
*----------------------------------------------------------------------*
form zf_show_data.

* Declaração de Classes.
  data: lcl_table         type ref to cl_salv_table,
        lcl_pfstatus      type ref to cl_salv_functions_list,
        lcl_display       type ref to cl_salv_display_settings,
        lcl_columns       type ref to cl_salv_columns_table,
        lcl_column        type ref to cl_salv_column_table,
        lcl_aggreg        type ref to cl_salv_aggregations,
        lcl_layout        type ref to cl_salv_layout,
        lcl_events        type ref to cl_salv_events_table,
        lcl_event_handler type ref to clg_event_handler.

  data: tl_tpool type textpool_t.

  data: el_layout type lvc_s_layo,
        el_key    type salv_s_layout_key,
        begin of el_txt_fld,
          tx_short  type scrtext_s,
          tx_medium type scrtext_m,
          tx_long   type scrtext_l,
        end   of el_txt_fld.

  constants: cl_show       type seu_action value 'SHOW',
             cl_aster      type c          value '*',
             cl_docnum_qbr type fieldname  value 'DOCNUM_QBR',
             cl_docnum_rfl type fieldname  value 'DOCNUM_RFL'.

  check not tg_out is initial.

*... Create Instance
  call method cl_salv_table=>factory
    importing
      r_salv_table = lcl_table
    changing
      t_table      = tg_out.
*... Set PFSTATUS
  call method lcl_table->get_functions
    receiving
      value = lcl_pfstatus.
  call method lcl_pfstatus->set_all.
* ... Implement ZEBRA.
  lcl_display = lcl_table->get_display_settings( ).
  lcl_display->set_striped_pattern( abap_true ).
  lcl_columns = lcl_table->get_columns( ).
  lcl_columns->set_optimize( abap_true ).
*... Set EVENTS
  lcl_events = lcl_table->get_event( ).
*... Creating an instance for the event handler
  create object lcl_event_handler.
*... Registering handler methods to handle ALV Grid events
  set handler lcl_event_handler->zm_link_click for lcl_events .
* Busca os dados do Elemento de textos do programa.
  call function 'RS_TEXTPOOL_READ'
    exporting
      objectname = sy-repid
      action     = cl_show
    tables
      tpool      = tl_tpool.
* Carrega o Field Catlog gerado pela instância da classe CL_SALV_TABLE.
  data(tl_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                  r_columns      = lcl_columns
                  r_aggregations = lcl_aggreg ).
* Ajuste as descrições do campos conforme o elemento de textos.
  loop at tl_tpool into data(el_tpool) where    id eq sy-abcde+8(1) "I
                                         and entry cs cl_aster.
    split el_tpool-entry at cl_aster into data(vl_txt_fld) data(vl_nam_fld).

    read table tl_fcat into data(el_fcat) with key fieldname = vl_nam_fld.

    if sy-subrc is initial.
      lcl_column ?= lcl_columns->get_column( el_fcat-fieldname ).
      el_txt_fld-tx_short = el_txt_fld-tx_medium  = el_txt_fld-tx_long = vl_txt_fld.
      lcl_column->set_short_text( el_txt_fld-tx_short ).
      lcl_column->set_medium_text( el_txt_fld-tx_medium ).
      lcl_column->set_long_text( el_txt_fld-tx_long ).
* Verifica se é Documento de Quebra ou de Retorno de Formação de Lote para configurar o hotspot.
      if el_fcat-fieldname eq cl_docnum_qbr or
         el_fcat-fieldname eq cl_docnum_rfl.
        lcl_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

      endif.

    endif.

  endloop.
* ... Set Configuration of the Save Layout Variant.
  lcl_layout = lcl_table->get_layout( ).
  lcl_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  el_key-report = sy-repid.
  lcl_layout->set_key( el_key ).
* Verifica se foi definido o Layout na tela de seleção.
  if not p_layout is initial.
    lcl_layout->set_initial_layout( p_layout ).

  endif.
* ... Set Configuration of the Save Layout Variant.
  lcl_layout = lcl_table->get_layout( ).
*... Display table in ALV Grid
  lcl_table->display( ).

endform.
*&----------------------------------------------------------------------------------*
*&      Form  ZF_LIMIT_SELECT_OPTION
*&----------------------------------------------------------------------------------*
*       Restringe as opções da tela de seleção
*-----------------------------------------------------------------------------------*
form zf_limit_select_option.

  type-pools sscr. "Tipo da tela de selação
* Restringe os dados do parâmetro da tela de seleção
  data: tl_screen type sscr_restrict. "Tabelda tela de seleção
* Estruturas para preencher a tab. t_screen
  data: el_opts  type sscr_opt_list, "Estrutura da restrição da lista de opções
        el_assoc type sscr_ass.      "Estrutura da lista do nome da variável restringida
  constants: cl_objectkey1(10) type c value 'OBJECTKEY1',
             cl_objectkey2(10) type c value 'OBJECTKEY2',
             cl_objectkey3(10) type c value 'OBJECTKEY3',
             cl_objectkey4(10) type c value 'OBJECTKEY4'.

* Restringe o campo "Modificado em"  selection para somente EQ.
* Filial
  el_opts-name       = cl_objectkey1.
  el_opts-options-eq = sy-abcde+23(1). "X
  append el_opts to tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_BRANCH'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey1.
  append el_assoc to tl_screen-ass_tab.
* Material
  el_opts-name       = cl_objectkey2.
  el_opts-options-eq = sy-abcde+23(1). "X
  append el_opts to tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_MATNR'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey2.
  append el_assoc to tl_screen-ass_tab.
* Documento Nª de quebra
  el_opts-name       = cl_objectkey3.
  el_opts-options-eq = sy-abcde+23(1). "X
  append el_opts to tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_DOCNUM'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey3.
* Chave de quebra
  el_opts-name       = cl_objectkey4.
  el_opts-options-eq = sy-abcde+23(1). "X
  append el_opts to tl_screen-opt_list_tab.
  el_assoc-kind      = sy-abcde+18(1). "S
  el_assoc-name      = 'S_AC_KEY'.
  el_assoc-sg_main   = sy-abcde+8(1).  "I
  el_assoc-sg_addy   = space.
  el_assoc-op_main   = cl_objectkey4.
  append el_assoc to tl_screen-ass_tab.
* Função para restringir Selection  Option
  call function 'SELECT_OPTIONS_RESTRICT'
    exporting
      restriction            = tl_screen
    exceptions
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      others                 = 9.
* Verifica de função executou com erro.
  if not sy-subrc is initial.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endif.

endform.
*&---------------------------------------------------------------------*
*&     Form ZF_LINK_CLICK
*&---------------------------------------------------------------------*
*&     To implement link click in ALV Grid
*&---------------------------------------------------------------------*
*      -->UV_ROW    Line position clic in ALV grid
*      -->UV_COLUMN Column position clic in ALV grid
*&---------------------------------------------------------------------*
form zf_link_click using uv_row    type salv_de_row
                         uv_column type salv_de_column.

  read table tg_out into data(el_out) index uv_row.

  if sy-subrc is initial.
* Verifica qual a coluna foi clicada.
    case uv_column.
      when 'DOCNUM_QBR'.
        data(vl_docnum) = el_out-docnum_qbr.


      when 'DOCNUM_RFL'.
        vl_docnum = el_out-docnum_rfl.

      when others.
*   Do nothing
    endcase.

  endif.
* Carrega para a área de memória o Nº do Documento.
  set parameter id 'JEF' field vl_docnum.
  call transaction 'J1B3N' and skip first screen.
  clear vl_docnum.
* Limpa o Nº do Documento da área de memória.
  set parameter id 'JEF' field vl_docnum.

endform.
