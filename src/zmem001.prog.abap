*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.       *
*****************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                      *
* Data desenv ...: 16.01.2012                                               *
* Objetivo    ...: Relatório de Acompanhamento de Notas Fiscais Exportação  *
* Transação   ...: ZMEM010                                                  *
* Autor       ...: ABAP                                                     *
*****************************************************************************

report  zmem001.
****************************************************
*  Type Pools
****************************************************
type-pools: icon.
****************************************************
*  Tables
****************************************************
tables: j_1bbranch, j_1bnfdoc, mara, mch1.
****************************************************
*  Structure
****************************************************
types:
      begin of ty_j_1bbranch,
        bukrs  type j_1bbranch-bukrs,  "Empresa.
        branch type j_1bbranch-branch, "Local de Negócio.
      end of ty_j_1bbranch,

      begin of ty_j_1bnfdoc,
        bukrs  type j_1bnfdoc-bukrs,  "Empresa
        branch type j_1bnfdoc-branch, "Local de Negócio.
        docdat type j_1bnfdoc-docdat, "Data do documento
        direct type j_1bnfdoc-direct, "Direção do movimento de mercadorias
        doctyp type j_1bnfdoc-doctyp, "Tipo de documento
        cancel type j_1bnfdoc-cancel, "Estornado
        docnum type j_1bnfdoc-docnum, "Nº documento
        series type j_1bnfdoc-series, "Série
        nfnum  type j_1bnfdoc-nfnum,  "Nº nota fiscal
        nfe    type j_1bnfdoc-nfe,    "Nota Fiscal eletrônica
        nfenum type j_1bnfdoc-nfenum, "Nº NF-e de nove posições
      end of   ty_j_1bnfdoc,

      begin of ty_j_1bnflin,
        docnum    type j_1bnflin-docnum, "Nº documento
        matnr     type j_1bnflin-matnr,  "Nº do material
        charg     type j_1bnflin-charg,  "Número do lote
        cfop      type j_1bnflin-cfop,   "Código CFOP e extensão
        itmnum    type j_1bnflin-itmnum, "Nº item do documento
        menge     type j_1bnflin-menge,  "Quantidade
        meins     type j_1bnflin-meins,  "Unidade de medida básica
        menge_aux type j_1bnflin-menge,  "Quantidade
      end of ty_j_1bnflin,

      begin of ty_mara,
        matnr type mara-matnr, "Nº do material
      end of ty_mara,

      begin of ty_makt,
        matnr type makt-matnr, "Nº do material
        maktx type makt-maktx, "Texto breve de material
        spras type makt-spras, "Código de idioma
      end of ty_makt,

      begin of ty_zdoc_memo_nota_s,
          docnum      type zdoc_memo_nota_s-docnum, "Nº documento
          itmnum      type zdoc_memo_nota_s-itmnum, "Nº item do documento
          menge       type zdoc_memo_nota_s-menge, "Quantidade
      end of ty_zdoc_memo_nota_s,

      begin of ty_t001w,
        ekorg       type  t001w-ekorg,
        j_1bbranch  type  t001w-j_1bbranch,
      end of ty_t001w,

      begin of ty_saida,
          docnum      type j_1bnfdoc-docnum, "Nº documento
          docdat      type j_1bnfdoc-docdat, "Data do documento
          series      type j_1bnfdoc-series, "Série
          nfenum      type j_1bnfdoc-nfenum, "Nº NF-e de nove posições
          bukrs       type j_1bnfdoc-bukrs,  "Empresa
          branch      type j_1bnfdoc-branch, "Local de negócios
          charg       type j_1bnflin-charg,  "Número do lote
          matnr       type j_1bnflin-matnr,  "Nº do material
          maktx       type makt-maktx,       "Texto breve de material
          menge       type j_1bnflin-menge,  "Quantidade
          meins       type j_1bnflin-meins,  "Unidade de medida básica
          menge_memo  type zdoc_memo_nota_s-menge, "Quantidade
          menge_saldo type zdoc_memo_nota_s-menge, "Quantidade
          status      type char04,
      end of ty_saida.

****************************************************
*  Internal Table
****************************************************
data:
      it_j_1bbranch           type table of ty_j_1bbranch, " Internal Table - Local de negócios
      it_j_1bbranch_aux       type table of ty_j_1bbranch, " Internal Table - Local de negócios
      it_j_1bnfdoc            type table of ty_j_1bnfdoc,  " Internal Table - Cabeçalho da nota fiscal
      it_j_1bnflin            type table of ty_j_1bnflin,  " Internal Table - Partidas individuais da nota fiscal
      it_makt                 type table of ty_makt,       " Internal Table - Textos breves de material
      it_t001w                type table of ty_t001w,      " Internal Table - Centro/Filiais
      it_j_1bnflin_aux        type table of ty_j_1bnflin,  " Internal Table Auxiliar - Partidas individuais da nota fiscal
      it_zdoc_memo_nota_s     type table of ty_zdoc_memo_nota_s, " Internal Table - Notas Vinculadas - Notas de Saida
      it_zdoc_memo_nota_s_aux type table of ty_zdoc_memo_nota_s, " Internal Table - Notas Vinculadas - Notas de Saida
      it_fcat                 type table of lvc_s_fcat,    " Internal Table - Controle VLA: catálogo de campos
      it_cfops                type standard table of lxhme_range_c10, " Internal Table - Estrutura tabela intervalo de valores p/campo 10 caracteres
      it_saida                type table of ty_saida.      " Internal Table - Estrutura para Saída do ALV
****************************************************
*  Work Area
****************************************************
data:
      wa_j_1bbranch           type ty_j_1bbranch, " Work Area - Local de Negócios
      wa_j_1bbranch_aux       type ty_j_1bbranch, " Work Area - Local de Negócios
      wa_j_1bnfdoc            type ty_j_1bnfdoc,  " Work Area - Cabeçalho da nota fiscal
      wa_j_1bnflin            type ty_j_1bnflin,  " Work Area - Partidas individuais da nota fiscal
      wa_zdoc_memo_nota_s     type ty_zdoc_memo_nota_s, " Internal Table - Notas Vinculadas - Notas de Saida
      wa_makt                 type ty_makt,       " Internal Table - Textos breves de material
      wa_t001w                type ty_t001w,      " Internal Table - Centro/Filiais
      wa_saida                type ty_saida,      " Internal Table - Estrutura de Saída do ALV
      wa_j_1bnflin_aux        type ty_j_1bnflin,  " Work Area Auxiliar - Partidas individuais da nota fiscal
      wa_zdoc_memo_nota_s_aux type ty_zdoc_memo_nota_s, " Internal Table - Notas Vinculadas - Notas de Saida
      wa_cont                 type ref to cl_gui_custom_container,
      wa_alv                  type ref to  cl_gui_alv_grid,
      wa_layout               type lvc_s_layo.

****************************************************
*  Selection Window
****************************************************
selection-screen: begin of block b1 with frame title text-001.
select-options: p_bukrs  for j_1bbranch-bukrs  no intervals no-extension obligatory,   " Empresa
                p_branch for j_1bbranch-branch no intervals obligatory,                " Filial
                p_docdat for j_1bnfdoc-docdat  obligatory,                             " Periodo
                p_matnr  for mara-matnr        no intervals no-extension,              " Material
                p_charg  for mch1-charg        no intervals no-extension.              " Safra
selection-screen: end of block b1.


****************************************************
*  Start Of Selection
****************************************************
start-of-selection.
  "Perform para selecionar dados; chamada da ALV.
  perform:
           z_seleciona_dados,
           z_alv.

  "Chamada para ALV - Tela 0100.
  call screen 0100.

end-of-selection.
****************************************************
*      Form  Z_SELECIONA_DADOS
****************************************************
*       Seleciona Dados
****************************************************
form z_seleciona_dados.

  " Variaveis Locais
  data: menge type j_1bnflin-menge.

  select ekorg j_1bbranch
    from t001w
  into table it_t001w
    where ekorg in p_branch.

  if ( sy-subrc eq 0 ).

    " Local de Negócios
    select bukrs branch
      from j_1bbranch
    into table it_j_1bbranch
      for all entries in it_t001w
     where branch eq it_t001w-j_1bbranch
       and bukrs  in p_bukrs.

  else.

    select bukrs branch
      from j_1bbranch
    into table it_j_1bbranch
     where branch in p_branch
       and bukrs  in p_bukrs.

  endif.

  check not it_j_1bbranch[] is initial.

  " Cabeçalho da nota fiscal
  select bukrs branch docdat direct doctyp cancel docnum series nfnum nfe nfenum
    from j_1bnfdoc
  into table it_j_1bnfdoc
    for all entries in it_j_1bbranch
  where bukrs  eq it_j_1bbranch-bukrs
    and branch eq it_j_1bbranch-branch
    and docdat in p_docdat
    and direct eq '2'
    and doctyp ne '5'
    and cancel ne 'X'.

  check not it_j_1bnfdoc[] is initial.

  call function 'Z_MEMO_CFOP_SAIDA'
    exporting
      exp_propria = 'X'
    tables
      cfops       = it_cfops.

  check not it_cfops[] is initial.

  select docnum matnr charg cfop itmnum menge meins
    from j_1bnflin
   into table it_j_1bnflin_aux
    for all entries in it_j_1bnfdoc
   where docnum eq it_j_1bnfdoc-docnum
     and matnr  in p_matnr
     and charg  in p_charg
     and cfop   in it_cfops.

  loop at it_j_1bnflin_aux into wa_j_1bnflin_aux.

    call function 'ME_CONVERSION_MEINS'
      exporting
        i_matnr             = wa_j_1bnflin-matnr
        i_mein1             = wa_j_1bnflin-meins
        i_meins             = 'KG'
        i_menge             = wa_j_1bnflin-menge
      importing
        menge               = menge
      exceptions
        error_in_conversion = 1
        no_success          = 2
        others              = 3.

    wa_j_1bnflin_aux-menge_aux = menge.

    append wa_j_1bnflin_aux to it_j_1bnflin.
  endloop.


  check not it_j_1bnflin[] is initial.


  select matnr maktx spras
    from makt
  into table it_makt
    for all entries in it_j_1bnflin
   where matnr eq it_j_1bnflin-matnr
     and spras eq sy-langu.

  "Notas Vinculadas - Notas de Saida
  select docnum itmnum menge
    from zdoc_memo_nota_s
  into table it_zdoc_memo_nota_s_aux
    for all entries in it_j_1bnflin
  where  docnum  eq it_j_1bnflin-docnum
    and  itmnum  eq it_j_1bnflin-itmnum.

  sort: it_zdoc_memo_nota_s_aux by docnum
                                   itmnum.

  loop at it_zdoc_memo_nota_s_aux into wa_zdoc_memo_nota_s_aux.
    move-corresponding wa_zdoc_memo_nota_s_aux to wa_zdoc_memo_nota_s.
    collect wa_zdoc_memo_nota_s into it_zdoc_memo_nota_s.
  endloop.

  perform: z_saida.

endform.                    " Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_SAIDA
*&---------------------------------------------------------------------*
form z_saida .

  sort: it_j_1bbranch       by bukrs branch,
        it_j_1bnflin        by docnum,
        it_makt             by matnr,
        it_zdoc_memo_nota_s by docnum itmnum.


  loop at it_j_1bnfdoc into wa_j_1bnfdoc.


    read table it_j_1bbranch into wa_j_1bbranch with key bukrs =  wa_j_1bnfdoc-bukrs
                                                         branch = wa_j_1bnfdoc-branch binary search.

    if ( sy-subrc eq 0 ).

      wa_saida-docnum = wa_j_1bnfdoc-docnum.
      wa_saida-docdat = wa_j_1bnfdoc-docdat.
      wa_saida-series = wa_j_1bnfdoc-series.

      if ( wa_j_1bnfdoc-nfe eq 'X' ).
        wa_saida-nfenum = wa_j_1bnfdoc-nfenum.
      else.
        wa_saida-nfenum = wa_j_1bnfdoc-nfnum.
      endif.

      wa_saida-bukrs  = wa_j_1bnfdoc-bukrs.
      wa_saida-branch = wa_j_1bnfdoc-branch.

      read table it_j_1bnflin into wa_j_1bnflin  with key docnum = wa_j_1bnfdoc-docnum binary search.

      if ( sy-subrc eq 0 ).

        wa_saida-charg = wa_j_1bnflin-charg.
        wa_saida-matnr = wa_j_1bnflin-matnr.
        wa_saida-menge = wa_j_1bnflin-menge.
        wa_saida-meins = wa_j_1bnflin-meins.

        read table it_makt into wa_makt with key matnr = wa_j_1bnflin-matnr binary search.

        wa_saida-maktx = wa_makt-maktx.

        read table it_zdoc_memo_nota_s into wa_zdoc_memo_nota_s with key docnum = wa_j_1bnflin-docnum
                                                                         itmnum = wa_j_1bnflin-itmnum binary search.

        wa_saida-menge_memo  = wa_zdoc_memo_nota_s-menge.
        wa_saida-menge_saldo = ( wa_j_1bnflin-menge - wa_zdoc_memo_nota_s-menge ).

        if ( wa_saida-menge_memo > 0 ).
          wa_saida-status = icon_checked.
        else.
          wa_saida-status = icon_incomplete.
        endif.

        append wa_saida to it_saida.

        clear: wa_j_1bbranch,
               wa_j_1bnfdoc,
               wa_j_1bnflin,
               wa_makt,
               wa_zdoc_memo_nota_s,
               wa_saida.


      endif.
    endif.
  endloop.

endform.                    " Z_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
form z_alv .
  perform alv_preenche_cat using:
      'STATUS'      text-023 '5'  '' ''  '', "
      'DOCNUM'      text-010 '11' '' ''  '', "
      'DOCDAT'      text-011 '10' '' ''  '', "
      'SERIES'      text-012 '4'  '' ''  '', "
      'NFENUM'      text-013 '10' '' ''  '', "
      'BUKRS'       text-014 '5'  '' ''  '', "
      'BRANCH'      text-015 '5'  '' ''  '', "
      'CHARG'       text-016 '10' '' 'X' '', "
      'MATNR'       text-017 '18' '' ''  '', "
      'MAKTX'       text-018 '40' '' ''  '', "
      'MENGE'       text-019 '14' '' ''  '', "
      'MEINS'       text-020 '4'  '' ''  '', "
      'MENGE_MEMO'  text-021 '13' '' ''  '', "
      'MENGE_SALDO' text-022 '13' '' ''  ''. "
endform.                    " Z_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
form alv_preenche_cat   using  p_campo type c
                               p_desc  type c
                               p_tam   type c
                               p_hot   type c
                               p_zero  type c
                               p_sum   type c.
  data: wl_fcat type lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  wl_fcat-outputlen = p_tam.

  append wl_fcat to it_fcat.


endform.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
module z_status output.
  set pf-status 'FF0100'.
  set titlebar  'TB0100'.
endmodule.                 " Z_STATUS  OUTPUT


class lcl_event_receiver definition deferred.
data wa_event type ref to lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

  public section.
    methods:

    zm_handle_hotspot for event hotspot_click of cl_gui_alv_grid
    importing e_row_id
              e_column_id
              es_row_no,

    zm_handle_toolbar for event toolbar of cl_gui_alv_grid
    importing
        e_object e_interactive,

    zm_handle_user_command for event user_command of cl_gui_alv_grid
    importing
         e_ucomm.
endclass.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

  method: zm_handle_hotspot.
    perform z_handle_hotspot using    e_row_id
                                      e_column_id
                                      es_row_no.
  endmethod.                    "zm_handle_hotspot

  method zm_handle_toolbar.
*   Incluindo Botão ALV
    perform z_handle_toolbar using e_object
                                   e_interactive.
  endmethod.                    "zm_handle_toolbar

  method zm_handle_user_command.
*   User Command Botões Incluidos
    perform z_handle_command using e_ucomm.
  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
module z_exibe_alv output.


  if wa_cont is initial.

    create object wa_cont
      exporting
        container_name              = 'CC_ALV'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
  endif.
  if wa_alv is initial and not
    wa_cont is initial.

    create object wa_alv
      exporting
        i_parent          = wa_cont
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.
  endif.

  if wa_event is initial.

    create object wa_event.
    set handler: wa_event->zm_handle_hotspot for wa_alv.
    set handler: wa_event->zm_handle_toolbar for wa_alv.
    set handler: wa_event->zm_handle_user_command for wa_alv.

  endif.

  call method wa_alv->set_table_for_first_display
    exporting
      is_layout                     = wa_layout
    changing
      it_outtab                     = it_saida
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
  check not wa_alv is initial.


endmodule.                 " Z_EXIBE_ALV  OUTPUT
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
    endcase.
  endif.
endmodule.                 " Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
form z_handle_command    using p_ucomm type syucomm.


endform.                    " Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
form z_handle_hotspot  using    p_e_row_id
                                p_e_column_id
                                p_es_row_no.

endform.                    " Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*

form z_handle_toolbar  using    p_e_object
                                p_e_interactive.

endform.                    " Z_HANDLE_TOOLBAR
