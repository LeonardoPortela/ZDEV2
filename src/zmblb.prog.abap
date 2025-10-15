*&---------------------------------------------------------------------*
*& Report  ZMBLB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmblb.
*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
tables: sscrfields, lfa1, mseg, makt, j_1bbranch.

types: begin of ty_saida,
         bukrs  type mseg-bukrs,
         werks  type mseg-werks,
         lifnr  type mseg-lifnr,
         name1  type lfa1-name1,
         matnr  type mseg-matnr,
         maktx  type makt-maktx,
         lgort  type mseg-lgort,
         charg  type mseg-charg,
         menge  type mseg-menge,
         meins  type mseg-meins,
         salk31 type ckmlcr-salk3,
         salk34 type ckmlcr-salk3,
       end   of ty_saida,

       begin of ty_saida_det,
         lifnr      type mseg-lifnr,
         name1      type lfa1-name1,
         matnr      type mseg-matnr,
         maktx      type makt-maktx,
         mblnr      type mseg-mblnr,
         werks      type mseg-werks,
         name       type j_1bbranch-name,
         lgort      type mseg-lgort,
         charg      type mseg-charg,
         bwart      type mseg-bwart,
         shkzg      type mseg-shkzg,
         menge      type mseg-menge,
         budat_mkpf type mseg-budat_mkpf,
         bukrs      type mseg-bukrs,
         meins      type mseg-meins,
       end   of ty_saida_det,


       begin of ty_lfa1,
         lifnr type lfa1-lifnr,
         name1 type lfa1-name1,
       end   of ty_lfa1,

       begin of ty_makt,
         matnr     type makt-matnr,
         descricao type makt-maktx,
       end   of ty_makt,

       begin of ty_j_1bbranch,
         bukrs  type j_1bbranch-bukrs,
         branch type j_1bbranch-branch,
         name   type j_1bbranch-name,
       end   of ty_j_1bbranch.


*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

type-pools: slis,
            kkblo.

data: repid           like sy-repid.
data: fieldcat       type slis_t_fieldcat_alv with header line,
      t_fieldcatalog type lvc_t_fcat,
      w_fieldcatalog type lvc_s_fcat.
data: layout          type slis_layout_alv.
data: print           type slis_print_alv.
data: sort        type slis_t_sortinfo_alv with header line,
      events      type slis_t_event,
      xs_events   type slis_alv_event,
      gt_exclude  type           slis_t_extab,
      gt_exclude1 type           ui_functions.
data: w_tit(70).


data: wa_kalnr  type ckmv0_matobj_str,
      lt_kalnr  type ckmv0_matobj_tbl,
      lt_ckmlhd type table of ckmlhd,
      lt_ckmlpp like ckmlpp occurs 0 with header line,
      lt_ckmlcr like ckmlcr occurs 0 with header line.

data: it_saida         type table of ty_saida,
      it_mseg          type table of mseg,
      it_mseg_aux      type table of mseg,
      it_lfa1          type table of ty_lfa1,
      it_makt          type table of ty_makt,
      it_saida_det     type table of ty_saida_det,
      it_branch        type table of ty_j_1bbranch,
      wa_saida         type ty_saida,
      wa_mseg          type mseg,
      wa_mseg_aux      type mseg,
      wa_lfa1          type ty_lfa1,
      wa_makt          type ty_makt,
      wa_saida_det     type ty_saida_det,
      wa_branch        type ty_j_1bbranch,
      kzall            like sy-marky,
      gob_gui_alv_grid type ref to cl_gui_alv_grid,
      kzbew_werk       like sy-marky,
      hlgort           like mard-lgort,
      hwerk            like t001w-werks.

data: grid1       type ref to cl_gui_alv_grid,
      container1  type ref to cl_gui_custom_container,
      wa_layout   type lvc_s_layo,
      wa_stable   type lvc_s_stbl value 'XX',
      wa_variante type disvariant.
data:
  l_sel_button                type smp_dyntxt,
  l_sel_button_param          type smp_dyntxt,
  "Objetos
  gob_custom_container        type ref to cl_gui_custom_container,
  gob_dd_document             type ref to cl_dd_document,
  gob_splitter_container_main type ref to cl_gui_splitter_container,
  gob_splitter_container_topo type ref to cl_gui_splitter_container,

  gob_gui_container_topo      type ref to cl_gui_container,
  gob_gui_container_filtro    type ref to cl_gui_container,
  gob_gui_container_logo      type ref to cl_gui_container,
  gob_gui_container_grid      type ref to cl_gui_container,
  gob_gui_picture             type ref to cl_gui_picture,
  git_fcat                    type lvc_t_fcat,
  lines                       type sy-tabix,
  wa_selected_rows            type lvc_s_row,
  it_selected_rows            type lvc_t_row.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------

class zcl_alv_toolbar definition.

  public section.
    methods: on_click         for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.

endclass.                    "ZCL_ALV_TOOBAR IMPLEMENTATION


class zcl_alv_toolbar implementation.

  method on_click.

  endmethod.

endclass.                    "LCL_ALV_TOOLBAR DEFINITION

*-US 164011-03-02-2025-#164011-RJF-Fim
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

selection-screen begin of block b1 with frame title text-001.

  select-options: p_forn   for lfa1-lifnr,
                  p_matnr  for mseg-matnr,
                  p_werks  for mseg-werks,
                  p_bukrs  for mseg-bukrs,
                  p_data   for mseg-budat_mkpf obligatory no intervals no-extension.

selection-screen end of block b1.


at selection-screen.


at selection-screen output.
  if p_bukrs is initial and p_werks is initial.
    message 'Inform a Empresa e/ou Centro' type 'I'.
    set cursor field p_bukrs-low.
  endif.

initialization.



start-of-selection.
  if p_bukrs is not initial or p_werks is not initial.
    perform seleciona_dados.
    perform organiza_dados.
    perform chama_alv.
  endif.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
form seleciona_dados .


  data: lv_bdatj_1 type  ckmlpp-bdatj,
        lv_poper_1 type  ckmlpp-poper.

  lv_bdatj_1 = p_data-low+(4).
  lv_poper_1 = p_data-low+4(2).


  select *
    from mseg
    into table it_mseg
    where bwart  in ( '541', '542', 'Z41', 'Z42', 'Z43', 'Z44' )
  and xauto      eq 'X'
*  and lgort      eq ' '
  and lifnr      in p_forn
  and budat_mkpf le p_data-low
  and budat_mkpf gt '20200101'
  and matnr      in p_matnr
  and werks      in p_werks
  and bukrs      in p_bukrs.

  if it_mseg is not initial.
    select lifnr name1
      from lfa1
      into table it_lfa1
      for all entries in  it_mseg
      where lifnr eq it_mseg-lifnr.

    select matnr maktx
    from makt
    into table it_makt
    for all entries in  it_mseg
    where matnr eq it_mseg-matnr.


    select *
      from ckmlhd
      into table lt_ckmlhd
      for all entries in it_mseg
      where matnr eq it_mseg-matnr
      and   bwkey eq it_mseg-werks.


    loop at lt_ckmlhd into data(wa_ckmlhd).
      wa_kalnr-kalnr = wa_ckmlhd-kalnr.
      append wa_kalnr to lt_kalnr.
    endloop.

    call function 'CKMS_PERIOD_READ_WITH_ITAB'
      exporting
        i_bdatj_1               = lv_bdatj_1
        i_poper_1               = lv_poper_1
      tables
        t_kalnr                 = lt_kalnr
        t_ckmlpp                = lt_ckmlpp
        t_ckmlcr                = lt_ckmlcr
      exceptions
        no_data_found           = 1
        input_data_inconsistent = 2
        buffer_inconsistent     = 3
        others                  = 4.


  else.
  endif.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
form organiza_dados .

  data vsalk3_10          type ckmlcr-salk3.
  data vsalk3_40          type ckmlcr-salk3.
  sort it_mseg[] by bukrs werks lifnr matnr lgort charg .
  sort lt_ckmlhd by matnr bwkey.
  sort lt_ckmlpp by kalnr.

  it_mseg_aux = it_mseg.

  delete adjacent duplicates from it_mseg_aux comparing bukrs werks lifnr matnr lgort charg .

  loop at it_mseg_aux into wa_mseg_aux.

    wa_saida-bukrs = wa_mseg_aux-bukrs.
    wa_saida-werks = wa_mseg_aux-werks.
    wa_saida-lifnr = wa_mseg_aux-lifnr.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_mseg_aux-lifnr.
    if sy-subrc is initial.
      wa_saida-name1 = wa_lfa1-name1.
    endif.

    wa_saida-matnr  = wa_mseg_aux-matnr.
    read table it_makt into wa_makt with key matnr = wa_mseg_aux-matnr.
    if sy-subrc is initial.
      wa_saida-maktx = wa_makt-descricao.
    endif.

    wa_saida-lgort   = wa_mseg_aux-lgort.
    wa_saida-charg = wa_mseg_aux-charg.

    loop at it_mseg into wa_mseg where bukrs = wa_mseg_aux-bukrs and
                                       werks = wa_mseg_aux-werks and
                                       lifnr = wa_mseg_aux-lifnr and
                                       matnr = wa_mseg_aux-matnr and
*                                       lgort = wa_mseg_aux-lgort and
                                       charg = wa_mseg_aux-charg.
      if wa_mseg-shkzg eq 'H'.
        wa_saida-menge = wa_saida-menge - wa_mseg-menge.
      else.
        wa_saida-menge = wa_saida-menge + wa_mseg-menge.
      endif.
    endloop.

    read table lt_ckmlhd into data(wa_ckmlhd) with key  matnr = wa_mseg_aux-matnr
                                                        bwkey = wa_mseg_aux-werks binary search.
    if sy-subrc = 0.
      read table lt_ckmlpp into data(wa_ckmlpp) with key kalnr = wa_ckmlhd-kalnr binary search.
      if sy-subrc = 0.
        clear: vsalk3_10, vsalk3_40.
        loop at lt_ckmlcr into data(wa_ckmlcr) where kalnr = wa_ckmlhd-kalnr.
          if wa_ckmlcr-curtp = '10'.
            add wa_ckmlcr-salk3     to vsalk3_10.
          elseif wa_ckmlcr-curtp = '40'.
            add wa_ckmlcr-salk3     to vsalk3_40.
          endif.
        endloop.
        if wa_ckmlpp-lbkum ne 0.
          wa_saida-salk31 = ( vsalk3_10 / wa_ckmlpp-lbkum ) * wa_saida-menge.
          wa_saida-salk34 = ( vsalk3_40 / wa_ckmlpp-lbkum ) * wa_saida-menge.
        endif.

      endif.
    endif.

    wa_saida-meins = wa_mseg_aux-meins.
    append wa_saida to it_saida.
    clear: wa_mseg, wa_saida,  wa_mseg_aux.
  endloop.
  delete it_saida where menge <= 0.

endform.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
form chama_alv .

  refresh: fieldcat.

  perform monta_fieldcat using:
      'BUKRS'     'IT_SAIDA' 'MSEG'     ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'WERKS'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' ' ' '' ' ',
      'LIFNR'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' ' ' '' ' ',
      'NAME1'     'IT_SAIDA' 'LFA1' ' ' ' ' ' ' ' ' ' ' '' ' ',
      'MATNR'     'IT_SAIDA' 'MARC' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MAKTX'     'IT_SAIDA' 'MAKT' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'LGORT'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'CHARG'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MENGE'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' 'X' ' ' ' ',
      'MEINS'     'IT_SAIDA' 'MSEG' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'SALK31'    'IT_SAIDA' 'CKMLCR' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'SALK34'    'IT_SAIDA' 'CKMLCR' ' ' ' ' ' ' ' ' ' ' ' ' ' '.
  repid = sy-repid.
  layout-colwidth_optimize = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'COMANDO'
      it_fieldcat              = fieldcat[]
*     it_sort                  = sort[]
      is_layout                = layout
      i_grid_title             = w_tit
*     i_default                = 'X'
      i_save                   = 'X'
*     it_events                = events
      is_print                 = print
    tables
      t_outtab                 = it_saida
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.



endform.                    " CHAMA_ALV

*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
form chama_alv_det .

  refresh: fieldcat.

  perform monta_fieldcat using:
      'LIFNR'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'NAME1'      'IT_SAIDA_DET' 'LFA1'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MATNR'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MAKTX'      'IT_SAIDA_DET' 'MAKT'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MBLNR'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'WERKS'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'NAME '      'IT_SAIDA_DET' 'J_1BBRANCH' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'LGORT'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'CHARG'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'BWART'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'SHKZG'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MENGE'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'BUDAT_MKPF' 'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'BUKRS'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MEINS'      'IT_SAIDA_DET' 'MSEG'       ' ' ' ' ' ' ' ' ' ' ' ' ' '.

  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      "is_variant               = gs_variant_c
*      "it_events                = events
*      is_print                 = t_print
*    TABLES
*      t_outtab                 = gt_saida.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = repid
      i_callback_pf_status_set = 'SET_STATUS'
*     i_callback_user_command  = 'COMANDO'
      it_fieldcat              = fieldcat[]
*     it_sort                  = sort[]
      is_layout                = layout
      i_grid_title             = w_tit
*     i_default                = 'X'
      i_save                   = 'X'
*     it_events                = events
      is_print                 = print
    tables
      t_outtab                 = it_saida_det
    exceptions
      program_error            = 1
      others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.



endform.                    " CHAMA_ALV


*----------------------------------------------------------------------*
form monta_fieldcat using
               x_field x_tab x_ref x_text x_sum x_just x_qfield
               x_hotspot x_key x_zero.
*----------------------------------------------------------------------*
*

  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-do_sum        = x_sum.
  fieldcat-just          = x_just.
  fieldcat-qfieldname    = x_qfield.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-key           = x_key.
  fieldcat-no_zero       = x_zero.

  w_fieldcatalog-fieldname     = x_field.
  w_fieldcatalog-tabname       = x_tab.
  w_fieldcatalog-ref_table     = x_ref.
  w_fieldcatalog-do_sum        = x_sum.
  w_fieldcatalog-just          = x_just.
  w_fieldcatalog-qfieldname    = x_qfield.
  w_fieldcatalog-hotspot       = x_hotspot.
  w_fieldcatalog-key           = x_key.
  w_fieldcatalog-no_zero       = x_zero.
  if x_field = 'SALK31'.
    w_fieldcatalog-ref_field = 'SALK3'.
    w_fieldcatalog-scrtext_s = 'Valor BRL'.
    w_fieldcatalog-scrtext_l = w_fieldcatalog-scrtext_s.
    w_fieldcatalog-scrtext_m = w_fieldcatalog-scrtext_s.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-seltext_l = 'Valor BRL'.
    fieldcat-seltext_m = 'Valor BRL'.
    fieldcat-seltext_s = 'Valor BRL'.
  endif.
  if x_field = 'SALK34'.
    w_fieldcatalog-ref_field = 'SALK3'.
    w_fieldcatalog-scrtext_s = 'Valor USD'.
    w_fieldcatalog-scrtext_l = w_fieldcatalog-scrtext_s.
    w_fieldcatalog-scrtext_m = w_fieldcatalog-scrtext_s.
    fieldcat-ref_fieldname = 'SALK3'.
    fieldcat-seltext_l = 'Valor USD'.
    fieldcat-seltext_m = 'Valor USD'.
    fieldcat-seltext_s = 'Valor USD'.
  endif.


  append fieldcat.
  append w_fieldcatalog to t_fieldcatalog.
  clear: fieldcat, w_fieldcatalog.
*
endform.                               " MONTA_FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  comando
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
form comando using ucomm like sy-ucomm
                        selfield type slis_selfield.

  refresh it_saida_det.
  read table it_saida into data(wa_saida) index selfield-tabindex.

  if sy-subrc is initial.
    loop at it_mseg into wa_mseg where bukrs = wa_saida-bukrs and
                                       werks = wa_saida-werks and
                                       lifnr = wa_saida-lifnr and
                                       matnr = wa_saida-matnr and
*                                       lgort = wa_saida-lgort and
                                       charg = wa_saida-charg.

      wa_saida_det-lifnr      = wa_mseg-lifnr.
      wa_saida_det-name1      = wa_saida-name1.
      wa_saida_det-matnr      = wa_mseg-matnr.

      read table it_makt into wa_makt with key matnr = wa_mseg-matnr.
      if sy-subrc is initial.
        wa_saida_det-maktx      = wa_makt-descricao.
      endif.
      wa_saida_det-mblnr      = wa_mseg-mblnr.
      wa_saida_det-werks      = wa_mseg-werks.

      read table it_branch into wa_branch with key bukrs = wa_mseg-bukrs
                                                   branch = wa_mseg-werks.
      if sy-subrc is initial.
        wa_saida_det-name       = wa_branch-name.
      endif.
      wa_saida_det-lgort      = wa_mseg-lgort.
      wa_saida_det-charg      = wa_mseg-charg.
      wa_saida_det-bwart      = wa_mseg-bwart.
      wa_saida_det-shkzg      = wa_mseg-shkzg.
      if wa_mseg-shkzg = 'H'.
        wa_saida_det-menge      = wa_mseg-menge * -1.
      else.
        wa_saida_det-menge      = wa_mseg-menge .
      endif.
      wa_saida_det-budat_mkpf = wa_mseg-budat_mkpf.
      wa_saida_det-bukrs      = wa_mseg-bukrs.
      wa_saida_det-meins      = wa_mseg-meins.

      append wa_saida_det to it_saida_det.

    endloop.
    perform chama_alv_det .
  endif.

endform.                    "comando

*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
form set_status using pf_tab type slis_t_extab.

  set pf-status 'STANDARD'.

endform.                    "SET_STATUS





*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0100 output.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
  endcase.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100_exit input.
  leave to screen 0.
endmodule.
*&---------------------------------------------------------------------*
*& Form fm_cria_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'SET PF-STATUS'.
  set titlebar  'TITLE_0200'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  case sy-ucomm.
    when 'BACK'.
      set screen 0.
      leave screen.
  endcase.
endmodule.
