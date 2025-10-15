*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 18/08/2010                                              &*
*& Descrição: Relatório de consulta de estoque                        &*
*& Transação: ZMM0028                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         03.08.2010                            &*
*&--------------------------------------------------------------------&*

report  zmmr028.
include <icon>.
type-pools: vrm, slis.
types: begin of ty_ind_matnr,
       box,
       matnr type mara-matnr,
       maktx type makt-maktx,
       end of ty_ind_matnr,

       begin of ty_ind_werks,
       box,
       werks type t001w-werks,
       name1 type t001w-name1,
       end of ty_ind_werks,

       begin of ty_ind_lgort,
       box,
       werks type t001w-werks,
       lgort type t001l-lgort,
       lgobe type t001l-lgobe,
       end of ty_ind_lgort,

       begin of ty_parametros,
         matnr type mara-matnr,
         werks type t001w-werks,
         lgort type t001l-lgort,
       end of ty_parametros,

       begin of ty_dt_mov,
         werks type t001w-werks,
         lgort type mardh-lgort,
         lfmon type mardh-lfmon,
         lfgja type mardh-lfgja,
       end of ty_dt_mov,

       begin of ty_mkpf,
        mblnr type mkpf-mblnr,
        mjahr type mkpf-mjahr,
        budat type mkpf-budat,
       end of ty_mkpf,

       begin of ty_mseg,
        mblnr type mseg-mblnr,
        mjahr type mseg-mjahr,
        bwart type mseg-bwart,
        matnr type mseg-matnr,
        werks type mseg-werks,
        shkzg type mseg-shkzg,
        menge type mseg-menge,
        lgort type mseg-lgort,
       end of ty_mseg,

       begin of ty_mslb,
         matnr type mslb-matnr,
         werks type mslb-werks,
         lifnr type mslb-lifnr,
         lblab type mslb-lblab,
         lbvla type mslb-lbvla,
         charg type mslb-charg,
       end of ty_mslb,

       begin of ty_lfa1,
         lifnr type lfa1-lifnr,
         name1 type lfa1-name1,
       end of ty_lfa1,

       begin of ty_saida,
         butxt type t001-butxt,
         name1 type zmmt0019-nome,
         tipo(30),                  "PORTOS
         tipo1(30),                 "TERMINAIS
         tipo2(30),                 "DEPOSITOS
         labst type mard-labst,
         speme type mard-speme,
         afixa type mseg-menge,
         datae type mkpf-budat,
       end of ty_saida.

types: begin of ty_estrutura.
include type slis_fieldcat_main.
include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

* create container for alv-tree
data: l_tree_container_name(30) type c,
      l_custom_container type ref to cl_gui_custom_container.
l_tree_container_name = 'TREE1'.

data : container1        type ref to cl_gui_docking_container,
      ob_html_cntrl      type ref to cl_gui_html_viewer,
      g_parent_html      type ref to cl_gui_container,
      ob_splitter   type ref to cl_gui_splitter_container,
      g_parent_grid type ref to cl_gui_container,
      container_pic type ref to cl_gui_custom_container,
      picture type ref to cl_gui_picture.

data: tree1  type ref to cl_gui_alv_tree,
      gt_fieldcatalog type lvc_t_fcat,
      gs_fieldcatalog type lvc_s_fcat. "Fieldcatalog
data: s_layout type lvc_s_layo.
data: begin of dynpfields occurs 1.
        include structure dynpread.
data: end of dynpfields.

data: tg_0018 type table of zmmt0018 with header line,
      tg_0019 type table of zmmt0019 with header line,
      tg_0020 type table of zmmt0020 with header line,
      tg_0021 type table of zmmt0021 with header line,
      tg_ind_matnr type table of ty_ind_matnr with header line,
      tg_ind_matnr_old type table of ty_ind_matnr with header line,
      tg_ind_werks type table of ty_ind_werks with header line,
      tg_ind_werks_old type table of ty_ind_werks with header line,
      tg_ind_lgort type table of ty_ind_lgort with header line,
      tg_ind_lgort_old type table of ty_ind_lgort with header line,
      tg_parametros type table of ty_parametros with header line,
      tg_marc      type table of marc with header line,
      tg_mard     type table of mard with header line,
      tg_mardh     type table of mardh with header line,
      tg_mseg      type table of ty_mseg with header line,
      tg_setleaf   type table of setleaf with header line,
      tg_mkpf      type table of ty_mkpf with header line,
      tg_mslb      type table of ty_mslb with header line,
      tg_lfa1      type table of ty_lfa1 with header line,
      tg_saida     type table of ty_saida with header line,
      tg_armazem   type table of ty_saida with header line,
      tg_porto     type table of zmmt0019 with header line,
      tg_lgort     type table of zmmt0020 with header line,
      tw_saida     type table of ty_saida with header line,
      tg_zsdt_cent type table of zsdt_depara_cen with header line,
      tg_dt_mov    type table of ty_dt_mov with header line,
      wg_t001      type t001,
      p_bukrs      type t001-bukrs,
      p_col_matnr  type zmmt0018-maktx,
      p_col_matnr_old  type zmmt0018-maktx,
      p_ind_matnr  type mara-matnr,
*      p_matnr_high TYPE mara-matnr,
      p_col_werks  type zmmt0019-tipo,
      p_col_werks_old  type zmmt0019-tipo,
      p_ind_werks  type t001w-werks,
*      p_werks_high TYPE t001w-werks,
      p_col_lgort  type zmmt0020-nome,
      p_col_lgort_old type zmmt0020-nome,
      p_ind_lgort  type zmmt0020-lgort,
      p_porto(30),
      p_porto_low(30),
      p_porto_high(30),
      init,
      wg_cont type sy-tabix,
      wg_desc_mat(25),
      wg_desc_fil(25),
      wg_desc_dep(25).

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
data: xs_events    type slis_alv_event,
      events       type slis_t_event,
      t_print      type slis_print_alv,
      estrutura    type table of ty_estrutura,
      wa_estrutura type ty_estrutura,
      v_report     like sy-repid,
      wa_layout    type slis_layout_alv,
      t_top        type slis_t_listheader,
      t_sort       type slis_t_sortinfo_alv with header line.

call screen 100.

*-----------------------------------------------------------------*
*                      DEFINITION                                 *
*-----------------------------------------------------------------*
* Classe para implementação do Cabeçalho HTML
class lcl_event_handler definition .
  public section .

    methods handle_top_of_page
      for event top_of_page of cl_gui_alv_tree.

*    methods handle_end_of_list
*       for event end_of_list of cl_gui_alv_tree


*    methods print_top_of_page    for event print_top_of_page
*                              of cl_gui_alv_tree.


endclass .                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler implementation .

*** TOP-OF-PAGE
  method handle_top_of_page.
    data: gt_pageheader type slis_t_listheader,
          ls_line       type slis_listheader.

    ls_line-typ = 'H'.
    ls_line-info = 'Teste'.
    append ls_line to gt_pageheader.

    call function 'REUSE_ALV_COMMENTARY_WRITE'
      exporting
        it_list_commentary = gt_pageheader.

*    data: tab  type ref to cl_dd_table_element,
*          col1 type ref to cl_dd_area ,
*          col2 type ref to cl_dd_area,
*          col3 type ref to cl_dd_area,
*          col4 type ref to cl_dd_area.
*
*    data: vl_text type sdydo_text_element.
*
*    constants c_sapstyle type sdydo_attribute value 'HEADER1'.
*
*    call method e_dyndoc_id->initialize_document .
*
*
**** Cria uma tabela com "n" colunas.
*    call method e_dyndoc_id->add_table
*      exporting
*        no_of_columns               = 2
*        border                      = '0'
*        width                       = '100%'
*        cell_background_transparent = 'X'
*      importing
*        table                       = tab.
*
*
*    call method tab->add_column
*      exporting
*        width  = '10%'
*      importing
*        column = col1.
*
*    call method tab->add_column
*      exporting
*        width  = '40%'
*      importing
*        column = col2.
*
*    call method tab->add_column
*      exporting
*        width  = '10%'
*      importing
*        column = col3.
*
*    call method tab->add_column
*      exporting
*        width  = '40%'
*      importing
*        column = col4.
*
** Posiciona texto nas colunas do html
*    do 2 times.
*
*      call method tab->set_column_style
*        exporting
*          col_no     = sy-index
*          sap_align  = 'LEFT'
*          sap_valign = 'TOP'.
*    enddo.
*
*data:  w_repid(255) type c value sy-repid.
** Escreve texto do html
*    call method col1->add_text
*      exporting
*        text         = w_repid
*        sap_emphasis = 'STRONG'.
*
*    vl_text = 'Relatório de Acidentes por período'.
*
*    call method col2->add_text
*      exporting
*        text = vl_text.
**        sap_style = 'HEADING'.
*
*    call method tab->new_row.
*
*    write 'Data' to vl_text.
*
*    call method col1->add_text
*      exporting
*        text         = vl_text
*        sap_emphasis = 'STRONG'.
*
*
*    write sy-datum to vl_text.
*
*    call method col2->add_text
*      exporting
*        text = vl_text.
**        sap_style = 'HEADING'.
*
*    call method tab->new_row.
*
*    write 'Hora' to vl_text.
*
*    call method col1->add_text
*      exporting
*        text         = vl_text
*        sap_emphasis = 'STRONG'.
*
*    write sy-uzeit to vl_text.
*
*    call method col2->add_text
*      exporting
*        text = vl_text.
**        sap_style = 'HEADING'.
*
*    call method tab->new_row.

  endmethod.                    "handle_top_of_page

endclass .                    "lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  case sy-ucomm.
    when 'GET_BUK'.
      clear: p_col_matnr, p_col_lgort, p_col_werks,
             p_ind_matnr, p_ind_lgort, p_ind_werks.
      perform update_matnr.
      perform carrega_centros using 'X'.
      perform update_lgort.
    when 'GET_MATNR'.
      perform carrega_materias.
    when 'GET_WERKS'.
      perform carrega_centros using space.
      perform update_lgort.
    when 'GET_LGORT'.
      perform carrega_depositos using space.
    when 'RUN'.
      perform seleciona_dados.
    when 'BACK'
      or 'EXIT'.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: tl_0018 type table of zmmt0018 with header line,
        tl_0019 type table of zmmt0019 with header line,
        tl_0020 type table of zmmt0020 with header line,
        tl_0022 type table of zmmt0022 with header line,
        tl_values_18       type vrm_values with header line,
        tl_values_19       type vrm_values with header line,
        tl_values_20       type vrm_values with header line,
        tl_values_22       type vrm_values with header line.
  set pf-status 'Z001'.
  set titlebar 'Z001'.


  if init is initial.
    perform cria_logo.
    select *
      from zmmt0018
      into table tl_0018.
*       where bukrs in rgl_col_bukrs.

    select *
      from zmmt0019
      into table tl_0019.

    select *
      from zmmt0020
      into table tl_0020.

    select *
      from zmmt0022
      into table tl_0022.


    loop at tl_0018.
      move: tl_0018-maktx to tl_values_18-key.
      append tl_values_18.
      clear: tl_values_18.
    endloop.

    sort tl_values_18 by key.
    delete adjacent duplicates from tl_values_18.

    call function 'VRM_SET_VALUES'
      exporting
        id              = 'P_COL_MATNR'
        values          = tl_values_18[]
      exceptions
        id_illegal_name = 1
        others          = 2.

    loop at tl_0019.
      move: tl_0019-tipo to tl_values_19-key.

      if tl_values_19-key eq  'PO'.
        move: 'Porto' to tl_values_19-text.
      elseif tl_values_19-key eq  'FI'.
        move: 'Filial' to tl_values_19-text.
      elseif tl_values_19-key eq  'FA'.
        move: 'Fabrica' to tl_values_19-text.
      endif.
      append tl_values_19.
      clear: tl_values_19.
    endloop.
    sort tl_values_19 by key.
    delete adjacent duplicates from tl_values_19.

    call function 'VRM_SET_VALUES'
      exporting
        id              = 'P_COL_WERKS'
        values          = tl_values_19[]
      exceptions
        id_illegal_name = 1
        others          = 2.

    loop at tl_0020.
      move: tl_0020-nome to tl_values_20-key.
      append tl_values_20.
      clear: tl_values_20.
    endloop.
    sort tl_values_20 by key.
    delete adjacent duplicates from tl_values_20.

    call function 'VRM_SET_VALUES'
      exporting
        id              = 'P_COL_LGORT'
        values          = tl_values_20[]
      exceptions
        id_illegal_name = 1
        others          = 2.

    loop at tl_0022.
      move: tl_0022-bukrs to tl_values_22-key,
            tl_0022-nome  to tl_values_22-text.
      append tl_values_22.
      clear: tl_values_22.
    endloop.

    sort tl_values_18 by key.
    delete adjacent duplicates from tl_values_18.

    call function 'VRM_SET_VALUES'
      exporting
        id              = 'P_BUKRS'
        values          = tl_values_22[]
      exceptions
        id_illegal_name = 1
        others          = 2.

    perform carrega_centros using space.
    perform carrega_depositos using space.
    init = 'X'.
  endif.
  if  sy-ucomm ne 'BACK'
  and sy-ucomm ne 'EXIT'
  and sy-ucomm ne '&ONT'.
*    IF tg_ind_matnr[] IS INITIAL.
    perform carrega_materias.
*    ENDIF.

**    IF tg_ind_werks[] IS INITIAL.
*    perform carrega_centros.
**    ENDIF.
*
**    IF tg_ind_lgort[] IS INITIAL.
*    perform carrega_depositos.
**    ENDIF.
  endif.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_material input.
  data: tl_fieldtab type table of dynpread with header line,
        wl_cont_aux(5).

  refresh tl_fieldtab.
  clear: tl_fieldtab, wl_cont_aux.
*  DATA: tl_makt TYPE TABLE OF makt WITH HEADER LINE.
*
*  RANGES: rgl_col_matnr FOR p_col_matnr.
*
*  REFRESH: tl_0018, tl_makt, tg_ind_matnr, dynpfields, rgl_col_matnr.
*  CLEAR: tl_0018, tl_makt, tg_ind_matnr, dynpfields, rgl_col_matnr, wg_cont, WA_LAYOUT..
*
*  MOVE 'P_COL_MATNR'  TO dynpfields-fieldname.
*  APPEND dynpfields.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname                   = sy-repid
*      dynumb                   = sy-dynnr
*      perform_input_conversion = 'X'
*    TABLES
*      dynpfields               = dynpfields
*    EXCEPTIONS
*      invalid_abapworkarea     = 1
*      invalid_dynprofield      = 2
*      invalid_dynproname       = 3
*      invalid_dynpronummer     = 4
*      invalid_request          = 5
*      no_fielddescription      = 6
*      invalid_parameter        = 7
*      undefind_error           = 8
*      OTHERS                   = 9.
*
*  READ TABLE dynpfields
*      WITH KEY fieldname = 'P_COL_MATNR'.
*  IF sy-subrc IS INITIAL
*  and dynpfields-fieldvalue is not initial.
*    rgl_col_matnr-sign    = 'I'.
*    rgl_col_matnr-option  = 'EQ'.
*    rgl_col_matnr-low     = dynpfields-fieldvalue.
*
*    APPEND rgl_col_matnr.
*    CLEAR: rgl_col_matnr.
*  ENDIF.
*
*  SELECT *
*    FROM zmmt0018
*    INTO TABLE tl_0018
*     WHERE maktx IN rgl_col_matnr.
*
*  IF sy-subrc IS INITIAL.
*    SELECT *
*      FROM makt
*      INTO TABLE tl_makt
*       FOR ALL ENTRIES IN tl_0018
*       WHERE matnr EQ tl_0018-matnr.
*
*  ENDIF.
*
*  SORT: tl_makt BY matnr.
*
*  LOOP AT tl_0018.
*    READ TABLE tl_makt
*      WITH KEY matnr = tl_0018-matnr
*               BINARY SEARCH.
*
*    MOVE: tl_0018-matnr TO tg_ind_matnr-matnr,
*          tl_makt-maktx TO tg_ind_matnr-maktx.
*
*    APPEND tg_ind_matnr.
*    CLEAR tg_ind_matnr.
*  ENDLOOP.
**  IF tg_ind_matnr[] IS INITIAL.
**    PERFORM carrega_materias.
**  ENDIF.

  describe table tg_ind_matnr lines wg_cont.
  if wg_cont gt 1.
    perform montar_layout using 'MATNR'.

    wa_layout-box_fieldname     = 'BOX'.
    wa_layout-box_tabname       = 'TG_IND_MATNR'.
    call function 'REUSE_ALV_GRID_DISPLAY'
         exporting
              i_callback_program       = sy-repid
*              I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND'
              it_fieldcat              = estrutura[]
*            IT_SORT                 = T_SORT[]
              i_default                = ' '
              i_save                   = ' '
*            IT_EVENTS               = EVENTS
              i_screen_start_column    = 10
              i_screen_start_line      = 3
              i_screen_end_column      = 80
              i_screen_end_line        = 20
              is_layout                = wa_layout
         tables
              t_outtab                 = tg_ind_matnr.

    move: 'P_IND_MATNR' to tl_fieldtab-fieldname,
           space        to tl_fieldtab-fieldvalue.
    append tl_fieldtab.

    call function 'DYNP_VALUES_UPDATE'
      exporting
        dyname               = sy-repid
        dynumb               = sy-dynnr
      tables
        dynpfields           = tl_fieldtab
      exceptions
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        others               = 8.

    clear: p_ind_matnr.
  elseif wg_cont eq 1.
    read table tg_ind_matnr index 1.
    move tg_ind_matnr-matnr to p_ind_matnr.
  endif.
endmodule.                 " BUSCA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0446   text
*----------------------------------------------------------------------*
form montar_layout  using    value(p_tipo).

  if p_tipo eq 'MATNR'.
    refresh: estrutura.
    perform montar_estrutura using:
*    1  ' '   ' '    'TG_IND_MATNR' 'BOX'     ' '  ' ',
    2  'MAKT'   'MATNR'    'TG_IND_MATNR' 'MATNR'    'Nr.Material' ' ' ' ',
    3  'MAKT'   'MAKTX'    'TG_IND_MATNR' 'MAKTX'     'Descrição do Material'  ' ' ' '.

  elseif p_tipo eq 'WERKS'.
    refresh: estrutura.
    perform montar_estrutura using:
*    1  ' '   ' '    'TG_IND_WERKS' 'BOX'     ' '  ' ' ' ',
    2  'T001W'   'WERKS'    'TG_IND_WERKS' 'WERKS'     'Centro' ' ' ' ',
    3  'T001W'   'NAME1'    'TG_IND_WERKS' 'NAME1'     'Nome do Centro'  ' ' ' '.
  elseif p_tipo eq 'LGORT'.
    refresh: estrutura.
    perform montar_estrutura using:
*    1  ' '   ' '    'TG_IND_WERKS' 'BOX'     ' '  ' ',
    1  'T001W'   'WERKS'    'TG_IND_LGORT' 'WERKS'     'Centro' ' ' ' ',
    2  'T001L'   'LGORT'    'TG_IND_LGORT' 'LGORT'     'Depósito' ' ' ' ',
    3  'T001L'   'LGOBE'    'TG_IND_LGORT' 'LGOBE'     'Nome do Depósito'  ' ' ' '.
  elseif p_tipo eq 'SAIDA'.
    refresh: gt_fieldcatalog.
    perform montar_estrutura_oo using:
    1  'MARD'   'LABST'    'TW_SAIDA' 'LABST'     'Disponivel' '35' 'X',
    2  'MARD'   'SPEME'    'TW_SAIDA' 'SPEME'     'Bloqueado' '35' 'X',
    3  'MSEG'   'MENGE'    'TW_SAIDA' 'AFIXA'     'A Fixar'  '35' 'X'.
*    3  'MKPF'   'BUDAT'    'TW_SAIDA' 'DATAE'     'Data Estoque'  ' ' ' '.
  endif.
endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_sum).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

*  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

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
*  WA_ESTRUTURA-OUTPUTLEN     = X_CON.


  append wa_estrutura to estrutura.
  clear: wa_estrutura.
endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  BUSCA_CENTROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_centros input.
* data: tl_fieldtab type table of dynpread with header line.
  refresh: tl_fieldtab.
  clear: tl_fieldtab.
*  IF tg_ind_werks[] IS INITIAL.
*    PERFORM carrega_centros.
*  ENDIF.

  describe table tg_ind_werks lines wg_cont.

  if wg_cont gt 1.
    perform montar_layout using 'WERKS'.

    wa_layout-box_fieldname     = 'BOX'.
    wa_layout-box_tabname       = 'TG_IND_WERKS'.
    call function 'REUSE_ALV_GRID_DISPLAY'
         exporting
              i_callback_program       = sy-repid
*              I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND'
              it_fieldcat              = estrutura[]
*            IT_SORT                 = T_SORT[]
              i_default                = ' '
              i_save                   = ' '
*            IT_EVENTS               = EVENTS
              i_screen_start_column    = 10
              i_screen_start_line      = 3
              i_screen_end_column      = 80
              i_screen_end_line        = 20
              is_layout                = wa_layout
         tables
              t_outtab                 = tg_ind_werks.

    move: 'P_IND_WERKS' to tl_fieldtab-fieldname,
           space        to tl_fieldtab-fieldvalue.
    append tl_fieldtab.

    call function 'DYNP_VALUES_UPDATE'
      exporting
        dyname               = sy-repid
        dynumb               = sy-dynnr
      tables
        dynpfields           = tl_fieldtab
      exceptions
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        others               = 8.

    clear: p_ind_werks.
  elseif wg_cont eq 1.
    read table tg_ind_werks index 1.
    move tg_ind_werks-werks to p_ind_werks.
  endif.

  perform update_lgort.
endmodule.                 " BUSCA_CENTROS  INPUT
*&---------------------------------------------------------------------*
*&      Module  BUSCA_DEPOSITO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module busca_deposito input.
* data: tl_fieldtab type table of dynpread with header line.

  refresh tl_fieldtab.
  clear: tl_fieldtab.
*  IF tg_ind_lgort[] IS INITIAL.
  perform carrega_depositos using space.
*  ENDIF.

  describe table tg_ind_lgort lines wg_cont.

  if wg_cont gt 1.
    perform montar_layout using 'LGORT'.

    wa_layout-box_fieldname     = 'BOX'.
    wa_layout-box_tabname       = 'TG_IND_LGORT'.
    call function 'REUSE_ALV_GRID_DISPLAY'
         exporting
              i_callback_program       = sy-repid
*              I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND'
              it_fieldcat              = estrutura[]
*            IT_SORT                 = T_SORT[]
              i_default                = ' '
              i_save                   = ' '
*            IT_EVENTS               = EVENTS
              i_screen_start_column    = 10
              i_screen_start_line      = 3
              i_screen_end_column      = 80
              i_screen_end_line        = 20
              is_layout                = wa_layout
         tables
              t_outtab                 = tg_ind_lgort.

    move: 'P_IND_LGORT' to tl_fieldtab-fieldname,
           space        to tl_fieldtab-fieldvalue.
    append tl_fieldtab.
    clear tl_fieldtab.

    move: 'WG_DESC_DEP'     to tl_fieldtab-fieldname.
    wl_cont_aux = wg_cont.
    shift wl_cont_aux left deleting leading '0'.
    condense wl_cont_aux no-gaps.
    if wg_cont eq 1.
      concatenate '@28@' wl_cont_aux 'Registro select.'
       into tl_fieldtab-fieldvalue separated by space.
    elseif wg_cont gt 1.
      concatenate '@28@' wl_cont_aux 'Registros select.'
     into tl_fieldtab-fieldvalue separated by space.
    endif.
    append tl_fieldtab.
    clear tl_fieldtab.

    call function 'DYNP_VALUES_UPDATE'
      exporting
        dyname               = sy-repid
        dynumb               = sy-dynnr
      tables
        dynpfields           = tl_fieldtab
      exceptions
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        others               = 8.

    wg_desc_dep = 'TESTE'.
    clear: p_ind_lgort.
  elseif wg_cont eq 1.
    read table tg_ind_lgort index 1.
    move tg_ind_lgort-lgort to p_ind_lgort.
  endif.
endmodule.                 " BUSCA_DEPOSITO  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .
  ranges: rgl_bwart for mseg-bwart.

  data: wl_data type sy-datum,
        tg_ind_werks_aux like table of tg_ind_werks with header line,
        tg_parametros_aux like table of tg_parametros with header line,
        wl_tabix type sy-tabix.

  refresh: tg_parametros, tg_marc, tg_mardh, tg_mseg, tg_setleaf, tg_mkpf, tg_mslb,
           tg_lfa1, tg_saida, tg_armazem, tg_porto, tg_lgort, tg_zsdt_cent, tg_parametros_aux.

  if tg_ind_werks[] is initial.
    perform carrega_centros using space.
  endif.

  if tg_ind_werks[] is not initial.
    tg_ind_werks_aux[] = tg_ind_werks[].
    delete tg_ind_werks_aux where box is initial.

    select *
      from zsdt_depara_cen
      into table tg_zsdt_cent
       for all entries in tg_ind_werks_aux
        where vkorg eq p_bukrs
          and centro_real eq tg_ind_werks_aux-werks.
  endif.

  loop at tg_ind_matnr where box is not initial.
    loop at tg_ind_werks. "WHERE box IS NOT INITIAL.
      wl_tabix = sy-tabix.
      read table tg_zsdt_cent
        with key centrov_1 = tg_ind_werks-werks.
      if sy-subrc is initial
      or tg_ind_werks-box is not initial.
*     read table tg_ind_lgort
*       with key werks =
        move: tg_ind_matnr-matnr to tg_parametros-matnr,
              tg_ind_werks-werks to tg_parametros-werks.

        move: 'X' to tg_ind_werks-box.

        append tg_parametros.
        modify tg_ind_werks index wl_tabix.
        clear: tg_parametros, wl_tabix.
      endif.
    endloop.
  endloop.

  read table tg_ind_lgort
    with key box = 'X'.
  if sy-subrc is initial.
    tg_parametros_aux[] = tg_parametros[].
    refresh: tg_parametros.
    loop at tg_ind_lgort
      where box is not initial.
      read table tg_parametros_aux  into tg_parametros
        with key werks = tg_ind_lgort-werks.
      if sy-subrc is initial.
        move: tg_ind_lgort-lgort to tg_parametros-lgort.

        append tg_parametros.
      endif.
    endloop.
  endif.
*  LOOP AT tg_ind_lgort.
*    MOVE: tg_ind_lgort-lgort TO tg_parametros-lgort,
*          tg_ind_lgort-werks TO tg_parametros-werks.
*
*    APPEND tg_parametros.
*    CLEAR: tg_parametros.
*  ENDLOOP.
*
*  LOOP AT tg_ind_werks.
*    MOVE: tg_ind_werks-werks TO tg_parametros-werks.
*
*    APPEND tg_parametros.
*    CLEAR: tg_parametros.
*  ENDLOOP.

  select single *
    from t001
    into wg_t001
     where bukrs eq p_bukrs.

  if tg_parametros[] is not initial.
** Transferencia entre filiais
    select *
      from marc
      into table tg_marc
      for all entries in tg_parametros
       where werks eq tg_parametros-werks
         and matnr eq tg_parametros-matnr.

** Armazém
    select *
      from mardh
      into table tg_mardh
       for all entries in tg_parametros
       where werks eq tg_parametros-werks
          and matnr eq tg_parametros-matnr.

    select *
          from mard
          into table tg_mard
           for all entries in tg_parametros
           where werks eq tg_parametros-werks
              and matnr eq tg_parametros-matnr.
*    wl_data = sy-datum.
*
*    SUBTRACT 1 FROM wl_data+4(2).
*    LOOP AT tg_mard.
*      IF  tg_mard-lfgja NE wl_data(4)
*      OR tg_mard-lfmon NE wl_data+4(2).
*        DELETE tg_mard.
*      ENDIF.
*    ENDLOOP.

    select *
        from setleaf
        into table tg_setleaf
        where setname eq 'MAGGI_ZMM0027'.

    select mblnr mjahr budat
      from mkpf
      into table tg_mkpf
       where mjahr eq sy-datum(4).

    loop at tg_mkpf.
      if not ( tg_mkpf-budat+4(2) eq sy-datum+4(2)
          and  tg_mkpf-budat(4)   eq sy-datum(4)
          and  tg_mkpf-budat+6(2) lt sy-datum+6(2) ).
        delete tg_mkpf index sy-tabix.
      endif.
    endloop.

    if tg_mkpf[] is not initial.
      select mblnr mjahr bwart matnr werks shkzg menge lgort
        from mseg
        into table tg_mseg
        for all entries in tg_mkpf
         where mblnr eq tg_mkpf-mblnr
           and mjahr eq tg_mkpf-mjahr.

      loop at tg_mseg.
        read table tg_setleaf
          with key valfrom = tg_mseg-bwart.
        if sy-subrc is not initial.
          delete tg_mseg.
          continue.
        endif.
        read table tg_parametros
          with key matnr = tg_mseg-matnr
                   werks = tg_mseg-werks.
        if sy-subrc is not initial.
          delete tg_mseg.
          continue.
        endif.

        read table tg_ind_lgort
          with key lgort = tg_mseg-lgort
                   box   = 'X'.
        if sy-subrc is not initial.
          delete tg_mseg.
          continue.
        endif.
      endloop.
    endif.
** Terceiros
    select matnr werks lifnr lblab lbvla charg
      from mslb
      into table tg_mslb
       for all entries in tg_parametros
       where matnr eq tg_parametros-matnr
         and werks eq tg_parametros-werks.

    if sy-subrc is initial.
      select lifnr name1
        from lfa1
        into table tg_lfa1
         for all entries in tg_mslb
         where lifnr eq tg_mslb-lifnr.

    endif.
** Portos
    select *
      from zmmt0019
      into table tg_porto
      for all entries in tg_parametros
       where werks eq tg_parametros-werks "tg_ind_werks-werks
         and tipo  eq 'PO'.

    if sy-subrc is initial.
      select *
        from zmmt0020
        into table tg_lgort
         for all entries in tg_porto
          where bukrs eq p_bukrs
            and werks eq tg_porto-werks.

    endif.
    call screen 200.
  endif.
*  MESSAGE E836(SD) WITH 'Não foram encontrados dados para sua seleção'.
endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_MATERIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form carrega_materias .

  data: tl_makt type table of makt with header line,
        wl_changed.

  ranges: rgl_col_matnr for p_col_matnr.

  refresh: dynpfields, rgl_col_matnr.
  clear: tl_0018, tl_makt, tg_ind_matnr, dynpfields, rgl_col_matnr, wg_cont, wa_layout,
         wl_changed.


  move 'P_COL_MATNR'  to dynpfields-fieldname.
  append dynpfields.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    tables
      dynpfields               = dynpfields
    exceptions
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      others                   = 9.

  read table dynpfields
      with key fieldname = 'P_COL_MATNR'.
  if sy-subrc is initial
  and dynpfields-fieldvalue is not initial.
    rgl_col_matnr-sign    = 'I'.
    rgl_col_matnr-option  = 'EQ'.
    rgl_col_matnr-low     = dynpfields-fieldvalue.

    append rgl_col_matnr.
    clear: rgl_col_matnr.
  else.
    rgl_col_matnr-low = p_col_matnr.
  endif.

  read table rgl_col_matnr index 1.
  if rgl_col_matnr-low ne p_col_matnr_old
  or rgl_col_matnr-low is initial.
    wl_changed = 'X'.
    p_col_matnr_old = rgl_col_matnr-low.

  endif.

  if wl_changed is not initial.
    refresh: tl_0018, tl_makt, tg_ind_matnr.

    select *
      from zmmt0018
      into table tl_0018
       where maktx in rgl_col_matnr.

    if sy-subrc is initial.
      select *
        from makt
        into table tl_makt
         for all entries in tl_0018
         where matnr eq tl_0018-matnr.

    endif.

    sort: tl_makt by matnr.

    loop at tl_0018.
      if p_bukrs is not initial.
        if tl_0018-bukrs ne p_bukrs.
          continue.
        endif.
      endif.
      read table tl_makt
        with key matnr = tl_0018-matnr
                 binary search.

      move: tl_0018-matnr to tg_ind_matnr-matnr,
            tl_makt-maktx to tg_ind_matnr-maktx,
            'X'           to tg_ind_matnr-box.

      append tg_ind_matnr.
      clear tg_ind_matnr.
    endloop.
    describe table tg_ind_matnr lines wg_cont.

    clear: p_ind_matnr.
    if wg_cont eq 1.
      read table tg_ind_matnr index 1.
      move tg_ind_matnr-matnr to p_ind_matnr.
    endif.
  endif.
endform.                    " CARREGA_MATERIAS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_CENTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form carrega_centros using p_flag.
  data: tl_t001w type table of t001w with header line,
        wl_changed.

  ranges: rgl_col_werks for p_col_werks,
          rgl_bukrs for p_bukrs.

  refresh: dynpfields, rgl_bukrs.
  clear: wa_layout, dynpfields, tl_0019, tl_t001w, rgl_col_werks,
         wl_changed, rgl_bukrs.

  move 'P_COL_WERKS'  to dynpfields-fieldname.
  append dynpfields.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    tables
      dynpfields               = dynpfields
    exceptions
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      others                   = 9.

  read table dynpfields
      with key fieldname = 'P_COL_WERKS'.
  if sy-subrc is initial
  and dynpfields-fieldvalue is not initial.
    rgl_col_werks-sign    = 'I'.
    rgl_col_werks-option  = 'EQ'.
    rgl_col_werks-low     = dynpfields-fieldvalue.

    append rgl_col_werks.
    clear: rgl_col_werks.
  else.
    rgl_col_werks-low = p_col_werks.
  endif.

  if p_flag is not initial.
    refresh: rgl_col_werks.
    clear: rgl_col_werks.
  endif.

  read table rgl_col_werks index 1.
  if rgl_col_werks-low ne p_col_werks_old
  or rgl_col_werks-low is initial.
    wl_changed = 'X'.
    if sy-ucomm ne 'GET_WERKS'.
      p_col_werks_old = rgl_col_werks-low.
    endif.
  endif.

  if wl_changed is not initial.
    refresh: tl_0019, tl_t001w, tg_ind_werks.

    if p_bukrs is not initial.
      rgl_bukrs-sign    = 'I'.
      rgl_bukrs-option  = 'EQ'.
      rgl_bukrs-low     = p_bukrs.

      append rgl_bukrs.
      clear: rgl_bukrs.
    endif.

    select *
      from zmmt0019
      into table tl_0019
       where tipo in rgl_col_werks
         and bukrs in rgl_bukrs.

    if sy-subrc is initial.
      select *
        from t001w
        into table tl_t001w
        for all entries in tl_0019
         where werks eq tl_0019-werks.
    endif.

    sort: tl_t001w by werks.

    loop at tl_0019.
      read table tl_t001w
        with key werks = tl_0019-werks
                 binary search.

      move: tl_0019-werks  to tg_ind_werks-werks,
*          tl_t001w-name1 TO tg_ind_werks-name1,
            tl_0019-nome   to tg_ind_werks-name1,
            'X'            to tg_ind_werks-box.

      append tg_ind_werks.
      clear: tg_ind_werks.
    endloop.

    describe table tg_ind_werks lines wg_cont.
    clear: p_ind_werks.
    if wg_cont eq 1.
      read table tg_ind_werks index 1.
      move tg_ind_werks-werks to p_ind_werks.
    endif.
  endif.
endform.                    " CARREGA_CENTROS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_DEPOSITOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form carrega_depositos using p_flag .
  data: tl_t001l type table of t001l with header line,
        tl_ind_werks like table of tg_ind_werks with header line,
        wl_changed,
        wl_cont_old type sy-tabix,
        wl_cont     type sy-tabix,
        wl_cont_aux(5).

  ranges: rgl_col_lgort for p_col_lgort.

  refresh: dynpfields." tl_0020, tl_t001l, tl_ind_werks, tg_ind_lgort.
  clear: wa_layout, dynpfields, tl_0020, tl_t001l, rgl_col_lgort, wl_changed,
         wl_cont_old, wl_cont, wl_cont_aux.

  move 'P_COL_LGORT'  to dynpfields-fieldname.
  append dynpfields.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    tables
      dynpfields               = dynpfields
    exceptions
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      others                   = 9.

  read table dynpfields
      with key fieldname = 'P_COL_LGORT'.
  if sy-subrc is initial
  and dynpfields-fieldvalue is not initial.
    rgl_col_lgort-sign    = 'I'.
    rgl_col_lgort-option  = 'EQ'.
    rgl_col_lgort-low     = dynpfields-fieldvalue.

    append rgl_col_lgort.
    clear: rgl_col_lgort.
  else.
    rgl_col_lgort-low = p_col_lgort.
  endif.

  if p_flag is not initial.
    refresh: rgl_col_lgort.
    clear: rgl_col_lgort.
  endif.

  read table rgl_col_lgort index 1.
  if rgl_col_lgort-low ne p_col_lgort_old
  or rgl_col_lgort-low is initial.
    wl_changed = 'X'.
    if sy-ucomm ne 'GET_LGORT'.
      p_col_lgort_old = rgl_col_lgort-low.
    endif.
  else.
    refresh: tl_ind_werks.
    tl_ind_werks[] = tg_ind_werks[].
    delete tl_ind_werks where box is initial.
    describe table tg_ind_werks_old lines wl_cont_old.
    describe table tl_ind_werks lines wl_cont.
    if wl_cont_old eq wl_cont.
      loop at tl_ind_werks.
        read table tg_ind_werks_old
          with key werks = tl_ind_werks-werks.
        if sy-subrc is not initial.
          wl_changed = 'X'.
          tg_ind_werks_old[] = tl_ind_werks[].
          exit.
        endif.
      endloop.
    else.
      wl_changed = 'X'.
      tg_ind_werks_old[] = tl_ind_werks[].
    endif.
  endif.

  if wl_changed is not initial.
    refresh: tl_0020, tl_t001l, tg_ind_lgort.
    tl_ind_werks[] = tg_ind_werks[].
    delete tl_ind_werks where box is initial.


    select *
      from zmmt0020
      into table tl_0020
      for all entries in tl_ind_werks
       where nome in rgl_col_lgort
         and werks eq tl_ind_werks-werks.

    if sy-subrc is initial.
      select *
        from t001l
        into table tl_t001l
        for all entries in tl_0020
         where lgort eq tl_0020-lgort.
    endif.

    sort: tl_t001l by lgort.

    loop at tl_0020.
      read table tl_t001l
        with key lgort = tl_0020-lgort
                 binary search.

      move: tl_0020-werks  to tg_ind_lgort-werks,
            tl_0020-lgort  to tg_ind_lgort-lgort,
            tl_t001l-lgobe to tg_ind_lgort-lgobe,
            'X'            to tg_ind_lgort-box.

      append tg_ind_lgort.
      clear: tg_ind_lgort.
    endloop.

    describe table tg_ind_lgort lines wg_cont.
    clear: p_ind_lgort.
    if wg_cont eq 1.
      read table tg_ind_lgort index 1.
      move tg_ind_lgort-lgort to p_ind_lgort.
      wl_cont_aux = wg_cont.
      shift wl_cont_aux left deleting leading '0'.
      condense wl_cont_aux no-gaps.
      concatenate '@28@' wl_cont_aux 'Registro select.' into wg_desc_dep separated by space.
    elseif wg_cont eq 0.
      wg_desc_dep = '@26@'.
    elseif wg_cont gt 1.
      wl_cont_aux = wg_cont.
      shift wl_cont_aux left deleting leading '0'.
      condense wl_cont_aux no-gaps.
      concatenate '@28@' wl_cont_aux 'Registros select.' into wg_desc_dep separated by space.
    endif.
  endif.
endform.                    " CARREGA_DEPOSITOS
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBEJTOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_obejtos output.
  data:         event type cntl_simple_event,
            tl_events type cntl_simple_events.

  if container1 is initial.
    create object container1
      exporting
        repid     = sy-repid
        dynnr     = '0200'
        side      = container1->dock_at_top
        extension = 400.

    if sy-subrc <> 0.
      message x208(00) with 'ERROR'.                        "#EC NOTEXT
    endif.

*    create object ob_splitter
*      exporting
*        parent  = container1
*        rows    = 2
*        columns = 1.
*
*
**** Chama o método que retorna os containers do TOP e do GRID
*    call method ob_splitter->get_container
*      exporting
*        row       = 1
*        column    = 1
*      receiving
*        container = g_parent_html.
*
**** Seta o tamanho do TOP-OF-PAGE
*    call method ob_splitter->set_row_height
*      exporting
*        id     = 1
*        height = 15.
*
*    call method ob_splitter->get_container
*      exporting
*        row       = 2
*        column    = 1
*      receiving
*        container = g_parent_grid.

***** Seta o tamanho do Grid
*    call method ob_splitter->set_row_height
*      exporting
*        id     = 2
*        height = 100.
*create tree control

    create object tree1
      exporting
        parent                      = container1
        node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
        no_html_header              = ' '
        no_toolbar                  = ''
      exceptions
        cntl_system_error           = 1
        create_error                = 2
        failed                      = 3
        illegal_node_selection_mode = 4
        lifetime_error              = 5.

*  evento top-of-page
    data: ob_event_handler   type ref to lcl_event_handler .
    create object ob_event_handler .
    set handler ob_event_handler->handle_top_of_page for
                                                tree1 .
*    set handler ob_event_handler->print_top_of_page for
*                                                tree1 .

* create Hierarchy-header
    data l_hierarchy_header type treev_hhdr.

    perform build_hierarchy_header changing l_hierarchy_header.
    perform montar_layout using 'SAIDA'.
* repid for saving variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.

    data: gd_report_title type slis_t_listheader,
          ls_line type slis_listheader,
           ld_date(10) type c,
           wl_date type sy-datum,
           wl_uzeit(8).

    refresh: gd_report_title.

    clear ls_line.
    ls_line-key = 'Empresa:'.
    select single nome
      from zmmt0022
      into ls_line-info
       where bukrs = p_bukrs.

    ls_line-typ  = 'S'.
    append ls_line to gd_report_title.


    clear ls_line.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Material:'.
    ls_line-info = p_col_matnr.
    append ls_line to gd_report_title.

* Status Line(TYPE S)
    ld_date(2) = sy-datum+6(2).
    ld_date+2(1) = '/'.
    ld_date+3(2) = sy-datum+4(2).
    ld_date+5(1) = '/'.
    ld_date+6(4) = sy-datum(4).

    ls_line-typ  = 'S'.
    ls_line-key  = 'Data:'.
    ls_line-info = ld_date.
    append ls_line to gd_report_title.

    clear: ls_line.
    ls_line-typ  = 'S'.
    ls_line-key =  '*'.
    ls_line-info = ' '.
    append ls_line to gd_report_title.
    append ls_line to gd_report_title.

    clear: ls_line.
    ls_line-typ  = 'S'.
    ls_line-info  = '** Atualização saldo do estoque **'.
*    ls_line-info = ld_date.
    append ls_line to gd_report_title.

    clear: ls_line.
    if p_col_werks eq 'FI'.
      ls_line-key  = 'Filial:'.
      ls_line-typ  = 'S'.

      wl_date = sy-datum - 1.
      ld_date(2) = wl_date+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = wl_date+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = wl_date(4).

      concatenate text-001 ld_date into ls_line-info separated by space.
      append ls_line to gd_report_title.
    elseif p_col_werks eq 'FA'.
      ls_line-key  = 'Fabrica:'.
      ls_line-typ  = 'S'.
      ld_date(2) = sy-datum+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = sy-datum+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = sy-datum(4).
      write sy-uzeit to wl_uzeit using edit mask '__:__:__'.

      concatenate text-001 ld_date wl_uzeit into ls_line-info separated by space.
      append ls_line to gd_report_title.
    elseif p_col_werks eq 'PO'.
      ls_line-key  = 'Porto:'.
      ls_line-typ  = 'S'.
      ld_date(2) = sy-datum+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = sy-datum+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = sy-datum(4).
      write sy-uzeit to wl_uzeit using edit mask '__:__:__'.

      concatenate text-001 ld_date wl_uzeit into ls_line-info separated by space.
      append ls_line to gd_report_title.
    else.
      ls_line-key  = 'Filial:'.
      ls_line-typ  = 'S'.
      wl_date = sy-datum - 1.
      ld_date(2) = wl_date+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = wl_date+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = wl_date(4).

      concatenate text-001 ld_date into ls_line-info separated by space.
      append ls_line to gd_report_title.

      clear ls_line.
      ls_line-key  = 'Fabrica:'.
      ls_line-typ  = 'S'.
      ld_date(2) = sy-datum+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = sy-datum+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = sy-datum(4).
      write sy-uzeit to wl_uzeit using edit mask '__:__:__'.

      concatenate text-001 ld_date wl_uzeit into ls_line-info separated by space.
      append ls_line to gd_report_title.

      clear ls_line.
      ls_line-key  = 'Porto:'.
      ls_line-typ  = 'S'.
      ld_date(2) = sy-datum+6(2).
      ld_date+2(1) = '/'.
      ld_date+3(2) = sy-datum+4(2).
      ld_date+5(1) = '/'.
      ld_date+6(4) = sy-datum(4).
      write sy-uzeit to wl_uzeit using edit mask '__:__:__'.

      concatenate text-001 ld_date wl_uzeit into ls_line-info separated by space.
      append ls_line to gd_report_title.
    endif.

* create emty tree-control
    call method tree1->set_table_for_first_display
      exporting
*      IS_LAYOUT           = s_layout
        is_hierarchy_header = l_hierarchy_header
        i_save              = 'A'
        is_variant          = ls_variant
        it_list_commentary   = gd_report_title
        i_logo               = 'LOGOAMAGGI'
      changing
        it_outtab           = tw_saida[]
        it_fieldcatalog     = gt_fieldcatalog.


    " link click
    event-eventid = cl_gui_column_tree=>eventid_link_click.
    event-appl_event = 'X'.
    append event to tl_events.

    " expand no children
    event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    event-appl_event = 'X'.
    append event to tl_events.

    " expand no children
    event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    event-appl_event = 'X'.
    append event to tl_events.

    call method tree1->set_registered_events
      exporting
        events                    = tl_events
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
    if sy-subrc <> 0.
*    MESSAGE a000.
    endif.

    perform cria_hierarquia.
  endif.
endmodule.                 " CRIA_OBEJTOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
form build_hierarchy_header changing
                               p_hierarchy_header type treev_hhdr.

  p_hierarchy_header-heading = ' '.                         "#EC NOTEXT
  p_hierarchy_header-tooltip = 'Conhecimento de Estoque'.   "#EC NOTEXT
  p_hierarchy_header-width = 74.
  p_hierarchy_header-width_pix = 'X'.

endform.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_hierarquia .

  data: p_name1 type lvc_nkey,
        p_tipo  type lvc_nkey,
        p_tipo1 type lvc_nkey,
        p_tipo2 type lvc_nkey,
        p_last_key type lvc_nkey,
        wl_saida like tg_saida,
        wl_n0(50),
        wl_n1(50),
        wl_n2(50),
        wl_n3(50),
        wl_n4(50),
        wl_n5(50).

  perform organiza_dados.
  delete tg_saida where labst is initial
                    and speme is initial
                    and afixa is initial.

  clear: wl_n0, wl_n1, wl_n2, wl_n3, wl_n4, wl_n5.
  sort: tg_saida by butxt name1 tipo tipo1 tipo2.
  loop at tg_saida into wl_saida.
    if wl_saida-butxt is not initial
    and wl_saida-butxt ne wl_n0.
**   buktxt ( empresa )
*      ON CHANGE OF wl_saida-butxt.
      perform cria_pasta using wl_saida
                               space
                               wl_saida-butxt
                      changing p_name1.

*      CLEAR: wl_saida.
*      ENDON.
      wl_n0 = wl_saida-butxt.
    endif.
** name1 ( nome filial )
    if wl_saida-name1 is not initial
    and wl_saida-name1 ne wl_n1.
*      ON CHANGE OF wl_saida-name1.
*      wl_saida-butxt = tg_saida-butxt.
      perform cria_pasta using wl_saida
                               p_name1
                               wl_saida-name1
                      changing p_tipo.
*      CLEAR: wl_saida.
*      ENDON.
      wl_n1 = wl_saida-name1.
      clear: wl_n2.
    endif.
** tipo ( tipo PORTOS )
    if wl_saida-tipo is not initial
    and wl_saida-tipo ne wl_n2.
*      ON CHANGE OF wl_saida-tipo.
*      wl_saida-name1 = tg_saida-name1.
      perform cria_pasta using wl_saida
                               p_tipo
                               wl_saida-tipo
                      changing p_tipo1.
*      CLEAR: wl_saida.
*      ENDON.
      wl_n2 = wl_saida-tipo.
      clear: wl_n3.
    endif.
** tipo1 ( tipo TERMINAIS )
    if wl_saida-tipo1 is not initial
    and wl_saida-tipo1 ne wl_n3.
*      ON CHANGE OF wl_saida-tipo1.
*      wl_saida-name1 = tg_saida-name1.
      perform cria_pasta using wl_saida
                               p_tipo1
                               wl_saida-tipo1
                      changing p_tipo2.
*      CLEAR: wl_saida.
*      ENDON.
      wl_n3 = wl_saida-tipo1.
      clear: wl_n4.
    endif.
** tipo2 ( tipo DEPOSITOS )
    if wl_saida-tipo2 is not initial
    and wl_saida-tipo2 ne wl_n4.
*      ON CHANGE OF wl_saida-tipo2.
*      wl_saida-name1 = tg_saida-name1.
      perform cria_pasta using wl_saida
                               p_tipo2
                               wl_saida-tipo2
                      changing p_last_key.
*      CLEAR: wl_saida.
*      ENDON.
      wl_n4 = wl_saida-tipo2.
    endif.
  endloop.
* calculate totals
  call method tree1->update_calculations.

* this method must be called to send the data to the frontend
  call method tree1->frontend_update.
endform.                    " CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
form montar_estrutura_oo using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_sum).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

*  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  clear gs_fieldcatalog.
  gs_fieldcatalog-fieldname     = p_field.
  gs_fieldcatalog-tabname       = p_tabname.
  gs_fieldcatalog-ref_table     = p_ref_tabname.
  gs_fieldcatalog-ref_field     = p_ref_fieldname.
  gs_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
*  gs_fieldcatalog-edit          = p_edit.
  gs_fieldcatalog-do_sum        = p_sum.

  gs_fieldcatalog-col_pos         = p_col_pos.
  if p_outputlen is not initial.
    gs_fieldcatalog-outputlen      = p_outputlen.
  endif.
  gs_fieldcatalog-no_out        = ' '.
  gs_fieldcatalog-reptext       = p_scrtext_l.
  gs_fieldcatalog-scrtext_s     = p_scrtext_l.
  gs_fieldcatalog-scrtext_m     = p_scrtext_l.
  gs_fieldcatalog-scrtext_l     = p_scrtext_l.

  append gs_fieldcatalog to gt_fieldcatalog.
  clear: gs_fieldcatalog.
endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form organiza_dados .
  types: begin of tyl_total_fix,
          werks type mseg-werks,
          lgort type mseg-lgort,
          menge type mseg-menge,
         end of tyl_total_fix.

  data: wl_lfmon type mard-lfmon,
        wl_lfgja type mard-lfgja,
        wl_saida like line of tg_saida,
        wl_data_aux type sy-datum,
        wl_dt_mov like line of tg_dt_mov,
        wl_mard   like line of tg_mard,
        wl_mardh  like line of tg_mardh,
        wl_mseg   like line of tg_mseg,
*        wl_dt_mov like line of tg_dt_mov,
        tl_total_fix type table of tyl_total_fix with header line.
** Transferencia entre Filiais
  clear: tg_marc, tg_ind_werks, tg_mardh, tg_mseg, tg_mslb, tg_lfa1, tg_porto,
         tg_lgort, wl_lfmon, wl_lfgja, tg_mard, wl_saida, tg_dt_mov, wl_dt_mov,
         wl_mard, wl_mardh, wl_mseg, wl_dt_mov.


  loop at tg_marc.
    read table tg_ind_werks
      with key werks = tg_marc-werks.

    read table tg_mard
      with key werks = tg_marc-werks
               matnr = tg_marc-matnr.

    move: wg_t001-butxt                 to tg_saida-butxt,
          tg_ind_werks-name1            to tg_saida-name1,
          'Filial'                     to tg_saida-tipo,
          'Transito' to tg_saida-tipo1.
    tg_saida-labst = tg_marc-umlmc + tg_marc-trame.

    collect tg_saida.
    clear: tg_saida, tg_mard, tg_ind_werks, tg_marc.
  endloop.

  sort tg_mardh descending by lfgja lfmon .
  loop at tg_mardh.
    move-corresponding: tg_mardh to tg_dt_mov.
    read table tg_dt_mov into wl_dt_mov"transporting no fields
      with key werks = tg_dt_mov-werks
               lgort = tg_dt_mov-lgort.
*    if sy-subrc is not initial.
    if tg_dt_mov-lfgja gt wl_dt_mov-lfgja
    or (  tg_dt_mov-lfgja eq wl_dt_mov-lfgja
      and tg_dt_mov-lfmon ge wl_dt_mov-lfmon ).
      if sy-subrc is initial.
        delete tg_dt_mov index sy-tabix.
      endif.
      append tg_dt_mov.
    endif.
    clear: tg_dt_mov, wl_dt_mov.
  endloop.

  sort tg_dt_mov descending by lfgja lfmon werks lgort.
  delete adjacent duplicates from tg_dt_mov comparing werks lgort.

** Busca salto total do a fixar do centro real
** Monta saldo total Afixar atraves da MSEG (MARDH + MSEG)
  refresh: tl_total_fix.
  wl_data_aux = sy-datum.
  if wl_data_aux+4(2) eq '01'.
    subtract 1 from wl_data_aux(4).
  else.
    subtract 1 from wl_data_aux+4(2).
  endif.

  loop at tg_mseg
    where werks(2) eq 'AF'.

    read table tg_zsdt_cent
      with key vkorg        = p_bukrs
               centrov_1    = tg_mseg-werks.

    if sy-subrc is initial.
      read table tg_mardh
        with key lfmon = wl_data_aux+4(2) "tg_dt_mov-lfmon
                 lfgja = wl_data_aux(4)   "tg_dt_mov-lfgja
                 werks = tg_zsdt_cent-centro_real
                 lgort = tg_mseg-lgort.

      if tg_mardh-labst is not initial
      and tg_mardh-lgort ne 'WA01'.

        if tg_mseg-shkzg eq 'H'.
          multiply tg_mseg-menge by -1.
        endif.

        move: tg_zsdt_cent-centro_real to tl_total_fix-werks,
              tg_mseg-lgort            to tl_total_fix-lgort,
              tg_mseg-menge            to tl_total_fix-menge.
        collect tl_total_fix.
      endif.
    endif.
    clear:  tl_total_fix, tg_zsdt_cent, tg_mardh.
  endloop.
** Monta saldo total Afixar atraves da MardH (MARDH + MSEG)
  clear : wl_dt_mov, wl_mardh.
  loop at tg_dt_mov
     where werks(2) eq 'AF'
       and lfgja    eq wl_data_aux(4)
       and lfmon    eq wl_data_aux+4(2).
    loop at tg_mardh
     where lfgja eq tg_dt_mov-lfgja
       and lfmon eq tg_dt_mov-lfmon
       and werks eq tg_dt_mov-werks
       and lgort eq tg_dt_mov-lgort.

      read table tg_zsdt_cent
        with key vkorg        = p_bukrs
                 centrov_1    = tg_mardh-werks.

      if sy-subrc is initial.
        read table tg_mardh into wl_mardh
          with key lfmon = wl_data_aux+4(2)
                   lfgja = wl_data_aux(4)
                   werks = tg_zsdt_cent-centro_real.

        if wl_mardh-labst is not initial
        and wl_mardh-lgort ne 'WA01'.

          move: tg_zsdt_cent-centro_real to tl_total_fix-werks,
                tg_mardh-lgort           to tl_total_fix-lgort,
                tg_mardh-labst           to tl_total_fix-menge.
          collect tl_total_fix.
        endif.

      endif.
      clear:  tl_total_fix, tg_zsdt_cent, tg_mardh, wl_mardh, wl_dt_mov.
    endloop.
  endloop.
*** Monta saldo A fixar da atraves da Mard
  loop at tg_mard
  where werks(2) eq 'AF'.

    read table tg_zsdt_cent
      with key vkorg        = p_bukrs
               centrov_1    = tg_mard-werks.

    if sy-subrc is initial.
      read table tl_total_fix transporting no fields
        with key werks = tg_zsdt_cent-centro_real
                 lgort = tg_mard-lgort.

      if sy-subrc is not initial.
        move: tg_zsdt_cent-centro_real to tl_total_fix-werks,
              tg_mard-lgort            to tl_total_fix-lgort,
              tg_mard-labst            to tl_total_fix-menge.
        collect tl_total_fix.

      endif.
    endif.
    clear:  tl_total_fix, tg_zsdt_cent, tg_mardh, tg_mard.
  endloop.


  loop at tg_mseg
     where werks(2) ne 'AF'.
    read table tg_ind_werks
      with key werks = tg_mseg-werks.

    read table tg_ind_lgort
       with key werks = tg_ind_werks-werks
                lgort = tg_mseg-lgort
                box   = 'X'.
    if sy-subrc is initial.
      read table tg_dt_mov
        with key werks = tg_mseg-werks
                 lgort = tg_mseg-lgort.

      read table tg_mardh
        with key lfmon = wl_data_aux+4(2)
                 lfgja = wl_data_aux(4)
                 werks = tg_dt_mov-werks
                 lgort = tg_mseg-lgort.

      if tg_mardh-labst is not initial
      and tg_mseg-lgort ne 'WA01'.
        read table tg_porto
               with key werks = tg_ind_werks-werks.
        if sy-subrc is not initial.
          move: wg_t001-butxt                to tg_saida-butxt,
               tg_ind_werks-name1            to tg_saida-name1,
               'Filial'                      to tg_saida-tipo,
               tg_ind_lgort-lgobe            to tg_saida-tipo1.
          translate tg_saida-tipo1+1 to lower case.

          if tg_mseg-shkzg eq 'H'.
            multiply tg_mseg-menge by -1.

          endif.
          tg_saida-labst = tg_mseg-menge." + wl_mseg-menge.

          collect tg_saida.
        endif.
      endif.
    endif.
    clear: tg_saida, tg_ind_werks, tg_mardh, tg_mseg, wl_mseg.
  endloop.

  loop at tg_dt_mov
    where werks(2) ne 'AF'
      and lfgja    eq wl_data_aux(4)
      and lfmon    eq wl_data_aux+4(2).
    read table tg_mardh
      with key lfmon = tg_dt_mov-lfmon
               lfgja = tg_dt_mov-lfgja
               werks = tg_dt_mov-werks
               lgort = tg_dt_mov-lgort.
    if sy-subrc is initial.
      if tg_mardh-labst is not initial
      and tg_mardh-lgort ne 'WA01'.
        read table tg_ind_werks
          with key werks = tg_mardh-werks
                   box   = 'X'.
        if sy-subrc is initial.
          read table tg_ind_lgort
          with key werks = tg_ind_werks-werks
                   lgort = tg_dt_mov-lgort
                   box   = 'X'.
          if sy-subrc is initial.
            read table tg_porto
              with key werks = tg_ind_werks-werks.
            if sy-subrc is not initial.

              move: wg_t001-butxt                to tg_saida-butxt,
                   tg_ind_werks-name1            to tg_saida-name1,
                   'Filial'                      to tg_saida-tipo,
                   tg_ind_lgort-lgobe            to tg_saida-tipo1.
              translate tg_saida-tipo1+1 to lower case.

              tg_saida-labst = tg_mardh-labst. "+ wl_mardh-labst.
              collect tg_saida.

            endif.
          endif.
        endif.
      endif.
    endif.
    clear: tg_saida, tg_ind_werks, tg_mardh, wl_saida, wl_mardh.
  endloop.

  loop at tg_mard
    where werks(2) ne 'AF'.
    read table tg_ind_werks
      with key werks = tg_mard-werks.
    if sy-subrc is initial.
      read table tg_ind_lgort
      with key werks = tg_mard-werks
*               lgort = 'ARMZ'
               lgort = tg_mard-lgort
               box   = 'X'.
      if sy-subrc is initial.
        read table tg_porto
          with key werks = tg_ind_werks-werks.
        if sy-subrc is not initial.

          move: wg_t001-butxt                to tg_saida-butxt,
                tg_ind_werks-name1            to tg_saida-name1,
                'Filial'                      to tg_saida-tipo,
                tg_ind_lgort-lgobe            to tg_saida-tipo1.
          translate tg_saida-tipo1+1 to lower case.

          tg_saida-speme = tg_mard-speme.

          read table tg_saida transporting no fields
                    with key name1 = tg_ind_werks-name1
                             tipo1 = tg_saida-tipo1.
          if sy-subrc is not initial.
            tg_saida-labst = tg_mard-labst.

          endif.
          collect tg_saida.
        endif.
      endif.
    endif.
    clear: tg_saida, tg_ind_werks, tg_mard, wl_mard.
  endloop.

*** Tabela contento todos os saldos A fixar dos depositos
  loop at tl_total_fix.
    read table tg_ind_werks
          with key werks = tl_total_fix-werks.
    if sy-subrc is initial.
      read table tg_ind_lgort
      with key werks = tl_total_fix-werks
               lgort = tl_total_fix-lgort
               box   = 'X'.
      if sy-subrc is initial.

        move: wg_t001-butxt                to tg_saida-butxt,
              tg_ind_werks-name1            to tg_saida-name1,
              'Filial'                      to tg_saida-tipo,
              tg_ind_lgort-lgobe            to tg_saida-tipo1,
              tl_total_fix-menge            to tg_saida-labst,
              tl_total_fix-menge            to tg_saida-afixa.
        translate tg_saida-tipo1+1 to lower case.

        collect tg_saida.

      endif.
    endif.
    clear: tg_saida, tg_ind_werks, tg_ind_lgort.
  endloop.

** Terceiros
  loop at tg_mslb.
    read table tg_ind_werks
      with key werks = tg_mslb-werks.

    read table tg_lfa1
      with key lifnr = tg_mslb-lifnr.

    move: wg_t001-butxt                 to tg_saida-butxt,
          tg_ind_werks-name1            to tg_saida-name1,
          'Terceiro'                    to tg_saida-tipo,
          tg_lfa1-name1                 to tg_saida-tipo1,
          tg_mslb-charg                 to tg_saida-tipo2,
          tg_mslb-lblab                 to tg_saida-labst.

*    tg_saida-labst = tg_mslb-lblab + tg_mslb-lbvla.

    collect tg_saida.
    clear: tg_saida, tg_lfa1, tg_ind_werks, tg_mslb.
  endloop.


** Portos
  loop at tg_lgort.
    read table tg_ind_werks
      with key werks = tg_lgort-werks.

    read table tg_porto
      with key werks = tg_lgort-werks.

    read table tg_mard
      with key werks = tg_lgort-werks
               lgort = tg_lgort-lgort.

    read table tg_ind_lgort
      with key werks = tg_lgort-werks
               lgort = tg_lgort-lgort
               box   = 'X'.

    if sy-subrc is initial.

      move: wg_t001-butxt                 to tg_saida-butxt,
            tg_ind_werks-name1            to tg_saida-name1,
            'Terminais'                      to tg_saida-tipo,
*          tg_porto-nome                 TO tg_saida-tipo1,
            tg_lgort-nome                 to tg_saida-tipo1,
            tg_lgort-snome                to tg_saida-tipo2,
            tg_mard-labst                 to tg_saida-labst,
            tg_mard-speme                 to tg_saida-speme.

      collect tg_saida.
    endif.
    clear: tg_saida , tg_ind_werks, tg_porto, tg_lgort, tg_mard.
  endloop.
endform.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CRIA_PASTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_SAIDA  text
*      -->P_SPACE  text
*      <--P_P_NAME1  text
*----------------------------------------------------------------------*
form cria_pasta  using    wl_saida
                          p_relat_key
                          p_node_text
                 changing p_next_key.

  data: l_node_text type lvc_value.
  data: w_txt type t001-butxt.
  data: p_node_key type lvc_nkey.

* set item-layout
  data: lt_item_layout type lvc_t_layi,
        ls_item_layout type lvc_s_layi.

  ls_item_layout-t_image = '@A8@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  append ls_item_layout to lt_item_layout.

* add node
  l_node_text =  p_node_text.

  data: ls_node type lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.
  call method tree1->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = wl_saida
*      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    importing
      e_new_node_key   = p_next_key.
endform.                    " CRIA_PASTA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  call method cl_gui_cfw=>flush.
  case sy-ucomm.
    when 'BACK'
      or 'EXIT'
      or 'CANCEL'.
      call method tree1->free.
      call method container1->free.
      clear: tw_saida, tg_saida, container1, tree1.
      free: tw_saida, tg_saida, container1, tree1.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  data: tl_fcode type table of sy-ucomm with header line.
  refresh: tl_fcode.
  move 'RUN' to tl_fcode.
  append tl_fcode.
  set pf-status 'Z001' excluding tl_fcode.
*  SET TITLEBAR 'xxx'.

endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_matnr .

  refresh: tl_0018, tl_values_18.
  clear: p_col_matnr.
  select *
     from zmmt0018
     into table tl_0018
       where bukrs eq p_bukrs.

  loop at tl_0018.
    move: tl_0018-maktx to tl_values_18-key.
    append tl_values_18.
    clear: tl_values_18.
  endloop.

  sort tl_values_18 by key.
  delete adjacent duplicates from tl_values_18.

  call function 'VRM_SET_VALUES'
    exporting
      id              = 'P_COL_MATNR'
      values          = tl_values_18[]
    exceptions
      id_illegal_name = 1
      others          = 2.
*  leave to screen 100.
endform.                    " UPDATE_MATNR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_lgort .
  data: tg_ind_werks_aux like table of tg_ind_werks with header line,
        wl_tabix type sy-tabix.

  ranges: rgl_bukrs for zmmt0020-bukrs,
          rgl_werks for zmmt0020-werks.

  refresh: tl_0020, tl_values_20, rgl_bukrs, rgl_werks, tg_ind_werks_aux,
           tg_zsdt_cent.
  clear: p_col_lgort, p_ind_lgort.

  if p_bukrs is not initial.
    rgl_bukrs-sign   = 'I'.
    rgl_bukrs-option = 'EQ'.
    rgl_bukrs-low    = p_bukrs.

  endif.

  if tg_ind_werks[] is not initial.
    tg_ind_werks_aux[] = tg_ind_werks[].
    delete tg_ind_werks_aux where box is initial.

    select *
      from zsdt_depara_cen
      into table tg_zsdt_cent
       for all entries in tg_ind_werks_aux
        where vkorg eq p_bukrs
          and centro_real eq tg_ind_werks_aux-werks.
  endif.

  loop at tg_ind_werks.
    wl_tabix = sy-tabix.
*     where box is not initial.
    read table tg_zsdt_cent
           with key centrov_1 = tg_ind_werks-werks.
    if sy-subrc is initial
    or tg_ind_werks-box is not initial.

      rgl_werks-sign   = 'I'.
      rgl_werks-option = 'EQ'.
      rgl_werks-low    = tg_ind_werks-werks.
      tg_ind_werks-box = 'X'.

      append rgl_werks.
      modify  tg_ind_werks index wl_tabix.
      clear: rgl_werks, wl_tabix.
    endif.

  endloop.

  select *
     from zmmt0020
     into table tl_0020
       where bukrs in rgl_bukrs
         and werks in rgl_werks.

  loop at tl_0020.
    move: tl_0020-nome to tl_values_20-key.
    append tl_values_20.
    clear: tl_values_20.
  endloop.
  sort tl_values_20 by key.
  delete adjacent duplicates from tl_values_20.

  call function 'VRM_SET_VALUES'
    exporting
      id              = 'P_COL_LGORT'
      values          = tl_values_20[]
    exceptions
      id_illegal_name = 1
      others          = 2.

  perform carrega_depositos using 'X'.
  leave to screen 100.
endform.                    " UPDATE_LGORT
*&---------------------------------------------------------------------*
*&      Form  CRIA_LOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_logo .

  create object container_pic
    exporting
      container_name = 'CC_LOGO'.
  create object picture
    exporting
      parent = container_pic
    exceptions
      error  = 1.
  if sy-subrc ne 0.
* Fehlerbehandlung
  endif.
  perform load_pic_from_db.

endform.                    " CRIA_LOGO
*&---------------------------------------------------------------------*
*&      Form  LOAD_PIC_FROM_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form load_pic_from_db.

  data query_table like w3query occurs 1 with header line.
  data html_table like w3html occurs 1.
  data return_code like  w3param-ret_code.
  data content_type like  w3param-cont_type.
  data content_length like  w3param-cont_len.
  data pic_data like w3mime occurs 0.
  data: pic_size type i,
        url(255)." TYPE CNDP_URL.

  refresh query_table.
  query_table-name = '_OBJECT_ID'.
  query_table-value = 'LOGOMAGGI'.
  append query_table.
*call function 'DP_PUBLISH_WWW_URL'
*  exporting
*    objid                       = 'LOGOAMAGGI'
*    lifetime                    = 'T'
* IMPORTING
*   URL                         = URL
* EXCEPTIONS
*   DP_INVALID_PARAMETERS       = 1
*   NO_OBJECT                   = 2
*   DP_ERROR_PUBLISH            = 3
*   OTHERS                      = 4
  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

*  call function 'WWW_GET_MIME_OBJECT'
*    tables
*      query_string        = query_table
*      html                = html_table
*      mime                = pic_data
*    changing
*      return_code         = return_code
*      content_type        = content_type
*      content_length      = content_length
*    exceptions
*      object_not_found    = 1
*      parameter_not_found = 2
*      others              = 3.
*  if sy-subrc = 0.
*    pic_size = content_length.
*  endif.

*  call function 'DP_CREATE_URL'
*    exporting
*      type     = 'image'
*      subtype  = cndp_sap_tab_unknown
*      size     = pic_size
*      lifetime = cndp_lifetime_transaction
*    tables
*      data     = pic_data
*    changing
*      url      = url
*    exceptions
*      others   = 1.
*
**url = 'SAP/PUBLIC/ZMIME/amaggi.gif'.
*    CALL METHOD picture->load_picture_from_url
*        EXPORTING url = url.
*      IF sy-subrc ne 0.
** Fehlerbehandlung
*      ENDIF.

endform.                               " LOAD_PIC_FROM_DB
