* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR168                                                *
* Title.......: Envio de Requisições de Compras para o COUPA           *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 02/02/2022                                             *
* -------------------------------------------------------------------- *
report zmmr168.

type-pools: slis, abap, icon.

tables: zmms_dados_int_coupa_alv, sscrfields.

constants gc_internal_tab type slis_tabname value 'GT_DADOS_ALV'.
constants gc_struc_name type dd02l-tabname value 'ZMMS_DADOS_INT_COUPA_ALV'.
constants gc_select_field type slis_fieldname value 'SELEC'.
constants gc_icon_field type slis_fieldname value 'COUPA_ICON'.
constants gc_service type /ui2/service_name value 'COUPA_INT_ENVIA_REQ_COMPRA'.
constants gc_aguardando type zemm_status_coupa value space.
constants gc_enviado type zemm_status_coupa value 'S'.
constants gc_nao_env type zemm_status_coupa value 'N'.
constants gc_liberado type zemm_status_coupa value 'L'.

data gt_envio_om type table of zintegrcoupa01.
data gt_aufk type table of aufk.
data gt_cskt type table of cskt.
data gt_skat type table of skat.
data gt_npact type table of v_npact.
data gt_ebkn type table of ebkn.
data gt_caufv type table of caufv. "137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
data gt_afvc type table of afvc.   "137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
data gt_esll type table of esll.
data gt_esll_sub type table of esll.
data gt_eban type table of eban.
data gt_t001 type table of t001.
data gt_t001k type table of t001k.
data gt_t001w type table of t001w.
data gt_set_catc type table of rgsbv.
data gt_set_dat type table of rgsbv.
data git_filtro type zif_screen_linha_filtro_t.
data gt_set  type table of rgsbv.
data gv_erro type c.
data gt_dados_alv type standard table of zmms_dados_int_coupa_alv.
data gt_dados_aux type standard table of zmms_dados_int_coupa_alv.
data gt_fieldcat type lvc_t_fcat. "slis_t_fieldcat_alv. .
data gt_bapiret2 type table of bapiret2.
*DATA gt_zmmt0174 TYPE TABLE OF zmmt0174.
data: tl_function type ui_functions,
      wl_function like tl_function with header line.
data: lc_data_view    type string,
      lc_open_browser type char01.

data: t_header         type slis_t_listheader,
      e_header         type slis_listheader,
      st_grid_settings type lvc_s_glay,
      gwa_stable       type lvc_s_stbl.

data go_int type ref to zcl_integracao_coupa_req_comp.

data: sort      type slis_t_sortinfo_alv with header line,
      events    type slis_t_event, "evento HOTSPOT
      xs_events type slis_alv_event. "evento para HotSpot

data: wa_selected_rows type lvc_s_row,
      it_selected_rows type lvc_t_row.

"Objetos
data: gob_custom_container        type ref to cl_gui_custom_container,
      gob_dd_document             type ref to cl_dd_document,
      gob_splitter_container_main type ref to cl_gui_splitter_container,
      gob_splitter_container_topo type ref to cl_gui_splitter_container,

      gob_gui_container_topo      type ref to cl_gui_container,
      gob_gui_container_filtro    type ref to cl_gui_container,
      gob_gui_container_logo      type ref to cl_gui_container,
      gob_gui_container_grid      type ref to cl_gui_container,
      gob_gui_picture             type ref to cl_gui_picture,
      git_fcat                    type lvc_t_fcat,
      gob_gui_alv_grid            type ref to cl_gui_alv_grid,
      dg_splitter_1               type ref to cl_gui_splitter_container,
      lines                       type sy-tabix.

" Classe
class lcl_event_receiver definition deferred.
data:  event_receiver   type ref to lcl_event_receiver.

class lcl_event_receiver definition.
  public section.
    methods:

      handle_toolbar
        for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive,

      hotspot_click
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no ,

      handle_data_changed_req
        for event data_changed of cl_gui_alv_grid
        importing er_data_changed,

      get_ucomm for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.

class lcl_event_receiver implementation.
  method handle_toolbar.

    data: ls_toolbar  type stb_button.

    clear ls_toolbar.
    move 3 to ls_toolbar-butn_type.
    append ls_toolbar to e_object->mt_toolbar.

    " botão.
    clear ls_toolbar.
    move 'ENVIAR'                   to ls_toolbar-function.
    move icon_generate              to ls_toolbar-icon.
    move 'Enviar para o COUPA'(111) to ls_toolbar-quickinfo.
    move 'Enviar para o COUPA'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.

    clear ls_toolbar.
    move 'LIBERAR'                                  to ls_toolbar-function.
    move icon_page_right                            to ls_toolbar-icon.
    move 'Liberar linha req. para modificação'(111) to ls_toolbar-quickinfo.
    move 'Liberar linha req. para modificação'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.

    clear ls_toolbar.
    move 'REQUIS'                    to ls_toolbar-function.
    move icon_agent                  to ls_toolbar-icon.
    move 'Alterar Requisitante'(111) to ls_toolbar-quickinfo.
    move 'Requisitante'(112)         to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.


  endmethod.

  method hotspot_click.

    case e_column_id.
      when 'BANFN' or 'BNFPO'.
        perform f_hyperlink using e_row_id e_column_id."rs_selfield.
    endcase.

    "CALL METHOD gob_gui_alv_grid->refresh_table_display.

  endmethod.

  method get_ucomm.

    clear: it_selected_rows[].

    call method gob_gui_alv_grid->get_selected_rows
      importing
        et_index_rows = it_selected_rows.

    case e_ucomm.
        "WHEN '&IC1'.
        "PERFORM f_hyperlink USING e_row e_column_id. "rs_selfield.

      when 'LIBERAR'.
        perform f_liberar. "USING rs_selfield.

      when 'ENVIAR'.
        perform f_enviar. "USING rs_selfield.

      when 'REQUIS'.
        perform f_requisitante. "USING rs_selfield.

      when 'EXIT'.
        leave to screen 0.
      when others.
    endcase.

    call method gob_gui_alv_grid->refresh_table_display.

  endmethod.

  method handle_data_changed_req.

    data: wa_good_cells like line of er_data_changed->mt_good_cells.

    loop at er_data_changed->mt_good_cells into wa_good_cells.
      read table gt_dados_alv into data(wa_dados_alv) index wa_good_cells-row_id.
      if sy-subrc is initial.
        case wa_good_cells-fieldname.
          when 'AFNAM'.
            wa_dados_alv-afnam  = wa_good_cells-value.
          when 'PRIO_URG'.
            wa_dados_alv-prio_urg = wa_good_cells-value.
        endcase.

        modify gt_dados_alv from wa_dados_alv index wa_good_cells-row_id.
      endif.
    endloop.

    call method gob_gui_alv_grid->refresh_table_display.

  endmethod.
endclass.

selection-screen begin of block b1 with frame title text-001.

  select-options so_banfn for zmms_dados_int_coupa_alv-banfn.
  select-options so_bnfpo for zmms_dados_int_coupa_alv-bnfpo.
  select-options so_werks for zmms_dados_int_coupa_alv-werks obligatory.
  select-options so_ernam for zmms_dados_int_coupa_alv-ernam.
  select-options so_badat for zmms_dados_int_coupa_alv-badat obligatory.
  select-options so_lfdat for zmms_dados_int_coupa_alv-lfdat.
  select-options so_prio  for zmms_dados_int_coupa_alv-prio_urg.
  select-options so_stat  for zmms_dados_int_coupa_alv-status_coupa.
*PARAMETER r_pm   LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X'.
*PARAMETER r_es   LIKE bsid-umskz AS CHECKBOX.
  parameters:
    r_pm radiobutton group rad1 user-command act,
    r_es radiobutton group rad1 default 'X'.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.


selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
  parameters p_vari type slis_vari.
selection-screen end of block b3.

initialization.
  perform default_variant changing p_vari.

at selection-screen on value-request for p_vari.
  perform f4_for_variant changing p_vari.

at selection-screen.
  perform f_botao_command.

start-of-selection.

  data lv_erro.

  perform f_verifica_job changing lv_erro.

  check lv_erro is initial.

  perform f_clear_vars.

  perform f_seleciona changing lv_erro.

  perform f_processa_selec changing lv_erro.

*--------------------------------------------------------------------------------------------------------*
*   ""CS2023000015 Integraqção SAP MRPxCOUPA Consultar Status Requisição / AOENNING.
*--------------------------------------------------------------------------------------------------------*
  perform f_check_status_coupa.

*  PERFORM f_exibe_alv CHANGING lv_erro.

end-of-selection.

  perform fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
form f_user_command  using r_ucomm     type sy-ucomm
            rs_selfield type slis_selfield.                 "#EC CALLED

  case r_ucomm.
    when '&IC1'.
      "PERFORM f_hyperlink USING rs_selfield.

    when 'LIBERAR'.
      "PERFORM f_liberar USING rs_selfield.

    when 'ENVIAR'.
      "PERFORM f_enviar USING rs_selfield.

    when 'REQUIS'.
      "PERFORM f_requisitante USING rs_selfield.

    when 'EXIT'.
      leave to screen 0.
    when others.
  endcase.

  if r_ucomm ne 'OK'.
    rs_selfield-refresh = 'X'.
  endif.

  r_ucomm = '&REFRESH'.

endform.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
form f_seleciona changing p_erro type c.

  data lv_date type syst_datum.
  data lv_msgv type sy-msgv4.
  data lr_knttp type range of knttp.

  data: rg_chave type range of afvc-steus.

  perform f_sap_indicator using 'selecionando requisições...' 50.

  perform f_carrega_set using '0000MAGGI_REQUISICAO_COUPA' changing gt_set_dat.
  perform f_carrega_set using '0000MAGGI_COUPA_REQ_CATC' changing gt_set_catc.

  loop at gt_set_catc assigning field-symbol(<fs_catc>).

    "append INITIAL LINE TO lr_knttp ASSIGNING FIELD-SYMBOL(<fs_knttp>).

    append 'IEQ' && <fs_catc>-from to lr_knttp.


  endloop.

  read table gt_set_dat assigning field-symbol(<fs_dat>) index 1.

  if sy-subrc eq 0.

    lv_date = <fs_dat>-from.

  else.

    lv_date = '20220201'.

  endif.

  if sy-sysid ne 'DEV'.

    loop at so_badat assigning field-symbol(<fs_badat>) where low < lv_date.

      write lv_date to lv_msgv.

      perform f_mensagem_exibe
        using 'E'
              'A Data de Solicitação de'
              'Requisição tem que ser'
              'a partir de' lv_msgv.

      p_erro = 'X'.

      exit.

    endloop.

  endif.

  check p_erro is initial.


  check p_erro is initial.

*  SELECT *
*         FROM zmmt0174
*         INTO TABLE gt_zmmt0174.
*
*  SORT gt_zmmt0174 BY grup_comp_sap.

  if r_pm = 'X'.
    select * from eban
      into table gt_eban
        where banfn in so_banfn
          and bnfpo in so_bnfpo
          and werks in so_werks
          and ernam in so_ernam
          and badat in so_badat
          and lfdat in so_lfdat
          and prio_urg  in so_prio
*          AND status_coupa  IN so_stat
          and knttp in lr_knttp
*          and frgkz = '2' "-US 154383-06-11-2024-#154383-RJF
*          and frgzu = 'X' "-US 154383-06-11-2024-#154383-RJF
*          and frgkz IN ( 'X', '2' ) "-US 154383-06-11-2024-#154383-RJF
*          and frgzu IN ( ' ', 'X' ) "-US 154383-06-11-2024-#154383-RJF
          and ebeln = space
          and loekz = space "14.07.2022
          and ebakz = space.
  endif.

  if r_es = 'X'.
    select * from eban
      appending table gt_eban
        where banfn in so_banfn
          and bnfpo in so_bnfpo
          and werks in so_werks
          and ernam in so_ernam
          and badat in so_badat
          and lfdat in so_lfdat
          and prio_urg  in so_prio
*          AND status_coupa  IN so_stat
          and knttp = ' '
*          AND frgkz = '2'
*          AND frgzu = 'X'
          and ebeln = space
          and loekz = space "14.07.2022
          and ebakz = space.
  endif.
  check gt_eban[] is not initial.

  check sy-subrc eq 0.

  select * from t001k
    into table gt_t001k
      for all entries in gt_eban
        where bwkey	=	gt_eban-werks.

  check sy-subrc eq 0.

  select * from t001
    into table gt_t001
      for all entries in gt_t001k
        where bukrs = gt_t001k-bukrs.

  select * from t001w
     into table gt_t001w
        for all entries in gt_eban
          where werks	=	gt_eban-werks.

  check sy-subrc eq 0 .

  select * from esll
    into table gt_esll
      for all entries in gt_eban
        where packno = gt_eban-packno.

  if sy-subrc eq 0.

    select * from esll
    into table gt_esll_sub
      for all entries in gt_esll
        where packno = gt_esll-sub_packno.

  endif.

  select * from ebkn
    into table gt_ebkn
         for all entries in gt_eban
          where banfn  = gt_eban-banfn
            and bnfpo = gt_eban-bnfpo.

  if gt_ebkn is not initial.

*>>>Begin-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

    select *
      from caufv
      into table gt_caufv
      for all entries in gt_ebkn
      where aufnr = gt_ebkn-aufnr.

    if gt_caufv is not initial.

      select *
        from afvc
        into table gt_afvc
        for all entries in gt_caufv
        where aufpl = gt_caufv-aufpl.

      free: rg_chave.
      append value #( sign = 'I' option = 'EQ' low = 'PM04') to rg_chave.
      append value #( sign = 'I' option = 'EQ' low = 'PM03') to rg_chave.
      append value #( sign = 'I' option = 'EQ' low = 'PM05') to rg_chave.

      delete gt_afvc where steus not in rg_chave.

    endif.

*<<<End-Stefanini-MM-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

    select * from v_npact
      into table gt_npact
        for all entries in gt_ebkn
          where aufnr  = gt_ebkn-aufnr.

    " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO -->
    if sy-subrc eq 0.

      data lr_id_om type range of zcoupa_id_integr.
      data lv_om type c length 20.

      loop at gt_npact assigning field-symbol(<fs_npact>).

        lv_om = <fs_npact>-aufnr.

        shift lv_om left deleting leading '0'.

        append 'IEQ' && lv_om to lr_id_om.

      endloop.

      delete adjacent duplicates from lr_id_om.

      if lr_id_om is not initial.

        select * from zintegrcoupa01
          into table gt_envio_om
            where id_integr in lr_id_om
              and ident_proc = 'OM'
              and status = 'S'.

      endif.

    endif.
    " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO --<

    select * from skat
      into table gt_skat
        for all entries in gt_ebkn
          where saknr = gt_ebkn-sakto
            and spras = sy-langu
            and ktopl = '0050'.

    if sy-subrc eq 0.

      select * from cskt
        into table gt_cskt
        for all entries in gt_ebkn
          where spras = sy-langu
           and kokrs = 'MAGI'
           and kostl = gt_ebkn-kostl.
    endif.

    select * from aufk
      into table gt_aufk
      for all entries in gt_ebkn
        where aufnr  = gt_ebkn-aufnr
          and autyp = '30'.

  endif.


  call function 'G_SET_FETCH'
    exporting
      setnr           = '0000MAGGI_EMPRESAS_COUPA'
    tables
      set_lines_basic = gt_set
    exceptions
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      others          = 4.

  if sy-subrc <> 0.
    exit.
  endif.

endform.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
form f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
form f_botao_command.

  if sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
form f_exibe_alv using p_erro type c.

  data lw_layout type slis_layout_alv.
  data lw_variant type disvariant.

*  PERFORM definir_eventos. "evento para HotSpot
*
*  CHECK p_erro IS INITIAL.
*
*  IF gt_dados_alv IS NOT INITIAL.
*
*    IF p_vari IS NOT INITIAL.
*      lw_variant-report = sy-repid.
*      lw_variant-variant = p_vari.
*    ENDIF.
*
*    PERFORM f_monta_fieldcat.
*
*    lw_layout-zebra             = abap_true.
*    lw_layout-colwidth_optimize = abap_true.
*    lw_layout-box_fieldname = gc_select_field.
*    lw_layout-info_fieldname = 'COLOR'.
*
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        i_callback_program       = sy-repid
*        i_callback_pf_status_set = 'F_STATUS_SET'
*        i_callback_user_command  = 'F_USER_COMMAND'
*        is_layout                = lw_layout
*        it_fieldcat              = gt_fieldcat
*        i_save                   = 'A'
*        is_variant               = lw_variant
*      TABLES
*        t_outtab                 = gt_dados_alv
*      EXCEPTIONS
*        program_error            = 1
*        OTHERS                   = 2.
*
*    IF sy-subrc <> 0.
*      PERFORM f_mensagem_sistema.
*    ENDIF.
*
*  ELSE.
*
*    IF NOT line_exists( gt_bapiret2[ type = 'E' ] ).
*
*      MESSAGE s213(v4) DISPLAY LIKE 'E'.
*      EXIT.
*    ELSE.
*
*      PERFORM f_formatar_msgs_global.
*      PERFORM f_mensagem_exibe_popup USING gt_bapiret2.
*
*    ENDIF.
*
*  ENDIF.

endform.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
form f_status_set using p_extab type slis_t_extab.          "#EC CALLED

  set pf-status 'STANDARD'.

endform.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
form f_monta_fieldcat.

  "LVC_FIELDCATALOG_MERGE
  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = sy-cprog
*      i_internal_tabname     = gc_internal_tab
*      i_structure_name       = gc_struc_name
*    CHANGING
*      ct_fieldcat            = gt_fieldcat
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*
*  IF sy-subrc <> 0.
*    PERFORM f_mensagem_sistema.
*  ENDIF.


  clear gt_fieldcat[].

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name   = gc_struc_name
      i_internal_tabname = gc_internal_tab
    changing
      ct_fieldcat        = gt_fieldcat
    exceptions
      others             = 3.


  read table gt_fieldcat assigning field-symbol(<fs_fcat>)
    with key fieldname = gc_icon_field.

  if sy-subrc eq 0.
    <fs_fcat>-icon = abap_true.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-coltext = 'Status'.
    <fs_fcat>-outputlen = 000010.
  endif.


  read table gt_fieldcat assigning <fs_fcat> with key fieldname = 'BANFN'.
  if sy-subrc eq 0.
    <fs_fcat>-hotspot = abap_true.
  endif.

  read table gt_fieldcat assigning <fs_fcat> with key fieldname = 'BNFPO'.
  if sy-subrc eq 0.
    <fs_fcat>-hotspot = abap_true.
  endif.

  read table gt_fieldcat assigning <fs_fcat> with key fieldname = 'SELEC'.
  if sy-subrc eq 0.
    <fs_fcat>-checkbox = abap_true.
  endif.

  read table gt_fieldcat assigning <fs_fcat> with key fieldname = 'ID_COUPA'.
  if sy-subrc eq 0.
    <fs_fcat>-outputlen = 000010.
  endif.

  read table gt_fieldcat assigning <fs_fcat> with key fieldname = 'AFNAM'.
  if sy-subrc eq 0.
    <fs_fcat>-edit = 'X'.
    <fs_fcat>-outputlen = 000020.
  endif.

  read table gt_fieldcat assigning <fs_fcat>
  with key fieldname = 'PRIO_URG'.
  if sy-subrc eq 0.
    <fs_fcat>-edit = 'X'.
  endif.

  delete gt_fieldcat where fieldname = gc_select_field.
  delete gt_fieldcat where fieldname = 'STATUS_COUPA'.
  delete gt_fieldcat where fieldname = 'TOTAL_ITENS'.
  delete gt_fieldcat where fieldname = 'COLOR'.
  delete gt_fieldcat where fieldname = 'AUFNR'.

endform.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
form f_mensagem_sistema.

  message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

endform.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
form f_mensagem_sistema_s.

  message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.

endform.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
form f_mensagem_exibe using p_type type sy-msgty
                            p_msgv1 type sy-msgv1
                            p_msgv2 type sy-msgv2
                            p_msgv3 type sy-msgv3
                            p_msgv4 type sy-msgv4.

  message id 'DS' type 'S' number '016'
    with p_msgv1 p_msgv2 p_msgv3 p_msgv4 display like p_type.

endform.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
form f_mensagem_sistema_insere.

  perform f_mensagem_insere
    tables gt_bapiret2
     using sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
form f_sap_indicator using p_text type c
                           p_percent type i.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = p_percent
      text       = p_text.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
form f4_for_variant changing f_vari type slis_vari.

  data: lw_variant type disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = lw_variant
      i_save     = 'A'
    importing
      es_variant = lw_variant
    exceptions
      not_found  = 2.

  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    f_vari = lw_variant-variant.
  endif.

endform.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
form default_variant changing f_vari type slis_vari.
  data: lw_variant type disvariant.

  lw_variant-report = sy-repid.
  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save        = 'A'
    changing
      cs_variant    = lw_variant
    exceptions
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      others        = 4.

  if sy-subrc = 0.
    f_vari = lw_variant-variant.
  endif.

endform.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
form f_preenche_data .

  "DATA(lv_ini) = sy-datum.
  "DATA(lv_fim) = sy-datum.

  "CHECK so_data[] IS INITIAL.

  "SUBTRACT 15 FROM lv_ini.
  "ADD 15 TO lv_fim.

  "APPEND 'IBT' && lv_ini && lv_fim TO so_data.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
form f_hyperlink using e_row type lvc_s_row e_column_id type lvc_s_col."rs_selfield TYPE slis_selfield.

  data lw_saida_alv like line of gt_dados_alv.

  check e_column_id is not initial.
  "CHECK rs_selfield-value IS NOT INITIAL.

  read table gt_dados_alv into lw_saida_alv index e_row-index. "rs_selfield-tabindex.

  case e_column_id-fieldname.

    when 'BANFN' or 'BNFPO'.

      call function 'MMPUR_REQUISITION_DISPLAY'
        exporting
          im_banfn = lw_saida_alv-banfn
        exceptions
          others   = 1.

      if sy-subrc <> 0.
        message id sy-msgid type 'S' number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

    when others.
  endcase.

endform.                    " F_HYPERLINK

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

form f_mensagem_bapiret using p_mess type bapiret2.

  message id p_mess-id type 'S' number p_mess-number
    with p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

endform.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
form f_mensagem_insere_txt using i_type type bapi_mtype
                                 p_string type string.

  data: lt_trtexts     type trtexts,
        lw_trtexts     type trtext,
        lv_texto(4000).

  data lv_msg1 type sy-msgv1.
  data lv_msg2 type sy-msgv1.
  data lv_msg3 type sy-msgv1.
  data lv_msg4 type sy-msgv1.

  lv_texto = p_string.

  call function 'TR_SPLIT_TEXT'
    exporting
      iv_text  = lv_texto
      iv_len   = 30
    importing
      et_lines = lt_trtexts.

  loop at lt_trtexts assigning field-symbol(<fs_line>).

    case sy-tabix.
      when 1.
        lv_msg1 = <fs_line>.
      when 2.
        lv_msg2 = <fs_line>.
      when 3.
        lv_msg3 = <fs_line>.
      when 4.
        lv_msg4 = <fs_line>.
    endcase.

  endloop.

  perform f_mensagem_insere
    tables gt_bapiret2
     using i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
form f_mensagem_insere tables p_ret_tab structure bapiret2
                        using i_type type bapi_mtype
                              i_id  type  symsgid
                              i_number  type  symsgno
                              i_mess_v1 type any
                              i_mess_v2 type any
                              i_mess_v3 type any
                              i_mess_v4 type any.

  append initial line to p_ret_tab assigning field-symbol(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  message id <fs_ret>-id type <fs_ret>-type number <fs_ret>-number
    with <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 into <fs_ret>-message.

endform.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
form f_mensagem_exibe_popup using p_bapiret2_tab type bapiret2_t.

  data: l_lines type i.

  describe table p_bapiret2_tab lines l_lines.

  if l_lines <= 1.

    loop at p_bapiret2_tab assigning field-symbol(<fs_ret2>).

      message id <fs_ret2>-id
            type 'S'
          number <fs_ret2>-number
            with <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 display like <fs_ret2>-type.

    endloop.

  else.

    call function 'MESSAGES_INITIALIZE'.

    loop at p_bapiret2_tab assigning <fs_ret2>.

      if <fs_ret2>-id is initial or <fs_ret2>-system <> sy-sysid.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      endif.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.     "#EC CI_SUBRC

    endloop.

    call function 'MESSAGES_STOP'
      exceptions
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        others    = 5.     "#EC CI_SUBRC

    call function 'MESSAGES_SHOW'
      exporting
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      exceptions
        inconsistent_range  = 1
        no_messages         = 2
        others              = 3.     "#EC CI_SUBRC

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
form f_coluna_edita  using p_fieldname type slis_fieldname
                           p_text type scrtext_l.

*  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
*    WITH KEY fieldname = p_fieldname.
*
*  CHECK sy-subrc EQ 0.
*
*  <fs_cat>-seltext_s = p_text.
*  <fs_cat>-seltext_m = p_text.
*  <fs_cat>-seltext_l = p_text.

endform.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
form f_verifica_linha_selec changing p_error type c.

  read table gt_dados_alv with key selec = 'X' transporting no fields.

  if sy-subrc ne 0.
    message s851(v4) display like 'E'.
    p_error = 'X'.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
form f_processa.

  data lr_selec type range of flag.

  data lv_ret.

  if sy-batch is initial.

    perform f_verifica_linha_selec changing lv_ret.

    check lv_ret is initial.

    perform f_popup_to_confirm using text-t01 changing lv_ret.

    check lv_ret = '1'.

    append 'IEQX' to lr_selec.

  else.
    clear lr_selec.

  endif.

  loop at gt_dados_alv assigning field-symbol(<fs_dados>) where selec in lr_selec.



  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
form f_popup_to_confirm using p_question type c
                     changing p_answer type c.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar       = sy-title
      text_question  = p_question
    importing
      answer         = p_answer
    exceptions
      text_not_found = 1
      others         = 2.

  if sy-subrc <> 0.
    perform f_mensagem_sistema.
  endif.

endform.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_SELEC
*&---------------------------------------------------------------------*
form f_processa_selec using p_erro type c.

  check p_erro is initial.

  perform f_sap_indicator using 'processando dados...' 50.

  loop at gt_eban assigning field-symbol(<fs_eban>).

    read table gt_t001k assigning field-symbol(<fs_t001k>)
      with key bwkey = <fs_eban>-werks.

    if sy-subrc ne 0.

      " lançar erro  - "Centro <fs_eban>-werks não existe na T001K"
      perform f_mensagem_insere
        tables gt_bapiret2
         using 'E' 'DS' '016' 'Centro'
               <fs_eban>-werks 'não existe na' 'T001K'.

      if sy-sysid ne 'DEV'.
        continue.
      endif.


    endif.

    read table gt_set assigning field-symbol(<fs_set>)
      with key from = <fs_t001k>-bukrs.

    if sy-subrc ne 0.

      " lançar erro - "Centro X não está habilitado no COUPA"
      perform f_mensagem_insere
      tables gt_bapiret2
       using 'E' 'DS' '016' 'Centro'
             <fs_eban>-werks 'não está habilitado no' 'COUPA'.

      if sy-sysid ne 'DEV'.
        continue.
      endif.

    endif.

    read table gt_t001w assigning field-symbol(<fs_t001w>)
      with key werks = <fs_eban>-werks.

    check sy-subrc eq 0.

    " 14.07.2022 - RAMON -->
    if r_pm = 'X'.
      read table gt_ebkn assigning field-symbol(<fs_ebkn>)
        with key banfn = <fs_eban>-banfn
                 bnfpo =  <fs_eban>-bnfpo.

      check sy-subrc eq 0.

      read table gt_aufk assigning field-symbol(<fs_aufk>)
        with key aufnr  = <fs_ebkn>-aufnr.

**      CHECK sy-subrc EQ 0.
    endif.

    " 14.07.2022 - RAMON --<


    append initial line to gt_dados_alv assigning field-symbol(<fs_dados_alv>).

    move-corresponding <fs_eban> to <fs_dados_alv>.

    <fs_dados_alv>-bukrs = <fs_t001k>-bukrs.
    <fs_dados_alv>-name1 = <fs_t001w>-name1.
    if <fs_ebkn> is assigned.
      <fs_dados_alv>-aufnr = <fs_ebkn>-aufnr.
    endif.

    perform f_atualiza_icon
      using <fs_dados_alv>-status_coupa
   changing <fs_dados_alv>-coupa_icon
            <fs_dados_alv>-color.

    add 1 to <fs_dados_alv>-total_itens.

    loop at gt_dados_alv assigning field-symbol(<fs_check>)
        where banfn = <fs_dados_alv>-banfn and bnfpo ne <fs_dados_alv>-bnfpo.
      add 1 to <fs_dados_alv>-total_itens.
    endloop.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_VARS
*&---------------------------------------------------------------------*
form f_clear_vars .

  clear: gt_eban, gt_t001k, gt_t001w, gt_set , gv_erro, gt_dados_alv, gt_bapiret2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_FORMATAR_MSGS_GLOBAL
*&---------------------------------------------------------------------*
form f_formatar_msgs_global.

  sort gt_bapiret2 by message_v1 message_v2 message_v3 message_v4.

  delete adjacent duplicates from gt_bapiret2 comparing message_v1 message_v2 message_v3 message_v4.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_LIBERAR
*&---------------------------------------------------------------------*
form f_liberar. "USING rs_selfield TYPE slis_selfield.

  data lv_erro type c.
  data lv_answer type c.

  perform f_validacoes_liberar changing lv_erro.

  clear gt_bapiret2.

  check lv_erro is initial.

  perform f_popup_to_confirm
    using text-t01
 changing lv_answer.

  check lv_answer eq '1'.

  loop at it_selected_rows into wa_selected_rows.

    read table gt_dados_alv assigning field-symbol(<fs_dados>) index wa_selected_rows-index.
    if <fs_dados>-status_coupa = gc_enviado.
      "LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec = 'X' AND status_coupa = gc_enviado.

*      IF <fs_dados>-bnfpo = '00010'. " <--- chama uma vez só para atualizar pelo banfn

      perform f_atualiza_ebban
        using <fs_dados>-banfn
              <fs_dados>-bnfpo
              <fs_dados>-id_coupa
              gc_liberado
              <fs_dados>-afnam
              <fs_dados>
     changing lv_erro.

*      ENDIF.

      if lv_erro is initial.

        <fs_dados>-status_coupa = gc_liberado.

        perform f_atualiza_icon
          using <fs_dados>-status_coupa
       changing <fs_dados>-coupa_icon
                <fs_dados>-color.

      endif.

    endif.

  endloop.


endform.

form f_requisitante.  "USING rs_selfield TYPE slis_selfield.

  data :
    lt_return  type table of bapiret2         with header line,
    lt_pr_itm  type table of bapimereqitemimp with header line,
    lt_pr_itmx type table of bapimereqitemx   with header line.


  loop at it_selected_rows into wa_selected_rows.

    "LOOP AT gt_dados_alv INTO DATA(lw_dados) WHERE selec = 'X'.

    read table gt_dados_alv into data(lw_dados) index wa_selected_rows-index.
    if sy-subrc eq 0.

      select single prio_urg
        from eban
        into @data(vprio)
        where banfn = @lw_dados-banfn
        and   bnfpo = @lw_dados-bnfpo.
      refresh: lt_return,lt_pr_itm,lt_pr_itmx.
      lt_pr_itm-preq_item  = lw_dados-bnfpo.
      lt_pr_itm-preq_name  = lw_dados-afnam.
      if vprio = 0.
        lt_pr_itm-prio_urgency = '02'.
      endif.
      append lt_pr_itm.
      "
      lt_pr_itmx-preq_item = lw_dados-bnfpo.
      lt_pr_itmx-preq_name           = 'X'.
      if vprio = 0.
        lt_pr_itmx-prio_urgency    = 'X'.
      endif.
      append lt_pr_itmx.
      call function 'BAPI_PR_CHANGE'
        exporting
          number  = lw_dados-banfn
        tables
          return  = lt_return
          pritem  = lt_pr_itm
          pritemx = lt_pr_itmx.

      clear : lt_return.
      read table lt_return with key type = 'E'.
      if sy-subrc = 0.
        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.
      endif.

    endif.

  endloop.

endform.


*&---------------------------------------------------------------------*
*&      Form  F_ENVIAR
*&---------------------------------------------------------------------*
form f_enviar. "USING rs_selfield TYPE slis_selfield.

  data lv_status_coupa type zemm_status_coupa.
  data lv_id_coupa type zemm_id_coupa.

  data lv_count type i.
  data lv_erro type c.
  data lt_dados_envio type table of zmms_dados_int_coupa_eban.
  data lt_inter_tab type zmmc_dados_int_coupa_intermedi.
  data lt_dados_retorno type zmmc_dados_int_coupa_ret.

  loop at gt_dados_alv  assigning field-symbol(<fs_dados>).
    clear <fs_dados>-selec.
    modify gt_dados_alv from <fs_dados> index sy-tabix transporting selec.
  endloop.

  clear lv_erro.

  loop at it_selected_rows into wa_selected_rows.

    read table gt_dados_alv assigning <fs_dados> index wa_selected_rows-index. "selec = 'X'.

    if sy-subrc eq 0.
      if <fs_dados>-status_coupa = gc_enviado or <fs_dados>-status_coupa = gc_liberado.
*      TRY .
*          zcl_int_ob_cons_req_coupa=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = <fs_dados> IMPORTING e_integracao = DATA(r_rusult) ).
*
*          IF r_rusult IS NOT INITIAL.
*            PERFORM f_mensagem_exibe
*            USING 'E'
*                'Há linhas selecionadas que'
*                'não podem ser enviadas para o COUPA'
*                space space.
*            EXIT.
*          ENDIF.
*        CATCH zcx_integracao.    "
*
*      ENDTRY.

        lv_erro = 'X'.

        perform f_mensagem_exibe
          using 'E'
                'Há linhas selecionadas que'
                'não podem ser enviadas para o COUPA'
                space space.
        exit.
      else.
        <fs_dados>-selec = 'X'.
        modify gt_dados_alv from <fs_dados> index wa_selected_rows-index transporting selec.
      endif.

*      READ TABLE gt_zmmt0173 INTO DATA(wa_zmmt0173) WITH KEY grup_comp_sap = <fs_dados>-ekgrp.
*      IF sy-subrc NE 0.
*        PERFORM f_mensagem_exibe
*         USING 'E'
*               'Grupo de comprador'
*               'não cadastrado na Tabela ZMMT0173'
*               space space.
*        EXIT.
*      ENDIF.

    endif.

  endloop.

  if lv_erro = 'X'.
    exit.
  endif.

  sort gt_dados_alv by banfn bnfpo.

  perform f_corrige_selecao.

  clear gt_bapiret2.
  if r_pm = 'X'.
    refresh gt_dados_aux.
    loop at gt_dados_alv into data(lw_dados) where  selec = 'X'  .

* Ini - 2000008277/IR181786 - Stefanini - PRB - 27/05/2024 - Correção de integração manual Coupa
      data(lv_afnam) = lw_dados-afnam.
      data(lv_prio_urg) = lw_dados-prio_urg.
* Fim - 2000008277/IR181786 - Stefanini - PRB - 27/05/2024

      " ---- a cada nova req.
      at new banfn.

        clear lv_count.

        clear: lt_dados_envio, lt_inter_tab.
      endat.

      " ---- a cada novo item.
      at new bnfpo.

        append initial line to lt_dados_envio assigning field-symbol(<fs_envio>).

        add 1 to lv_count.

        read table gt_eban assigning field-symbol(<fs_eban>)
          with key banfn = lw_dados-banfn
                   bnfpo = lw_dados-bnfpo.

        if sy-subrc eq 0.

* Ini - 2000008277/IR181786 - Stefanini - PRB - 27/05/2024 - Correção de integração manual Coupa
          if <fs_eban>-afnam ne lv_afnam.
            <fs_eban>-afnam = lv_afnam.
          endif.

          if <fs_eban>-prio_urg ne lv_prio_urg.
            <fs_eban>-prio_urg = lv_prio_urg.
          endif.

          clear: lv_afnam, lv_prio_urg.
* Fim - 2000008277/IR181786 - Stefanini - PRB - 27/05/2024

*          READ TABLE gt_zmmt0174 INTO DATA(wa_zmmt0174_ekgrp) WITH KEY grup_comp_sap = <fs_dados>-ekgrp BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            <fs_eban>-ekgrp = wa_zmmt0174_ekgrp-grup_comp_coupa.
*          ENDIF.

          move-corresponding <fs_eban> to <fs_envio>.

          perform f_dados_envio_item using <fs_eban> changing <fs_envio>.

          append lw_dados to gt_dados_aux.

        endif.

      endat.

      " ---- a final da req.
      at end of banfn .
        lv_count = 99.
      endat.

      if lv_count ge 99.

*        " se não selecionou todos os itens da req.
*        if lv_count < <fs_dados>-total_itens.
*
*          perform f_mensagem_exibe
*            using 'E'
*                  'Para enviar para o COUPA as requisições'
*                  'precisam ser selecionadas'
*                  'todas as linhas da mesma requisição'
*                  space.
*
*          " #duvida, retiro só o que deu erro e sigo, ou paro o processamento?
*          lv_erro = 'X'.
*
*          "DELETE lt_dados_envio WHERE banfn = <fs_dados>-banfn.
*
*          exit.
*
*        endif.

        append initial line to lt_dados_retorno assigning field-symbol(<fs_dados_ret>).

        " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO -->
        perform f_envia_ordem using lt_dados_envio.
        " 05.08.2022 - RAMON - ENVIO DA ORDEM DE PRODUÇÃO --<

        perform f_envia_req_compra
          using lt_dados_envio
       changing lv_id_coupa
                lv_status_coupa
                lv_erro.

        if lv_erro is initial.
          loop at gt_dados_alv assigning field-symbol(<fs_dados_atu>) where selec = 'X'.
            read table gt_dados_aux into data(wa_dados_aux) with key bukrs = <fs_dados_atu>-bukrs
                                                                     banfn = <fs_dados_atu>-banfn
                                                                     bnfpo = <fs_dados_atu>-bnfpo.
            if sy-subrc ne 0.
              continue.
            endif.
            perform f_atualiza_ebban
                using <fs_dados_atu>-banfn
                      <fs_dados_atu>-bnfpo
                      lv_id_coupa
                      lv_status_coupa
                      <fs_dados_atu>-afnam
                      <fs_dados_atu>
              changing lv_erro.

            <fs_dados_atu>-status_coupa = lv_status_coupa.
            <fs_dados_atu>-id_coupa = lv_id_coupa.

            perform f_atualiza_icon
              using <fs_dados_atu>-status_coupa
           changing <fs_dados_atu>-coupa_icon
                    <fs_dados_atu>-color.


          endloop.

        endif.
        clear lv_count.

        clear: lt_dados_envio, lt_inter_tab.
        refresh gt_dados_aux.
      endif.

*      endat.

    endloop.


  else. "Agrupa RC diferentes, de acordo com a seleção do usuário

    clear lv_count.
    clear: lt_dados_envio, lt_inter_tab.
    loop at gt_dados_alv into lw_dados where selec = 'X'.

      append initial line to lt_dados_envio assigning <fs_envio>.

      read table gt_eban assigning <fs_eban>
        with key banfn = lw_dados-banfn
                 bnfpo = lw_dados-bnfpo.

      if sy-subrc eq 0.

        if <fs_eban>-afnam ne lw_dados-afnam.
          <fs_eban>-afnam = lw_dados-afnam.
        endif.

        if <fs_eban>-prio_urg ne lw_dados-prio_urg.
          <fs_eban>-prio_urg = <fs_dados>-prio_urg.
        endif.

        move-corresponding <fs_eban> to <fs_envio>.

        perform f_dados_envio_item using <fs_eban> changing <fs_envio>.

      endif.
    endloop.
    append initial line to lt_dados_retorno assigning <fs_dados_ret>.

    perform f_envia_req_compra
      using lt_dados_envio
   changing lv_id_coupa
            lv_status_coupa
            lv_erro.

    if lv_erro is initial.
      loop at gt_dados_alv assigning <fs_dados_atu> where selec = 'X'.
        perform f_atualiza_ebban
            using <fs_dados_atu>-banfn
                  <fs_dados_atu>-bnfpo
                  lv_id_coupa
                  lv_status_coupa
                  <fs_dados_atu>-afnam
                  <fs_dados_atu>
          changing lv_erro.

        <fs_dados_atu>-status_coupa = lv_status_coupa.
        <fs_dados_atu>-id_coupa = lv_id_coupa.

        perform f_atualiza_icon
          using <fs_dados_atu>-status_coupa
       changing <fs_dados_atu>-coupa_icon
                <fs_dados_atu>-color.


      endloop.
    endif.

  endif.


  if line_exists( gt_bapiret2[ type = 'E' ] ).
    perform f_mensagem_exibe_popup using gt_bapiret2.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDACOES_LIBERAR
*&---------------------------------------------------------------------*
form f_validacoes_liberar changing p_erro type c.

  data lv_count type i.


  read table it_selected_rows into wa_selected_rows index 1.

  read table gt_dados_alv into data(wa_dados_alv1) index wa_selected_rows-index. "WITH KEY status_coupa = gc_enviado.

  if sy-subrc ne 0 and wa_dados_alv1-status_coupa ne gc_enviado.
    perform f_mensagem_exibe
    using 'E'
          'Para liberar é necessário'
          'estar no status ENVIADO ao COUPA'
          space space.
    p_erro = 'X'.
  endif.

  loop at it_selected_rows into wa_selected_rows.
    read table gt_dados_alv assigning field-symbol(<fs_dados>) index wa_selected_rows-index.
    if sy-subrc eq 0.
      add 1 to lv_count.
    endif..
  endloop.

  if lv_count > 1.

    perform f_mensagem_exibe
     using 'E'
           'Para liberar é necessário'
           'escolher apenas uma linha'
           space space.

    p_erro = 'X'.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERTE_EBAN
*&---------------------------------------------------------------------*
form f_converte_eban using p_eban_tab type mereq_t_eban
                  changing p_inte_tab type zmmc_dados_int_coupa_intermedi.

  break-point.

  call function 'ZMM_CONVERTE_EBAN_COUPA_INTERM'
    exporting
      it_eban         = p_eban_tab
    importing
      et_intermed_tab = p_inte_tab.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_REQ_COMPRA
*&---------------------------------------------------------------------*
form f_envia_req_compra using p_dados_tab type zmmc_dados_int_coupa_eban
                     changing p_id_coupa type zemm_id_coupa
                              p_stat_coupa type zemm_status_coupa
                              p_error type c.

  data: lo_xml_ret  type ref to cl_xml_document,
        lc_retorno  type zde_zmme0002,
        zdata       type string,
        l_data      type table of w3mime,
        t_arquivo   type table of bdn_con,
        t_zmmt0181  type table of zmmt0181,
        it_zmmt0181 type table of zmmt0181.

  data: it_lista      type table of bdn_con,
        lc_objkey     type swotobjid-objkey,
        document_id   type sofolenti1-doc_id,
        document_data type sofolenti1,
        cont_hex      type table of solix,
        wa_anexos     type zde_zmme0001,
        it_anexos     type zde_zmme0001_t,
        vg_obkey      type swotobjid-objkey.

  data: e_xstring type xstring,
        e_string  type string.

  check p_dados_tab is not initial.

  clear gt_bapiret2.

  p_error = 'X'. "<- se nao for erro, vai ser limpado

  try.
      create object go_int
        exporting
          i_servico = gc_service
          i_req     = p_dados_tab.

      if go_int is bound.
*&-------Inicio melhoria chamado CS2023000129 / AOENNING------------*
*        Buscando id dos documentos anexado a requisição.

        select * from zmmt0181 into corresponding fields of table it_zmmt0181 for all entries in p_dados_tab
          where id_documento eq p_dados_tab-banfn.

        loop at p_dados_tab into data(wa_dados).
*          select * from srgbtbrel into table @data(t_srgbtbrel) where instid_a eq @wa_dados-banfn and typeid_a = 'BUS2105'.

          clear: vg_obkey.
          vg_obkey = wa_dados-banfn.

          call function 'BDS_GOS_CONNECTIONS_GET'
            exporting
*             LOGICAL_SYSTEM     =
              classname          = 'BUS2105'
              objkey             = vg_obkey
              client             = sy-mandt
            tables
              gos_connections    = t_arquivo
            exceptions
              no_objects_found   = 1
              internal_error     = 2
              internal_gos_error = 3
              others             = 4.

          if t_arquivo is not initial.
*            DESCRIPT - Descrição
*            DOCUCLASS - Tipo de documento
            loop at t_arquivo assigning field-symbol(<l_arq>).

              "Verifica se o arquivo ja foi integrado com COUPA.
              read table it_zmmt0181 into data(ws_zmmt0181) with key id_documento = wa_dados-banfn
                                                                     sibfboriid   = <l_arq>-loio_id.

              if sy-subrc eq 0.
                continue.
              endif.


              wa_anexos-id_documento = wa_dados-banfn.
              wa_anexos-sibfboriid   = <l_arq>-loio_id.

              document_id = <l_arq>-loio_id.

              call function 'SO_DOCUMENT_READ_API1'
                exporting
                  document_id                = document_id
                importing
                  document_data              = document_data
                tables
                  contents_hex               = cont_hex
                exceptions
                  document_id_not_exist      = 1
                  operation_no_authorization = 2
                  x_error                    = 3
                  others                     = 4.


              clear: e_xstring.
              loop at cont_hex into data(wa_cont_hex).
                e_xstring = e_xstring && wa_cont_hex-line.
                e_string = e_string && wa_cont_hex-line.

              endloop.

              wa_anexos-doc_xstring = e_xstring.
              wa_anexos-doc_string  = e_string.
              wa_anexos-descript    = <l_arq>-descript. "Descrição do documento
              wa_anexos-docuclass   = <l_arq>-docuclass. "Tipo de documento (ex: xml,pdf, xls, txt, etc )

              call function 'SSFC_BASE64_ENCODE'
                exporting
                  bindata                  = e_xstring
*                 BINLENG                  =
                importing
                  b64data                  = wa_anexos-docbase64
                exceptions
                  ssf_krn_error            = 1
                  ssf_krn_noop             = 2
                  ssf_krn_nomemory         = 3
                  ssf_krn_opinv            = 4
                  ssf_krn_input_data_error = 5
                  ssf_krn_invalid_par      = 6
                  ssf_krn_invalid_parlen   = 7
                  others                   = 8.

              append wa_anexos to it_anexos.
              clear: wa_anexos, ws_zmmt0181.

            endloop.
          endif.
          exit. " 168431-IR224914 - ZMM0182 dump memory - anexos
        endloop.

        try .
*         "Enviar anexo para Coupa.
            free: t_zmmt0181.
            loop at it_anexos into wa_anexos.
              zcl_int_ob_send_anexo_req=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = wa_anexos importing e_integracao = data(r_response) ).
              if r_response is not initial.
                clear: zdata, lc_retorno.
                zdata = r_response-ds_data_retorno.
                replace all occurrences of '-' in zdata with '_'.

                /ui2/cl_json=>deserialize( exporting json = zdata changing data = lc_retorno ).
                if lc_retorno is not initial.

                  append value #(
                  id_documento = wa_anexos-id_documento
                    sibfboriid = wa_anexos-sibfboriid
                      descript = wa_anexos-sibfboriid
                     docuclass = wa_anexos-docuclass
              id_retorno_anexo = lc_retorno-id
                    us_criacao = sy-uname
                    hr_criacao = sy-uzeit
                    dt_criacao = sy-datum
                   zcreated_at = lc_retorno-created_at
                   zupdated_at = lc_retorno-updated_at
                         ztype = lc_retorno-type
                        intent = lc_retorno-intent
                         zfile = lc_retorno-file
                     zfile_url = lc_retorno-file_url
               zfile_file_size = lc_retorno-file_file_size
                  ) to t_zmmt0181.
                endif.
              endif.
            endloop.

            if t_zmmt0181[] is not initial.
              modify zmmt0181 from table t_zmmt0181.
              commit work.

            endif.
          catch zcx_integracao.
          catch zcx_error.
        endtry.

*&-------Fim melhoria chamado CS2023000129 / AOENNING------------*

        data(lv_id_ret) = go_int->zif_integracao_coupa_req_comp~enviar_coupa( ).

        if lv_id_ret co '0123456789'.

          p_stat_coupa = gc_enviado.

          p_id_coupa = lv_id_ret.

          p_error = space.

        endif.

      endif.

    catch zcx_integracao into data(ex_int).

      data(lv_text) = ex_int->get_longtext( ).

      perform f_mensagem_insere_txt using 'E' lv_text.

    catch zcx_error into data(ex_erro).

      select single ds_data_retorno into lc_data_view from zintegracao where id_integracao = ex_erro->zif_error~msgv1.
      if sy-subrc = 0.
        cl_abap_browser=>show_html(
          exporting
            html_string = lc_data_view
            modal       = abap_false
            format      = cl_abap_browser=>landscape
            size        = cl_abap_browser=>small ).
      endif.

      lv_text = ex_erro->get_longtext( ).

      perform f_mensagem_insere_txt using 'E' lv_text.

  endtry.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_ENVIO_ITEM
*&---------------------------------------------------------------------*
form f_dados_envio_item using p_eban type eban
                     changing p_envio type zmms_dados_int_coupa_eban.

  read table gt_t001w assigning field-symbol(<fs_t001w>)
    with key werks = p_eban-werks.

  if sy-subrc eq 0.

    p_envio-t001w_name1 = <fs_t001w>-name1.

  endif.

  read table gt_t001k assigning field-symbol(<fs_t001k>)
    with key bwkey = p_eban-werks.

  if sy-subrc eq 0.

    read table gt_t001 assigning field-symbol(<fs_t001>)
      with key bukrs = <fs_t001k>-bukrs.

    if sy-subrc eq 0.
      p_envio-t001_bukrs = <fs_t001>-bukrs.
      p_envio-t001_butxt = <fs_t001>-butxt.
    endif.

  endif.

  read table gt_ebkn assigning field-symbol(<fs_ebkn>)
    with key banfn = p_eban-banfn
             bnfpo =  p_eban-bnfpo.

  if sy-subrc eq 0.

    p_envio-ebkn_kostl = <fs_ebkn>-kostl.
    p_envio-ebkn_sakto = <fs_ebkn>-sakto.
    p_envio-ebkn_aufpl_ord = <fs_ebkn>-aufpl_ord.
    p_envio-ebkn_aplzl_ord = <fs_ebkn>-aplzl_ord.
    p_envio-ebkn_aufnr = <fs_ebkn>-aufnr.

    " 22.03.2022 - RAMON LIMA - ALTERAÇÃO NA CONSULTA -->
    "p_envio-objnr = 'OV' && <fs_ebkn>-aufpl_ord && <fs_ebkn>-aplzl_ord.
    p_envio-objnr = 'OV' && <fs_ebkn>-aufpl && <fs_ebkn>-aplzl.
    " 22.03.2022 - RAMON LIMA - ALTERAÇÃO NA CONSULTA --<

    read table gt_skat assigning field-symbol(<fs_skat>)
      with key saknr = <fs_ebkn>-sakto.

    if sy-subrc eq 0.

      p_envio-skat_ktopl = <fs_skat>-ktopl.
      p_envio-skat_saknr = <fs_skat>-saknr.
      p_envio-skat_txt50 = <fs_skat>-txt50.

      read table gt_cskt assigning field-symbol(<fs_cskt>)
        with key kostl = <fs_ebkn>-kostl.

      if sy-subrc eq 0.
        p_envio-cskt_ltext = <fs_cskt>-ltext.
      endif.

*>>>Begin-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo
      data(lv_aufpl) = value #( gt_caufv[ aufnr = <fs_ebkn>-aufnr ]-aufpl optional ).

      if lv_aufpl is not initial.
        data(lv_steus) = value #( gt_afvc[ aufpl = lv_aufpl ]-steus optional ).

        if lv_steus = 'PM04' or lv_steus = 'PM03' or lv_steus = 'PM05'.
          p_envio-steus_afvc = lv_steus.
        endif.

      endif.

*<<<End-137646- Identificação de Req SCAE no SAP PM -27.06.2024 Vitor Rienzo

    endif.

    read table gt_npact assigning field-symbol(<fs_npact>)
      with key aufnr = <fs_ebkn>-aufnr
               objnr = p_envio-objnr.

    if sy-subrc eq 0.

      p_envio-v_npact_vornr = <fs_npact>-vornr.
      p_envio-v_npact_ltxa1 = <fs_npact>-ltxa1.

    endif.

    read table gt_aufk assigning field-symbol(<fs_aufk>)
      with key aufnr = <fs_ebkn>-aufnr.

    if sy-subrc eq 0.
      p_envio-aufk_ktext = <fs_aufk>-ktext.
    endif.
  else. "ESTOQUE
    p_envio-steus_afvc = 'ESTQ'.
  endif.

  read table gt_esll assigning field-symbol(<fs_esll>)
    with key packno = p_eban-packno.

  if sy-subrc eq 0.

    p_envio-esll_srvpos = <fs_esll>-srvpos.
    p_envio-esll_ktext1 = <fs_esll>-ktext1.
    p_envio-esll_extrow  = <fs_esll>-extrow.
    p_envio-esll_menge = <fs_esll>-menge.

    if p_envio-esll_srvpos is initial.

      read table gt_esll_sub assigning field-symbol(<fs_esll_sub>)
        with key packno = <fs_esll>-sub_packno.

      if sy-subrc eq 0.

        p_envio-esll_srvpos = <fs_esll_sub>-srvpos.
        p_envio-esll_ktext1 = <fs_esll_sub>-ktext1.
        p_envio-esll_extrow  = <fs_esll_sub>-extrow.
        p_envio-esll_menge = <fs_esll_sub>-menge.

      endif.

    endif.


  endif.

  perform f_recupera_cab_text using p_eban changing p_envio-cab_txt.

  perform f_retira_zeros
    changing: p_envio-ebkn_aufnr,
              p_envio-ebkn_sakto,
              p_envio-matnr,
              p_envio-esll_srvpos,
              p_envio-ebkn_kostl.
  "p_envio-v_npact_vornr. " 11.05.2022 - RAMON - deve ir com zeros a esquerda

  perform f_remove_char_special
    changing: p_envio-t001_bukrs,
              p_envio-t001_butxt,
              p_envio-t001w_name1,
              p_envio-aufk_ktext,
              p_envio-v_npact_ltxa1,
              p_envio-skat_txt50,
              p_envio-esll_ktext1,
              p_envio-cskt_ltext.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CORRIGE_SELECAO
*&---------------------------------------------------------------------*
form f_corrige_selecao .

*  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>).
*
*    READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
*    IF sy-subrc NE 0 AND <fs_dados>-status_coupa = gc_aguardando OR <fs_dados>-status_coupa = gc_liberado.
*
**    LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_correcao>) WHERE banfn = <fs_dados>-banfn
**                                                               AND selec = space
**                                                               AND status_coupa = gc_aguardando OR
**                                                               status_coupa = gc_liberado.
*
**      <fs_correcao>-selec = 'X'.
*      <fs_dados>-selec = 'X'.
*
**    ENDLOOP.
*
*    ENDIF.
*
*  ENDLOOP.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_EBBAN
*&---------------------------------------------------------------------*
form f_atualiza_ebban using p_banfn type banfn
                            p_bnfpo type bnfpo
                            p_id_coupa type zemm_id_coupa
                            p_status_coupa type zemm_status_coupa
                            p_afnam type eban-afnam
                            p_dados type zmms_dados_int_coupa_alv
                            p_erro type c.

  clear p_erro.

  if p_status_coupa is not initial and p_id_coupa is not initial.

    call function 'ENQUEUE_EMEBANE'
      exporting
        mode_eban      = 'E'
        mandt          = sy-mandt
        banfn          = p_banfn
        "bnfpo          = p_eban-bnfpo
        x_banfn        = ' '
        x_bnfpo        = ' '
        _scope         = '1'
        _wait          = ' '
        _collect       = ' '
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.

    if sy-subrc <> 0.
      perform f_mensagem_sistema_insere.
      p_erro = 'X'.
      exit.
    endif.

    update eban set id_coupa = p_id_coupa
                status_coupa = p_status_coupa
                afnam        = p_afnam
                prio_urg     = p_dados-prio_urg
                 where banfn = p_banfn
                 and   bnfpo = p_bnfpo.

  endif.

  if line_exists( gt_bapiret2[ type = 'E' ] ).

    rollback work.

    perform f_mensagem_exibe_popup using gt_bapiret2.

  else.

    commit work.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ICON
*&---------------------------------------------------------------------*
form f_atualiza_icon using p_status type zemm_status_coupa
                  changing p_coupa_icon type icon_d
                           p_color type c.

  clear p_color.

  case p_status.
    when gc_aguardando.
      p_coupa_icon = icon_generate.
    when gc_enviado.
      p_coupa_icon = icon_complete.
    when gc_nao_env.
      p_coupa_icon = ''. "#DUVIDA - QUAL ICONE VAI NESSA SITUAÇÃO
    when gc_liberado.
      p_coupa_icon = icon_page_right.
      p_color = 'C600'.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_RETIRA_ZEROS
*&---------------------------------------------------------------------*
form f_retira_zeros changing p_var type c.

  check p_var is not initial.

  shift p_var left deleting leading '0'.

endform.
*&---------------------------------------------------------------------*
*& FORM F_REMOVE_CHAR_SPECIAL
*&---------------------------------------------------------------------*
form f_remove_char_special changing p_text type c.

  data lv_text2 type char100.

  lv_text2 = p_text.

  call function 'ES_REMOVE_SPECIAL_CHARACTER'
    exporting
      text1       = lv_text2
    importing
      corr_string = lv_text2.

  p_text = lv_text2.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_CAB_TEXT
*&---------------------------------------------------------------------*
form f_recupera_cab_text using p_eban type eban
                      changing p_cab_txt type string_data.

  data lv_name type tdobname.
  data lt_lines type table of tline.

  clear p_cab_txt.

  lv_name = p_eban-banfn.

  call function 'READ_TEXT'
    exporting
      id       = 'B01'
      language = sy-langu
      name     = lv_name
      object   = 'EBANH'
    tables
      lines    = lt_lines
    exceptions
      others   = 1.

  if sy-subrc ne 0.
    exit.
  endif.

  check lt_lines is not initial.

  clear: p_cab_txt.
  loop at lt_lines assigning field-symbol(<ws_lines>).

    p_cab_txt = |{ p_cab_txt } { <ws_lines>-tdline }|.

*  p_cab_txt = lt_lines[ 1 ]-tdline.

  endloop.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_JOB
*&---------------------------------------------------------------------*
form f_verifica_job changing p_erro.

  call function 'ZMM_JOB_CHECK'
    exporting
      i_cprog   = sy-cprog
    importing
      ev_active = p_erro.

  if p_erro = 'X'.
    message s016(ds) with 'Já existe um job para o programa' sy-cprog display like 'E'.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_SET
*&---------------------------------------------------------------------*
form f_carrega_set  using p_setnr type c
                    changing p_set_tab type rgsbv_tab.

  call function 'G_SET_FETCH'
    exporting
      setnr           = p_setnr
    tables
      set_lines_basic = p_set_tab
    exceptions
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      others          = 4.

  if sy-subrc <> 0.
    "p_erro = 'X'.
    "EXIT.
  endif.


endform.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIA_ORDEM
*&---------------------------------------------------------------------*
form f_envia_ordem  using p_dados_tab type zmmc_dados_int_coupa_eban.

  data(lt_om) = p_dados_tab.


  sort lt_om by ebkn_aufnr.

  delete lt_om where ebkn_aufnr is initial.

  delete adjacent duplicates from lt_om comparing ebkn_aufnr.

  loop at lt_om assigning field-symbol(<fs_om>).

    read table gt_envio_om transporting no fields
      with key id_integr = <fs_om>-ebkn_aufnr.

    " somente se nao existe na tabela
    check sy-subrc ne 0.

    perform f_sap_indicator using 'Enviando ordem de manutenção...' 50.

    submit zmmr0035
            with s_lookup = 'OC'
            with p_op_obj = 'OM'
            with s_chave = <fs_om>-ebkn_aufnr
            with p_batch = 'X'  and return.

  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_STATUS_COUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_check_status_coupa .

*--------------------------------------------------------------------------------------------------------*
*   ""CS2023000015 Integraqção SAP MRPxCOUPA Consultar Status Requisição / AOENNING.
*--------------------------------------------------------------------------------------------------------*

  data: vg_requisicao type eban-banfn,
        vg_item_req   type eban-bnfpo,
        it_eban       type table of eban,
        it_eban_aux   type table of eban,
        ws_eban       type eban,
        lc_retorno    type zmme0007,
        zdata         type string.
  if gt_dados_alv is not initial.
    gt_dados_aux[] = gt_dados_alv[].
    delete gt_dados_aux where id_coupa is initial.
    sort gt_dados_aux by id_coupa.
    delete adjacent duplicates from gt_dados_aux comparing id_coupa.

    if gt_dados_aux is not initial.
      free: it_eban_aux.
      select * from eban into table it_eban_aux
        for all entries in gt_dados_aux
        where banfn eq gt_dados_aux-banfn
          and bnfpo eq gt_dados_aux-bnfpo.


    endif.

    loop at gt_dados_aux into data(wa_dados_aux).
      try .
          zcl_int_ob_cons_req_coupa=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = wa_dados_aux importing e_integracao = data(r_response) ).
          if r_response is not initial.
            clear: zdata, ws_eban.
            free: it_eban.
            zdata = r_response-ds_data_retorno.
            replace all occurrences of '-' in zdata with '_'.

            /ui2/cl_json=>deserialize( exporting json = zdata changing data = lc_retorno ).
            if lc_retorno is not initial.
              loop at lc_retorno-requisition_lines assigning field-symbol(<ls_data>).

                split <ls_data>-custom_fields-requisicao_sap at '/' into: vg_requisicao vg_item_req.
                condense: vg_requisicao no-gaps.
                condense: vg_item_req no-gaps.

                if vg_requisicao is not initial and vg_requisicao is not initial.
                  append value #( banfn = vg_requisicao bnfpo = vg_item_req ) to it_eban.
                endif.
              endloop.

            endif.
          endif.

        catch zcx_integracao.    "

        catch zcx_error.

      endtry.
      loop at gt_dados_alv assigning field-symbol(<fs_dados>) where id_coupa = wa_dados_aux-id_coupa.
        "Verifica se a requisição foi excluida no COUPA.
        read table it_eban into ws_eban with key banfn = <fs_dados>-banfn
                                                 bnfpo = <fs_dados>-bnfpo.
        if sy-subrc ne 0.
          <fs_dados>-status_coupa = 'N'.
          <fs_dados>-id_coupa = ''.
          <fs_dados>-coupa_icon = icon_generate.

*&---------------------------------------------------------------------
*&    Inicio ajuste no status da requisição caso seja excluida no COUPA / 07-05-2024 / AOENNING.
*&---------------------------------------------------------------------
          read table it_eban_aux into ws_eban with key banfn = <fs_dados>-banfn
                                                       bnfpo = <fs_dados>-bnfpo.
          if sy-subrc eq 0.
            ws_eban-status_coupa = 'N'.
            ws_eban-id_coupa     = ''.
            modify eban from ws_eban.
            commit work.
          endif.
*&---------------------------------------------------------------------
*&    Fim ajuste no status da requisição caso seja excluida no COUPA / 07-05-2024 / AOENNING.
*&---------------------------------------------------------------------
        endif.
        clear: ws_eban.
      endloop.
    endloop.
  endif.
  delete gt_dados_alv where status_coupa  not in so_stat.


endform.


form comando using ucomm like sy-ucomm
                         selfield type kkblo_selfield.
  selfield = selfield.                                      "#EC CALLED
  case ucomm.
    when '&IC1'.
* Lê na tabela de saída

  endcase.

endform. "COMANDO
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text "evento para HotSpot
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form definir_eventos.
  perform f_carregar_eventos using:
                                 slis_ev_user_command 'COMANDO'.
*                                 SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

endform.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text "evento para HotSpot
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.
endform.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*& Form fm_end_of_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_end_of_selection .
  "PERFORM fm_filtros.
  call screen 0100.
endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STANDARD'.  "'ZMMR168_100'.
  set titlebar 'SET_TIT_ZMMR168_100'.

  perform fm_criar_objetos.

endmodule.
*&---------------------------------------------------------------------*
*& Form fm_criar_objetos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_criar_objetos.

  data: lva_data(22) type c,
        w_layout     type lvc_s_layo.

  data: lt_sort type lvc_t_sort,
        ls_sort type lvc_s_sort.

  data: gs_variant  type disvariant.
  gs_variant-report      = sy-repid.

  loop at gt_dados_alv assigning field-symbol(<fs_dados>).
    if <fs_dados>-knttp eq 'F'.
      <fs_dados>-prio_urg = 02.
    endif.
  endloop.


  refresh: lt_sort.
  clear: ls_sort.
  ls_sort-spos = '1'.
  ls_sort-fieldname = 'BANFN'.
  ls_sort-up        = abap_true.
  append ls_sort to lt_sort.
  ls_sort-spos = '2'.
  ls_sort-fieldname = 'BNFPO'.
  ls_sort-up        = abap_true.
  append ls_sort to lt_sort.

  perform f_monta_fieldcat.

  concatenate sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) into lva_data.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    exporting
       i_titulo  = 'Requisições de Compras para o COUPA'
    i_filtros = value zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
  changing
    split    = dg_splitter_1
    alv      = gob_gui_alv_grid
  )
  eq abap_true.


    create object event_receiver.
    set handler event_receiver->hotspot_click           for gob_gui_alv_grid.
    set handler event_receiver->get_ucomm               for gob_gui_alv_grid.
    set handler event_receiver->handle_toolbar          for gob_gui_alv_grid.
    set handler event_receiver->handle_data_changed_req for gob_gui_alv_grid.
    call method gob_gui_alv_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      exceptions
        error      = 1
        others     = 2.

    gwa_stable = value #(
                       row = abap_true
                       col = abap_true
                       ).

    w_layout-cwidth_opt    = abap_true.
    w_layout-zebra         = 'X'.
    w_layout-sel_mode      = 'A'.
    w_layout-box_fname     = gc_select_field.
    w_layout-col_opt       = abap_true.
    w_layout-info_fname    = 'COLOR'.

*    lw_layout-zebra             = abap_true.
*    lw_layout-colwidth_optimize = abap_true.
*    lw_layout-box_fieldname = gc_select_field.
*    lw_layout-info_fieldname = 'COLOR'.

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
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_sort.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_sort_asc.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_sort_dsc.
*    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_subtot.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_sum.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_to_office.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_print.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_pc_file.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_views.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_export.
    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_mb_filter.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_mb_variant.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_mb_view.
*    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_info.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_graph.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_find.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_find_more.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_maximum.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_minimum.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_detail.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_average.
    append wl_function to tl_function.

*    REFRESH tl_function.
    sort gt_dados_alv by banfn bnfpo.
    call method gob_gui_alv_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
*       i_default                     = 'X'
        is_variant                    = gs_variant
        it_toolbar_excluding          = tl_function
      changing
        it_outtab                     = gt_dados_alv
        it_fieldcatalog               = gt_fieldcat
        it_sort                       = lt_sort
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    call method gob_gui_alv_grid->set_toolbar_interactive.

  else.


    check gob_gui_alv_grid is not initial.

    call method gob_gui_alv_grid->refresh_table_display
      exporting
        is_stable = gwa_stable.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100_exit input.
  leave to screen 0.
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
    when 'EXIT'.
      leave to screen 0.
  endcase.
endmodule.
