*&---------------------------------------------------------------------*
*& Report  ZMMR053
*& Descrição: Baixa de reserva de materiais
*&---------------------------------------------------------------------*

report zmmr053.

tables: resb, rkpf.
selection-screen: begin of block b1 with frame title text-008.
  select-options:
    p_rsnum for resb-rsnum,
    p_werks for resb-werks,
    p_matnr for resb-matnr,
    p_kostl for rkpf-kostl,
    p_date  for rkpf-rsdat obligatory.
selection-screen end of block b1.

selection-screen: begin of block b2 with frame title text-007.
  parameter p_open type char1 radiobutton group gr1 user-command radio default 'X'.

  parameter p_clos type char1 radiobutton group gr1.

selection-screen end of block b2.

types:
  begin of ty_item,
    node_key      type lvc_nkey,
    reserva       type resb-rsnum,
    item          type resb-rspos,
    filial        type resb-werks,
    material      type resb-matnr,
    material_text type makt-maktx,
    filial_text   type t001w-name1,
    centro_custo  type csks-kostl,
    deposito      type mchb-lgort,
    quantidade    type bdmng, "P,
    qtd_a_retirar type bdmng, "P,
    charg         type resb-charg,
    recebedor     type resb-wempf,
    data_neces    type resb-bdter,
    data_entrega  type zmmt0087-data,
    loc_material  type mard-lgpbe,
    matricula     type pa0001-pernr,
    colaborador   type pa0001-ename,
    data_lcto     type sy-datum,
    entregador    type sy-uname,
    documento     type mblnr,
    bktxt         type mkpf-bktxt,
    sgtxt         type mseg-sgtxt,
    nome_terc_emp type zmmt0081-nome_terc_emp,
    nome_terceiro type zmmt0081-nome_terceiro,
    bwart         type resb-bwart,
    umwrk         type resb-umwrk,
    umlgo         type resb-umlgo,
    aufnr         type aufk-aufnr,
    labst         type mard-labst, "BUG 156677 - MMSILVA - 02.12.2024
    enmng         type resb-enmng, "BUG 156677 - MMSILVA - 02.12.2024
    sum_banfn     type bdmng, "BUG 156677 - MMSILVA - 02.12.2024
    sum_ebeln     type bdmng, "BUG 156677 - MMSILVA - 02.12.2024
  end of ty_item,

  ty_t_res_items type table of ty_item with default key,
  ty_t_ficha_epi type table of zmme_ficha_controle_epi with default key,

  begin of ty_reservation_detail,
    reserva          type resb-rsnum,
    matricula        type pa0001-pernr,
    cargo            type stext,
    unidade          type j_1bbranc_,
    nome_unidade     type name1,
    cpf              type pa0465-cpf_nr,
    nome             type pa0001-ename,
    bktxt            type mkpf-bktxt,
    hash_left_thumb  type string,
    hash_right_thumb type string,
    hash_foto        type string,
    reservations     type ty_t_res_items,
    senha            type zmmt0120-senha,
  end of ty_reservation_detail,

  begin of ty_messages_detail,
    status       type icon-id,
    reserva      type resb-rsnum,
    item         type resb-rspos,
    qtd_retirada type p,
    documento    type mblnr,
    message      type bapi_msg,
  end of ty_messages_detail,

  begin of ty_fields,
    group type char3,
    value type num1,
  end of ty_fields,

  t_item         type table of ty_item  with empty key,
  t_ret          type table of ty_messages_detail with empty key,
  t_sol          type table of zmmt0081 with default key,
  t_aut          type table of zmmt0096 with default key,
  t_res_baixadas type table of zmmt0087 with default key.

data gt_reservations       type table of ty_item.
data gt_messages           type table of ty_messages_detail.
data gw_reservation_detail type ty_reservation_detail.
data width                 type i.
data height       type i.
data ext type i.

data subscreen_display     type sy-dynnr.
data subscreen_detail      type sy-dynnr value 0010.

data: vg_dias type p.

interface if_reservation_detail.
  methods user_command
    importing
      ucomm type sy-ucomm.

  methods process_before_output.

  methods set_screen_fields.

  methods set_detail_dynpro
    importing dynpro type sy-dynnr.

  methods display.

  methods display_messages.

  methods get_fieldcatalog
    importing
      name           type string
    returning
      value(r_table) type lvc_t_fcat.

  methods baixar_material
    importing
      item type ty_item.

  methods set_detail
    importing
      matricula type pa0465-pernr
      cpf       type pa0465-cpf_nr.

  methods register_digital
    exceptions
      registration_failed.

  methods set_return_detail
    importing
      detail type ty_messages_detail.

  methods get_return_details
    returning
      value(details) type t_ret.

  methods get_cpf
    importing
      matricula  type pa0001-pernr
    returning
      value(cpf) type pa0465-cpf_nr.

  methods has_errors
    returning value(value) type abap_bool.

  methods set_fields_group1
    importing
      enable  type abap_bool optional
      disable type abap_bool optional.

  methods set_fields_group2
    importing
      enable  type abap_bool optional
      disable type abap_bool optional.

  methods set_fields_group9
    importing
      enable  type abap_bool optional
      disable type abap_bool optional.

  methods get_user_data
    importing
      cpf       type pa0465-cpf_nr optional
      matricula type pa0001-pernr  optional.

  constants:
    closed type i value 5000,
    opened type i value 300.
endinterface.

interface if_display.
  methods process_before_output.

  methods user_command
    importing
      ucomm type sy-ucomm.

  methods display.
  methods set_title.
  methods set_status.

  methods select_reservations
    exceptions
      data_not_found.

  methods get_reservation_items
    returning
      value(table) type resb_t.

  methods get_reservations_header
    returning value(table) type ty_t_rkpf.

  methods get_res_solicitations
    returning value(table) type t_sol.

  methods get_autorizados
    returning value(table) type t_aut.

  methods get_res_baixadas
    returning value(table) type t_res_baixadas.

  methods get_display_dynnr
    returning value(r_dynpro) type sy-dynnr.

  methods set_display_dynnr
    importing i_dynpro type sy-dynnr.

  methods set_dock_extension
    importing
      extension type i.

  methods print_epi_list
    importing
      matricula type persno
      items     type ty_t_ficha_epi.

  methods set_register_events.

  methods set_hierarchy_reservations
    changing alv_tree type ref to cl_gui_alv_tree.

  methods get_tree_header
    returning value(r_table) type treev_hhdr.

  methods get_fieldcatalog
    returning value(r_table) type lvc_t_fcat.

  methods add_toolbar.

  methods get_material_description
    importing
      input         type any
    returning
      value(result) type makt-maktx.

  methods get_material_location
    importing
      input         type any
    returning
      value(result) type mard-lgpbe.

  "BUG 156677 - MMSILVA - 06.12.2024 - Inicio
  methods get_labst_item
    importing
      input         type any
    returning
      value(result) type mard-labst.

  methods get_enmng_item
    importing
      input         type any
    returning
      value(result) type resb-enmng.

  methods get_sum_banfn_item
    importing
      input         type any
    returning
      value(result) type i.

  methods get_sum_ebeln_item
    importing
      input         type any
    returning
      value(result) type i.
  "BUG 156677 - MMSILVA - 06.12.2024 - Fim

  methods get_descr_item
    importing
      input         type any
    returning
      value(result) type mseg-sgtxt.

  methods get_description_conta_razao
    importing
      input        type any
    returning
      value(value) type string.

  methods get_description_centro_custo
    importing
      input        type any
    returning
      value(value) type string.

  methods get_description_filial
    importing
      input        type any
    returning
      value(value) type string.

  methods get_name_by_matricula
    importing
      input        type any
    returning
      value(value) type string.

  methods material_movement_output
    importing
      header   type bapi2017_gm_head_01
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
*      items    TYPE bapi2017_gm_item_create_t
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
      vcode    type bapi2017_gm_code-gm_code
    exporting
      returns  type bapiret2_t
      document type bapi2017_gm_head_ret-mat_doc
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
    changing
      items    type bapi2017_gm_item_create_t
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
    exceptions
      error_when_processing.

  methods baixar_material
    importing
      hash_left_thumb  type string
      hash_right_thumb type string
      data             type bapi2017_gm_head_01-pstng_date
      reserva          type bapi2017_gm_item_create-reserv_no
      deposito         type bapi2017_gm_item_create-stge_loc
      item             type bapi2017_gm_item_create-res_item
      material         type bapi2017_gm_item_create-material
      filial           type bapi2017_gm_item_create-plant
      ccusto           type bapi2017_gm_item_create-costcenter
      lote             type bapi2017_gm_item_create-batch
      quantidade       type bapi2017_gm_item_create-quantity
      movimento        type bapi2017_gm_item_create-move_type
    exporting
      document         type bapi2017_gm_head_ret-mat_doc.

  methods save_log_baixa
    importing
      reserva         type rsnum
      item            type rspos
      document        type mblnr
      material        type matnr
      deposito        type lgort_d
      operacao        type zmmt0087-operacao
      qtd_retirada    type zmmt0087-quant
      hash_validation type string
      lado            type zmmt0081-lado
      polegar         type zmmt0081-polegar
      im_polegar      type zmmt0081-im_polegar
      senha           type zmmt0120-senha
      matricula       type persno.

  class-methods build_fieldcatalog
    importing
      fieldname   type lvc_fname
      description type scrtext_l  optional
      optimize    type lvc_colopt optional
      hotspot     type lvc_hotspt optional
      no_zero     type lvc_nozero optional
      checkbox    type lvc_checkb optional
      outputlen   type lvc_outlen optional
      icon        type lvc_icon   optional
      edit        type lvc_edit   optional
      ref_table   type lvc_rtname optional
      ref_field   type lvc_rfname optional
      tabname     type lvc_tname  optional
      no_out      type abap_bool  optional
      style       type lvc_style  optional
      f4          type ddf4avail  optional
      just        type c          optional
      sum         type abap_bool  optional
      quantity    type char3      optional
    changing
      r_table     type lvc_t_fcat.

  "//Events Methods
  methods handle_double_click for event node_double_click of cl_gui_alv_tree
    importing node_key.

  methods handle_item_double_click for event item_double_click of cl_gui_alv_tree
    importing node_key fieldname.

  methods handle_button_selected for event function_selected of cl_gui_toolbar
    importing fcode.

  methods handle_checkbox_change for event checkbox_change of cl_gui_alv_tree
    importing node_key checked fieldname.

  data alv_tree         type ref to cl_gui_alv_tree.
  data image_helper     type ref to zcl_image_helper.
  data biometry_service type ref to zcl_biometry.
endinterface.

class cl_util definition.
  public section.
    class-methods build_fieldcatalog
      importing
        fieldname   type lvc_fname
        description type scrtext_l  optional
        optimize    type lvc_colopt optional
        hotspot     type lvc_hotspt optional
        no_zero     type lvc_nozero optional
        checkbox    type lvc_checkb optional
        outputlen   type lvc_outlen optional
        icon        type lvc_icon   optional
        edit        type lvc_edit   optional
        ref_table   type lvc_rtname optional
        ref_field   type lvc_rfname optional
        tabname     type lvc_tname  optional
        style       type lvc_style  optional
        f4          type ddf4avail  optional
        just        type c          optional
        sum         type abap_bool  optional
        quantity    type char3      optional
      changing
        r_table     type lvc_t_fcat.

    class-methods build_fieldcatalogu
      importing
        fieldname   type lvc_fname
        description type scrtext_l  optional
        optimize    type lvc_colopt optional
        hotspot     type lvc_hotspt optional
        no_zero     type lvc_nozero optional
        checkbox    type lvc_checkb optional
        outputlen   type lvc_outlen optional
        icon        type lvc_icon   optional
        edit        type lvc_edit   optional
        tabname     type lvc_tname  optional
      changing
        r_table     type  slis_t_fieldcat_alv.
endclass.

class cl_util implementation.

  method build_fieldcatalog.
    data(fcat) = value lvc_s_fcat(
                       fieldname  = fieldname
                       scrtext_l  = description
                       scrtext_m  = description
                       scrtext_s  = description
                       reptext    = description
                       coltext    = description
                       checkbox   = checkbox
                       col_opt    = optimize
                       hotspot    = hotspot
                       no_zero    = no_zero
                       outputlen  = outputlen
                       do_sum     = sum
                       quantity   = quantity
                       icon       = icon
                       edit       = edit
                       style      = style
                       ref_table  = ref_table
                       ref_field  = ref_field
                       tabname    = tabname
                       f4availabl = f4
                       just       = just
                                ).
    append fcat to r_table.
  endmethod.

  method build_fieldcatalogu.
    data(fcat) = value  slis_fieldcat_alv(
                       fieldname  = fieldname
                       seltext_l  = description
                       seltext_m   = description
                       seltext_s   = description
                       checkbox   = checkbox
                       hotspot    = hotspot
                       no_zero    = no_zero
                       outputlen  = outputlen
                       edit       = edit
                       tabname    = tabname
                                ).
    append fcat to r_table.
  endmethod.
endclass.

class cl_detail definition.
  public section.
    interfaces if_reservation_detail.

    methods constructor
      importing
        display_instance type ref to if_display.

    methods call_for_biometry_register
      importing
        matricula type pa0465-pernr.

    methods handle_set_toolbar for event toolbar of cl_gui_alv_grid
      importing
        e_object.

    methods handle_user_command for event user_command of cl_gui_alv_grid
      importing
        e_ucomm.

    methods handle_data_changed for event data_changed of cl_gui_alv_grid
      importing
        er_data_changed
        e_onf4
        e_onf4_before
        e_onf4_after
        e_ucomm.

    data custom_messages  type ref to cl_gui_custom_container.
    data alv_grid         type ref to cl_gui_alv_grid.
    data custom_reservations type ref to cl_gui_custom_container.
    data alv_reservations type ref to cl_gui_alv_grid.
    data fields_group1    type table of ty_fields.
    data fields_group2    type table of ty_fields.
    data fields_group9    type table of ty_fields.
    data custom_photo     type ref to cl_gui_custom_container.
    data picture          type ref to cl_gui_picture.
    data display_instance type ref to if_display.
    data image_helper     type ref to zcl_image_helper.
    data biometry_service  type ref to zcl_biometry.
endclass.

class cl_detail implementation.
  method constructor.
    create object biometry_service.
    "//Received from display_instance calling
    me->display_instance = display_instance.
  endmethod.

  method handle_set_toolbar.
    if subscreen_detail = 0004.
      e_object->mt_toolbar = value #( ).
      e_object->mt_toolbar = value #( ( butn_type = 0
                                        function  = 'CLEAR_MESSAGES'
                                        icon      = icon_delete
                                        text      = 'Limpar mensagens'
                                      )
*
*                                    ( BUTN_TYPE = 0
*                                      DISABLED  = SWITCH #( ME->GET_ALV_FIELDS_MODE( ) WHEN ABAP_FALSE THEN ABAP_TRUE ELSE ABAP_FALSE )
*                                      FUNCTION  = 'DEL'
*                                      ICON      = ICON_DELETE_ROW
*                                      QUICKINFO = '' )
                                    ).
    else.
      clear e_object->mt_toolbar.
    endif.
  endmethod.

  method handle_user_command.
    case e_ucomm.
      when 'CLEAR_MESSAGES'.
        clear gt_messages.
        me->alv_grid->refresh_table_display( ).
      when 'BAIXAR_MATERIAIS'.
    endcase.
  endmethod.

  method handle_data_changed.
    loop at er_data_changed->mt_good_cells into data(_item_changed).

      if ( _item_changed-fieldname = 'QTD_A_RETIRAR' ).
        read table gw_reservation_detail-reservations assigning field-symbol(<fs_reservation>) index _item_changed-row_id.
        if ( <fs_reservation> is not assigned ).
          continue.
        endif.

        if _item_changed-value > <fs_reservation>-quantidade.
          message text-017 type 'I' display like 'E'.

          <fs_reservation>-qtd_a_retirar = <fs_reservation>-quantidade.
          leave to screen 0001.
        endif.
      endif.

*     BUG 156677 - MMSILVA - 02.12.2024 - Inicio
      if ( _item_changed-fieldname = 'DEPOSITO' ).
        read table gw_reservation_detail-reservations assigning field-symbol(<fs_labst>) index _item_changed-row_id.
        if ( <fs_labst> is not assigned ).
          continue.
        endif.

        data: it_mchb type table of mchb,
              wa_mchb type mchb,
              it_mard type table of mard,
              wa_mard type mard.

        select clabs
          from mchb
          where matnr = @<fs_labst>-material
          and   werks = @<fs_labst>-filial
          and   lgort = @_item_changed-value
          into corresponding fields of table @it_mchb
          up to 1 rows.


        if sy-subrc <> 0.
          select a~labst
            from mard as a
            where matnr = @<fs_labst>-material
            and   werks = @<fs_labst>-filial
            and   lgort = @_item_changed-value
            order by lfgja descending
            into corresponding fields of table @it_mard
            up to 1 rows.
        endif.

        if it_mchb is not initial.
          loop at it_mchb into wa_mchb.
            <fs_labst>-deposito = _item_changed-value.
            <fs_labst>-labst = wa_mchb-clabs.
            leave to screen 0001.
          endloop.
        elseif it_mard is not initial.
          if it_mchb is not initial.
            continue.
          endif.
          loop at it_mard into wa_mard.
            <fs_labst>-deposito = _item_changed-value.
            <fs_labst>-labst = wa_mard-labst.
            leave to screen 0001.
          endloop.
        elseif it_mchb is initial and it_mard is initial.
          <fs_labst>-deposito = _item_changed-value.
          <fs_labst>-labst = '0'.
          leave to screen 0001.
        endif.
      endif.
*     BUG 156677 - MMSILVA - 02.12.2024 - Fim

    endloop.
  endmethod.

  method if_reservation_detail~set_screen_fields.
    check me->fields_group1  is not initial
        or me->fields_group2 is not initial
        or me->fields_group9 is not initial.

    loop at screen.
      case subscreen_detail.
        when 0003.
          if screen-name = 'BTN_BACK'.
            screen-input = 0.
            modify screen.
          endif.
        when 0004.
          if screen-name = 'BTN_BAIXAR_MATERIAIS'.
            screen-input = 0.
            modify screen.
          endif.

          if screen-name = 'BTN_BACK'.
            if me->if_reservation_detail~has_errors( ) = abap_false.
              screen-input = 0.
              modify screen.
            endif.
          endif.
      endcase.

      loop at me->fields_group1 into data(_field_group1).
        if screen-group1 = _field_group1-group.
          screen-input = _field_group1-value.
          modify screen.
        endif.
      endloop.

      loop at me->fields_group2 into data(_field_group2).
        if screen-group1 = _field_group2-group.
          screen-invisible = _field_group2-value.
          modify screen.
        endif.
      endloop.

      loop at me->fields_group9 into data(_field_group9).
        if screen-group1 = _field_group9-group.
          screen-invisible = _field_group9-value.
          if _field_group9-value = '1'.
            screen-input = 0.
          else.
            if  screen-name = 'GW_RESERVATION_DETAIL-SENHA'.
              screen-invisible = 1.
            endif.
            screen-input = 1.
          endif.
          modify screen.
        endif.
      endloop.

    endloop.
  endmethod.

  method call_for_biometry_register.
    data(_parameters) =
        value bdcdata_tab(  ( program = 'ZMMR050'    dynpro = '0001' dynbegin = abap_true      )
                            ( fnam    = 'BDC_OKCODE' fval   = '=ENTER'   )
                            ( fnam    = 'SEARCH_ID'  fval   = matricula  )
                         ).

    call transaction 'ZMM0121' using _parameters mode 'E'.
  endmethod.

  method if_reservation_detail~set_detail_dynpro.

    if dynpro = '0010'.
      me->display_instance->set_dock_extension( 5000 ).
      me->display_instance->set_display_dynnr( '0010' ).
    else.
      me->display_instance->set_dock_extension( 200 ).
      me->display_instance->set_display_dynnr( '0002' ).
*      ENDIF.

      subscreen_detail = dynpro.
    endif.

  endmethod.

  method if_reservation_detail~process_before_output.
    data url type char255.

    case subscreen_detail.
      when 0003.

        "//Display photo
        me->display_instance->image_helper->display(
          exporting
            custom_name      = 'CUSTOM_PHOTO'
            url              = me->display_instance->image_helper->create_internal_url( gw_reservation_detail-hash_foto )
        changing
          custom_instance  = me->custom_photo
          picture_instance = me->picture
      ).

        me->if_reservation_detail~display( ).

      when 0004. "//Messages
        me->if_reservation_detail~display_messages( ).

      when 0005. "//Employee data

        "//Set screen fields
        me->if_reservation_detail~set_screen_fields( ).


*        IF URL IS INITIAL.
*          ME->DISPLAY_INSTANCE->IMAGE_HELPER->GET_PHOTO_URL(
*            EXPORTING
*              NAME   = 'LOGO_AMAGGI'
*            RECEIVING
*              RESULT = URL
*          ).
*        ENDIF.

*        ME->DISPLAY_INSTANCE->IMAGE_HELPER->DISPLAY(
*          EXPORTING
*            CUSTOM_NAME      = 'CUSTOM_PHOTO'
*            URL              = URL
*          CHANGING
*            CUSTOM_INSTANCE  = ME->CUSTOM_PHOTO
*            PICTURE_INSTANCE = ME->PICTURE
*        ).
*
*        CLEAR URL.
    endcase.
  endmethod.

  method if_reservation_detail~display.
    if ( me->custom_reservations is not bound ).
      create object me->custom_reservations
        exporting
          container_name = 'CUSTOM_OUTPUT_MATERIALS'.
    endif.

    "//Get Field Catalog
    data(fieldcat) = me->if_reservation_detail~get_fieldcatalog( '' ).

    data(layout)   =
      value lvc_s_layo( no_rowmark = abap_true no_vgridln = abap_true grid_title = |Reserva { conv i( gw_reservation_detail-reserva ) }: Itens selecionados ({ lines( gw_reservation_detail-reservations ) })| ).

    if ( me->alv_reservations is not bound ).
      "//Create Alv Object
      create object me->alv_reservations
        exporting
          i_parent = me->custom_reservations.

      set handler me->handle_set_toolbar  for me->alv_reservations.
      set handler me->handle_user_command for me->alv_reservations.
      set handler me->handle_data_changed for me->alv_reservations.

      "//Display Datas
      call method me->alv_reservations->set_table_for_first_display
        exporting
          is_layout       = layout
          i_save          = abap_on
        changing
          it_outtab       = gw_reservation_detail-reservations
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      call method me->alv_reservations->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      call method me->alv_reservations->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    else.
      me->alv_reservations->set_frontend_layout( layout ).
      me->alv_reservations->refresh_table_display( exporting is_stable = value #( col = abap_true row = abap_true ) ).
    endif.


  endmethod.

  method if_reservation_detail~get_fieldcatalog.

    if name = 'MESSAGE_DETAILS'.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname = 'STATUS'
          icon      = abap_true
          outputlen = 3
        changing
          r_table   = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'RESERVA'
          description = 'Reserva'
          outputlen   = 15
          no_zero     = abap_true
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'ITEM'
          description = 'Item'
          outputlen   = 5
        changing
          r_table     = r_table.

      if me->if_reservation_detail~has_errors( ) = abap_false.
        call method cl_util=>build_fieldcatalog
          exporting
            fieldname   = 'QTD_RETIRADA'
            description = 'Qtd. Retirada'
            outputlen   = 15
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalog
          exporting
            fieldname   = 'DOCUMENTO'
            description = 'Documento'
            outputlen   = 15
          changing
            r_table     = r_table.
      endif.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'MESSAGE'
          description = 'Mensagem'
          outputlen   = 70
        changing
          r_table     = r_table.

    else.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'RESERVA'
          description = 'Reserva'
          outputlen   = 8
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'ITEM'
          description = 'Item'
          outputlen   = 5
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'MATERIAL'
          description = 'Material'
          outputlen   = 12
          no_zero     = abap_true
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'MATERIAL_TEXT'
          description = 'Descrição'
          outputlen   = 25
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'QUANTIDADE'
          description = 'Qtd. reserva'
*         REF_TABLE   = 'RESB'
*         REF_FIELD   = 'BDMNG'
          outputlen   = 15
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'DEPOSITO'
          description = 'Depósito'
*         REF_TABLE   = 'MCHB'
*         REF_FIELD   = 'LGORT'
          edit        = abap_true
          outputlen   = 10
        changing
          r_table     = r_table.

*     BUG 156677 - MMSILVA - 02.12.2024 - Inicio
      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'LABST'
          description = 'Saldo em Estoque'
*         REF_TABLE   = 'MCHB'
*         REF_FIELD   = 'LGORT'
          outputlen   = 15
        changing
          r_table     = r_table.
*     BUG 156677 - MMSILVA - 02.12.2024 - Fim

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'CHARG'
          description = 'Lote'
*         REF_TABLE   = 'MCHB'
*         REF_FIELD   = 'LGORT'
          edit        = abap_true
          outputlen   = 10
        changing
          r_table     = r_table.

      call method cl_util=>build_fieldcatalog
        exporting
          fieldname   = 'QTD_A_RETIRAR'
          description = 'Qtd. a retirar'
          ref_table   = 'RESB'
          ref_field   = 'BDMNG'
          edit        = abap_true
          outputlen   = 15
        changing
          r_table     = r_table.

*      CALL METHOD CL_UTIL=>BUILD_FIELDCATALOG
*        EXPORTING
*          FIELDNAME   = 'SGTXT'
*          DESCRIPTION = 'Texto Item'
*          REF_TABLE   = 'MSEG'
*          REF_FIELD   = 'SGTXT'
*          EDIT        = ABAP_TRUE
*          OUTPUTLEN   = 15
*        CHANGING
*          R_TABLE     = R_TABLE.
      "

    endif.
  endmethod.

  method if_reservation_detail~baixar_material.

  endmethod.

  method if_reservation_detail~set_detail.
    data outtab_item_display type ty_item.
    data v_dt_desligamento type dats.
    if matricula+0(1) = '7'.
      call function 'RP_GET_FIRE_DATE'
        exporting
          persnr   = matricula
          status2  = '0'
        importing
          firedate = v_dt_desligamento.
      if v_dt_desligamento is not initial.
        message text-034 type 'S' display like 'E'.
        exit.
      endif.
    endif.

    zcl_hrst_commons=>get_employee(
       exporting
         matricula      = matricula
         cpf            = cpf
       receiving
         employee       = data(_employee)
       exceptions
         data_not_found = 1
         others         = 2
     ).

    if ( sy-subrc is initial ).
      gw_reservation_detail-matricula = _employee-matricula.
      gw_reservation_detail-cargo     = _employee-cargo.
      gw_reservation_detail-unidade   = _employee-unidade.
      gw_reservation_detail-nome_unidade = _employee-nome_unidade.
      gw_reservation_detail-cpf       = _employee-cpf.
      gw_reservation_detail-nome      = _employee-nome.
      gw_reservation_detail-hash_foto = _employee-foto_base64.

      try.
          me->display_instance->biometry_service->read_digital(
            exporting
              registration = gw_reservation_detail-matricula
            receiving
              result       = data(_result)
          ).
        catch zcx_biometry.
      endtry.

      gw_reservation_detail-hash_left_thumb  = _result-polegar_esquerdo.
      gw_reservation_detail-hash_right_thumb = _result-polegar_direito.

      if gw_reservation_detail-hash_left_thumb  is initial
      or gw_reservation_detail-hash_right_thumb is initial.
        me->if_reservation_detail~register_digital( exceptions registration_failed = 1 ).
      endif.

    else.
      data(_reservations) = gw_reservation_detail-reservations.
      clear gw_reservation_detail.

      try.
          gw_reservation_detail-reservations = _reservations.
          gw_reservation_detail-reserva      = _reservations[ 1 ]-reserva.
        catch cx_sy_itab_line_not_found.
      endtry.

      message text-003 type 'I' display like 'E'.
    endif.
  endmethod.

  method if_reservation_detail~register_digital.
    data answer type char1.

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar       = 'Digital não cadastrada'
        text_question  = |Ainda não existe digital cadastrada para a matricula { gw_reservation_detail-matricula } ({ gw_reservation_detail-nome }). Deseja cadastrar agora?|
        text_button_1  = 'Sim'
        text_button_2  = 'Não'
        default_button = '1'
      importing
        answer         = answer.

    "//Call transaction to register biometry
    if ( answer = '1' ).
      me->call_for_biometry_register( matricula = gw_reservation_detail-matricula ).
    endif.

    "//Try to find the biometry again
    try.
        me->display_instance->biometry_service->read_digital(
          exporting
            registration = gw_reservation_detail-matricula
          receiving
            result       = data(_result)
        ).
      catch zcx_biometry.
    endtry.

    gw_reservation_detail-hash_left_thumb  = _result-polegar_esquerdo.
    gw_reservation_detail-hash_right_thumb = _result-polegar_direito.

    if ( gw_reservation_detail-hash_left_thumb is not initial ).
      message text-011 type 'S'.
    else.
      message text-012 type 'I' display like 'E'.
      raise registration_failed.
    endif.
  endmethod.

  method if_reservation_detail~set_return_detail.
    append detail to gt_messages.
  endmethod.

  method if_reservation_detail~get_return_details.
    move gt_messages to details.
  endmethod.

  method if_reservation_detail~get_cpf.
    select single cpf_nr
    from pa0465
    into cpf
   where pernr = matricula.
  endmethod.

  method if_reservation_detail~has_errors.
    if line_exists( gt_messages[ status = icon_led_red ] ).
      move abap_true to value.
    endif.
  endmethod.

  method if_reservation_detail~set_fields_group1.
    "//Fields:
    "//-> GW_RESERVATION_DETAIL-MATRICULA
    "//-> GW_RESERVATION_DETAIL-CPF
    "//Screen: 0002

    if enable = 'X'.
      me->fields_group1 = value #( ( group = 'GR1' value = 1 ) ).
    elseif disable = 'X'.
      me->fields_group1 = value #( ( group = 'GR1' value = 0 ) ).
    endif.
  endmethod.

  method if_reservation_detail~set_fields_group2.
    "//Fields:
    "//-> GW_RESERVATION_DETAIL-MATRICULA
    "//-> GW_RESERVATION_DETAIL-CPF
    "//Screen: 0002

    if enable = 'X'.
      me->fields_group2 = value #( ( group = 'GR2' value = 0 ) ).
    elseif disable = 'X'.
      me->fields_group2 = value #( ( group = 'GR2' value = 1 ) ).
    endif.
  endmethod.

  method if_reservation_detail~set_fields_group9.
    "//Fields:
    "//-> GW_RESERVATION_DETAIL-MATRICULA
    "//-> GW_RESERVATION_DETAIL-CPF
    "//Screen: 0002

    if enable = 'X'.
      me->fields_group9 = value #( ( group = 'GR9' value = 0 ) ).
    elseif disable = 'X'.
      me->fields_group9 = value #( ( group = 'GR9' value = 1 ) ).
    endif.
  endmethod.

  method if_reservation_detail~user_command.
    case ucomm.
      when 'ENTER'.
        if subscreen_detail = '0003'.
          call method me->if_reservation_detail~set_detail
            exporting
              matricula = gw_reservation_detail-matricula
              cpf       = gw_reservation_detail-cpf.
          select single *
               from zmmt0120
               into @data(_zmmt0120)
              where matricula  eq @gw_reservation_detail-matricula.
          "
          if sy-subrc = 0.
            me->if_reservation_detail~set_fields_group9( enable  = abap_true ).
            me->if_reservation_detail~set_detail_dynpro( '0003' ).
          endif.
        endif.
      when 'BTN_CLOSE_VIEW'.
        me->if_reservation_detail~set_detail_dynpro( '0010' ).

      when 'BTN_BACK'.
        me->if_reservation_detail~set_detail_dynpro( '0003' ).

      when 'BTN_BAIXAR_MATERIAIS'.
        data movement_items type bapi2017_gm_item_create_t.
        data: lva_data(10).

        if gw_reservation_detail-matricula is initial.
          message text-004 type 'I' display like 'W'.
        else.
          data(_sair) = 'N'.
          data v_dt_desligamento type dats.
          if gw_reservation_detail-matricula+0(1) = '7'.
            call function 'RP_GET_FIRE_DATE'
              exporting
                persnr   = gw_reservation_detail-matricula
                status2  = '0'
              importing
                firedate = v_dt_desligamento.

            if v_dt_desligamento is not initial.
              concatenate  v_dt_desligamento+6(2) v_dt_desligamento+4(2) v_dt_desligamento(4) into lva_data separated by '.'.
              message |A matricula esta desativada desde { lva_data } | type 'S' display like 'E'.
              exit.
            endif.
          endif.

          loop at gw_reservation_detail-reservations into data(_reservation_check).
            if _reservation_check-bwart = '262'.
              select sum( enmng )
              from resb
              into @data(v_retirada)
              where rsnum = @_reservation_check-reserva
              and   bwart = '261'.
              "
              select sum( enmng )
              from resb
              into @data(v_devol)
              where rsnum = @_reservation_check-reserva
              and   bwart = '262'.
              "
              data(_saldo) = v_retirada - v_devol.
              if _reservation_check-qtd_a_retirar gt _saldo.
                _sair = 'S'.
                message |Não existe retirada para cobrir esta devolução na reserva! item { _reservation_check-item } | type 'S' display like 'E'.
              endif.
            endif.
          endloop.
          "
          if _sair = 'S'.
            exit.
          endif.

          if gw_reservation_detail-hash_left_thumb is initial and gw_reservation_detail-senha is initial.
            me->if_reservation_detail~register_digital( exceptions registration_failed = 1 ).
            check sy-subrc is initial.
          endif.

          try.
              if gw_reservation_detail-senha is initial.
                call method me->biometry_service->build_hash_validation
                  exporting
                    digital_left_thumb  = gw_reservation_detail-hash_left_thumb
                    digital_right_thumb = gw_reservation_detail-hash_right_thumb
                  receiving
                    result              = data(_hash_validation).
                "//Start reading digital
                message text-016 type 'I' display like 'I'.

                call method me->biometry_service->validate_digital
                  exporting
                    hash_validation_l = gw_reservation_detail-hash_left_thumb
                    hash_validation_r = gw_reservation_detail-hash_right_thumb
                  receiving
                    result            = data(_zmmt0081).
              else.
                message text-030 type 'I' display like 'I'.
                me->biometry_service->read_password(
                  exporting
                    registration = gw_reservation_detail-matricula
                  receiving
                    result       = data(_results) ).

                if _results-senha ne gw_reservation_detail-senha.
                  clear gw_reservation_detail-senha.
                  message text-031 type 'I' display like 'I'.
                  exit.
                endif.
              endif.

              clear gt_messages.

              "//Set header
              data(_movement_header) = value bapi2017_gm_head_01( pstng_date = sy-datum doc_date = sy-datum header_txt = |Biometria { gw_reservation_detail-bktxt } | ).

              loop at gw_reservation_detail-reservations into data(_reservation_detail).

                call function 'SAPGUI_PROGRESS_INDICATOR'
                  exporting
                    percentage = 50
                    text       = |Iniciando baixa(s) { sy-tabix } de { lines( gw_reservation_detail-reservations ) }...|.

*---> 16/06/2023 - Migração S4 - DG
                data(v_len) = strlen( _reservation_detail-material ).

                if v_len > 18.
                  data(lv_material_long) = _reservation_detail-material.
                else.
                  data(lv_material)      = _reservation_detail-material.
                endif.
*<--- 16/06/2023 - Migração S4 - DG

                if _reservation_detail-bwart = '311'.
                  append value #(
*---> 16/06/2023 - Migração S4 - DG
                   "material   = _reservation_detail-material
                   material        = lv_material
                   material_long   = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                   plant      = _reservation_detail-filial
                   stge_loc   = _reservation_detail-deposito
                   costcenter = _reservation_detail-centro_custo
                   batch      = _reservation_detail-charg
                   reserv_no  = _reservation_detail-reserva
                   res_item   = _reservation_detail-item
                   entry_qnt  = conv #( _reservation_detail-qtd_a_retirar )
                   move_type  = _reservation_detail-bwart
                   move_plant = _reservation_detail-umwrk
                   move_stloc = _reservation_detail-umlgo
               ) to movement_items.
                else.
                  append value #(
*---> 16/06/2023 - Migração S4 - DG
                      "material   = _reservation_detail-material
                      material        = lv_material
                      material_long   = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                      plant      = _reservation_detail-filial
                      stge_loc   = _reservation_detail-deposito
                      costcenter = _reservation_detail-centro_custo
                      batch      = _reservation_detail-charg
                      reserv_no  = _reservation_detail-reserva
                      res_item   = _reservation_detail-item
                      entry_qnt  = conv #( _reservation_detail-qtd_a_retirar )
                      move_type  = _reservation_detail-bwart
                      item_text  = _reservation_detail-sgtxt
                  ) to movement_items.
                endif.
              endloop.

              data _vcode(02).
              if _reservation_detail-bwart = '311'.
                _vcode = '06'.
              else.
                _vcode = '03'.
              endif.
              call method me->display_instance->material_movement_output
                exporting
                  header                = _movement_header
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
*                 items                 = movement_items
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
                  vcode                 = _vcode
                importing
                  returns               = data(_errors)
                  document              = data(_document)
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
                changing
                  items                 = movement_items
* ---> S4 Migration - 22/07/2023 - MG-5697 - JS
                exceptions
                  error_when_processing = 1
                  others                = 2.
              if sy-subrc <> 0.
                message text-014 type 'S' display like 'E'.

                call function 'FINB_BAPIRET2_DISPLAY'
                  exporting
                    it_message = _errors.
              else.
                message text-013 type 'S'.

                data(index) = 1.
                do lines( gw_reservation_detail-reservations ) times.
                  data(_reservation) = gw_reservation_detail-reservations[ index ].
                  "//Save log into table
                  data(_oper) = 'E'.
                  if _reservation-bwart = '202' or _reservation-bwart = '262'.
                    _oper = 'D'.
                  endif.
                  call method me->display_instance->save_log_baixa
                    exporting
                      reserva         = _reservation-reserva
                      item            = _reservation-item
                      material        = _reservation-material
                      deposito        = _reservation-deposito
                      operacao        = _oper "//Entrega
                      qtd_retirada    = conv #( _reservation-qtd_a_retirar )
                      hash_validation = _hash_validation
                      lado            = _zmmt0081-lado
                      polegar         = _zmmt0081-polegar
                      im_polegar      = _zmmt0081-im_polegar
                      senha           = gw_reservation_detail-senha
                      document        = _document
                      matricula       = gw_reservation_detail-matricula.

                  me->display_instance->alv_tree->get_parent(
                    exporting
                      i_node_key        = _reservation-node_key
                    importing
                      e_parent_node_key = data(_parent_node_key)
                  ).

                  if ( _reservation-quantidade = _reservation-qtd_a_retirar ).
                    me->display_instance->alv_tree->delete_subtree( i_node_key = _reservation-node_key ).
                  else.
                    _reservation-quantidade  = _reservation-quantidade - _reservation-qtd_a_retirar.
                    _reservation-matricula   = gw_reservation_detail-matricula.
                    _reservation-colaborador = gw_reservation_detail-nome.

                    me->display_instance->alv_tree->change_node(
                      i_node_key    = _reservation-node_key
                      i_outtab_line =  _reservation ).
                  endif.

                  add 1 to index.
                enddo.

                "//Delete higher tree
                if _reservation-node_key gt 0.
                  call method me->display_instance->alv_tree->get_parent
                    exporting
                      i_node_key        = _reservation-node_key
                    importing
                      e_parent_node_key = data(_parent_is_valid).

                  if _parent_is_valid is initial.
                    me->display_instance->alv_tree->delete_subtree( i_node_key = _parent_node_key ).
                  endif.
                endif.
                "//
                me->if_reservation_detail~set_detail_dynpro( '0010' ).
              endif.

            catch zcx_biometry into data(_cx).
              message _cx->get_text( ) type 'S' display like 'E'.
          endtry.
        endif.
        clear gw_reservation_detail-senha.

*     BUG 156677 - MMSILVA - 02.12.2024 - Inicio
      when 'BTN_COPY'.
        loop at gw_reservation_detail-reservations assigning field-symbol(<fs_copy>).
          if ( <fs_copy> is not assigned ).
            continue.
          endif.

          if <fs_copy>-deposito is not initial.
            loop at gw_reservation_detail-reservations assigning field-symbol(<fs_paste>).
              <fs_paste>-deposito = <fs_copy>-deposito.

              data: it_mchb type table of mchb,
                    wa_mchb type mchb,
                    it_mard type table of mard,
                    wa_mard type mard.

              select clabs
                from mchb
                where matnr = @<fs_paste>-material
                and   werks = @<fs_paste>-filial
                and   lgort = @<fs_paste>-deposito
                into corresponding fields of table @it_mchb
                up to 1 rows.


              if sy-subrc <> 0.
                select a~labst
                  from mard as a
                  where matnr = @<fs_paste>-material
                  and   werks = @<fs_paste>-filial
                  and   lgort = @<fs_paste>-deposito
                  order by lfgja descending
                  into corresponding fields of table @it_mard
                  up to 1 rows.
              endif.

              if it_mchb is not initial.
                loop at it_mchb into wa_mchb.
                  <fs_paste>-deposito = <fs_paste>-deposito.
                  <fs_paste>-labst = wa_mchb-clabs.
                endloop.
              elseif it_mard is not initial.
                if it_mchb is not initial.
                  continue.
                endif.
                loop at it_mard into wa_mard.
                  <fs_paste>-deposito = <fs_paste>-deposito.
                  <fs_paste>-labst = wa_mard-labst.
                endloop.
              elseif it_mchb is initial and it_mard is initial.
                <fs_paste>-deposito = <fs_paste>-deposito.
                <fs_paste>-labst = '0'.
              endif.
            endloop.
          endif.

          if <fs_copy>-deposito is not initial.
            exit.
          endif.
        endloop.
*     BUG 156677 - MMSILVA - 02.12.2024 - Fim

      when others.
    endcase.
  endmethod.

  method if_reservation_detail~get_user_data.

  endmethod.

  method if_reservation_detail~display_messages.
*    ME->IF_RESERVATION_DETAIL~SET_DETAIL_DYNPRO( '0004' ).

*    DATA(_MESSAGES) = ME->IF_RESERVATION_DETAIL~GET_RETURN_DETAILS( ).
    sort gt_messages by reserva item ascending.

    if ( me->custom_messages is not bound ).
      create object me->custom_messages
        exporting
          container_name = 'CUSTOM_MESSAGE_DETAILS'.
    endif.

    "//Get Field Catalog
    data(fieldcat) = me->if_reservation_detail~get_fieldcatalog( 'MESSAGE_DETAILS' ).

    if ( me->alv_grid is not bound ).

      "//Create Alv Object
      create object me->alv_grid
        exporting
          i_parent = me->custom_messages.

*      me->layout-sel_mode   = sy-abcde(1).
*      ME->LAYOUT-EDIT       = ABAP_TRUE.
*      ME->LAYOUT-STYLEFNAME = 'STYLE'.
*      me->layout-no_vgridln = abap_true.
*
      set handler me->handle_set_toolbar  for me->alv_grid.
      set handler me->handle_user_command for me->alv_grid.
*      ME->HANDLE_DATA_CHANGED      FOR ME->ALV_TABLE,
*      ME->HANDLE_F4_POPUP          FOR ME->ALV_TABLE.

      "//Display Datas
      call method me->alv_grid->set_table_for_first_display
        exporting
          is_layout       = value #( grid_title = 'Detalhes do processamento' )
          i_save          = abap_on
        changing
          it_outtab       = gt_messages
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      call method me->alv_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      call method me->alv_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    else.
      me->alv_grid->refresh_table_display( ).
*      ME->ALV_GRID->SET_FRONTEND_FIELDCATALOG( FIELDCAT ).
*      ME->REFRESH_SCREEN( ).
    endif.
  endmethod.
endclass.

class cl_display definition.
  public section.
    methods constructor.

    methods set_top_nodes_key
      importing node_key type lvc_nkey.

    methods get_top_nodes_key
      returning value(table) type lvc_t_nkey.

    interfaces if_display.

    aliases biometry_service for if_display~biometry_service.
    aliases alv_tree         for if_display~alv_tree.
    aliases image_helper     for if_display~image_helper.

    data detail_instance     type ref to if_reservation_detail.
    data reservations_header type table of rkpf.
    data reservation_items   type table of resb.
    data solicitations       type table of zmmt0081.
    data autorizados         type table of zmmt0096.
    data res_baixadas        type table of zmmt0087.
    data docking             type ref to cl_gui_docking_container.
    data toolbar             type ref to cl_gui_toolbar.
    data dynpro_display      type sy-dynnr value '0010'.
    data outtab_line         type ty_item.
    data top_nodes           type lvc_t_nkey.

    data reserva_baixada type zmmt0087.
    data reserva_item    type resb.
    data reserva_epi     type zmmt0081.
    data autorizados_pm  type zmmt0096.
    data reserva_header  type rkpf.
endclass.

class cl_display implementation.
  method constructor.
    create object biometry_service.
    create object image_helper.

    me->detail_instance = new cl_detail( me ).
  endmethod.

  method set_top_nodes_key.
    append node_key to me->top_nodes.
  endmethod.

  method get_top_nodes_key.
    move me->top_nodes to table.
  endmethod.

  method if_display~process_before_output.
    subscreen_display = me->if_display~get_display_dynnr( ).

    me->if_display~set_status( ).
    me->if_display~set_title( ).
    me->if_display~display( ).
  endmethod.

  method if_display~user_command.
    case ucomm.
      when 'BACK'.
        leave to screen 0.
      when 'EXIT' or 'CANCEL'.
        leave program.
      when 'REFRESH'.
        clear gt_reservations.
        clear gt_messages.

        me->if_display~select_reservations(
          exceptions
            data_not_found = 1
            others         = 2
        ).

*        IF SY-SUBRC IS INITIAL.
*          CALL SCREEN 0001.
        if not sy-subrc is initial.
          message text-009 type 'S' display like 'E'.
          leave to screen 0.
        endif.

        me->alv_tree->free( ).
        clear me->alv_tree.

        me->detail_instance->set_detail_dynpro( '0010' ).
*        LEAVE TO SCREEN 0001.
      when others.
    endcase.
  endmethod.

  method if_display~display.
    if ( docking is not bound ).
      create object me->docking
        exporting
          side      = cl_gui_docking_container=>dock_at_top
          extension = 5000
          repid     = sy-repid
          dynnr     = '0001'.
    endif.

    if me->alv_tree is initial.

      create object me->alv_tree
        exporting
          parent              = me->docking
          node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
          item_selection      = abap_true
          no_html_header      = abap_true
          no_toolbar          = abap_false.

      "// Get Field Catalog

      "ZCL_UTIL=>GET_STRUCTURE_DESCRIPTION( 'ZPPT0008' ).

      data(fieldcat) = me->if_display~get_fieldcatalog( ).
      data(header)   = me->if_display~get_tree_header( ).

      "// Set Tree Display
      call method me->alv_tree->set_table_for_first_display
        exporting
          is_hierarchy_header = header
          i_default           = 'X'
*         I_SAVE              = 'A'
        changing
          it_outtab           = gt_reservations
          it_fieldcatalog     = fieldcat.

      "// Set Ov Hierarchy
      me->if_display~set_hierarchy_reservations(
        changing alv_tree = me->alv_tree ).

      me->if_display~add_toolbar( ).
      me->alv_tree->expand_nodes( me->get_top_nodes_key( ) ).

      "// Set Registered Events
      me->if_display~set_register_events( ).

      call method me->alv_tree->get_first_child(
        exporting
          i_node_key       = cl_alv_tree_base=>c_virtual_root_node
        importing
          e_child_node_key = data(_child_node_key)
      ).

      me->alv_tree->set_top_node( _child_node_key ).

*    ELSE.
*      CL_GUI_CFW=>FLUSH( ).
    endif.

    cl_gui_cfw=>flush( ).
    me->alv_tree->frontend_update( ).
  endmethod.

  method if_display~set_title.
    data(_text) = switch #( let x = |Lista de Reserva de Materiais - | in p_open when abap_true then x && ' Em aberto' else x && ' Finalizadas' ).
    set titlebar 'TITLE_MAIN' with _text.
  endmethod.

  method if_display~set_status.
    set pf-status 'STATUS_MAIN'.
  endmethod.

  method if_display~set_display_dynnr.
    me->dynpro_display = i_dynpro.
  endmethod.

  method if_display~set_dock_extension.
    me->docking->set_extension( extension ).
  endmethod.

  method if_display~print_epi_list.
    data form_name type rs38l_fnam.

    data(_options_ordem) =
      value ssfcompop( tddest    = 'LOCL'
                       tdnoprint = abap_false
                       tdimmed   = abap_true
                       tdnewid   = abap_true
                       tdnoarch  = abap_true ).

    data(_control_ordem) =
      value ssfctrlop( device    = 'PRINTER'
                       preview   = abap_true
                       no_dialog = abap_false ).

    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname = 'ZMMSF_FICHA_CONTROLE_EPI'
      importing
        fm_name  = form_name.

    call function form_name
      exporting
        control_parameters = _control_ordem
        output_options     = _options_ordem
        user_settings      = abap_true
        matricula          = matricula
      tables
        items_epi          = items
      exceptions
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        others             = 5.
  endmethod.

  method if_display~select_reservations.
    if p_open = abap_true.
      select a~*
        from resb as a
       inner join rkpf as  b on b~rsnum = a~rsnum
       into corresponding fields of table @me->reservation_items
       where a~rsnum in @p_rsnum
         and a~werks in @p_werks
         and a~matnr in @p_matnr
         and a~xwaok eq @abap_true
         and a~kzear eq @abap_false
         and a~xloek eq @abap_false
         and b~kostl in @p_kostl
         and a~bdter in @p_date. "Ajuste IR081258 - Anderson Oenning 03.01.2022
*         AND b~rsdat IN @p_date.


      if not sy-subrc is initial.
        raise data_not_found.
      endif.


    else.
      select a~*
        from zmmt0087 as a
        inner join resb as b on b~rsnum = a~rsnum and a~rspos = b~rspos
        inner join rkpf as c on c~rsnum = a~rsnum
        inner join mseg as d on d~rsnum = a~rsnum and a~rspos = b~rspos "Ajuste IR081258 - Anderson Oenning 03.01.2022
        into corresponding fields of table @me->res_baixadas
       where a~rsnum in @p_rsnum
         and b~werks in @p_werks
         and b~matnr in @p_matnr
         and b~xwaok eq @abap_true
*         AND B~XLOEK EQ ABAP_FALSE
         and c~kostl in @p_kostl
         and d~budat_mkpf in @p_date. "Ajuste IR081258 - Anderson Oenning 03.01.2022
*         AND c~rsdat IN p_date.

      if me->res_baixadas is not initial.
        sort me->res_baixadas by rsnum rspos.
        delete adjacent duplicates from me->res_baixadas comparing rsnum rspos.
      endif.

      if me->res_baixadas[] is not initial.
        select *
          from resb
          into table me->reservation_items
           for all entries in me->res_baixadas
         where rsnum = me->res_baixadas-rsnum
           and rspos = me->res_baixadas-rspos.
      endif.

      if me->reservation_items is initial.
        raise data_not_found.
      endif.

    endif.

    check sy-subrc is initial.

    if me->reservation_items is not initial.
      sort me->reservation_items by rsnum rspos.
      delete adjacent duplicates from me->reservation_items comparing rsnum rspos.
    endif.

    select *
      from rkpf
      into table me->reservations_header
   for all entries in me->reservation_items
     where rsnum = me->reservation_items-rsnum.

    select *
      from zmmt0081
      into table me->solicitations
   for all entries in me->reservation_items
     where rsnum = me->reservation_items-rsnum.

    select *
      from zmmt0096
      into table me->autorizados
   for all entries in me->reservation_items
     where rsnum = me->reservation_items-rsnum
     and   mblnr = ''.
  endmethod.

  method if_display~get_reservation_items.
    move me->reservation_items to table.
  endmethod.

  method if_display~get_autorizados.
    move me->autorizados to table.
  endmethod.


  method if_display~get_reservations_header.
    move me->reservations_header to table.
  endmethod.

  method if_display~get_res_solicitations.
    move me->solicitations to table.
  endmethod.

  method if_display~get_res_baixadas.
    move me->res_baixadas to table.
  endmethod.

  method if_display~baixar_material.
*    CALL METHOD ME->BIOMETRY_SERVICE->BUILD_HASH_VALIDATION
*      EXPORTING
*        DIGITAL_LEFT_THUMB  = HASH_LEFT_THUMB
*        DIGITAL_RIGHT_THUMB = HASH_RIGHT_THUMB
*      RECEIVING
*        RESULT              = DATA(_HASH_VALIDATION).

*    IF ME->BIOMETRY_SERVICE->VALIDATE_DIGITAL( HASH_VALIDATION = _HASH_VALIDATION ) = ABAP_TRUE.
*      ME->IF_DISPLAY~MATERIAL_MOVEMENT_OUTPUT(
*        EXPORTING
*          DATA      = SY-DATUM
*          RESERVA   = RESERVA
*          DEPOSITO  = DEPOSITO
*          ITEM      = ITEM
*          MATERIAL  = MATERIAL
*          CENTRO    = FILIAL
*          CCUSTO    = CCUSTO
*          LOTE      = ''
*          QTD       = QUANTIDADE
*          MOVIMENTO = '201'
*        IMPORTING
*          DOCUMENT  = DATA(_DOCUMENT)
*      ).
*    ELSE.
*      MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
  endmethod.

  method if_display~save_log_baixa.
    call method me->biometry_service->get_system_data(
      importing
        username      = data(_username)
        computer_name = data(_computer_name)
        ip_adress     = data(_ip_adress) ).

    data: leitura type ref to zcl_nbiobsp.

    data(_log) =
        value zmmt0087( rsnum   = reserva
                        rspos     = item
                        mblnr  = document
                        pernr = matricula
                        matnr = material
                        operacao = operacao
                        lgort = deposito
                        hash_validation = hash_validation
                        lado_log  = lado
                        polegar_log = polegar
                        im_polegar_log = im_polegar
                        senha = senha
                        quant = qtd_retirada
                        data      = sy-datum
                        hora      = sy-uzeit
                        operador  = sy-uname
                        usuario_logado = _username
                        nome_computador = _computer_name
                        ip = _ip_adress
                      ).

    modify zmmt0087 from _log.
    if matricula is not initial.
      update zmmt0081 set lado = lado
                          polegar = polegar
                          im_polegar = im_polegar
      where rsnum = reserva.
      "
      update zmmt0096 set mblnr  = document
      where rsnum = reserva
      and   rspos = item
      and   pernr = matricula
      and   mblnr = ''.
      commit work.
      "
    endif.
    commit work.
  endmethod.

  method if_display~material_movement_output.


    call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      exporting
        goodsmvt_header  = header
        goodsmvt_code    = vcode
      importing
        materialdocument = document
*       MATDOCUMENTYEAR  = DOCUMENT-DOC_YEAR
      tables
        goodsmvt_item    = items
        return           = returns.

    delete returns where type <> 'E'.

    if returns is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = 'X'.
    else.
      raise error_when_processing.
*      ME->SET_MESSAGES( RETURN ).
*      RAISE EXCEPTION TYPE ZCX_LES_BILLING.
    endif.
  endmethod.

  method if_display~get_display_dynnr.
    move me->dynpro_display to r_dynpro.
  endmethod.

  method if_display~set_hierarchy_reservations.
    data outtab_lines type ty_item.
    data layout_node  type lvc_s_layn.
    data item_key     type lvc_nkey.
* ---> S4 Migration - 19/06/2023 - MA
*    DATA locali(50).
    data locali(60).
* <--- S4 Migration - 19/06/2023 - MA
    data docmat(50).
*    DATA W_AUTORIZADOS TYPE ZMMT0096.

    data(_reservations_header) = me->if_display~get_reservations_header( ).
    data(_reservas_epi)        = me->if_display~get_res_solicitations( ).
    data(_reservas_baixadas)   = me->if_display~get_res_baixadas( ).
    data(_reservations)        = me->if_display~get_reservation_items( ).
    data(_autorizados)         = me->if_display~get_autorizados( ).

    sort _reservations by rsnum rspos.
    sort _autorizados  by rsnum rspos ascending data hora descending.

    if p_open = abap_true.

      loop at _reservations into reserva_item.
        try.
            reserva_header = _reservations_header[ rsnum = reserva_item-rsnum ].
            reserva_epi    = _reservas_epi[ rsnum = reserva_item-rsnum   ].
          catch cx_sy_itab_line_not_found.
        endtry.

        " Reserva MB21 (standard)
        if reserva_item-wempf+0(1) = '7'.
          reserva_epi-pernr = reserva_item-wempf.
        else.
          read table _autorizados into autorizados_pm with key rsnum = reserva_item-rsnum
                                                               rspos = reserva_item-rspos binary search.
          if sy-subrc = 0.
            reserva_epi-pernr = autorizados_pm-pernr.
          endif.
        endif.

        locali =  |{ reserva_item-matnr } {  reserva_item-werks } {  reserva_item-lgort } |.
*       BUG 156677 - MMSILVA - 02.12.2024 - Inicio
        data locali_enmng(60).
        locali_enmng = |{ reserva_item-matnr } {  reserva_item-werks } {  reserva_item-rsnum } |.

        data locali_sum_banfn(60).
        locali_sum_banfn = |{ reserva_item-matnr } { reserva_item-werks } |.

        data locali_sum_ebeln(60).
        locali_sum_ebeln = |{ reserva_item-matnr } { reserva_item-werks } |.
*       BUG 156677 - MMSILVA - 02.12.2024 - Fim

        outtab_lines =
          value ty_item(
              reserva          = reserva_item-rsnum
              item             = reserva_item-rspos
              filial           = reserva_item-werks
              material         = reserva_item-matnr
              material_text    = me->if_display~get_material_description( reserva_item-matnr )
              filial_text      = me->if_display~get_description_filial( reserva_item-werks )
              loc_material     = me->if_display~get_material_location( locali  )
              labst            = me->if_display~get_labst_item( locali ) "BUG 156677 - MMSILVA - 02.12.2024
              enmng            = me->if_display~get_enmng_item( locali_enmng ) "BUG 156677 - MMSILVA - 02.12.2024
              sum_banfn        = me->if_display~get_sum_banfn_item( locali_sum_banfn ) "BUG 156677 - MMSILVA - 02.12.2024
              sum_ebeln        = me->if_display~get_sum_ebeln_item( locali_sum_ebeln ) "BUG 156677 - MMSILVA - 02.12.2024
              centro_custo     = reserva_header-kostl
              deposito         = reserva_item-lgort
              quantidade       = reserva_item-bdmng - reserva_item-enmng
              recebedor        = reserva_item-wempf
              data_neces       = reserva_item-bdter
              matricula        = reserva_epi-pernr
              bwart            = reserva_item-bwart
              umwrk            = reserva_item-umwrk
              umlgo            = reserva_item-umlgo
              nome_terceiro    = reserva_epi-nome_terceiro
              nome_terc_emp    = reserva_epi-nome_terc_emp
              colaborador      = me->if_display~get_name_by_matricula( reserva_epi-pernr )
              data_lcto        = reserva_header-rsdat
              aufnr            = reserva_item-aufnr
*              HASH_LEFT_THUMB  = ME->BIOMETRY_SERVICE->READ_DIGITAL( REGISTRATION = CONV #( RESERVA_EPI-PERNR  ) THUMB = ZCL_BIOMETRY=>THUMB-LEFT_THUMB  )
*              HASH_RIGHT_THUMB = ME->BIOMETRY_SERVICE->READ_DIGITAL( REGISTRATION = CONV #( RESERVA_EPI-PERNR  ) THUMB = ZCL_BIOMETRY=>THUMB-RIGHT_THUMB )
             ).

        "// Sets higher tree
        at new rsnum.
*        APPEND SY-TABIX TO NODES.

          layout_node =
            value lvc_s_layn( isfolder  = abap_false
                              n_image   = icon_order
                              exp_image = icon_order
                              style     = cl_gui_column_tree=>style_emphasized
                            ).

          call method alv_tree->add_node
            exporting
              i_relat_node_key = space
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = conv #( reserva_item-rsnum )
              is_node_layout   = layout_node
            importing
              e_new_node_key   = item_key.

          me->set_top_nodes_key( item_key ).
        endat.

        data node_layout type lvc_s_layn.
        data item_layout type lvc_t_layi.

*      NODE_LAYOUT-STYLE = CL_GUI_COLUMN_TREE=>ITEM_CLASS_CHECKBOX.

*      IF P_OPEN = ABAP_TRUE.
        item_layout = value #( ( fieldname = 'MATERIAL'     class = cl_gui_column_tree=>item_class_checkbox editable = abap_true  )
*                             ( FIELDNAME = 'MATRICULA' STYLE = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_POSITIVE )
                             ).
*      ENDIF.
*      ITEM_LAYOUT = VALUE #( ( FIELDNAME = 'MATRICULA' EDITABLE = ABAP_TRUE  ) ).

        "// Sets lower tree
        call method alv_tree->add_node
          exporting
            i_relat_node_key = item_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = conv #( reserva_item-rspos )
*           IS_NODE_LAYOUT   = NODE_LAYOUT
            is_outtab_line   = outtab_lines
            it_item_layout   = item_layout.
*        IMPORTING
*          E_NEW_NODE_KEY   = DATA(LAST_KEY).

        clear reserva_epi.
      endloop.

    else.

      sort _reservas_baixadas by rsnum rspos.
      loop at _reservas_baixadas into reserva_baixada.

        try.
            reserva_header  = _reservations_header[ rsnum = reserva_baixada-rsnum ].
          catch cx_sy_itab_line_not_found.
        endtry.

        try.
            reserva_item    = _reservations[ rsnum = reserva_baixada-rsnum rspos = reserva_baixada-rspos ].
          catch cx_sy_itab_line_not_found.
        endtry.

        try.
            reserva_epi     = _reservas_epi[ rsnum = reserva_baixada-rsnum ].
          catch cx_sy_itab_line_not_found.
        endtry.
        locali =  |{ reserva_item-matnr } {  reserva_item-werks } {  reserva_item-lgort } |.
        docmat =  |{ reserva_baixada-mblnr } {  reserva_item-rsnum } {  reserva_item-rspos } |.

        read table _reservations into data(w_reservations) with key rsnum = reserva_baixada-rsnum
                                                          rspos = reserva_baixada-rspos binary search.

        outtab_lines =
          value ty_item(
              reserva          = reserva_item-rsnum
              item             = reserva_item-rspos
              filial           = reserva_item-werks
              material         = reserva_item-matnr
              deposito         = reserva_baixada-lgort
              material_text    = me->if_display~get_material_description( reserva_item-matnr )
              filial_text      = me->if_display~get_description_filial( reserva_item-werks )
              loc_material     = me->if_display~get_material_location( locali )
              labst            = me->if_display~get_labst_item( locali ) "BUG 156677 - MMSILVA - 02.12.2024
              enmng            = me->if_display~get_enmng_item( locali_enmng ) "BUG 156677 - MMSILVA - 02.12.2024
              sum_banfn        = me->if_display~get_sum_banfn_item( locali_sum_banfn ) "BUG 156677 - MMSILVA - 02.12.2024
              sum_ebeln        = me->if_display~get_sum_ebeln_item( locali_sum_ebeln ) "BUG 156677 - MMSILVA - 02.12.2024
              centro_custo     = reserva_header-kostl
              quantidade       = reserva_baixada-quant
              recebedor        = reserva_item-wempf
              data_neces       = reserva_item-bdter
              matricula        = reserva_baixada-pernr
              bwart            = reserva_item-bwart
              entregador       = reserva_baixada-operador
              data_entrega     = reserva_baixada-data
              documento        = reserva_baixada-mblnr
              bktxt            = me->if_display~get_descr_item( docmat )
              nome_terceiro    = reserva_epi-nome_terceiro
              nome_terc_emp    = reserva_epi-nome_terc_emp
              colaborador      = me->if_display~get_name_by_matricula( reserva_baixada-pernr )
              data_lcto        = reserva_baixada-data
              aufnr            = w_reservations-aufnr
*              HASH_LEFT_THUMB  = ME->BIOMETRY_SERVICE->READ_DIGITAL( REGISTRATION = CONV #( RESERVA_BAIXADA-MATRICULA ) THUMB = ZCL_BIOMETRY=>THUMB-LEFT_THUMB  )
*              HASH_RIGHT_THUMB = ME->BIOMETRY_SERVICE->READ_DIGITAL( REGISTRATION = CONV #( RESERVA_BAIXADA-MATRICULA ) THUMB = ZCL_BIOMETRY=>THUMB-RIGHT_THUMB )
             ).

        "// Sets higher tree
        at new rsnum.
*        APPEND SY-TABIX TO NODES.

          layout_node =
            value lvc_s_layn( isfolder  = abap_false
                              n_image   = icon_order
                              exp_image = icon_order
                              style     = cl_gui_column_tree=>style_emphasized
                            ).

          call method alv_tree->add_node
            exporting
              i_relat_node_key = space
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = conv #( reserva_item-rsnum )
              is_node_layout   = layout_node
            importing
              e_new_node_key   = item_key.

          me->set_top_nodes_key( item_key ).
        endat.

        item_layout = value #( ( fieldname = 'MATERIAL' class = cl_gui_column_tree=>item_class_checkbox editable = abap_true  ) ).

        data count type i.

        loop at _reservas_baixadas transporting no fields
            where rsnum = reserva_item-rsnum
              and rspos    = reserva_item-rspos.

          count = count + 1.
        endloop.

        if count > 1.
          at new rspos.
            "// Sets lower tree
            call method alv_tree->add_node
              exporting
                i_relat_node_key = item_key
                i_relationship   = cl_gui_column_tree=>relat_last_child
                i_node_text      = conv #( reserva_item-rspos )
*               IS_NODE_LAYOUT   = NODE_LAYOUT
*               IS_OUTTAB_LINE   = OUTTAB_LINES
*               IT_ITEM_LAYOUT   = ITEM_LAYOUT
              importing
                e_new_node_key   = data(last_key).

            me->set_top_nodes_key( last_key ).
          endat.

          call method alv_tree->add_node
            exporting
              i_relat_node_key = last_key
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = conv #( reserva_item-rspos )
*             IS_NODE_LAYOUT   = NODE_LAYOUT
              is_outtab_line   = outtab_lines
              it_item_layout   = item_layout.
*            IMPORTING
*              E_NEW_NODE_KEY   = DATA(LAST_KEY).
        else.

          call method alv_tree->add_node
            exporting
              i_relat_node_key = item_key
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = conv #( reserva_item-rspos )
*             IS_NODE_LAYOUT   = NODE_LAYOUT
              is_outtab_line   = outtab_lines
              it_item_layout   = item_layout.
*            IMPORTING
*              E_NEW_NODE_KEY   = DATA(LAST_KEY).

        endif.

        clear: count, reserva_epi.
      endloop.
    endif.
  endmethod.

  method if_display~get_tree_header.
    r_table = value treev_hhdr( heading = 'Reserva/Item'
                                tooltip = 'Reserva/Item'
                                width   = 28 ).
  endmethod.

  method if_display~get_fieldcatalog.
*    CALL METHOD ME->IF_DISPLAY~BUILD_FIELDCATALOG
*      EXPORTING
*        FIELDNAME = 'CHECK'
*        CHECKBOX  = ABAP_TRUE
*        EDIT      = ABAP_TRUE
*        OUTPUTLEN = 5
*      CHANGING
*        R_TABLE   = R_TABLE.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'MATERIAL'
        description = 'Material'
        outputlen   = 15
        optimize    = abap_true
        no_zero     = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'MATERIAL_TEXT'
        description = 'Descrição'
        outputlen   = 30
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'FILIAL'
        description = 'Filial'
        optimize    = abap_true
        outputlen   = 7
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'FILIAL_TEXT'
        description = 'Desc.Filial'
        outputlen   = 20
        no_out      = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'CENTRO_CUSTO'
        description = 'Centro de Custo'
        outputlen   = 20
        optimize    = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'DEPOSITO'
        description = 'Depósito'
        outputlen   = 18
        optimize    = abap_true
      changing
        r_table     = r_table.

    "BUG 156677 - MMSILVA - 02.12.2024 - Inicio
    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'LABST'
        description = 'Saldo em Estoque'
        outputlen   = 18
        optimize    = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'SUM_BANFN'
        description = 'Quantidade em Requisição'
        outputlen   = 18
        optimize    = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'SUM_EBELN'
        description = 'Quantidade em Pedido'
        outputlen   = 18
        optimize    = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'ENMNG'
        description = 'Quantidade Total Reservado'
        outputlen   = 18
        optimize    = abap_true
      changing
        r_table     = r_table.
    "BUG 156677 - MMSILVA - 02.12.2024 - Fim

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'BWART'
        description = 'Mvt'
        outputlen   = 5
        optimize    = abap_true
      changing
        r_table     = r_table.

    if p_open = abap_true.
      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'QUANTIDADE'
          description = 'Qtd. a retirar'
          outputlen   = 20
          optimize    = abap_true
          ref_table   = 'RESB'
          ref_field   = 'BDMNG'
        changing
          r_table     = r_table.
    else.
      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'QUANTIDADE'
          description = 'Qtd. Retirada'
          outputlen   = 20
          optimize    = abap_true
          ref_table   = 'RESB'
          ref_field   = 'BDMNG'
        changing
          r_table     = r_table.
    endif.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'RECEBEDOR'
        description = 'Recebedor'
        outputlen   = 10
        optimize    = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'DATA_NECES'
        description = 'Dt.necessidade'
        outputlen   = 23
        ref_field   = 'DATA'
        ref_table   = 'ZMMT0087'
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'LOC_MATERIAL'
        description = 'Endereçamento'
        outputlen   = 15
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'MATRICULA'
        description = 'Matricula'
        outputlen   = 15
        edit        = abap_true
        optimize    = abap_true
        no_zero     = abap_true
      changing
        r_table     = r_table.

    call method me->if_display~build_fieldcatalog
      exporting
        fieldname   = 'COLABORADOR'
        description = 'Responsável pela retirada'
        outputlen   = 25
      changing
        r_table     = r_table.


    if p_clos = abap_true.


      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'DATA_LCTO'
          description = 'Dt. lcto.'
          outputlen   = 15
          ref_field   = 'DATA'
          ref_table   = 'ZMMT0087'
        changing
          r_table     = r_table.

      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'ENTREGADOR'
          description = 'Entregador'
          outputlen   = 15
        changing
          r_table     = r_table.

      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'DOCUMENTO'
          description = 'Documento'
          outputlen   = 15
        changing
          r_table     = r_table.

      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'BKTXT'
          description = 'Texto'
          outputlen   = 20
        changing
          r_table     = r_table.
    else.
      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'NOME_TERCEIRO'
          description = 'Empresa Terceiro'
          outputlen   = 20
        changing
          r_table     = r_table.
      call method me->if_display~build_fieldcatalog
        exporting
          fieldname   = 'NOME_TERC_EMP'
          description = 'Retirada EPI por Terceiro'
          outputlen   = 20
        changing
          r_table     = r_table.
    endif.
  endmethod.

  method if_display~add_toolbar.
    call method me->alv_tree->get_toolbar_object
      importing
        er_toolbar = me->toolbar.

    check not me->toolbar is initial.

    "//Modify toolbar with methods of CL_GUI_TOOLBAR:
    "//add seperator to toolbar
    call method me->toolbar->add_button
      exporting
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep.

    call method me->toolbar->add_button
      exporting
        fcode     = 'REPORT_EXCEL'
        icon      = icon_print
        butn_type = cntb_btype_button
        text      = 'Excel'
        quickinfo = 'Excel'.

    "//Add Standard Button to toolbar
    if p_open = abap_true.

*      CALL METHOD ME->TOOLBAR->ADD_BUTTON
*        EXPORTING
*          FCODE     = 'UNSELECT_ALL'
*          ICON      = ICON_DESELECT_ALL
*          BUTN_TYPE = CNTB_BTYPE_BUTTON
*          TEXT      = TEXT-025
*          QUICKINFO = TEXT-025.
*
*      CALL METHOD ME->TOOLBAR->ADD_BUTTON
*        EXPORTING
*          FCODE     = 'SELECT_ALL'
*          ICON      = ICON_SELECT_ALL
*          BUTN_TYPE = CNTB_BTYPE_BUTTON
*          TEXT      = TEXT-026
*          QUICKINFO = TEXT-026.
*
*
*      "//add seperator to toolbar
*      CALL METHOD ME->TOOLBAR->ADD_BUTTON
*        EXPORTING
*          FCODE     = ''
*          ICON      = ''
*          BUTN_TYPE = CNTB_BTYPE_SEP.


      call method me->toolbar->add_button
        exporting
          fcode     = 'RETIRAR_MATERIAL'
          icon      = icon_outbox
          butn_type = cntb_btype_button
          text      = text-001
          quickinfo = text-001.

      "//add seperator to toolbar
      call method me->toolbar->add_button
        exporting
          fcode     = ''
          icon      = ''
          butn_type = cntb_btype_sep.

      call method me->toolbar->add_button
        exporting
          fcode     = 'REPORT_MATERIAL_OUTPUT'
          icon      = icon_report
          butn_type = cntb_btype_button
          text      = text-010
          quickinfo = text-010.

      call method me->toolbar->add_button
        exporting
          fcode     = 'REPORT_RESERVA'
          icon      = icon_report
          butn_type = cntb_btype_button
          text      = text-028
          quickinfo = text-028.

*       BUG 156677 - MMSILVA - 02.12.2024 - Inicio
*      call method me->toolbar->add_button
*        exporting
*          fcode     = 'REPORT_SELECT_ALL'
*          icon      = icon_select_all
*          butn_type = cntb_btype_button
*          text      = text-036
*          quickinfo = text-036.
*
*      call method me->toolbar->add_button
*        exporting
*          fcode     = 'REPORT_MARK_ALL'
*          icon      = icon_mass_change
*          butn_type = cntb_btype_button
*          text      = text-037
*          quickinfo = text-037.
*       BUG 156677 - MMSILVA - 02.12.2024 - Fim


    else.
      authority-check object 'ZFICHA_EPI' id 'ACTVT' field '02'.

      if sy-subrc is initial.
        call method me->toolbar->add_button
          exporting
            fcode     = 'IMPRIMIR_FORMULARIO'
            icon      = icon_print
            butn_type = cntb_btype_button
            text      = text-024
            quickinfo = text-024.
      endif.
    endif.
  endmethod.

  method if_display~set_register_events.
    data lt_events type cntl_simple_events.

    lt_events = value #( ( eventid    = cl_gui_column_tree=>eventid_node_double_click  appl_event = abap_true  )
                         ( eventid    = cl_gui_column_tree=>eventid_expand_no_children appl_event = abap_true  )
                         ( eventid    = cl_gui_column_tree=>eventid_item_double_click  appl_event = abap_true  )
                         ( eventid    = cl_gui_column_tree=>eventid_checkbox_change )
                       ).

    call method me->alv_tree->set_registered_events
      exporting
        events = lt_events.

    set handler me->if_display~handle_double_click      for me->alv_tree.
    set handler me->if_display~handle_item_double_click for me->alv_tree.
    set handler me->if_display~handle_button_selected   for me->toolbar.

*    DATA:
*      L_EVENT   TYPE CNTL_SIMPLE_EVENT,
*      L_T_EVENT TYPE CNTL_SIMPLE_EVENTS.
*
*    CALL METHOD ALV_TREE->GET_REGISTERED_EVENTS
*      IMPORTING
*        EVENTS = L_T_EVENT.
*
*    L_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
*    APPEND L_EVENT TO L_T_EVENT.
*
*    CALL METHOD ALV_TREE->SET_REGISTERED_EVENTS
*      EXPORTING
*        EVENTS                    = L_T_EVENT
*      EXCEPTIONS
*        CNTL_ERROR                = 1
*        CNTL_SYSTEM_ERROR         = 2
*        ILLEGAL_EVENT_COMBINATION = 3.
*    IF SY-SUBRC <> 0.
*      MESSAGE X534(0K).
*    ENDIF.

    set handler me->if_display~handle_checkbox_change for me->alv_tree.
  endmethod.

  method if_display~get_material_description.
    select single maktx from makt into  result where matnr = input.
  endmethod.

  method if_display~get_material_location.
    select single lgpbe from mard into result where matnr = input+0(18)
                                              and   werks = input+19(4)
                                              and   lgort = input+24(4).
  endmethod.

* BUG 156677 - MMSILVA - 02.12.2024 - Inicio
  method if_display~get_labst_item.

    select single clabs
      from mchb
      into result
      where matnr = input+0(18)
      and   werks = input+19(4)
      and   lgort = input+24(4).

    if sy-subrc <> 0.
      select a~labst
        from mard as a
        where    matnr = @input+0(18)
        and      werks = @input+19(4)
        and      lgort = @input+24(4)
        order by lfgja descending
        into @result
        up to 1 rows.
      endselect.
    endif.

  endmethod.

  method if_display~get_enmng_item.

    select single enmng
      from resb
      into result
      where matnr = input+0(18)
      and   werks = input+19(4)
      and   rsnum = input+24(10).

  endmethod.

  method if_display~get_sum_banfn_item.

    select count( * )
      into @result
      from eban
      where matnr = @input+0(18)
      and   werks = @input+19(4)
      and   ebeln = ''.

  endmethod.

  method if_display~get_sum_ebeln_item.

    data lv_count type bdmng.

    select ebeln
      from  ekpo
      into table @data(it_ekpo)
      where matnr = @input+0(18)
      and   werks = @input+19(4).

    loop at it_ekpo into data(wa_ekpo).
      select ebeln
        from ekbe
        into table @data(it_ekbe)
        where ebeln = @wa_ekpo-ebeln.

      if sy-subrc ne 0.
        lv_count = lv_count + 1.
      endif.

    endloop.

    result = lv_count.

  endmethod.
* BUG 156677 - MMSILVA - 02.12.2024 - Fim

  method if_display~get_descr_item.
    select single bktxt from mkpf  into result where mblnr = input+0(10).
  endmethod.

  method if_display~get_description_conta_razao.
    select single txt50 from skat into value where spras = sy-langu and saknr = input.
  endmethod.

  method if_display~get_description_filial.
    select single name1 from t001w into value where werks = input.
  endmethod.

  method if_display~get_name_by_matricula.
    if input+0(1) ='7'.
      select single ename from pa0001 into value where pernr = input.
    else.
      select single nome from zmmt0101 into value where matricula = input.
    endif.

  endmethod.

  method if_display~get_description_centro_custo.
    select single ltext from cskt into value where kostl = input.
  endmethod.

  method if_display~build_fieldcatalog.
    data(fcat) = value lvc_s_fcat(
                       fieldname  = fieldname
                       scrtext_l  = description
                       scrtext_m  = description
                       scrtext_s  = description
                       reptext    = description
                       coltext    = description
                       checkbox   = checkbox
                       col_opt    = optimize
                       no_out     = no_out
                       hotspot    = hotspot
                       no_zero    = no_zero
                       outputlen  = outputlen
                       do_sum     = sum
                       quantity   = quantity
                       icon       = icon
                       edit       = edit
                       style      = style
                       ref_table  = ref_table
                       ref_field  = ref_field
                       tabname    = tabname
                       f4availabl = f4
                       just       = just
                                ).
    append fcat to r_table.
  endmethod.

  method if_display~handle_double_click.
    clear gw_reservation_detail.

*    CALL METHOD ME->ALV_TREE->GET_OUTTAB_LINE
*      EXPORTING
*        I_NODE_KEY    = NODE_KEY
*      IMPORTING
*        E_OUTTAB_LINE = ME->OUTTAB_LINE.
*
*    IF ( ME->OUTTAB_LINE IS NOT INITIAL ).
*      GW_RESERVATION_DETAIL-RESERVATION = ME->OUTTAB_LINE.
*
*      "//User data
*      IF NOT ME->OUTTAB_LINE-MATRICULA IS INITIAL.
*        GW_RESERVATION_DETAIL-IS_RES_EPI = ABAP_TRUE.
*        GW_RESERVATION_DETAIL-MATRICULA  = ME->OUTTAB_LINE-MATRICULA.
*        GW_RESERVATION_DETAIL-NOME       = ME->OUTTAB_LINE-COLABORADOR.
*
*        ME->BIOMETRY_SERVICE->READ_DIGITAL(
*          EXPORTING
*            REGISTRATION = CONV #( GW_RESERVATION_DETAIL-MATRICULA )
*            THUMB        = ZCL_BIOMETRY=>THUMB-LEFT_THUMB
*          RECEIVING
*            RESULT       = GW_RESERVATION_DETAIL-HASH_LEFT_THUMB
*        ).
*
*        ME->BIOMETRY_SERVICE->READ_DIGITAL(
*          EXPORTING
*            REGISTRATION = CONV #( GW_RESERVATION_DETAIL-MATRICULA )
*            THUMB        = ZCL_BIOMETRY=>THUMB-LEFT_THUMB
*          RECEIVING
*            RESULT       = GW_RESERVATION_DETAIL-HASH_RIGHT_THUMB
*        ).
*
*        SELECT SINGLE CPF_NR
*          FROM PA0465
*          INTO GW_RESERVATION_DETAIL-CPF
*         WHERE PERNR = GW_RESERVATION_DETAIL-MATRICULA.
*      ENDIF.
*
*      ME->DETAIL_INSTANCE->SET_FIELDS_GROUP1( DISABLE = ABAP_TRUE ).
*      ME->DETAIL_INSTANCE->DISPLAY( '0003' ).
*
*    ELSE.
*      ME->DETAIL_INSTANCE->DISPLAY( '0010' ).
*    ENDIF.
  endmethod.

  method if_display~handle_item_double_click.
    data outtab_item type ty_item.

    if fieldname = '&Hierarchy'.
      call method me->alv_tree->get_outtab_line
        exporting
          i_node_key    = node_key
        importing
          e_outtab_line = outtab_item.

      if outtab_item is initial.
        add 1 to node_key.

        call method me->alv_tree->get_outtab_line
          exporting
            i_node_key    = node_key
          importing
            e_outtab_line = outtab_item.
      endif.

      set parameter id 'RES' field outtab_item-reserva.
      call transaction 'MB23' and skip first screen.
    endif.
  endmethod.

  method if_display~handle_button_selected.
    data selected_nodes       type lvc_t_nkey.
    data reservation_number   type ty_item-reserva.
    data reservation_bwart    type ty_item-bwart.
    data reservation_register type ty_item-matricula.
    data matricula            type pa0001-pernr.
    data outtab_item          type ty_item.
    data outtab_items         type table of ty_item.
    data checked_items        type lvc_t_chit.
    data checked_item         type lvc_s_chit.

    case fcode.
      when 'REPORT_EXCEL'.
        data  r_table     type slis_t_fieldcat_alv.
        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'RESERVA'
            description = 'Reserva'
            outputlen   = 8
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'ITEM'
            description = 'Item'
            outputlen   = 5
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'FILIAL'
            description = 'Centro'
            outputlen   = 10
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'FILIAL_TEXT'
            description = 'Desc.Filial'
            outputlen   = 20
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'CENTRO_CUSTO'
            description = 'Centro de custo'
            outputlen   = 15
          changing
            r_table     = r_table. "

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'MATERIAL'
            description = 'Material'
            outputlen   = 12
            no_zero     = abap_true
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'MATERIAL_TEXT'
            description = 'Descrição'
            outputlen   = 25
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'QUANTIDADE'
            description = 'Qtd. reserva'
*           REF_TABLE   = 'RESB'
*           REF_FIELD   = 'BDMNG'
            outputlen   = 15
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'DEPOSITO'
            description = 'Depósito'
*           REF_TABLE   = 'MCHB'
*           REF_FIELD   = 'LGORT'
            edit        = abap_true
            outputlen   = 10
          changing
            r_table     = r_table.

*       BUG 156677 - MMSILVA - 02.12.2024 - Inicio
        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'LABST'
            description = 'Saldo em Estoque'
*           REF_TABLE   = 'MCHB'
*           REF_FIELD   = 'LGORT'
            edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.
*       BUG 156677 - MMSILVA - 02.12.2024 - Fim

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'CHARG'
            description = 'Lote'
*           REF_TABLE   = 'MCHB'
*           REF_FIELD   = 'LGORT'
            edit        = abap_true
            outputlen   = 10
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'QTD_A_RETIRAR'
            description = 'Qtd. a retirar'
            edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'MATRICULA'
            description = 'Matricula'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'RECEBEDOR'
            description = 'Recebedor'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'COLABORADOR'
            description = 'Responsável pela retirada'
*           edit        = abap_true
            outputlen   = 25
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'ENTREGADOR'
            description = 'Entregador'
*           edit        = abap_true
            outputlen   = 25
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'DATA_LCTO'
            description = 'Dt. lcto.'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'DATA_NECES'
            description = 'Dt.necessidade'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'DATA_ENTREGA'
            description = 'Data da entrega'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.

        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'AUFNR'
            description = 'Ordem'
*           edit        = abap_true
            outputlen   = 10
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'DOCUMENTO'
            description = 'Nº Documento'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'BKTXT'
            description = 'Texto'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.


        call method cl_util=>build_fieldcatalogu
          exporting
            fieldname   = 'BWART'
            description = 'Mvt'
*           edit        = abap_true
            outputlen   = 15
          changing
            r_table     = r_table.

        data(gt_reservationsu) = gt_reservations.
        delete gt_reservationsu where reserva is initial.
        call function 'REUSE_ALV_GRID_DISPLAY'
          exporting
            it_fieldcat   = r_table
          tables
            t_outtab      = gt_reservationsu
          exceptions
            program_error = 1
            others        = 2.

      when 'RETIRAR_MATERIAL'.
        alv_tree->get_checked_items( importing et_checked_items = checked_items ).

        clear gw_reservation_detail.

        if ( checked_items is not initial ).
          clear: reservation_bwart, reservation_register.
          loop at checked_items into checked_item.
            call method me->alv_tree->get_outtab_line
              exporting
                i_node_key    = checked_item-nodekey
              importing
                e_outtab_line = outtab_item.
            reservation_register = outtab_item-matricula.
            exit.
          endloop.
          "//Check if it's a unique reservation
          loop at checked_items into checked_item.
            call method me->alv_tree->get_outtab_line
              exporting
                i_node_key    = checked_item-nodekey
              importing
                e_outtab_line = outtab_item.

            if reservation_bwart is initial.
              reservation_bwart = outtab_item-bwart.
            endif.

            if ( reservation_bwart <> outtab_item-bwart ).
              message text-029 type 'I' display like 'W'.
              clear outtab_items.
              exit.
            endif.

            if reservation_register is initial.
              if ( reservation_register <> outtab_item-matricula ).
                message text-027 type 'I' display like 'W'.
                clear outtab_items.
                exit.
              endif.
            else.
              if reservation_number is initial.
                reservation_number = outtab_item-reserva.
              endif.

              if ( reservation_number <> outtab_item-reserva ).
                message text-006 type 'I' display like 'W'.
                clear outtab_items.
                exit.
              endif.
            endif.

            outtab_item-node_key      = checked_item-nodekey.
            outtab_item-qtd_a_retirar = outtab_item-quantidade.
            append outtab_item to outtab_items.
          endloop.

          check not outtab_items is initial.

          if line_exists( outtab_items[ matricula = space ] ).
            me->detail_instance->set_fields_group1( enable  = abap_true ).
          else.
            me->detail_instance->set_fields_group1( disable = abap_true ).
            data v_dt_desligamento type dats.
            if outtab_items[ 1 ]-matricula+0(1) = '7'.
              call function 'RP_GET_FIRE_DATE'
                exporting
                  persnr   = outtab_items[ 1 ]-matricula
                  status2  = '0'
                importing
                  firedate = v_dt_desligamento.
              if v_dt_desligamento is not initial.
                message text-034 type 'S' display like 'E'.
                exit.
              endif.
            endif.
            zcl_hrst_commons=>get_employee(
               exporting
                 matricula      = outtab_items[ 1 ]-matricula
               receiving
                 employee       = data(_employee)
               exceptions
                 data_not_found = 1
                 others         = 2
             ).

            if ( sy-subrc is initial ).
              gw_reservation_detail-matricula    = _employee-matricula.
              gw_reservation_detail-cargo        = _employee-cargo.
              gw_reservation_detail-unidade      = _employee-unidade.
              gw_reservation_detail-nome_unidade = _employee-nome_unidade.
              gw_reservation_detail-cpf          = _employee-cpf.
              gw_reservation_detail-nome         = _employee-nome.
              gw_reservation_detail-hash_foto    = _employee-foto_base64.

              try.
                  me->biometry_service->read_digital(
                    exporting
                      registration = gw_reservation_detail-matricula
                    receiving
                      result       = data(_result)
                  ).
                catch zcx_biometry.
              endtry.

              gw_reservation_detail-hash_left_thumb  = _result-polegar_esquerdo.
              gw_reservation_detail-hash_right_thumb = _result-polegar_direito.
              "
              select single *
               from zmmt0120
               into @data(_zmmt0120)
              where matricula  eq @_employee-matricula.
              "
              if sy-subrc = 0.
                me->detail_instance->set_fields_group9( enable  = abap_true ).
              else.
                me->detail_instance->set_fields_group9( disable = abap_true ).
              endif.

            endif.
          endif.

          gw_reservation_detail-reserva          = outtab_items[ 1 ]-reserva.
          gw_reservation_detail-reservations     = outtab_items.
          me->detail_instance->set_detail_dynpro( '0003' ).
        else.
          message text-002 type 'S' display like 'E'.
        endif.

      when 'SELECT_ALL'.

      when 'UNSELECT_ALL'.

      when 'IMPRIMIR_FORMULARIO'.
        data valuesnum  type table of bapi1003_alloc_values_num.
        data valueschar type table of bapi1003_alloc_values_char.
        data valuescurr type table of bapi1003_alloc_values_curr.
        data return     type table of  bapiret2.

        data fichas_epi type table of zmme_ficha_controle_epi.
        data items_epi  type rsis_t_keys.

        alv_tree->get_checked_items( importing et_checked_items = checked_items ).
        clear gw_reservation_detail.

        if ( checked_items is not initial ).
          "//Check if it's a unique reservation
          loop at checked_items into checked_item.
            call method me->alv_tree->get_outtab_line
              exporting
                i_node_key    = checked_item-nodekey
              importing
                e_outtab_line = outtab_item.

            if reservation_number is initial.
              reservation_number = outtab_item-reserva.
            endif.

            if matricula is initial.
              matricula = outtab_item-matricula.
            endif.

            if ( matricula <> outtab_item-matricula ).
              message text-023 type 'I' display like 'W'.
              clear outtab_items.
              exit.
            endif.

            outtab_item-node_key = checked_item-nodekey.
            append outtab_item to outtab_items.
          endloop.

          loop at outtab_items into outtab_item.
            "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
            call function 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131]
              exporting
                objectkey       = conv objnum( outtab_item-material )
                objecttable     = 'MARA'
                classnum        = 'MATEPI'
                classtype       = '023'
              tables
                allocvaluesnum  = valuesnum
                allocvalueschar = valueschar
                allocvaluescurr = valuescurr
                return          = return.

            try.
                data(_ca_number) = valueschar[ charact = 'ZEPI_CA' ]-value_char.
              catch cx_sy_itab_line_not_found.
            endtry.

            append value #( codigo_epi    = outtab_item-material
                            numero_ca     = _ca_number
                            descricao_epi = outtab_item-material_text
                            qtd_retirada  = outtab_item-quantidade
                            data_entrega  = outtab_item-data_entrega
                          ) to fichas_epi.

            clear _ca_number.
          endloop.

          check outtab_items is not initial.

          me->if_display~print_epi_list(
            exporting
              matricula = outtab_item-matricula
              items     = fichas_epi
          ).

        endif.

      when 'REPORT_RESERVA'.
        alv_tree->get_checked_items( importing et_checked_items = checked_items ).

        if lines( checked_items ) = 1.
          "//Check if it's a unique reservation
          loop at checked_items into checked_item.

            call method me->alv_tree->get_outtab_line
              exporting
                i_node_key    = checked_item-nodekey
              importing
                e_outtab_line = outtab_item.


            submit zpmr0017 and return
              with p_werks   eq outtab_item-filial
              with p_rsnum   eq outtab_item-reserva.
          endloop.

        elseif lines( checked_items ) > 1.
          message text-015 type 'S' display like 'E'.
        else.
          message text-002 type 'S' display like 'E'.
        endif.
      when 'REPORT_MATERIAL_OUTPUT'.
        alv_tree->get_checked_items( importing et_checked_items = checked_items ).

        if lines( checked_items ) = 1.
          "//Check if it's a unique reservation
          loop at checked_items into checked_item.

            call method me->alv_tree->get_outtab_line
              exporting
                i_node_key    = checked_item-nodekey
              importing
                e_outtab_line = outtab_item.

*            SET PARAMETER ID 'RES' FIELD OUTTAB_ITEM-RESERVA.
*            SET PARAMETER ID 'MBN' FIELD OUTTAB_ITEM-DOCUMENTO.
*
*            CALL TRANSACTION 'ZMM0022' AND SKIP FIRST SCREEN.

            data(_res) = value rsis_t_range( ( sign = 'I' option = 'EQ' low = outtab_item-reserva ) ).

            submit zmmr023 and return
              with n_rsv     in _res
              with p_mblnr   eq outtab_item-documento.
          endloop.

        elseif lines( checked_items ) > 1.
          message text-015 type 'S' display like 'E'.
        else.
          message text-002 type 'S' display like 'E'.
        endif.
    endcase.

    clear outtab_items.
  endmethod.

  method if_display~handle_checkbox_change.
*    DATA OUTTAB_ITEM   TYPE TY_ITEM.
*
*    CALL METHOD ALV_TREE->GET_CHECKED_ITEMS
*      IMPORTING
*        ET_CHECKED_ITEMS = DATA(_CHECKED_ITEMS).
*
*    CALL METHOD ME->ALV_TREE->GET_OUTTAB_LINE
*      EXPORTING
*        I_NODE_KEY     = NODE_KEY
*      IMPORTING
*        E_OUTTAB_LINE  = OUTTAB_ITEM
*        ET_ITEM_LAYOUT = DATA(_ITEM_LAYOUT).
*
*    IF OUTTAB_ITEM-MATRICULA IS INITIAL.
*      LOOP AT _CHECKED_ITEMS INTO DATA(_CHECKED_ITEM).
*        CALL METHOD ME->ALV_TREE->GET_OUTTAB_LINE
*          EXPORTING
*            I_NODE_KEY    = _CHECKED_ITEM-NODEKEY
*          IMPORTING
*            E_OUTTAB_LINE = OUTTAB_ITEM.
*
*        IF OUTTAB_ITEM-MATRICULA IS NOT INITIAL.
*          _ITEM_LAYOUT[ FIELDNAME = 'CHECK' ]-CHOSEN = ABAP_FALSE.
*          DATA(_NEW_LAYOUT) = CORRESPONDING LVC_T_LACI( _ITEM_LAYOUT ).
*
*          ALV_TREE->CHANGE_NODE(
*            EXPORTING
*              I_NODE_KEY     = _CHECKED_ITEM-NODEKEY
*              I_OUTTAB_LINE  = OUTTAB_ITEM
**              IS_NODE_LAYOUT =
*              IT_ITEM_LAYOUT = _NEW_LAYOUT
**              I_NODE_TEXT    =
**              I_U_NODE_TEXT  =
**            EXCEPTIONS
**              NODE_NOT_FOUND = 1
**              OTHERS         = 2
*          ).
*          IF SY-SUBRC <> 0.
**           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**                      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ENDIF.
*
*          MESSAGE 'Não é possível agrupar' TYPE 'S' DISPLAY LIKE 'E'.
*
*          ALV_TREE->UPDATE_CALCULATIONS(
**              NO_FRONTEND_UPDATE =
*          ).
*
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

  endmethod.
endclass.

data main_instance type ref to cl_display.

*AT SELECTION-SCREEN.

*AT SELECTION-SCREEN OUTPUT.
**  data icon type text.
*  WRITE ICON_RETRIEVE AS ICON TO ICON.
*  WRITE TEXT-022 TO TEXT.
*
*  IF P_OPEN = ABAP_TRUE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'TPM'.
*        SCREEN-INVISIBLE = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

start-of-selection.
  main_instance = new cl_display( ).

  clear: vg_dias.

  if p_date-high is not initial.
    vg_dias = p_date-high - p_date-low.
    if vg_dias > 120.
      message i024(sd) with 'Não é permitido realizar a consulta acima 120 dias'.
      exit.
    endif.
  else.
    message i024(sd) with 'Informar a data fim'.
    exit.
  endif.

  main_instance->if_display~select_reservations(
    exceptions
      data_not_found = 1
      others         = 2
  ).

  if sy-subrc is initial.
    call screen 0001.
  else.
    message text-009 type 'S' display like 'E'.
  endif.

end-of-selection.

module output_main output.
*  MAIN_INSTANCE = COND #( WHEN MAIN_INSTANCE IS INITIAL THEN NEW CL_DISPLAY( ) ELSE MAIN_INSTANCE ).
  main_instance->if_display~process_before_output( ).
endmodule.

module user_command_main input.
  main_instance->if_display~user_command( sy-ucomm ).
endmodule.

module output_detail output.
  main_instance->detail_instance->process_before_output( ).
endmodule.

module user_command_detail input.
  main_instance->detail_instance->user_command( sy-ucomm ).
endmodule.

module output_message_detail output.
  main_instance->detail_instance->display_messages( ).
endmodule.

module set_screen_fields output.
  main_instance->detail_instance->set_screen_fields( ).
endmodule.

"BUG 156677 - MMSILVA - 02.12.2024 - Inicio
*method busca_estoque_disp.
*  data: it_mard type table of mard,
*        wa_mard type mard,
*        it_mchb type table of mchb,
*        wa_mchb type mchb.
*endmethod.
"BUG 156677 - MMSILVA - 02.12.2024 - Fim
