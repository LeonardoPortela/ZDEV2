
*&---------------------------------------------------------------------*
*& Report  zmmr198
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmmr212.

*--------------------------------------------------------------------------------------------------*
* Developer: Antonio Rodrigues
* Data.....: 25.07.2025
*--------------------------------------------------------------------------------------------------*

type-pools icon.

types: begin of ty_saida,
         status       type icon-id,
         contract_id  type zmmt0177-contract_id,
         item_number  type zmmt0177-item_number,
         message(255) type c,
         check        type c,
       end of ty_saida.


data: obj_custom         type ref to cl_gui_custom_container,
      obj_custom_log     type ref to cl_gui_container,
      obj_splitter       type ref to cl_gui_splitter_container,
      ls_stable          type lvc_s_stbl,
      obj_alv            type ref to cl_gui_alv_grid,
      gt_planilha        like alsmex_tabline occurs 0 with header line,
      gt_planilha2       like alsmex_tabline occurs 0 with header line,
      gt_msg_return      type table of zfiwrs0002,
      gt_zibcontabil     type table of zib_contabil,
      gt_zibcontabil_err type table of zib_contabil_err,
      gt_fcat            type table of lvc_s_fcat,
      gt_saida           type table of ty_saida,

      wa_saida           type ty_saida,
      wl_planilha        like alsmex_tabline,
      wl_saida           type ty_saida,
      wl_layout          type lvc_s_layo,
      wl_mensagem        type char30,
      wl_stable          type lvc_s_stbl,
      wl_toolbar         type stb_button,
      p_file             type rlgrap-filename,
      p_sap(1).

data: lt_retorno       type table of zmme_supp_itens,
      code_retorno(10).

start-of-selection.

  call screen 0100.

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_toolbar definition.
  public section.
    class-methods:
      set_toolbar  for event toolbar of cl_gui_alv_grid
        importing e_object.

    class-methods:
      get_ucomm   for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_toolbar implementation.

  method set_toolbar.
    clear: wl_toolbar.
*
*    wl_toolbar-butn_type    = 1.
*    APPEND wl_toolbar TO e_object->mt_toolbar.
*    CLEAR wl_toolbar.
*
    if gt_saida is not initial.
      wl_toolbar-function     = 'BTN_PROC'.
      wl_toolbar-icon         = icon_oo_interface.
      wl_toolbar-butn_type    = 0.
      wl_toolbar-text         = 'Processar'.
      append wl_toolbar to e_object->mt_toolbar.
      clear wl_toolbar.
    endif.
  endmethod.                    "SET_TOOLBAR

  method get_ucomm.
    case e_ucomm.
      when 'BTN_ATUALIZAR'.
        data: at_index type sy-tabix.
        clear: gt_msg_return.

        if ( not gt_msg_return is initial ).
          perform show_msg.
        endif.

        message s836(sd) with text-s01 display like 'S'.

        call method obj_alv->refresh_table_display
          exporting
            is_stable = wl_stable.

      when 'BTN_PROC'.
        perform fm_proc_dados.

    endcase.
  endmethod.                    "GET_UCOMM
endclass.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  TRATAR_EXCEL
*&---------------------------------------------------------------------*
form tratar_arquivo.
  refresh: gt_planilha, gt_saida, gt_zibcontabil.
  data: ws_ekpo type ekpo.
  data: vmatnr         type matnr18.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = text-i01.

  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 2
      i_end_row               = 99999
    tables
      intern                  = gt_planilha
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  loop at gt_planilha into wl_planilha.
    at new row.
      clear wl_saida.
    endat.

    if wl_planilha-value(1) = space.
      shift wl_planilha-value left deleting leading space.
    endif.

    case wl_planilha-col.
      when 1.
        wa_saida-contract_id = wl_planilha-value.
      when 2.
        wa_saida-item_number  = wl_planilha-value.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wa_saida-item_number
          importing
            output = vmatnr.



    endcase.

    at end of row.
      append wa_saida to gt_saida.
    endat.
  endloop.


  check gt_saida is not initial.

  loop at gt_saida assigning field-symbol(<ws_saida>).
    <ws_saida>-status = icon_generate.
    <ws_saida>-message = 'Dados importado, aguardando processamento'.
    <ws_saida>-check   = abap_true.


  endloop.


  clear: wa_saida.
*  MESSAGE text-s02 TYPE 'I' DISPLAY LIKE 'S'.
endform.                    "TRATAR_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
form alv_preenche_cat       using: p_campo         type c
                                   p_desc          type c
                                   p_tam           type c
                                   p_hot           type c
                                   p_zero          type c
                                   p_sum           type c
                                   p_icon          type c.
  data:
  wl_fcat type lvc_s_fcat.

  wl_fcat-fieldname  = p_campo.
  wl_fcat-scrtext_l  = p_desc.
  wl_fcat-scrtext_m  = p_desc.
  wl_fcat-scrtext_s  = p_desc.
  wl_fcat-hotspot    = p_hot.
  wl_fcat-no_zero    = p_zero.
  wl_fcat-outputlen  = p_tam.
  wl_fcat-icon       = p_icon.

  append wl_fcat to gt_fcat.
endform.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  tratar_campo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form tratar_campo changing v_value.
*  REPLACe '.' WITH ' ' INTO V_VALUE.
*  REPLACE ',' WITH '.' INTO V_VALUE.
*  REPLACE '-' WITH ' ' INTO V_VALUE.

*  TRANSLATE v_value USING '. '.
  translate v_value using ',.'.
*  TRANSLATE v_value USING '- '.

  condense v_value no-gaps.
endform.                    "tratar_campo

*&---------------------------------------------------------------------*
*&      Form  show_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form show_msg .

  call function 'Z_DOC_CHECK_NEW'
    exporting
      i_screen   = '100'
      i_show     = 'X'
      i_repid    = sy-repid
    importing
      e_messagem = wl_mensagem
    tables
      it_msgs    = gt_msg_return.
endform.                    "show_msg

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status '0100'.
  set titlebar '0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
module pbo_0100 output.

  refresh gt_fcat.
  perform alv_preenche_cat using:
        'STATUS          '        'Status proc        '         '6'   ''  ''  '' 'X',
        'CONTRACT_ID     '        'Id Contrato        '         '20'  ''  ''  '' '',
        'ITEM_NUMBER     '        'Id Item            '         '10'  ''  ''  '' '',
        'MESSAGE         '        'Mensagem           '         '255' ''  ''  '' ''.

  if ( obj_custom is initial ).
    create object obj_custom
      exporting
        container_name              = 'CUSTOM'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    create object obj_alv
      exporting
        i_parent          = obj_custom
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    wl_layout-zebra      = 'X'.
  endif.

  create object obj_splitter
    exporting
      parent  = obj_custom
      rows    = 2
      columns = 1.

  call method obj_splitter->get_container
    exporting
      row       = 1
      column    = 1
    receiving
      container = obj_custom_log.

  set handler:
  lcl_event_toolbar=>set_toolbar     for obj_alv,
  lcl_event_toolbar=>get_ucomm       for obj_alv.

  wl_layout-cwidth_opt = abap_true.
  wl_layout-zebra      = abap_true.


  call method obj_alv->set_table_for_first_display
    exporting
      is_layout                     = wl_layout
      i_default                     = 'X'
      i_save                        = 'A'
    changing
      it_outtab                     = gt_saida
      it_fieldcatalog               = gt_fcat
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.



  call method obj_splitter->set_row_height
    exporting
      id     = 1
      height = 100.

  call method obj_alv->refresh_table_display
    exporting
      is_stable = ls_stable
    exceptions
      finished  = 1
      others    = 2.
endmodule.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
module pai_0100 input.
  case sy-ucomm.
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when 'BTN_EXECUTAR'.
      if ( p_file is initial ).
        message text-e01 type 'I' display like 'E'.
      else.
        check ( gt_msg_return is initial ).
        perform tratar_arquivo.
      endif.
    when 'SHOW_MSG'.
      perform show_msg.
    when others.
  endcase.

  clear sy-ucomm.
endmodule.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  CARREGA_ARQUIVO  INPUT
*&---------------------------------------------------------------------*
module carrega_arquivo input.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    importing
      filename         = p_file
    exceptions
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

endmodule.                 " CARREGA_ARQUIVO  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  FM_PROC_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fm_proc_dados .

  data: w_answer(1).
  data: vmatnr18 type matnr18.


  call function 'POPUP_TO_CONFIRM'
    exporting
      text_question         = text-m04
*     TEXT_BUTTON_1         = 'Sim'(100)
      text_button_1         = text-b01
      icon_button_1         = 'ICON_OKAY'
*     TEXT_BUTTON_2         = 'Não'(101)
      text_button_2         = text-b02
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    importing
      answer                = w_answer
    exceptions
      text_not_found        = 1
      others                = 2.


  if w_answer eq 1.

    check gt_saida is not initial.

    loop at gt_saida assigning field-symbol(<ws_saida>).
      if p_sap = 'X'.
        <ws_saida>-status = icon_green_light.
        <ws_saida>-message = 'PROCESSADO COM SUCESSO NO SAP'.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <ws_saida>-contract_id
          importing
            output = <ws_saida>-contract_id.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = <ws_saida>-item_number
          importing
            output = vmatnr18.

        <ws_saida>-item_number = vmatnr18.

        update zmmt0177 set purchasable = 'false'
                            id_item     = 99999
                            user_elim   = sy-uname
                            data_elim   = sy-datum
        where contract_id = <ws_saida>-contract_id
        and   item_number = <ws_saida>-item_number.
        if sy-subrc ne 0.
          <ws_saida>-message = 'PROCESSADO COM SUCESSO, mas nao encontrado no SAP'.
        endif.
        commit work.
      else.
        refresh lt_retorno.
        clear code_retorno.
        perform f_check_coupa using <ws_saida>-contract_id <ws_saida>-item_number changing lt_retorno.
        if lt_retorno[] is initial.
          code_retorno = '0100'.
        endif.
        loop at lt_retorno into data(wt_retorno).
          if wt_retorno-id is not initial.
            perform f_elimina_coupa using 'DELETE' wt_retorno-id changing  code_retorno.
            exit.
          endif.
        endloop.
        <ws_saida>-status = icon_red_light.
        if code_retorno = '0100'.
          <ws_saida>-message = 'MATERIAL NÃO ENCONTRADO NO COUPA PARA O CONTRATO'.
        elseif code_retorno = '0500'.
          <ws_saida>-message = 'SEM RETORNO DO COUPA'.
        elseif code_retorno ne '0200'.
          <ws_saida>-message = 'CODIGO DE RETORNO DO COUPA COM ERRO ' && code_retorno.
        else.
          <ws_saida>-status = icon_green_light.
          <ws_saida>-message = 'PROCESSADO COM SUCESSO'.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = <ws_saida>-contract_id
            importing
              output = <ws_saida>-contract_id.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = <ws_saida>-item_number
            importing
              output = vmatnr18.

          <ws_saida>-item_number = vmatnr18.

          update zmmt0177 set purchasable = 'false'
                              id_item     = wt_retorno-id
                              user_elim   = sy-uname
                              data_elim   = sy-datum
          where contract_id = <ws_saida>-contract_id
          and   item_number = <ws_saida>-item_number.
          if sy-subrc ne 0.
            <ws_saida>-message = 'PROCESSADO COM SUCESSO, mas nao encontrado no SAP'.
          endif.
          commit work.
        endif.
      endif.

    endloop.
  endif.

  call method obj_alv->refresh_table_display
    exporting
      is_stable = ls_stable
    exceptions
      finished  = 1
      others    = 2.

  if sy-subrc <> 0.
  endif.


endform.

form f_check_coupa  using p_contrato p_material changing   lt_retorno.
  data: lv_string       type string,
        lv_data_conv    type string,
        lv_datum        type timestamp,
        lv_info_request type string,
        lv_offset       type string,
        lv_matnr        type matnr,
        lv_matnr18      type matnr18,
        c_numeric       type string value ' -.0123456789'.


  lv_info_request = p_contrato && ';' && p_material && ';' && lv_offset.
  lv_offset = lv_offset + 50.
  zcl_int_ob_supplier_item_contr=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = lv_info_request importing e_integracao = data(r_response) ).
  if r_response is not initial.
    lv_string = r_response-ds_data_retorno.
    replace all occurrences of '-' in lv_string with '_'.
    /ui2/cl_json=>deserialize( exporting json = lv_string changing data = lt_retorno ).
  endif.


endform.

form f_elimina_coupa  using p_contrato p_material changing retorno.
  data: lv_string       type string,
        lv_data_conv    type string,
        lv_datum        type timestamp,
        lv_info_request type string,
        lv_offset       type string,
        lv_matnr        type matnr,
        lv_matnr18      type matnr18,
        c_numeric       type string value ' -.0123456789'.



  lv_info_request = p_contrato && ';' && p_material.
  lv_offset = lv_offset + 50.
  zcl_int_ob_supplier_item_contr=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = lv_info_request importing e_integracao = data(r_response) ).
  if r_response is not initial.
    retorno = r_response-nm_code.
  else.
    retorno = '0500'.
  endif.


endform.
