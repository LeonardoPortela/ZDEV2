
*&---------------------------------------------------------------------*
*& Report  zmmr198
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmmr198.

*--------------------------------------------------------------------------------------------------*
* Developer: Anderson Oenning
* Data.....: 25.04.2023
*--------------------------------------------------------------------------------------------------*

type-pools icon.

types: begin of ty_saida,
         status          type icon-id,
         contract_id     type zmmt0177-contract_id,
         item_number     type zmmt0177-item_number,
         price           type zmmt0177-price,
         supplier_number type zmmt0177-supplier_number,
         created_at      type zmmt0177-created_at,
         updated_at      type zmmt0177-updated_at,
         purchasable     type zmmt0177-purchasable,
         message(255)    type c,
         check           type c,
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
      gt_j_1bnfdoc       type table of j_1bnfdoc,
      gt_j_1bnflin       type table of j_1bnflin,
      gt_j_1bnfstx       type table of j_1bnfstx,
      it_j_1bnflin       type table of j_1bnflin,
      it_j_1bnfstx       type table of j_1bnfstx,

      wa_saida           type ty_saida,
      wl_planilha        like alsmex_tabline,
      wl_msg_return      type zfiwrs0002,
      wl_saida           type ty_saida,
      wl_zibcontabil     type zib_contabil,
      wl_layout          type lvc_s_layo,
      wl_mensagem        type char30,
      wl_stable          type lvc_s_stbl,
      wl_zibcontabil_chv type zib_contabil_chv,
      wl_zibcontabil_err type zib_contabil_err,
      wl_toolbar         type stb_button,
      ok_code            like sy-ucomm,
      vg_ped,
      vg_req,
      p_file             type rlgrap-filename.



data: it_zmmt0177 type table of zmmt0177,
      wa_zmmt0177 type zmmt0177.


data: t_return  like bapiret2      occurs 0 with header line.
data: t_item    like bapimepoitem  occurs 0 with header line.
data: t_itemx   like bapimepoitemx occurs 0 with header line.

data: t_itemr   type table of bapimereqitemimp with header line, " RJF
      t_itemxr  type table of bapimereqitemx   with header line,
      t_returnr type table of bapiret2         with header line.

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
      i_end_col               = 7
      i_end_row               = 99999
    tables
      intern                  = gt_planilha
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.
*
*TYPES: BEGIN OF struct_accsymbrep,
*        chart_of_acc   TYPE ktopl,
*        acc_symbol     TYPE tpm_acc_symbol,
*        valuation_area TYPE tpm_val_area,
*        aa_ref         TYPE tpm_aa_ref,
*        currency       TYPE tpm_position_curr,
*        gl_account     TYPE saknr,
*       END OF struct_accsymbrep,
*gtt_excel_data  TYPE STANDARD TABLE OF struct_accsymbrep.
*
*DATA: lt_raw_data   TYPE truxs_t_text_data,
*      tpmco_xtrue   TYPE c VALUE 'X',
**      gtt_excel_data  TYPE STANDARD TABLE OF struct_accsymbrep,
*      ct_excel_data TYPE gtt_excel_data.

**Upload from presentation server\PC
*  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*    EXPORTING
**     I_FIELD_SEPERATOR    =
*      i_line_header        = tpmco_xtrue
*      i_tab_raw_data       = lt_raw_data
*      i_filename           = p_file
*    TABLES
*      i_tab_converted_data = ct_excel_data
*    EXCEPTIONS
*      conversion_failed    = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
**Check if input file has any record or not
*  IF lines( ct_excel_data ) IS INITIAL.
*    MESSAGE e029(tpm_trac1).
*  ENDIF.


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

        wa_saida-item_number = vmatnr.
      when 3.
*       PERFORM tratar_campo USING wl_planilha-value.
        wa_saida-price      = wl_planilha-value.
      when 4.
        wa_saida-supplier_number   = wl_planilha-value.
      when 5.
        wa_saida-created_at = wl_planilha-value.
      when 6.
        wa_saida-updated_at = wl_planilha-value.
      when 7.
        wa_saida-purchasable     = wl_planilha-value.
    endcase.

    at end of row.
      append wa_saida to gt_saida.
    endat.
  endloop.


  check gt_saida is not initial.

*  MOVE-CORRESPONDING gt_saida TO IT_ZMMT0177.
*  IF IT_ZMMT0177 IS NOT INITIAL.
*  MODIFY ZMMT0177 FROM TABLE IT_ZMMT0177.
*  COMMIT WORK.
*  ENDIF.

  loop at gt_saida assigning field-symbol(<ws_saida>).
    <ws_saida>-status = icon_generate.
    <ws_saida>-message = 'Dados importado, aguardando processamento'.
    <ws_saida>-check   = abap_true.

*       MOVE-CORRESPONDING <ws_saida> TO WA_ZMMT0177.
*
*      MODIFY zmmt0177 from WA_ZMMT0177.
*      commit WORK.
*      IF sy-subrc eq 0.
*       <ws_saida>-status = icon_green_light.
*        <ws_saida>-message = 'Processado com sucesso'.
*      else.
*        <ws_saida>-status = ICON_RED_LIGHT.
*        <ws_saida>-message = 'Erro ao gravar informação'.
*      ENDIF.
*      CLEAR: WA_ZMMT0177.

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
*  delete from ZMMT0177.
*  COMMIT WORK.

  perform alv_preenche_cat using:
        'STATUS          '        'Status proc        '         '6'   ''  ''  '' 'X',
        'CONTRACT_ID     '        'Id Contrato        '         '20'  ''  ''  '' '',
        'ITEM_NUMBER     '        'Id Item            '         '10'  ''  ''  '' '',
        'PRICE           '        'Valor              '         '20'  ''  ''  '' '',
        'SUPPLIER_NUMBER '        'Nº conta fornecedor'         '30'  ''  ''  '' '',
        'CREATED_AT      '        'Data criação       '         '20'  ''  ''  '' '',
        'UPDATED_AT      '        'Data alteração     '         '10'  ''  ''  '' '',
        'PURCHASABLE     '        'Status ativo       '         '10'  ''  ''  '' '',
        'MESSAGE         '        'Mensagem           '         '255'  ''  ''  '' ''.

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

      move-corresponding <ws_saida> to wa_zmmt0177.
      if wa_zmmt0177 is not initial.
        modify zmmt0177 from wa_zmmt0177.
        commit work.

        if sy-subrc eq 0.
          <ws_saida>-status = icon_green_light.
          <ws_saida>-message = 'Processado com sucesso'.
        else.
          <ws_saida>-status = icon_red_light.
          <ws_saida>-message = 'Erro ao gravar informação'.
        endif.
        clear: wa_zmmt0177.
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
