*&---------------------------------------------------------------------*
*& Include          ZPMR0087_CLA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Eventhandler
class lcl_report definition deferred.
data: lo_report type ref to lcl_report.
class lcl_report definition.
  public section.
    data: o_alv    type ref to cl_salv_table,
          it_saida type standard table of zpme0087 initial size 0.
    methods:
      get_data,
      generate_output,
      set_columns
        changing
          co_alv type ref to cl_salv_table,
      set_pf_status
        changing
          co_alv type ref to cl_salv_table,
      set_layout
        changing
          co_alv type ref to cl_salv_table,
      on_user_command for event added_function of cl_salv_events importing e_salv_function sender,
      set_HANDLER changing co_alV   type ref to cl_salv_table co_report type ref to lcl_report,
      MAKE_toolbar for event toolbar of cl_gui_alv_grid importing e_interactive e_object sender,
      on_link_click for event link_click of cl_salv_events_table importing row column .

endclass.


class lcl_report implementation.

  method get_data.

    data nmjob(8) type c value 'RKO7KO8G'.
    free: it_saida.
    select
       substring( a~jobname,9,4 ) as empresa
       ,a~jobname
       ,a~jobcount
       ,case when a~reluname <> ' ' then a~reluname else a~sdluname end as executor_nm
       ,a~reldate as dtstart
       ,a~reltime as hrstart
       ,a~enddate as dtfinish
       ,a~endtime as hrfinish
       ,case
       when a~status = 'A' then '@0A@'
       when a~status = 'F' then '@08@'
       else '@09@'
       end as job_status
       ,case
       when a~status = 'A' then 'Cancelado'
       when a~status = 'F' then 'Completado'
       when a~status = 'P' then 'Agendado'
       when a~status = 'R' then 'Ativo'
       when a~status = 'S' then 'Lançado'
       end as job_desc
       ,' ' as liq_status
       ,cast( b~listident as int4 ) as ident
       from tbtco as a
       left join tbtcp as b on a~jobname = b~jobname
       and a~jobcount = b~jobcount
   where substring( a~jobname,1,8 ) = 'RKO7KO8G'
   and a~reldate > 0
   into table @it_saida.

    sort it_saida by empresa ascending dtstart descending hrstart descending.

    delete adjacent duplicates from it_saida comparing empresa.

    data ident type i.
    data lt_spool type standard table of lvc_s_1022 initial size 0 .
    data l_file_length type rspoid.
    data spool_contents type soli_tab.

    loop at it_saida assigning field-symbol(<_get_spoll>).
      clear: l_file_length,ident.
      free: lt_spool.

      if <_get_spoll>-ident > 0.

        "Verificar se a ordem spool ainda esta disponivel.
        call function 'RSPO_CHECK_JOB_ID_PERMISSION'
          exporting
            rqident       = <_get_spoll>-ident
            access        = 'DISP'
          exceptions
            no_such_job   = 1
            no_permission = 2
            others        = 3.
        if sy-subrc eq 0.

          call function 'RSPO_RETURN_ABAP_SPOOLJOB'
            exporting
              rqident = <_get_spoll>-ident
            tables
              buffer  = spool_contents.
          if sy-subrc = 0.

            loop at spool_contents into data(lr_spool).
              if sy-tabix = 22 .
                <_get_spoll>-liq_result = lr_spool.
                condense lr_spool no-gaps.

                clear: <_get_spoll>-liq_status.
                case lr_spool.
                  when 'Processamentoencerradocomerros'.
                    <_get_spoll>-liq_status = '@0A@'.
                  when others.
                    <_get_spoll>-liq_status = '@08@'.
                endcase.
              endif.
            endloop.
          endif.
        endif.
      endif.
      free:spool_contents.
    endloop.

    sort it_saida by liq_status.
    delete it_saida where liq_status eq space.


  endmethod.
  method generate_output.
    data(container) =
      new cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      container_name = 'CC_ALV' ).


    data: lx_msg type ref to cx_salv_msg.
    try.
        cl_salv_table=>factory(
          exporting
            r_container    = container
            container_name = 'CC_ALV'
          importing
            r_salv_table   = o_alv
          changing
            t_table        = it_saida ).
      catch cx_salv_msg into lx_msg.
    endtry.

    call method set_pf_status
      changing
        co_alv = o_alv.

    call method set_layout
      changing
        co_alv = o_alv.

    call method me->set_columns
      changing
        co_alv = o_alv.

    call method set_HANDLER
      changing
        co_alv    = o_alv
        co_report = lo_report.


    o_alv->display( ).

  endmethod.

  method set_HANDLER.
*
*...HotSpot
    data: lo_cols_tab type ref to cl_salv_columns_table,
          lo_col_tab  type ref to cl_salv_column_table.
*
*   get Columns object
    lo_cols_tab = co_alv->get_columns( ).
*
*   Get VBELN column
    try.
        lo_col_tab ?= lo_cols_tab->get_column( 'LIQ_STATUS' ).
      catch cx_salv_not_found.
    endtry.
*
*   Set the HotSpot for VBELN Column
    try.
        call method lo_col_tab->set_cell_type
          exporting
            value = if_salv_c_cell_type=>hotspot.
        .
      catch cx_salv_data_error .
    endtry.
*
*...Events
    data: lo_events type ref to cl_salv_events_table.
*
*   all events
    lo_events = o_alv->get_event( ).
*
*   event handler
    set handler co_report->on_link_click for lo_events.
    set handler co_report->make_toolbar for all instances.
    set handler co_report->on_user_command for lo_events.
  endmethod.

  method on_link_click.

    data: wa_saida type zpme0087.
    clear: wa_saida.

    if column = 'LIQ_STATUS'.
      read table lo_report->it_saida into wa_saida index row.
      if wa_saida-ident is not initial.

        data: lv_spoolid    type tsp01-rqident,
              lv_pdf_size   type i,               " PDF size
              lt_pdf_table  type table of tline,  " Internal table to store PDF
              lv_html       type string,
              lv_base64_pdf type string,
              lv_xstring    type xstring,
              lt_binary     type table of x255,   " Binary internal table
              lt_lines      type i,
              ev_spoolid    type tsp01-rqident,
              lv_bin_length type i.               " Binary length

        clear:lv_spoolid,lv_base64_pdf,lv_pdf_size,lt_pdf_table[],lv_xstring,lt_binary,lv_bin_length.

        lv_spoolid = wa_saida-ident.

        call function 'CONVERT_ABAPSPOOLJOB_2_PDF'
          exporting
            src_spoolid              = lv_spoolid   " Spool Request ID
            no_dialog                = 'X'
            dst_device               = 'PDF1'
            pdf_destination          = 'X'
          importing
            pdf_bytecount            = lv_pdf_size  " PDF Size
            pdf_spoolid              = ev_spoolid
            bin_file                 = lv_xstring
          tables
            pdf                      = lt_pdf_table " PDF content
          exceptions
            err_no_abap_spooljob     = 1
            err_no_spooljob          = 2
            err_no_permission        = 3
            err_conv_not_possible    = 4
            err_bad_destdevice       = 5
            user_cancelled           = 6
            err_spoolerror           = 7
            err_temseerror           = 8
            err_btcjob_open_failed   = 9
            err_btcjob_submit_failed = 10
            err_btcjob_close_failed  = 11
            others                   = 12.

        call function 'SCMS_BASE64_ENCODE_STR'
          exporting
            input  = lv_xstring
          importing
            output = lv_base64_pdf.

        lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.

        cl_abap_browser=>show_html( html_string = lv_html title = 'PDF' ).

      endif.
    endif.



  endmethod.



  method set_pf_status.

    data: lo_functions type ref to cl_salv_functions_list.
    lo_functions = co_alv->get_functions( ).
    lo_functions->set_all( abap_true ).
    "lo_functions->set_default( abap_true ).

    try.
        lo_functions->add_function( name     = 'BT_REFRESH_JOB'
                                    icon     = '@42@'
                                    text     = 'Atualizar Status Job'
                                    tooltip  = ''
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

        lo_functions->add_function( name     = 'BT_START_JOB'
                                    icon     = '@M4@'
                                    text     = 'Iniciar Job'
                                    tooltip  = 'Cria job - Período atual'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).


        lo_functions->add_function( name     = 'BT_START_JOB_ANT'
                                    icon     = '@EP@'
                                    text     = 'Iniciar Job ( período anterior)'
                                    tooltip  = 'Cria job - Período anterior'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

        lo_functions->add_function( name     = 'BT_CLOSE_JOB'
                                    icon     = '@LQ@'
                                    text     = 'Cancelar Job'
                                    tooltip  = 'Botão 2 dica'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).

      catch cx_root.

    endtry.




*TRY.
**      enable buttons for SALV
*      p_alv->get_functions( )->set_all( abap_true ).
*
*
*      CLEAR: lv_text, lv_tooltip.
*
*      lv_text     = 'Atualizar Status Job'.
**      lv_tooltip  = ''.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_refresh_job }|
*                                             icon     = |{ gc_icon_refresh  }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      lv_text     = 'Iniciar Job'.
*      lv_tooltip  = 'Cria job - Período atual'.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job }|
*                                             icon     = |{ gc_background_job }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lv_text     = 'Iniciar Job ( período anterior)'.
*      lv_tooltip  = 'Cria job - Período anterior'.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job_ant }|
*                                             icon     = |{ gc_background_job_ant }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CLEAR: lv_text, lv_tooltip.
*
*      lv_text     = 'Cancelar Job'.
**     lv_tooltip  = 'Botão 2 dica'.
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_close_job }|
*                                             icon     = |{ gc_terminated_job }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
***********************************************************************145948 - Ajuste para visualizar erros ZPM0106 - PSA
**      CLEAR: lv_text, lv_tooltip.
**
**      lv_text     = 'Erros'.
**      lv_tooltip  = 'Erros de KO8G'.
**
**      p_alv->get_functions( )->add_function( name     = |ERROS|
**                                             icon     = |@UI@|
**                                             text     = lv_text
**                                             tooltip  = lv_tooltip
**                                             position = if_salv_c_function_position=>right_of_salv_functions ).
***********************************************************************
**      set event handler
*      SET HANDLER lcl_events=>on_toolbar_click FOR p_alv->get_event( ).
*
*    CATCH cx_root.
**     maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
*
*  TRY.




  endmethod.

  method set_layout.
*
    data: lo_layout  type ref to cl_salv_layout,
          lf_variant type slis_vari,
          ls_key     type salv_s_layout_key.
*   get layout object
    lo_layout = co_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
    lf_variant = 'DEFAULT'.
    lo_layout->set_initial_layout( lf_variant ).
  endmethod.

  method set_columns.
*
*...Get all the Columns
    data: lo_cols type ref to cl_salv_columns.
    lo_cols = o_alv->get_columns( ).
*
*   set the Column optimization
    "lo_cols->set_optimize( 'X' ).
*
*...Process individual columns
    data: lo_column type ref to cl_salv_column.
*
*   Change the properties of the Columns KUNNR
    try.
        "Mostra
        lo_column = lo_cols->get_column( 'EMPRESA' ).
        lo_column->set_long_text( 'Empresa' ).
        lo_column->set_medium_text( 'Empresa' ).
        lo_column->set_short_text( 'Empresa' ).
        lo_column->set_output_length( 8 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'EXECUTOR_NM' ).
        lo_column->set_long_text( 'Executor' ).
        lo_column->set_medium_text( 'Executor' ).
        lo_column->set_short_text( 'Excutor' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'DTSTART' ).
        lo_column->set_long_text( 'Dt Inicio' ).
        lo_column->set_medium_text( 'Dt Inicio' ).
        lo_column->set_short_text( 'Dt Inicio' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'HRSTART' ).
        lo_column->set_long_text( 'Hr Inicio' ).
        lo_column->set_medium_text( 'Hr Inicio' ).
        lo_column->set_short_text( 'Hr Inicio' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'DTFINISH' ).
        lo_column->set_long_text( 'Dt Fim' ).
        lo_column->set_medium_text( 'Dt Fim' ).
        lo_column->set_short_text( 'Dt Fim' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'HRFINISH' ).
        lo_column->set_long_text( 'Hr Fim' ).
        lo_column->set_medium_text( 'Hr Fim' ).
        lo_column->set_short_text( 'Hr Fim' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'JOB_STATUS' ).
        lo_column->set_long_text( 'Job Status' ).
        lo_column->set_medium_text( 'Job Status' ).
        lo_column->set_short_text( 'Job Status' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).
        lo_column->set_alignment( if_salv_c_alignment=>centered ).

        lo_column = lo_cols->get_column( 'JOB_DESC' ).
        lo_column->set_long_text( 'Job Descrição' ).
        lo_column->set_medium_text( 'Job Desc.' ).
        lo_column->set_short_text( 'Job Desc.' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_cols->get_column( 'LIQ_STATUS' ).
        lo_column->set_long_text( 'Liq. Status' ).
        lo_column->set_medium_text( 'Liq. Status' ).
        lo_column->set_short_text( 'Liq. Stat.' ).
        lo_column->set_output_length( 10 ).
        lo_column->set_optimized( abap_false ).
        lo_column->set_alignment( if_salv_c_alignment=>centered ).

        lo_column = lo_cols->get_column( 'LIQ_RESULT' ).
        lo_column->set_long_text( 'Liquidação Resultado' ).
        lo_column->set_medium_text( 'Liq.Result.' ).
        lo_column->set_short_text( 'Liq.Res.' ).
        lo_column->set_output_length( 40 ).
        lo_column->set_optimized( abap_false ).

        "Esconde
        lo_column = lo_cols->get_column( 'IDENT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column = lo_cols->get_column( 'JOBNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column = lo_cols->get_column( 'JOBCOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column = lo_cols->get_column( 'DTTIME_START' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).





        "Organiza
        lo_cols->set_column_position( columnname = 'EMPRESA' position = 01 ).
        lo_cols->set_column_position( columnname = 'EXECUTOR_NM' position = 02 ).
        lo_cols->set_column_position( columnname = 'DTSTART' position = 03 ).
        lo_cols->set_column_position( columnname = 'HRSTART' position = 04 ).
        lo_cols->set_column_position( columnname = 'DTFINISH' position = 05 ).
        lo_cols->set_column_position( columnname = 'HRFINISH' position = 06 ).
        lo_cols->set_column_position( columnname = 'JOB_STATUS' position = 07 ).
        lo_cols->set_column_position( columnname = 'JOB_DESC' position = 08 ).
        lo_cols->set_column_position( columnname = 'LIQ_STATUS' position = 09 ).
        lo_cols->set_column_position( columnname = 'LIQ_RESULT' position = 10 ).


      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.




  endmethod.                    "SET_COLUMNS


  method on_user_command.

    case e_salv_function.

      when 'BT_START_JOB'.

        perform start_job_manual using abap_false changing gt_data.

      when 'BT_REFRESH_JOB'.

        "PERFORM f_refresh.
        lo_report->get_data( ).
        o_alv->refresh( ).

      when 'BT_START_JOB_ANT'. "Periodo anterior

*        PERFORM open_job    USING lt_rows
*                                  abap_true
*                            CHANGING gt_data.

        perform open_job_manual    using abap_true
                            changing gt_data.

      when 'BT_CLOSE_JOB'.

*        PERFORM close_job   USING lt_rows
*                            CHANGING gt_data.

        perform close_job_manual changing gt_data.

      when others.

    endcase.

    lo_report->get_data( ).
    o_alv->refresh( ).


  endmethod.


  method MAKE_toolbar.

*    DATA : mt_toolbar TYPE stb_button.
*
*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '3'.   "separator
*    APPEND mt_toolbar TO e_object->mt_toolbar.
*
*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.
*
*    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
*      "3 DESABILITA E 0 HABILITA
*      IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
*        <fs_tollbar>-butn_type = '3'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
*        <fs_tollbar>-butn_type = '3'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
*        <fs_tollbar>-butn_type = '3'.
*      ENDIF.
*      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
*        <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
*        <fs_tollbar>-function = 'DELETE_ROW'.
*      ENDIF.
*    ENDLOOP.
  endmethod.

endclass.



*CLASS lcl_events DEFINITION.
*  PUBLIC SECTION.
*
*    CLASS-METHODS : on_toolbar_click FOR EVENT added_function OF cl_salv_events_table
*      IMPORTING
*        e_salv_function
*        sender.
*
*ENDCLASS.
*
*
**----------------------------------------------------------------------*
** CLASS IMPLEMENTATION                                                 *
**----------------------------------------------------------------------*
*CLASS lcl_events IMPLEMENTATION.
*  METHOD on_toolbar_click.
*
*    DATA:
*      lr_selections   TYPE REF TO cl_salv_selections.
*
*    DATA:
*      lt_rows         TYPE        salv_t_row.
*
*
*    lr_selections = gr_alv->get_selections( ).
*    lt_rows = lr_selections->get_selected_rows( ).
*
*
*    CASE e_salv_function.
*
*      WHEN gc_btn_start_job.
*        "PSA
**        PERFORM open_job    USING lt_rows
**                                  abap_false
**                            CHANGING gt_data.
*
*PERFORM start_job_manual USING abap_false CHANGING gt_data.
*
*      WHEN gc_btn_refresh_job.
*
*        PERFORM f_refresh.
*
*      WHEN gc_btn_start_job_ant. "Periodo anterior
*
*        PERFORM open_job    USING lt_rows
*                                  abap_true
*                            CHANGING gt_data.
*
*      WHEN gc_btn_close_job.
*
*        PERFORM close_job   USING lt_rows
*                            CHANGING gt_data.
*
*      WHEN 'ERROS'. "145948 - Ajuste para visualizar erros ZPM0106 - PSA
***********************************************************************145948 - Ajuste para visualizar erros ZPM0106 - PSA
*
*        "WHEN OTHERS.
*
*    ENDCASE.
*
*
*    gr_alv->refresh( ).
*
*  ENDMETHOD.
*ENDCLASS.

*&---------------------------------------------------------------------*
*& Module D0100_PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module d0100_pbo output.
  set pf-status 'D0100'.
  set titlebar 'TITLE_0100'.
  create object lo_report.
  lo_report->get_data( ).
  lo_report->generate_output( ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      set screen 0.
      leave screen.
    when 'CANCEL'.

    when 'EXIT'.
      leave to screen 0.
      exit.
  endcase.
endmodule.
