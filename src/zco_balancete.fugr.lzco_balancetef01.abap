*----------------------------------------------------------------------*
***INCLUDE LZCO_BALANCETEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_excel  TABLES pt_data
                    USING  p_structure TYPE ddobjname.

  TYPES: BEGIN OF ty_data,
           fieldname TYPE c LENGTH 1024,
         END OF ty_data.

  DATA: lo_table      TYPE REF TO cl_salv_table,
        lx_xml        TYPE xstring,
        ls_length     TYPE i,
        ls_xml_stream TYPE xml_rawdata.

  DATA: lt_fieldname TYPE STANDARD TABLE OF ty_data,
        ls_fieldname TYPE ty_data.

  DATA: ls_file_name TYPE string,
        vg_vlr       TYPE zvlr_brl.

  DATA:
    lv_filetype TYPE char10,
    lv_codepage TYPE abap_encoding.

  FREE: gt_temp, gt_columns, ls_file_name.

  ls_file_name = diretorio.

  IF diretorio CS '.XLS' OR diretorio CS '.XLSX' OR diretorio CS '.xls' OR diretorio CS '.xlsx'.
    ls_file_name = diretorio.
  ELSE.
    IF gv_automoeda IS INITIAL.
      CONCATENATE diretorio '.XLSX' INTO ls_file_name.
    ENDIF.
  ENDIF.

  CASE p_structure.
    WHEN 'ZSCO_SALDO_CONTAS_INTERNA'.

      ls_fieldname-fieldname = 'Empresa'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Ano'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Mês'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'NºConta'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-PT'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-EN'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Sociedade'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Saldo-Interna'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'D/C-Interna'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Nivel'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.



      TRY.
          cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_saldo_contas_interna[] ).
        CATCH cx_salv_msg.
      ENDTRY.

      lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX


      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lx_xml
        IMPORTING
          output_length = ls_length
        TABLES
          binary_tab    = ls_xml_stream.


      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = ls_length
          filetype     = 'BIN'
          filename     = ls_file_name
        CHANGING
          data_tab     = ls_xml_stream
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


*      CALL FUNCTION 'XML_EXPORT_DIALOG'
*        EXPORTING
*          i_xml                      = lx_xml
*          i_default_extension        = 'XLSX'
*          i_initial_directory        = ''
*          i_default_file_name        = ls_file_name "'export.XLSX'
*          i_mask                     = 'Excel (*.XLSX)|*.XLSX'
*        EXCEPTIONS
*          application_not_executable = 1
*        OTHERS.


*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename                = ls_file_name
*          filetype                = 'DBF'
*          write_field_separator   = 'X'
*          trunc_trailing_blanks   = 'X'
*        TABLES
*          data_tab                = gt_saldo_contas_interna
*          fieldnames              = lt_fieldname
*        EXCEPTIONS
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          OTHERS                  = 22.
*      IF sy-subrc <> 0.
*      ENDIF.

    WHEN 'ZSCO_SALDO_CONTAS_FORTE'.

      ls_fieldname-fieldname = 'Empresa'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Ano'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Mês'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'NºConta'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-PT'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-EN'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Sociedade'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Saldo-Forte'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'D/C-Forte'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Nivel'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.


      TRY.
          cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_saldo_contas_forte[] ).
        CATCH cx_salv_msg.
      ENDTRY.

      lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX


      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lx_xml
        IMPORTING
          output_length = ls_length
        TABLES
          binary_tab    = ls_xml_stream.


      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = ls_length
          filetype     = 'BIN'
          filename     = ls_file_name
        CHANGING
          data_tab     = ls_xml_stream
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename                = ls_file_name
*          filetype                = 'DBF'
*          write_field_separator   = 'X'
*          trunc_trailing_blanks   = 'X'
*        TABLES
*          data_tab                = gt_saldo_contas_forte
*          fieldnames              = lt_fieldname
*        EXCEPTIONS
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          OTHERS                  = 22.
*      IF sy-subrc <> 0.
*      ENDIF.


    WHEN 'ZSCO_SALDO_CONTAS_INDICE'.

      ls_fieldname-fieldname = 'Empresa'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Ano'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Mês'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'NºConta'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-PT'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-EN'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Sociedade'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Saldo-Indice'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'D/C-Indice'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Nivel'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.


      TRY.
          cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_saldo_contas_indice[] ).
        CATCH cx_salv_msg.
      ENDTRY.

      lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX


      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lx_xml
        IMPORTING
          output_length = ls_length
        TABLES
          binary_tab    = ls_xml_stream.


      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = ls_length
          filetype     = 'BIN'
          filename     = ls_file_name
        CHANGING
          data_tab     = ls_xml_stream
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.



*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename                = ls_file_name
*          filetype                = 'DBF'
*          write_field_separator   = 'X'
*          trunc_trailing_blanks   = 'X'
*        TABLES
*          data_tab                = gt_saldo_contas_indice
*          fieldnames              = lt_fieldname
*        EXCEPTIONS
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          OTHERS                  = 22.
*      IF sy-subrc <> 0.
*      ENDIF.

    WHEN 'ZSCO_TP_MOEDA'.

      MOVE-CORRESPONDING gt_tp_moeda TO gt_vlr_moeda.

      LOOP AT gt_tp_moeda ASSIGNING FIELD-SYMBOL(<ls_tp_moeda>).

      ENDLOOP.

      LOOP AT gt_vlr_moeda ASSIGNING FIELD-SYMBOL(<ws_moeda>).

        "Check valor negativo BRL.
        CLEAR: vg_vlr.
        vg_vlr = <ws_moeda>-vlr_brl.

        gv_vlr_brl = <ws_moeda>-vlr_brl.

        IF vg_vlr < 0.
          REPLACE '-' IN gv_vlr_brl WITH ''.
          CONDENSE gv_vlr_brl NO-GAPS.

          gv_vlr_brl = |-{ gv_vlr_brl }|.

          <ws_moeda>-vlr_brl = gv_vlr_brl.
        ENDIF.

        "Check valor negativo USD.
        CLEAR: vg_vlr.
        vg_vlr = <ws_moeda>-vlr_usd.
        gv_vlr_usd = <ws_moeda>-vlr_usd.

        IF vg_vlr < 0.
          REPLACE '-' IN gv_vlr_usd WITH ''.
          CONDENSE gv_vlr_usd NO-GAPS.

          gv_vlr_usd = |-{ gv_vlr_usd }|.

          <ws_moeda>-vlr_usd = gv_vlr_usd.
        ENDIF.


*        REPLACE '.' IN <ws_moeda>-vlr_brl WITH ','.
*        CONDENSE <ws_moeda>-vlr_brl NO-GAPS.
*
*        REPLACE '.' IN <ws_moeda>-vlr_usd WITH ','.
*        CONDENSE <ws_moeda>-vlr_usd NO-GAPS.

      ENDLOOP.

      ls_fieldname-fieldname = 'Empresa'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Ano'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Mês'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'NºConta'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-PT'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Descrição-EN'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Sociedade'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Valor BRL'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'D/C-BRL'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Valor USD'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'D/C-USD'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
      ls_fieldname-fieldname = 'Nivel'.
      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.

* RJF - Ini - 2023.04.25
      IF gv_automoeda IS NOT INITIAL.

*        lv_filetype = 'ASC'.
*        lv_codepage = 4110.

        lv_filetype = 'DBF'.
        lv_codepage = abap_off.

        IF diretorio IS NOT INITIAL.
          ls_file_name = diretorio.
          gs_file_name = ls_file_name.
        ELSE.
          MOVE 'C:\TEMP\XLX_FILE.XLX' TO ls_file_name.
          gs_file_name = ls_file_name.
        ENDIF.

        IF gt_vlr_moeda[] IS INITIAL.
          FREE gs_file_name.
        ENDIF.
      ELSE.
        lv_filetype = 'ASC'."
        lv_codepage = abap_off.
      ENDIF.
      FREE gv_automoeda.
* RJF - Fim - 2023.04.25



      TRY.
          cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = gt_vlr_moeda[] ).
        CATCH cx_salv_msg.
      ENDTRY.

      lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX


      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lx_xml
        IMPORTING
          output_length = ls_length
        TABLES
          binary_tab    = ls_xml_stream.


      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = ls_length
          filetype     = 'BIN'
          filename     = ls_file_name
        CHANGING
          data_tab     = ls_xml_stream
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filename                = ls_file_name
*          filetype                = lv_filetype
*          write_field_separator   = 'X'
*          trunc_trailing_blanks   = 'X'
*          codepage                = lv_codepage
*        TABLES
*          data_tab                = gt_vlr_moeda
*          fieldnames              = lt_fieldname
*        EXCEPTIONS
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          OTHERS                  = 22.

      "PSA

*      CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
*        EXPORTING
*          i_filename     = 'C:\Users\pablo.alves\Downloads\PABLITO.xls'
*        TABLES
*          i_tab_sap_data = gt_vlr_moeda[].


      IF sy-subrc <> 0.

* RJF - Ini - 2023.04.25
        FREE gs_file_name.
* RJF - Fim - 2023.04.25

      ENDIF.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_CSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TP_MOEDA  text
*      -->P_0780   text
*----------------------------------------------------------------------*
FORM f_gerar_csv  TABLES pt_data
                    USING  p_structure TYPE ddobjname.

  TYPES truxs_t_text_data(4096) TYPE c OCCURS 0.
  DATA: csv_converted_table TYPE  truxs_t_text_data,
        csv_table           TYPE  truxs_t_text_data.
  DATA: csv_converted_line LIKE LINE OF csv_converted_table.
  DATA: it_arq                 TYPE TABLE OF zhcms_aux_alim_ref.

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata TYPE REF TO data.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name TYPE char20,
    END OF t_fieldnames.

  TYPES: BEGIN OF ty_data,
           fieldname TYPE c LENGTH 1024,
         END OF ty_data.

  DATA: lt_fieldname TYPE STANDARD TABLE OF ty_data,
        ls_fieldname TYPE ty_data.

  DATA: ls_file_name TYPE string,
        vg_vlr       TYPE zvlr_brl.

  DATA:
    lv_filetype TYPE char10,
    lv_codepage TYPE abap_encoding.

  FREE: gt_temp, gt_columns, ls_file_name.

  CASE p_structure.

    WHEN 'ZSCO_TP_MOEDA'.

      MOVE-CORRESPONDING gt_tp_moeda TO gt_vlr_moeda.

      LOOP AT gt_tp_moeda ASSIGNING FIELD-SYMBOL(<ls_tp_moeda>).

      ENDLOOP.

      LOOP AT gt_vlr_moeda ASSIGNING FIELD-SYMBOL(<ws_moeda>).

        "Check valor negativo BRL.
        CLEAR: vg_vlr.
        vg_vlr = <ws_moeda>-vlr_brl.
        gv_vlr_brl = <ws_moeda>-vlr_brl.

        IF vg_vlr < 0.
          REPLACE '-' IN gv_vlr_brl WITH ''.
          CONDENSE gv_vlr_brl NO-GAPS.

          gv_vlr_brl = |-{ gv_vlr_brl }|.

          <ws_moeda>-vlr_brl = gv_vlr_brl.

        ENDIF.

        "Check valor negativo USD.
        CLEAR: vg_vlr.
        vg_vlr     = <ws_moeda>-vlr_usd.
        gv_vlr_usd = <ws_moeda>-vlr_usd.

        IF vg_vlr < 0.
          REPLACE '-' IN gv_vlr_usd WITH ''.
          CONDENSE gv_vlr_usd NO-GAPS.

          gv_vlr_usd = |-{ gv_vlr_usd }|.

          <ws_moeda>-vlr_usd = gv_vlr_usd.

        ENDIF.


*        REPLACE '.' IN <ws_moeda>-vlr_brl WITH ','.
*        CONDENSE <ws_moeda>-vlr_brl NO-GAPS.
*
*        REPLACE '.' IN <ws_moeda>-vlr_usd WITH ','.
*        CONDENSE <ws_moeda>-vlr_usd NO-GAPS.

      ENDLOOP.

*      ls_fieldname-fieldname = 'Empresa'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Ano'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Mês'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'NºConta'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Descrição-PT'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Descrição-EN'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Sociedade'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Valor BRL'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'D/C-BRL'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Valor USD'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'D/C-USD'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.
*      ls_fieldname-fieldname = 'Nivel'.
*      APPEND ls_fieldname TO lt_fieldname. CLEAR: ls_fieldname.

* RJF - Ini - 2023.04.25
      IF gv_automoeda IS NOT INITIAL.

*        CALL FUNCTION 'WS_FILENAME_GET'
*          EXPORTING
*            def_filename     = ' '
*            def_path         = 'C:\TEMP\'
*            mask             = ',*.CSV,'
*            mode             = 'S'
*            title            = 'Local de Gravação'
*          IMPORTING
*            filename         = path
*          EXCEPTIONS
*            inv_winsys       = 1
*            no_batch         = 2
*            selection_cancel = 3
*            selection_error  = 4
*            OTHERS           = 5.
*
*        IF sy-subrc IS INITIAL.

        path = 'C:\TEMP\'.

        CONCATENATE path 'ARQ_BALANC.CSV' INTO p_local.

        csv_converted_line = 'empresa;ano;Mês;NºConta;Descrição-PT;Descrição-EN;Sociedade;Valor BRL;D/C-BRL;Valor USD;D/C-USD;Nivel'.
        APPEND csv_converted_line TO csv_converted_table.
        SORT csv_converted_table DESCENDING.

        CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
          EXPORTING
            i_field_seperator    = ';'
          TABLES
            i_tab_sap_data       = gt_tp_moeda[]
          CHANGING
            i_tab_converted_data = csv_table
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.

        IF csv_table IS NOT INITIAL.
          APPEND LINES OF csv_table TO csv_converted_table.
        ENDIF.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename            = p_local
            filetype            = 'DAT'
          TABLES
            data_tab            = csv_converted_table
            fieldnames          = t_fieldnames
          EXCEPTIONS
            file_open_error     = 1
            file_write_error    = 2
            invalid_filesize    = 3
            invalid_table_width = 4
            invalid_type        = 5
            no_batch            = 6
            unknown_error       = 7
            OTHERS              = 8.
        IF sy-subrc <> 0.

* RJF - Ini - 2023.04.25
          FREE gs_file_name.
        ELSE.
          gs_file_name = p_local.
* RJF - Fim - 2023.04.25

*          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.
