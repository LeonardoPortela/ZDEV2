*----------------------------------------------------------------------*
***INCLUDE LZSMARTFORMSO01.
*----------------------------------------------------------------------*

DATA: pdf_html_control TYPE REF TO cl_gui_html_viewer.
DATA: g_html_container TYPE REF TO cl_gui_custom_container. "*-CS2019001753-29.06.2023-#65723-JT-inicio

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TL0300'.

  IF pdf_html_control IS INITIAL.

    CREATE OBJECT pdf_html_control
      EXPORTING
        parent             = cl_gui_container=>screen0
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

    CALL METHOD pdf_html_control->load_data
      EXPORTING
        url          = 'smart.pdf'
        size         = pdf_size
        type         = 'application'
        subtype      = 'pdf'
      IMPORTING
        assigned_url = l_url
      CHANGING
        data_table   = l_pdf_data
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

* show data
    CALL METHOD pdf_html_control->show_data
      EXPORTING
        url    = l_url
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

  ENDIF.

ENDMODULE.

MODULE status_0301 OUTPUT.

  DATA: t_fcode TYPE TABLE OF sy-ucomm.

*-#149060-19.08.2024-JT-inicio
  LOOP AT SCREEN.
    IF screen-name = 'ASSINAR'.
      IF g_doc_simulacao IS INITIAL OR g_doc_simulacao = '000000000000000'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.

    IF screen-name = 'SALVAR'.
      IF g_salvar IS INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*-#149060-19.08.2024-JT-fim

  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TL0300'.

  CREATE OBJECT g_html_container   "*-CS2019001753-29.06.2023-#65723-JT-inicio
    EXPORTING
      container_name = 'PDF'.

  IF pdf_html_control IS INITIAL.

    CREATE OBJECT pdf_html_control
      EXPORTING
*       parent             = cl_gui_container=>screen0
        parent             = g_html_container                       "*-CS2019001753-29.06.2023-#65723-JT-inicio
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

    CALL METHOD pdf_html_control->load_data
      EXPORTING
        url          = 'smart.pdf'
        size         = pdf_size
        type         = 'application'
        subtype      = 'pdf'
      IMPORTING
        assigned_url = l_url
      CHANGING
        data_table   = l_pdf_data
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

* show data
    CALL METHOD pdf_html_control->show_data
      EXPORTING
        url    = l_url
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cntl_error.
    ENDIF.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300_exit INPUT.

  IF pdf_html_control IS NOT INITIAL.
    pdf_html_control->close_document(
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2
    ).

    pdf_html_control->free( ).
  ENDIF.

  CLEAR: pdf_html_control.
  LEAVE TO SCREEN 0.

ENDMODULE.

MODULE user_command_0301_exit INPUT.

*  IF pdf_html_control IS NOT INITIAL.
*    pdf_html_control->close_document(
*      EXCEPTIONS
*        cntl_error = 1
*        OTHERS     = 2
*    ).
*
*    pdf_html_control->free( ).
*
*    CALL METHOD g_html_container->free.
*
*  ENDIF.
*
*  CLEAR: pdf_html_control.
*  LEAVE TO SCREEN 0.

ENDMODULE.

MODULE user_command_0301 INPUT.

  DATA: l_abandona TYPE char01.

  CASE ok_code.

*-#149060-19.08.2024-JT-inicio
    WHEN 'SALVAR'.
      PERFORM f_salvar CHANGING l_abandona.

      IF l_abandona = abap_false.
        IF pdf_html_control IS NOT INITIAL.
          pdf_html_control->close_document(
            EXCEPTIONS
              cntl_error = 1
              OTHERS     = 2
          ).

          pdf_html_control->free( ).

          CALL METHOD g_html_container->free.
        ENDIF.

        CLEAR: pdf_html_control.
        LEAVE TO SCREEN 0.
      ENDIF.
*-#149060-19.08.2024-JT-fim

*-CS2019001753-29.06.2023-#65723-JT-inicio
    WHEN 'ENVIA'.

      SELECT SINGLE *
        INTO @DATA(w_0310)
        FROM zsdt0310
       WHERE id_documento = @g_doc_simulacao.

      IF ( sy-subrc = 0 ) AND ( ( w_0310-status           = '02'    OR
                                  w_0310-status           = '12' ) AND
                                  w_0310-tipo_doc_digital = 'S' ).

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 50
            text       = 'Aguarde...Enviando...'.

*-----------------------------------------
*------ ajusta status documento
*-----------------------------------------
        UPDATE zsdt0310 SET status       = '03'
                      WHERE id_documento = w_0310-id_documento.

        COMMIT WORK AND WAIT.

*-----------------------------------------
*------ solicitar assinatura Bry
*-----------------------------------------
        DATA(l_task) = 'SOLICITAR_ASSINATURA' && w_0310-nr_venda && w_0310-tipo_doc.

        CALL FUNCTION 'ZSD_INSUMOS_ASSINATURA_BRY' STARTING NEW TASK l_task
          EXPORTING
            i_doc_simulacao = w_0310-nr_venda
            i_tipo_doc      = w_0310-tipo_doc
            i_id_documento  = w_0310-id_documento.
      ENDIF.

      IF pdf_html_control IS NOT INITIAL.
        pdf_html_control->close_document(
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2
        ).

        pdf_html_control->free( ).

        CALL METHOD g_html_container->free.

      ENDIF.

      CLEAR: pdf_html_control.
      LEAVE TO SCREEN 0.
*-CS2019001753-29.06.2023-#65723-JT-fim

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      IF pdf_html_control IS NOT INITIAL.
        pdf_html_control->close_document(
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2
        ).

        pdf_html_control->free( ).

        CALL METHOD g_html_container->free.

      ENDIF.

      CLEAR: pdf_html_control.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

***********************************************************
* salvar documento
***********************************************************
FORM f_salvar CHANGING p_abandona.

  DATA: l_fullpath     TYPE string,
        l_filename     TYPE string,
        l_path         TYPE string,
        l_default_name TYPE string,
        l_length       TYPE i,
        lt_doc_content TYPE STANDARD TABLE OF soli-line,
        ls_doc_content LIKE LINE OF lt_doc_content.

  p_abandona = abap_false.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer          = pdf_data
      append_to_table = ' '
    IMPORTING
      output_length   = l_length
    TABLES
      binary_tab      = lt_doc_content.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Enter File Name'
      default_extension = 'PDF'
      default_file_name = l_default_name
    CHANGING
      filename          = l_filename             "Filename
      path              = l_path                 " path without file name
      fullpath          = l_fullpath.            " Full path including file name

  IF l_fullpath IS NOT INITIAL.
    cl_gui_frontend_services=>gui_download(
       EXPORTING
         bin_filesize = l_length
         filename     = l_fullpath
         filetype     = 'BIN'
       CHANGING
         data_tab     = lt_doc_content
       EXCEPTIONS
         OTHERS       = 1
    ).
  ELSE.
    p_abandona = abap_true.
  ENDIF.

ENDFORM.
***********************************************************
***********************************************************
