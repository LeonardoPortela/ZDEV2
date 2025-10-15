*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 18/04/2022                                              &*
*& Descrição: Programa para batch input texto PC                      &*
*& Transação: ZMM0191                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zmmr176.

TYPES: BEGIN OF ty_resultado,
         flanguage   LIKE  t002-laiso,
         fname       LIKE  thead-tdname,
         tdline(132) TYPE c,
         resultado   TYPE char50,
       END OF ty_resultado.


TYPES: BEGIN OF ty_saida_texto,
         flanguage TYPE  t002-laiso,
         fname     TYPE  thead-tdname,
         tdline    TYPE  string,
         "tdline(132) TYPE c,
       END OF ty_saida_texto.

DATA:
  git_saida_texto TYPE TABLE OF ty_saida_texto  WITH HEADER LINE,
  git_resultado   TYPE TABLE OF ty_resultado.

DATA: gva_arq    LIKE rlgrap-filename,
      gva_answer TYPE c.

DATA: BEGIN OF ty_arquivo,
        flanguage   TYPE  t002-laiso,
        fname       TYPE  thead-tdname,
        tdline(132) TYPE c,
      END OF ty_arquivo.

DATA: git_filetable TYPE filetable,
      git_entrada   LIKE STANDARD TABLE OF ty_arquivo,
      s_matnr       TYPE thead-tdname.

DATA: git_raw TYPE truxs_t_text_data.
FIELD-SYMBOLS : <git_data>       TYPE STANDARD TABLE .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-003.
  PARAMETERS:pfile   TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.
  IF pfile IS INITIAL.
    MESSAGE s368(00) WITH 'Por favor Infome o Arquivo'. STOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pfile.
  PERFORM fm_f4_filename.

START-OF-SELECTION.
  PERFORM fm_processa_dados.

*&---------------------------------------------------------------------*
*&      Form  FM_F4_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_f4_filename .

  DATA: lva_rc        TYPE i,
        lit_tline     TYPE TABLE OF tline,
        lwa_filetable TYPE LINE OF filetable.

  CLEAR: lwa_filetable.

  DATA: lva_retur TYPE c.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Selecione o arquivo para Upload'
      default_extension       = 'XLSX'
*     default_filename        =
      file_filter             = 'Arquivos do Execel (*.XLS)|*.XLS| Excel files (*.XLSX)|*.XLSX|'
*     with_encoding           =
*     initial_directory       =
*     multiselection          =
    CHANGING
      file_table              = git_filetable
      rc                      = lva_rc
*     user_action             =
*     file_encoding           =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE git_filetable INTO lwa_filetable INDEX 1.
    pfile = lwa_filetable-filename.
  ENDIF.
ENDFORM.                    " F4_FILENAME

*&---------------------------------------------------------------------*
*&      Form  FM_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_processa_dados .

  DATA: lwa_type          TYPE c,
        lwa_values        TYPE i,
        lva_ext(4)        TYPE c,
        lva_filename      TYPE string,
        lva_filelength    TYPE i,
        lva_headerxstring TYPE xstring,
        lit_records       TYPE solix_tab.

  IF pfile IS NOT INITIAL.
    lva_filename =  pfile.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = lva_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = lva_filelength
        header                  = lva_headerxstring
      TABLES
        data_tab                = lit_records
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lva_filelength
        IMPORTING
          buffer       = lva_headerxstring
        TABLES
          binary_tab   = lit_records
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.


      DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

      TRY .
          lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
            document_name = lva_filename
            xdocument     = lva_headerxstring ).
        CATCH cx_fdt_excel_core.
          MESSAGE i000(z01) WITH 'Dados não encontrados!'.
          RETURN.
      ENDTRY .

      lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING
          worksheet_names = DATA(lit_worksheets) ).

      IF NOT lit_worksheets IS INITIAL.
        READ TABLE lit_worksheets INTO DATA(lwa_woksheetname) INDEX 1.

        DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
          lwa_woksheetname ).

        ASSIGN lo_data_ref->* TO <git_data>.
      ENDIF.


      IF <git_data> IS ASSIGNED.
        PERFORM fm_proc_results.
      ELSE.
        MESSAGE i000(z01) WITH 'Dados não encontrados!'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_PROC_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_proc_results .

  DATA: s_matnr    TYPE thead-tdname.
  DATA: lva_count_erro  TYPE i,
        lva_count_ok    TYPE i,
        lva_fname       TYPE thead-tdname,
        lva_matnr       TYPE mara-matnr,
        lva_matnr18     TYPE matnr18,
        lva_msg(132)    TYPE c,
        lva_count_txt   TYPE char05,
        lva_flanguage   TYPE thead-tdspras,
        lva_texto(4000) TYPE c,
        lit_string      TYPE TABLE OF char32 WITH HEADER LINE,
        lit_trtexts     TYPE trtexts.

  DATA: lit_tline    TYPE TABLE OF tline.
  DATA : lva_numberofcolumns   TYPE i.
  FIELD-SYMBOLS : <lwa_data>  TYPE any,
                  <lva_field> TYPE any.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação de Processamento'
      text_question         = 'Tem certeza que deseja alterar os textos informados?'
      text_button_1         = 'Sim'
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = gva_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF gva_answer = '1'.

    lva_numberofcolumns = 3 .

    LOOP AT <git_data> ASSIGNING <lwa_data>.

      DO lva_numberofcolumns TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <lwa_data> TO <lva_field> .
        IF sy-subrc = 0 .
          CASE sy-index .
            WHEN 1 .
              git_saida_texto-flanguage = <lva_field>.
            WHEN 2 .
              git_saida_texto-fname = <lva_field>.
            WHEN 3 .
              git_saida_texto-tdline = <lva_field>.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
      ENDDO .
      APPEND git_saida_texto.
      CLEAR git_saida_texto.
    ENDLOOP .

    DELETE  git_saida_texto WHERE tdline IS INITIAL.

    LOOP AT git_saida_texto INTO DATA(lwa_saida_texto).
      IF lwa_saida_texto-flanguage <> 'PT' AND
         lwa_saida_texto-flanguage <> 'EN' AND
         lwa_saida_texto-flanguage <> 'ES' .

        DATA(lwa_resultado) = VALUE ty_resultado(
          flanguage = lwa_saida_texto-flanguage
          fname     = lwa_saida_texto-fname
          tdline    = lwa_saida_texto-tdline
          resultado           = 'Idioma Inválido!' ).

        APPEND lwa_resultado TO git_resultado[].
        CLEAR: lwa_resultado.

        lva_count_erro = lva_count_erro + 1.


      ELSE.

        CLEAR: lva_matnr,lva_matnr18.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_saida_texto-fname
          IMPORTING
            output = lva_matnr18.

        lva_matnr = lva_matnr18.

        SELECT SINGLE *
          FROM mara INTO @DATA(lwa_mara)
         WHERE matnr EQ @lva_matnr.

        IF lwa_mara IS INITIAL.

          lwa_resultado = VALUE ty_resultado(
                       flanguage = lwa_saida_texto-flanguage
                       fname     = lwa_saida_texto-fname
                       tdline    = lwa_saida_texto-tdline
                       resultado           = 'Material não existe no cadastro!' ).

          APPEND lwa_resultado TO git_resultado[].
          CLEAR: lwa_resultado, lwa_mara.

          lva_count_erro = lva_count_erro + 1.

        ELSE.
***** Faz a Carga de Dados.

          SELECT SINGLE *
            FROM t002 INTO @DATA(lwa_t002)
           WHERE laiso EQ @lwa_saida_texto-flanguage.

          lva_flanguage = lwa_t002-spras.

          FREE lit_tline.


          CLEAR: lit_string[], lva_texto, lit_tline, lit_tline[], lit_trtexts[].

          lva_texto = lwa_saida_texto-tdline.

          CALL FUNCTION 'TR_SPLIT_TEXT'
            EXPORTING
              iv_text  = lva_texto
              iv_len   = 72
            IMPORTING
              et_lines = lit_trtexts.

          LOOP AT lit_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).
            APPEND  VALUE #( tdformat = 'ST'
                       tdline   = <fs_line> ) TO lit_tline.
          ENDLOOP.

*            APPEND  VALUE #( tdformat = 'ST'
*                       tdline   = lwa_saida_texto-tdline ) TO lit_tline.

          CLEAR: s_matnr, lva_matnr.
          lva_matnr = lwa_saida_texto-fname.
*          lva_matnr = |{ lva_matnr ALPHA = IN }|.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_saida_texto-fname
            IMPORTING
              output = lva_matnr18.

          lva_matnr = lva_matnr18.
          s_matnr = lva_matnr.

          "Alterar o texto
          CALL FUNCTION 'CREATE_TEXT'
            EXPORTING
              fid         = 'BEST'          " Text ID of the text to be created
              flanguage   = lva_flanguage  " Language of the text to be created
              fname       = s_matnr         " Name of the text to be created
              fobject     = 'MATERIAL'      " Object of the text to be created
              save_direct = 'X'
*             fformat     = '*'
            TABLES
              flines      = lit_tline   " Lines of the text to be created
            EXCEPTIONS
              no_init     = 1
              no_save     = 2
              OTHERS      = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

            CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lva_msg SEPARATED BY space.

            lwa_resultado = VALUE ty_resultado(
              flanguage = lwa_saida_texto-flanguage
              fname     = lwa_saida_texto-fname
              tdline    = lwa_saida_texto-tdline
              resultado           = lva_msg ).

            APPEND lwa_resultado TO git_resultado[].
            CLEAR: lwa_resultado, lva_msg.

            lva_count_erro = lva_count_erro + 1.

          ELSE.

            lwa_resultado = VALUE ty_resultado(
                      flanguage = lwa_saida_texto-flanguage
                      fname     = lwa_saida_texto-fname
                      tdline    = lwa_saida_texto-tdline
                      resultado  = 'Alterado com Sucesso!' ).
            APPEND lwa_resultado TO git_resultado[].
            CLEAR: lwa_resultado.
            lva_count_ok = lva_count_ok + 1.

          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: lwa_t002, lva_flanguage.
    ENDLOOP.

    IF ( git_resultado[] IS NOT INITIAL ).

      IF lva_count_ok IS NOT INITIAL.

        MOVE lva_count_ok TO lva_count_txt.

        CONDENSE lva_count_txt NO-GAPS.
        CONCATENATE lva_count_txt '-' 'Registros Processados com Sucesso!' INTO lva_msg SEPARATED BY space.

        lwa_resultado = VALUE ty_resultado(
            "fname     = lva_fname
            resultado = lva_msg ).

        APPEND lwa_resultado TO git_resultado[].
        CLEAR: lwa_resultado, lva_msg, lva_count_txt.
      ENDIF.

      IF lva_count_erro IS NOT INITIAL.
        MOVE lva_count_erro TO lva_count_txt.

        CONDENSE lva_count_txt NO-GAPS.
        CONCATENATE lva_count_txt '-' 'Registros Processados com Erro!' INTO lva_msg SEPARATED BY space.

        lwa_resultado = VALUE ty_resultado(
            "fname     = lva_count_erro
            resultado           = lva_msg ).
        APPEND lwa_resultado TO git_resultado[].
        CLEAR: lwa_resultado, lva_msg,  lva_count_txt.
      ENDIF.

      SORT git_resultado BY flanguage fname ASCENDING.

      cl_demo_output=>new(
        )->begin_section( `Resultado do processamento:`
        )->write_text( |Textos alterados. \n|
        )->write_data( git_resultado[]
        )->end_section(
        )->display( ).

    ENDIF.
  ELSE.
    MESSAGE 'Operação Cancelada pelo Usuário!' TYPE 'I'.
    EXIT.
  ENDIF.
ENDFORM.
