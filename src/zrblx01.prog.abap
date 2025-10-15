*&---------------------------------------------------------------------*
*& Report  ZRBLX01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrblx01.
TABLES e070.
TYPES tp_linha(500) TYPE c.

DATA gt_arq TYPE TABLE OF tp_linha WITH HEADER LINE.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gv_copia TYPE trkorr.

CLASS zcl_tester DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_file TYPE string.
    METHODS load RETURNING VALUE(rv_erro) TYPE int4.
    METHODS run RETURNING VALUE(rv_erro) TYPE int4.
  PROTECTED SECTION.
    METHODS run_test IMPORTING iv_tabela TYPE string  iv_filtro TYPE string  iv_exibicao TYPE string RETURNING VALUE(rv_erro) TYPE int4.
  PRIVATE SECTION.
    DATA go_xml_doc TYPE REF TO cl_xml_document.
    DATA gv_file TYPE string.
    DATA gv_xml TYPE string.
    DATA gt_xml_tab TYPE TABLE OF tab_string.

ENDCLASS.
CLASS zcl_tester IMPLEMENTATION.
  METHOD constructor.

    CREATE OBJECT go_xml_doc.

    gv_file = iv_file.

  ENDMETHOD.
  METHOD load.

    rv_erro = 4.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename = gv_file
      TABLES
        data_tab = gt_xml_tab.

    CHECK gt_xml_tab IS NOT INITIAL.

    CLEAR gv_xml.

    LOOP AT gt_xml_tab ASSIGNING FIELD-SYMBOL(<fs_xml>).

      gv_xml = gv_xml && <fs_xml>.

    ENDLOOP.

    REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN gv_xml WITH space.

    rv_erro = 0.

    rv_erro = go_xml_doc->parse_string( stream = gv_xml ).

  ENDMETHOD.
  METHOD run.

    TRY .

        DATA(lo_node) = go_xml_doc->find_node( 'tabela' ).

        WHILE lo_node IS BOUND.

          DATA(lv_tabela) = lo_node->get_attributes( )->get_item( 0 )->get_value( ).
          DATA(lv_filtro) = lo_node->get_children( )->get_item( 0 )->get_value( ).
          DATA(lv_exibicao) = lo_node->get_children( )->get_item( 1 )->get_value( ).

          rv_erro = me->run_test( iv_tabela = lv_tabela iv_filtro = lv_filtro iv_exibicao = lv_exibicao ).

          lo_node = lo_node->get_next( ).

        ENDWHILE.

      CATCH cx_sy_ref_is_initial.

        MESSAGE s016(ds) WITH 'Problemas com o XML' DISPLAY LIKE 'E'.

        rv_erro = 4.

    ENDTRY.

  ENDMETHOD.
  METHOD run_test.

    DATA lv_table TYPE se16n_tab.
    DATA lt_output_fields TYPE TABLE OF se16n_output.
    DATA lr_selfields TYPE TABLE OF se16n_seltab.
    DATA ls_field LIKE LINE OF lr_selfields.
    DATA lt_values TYPE TABLE OF char200.

    CHECK iv_tabela IS NOT INITIAL.

    lv_table = iv_tabela.

    IF iv_filtro IS NOT INITIAL.

      SPLIT iv_filtro AT ';' INTO TABLE lt_values.

      LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<fs_values>).

        SPLIT <fs_values> AT ':' INTO DATA(lv_value1) DATA(lv_value2).

        ls_field-sign = 'I'.
        ls_field-option = 'EQ'.
        ls_field-field = lv_value1.
        ls_field-low = lv_value2.
        APPEND ls_field TO lr_selfields.

      ENDLOOP.

    ENDIF.

    IF iv_exibicao IS NOT INITIAL.

      SPLIT iv_exibicao AT ';' INTO TABLE lt_output_fields.

    ENDIF.

    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING
        i_tab            = lv_table
        i_edit           = 'X'
        i_sapedit        = 'X'
        i_tech_names     = 'X'
      TABLES
        it_selfields     = lr_selfields
        it_output_fields = lt_output_fields
      EXCEPTIONS
        no_values        = 1
        OTHERS           = 2.

    rv_erro = sy-subrc.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_teste_prog DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_progname TYPE progname iv_filelocal TYPE string iv_tolerance TYPE i.
    METHODS existence_check RETURNING VALUE(rv_safe) TYPE boolean.
    METHODS set_tolerance IMPORTING iv_tolerance TYPE i.
    METHODS tolerance_check RETURNING VALUE(rv_ok) TYPE boolean.
    METHODS write_report RETURNING VALUE(rv_ok) TYPE boolean.
  PROTECTED SECTION.
    METHODS import_program RETURNING VALUE(rv_imported) TYPE boolean.
  PRIVATE SECTION.
    DATA gv_exist TYPE flag.
    DATA gv_tolerence TYPE i.
    DATA gv_progname TYPE progname.
    DATA gv_file TYPE string.
    DATA gt_progfile TYPE TABLE OF tab_string.
    DATA gt_proglines TYPE TABLE OF thllines.

ENDCLASS.

CLASS zcl_teste_prog IMPLEMENTATION.
  METHOD constructor.

    gv_progname = iv_progname.
    gv_file = iv_filelocal.
    gv_tolerence = iv_tolerance.

    IF gv_tolerence IS INITIAL.
      gv_tolerence = 20.
    ENDIF.

  ENDMETHOD.

  METHOD tolerance_check.

    DATA lt_auxprog TYPE TABLE OF thllines.
    DATA lt_localprog TYPE TABLE OF thllines.
    DATA lt_globprog TYPE TABLE OF thllines.

    rv_ok = abap_false.

    CHECK existence_check( ) = abap_true.

    IF gt_progfile IS INITIAL.
      import_program( ).
    ENDIF.

    CHECK gt_proglines IS NOT INITIAL.

    LOOP AT gt_proglines ASSIGNING FIELD-SYMBOL(<fs_prog1>).

      CHECK <fs_prog1>-thline IS NOT INITIAL.
      CHECK <fs_prog1>-thline(1) NE '*'.
      CHECK <fs_prog1>-thline(1) NE '"'.

      APPEND <fs_prog1> TO lt_globprog.

    ENDLOOP.

    READ REPORT gv_progname INTO lt_auxprog[].

    LOOP AT lt_auxprog ASSIGNING FIELD-SYMBOL(<fs_prog2>).

      CHECK <fs_prog2>-thline IS NOT INITIAL.
      CHECK <fs_prog2>-thline(1) NE '*'.
      CHECK <fs_prog2>-thline(1) NE '"'.

      APPEND <fs_prog2> TO lt_localprog.

    ENDLOOP.

    DATA(lv_cont) = 0.
    DATA(lv_index) = 0.

    LOOP AT lt_localprog ASSIGNING FIELD-SYMBOL(<fs_lines2>).

      ADD 1 TO lv_index.

      READ TABLE lt_globprog[] ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX lv_index.

      CHECK sy-subrc EQ 0.

      DATA(lv_line1) = <fs_line>-thline.
      DATA(lv_line2) = <fs_lines2>-thline.

      TRANSLATE lv_line1 TO LOWER CASE.
      TRANSLATE lv_line2 TO LOWER CASE.

      IF lv_line1 = lv_line2.
        ADD 1 TO lv_cont.
      ENDIF.

    ENDLOOP.

    IF lv_cont < gv_tolerence.
      RETURN.
    ELSE.
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD existence_check.

    IF gv_exist = abap_false.

      rv_safe = abap_false.

      SELECT COUNT(*) FROM trdir WHERE name = gv_progname.

      " 23.01.2023 -->
      IF sy-subrc NE 0.

        DATA lv_func TYPE rs38l_fnam.

        lv_func = gv_progname.

        CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
          CHANGING
            funcname = lv_func
            include  = gv_progname.

      ENDIF.
      " 23.01.2023 --<

      CHECK sy-dbcnt > 0.

    ENDIF.

    rv_safe = abap_true.

    gv_exist = abap_true.

  ENDMETHOD.

  METHOD set_tolerance.

    gv_tolerence = iv_tolerance.

  ENDMETHOD.

  METHOD import_program.

    rv_imported = abap_false.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename = gv_file
      TABLES
        data_tab = gt_progfile.

    CHECK gt_progfile IS NOT INITIAL.

    LOOP AT gt_progfile ASSIGNING FIELD-SYMBOL(<fs_progfile>).

      APPEND INITIAL LINE TO gt_proglines ASSIGNING FIELD-SYMBOL(<fs_progline>).
      <fs_progline>-thline = <fs_progfile>.

    ENDLOOP.

    rv_imported = abap_true.

  ENDMETHOD.

  METHOD write_report.

    CHECK existence_check( ) = abap_true.

    CHECK tolerance_check( ) = abap_true.

    INSERT REPORT gv_progname FROM gt_proglines.

    rv_ok = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_teste_desc DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_obj,

             trtype TYPE trobjtype,
             trname TYPE trobj_name,
             descr  TYPE as4text,
           END OF ty_obj.

    METHODS constructor IMPORTING it_req TYPE trrngtrkor_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gt_objects TYPE TABLE OF ty_obj.
    DATA gt_reqs TYPE trrngtrkor_tab.

    METHODS get_objects RETURNING VALUE(rv_ok) TYPE boolean.

ENDCLASS.

CLASS zcl_teste_desc IMPLEMENTATION.
  METHOD constructor.
    gt_reqs = it_req.
  ENDMETHOD.
  METHOD get_objects.

  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_inser RADIOBUTTON GROUP rb01 USER-COMMAND ucom.
  PARAMETERS p_trans RADIOBUTTON GROUP rb01.
  PARAMETERS p_descr RADIOBUTTON GROUP rb01.
  PARAMETERS p_comme RADIOBUTTON GROUP rb01.
  PARAMETERS p_layo  RADIOBUTTON GROUP rb01.
  PARAMETERS p_se93 RADIOBUTTON GROUP rb01.
  PARAMETERS p_adde RADIOBUTTON GROUP rb01.
  PARAMETERS p_ulti RADIOBUTTON GROUP rb01.
  PARAMETERS p_test RADIOBUTTON GROUP rb01.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b001 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_arq TYPE rescfilename.
  PARAMETERS p_prog TYPE progname.
  PARAMETERS p_tol TYPE i DEFAULT 20.
SELECTION-SCREEN END OF BLOCK b001.

SELECTION-SCREEN BEGIN OF BLOCK b002 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_rqst TYPE e070-trkorr.
  PARAMETERS p_tar  TYPE tr_target.
  PARAMETERS p_info AS CHECKBOX.
  PARAMETERS p_lib  AS CHECKBOX DEFAULT 'X'.
  PARAMETERS p_qas  TYPE rfcdest.
  PARAMETERS p_sys  TYPE tmssysnam.
  PARAMETERS p_cli  TYPE sy-mandt.
SELECTION-SCREEN END OF BLOCK b002.

SELECTION-SCREEN BEGIN OF BLOCK b003 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS so_req FOR e070-trkorr.
  PARAMETERS p_down TYPE rescfilename.
SELECTION-SCREEN END OF BLOCK b003.

SELECTION-SCREEN BEGIN OF BLOCK b004 WITH FRAME TITLE TEXT-004.
  PARAMETERS p_txt_cr TYPE rescfilename DEFAULT 'CR SOLICITADA POR EMAIL'.
SELECTION-SCREEN END OF BLOCK b004.

SELECTION-SCREEN BEGIN OF BLOCK b005 WITH FRAME TITLE TEXT-005.
  PARAMETERS p_layf TYPE tabname.
SELECTION-SCREEN END OF BLOCK b005.

SELECTION-SCREEN BEGIN OF BLOCK b006 WITH FRAME TITLE TEXT-006.
  PARAMETERS p_tcode TYPE tcode.
SELECTION-SCREEN END OF BLOCK b006.

SELECTION-SCREEN BEGIN OF BLOCK b007 WITH FRAME TITLE TEXT-007.
  PARAMETERS p_reque TYPE e070-trkorr.
SELECTION-SCREEN END OF BLOCK b007.

SELECTION-SCREEN BEGIN OF BLOCK b008 WITH FRAME TITLE TEXT-008.
  PARAMETERS p_ultpro TYPE rs38m-programm.
SELECTION-SCREEN END OF BLOCK b008.

SELECTION-SCREEN BEGIN OF BLOCK b009 WITH FRAME TITLE TEXT-009.
  PARAMETERS p_tstf TYPE rescfilename.
SELECTION-SCREEN END OF BLOCK b009.

INITIALIZATION.
  PERFORM f_desativa_bloco USING: 2,3,4,5,6,7,8,9.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_control_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.
  PERFORM f4_arquivo CHANGING p_arq.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tstf.
  PERFORM f4_arquivo CHANGING p_tstf.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_down.
  PERFORM f4_arquivo CHANGING p_down.

START-OF-SELECTION.

  CASE 'X'.
    WHEN p_inser.
      PERFORM f_insere_prog.
    WHEN p_trans.
      PERFORM f_criar_request.
      IF p_qas IS NOT INITIAL AND p_sys IS NOT INITIAL.
        PERFORM f_importar_request.
      ENDIF.

    WHEN p_descr.
      PERFORM f_descri_obj.
    WHEN p_comme.
      PERFORM f_generate_comment.

    WHEN p_layo.
      PERFORM f_chama_btn_layout.

    WHEN p_se93.
      PERFORM f_get_program.

    WHEN p_adde.
      PERFORM f_add_abap.

    WHEN p_ulti.
      PERFORM f_get_ult_progr.
    WHEN p_test.
      PERFORM f_test.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  F_CONTROL_SCREEN
*&---------------------------------------------------------------------*
FORM f_control_screen .

  CASE 'X'.
    WHEN p_inser.
      PERFORM f_desativa_bloco USING: 2,3,4,5,6,7,8,9.
    WHEN p_trans.
      PERFORM f_desativa_bloco USING: 1,3,4,5,6,7,8,9.
    WHEN p_descr.
      PERFORM f_desativa_bloco USING: 1,2,4,5,6,7,8,9.
    WHEN p_comme.
      PERFORM f_desativa_bloco USING: 1,2,3,5,6,7,8,9.
    WHEN p_layo.
      PERFORM f_desativa_bloco USING: 1,2,3,4,6,7,8,9.
    WHEN p_se93.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,7,8,9.
    WHEN p_adde.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,6,8,9.
    WHEN p_ulti.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,6,7,9.
    WHEN p_test.
      PERFORM f_desativa_bloco USING: 1,2,3,4,5,6,7,8.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESATIVA_BLOCO
*&---------------------------------------------------------------------*
FORM f_desativa_bloco USING p_bloco TYPE i.

  CASE p_bloco.
    WHEN 1.
      LOOP AT SCREEN.
        IF screen-name CS 'P_ARQ'
        OR screen-name CS 'P_PROG'
        OR screen-name CS 'P_TOL'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN 2.

      LOOP AT SCREEN.

        IF screen-name CS 'P_RQST'
        OR screen-name CS 'P_TAR'
        OR screen-name CS 'P_INFO'
        OR screen-name CS 'P_LIB'
        OR screen-name CS 'P_QAS'
        OR screen-name CS 'P_SYS'
        OR screen-name CS 'P_CLI'.

          screen-active = 0.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

    WHEN 3.

      LOOP AT SCREEN.

        IF screen-name CS 'SO_REQ' OR screen-name CS 'P_DOWN'.

          screen-active = 0.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.


    WHEN 4.
      LOOP AT SCREEN.

        IF screen-name CS 'P_TXT_CR'.

          screen-active = 0.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

    WHEN 5.

      LOOP AT SCREEN.

        IF screen-name CS 'P_LAYF'.

          screen-active = 0.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

    WHEN 6.

      LOOP AT SCREEN.

        IF screen-name CS 'P_TCODE'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    WHEN 7.

      LOOP AT SCREEN.

        IF screen-name CS 'P_REQUE'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    WHEN 8.

      LOOP AT SCREEN.

        IF screen-name CS 'P_ULTPRO'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    WHEN 9.

      LOOP AT SCREEN.

        IF screen-name CS 'P_TSTF'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INSERE_PROG
*&---------------------------------------------------------------------*
FORM f_insere_prog .

  DATA lo_prog TYPE REF TO zcl_teste_prog.
  DATA lv_arq TYPE string.

  IF p_arq IS INITIAL OR p_prog IS INITIAL.
    PERFORM f_preencher_obr.
    EXIT.
  ENDIF.

  lv_arq = p_arq.

  CREATE OBJECT lo_prog
    EXPORTING
      iv_filelocal = lv_arq
      iv_progname  = p_prog
      iv_tolerance = p_tol.

  CHECK lo_prog IS BOUND.

  IF lo_prog->write_report( ) = abap_false.
    MESSAGE s666(01) WITH 'Erro ao importar' DISPLAY LIKE 'E'.
  ELSE.
    SET PARAMETER ID 'RID' FIELD p_prog.
    CALL TRANSACTION 'SE38' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_OBR
*&---------------------------------------------------------------------*
FORM f_preencher_obr .
  MESSAGE s666(01) WITH 'Preencher campos obrigatorios' DISPLAY LIKE 'E'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f4_arquivo
*&---------------------------------------------------------------------*
FORM f4_arquivo  CHANGING p_file.

  DATA l_file TYPE ibipparms-path.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = l_file.

  MOVE l_file TO p_file.

ENDFORM.                                                    "f4_arquivo
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& Form f_criar_request
*&---------------------------------------------------------------------*
FORM f_criar_request.


  DATA lv_exit.

  IF p_rqst IS INITIAL.
    PERFORM f_preencher_obr.
    EXIT.
  ENDIF.

  CLEAR gv_copia.

  SELECT SINGLE as4text FROM e07t
    INTO @DATA(lv_text)
      WHERE trkorr = @p_rqst
        AND langu = @sy-langu.

  CHECK sy-subrc EQ 0.

  IF p_info = 'X'.
    lv_text = 'TC(' && p_rqst && ')' && lv_text.
  ENDIF.

  " CRIA COPIA
  CALL FUNCTION 'IW_C_CREATE_TRANSPORT_REQUEST'
    EXPORTING
      description            = lv_text
      transport_kind         = 'T'
      langu                  = sy-langu
    IMPORTING
      commfile               = gv_copia
    EXCEPTIONS
      language_missing       = 1
      number_range_full      = 2
      unallowed_trfunction   = 3
      no_authorization       = 4
      create_transport_error = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema_insere.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM e070
    INTO @DATA(lw_e070_cp)
      WHERE trkorr = @gv_copia.

  CHECK sy-subrc EQ 0.

  lw_e070_cp-tarsystem = p_tar.

  SELECT SINGLE * FROM e07t
    INTO @DATA(lw_e07t_cp)
    WHERE trkorr = @gv_copia
      AND langu = @sy-langu.

  CHECK sy-subrc EQ 0.

  " ATUALIZA DESTINO
  CALL FUNCTION 'TRINT_UPDATE_COMM_HEADER'
    EXPORTING
      wi_e070            = lw_e070_cp
      wi_e07t            = lw_e07t_cp
*     wi_e070c           = ' '
*     is_e070m           = ' '
*     wi_save_user       = ' '
      wi_user            = sy-uname
      wi_sel_e070        = 'X'
      wi_sel_e07t        = 'X'
      wi_sel_e070c       = ' '
      iv_sel_e070m       = ' '
    EXCEPTIONS
      e070_update_error  = 1
      e07t_update_error  = 2
      e070c_update_error = 3
      e070m_update_error = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema_insere.
    EXIT.
  ENDIF.

  SELECT * FROM e070
    INTO TABLE @DATA(lt_070)
      WHERE strkorr = @p_rqst.

  " ------>
  IF sy-subrc NE 0.

    SELECT * FROM e070
      INTO TABLE lt_070
        WHERE trkorr = p_rqst.

  ENDIF.

  IF sy-subrc NE 0.

    PERFORM f_mensagem_insere
      TABLES gt_bapiret2
    USING 'E' 'DS' '016' 'Não foi possivel' 'localizar algo para' p_rqst space.

    EXIT.

  ENDIF.
  " ------<

  LOOP AT lt_070 ASSIGNING FIELD-SYMBOL(<fs_070>).

    " COPIA OBJETOS
    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ' '
        wi_trkorr_from           = <fs_070>-trkorr
        wi_trkorr_to             = gv_copia
        wi_without_documentation = 'X'
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11
        OTHERS                   = 12.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema_insere.
      EXIT.
    ENDIF.

  ENDLOOP.

  " COMPACTA
  CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
    EXPORTING
      iv_object  = gv_copia
      iv_type    = 'REQU'
      iv_command = 'SORT'
    IMPORTING
      ev_exit    = lv_exit.

  IF p_lib IS NOT INITIAL.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = gv_copia
        iv_dialog                   = ' '
        iv_as_background_job        = ' '
*       IV_SUCCESS_MESSAGE          = ' '
        iv_without_objects_check    = 'X'
        iv_called_by_adt            = 'X'
        iv_called_by_perforce       = 'X'
        iv_without_docu             = 'X'
        iv_without_locking          = 'X'
        iv_display_export_log       = 'X'
        iv_ignore_warnings          = 'X'
*       IV_SIMULATION               = ' '
*       IV_ATC_OPTIONS              = ' '
*   IMPORTING
*       ES_REQUEST                  =
*       ET_DELETED_TASKS            =
*       ET_MESSAGES                 =
*       EV_TR_LOCK_TST              =
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17
        OTHERS                      = 18.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema_insere.
      EXIT.
    ENDIF.

  ENDIF.

  PERFORM f_to_clipboard USING gv_copia.

  PERFORM f_mensagem_insere TABLES gt_bapiret2
    USING 'S' 'TK' '126' gv_copia space space space.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_importar_request
*&---------------------------------------------------------------------*
FORM f_importar_request.

  DATA lt_req TYPE TABLE OF cts_req.

  APPEND INITIAL LINE TO lt_req ASSIGNING FIELD-SYMBOL(<fs_req>).
  <fs_req>-request = gv_copia.

  CALL FUNCTION 'CTS_API_IMPORT_CHANGE_REQUEST' DESTINATION p_qas
    EXPORTING
      system   = p_sys
      client   = p_cli
    TABLES
      requests = lt_req.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE <fs_ret2>-type
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL OR <fs_ret2>-system <> sy-sysid.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
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
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
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
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_descri_obj
*&---------------------------------------------------------------------*
FORM f_descri_obj .

  DATA lr_req TYPE RANGE OF e070-trkorr.

  IF so_req[] IS INITIAL OR p_down IS INITIAL.
    PERFORM f_preencher_obr.
    EXIT.
  ENDIF.

  SELECT 'I' AS sign, 'EQ' AS option, trkorr AS low, trkorr AS high
    FROM e070
    INTO TABLE @DATA(lr_req2)
      WHERE strkorr IN @so_req.

  APPEND LINES OF lr_req2 TO lr_req.

  APPEND LINES OF so_req TO lr_req.

  SORT lr_req ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lr_req.

  SELECT e070~trkorr AS name,as4text AS text FROM e071
     INNER JOIN e070 ON e070~trkorr = e071~trkorr
     INNER JOIN e07t ON e07t~trkorr = e071~trkorr
     INTO TABLE @DATA(lt_req)
       WHERE e071~trkorr IN @lr_req.
  "AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' ).

  CHECK sy-subrc EQ 0.

  SORT lt_req.

  DELETE ADJACENT DUPLICATES FROM lt_req COMPARING name.

  SORT lt_req ASCENDING BY name.

  SELECT obj_name AS name,dd02t~ddtext AS text  FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN dd02t ON dd02t~tabname = e071~obj_name
    INTO TABLE @DATA(lt_tabs)
      WHERE e071~trkorr IN @lr_req
        ."AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' ).

  SORT lt_tabs.

  DELETE ADJACENT DUPLICATES FROM lt_tabs COMPARING name.

  SORT lt_tabs ASCENDING BY name.

  SELECT obj_name AS name,trdirt~text AS text  FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN trdirt ON trdirt~name = e071~obj_name
    INTO TABLE @DATA(lt_prog)
      WHERE e071~trkorr IN @lr_req
        ."AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' ).
  SORT lt_prog.

  SELECT obj_name AS name obj_name AS text  FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN reposrc ON reposrc~progname = e071~obj_name
    APPENDING TABLE lt_prog
      WHERE e071~trkorr IN lr_req
        ."AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' ).

  SORT lt_prog BY name.

  DELETE ADJACENT DUPLICATES FROM lt_prog COMPARING name.

  SELECT tftit~funcname AS name,tftit~stext AS text  FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN v_fdir ON v_fdir~funcname = e071~obj_name
    INNER JOIN tftit ON tftit~funcname = v_fdir~funcname
    INTO TABLE @DATA(lt_func)
      WHERE e071~trkorr IN @lr_req
       AND object = 'FUNC'
        ."AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' )



  SELECT tftit~funcname AS name tftit~stext AS text  FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN v_fdir ON v_fdir~area = e071~obj_name
    INNER JOIN tftit ON tftit~funcname = v_fdir~funcname
    APPENDING TABLE lt_func
      WHERE e071~trkorr IN lr_req
        AND object = 'FUGR'
        ."AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' )


  SORT lt_func BY name.

  DELETE ADJACENT DUPLICATES FROM lt_func COMPARING name.

  SELECT obj_name AS name,tstct~ttext AS text FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN tstct ON tstct~tcode = e071~obj_name
    INTO TABLE @DATA(lt_tcode)
      WHERE e071~trkorr IN @lr_req
        "AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' )
        AND object = 'TRAN'.

  SORT lt_tcode BY name.

  DELETE ADJACENT DUPLICATES FROM lt_tcode COMPARING name.

  SORT lt_tcode ASCENDING BY name.

  SELECT obj_name AS name, obj_name AS text FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INTO TABLE @DATA(lt_mess)
      WHERE e071~trkorr IN @lr_req
        "AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' )
        AND object = 'MESS'.

  SORT lt_mess ASCENDING BY name.

  DELETE ADJACENT DUPLICATES FROM lt_mess COMPARING name.

  SELECT obj_name AS name, stext AS text FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
  INNER JOIN cwbntstxt ON numm = e071~obj_name
    INTO TABLE @DATA(lt_note)
      WHERE e071~trkorr IN @lr_req
        "AND ( trfunction = 'K' OR trfunction = 'R' OR trfunction = 'S' )
        AND object = 'NOTE'.

  SORT lt_note ASCENDING BY name.

  DELETE ADJACENT DUPLICATES FROM lt_note COMPARING name.

  SELECT clsname AS name,seoclasstx~descript AS text FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN seoclasstx ON seoclasstx~clsname = e071~obj_name
    INTO TABLE @DATA(lt_class)
      WHERE e071~trkorr IN @lr_req
       AND object = 'CLAS'
       AND langu = @sy-langu.

  SELECT clsname AS name,seoclasstx~descript AS text FROM e071
   INNER JOIN e070 ON e070~trkorr = e071~trkorr
   INNER JOIN seoclasstx ON seoclasstx~clsname = e071~obj_name
   INTO TABLE @DATA(lt_intf)
     WHERE e071~trkorr IN @lr_req
      AND object = 'INTF'
      AND langu = @sy-langu
      AND objfunc <> 'D'.

  SELECT tnrot~object AS name, txtshort AS text FROM e071
   INNER JOIN e070 ON e070~trkorr = e071~trkorr
   INNER JOIN tnrot ON tnrot~object = e071~obj_name
   INTO TABLE @DATA(lt_snro)
     WHERE e071~trkorr IN @lr_req
      AND e071~object = 'NROB'
      AND langu = @sy-langu.

  SELECT obj_name AS name, obj_name AS text FROM e071
   INNER JOIN e070 ON e070~trkorr = e071~trkorr
   INTO TABLE @DATA(lt_index)
     WHERE e071~trkorr IN @lr_req
      AND e071~object = 'INDX'.

  SORT lt_index.
  DELETE ADJACENT DUPLICATES FROM lt_index COMPARING name.


  """""" cds ---->
  SELECT obj_name AS name, ddtext AS text FROM e071
    INNER JOIN e070 ON e070~trkorr = e071~trkorr
    INNER JOIN ddddlsrct ON ddlname = e071~obj_name
    INTO TABLE @DATA(lt_cds)
      WHERE e071~trkorr IN @lr_req
       AND object = 'DDLS'
       AND ddlanguage = @sy-langu.
  """""" cds ----<

  SORT lt_cds.

  DELETE ADJACENT DUPLICATES FROM lt_cds COMPARING name.

  DATA lw_dd12t TYPE dd12t.
  LOOP AT lt_index ASSIGNING FIELD-SYMBOL(<fs_index>).

    CONDENSE <fs_index>-text.

    REPLACE ` ` IN <fs_index>-text WITH ';'.

    SPLIT <fs_index>-text AT ';' INTO lw_dd12t-sqltab lw_dd12t-indexname.

    CONDENSE lw_dd12t-sqltab NO-GAPS.
    CONDENSE lw_dd12t-indexname NO-GAPS.

    SELECT SINGLE ddtext FROM dd12t
      INTO lw_dd12t-ddtext
        WHERE ddlanguage = sy-langu
          AND sqltab = lw_dd12t-sqltab
          AND indexname = lw_dd12t-indexname.

    CHECK sy-subrc EQ 0.

    <fs_index>-text = lw_dd12t-ddtext.

  ENDLOOP.

  LOOP AT lt_mess ASSIGNING FIELD-SYMBOL(<fs_mess>) WHERE name IS NOT INITIAL.

    SELECT SINGLE text FROM t100
      INTO @DATA(lv_text)
        WHERE arbgb = @<fs_mess>-name(3)
          AND msgnr = @<fs_mess>-name+3(3).

    CHECK sy-subrc EQ 0.

    <fs_mess>-text = lv_text.

  ENDLOOP.

  PERFORM f_monta_espec
    USING 'Requests:'
          lt_req.

  PERFORM f_enter.

  PERFORM f_monta_espec
    USING 'Transações:'
          lt_tcode.

  PERFORM f_enter.

  PERFORM f_monta_espec
  USING 'Tabelas:'
        lt_tabs.

  PERFORM f_enter.

  PERFORM f_monta_espec
  USING 'Indices:'
        lt_index.

  PERFORM f_enter.

  PERFORM f_monta_espec
  USING 'Programas:'
        lt_prog.

  PERFORM f_enter.

  PERFORM f_monta_espec
  USING 'Funções:'
        lt_func.

  PERFORM f_enter.

  " -->
  PERFORM f_monta_espec
  USING 'Interfaces:'
        lt_intf.

  PERFORM f_enter.

  PERFORM f_monta_espec
  USING 'Classes:'
        lt_class.

  PERFORM f_enter.

  PERFORM f_monta_espec
    USING 'Intervalo numeração (SNRO):'
          lt_snro.

  PERFORM f_enter.
  " -->

  PERFORM f_monta_espec
  USING 'Mensagens:'
        lt_mess.

  PERFORM f_enter.
  " -->

  PERFORM f_monta_cds
  USING 'CDS:'
        lt_cds.

  PERFORM f_monta_espec
    USING 'Notas SAP:'
          lt_note.

  DATA lv_string TYPE string.

  lv_string = p_down.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_string
    TABLES
      data_tab                = gt_arq[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ESPEC
*&---------------------------------------------------------------------*
FORM f_monta_espec  USING p_type TYPE c
                          p_tab TYPE ANY TABLE.

  FIELD-SYMBOLS <fs_name>.
  FIELD-SYMBOLS <fs_text>.

  APPEND p_type TO gt_arq.


  LOOP AT p_tab ASSIGNING FIELD-SYMBOL(<fs_teste>).

    ASSIGN ('<fs_teste>-name') TO <fs_name>.
    ASSIGN ('<fs_teste>-text') TO <fs_text>.

    APPEND <fs_name> && `: ` && <fs_text> TO gt_arq.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ESPEC
*&---------------------------------------------------------------------*
FORM f_monta_cds  USING p_type TYPE c
                          p_tab TYPE ANY TABLE.

  FIELD-SYMBOLS <fs_name>.
  FIELD-SYMBOLS <fs_text>.

  APPEND p_type TO gt_arq.


  LOOP AT p_tab ASSIGNING FIELD-SYMBOL(<fs_teste>).

    ASSIGN ('<fs_teste>-name') TO <fs_name>.
    ASSIGN ('<fs_teste>-text') TO <fs_text>.

    APPEND <fs_name> && `: ` && <fs_text> TO gt_arq.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENTER
*&---------------------------------------------------------------------*
FORM f_enter .

  APPEND '' TO gt_arq.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_generate_comment
*&---------------------------------------------------------------------*
FORM f_generate_comment .

  DATA lv_data_str TYPE c LENGTH 10.
  DATA lv_string TYPE c LENGTH 300.
  DATA lv_string2 TYPE c LENGTH 100.
  DATA lv_name TYPE c LENGTH 100.
  DATA: lw_addr LIKE  addr3_val.

  IF p_txt_cr IS INITIAL.
    PERFORM f_preencher_obr.
    EXIT.
  ENDIF.

  WRITE sy-datum TO lv_data_str.

  CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
    EXPORTING
      user_name              = sy-uname
    IMPORTING
      user_address           = lw_addr
    EXCEPTIONS
      user_address_not_found = 1
      OTHERS                 = 2.

  IF sy-subrc EQ 0.
    lv_name = lw_addr-name_text.
  ENDIF.

  IF lv_name IS INITIAL.
    lv_name = 'ABAP'.
  ENDIF.

  TRANSLATE lv_name TO UPPER CASE.

  lv_string2 = `" ` && lv_data_str && ` - ` && lv_name && ` - ` && p_txt_cr.

  lv_string = lv_string2 && ` -->` && cl_abap_char_utilities=>cr_lf && lv_string2 && ` <--`.

  PERFORM f_to_clipboard USING lv_string.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_to_clipboard
*&---------------------------------------------------------------------*
FORM f_to_clipboard USING p_string TYPE c.

  DATA lv_string TYPE c LENGTH 300.
  DATA lt_string LIKE TABLE OF lv_string.
  DATA lv_return TYPE i.

  APPEND p_string TO lt_string.

  CALL METHOD cl_gui_frontend_services=>clipboard_export
    EXPORTING
      no_auth_check        = space
    IMPORTING
      data                 = lt_string
    CHANGING
      rc                   = lv_return
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      no_authority         = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
    EXIT.
  ENDIF.

  MESSAGE s016(ds) WITH 'Copiado para o clipboard'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHAMA_BTN_IMPORTAR
*&---------------------------------------------------------------------*
FORM f_chama_btn_layout.

  DATA lv_file_title TYPE string.
  DATA lv_arq TYPE trfile.

  CHECK p_layf IS NOT INITIAL.

  lv_file_title = 'FILE_001.csv'.

  PERFORM f_f4help_local_file
    USING lv_file_title
 CHANGING lv_arq.

  CHECK lv_arq IS NOT INITIAL.

  PERFORM f_gera_layout_arq USING p_layf lv_arq.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_F4HELP_LOCAL_FILE
*&---------------------------------------------------------------------*
FORM f_f4help_local_file USING p_file_title TYPE string
                      CHANGING f_arq TYPE trfile.

  DATA: l_fullpath     TYPE string,
        l_path         TYPE string,
        l_filename     TYPE string,
        l_window_title TYPE string.

  l_window_title =  sy-repid.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = l_window_title
      prompt_on_overwrite  = 'X'
      default_file_name    = p_file_title
      file_filter          = '.csv'
    CHANGING
      filename             = l_filename
      path                 = l_path
      fullpath             = l_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  f_arq = l_fullpath.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERA_LAYOUT_ARQ
*&---------------------------------------------------------------------*
FORM f_gera_layout_arq USING p_estrutura TYPE dd02l-tabname
                             p_file TYPE trfile.

  DATA lt_fieldinfo TYPE TABLE OF fieldinfo.
  DATA lt_arq TYPE TABLE OF tp_linha.
  DATA lw_arq TYPE tp_linha.

  CALL FUNCTION 'DDIC_DATA_DESCRIPTION_SCAN'
    EXPORTING
      i_structure      = p_estrutura
    TABLES
      t_fieldinfo      = lt_fieldinfo
    EXCEPTIONS
      table_not_exists = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE lt_fieldinfo WHERE fieldname = 'ICON'
                         OR fieldname = 'MSGTX'
                         OR fieldname = 'MANDT'
                         OR fieldname = 'SELEC'.

  LOOP AT lt_fieldinfo ASSIGNING FIELD-SYMBOL(<fs_field>).
    lw_arq = lw_arq && ';' && <fs_field>-fieldname.
  ENDLOOP.

  lw_arq = lw_arq+1.

  APPEND lw_arq TO lt_arq.

  DATA lv_string TYPE string.

  lv_string = p_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_string
    TABLES
      data_tab                = lt_arq
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROGRAM
*&---------------------------------------------------------------------*
FORM f_get_program .

  DATA lv_prog TYPE program_id.

  IF p_tcode IS INITIAL.
    EXIT.
  ENDIF.

  SELECT SINGLE pgmna FROM tstc
    INTO lv_prog
      WHERE tcode = p_tcode.

  IF sy-subrc NE 0.
    MESSAGE s016(ds) WITH 'Não encontrado' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM f_to_clipboard USING lv_prog.

  MESSAGE s016(ds) WITH 'Copiado para o clipboard'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_ABAP
*&---------------------------------------------------------------------*
FORM f_add_abap .

  IF p_reque IS INITIAL.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM e07t
    INTO @DATA(lw_07t)
      WHERE trkorr = @p_reque
        AND langu = @sy-langu.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
    EXPORTING
      wi_kurztext   = lw_07t-as4text
      wi_trfunction = 'X'
      iv_username   = sy-uname
      wi_strkorr    = lw_07t-trkorr
      wi_client     = sy-mandt.

  MESSAGE s016(ds) WITH 'Empregado anexado'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_ULT_PROGR
*&---------------------------------------------------------------------*
FORM f_get_ult_progr .

  DATA lr_progs TYPE RANGE OF rs38m-programm.

  CHECK p_ultpro IS NOT INITIAL.

  IF p_ultpro CA '*'.
    APPEND 'ICP' && p_ultpro TO lr_progs.
  ELSE.
    APPEND 'ICP' && p_ultpro && '*' TO lr_progs.
  ENDIF.

  SELECT name FROM trdir
    INTO TABLE @DATA(lt_progs)
      WHERE name IN @lr_progs.

  SORT lt_progs BY name DESCENDING.

  LOOP AT lt_progs ASSIGNING FIELD-SYMBOL(<fs_prog>).

    WRITE: / <fs_prog>-name .


  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TEST
*&---------------------------------------------------------------------*
FORM f_test .

  DATA lo_test TYPE REF TO zcl_tester.

  CHECK p_tstf IS NOT INITIAL.

  CREATE OBJECT lo_test EXPORTING iv_file = CONV #( p_tstf ).

  IF lo_test->load( ) = 0.

    lo_test->run( ).

  ENDIF.

ENDFORM.
