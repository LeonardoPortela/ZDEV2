*----------------------------------------------------------------------*
*       CLASS CL_UTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_utils DEFINITION.
  PUBLIC SECTION.
    METHODS
      function_click FOR EVENT if_salv_events_functions~added_function
        OF cl_salv_events_table
        IMPORTING e_salv_function.

    METHODS valida_cabecalho
      IMPORTING
                i_arquivo       TYPE char01 DEFAULT abap_false
      RETURNING VALUE(e_status) TYPE char1.

    METHODS valida_itens
      RETURNING VALUE(e_status) TYPE abap_bool.

    METHODS show_errors   IMPORTING
                            i_show TYPE c.
    METHODS style_change  IMPORTING
                            fieldname TYPE any
                            style     TYPE any.
    METHODS save_text    IMPORTING
                           i_text TYPE STANDARD TABLE.
    METHODS read_werks   IMPORTING
                           i_evrtn TYPE evrtn
                           i_evrtp TYPE evrtp
                           i_werks TYPE werks_d
                         EXPORTING
                           e_lines TYPE STANDARD TABLE.
    METHODS text_edit_dialog
      IMPORTING
        i_displaymode TYPE c
      CHANGING
        ch_text       TYPE STANDARD TABLE.
    METHODS popup_to_confirm
      IMPORTING
        i_title    TYPE any
        i_question TYPE any
      EXPORTING
        e_answer   TYPE n.
    METHODS tratar_campos
      IMPORTING
        group1    TYPE char3
        value     TYPE char1
        invisible TYPE char1.
    METHODS set_keys_contract
      IMPORTING
        i_evrtn TYPE evrtn   OPTIONAL
        i_evrtp TYPE evrtp   OPTIONAL
        i_werks TYPE werks_d OPTIONAL.
    METHODS set_old_values
      IMPORTING
        i_header TYPE any
        i_itens  TYPE ANY TABLE.
    METHODS set_new_values
      IMPORTING
        i_header TYPE any
        i_itens  TYPE ANY TABLE.
    METHODS set_current_item
      IMPORTING
        i_item TYPE evrtp.
    METHODS get_current_item
      EXPORTING
        e_item TYPE evrtp.
    METHODS get_keys_contract
      EXPORTING
        e_evrtn TYPE evrtn
        e_evrtp TYPE evrtp
        e_werks TYPE werks_d.
    METHODS save_history_change
      IMPORTING
        i_fieldname TYPE fieldname
        i_fieldtxt  TYPE scrtext_m
        i_change_id TYPE c
        i_value_old TYPE any OPTIONAL
        i_value_new TYPE any OPTIONAL.
    METHODS get_history_change
      IMPORTING
        i_evrtn TYPE evrtn
        i_evrtp TYPE evrtp
        i_werks TYPE werks_d OPTIONAL.

    METHODS show_history_change
      IMPORTING
        i_pfstatus TYPE sypfkey
      CHANGING
        c_table    TYPE ANY TABLE.

    METHODS assert_values
      IMPORTING
        i_structure TYPE ddobjname
        i_fieldname TYPE dfies-fieldname OPTIONAL
        i_changeid  TYPE c OPTIONAL
        i_old_value TYPE any
        i_new_value TYPE any
        i_save_log  TYPE abap_bool
      EXCEPTIONS
        item_was_changed.

    METHODS validate_changes
      EXCEPTIONS
        records_already_saved.

    DATA: mo_alv             TYPE REF TO cl_salv_table,
          mo_column          TYPE REF TO cl_salv_column,
          mo_columns         TYPE REF TO cl_salv_columns,
          mt_lines           TYPE TABLE OF tline,
          mt_old_itens       TYPE TABLE OF ty_itens,
          mt_fieldnames      TYPE TABLE OF dfies,
          mt_new_itens       TYPE TABLE OF ty_itens,
          mt_text_tab        TYPE TABLE OF sotr_txt,
          mt_zmmt0068        TYPE TABLE OF zmmt0068,
          mt_history_out     TYPE TABLE OF ty_history,
          mt_text            TYPE catsxt_longtext_itab,
          mw_text            LIKE LINE OF mt_text,
          mw_fieldnames      TYPE dfies,
          mw_header          TYPE thead,
          mw_old_header      TYPE zmmt0063,
          mw_new_header      TYPE zmmt0063,
          mw_lines           TYPE tline,
          mw_text_tab        TYPE sotr_txt,
          mw_history_out     TYPE ty_history,
          mw_werks           TYPE werks_rang,
          mt_fieldvalues     TYPE TABLE OF werks_rang,
          mw_zmmt0067        TYPE zmmt0067,
          mw_zmmt0068        TYPE zmmt0068,
          mw_mbew            TYPE mbew,
          mw_marc            TYPE marc,
          mw_mard            TYPE mard,
          mw_t030k           TYPE t030k,
          at_werks           TYPE werks_d,
          at_status          VALUE abap_true,
          at_text            TYPE string,
          at_title           TYPE sytitle,
          at_answer          TYPE n,
          at_question        TYPE string,
          at_evrtn           TYPE evrtn,
          at_old_size        TYPE i,
          at_new_size        TYPE i,
          at_index           TYPE sy-tabix,
          at_evrtp           TYPE evrtp,
          at_item            TYPE evrtp,
          header_was_changed TYPE abap_bool,
          items_was_changed  TYPE abap_bool,
          v_matnr            TYPE mara-matnr.

    CONSTANTS:
      BEGIN OF c_crud,
        update VALUE 'U',
        insert VALUE 'I',
        delete VALUE 'D',
      END OF c_crud,

      BEGIN OF c_nivel,
        aprovado             VALUE 'A',
        rejeitado            VALUE 'R',
        aguardando_aprovacao VALUE ' ',
      END OF c_nivel.
ENDCLASS.                    "Z_UTILS DEFINITION

*----------------------------------------------------------------------*
*       CLASS Z_UTILS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_utils IMPLEMENTATION.
  METHOD get_history_change.
    SELECT *
      FROM zmmt0068
      INTO TABLE me->mt_zmmt0068
     WHERE evrtn = i_evrtn
       AND evrtp = i_evrtp.

    SELECT *
      FROM zmmt0068
 APPENDING TABLE me->mt_zmmt0068
     WHERE evrtn = i_evrtn
       AND evrtp = space.

    SORT me->mt_zmmt0068 BY data hora.
  ENDMETHOD.                    "GET_HITORY_CHANGE

  METHOD show_history_change.
    DATA: lo_functions      TYPE REF TO cl_salv_functions_list,
          lo_events         TYPE REF TO cl_salv_events_table,
          lw_disable_fields TYPE lvc_fname.

    TRY.
        cl_salv_table=>factory(
        IMPORTING
          r_salv_table = me->mo_alv
        CHANGING
          t_table      = c_table ).

        lo_events = me->mo_alv->get_event( ).
        SET HANDLER function_click FOR lo_events.

        me->mo_alv->set_screen_status(
                                      pfstatus      = i_pfstatus
                                      report        = sy-repid
                                     ).

        me->mo_alv->set_screen_popup( start_column = 8
                                      end_column   = 125
                                      start_line   = 5
                                      end_line     = 15 ).

        mo_columns = me->mo_alv->get_columns( ).
        mo_columns->set_optimize( ).

        mo_column ?= mo_columns->get_column( columnname = 'FNAME' ).
        mo_column->set_visible( value = if_salv_c_bool_sap=>false ).

        mo_column ?= mo_columns->get_column( 'CHANGEID' ).
        mo_column->set_visible( abap_false ).

        mo_column ?= mo_columns->get_column( 'VALUE_OLD' ).
        mo_column->set_long_text( 'Valor antigo/descrição' ).
        mo_column->set_medium_text( 'Vlr antigo/descrição' ).

      CATCH cx_salv_wrong_call
            cx_salv_not_found
            cx_salv_existing
            cx_salv_msg.
    ENDTRY.
  ENDMETHOD.                    "SHOW_HISTORY_CHANGE

  METHOD function_click.

    CASE e_salv_function.
      WHEN 'EXIT'.
        me->mo_alv->close_screen( ).
      WHEN 'VIEW_ITEM'.
        me->mo_alv->close_screen( ).
    ENDCASE.
  ENDMETHOD.                    "on_function_click

  METHOD popup_to_confirm.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = i_title
        text_question         = i_question
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ''
      IMPORTING
        answer                = e_answer.
  ENDMETHOD.                    "POPUP_TO_CONFIRM

  METHOD valida_itens.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = 'ZMMT0064'
      TABLES
        dfies_tab = me->mt_fieldnames.

    FIELD-SYMBOLS:
      <fieldname> TYPE any,
      <value>     TYPE any.

    e_status = abap_true.
    LOOP AT me->mt_fieldnames INTO me->mw_fieldnames WHERE fieldname = 'EMATN'
                                                        OR fieldname = 'KTMNG'
                                                        OR fieldname = 'NETPR'
                                                        OR fieldname = 'WERKS'
                                                        OR fieldname = 'MWSKZ'.

      ASSIGN me->mw_fieldnames-fieldname TO <fieldname>.

      IF ( <fieldname> = 'WERKS' ) .
        <fieldname> = |T_{ <fieldname> }|.
      ENDIF.

      LOOP AT gt_saida_itens INTO wl_saida_itens.
        ASSIGN COMPONENT <fieldname> OF STRUCTURE wl_saida_itens TO <value>.

        IF ( <value> IS INITIAL ) AND <fieldname> NE 'MWSKZ'.
          IF <fieldname> EQ 'KTMNG' AND wl_saida_cabecalho-evart NE 'WK'.
            MESSAGE e836(sd) WITH |É obrigatório o preenchimento do campo { me->mw_fieldnames-fieldtext }.|.
            e_status = abap_false.
            EXIT.
          ENDIF.
        ENDIF.
        IF ( <fieldname> = 'MWSKZ' ) AND wl_saida_itens-mwskz IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wl_saida_itens-ematn
            IMPORTING
              output = v_matnr.

          SELECT SINGLE *
             FROM t030k
               INTO  me->mw_t030k
             WHERE ktopl = '0050'
             AND   mwskz = wl_saida_itens-mwskz.
          IF sy-subrc NE 0.
            MESSAGE e836(sd) WITH 'IVA não existe:'
                                  v_matnr '/' wl_saida_itens-mwskz.
            e_status = abap_false.
            EXIT.
          ENDIF.
        ENDIF.

        IF ( <fieldname> = 'EMATN' ).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wl_saida_itens-ematn
            IMPORTING
              output = v_matnr.
          LOOP AT r_werks[] INTO r_werks.
            SELECT SINGLE *
                   FROM mbew
                   INTO me->mw_mbew
                  WHERE matnr    = wl_saida_itens-ematn
                    AND bwkey    = r_werks-low.

            IF sy-subrc NE 0.
*              MESSAGE E836(SD) WITH 'Expandir para contabilidade:'
*                                    V_MATNR  '/' R_WERKS-LOW.
*              E_STATUS = ABAP_FALSE.
*              EXIT.
            ELSE.
              SELECT SINGLE *
              FROM zmmt0067
              INTO me->mw_zmmt0067
             WHERE matnr    = wl_saida_itens-ematn
               AND werks    = r_werks-low.
              IF sy-subrc NE 0.
                SELECT SINGLE *
                     FROM zmmt0067
                     INTO me->mw_zmmt0067
                    WHERE werks    = r_werks-low.
                IF sy-subrc  NE 0.
                  MESSAGE e836(sd) WITH 'Atualizar estratégia para:'
                                         v_matnr  '/' r_werks-low.
                  e_status = abap_false.
                  EXIT.
                ENDIF.
              ENDIF.
              SELECT SINGLE *
               FROM marc
               INTO me->mw_marc
              WHERE matnr    = wl_saida_itens-ematn
                AND werks    = r_werks-low.
              IF sy-subrc NE 0.
                MESSAGE e836(sd) WITH 'Expandir para centro:'
                                       v_matnr  '/' r_werks-low.
                e_status = abap_false.
                EXIT.
              ELSEIF wl_saida_itens-lgort IS NOT INITIAL.
*                SELECT SINGLE *
*                    FROM MARD
*                    INTO ME->MW_MARD
*                   WHERE MATNR    = WL_SAIDA_ITENS-EMATN
*                     AND WERKS    = R_WERKS-LOW
*                     AND LGORT    = WL_SAIDA_ITENS-LGORT.
*                IF SY-SUBRC NE 0.
*                  MESSAGE E836(SD) WITH 'Expandir para depósito:'
*                                         V_MATNR  '/' R_WERKS-LOW.
*                  E_STATUS = ABAP_FALSE.
*                  EXIT.
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.                    "VALIDA_ITENS

  METHOD save_text.
    LOOP AT i_text INTO me->mw_text.
      CONCATENATE me->mw_lines-tdline me->mw_text INTO me->mw_lines-tdline.
    ENDLOOP.
  ENDMETHOD.                    "SAVE_TEXT

  METHOD read_werks.
    CONCATENATE i_evrtn i_evrtp i_werks INTO mw_header-tdname.

    mw_header-tdobject = 'ZMMR104'.
    mw_header-tdid     = 'ZREJ'.
    mw_header-tdspras  = sy-langu.

*    REFRESH ME->MT_LINES.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client         = sy-mandt
        id             = mw_header-tdid
        language       = mw_header-tdspras
        name           = mw_header-tdname
        object         = mw_header-tdobject
        archive_handle = 0
        local_cat      = ' '
      TABLES
        lines          = me->mt_lines.
  ENDMETHOD.                    "READ_TEXT

  METHOD text_edit_dialog.
    CONCATENATE 'Motivo rejeição Contrato:' me->at_evrtn '-' me->at_evrtp '-' me->at_werks INTO me->at_title
      SEPARATED BY space.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = me->at_title
        im_display_mode = i_displaymode
*       IM_START_COLUMN = 10
*       IM_START_ROW    = 10
      CHANGING
        ch_text         = ch_text.
  ENDMETHOD.                    "SHOW_TEXT_EDITOR

  METHOD tratar_campos.

    wl_fields-group1    = group1.
    wl_fields-value     = value.
    wl_fields-invisible = invisible.

    APPEND wl_fields TO gt_fields.
  ENDMETHOD.                    "Z_TRATAR_CAMPOS

  METHOD set_old_values.
    me->mw_old_header = i_header.
    me->mt_old_itens  = i_itens.
  ENDMETHOD.                    "SET_OLD_VALUES

  METHOD set_new_values.
    me->mw_new_header = i_header.
    me->mt_new_itens  = i_itens.
  ENDMETHOD.                    "SET_OLD_VALUES

  METHOD set_keys_contract.
    IF NOT ( i_evrtn IS INITIAL ).
      me->at_evrtn = i_evrtn.
    ENDIF.

    me->at_evrtp = i_evrtp.
    me->at_werks = i_werks.
  ENDMETHOD.                    "SET_KEYS_CONTRACT

  METHOD set_current_item.
    me->at_item = i_item.
  ENDMETHOD.                    "SET_CURRENT_ITEM

  METHOD get_current_item.
    e_item = me->at_item.
  ENDMETHOD.                    "GET_CURRENT_ITEM

  METHOD get_keys_contract.
    e_evrtn = me->at_evrtn.
    e_evrtp = me->at_evrtp.
    e_werks = me->at_werks.
  ENDMETHOD.                    "GET_KEYS_CONTRACT

  METHOD save_history_change.
    get_keys_contract( IMPORTING
                       e_evrtn = mw_zmmt0068-evrtn
                       e_evrtp = mw_zmmt0068-evrtp ).

    SELECT SINGLE
       MAX( changenr )
      FROM zmmt0068
      INTO me->mw_zmmt0068-changenr
     WHERE evrtn    = mw_zmmt0068-evrtn
       AND evrtp    = mw_zmmt0068-evrtp.
*       AND CHANGEID = I_CHANGE_ID.

    ADD 1 TO mw_zmmt0068-changenr.

    mw_zmmt0068-fname     = i_fieldname.
    mw_zmmt0068-ftext     = i_fieldtxt.
    mw_zmmt0068-username  = sy-uname.
    mw_zmmt0068-data      = sy-datum.
    mw_zmmt0068-hora      = sy-uzeit.
    mw_zmmt0068-changeid  = i_change_id.
    mw_zmmt0068-value_old = i_value_old.
    mw_zmmt0068-value_new = i_value_new.

    MODIFY zmmt0068 FROM mw_zmmt0068.
    COMMIT WORK.
  ENDMETHOD.                    "SAVE_data_HISTORY

  METHOD assert_values.
    DATA: lw_zmmt0068  TYPE zmmt0068,
          lw_old_itens TYPE ty_itens,
          lw_new_itens TYPE ty_itens,
          lw_new_werks TYPE werks_rang,
          lw_old_werks TYPE werks_rang.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = i_structure
        fieldname = i_fieldname
      TABLES
        dfies_tab = mt_fieldnames.

    FIELD-SYMBOLS:
      <fieldname> TYPE any,
      <old_value> TYPE any,
      <new_value> TYPE any.

    LOOP AT me->mt_fieldnames INTO me->mw_fieldnames.
      ASSIGN me->mw_fieldnames-fieldname TO <fieldname>.

      ASSIGN COMPONENT:
      <fieldname> OF STRUCTURE i_old_value TO <old_value>,
      <fieldname> OF STRUCTURE i_new_value TO <new_value>.

      IF ( <old_value> NE <new_value> ).
        IF i_save_log = abap_true.
          save_history_change( i_fieldname = me->mw_fieldnames-fieldname
                               i_fieldtxt  = me->mw_fieldnames-scrtext_m
                               i_change_id = i_changeid
                               i_value_old = <old_value>
                               i_value_new = <new_value> ).
        ELSE.
          RAISE item_was_changed.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "ASSERT_TABLE_CHANGES

  METHOD validate_changes.
    DATA: lw_zmmt0068  TYPE zmmt0068,
          lw_old_itens TYPE ty_itens,
          lw_new_itens TYPE ty_itens,
          lw_new_werks TYPE werks_rang,
          lw_old_werks TYPE werks_rang.

    IF me->mw_old_header = me->mw_new_header
   AND me->mt_old_itens  = me->mt_new_itens.
      MESSAGE TEXT-w05 TYPE 'S' DISPLAY LIKE 'W'.
      RAISE records_already_saved.
    ENDIF.

    IF ( wl_saida_cabecalho-evrtn IS INITIAL ).
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'Z_NUM_CTRA'
          quantity    = '1'
        IMPORTING
          number      = me->mw_new_header-evrtn.

      wl_saida_cabecalho-evrtn = me->mw_new_header-evrtn.
      set_keys_contract( i_evrtn = wl_saida_cabecalho-evrtn ).

      assert_values( i_structure = 'ZMMT0063'
                     i_fieldname = 'EVRTN'
                     i_changeid  = c_crud-insert
                     i_old_value = me->mw_old_header
                     i_new_value = me->mw_new_header
                     i_save_log  = abap_true ).
    ELSE.
      set_keys_contract( i_evrtn = wl_saida_cabecalho-evrtn ).

      assert_values( i_structure = 'ZMMT0063'
                     i_changeid  = c_crud-update
                     i_old_value = me->mw_old_header
                     i_new_value = me->mw_new_header
                     i_save_log  = abap_true  ).
    ENDIF.

    LOOP AT me->mt_new_itens INTO lw_new_itens.
      READ TABLE me->mt_old_itens INTO lw_old_itens WITH KEY evrtp = lw_new_itens-evrtp.

      set_keys_contract( i_evrtp = lw_new_itens-evrtp ).

      DESCRIBE TABLE:
      lw_old_itens-t_werks LINES me->at_old_size,
      lw_new_itens-t_werks LINES me->at_new_size.

      "// Check deleted werks;
      IF ( me->at_old_size > me->at_new_size ).

        LOOP AT lw_old_itens-t_werks INTO lw_old_werks.
          READ TABLE lw_new_itens-t_werks INTO lw_new_werks WITH KEY low = lw_old_werks-low.
          IF NOT ( sy-subrc IS INITIAL ).
            save_history_change( i_fieldname = 'WERKS'
                                 i_fieldtxt  = 'Centro'
                                 i_change_id = c_crud-delete
                                 i_value_old = lw_old_werks-low
                               ).
          ENDIF.
        ENDLOOP.

        "// Check added werks;
      ELSEIF ( me->at_new_size > me->at_old_size ).

        LOOP AT lw_new_itens-t_werks INTO lw_new_werks.
          READ TABLE lw_old_itens-t_werks INTO lw_old_werks WITH KEY low = lw_new_werks-low.

          IF NOT ( sy-subrc IS INITIAL ).
            save_history_change( i_fieldname = 'WERKS'
                                 i_fieldtxt  = 'Centro'
                                 i_change_id = c_crud-insert
                                 i_value_new = lw_new_werks-low
                               ).
          ENDIF.
        ENDLOOP.

        "// Check updated werks;
      ELSE.
        LOOP AT lw_old_itens-t_werks INTO lw_old_werks.
          me->at_index = sy-tabix.
          READ TABLE lw_new_itens-t_werks INTO lw_new_werks WITH KEY low = lw_old_werks-low.

          IF NOT ( sy-subrc IS INITIAL ).
            READ TABLE lw_new_itens-t_werks INTO lw_new_werks INDEX me->at_index.

            save_history_change( i_fieldname = 'WERKS'
                                 i_fieldtxt  = 'Centro'
                                 i_change_id = c_crud-update
                                 i_value_old = lw_old_werks-low
                                 i_value_new = lw_new_werks-low
                               ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF ( lw_new_itens-changeid NE lw_old_itens-changeid ).
        CASE lw_new_itens-changeid.
            "//Check added item;
          WHEN c_crud-insert.
            save_history_change( i_fieldname = 'EVRTP'
                                 i_fieldtxt  = 'Item do contrato'
                                 i_change_id = c_crud-insert
                                 i_value_new = lw_new_itens-evrtp ).
            "//Check deleted item;
          WHEN c_crud-delete.
            save_history_change( i_fieldname = 'EVRTP'
                                 i_fieldtxt  = 'Item do contrato'
                                 i_change_id = c_crud-delete
                                 i_value_old = lw_new_itens-evrtp ).
        ENDCASE.

      ELSE.
        "//Check updated itens;
        assert_values( i_structure = 'ZMMT0064'
                       i_changeid  = c_crud-update
                       i_old_value = lw_old_itens
                       i_new_value = lw_new_itens
                       i_save_log  = abap_true ).
      ENDIF.

      CLEAR lw_old_itens.
    ENDLOOP.
  ENDMETHOD.                    "VALIDATE_CHANGES

  METHOD valida_cabecalho.
    DATA: wl_lfa1 TYPE lfa1.

    me->at_status = abap_true.

    IF r_werks-low IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-evart IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-vedat IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-kdatb IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-kdate IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-zterm IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-kdate IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-waers IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-ekorg IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-ekgrp IS INITIAL.
      me->at_status = abap_false.
    ELSEIF wl_saida_cabecalho-lifnr IS INITIAL.
      me->at_status = abap_false.
    ELSE.

      SELECT SINGLE *
        FROM tvzbt
        INTO @DATA(lw_payment)
       WHERE zterm = @wl_saida_cabecalho-zterm.

      IF ( sy-subrc IS NOT INITIAL ).
        MESSAGE TEXT-e11 TYPE 'S' DISPLAY LIKE 'E'.
        me->at_status = abap_false.
      ELSE.
        SELECT SINGLE ztag1 FROM t052 INTO wl_saida_cabecalho-zbd1t WHERE zterm = wl_saida_cabecalho-zterm.
      ENDIF.

*      CHECK ( ME->AT_STATUS NE ABAP_FALSE ).
*
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_saida_cabecalho-lifnr
        IMPORTING
          output = wl_saida_cabecalho-lifnr.

      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
       WHERE lifnr = wl_saida_cabecalho-lifnr.

      wl_fornecedor = wl_lfa1-name1.

      SELECT SINGLE *
        FROM ekko
        INTO @DATA(lw_pedido)
        WHERE lifnr = @wl_saida_cabecalho-lifnr
        AND  bstyp = 'K' "contrato
        AND  kdatb LE @sy-datum
        AND  kdate GE @sy-datum.
      IF sy-subrc EQ 0.
        MESSAGE i836(sd) WITH 'Existe um contrato válido para:'
                                  wl_saida_cabecalho-lifnr
                                  lw_pedido-ebeln.
      ENDIF.

    ENDIF.

    IF ( me->at_status = abap_false ).
      MESSAGE s836(sd) WITH TEXT-e01 DISPLAY LIKE 'E'.
    ENDIF.

    MOVE me->at_status TO e_status.
  ENDMETHOD.                    "Z_VALIDAR_INFO_CABECALHO

  METHOD show_errors.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = i_show
        i_repid    = sy-repid
        i_set_cell = 'WL_CELL'
        i_set_obj  = 'WL_OBJ'
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.
  ENDMETHOD.                    "Z_SHOW_SPLITTER_ERROR

  METHOD style_change.
    wl_estilo-fieldname = fieldname.
    wl_estilo-style     = style.
    APPEND wl_estilo TO gt_estilo.
  ENDMETHOD.                    "Z_STYLE_ENABLE_EDIT
ENDCLASS.                    "Z_UTILS IMPLEMENTATION

INITIALIZATION.
  DATA r_utils TYPE REF TO cl_utils.

*&---------------------------------------------------------------------*
*&  Include           Z_TIPO_OPERACAO
*&---------------------------------------------------------------------*


CLASS cl_tipo_operacao DEFINITION.
  PUBLIC SECTION.
    METHODS contract_generate
      IMPORTING
        i_evrtn            TYPE evrtn
      RETURNING
        VALUE(r_msg_error) TYPE bapireturn_t.

    METHODS search_registros
      IMPORTING
        i_evrtn TYPE evrtn
        i_evrtp TYPE evrtp   OPTIONAL
        i_werks TYPE werks_d OPTIONAL.

    METHODS novo_registro.
    METHODS salvar_registros.
    METHODS set_deletion_flag.

    CONSTANTS:
      BEGIN OF c_status,
        pendente  VALUE ' ',
        aprovado  VALUE 'A',
        rejeitado VALUE 'R',
      END OF c_status,

      c_tp_servico VALUE 'D'.

  PRIVATE SECTION.
    DATA: gt_item                TYPE TABLE OF bapimeoutitem,
          gt_itemx               TYPE TABLE OF bapimeoutitemx,
          gt_account             TYPE TABLE OF bapimeoutaccount,
          gt_accountx            TYPE TABLE OF bapimeoutaccountx,
          gt_item_cond_validity  TYPE TABLE OF bapimeoutvalidity,
          gt_item_validity       TYPE TABLE OF bapimeoutvalidity,
          gt_item_cond_validityx TYPE TABLE OF bapimeoutvalidityx,
          gt_item_condition      TYPE TABLE OF bapimeoutcondition,
          gt_item_conditionx     TYPE TABLE OF bapimeoutconditionx,
          mt_lines               TYPE TABLE OF tline,
          mt_text_tab            TYPE TABLE OF sotr_txt,
          mt_zmmt0064            TYPE TABLE OF zmmt0064,

          wl_item                TYPE bapimeoutitem,
          wl_itemx               TYPE bapimeoutitemx,
          wl_header              TYPE bapimeoutheader,
          wl_headerx             TYPE bapimeoutheaderx,
          mw_zmmt0063            TYPE zmmt0063,
          mw_lfa1                TYPE lfa1,
          mw_account             TYPE bapimeoutaccount,
          mw_accountx            TYPE bapimeoutaccountx,
          wl_item_cond_validity  TYPE bapimeoutvalidity,
          wl_item_validity       TYPE bapimeoutvalidity,
          wl_item_cond_validityx TYPE bapimeoutvalidityx,
          wl_item_condition      TYPE bapimeoutcondition,
          wl_item_conditionx     TYPE bapimeoutconditionx,
          mw_lines               TYPE tline,
          mw_text_tab            TYPE sotr_txt,
          mw_zmmt0064            TYPE zmmt0064,
          mw_werks               TYPE werks_rang,
          mv_sequence            TYPE evrtn,
          at_text                TYPE string,
          at_evrtp               TYPE zmmt0064-evrtp.
ENDCLASS.                    "Z_TIPO_OPERACAO DEFINITION


*----------------------------------------------------------------------*
*       CLASS Z_TIPO_OPERACAO IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_tipo_operacao IMPLEMENTATION.
  METHOD contract_generate.
    CLEAR: gt_item, gt_itemx, gt_account, gt_accountx, at_evrtp.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 1
        text       = TEXT-w03.

    SELECT SINGLE *
      FROM zmmt0063
      INTO me->mw_zmmt0063
     WHERE evrtn = i_evrtn.

    SELECT *
      FROM zmmt0064
      INTO TABLE me->mt_zmmt0064
     WHERE evrtn = i_evrtn
      ORDER BY evrtp_ctr DESCENDING.

    IF me->mw_zmmt0063-loekz IS NOT INITIAL.
      wl_header-delete_ind = 'L'.
      wl_headerx-delete_ind = abap_true.
    ENDIF.

    SELECT SINGLE *
      FROM lfa1
      INTO  me->mw_lfa1
      WHERE lifnr = me->mw_zmmt0063-lifnr.

    IF ( me->mw_zmmt0063-ctra_superior IS NOT INITIAL ).
      CALL FUNCTION 'BAPI_CONTRACT_GETDETAIL' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          purchasingdocument = me->mw_zmmt0063-ctra_superior
          condition_data     = 'X'
        TABLES
          item_cond_validity = gt_item_validity.

    ENDIF.
    "// Cabeçalho do contrato;
    wl_header-comp_code   = me->mw_zmmt0063-bukrs.
    wl_header-doc_type    = me->mw_zmmt0063-evart.
    wl_header-vendor      = me->mw_zmmt0063-lifnr.
    wl_header-pmnttrms    = me->mw_zmmt0063-zterm.
    wl_header-dscnt1_to   = me->mw_zmmt0063-zbd1t.
    wl_header-purch_org   = me->mw_zmmt0063-ekorg.
    wl_header-pur_group   = me->mw_zmmt0063-ekgrp.
    wl_header-currency    = me->mw_zmmt0063-waers.
    wl_header-doc_date    = me->mw_zmmt0063-vedat.
    wl_header-vper_start  = me->mw_zmmt0063-kdatb.
    wl_header-vper_end    = me->mw_zmmt0063-kdate.
    wl_header-incoterms1  = me->mw_zmmt0063-inco1.
    wl_header-dscnt1_to   = me->mw_zmmt0063-zbd1t.
    wl_header-acum_value  = me->mw_zmmt0063-ktwrt.

    wl_headerx-comp_code  = abap_true.
    wl_headerx-doc_type   = abap_true.
    wl_headerx-vendor     = abap_true.
    wl_headerx-pmnttrms   = abap_true.
    wl_headerx-dscnt1_to  = abap_true.
    wl_headerx-purch_org  = abap_true.
    wl_headerx-pur_group  = abap_true.
    wl_headerx-currency   = abap_true.
    wl_headerx-doc_date   = abap_true.
    wl_headerx-vper_start = abap_true.
    wl_headerx-vper_end   = abap_true.
    wl_headerx-incoterms1 = abap_true.
    wl_headerx-dscnt1_to  = abap_true.
    wl_headerx-acum_value = abap_true.
    "// --

    READ TABLE me->mt_zmmt0064 INTO me->mw_zmmt0064 INDEX 1.
    IF me->mw_zmmt0064-evrtp_ctr IS NOT INITIAL.
      at_evrtp = me->mw_zmmt0064-evrtp_ctr.
    ENDIF.

    SORT me->mt_zmmt0064 BY evrtp evrtp_ctr. "ordem de item table "Z"
    "// Cria os itens para todos os centros informados;
    LOOP AT me->mt_zmmt0064 INTO me->mw_zmmt0064.

      IF (  me->mw_zmmt0064-changeid = r_utils->c_crud-delete ).
        wl_item-delete_ind  = 'L'.
        wl_itemx-delete_ind = abap_true.
      ENDIF.

      IF me->mw_zmmt0064-evrtp_ctr IS NOT INITIAL.
        wl_item-item_no        = me->mw_zmmt0064-evrtp_ctr.
      ELSE.
        ADD 10 TO at_evrtp.
        wl_item-item_no        = at_evrtp.
      ENDIF.
*---> 04/07/2023 - Migração S4 - AF
*     wl_item-material       = me->mw_zmmt0064-ematn.
      wl_item-material       = CONV #( me->mw_zmmt0064-ematn ).
*---> 04/07/2023 - Migração S4 - AF
      wl_item-item_cat       = me->mw_zmmt0064-epstp.
      wl_item-acctasscat     = me->mw_zmmt0064-knttp.
      wl_item-plant          = me->mw_zmmt0064-werks.
      wl_item-matl_group     = me->mw_zmmt0064-matkl.
      wl_item-target_qty     = me->mw_zmmt0064-ktmng.
      wl_item-po_unit        = me->mw_zmmt0064-meins.
      wl_item-orderpr_un     = me->mw_zmmt0064-meins.
      wl_item-conf_ctrl      = '0001'.
      wl_item-taxjurcode     = me->mw_lfa1-txjcd.

      wl_item-distrib        = me->mw_zmmt0064-vrtkz.
      wl_item-part_inv       = me->mw_zmmt0064-twrkz.
      wl_item-net_price      = me->mw_zmmt0064-netpr.
      wl_item-price_unit     = me->mw_zmmt0064-peinh.
      wl_item-tax_code       = me->mw_zmmt0064-mwskz.
      wl_item-stge_loc       = me->mw_zmmt0064-lgort.

      APPEND wl_item TO gt_item.
*      CLEAR WL_ITEM.

      wl_itemx-item_no        = wl_item-item_no.
      wl_itemx-material       = abap_true.
      wl_itemx-item_cat       = abap_true.
      wl_itemx-acctasscat     = abap_true.
      wl_itemx-plant          = abap_true.
      wl_itemx-matl_group     = abap_true.
      wl_itemx-po_unit        = abap_true.
      wl_itemx-orderpr_un     = abap_true.
      wl_itemx-conf_ctrl      = abap_true.
      wl_itemx-taxjurcode     = abap_true.
      wl_itemx-distrib        = abap_true.
      wl_itemx-net_price      = abap_true.
      wl_itemx-price_unit     = abap_true.
      wl_itemx-target_qty     = abap_true.
      wl_itemx-part_inv       = abap_true.
      wl_itemx-tax_code       = abap_true.

      APPEND wl_itemx TO gt_itemx.
      CLEAR wl_itemx.

      IF ( me->mw_zmmt0063-ctra_superior IS NOT INITIAL ).
*        READ TABLE GT_ITEM_VALIDITY INTO WL_ITEM_VALIDITY WITH KEY ITEM_NO = WL_ITEM-ITEM_NO.
        CLEAR wl_item_validity.
        LOOP AT gt_item_validity INTO wl_item_validity WHERE item_no = wl_item-item_no.
          IF me->mw_zmmt0063-kdatb GE wl_item_validity-valid_from.
            EXIT.
          ENDIF .
        ENDLOOP.
        IF wl_item_validity-valid_from IS NOT INITIAL.
          wl_item_cond_validity-item_no = wl_item-item_no.
          wl_item_cond_validity-serial_id = wl_item_validity-serial_id.
          wl_item_cond_validity-valid_from = wl_item_validity-valid_from. " ME->MW_ZMMT0063-KDATB.
          wl_item_cond_validity-valid_to   = wl_item_validity-valid_to. "ME->MW_ZMMT0063-KDATE.
        ELSE.
          wl_item_cond_validity-item_no = wl_item-item_no.
          CLEAR wl_item_cond_validity-serial_id.
          wl_item_cond_validity-valid_from = me->mw_zmmt0063-kdatb.
          wl_item_cond_validity-valid_to   = me->mw_zmmt0063-kdate.
        ENDIF.
        APPEND wl_item_cond_validity TO gt_item_cond_validity.

        wl_item_cond_validityx-item_no = wl_item-item_no.
        wl_item_cond_validityx-serial_id = wl_item_validity-serial_id.
        wl_item_cond_validityx-item_nox = 'X'.
        wl_item_cond_validityx-valid_from = 'X'.
        wl_item_cond_validityx-valid_to = 'X'.
        APPEND wl_item_cond_validityx TO gt_item_cond_validityx.


        wl_item_condition-item_no = wl_item-item_no.
        wl_item_condition-serial_id = wl_item_validity-serial_id.
        wl_item_condition-cond_count = '1'.
        wl_item_condition-cond_type = 'PB00'.
        wl_item_condition-scale_type = 'A'.
        wl_item_condition-calctypcon = 'C'.
        wl_item_condition-cond_value = me->mw_zmmt0064-netpr.
        wl_item_condition-currency = 'BRL'.
        wl_item_condition-currency_iso = 'BRL'.

*NUMERATOR
*DENOMINATOR

        wl_item_condition-cond_p_unt = me->mw_zmmt0064-peinh.
        wl_item_condition-cond_unit = me->mw_zmmt0064-meins.
        wl_item_condition-cond_unit_iso = me->mw_zmmt0064-meins.
        wl_item_condition-base_uom = me->mw_zmmt0064-meins.
        wl_item_condition-base_uom_iso = me->mw_zmmt0064-meins.

*LOWERLIMIT

*UPPERLIMIT

        wl_item_condition-vendor_no = me->mw_zmmt0063-lifnr.
        wl_item_condition-change_id = 'U'.
        APPEND wl_item_condition TO gt_item_condition.


        wl_item_conditionx-item_no = wl_item-item_no.
        wl_item_conditionx-serial_id = wl_item_validity-serial_id.
        wl_item_conditionx-cond_count = '2'.
        wl_item_conditionx-item_nox = 'X'.
        wl_item_conditionx-cond_countx = 'X'.
        wl_item_conditionx-cond_type = 'X'.
        wl_item_conditionx-scale_type = 'X'.
        wl_item_conditionx-calctypcon = 'X'.
        wl_item_conditionx-cond_value = 'X'.
        wl_item_conditionx-currency = 'X'.
        wl_item_conditionx-currency_iso = 'X'.
        wl_item_conditionx-cond_unit = 'X'.
        wl_item_conditionx-cond_unit_iso = 'X'.
        wl_item_conditionx-cond_p_unt = 'X'.
        wl_item_conditionx-base_uom = 'X'.
        wl_item_conditionx-base_uom_iso = 'X'.
        wl_item_conditionx-vendor_no = 'X'.

        APPEND wl_item_conditionx TO gt_item_conditionx.
      ENDIF.

      CLEAR wl_item.


      "// Lê itens de clas.Contábil
*        LOOP AT WL_SAIDA_ITENS-ITENS_CLAS INTO WL_ITEM_0200.
*          WL_ACCOUNT-ITEM_NO     = WL_ITEM_0200-EBELP.
*          WL_ACCOUNT-GL_ACCOUNT  = WL_ITEM_0200-SAKTO.
*          WL_ACCOUNT-DISTR_PERC  = WL_ITEM_0200-VPROZ.
*          WL_ACCOUNT-COSTCENTER  = WL_ITEM_0200-KOSTL.
*          WL_ACCOUNT-ORDERID     = WL_ITEM_0200-AUFNR.
*          APPEND WL_ACCOUNT TO GT_ACCOUNT.
*          CLEAR WL_ACCOUNT.

*          WL_ACCOUNTX-ITEM_NO    = WL_ACCOUNTX-GL_ACCOUNT =
*          WL_ACCOUNTX-DISTR_PERC = WL_ACCOUNTX-COSTCENTER =
*          WL_ACCOUNTX-ORDERID    = ABAP_TRUE.
*          APPEND WL_ACCOUNTX TO GT_ACCOUNTX.
*          CLEAR WL_ACCOUNTX.
*        ENDLOOP.
      "// --

    ENDLOOP.
    "// --

    "// Check if the higher contract wasn't created yet;
    IF ( me->mw_zmmt0063-ctra_superior IS INITIAL ).
      CALL FUNCTION 'BAPI_CONTRACT_CREATE'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          header             = wl_header
          headerx            = wl_headerx
        IMPORTING
          purchasingdocument = me->mw_zmmt0063-ctra_superior
        TABLES
          return             = r_msg_error
          item               = gt_item
          itemx              = gt_itemx
          account            = gt_account
          accountx           = gt_accountx.

      DELETE r_msg_error WHERE type NE 'E'.
      IF r_msg_error IS INITIAL.
        r_utils->save_history_change( i_fieldname = 'EVRTN'
                                      i_fieldtxt  = 'Contrato Criado'
                                      i_change_id = me->c_status-aprovado
                                      i_value_new = me->mw_zmmt0063-ctra_superior
                                    ).
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_CONTRACT_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          purchasingdocument  = me->mw_zmmt0063-ctra_superior
          header              = wl_header
          headerx             = wl_headerx
        TABLES
          return              = r_msg_error
          item                = gt_item
          itemx               = gt_itemx
          account             = gt_account
          accountx            = gt_accountx
          item_cond_validity  = gt_item_cond_validity
          item_cond_validityx = gt_item_cond_validityx
          item_condition      = gt_item_condition
          item_conditionx     = gt_item_conditionx.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = wl_return.

    DELETE r_msg_error WHERE type NE 'E'.

    "// Verifica se possuí mensagens de erro;
    IF NOT ( r_msg_error IS INITIAL ).
      CLEAR me->mw_zmmt0063-ctra_superior.
      MESSAGE TEXT-e09 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MODIFY zmmt0063 FROM me->mw_zmmt0063.
      COMMIT WORK.
      MESSAGE s836(sd) WITH TEXT-s02.
    ENDIF.
  ENDMETHOD.                    "Z_BAPI_CONTRACT_CREATE

  METHOD novo_registro.
    CLEAR: wl_saida_cabecalho, gt_saida_itens, wl_mensagem,
           gt_msg_return, r_werks, gt_fields, r_werks[]. "GT_ITEM_0200.

    btn_display_werks = icon_enter_more.

    vg_op_mode = c_novo.
    r_utils->tratar_campos( group1    = 'GR1'
                            value     = '0'
                            invisible = '0' ).

    r_utils->tratar_campos( group1    = 'GR2'
                            value     = '1'
                            invisible = '0' ).

    FREE r_utils.
  ENDMETHOD.                    "Z_CREATE_NEW_OPERACAO

  METHOD salvar_registros.
    REFRESH: gt_zmmt0064,
             gt_ekpo,
             gt_fields_style.

    DATA: nivel_aprovacao TYPE c,
          v_evrtp_ctr     TYPE zmmt0064-evrtp_ctr.
    "// Define os campos a serem desabilitados *
    APPEND: 'EMATN' TO gt_fields_style,
            'EPSTP' TO gt_fields_style,
            'EVRTP' TO gt_fields_style,
            'KNTTP' TO gt_fields_style,
            'KTMNG' TO gt_fields_style,
            'LGORT' TO gt_fields_style,
            'MATKL' TO gt_fields_style,
            'MEINS' TO gt_fields_style,
            'MWSKZ' TO gt_fields_style,
            'NETPR' TO gt_fields_style,
            'TWRKZ' TO gt_fields_style,
            'TXZ01' TO gt_fields_style,
            'WERKS' TO gt_fields_style,
            'PEINH' TO gt_fields_style.
    IF  wl_saida_cabecalho-ctra_superior IS NOT INITIAL.
      SELECT *
        FROM ekpo
        INTO TABLE gt_ekpo
        WHERE ebeln = wl_saida_cabecalho-ctra_superior.
    ENDIF.
    SORT gt_ekpo BY werks matnr.
    SORT gt_fields_style BY fieldname.
    CLEAR v_evrtp_ctr.
    LOOP AT gt_saida_itens INTO wl_saida_itens.
      CLEAR: gt_estilo[], wl_saida_itens-estilo.

      DATA(index) = sy-tabix.

      LOOP AT wl_saida_itens-t_werks INTO mw_werks.
        CLEAR wl_zmmt0064.

        IF wl_saida_itens-status = icon_light_out.
          nivel_aprovacao = r_utils->c_nivel-aguardando_aprovacao.
        ELSEIF wl_saida_itens-status = icon_delete.
*          NIVEL_APROVACAO = R_UTILS->C_NIVEL-APROVADO.
          nivel_aprovacao = r_utils->c_nivel-aguardando_aprovacao. " ´para salvar o contrato novamente
        ELSE.
          SELECT SINGLE nivel
            FROM zmmt0064
            INTO nivel_aprovacao
           WHERE evrtn = wl_saida_cabecalho-evrtn
             AND evrtp = wl_saida_itens-evrtp
             AND werks = mw_werks-low.
        ENDIF.

        IF ( wl_saida_itens-epstp = c_tp_servico ).
          wl_zmmt0064-txz01 = wl_saida_itens-txz01.
        ENDIF.

        wl_zmmt0064-evrtn      = wl_saida_cabecalho-evrtn.
        wl_zmmt0064-evrtp      = wl_saida_itens-evrtp.
        wl_zmmt0064-werks      = mw_werks-low.
        wl_zmmt0064-epstp      = wl_saida_itens-epstp.
        wl_zmmt0064-knttp      = wl_saida_itens-knttp.
        wl_zmmt0064-ematn      = wl_saida_itens-ematn.
        wl_zmmt0064-sakto      = wl_saida_itens-sakto.
        wl_zmmt0064-matkl      = wl_saida_itens-matkl.
        wl_zmmt0064-ktmng      = wl_saida_itens-ktmng.
        wl_zmmt0064-meins      = wl_saida_itens-meins.
        wl_zmmt0064-netpr      = wl_saida_itens-netpr.
        wl_zmmt0064-mwskz      = wl_saida_itens-mwskz.
        wl_zmmt0064-lgort      = wl_saida_itens-lgort.
        wl_zmmt0064-peinh      = wl_saida_itens-peinh.
        wl_zmmt0064-nivel      = nivel_aprovacao.
        wl_zmmt0064-changeid   = wl_saida_itens-changeid.

        IF  wl_saida_cabecalho-ctra_superior IS NOT INITIAL.
          READ TABLE gt_ekpo INTO wl_ekpo WITH KEY werks = mw_werks-low
                                                   matnr = wl_zmmt0064-ematn BINARY SEARCH.
          IF sy-subrc = 0.
            wl_zmmt0064-evrtp_ctr = wl_ekpo-ebelp.
          ENDIF.
        ENDIF.

        APPEND wl_zmmt0064 TO gt_zmmt0064.
      ENDLOOP.

      LOOP AT gt_fields_style INTO wl_fields_style.
        r_utils->style_change( fieldname = wl_fields_style-fieldname
                               style     = cl_gui_alv_grid=>mc_style_disabled ).
      ENDLOOP.

      INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
      MODIFY gt_saida_itens FROM wl_saida_itens INDEX index.
    ENDLOOP.

    DELETE FROM zmmt0064 WHERE evrtn = wl_saida_cabecalho-evrtn.
    MODIFY zmmt0063 FROM wl_saida_cabecalho.
    MODIFY zmmt0064 FROM TABLE gt_zmmt0064.
    COMMIT WORK.

    MESSAGE TEXT-s01 TYPE 'S'.
  ENDMETHOD.                    "Z_SALVAR_REGISTROS

  METHOD set_deletion_flag.
    REFRESH: gt_fields_style, gt_estilo.

    "//Define os campos a serem desabilitados
    APPEND: 'EMATN' TO gt_fields_style,
            'EPSTP' TO gt_fields_style,
            'EVRTP' TO gt_fields_style,
            'KNTTP' TO gt_fields_style,
            'KTMNG' TO gt_fields_style,
            'LGORT' TO gt_fields_style,
            'MATKL' TO gt_fields_style,
            'MEINS' TO gt_fields_style,
            'MWSKZ' TO gt_fields_style,
            'NETPR' TO gt_fields_style,
            'TWRKZ' TO gt_fields_style,
            'TXZ01' TO gt_fields_style,
            'WERKS' TO gt_fields_style,
            'PEINH' TO gt_fields_style.

    SORT gt_fields_style BY fieldname.
    LOOP AT gt_fields_style INTO wl_fields_style.
      r_utils->style_change( fieldname = wl_fields_style-fieldname
                             style     = cl_gui_alv_grid=>mc_style_disabled ).
    ENDLOOP.

    "//Set deletion flag
    wl_saida_cabecalho-loekz = abap_true.

    LOOP AT gt_saida_itens ASSIGNING FIELD-SYMBOL(<fs_saida_itens>).
      IF wl_saida_cabecalho-ctra_superior IS INITIAL.
        <fs_saida_itens>-status   = icon_delete.
        <fs_saida_itens>-changeid = r_utils->c_crud-delete.
      ELSE.
        <fs_saida_itens>-status   = icon_yellow_light.
        <fs_saida_itens>-changeid = r_utils->c_crud-update.
      ENDIF.

      <fs_saida_itens>-estilo   = gt_estilo[].
    ENDLOOP.
  ENDMETHOD.

  METHOD search_registros.
    REFRESH: gt_fields_style,
             gt_saida_itens,
              r_werks[].

    CHECK ( i_evrtn IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_evrtn
      IMPORTING
        output = wl_saida_cabecalho-evrtn.

    SELECT SINGLE *
      FROM zmmt0063
      INTO wl_saida_cabecalho
     WHERE evrtn = wl_saida_cabecalho-evrtn.

    IF ( sy-subrc IS INITIAL ).
      CLEAR gt_fields.
      r_utils->tratar_campos( group1    = 'GR1'
                              value     = '0'
                              invisible = '0' ).

      r_utils->tratar_campos( group1    = 'GR2'
                              value     = '0'
                              invisible = '0' ).

      IF ( i_evrtp IS INITIAL ).
        SELECT *
          FROM zmmt0064
          INTO TABLE gt_zmmt0064
         WHERE evrtn = wl_saida_cabecalho-evrtn.
      ELSE.
        SELECT *
          FROM zmmt0064
          INTO TABLE gt_zmmt0064
         WHERE evrtn = wl_saida_cabecalho-evrtn
           AND evrtp = i_evrtp
           AND werks = i_werks.

      ENDIF.

      "// Define os campos a serem desabilitados *
      APPEND: 'EMATN' TO gt_fields_style,
              'EPSTP' TO gt_fields_style,
              'EVRTP' TO gt_fields_style,
              'KNTTP' TO gt_fields_style,
              'KTMNG' TO gt_fields_style,
              'LGORT' TO gt_fields_style,
              'MATKL' TO gt_fields_style,
              'MEINS' TO gt_fields_style,
              'MWSKZ' TO gt_fields_style,
              'NETPR' TO gt_fields_style,
              'TWRKZ' TO gt_fields_style,
              'TXZ01' TO gt_fields_style,
              'WERKS' TO gt_fields_style,
              'PEINH' TO gt_fields_style.

      SORT: gt_fields_style BY fieldname,
            gt_zmmt0064     BY evrtn evrtp.

      me->mt_zmmt0064 = gt_zmmt0064.
      DELETE ADJACENT DUPLICATES FROM gt_zmmt0064 COMPARING evrtp.

      r_werks-option = 'EQ'.
      r_werks-sign   = 'I'.

      LOOP AT gt_zmmt0064 INTO wl_zmmt0064.
        CLEAR: gt_estilo[], wl_saida_itens-estilo, wl_saida_itens-t_werks.

        LOOP AT me->mt_zmmt0064 INTO me->mw_zmmt0064 WHERE ( evrtn = wl_zmmt0064-evrtn )
                                                       AND ( evrtp = wl_zmmt0064-evrtp ).
          r_werks-low = me->mw_zmmt0064-werks.
          APPEND: r_werks TO wl_saida_itens-t_werks,
                  r_werks TO r_werks[].
        ENDLOOP.

        IF ( wl_zmmt0064-epstp NE '9' ).
          SELECT SINGLE maktx
            FROM makt
            INTO (wl_saida_itens-txz01)
           WHERE matnr = wl_zmmt0064-ematn
             AND spras = sy-langu.
        ELSE.
          wl_saida_itens-txz01 = wl_zmmt0064-txz01.
        ENDIF.

        SELECT SINGLE *
          FROM zmmt0064
          INTO @DATA(_aprovado)
         WHERE evrtn = @wl_zmmt0064-evrtn
           AND evrtp = @wl_zmmt0064-evrtp
           AND nivel = @c_status-aprovado.

        IF ( sy-subrc IS INITIAL ).
          IF wl_saida_cabecalho-loekz = abap_true.
            wl_saida_itens-status = icon_delete.
          ELSE.
            wl_saida_itens-status = icon_green_light.
          ENDIF.
        ELSE.
          SELECT SINGLE *
            FROM zmmt0064
            INTO @DATA(_reject)
           WHERE evrtn = @wl_zmmt0064-evrtn
             AND evrtp = @wl_zmmt0064-evrtp
             AND nivel = @c_status-rejeitado.

          IF sy-subrc IS INITIAL.
            wl_saida_itens-status = icon_red_light.
          ELSE.
            wl_saida_itens-status = icon_yellow_light.
          ENDIF.
        ENDIF.

*          CASE WL_ZMMT0064-NIVEL.
**          WHEN C_STATUS-PENDENTE.
**            WL_SAIDA_ITENS-STATUS = ICON_YELLOW_LIGHT.
*            WHEN C_STATUS-APROVADO.
*              WL_SAIDA_ITENS-STATUS = ICON_GREEN_LIGHT.
*            WHEN C_STATUS-REJEITADO.
*              WL_SAIDA_ITENS-STATUS = ICON_RED_LIGHT.
*          ENDCASE.
*        ENDIF.

*        IF ( WL_ZMMT0064-CHANGEID = R_UTILS->C_CRUD-DELETE ).
*          WL_SAIDA_ITENS-STATUS = ICON_DELETE.
*        ENDIF.

        r_utils->set_current_item( wl_zmmt0064-evrtp ).

        wl_saida_itens-evrtn    = wl_zmmt0064-evrtn.
        wl_saida_itens-evrtp    = wl_zmmt0064-evrtp.
        wl_saida_itens-evrtp_ctr = wl_zmmt0064-evrtp_ctr.
        wl_saida_itens-epstp    = wl_zmmt0064-epstp.
        wl_saida_itens-knttp    = wl_zmmt0064-knttp.
        wl_saida_itens-ematn    = wl_zmmt0064-ematn.
        wl_saida_itens-ktmng    = wl_zmmt0064-ktmng.
        wl_saida_itens-meins    = wl_zmmt0064-meins.
        wl_saida_itens-matkl    = wl_zmmt0064-matkl.
        wl_saida_itens-netpr    = wl_zmmt0064-netpr.
        wl_saida_itens-mwskz    = wl_zmmt0064-mwskz.
        wl_saida_itens-werks    = icon_te_costs_assign.
        wl_saida_itens-lgort    = wl_zmmt0064-lgort.
        wl_saida_itens-peinh    = wl_zmmt0064-peinh.
        wl_saida_itens-nivel    = wl_zmmt0064-nivel.
        wl_saida_itens-changeid = wl_zmmt0064-changeid.

        LOOP AT gt_fields_style INTO wl_fields_style.
          r_utils->style_change( fieldname = wl_fields_style-fieldname
                                 style     = cl_gui_alv_grid=>mc_style_disabled ).
        ENDLOOP.

        INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
        APPEND wl_saida_itens TO gt_saida_itens.
      ENDLOOP.

      SORT r_werks BY low.
      DELETE ADJACENT DUPLICATES FROM r_werks.

      r_utils->set_old_values(
        EXPORTING
          i_header = wl_saida_cabecalho
          i_itens  = gt_saida_itens
      ).

      btn_display_werks = icon_display_more.
    ELSE.
      MESSAGE TEXT-e08 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "SEARCH_REGISTROS
ENDCLASS.                    "CL_TIPO_OPERACAO IMPLEMENTATION

DATA r_operacao TYPE REF TO cl_tipo_operacao.

*&---------------------------------------------------------------------*
*&  Include           Z_CLASS_EVENT_TOOLBAR
*&---------------------------------------------------------------------*
CLASS lcl_event_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT obj_toolbar_manager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD set_toolbar.
    DATA: wl_toolbar TYPE stb_button,
          lv_disable TYPE c.

    DEFINE d_set.
      wl_toolbar-function     = &1.
      wl_toolbar-icon         = &2.
      wl_toolbar-butn_type    = &3.
      wl_toolbar-text         = &4.
      wl_toolbar-disabled     = &5.

      APPEND wl_toolbar TO e_object->mt_toolbar.
      CLEAR wl_toolbar.
    END-OF-DEFINITION.

    IF vg_op_mode = c_search.
      lv_disable = abap_true.
    ENDIF.

    d_set:
    'BTN_CARGA_EXCEL' icon_transfer   0 '' lv_disable, "// insert row
    'BTN_INSERT_ROW'  icon_insert_row 0 '' lv_disable, "// insert row
    'BTN_DELETE_ROW'  icon_delete_row 0 '' lv_disable, "// delete row
    ''                ''              3 '' lv_disable. "// separator

*    IF ( SY-DYNNR = 0110 OR
*         SY-DYNNR = 0100 ).

*      D_SET:
*      'BTN_CLAS_CONTABIL' ICON_DETAIL 0 'Classificações contábeis' LV_DISABLE. "// class contábil
*      'BTN_SERVICOS'      ICON_TOOLS  0 'Serviços' LV_DISABLE.                 "// serviços

*    ENDIF.

    CALL METHOD obj_toolbar_manager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    DATA: vl_status_header TYPE c,
          lw_zmmt0068      TYPE zmmt0068,
          vl_item          TYPE evrtp,
          wl_makt          TYPE makt.

    DATA: w_excel  LIKE LINE OF t_excel,
          w_excel2 LIKE LINE OF t_excel2,
          vematn   TYPE mara-matnr,
* ---> S4 Migration - 19/06/2023 - MA
*          VMSG(50).
          vmsg(70).
* <--- S4 Migration - 19/06/2023 - MA

    CLEAR t_excel.
    REFRESH t_excel.

    CASE e_ucomm.
      WHEN 'BTN_CARGA_EXCEL'.
        CASE sy-dynnr.
          WHEN 0110 OR 0100.
            IF p_file IS NOT INITIAL.

              TRY .

                  IF ( r_utils IS INITIAL ).
                    CREATE OBJECT r_utils.
                  ENDIF.
                  vl_status_header = r_utils->valida_cabecalho( i_arquivo = abap_true ).

                  CHECK ( vl_status_header = abap_true ).

                  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
                    EXPORTING
                      filename                = p_file
                      i_begin_col             = 1
                      i_begin_row             = 2
                      i_end_col               = 15
                      i_end_row               = 10000
                    TABLES
                      intern                  = t_excel
                    EXCEPTIONS
                      inconsistent_parameters = 1
                      upload_ole              = 2
                      OTHERS                  = 3.

                  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
                    EXPORTING
                      text = 'Atualizando Dados...'.

                  REFRESH gt_saida_itens.
                  r_utils->set_current_item( i_item = 0 ).

                  t_excel2[] = t_excel[].
                  SORT t_excel2 BY row col.
                  CLEAR t_excel2.
                  LOOP AT t_excel INTO w_excel.
                    IF w_excel-row = w_excel2-row.
                      CONTINUE.
                    ENDIF.
                    CLEAR w_carga.
                    LOOP AT t_excel2 INTO w_excel2 WHERE row = w_excel-row.
                      CASE w_excel2-col.
                        WHEN 1.
                          "Material
                          w_carga-ematn         = w_excel2-value.
                        WHEN 2.
                          "Quantidade
                          REPLACE REGEX '[.]' IN w_excel2-value WITH ''.
                          REPLACE REGEX '[,]' IN w_excel2-value WITH '.'.
                          w_carga-ktmng          = w_excel2-value.
                        WHEN 3.
                          "Valor
                          REPLACE REGEX '[.]' IN w_excel2-value WITH ''.
                          REPLACE REGEX '[,]' IN w_excel2-value WITH '.'.
                          w_carga-netpr           = w_excel2-value.
                        WHEN 4.
                          "Depísito
                          w_carga-lgort          = w_excel2-value.
                        WHEN 5.
                          "Iva
                          w_carga-mwskz          = w_excel2-value.
                        WHEN 6.
                          "Por
                          REPLACE REGEX '[.]' IN w_excel2-value WITH ''.
                          REPLACE REGEX '[,]' IN w_excel2-value WITH '.'.
                          w_carga-peinh = w_excel2-value.
                        WHEN 7.
                          "Filiais
                          IF w_excel2-value IS NOT INITIAL.
                            zcl_string=>split( EXPORTING i_str = CONV #( w_excel2-value ) i_quebra = ',' RECEIVING r_table  = DATA(r_table) ).
                            LOOP AT r_table INTO DATA(wa_table).
                              APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_table high = wa_table ) TO w_carga-t_werks.
                            ENDLOOP.
                          ENDIF.
                      ENDCASE.
                    ENDLOOP.
                    CONCATENATE 'Linha ' w_excel-row 'Material '  w_carga-ematn INTO vmsg SEPARATED BY space.

                    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
                      EXPORTING
                        text = vmsg.
                    CLEAR wl_saida_itens.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = w_carga-ematn
                      IMPORTING
                        output = w_carga-ematn.

                    vematn  =  w_carga-ematn.

                    SELECT SINGLE *
                    FROM mara
                    INTO @DATA(lw_mara)
                   WHERE matnr = @vematn.

                    SELECT SINGLE *
                     FROM makt
                     INTO wl_makt
                    WHERE matnr = w_carga-ematn
                      AND spras = sy-langu.

                    IF ( sy-subrc IS INITIAL ).
                      wl_saida_itens-ematn = wl_makt-matnr.
                      wl_saida_itens-txz01 = wl_makt-maktx.
                      wl_saida_itens-meins = lw_mara-meins.
                      wl_saida_itens-matkl = lw_mara-matkl.
                    ELSE.
                      wl_saida_itens-txz01 = 'Não cadastrado no SAP'.
                    ENDIF.

                    r_utils->get_current_item( IMPORTING e_item = vl_item ).
                    wl_saida_itens-evrtp = vl_item + 10.
                    wl_saida_itens-peinh = 1.

                    r_utils->set_current_item( i_item = wl_saida_itens-evrtp ).

                    "dados da planilha de carga
                    wl_saida_itens-ematn = w_carga-ematn."material
                    wl_saida_itens-ktmng = w_carga-ktmng. "Quantidade
                    wl_saida_itens-netpr = w_carga-netpr. "valou unit
                    wl_saida_itens-lgort = w_carga-lgort. "deposito
                    wl_saida_itens-mwskz = w_carga-mwskz. "iva
                    wl_saida_itens-peinh = w_carga-peinh. "Por
                    wl_saida_itens-t_werks  = w_carga-t_werks. "Filiais
                    "
                    wl_saida_itens-status   = icon_light_out.
                    wl_saida_itens-changeid = r_utils->c_crud-insert.
                    wl_saida_itens-werks    = icon_te_costs_assign.

                    IF r_werks[] IS INITIAL.
                      r_werks[] = w_carga-t_werks[].
                    ENDIF.

                    IF w_carga-t_werks[] IS INITIAL.
                      MESSAGE s836(sd) WITH TEXT-e14 DISPLAY LIKE 'E'.
                      CONTINUE.
                    ENDIF.

                    APPEND wl_saida_itens     TO gt_saida_itens.
                  ENDLOOP.
                  CALL METHOD obj_alv_0110->refresh_table_display
                    EXPORTING
                      is_stable = wl_stable.

                CATCH cx_root INTO DATA(exroot).
                  MESSAGE exroot->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
              ENDTRY.

            ENDIF.
        ENDCASE.

      WHEN 'BTN_INSERT_ROW'.
        CASE sy-dynnr.
          WHEN 0110 OR 0100.
            CLEAR wl_saida_itens.

            IF ( r_utils IS INITIAL ).
              CREATE OBJECT r_utils.
            ENDIF.

            vl_status_header = r_utils->valida_cabecalho( ).

            CHECK ( vl_status_header = abap_true ).

            r_utils->get_current_item( IMPORTING e_item = vl_item ).
            wl_saida_itens-evrtp = vl_item + 10.
            wl_saida_itens-peinh = 1.

            r_utils->set_current_item( i_item = wl_saida_itens-evrtp ).

            wl_saida_itens-status   = icon_light_out.
            wl_saida_itens-changeid = r_utils->c_crud-insert.
            wl_saida_itens-werks    = icon_te_costs_assign.

            APPEND LINES OF r_werks[] TO wl_saida_itens-t_werks.
            APPEND wl_saida_itens     TO gt_saida_itens.

            CALL METHOD obj_alv_0110->refresh_table_display
              EXPORTING
                is_stable = wl_stable.

          WHEN 0200.
*            CLEAR: GT_ESTILO[],
*                   "WL_ITEM_0200-STYLE,
*                   GT_FIELDS.
*
*            IF ( <FS_SAIDA_ITENS>-KNTTP NE 'F' ).
*              R_UTILS->STYLE_CHANGE( FIELDNAME = 'AUFNR'
*                                     STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).
*            ENDIF.
*
*            IF ( <FS_SAIDA_ITENS>-EPSTP NE 'K' ).
*              R_UTILS->STYLE_CHANGE( FIELDNAME = 'KOSTL'
*                                     STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).
*            ENDIF.
*
*            ADD 1 TO WL_ITEM_0200-EBELP.
*
*            INSERT LINES OF GT_ESTILO INTO TABLE WL_ITEM_0200-STYLE.
*            APPEND WL_ITEM_0200 TO GT_ITEM_0200.
*
*            IF ( WL_ITEM_0200-EBELP > 1 ).
*              R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
*                                      VALUE     = '1'
*                                      INVISIBLE = '0' ).
*
*              LEAVE TO SCREEN 0200.
*            ENDIF.
*
*            CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*              EXPORTING
*                IS_STABLE = WL_STABLE.
*
*            WRITE ''.

          WHEN 0300.

            ADD 1 TO vg_item_0400 .
            wl_saida_servicos-item_no = vg_item_0400.

            APPEND wl_saida_servicos TO gt_saida_servicos.

            CALL METHOD obj_alv_0400->refresh_table_display
              EXPORTING
                is_stable = wl_stable.

        ENDCASE.

      WHEN 'BTN_DELETE_ROW'.
        CLEAR: gt_selected_rows, gt_fields.

        CASE sy-dynnr.
          WHEN 0100 OR 0110.
            CALL METHOD obj_alv_0110->get_selected_rows
              IMPORTING
                et_index_rows = gt_selected_rows.

            REFRESH gt_fields_style.
            APPEND: 'EMATN' TO gt_fields_style,
                    'EPSTP' TO gt_fields_style,
                    'EVRTP' TO gt_fields_style,
                    'KNTTP' TO gt_fields_style,
                    'KTMNG' TO gt_fields_style,
                    'LGORT' TO gt_fields_style,
                    'MATKL' TO gt_fields_style,
                    'MEINS' TO gt_fields_style,
                    'MWSKZ' TO gt_fields_style,
                    'NETPR' TO gt_fields_style,
                    'TWRKZ' TO gt_fields_style,
                    'TXZ01' TO gt_fields_style,
                    'WERKS' TO gt_fields_style.

            SORT gt_selected_rows STABLE DESCENDING.
            LOOP AT gt_selected_rows INTO wl_selected_rows.
              READ TABLE gt_saida_itens INTO wl_saida_itens INDEX wl_selected_rows-index.

              IF NOT ( wl_saida_cabecalho-ctra_superior IS INITIAL ).
                CONCATENATE TEXT-w03 wl_saida_itens-evrtp '?' INTO r_utils->at_question
                SEPARATED BY space.

                r_utils->popup_to_confirm( EXPORTING
                                           i_title    = 'Eliminar item'
                                           i_question = r_utils->at_question
                                           IMPORTING
                                           e_answer   = r_utils->at_answer ).

                CLEAR: gt_estilo[], wl_saida_itens-estilo.

                LOOP AT gt_fields_style INTO wl_fields_style.
                  r_utils->style_change( fieldname = wl_fields_style-fieldname
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
                ENDLOOP.

                wl_saida_itens-status   = icon_delete.
                wl_saida_itens-changeid = r_utils->c_crud-delete.

                INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
                MODIFY gt_saida_itens FROM wl_saida_itens INDEX wl_selected_rows-index.

              ELSE.
                r_utils->get_current_item( IMPORTING e_item = vl_item ).

                vl_item = vl_item - 10.

                r_utils->set_current_item( EXPORTING i_item = vl_item ).
                DELETE gt_saida_itens INDEX wl_selected_rows-index.
              ENDIF.
            ENDLOOP.

            CALL METHOD obj_alv_0110->refresh_table_display
              EXPORTING
                is_stable = wl_stable.

          WHEN 0200.
*            CALL METHOD OBJ_ALV_0200->GET_SELECTED_CELLS
*              IMPORTING
*                ET_CELL = GT_SELECTEDCELL.
*
*            LOOP AT GT_SELECTEDCELL INTO WL_SELECTEDCELL.
*              SUBTRACT 1 FROM WL_ITEM_0200-EBELP.
*              DELETE GT_ITEM_0200 INDEX WL_SELECTEDCELL-ROW_ID-INDEX.
*
*              IF ( WL_ITEM_0200-EBELP = 1 ).
*                CLEAR: <FS_SAIDA_ITENS>-VRTKZ,
*                       <FS_SAIDA_ITENS>-TWRKZ.
*
*                R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
*                                        VALUE     = '0'
*                                        INVISIBLE = '0' ).
*
*                LEAVE TO SCREEN 0200.
*              ENDIF.
*            ENDLOOP.
*
*            CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*              EXPORTING
*                IS_STABLE = WL_STABLE.
          WHEN 0300.
            CALL METHOD obj_alv_0400->get_selected_cells
              IMPORTING
                et_cell = gt_selectedcell.

            LOOP AT gt_selectedcell INTO wl_selectedcell.
              vg_item_0400 = vg_item_0400 - 1.
              DELETE gt_saida_servicos INDEX wl_selectedcell-row_id-index.
            ENDLOOP.

            CALL METHOD obj_alv_0400->refresh_table_display
              EXPORTING
                is_stable = wl_stable.
        ENDCASE.

      WHEN 'BTN_CLAS_CONTABIL' OR
           'BTN_SERVICOS'.

        REFRESH gt_selected_rows.

        CALL METHOD obj_alv_0110->get_selected_rows
          IMPORTING
            et_index_rows = gt_selected_rows.

        DESCRIBE TABLE gt_selected_rows LINES vg_lines.

        IF ( vg_lines IS INITIAL ).
          MESSAGE TEXT-e05 TYPE 'I' DISPLAY LIKE 'E'.
        ELSEIF ( vg_lines >= 2 ).
          MESSAGE TEXT-e06 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.

          LOOP AT gt_selected_rows INTO wl_selected_rows.
            READ TABLE gt_saida_itens ASSIGNING <fs_saida_itens> INDEX wl_selected_rows-index.
          ENDLOOP.

          CASE e_ucomm.
            WHEN 'BTN_CLAS_CONTABIL'.
*              CLEAR GT_ITEM_0200.
*
*              IF ( NOT <FS_SAIDA_ITENS>-KNTTP IS INITIAL ).
*                SORT GT_ITEM_0200 BY EBELP.
*
*                APPEND LINES OF <FS_SAIDA_ITENS>-ITENS_CLAS TO GT_ITEM_0200.
*                DESCRIBE TABLE GT_ITEM_0200 LINES WL_ITEM_0200-EBELP.
*
*                IF ( WL_ITEM_0200-EBELP < 2 ).
*                  CLEAR GT_FIELDS.
*                  R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
*                                          VALUE     = '0'
*                                          INVISIBLE = '0' ).
*                ENDIF.
*                CALL SCREEN 0200 STARTING AT 5 5.
*
*              ELSE.
*                MESSAGE TEXT-E11 TYPE 'S' DISPLAY LIKE 'E'.
*              ENDIF.

            WHEN 'BTN_SERVICOS'.
              IF ( wl_saida_itens_aux-epstp EQ 'D' ).
                CALL SCREEN 0300 STARTING AT 5 5.
              ELSE.
                MESSAGE TEXT-e12 TYPE 'S' DISPLAY LIKE 'E'.
              ENDIF.
          ENDCASE.

        ENDIF.
    ENDCASE.

*    CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WL_STABLE.
  ENDMETHOD.                    "GET_UCOMM

  METHOD on_click.
    READ TABLE gt_saida_itens INTO wl_saida_itens INDEX e_row_id.
    CASE e_column_id.
      WHEN 'STATUS'.
*        CHECK WL_SAIDA_ITENS-STATUS = ICON_RED_LIGHT.

        r_utils->get_history_change( EXPORTING
                                     i_evrtn   = wl_saida_cabecalho-evrtn
                                     i_evrtp   = wl_saida_itens-evrtp ).

*        DATA(_LAST_LOG) = R_UTILS->MT_ZMMT0068[ LINES( R_UTILS->MT_ZMMT0068 ) ]-

*        DELETE R_UTILS->MT_ZMMT0068
*         WHERE CHANGEID NE R_UTILS->C_NIVEL-REJEITADO
*           AND CHANGEID NE R_UTILS->C_NIVEL-APROVADO.

        CLEAR r_utils->mt_history_out.
        LOOP AT r_utils->mt_zmmt0068 INTO r_utils->mw_zmmt0068.
          CASE r_utils->mw_zmmt0068-changeid.
            WHEN r_utils->c_crud-delete.
              r_utils->mw_history_out-action = 'Delet.'.
            WHEN r_utils->c_crud-insert.
              r_utils->mw_history_out-action = 'Inser.'.
            WHEN r_utils->c_crud-update.
              r_utils->mw_history_out-action = 'Modif.'.
            WHEN r_utils->c_nivel-aprovado.
              r_utils->mw_history_out-action = 'Aprov.'.
            WHEN r_utils->c_nivel-rejeitado.
              r_utils->mw_history_out-action = 'Rejei.'.
          ENDCASE.

          IF r_utils->mw_history_out-action <> 'Rejei.'.
            r_utils->mw_history_out-value_new  = r_utils->mw_zmmt0068-value_new.
          ENDIF.

          r_utils->mw_history_out-evrtp      = r_utils->mw_zmmt0068-evrtp.
          r_utils->mw_history_out-ftext      = r_utils->mw_zmmt0068-ftext.
          r_utils->mw_history_out-value_old  = r_utils->mw_zmmt0068-value_old.
          r_utils->mw_history_out-username   = r_utils->mw_zmmt0068-username.
          r_utils->mw_history_out-data       = r_utils->mw_zmmt0068-data.
          r_utils->mw_history_out-hora       = r_utils->mw_zmmt0068-hora.

          APPEND r_utils->mw_history_out TO r_utils->mt_history_out.
          CLEAR r_utils->mw_history_out.
        ENDLOOP.

        IF NOT ( r_utils->mt_history_out IS INITIAL ).
          r_utils->show_history_change( EXPORTING i_pfstatus = '0610'
                                        CHANGING  c_table    = r_utils->mt_history_out ).

          TRY.
              r_utils->mo_column ?= r_utils->mo_columns->get_column( 'CHANGEID' ).
              r_utils->mo_column->set_visible( abap_false ).

              r_utils->mo_column ?= r_utils->mo_columns->get_column( 'VALUE_OLD' ).
              r_utils->mo_column->set_long_text( 'Valor antigo/descrição' ).
              r_utils->mo_column->set_medium_text( 'Vlr antigo/descrição' ).

              r_utils->mo_column ?= r_utils->mo_columns->get_column( 'VALUE_NEW' ).
              r_utils->mo_column->set_long_text( 'Centro' ).
            CATCH cx_salv_not_found.
          ENDTRY.

          r_utils->mo_alv->display( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "ON_CLICK
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

*&---------------------------------------------------------------------*
*&  Include           Z_CLASS_LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key,

      handle_button_click FOR EVENT button_click OF cl_gui_column_tree
        IMPORTING node_key item_name,

      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA ls_good TYPE lvc_s_modi.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE sy-dynnr.
        WHEN 0100.
          READ TABLE gt_saida_itens INTO wl_saida_itens INDEX ls_good-row_id.

          wl_saida_itens-status = icon_light_out.

          CASE ls_good-fieldname.
            WHEN 'EPSTP'. "CATEGORIA ITEM
              REFRESH gt_estilo[].
              wl_saida_itens-epstp = ls_good-value.

              DELETE wl_saida_itens-estilo WHERE fieldname = 'EMATN'.
              DELETE wl_saida_itens-estilo WHERE fieldname = 'TXZ01'.

              IF ( ls_good-value = '9' ). "// Prestação de serviço;
                r_utils->style_change( fieldname = 'EMATN'
                                       style     = cl_gui_alv_grid=>mc_style_disabled ).

                r_utils->style_change( fieldname = 'TXZ01'
                                       style     = cl_gui_alv_grid=>mc_style_enabled ).

                INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
              ELSE.
                r_utils->style_change( fieldname = 'EMATN'
                                       style     = cl_gui_alv_grid=>mc_style_enabled ).

                r_utils->style_change( fieldname = 'TXZ01'
                                       style     = cl_gui_alv_grid=>mc_style_disabled ).

                INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
              ENDIF.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING epstp estilo status.

            WHEN 'KNTTP'.
              wl_saida_itens-knttp = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING knttp status.

            WHEN 'EMATN'.

              SELECT SINGLE *
                FROM mara
                INTO @DATA(lw_mara)
               WHERE matnr = @ls_good-value.

*              SELECT SINGLE LGORT
*                FROM MARD
*                INTO WL_SAIDA_ITENS-LGORT
*               WHERE MATNR = LS_GOOD-VALUE.

              SELECT SINGLE *
                FROM makt
                INTO wl_makt
               WHERE matnr = ls_good-value
                 AND spras = sy-langu.

              IF ( sy-subrc IS INITIAL ).
                wl_saida_itens-ematn = wl_makt-matnr.
                wl_saida_itens-txz01 = wl_makt-maktx.
                wl_saida_itens-meins = lw_mara-meins.
                wl_saida_itens-matkl = lw_mara-matkl.
              ELSE.
                MESSAGE s836(sd) WITH TEXT-e03 DISPLAY LIKE 'E'.
                LEAVE TO SCREEN 0100.
              ENDIF.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING ematn txz01 status meins matkl lgort.

            WHEN 'TXZ01'. "DESCR MATERIAL
              wl_saida_itens-txz01 = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING txz01 status.

            WHEN 'KTMNG'. "QTD PREVISTA
              wl_saida_itens-ktmng = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING ktmng status.

            WHEN 'NETPR'. "PREÇO LIQUIDO
              wl_saida_itens-netpr = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING netpr status.

            WHEN 'PEINH'. "unidade de preço
              wl_saida_itens-peinh = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING peinh status.
            WHEN 'MEINS'. "UNIDADE MEDIDA
              wl_saida_itens-meins = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING meins status.

            WHEN 'MATKL'. "GRUPO MERCADORIA
              wl_saida_itens-matkl = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING matkl status.

            WHEN 'CENTRO'. "CENTRO
*              WL_SAIDA_ITENS-CENTRO = LS_GOOD-VALUE.

*              MODIFY GT_SAIDA_ITENS FROM WL_SAIDA_ITENS INDEX LS_GOOD-ROW_ID
*              TRANSPORTING CENTRO.

            WHEN 'NR_ORDEM'. "NUMERO ORDEM
*              WL_SAIDA_ITENS-NR_ORDEM = LS_GOOD-VALUE.

*              MODIFY GT_SAIDA_ITENS FROM WL_SAIDA_ITENS INDEX LS_GOOD-ROW_ID
*              TRANSPORTING NR_ORDEM.

            WHEN 'MWSKZ'. "CÓDIGO IVA
              wl_saida_itens-mwskz = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING mwskz status.

            WHEN 'SAKTO'.
              wl_saida_itens-sakto  = ls_good-value.

              MODIFY gt_saida_itens FROM wl_saida_itens INDEX ls_good-row_id
              TRANSPORTING sakto status.

            WHEN OTHERS.
              EXIT.
          ENDCASE.

          CALL METHOD obj_alv_0110->refresh_table_display
            EXPORTING
              is_stable = wl_stable.

*        WHEN 0200.
*          READ TABLE GT_ITEM_0200 INTO WL_ITEM_0200 INDEX LS_GOOD-ROW_ID.
*
*          CASE LS_GOOD-FIELDNAME.
*            WHEN 'VPROZ'.
*
*              "// Verifica se possuí + de 2 itens para aceitar (%)porcentagem
*              IF ( WL_ITEM_0200-EBELP = 1 ).
*                MESSAGE S836(SD) WITH TEXT-E07 DISPLAY LIKE 'E'.
*                LEAVE TO SCREEN 0200.
*              ELSE.
*                WL_ITEM_0200-VPROZ = LS_GOOD-VALUE.
*              ENDIF.
*
*              MODIFY GT_ITEM_0200 FROM WL_ITEM_0200 INDEX LS_GOOD-ROW_ID
*              TRANSPORTING VPROZ.
*
*              CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*                EXPORTING
*                  IS_STABLE = WL_STABLE.
*
*            WHEN OTHERS.
*              EXIT.
*          ENDCASE.
      ENDCASE.
    ENDLOOP.

    CLEAR: ls_good, er_data_changed->mt_good_cells.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD handle_double_click.
    CONSTANTS: c_create_contrato  TYPE n VALUE 1,
               c_release_contrato TYPE n VALUE 2.

    CASE node_key.
      WHEN c_create_contrato.
        vg_screen_principal = '0110'.
      WHEN c_release_contrato.
        vg_screen_principal = '0120'.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD on_button_click.
    DATA lv_disabled VALUE abap_off.

    IF ( es_col_id = 'WERKS' ).
      READ TABLE gt_saida_itens ASSIGNING <fs_saida_itens> INDEX es_row_no-row_id.

      IF ( <fs_saida_itens>-estilo[] IS NOT INITIAL )
      OR ( <fs_saida_itens>-status   EQ icon_green_light ).
        lv_disabled = abap_on.
      ENDIF.

      DATA(_old_centros) = wl_saida_itens-t_werks.
      PERFORM f_selections_dialog TABLES <fs_saida_itens>-t_werks
                                   USING space
                                         lv_disabled
                                         space.

      CHECK lv_disabled = abap_false.
      DELETE ADJACENT DUPLICATES FROM <fs_saida_itens>-t_werks.

      TRY.
          LOOP AT <fs_saida_itens>-t_werks INTO r_utils->mw_werks.
            IF ( r_utils->mw_werks-low NOT IN r_werks[] ).
              RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
            ENDIF.
          ENDLOOP.

          IF <fs_saida_itens>-t_werks <> _old_centros.
            <fs_saida_itens>-status = icon_light_out.
          ENDIF.

        CATCH cx_sy_itab_line_not_found.
          MESSAGE TEXT-e12 TYPE 'I' DISPLAY LIKE 'E'.
          <fs_saida_itens>-t_werks = _old_centros.
      ENDTRY.
    ENDIF.

    CALL METHOD obj_alv_0110->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "ON_BUTTON_CLICK

  METHOD handle_button_click.
    DATA: lw_item_table   TYPE mtreeitm,
          lt_nodes_select TYPE lvc_t_nkey,
          lw_zmmt0064     TYPE zmmt0064.

    IF ( r_utils IS INITIAL ).
      CREATE OBJECT r_utils.
    ENDIF.

    CREATE OBJECT r_operacao.

    "// Obtem nº Contrato/Item/Centro;
    TRY.
        r_utils->at_evrtn = gt_item_table[ item_name = 'Contrato' node_key  = node_key ]-text.
        r_utils->at_werks = gt_item_table[ item_name = 'Centro'   node_key  = node_key ]-text.
        r_utils->at_evrtp = gt_item_table[ item_name = 'Item'     node_key  = node_key ]-text.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    "// ---

    CASE item_name.
      WHEN 'Display'. "//Display

        "//Define a aba "Criar/visualizar contratos" como atual p/ visualizar o contrato
        APPEND 1 TO lt_nodes_select.
        obj_alv_tree_0100->set_selected_nodes( lt_nodes_select ).

        r_utils->get_history_change( EXPORTING
                                     i_evrtn   = r_utils->at_evrtn
                                     i_evrtp   = r_utils->at_evrtp
                                     i_werks   = r_utils->at_werks
                                    ).

        DELETE r_utils->mt_zmmt0068 WHERE fname     EQ 'WERKS'
                                      AND value_new NE r_utils->at_werks.

        CLEAR r_utils->mt_history_out.
        LOOP AT r_utils->mt_zmmt0068 INTO r_utils->mw_zmmt0068.
          CASE r_utils->mw_zmmt0068-changeid.
            WHEN r_utils->c_crud-delete.
              r_utils->mw_history_out-action = 'Delet.'.
            WHEN r_utils->c_crud-insert.
              r_utils->mw_history_out-action = 'Inser.'.
            WHEN r_utils->c_crud-update.
              r_utils->mw_history_out-action = 'Modif.'.
            WHEN r_utils->c_nivel-aprovado.
              r_utils->mw_history_out-action = 'Aprov.'.
            WHEN r_utils->c_nivel-rejeitado.
              r_utils->mw_history_out-action = 'Rejei.'.
          ENDCASE.

          IF r_utils->mw_history_out-action <> 'Rejei.'.
            r_utils->mw_history_out-value_new  = r_utils->mw_zmmt0068-value_new.
          ENDIF.

          r_utils->mw_history_out-evrtp      = r_utils->mw_zmmt0068-evrtp.
          r_utils->mw_history_out-ftext      = r_utils->mw_zmmt0068-ftext.
          r_utils->mw_history_out-value_old  = r_utils->mw_zmmt0068-value_old.
          r_utils->mw_history_out-username   = r_utils->mw_zmmt0068-username.
          r_utils->mw_history_out-data       = r_utils->mw_zmmt0068-data.
          r_utils->mw_history_out-hora       = r_utils->mw_zmmt0068-hora.

          APPEND r_utils->mw_history_out TO r_utils->mt_history_out.
          CLEAR r_utils->mw_history_out.
        ENDLOOP.

        r_utils->show_history_change(
          EXPORTING i_pfstatus = '0600'
          CHANGING  c_table    = r_utils->mt_history_out ).

        r_utils->mo_alv->display( ).

        IF ( sy-ucomm = c_view_item ).
          vg_op_mode  = c_search.
          r_operacao->search_registros( i_evrtn = r_utils->at_evrtn
                                        i_evrtp = r_utils->at_evrtp
                                        i_werks = r_utils->at_werks ).
          vg_screen_principal = '0110'.
        ENDIF.

      WHEN 'Allow'. "//Aprovar
        DATA: lv_evrtn     TYPE evrtn,
              lv_pendente  VALUE abap_false,
              lv_rejeitado VALUE abap_false.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 1
            text       = TEXT-w04.

        lv_evrtn = r_utils->at_evrtn.
        SHIFT lv_evrtn LEFT DELETING LEADING '0'.

        IF vg_op_mode <> c_allow_all.
          r_utils->popup_to_confirm( EXPORTING
                                     i_title    = |Aprovar Contrato nº - { lv_evrtn }|
                                     i_question = |Deseja aprovar o contrato: { lv_evrtn } item: { r_utils->at_evrtp } centro: { r_utils->at_werks }?|
                                     IMPORTING
                                     e_answer   = r_utils->at_answer ).

          CHECK ( r_utils->at_answer = 1 ). "//Sim
        ENDIF.

        UPDATE zmmt0064 SET nivel = r_operacao->c_status-aprovado
                      WHERE evrtn = r_utils->at_evrtn
                        AND evrtp = r_utils->at_evrtp
                        AND werks = r_utils->at_werks.
        COMMIT WORK.

        SELECT SINGLE *
          FROM zmmt0064
          INTO lw_zmmt0064
         WHERE evrtn = r_utils->at_evrtn
           AND nivel = r_operacao->c_status-pendente.

        IF ( sy-subrc IS INITIAL ).
          lv_pendente = abap_true.
        ENDIF.

        SELECT SINGLE *
          FROM zmmt0064
          INTO lw_zmmt0064
         WHERE evrtn = r_utils->at_evrtn
           AND nivel = r_operacao->c_status-rejeitado.

        IF ( sy-subrc IS INITIAL ).
          lv_rejeitado = abap_true.
        ENDIF.

        IF ( lv_pendente  IS INITIAL ) AND
           ( lv_rejeitado IS INITIAL ).
          DATA(msg_errors) = r_operacao->contract_generate( i_evrtn = r_utils->at_evrtn ).
        ENDIF.

        IF ( msg_errors IS INITIAL ).
          r_utils->save_history_change( i_fieldname = 'EVRTN'
                              i_fieldtxt  = 'Contrato'
                              i_change_id = r_operacao->c_status-aprovado
                              i_value_new = r_utils->at_werks
                            ).
        ELSE.
          DATA all_werks TYPE string.
*
*          SELECT *
*            FROM ZMMT0064
*            INTO TABLE @DATA(_CONTRATOS)
*           WHERE EVRTN = @R_UTILS->AT_EVRTN
*             AND NIVEL = @R_OPERACAO->C_STATUS-PENDENTE.
*
*          LOOP AT _CONTRATOS INTO DATA(_CONTRATO).
*            CONCATENATE ALL_WERKS _CONTRATO-WERKS INTO ALL_WERKS SEPARATED BY ','.
*          ENDLOOP.

*          ALL_WERKS = ALL_WERKS+1.



          LOOP AT msg_errors INTO DATA(msg_error).
            r_utils->save_history_change( i_fieldname = 'EVRTN'
                                          i_fieldtxt  = 'Standard BAPI'
                                          i_change_id = r_operacao->c_status-rejeitado
                                          i_value_old = msg_error-message
                                          i_value_new = all_werks
                                        ).
          ENDLOOP.

          UPDATE zmmt0064 SET nivel = r_operacao->c_status-pendente "Reinicializa para poder refazer mesmo gerado parcialmente em outra aprovação, pois o contrato será alterado por inteiro
                 WHERE evrtn = r_utils->at_evrtn
                 AND   nivel = r_operacao->c_status-aprovado.

*          UPDATE ZMMT0064 SET NIVEL = R_OPERACAO->C_STATUS-REJEITADO
*           WHERE EVRTN = R_UTILS->AT_EVRTN
*           AND   NIVEL = R_OPERACAO->C_STATUS-PENDENTE.

          COMMIT WORK.
        ENDIF.

        IF vg_op_mode <> c_allow_all.
          IF msg_errors IS INITIAL.
            MESSAGE |Contrato { r_utils->at_evrtn }, item { r_utils->at_evrtp }, centro { r_utils->at_werks }, aprovado com sucesso.| TYPE 'S'.
          ELSE.
            MESSAGE |Contrato { r_utils->at_evrtn }, item { r_utils->at_evrtp }, centro { r_utils->at_werks }, aprovado com erros.|   TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.
          obj_alv_tree_0120->delete_node( node_key ).
        ENDIF.

      WHEN 'Reject'. "//Rejeitar
        CLEAR r_utils->mt_text.
        r_utils->text_edit_dialog( EXPORTING
                                   i_displaymode = abap_off
                                   CHANGING
                                   ch_text       = r_utils->mt_text ).

        IF ( sy-ucomm = 'CX_CONT' ).

          IF NOT ( r_utils->mt_text IS INITIAL ).
            r_utils->save_text( i_text = r_utils->mt_text ).

            r_utils->save_history_change( i_fieldname = 'EVRTN'
                                          i_fieldtxt  = 'Contrato'
                                          i_change_id = r_operacao->c_status-rejeitado
                                          i_value_old = r_utils->mw_lines-tdline
                                          i_value_new = r_utils->at_werks
                                        ).

            UPDATE zmmt0064 SET nivel = r_operacao->c_status-rejeitado
                          WHERE evrtn = r_utils->at_evrtn
                            AND evrtp = r_utils->at_evrtp
                            AND werks = r_utils->at_werks.
            COMMIT WORK.

            obj_alv_tree_0120->delete_node( node_key ).

            MESSAGE |Contrato { r_utils->at_evrtn }, item { r_utils->at_evrtp }, centro { r_utils->at_werks }, rejeitado com sucesso.|
               TYPE 'S'.
          ELSE.
            MESSAGE TEXT-e10 TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

    ENDCASE.
  ENDMETHOD.                    "handle_item_double_click

  METHOD on_onf4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,
             fieldname  TYPE dd03l-fieldname,
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    DATA: gt_valuetab LIKE TABLE OF wl_valuetab,
          gt_field    TYPE TABLE OF ty_field,
          gt_value    TYPE TABLE OF ty_value,
          gt_t163y    TYPE TABLE OF t163y,
          gt_t163i    TYPE TABLE OF t163i,
          gt_t006a    TYPE TABLE OF t006a,
          gt_t023t    TYPE TABLE OF t023t,
          gt_aufk     TYPE TABLE OF aufk,
          gt_skb1     TYPE TABLE OF skb1,
          gt_t007s    TYPE TABLE OF t007s.

    DATA: wl_value         TYPE ty_value,
          wl_field         TYPE ty_field,
          wl_index         TYPE sy-tabix,
          wl_t163y         TYPE t163y,
          wl_t163i         TYPE t163i,
          wl_t006a         TYPE t006a,
          wl_t023t         TYPE t023t,
          wl_skb1          TYPE skb1,
          wl_aufk          TYPE aufk,
          wl_t007s         TYPE t007s,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

    CASE sy-dynnr.
      WHEN 0100.
        READ TABLE gt_saida_itens INTO wl_saida_itens INDEX es_row_no-row_id.
*
*        CASE E_FIELDNAME.
*          WHEN 'MATERIAL'.
*
*            SELECT MATNR
*                  FROM MARA
*                  INTO CORRESPONDING FIELDS OF TABLE GT_MARA.
*
*            SELECT DISTINCT MAKTX MATNR
*              FROM MAKT
*              INTO CORRESPONDING FIELDS OF TABLE GT_MAKT
*           FOR ALL ENTRIES IN GT_MARA
*             WHERE MATNR = GT_MARA-MATNR.
*
*            WL_FIELDNAME = 'MATNR'.
*            WL_TABNAME   = 'MARA'.
*
*            SORT GT_MAKT BY MATNR.
*            LOOP AT GT_MARA INTO WL_MARA.
*              READ TABLE GT_MAKT INTO WL_MAKT WITH KEY MATNR = WL_MARA-MATNR
*                  BINARY SEARCH.
*
*              MOVE WL_MARA-MATNR TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_MAKT-MAKTX TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MATNR'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = 'MAKT'.
*            WL_FIELD-FIELDNAME = 'MAKTX'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*          WHEN 'CTG_ITEM'.
*            SELECT *
*              FROM T163Y
*              INTO TABLE GT_T163Y
*             WHERE SPRAS = SY-LANGU.
*
*            WL_FIELDNAME = 'EPSTP'.
*            WL_TABNAME   = 'T163Y'.
*
*            LOOP AT GT_T163Y INTO WL_T163Y.
*              MOVE WL_T163Y-EPSTP TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T163Y-PTEXT TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'EPSTP'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'PTEXT'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.

*          WHEN 'CTG_CLAS'.
*            SELECT *
*              FROM T163I
*              INTO TABLE GT_T163I
*             WHERE SPRAS EQ 'PT'.
*
*            WL_FIELDNAME = 'KNTTP'.
*            WL_TABNAME   = 'T163I'.
*
*            LOOP AT GT_T163I INTO WL_T163I.
*              MOVE WL_T163I-KNTTP TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T163I-KNTTX TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'KNTTP'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'KNTTX'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.

*          WHEN 'UNID_MED'.
*            SELECT *
*              FROM T006A
*              INTO TABLE GT_T006A
*             WHERE SPRAS = 'PT'.
*
*            WL_FIELDNAME = 'MSEHI'.
*            WL_TABNAME   = 'T006A'.
*
*            LOOP AT GT_T006A INTO WL_T006A.
*              MOVE WL_T006A-MSEHI TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T006A-MSEH3 TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T006A-MSEH6 TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MSEHI'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MSEH3'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MSEH6'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*          WHEN 'MATKL'.
*            SELECT *
*              FROM T023T
*              INTO TABLE GT_T023T
*              WHERE SPRAS = 'PT'.
*
*            WL_FIELDNAME = 'MATKL'.
*            WL_TABNAME   = 'T023T'.
*
*            LOOP AT GT_T023T INTO WL_T023T.
*              MOVE WL_T023T-MATKL TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T023T-WGBEZ TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MATKL'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'WGBEZ'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*          WHEN 'MWSKZ'.
*            SELECT *
*              FROM T007S
*              INTO TABLE GT_T007S
*             WHERE SPRAS = 'PT'.
*
*            WL_FIELDNAME = 'MWSKZ'.
*            WL_TABNAME   = 'T007S'.
*
*            LOOP AT GT_T007S INTO WL_T007S.
*              MOVE WL_T007S-MWSKZ TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_T007S-TEXT1 TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'MWSKZ'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'TEXT1'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*          WHEN OTHERS.
*        ENDCASE.

      WHEN 0200.
*        READ TABLE GT_ITEM_0200 INTO WL_ITEM_0200 INDEX ES_ROW_NO-ROW_ID.
**
*        CASE E_FIELDNAME.
*          WHEN 'SAKTO'.
*            SELECT *
*              FROM SKB1
*              INTO TABLE GT_SKB1
*             WHERE BUKRS = WL_SAIDA_CABECALHO-BUKRS
*               AND FSTAG = 'YB06'.
*
*            WL_FIELDNAME = 'SAKNR'.
*            WL_TABNAME   = 'SKB1'.
*
*            LOOP AT GT_SKB1 INTO WL_SKB1.
*              MOVE WL_SKB1-SAKNR TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_SKB1-FSTAG TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*
*              MOVE WL_SKB1-STEXT TO WL_VALUETAB-FIELD.
*              APPEND WL_VALUETAB TO GT_VALUETAB.
*            ENDLOOP.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'SAKTO'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'FSTAG'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*
*            WL_FIELD-TABNAME   = WL_TABNAME.
*            WL_FIELD-FIELDNAME = 'STEXT'.
*            WL_FIELD-S         = 'X'.
*            APPEND WL_FIELD TO GT_FIELD.
*          WHEN OTHERS.
*        ENDCASE.
    ENDCASE.
*
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        cucol                     = '10'
        curow                     = '5'
        fieldname                 = wl_fieldname
        tabname                   = wl_tabname
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = gt_field
        select_values             = gt_value
        valuetab                  = gt_valuetab
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

    CHECK ( vg_op_mode <> c_search ).
*
    CASE sy-dynnr.
      WHEN 0100.
        READ TABLE gt_saida_itens INTO wl_saida_itens INDEX es_row_no-row_id.
        CASE e_fieldname.
          WHEN 'CTG_ITEM'.
            READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'EPSTP'.
            MOVE wl_value-char79 TO wl_saida_itens-epstp.

            REFRESH gt_estilo[].

            DELETE wl_saida_itens-estilo WHERE fieldname = 'MATERIAL'.
            DELETE wl_saida_itens-estilo WHERE fieldname = 'SHORT_TXT'.

            IF ( wl_saida_itens-epstp = 'D' ).
              r_utils->style_change( fieldname = 'MATERIAL'
                                     style     = cl_gui_alv_grid=>mc_style_disabled ).

              r_utils->style_change( fieldname = 'SHORT_TXT'
                                     style     = cl_gui_alv_grid=>mc_style_enabled ).

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
            ELSE.
              r_utils->style_change( fieldname = 'MATERIAL'
                                     style     = cl_gui_alv_grid=>mc_style_enabled ).

              r_utils->style_change( fieldname = 'SHORT_TXT'
                                     style     = cl_gui_alv_grid=>mc_style_disabled ).

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_itens-estilo.
            ENDIF.

            MODIFY gt_saida_itens FROM wl_saida_itens INDEX es_row_no-row_id
                   TRANSPORTING epstp estilo.

          WHEN 0200.
*            READ TABLE GT_ITEM_0200 INTO WL_ITEM_0200 INDEX ES_ROW_NO-ROW_ID.
*            CASE E_FIELDNAME.
*              WHEN 'SAKTO'.
*                READ TABLE GT_VALUE INTO WL_VALUE WITH KEY FIELDNAME = 'SAKTO'.
*                MOVE WL_VALUE-CHAR79 TO WL_ITEM_0200-SAKTO.
*
*                MODIFY GT_ITEM_0200 FROM WL_ITEM_0200 INDEX ES_ROW_NO-ROW_ID
*                       TRANSPORTING SAKTO.
*            ENDCASE.

*            CALL METHOD OBJ_ALV_0200->REFRESH_TABLE_DISPLAY
*              EXPORTING
*                IS_STABLE = WL_STABLE.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.                    "ON_ONF4
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
