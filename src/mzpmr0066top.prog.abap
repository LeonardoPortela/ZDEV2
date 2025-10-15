*&---------------------------------------------------------------------*
*& Include MZPMR0066TOP                                      PoolMóds. *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzpmr0066 MESSAGE-ID z1.

DATA: okcode           TYPE sy-ucomm.

DATA: znotif_type         TYPE bapi2080-notif_type,
      zorderid            TYPE bapi2080_nothdre-orderid,
      ztask_determination TYPE bapiflag,
      znumber             TYPE bapi2080_nothdre-notif_no,
      ztask_key           TYPE bapi2080_nottaske-task_key,
      zmat_doc            TYPE bapi2017_gm_head_ret-mat_doc,
      zmat_doc_year       TYPE bapi2017_gm_head_ret-doc_year,
      zdata_bapi          TYPE installdat,
      ztime_bapi          TYPE installtim.

DATA: ls_pa0001 TYPE pa0001.

DATA: t_return TYPE STANDARD TABLE OF bapiret2,
      w_return TYPE bapiret2.

DATA: e_return  TYPE STANDARD TABLE OF bapiret2,
      we_return TYPE bapiret2.

DATA: t_methods TYPE STANDARD TABLE OF bapi_alm_order_method,
      w_methods TYPE bapi_alm_order_method.

DATA: t_header TYPE STANDARD TABLE OF bapi_alm_order_headers_i,
      w_header TYPE bapi_alm_order_headers_i.

DATA: t_header_up TYPE STANDARD TABLE OF bapi_alm_order_headers_up,
      w_header_up TYPE bapi_alm_order_headers_up.

DATA: t_operation TYPE STANDARD TABLE OF bapi_alm_order_operation,
      w_operation TYPE bapi_alm_order_operation.

DATA: t_numbers TYPE STANDARD TABLE OF bapi_alm_numbers,
      w_numbers TYPE bapi_alm_numbers.

DATA: t_component TYPE STANDARD TABLE OF bapi_alm_order_component,
      w_component TYPE bapi_alm_order_component.

DATA: e_notifheader  TYPE STANDARD TABLE OF bapi2080_nothdri,
      we_notifheader TYPE bapi2080_nothdri.

DATA: e_notifheader_export  TYPE STANDARD TABLE OF bapi2080_nothdre,
      we_notifheader_export TYPE bapi2080_nothdre.

DATA: t_itens TYPE STANDARD TABLE OF bapi2080_notitemi,
      w_itens TYPE bapi2080_notitemi.

DATA: t_notiftask TYPE STANDARD TABLE OF bapi2080_nottaski,
      w_notiftask TYPE  bapi2080_nottaski.

DATA: t_is_goodsmvt_header TYPE STANDARD TABLE OF bapi2017_gm_head_01,
      w_is_goodsmvt_header TYPE bapi2017_gm_head_01.

DATA: t_is_goodsmvt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create,
      w_is_goodsmvt_item TYPE bapi2017_gm_item_create.

DATA: t_is_goodsmvt_serialnumber TYPE STANDARD TABLE OF bapi2017_gm_serialnumber,
      w_is_goodsmvt_serialnumber TYPE bapi2017_gm_serialnumber.

DATA: t_serialno TYPE STANDARD TABLE OF bapi2017_gm_serialnumber,
      w_serialno TYPE bapi2017_gm_serialnumber.

DATA: t_data_general   TYPE bapi_itob.
DATA: t_data_generalx  TYPE bapi_itobx.
DATA: t_data_specific  TYPE bapi_itob_eq_only.
DATA: t_data_specificx TYPE bapi_itob_eq_onlyx.


DATA: save_ok            LIKE sy-ucomm,
      g_container        TYPE scrfname VALUE 'GRID_0100_CONT1',
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      gt_fieldcat        TYPE lvc_t_fcat,
      gs_layout          TYPE lvc_s_layo,
      g_max              TYPE i VALUE 100,
      gs_spfli           TYPE spfli,
      g_success          TYPE c.

DATA: gv_url_digital_left  TYPE char255,
      gv_url_digital_right TYPE char255.

*local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_verifier TYPE REF TO lcl_event_receiver.

DATA: BEGIN OF gt_outtab OCCURS 0.     "with header line
        INCLUDE STRUCTURE zpms0040.
DATA:   celltab TYPE lvc_t_styl.
DATA: process TYPE c.
DATA: END OF gt_outtab.


TYPES: BEGIN OF ty_resultado,
         id      TYPE  symsgid,  "CHAR20
         number  TYPE  symsgno,  "NUMC 3
         message TYPE  bapi_msg, "CHAR 220
       END OF ty_resultado.

DATA: git_resultado  TYPE TABLE OF ty_resultado.

DATA: gs_outtab LIKE LINE OF gt_outtab.

DATA: gv_werks    TYPE zpms0040-werks,
      gv_budat    LIKE sy-datum,
      gv_pernr    TYPE zpms0040-pernr,
      gv_name1    TYPE zpms0040-name1,
      gv_labst    TYPE zpms0040-labst,
      gv_matnr    TYPE zpms0040-matnr,
      gv_equnr    TYPE zpms0040-equnr,  "*CS2022000423-#77010-02.05.2022-JT-inicio
      gv_desc(50) TYPE c.

DATA: tg_zpmt0044 TYPE TABLE OF zpmt0044.
DATA: eg_zpmt0044 TYPE zpmt0044.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.


    TYPES: zpms0040_keys  TYPE STANDARD TABLE OF zpms0040,
           zpms0040_table TYPE STANDARD TABLE OF zpms0040.

    METHODS:
      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

    METHODS:
      get_inserted_rows
        EXPORTING
          inserted_rows TYPE zpms0040.

    METHODS:
      get_deleted_rows
        EXPORTING
          deleted_rows TYPE zpms0040.

    METHODS:
      refresh_delta_tables.

    METHODS: set_table_is_initial.

    METHODS: set_table_is_not_initial.

    METHODS: table_is_initial
      RETURNING VALUE(initial) TYPE char01.

*..............

  PRIVATE SECTION.

    DATA: inserted_rows TYPE zpms0040,
          deleted_rows  TYPE zpms0040.

    DATA: error_in_data TYPE c.

    DATA: initial_table TYPE c.
*
    METHODS:
      check_double_entries
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      update_delta_tables
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      perform_semantic_checks
        IMPORTING
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      get_cell_values
        IMPORTING
          row_id          TYPE int4
          pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
        EXPORTING
          key             TYPE zpms0040.



ENDCLASS.
**---------------------------------------------------------
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.
*
    DATA: ls_good TYPE lvc_s_modi,
          l_price TYPE s_price,
          ls_new  TYPE lvc_s_moce.

    error_in_data = space.

* check if there exist double entries
    CALL METHOD check_double_entries( er_data_changed ).

* remember new or deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

* check mt_good_cells semantically
    CALL METHOD perform_semantic_checks( er_data_changed ).

    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

*-----------------------------------------------------------------------
  METHOD check_double_entries.

  ENDMETHOD.
*-------------------------------------------------------
  METHOD update_delta_tables.
    DATA: l_ins_row   TYPE lvc_s_moce,
          l_del_row   TYPE lvc_s_moce,
          ls_key      TYPE zpms0040,
          ls_zpms0040 TYPE zpms0040,
          ls_outtab   LIKE LINE OF gt_outtab.


    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_outtab INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH TEXT-e01."Fehler beim Löschen
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_zpms0040.

        deleted_rows = ls_zpms0040.

      ENDIF.
    ENDLOOP.

    IF me->table_is_initial( ) EQ 'X'.

      CALL METHOD get_cell_values
        EXPORTING
          row_id          = 1
          pr_data_changed = pr_data_changed
        IMPORTING
          key             = ls_key.

      inserted_rows = ls_key.
      CALL METHOD me->set_table_is_not_initial.
    ENDIF.

    LOOP AT pr_data_changed->mt_inserted_rows INTO l_ins_row.
      CALL METHOD get_cell_values
        EXPORTING
          row_id          = l_ins_row-row_id
          pr_data_changed = pr_data_changed
        IMPORTING
          key             = ls_key.

      inserted_rows = ls_key.
    ENDLOOP.

  ENDMETHOD.
*---------------------------------------------------------
  METHOD get_cell_values.

    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_row_id    = row_id
        i_fieldname = 'MATNR'
      IMPORTING
        e_value     = key-matnr.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH TEXT-e02.  "Fehler beim Einfügen
    ENDIF.

    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_row_id    = row_id
        i_fieldname = 'WERKS'
      IMPORTING
        e_value     = key-werks.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH TEXT-e02.  "Fehler beim Einfügen
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------
  METHOD perform_semantic_checks.
    DATA: ls_good TYPE lvc_s_modi,
          l_matnr TYPE matnr,
          l_equnr TYPE equnr,
          l_pernr TYPE zpmt0040-pernr,
          l_descr TYPE maktx,
          l_eqktx TYPE ktx01.

    DATA: l_name TYPE pa0001-ename.

    DATA: ls_outtab LIKE LINE OF gt_outtab.

    DATA: l_firedate TYPE p0000-begda.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.

        WHEN 'MATNR'.
          CALL METHOD pr_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = l_matnr.

          IF l_matnr IS NOT INITIAL.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = l_matnr
              IMPORTING
                output       = l_matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            SELECT SINGLE maktx FROM makt INTO l_descr
                          WHERE matnr = l_matnr
                            AND spras = sy-langu.

            IF sy-subrc NE 0.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = '0K'
                  i_msgno     = '000'
                  i_msgty     = 'E'
                  i_msgv1     = TEXT-m02
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.

              error_in_data = 'X'.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'DESCR'
                  i_value     = ''.
            ELSE.
              SELECT SINGLE matnr INTO l_matnr
                  FROM mara
                  WHERE matnr = l_matnr
                    AND mtart = 'UNBW'.
              IF sy-subrc NE 0.
                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = '0K'
                    i_msgno     = '000'
                    i_msgty     = 'E'
                    i_msgv1     = TEXT-m04
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.

                error_in_data = 'X'.
                CALL METHOD pr_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'DESCR'
                    i_value     = ''.
              ELSE.
                CALL METHOD pr_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'DESCR'
                    i_value     = l_descr.
              ENDIF.
            ENDIF.
          ENDIF.

*-CS2022000423-#77010-02.05.2022-JT-inicio
        WHEN 'EQUNR'.
          CALL METHOD pr_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = l_equnr.

          IF l_equnr IS NOT INITIAL.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_equnr
              IMPORTING
                output = l_equnr.

            SELECT SINGLE eqktx FROM eqkt INTO l_eqktx
                          WHERE equnr = l_equnr
                            AND spras = sy-langu.

            IF sy-subrc NE 0.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = '0K'
                  i_msgno     = '000'
                  i_msgty     = 'E'
                  i_msgv1     = TEXT-m10
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.

              error_in_data = 'X'.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'EQKTX'
                  i_value     = ''.
            ELSE.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'EQKTX'
                  i_value     = l_eqktx.
            ENDIF.
          ENDIF.
*-CS2022000423-#77010-02.05.2022-JT-fim

        WHEN 'PERNR'.
          CALL METHOD pr_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = l_pernr.
          IF l_pernr IS NOT INITIAL.
            IF gt_outtab[] IS NOT INITIAL.
              CLEAR ls_outtab.
              READ TABLE gt_outtab INTO ls_outtab INDEX 1.
              IF sy-subrc = 0 AND ls_outtab-pernr IS NOT INITIAL AND ls_outtab-pernr NE l_pernr.
                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = '0K'
                    i_msgno     = '000'
                    i_msgty     = 'E'
                    i_msgv1     = TEXT-m05
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.

                error_in_data = 'X'.
                CLEAR l_pernr.
              ENDIF.
            ENDIF.
            IF l_pernr IS NOT INITIAL.
              CALL FUNCTION 'RP_GET_FIRE_DATE'
                EXPORTING
                  persnr   = l_pernr
*                 STATUS2  = '0'
                IMPORTING
                  firedate = l_firedate.
              IF l_firedate IS NOT INITIAL.
                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = '0K'
                    i_msgno     = '000'
                    i_msgty     = 'E'
                    i_msgv1     = TEXT-m01
                    i_fieldname = ls_good-fieldname
                    i_row_id    = ls_good-row_id.

                error_in_data = 'X'.
              ELSE.
                CLEAR l_name.
                SELECT SINGLE ename INTO l_name
                  FROM pa0001
                  WHERE pernr = l_pernr.
                IF l_name IS INITIAL.
                  CALL METHOD pr_data_changed->add_protocol_entry
                    EXPORTING
                      i_msgid     = '0K'
                      i_msgno     = '000'
                      i_msgty     = 'E'
                      i_msgv1     = TEXT-m03
                      i_fieldname = ls_good-fieldname
                      i_row_id    = ls_good-row_id.

                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'NAME1'
                      i_value     = ''.

                  error_in_data = 'X'.
                ELSE.
                  CALL METHOD pr_data_changed->modify_cell
                    EXPORTING
                      i_row_id    = ls_good-row_id
                      i_fieldname = 'NAME1'
                      i_value     = l_name.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

*------------------------------------------------------

  METHOD get_inserted_rows.
    inserted_rows = me->inserted_rows.
  ENDMETHOD.
*------------------------------------------------------

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.
*------------------------------------------------------
  METHOD refresh_delta_tables.
    CLEAR me->inserted_rows.
    CLEAR me->deleted_rows.
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_initial.
    initial_table = 'X'.
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_not_initial.
    initial_table = space.
  ENDMETHOD.
*------------------------------------------------------
  METHOD table_is_initial.
    IF initial_table = 'X'.
      initial = 'X'.
    ELSE.
      initial = space.
    ENDIF.
  ENDMETHOD.


ENDCLASS.

CLASS  handle_event DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      mt_sel_rows     TYPE lvc_t_row.

    CLASS-METHODS : handle_toolbar

      FOR EVENT toolbar OF cl_gui_alv_grid

      IMPORTING e_object

                e_interactive.

    CLASS-METHODS : handle_user_command

      FOR EVENT user_command OF cl_gui_alv_grid

      IMPORTING e_ucomm
                sender.

ENDCLASS.                    "HANDLE_EVENT DEFINITION

CLASS  handle_event IMPLEMENTATION.

  METHOD handle_toolbar.

    DATA : is_btn TYPE stb_button.

    is_btn-function = 'INSERT'.
    is_btn-icon = icon_insert_row.
    is_btn-quickinfo = 'INSERT'.
    is_btn-disabled = ' '.
    APPEND is_btn TO e_object->mt_toolbar.

    is_btn-function = 'DELETE'.
    is_btn-icon = icon_delete_row.
    is_btn-quickinfo = 'DELETE'.
    is_btn-disabled = ' '.
    APPEND is_btn TO e_object->mt_toolbar.

    is_btn-function = 'SAVE'.
    is_btn-icon = icon_system_save.
    is_btn-text = 'Efetivar Emprestimo'.
    is_btn-quickinfo = 'SAVE'.
    is_btn-disabled = ' '.

    APPEND is_btn TO e_object->mt_toolbar.

    is_btn-function = 'IMPR'.
    is_btn-icon = icon_system_print.
    is_btn-text = 'Impressão Termo Responsabilidade'.
    is_btn-quickinfo = 'IMPR'.
    is_btn-disabled = ' '.
    APPEND is_btn TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command .

    DATA: lv_err TYPE c.
    DATA: lv_answer TYPE c.
    DATA: biometry             TYPE REF TO zcl_biometry,
          w_notifheader_export TYPE bapi2080_nothdre.

    DATA: w_sttxt TYPE caufvd-sttxt,
          w_asttx TYPE caufvd-asttx,
          l_tabix TYPE sy-tabix.

    CALL METHOD sender->get_selected_rows
      IMPORTING
        et_index_rows = mt_sel_rows
*       et_row_no     =
      .

    CASE e_ucomm.

      WHEN 'INSERT'.
        PERFORM insert_row.

      WHEN 'DELETE'.

        PERFORM delete_rows.

      WHEN 'SAVE'.

        LOOP AT gt_outtab INTO gs_outtab.

          FREE: w_sttxt, w_asttx.

          l_tabix = sy-tabix.

          IF gs_outtab-werks IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Centro não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
          ELSEIF gs_outtab-matnr IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Material não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
          ELSEIF gs_outtab-budat IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Dt.Lcto não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
          ELSEIF gs_outtab-budat IS NOT INITIAL AND gs_outtab-budat > sy-datum.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Dt.Lcto maior que data atual, linha' l_tabix DISPLAY LIKE 'E'.
          ELSEIF gs_outtab-pernr IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo No.Pessoal não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
          ELSEIF gs_outtab-labst IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Qtde não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
*-CS2022000423-#77010-02.05.2022-JT-inicio
          ELSEIF gs_outtab-equnr IS INITIAL.
            lv_err = 'X'.
            MESSAGE s000(z_mm) WITH 'Campo Equipamento não preenchido, linha' l_tabix DISPLAY LIKE 'E'.
          ELSE.
            SELECT iwerk, submt
              INTO @DATA(w_equz)
              FROM equz
                UP TO 1 ROWS
             WHERE equnr  = @gs_outtab-equnr
               AND datbi >= @sy-datum.
            ENDSELECT.

            IF sy-subrc <> 0.
              lv_err = 'X'.
              MESSAGE s000(z_mm) WITH 'Equipamento não localizado, linha'  l_tabix DISPLAY LIKE 'E'.
            ELSE.
              IF w_equz-iwerk <> gs_outtab-werks.
                lv_err = 'X'.
                MESSAGE s000(z_mm) WITH 'Equipamento com Centro diferente '
                                        'do centro do Empréstimo, linha' l_tabix DISPLAY LIKE 'E'.
              ENDIF.
              IF w_equz-submt <> gs_outtab-matnr.
                lv_err = 'X'.
                MESSAGE s000(z_mm) WITH 'Equipamento com Material diferente '
                                        'em seu cadastro, linha' l_tabix DISPLAY LIKE 'E'.
              ENDIF.
            ENDIF.

            SELECT objnr
              INTO @DATA(l_objnr)
              FROM equi
                UP TO 1 ROWS
             WHERE equnr  = @gs_outtab-equnr.
            ENDSELECT.

            IF sy-subrc = 0.
              CALL FUNCTION 'STATUS_TEXT_EDIT'
                EXPORTING
                  flg_user_stat    = abap_true
                  objnr            = l_objnr
                  spras            = sy-langu
                IMPORTING
                  line             = w_sttxt             "1695763
                  user_line        = w_asttx             "1695763
                EXCEPTIONS
                  object_not_found = 1
                  OTHERS           = 2.

              SPLIT w_sttxt AT abap_off INTO TABLE DATA(t_status).
              READ TABLE t_status INTO DATA(w_status) WITH KEY table_line =  'MREL'.

              IF sy-subrc = 0.
                lv_err = 'X'.
                MESSAGE s000(z_mm) WITH 'Equipamento marcado para Eliminação, linha' l_tabix DISPLAY LIKE 'E'.
              ELSE.
                CLEAR:  w_notifheader_export.

                SELECT *
                  INTO TABLE @DATA(t_0041)
                  FROM zpmt0041
                 WHERE werks  = @gs_outtab-werks
                   AND matnr  = @gs_outtab-matnr
                   AND equnr  = @gs_outtab-equnr.

                READ TABLE t_0041 INTO DATA(w_0041) WITH KEY descarte = abap_true.
                IF sy-subrc = 0.
                  lv_err = 'X'.
                  MESSAGE s000(z_mm) WITH 'Equipamento Descartado, '
                                          'não é possível empréstimo, linha' l_tabix DISPLAY LIKE 'E'.
                ELSE.
                  SELECT *
                    INTO TABLE @DATA(t_0044)
                    FROM zpmt0044
                   WHERE werks  = @gs_outtab-werks
                     AND matnr  = @gs_outtab-matnr
                     AND equnr  = @gs_outtab-equnr.

                  READ TABLE t_0044 INTO DATA(w_0044) WITH KEY descarte = abap_true.
                  IF sy-subrc = 0.
                    lv_err = 'X'.
                    MESSAGE s000(z_mm) WITH 'Equipamento Descartado, '
                                            'não é possível empréstimo, linha' l_tabix DISPLAY LIKE 'E'.
                  ENDIF.
                ENDIF.

                IF lv_err = abap_false.
                  SELECT *
                    INTO TABLE @DATA(t_0040)
                    FROM zpmt0040
                   WHERE matnr  = @gs_outtab-matnr
                     AND equnr  = @gs_outtab-equnr
                     AND qmnum <> @abap_off.

                  IF t_0040[] IS NOT INITIAL.
                    SORT t_0040 BY data_cri DESCENDING
                                   hora_cri DESCENDING.
                    READ TABLE t_0040 INTO DATA(w_0040) INDEX 1.

                    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
                      EXPORTING
                        number             = w_0040-qmnum
                      IMPORTING
                        notifheader_export = w_notifheader_export.

                    IF w_notifheader_export-sys_status <> 'MSEN'.
                      lv_err = 'X'.
                      MESSAGE s000(z_mm) WITH 'Equipamento já foi Emprestado, linha' l_tabix DISPLAY LIKE 'E'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*-CS2022000423-#77010-02.05.2022-JT-fim

          IF lv_err = 'X'.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF lv_err IS INITIAL AND  gt_outtab[] IS NOT INITIAL.
**********************************************************************"146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING
*              titlebar       = 'Atenção'
*              text_question  = 'Utilizar biometria?'
*              text_button_1  = 'Sim'(p01)
*              text_button_2  = 'Não'(p02)
*            IMPORTING
*              answer         = lv_answer
*            EXCEPTIONS
*              text_not_found = 1
*              OTHERS         = 2.
*          IF lv_answer = '1'.
*            biometry                   = NEW zcl_biometry( ).
*            CLEAR: gv_url_digital_right,
*                   gv_url_digital_left.
*
*            TRY.
*                CALL METHOD biometry->read_digital(
*                  EXPORTING
*                    registration = gv_pernr
*                  RECEIVING
*                    result       = DATA(_result) ).
*                DATA(_urlr) =
*                  biometry->get_digital_as_url_image2(
*                  image_xstring = _result-im_polegar_direito ).
*                gv_url_digital_right = _urlr.
*
*                DATA(_urll) =
*                  biometry->get_digital_as_url_image2(
*                  image_xstring = _result-im_polegar_esquerdo ).
*                gv_url_digital_left = _urll.
*              CATCH zcx_biometry.
*                MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
*            ENDTRY.
*
*          ENDIF.

          DATA: _resultado TYPE char1,
                _msg       TYPE string.

          CALL FUNCTION 'ZPM_CHECK_AUTHORIZE'
            EXPORTING
              i_matricula = gv_pernr
            IMPORTING
              e_result    = _resultado
              e_message   = _msg.

          IF sy-subrc = 0.
            IF _resultado = 'S'.
              MESSAGE _msg TYPE 'I' DISPLAY LIKE _resultado.
              PERFORM efetuar_lancamento.
            ELSE.
              MESSAGE _msg TYPE 'I' DISPLAY LIKE _resultado.
              EXIT.
            ENDIF.
          ENDIF.
**********************************************************************
        ENDIF.

      WHEN 'IMPR'.
        PERFORM  z_call_form.
    ENDCASE.


    CALL METHOD g_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "HANDLE_EVENT IMPLEMENTATION
