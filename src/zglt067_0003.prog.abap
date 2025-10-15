*----------------------------------------------------------------------*
***INCLUDE ZGLT067_0003 .
*----------------------------------------------------------------------*

TABLES: zde_mov_fornecedor, zde_mov_lct_banco, lfa1, kna1.


TYPES: BEGIN OF ty_zde_mov_lct_banco. " RJF
         INCLUDE STRUCTURE zde_mov_lct_banco.
TYPES:  style TYPE lvc_t_styl,
       END OF ty_zde_mov_lct_banco.

"Contabilidade: índice secundário para fornecedores
DATA: ctl_alv_0302 TYPE REF TO cl_gui_alv_grid,
      ctl_alv_0308 TYPE REF TO cl_gui_alv_grid,
      it_bsik      TYPE TABLE OF bsik WITH HEADER LINE,
      it_bsid      TYPE TABLE OF bsid WITH HEADER LINE,
      it_bkpf_1    TYPE TABLE OF bkpf WITH HEADER LINE,
      it_bsik_2    TYPE TABLE OF bsik WITH HEADER LINE,
      it_bsid_2    TYPE TABLE OF bsid WITH HEADER LINE,
      it_bkpf_2    TYPE TABLE OF bkpf WITH HEADER LINE,
      it_lfa1      TYPE TABLE OF lfa1 WITH HEADER LINE,
      it_kna1      TYPE TABLE OF kna1 WITH HEADER LINE,
      it_bsik_alv  TYPE TABLE OF ty_zde_mov_fornecedor WITH HEADER LINE,
      it_bsik_alvo TYPE TABLE OF ty_zde_mov_fornecedor WITH HEADER LINE,
      lc_ico_saldo TYPE icons-text.

DATA: wa_stable TYPE lvc_s_stbl,
      lc_celula TYPE lvc_s_modi.

CONSTANTS: lc_dunnr_0302 TYPE sydynnr VALUE '0302',
           lc_dunnr_0308 TYPE sydynnr VALUE '0308'.

CONTROLS: pagtab   TYPE TABSTRIP,
          tbmesma  TYPE TABLEVIEW USING SCREEN 0302,
          tboutras TYPE TABLEVIEW USING SCREEN 0308.

DATA: lc_dynnr_0307 TYPE sydynnr.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0302 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0302 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION


*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0308 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0308 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0302 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0302 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_checked.
    ty_toolbar-function  = 'MARCAR'.
    ty_toolbar-quickinfo = text-001.
    "TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_incomplete.
    ty_toolbar-function  = 'DESMARCAR'.
    ty_toolbar-quickinfo = text-002.
    "TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA: p_parid TYPE j_1bparid,
          wa_aux  LIKE LINE OF it_bsik_alv.

    FIELD-SYMBOLS: <fsmov> TYPE ty_zde_mov_fornecedor.

    CASE e_ucomm.
      WHEN 'MARCAR'.

        READ TABLE it_bsik_alv INTO wa_aux WITH KEY ck_payment = abap_true.
        IF sy-subrc IS INITIAL.
          p_parid = wa_aux-parid.
        ENDIF.

        LOOP AT it_bsik_alv ASSIGNING <fsmov>.
          IF p_parid IS INITIAL.
            p_parid = <fsmov>-parid.
          ENDIF.
          IF ( p_parid EQ <fsmov>-parid ) OR ( ck_varios_cliente EQ abap_true ).
            <fsmov>-ck_payment     = 'X'.
            <fsmov>-valor_payments = <fsmov>-wrbtr.
          ENDIF.
        ENDLOOP.
      WHEN 'DESMARCAR'.
        LOOP AT it_bsik_alv ASSIGNING <fsmov>.
          <fsmov>-ck_payment = ' '.
          <fsmov>-valor_payments = 0.
        ENDLOOP.
    ENDCASE.

    zde_saldo_cta_banco-saldod = copia-saldod.
    CLEAR sd_compen.
    LOOP AT it_bsik_alv ASSIGNING <fsmov>.
      CLEAR: <fsmov>-valor_residual.
      CASE <fsmov>-shkzg.
        WHEN 'H'. "Crédito
          IF <fsmov>-ck_payment EQ abap_true.
*            ZDE_SALDO_CTA_BANCO-SALDOD = ZDE_SALDO_CTA_BANCO-SALDOD - <FSMOV>-VALOR_PAYMENTS.
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments.
            sd_compen = sd_compen + <fsmov>-valor_payments.
            <fsmov>-valor_residual =  <fsmov>-wrbtr - <fsmov>-valor_payments.
          ENDIF.
*          <FSMOV>-VALOR_RESIDUAL = ( ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS ) * 1.
        WHEN 'S'. "Debito
          IF <fsmov>-ck_payment EQ abap_true.
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments.
            sd_compen = sd_compen + <fsmov>-valor_payments.
            <fsmov>-valor_residual =  <fsmov>-wrbtr - <fsmov>-valor_payments.
          ENDIF.
*          <FSMOV>-VALOR_RESIDUAL = ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS.
      ENDCASE.
    ENDLOOP.
    PERFORM calcular_saldo.

    LEAVE TO SCREEN 0003.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0302 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0308 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0308 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarma_0308
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_checked.
    ty_toolbar-function  = 'MARCAR'.
    ty_toolbar-quickinfo = text-001.
    "TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_incomplete.
    ty_toolbar-function  = 'DESMARCAR'.
    ty_toolbar-quickinfo = text-002.
    "TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    CALL METHOD C_ALV_TOOLBARMA_0308->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA: p_parid TYPE j_1bparid,
          wa_aux  LIKE LINE OF it_bsik_alv.

    FIELD-SYMBOLS: <fsmov> TYPE ty_zde_mov_fornecedor.

    CASE e_ucomm.
      WHEN 'MARCAR'.

        READ TABLE it_bsik_alvo INTO wa_aux WITH KEY ck_payment = abap_true.
        IF sy-subrc IS INITIAL.
          p_parid = wa_aux-parid.
        ENDIF.

        LOOP AT it_bsik_alvo ASSIGNING <fsmov>.
          IF p_parid IS INITIAL.
            p_parid = <fsmov>-parid.
          ENDIF.
          IF ( p_parid EQ <fsmov>-parid ) OR ( ck_varios_cliente EQ abap_true ).
            <fsmov>-ck_payment = 'X'.
          ENDIF.
        ENDLOOP.

      WHEN 'DESMARCAR'.

        LOOP AT it_bsik_alvo ASSIGNING <fsmov>.
          <fsmov>-ck_payment = ' '.
        ENDLOOP.

    ENDCASE.

    zde_saldo_cta_banco-saldod = copia-saldod.
    CLEAR sd_compen.
    LOOP AT it_bsik_alvo ASSIGNING <fsmov>.
      CASE <fsmov>-shkzg.
        WHEN 'H'. "Crédito
          IF <fsmov>-ck_payment EQ abap_true.
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod - <fsmov>-valor_payments_b.
            sd_compen = sd_compen  - <fsmov>-valor_payments_b.
          ENDIF.
        WHEN 'S'. "Debito
          IF <fsmov>-ck_payment EQ abap_true.
            zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments_b.
            sd_compen = sd_compen  + <fsmov>-valor_payments_b.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    PERFORM calcular_saldo.

    LEAVE TO SCREEN 0003.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0302 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c.

    METHODS: data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    METHODS: data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.

    METHODS: subtotal_text FOR EVENT subtotal_text OF cl_gui_alv_grid IMPORTING es_subtottxt_info ep_subtot_line e_event_data.

  PRIVATE SECTION.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_0308 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_0308 DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c.

    METHODS: data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    METHODS: data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.

    METHODS: subtotal_text FOR EVENT subtotal_text OF cl_gui_alv_grid IMPORTING es_subtottxt_info ep_subtot_line e_event_data.

  PRIVATE SECTION.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_receiver DEFINITION



DATA: event_handler_0302 TYPE REF TO lcl_event_receiver.
DATA: event_handler_0308 TYPE REF TO lcl_event_receiver_0308.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD subtotal_text.
    DATA: wa_bsik_alv2 TYPE ty_zde_mov_fornecedor,
          wa_bsik_alv  TYPE ty_zde_mov_fornecedor,
          lc_saldod	   TYPE zde_saldo_cta_d.
    FIELD-SYMBOLS: <fs>  TYPE any.
    FIELD-SYMBOLS: <fs2> TYPE any.

    "BREAK-POINT.

    ASSIGN e_event_data->m_data->* TO <fs>.
    IF sy-subrc EQ 0.

      IF es_subtottxt_info(11) EQ 'PARID_KOART'.

        ASSIGN ep_subtot_line->* TO <fs2>.
        wa_bsik_alv2 = <fs2>.

        READ TABLE it_bsik_alv INTO wa_bsik_alv WITH KEY parid_koart = wa_bsik_alv2-parid_koart.
        CASE wa_bsik_alv2-parid_koart+10(1).
          WHEN 'K'.
            CONCATENATE 'Vendor:' wa_bsik_alv-name1 INTO <fs> SEPARATED BY space.
          WHEN 'D'.
            CONCATENATE 'Customer:' wa_bsik_alv-name1 INTO <fs> SEPARATED BY space.
        ENDCASE.

        lc_saldod = 0.
        LOOP AT it_bsik_alv INTO wa_bsik_alv WHERE ck_payment EQ abap_true AND parid_koart EQ wa_bsik_alv2-parid_koart.
*          CASE WA_BSIK_ALV-SHKZG.
*            WHEN 'H'. "Credito
*              LC_SALDOD = LC_SALDOD - WA_BSIK_ALV-VALOR_PAYMENTS.
*            WHEN 'S'. "Debito
*              LC_SALDOD = LC_SALDOD + WA_BSIK_ALV-VALOR_PAYMENTS.
*          ENDCASE.
          lc_saldod = lc_saldod + wa_bsik_alv-valor_payments.
        ENDLOOP.

        wa_bsik_alv = <fs2>.
        wa_bsik_alv-valor_payments = lc_saldod.
        <fs2> = wa_bsik_alv.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "subtotal_text

  METHOD handle_hotspot_click.
    DATA: wa_registro TYPE ty_zde_mov_fornecedor.

    IF es_row_no-row_id GT 0.
      READ TABLE it_bsik_alv INDEX es_row_no-row_id INTO wa_registro.

      CASE e_column_id-fieldname.
        WHEN 'BELNR'.
          PERFORM chama_tela_fb03 USING wa_registro-bukrs wa_registro-gjahr wa_registro-belnr.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  "BCALV_EDIT_03
  METHOD perform_semantic_checks.

    FIELD-SYMBOLS: <fs_cell> TYPE lvc_s_modi.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          vl_value    TYPE lvc_value,
          wl_valor    TYPE zde_payments,
          vg_tabix    TYPE  sy-tabix,
          wa_bsik_alv TYPE ty_zde_mov_fornecedor,
          wa_bsik_ala TYPE ty_zde_mov_fornecedor,
          ck_alterou  TYPE c LENGTH 1,
          i_tabix     TYPE int4.

    IF ck_varios_cliente EQ abap_false.

      ck_alterou = abap_false.

      LOOP AT pr_data_changed->mt_good_cells ASSIGNING <fs_cell> WHERE fieldname = 'CK_PAYMENT'.
        IF <fs_cell>-value EQ abap_true.
          READ TABLE it_bsik_alv INTO wa_bsik_ala INDEX <fs_cell>-row_id.
          IF wa_bsik_ala-budat GT zde_saldo_cta_banco-budat.
            CLEAR lv_value..
            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = <fs_cell>-row_id
                i_fieldname = 'CK_PAYMENT'
                i_value     = lv_value.

            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = <fs_cell>-row_id
                i_fieldname = 'VALOR_PAYMENTS'
                i_value     = lv_value.
            MESSAGE s122 WITH wa_bsik_ala-belnr DISPLAY LIKE 'E'.
          ENDIF.
          IF wa_bsik_ala-valor_payments IS INITIAL.
            wa_bsik_ala-valor_payments =  wa_bsik_ala-wrbtr .
            wl_valor                   =  wa_bsik_ala-wrbtr .
            MOVE wl_valor TO lv_value.

            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = <fs_cell>-row_id
                i_fieldname = 'VALOR_PAYMENTS'
                i_value     = lv_value.
          ENDIF.
          LOOP AT it_bsik_alv INTO wa_bsik_alv WHERE ck_payment EQ abap_true AND parid NE wa_bsik_ala-parid.
            <fs_cell>-value = abap_false.
            ck_alterou = abap_true.
          ENDLOOP.
        ELSE.
          CLEAR lv_value.
          CALL METHOD pr_data_changed->modify_cell
            EXPORTING
              i_row_id    = <fs_cell>-row_id
              i_fieldname = 'VALOR_PAYMENTS'
              i_value     = lv_value.
        ENDIF.
      ENDLOOP.

*      IF CK_ALTEROU EQ ABAP_TRUE.
*        CLEAR:
*        PR_DATA_CHANGED->MT_MOD_CELLS,
*        PR_DATA_CHANGED->MT_GOOD_CELLS.
*        MESSAGE S047 WITH WA_BSIK_ALA-PARID DISPLAY LIKE 'E'.
*      ENDIF.
    ENDIF.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'VALOR_PAYMENTS'.

      vg_tabix = sy-tabix.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_valor = lv_value.
      READ TABLE it_bsik_alv INTO wa_bsik_alv INDEX ls_good-row_id.
      IF wa_bsik_alv-wrbtr < 0 AND wl_valor > 0.
        MULTIPLY wl_valor BY -1.
        MOVE wl_valor TO lv_value.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_PAYMENTS'
            i_value     = lv_value.
      ENDIF.
      IF abs( wa_bsik_alv-wrbtr ) LT abs( wl_valor ).
        error_in_data = abap_true.

        wa_bsik_alv-wrbtr = abs( wa_bsik_alv-wrbtr ).
        WRITE wa_bsik_alv-wrbtr TO lv_value.
        CONDENSE lv_value NO-GAPS.

        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZFI'
            i_msgno     = '032'
            i_msgty     = 'E'
            i_msgv1     = lv_value
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.
      ELSEIF  abs( wl_valor ) LT 0.
        error_in_data = abap_true.
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZFI'
            i_msgno     = '033'
            i_msgty     = 'E'
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.
      ELSEIF abs( wl_valor ) LT abs( wa_bsik_alv-wrbtr ).
        CLEAR: zde_mov_fornecedor-data_residual,zde_mov_fornecedor-sgtxt.
        IF wl_valor NE 0.
          zde_mov_fornecedor-data_residual = wa_bsik_alv-data_residual.
          zde_mov_fornecedor-sgtxt         = wa_bsik_alv-sgtxt.
          CALL SCREEN 0303 STARTING AT 15 01.
        ENDIF.

        IF zde_mov_fornecedor IS INITIAL AND wl_valor NE 0.
          error_in_data = abap_true.
          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZFI'
              i_msgno     = '034'
              i_msgty     = 'E'
              i_fieldname = ls_good-fieldname
              i_row_id    = ls_good-row_id.
        ELSEIF zde_mov_fornecedor-data_residual IS NOT INITIAL AND zde_mov_fornecedor-sgtxt IS NOT INITIAL.

          MOVE zde_mov_fornecedor-data_residual TO lv_value.

          CALL METHOD pr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'DATA_RESIDUAL'
              i_value     = lv_value.

          MOVE zde_mov_fornecedor-sgtxt TO lv_value.

          CALL METHOD pr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'SGTXT'
              i_value     = lv_value.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "perform_semantic_checks

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    FIELD-SYMBOLS: <fsmov> TYPE ty_zde_mov_fornecedor.

    IF e_modified IS NOT INITIAL.

      zde_saldo_cta_banco-saldod = copia-saldod.
      CLEAR sd_compen.
      LOOP AT it_bsik_alv ASSIGNING <fsmov>.
        CLEAR <fsmov>-valor_residual.
        CASE <fsmov>-shkzg.
          WHEN 'H'. "Crédito
            IF <fsmov>-ck_payment EQ abap_true.
*              ZDE_SALDO_CTA_BANCO-SALDOD = ZDE_SALDO_CTA_BANCO-SALDOD - <FSMOV>-VALOR_PAYMENTS.
              zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments.
              sd_compen = sd_compen + <fsmov>-valor_payments.
              <fsmov>-valor_residual =  <fsmov>-wrbtr  - <fsmov>-valor_payments .
            ENDIF.
*            <FSMOV>-VALOR_RESIDUAL = ( ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS ) * 1.
          WHEN 'S'. "Debito
            IF <fsmov>-ck_payment EQ abap_true.
              zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments.
              sd_compen = sd_compen + <fsmov>-valor_payments.
              <fsmov>-valor_residual = <fsmov>-wrbtr - <fsmov>-valor_payments.
            ENDIF.
*            <FSMOV>-VALOR_RESIDUAL = ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS.
        ENDCASE.

*        IF ( zde_mov_fornecedor-sgtxt IS NOT INITIAL ) AND ( <fsmov>-data_residual IS NOT INITIAL ).
*          <fsmov>-sgtxt = zde_mov_fornecedor-sgtxt.
*        ELSEIF <fsmov>-ck_payment = 'X'.
*          CLEAR <fsmov>-sgtxt .
*        ENDIF.
      ENDLOOP.

      PERFORM calcular_saldo.

*      WA_STABLE-ROW = ABAP_TRUE.
*      WA_STABLE-COL = ABAP_TRUE.
*      CALL METHOD CTL_ALV_0302->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*      DATA: IS_COL_INFO TYPE LVC_S_COL,
*            IS_ROW_NO   TYPE LVC_S_ROID.
*
*      READ TABLE ET_GOOD_CELLS INTO LC_CELULA INDEX 1.
*      IF LC_CELULA-FIELDNAME EQ 'CK_PAYMENT'.
*        IS_COL_INFO-FIELDNAME = 'VALOR_PAYMENTS'.
*      ELSEIF LC_CELULA-FIELDNAME EQ 'VALOR_PAYMENTS'.
*        IS_COL_INFO-FIELDNAME = 'CK_PAYMENT'.
*      ENDIF.
*      IS_ROW_NO-ROW_ID      = LC_CELULA-ROW_ID.
*      IS_ROW_NO-SUB_ROW_ID  = LC_CELULA-SUB_ROW_ID.
*
*      CALL METHOD CTL_ALV_0302->SET_SCROLL_INFO_VIA_ID
*        EXPORTING
*          IS_COL_INFO = IS_COL_INFO
*          IS_ROW_NO   = IS_ROW_NO.

      LEAVE TO SCREEN 0003.

    ENDIF.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER_0308 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_0308 IMPLEMENTATION.

  METHOD subtotal_text.
    DATA: wa_bsik_alv2 TYPE ty_zde_mov_fornecedor,
          wa_bsik_alv  TYPE ty_zde_mov_fornecedor,
          lc_saldod	   TYPE zde_saldo_cta_d.
    FIELD-SYMBOLS: <fs>  TYPE any.
    FIELD-SYMBOLS: <fs2> TYPE any.

    ASSIGN e_event_data->m_data->* TO <fs>.
    IF sy-subrc EQ 0.

      IF es_subtottxt_info(11) EQ 'PARID_KOART'.

        ASSIGN ep_subtot_line->* TO <fs2>.
        wa_bsik_alv2 = <fs2>.

        READ TABLE it_bsik_alvo INTO wa_bsik_alv WITH KEY parid_koart = wa_bsik_alv2-parid_koart.
        CASE wa_bsik_alv2-parid_koart+10(1).
          WHEN 'K'.
            CONCATENATE 'Vendor:' wa_bsik_alv-name1 INTO <fs> SEPARATED BY space.
          WHEN 'D'.
            CONCATENATE 'Customer:' wa_bsik_alv-name1 INTO <fs> SEPARATED BY space.
        ENDCASE.

        lc_saldod = 0.
        LOOP AT it_bsik_alvo INTO wa_bsik_alv WHERE ck_payment EQ abap_true AND parid_koart EQ wa_bsik_alv2-parid_koart.
*          CASE WA_BSIK_ALV-SHKZG.
*            WHEN 'H'. "Credito
*              LC_SALDOD = LC_SALDOD - WA_BSIK_ALV-VALOR_PAYMENTS_B.
*            WHEN 'S'. "Debito
*              LC_SALDOD = LC_SALDOD + WA_BSIK_ALV-VALOR_PAYMENTS_B.
*          ENDCASE.
          lc_saldod = lc_saldod + wa_bsik_alv-valor_payments_b.
        ENDLOOP.

        wa_bsik_alv = <fs2>.
        wa_bsik_alv-valor_payments_b = lc_saldod.
        <fs2> = wa_bsik_alv.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "subtotal_text

  METHOD handle_hotspot_click.
    DATA: wa_registro TYPE ty_zde_mov_fornecedor.

    IF es_row_no-row_id GT 0.
      READ TABLE it_bsik_alvo INDEX es_row_no-row_id INTO wa_registro.

      CASE e_column_id-fieldname.
        WHEN 'BELNR'.
          PERFORM chama_tela_fb03 USING wa_registro-bukrs wa_registro-gjahr wa_registro-belnr.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  "BCALV_EDIT_03
  METHOD perform_semantic_checks.

    FIELD-SYMBOLS: <fs_cell> TYPE lvc_s_modi.

    DATA: obj_zcl_util_sd  TYPE REF TO zcl_util_sd,
          i_data           TYPE gdatu_inv,
          i_data_pagamento TYPE gdatu_inv,
          e_ukurs  	       TYPE ukurs_curr,
          e_ukurs_2	       TYPE ukurs_curr,
          e_ukurs_pag      TYPE ukurs_curr,
          e_ukurs_pag_2    TYPE ukurs_curr,
          e_ukurs_pag_3    TYPE ukurs_curr,
          e_ukurs_novo     TYPE ukurs_curr,
          lc_int_varia     TYPE i.

    DATA: lc_value TYPE p DECIMALS 4.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          lc_fator    TYPE i,
          lc_conta    TYPE zde_conta_var_cambio,
          vl_value    TYPE lvc_value,
          wl_valor    TYPE zde_payments,
          wl_valor_c  TYPE zde_payments,
          vg_tabix    TYPE  sy-tabix,
          wa_bsik_alv TYPE ty_zde_mov_fornecedor,
          wa_bsik_ala TYPE ty_zde_mov_fornecedor,
          ck_alterou  TYPE c LENGTH 1,
          wa_t030h    TYPE t030h,
          wa_t001     TYPE t001,
          wa_moedas   TYPE x001.

    "
    CLEAR wl_valor_c.
    SELECT SINGLE * INTO wa_t001
       FROM t001
      WHERE bukrs EQ zde_saldo_cta_banco-bukrs.

    IF ck_varios_cliente EQ abap_false.
      ck_alterou = abap_false.
      LOOP AT pr_data_changed->mt_good_cells ASSIGNING <fs_cell> WHERE fieldname = 'CK_PAYMENT'.
        IF <fs_cell>-value EQ abap_true.
          READ TABLE it_bsik_alvo INTO wa_bsik_ala INDEX <fs_cell>-row_id.
          IF wa_bsik_ala-budat GT zde_saldo_cta_banco-budat.
            CLEAR lv_value.
            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = <fs_cell>-row_id
                i_fieldname = 'CK_PAYMENT'
                i_value     = lv_value.

            CALL METHOD pr_data_changed->modify_cell
              EXPORTING
                i_row_id    = <fs_cell>-row_id
                i_fieldname = 'VALOR_PAYMENTS_B'
                i_value     = lv_value.
            MESSAGE s122 WITH wa_bsik_ala-belnr DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          CLEAR lv_value.
          CALL METHOD pr_data_changed->modify_cell
            EXPORTING
              i_row_id    = <fs_cell>-row_id
              i_fieldname = 'VALOR_PAYMENTS_B'
              i_value     = lv_value.
        ENDIF.
      ENDLOOP.

      IF ck_alterou EQ abap_true.
        MESSAGE s047 WITH wa_bsik_ala-parid DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'VALOR_RESIDUAL'.
      vg_tabix = sy-tabix.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_valor = lv_value.
      READ TABLE it_bsik_alvo INTO wa_bsik_alv INDEX ls_good-row_id.
      CLEAR: zde_mov_fornecedor-data_residual, zde_mov_fornecedor-sgtxt.
      IF wl_valor NE 0.
        zde_mov_fornecedor-data_residual = wa_bsik_alv-data_residual.
        zde_mov_fornecedor-sgtxt         = wa_bsik_alv-sgtxt.
        CALL SCREEN 0303 STARTING AT 15 01.
      ENDIF.

      IF zde_mov_fornecedor IS INITIAL AND wl_valor NE 0.
        error_in_data = abap_true.
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = 'ZFI'
            i_msgno     = '034'
            i_msgty     = 'E'
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.
      ELSEIF zde_mov_fornecedor-data_residual IS NOT INITIAL AND zde_mov_fornecedor-sgtxt IS NOT INITIAL.

        MOVE zde_mov_fornecedor-data_residual TO lv_value.

        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'DATA_RESIDUAL'
            i_value     = lv_value.

        MOVE zde_mov_fornecedor-sgtxt TO lv_value.

        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'SGTXT'
            i_value     = lv_value.

      ENDIF.

      IF wa_bsik_alv-wrbtr < 0 AND wl_valor > 0 OR
         wa_bsik_alv-wrbtr > 0 AND wl_valor < 0.
        MULTIPLY wl_valor BY -1.
        MOVE wl_valor TO lv_value.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_RESIDUAL'
            i_value     = lv_value.
        wa_bsik_alv-valor_residual = wl_valor.
      ENDIF.
      i_data           = wa_bsik_alv-budat.
      i_data_pagamento = zde_saldo_cta_banco-budat.

      CREATE OBJECT obj_zcl_util_sd.

      IF zde_saldo_cta_banco-waers = wa_t001-waers.
        e_ukurs = 1.
      ELSE.
        obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_saldo_cta_banco-waers ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
      ENDIF.

      IF zde_saldo_cta_banco-waers = wa_t001-waers.
        e_ukurs_pag = 1.
      ELSE.
        obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data_pagamento ).
        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_saldo_cta_banco-waers ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_pag ).
      ENDIF.

      IF wa_t001-waers = wa_bsik_alv-waers.
        e_ukurs_pag_2 = 1.
        IF zde_saldo_cta_banco-bukrs = '0200'.
          e_ukurs_pag_2 = -1.
        ENDIF.
      ELSE.
        obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data_pagamento ).
        obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
        obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_bsik_alv-waers ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_pag_2 ).
      ENDIF.

      "Variação para split com residual
      IF ( zde_saldo_cta_banco-bukrs = '0200' AND e_ukurs LT 0 ) OR ( '0201_0202' CS zde_saldo_cta_banco-bukrs AND e_ukurs_pag_2 GT 0 ).
        wa_bsik_alv-valor_payments_a       = ( wa_bsik_alv-wrbtr - wl_valor )  / abs( e_ukurs_pag_2 ).              " Converte da moeda do doc para a moeda interna
        IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
        ELSE.
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
        ENDIF.
      ELSE.
        wa_bsik_alv-valor_payments_a       = ( wa_bsik_alv-wrbtr - wl_valor ) * abs( e_ukurs_pag_2 ).              " Converte da moeda do doc para a moeda interna
        IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
        ELSE.
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
        ENDIF.
      ENDIF.
      "Variação na Moeda do Documento
      wa_bsik_alv-valor_variacao_c = wa_bsik_alv-valor_payments_b - wa_bsik_alv-valor_payments_a.

      "
      lv_value = wa_bsik_alv-valor_variacao_c.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_VARIACAO_C'
          i_value     = lv_value.


    ENDLOOP.


    LOOP AT pr_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'VALOR_PAYMENTS_B'.

      SELECT SINGLE * INTO wa_t001
        FROM t001
       WHERE bukrs EQ zde_saldo_cta_banco-bukrs.

      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs = zde_saldo_cta_banco-bukrs
        IMPORTING
          e_x001  = wa_moedas.

      wa_bsik_alv-waers_c = wa_t001-waers.
      wa_bsik_alv-waers_d = wa_moedas-hwae2.

      READ TABLE it_bsik_alvo INTO wa_bsik_alv INDEX ls_good-row_id.
      vg_tabix = sy-tabix.

      IF wl_valor_c  NE 0 AND ls_good-value IS INITIAL.
        MOVE wl_valor_c  TO lv_value.
      ELSE.
        lv_value = ls_good-value.
      ENDIF.
      CONDENSE lv_value NO-GAPS.

      wl_valor = lv_value.
      IF wa_bsik_alv-wrbtr < 0 AND wl_valor > 0 OR
         wa_bsik_alv-wrbtr > 0 AND wl_valor < 0.
        MULTIPLY wl_valor BY -1.
        MOVE wl_valor TO lv_value.
        CONDENSE lv_value NO-GAPS.
      ENDIF.

      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_PAYMENTS_B'
          i_value     = lv_value.

      MOVE zde_saldo_cta_banco-waers TO lv_value.

      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WAERS_B'
          i_value     = lv_value.

      i_data           = wa_bsik_alv-budat.
      i_data_pagamento = zde_saldo_cta_banco-budat.
      wa_bsik_alv-valor_payments_b = wl_valor.
      CREATE OBJECT obj_zcl_util_sd.

      IF zde_saldo_cta_banco-waers EQ wa_bsik_alv-waers. "Inicio
        lv_value = wl_valor.

        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_PAYMENTS'
            i_value     = lv_value.

        "Residual
        CLEAR wa_bsik_alv-valor_residual.
        IF  wl_valor NE 0.
          wa_bsik_alv-valor_residual   = wa_bsik_alv-wrbtr - wl_valor.
        ENDIF.

      ELSE. "outras moedas
        IF zde_saldo_cta_banco-waers = wa_t001-waers.
          e_ukurs = 1.
          IF '0201_0202' CS zde_saldo_cta_banco-bukrs.
            e_ukurs = -1.
          ENDIF.
        ELSE.
          obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_saldo_cta_banco-waers ).
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
          obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        ENDIF.

        IF zde_saldo_cta_banco-waers = wa_t001-waers.
          e_ukurs_pag = 1.
        ELSE.
          obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data_pagamento ).
          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_saldo_cta_banco-waers ).
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_t001-waers ).
          obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_pag ).
        ENDIF.

        IF wa_t001-waers = wa_bsik_alv-waers.
          e_ukurs_2 = 1.
        ELSE.
          obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data ).
          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_bsik_alv-waers ).
          obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_2 ).
        ENDIF.

        IF wa_t001-waers = wa_bsik_alv-waers.
          e_ukurs_pag_2 = 1.
          IF zde_saldo_cta_banco-bukrs = '0200'.
            e_ukurs_pag_2 = -1.
          ENDIF.
        ELSE.
          obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data_pagamento ).
          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = wa_t001-waers ).
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_bsik_alv-waers ).
          obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_pag_2 ).
        ENDIF.

        IF zde_saldo_cta_banco-waers = wa_bsik_alv-waers.
          e_ukurs_pag_3 = 1.
        ELSE.
          obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data  ).
          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = zde_saldo_cta_banco-waers ).
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wa_bsik_alv-waers ).
          obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs_pag_3 ).
        ENDIF.

        wa_bsik_alv-taxa_cambio_a_b_dt_doc = e_ukurs.
        IF ( zde_saldo_cta_banco-bukrs = '0200' AND e_ukurs LT 0 ) OR ( '0201_0202' CS zde_saldo_cta_banco-bukrs AND e_ukurs_pag_2 GT 0 ).
          IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( zde_saldo_cta_banco-waers EQ 'USD' AND wa_t001-waers EQ 'EUR' ).
            wa_bsik_alv-taxa_cambio_a_b_dt_doc = e_ukurs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_b * abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b * abs( e_ukurs_pag ).

          ELSE.
            wa_bsik_alv-taxa_cambio_a_b_dt_doc = e_ukurs * -1.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_b / abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b / abs( e_ukurs_pag ).
          ENDIF.
        ELSE.
          IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( zde_saldo_cta_banco-waers EQ 'EUR' AND wa_t001-waers EQ 'USD' ).
            wa_bsik_alv-taxa_cambio_a_b_dt_doc = e_ukurs * -1.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_b / abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b / abs( e_ukurs_pag ).
          ELSE.
            wa_bsik_alv-taxa_cambio_a_b_dt_doc = e_ukurs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_b * abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b * abs( e_ukurs_pag ).
          ENDIF.
        ENDIF.

        IF e_ukurs_2 LT 0.
          IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( wa_t001-waers EQ 'USD' AND wa_bsik_alv-waers EQ 'EUR' ).
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a * abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_a * abs( e_ukurs_pag ).
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a / abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_a / abs( e_ukurs_pag ).
          ENDIF.
        ELSE.
          IF '0201_0202' CS zde_saldo_cta_banco-bukrs AND ( wa_t001-waers EQ 'EUR' AND wa_bsik_alv-waers EQ 'USD' ).
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a / abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_a / abs( e_ukurs_pag ).
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a * abs( e_ukurs ).
            wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_a * abs( e_ukurs_pag ).
          ENDIF.
        ENDIF.

        "Residual
        IF e_ukurs_pag_3  LT 0.
          wa_bsik_alv-valor_residual         = wa_bsik_alv-wrbtr  - ( wa_bsik_alv-valor_payments_b /  abs( e_ukurs_pag_3 ) ).         " residual na moeda do documento
        ELSE.
          wa_bsik_alv-valor_residual         = wa_bsik_alv-wrbtr  - ( wa_bsik_alv-valor_payments_b *  abs( e_ukurs_pag_3 ) ).           " residual na moeda do documento
        ENDIF.

        IF wa_bsik_alv-wrbtr < 0 AND wa_bsik_alv-valor_residual > 0.
          wa_bsik_alv-valor_residual  = 0.
        ELSEIF  wa_bsik_alv-wrbtr > 0 AND wa_bsik_alv-valor_residual < 0.
          wa_bsik_alv-valor_residual  = 0.
        ENDIF.


        IF ( zde_saldo_cta_banco-bukrs = '0200' AND e_ukurs LT 0 ) OR ( '0201_0202' CS zde_saldo_cta_banco-bukrs AND e_ukurs_pag_2 GT 0 ).
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-wrbtr  / abs( e_ukurs_pag_2 ).          " Converte da moeda do doc para a moeda interna
          IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ).
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ENDIF.
          wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b.
        ELSE.
          wa_bsik_alv-valor_payments_a       = wa_bsik_alv-wrbtr  * abs( e_ukurs_pag_2 ).              " Converte da moeda do doc para a moeda interna
          IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ENDIF.
          wa_bsik_alv-valor_payments_a_atual = wa_bsik_alv-valor_payments_b.
        ENDIF.

        "Variação na Moeda do Documento
        wa_bsik_alv-valor_variacao_a = wa_bsik_alv-valor_payments_a_atual - wa_bsik_alv-valor_payments_a.

        "Variação para split com residual
*        IF  E_UKURS LT 0.
        IF e_ukurs_pag_2 GT 0.
          wa_bsik_alv-valor_payments_a       = ( wa_bsik_alv-wrbtr - wa_bsik_alv-valor_residual )  / abs( e_ukurs_pag_2 ).              " Converte da moeda do doc para a moeda interna
          IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ENDIF.
        ELSE.
          wa_bsik_alv-valor_payments_a       = ( wa_bsik_alv-wrbtr - wa_bsik_alv-valor_residual ) * abs( e_ukurs_pag_2 ).              " Converte da moeda do doc para a moeda interna
          IF ( zde_saldo_cta_banco-waers = 'GBP' OR wa_bsik_alv-waers = 'GBP' ) AND '0201_0202' CS zde_saldo_cta_banco-bukrs.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a *  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ELSE.
            wa_bsik_alv-valor_payments_a       = wa_bsik_alv-valor_payments_a /  abs( e_ukurs_pag ). " converte da moeda interna para moeda do banco
          ENDIF.
        ENDIF.
        "Variação na Moeda do Documento
        wa_bsik_alv-valor_variacao_c = wa_bsik_alv-valor_payments_a_atual - wa_bsik_alv-valor_payments_a.

        wa_bsik_alv-valor_payments_a = wa_bsik_alv-valor_payments_a_atual.

        SELECT SINGLE * INTO wa_t030h FROM t030h
         WHERE ktopl EQ '0050'
           AND hkont EQ wa_bsik_alv-saknr.

        IF wa_bsik_alv-valor_variacao_a GT 0 AND wa_bsik_alv-koart EQ 'K'.
          wa_bsik_alv-saknr_variacao_b = wa_t030h-lsrea.
        ELSEIF wa_bsik_alv-valor_variacao_a LE 0 AND wa_bsik_alv-koart EQ 'K'.
          wa_bsik_alv-saknr_variacao_b = wa_t030h-lhrea.
        ENDIF.

        IF wa_bsik_alv-valor_variacao_a GT 0 AND wa_bsik_alv-koart EQ 'D'.
          wa_bsik_alv-saknr_variacao_b = wa_t030h-lhrea.
        ELSEIF wa_bsik_alv-valor_variacao_a LT 0 AND wa_bsik_alv-koart EQ 'D'.
          wa_bsik_alv-saknr_variacao_b = wa_t030h-lsrea.
        ENDIF.
      ENDIF. "Fim

      lv_value = wa_bsik_alv-valor_payments_a.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_PAYMENTS_A'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_payments_c.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_PAYMENTS_C'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_payments_d.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_PAYMENTS_D'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_variacao_a.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_VARIACAO_A'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-taxa_cambio_a_b_dt_doc.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TAXA_CAMBIO_A_B_DT_DOC'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_variacao_b.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_VARIACAO_B'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_variacao_c.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_VARIACAO_C'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_variacao_d.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_VARIACAO_D'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_residual.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_RESIDUAL'
          i_value     = lv_value.

      lv_value = wa_bsik_alv-valor_payments_a_atual.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_PAYMENTS_A_ATUAL'
          i_value     = lv_value.

*      MOVE ZDE_MOV_FORNECEDOR-SGTXT TO LV_VALUE.
*
*      CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'SGTXT'
*          I_VALUE     = LV_VALUE.

      lv_value = wa_bsik_alv-saknr_variacao_b.
      CALL METHOD pr_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SAKNR_VARIACAO_B'
          i_value     = lv_value.

    ENDLOOP.

  ENDMETHOD.                    "perform_semantic_checks

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    FIELD-SYMBOLS: <fsmov> TYPE ty_zde_mov_fornecedor.

    IF e_modified IS NOT INITIAL.

      zde_saldo_cta_banco-saldod = copia-saldod.
      CLEAR sd_compen.
      LOOP AT it_bsik_alvo ASSIGNING <fsmov>.

        CASE <fsmov>-shkzg.
          WHEN 'H'. "Crédito
            IF <fsmov>-ck_payment EQ abap_true.
              zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod - <fsmov>-valor_payments_b.
              sd_compen = sd_compen + <fsmov>-valor_payments_b.
            ENDIF.
            "<FSMOV>-VALOR_RESIDUAL = ( ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS ) * 1.
          WHEN 'S'. "Debito
            IF <fsmov>-ck_payment EQ abap_true.
              zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + <fsmov>-valor_payments_b.
              sd_compen = sd_compen + <fsmov>-valor_payments_b.
            ENDIF.
            "<FSMOV>-VALOR_RESIDUAL = ABS( <FSMOV>-WRBTR ) - <FSMOV>-VALOR_PAYMENTS.
        ENDCASE.

*        IF ( ZDE_MOV_FORNECEDOR-SGTXT IS NOT INITIAL ) AND ( <FSMOV>-DATA_RESIDUAL IS NOT INITIAL ).
*          <FSMOV>-SGTXT = ZDE_MOV_FORNECEDOR-SGTXT.
*        ELSEif <FSMOV>-CK_PAYMENT EQ ABAP_TRUE.
*          CLEAR <FSMOV>-SGTXT.
*        ENDIF.
      ENDLOOP.

      PERFORM calcular_saldo.

      LEAVE TO SCREEN 0003.
    ELSE.
      LOOP AT it_bsik_alvo ASSIGNING <fsmov>.
        DATA(tabix) = sy-tabix.
        IF <fsmov>-ck_payment = 'X' AND <fsmov>-valor_residual NE 0 AND <fsmov>-data_residual IS INITIAL.
          zde_mov_fornecedor-data_residual = <fsmov>-data_residual.
          zde_mov_fornecedor-sgtxt         = <fsmov>-sgtxt.
          CALL SCREEN 0303 STARTING AT 15 01.
          IF zde_mov_fornecedor-data_residual IS NOT INITIAL AND zde_mov_fornecedor-sgtxt IS NOT INITIAL.
            <fsmov>-data_residual = zde_mov_fornecedor-data_residual.
            <fsmov>-sgtxt        = zde_mov_fornecedor-sgtxt.
            MODIFY it_bsik_alvo FROM <fsmov> INDEX tabix TRANSPORTING data_residual sgtxt.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

ENDCLASS.                    "LCL_EVENT_RECEIVER_0308 IMPLEMENTATION


DATA: ctl_con_0302       TYPE REF TO cl_gui_custom_container,
      gs_lay_0302        TYPE lvc_s_layo,
      gs_var_0302        TYPE disvariant,
      gs_scroll_col_0302 TYPE lvc_s_col,
      gs_scroll_row_0302 TYPE lvc_s_roid,
      it_catalog_0302    TYPE lvc_t_fcat,
      obg_toolbar_0302   TYPE REF TO lcl_alv_toolbar_0302.

DATA: it_selected_0302 TYPE lvc_t_row,
      wa_selected_0302 TYPE lvc_s_row.

DATA: it_exclude_0302 TYPE ui_functions,
      wa_exclude_0302 LIKE LINE OF it_exclude_0302.

DATA: ctl_con_0308       TYPE REF TO cl_gui_custom_container,
      gs_lay_0308        TYPE lvc_s_layo,
      gs_var_0308        TYPE disvariant,
      gs_scroll_col_0308 TYPE lvc_s_col,
      gs_scroll_row_0308 TYPE lvc_s_roid,
      it_catalog_0308    TYPE lvc_t_fcat,
      obg_toolbar_0308   TYPE REF TO lcl_alv_toolbar_0308.

DATA: it_selected_0308 TYPE lvc_t_row,
      wa_selected_0308 TYPE lvc_s_row.

DATA: it_exclude_0308 TYPE ui_functions,
      wa_exclude_0308 LIKE LINE OF it_exclude_0308.

DATA: dt_inicial  TYPE dzfbdt,
      dt_final    TYPE dzfbdt,
* Inicio - falheiros - 22.12.2022 - CS2022000110
      dt_inicial2 TYPE dzfbdt,
      dt_final2   TYPE dzfbdt.

* Fim - falheiros - 22.12.2022 - CS2022000110
*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  ck_tela_baixa = abap_true.

  SET PF-STATUS 'PF0003'.
  SET TITLEBAR 'TL0003' WITH zde_saldo_cta_banco-budat.

ENDMODULE.                 " STATUS_0003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.
  DATA: it_zglt036 TYPE TABLE OF zglt036,
        v_ok(1).
  CASE ok_code.
    WHEN 'TLOTE'.
      PERFORM gerar_lote_documentos.
*      CLEAR OK_CODE.
      ok_code = 'ATUALI'.
    WHEN 'COMP'.
      REFRESH it_zglt036.
      PERFORM gerar_compensacao_fornecedor TABLES it_zglt036.
*      CLEAR OK_CODE.
      ok_code = 'ATUALI'.
    WHEN 'COMPLOTE'.
      CLEAR v_ok.
      LOOP AT it_bsik_alv WHERE ck_payment EQ abap_true.
        v_ok = 'S'.
      ENDLOOP.
      LOOP AT it_bsik_alvo WHERE ck_payment EQ abap_true.
        v_ok = 'S'.
      ENDLOOP.
      IF v_ok = 'S'.
        tp_comp = 'D'.
        PERFORM gerar_lote_documentos.
        CLEAR  tp_comp.
        ok_code = 'ATUALI'.
      ENDIF.
    WHEN 'ATUALI'.

* Inicio - falheiros - 22.12.2022 - CS2022000110
*      PERFORM atualizar_partidas_aberto USING dt_inicial dt_final kna1-kunnr lfa1-lifnr.

      IF dt_inicial2 IS NOT INITIAL AND dt_inicial IS NOT INITIAL.
        MESSAGE text-100 TYPE 'E'.
        EXIT.
      ENDIF.
      PERFORM atualizar_partidas_aberto
      USING dt_inicial dt_final kna1-kunnr lfa1-lifnr dt_inicial2 dt_final2.
* Fim - falheiros - 22.12.2022 - CS2022000110

*      CLEAR OK_CODE.
      ok_code = 'ATUALI'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003_exit INPUT.
  ck_tela_baixa = abap_false.
  MOVE copia TO zde_saldo_cta_banco.
  CLEAR: it_bsik_alv[], it_bsik_alvo[].
  PERFORM busca_dados.
*  LEAVE TO SCREEN 0001.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0003_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  vg_text_008 = text-008.
  vg_text_009 = text-009.
  vg_text_010 = text-018.

  CLEAR: zde_mov_fornecedor-name1.
  IF lfa1-lifnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO lfa1-name1 FROM lfa1 WHERE lifnr EQ lfa1-lifnr.
    IF kna1-kunnr IS INITIAL.
      SELECT SINGLE kunnr INTO  kna1-kunnr FROM lfa1 WHERE lifnr EQ lfa1-lifnr.
    ENDIF.
  ELSE.
    CLEAR: lfa1-name1.
  ENDIF.

  IF kna1-kunnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO kna1-name1 FROM kna1 WHERE kunnr EQ kna1-kunnr.
    IF lfa1-lifnr IS INITIAL.
      SELECT SINGLE lifnr INTO  lfa1-lifnr FROM kna1 WHERE kunnr EQ kna1-kunnr.
      IF sy-subrc = 0.
        SELECT SINGLE name1 INTO lfa1-name1 FROM lfa1 WHERE lifnr EQ lfa1-lifnr.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: kna1-name1.
  ENDIF.

ENDMODULE.                 " STATUS_0301  OUTPUT

FORM estorno_fbra USING p_mov_cta_banco     TYPE zde_mov_cta_banco.
  DATA: ti_bdcdata  TYPE TABLE OF bdcdata WITH HEADER LINE,
        it_message  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        wa_retorno  TYPE bdcmsgcoll,
        lds_return  TYPE bapiret2,
        lc_mode     TYPE char01,
        msgtxt      TYPE string,
        t_bsak      TYPE TABLE OF bsak,
        w_bsak      TYPE bsak,
        v_belnr     TYPE bsak-belnr,
        w_erro(1),
        w_answer(1).

  IF p_mov_cta_banco-xstov = 'X'.
    MESSAGE text-020 TYPE 'I'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = text-022
      text_button_1         = 'Yes'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'No'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF w_answer = '2'. "não
    EXIT.
  ENDIF.


  SELECT *
     INTO TABLE t_bsak
     FROM bsak
     WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     "AND   LIFNR EQ P_MOV_CTA_BANCO-LIFNR
     AND   augbl EQ p_mov_cta_banco-belnr
     AND   gjahr EQ p_mov_cta_banco-gjahr.
  DELETE t_bsak WHERE belnr = p_mov_cta_banco-belnr.

  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE t_bsak
    FROM bsad
    WHERE bukrs EQ zde_saldo_cta_banco-bukrs
    "AND   KUNNR EQ P_MOV_CTA_BANCO-KUNNR
    AND   augbl EQ p_mov_cta_banco-belnr
    AND   gjahr EQ p_mov_cta_banco-gjahr.
  DELETE t_bsak WHERE belnr = p_mov_cta_banco-belnr.

  "
  "Baixado no banco
  CALL FUNCTION 'CALL_FBRA'
    EXPORTING
      i_bukrs      = zde_saldo_cta_banco-bukrs
      i_augbl      = p_mov_cta_banco-belnr
      i_gjahr      = p_mov_cta_banco-gjahr
      i_mode       = 'N'
    EXCEPTIONS
      not_possible = 1
      OTHERS       = 2.
  COMMIT WORK.


  IF sy-subrc = 0.
    CALL FUNCTION 'CALL_FB08'
      EXPORTING
        i_bukrs      = zde_saldo_cta_banco-bukrs
        i_belnr      = p_mov_cta_banco-belnr
        i_gjahr      = p_mov_cta_banco-gjahr
        i_stgrd      = '01'
        i_mode       = 'N'
      EXCEPTIONS
        not_possible = 1
        OTHERS       = 2.
    COMMIT WORK.
  ELSE.
    MESSAGE s000 WITH text-021 p_mov_cta_banco-belnr DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  "Split mesma data compensação
  LOOP AT t_bsak INTO w_bsak.
    v_belnr =  w_bsak-belnr.
    SELECT SINGLE *
      FROM bkpf
      INTO @DATA(w_bkpf)
      WHERE bukrs      = @zde_saldo_cta_banco-bukrs
      AND   belnr      = @v_belnr
      AND   gjahr      = @p_mov_cta_banco-gjahr.
    SELECT SINGLE *
    FROM bkpf
    INTO @DATA(w_bkpf2)
    WHERE bukrs      = @zde_saldo_cta_banco-bukrs
    AND   belnr      = @p_mov_cta_banco-belnr
    AND   gjahr      = @p_mov_cta_banco-gjahr.
    IF w_bkpf-budat = w_bkpf2-budat AND w_bkpf-waers NE w_bkpf2-waers.
      CALL FUNCTION 'CALL_FBRA'
        EXPORTING
          i_bukrs      = zde_saldo_cta_banco-bukrs
          i_augbl      = v_belnr
          i_gjahr      = p_mov_cta_banco-gjahr
          i_mode       = 'N'
        EXCEPTIONS
          not_possible = 1
          OTHERS       = 2.
      COMMIT WORK.

      IF sy-subrc = 0.
        CALL FUNCTION 'CALL_FB08'
          EXPORTING
            i_bukrs      = zde_saldo_cta_banco-bukrs
            i_belnr      = v_belnr
            i_gjahr      = p_mov_cta_banco-gjahr
            i_stgrd      = '01'
            i_mode       = 'N'
          EXCEPTIONS
            not_possible = 1
            OTHERS       = 2.
        COMMIT WORK.
      ELSE.
        v_belnr =
        w_erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF w_erro = 'X'.
    MESSAGE s000 WITH text-021 v_belnr DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE text-019 TYPE 'I'.
  ENDIF.

ENDFORM.

"
*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_PARTIDAS_ABERTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DT_INICIAL  text
*      -->P_DT_FINAL  text
*----------------------------------------------------------------------*
FORM atualizar_partidas_aberto  USING  p_inicial TYPE dzfbdt
                                       p_final   TYPE dzfbdt
                                       p_cliente TYPE kunnr
                                       p_fornece TYPE lifnr
* Inicio - falheiros - 22.12.2022 - CS2022000110
                                       p_inicial2 TYPE dzfbdt
                                       p_final2   TYPE dzfbdt.

  DATA: v_bldat TYPE char1.
* Fim - falheiros - 22.12.2022 - CS2022000110

  DATA: e_x001        LIKE x001,
        lc_fator      TYPE i,
        wa_t001_waers TYPE t001-waers,
        wa_style      TYPE lvc_s_styl,
        style         TYPE lvc_t_styl WITH HEADER LINE.

  CLEAR: it_bsik[],
         it_bsid[],
         it_bsik_2[],
         it_bsid_2[].

  SELECT SINGLE waers INTO wa_t001_waers
    FROM t001
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs.

* Inicio - falheiros - 23.12.2022
*  IF p_inicial IS INITIAL AND ( p_cliente IS INITIAL AND p_fornece IS INITIAL ).
  IF p_inicial IS INITIAL AND p_inicial2 IS INITIAL
    AND ( p_cliente IS INITIAL AND p_fornece IS INITIAL ).
* Fim - falheiros - 23.12.2022
    MESSAGE s052 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR sd_compen.
  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = zde_saldo_cta_banco-bukrs
    IMPORTING
      e_x001  = e_x001.

  IF p_cliente IS NOT INITIAL OR p_fornece IS NOT INITIAL.
    IF p_cliente IS INITIAL AND p_fornece IS NOT INITIAL.
      SELECT SINGLE kunnr INTO p_cliente FROM lfa1 WHERE lifnr = p_fornece.
    ENDIF.
    IF p_cliente IS NOT INITIAL AND p_fornece IS INITIAL.
      SELECT SINGLE lifnr INTO p_fornece FROM kna1 WHERE kunnr = p_cliente.
    ENDIF.
  ENDIF.

* Inicio - falheiros - 23.12.2022
  FREE MEMORY ID 'M_BLDAT'.
  IF p_inicial2 IS NOT INITIAL.
    p_inicial = p_inicial2.
    p_final   = p_final2.

* Inicio - falheiros - 23.12.2022
    v_bldat = abap_true.
    EXPORT: v_bldat  TO MEMORY ID 'M_BLDAT'.
* Fim - falheiros - 23.12.2022
  ENDIF.
* Fim - falheiros - 23.12.2022

  IF p_cliente IS NOT INITIAL OR p_fornece IS NOT INITIAL.
    IF p_cliente IS NOT INITIAL.
      CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
        EXPORTING
          i_company            = zde_saldo_cta_banco-bukrs
          "I_MOEDA_DOC          = ZDE_SALDO_CTA_BANCO-WAERS
          i_forne              = abap_false
          i_cliente            = abap_true
          i_parid              = p_cliente
          i_data_venc_ini      = p_inicial
          i_data_venc_final    = p_final
          i_nao_razao_especial = abap_false
        TABLES
          it_bsxk              = it_bsik
          it_bsxd              = it_bsid
          it_bkpf              = it_bkpf_1.
    ENDIF.

    IF p_fornece IS NOT INITIAL.

      CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
        EXPORTING
          i_company            = zde_saldo_cta_banco-bukrs
          "I_MOEDA_DOC         = ZDE_SALDO_CTA_BANCO-WAERS
          i_forne              = abap_true
          i_cliente            = abap_false
          i_parid              = p_fornece
          i_data_venc_ini      = p_inicial
          i_data_venc_final    = p_final
          i_nao_razao_especial = abap_false
        TABLES
          it_bsxk              = it_bsik_2
          it_bsxd              = it_bsid_2
          it_bkpf              = it_bkpf_2.

      LOOP AT it_bsik_2.
        APPEND it_bsik_2 TO it_bsik.
      ENDLOOP.

      LOOP AT it_bsid_2.
        APPEND it_bsid_2 TO it_bsid.
      ENDLOOP.

      LOOP AT it_bkpf_2.
        APPEND it_bkpf_2 TO it_bkpf_1.
      ENDLOOP.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FI_GL_PART_ABERTO'
      EXPORTING
        i_company            = zde_saldo_cta_banco-bukrs
        "I_MOEDA_DOC          = ZDE_SALDO_CTA_BANCO-WAERS
        i_forne              = abap_true
        i_cliente            = abap_true
        i_data_venc_ini      = p_inicial
        i_data_venc_final    = p_final
        i_nao_razao_especial = abap_false
      TABLES
        it_bsxk              = it_bsik
        it_bsxd              = it_bsid
        it_bkpf              = it_bkpf_1.
  ENDIF.

  CLEAR: it_bsik_alv[],
         it_bsik_alvo[].

  DELETE it_bsik WHERE blart = 'VC'.

  IF it_bsik[] IS NOT INITIAL.

    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_bsik
     WHERE lifnr EQ it_bsik-lifnr.

    SORT it_lfa1 BY lifnr.
    LOOP AT it_bsik.
      "
      CLEAR: it_bsik_alv.

      IF it_bsik-shkzg EQ 'H'.
        lc_fator = -1.
      ELSE.
        lc_fator = 1.
      ENDIF.

      it_bsik_alv-zfbdt          = it_bsik-zfbdt.
      it_bsik_alv-parid          = it_bsik-lifnr.
      it_bsik_alv-zuonr          = it_bsik-zuonr.
      it_bsik_alv-belnr          = it_bsik-belnr.
      it_bsik_alv-buzei          = it_bsik-buzei.
      it_bsik_alv-umskz          = it_bsik-umskz.
      it_bsik_alv-bldat          = it_bsik-bldat.
      it_bsik_alv-budat          = it_bsik-budat.
      it_bsik_alv-waers          = it_bsik-waers.
      it_bsik_alv-wrbtr          = it_bsik-wrbtr * lc_fator.
      it_bsik_alv-dmbe2          = it_bsik-dmbe2 * lc_fator.
      it_bsik_alv-dmbe3          = it_bsik-dmbe3 * lc_fator.
      it_bsik_alv-dmbtr          = it_bsik-dmbtr * lc_fator.
      it_bsik_alv-ck_payment     = abap_false.
      it_bsik_alv-hwae2          = e_x001-hwae2.
*      IT_BSIK_ALV-VALOR_PAYMENTS = IT_BSIK-WRBTR.
      it_bsik_alv-valor_residual = 0.
      it_bsik_alv-bukrs          = it_bsik-bukrs.
      it_bsik_alv-gjahr          = it_bsik-gjahr.
      it_bsik_alv-shkzg          = it_bsik-shkzg.
      it_bsik_alv-saknr          = it_bsik-saknr.
      it_bsik_alv-sgtxt          = it_bsik-sgtxt.
      it_bsik_alv-koart          = 'K'.
      CONCATENATE it_bsik_alv-parid  it_bsik_alv-koart INTO it_bsik_alv-parid_koart.
      READ TABLE it_lfa1 WITH KEY lifnr = it_bsik-lifnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        it_bsik_alv-name1 = it_lfa1-name1.
      ENDIF.

      " Ajusta Informação de Referência """""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE it_bkpf_1 WITH KEY bukrs = it_bsik_alv-bukrs
                                    belnr = it_bsik_alv-belnr
                                    gjahr = it_bsik_alv-gjahr.
      IF sy-subrc IS INITIAL.
        IF it_bkpf_1-bktxt EQ 'COMEX'.
          it_bsik_alv-xblnr = it_bsik-kidno.
        ELSE.
          it_bsik_alv-xblnr = it_bkpf_1-xblnr.
        ENDIF.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      REFRESH: style.
      CLEAR: wa_style.

      IF it_bsik_alv-waers = zde_saldo_cta_banco-waers.
        wa_style-fieldname = 'VALOR_RESIDUAL'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        it_bsik_alv-style[] = style[].
      ENDIF.
      APPEND it_bsik_alv.
    ENDLOOP.
  ENDIF.

  DELETE it_bsid WHERE blart = 'VC'.
  IF it_bsid[] IS NOT INITIAL.

    SELECT * INTO TABLE it_kna1
      FROM kna1
       FOR ALL ENTRIES IN it_bsid
     WHERE kunnr EQ it_bsid-kunnr.

    SORT it_kna1 BY kunnr.
    LOOP AT it_bsid.

      CLEAR: it_bsik_alv.
      IF it_bsid-shkzg EQ 'H'.
        lc_fator = -1.
      ELSE.
        lc_fator = 1.
      ENDIF.

      it_bsik_alv-zfbdt          = it_bsid-zfbdt.
      it_bsik_alv-parid          = it_bsid-kunnr.
      it_bsik_alv-zuonr          = it_bsid-zuonr.
      it_bsik_alv-belnr          = it_bsid-belnr.
      it_bsik_alv-buzei          = it_bsid-buzei.
      it_bsik_alv-umskz          = it_bsid-umskz.
      it_bsik_alv-bldat          = it_bsid-bldat.
      it_bsik_alv-budat          = it_bsid-budat.
      it_bsik_alv-waers          = it_bsid-waers.
      it_bsik_alv-wrbtr          = it_bsid-wrbtr * lc_fator.
      it_bsik_alv-dmbe2          = it_bsid-dmbe2 * lc_fator.
      it_bsik_alv-dmbe3          = it_bsid-dmbe3 * lc_fator.
      it_bsik_alv-dmbtr          = it_bsid-dmbtr * lc_fator.
      it_bsik_alv-ck_payment     = abap_false.
      it_bsik_alv-hwae2          = e_x001-hwae2.
*      IT_BSIK_ALV-VALOR_PAYMENTS = IT_BSID-WRBTR.
      it_bsik_alv-valor_residual = 0.
      it_bsik_alv-bukrs          = it_bsid-bukrs.
      it_bsik_alv-gjahr          = it_bsid-gjahr.
      it_bsik_alv-sgtxt          = it_bsid-sgtxt.
      it_bsik_alv-shkzg          = it_bsid-shkzg.
      it_bsik_alv-saknr          = it_bsid-saknr.
      it_bsik_alv-koart          = 'D'.
      CONCATENATE it_bsik_alv-parid  it_bsik_alv-koart INTO it_bsik_alv-parid_koart.

      READ TABLE it_kna1 WITH KEY kunnr = it_bsid-kunnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        it_bsik_alv-name1 = it_kna1-name1.
      ENDIF.

      " Ajusta Informação de Referência """""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE it_bkpf_1 WITH KEY bukrs = it_bsik_alv-bukrs
                                    belnr = it_bsik_alv-belnr
                                    gjahr = it_bsik_alv-gjahr.
      IF sy-subrc IS INITIAL.
        IF it_bkpf_1-bktxt EQ 'COMEX'.
          it_bsik_alv-xblnr = it_bkpf_1-awkey.
        ELSE.
          it_bsik_alv-xblnr = it_bkpf_1-xblnr.
        ENDIF.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      REFRESH: style.
      CLEAR: wa_style.

      IF it_bsik_alv-waers = zde_saldo_cta_banco-waers.
        wa_style-fieldname = 'VALOR_RESIDUAL'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        it_bsik_alv-style[] = style[].
      ENDIF.

      APPEND it_bsik_alv.
    ENDLOOP.
  ENDIF.

*  IF ZDE_SALDO_CTA_BANCO-WAERS EQ WA_T001_WAERS OR
*     ZDE_SALDO_CTA_BANCO-WAERS EQ E_X001-HWAE2  OR
*     ZDE_SALDO_CTA_BANCO-WAERS EQ E_X001-HWAE3 .
  MOVE it_bsik_alv[] TO it_bsik_alvo[].
*  ENDIF.

  DELETE it_bsik_alv  WHERE waers NE zde_saldo_cta_banco-waers.
  DELETE it_bsik_alvo WHERE waers EQ zde_saldo_cta_banco-waers.

*  MOVE IT_BSIK_ALV[] TO IT_BSIK_ALVO[].
*  REFRESH IT_BSIK_ALV.
  IF ctl_alv_0302 IS NOT INITIAL.
    CALL METHOD ctl_alv_0302->refresh_table_display.
  ENDIF.

  IF ctl_alv_0308 IS NOT INITIAL.
    CALL METHOD ctl_alv_0308->refresh_table_display.
  ENDIF.

* Inicio - falheiros - 23.12.2022
  IF p_inicial2 IS NOT INITIAL.
    CLEAR: p_inicial, p_final.
  ENDIF.
* Fim - falheiros - 23.12.2022

ENDFORM.                    " ATUALIZAR_PARTIDAS_ABERTO

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0302 OUTPUT.
  DATA: lc_text_date3 TYPE c LENGTH 10.
  DATA: fs_sort TYPE lvc_s_sort,
        gt_sort TYPE lvc_t_sort.

  IF ctl_con_0302 IS INITIAL.

    CREATE OBJECT ctl_con_0302
      EXPORTING
        container_name = 'ALV_PART'.

    CREATE OBJECT ctl_alv_0302
      EXPORTING
        i_parent = ctl_con_0302.

    CREATE OBJECT obg_toolbar_0302
      EXPORTING
        io_alv_grid = ctl_alv_0302.

    SET HANDLER obg_toolbar_0302->on_toolbar FOR ctl_alv_0302.
    SET HANDLER obg_toolbar_0302->handle_user_command FOR ctl_alv_0302.

    PERFORM fill_it_fieldcatalog_0302.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0302.
*   Set layout parameters for ALV grid

    gs_lay_0302-zebra      = 'X'.
    gs_lay_0302-edit_mode  = 'X'.
*    GS_LAY_0302-NO_TOTLINE = 'X'.
    gs_lay_0302-col_opt    = 'X'.
    gs_lay_0302-stylefname = 'STYLE'.


    APPEND '&LOCAL&CUT'           TO it_exclude_0302.
    APPEND '&LOCAL&INSERT_ROW'    TO it_exclude_0302.
    APPEND '&LOCAL&MOVE_ROW'      TO it_exclude_0302.
    APPEND '&LOCAL&PASTE'         TO it_exclude_0302.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO it_exclude_0302.
    APPEND '&LOCAL&UNDO'          TO it_exclude_0302.
    APPEND '&VARI_ADMIN'          TO it_exclude_0302.
    APPEND '&LOCAL&APPEND'        TO it_exclude_0302.
    APPEND '&LOCAL&COPY'          TO it_exclude_0302.
    APPEND '&LOCAL&COPY_ROW'      TO it_exclude_0302.
    APPEND '&VLOTUS'              TO it_exclude_0302.
    APPEND '&AQW'                 TO it_exclude_0302.
    APPEND '&PRINT'               TO it_exclude_0302.
*    APPEND '&MB_SUM'              TO IT_EXCLUDE_0302.
*    APPEND '&AVERAGE'             TO IT_EXCLUDE_0302.
    APPEND '&MB_VIEW'             TO it_exclude_0302.
    APPEND '&MB_EXPORT'           TO it_exclude_0302.
*    APPEND '&MB_FILTER'           TO it_exclude_0302.
    APPEND '&GRAPH'               TO it_exclude_0302.
    APPEND '&INFO'                TO it_exclude_0302.
    APPEND '&LOCAL&DELETE_ROW'    TO it_exclude_0302.
    APPEND '&CHECK'               TO it_exclude_0302.

    fs_sort-spos       = 1.     "first sorting key
    fs_sort-fieldname  = 'PARID_KOART'. "fieldname for sort
    fs_sort-up         = 'X'. "sort ascending
    fs_sort-subtot     = 'X'. "do subtotal
    fs_sort-no_out     = 'X'. "no display
    fs_sort-obligatory = 'X'. "sort is obligatory
    INSERT fs_sort INTO TABLE gt_sort. "insert to sort table

    CLEAR: fs_sort.
    fs_sort-spos       = 2.     "first sorting key
    fs_sort-fieldname  = 'ZFBDT'. "fieldname for sort
    fs_sort-up         = 'X'. "sort ascending
    INSERT fs_sort INTO TABLE gt_sort. "insert to sort table

*    CLEAR gs_var_0302.
    CALL METHOD ctl_alv_0302->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0302
        is_variant           = gs_var_0302
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0302
      CHANGING
        it_fieldcatalog      = it_catalog_0302        "IT_EXCEPT_QINFO = IT_HINTS
        it_outtab            = it_bsik_alv[]
        it_sort              = gt_sort[].

    CALL METHOD ctl_alv_0302->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_0302->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_0302.
    SET HANDLER event_handler_0302->data_changed_finished FOR ctl_alv_0302.
    SET HANDLER event_handler_0302->data_changed          FOR ctl_alv_0302.
    SET HANDLER event_handler_0302->handle_hotspot_click  FOR ctl_alv_0302.
    SET HANDLER event_handler_0302->subtotal_text         FOR ctl_alv_0302.

    CALL METHOD ctl_alv_0302->refresh_table_display.

  ELSE.
    wa_stable-row = abap_true.
    wa_stable-col = abap_true.
    CALL METHOD ctl_alv_0302->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD ctl_alv_0302->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0302
      es_row_no   = gs_scroll_row_0302.

ENDMODULE.                 " STATUS_0302  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0302 .

*-CS2019001066 - 04.05.2021 - JT - inicio
  gs_var_0302-report      = sy-repid.
  CLEAR gs_var_0302-handle.
  gs_var_0302-handle      = '0302'.
  gs_var_0302-username    = sy-uname.
*  gs_var_0302-log_group   = abap_false.
*  gs_var_0302-username    = abap_false.
*  gs_var_0302-variant     = abap_false.
*  gs_var_0302-text        = abap_false.
*  gs_var_0302-dependvars  = abap_false.
*-CS2019001066 - 04.05.2021 - JT - fim

ENDFORM.                    " FILL_GS_VARIANT_0302

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0308
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0308 .

*-CS2019001066 - 04.05.2021 - JT - inicio
  gs_var_0308-report      = sy-repid.
  gs_var_0308-username    = sy-uname.
  gs_var_0308-handle      = '0308'.
*  gs_var_0308-log_group   = abap_false.
*  gs_var_0308-username    = abap_false.
*  gs_var_0308-variant     = abap_false.
*  gs_var_0308-text        = abap_false.
*  gs_var_0308-dependvars  = abap_false.
*-CS2019001066 - 04.05.2021 - JT - fim

ENDFORM.                    " FILL_GS_VARIANT_0302

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0302 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0302> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_MOV_FORNECEDOR'
    CHANGING
      ct_fieldcat      = it_catalog_0302.

  lc_col_pos = 1.

  LOOP AT it_catalog_0302 ASSIGNING <fs_cat_0302>.
    CASE <fs_cat_0302>-fieldname.
      WHEN 'CK_PAYMENT'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'ZFBDT'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'PARID'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'NAME1'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'ZUONR'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BELNR'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'UMSKZ'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BLDAT'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BUDAT'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'WAERS'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'WRBTR'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'HWAE2'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'DMBE2'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_PAYMENTS'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_RESIDUAL'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'DATA_RESIDUAL'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'AUGTX'.
        <fs_cat_0302>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
    ENDCASE.
  ENDLOOP.

  LOOP AT it_catalog_0302 ASSIGNING <fs_cat_0302>.
    IF <fs_cat_0302>-col_pos IS INITIAL.
      <fs_cat_0302>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_0302>-tabname = 'IT_MOV_CTA_BANCO'.
    CASE <fs_cat_0302>-fieldname.
      WHEN 'CK_PAYMENT'.
        <fs_cat_0302>-checkbox  = abap_true.
        <fs_cat_0302>-edit      = abap_true.
        <fs_cat_0302>-outputlen = 03.
        CLEAR: <fs_cat_0302>-coltext,
               <fs_cat_0302>-scrtext_l,
               <fs_cat_0302>-scrtext_m,
               <fs_cat_0302>-scrtext_s.
      WHEN 'VALOR_PAYMENTS' OR 'VALOR_RESIDUAL'.
        <fs_cat_0302>-do_sum    = abap_true.
        <fs_cat_0302>-outputlen = 15.
        <fs_cat_0302>-edit      = abap_true.
      WHEN 'ZUONR'.
        <fs_cat_0302>-outputlen = 10.
      WHEN 'BUKRS'            OR 'SHKZG'            OR 'PARID_KOART'      OR 'GJAHR'            OR 'WAERS_B'          OR 'VALOR_PAYMENTS_B' OR
           'VALOR_VARIACAO_B' OR 'DMBTR'            OR 'DMBE3'            OR 'SAKNR_VARIACAO_B' OR 'SAKNR'            OR 'WAERS_C'          OR
           'WAERS_D'          OR 'VALOR_PAYMENTS_A' OR 'VALOR_PAYMENTS_C' OR 'VALOR_PAYMENTS_D' OR 'VALOR_VARIACAO_A' OR 'VALOR_VARIACAO_C' OR
           'VALOR_VARIACAO_D'.
        <fs_cat_0302>-no_out    = abap_true.
      WHEN 'BELNR' OR 'STBLG'.
        <fs_cat_0302>-hotspot   = abap_true.
      WHEN 'DMBTRH' OR 'DMBTRS' OR 'WRBTR'.
        <fs_cat_0302>-outputlen = 15.
      WHEN 'SGTXT' OR 'NAME1'.
        <fs_cat_0302>-outputlen = 20.
      WHEN 'AUGTX'.
        <fs_cat_0302>-outputlen = 20.
        <fs_cat_0302>-edit      = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0302

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0308
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0308 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0308> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_MOV_FORNECEDOR'
    CHANGING
      ct_fieldcat      = it_catalog_0308.

  lc_col_pos = 1.

  LOOP AT it_catalog_0308 ASSIGNING <fs_cat_0308>.
    CASE <fs_cat_0308>-fieldname.
      WHEN 'CK_PAYMENT'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'ZFBDT'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'PARID'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'NAME1'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'ZUONR'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BELNR'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'UMSKZ'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BLDAT'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'BUDAT'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'WAERS'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'WRBTR'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'HWAE2'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'DMBE2'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_PAYMENTS'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.

        "A - Moeda do Documento
        "B - Moeda do Banco
        "C - Moeda Interna
        "D - Moeda Forte

      WHEN 'VALOR_PAYMENTS_B'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_VARIACAO_B'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_PAYMENTS_A'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_VARIACAO_A'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_PAYMENTS_C'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_VARIACAO_C'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_PAYMENTS_D'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'VALOR_VARIACAO_D'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'AUGTX'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
      WHEN 'SAKNR_VARIACAO_B'.
        <fs_cat_0308>-col_pos = lc_col_pos.
        ADD 1 TO lc_col_pos.
    ENDCASE.
  ENDLOOP.

  LOOP AT it_catalog_0308 ASSIGNING <fs_cat_0308>.
    IF <fs_cat_0308>-col_pos IS INITIAL.
      <fs_cat_0308>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.
    <fs_cat_0308>-tabname = 'IT_MOV_CTA_BANCO'.
    CASE <fs_cat_0308>-fieldname.
      WHEN 'CK_PAYMENT'.
        <fs_cat_0308>-checkbox  = abap_true.
        <fs_cat_0308>-edit      = abap_true.
        <fs_cat_0308>-outputlen = 03.
        CLEAR: <fs_cat_0308>-coltext,
               <fs_cat_0308>-scrtext_l,
               <fs_cat_0308>-scrtext_m,
               <fs_cat_0308>-scrtext_s.
      WHEN 'WAERS_B'.
        <fs_cat_0308>-outputlen = 08.
      WHEN 'VALOR_PAYMENTS_B'.
        <fs_cat_0308>-do_sum    = abap_true.
        <fs_cat_0308>-outputlen = 15.
        <fs_cat_0308>-edit      = abap_true.
      WHEN 'UKURS_VARIACAO_B'.
        <fs_cat_0308>-outputlen = 10.
        <fs_cat_0308>-edit      = abap_false.
      WHEN 'VALOR_RESIDUAL'.
        <fs_cat_0308>-outputlen = 15.
        <fs_cat_0308>-edit      = abap_true.
      WHEN 'VALOR_PAYMENTS'   OR 'VALOR_PAYMENTS_A' OR 'VALOR_VARIACAO_B' OR 'DMBTR' OR 'DMBE3' OR 'DMBE2' OR 'WRBTR' OR 'VALOR_VARIACAO_A' OR
           'VALOR_VARIACAO_C' OR 'VALOR_VARIACAO_D' OR 'VALOR_PAYMENTS_C' OR 'VALOR_PAYMENTS_D'.
        <fs_cat_0308>-do_sum    = abap_true.
        <fs_cat_0308>-outputlen = 15.
        <fs_cat_0308>-edit      = abap_false.
      WHEN 'ZUONR'.
        <fs_cat_0308>-outputlen = 10.
      WHEN 'BUKRS' OR 'SHKZG' OR 'PARID_KOART' OR 'VALOR_RESIDUAL' OR 'DATA_RESIDUAL' OR 'SAKNR'.
        <fs_cat_0308>-no_out    = abap_true.
      WHEN 'GJAHR'.
        <fs_cat_0308>-no_out    = abap_true.
      WHEN 'BELNR' OR 'STBLG'.
        <fs_cat_0308>-hotspot   = abap_true.
      WHEN 'DMBTRH' OR 'DMBTRS'.
        <fs_cat_0308>-outputlen = 15.
      WHEN 'SGTXT' OR 'NAME1'.
        <fs_cat_0308>-outputlen = 20.
      WHEN 'AUGTX'.
        <fs_cat_0308>-outputlen = 20.
        <fs_cat_0308>-edit      = abap_true.
      WHEN 'SAKNR_VARIACAO_B'.
        <fs_cat_0308>-edit      = abap_false.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0308

*&---------------------------------------------------------------------*
*&      Form  GERAR_COMPENSACAO_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_compensacao_fornecedor TABLES p_it_zglt036_comp STRUCTURE zglt036.

  DATA: is_col_info  TYPE lvc_s_col,
        is_row_no    TYPE lvc_s_roid,
        ck_erro      TYPE c,
        e_blntab     TYPE  blntab,
        msg_text     TYPE  string,
        it_compe     TYPE TABLE OF zde_doc_valor WITH HEADER LINE,
        it_retorno   TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        "IT_RETORNO   TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
        it_bkpf_ret  TYPE TABLE OF zde_doc_valor WITH HEADER LINE,
        it_bkpf_ret2 TYPE TABLE OF bapiret2 WITH HEADER LINE,
        v_cli(1),
        v_for(1).

  CLEAR: ck_erro.


  LOOP AT it_bsik_alv WHERE ck_payment EQ abap_true.

    IF ( it_bsik_alv-valor_residual NE 0 ) AND ( it_bsik_alv-data_residual IS INITIAL ).
      MESSAGE s034 DISPLAY LIKE 'E'.

      is_col_info-fieldname = 'DATA_RESIDUAL'.
      is_row_no-row_id      = sy-tabix.

      CALL METHOD ctl_alv_0302->set_scroll_info_via_id
        EXPORTING
          is_col_info = is_col_info
          is_row_no   = is_row_no.

      ck_erro = abap_true.
      EXIT.
    ENDIF.

    IF it_bsik_alv-koart = 'D'.
      v_cli = 'S'.
    ENDIF.

    IF it_bsik_alv-koart = 'K'.
      v_for = 'S'.
    ENDIF.

    it_compe-bukrs     = it_bsik_alv-bukrs.
    it_compe-belnr     = it_bsik_alv-belnr.
    it_compe-buzei     = it_bsik_alv-buzei.
    it_compe-gjahr     = it_bsik_alv-gjahr.
    it_compe-waers     = it_bsik_alv-waers.
    it_compe-dmbtr     = abs( it_bsik_alv-valor_payments ).
    it_compe-konto     = zde_saldo_cta_banco-saknr.
    it_compe-budat     = zde_saldo_cta_banco-budat.
    it_compe-bldat     = zde_saldo_cta_banco-budat.
    it_compe-koart     = it_bsik_alv-koart.
    it_compe-umskz     = it_bsik_alv-umskz.
    it_compe-dmbtr_res = it_bsik_alv-valor_residual.
    it_compe-zfbdt_res = it_bsik_alv-data_residual.
    it_compe-parid     = it_bsik_alv-parid.
    it_compe-shkzg     = it_bsik_alv-shkzg.
    it_compe-sgtxt     = it_bsik_alv-sgtxt.
    it_compe-augtx     = it_bsik_alv-augtx.
    APPEND it_compe.
  ENDLOOP.

  LOOP AT it_bsik_alvo WHERE ck_payment EQ abap_true.

    IF ( it_bsik_alvo-valor_residual NE 0 ) AND ( it_bsik_alvo-data_residual IS INITIAL ).
      MESSAGE s034 DISPLAY LIKE 'E'.

      is_col_info-fieldname = 'DATA_RESIDUAL'.
      is_row_no-row_id      = sy-tabix.

      CALL METHOD ctl_alv_0308->set_scroll_info_via_id
        EXPORTING
          is_col_info = is_col_info
          is_row_no   = is_row_no.

      ck_erro = abap_true.
      EXIT.
    ENDIF.

    IF it_bsik_alvo-koart = 'D'.
      v_cli = 'S'.
    ENDIF.

    IF it_bsik_alvo-koart = 'K'.
      v_for = 'S'.
    ENDIF.


    it_compe-bukrs            = it_bsik_alvo-bukrs.
    it_compe-belnr            = it_bsik_alvo-belnr.
    it_compe-buzei            = it_bsik_alvo-buzei.
    it_compe-gjahr            = it_bsik_alvo-gjahr.
    it_compe-waers            = it_bsik_alvo-waers.
    it_compe-dmbtr            = abs( it_bsik_alvo-valor_payments ).
    it_compe-konto            = zde_saldo_cta_banco-saknr.
    it_compe-budat            = zde_saldo_cta_banco-budat.
    it_compe-bldat            = zde_saldo_cta_banco-budat.
    it_compe-koart            = it_bsik_alvo-koart.
    it_compe-umskz            = it_bsik_alvo-umskz.
    it_compe-dmbtr_res        = it_bsik_alvo-valor_residual.
    it_compe-zfbdt_res        = it_bsik_alvo-data_residual.
    it_compe-parid            = it_bsik_alvo-parid.
    it_compe-shkzg            = it_bsik_alvo-shkzg.
    it_compe-sgtxt            = it_bsik_alvo-sgtxt.
    it_compe-augtx            = it_bsik_alvo-augtx.
    it_compe-waers_b          = zde_saldo_cta_banco-waers.
    it_compe-waers_c          = it_bsik_alvo-waers_c.
    it_compe-waers_d          = it_bsik_alvo-waers_d.
    IF it_bsik_alvo-valor_payments GT 0.
      it_compe-valor_payments_b = it_bsik_alvo-valor_payments_b.
    ENDIF.
    it_compe-valor_payments_a = it_bsik_alvo-valor_payments_a_atual.
    it_compe-valor_payments_c = it_bsik_alvo-valor_payments_a.
    it_compe-valor_payments_d = it_bsik_alvo-wrbtr.
    it_compe-valor_variacao_a = it_bsik_alvo-valor_variacao_a.
    it_compe-valor_variacao_b = it_bsik_alvo-valor_variacao_b.
    it_compe-valor_variacao_c = it_bsik_alvo-valor_variacao_c.
    it_compe-valor_variacao_d = it_bsik_alvo-valor_variacao_d.
    it_compe-saknr_variacao_b = it_bsik_alvo-saknr_variacao_b.
    APPEND it_compe.
  ENDLOOP.

  IF ck_erro = abap_true.
    MESSAGE s034 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT it_bsik_alvo INTO DATA(wa_bsik_alvo) WHERE valor_residual NE 0.
    DATA(taxa_baixa) = wa_bsik_alvo-taxa_cambio_a_b_dt_doc.
  ENDLOOP.


  IF it_compe[] IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_COMPENSAR'
      EXPORTING
        i_kursf      = taxa_baixa
*       I_SGTXT      = ZDE_MOV_FORNECEDOR-SGTXT
      IMPORTING
        e_blntab     = e_blntab
        msg_text     = msg_text
      TABLES
        it_compensar = it_compe
        it_zglt036   = p_it_zglt036_comp.
    IF msg_text IS NOT INITIAL.
      MESSAGE msg_text TYPE 'I'.
      EXIT.
    ENDIF.
    PERFORM gravar_dados USING abap_true.

* Inicio - falheiros - 22.12.2022 - CS2022000110
*    PERFORM atualizar_partidas_aberto USING dt_inicial dt_final kna1-kunnr lfa1-lifnr .
    PERFORM atualizar_partidas_aberto
    USING dt_inicial dt_final kna1-kunnr lfa1-lifnr dt_inicial2 dt_final2 .

* Fim - falheiros - 22.12.2022 - CS2022000110
  ELSE.
    PERFORM gravar_dados USING abap_false.
  ENDIF.

ENDFORM.                    " GERAR_COMPENSACAO_FORNECEDOR

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0303_exit INPUT.
  CLEAR: zde_mov_fornecedor.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0303_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0303 OUTPUT.
  SET PF-STATUS 'PF0303'.
  SET TITLEBAR 'TL0303'.

ENDMODULE.                 " STATUS_0303  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0303 INPUT.
  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0303  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0307  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0307 INPUT.

  CASE ok_code.
    WHEN 'TBMESMA'.
      lc_dynnr_0307 = lc_dunnr_0302.
      pagtab-activetab = ok_code.
    WHEN 'TBOUTRAS'.
      lc_dynnr_0307 = lc_dunnr_0308.
      pagtab-activetab = ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0308  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0308 OUTPUT.

  DATA: fs_sort_0308 TYPE lvc_s_sort,
        gt_sort_0308 TYPE lvc_t_sort.

  IF ctl_con_0308 IS INITIAL.

    CREATE OBJECT ctl_con_0308
      EXPORTING
        container_name = 'ALV_PART_OTHERS'.

    CREATE OBJECT ctl_alv_0308
      EXPORTING
        i_parent = ctl_con_0308.

    CREATE OBJECT obg_toolbar_0308
      EXPORTING
        io_alv_grid = ctl_alv_0308.

    SET HANDLER obg_toolbar_0308->on_toolbar FOR ctl_alv_0308.
    SET HANDLER obg_toolbar_0308->handle_user_command FOR ctl_alv_0308.

    PERFORM fill_it_fieldcatalog_0308.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0308.
*   Set layout parameters for ALV grid

    gs_lay_0308-zebra      = 'X'.
    gs_lay_0308-edit_mode  = 'X'.
*    GS_LAY_0308-NO_TOTLINE = 'X'.
    gs_lay_0308-col_opt    = 'X'.
    gs_lay_0308-stylefname = 'STYLE'.

    APPEND '&LOCAL&CUT'           TO it_exclude_0308.
    APPEND '&LOCAL&INSERT_ROW'    TO it_exclude_0308.
    APPEND '&LOCAL&MOVE_ROW'      TO it_exclude_0308.
    APPEND '&LOCAL&PASTE'         TO it_exclude_0308.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO it_exclude_0308.
    APPEND '&LOCAL&UNDO'          TO it_exclude_0308.
    APPEND '&VARI_ADMIN'          TO it_exclude_0308.
    APPEND '&LOCAL&APPEND'        TO it_exclude_0308.
    APPEND '&LOCAL&COPY'          TO it_exclude_0308.
    APPEND '&LOCAL&COPY_ROW'      TO it_exclude_0308.
    APPEND '&VLOTUS'              TO it_exclude_0308.
    APPEND '&AQW'                 TO it_exclude_0308.
    APPEND '&PRINT'               TO it_exclude_0308.
*    APPEND '&MB_SUM'              TO IT_EXCLUDE_0308.
*    APPEND '&AVERAGE'             TO IT_EXCLUDE_0308.
    APPEND '&MB_VIEW'             TO it_exclude_0308.
    APPEND '&MB_EXPORT'           TO it_exclude_0308.
    "APPEND '&MB_FILTER'           TO it_exclude_0308.
    APPEND '&GRAPH'               TO it_exclude_0308.
    APPEND '&INFO'                TO it_exclude_0308.
    APPEND '&LOCAL&DELETE_ROW'    TO it_exclude_0308.
    APPEND '&CHECK'               TO it_exclude_0308.

    fs_sort_0308-spos       = 1.     "first sorting key
    fs_sort_0308-fieldname  = 'PARID_KOART'. "fieldname for sort
    fs_sort_0308-up         = 'X'. "sort ascending
    fs_sort_0308-subtot     = 'X'. "do subtotal
    fs_sort_0308-no_out     = 'X'. "no display
    fs_sort_0308-obligatory = 'X'. "sort is obligatory
    INSERT fs_sort_0308 INTO TABLE gt_sort_0308. "insert to sort table

    CLEAR: fs_sort_0308.
    fs_sort_0308-spos       = 2.     "first sorting key
    fs_sort_0308-fieldname  = 'ZFBDT'. "fieldname for sort
    fs_sort_0308-up         = 'X'. "sort ascending
    INSERT fs_sort_0308 INTO TABLE gt_sort_0308. "insert to sort table

    CALL METHOD ctl_alv_0308->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0308
        is_variant           = gs_var_0308
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0308
      CHANGING
        it_fieldcatalog      = it_catalog_0308
        it_outtab            = it_bsik_alvo[]
        it_sort              = gt_sort_0308[].

    CALL METHOD ctl_alv_0308->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_0308->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_0308.
    SET HANDLER event_handler_0308->data_changed_finished FOR ctl_alv_0308.
    SET HANDLER event_handler_0308->data_changed          FOR ctl_alv_0308.
    SET HANDLER event_handler_0308->handle_hotspot_click  FOR ctl_alv_0308.
    SET HANDLER event_handler_0308->subtotal_text         FOR ctl_alv_0308.

    CALL METHOD ctl_alv_0308->refresh_table_display.

  ELSE.
    wa_stable-row = abap_true.
    wa_stable-col = abap_true.
    CALL METHOD ctl_alv_0308->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL METHOD ctl_alv_0308->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0308
      es_row_no   = gs_scroll_row_0308.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0307  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0307 OUTPUT.

  IF lc_dynnr_0307 IS INITIAL.
    lc_dynnr_0307 = '0302'.
*    LC_DYNNR_0307 = '0308'.
  ENDIF.

ENDMODULE.
