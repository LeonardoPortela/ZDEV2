*&---------------------------------------------------------------------*
*& Report  ZPPR013
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr013.

TABLES: mchb, zppt0016, mara, sscrfields.

*** Inicio - Rubenilson - 25.09.24 - #150189
TYPES:
  BEGIN OF ty_saida.
    INCLUDE STRUCTURE zpp_modelo_plan.
TYPES: status TYPE zppe_status,
    msg    TYPE zde_msg.
TYPES END OF ty_saida.

DATA: gt_saida TYPE TABLE OF ty_saida.


*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events  IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
ENDCLASS.
*** Fim - Rubenilson - 25.09.24 - #150189

TYPES: BEGIN OF ty_mchb,
         werks    TYPE mchb-werks,
         name     TYPE name1,
         lgort    TYPE mchb-lgort,
         prod(40) TYPE c,
         form(11) TYPE c,
       END OF ty_mchb.

TYPES: BEGIN OF ty_c_de,
         matnr        TYPE matnr,
         maktx        TYPE maktx,
         charg        TYPE charg_d,
         clabs        TYPE labst,
         unit         TYPE p DECIMALS 6,
         total        TYPE ck_pvprs_1,
         doc_z12      TYPE  zppe_doc_z12,
         doc_mr22_usd TYPE  zppe_doc_mr22,
         cellstyles   TYPE lvc_t_styl,
       END OF ty_c_de.

TYPES: BEGIN OF ty_c_para,
         pmatnr       TYPE matnr,
         pmaktx       TYPE maktx,
         pcharg       TYPE charg_d,
         pclabs       TYPE labst,
         punit        TYPE p DECIMALS 6,
         ptotal       TYPE ck_pvprs_1,
         doc_z11      TYPE  zppe_doc_z12,
         doc_mr22_brl	TYPE zppe_doc_mr22_brl,
         cellstyles   TYPE lvc_t_styl,
       END OF ty_c_para.


TYPES: BEGIN OF ty_estorno,
         matnr TYPE matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE charg_d,
         clabs TYPE labst,
         total TYPE ck_pvprs_1,
       END OF  ty_estorno.


DATA: it_c_de    TYPE TABLE OF ty_c_de,
      it_c_para  TYPE TABLE OF ty_c_para,
      wa_c_de    TYPE ty_c_de,
      wa_c_para  TYPE ty_c_para,
      wa_tela    TYPE ty_mchb,
      wa_mard    TYPE mard,
      it_mchb    TYPE TABLE OF mchb,
      wa_mchb    TYPE mchb,
      it_estorno TYPE TABLE OF ty_estorno,
      wa_estorno TYPE ty_estorno.

DATA:
  tg_de   TYPE TABLE OF ty_c_de,
  it_mat  TYPE TABLE OF ty_c_de,
  tg_soma TYPE TABLE OF ty_c_de,
  tg_para TYPE TABLE OF ty_c_para,
  tg_aux  TYPE TABLE OF ty_c_para.

DATA: ls_fcat     TYPE lvc_s_fcat,
      pt_fieldcat TYPE lvc_t_fcat,
      wa_fcat     TYPE lvc_s_fcat,
      it_fieldcat TYPE lvc_t_fcat,
      lt_celltab  TYPE lvc_t_styl,
      wa_celltab  TYPE lvc_s_styl.

DATA: cc_de   TYPE REF TO cl_gui_custom_container,
      gr_de   TYPE REF TO cl_gui_alv_grid,
      cc_para TYPE REF TO cl_gui_custom_container,
      gr_para TYPE REF TO cl_gui_alv_grid.

DATA: ty_toolbar           TYPE stb_button,
      tg_selectedrow       TYPE lvc_t_row,
      wg_selectedrow       TYPE lvc_s_row,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      wa_stable            TYPE lvc_s_stbl VALUE 'XX',
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_layout            TYPE lvc_s_layo,
*** Inicio - Rubenilson - 25.09.24 - #150189
      gr_alv               TYPE REF TO cl_salv_table,
      gr_events            TYPE REF TO lcl_handle_events.
*** Fim - Rubenilson - 25.09.24 - #150189

DATA: vedit      TYPE lvc_edit,
      ved_lot    TYPE c,
      soma_clabs TYPE labst,
      soma_total TYPE labst,
      txt(200)   TYPE c,
      check      TYPE c.

DATA: sl_header        TYPE bapi2017_gm_head_01,
      vl_code          TYPE bapi2017_gm_code,
      vl_doc           TYPE bapi2017_gm_head_ret-mat_doc,
      vl_year          TYPE bapi2017_gm_head_ret-doc_year,
      tl_item          TYPE TABLE OF bapi2017_gm_item_create,
      tl_return        TYPE TABLE OF bapiret2,
      sl_return        TYPE bapiret2,
      sl_item          TYPE bapi2017_gm_item_create,
      it_return        TYPE TABLE OF bapiret2,
      wa_return        TYPE bapiret2,
      es_bflushflags   LIKE bapi_rm_flg,
      es_bflushdatagen LIKE bapi_rm_datgen,
      es_confirmation  LIKE bapi_rm_datkey-confirmation,
      sperr_user       TYPE sy-msgv1,
      fg_bloqueio(1),
      vlines           TYPE sy-tabix,
      vg_interface(2),
      vg_obj_key       TYPE zmmt_ee_zgr-obj_key,
      it_outreturn     TYPE TABLE OF zfie_ret_document,
      wa_outreturn     TYPE zfie_ret_document,
      it_zppt0006      TYPE TABLE OF zppt0006,
      wa_zppt0006      TYPE zppt0006,
      wa_head_ret      TYPE bapi2017_gm_head_ret,
      vl_doc_de        TYPE bapi2017_gm_head_ret-mat_doc,
      vl_doc_para      TYPE bapi2017_gm_head_ret-mat_doc,
      vl_doc_est       TYPE bapi2017_gm_head_ret-mat_doc,
      vl_doc_year      TYPE bapi2017_gm_head_02-doc_year,
      document_storno  TYPE bapi2017_gm_head_ret,
      tl_return_cancel TYPE TABLE OF bapiret2,
      sl_return_cancel TYPE bapiret2,
      l_sel_button     TYPE smp_dyntxt,
      t_file           TYPE TABLE OF zpp_modelo_plan. " ***Rubenilson - 25.09.24 - #150189


DATA: msg(200)   TYPE c,
      msg01(200) TYPE c,
      verro      TYPE c,
      con_lote   TYPE c.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_werks FOR mchb-werks NO-EXTENSION NO INTERVALS ,
                  p_lgort FOR mchb-lgort NO-EXTENSION NO INTERVALS .
  PARAMETERS: p_form TYPE bktxt. "US #156567 - MMSILVA - 02.05.2025 - Alterado o tipo para 'BKTXT'
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: psoma  RADIOBUTTON GROUP gp,
              pmaior RADIOBUTTON GROUP gp,
              pmenor RADIOBUTTON GROUP gp.
SELECTION-SCREEN END OF BLOCK b2.

*** Inicio - Rubenilson - 25.09.24 - #150189
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_manu TYPE c RADIOBUTTON GROUP g1 USER-COMMAND mass,
              p_mass TYPE c RADIOBUTTON GROUP g1,
              p_troc TYPE c AS CHECKBOX. " Rubenilson - 05.02.24 - US156567

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.

  PARAMETERS: p_file TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM gera_modelo_planillha.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_select_file USING p_file.

AT SELECTION-SCREEN OUTPUT.

  IF p_mass IS NOT INITIAL.
    LOOP AT SCREEN.
*      if screen-name ns 'PSOMA'   and " US 156567 // MMSILVA - 30.10.2024
*         screen-name ns 'PMAIOR'  and " US 156567 // MMSILVA - 30.10.2024
*         screen-name ns 'B002'    and " US 156567 // MMSILVA - 30.10.2024
      IF screen-name NS 'P_MANU'  AND
        screen-name NS 'P_MASS'  AND
        screen-name NS 'B003'    AND
        screen-name NS 'P_FILE'  AND
        screen-name NS 'B004' AND
        screen-name NS 'P_TROC' AND " Rubenilson - 05.02.24 - US156567
        screen-name NS 'B001' AND "US #156567 - MMSILVA - 02.05.2025
        screen-name NS 'P_FORM'.  "US #156567 - MMSILVA - 02.05.2025
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

*     US #156567 - MMSILVA - 02.05.2025 - Inicio
      IF screen-name EQ 'P_TROC'.
        p_troc = abap_true.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
*     US #156567 - MMSILVA - 02.05.2025 - Fim
    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.
      IF screen-name CS 'P_FILE' OR
         screen-name CS 'B004' OR
         screen-name CS 'P_TROC'. " Rubenilson - 05.02.24 - US156567
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
*** Fim - Rubenilson - 25.09.24 - #150189

INITIALIZATION.

  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Download Planilha Modelo Carga'.
  sscrfields-functxt_01  = l_sel_button.

START-OF-SELECTION.

*** Inicio - Rubenilson - 25.09.24 - #150189
  IF p_manu  IS NOT INITIAL AND (
     p_werks IS INITIAL OR
     p_lgort IS INITIAL OR
     p_form  IS INITIAL ).
    MESSAGE 'Os campos centro, depósito e Nº SRE são obrigatórios' TYPE 'S' DISPLAY LIKE 'E'. "US #156567 - MMSILVA - 02.05.2025 - Alterado de 'Nº Formulação' para 'Nº SRE'
    RETURN.
  ENDIF.

  IF p_mass IS NOT INITIAL.
*   US #156567 - MMSILVA - 02.05.2025 - Inicio
    IF p_form IS INITIAL.
      MESSAGE 'O campo Nº SRE é obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
*   US #156567 - MMSILVA - 02.05.2025 - Inicio

    PERFORM f_lancamento_massivo.

  ELSE.
*** Fim - Rubenilson - 25.09.24 - #150189

    wa_tela-werks = p_werks-low.

    SELECT SINGLE name1
      FROM t001w
      INTO wa_tela-name
      WHERE werks EQ wa_tela-werks.

    wa_tela-lgort = p_lgort-low.
    wa_tela-form  = p_form.

    IF psoma = 'X'.
      wa_tela-prod = 'Produção = Soma de matérias primas'.
    ELSEIF pmaior = 'X'.
      wa_tela-prod = 'Produção > soma de matérias primas'.
    ELSEIF pmenor = 'X'.
      wa_tela-prod = 'Produção < soma de matérias primas'.
    ENDIF.

    ved_lot = 'X'.
    IF psoma = 'X'.
      vedit = abap_false.
    ELSEIF pmaior = 'X'.
      vedit = abap_true.
    ELSEIF pmenor = 'X'.
      vedit = abap_true.
    ENDIF.

    APPEND INITIAL LINE TO tg_para.
    CALL SCREEN 0100.

  ENDIF.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed_finished.


    CALL METHOD gr_de->get_selected_rows
      IMPORTING
        et_index_rows = tg_selectedrow.

    IF cl_gui_alv_grid=>mc_style_no_delete_row IS NOT INITIAL.
      IF tg_para[] IS INITIAL .
        APPEND INITIAL LINE TO tg_para.
      ENDIF.
    ENDIF.

    CLEAR: wa_celltab, lt_celltab[].

    LOOP AT et_good_cells INTO DATA(wl_good_cells).
      CASE wl_good_cells-fieldname.
        WHEN 'MATNR'.
          LOOP AT tg_de ASSIGNING FIELD-SYMBOL(<de>).
            CHECK wl_good_cells-row_id EQ sy-tabix.

            <de>-matnr = wl_good_cells-value.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <de>-matnr
              IMPORTING
                output       = <de>-matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            READ TABLE tg_para INTO DATA(wa_para) WITH KEY pmatnr = <de>-matnr.
            IF sy-subrc = 0.
              MESSAGE 'Material ja informado!' TYPE 'I'.
              CLEAR <de>-matnr.
              EXIT.
            ENDIF.

            SELECT SINGLE * FROM mard INTO @DATA(wa_mard)
              WHERE matnr EQ @<de>-matnr
              AND   werks EQ @wa_tela-werks
              AND   lgort EQ @wa_tela-lgort.

            IF sy-subrc NE 0.
              CLEAR txt.
              CONCATENATE  'Material não esta expandido para o depósito ' wa_tela-lgort INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'I'.
              CLEAR: <de>-matnr, <de>-maktx.
              EXIT.
            ELSEIF  wa_mard-sperr IS NOT INITIAL.
              CLEAR txt.
              txt  = 'Movimentação do material bloqueado. Contagem inventário'.
              MESSAGE txt TYPE 'I'.
              CLEAR: <de>-matnr, <de>-maktx.
              EXIT.
            ELSE.
              SELECT SINGLE * FROM mara INTO @DATA(wa_mara)
                WHERE matnr EQ @<de>-matnr.

              SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
               WHERE matnr EQ @<de>-matnr.

              IF sy-subrc IS NOT INITIAL.
                MESSAGE 'Produto informado não existe!' TYPE 'I'.
                EXIT.
              ELSE.
                <de>-maktx = wa_makt-maktx.
              ENDIF.

              IF it_mat IS NOT INITIAL.
                READ TABLE  it_mat ASSIGNING FIELD-SYMBOL(<wa_mat>) WITH KEY matnr = <de>-matnr.
                IF <wa_mat> IS ASSIGNED.
                  IF <wa_mat>-matnr EQ <de>-matnr.
                    IF wa_mara-xchpf IS INITIAL OR wa_mara-xchpf EQ <de>-charg.
                      CLEAR txt.
                      txt = 'Material já informado!'.
                      MESSAGE txt TYPE 'I'.
                      CLEAR: <de>.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF wa_mara-xchpf IS INITIAL.
                wa_celltab-fieldname = 'CHARG'.
                wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                APPEND wa_celltab TO lt_celltab[].
              ENDIF.
              wa_celltab-fieldname = 'MATNR'.
              wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wa_celltab TO lt_celltab[].
              INSERT LINES OF lt_celltab INTO TABLE <de>-cellstyles.
            ENDIF.
            MOVE tg_de TO it_mat.
          ENDLOOP.

        WHEN 'CHARG'.
          LOOP AT tg_de ASSIGNING <de>.
            CHECK wl_good_cells-row_id EQ sy-tabix.
            <de>-charg = wl_good_cells-value.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <de>-matnr
              IMPORTING
                output       = <de>-matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            SELECT SINGLE * FROM mchb INTO wa_mchb
              WHERE matnr EQ <de>-matnr
              AND   charg EQ <de>-charg
              AND   werks EQ wa_tela-werks
              AND   lgort EQ wa_tela-lgort.

            IF sy-subrc NE 0.
              MESSAGE 'Lote Informado não existe !' TYPE 'I'.
              CLEAR <de>-charg.
              EXIT.
            ENDIF.

            IF it_mat IS NOT INITIAL.
              READ TABLE  it_mat ASSIGNING <wa_mat> WITH KEY matnr = <de>-matnr.
              IF <wa_mat> IS ASSIGNED.

                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                  EXPORTING
                    input        = <de>-matnr
                  IMPORTING
                    output       = <de>-matnr
                  EXCEPTIONS
                    length_error = 1
                    OTHERS       = 2.

                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                  EXPORTING
                    input        = <wa_mat>-matnr
                  IMPORTING
                    output       = <wa_mat>-matnr
                  EXCEPTIONS
                    length_error = 1
                    OTHERS       = 2.

                IF <wa_mat>-matnr EQ <de>-matnr.
                  IF  <wa_mat>-charg EQ <de>-charg.
                    CLEAR txt.
                    txt = 'Material e lote já informado!'.
                    MESSAGE txt TYPE 'I'.
                    CLEAR: <de>.
                    EXIT.
                  ELSE.
                    CLEAR it_mat[].
                    MOVE tg_de TO it_mat.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            IF wa_mchb-clabs IS INITIAL.
              CLEAR txt.
              CONCATENATE 'Lote '  <de>-charg ' Sem saldo!' INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'I'.
              <de>-clabs = '0.000'.
              EXIT.
            ELSEIF <de>-clabs > wa_mchb-clabs.
              CLEAR txt.
              CONCATENATE 'Saldo insuficiente para o processo! '  'Material: ' <de>-matnr 'Lote:'  <de>-charg INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'I'.
              <de>-clabs = '0.000'.
              EXIT.
            ELSE.
              wa_celltab-fieldname = 'CHARG'.
              wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wa_celltab TO lt_celltab[].
              INSERT LINES OF lt_celltab INTO TABLE <de>-cellstyles.
            ENDIF.
          ENDLOOP.

        WHEN 'CLABS'.

          soma_clabs = 0.
          soma_total = 0.

          DATA _lbkum TYPE mbew-lbkum.
          DATA _salk3 TYPE mbew-salk3.

          LOOP AT tg_de ASSIGNING <de>.
            CHECK wl_good_cells-row_id EQ sy-tabix.
            <de>-clabs = wl_good_cells-value.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <de>-matnr
              IMPORTING
                output       = <de>-matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            "            IF <DE>-CHARG IS INITIAL.
            SELECT SINGLE * FROM mard INTO wa_mard
              WHERE matnr EQ <de>-matnr
              AND   werks EQ wa_tela-werks
              AND   lgort EQ wa_tela-lgort.

            IF sy-subrc = 0.

              IF wa_mard-labst IS INITIAL.
                CLEAR txt.
                CONCATENATE  'Material '  <de>-matnr ' sem saldo !' INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'I'.
                <de>-clabs = '0.000'.
                EXIT.
              ENDIF.

              IF  <de>-clabs > wa_mard-labst .
                CLEAR txt.
                CONCATENATE 'Saldo insuficiente para o processo!  Material: ' <de>-matnr INTO txt SEPARATED BY space.
                MESSAGE txt  TYPE 'I'.
                <de>-clabs = '0.000'.
                EXIT.
              ENDIF.
            ENDIF.

            SELECT SINGLE lbkum  salk3
              FROM mbew
              INTO ( _lbkum, _salk3 )
              WHERE matnr EQ <de>-matnr
              AND   bwkey EQ wa_tela-werks.

            <de>-unit = _salk3 / _lbkum.

            <de>-total = <de>-unit * <de>-clabs.
            MODIFY tg_de FROM <de> INDEX wl_good_cells-row_id.
            MOVE tg_de TO tg_soma.

            LOOP AT tg_soma ASSIGNING FIELD-SYMBOL(<soma_de>).
              soma_total =  soma_total +  <soma_de>-total.
            ENDLOOP.

            IF psoma = 'X'.
              LOOP AT tg_soma ASSIGNING <soma_de>.
                ADD <soma_de>-clabs TO soma_clabs.

                LOOP AT tg_para ASSIGNING FIELD-SYMBOL(<para>).
                  <para>-pclabs = soma_clabs.
                ENDLOOP.
              ENDLOOP.

              <para>-punit  = soma_total / <para>-pclabs.
              <para>-ptotal = soma_total.
              MODIFY tg_para FROM <para> INDEX wl_good_cells-row_id.
            ENDIF.

            READ TABLE <de>-cellstyles INTO DATA(cells) WITH KEY fieldname = 'UNIT'.
            IF sy-subrc NE 0.
              wa_celltab-fieldname = 'UNIT'.
              wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wa_celltab TO lt_celltab[].
              INSERT LINES OF lt_celltab INTO TABLE <de>-cellstyles.
              CLEAR: wa_celltab, lt_celltab[].
            ENDIF.
            "ENDIF.
          ENDLOOP.

        WHEN 'PMATNR'.
          LOOP AT tg_para ASSIGNING <para>.
            CHECK wl_good_cells-row_id EQ sy-tabix.
            <para>-pmatnr = wl_good_cells-value.

            IF tg_de[] IS INITIAL.
              CLEAR txt.
              txt = 'Favor informar primeiro os material de origem!'.
              MESSAGE txt TYPE 'I'.
              CLEAR <para>-pmatnr.
              EXIT.
            ENDIF.

            READ TABLE tg_de INTO DATA(wa_tg) WITH KEY matnr = <para>-pmatnr.
            IF sy-subrc = 0.
              MESSAGE 'Material ja informado!' TYPE 'I'.
              CLEAR <para>-pmatnr.
              EXIT.
            ELSE.

              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = <para>-pmatnr
                IMPORTING
                  output       = <para>-pmatnr
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.

              SELECT SINGLE * FROM mard INTO wa_mard
                WHERE matnr EQ <para>-pmatnr
                AND   werks EQ wa_tela-werks
                AND   lgort EQ wa_tela-lgort.

              IF sy-subrc NE 0.
                CLEAR txt.
                CONCATENATE 'Material não esta expandido para o Depósito ' wa_tela-lgort INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'I'.
                CLEAR: <para>-pmatnr, <para>-pmaktx.
                EXIT.
              ELSEIF wa_mard-sperr IS NOT INITIAL.
                CLEAR txt.
                txt  = 'Movimentação do material bloqueado. Contagem inventário'.
                MESSAGE txt TYPE 'I'.
                CLEAR: <para>-pmatnr, <para>-pmaktx.
                EXIT.
              ELSE.
                SELECT SINGLE * FROM mara INTO wa_mara
                  WHERE matnr EQ <para>-pmatnr.

                SELECT SINGLE * FROM makt INTO wa_makt
                 WHERE matnr EQ <para>-pmatnr.

                IF sy-subrc IS NOT INITIAL.
                  MESSAGE 'Produto informado não existe!' TYPE 'I'.
                  EXIT.
                ELSE.
                  <para>-pmaktx = wa_makt-maktx.
                ENDIF.

                IF wa_mara-xchpf IS INITIAL.
                  wa_celltab-fieldname = 'PCHARG'.
                  wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                  APPEND wa_celltab TO lt_celltab[].
                ELSE.
                  wa_celltab-fieldname = 'PCHARG'.
                  wa_celltab-style     = cl_gui_alv_grid=>mc_style_enabled.
                  APPEND wa_celltab TO lt_celltab[].
                ENDIF.

                IF psoma = 'X'.
                  wa_celltab-fieldname = 'PUNIT'.
                  wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                  APPEND wa_celltab TO lt_celltab[].
                ENDIF.
                INSERT LINES OF lt_celltab INTO TABLE <para>-cellstyles.
                CLEAR: wa_celltab, lt_celltab[].
              ENDIF.
            ENDIF.
          ENDLOOP.

        WHEN 'PCHARG'.
          LOOP AT tg_para ASSIGNING <para>.
            CHECK wl_good_cells-row_id EQ sy-tabix.
            <para>-pcharg = wl_good_cells-value.

            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = <para>-pmatnr
              IMPORTING
                output       = <para>-pmatnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.

            SELECT SINGLE * FROM mard INTO wa_mard
              WHERE matnr EQ <para>-pmatnr
              AND   werks EQ wa_tela-werks
              AND   lgort EQ wa_tela-lgort.

            IF sy-subrc NE 0.
              MESSAGE 'Lote Informado não existe.  !' TYPE 'I'.
              CLEAR <para>-pcharg.
              EXIT.
            ENDIF.
          ENDLOOP.

        WHEN 'PCLABS'.
          soma_total = 0.
          soma_clabs = 0.

          LOOP AT tg_para ASSIGNING <para>.
            CHECK wl_good_cells-row_id EQ sy-tabix.
            <para>-pclabs = wl_good_cells-value.

            LOOP AT tg_soma ASSIGNING <soma_de>.
              soma_total =  soma_total + <soma_de>-total.
              soma_clabs =  soma_clabs + <soma_de>-clabs.
            ENDLOOP.

            IF pmaior = 'X'.

              IF <para>-pclabs > soma_clabs.
                <para>-punit  = soma_total / <para>-pclabs.
                <para>-ptotal = soma_total.
                MODIFY tg_para FROM <para> INDEX wl_good_cells-row_id.

                wa_celltab-fieldname = 'PCLABS'.
                wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                APPEND wa_celltab TO lt_celltab[].


                READ TABLE <para>-cellstyles INTO cells WITH KEY fieldname = 'PUNIT'.
                IF sy-subrc NE 0.
                  wa_celltab-fieldname = 'PUNIT'.
                  wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                  APPEND wa_celltab TO lt_celltab[].
                  INSERT LINES OF lt_celltab INTO TABLE <para>-cellstyles.
                  CLEAR: wa_celltab, lt_celltab[].
                ENDIF.

              ELSE.
                CLEAR txt.
                CONCATENATE 'A quantidade informada no PARA, deverá ser maior que a soma da quantidade informada no DE. Material: ' <para>-pmatnr INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'I'.
                <para>-pclabs = '0.000'.  <para>-punit  = '0.00'.  <para>-ptotal = '0.00'.
                EXIT.
              ENDIF.

            ELSEIF pmenor = 'X'.

              IF <para>-pclabs < soma_clabs.
                <para>-punit  = soma_total / <para>-pclabs.
                <para>-ptotal = soma_total.
                MODIFY tg_para FROM <para> INDEX wl_good_cells-row_id.

                wa_celltab-fieldname = 'PCLABS'.
                wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                APPEND wa_celltab TO lt_celltab[].

                READ TABLE <para>-cellstyles INTO cells WITH KEY fieldname = 'PUNIT'.
                IF sy-subrc NE 0.
                  wa_celltab-fieldname = 'PUNIT'.
                  wa_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
                  APPEND wa_celltab TO lt_celltab[].
                  INSERT LINES OF lt_celltab INTO TABLE <para>-cellstyles.
                  CLEAR: wa_celltab, lt_celltab[].
                ENDIF.
              ELSE.
                CLEAR txt.
                CONCATENATE 'A quantidade informada no PARA, deverá ser menor que a soma da quantidade informada no DE. Material: ' <para>-pmatnr INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'I'.
                <para>-pclabs = '0.000'.  <para>-punit  = '0.00'.   <para>-ptotal = '0.00'.
                EXIT.
              ENDIF.

            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    CALL METHOD gr_de->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    CALL METHOD gr_para->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


  ENDMETHOD.
ENDCLASS.


CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD  on_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon = icon_insert_row.
    ty_toolbar-function = 'INSERIR'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon = icon_delete_row.
    ty_toolbar-function = 'EXCLUIR'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.


  METHOD handle_user_command.

    DATA: it_saida TYPE TABLE OF ty_c_de,
          wa_saida TYPE  ty_c_de.

    CASE e_ucomm.
      WHEN 'INSERIR'.
        APPEND INITIAL LINE TO tg_de.
      WHEN 'EXCLUIR'.
        MOVE tg_de TO it_saida.
        LOOP AT tg_selectedrow INTO wg_selectedrow.
          READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
          IF sy-subrc = 0.
            LOOP AT tg_para ASSIGNING FIELD-SYMBOL(<para>).
              <para>-pclabs =   <para>-pclabs - wa_saida-clabs.
            ENDLOOP.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = wa_saida-matnr
            IMPORTING
              output       = wa_saida-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.

          DELETE  tg_de WHERE matnr = wa_saida-matnr
                         AND  charg = wa_saida-charg
                         AND  clabs = wa_saida-clabs
                         AND  unit  = wa_saida-unit
                         AND  total  = wa_saida-total.

          DELETE it_mat  WHERE matnr = wa_saida-matnr
                         AND  charg = wa_saida-charg.




          CLEAR: wa_saida.
        ENDLOOP.
        REFRESH it_saida.
    ENDCASE.

    IF gr_para IS NOT INITIAL.
      CALL METHOD gr_para->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

    IF gr_de IS NOT INITIAL.
      CALL METHOD gr_de->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA ans   TYPE c.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BNTPRO'.
      PERFORM z_validacoes.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = 'Confirmação'
          text_question = 'Deseja realizar o processo?'
          text_button_1 = 'Sim'
          icon_button_1 = 'ICON_CHECKED'
          text_button_2 = 'Não'
          icon_button_2 = 'ICON_CANCEL'
          popup_type    = 'ICON_MESSAGE_QUESTION'
        IMPORTING
          answer        = ans.
      CASE ans.
        WHEN '2' OR 'A'.
          LEAVE SCREEN.
        WHEN 1.
          PERFORM z_processar.
      ENDCASE.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: grid_event_p TYPE REF TO lcl_event_handler,
        obg_toolbar  TYPE REF TO lcl_alv_toolbar.
  DATA(botao_alv) = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  wa_stable = 'X'.
  gs_layout-stylefname = 'CELLSTYLES'.

  "DE
  CREATE OBJECT cc_de
    EXPORTING
      container_name = 'C_DE'.

  CREATE OBJECT gr_de
    EXPORTING
      i_parent = cc_de.

  CREATE OBJECT obg_toolbar
    EXPORTING
      io_alv_grid = gr_de.


  wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wl_function TO tl_function.


  SET HANDLER obg_toolbar->on_toolbar FOR gr_de.
  SET HANDLER obg_toolbar->handle_user_command FOR gr_de.

  PERFORM alv_de.

  CALL METHOD cl_gui_cfw=>flush.

  CALL METHOD gr_de->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = tl_function
      is_layout            = gs_layout
    CHANGING
      it_outtab            = tg_de
      it_fieldcatalog      = pt_fieldcat.

  SET HANDLER:
    lcl_event_handler=>on_data_changed_finished FOR gr_de.

  CALL METHOD gr_de->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gr_de->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD gr_de->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.



  "PARA
  gs_layout-stylefname = 'CELLSTYLES'.
  CREATE OBJECT cc_para
    EXPORTING
      container_name = 'C_PARA'.

  CREATE OBJECT gr_para
    EXPORTING
      i_parent = cc_para.

  SET HANDLER:
    lcl_event_handler=>on_data_changed_finished FOR gr_para.

  PERFORM alv_para.

  CALL METHOD gr_para->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = botao_alv
      i_save               = abap_true
      is_layout            = gs_layout
    CHANGING
      it_outtab            = tg_para
      it_fieldcatalog      = it_fieldcat.


  CALL METHOD gr_para->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gr_para->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
ENDMODULE.


FORM alv_de.
  PERFORM preenche_cat USING:
      'MATNR'     'Material'       '08'    'X'        'X'     'MATNR'   'MARA',
      'MAKTX'     'Desc.Material'  '20'    ''         ''      ''        '',
      'CHARG'     'Lote'           '10'    'X'        ''      ''        '',
      'CLABS'     'Quantidade'     '10'    'X'        ''      'CLABS'   'MCHB',
      'UNIT'      'Vlr. Unitário'  '12'    'X'        ''      ''        '',
      'TOTAL'     'Vlr. Total'     '12'    ''         ''      ''        ''.
ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_edit)
                        VALUE(p_zero)
                        VALUE(p_field)
                        VALUE(p_table).

  ls_fcat-fieldname   =  p_campo.
  ls_fcat-coltext     =  p_desc.
  ls_fcat-outputlen   =  p_tam.
  ls_fcat-edit        =  p_edit.
  ls_fcat-no_zero     =  p_zero.
  ls_fcat-ref_field   =  p_field.
  ls_fcat-ref_table   =  p_table.

  APPEND ls_fcat TO pt_fieldcat.

ENDFORM.


FORM alv_para.
  PERFORM p_preenche_cat USING:
      'PMATNR'     'Material'       '08'    'X'         'X'     'MATNR'   'MARA',
      'PMAKTX'     'Desc.Material'  '20'    ''          ''      ''        '',
      'PCHARG'     'Lote'           '10'    'X'         ''      ''        '',
      'PCLABS'     'Quantidade'     '10'    vedit       ''      'CLABS'   'MCHB',
      'PUNIT'      'Vlr. Unitário'  '12'    vedit       ''      ''        '',
      'PTOTAL'     'Vlr. Total'     '12'    ''          ''      ''        ''.
ENDFORM.

FORM p_preenche_cat USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_edit)
                          VALUE(p_zero)
                          VALUE(p_field)
                          VALUE(p_table).

  wa_fcat-fieldname   =  p_campo.
  wa_fcat-coltext     =  p_desc.
  wa_fcat-outputlen   =  p_tam.
  wa_fcat-edit        =  p_edit.
  wa_fcat-no_zero     =  p_zero.
  wa_fcat-ref_field   =  p_field.
  wa_fcat-ref_table   =  p_table.

  APPEND wa_fcat TO it_fieldcat.
ENDFORM.


FORM z_processar.

  REFRESH : tl_item, tl_return.
  CLEAR: sl_item, vl_doc_de, vl_doc_year, sl_return, sl_header.

  DATA: lv_erro                 TYPE c,
        vga_value_z12_brl       TYPE fins_vkcur12,
        vga_value_z12_brl_total TYPE fins_vkcur12,          "FF #185986

        vga_value_z11_brl       TYPE fins_vkcur12,
        vga_value_z12_usd       TYPE fins_vkcur12,
        vga_value_z12_usd_total TYPE fins_vkcur12,          "FF #185986
        vga_value_z11_usd       TYPE fins_vkcur12,
        vga_value_dif_brl       TYPE ckmpc_zuumb,
        vga_value_dif_usd       TYPE ckmpc_zuumb,
        vga_mes                 TYPE poper,
        vga_date_ano            TYPE jahrper.

  DATA: zvg_matnr         TYPE char18,
        zvg_price_brl     TYPE ck_pvprs_1,
        zvg_price_usd     TYPE ck_pvprs_1,
        zvg_vlr_total_brl TYPE ck_pvprs_1,
        zvg_vlr_total_usd TYPE ck_pvprs_1.

  LOOP AT tg_de ASSIGNING FIELD-SYMBOL(<de>).
    CALL FUNCTION 'ENQUEUE_EMMARCE'
      EXPORTING
        matnr          = <de>-matnr
        werks          = wa_tela-werks
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    sperr_user =  sy-msgv1.

    IF sy-subrc = 1.
      CONCATENATE 'Material ' <de>-matnr 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
      MESSAGE msg TYPE 'E'.
      EXIT.
    ENDIF.

    IF <de>-charg IS NOT INITIAL.
      fg_bloqueio      = 'X'.
      CALL FUNCTION 'ENQUEUE_EMMCH1E'
        EXPORTING
          mode_mch1      = 'E'
          mandt          = sy-mandt
          matnr          = <de>-matnr
          charg          = <de>-charg
          _scope         = '2'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      sperr_user  = sy-msgv1.

      IF sy-subrc = 1.
        CONCATENATE 'Lote ' <de>-charg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
        MESSAGE msg TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

*  "iniciando a bapi
    vl_code = '05'.
    sl_header-pstng_date      =  sy-datum.
    sl_header-doc_date        =  sy-datum.
    sl_header-header_txt      =  p_form.
    sl_header-ref_doc_no      =  p_form. "US #156567 - MMSILVA - 02.05.2025
    sl_item-move_type         = 'Z12'.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
    "    sl_item-material          = <de>-matnr.
    DATA(v_len3) = strlen( <de>-matnr ).
    IF v_len3 > 18.
      sl_item-material_long = <de>-matnr .
    ELSE.
      sl_item-material = <de>-matnr .
    ENDIF.

    zvg_matnr    = |{ sl_item-material ALPHA = OUT }|.
    zvg_matnr    = |{ zvg_matnr ALPHA = IN }|.
    sl_item-material = zvg_matnr.


*         Verifica preço material.
    CALL FUNCTION 'ZPP_CALCULATION_PRICE_MATERIAL'
      EXPORTING
        i_matnr     = sl_item-material
        i_werks     = wa_tela-werks
      IMPORTING
        e_price_usd = zvg_price_usd
        e_price_brl = zvg_price_brl.



*<-- 16.06.2023 - Migration S4 – MIGNOW – End
    sl_item-plant             = wa_tela-werks.
    sl_item-stge_loc          = wa_tela-lgort.
    sl_item-batch             = <de>-charg.  "185986 - RGARCIA
    sl_item-entry_qnt         = <de>-clabs."quantidade
    sl_item-gl_account        = '0000511000'.
    APPEND sl_item TO tl_item.
    CLEAR sl_item.

    wa_estorno-matnr  = <de>-matnr.
    wa_estorno-werks  = wa_tela-werks.
    wa_estorno-lgort  = wa_tela-lgort.
    wa_estorno-clabs  = <de>-clabs.
    wa_estorno-charg  = <de>-charg.
    wa_estorno-total  = <de>-total.
    APPEND wa_estorno TO it_estorno.
    CLEAR wa_estorno.
  ENDLOOP.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = sl_header
      goodsmvt_code    = vl_code
    IMPORTING
      materialdocument = vl_doc
      matdocumentyear  = vl_year
    TABLES
      goodsmvt_item    = tl_item
      return           = tl_return.

  vl_doc_de    = vl_doc.
  vl_doc_year  = vl_year.


  READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


    <de>-doc_z12 = vl_doc_de.
    CLEAR sl_return.
  ELSE.
    MESSAGE sl_return-message TYPE 'E'.
    EXIT.
  ENDIF.


  IF fg_bloqueio    = 'X'.
* ---> S4 Migration - 21/06/2023 - MA
    DATA: lv_material TYPE marc-matnr .
* <--- S4 Migration - 21/06/2023 - MA
    LOOP AT tl_item INTO sl_item.
* ---> S4 Migration - 21/06/2023 - MA
      lv_material = sl_item-material.

*      CALL FUNCTION 'DEQUEUE_EMMARCE'
*        EXPORTING
*          matnr = sl_item-material
*          werks = sl_item-plant.

      CALL FUNCTION 'DEQUEUE_EMMARCE'
        EXPORTING
          matnr = lv_material
          werks = sl_item-plant.


      IF sl_item-batch IS NOT INITIAL.
*        CALL FUNCTION 'DEQUEUE_EMMCH1E'
*          EXPORTING
*            mode_mch1 = 'E'
*            mandt     = sy-mandt
*            matnr     = sl_item-material
*            charg     = sl_item-batch.

        CALL FUNCTION 'DEQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1 = 'E'
            mandt     = sy-mandt
            matnr     = lv_material
            charg     = sl_item-batch.
* <--- S4 Migration - 21/06/2023 - MA
      ENDIF.
      CLEAR sl_item.
    ENDLOOP.
  ENDIF.

  REFRESH : tl_item, tl_return, tl_return_cancel.
  CLEAR: sl_item, vl_doc_para, vl_doc_year, sl_return, sl_return_cancel.

  "para
  LOOP AT tg_para ASSIGNING FIELD-SYMBOL(<para>).

    DATA(lv_tabix) = sy-tabix.

    CALL FUNCTION 'ENQUEUE_EMMARCE'
      EXPORTING
        matnr          = <para>-pmatnr
        werks          = wa_tela-werks
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    sperr_user =  sy-msgv1.

    IF sy-subrc = 1.
      CONCATENATE 'Material ' <para>-pmatnr 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
      MESSAGE msg TYPE 'E'.
      EXIT.
    ENDIF.



    IF <de>-charg IS NOT INITIAL.

      fg_bloqueio      = 'X'.
      CALL FUNCTION 'ENQUEUE_EMMCH1E'
        EXPORTING
          mode_mch1      = 'E'
          mandt          = sy-mandt
          matnr          = <para>-pmatnr
          charg          = <para>-pcharg
          _scope         = '2'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      sperr_user  = sy-msgv1.

      IF sy-subrc = 1.
        CONCATENATE 'Lote ' <para>-pcharg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
        MESSAGE msg TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    vl_code = '05'.
    sl_header-pstng_date       =  sy-datum.
    sl_header-doc_date         =  sy-datum.
    sl_header-header_txt       =  p_form.
    sl_header-ref_doc_no      =  p_form. "US #156567 - MMSILVA - 02.05.2025
    sl_item-move_type          = 'Z11'.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
    "    sl_item-material           = <para>-pmatnr.
    DATA(v_len2) = strlen( <para>-pmatnr ).
    IF v_len2 > 18.
      sl_item-material_long = <para>-pmatnr .
    ELSE.
      sl_item-material = <para>-pmatnr .
    ENDIF.


    CLEAR: zvg_matnr.
    zvg_matnr        = |{ sl_item-material ALPHA = OUT }|.
    zvg_matnr        = |{ zvg_matnr ALPHA = IN }|.
    sl_item-material = zvg_matnr.

    "185986 RGARCIA
    IF zvg_price_brl IS NOT INITIAL.
      zvg_vlr_total_brl = zvg_price_brl * <para>-pclabs.
    ENDIF.

    "185986 RGARCIA
    IF zvg_price_usd IS NOT INITIAL.
      zvg_vlr_total_usd = zvg_price_usd * <para>-pclabs.
    ENDIF.

*<-- 16.06.2023 - Migration S4 – MIGNOW – End
    sl_item-plant              = wa_tela-werks.
    sl_item-stge_loc           = wa_tela-lgort.
    sl_item-batch              = <para>-pcharg.
    sl_item-entry_qnt          = <para>-pclabs."quantidade
*    sl_item-amount_lc          = zvg_vlr_total_brl. "<para>-ptotal. "185986 - RGARCIA "FF #185986 del
    sl_item-amount_lc      = <para>-ptotal. "FF #185986 ins
    sl_item-gl_account         = '0000511000'.
    APPEND sl_item TO tl_item.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = sl_header
        goodsmvt_code    = vl_code
      IMPORTING
        materialdocument = vl_doc
        matdocumentyear  = vl_year
      TABLES
        goodsmvt_item    = tl_item
        return           = tl_return.

    vl_doc_para = vl_doc.
    vl_doc_year = vl_year.

    READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      <para>-doc_z11 = vl_doc_para.

      CLEAR sl_return.

      CLEAR txt.
      CONCATENATE 'Processo gerado com sucesso! Documento gerado: ' vl_doc_para INTO txt SEPARATED BY space.
      MESSAGE txt TYPE 'I'.

    ELSE.

      CLEAR:  sl_header,  vl_code, vl_doc,  vl_year, tl_item.

      LOOP AT it_estorno INTO wa_estorno.

        CALL FUNCTION 'ENQUEUE_EMMARCE'
          EXPORTING
            matnr          = wa_estorno-matnr
            werks          = wa_estorno-werks
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        sperr_user =  sy-msgv1.

        IF sy-subrc = 1.
          CONCATENATE 'Material ' wa_estorno-matnr 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
          MESSAGE msg TYPE 'E'.
          EXIT.
        ENDIF.

        IF <de>-charg IS NOT INITIAL.

          fg_bloqueio      = 'X'.
          CALL FUNCTION 'ENQUEUE_EMMCH1E'
            EXPORTING
              mode_mch1      = 'E'
              mandt          = sy-mandt
              matnr          = wa_estorno-matnr
              charg          = wa_estorno-charg
              _scope         = '2'
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          sperr_user  = sy-msgv1.

          IF sy-subrc = 1.
            CONCATENATE 'Lote ' wa_estorno-charg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
            MESSAGE msg TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

        vl_code = '05'.
        sl_header-pstng_date       = sy-datum.
        sl_header-doc_date         = sy-datum.
        sl_header-header_txt       = p_form.
        sl_header-ref_doc_no      =  p_form. "US #156567 - MMSILVA - 02.05.2025
        sl_item-move_type          = 'Z11'.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
        "        sl_item-material           = wa_estorno-matnr.
        DATA(v_len1) = strlen( wa_estorno-matnr ).
        IF v_len1 > 18.
          sl_item-material_long = wa_estorno-matnr .
        ELSE.
          sl_item-material = wa_estorno-matnr .
        ENDIF.
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
        sl_item-plant              = wa_estorno-werks.
        sl_item-stge_loc           = wa_estorno-lgort.
        sl_item-batch              = wa_estorno-charg.
        sl_item-entry_qnt          = wa_estorno-clabs.
        sl_item-amount_lc          = wa_estorno-total.
        sl_item-gl_account         = '0000511000'.
        APPEND sl_item TO tl_item.
        CLEAR: wa_estorno.
      ENDLOOP.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = sl_header
          goodsmvt_code    = vl_code
        IMPORTING
          materialdocument = vl_doc
          matdocumentyear  = vl_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return_cancel.

      vl_doc_est = vl_doc.

      READ TABLE tl_return_cancel INTO sl_return_cancel WITH KEY type = 'E'.

      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CLEAR sl_return.

        CLEAR txt.
        CONCATENATE 'Ocorreu um erro no processamento. Foi realizado um estorno. Documento:' vl_doc_est INTO txt SEPARATED BY space.
        MESSAGE txt TYPE 'I'.
      ELSE.
        MESSAGE sl_return_cancel-message TYPE 'E'.
        EXIT.
      ENDIF.

    ENDIF.

    IF fg_bloqueio    = 'X'.
* ---> S4 Migration - 21/06/2023 - MA
      DATA: lv_move_mat TYPE marc-matnr.
* <--- S4 Migration - 21/06/2023 - MA

      LOOP AT tl_item INTO sl_item.
* ---> S4 Migration - 21/06/2023 - MA
        lv_material = sl_item-material.
        lv_move_mat = sl_item-move_mat.

*        call function 'DEQUEUE_EMMARCE'
*          exporting
*            MATNR = SL_ITEM-MATERIAL
*            WERKS = SL_ITEM-PLANT.

        CALL FUNCTION 'DEQUEUE_EMMARCE'
          EXPORTING
            matnr = lv_material
            werks = sl_item-plant.


*        call function 'DEQUEUE_EMMARCE'
*          exporting
*            MATNR = SL_ITEM-MOVE_MAT
*            WERKS = SL_ITEM-MOVE_PLANT.

        CALL FUNCTION 'DEQUEUE_EMMARCE'
          EXPORTING
            matnr = lv_move_mat
            werks = sl_item-move_plant.

        IF sl_item-batch IS NOT INITIAL.
*          call function 'DEQUEUE_EMMCH1E'
*            exporting
*              MODE_MCH1 = 'E'
*              MANDT     = SY-MANDT
*              MATNR     = SL_ITEM-MATERIAL
*              CHARG     = SL_ITEM-BATCH.

          CALL FUNCTION 'DEQUEUE_EMMCH1E'
            EXPORTING
              mode_mch1 = 'E'
              mandt     = sy-mandt
              matnr     = lv_material
              charg     = sl_item-batch.
        ENDIF.

        IF sl_item-move_batch IS NOT INITIAL.
*          call function 'DEQUEUE_EMMCH1E'
*            exporting
*              MODE_MCH1 = 'E'
*              MANDT     = SY-MANDT
*              MATNR     = SL_ITEM-MOVE_MAT
*              CHARG     = SL_ITEM-MOVE_BATCH.

          CALL FUNCTION 'DEQUEUE_EMMCH1E'
            EXPORTING
              mode_mch1 = 'E'
              mandt     = sy-mandt
              matnr     = lv_move_mat
              charg     = sl_item-move_batch.
        ENDIF.
* <--- S4 Migration - 21/06/2023 - MA
        CLEAR sl_item.
      ENDLOOP.
    ENDIF.

    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA

    CLEAR: vga_date_ano.
    vga_date_ano = |{ sy-datum+4(2) }{ sy-datum+0(4) }|.
    vga_mes      = sy-datum+4(2).


    "FF #185986  - inicio
    CLEAR: vga_value_z12_usd_total,
           vga_value_z12_brl_total.

    LOOP AT tg_de INTO DATA(wa_de).

      CALL FUNCTION 'ZPP_GET_VALUE_KSL_ACDOCA'
        EXPORTING
          i_rldnr = '0L'
*         i_rbukrs      = ''
*         i_fiscyearper = vga_date_ano
          i_matnr = wa_de-matnr
          i_bwkey = p_werks-low
          i_belnr = <de>-doc_z12
          i_mjahr = vl_year
          i_poper = vga_mes
        IMPORTING
          e_ksl   = vga_value_z12_usd
          e_hsl   = vga_value_z12_brl.

      vga_value_z12_usd_total = vga_value_z12_usd_total + vga_value_z12_usd.
      vga_value_z12_brl_total = vga_value_z12_brl_total + vga_value_z12_brl.

    ENDLOOP.
    "FF #185986 - fim


    "Seleciona valor documento tipo de movimento Z11.
    CALL FUNCTION 'ZPP_GET_VALUE_KSL_ACDOCA'
      EXPORTING
        i_rldnr = '0L'
*       i_rbukrs      = ''
*       i_fiscyearper = vga_date_ano
        i_matnr = <para>-pmatnr
        i_bwkey = p_werks-low
        i_belnr = <para>-doc_z11
        i_mjahr = vl_year
        i_poper = vga_mes
      IMPORTING
        e_ksl   = vga_value_z11_usd
        e_hsl   = vga_value_z11_brl.


    "Ajustar preço USD - Dolar.
    IF vga_value_z12_usd_total NE vga_value_z11_usd.        "FF #185986
      vga_value_dif_usd = ( VGA_VALUE_Z12_USD_TOTAL - vga_value_z11_usd ). "FF #185986

      CALL FUNCTION 'ZPP_CHANGE_MR22_USD'
        EXPORTING
*         i_matnr  = <de>-matnr  "FF #185986
          i_matnr  = <para>-pmatnr  "FF #185986
          i_werks  = p_werks-low
          i_text   = p_form
          i_data   = sy-datum
          i_amount = vga_value_dif_usd
        IMPORTING
          e_belnr  = <de>-doc_mr22_usd.
    ENDIF.

    "Ajustar preço BRL - Reais.
    IF VGA_VALUE_Z12_BRL_TOTAL NE vga_value_z11_brl. "FF #185986
      vga_value_dif_brl = ( VGA_VALUE_Z12_BRL_TOTAL - vga_value_z11_brl ). "FF #185986

      CALL FUNCTION 'ZPP_CHANGE_MR22_BRL'
        EXPORTING
          i_matnr  = <para>-pmatnr
          i_werks  = p_werks-low
          i_text   = p_form
          i_data   = sy-datum
          i_amount = vga_value_dif_brl
        IMPORTING
          e_belnr  = <para>-doc_mr22_brl.
    ENDIF.
    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA
  ENDLOOP.

  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_validacoes.

  soma_total = 0.
  soma_clabs = 0.
  CLEAR: verro, con_lote.

  LOOP AT tg_de ASSIGNING FIELD-SYMBOL(<de>).

    DATA(lv_tabix) = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <de>-matnr
      IMPORTING
        output       = <de>-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF <de>-matnr IS INITIAL.
      CLEAR txt.
      txt = 'Favor informar o material!'.
      MESSAGE txt TYPE 'S'.
      EXIT.
    ELSE.
      SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
       WHERE matnr EQ @<de>-matnr.
      IF sy-subrc NE 0.
        CLEAR txt.
        txt = 'Material informado não existe!'.
        MESSAGE txt TYPE 'S'.
        EXIT.
      ELSE.
        SELECT SINGLE * FROM mard INTO @DATA(wa_mard)
         WHERE matnr EQ @<de>-matnr
         AND   werks EQ @wa_tela-werks
         AND   lgort EQ @wa_tela-lgort.

        IF sy-subrc NE 0.
          CLEAR txt.
          CONCATENATE  'Material não esta expandido para o depósito ' wa_tela-lgort INTO txt SEPARATED BY space.
          MESSAGE txt TYPE 'S'.
          CLEAR: <de>-matnr, <de>-maktx.
          EXIT.
        ELSEIF wa_mard-sperr IS NOT INITIAL.
          CLEAR txt.
          txt  = 'Movimentação do material bloqueado. Contagem inventário'.
          MESSAGE txt TYPE 'E'.
          CLEAR: <de>-matnr, <de>-maktx.
          EXIT.
        ELSE.

          SELECT SINGLE * FROM mara INTO @DATA(wa_mara)
           WHERE matnr EQ @<de>-matnr.

          IF wa_mara-xchpf IS NOT INITIAL.
            IF <de>-charg IS INITIAL.
              CLEAR txt.
              CONCATENATE 'Favor informar o numero do lote para o material ' <de>-matnr INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'S'.
              EXIT.
            ELSE.
              SELECT SINGLE * FROM mchb INTO wa_mchb
                             WHERE matnr EQ <de>-matnr
                             AND   charg EQ <de>-charg
                             AND   werks EQ wa_tela-werks
                             AND   lgort EQ wa_tela-lgort.

              IF sy-subrc = 0.
                IF  <de>-clabs > wa_mchb-clabs.
                  CLEAR txt.
                  CONCATENATE 'Saldo insuficiente para o pocesso!  Material:  ' <de>-matnr   ' Lote: ' <de>-charg  INTO txt SEPARATED BY space.
                  MESSAGE msg01 TYPE 'E'.
                  EXIT.
                ELSEIF  wa_mchb-clabs IS INITIAL.
                  CLEAR txt.
                  CONCATENATE 'Lote '  <de>-charg ' Sem saldo!' INTO txt SEPARATED BY space.
                  MESSAGE txt TYPE 'S'.
                  <de>-clabs = '0.000'.
                  EXIT.
                ENDIF.
              ELSE.
                CLEAR txt.
                CONCATENATE 'Lote ' <de>-charg ' não existe' INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'S'.
                EXIT.
              ENDIF.
            ENDIF.

          ELSE.
            SELECT SINGLE * FROM mard INTO wa_mard
                           WHERE matnr EQ <de>-matnr
                           AND   werks EQ wa_tela-werks
                           AND   lgort EQ wa_tela-lgort.

            IF sy-subrc = 0.
              IF  <de>-clabs > wa_mard-labst.
                CLEAR msg01.
                CONCATENATE 'Saldo insuficiente para o pocesso!  Material:  ' <de>-matnr INTO msg01 SEPARATED BY space.
                MESSAGE msg01 TYPE 'S'.
                EXIT.
              ELSEIF wa_mard-labst IS INITIAL.
                CLEAR txt.
                CONCATENATE  'Material '  <de>-matnr ' sem saldo !' INTO txt SEPARATED BY space.
                MESSAGE txt TYPE ''.
                <de>-clabs = '0.000'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <de>-clabs EQ '0.000'.
          CLEAR txt.
          CONCATENATE  'Favor informar a Quantidade para o Material ' <de>-matnr INTO txt SEPARATED BY space.
          MESSAGE txt TYPE 'S'.
          EXIT.
        ENDIF.


        READ TABLE tg_para ASSIGNING FIELD-SYMBOL(<fs_para>) INDEX lv_tabix.

        "FF #185986 - inicio

        IF psoma = abap_true.

          SELECT SINGLE meins
          INTO @DATA(lv_unid_para)
          FROM mara
          WHERE matnr = @<fs_para>-pmatnr.

          IF sy-subrc = 0.

            DATA(lv_unid_de) = wa_mara-meins.

            IF lv_unid_de = lv_unid_para.

              CLEAR txt.
              "VERRO = 'X'.
              CONCATENATE 'A unidade de medida de origem' lv_unid_de' deve ser diferente da UM destino' lv_unid_para INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'E'.
              EXIT.
            ENDIF.

          ENDIF.
        ENDIF.
        "FF #185986 - fim

        IF <de>-matnr EQ <fs_para>-pmatnr.

          CLEAR txt.
          "VERRO = 'X'.
          CONCATENATE 'Material de origem' <de>-matnr 'não pode ser igual a material destino' <fs_para>-pmatnr INTO txt SEPARATED BY space.
          MESSAGE txt TYPE 'E'.
          EXIT.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.


  LOOP AT tg_para ASSIGNING FIELD-SYMBOL(<para>).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <para>-pmatnr
      IMPORTING
        output       = <para>-pmatnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF <para>-pmatnr IS INITIAL.
      CLEAR txt.
      txt = 'Favor informar o material!'.
      MESSAGE txt TYPE 'E'.
      EXIT.
    ELSE.
      SELECT SINGLE * FROM makt INTO wa_makt
       WHERE matnr EQ <para>-pmatnr.
      IF sy-subrc NE 0.
        CLEAR txt.
        txt = 'Material informado não existe!'.
        MESSAGE txt TYPE 'E'.
        EXIT.
      ELSE.

        SELECT SINGLE * FROM mard INTO wa_mard
         WHERE matnr EQ <para>-pmatnr
         AND   werks EQ wa_tela-werks
         AND   lgort EQ wa_tela-lgort.

        IF sy-subrc NE 0.
          CLEAR txt.
          CONCATENATE  'Material não esta expandido para o depósito ' wa_tela-lgort INTO txt SEPARATED BY space.
          MESSAGE txt TYPE 'E'.
          CLEAR: <para>-pmatnr, <para>-pmaktx.
          EXIT.
        ELSEIF wa_mard-sperr IS NOT INITIAL.
          CLEAR txt.
          txt  = 'Movimentação do material bloqueado. Contagem inventário'.
          MESSAGE txt TYPE 'E'.
          CLEAR: <para>-pmatnr, <para>-pmaktx.
          EXIT.
        ELSE.
          SELECT SINGLE * FROM mara INTO wa_mara
           WHERE matnr EQ <para>-pmatnr.

          IF wa_mara-xchpf IS NOT INITIAL.
            IF <para>-pcharg IS INITIAL.
              CLEAR txt.
              CONCATENATE 'Favor informar o numero do lote para o material ' <para>-pmatnr INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'E'.
              EXIT.
            ELSE.
              SELECT SINGLE * FROM mchb INTO wa_mchb
                             WHERE matnr EQ <para>-pmatnr
                             AND   charg EQ <para>-pcharg
                             AND   werks EQ wa_tela-werks
                             AND   lgort EQ wa_tela-lgort.

              IF sy-subrc NE 0.
                CLEAR txt.
                CONCATENATE 'Lote informado para o material ' <para>-pmatnr 'não existe! ' INTO txt SEPARATED BY space.
                MESSAGE txt TYPE 'E'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <para>-pclabs EQ '0.000'.
          CLEAR txt.
          CONCATENATE  'Favor informar a Quantidade para o Material ' <de>-matnr INTO txt SEPARATED BY space.
          MESSAGE txt TYPE 'E'.
          EXIT.
        ELSE.
          LOOP AT tg_soma ASSIGNING FIELD-SYMBOL(<soma_de>).
            soma_total =  soma_total + <soma_de>-total.
            soma_clabs =  soma_clabs + <soma_de>-clabs.
          ENDLOOP.

          IF pmaior = 'X'.
            IF <para>-pclabs < soma_clabs.
              CLEAR txt.
              "VERRO = 'X'.
              CONCATENATE 'A quantidade informada no PARA, deverá ser maior que a soma da quantidade informada no DE. Material: ' <para>-pmatnr INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'E'.
              EXIT.
            ENDIF.
          ELSEIF pmenor = 'X'.

            IF <para>-pclabs > soma_clabs.
              CLEAR txt.
              "VERRO = 'X'.
              CONCATENATE 'A quantidade informada no PARA, deverá ser menor que a soma da quantidade informada no DE. Material: ' <para>-pmatnr INTO txt SEPARATED BY space.
              MESSAGE txt TYPE 'E'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.


      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*** Inicio - Rubenilson - 25.09.24 - #150189
*&---------------------------------------------------------------------*
*& Form f_select_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FILE
*&---------------------------------------------------------------------*
FORM f_select_file  USING p_filename TYPE localfile.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO p_filename INDEX 1.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form gera_modelo_planillha
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gera_modelo_planillha .

*  perform cria_linhas.

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata      TYPE REF TO data,
        v_nome_arquivo TYPE char50.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name(50) TYPE c,
    END OF t_fieldnames.

  CONCATENATE 'PlanilhaCargaModelo_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '_' sy-uzeit INTO v_nome_arquivo.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = v_nome_arquivo
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.xls' INTO p_local.
*    if p_troc is initial.
*      t_fieldnames-name    = 'Lote'.
*      append t_fieldnames.
*      t_fieldnames-name    = 'Depósito'.
*      append t_fieldnames.
*      t_fieldnames-name    = 'Quantidade Origem'.
*      append t_fieldnames.
**    t_fieldnames-name    = 'Quantidade Destino'. " US 156567 // MMSILVA - 30.10.2024
**    append t_fieldnames. " US 156567 // MMSILVA - 30.10.2024
*      t_fieldnames-name    = 'Centro'.
*      append t_fieldnames.
*      t_fieldnames-name    = 'Código Material Origem'.
*      append t_fieldnames.
*      t_fieldnames-name    = 'Código Material Destino'.
*      append t_fieldnames.
*      t_fieldnames-name    = 'Valor total'.
*      append t_fieldnames.
*** Inicio - Rubenilson - 05.02.24 - US156567

*      t_fieldnames-name    = 'Quantidade Material Destino'.
*      APPEND t_fieldnames.
*    ENDIF.
*** Fim - Rubenilson - 05.02.24 - US156567

*    else.
    t_fieldnames-name    = 'De'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Depósito origem'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Quantidade origem'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Und origem'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Lote'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Para'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Deposito destino'.
    APPEND t_fieldnames.
*      if p_troc is not initial.
    t_fieldnames-name    = 'Quantidade Destino'.
    APPEND t_fieldnames.
*      endif.
    t_fieldnames-name    = 'Und destino'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Lote destino'.
    APPEND t_fieldnames.
*    endif.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = p_local
        filetype              = 'ASC'
        write_field_separator = 'X'
        "CODEPAGE            = '8404'
      TABLES
        data_tab              = t_file
        fieldnames            = t_fieldnames
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        invalid_filesize      = 3
        invalid_table_width   = 4
        invalid_type          = 5
        no_batch              = 6
        unknown_error         = 7
        OTHERS                = 8.

    IF sy-subrc = 0.
      MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
    ELSE.
      MESSAGE 'Arquivo processado com erro' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_lancamento_massivo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_lancamento_massivo .

  PERFORM f_carrega_arquivo.
  PERFORM f_processa_dados.

ENDFORM.


**********************************************************************
* carregar arquivo
**********************************************************************
FORM f_carrega_arquivo.

  DATA: l_erro       TYPE char1,
        l_cols       TYPE i,
        t_tab        TYPE TABLE OF alsmex_tabline,
        wa_file      TYPE zpp_modelo_plan,
        lv_matnr_org TYPE matnr18,
        lv_matnr_des TYPE matnr18.

  FIELD-SYMBOLS: <fs_fld> TYPE any.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = TEXT-130.

*----------------------------------------
* upload excel
*----------------------------------------
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 12
      i_end_row               = 30000
    TABLES
      intern                  = t_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH TEXT-100 p_file DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  FREE: l_erro, l_cols.

  DELETE t_tab WHERE row = 0001.

  LOOP AT t_tab INTO DATA(w_tab).

    l_cols = l_cols + 1.

*    if w_tab-col = 7.
*      replace ',' into w_tab-value with '.'.
*    endif.
    ASSIGN COMPONENT w_tab-col OF STRUCTURE wa_file TO <fs_fld>.
    <fs_fld> = w_tab-value.
    AT END OF row.

      IF l_cols <> 12 AND  w_tab-row = 0001.
        l_erro = abap_true.
      ENDIF.
      APPEND wa_file TO t_file.
      CLEAR wa_file.
      FREE l_cols.
    ENDAT.
  ENDLOOP.


  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH TEXT-115 TEXT-116 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  LOOP AT t_file ASSIGNING FIELD-SYMBOL(<fs_file>).
    lv_matnr_org    = |{ <fs_file>-matnr_org ALPHA = IN }|.
    <fs_file>-matnr_org = lv_matnr_org.
    lv_matnr_des    = |{ <fs_file>-matnr_des ALPHA = IN }|.
    <fs_file>-matnr_des = lv_matnr_des.
    <fs_file>-werks = |{ <fs_file>-werks     ALPHA = IN }|.
  ENDLOOP.

ENDFORM.


FORM f_processa_dados.

  DATA: lr_matnr TYPE RANGE OF matnr18,
        lv_matnr TYPE matnr,
        lv_msg   TYPE string.

  DATA: lr_events     TYPE REF TO cl_salv_events_table,
        lr_selections TYPE REF TO cl_salv_selections,
        lr_functions  TYPE REF TO cl_salv_functions_list,
        lr_columns    TYPE REF TO cl_salv_columns_table,
        lr_column     TYPE REF TO cl_salv_column.


  "FF #185986 - inicio
  LOOP AT t_file ASSIGNING FIELD-SYMBOL(<fs>).
    <fs>-lgort_org = to_upper( <fs>-lgort_org ).
    <fs>-lgort_des = to_upper( <fs>-lgort_des ).
  ENDLOOP.
  "FF #185986 - fim

  DATA(lt_file) = t_file.

  SELECT matnr, meins                                       "FF #185986
    FROM mara
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @lt_file
    WHERE matnr EQ @lt_file-matnr_org
      OR  matnr EQ @lt_file-matnr_des.
  IF sy-subrc IS INITIAL.
    SORT lt_mara BY matnr.
  ENDIF.

  SELECT matnr,werks, lgort "BUG #180679 - MMSILVA - 27.05.2025 - Acrescentado "LGORT"
    FROM mard
    INTO TABLE @DATA(lt_mard)
    FOR ALL ENTRIES IN @lt_file
    WHERE ( matnr = @lt_file-matnr_org
       OR   matnr = @lt_file-matnr_des )
      AND   werks = @lt_file-werks.
  IF sy-subrc IS INITIAL.
    SORT lt_mard BY matnr werks.
  ENDIF.

  DELETE lt_file WHERE charg_org IS INITIAL.

  IF lt_file IS NOT INITIAL.

    SELECT matnr,werks,lgort,charg
      FROM mchb
      INTO TABLE @DATA(lt_mchb)
      FOR ALL ENTRIES IN @lt_file
      WHERE ( matnr = @lt_file-matnr_org
         OR   matnr = @lt_file-matnr_des )
        AND werks = @lt_file-werks

"FF #185986 - inicio
    AND (
          ( matnr = @lt_file-matnr_org
            AND lgort = @lt_file-lgort_org
            AND charg = @lt_file-charg_org )
       OR ( matnr = @lt_file-matnr_des
            AND lgort = @lt_file-lgort_des
            AND charg = @lt_file-charg_des )
        ).
    "FF #185986 - fim

    IF sy-subrc IS INITIAL.
      SORT lt_mchb BY matnr werks lgort charg.
    ENDIF.

  ENDIF.

  DATA lv_und_org TYPE meins.
  DATA lv_und_des TYPE meins.

  LOOP AT t_file ASSIGNING FIELD-SYMBOL(<fs_file>).

    APPEND INITIAL LINE TO gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    MOVE-CORRESPONDING <fs_file> TO <fs_saida>.

    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = <fs_saida>-und_org
        language       = sy-langu
      IMPORTING
        output         = lv_und_org
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_und_org IS NOT INITIAL.
      <fs_saida>-und_org = lv_und_org.
      CLEAR lv_und_org.
    ENDIF.
    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA - FIM
*    if <fs_saida>-valor_total is initial.
*      <fs_saida>-msg = 'Informe o valor total'.
*      <fs_saida>-status = '@S_TL_R@'.
*      continue.
*    endif.

    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = <fs_saida>-und_des
        language       = sy-langu
      IMPORTING
        output         = lv_und_des
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_und_des IS NOT INITIAL.
      <fs_saida>-und_des = lv_und_des.
      CLEAR lv_und_des.
    ENDIF.
    "185986 - CS2025000597 Ajustes e melhorias na transação ZPP0016 - RGARCIA - FIM


    READ TABLE lt_mara TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_file>-matnr_org
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Material' <fs_file>-matnr_org 'inválido' INTO <fs_saida>-msg SEPARATED BY space.
      <fs_saida>-status = '@S_TL_R@'.
      CONTINUE.
    ENDIF.

    READ TABLE lt_mara TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_file>-matnr_des
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Material' <fs_file>-matnr_des 'inválido' INTO <fs_saida>-msg SEPARATED BY space.
      <fs_saida>-status = '@S_TL_R@'.
      CONTINUE.
    ENDIF.

    READ TABLE lt_mard TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_file>-matnr_org
             werks = <fs_file>-werks
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Material' <fs_file>-matnr_org 'e centro' <fs_file>-werks 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
      <fs_saida>-status = '@S_TL_R@'.
      CONTINUE.
    ENDIF.

    READ TABLE lt_mard TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_file>-matnr_des
             werks = <fs_file>-werks
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONCATENATE 'Material' <fs_file>-matnr_des 'e centro' <fs_file>-werks 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
      <fs_saida>-status = '@S_TL_R@'.
      CONTINUE.
    ENDIF.

    IF <fs_file>-charg_org IS NOT INITIAL.

      READ TABLE lt_mchb TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_file>-matnr_org
               werks = <fs_file>-werks
               lgort = <fs_file>-lgort_org
               charg = <fs_file>-charg_org
      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Material' <fs_file>-matnr_org 'e lote' <fs_file>-charg_org 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
        <fs_saida>-status = '@S_TL_R@'.
        CONTINUE.
      ENDIF.

      READ TABLE lt_mchb TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_file>-matnr_des
               werks = <fs_file>-werks
               lgort = <fs_file>-lgort_des
               charg = <fs_file>-charg_des "185986 - RGARCIA
      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Material' <fs_file>-matnr_des 'e lote' <fs_file>-charg_des 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
        <fs_saida>-status = '@S_TL_R@'.
        CONTINUE.
      ENDIF.

    ENDIF.

*   BUG #180679 - MMSILVA - 27.05.2025 - Inicio
    IF <fs_file>-lgort_org IS NOT INITIAL.
      READ TABLE lt_mard TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_file>-matnr_org
               werks = <fs_file>-werks
               lgort = <fs_file>-lgort_org.
*      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Material' <fs_file>-matnr_org 'e depósito' <fs_file>-lgort_org 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
        <fs_saida>-status = '@S_TL_R@'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF <fs_file>-lgort_des IS NOT INITIAL.

      READ TABLE lt_mard TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_file>-matnr_des
               werks = <fs_file>-werks
               lgort = <fs_file>-lgort_des.

      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Material' <fs_file>-matnr_des 'e depósito' <fs_file>-lgort_des 'não expandido' INTO <fs_saida>-msg SEPARATED BY space.
        <fs_saida>-status = '@S_TL_R@'.
        CONTINUE.
      ENDIF.
    ENDIF.
*   BUG #180679 - MMSILVA - 27.05.2025 - Fim


    "FF #185986 - inicio
    READ TABLE lt_mara INTO DATA(wa_mara)
    WITH KEY matnr = <fs_file>-matnr_des.
    IF sy-subrc IS INITIAL.

      DATA(lv_unid_para) = wa_mara-meins.
    ENDIF.

    READ TABLE lt_mara INTO DATA(wa_mara2)
    WITH KEY matnr = <fs_file>-matnr_org.
    IF sy-subrc IS INITIAL.

      DATA(lv_unid_de) = wa_mara2-meins.
    ENDIF.


    IF lv_unid_de = lv_unid_para.

      CONCATENATE 'A unidade de medida de origem' lv_unid_de' deve ser diferente da UM destino' lv_unid_para INTO <fs_saida>-msg SEPARATED BY space.
      <fs_saida>-status = '@S_TL_R@'.
      CONTINUE.

    ENDIF.
    "FF #185986 - fim

  ENDLOOP.

  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = gr_alv
    CHANGING
      t_table      = gt_saida.

*** Inicio - Rubenilson - 05.02.24 - US156567




*  lr_columns = gr_alv->get_columns( ).
*  lr_column = lr_columns->get_column( 'UND_ORI' ).
*  lr_column->set_
*  if p_troc is initial.
*    lr_column->set_visible( abap_false ).
*  else.
*    lr_column->set_long_text( 'Quantidade Material Destino' ).
*    lr_column->set_medium_text( 'Qtd Material Destino' ).
*    lr_column->set_short_text( 'QtdMatDest' ).
*  endif.
*** Fim - Rubenilson - 05.02.24 - US156567

  gr_alv->set_screen_status(
    pfstatus      = 'SALV_STANDARD'
    report        = sy-repid
    set_functions = gr_alv->c_functions_all ).

  lr_events = gr_alv->get_event( ).

  CREATE OBJECT gr_events.

  SET HANDLER gr_events->on_user_command FOR lr_events.

  lr_selections = gr_alv->get_selections( ).

  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  gr_alv->display( ).
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  DATA: lv_erro           TYPE c,
        vga_value_z12_brl TYPE fins_vkcur12,
        vga_value_z11_brl TYPE fins_vkcur12,
        vga_value_z12_usd TYPE fins_vkcur12,
        vga_value_z11_usd TYPE fins_vkcur12,
        vga_value_dif_brl TYPE ckmpc_zuumb,
        vga_value_dif_usd TYPE ckmpc_zuumb,
        vga_mes           TYPE poper,
        vga_date_ano      TYPE jahrper.

  DATA:         lwa_budat    TYPE bapi_matval_debi_credi_date.
  DATA: l_refdoc     TYPE bapi_matval_debi_credi_misc-ref_doc_no.
  DATA: l_bwtyp      TYPE bapi_matval_key-val_type. " stays initial
  DATA: lwa_belnr    TYPE bapi_pricechange_document.
  DATA: lt_return    TYPE TABLE OF bapiret2.
  DATA: lt_post  TYPE bapi_mat_debit_credit_amt_tty.

  CASE i_ucomm.
    WHEN 'PROC_MASS'.

      DATA: lr_selections TYPE REF TO cl_salv_selections.

      DATA: lt_rows TYPE salv_t_row,
            lt_cols TYPE salv_t_column,
            ls_cell TYPE salv_s_cell.

      DATA: zvg_matnr         TYPE char18,
            zvg_price_brl     TYPE ck_pvprs_1,
            zvg_price_usd     TYPE ck_pvprs_1,
            zvg_vlr_total_brl TYPE ck_pvprs_1,
            zvg_vlr_total_usd TYPE ck_pvprs_1.

      lr_selections = gr_alv->get_selections( ).
      lt_rows = lr_selections->get_selected_rows( ).

      FIELD-SYMBOLS <fs_saida> TYPE ty_saida.


      LOOP AT lt_rows  ASSIGNING FIELD-SYMBOL(<fs_rows>).

        FREE: tl_item, it_estorno.
        CLEAR: zvg_matnr, zvg_price_usd, zvg_price_brl,  zvg_vlr_total_brl,
               zvg_vlr_total_usd, vga_value_dif_brl, vga_value_dif_brl, vga_value_z12_brl, vga_value_z12_usd, vga_value_z11_brl, vga_value_z11_usd, vga_mes, lwa_belnr.

        READ TABLE gt_saida ASSIGNING <fs_saida> INDEX <fs_rows>.

* ----> US #156567 - MMSILVA - 02.05.2025 - Inicio <----
        IF <fs_saida>-status = '@S_TL_R@'.
          CONTINUE.
        ENDIF.
* ----> US #156567 - MMSILVA - 02.05.2025 - Fim <----

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'ENQUEUE_EMMARCE'
            EXPORTING
              matnr          = <fs_saida>-matnr_org
              werks          = <fs_saida>-werks
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          sperr_user =  sy-msgv1.

          IF sy-subrc = 1.
            CONCATENATE 'Material ' <fs_saida>-matnr_org 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
            <fs_saida>-status = '@S_TL_R@'.
            CONTINUE.
          ENDIF.

          IF <fs_saida>-charg_org IS NOT INITIAL.
            fg_bloqueio      = 'X'.
            CALL FUNCTION 'ENQUEUE_EMMCH1E'
              EXPORTING
                mode_mch1      = 'E'
                mandt          = sy-mandt
                matnr          = <fs_saida>-matnr_org
                charg          = <fs_saida>-charg_org
                _scope         = '2'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            sperr_user  = sy-msgv1.

            IF sy-subrc = 1.
              CONCATENATE 'Lote ' <fs_saida>-charg_org 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
              <fs_saida>-status = '@S_TL_R@'.
              CONTINUE.
            ENDIF.
          ENDIF.

*  "iniciando a bapi
          vl_code = '05'.
          sl_header-pstng_date      =  sy-datum.
          sl_header-doc_date        =  sy-datum.
          sl_header-header_txt      =  p_form.
          sl_header-ref_doc_no      =  p_form. "US #156567 - MMSILVA - 02.05.2025
          sl_item-move_type         = 'Z12'.

          DATA(v_len3) = strlen( <fs_saida>-matnr_org ).
          IF v_len3 > 18.
            sl_item-material_long = <fs_saida>-matnr_org .
          ELSE.
            sl_item-material = <fs_saida>-matnr_org .
          ENDIF.

          zvg_matnr    = |{ sl_item-material ALPHA = OUT }|.
          zvg_matnr    = |{ zvg_matnr ALPHA = IN }|.
          sl_item-material = zvg_matnr.


*         Verifica preço material.
          CALL FUNCTION 'ZPP_CALCULATION_PRICE_MATERIAL'
            EXPORTING
              i_matnr     = sl_item-material
              i_werks     = <fs_saida>-werks
            IMPORTING
              e_price_usd = zvg_price_usd
              e_price_brl = zvg_price_brl.

          sl_item-plant             = <fs_saida>-werks.
          sl_item-stge_loc          = <fs_saida>-lgort_org.
          sl_item-batch             = <fs_saida>-charg_org.

*** Inicio - Rubenilson - 05.02.24 - US156567
*          if p_troc is not initial.
*            sl_item-entry_qnt         = <fs_saida>-menge_des."quantidade destino
*          else.
          sl_item-entry_qnt         = <fs_saida>-menge_org."quantidade
*          endif.
*** Fim - Rubenilson - 05.02.24 - US156567

          sl_item-gl_account        = '0000511000'.

          APPEND sl_item TO tl_item.
          CLEAR sl_item.

          wa_estorno-matnr  = <fs_saida>-matnr_org.
          wa_estorno-werks  = <fs_saida>-werks.
          wa_estorno-lgort  = <fs_saida>-lgort_org.
          wa_estorno-clabs  = <fs_saida>-menge_org.
          wa_estorno-charg  = <fs_saida>-charg_org.
*          wa_estorno-total  = <fs_saida>-menge.
          APPEND wa_estorno TO it_estorno.
          CLEAR wa_estorno.

          CLEAR: vl_doc, vl_year, vl_doc_de, vl_doc_year.
          FREE: tl_return.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
            EXPORTING
              goodsmvt_header  = sl_header
              goodsmvt_code    = vl_code
            IMPORTING
              materialdocument = vl_doc
              matdocumentyear  = vl_year
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          vl_doc_de    = vl_doc.
          vl_doc_year  = vl_year.


          READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            <fs_saida>-doc_z12 = vl_doc.
            CLEAR sl_return.
          ELSE.
            <fs_saida>-msg = sl_return-message.
            <fs_saida>-status = '@S_TL_R@'.
            CONTINUE.
          ENDIF.

          IF fg_bloqueio    = 'X'.

            DATA: lv_material TYPE marc-matnr .

            LOOP AT tl_item INTO sl_item.

              lv_material = sl_item-material.

              CALL FUNCTION 'DEQUEUE_EMMARCE'
                EXPORTING
                  matnr = lv_material
                  werks = sl_item-plant.


              IF sl_item-batch IS NOT INITIAL.

                CALL FUNCTION 'DEQUEUE_EMMCH1E'
                  EXPORTING
                    mode_mch1 = 'E'
                    mandt     = sy-mandt
                    matnr     = lv_material
                    charg     = sl_item-batch.
              ENDIF.

              CLEAR sl_item.
            ENDLOOP.
          ENDIF.

          FREE: tl_item.
          CLEAR: sl_item.
*** Inicio Z11
          CALL FUNCTION 'ENQUEUE_EMMARCE'
            EXPORTING
              matnr          = <fs_saida>-matnr_des
              werks          = <fs_saida>-werks
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          sperr_user =  sy-msgv1.

          IF sy-subrc = 1.
            CONCATENATE 'Material ' <fs_saida>-matnr_des 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
            <fs_saida>-status = '@S_TL_R@'.
            CONTINUE.
          ENDIF.

          IF <fs_saida>-charg_des IS NOT INITIAL.

            fg_bloqueio      = 'X'.
            CALL FUNCTION 'ENQUEUE_EMMCH1E'
              EXPORTING
                mode_mch1      = 'E'
                mandt          = sy-mandt
                matnr          = <fs_saida>-matnr_des
                charg          = <fs_saida>-charg_des
                _scope         = '2'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            sperr_user  = sy-msgv1.

            IF sy-subrc = 1.
              CONCATENATE 'Lote ' <fs_saida>-charg_des 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
              <fs_saida>-status = '@S_TL_R@'.
              CONTINUE.
            ENDIF.
          ENDIF.

          vl_code = '05'.
          sl_header-pstng_date       =  sy-datum.
          sl_header-doc_date         =  sy-datum.
          sl_header-header_txt       =  p_form.
          sl_header-ref_doc_no       =  p_form. "US #156567 - MMSILVA - 02.05.2025
          sl_item-move_type          = 'Z11'.

          DATA(v_len2) = strlen( <fs_saida>-matnr_des ).
          IF v_len2 > 18.
            sl_item-material_long = <fs_saida>-matnr_des.
          ELSE.
            sl_item-material = <fs_saida>-matnr_des .
          ENDIF.

          CLEAR: zvg_matnr.
          zvg_matnr        = |{ sl_item-material ALPHA = OUT }|.
          zvg_matnr        = |{ zvg_matnr ALPHA = IN }|.
          sl_item-material = zvg_matnr.

          IF zvg_price_brl IS NOT INITIAL.
            zvg_vlr_total_brl = zvg_price_brl * <fs_saida>-menge_org. "FF #185986 - <fs_saida>-menge_des.
          ENDIF.

          IF zvg_price_usd IS NOT INITIAL.
            zvg_vlr_total_usd = zvg_price_usd * <fs_saida>-menge_org. "FF #185986 - <fs_saida>-menge_des.
          ENDIF.

*          <fs_saida>-valor_total = zvg_vlr_total_brl.

          sl_item-plant              = <fs_saida>-werks.

          IF <fs_saida>-lgort_des IS INITIAL.
            sl_item-stge_loc           = <fs_saida>-lgort_org.
          ELSE.
            sl_item-stge_loc           = <fs_saida>-lgort_des.
          ENDIF.

          IF <fs_saida>-charg_des IS INITIAL.
            sl_item-batch              = <fs_saida>-charg_org.
          ELSE.
            sl_item-batch              = <fs_saida>-charg_des.
          ENDIF.

          IF <fs_saida>-menge_des IS INITIAL.
            sl_item-entry_qnt          = <fs_saida>-menge_org.
          ELSE.
            sl_item-entry_qnt          = <fs_saida>-menge_des.
          ENDIF.
          sl_item-amount_lc          = zvg_vlr_total_brl.

*          sl_item-amount_sv          = zvg_vlr_total_usd.

          sl_item-gl_account         = '0000511000'.
          APPEND sl_item TO tl_item.
          CLEAR: sl_item.

          FREE: tl_return.
          CLEAR: vl_doc, vl_year, vl_doc_para, vl_doc_year.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "
            EXPORTING
              goodsmvt_header  = sl_header
              goodsmvt_code    = vl_code
            IMPORTING
              materialdocument = vl_doc
              matdocumentyear  = vl_year
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          vl_doc_para = vl_doc.
          vl_doc_year = vl_year.

          READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            <fs_saida>-doc_z11 = vl_doc.

            CLEAR sl_return.

            CLEAR txt.
            CONCATENATE 'Processo gerado com sucesso! Documentos gerados: ' vl_doc_de '-' vl_doc_para INTO <fs_saida>-msg SEPARATED BY space.
            <fs_saida>-status = '@S_TL_G@'.
          ELSE.

            CLEAR:  sl_header,  vl_code, vl_doc,  vl_year, tl_item.

            LOOP AT it_estorno INTO wa_estorno.

              CALL FUNCTION 'ENQUEUE_EMMARCE'
                EXPORTING
                  matnr          = wa_estorno-matnr
                  werks          = wa_estorno-werks
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              sperr_user =  sy-msgv1.

              IF sy-subrc = 1.
                CONCATENATE 'Material ' wa_estorno-matnr 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
                <fs_saida>-status = '@S_TL_R@'.
                CONTINUE.
              ENDIF.

              IF <fs_saida>-charg_des IS NOT INITIAL.

                fg_bloqueio      = 'X'.
                CALL FUNCTION 'ENQUEUE_EMMCH1E'
                  EXPORTING
                    mode_mch1      = 'E'
                    mandt          = sy-mandt
                    matnr          = wa_estorno-matnr
                    charg          = wa_estorno-charg
                    _scope         = '2'
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.

                sperr_user  = sy-msgv1.

                IF sy-subrc = 1.
                  CONCATENATE 'Lote ' wa_estorno-charg 'bloqueado por ' sperr_user INTO <fs_saida>-msg SEPARATED BY space.
                  <fs_saida>-status = '@S_TL_R@'.
                  CONTINUE.
                ENDIF.
              ENDIF.

              vl_code = '05'.
              sl_header-pstng_date       = sy-datum.
              sl_header-doc_date         = sy-datum.
              sl_header-header_txt       = p_form.
              sl_header-ref_doc_no       = p_form. "US #156567 - MMSILVA - 02.05.2025
              sl_item-move_type          = 'Z11'.

              DATA(v_len1) = strlen( wa_estorno-matnr ).
              IF v_len1 > 18.
                sl_item-material_long = wa_estorno-matnr .
              ELSE.
                sl_item-material = wa_estorno-matnr .
              ENDIF.

              sl_item-plant              = wa_estorno-werks.
              sl_item-stge_loc           = wa_estorno-lgort.
              sl_item-batch              = wa_estorno-charg.
              sl_item-entry_qnt          = wa_estorno-clabs.
              sl_item-amount_lc          = wa_estorno-total.
              sl_item-gl_account         = '0000511000'.
              APPEND sl_item TO tl_item.
              CLEAR: wa_estorno.
            ENDLOOP.

            CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
              EXPORTING
                goodsmvt_header  = sl_header
                goodsmvt_code    = vl_code
              IMPORTING
                materialdocument = vl_doc
                matdocumentyear  = vl_year
              TABLES
                goodsmvt_item    = tl_item
                return           = tl_return_cancel.

            vl_doc_est = vl_doc.

            READ TABLE tl_return_cancel INTO sl_return_cancel WITH KEY type = 'E'.

            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              CLEAR txt.
*              concatenate 'Ocorreu um erro no processamento. Foi realizado um estorno. Documento:' vl_doc_est into <fs_saida>-msg separated by space.
              CONCATENATE sl_return-message ' | ' 'Documento de Estorno:' vl_doc_est  INTO <fs_saida>-msg SEPARATED BY space.
              <fs_saida>-status = '@S_TL_R@'.
              CLEAR sl_return.
              CONTINUE.
            ELSE.
              <fs_saida>-msg = sl_return_cancel-message.
              <fs_saida>-status = '@S_TL_R@'.
              CONTINUE.
            ENDIF.

          ENDIF.

          IF fg_bloqueio    = 'X'.

            DATA: lv_move_mat TYPE marc-matnr.

            LOOP AT tl_item INTO sl_item.

              lv_material = sl_item-material.
              lv_move_mat = sl_item-move_mat.

              CALL FUNCTION 'DEQUEUE_EMMARCE'
                EXPORTING
                  matnr = lv_material
                  werks = sl_item-plant.

              CALL FUNCTION 'DEQUEUE_EMMARCE'
                EXPORTING
                  matnr = lv_move_mat
                  werks = sl_item-move_plant.

              IF sl_item-batch IS NOT INITIAL.

                CALL FUNCTION 'DEQUEUE_EMMCH1E'
                  EXPORTING
                    mode_mch1 = 'E'
                    mandt     = sy-mandt
                    matnr     = lv_material
                    charg     = sl_item-batch.
              ENDIF.

              IF sl_item-move_batch IS NOT INITIAL.

                CALL FUNCTION 'DEQUEUE_EMMCH1E'
                  EXPORTING
                    mode_mch1 = 'E'
                    mandt     = sy-mandt
                    matnr     = lv_move_mat
                    charg     = sl_item-move_batch.
              ENDIF.

              CLEAR sl_item.
            ENDLOOP.

          ENDIF.

          "Ajustar diferença preço dolar / MR22.
*          clear: vga_value_z12, vga_value_z11, vga_mes, lwa_belnr.
          "Seleciona valor documento tipo de movimento Z12.


          CLEAR: vga_date_ano.
          vga_date_ano = |{ sy-datum+4(2) }{ sy-datum+0(4) }|.
          vga_mes      = sy-datum+4(2).

          CALL FUNCTION 'ZPP_GET_VALUE_KSL_ACDOCA'
            EXPORTING
              i_rldnr = '0L'
*             i_rbukrs      = ''
*             i_fiscyearper = vga_date_ano
              i_matnr = <fs_saida>-matnr_org
              i_bwkey = <fs_saida>-werks
              i_belnr = <fs_saida>-doc_z12
              i_mjahr = vl_year
              i_poper = vga_mes
            IMPORTING
              e_ksl   = vga_value_z12_usd
              e_hsl   = vga_value_z12_brl.


          "Seleciona valor documento tipo de movimento Z11.
          CALL FUNCTION 'ZPP_GET_VALUE_KSL_ACDOCA'
            EXPORTING
              i_rldnr = '0L'
*             i_rbukrs      = ''
*             i_fiscyearper = vga_date_ano
              i_matnr = <fs_saida>-matnr_des
              i_bwkey = <fs_saida>-werks
              i_belnr = <fs_saida>-doc_z11
              i_mjahr = vl_year
              i_poper = vga_mes
            IMPORTING
              e_ksl   = vga_value_z11_usd
              e_hsl   = vga_value_z11_brl.


          "Ajustar preço USD - Dolar.
          IF vga_value_z12_usd NE vga_value_z11_usd.
            vga_value_dif_usd = ( vga_value_z12_usd - vga_value_z11_usd ).

            CALL FUNCTION 'ZPP_CHANGE_MR22_USD'
              EXPORTING
                i_matnr  = <fs_saida>-matnr_des
                i_werks  = <fs_saida>-werks
                i_text   = p_form
                i_data   = sy-datum
                i_amount = vga_value_dif_usd
              IMPORTING
                e_belnr  = <fs_saida>-doc_mr22_usd.

          ENDIF.

          "Ajustar preço BRL - Reais.
          IF vga_value_z12_brl NE vga_value_z11_brl.
            vga_value_dif_brl = ( vga_value_z12_brl - vga_value_z11_brl ).

            CALL FUNCTION 'ZPP_CHANGE_MR22_BRL'
              EXPORTING
                i_matnr  = <fs_saida>-matnr_des
                i_werks  = <fs_saida>-werks
                i_text   = p_form
                i_data   = sy-datum
                i_amount = vga_value_dif_brl
              IMPORTING
                e_belnr  = <fs_saida>-doc_mr22_brl.

          ENDIF.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  CALL METHOD gr_alv->refresh( ).

ENDFORM.
*** Fim - Rubenilson - 25.09.24 - #150189
