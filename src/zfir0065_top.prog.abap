*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_TOP
*&---------------------------------------------------------------------*


TYPE-POOLS icon.

TYPES: BEGIN OF ty_saida,
         codigo        TYPE zfit0109-codigo,
         clas_flx      TYPE zfit0109-clas_flx,
         seq           TYPE zfit0109-seq,
         descricao     TYPE zfit0109-descricao,
         ocultar       TYPE zfit0109-ocultar,
         tp_prev       TYPE zfit0109-tp_prev,
         st_calc_sdo   TYPE zfit0109-st_calc_sdo,
         tipo_doc      TYPE zfit0109-tipo_doc,
         tipo_ped      TYPE zfit0109-tipo_ped,
         tipo_ov       TYPE zfit0109-tipo_ov,
         tipo_doc_1    TYPE zfit0109-tipo_doc,
         tipo_ped_1    TYPE zfit0109-tipo_ped,
         tipo_ov_1     TYPE zfit0109-tipo_ov,
         bloq_pgto     TYPE zfit0109-bloq_pgto,
         bloq_pgto_1   TYPE zfit0109-bloq_pgto,
         forma_pgto    TYPE zfit0109-forma_pgto,
         bco_empresa   TYPE zfit0109-bco_empresa,
         opr_sobra_cxa TYPE zfit0109-opr_sobra_cxa,
         processo_esp  TYPE zfit0109-processo_esp,
         sistema_orig  TYPE zfit0109-sistema_orig,
         usnam         TYPE zfit0109-usnam,
         dt_atual      TYPE zfit0109-dt_atual,
         hr_atual      TYPE zfit0109-hr_atual,
         soc_parceira  TYPE zfit0109-soc_parceira,
       END OF ty_saida.

DATA: BEGIN OF tg_tp_doc OCCURS 0,
        blart TYPE t003-blart,
      END OF tg_tp_doc,

      BEGIN OF tg_tp_ped OCCURS 0,
        bsart TYPE t161-bsart,
      END OF tg_tp_ped,

      BEGIN OF tg_tp_ov OCCURS 0,
        auart TYPE tvak-auart,
      END OF tg_tp_ov.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida        TYPE TABLE OF ty_saida,
      wa_saida        TYPE ty_saida,
      it_zfit0109     TYPE TABLE OF zfit0109,
      wa_zfit0109     TYPE zfit0109,
      wa_zfit0109_aux TYPE zfit0109.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

**---------------------------------------------------------------------*
**  Inicio Implementação Classes
**---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.                                           "
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "

"DATA: EVENT_HANDLER_0106 TYPE REF TO LCL_EVENT_HANDLER_0106. "

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.                "

  METHOD on_f4.

    "TYPES
    TYPES: BEGIN OF ty_tp_doc,
             blart TYPE t003-blart,
             ltext TYPE t003t-ltext,
           END OF ty_tp_doc,

           BEGIN OF ty_tp_ped,
             bsart TYPE t161-bsart,
             batxt TYPE t161t-batxt,
           END OF ty_tp_ped,

           BEGIN OF ty_tp_ov,
             auart TYPE tvak-auart,
             bezei TYPE tvakt-bezei,
           END OF ty_tp_ov,

           BEGIN OF ty_forma_pgto,
             zlsch TYPE t042z-zlsch,
             text1 TYPE t042z-text1,
           END OF ty_forma_pgto,

           BEGIN OF ty_bloq_pgto,
             zahls TYPE t008-zahls,
             textl TYPE t008t-textl,
           END OF ty_bloq_pgto.

    DATA: lt_map        TYPE TABLE OF dselc,
          ls_map        TYPE dselc,
          lt_return     TYPE TABLE OF ddshretval,
          ls_return     TYPE ddshretval,
          ls_stable     TYPE lvc_s_stbl,

          "Tipo Doc
          lt_tp_doc     TYPE TABLE OF ty_tp_doc,
          ls_tp_doc     TYPE ty_tp_doc,
          it_t003t      TYPE TABLE OF t003t,
          wa_t003t      TYPE t003t,

          "Tipo Pedido
          lt_tp_ped     TYPE TABLE OF ty_tp_ped,
          ls_tp_ped     TYPE ty_tp_ped,
          it_t161t      TYPE TABLE OF t161t,
          wa_t161t      TYPE t161t,

          "Tipo Ordem Venda
          lt_tp_ov      TYPE TABLE OF ty_tp_ov,
          ls_tp_ov      TYPE ty_tp_ov,
          it_tvakt      TYPE TABLE OF tvakt,
          wa_tvakt      TYPE tvakt,

          "Forma Pagto.
          lt_forma_pgto TYPE TABLE OF ty_forma_pgto,
          ls_forma_pgto TYPE ty_forma_pgto,

          "Bloqueio Pagto.
          lt_bloq_pgto  TYPE TABLE OF ty_bloq_pgto,
          ls_bloq_pgto  TYPE ty_bloq_pgto,

          it_t008t      TYPE TABLE OF t008t,
          wa_t008t      TYPE t008t.


    DATA: vl_tp_doc_aux(100)    TYPE c,
          vl_tp_ped_aux(40)    TYPE c,
          vl_tp_ov_aux(40)     TYPE c,
          vl_bloq_pgto_aux(40) TYPE c.

    "FIELD-SYMBOLS
    FIELD-SYMBOLS: <l_out> TYPE ty_saida. " ALV TABLE LINE
    FIELD-SYMBOLS: <l_t003> TYPE ty_tp_doc.
    FIELD-SYMBOLS: <l_t161> TYPE ty_tp_ped.
    FIELD-SYMBOLS: <l_tvak> TYPE ty_tp_ov.
    FIELD-SYMBOLS: <l_t008> TYPE ty_bloq_pgto.

    " CHECK WHICH FIELD RAISE F4 EVENT
    CASE e_fieldname.
      WHEN 'TIPO_DOC_1'.

        " READ CURRENT LINE
        READ TABLE it_saida ASSIGNING <l_out> INDEX es_row_no-row_id.

        "LOAD F4 DATA
        SELECT blart
          INTO TABLE lt_tp_doc
          FROM t003.

        SELECT *
          INTO TABLE it_t003t
          FROM t003t
         WHERE spras = sy-langu.

        LOOP AT lt_tp_doc ASSIGNING <l_t003>.
          READ TABLE it_t003t INTO wa_t003t WITH KEY blart = <l_t003>-blart.

          <l_t003>-ltext = wa_t003t-ltext.

        ENDLOOP.

        SORT lt_tp_doc BY ltext.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'BLART'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'BLART'
            value_org       = 'S'
          TABLES
            value_tab       = lt_tp_doc
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          " UPDATE ALV TABLE

          CLEAR: vl_tp_doc_aux.
          CONDENSE <l_out>-tipo_doc_1 NO-GAPS.

          IF ( <l_out>-tipo_doc_1 IS INITIAL ).
            vl_tp_doc_aux = ls_return-fieldval.
          ELSE.
            CONCATENATE <l_out>-tipo_doc_1  ',' ls_return-fieldval INTO vl_tp_doc_aux.
          ENDIF.

          <l_out>-tipo_doc_1 = vl_tp_doc_aux.

        ENDIF.

      WHEN 'TIPO_PED_1'.

        " READ CURRENT LINE
        READ TABLE it_saida ASSIGNING <l_out> INDEX es_row_no-row_id.

        "LOAD F4 DATA
        SELECT bsart
          INTO TABLE lt_tp_ped
          FROM t161
         WHERE bstyp EQ 'F'.

        SELECT *
          INTO TABLE it_t161t
          FROM t161t
         WHERE spras = sy-langu.

        LOOP AT lt_tp_ped ASSIGNING <l_t161>.
          READ TABLE it_t161t INTO wa_t161t WITH KEY bsart = <l_t161>-bsart.

          <l_t161>-batxt = wa_t161t-batxt.

        ENDLOOP.

        SORT lt_tp_ped BY batxt.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'BSART'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'BSART'
            value_org       = 'S'
          TABLES
            value_tab       = lt_tp_ped
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          " UPDATE ALV TABLE

          CLEAR: vl_tp_ped_aux.
          CONDENSE <l_out>-tipo_ped_1 NO-GAPS.

          IF ( <l_out>-tipo_ped_1 IS INITIAL ).
            vl_tp_ped_aux = ls_return-fieldval.
          ELSE.
            CONCATENATE <l_out>-tipo_ped_1 ',' ls_return-fieldval INTO vl_tp_ped_aux.
          ENDIF.

          <l_out>-tipo_ped_1 = vl_tp_ped_aux.

        ENDIF.

      WHEN 'TIPO_OV_1'.

        " READ CURRENT LINE
        READ TABLE it_saida ASSIGNING <l_out> INDEX es_row_no-row_id.

        "LOAD F4 DATA
        SELECT auart
          INTO TABLE lt_tp_ov
          FROM tvak.

        SELECT *
          INTO TABLE it_tvakt
          FROM tvakt
         WHERE spras = sy-langu.

        LOOP AT lt_tp_ov ASSIGNING <l_tvak>.
          READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = <l_tvak>-auart.

          <l_tvak>-bezei = wa_tvakt-bezei.

        ENDLOOP.

        SORT lt_tp_ov BY bezei.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'AUART'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'AUART'
            value_org       = 'S'
          TABLES
            value_tab       = lt_tp_ov
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          " UPDATE ALV TABLE

          CLEAR: vl_tp_ov_aux.
          CONDENSE <l_out>-tipo_ov_1 NO-GAPS.

          IF ( <l_out>-tipo_ov_1 IS INITIAL ).
            vl_tp_ov_aux = ls_return-fieldval.
          ELSE.
            CONCATENATE <l_out>-tipo_ov_1 ',' ls_return-fieldval INTO vl_tp_ov_aux.
          ENDIF.

          <l_out>-tipo_ov_1 = vl_tp_ov_aux.

        ENDIF.

      WHEN 'BLOQ_PGTO_1'.

        " READ CURRENT LINE
        READ TABLE it_saida ASSIGNING <l_out> INDEX es_row_no-row_id.


        "LOAD F4 DATA
        SELECT zahls
          INTO TABLE lt_bloq_pgto
          FROM t008.

        SELECT *
          INTO TABLE it_t008t
          FROM t008t
         WHERE spras = sy-langu.

        LOOP AT lt_bloq_pgto ASSIGNING <l_t008>.
          READ TABLE it_t008t INTO wa_t008t WITH KEY zahls = <l_t008>-zahls.

          <l_t008>-textl = wa_t008t-textl.

        ENDLOOP.

        SORT lt_bloq_pgto BY textl.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'ZAHLS'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ZAHLS'
            value_org       = 'S'
          TABLES
            value_tab       = lt_bloq_pgto
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          " UPDATE ALV TABLE

          CLEAR: vl_bloq_pgto_aux.
          CONDENSE <l_out>-bloq_pgto_1 NO-GAPS.

          IF ( <l_out>-bloq_pgto_1 IS INITIAL ).
            vl_bloq_pgto_aux = ls_return-fieldval.
          ELSE.
            CONCATENATE <l_out>-bloq_pgto_1 ',' ls_return-fieldval INTO vl_bloq_pgto_aux.
          ENDIF.

          <l_out>-bloq_pgto_1 = vl_bloq_pgto_aux.

        ENDIF.

      WHEN 'FORMA_PGTO'.

        " READ CURRENT LINE
        READ TABLE it_saida ASSIGNING <l_out> INDEX es_row_no-row_id.

        "LOAD F4 DATA
        SELECT zlsch text1
          INTO TABLE lt_forma_pgto
          FROM t042z
         WHERE land1 = 'BR'.

        SORT lt_forma_pgto BY text1.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'ZLSCH'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ZLSCH'
            value_org       = 'S'
          TABLES
            value_tab       = lt_forma_pgto
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          " UPDATE ALV TABLE
          <l_out>-forma_pgto = vl_tp_ov_aux =  ls_return-fieldval.
        ENDIF.

    ENDCASE.

    ls_stable = ''. " SET STABLE REFRESH FOR ROW AND COLUMN

    " ALV REFRESH
    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = 'X'
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

    " AVOID POSSIBLE STANDARD SEARCH HELP
    er_event_data->m_event_handled = 'X'.

  ENDMETHOD.                    "ON_F4

ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION



CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.


ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_erase.
    ty_toolbar-function  = 'DEL_TIPO_DOC'.
    ty_toolbar-text      = 'Tp.Doc.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_erase.
    ty_toolbar-function  = 'DEL_TIPO_PED'.
    ty_toolbar-text      = 'Tp.Ped.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_erase.
    ty_toolbar-function  = 'DEL_TIPO_OV'.
    ty_toolbar-text      = 'Tp.OV.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_erase.
    ty_toolbar-function  = 'DEL_BLOQ_PGTO'.
    ty_toolbar-text      = 'Bloq.Pgto.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*** US 79059 - Inicio - CBRAND.
    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = 'PREV_PAG_INTER'.
    ty_toolbar-text      = 'Prev.Aut.Intercompany'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*** US 79059 - Fim - CBRAND.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'DEL'.
        PERFORM deletar_reg.
      WHEN 'DEL_TIPO_DOC'.
        PERFORM del_value USING 'TIPO_DOC'.
      WHEN 'DEL_TIPO_PED'.
        PERFORM del_value USING 'TIPO_PED'.
      WHEN 'DEL_TIPO_OV'.
        PERFORM del_value USING 'TIPO_OV'.
      WHEN 'DEL_BLOQ_PGTO'.
        PERFORM del_value USING 'BLOQ_PGTO'.
*** US 79059 - Inicio - CBRAND.
      WHEN  'PREV_PAG_INTER'.
        PERFORM prev_pag_inter.
*** US 79059 - Fim - CBRAND.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: obj_toolbar      TYPE REF TO lcl_alv_toolbar.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.


*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------
DATA: vg_erro      TYPE c,
      v_prefix_ent TYPE zprefix,
      v_mensagem   TYPE bapi_msg,
      t_dir_loc_f  TYPE TABLE OF sdokpath,
      t_dir_local  TYPE TABLE OF sdokpath,
      t_dir_unix   TYPE TABLE OF epsfili,
      v_file_aux   TYPE draw-filep,
      v_file_aux2  TYPE draw-filep,
      it_xml_forn  TYPE TABLE OF zxml,
      wa_xml_forn  TYPE zxml.
