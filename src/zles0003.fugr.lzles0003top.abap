  FUNCTION-POOL zles0003 MESSAGE-ID zpfe.

  "Bibliotecas
  TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
  TYPE-POOLS: slis.

  TYPES: BEGIN OF ty_saida.
          INCLUDE STRUCTURE zlest0062.
  TYPES:  icon(4),
         END OF ty_saida.

  DATA: BEGIN OF tg_saida_arq OCCURS 0,
        mark,
        filename TYPE sdok_filnm,
        END OF tg_saida_arq.

  TYPES: BEGIN OF ty_estrutura.
          INCLUDE TYPE slis_fieldcat_main.
          INCLUDE TYPE slis_fieldcat_alv_spec.
  TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
  DATA: tg_saida TYPE TABLE OF ty_saida WITH HEADER LINE,
        tg_filename  TYPE TABLE OF sdokpath WITH HEADER LINE.

*----------------------------------------------------------------------*
* WorkArea/Variaveis
*----------------------------------------------------------------------*
  DATA: wg_caminho(255).
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
  DATA: g_container           TYPE scrfname VALUE 'CC_ITENS_NOTA',
        g_custom_container    TYPE REF TO cl_gui_custom_container,
        container1            TYPE REF TO cl_gui_custom_container,
        grid1                 TYPE REF TO cl_gui_alv_grid.


  DATA: t_fieldcatalog        TYPE lvc_t_fcat,
        w_fieldcatalog        TYPE lvc_s_fcat,
        wa_layout             TYPE lvc_s_layo,
        wa_stable             TYPE lvc_s_stbl,
        tg_selected_rows      type lvc_t_row,
        wg_selected_rows      type lvc_s_row.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
  DATA: xs_events    TYPE slis_alv_event,
        events       TYPE slis_t_event,
        t_print      TYPE slis_print_alv,
        estrutura    TYPE TABLE OF ty_estrutura,
        wa_estrutura TYPE ty_estrutura,
        v_report     LIKE sy-repid,
        t_top        TYPE slis_t_listheader.

  "Tabelas
  TABLES: zib_contabil, zpfe_lote.

*---------- Definition -----------------------------------------------*
  CLASS cockip_event DEFINITION.
    PUBLIC SECTION.
      METHODS: cockpit_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.


  ENDCLASS.                    "cockip_event_handler_lotes DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
  CLASS lcl_event_handler DEFINITION.

    PUBLIC SECTION.
      CLASS-METHODS: on_delayed_changed_sel FOR EVENT delayed_callback OF
   cl_gui_alv_grid.

  ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*---------- Implementation -------------------------------------------*
  CLASS cockip_event IMPLEMENTATION.

    METHOD cockpit_hotspot.
      PERFORM cockpit_hotspot USING e_row_id e_column_id es_row_no-row_id e_column_id.
    ENDMETHOD.                    "cockpit_hotspot
    "cockpit_hotspot_click_lotes
  ENDCLASS.                    "cockip_event_handler_lotes IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
  CLASS lcl_event_handler IMPLEMENTATION.
      METHOD on_delayed_changed_sel.
        DATA char.
        BREAK-POINT.
*    set screen 200.
*    leave screen.
      ENDMETHOD.                    "on_delayed_changed_sel

  ENDCLASS.                    "lcl_event_handler DEFINITION

  "Comandos
  DATA: ok_code      TYPE sy-ucomm,
        ok_code_0002 TYPE sy-ucomm,
        wa_fcode     TYPE sy-ucomm,
        it_fcode     LIKE TABLE OF wa_fcode,
        vg_cancelado_0002 TYPE c LENGTH 1,
        vg_alv_0002       TYPE c LENGTH 1,
        ok_gravar    TYPE sy-ucomm VALUE 'GRAVAR',
        ok_ajustar   TYPE sy-ucomm VALUE 'AJUSTAR',
        ok_cancelar  TYPE sy-ucomm VALUE 'CANCELAR',
        tip_contabil TYPE ztipcontabil VALUE 'FC',
        it_itens     TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        it_itens_alv TYPE TABLE OF zpfe_lote_item_alv WITH HEADER LINE.

  "Contantes
  CONSTANTS: c_n TYPE c LENGTH 1 VALUE 'N',
             c_g TYPE c LENGTH 1 VALUE 'G',
             c_f TYPE c LENGTH 1 VALUE 'F',
             c_c TYPE c LENGTH 1 VALUE 'C',
             c_x TYPE c LENGTH 1 VALUE 'X',
             c_i TYPE c LENGTH 1 VALUE 'I',
             c_e TYPE c LENGTH 1 VALUE 'E'.

  DATA: vg_editar             TYPE c LENGTH 1,
        vg_alterado           TYPE c LENGTH 1,
        wa_lote_alv           TYPE zpfe_lote_alv,
        cockpit_container_alv TYPE REF TO cl_gui_custom_container,
        cockpit_alv           TYPE REF TO cl_gui_alv_grid    ,
        it_cockpit_catalog    TYPE lvc_t_fcat,
        cockpit_gs_layout     TYPE lvc_s_layo,
        is_variant            TYPE disvariant,
        cockpit_event         TYPE REF TO cockip_event,
        es_row_no	            TYPE lvc_s_roid,
        es_row_info	          TYPE lvc_s_row,
        es_col_info	          TYPE lvc_s_col,
        vg_total_ajustar      TYPE zpfe_lote-vl_ac_dc_lote.


*&---------------------------------------------------------------------*
*&      Form  cockpit_hotspot_click_item
*&---------------------------------------------------------------------*
  FORM cockpit_hotspot USING value(e_row_id)    LIKE lvc_s_row
                             value(e_column_id) LIKE lvc_s_col
                             value(row_id) LIKE lvc_s_roid-row_id
                             value(fieldname) LIKE lvc_s_col.

    DATA: wa_lotes_item_alv TYPE zpfe_lote_item_alv,
          vg_belnr          TYPE belnr_d,
          vg_gjahr          TYPE gjahr,
          vg_bukrs          TYPE bukrs,
          wa_zpfe_chvid_ag  TYPE zpfe_chvid_ag,
          e_row             TYPE lvc_s_row,
          vg_tabix          TYPE sy-tabix.

    CASE fieldname-fieldname.
      WHEN 'ICOSTATUS'.
        READ TABLE it_itens_alv  INDEX row_id INTO wa_lotes_item_alv.
        CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
        CALL FUNCTION 'Z_PFE_MSG_CONTAB'
          EXPORTING
            p_nm_lote      = wa_lotes_item_alv-nm_lote
            p_nm_lote_item = wa_lotes_item_alv-nm_lote_item.
      WHEN 'TKNUM'.
        READ TABLE it_itens_alv  INDEX row_id INTO wa_lotes_item_alv.
        CHECK: ( sy-subrc IS INITIAL ) AND ( NOT wa_lotes_item_alv-tknum IS INITIAL ).
        SET PARAMETER ID: 'TNR' FIELD wa_lotes_item_alv-tknum.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
      WHEN 'CK_CONFERIDO'.
        READ TABLE it_itens_alv INDEX row_id.
        vg_tabix = sy-tabix.
        IF it_itens_alv-ck_conferido EQ c_x.
          CLEAR it_itens_alv-ck_conferido.
        ELSE.
          it_itens_alv-ck_conferido = c_x.
        ENDIF.

        CALL METHOD cockpit_alv->get_scroll_info_via_id
          IMPORTING
            es_row_no   = es_row_no
            es_row_info = es_row_info
            es_col_info = es_col_info.

        MODIFY it_itens_alv INDEX vg_tabix TRANSPORTING ck_conferido.
        vg_total_ajustar = 0.
        LOOP AT it_itens_alv WHERE ck_conferido EQ c_x.
          vg_total_ajustar = vg_total_ajustar + it_itens_alv-vl_diferenca.
        ENDLOOP.

        CALL METHOD cockpit_alv->refresh_table_display.

      WHEN 'DOCNUM'.
        READ TABLE it_itens_alv  INDEX row_id INTO wa_lotes_item_alv.
        CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
        PERFORM visualizar_nota USING wa_lotes_item_alv-docnum.
      WHEN 'CD_CIOT'.
        READ TABLE it_itens_alv  INDEX row_id INTO wa_lotes_item_alv.
        CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
        PERFORM visualizar_ciot USING wa_lotes_item_alv-cd_ciot.
      WHEN 'BELNR'.
        READ TABLE it_itens_alv  INDEX row_id INTO wa_lotes_item_alv.
        CHECK ( sy-subrc IS INITIAL ) AND ( wa_lotes_item_alv-nm_lote IS NOT INITIAL ).
        IF wa_lotes_item_alv-belnr IS INITIAL.
          MESSAGE s006 WITH wa_lotes_item_alv-nm_lote.
          CHECK 1 = 2.
        ENDIF.
        vg_belnr = wa_lotes_item_alv-belnr.
        vg_gjahr = wa_lotes_item_alv-gjahr.
        SELECT SINGLE bukrs INTO vg_bukrs
          FROM zpfe_lote
         WHERE nm_lote EQ wa_lotes_item_alv-nm_lote.
        SET PARAMETER ID: 'BLN' FIELD vg_belnr, 'BUK' FIELD vg_bukrs, 'GJR' FIELD vg_gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDCASE.

    LEAVE TO SCREEN 0002.

  ENDFORM.                    " handle_hotspot_click

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_CIOT
*&---------------------------------------------------------------------*
  FORM visualizar_ciot  USING p_cd_ciot TYPE zciot.

    CALL FUNCTION 'Z_SD_EMITE_CIOT'
      EXPORTING
        p_cd_ciot        = p_cd_ciot
      EXCEPTIONS
        sem_dados_ciot   = 1
        nao_ciot         = 2
        erro_status      = 3
        erro_web_service = 4
        OTHERS           = 5.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDFORM.                    " VISUALIZAR_CIOT



*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LOTES_ITEM_ALV_DOCNUM  text
*----------------------------------------------------------------------*
  FORM visualizar_nota  USING  p_docnum TYPE j_1bdocnum.

    DATA gf_nfobjn  LIKE j_1binterf-nfobjn.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = p_docnum
      IMPORTING
        obj_number         = gf_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
      EXPORTING
        obj_number         = gf_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        scr_ctrl_not_found = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDFORM.                    " VISUALIZAR_NOTA
