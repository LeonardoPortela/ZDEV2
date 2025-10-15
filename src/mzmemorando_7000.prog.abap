*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_7000 .
*----------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA wa_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no                      ,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive                   ,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

  READ TABLE it_notas2 INTO wa_notas INDEX p_e_row_id-index.

  IF sy-subrc IS INITIAL.

    CASE p_e_column_id.
      WHEN 'DOCNUM'.
        PERFORM visualizar_nota USING wa_notas-docnum.
      WHEN 'QTDE_BAIXADAS'.
        PERFORM zf_listar_baixadas USING wa_notas.
    ENDCASE.

  ENDIF.

ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
*  MOVE c_separator  TO sl_toolbar-butn_type.
*  APPEND sl_toolbar TO p_object->mt_toolbar.

  DELETE p_object->mt_toolbar WHERE function = '&INFO'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP07'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP00'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP01'.
  DELETE p_object->mt_toolbar WHERE function = '&&SEP02'.

ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm.

*  CASE p_ucomm.
*    WHEN 'REMESSA'.
**     Gera Remessa
*      CALL METHOD wa_alv->refresh_table_display .
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_7000 INPUT.

  CASE ok_code.
    WHEN c_backv OR c_exitv OR c_cancelv.
      CLEAR ok_code.
      vg_dynnr_000 = vg_dynnr_ant.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_7000  INPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c
                               p_sum   TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_NOTAS2'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  STATUS_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_7000 OUTPUT.

  wa_layout-zebra = c_x.

  IF todos IS NOT INITIAL.
    LOOP AT SCREEN.
*      IF SCREEN-NAME EQ 'CAMPO_1'
*      OR SCREEN-NAME EQ 'CAMPO_2'
*      OR SCREEN-NAME EQ 'CAMPO_3'.
      screen-invisible = 1.
      MODIFY SCREEN.
*      ENDIF.
    ENDLOOP.
  ELSE.
    IF wa_export_acomp-nr_terminal IS INITIAL.
      LOOP AT SCREEN.
        IF screen-name EQ 'CAMPO_4'.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( wa_alv IS INITIAL ) AND ( NOT wa_cont IS INITIAL ).

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.
  ENDIF.

  gs_variant_c = sy-repid. "Enable users save own LAYOUTs
  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_variant                    = gs_variant_c
      is_layout                     = wa_layout
      i_save                        = 'X'
      i_default                     = 'X'
    CHANGING
      it_outtab                     = it_notas2
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.

ENDMODULE.                 " STATUS_7000  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_NOTA
*&---------------------------------------------------------------------*
*       text
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
*&---------------------------------------------------------------------*
*&      Form  ZF_LISTAR_BAIXADSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NOTAS_DOCNUM  text
*      -->P_WA_NOTAS_ITMNUM  text
*----------------------------------------------------------------------*
FORM zf_listar_baixadas  USING p_notas TYPE ty_zexport_notas.

  DATA: r_bukrs  TYPE RANGE OF j_1bnfdoc-bukrs,
        r_branch TYPE RANGE OF j_1bnfdoc-branch,
        r_docnum TYPE RANGE OF j_1bnfdoc-docnum,
        r_docdat TYPE RANGE OF j_1bnfdoc-docdat,
        r_dtbaix TYPE RANGE OF zsdt0276-dt_baixa,
        r_lifnr  TYPE RANGE OF j_1bnfdoc-parid,
        r_matnr  TYPE RANGE OF j_1bnflin-matnr,
        r_status TYPE RANGE OF zsdt0276-status.

  r_bukrs = VALUE #( ( sign = 'I' option = 'EQ' low = p_notas-bukrs ) ).

  r_docnum = VALUE #( ( sign = 'I' option = 'EQ' low = p_notas-docnum ) ).

  r_docdat  = VALUE #( ( sign = 'I' option = 'EQ' low = p_notas-dt_emissao ) ).

  r_branch[]  = VALUE #( ( sign = 'I' option = 'EQ' low = p_notas-werks ) ).

  r_status = VALUE #( ( sign = 'I' option = 'NE' low = 'R' ) ).

  r_matnr  = VALUE #( ( sign = 'I' option = 'EQ' low = p_notas-produto ) ).

  SUBMIT zsdr0132_listar
       WITH s_bukrs   IN r_bukrs
       WITH s_branch  IN r_branch
       WITH s_docnum  IN r_docnum
       WITH s_docdat  IN r_docdat
       WITH s_dtbaix  IN r_dtbaix
       WITH s_lifnr   IN r_lifnr
       WITH s_matnr   IN r_matnr
       WITH s_status  IN r_status
        AND RETURN.

ENDFORM.
