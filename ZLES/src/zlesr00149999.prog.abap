*----------------------------------------------------------------------*
***INCLUDE ZLESR00149999 .
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
***INCLUDE ZLESR00140202 .
*----------------------------------------------------------------------*

CLASS lcl_event_eventos DEFINITION DEFERRED.

DATA: wa_event_eventos TYPE REF TO lcl_event_eventos,
      wa_cont_eventos           TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv_eventos            TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      it_fcat_eventos           TYPE TABLE OF lvc_s_fcat            , "Controle VLA: catálogo de campos
      wa_layout_eventos         TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


*----------------------------------------------------------------------*
*       CLASS lcl_event_eventos DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_eventos DEFINITION.

  PUBLIC SECTION.
    METHODS: zm_handle_hotspot_eventos FOR EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id
                       e_column_id
                       es_row_no                      ,

            zm_handle_toolbar_eventos FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
                e_object e_interactive                   ,

            zm_handle_user_command_eventos FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
                 e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_eventos IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_eventos IMPLEMENTATION.

  METHOD: zm_handle_hotspot_eventos.
    PERFORM z_handle_hotspot_eventos USING e_row_id
                                         e_column_id
                                         es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar_eventos.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar_eventos USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command_eventos.
*   User Command Botões Incluidos
    PERFORM z_handle_command_eventos USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_hotspot_eventos USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

*  READ TABLE it_itemcap INDEX p_e_row_id INTO wa_itemdata.
*
*  CASE p_e_column_id.
*    WHEN 'INVOICE_DOC_ITEM'.
*      PERFORM visualizar_eventos_contas USING wa_itemdata-invoice_doc_item.
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_toolbar_eventos  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
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
FORM z_handle_command_eventos  USING p_ucomm TYPE syucomm.

*  CASE p_ucomm.
*    WHEN 'REMESSA'.
**     Gera Remessa
*      CALL METHOD wa_alv->refresh_table_display .
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Module  STATUS_9999  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9999 OUTPUT.

  SET PF-STATUS 'PFEVENTOS'.
  SET TITLEBAR 'TLEVENTOS'.

  wa_layout_eventos-zebra = c_x.

  IF wa_cont_eventos IS INITIAL.

    PERFORM alv_preenche_cat_eventos USING:
          'ICONE'      text-l01   '004'  space  space,
          'MSG_TEXT'   text-l02   '300'  space  space,
          'MSG_ID'     text-l03   '020'  space  space,
          'MSG_NO'     text-l04   '003'  space  space,
          'MSG_VAR1'   text-l05   '050'  space  space,
          'MSG_VAR2'   text-l06   '050'  space  space,
          'MSG_VAR3'   text-l07   '050'  space  space,
          'MSG_VAR4'   text-l08   '050'  space  space.

    CREATE OBJECT wa_cont_eventos
      EXPORTING
        container_name              = 'CC_ALV_EVENTOS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( wa_alv_eventos IS INITIAL ) AND ( NOT wa_cont_eventos IS INITIAL ).
    CREATE OBJECT wa_alv_eventos
      EXPORTING
        i_parent          = wa_cont_eventos
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event_eventos IS INITIAL.
    CREATE OBJECT wa_event_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_hotspot_eventos FOR wa_alv_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_toolbar_eventos FOR wa_alv_eventos.
    SET HANDLER: wa_event_eventos->zm_handle_user_command_eventos FOR wa_alv_eventos.
  ENDIF.

  CALL METHOD wa_alv_eventos->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_eventos
    CHANGING
      it_outtab                     = ti_eventos
      it_fieldcatalog               = it_fcat_eventos
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv_eventos IS INITIAL.

ENDMODULE.                 " STATUS_9999  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_eventos  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'TI_EVENTOS'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  APPEND wl_fcat TO it_fcat_eventos.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9999  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9999 INPUT.

  CASE ok_code.
    WHEN c_back OR c_exit OR c_cancel.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9999  INPUT
