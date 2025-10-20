*----------------------------------------------------------------------*
***INCLUDE ZLESR00140203 .
*----------------------------------------------------------------------*

CLASS lcl_event_contas DEFINITION DEFERRED.

DATA: wa_event_contas TYPE REF TO lcl_event_contas,
      wa_cont_contas           TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv_contas            TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      it_fcat_contas           TYPE TABLE OF lvc_s_fcat            , "Controle VLA: catálogo de campos
      wa_layout_contas         TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


*----------------------------------------------------------------------*
*       CLASS lcl_event_contas DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_contas DEFINITION.

  PUBLIC SECTION.
    METHODS: zm_handle_hotspot_contas FOR EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id
                       e_column_id
                       es_row_no                      ,

            zm_handle_toolbar_contas FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
                e_object e_interactive                   ,

            zm_handle_user_command_contas FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
                 e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_contas IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_contas IMPLEMENTATION.

  METHOD: zm_handle_hotspot_contas.
    PERFORM z_handle_hotspot_contas USING e_row_id
                                         e_column_id
                                         es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar_contas.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar_contas USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command_contas.
*   User Command Botões Incluidos
    PERFORM z_handle_command_contas USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_hotspot_contas USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

*  READ TABLE it_itemcap INDEX p_e_row_id INTO wa_itemdata.
*
*  CASE p_e_column_id.
*    WHEN 'INVOICE_DOC_ITEM'.
*      PERFORM visualizar_contas_contas USING wa_itemdata-invoice_doc_item.
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_handle_toolbar_contas  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
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
FORM z_handle_command_contas  USING p_ucomm TYPE syucomm.

*  CASE p_ucomm.
*    WHEN 'REMESSA'.
**     Gera Remessa
*      CALL METHOD wa_alv->refresh_table_display .
*  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0203 INPUT.

  CASE ok_code.
    WHEN c_exit OR c_cancel OR c_back.
      vg_tela_0200 = c_0202.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0203  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0203  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0203 OUTPUT.

  wa_layout_contas-zebra = c_x.

  IF wa_cont_contas IS INITIAL.

    PERFORM alv_preenche_cat_contas USING:
          'INVOICE_DOC_ITEM'   text-c01   '06'  space  space,
          'GL_ACCOUNT'         text-c02   '10'  space  space,
          'ITEM_AMOUNT'        text-c03   '20'  space  space,
          'DB_CR_IND'          text-c04   '01'  space  space,
          'COMP_CODE'          text-c05   '04'  space  space,
          'BUS_AREA'           text-c06   '04'  space  space,
          'ITEM_TEXT'          text-c07   '50'  space  space.

    CREATE OBJECT wa_cont_contas
      EXPORTING
        container_name              = 'CC_ALV_CONTAS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.

  IF ( wa_alv_contas IS INITIAL ) AND ( NOT wa_cont_contas IS INITIAL ).
    CREATE OBJECT wa_alv_contas
      EXPORTING
        i_parent          = wa_cont_contas
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event_contas IS INITIAL.
    CREATE OBJECT wa_event_contas.
    SET HANDLER: wa_event_contas->zm_handle_hotspot_contas FOR wa_alv_contas.
    SET HANDLER: wa_event_contas->zm_handle_toolbar_contas FOR wa_alv_contas.
    SET HANDLER: wa_event_contas->zm_handle_user_command_contas FOR wa_alv_contas.
  ENDIF.

  CALL METHOD wa_alv_contas->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_contas
    CHANGING
      it_outtab                     = it_itesconta
      it_fieldcatalog               = it_fcat_contas
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv_contas IS INITIAL.

ENDMODULE.                 " STATUS_0203  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_contas  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_ITESCONTA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  APPEND wl_fcat TO it_fcat_contas.

ENDFORM.                    " ALV_PREENCHE_CAT
