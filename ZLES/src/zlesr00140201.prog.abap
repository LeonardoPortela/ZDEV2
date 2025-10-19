*----------------------------------------------------------------------*
***INCLUDE ZLESR00140201 .
*----------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: wa_event TYPE REF TO lcl_event_receiver,
      wa_cont           TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv            TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      it_fcat           TYPE TABLE OF lvc_s_fcat            , "Controle VLA: catálogo de campos
      wa_layout         TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


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

  READ TABLE it_headerdata INDEX p_e_row_id INTO wa_headerdata.

  CASE p_e_column_id.
    WHEN 'REF_DOC_NO'.
      PERFORM visualizar_itens USING wa_headerdata-diff_inv wa_headerdata-ref_doc_no .
  ENDCASE.

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
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.

  wa_layout-zebra = c_x.

  IF wa_cont IS INITIAL.

    PERFORM alv_preenche_cat USING:
          'INVOICE_IND'     text-v01   '01'  space  space,
          'DOC_TYPE'        text-v02   '04'  space  space,
          'DOC_DATE'        text-v03   '10'  space  space,
          'PSTNG_DATE'      text-v04   '10'  space  space,
          'REF_DOC_NO'      text-v05   '10'  space  c_x,
          'COMP_CODE'       text-v06   '04'  space  space,
          'DIFF_INV'        text-v07   '10'  space  c_x,
          'CURRENCY'        text-v08   '05'  space  space,
          'GROSS_AMOUNT'    text-v09   '20'  space  space,
          'HEADER_TXT'      text-v10   '30'  space  space,
          'PMNT_BLOCK'      text-v11   '10'  space  space,
          'DEL_COSTS_TAXC'  text-v12   '02'  space  space,
          'PYMT_METH'       text-v13   '01'  space  space,
          'ALLOC_NMBR'      text-v14   '10'  space  space,
          'BUS_AREA'        text-v16   '04'  space  space,
          'CALC_TAX_IND'    text-v17   '10'  space  space,
          'PARTNER_BK'      text-v18   '10'  space  space.

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

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = it_headerdata
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT wa_alv IS INITIAL.

ENDMODULE.                 " STATUS_0201  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0201 INPUT.

  CASE ok_code.
    WHEN c_itens.
      PERFORM visualizar_itens USING space space.
    WHEN c_gerar.
      PERFORM z_gerar_miro.
    WHEN c_eventos.
      PERFORM z_mostra_eventos.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0201  INPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_HEADERDATA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM visualizar_itens  USING  emissor   TYPE lifre
                              documento TYPE xblnr.

  CLEAR: it_itemcap[].

  IF emissor IS NOT INITIAL AND documento IS NOT INITIAL.
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no	= documento AND diff_inv = emissor.
      APPEND wa_itemdata TO it_itemcap.
    ENDLOOP.
  ELSE.
    it_itemcap[] = it_itemdata[].
  ENDIF.

  vg_tela_0200 = c_0202.

ENDFORM.                    " VISUALIZAR_ITENS
