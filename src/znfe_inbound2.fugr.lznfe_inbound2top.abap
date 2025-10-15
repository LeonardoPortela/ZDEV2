FUNCTION-POOL znfe_inbound2.                "MESSAGE-ID ..

********************************************************************
* types
********************************************************************
TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE lfbw.
TYPES:   text40 TYPE t059u-text40.
TYPES: END   OF ty_saida.

********************************************************************
* variaveis glbais
********************************************************************
DATA: t_saida                TYPE TABLE OF ty_saida,
      w_saida                TYPE ty_saida,
      t_lfbw                 TYPE TABLE OF lfbw,
      w_lfbw                 TYPE lfbw,
      lc_zcl_nfe             TYPE REF TO zcl_nfe_inbound,
*
      g_grid_det             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_det TYPE REF TO cl_gui_custom_container,
*
      t_fieldcat             TYPE lvc_t_fcat,
      w_fieldcat             TYPE lvc_s_fcat,
      t_sort                 TYPE lvc_t_sort,
      w_sort                 TYPE lvc_s_sort,
      t_color                TYPE lvc_t_scol,
      w_color                TYPE lvc_s_scol,
      t_ucomm                TYPE TABLE OF syst_ucomm,
      t_ucomm_0110           TYPE TABLE OF syst_ucomm,
      w_ucomm                TYPE syst_ucomm,
      t_exctab               TYPE slis_t_extab,
      w_exctab               TYPE slis_extab,
      w_layout               TYPE lvc_s_layo,
      w_stable               TYPE lvc_s_stbl    VALUE 'XX',
      t_style                TYPE lvc_t_styl,
      w_style                TYPE lvc_s_styl,
      t_rows                 TYPE lvc_t_row,
      w_rows                 TYPE lvc_s_row,
      ok_code                TYPE syst_ucomm.

********************************************************************
* classes
********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.
    CASE e_ucomm.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD toolbar.
  ENDMETHOD.

ENDCLASS.

********************************************************************
********************************************************************
