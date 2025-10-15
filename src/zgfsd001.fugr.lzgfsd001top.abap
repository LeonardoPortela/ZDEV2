FUNCTION-POOL zgfsd001.                     "MESSAGE-ID ..
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TABLES zsde013.

TYPE-POOLS: slis, kkblo.

TYPES: BEGIN OF ty_ovs,
         vbeln TYPE vbak-vbeln,
       END OF ty_ovs,

       BEGIN OF ty_saida_exec,
         inco1    TYPE zsdt0041-werks,
         spart    TYPE zsdt0041-spart,
         auart    TYPE zsdt0041-auart,
         werks    TYPE zsdt0041-werks,
         vbeln    TYPE vbak-vbeln,
         msg(255),
       END OF ty_saida_exec.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
*        t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid.
*        t_top        TYPE slis_t_listheader,
*        t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.


" 12.12.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL -->
DATA gv_ucomm_9000 TYPE sy-ucomm.
DATA gv_erro_9000 TYPE flag.
DATA gv_edit_9000 TYPE flag.

" --- ALV
DATA go_cc_alv_9000 TYPE REF TO cl_gui_custom_container.
DATA go_alv_9000 TYPE REF TO cl_gui_alv_grid.
DATA gt_alv_9000 TYPE TABLE OF zsde013.

CONTROLS tc_alv TYPE TABLEVIEW USING SCREEN 9000.

" --- COMENTARIO
DATA go_cc_coment_9000 TYPE REF TO cl_gui_custom_container.
DATA go_coment_9000 TYPE REF TO cl_gui_textedit.
DATA gv_coment TYPE string.

" --- MOTIVO
DATA go_cc_motivo_9000 TYPE REF TO cl_gui_custom_container.
DATA go_motivo_9000 TYPE REF TO cl_gui_textedit.
DATA gv_motivo TYPE string.
DATA gv_venc_9000 TYPE sydatum.

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

*      handle_delayed_changed_sel_cb
*        FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.


ENDCLASS.                    "lcl_event_handler DEFINITION
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_delayed_changed_sel_cb.
*    PERFORM f_on_seleted_9000.
*  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
" 12.12.2022 - RAMON - MELHORIAS OPERAÇÃO DE HEDG #95473 RBL --<
