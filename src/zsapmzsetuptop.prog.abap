*&---------------------------------------------------------------------*
*&  Include           ZSAPMZSETUPTOP
*&---------------------------------------------------------------------*
type-pools cntl .

*&---------------------------------------------------------------------*
*&  Variáveis de Comando de Tela
*&---------------------------------------------------------------------*

data: ok_principal type sy-ucomm.

*&---------------------------------------------------------------------*
*&  Constantes de Comando de Tela
*&---------------------------------------------------------------------*
data: ok_back   type sy-ucomm value 'BACK',
      ok_exit   type sy-ucomm value 'EXIT',
      ok_cancel type sy-ucomm value 'CANCEL'.

types: cntl_simple_events type table of cntl_simple_event.

*&---------------------------------------------------------------------*
*&  Variáveis de Controle de Tela
*&---------------------------------------------------------------------*
data: init_principal  type c length 1,
* Fields on Dynpro 0001
      g_event(30),
      g_node_key      type tv_nodekey,
      g_item_name     type tv_itmname,
      setup_gs_layout type lvc_s_layo.

*&---------------------------------------------------------------------*
*&  Constantes
*&---------------------------------------------------------------------*
data: c_x type c length 1 value 'X'.

*&---------------------------------------------------------------------*
*&  Definição e Implementação de Classes
*&---------------------------------------------------------------------*

class lcl_application definition.
  public section.
    methods:
      handle_item_double_click
        for event item_double_click
        of cl_gui_list_tree
        importing node_key item_name.
endclass.                    "LCL_APPLICATION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_application implementation.

  method  handle_item_double_click.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    g_event     = 'ITEM_DOUBLE_CLICK'.
    g_node_key  = node_key.
    g_item_name = item_name.
    leave to screen 0001.
  endmethod.                    "HANDLE_ITEM_DOUBLE_CLICK

endclass.                    "LCL_APPLICATION IMPLEMENTATION

*&---------------------------------------------------------------------*
*&  Controle de Tela Principal
*&---------------------------------------------------------------------*
data: g_application      type ref to lcl_application,
      docking            type ref to cl_gui_docking_container,
      g_custom_container type ref to cl_gui_custom_container,
      g_tree             type ref to cl_gui_list_tree,
      picture            type ref to cl_gui_picture.
