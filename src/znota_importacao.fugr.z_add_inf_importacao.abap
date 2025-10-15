function z_add_inf_importacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"----------------------------------------------------------------------
  clear: cria_dock_tela, obj_tree_event_receiver.

  vg_docnum = p_docnum.

  create object g_application.
  create object obj_tree_event_receiver.


  call screen 0001 starting at 07 05 ending at 140 30.

endfunction.
