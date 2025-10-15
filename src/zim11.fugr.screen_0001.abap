
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zim011 CURSOR nextline.
    MODULE liste_show_liste.
    MODULE atualiza_descricao.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zim011-gjahr .
      FIELD zim011-cod_item .
      FIELD zim011-descr_item .
      FIELD zim011-cod_gpo .
      FIELD zim011-descr_grupo .
      FIELD zim011-preco_item .
      FIELD zim011-status_bloq .
      FIELD zim011-status_cta .
      FIELD zim011-knttp .
      FIELD zim011-knttx .
      FIELD zim011-observacoes .
      FIELD zim011-saknr .
      FIELD zim011-txt20 .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.

    FIELD zim011-cod_gpo MODULE atualiza_descricao.


    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zim011-gjahr .
      FIELD zim011-cod_item .
      MODULE liste_update_liste.
    ENDCHAIN.

    MODULE atualiza_descricao.

  ENDLOOP.
  MODULE liste_after_loop.
