*&---------------------------------------------------------------------*
*& Include          ZMMR209_TOP
*&---------------------------------------------------------------------*

TABLES: ekko,ekpo,zmmt0196.

TYPES:
  BEGIN OF ty_ekpa,
    ebeln TYPE ekpa-ebeln,
    lifn2 TYPE ekpa-lifn2,
  END OF ty_ekpa,

  BEGIN OF ty_observacoes,
    ebeln    TYPE ekpo-ebeln,
    ebelp    TYPE ekpo-ebelp,
    tdformat TYPE tline-tdformat,
    tdline   TYPE tline-tdline,
  END OF ty_observacoes,

  BEGIN OF ty_0201_sum,
    nro_sol TYPE zmmt0201-nro_sol,
    qtd     TYPE zmmt0201-qtd_total_kg,
  END OF ty_0201_sum.

DATA: t_dados       TYPE TABLE OF zcds_solic_receb_pedidos_comp,
      t_dados2      TYPE TABLE OF zcds_solic_receb_pedidos_comp,
      o_alv         TYPE REF TO cl_gui_alv_grid,
      t_saida       TYPE TABLE OF zmme_alv_solic_receb_compras,
      t_saida_sol_item TYPE TABLE OF zmme_alv_solic_receb_compras,
      t_saida_edit_solicitacao TYPE TABLE OF zmme_alv_solic_receb_compras,
      t_zmmt0196    TYPE TABLE OF zmmt0196,
      "T_ZMMT0196_2 TYPE TABLE OF ZMMT0196,
      t_zlest0181   TYPE TABLE OF zlest0181,
      "T_0201       TYPE TABLE OF ZMMT0201,
      "T_0201_SUM   TYPE TABLE OF TY_0201_SUM,
      t_t001w       TYPE TABLE OF t001w,
      t_ekpa        TYPE TABLE OF ty_ekpa,
      lt_rota       TYPE TABLE OF zsdt0132,
      ref1          TYPE REF TO cl_gui_alv_grid,
      ref2          TYPE REF TO cl_gui_alv_grid,
      init          TYPE c,
      init2         TYPE c,
      t_observacoes TYPE TABLE OF ty_observacoes,
      gv_obrig      TYPE c,
      lv_direcao    TYPE c.


TYPES: tb_saida    TYPE TABLE OF zmme_alv_solic_receb_compras,
       tb_zmmt0196 TYPE TABLE OF zmmt0196,
       tb_dados    TYPE TABLE OF zcds_solic_receb_pedidos_comp.
