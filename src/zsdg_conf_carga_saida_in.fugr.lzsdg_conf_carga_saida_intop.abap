FUNCTION-POOL zsdg_conf_carga_saida_in.   "MESSAGE-ID ..


DATA: go_custom_container_1000    TYPE REF TO cl_gui_custom_container,
      go_custom_container_1000_2  TYPE REF TO cl_gui_custom_container,
      go_custom_container_1002    TYPE REF TO cl_gui_custom_container,
      go_splitter_1_1000          TYPE REF TO cl_gui_splitter_container,
      go_splitter_2_1000          TYPE REF TO cl_gui_splitter_container,
      go_parent_1_1000            TYPE REF TO cl_gui_container,
      go_parent_2_1000            TYPE REF TO cl_gui_container,
      go_parent_3_1000            TYPE REF TO cl_gui_container,
      go_bordero_1000             TYPE REF TO cl_gui_alv_grid,
      go_carga_1000               TYPE REF TO cl_gui_alv_grid,
      go_notas_1000               TYPE REF TO cl_gui_alv_grid,
      go_de_para_1000             TYPE REF TO cl_gui_alv_grid,
      go_aceite_1002              TYPE REF TO cl_gui_alv_grid,
      gs_layout_1000_bordero      TYPE lvc_s_layo,
      gs_layout_1000_carga        TYPE lvc_s_layo,
      gs_layout_1000_notas        TYPE lvc_s_layo,
      gs_layout_1000_de_para      TYPE lvc_s_layo,
      gs_layout_1002              TYPE lvc_s_layo,
      t_fieldcatalog_bordero_1000 TYPE lvc_t_fcat,
      t_fieldcatalog_carga_1000   TYPE lvc_t_fcat,
      t_fieldcatalog_notas_1000   TYPE lvc_t_fcat,
      t_fieldcatalog_de_para_1000 TYPE lvc_t_fcat,
      t_fieldcatalog_aceite_1002  TYPE lvc_t_fcat.


DATA: t_bordero     TYPE zsds387_t,
      t_itens_carga TYPE zsds381_t,
      t_notas       TYPE zsds390_t,
      t_de_para     TYPE zsds388_t,
      t_aceite      TYPE ZSDS391_T.

DATA: gv_carga       TYPE znro_cg,
      gv_ucomm       TYPE sy-ucomm,
      "gv_bordero     TYPE c,
      gv_boleto_fase TYPE c,
      gv_termo       TYPE c,
      gv_operacao_aceite TYPE sy-ucomm.
