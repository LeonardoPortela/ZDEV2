FUNCTION zsd_ajusta_ov_dummy.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_VBELN_VENDA) TYPE  VBELN_VA
*"  TABLES
*"      T_XVBAP STRUCTURE  VBAPVB
*"----------------------------------------------------------------------

  FREE: w_header_in,     w_bape_vbak, w_bape_vbakx,
        t_bapiparex,     t_items_in,  t_schedules_in,
        t_items_inx,     t_conditions_in,
        t_partners,      t_return,
        t_vbap,          t_vbep,
        l_ov_dummy.

*-----------------------------
* busca ov dummy
*-----------------------------
  SELECT vbeln
    INTO l_ov_dummy
    FROM vbfa
      UP TO 1 ROWS
   WHERE vbelv   = i_vbeln_venda
     AND vbtyp_n = 'C'.
  ENDSELECT.

  CHECK sy-subrc = 0.

*-----------------------------
* busca itens
*-----------------------------
  SELECT *
    FROM vbak
    INTO w_vbak
   UP TO 1 ROWS
   WHERE vbeln = l_ov_dummy.
  ENDSELECT.

  CHECK sy-subrc = 0.

  SELECT *
    FROM vbep
    INTO TABLE t_vbep
   WHERE vbeln = i_vbeln_venda.

*-----------------------------
* limpar log
*-----------------------------
  zcl_remessa_terceiro=>zif_remessa_terceiro~set_free_return( ).
* zcl_remessa_terceiro=>zif_remessa_terceiro~set_free_log_proc(
*    EXPORTING i_vbeln      = i_vbeln_venda
*              i_nf_venda   = ''
*              i_etapa_proc = zcl_remessa_terceiro=>zif_remessa_terceiro~c_ajustar_ov_dummy ).

*-----------------------------
* monta estruturas bapi
*-----------------------------
* w_header_in-sales_org        = w_vbak-vkorg.
* w_header_inx-sales_org       = abap_true.
  w_header_inx-updateflag      = 'U'.

  LOOP AT t_xvbap            INTO w_xvbap.
    CLEAR w_vbep.
    READ TABLE t_vbep       INTO w_vbep WITH KEY posnr = w_xvbap-posnr.

    w_items_in-itm_number      = w_xvbap-posnr.
    w_items_in-target_qty      = w_xvbap-kwmeng.
    APPEND w_items_in         TO t_items_in.
*
    w_items_inx-itm_number     = w_xvbap-posnr.
    w_items_inx-target_qty     = abap_true.
    w_items_inx-updateflag     = 'U'.
    APPEND w_items_inx        TO t_items_inx.

    w_schedules_in-itm_number  = w_xvbap-posnr.
    w_schedules_in-sched_line  = w_vbep-etenr.
    w_schedules_in-req_qty     = w_xvbap-kwmeng. "  '1'.
    APPEND w_schedules_in     TO t_schedules_in.
*
    w_schedules_inx-itm_number = w_xvbap-posnr.
    w_schedules_inx-sched_line = w_vbep-etenr.
    w_schedules_inx-req_qty    = abap_true.
    w_schedules_inx-updateflag = 'U'.
    APPEND w_schedules_inx    TO t_schedules_inx.
  ENDLOOP.

*-----------------------------
* executa bapi
*-----------------------------
"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument    = l_ov_dummy
      order_header_in  = w_header_in
      order_header_inx = w_header_inx
    TABLES
      order_item_in    = t_items_in
      order_item_inx   = t_items_inx
      schedule_lines   = t_schedules_in
      schedule_linesx  = t_schedules_inx
      return           = t_return.

*--------------------------------
* Criar log
*--------------------------------
  zcl_remessa_terceiro=>zif_remessa_terceiro~set_criar_log(
     EXPORTING i_vbeln      = i_vbeln_venda
               i_doc_gerado = l_ov_dummy
               i_nf_venda   = ''
               i_etapa_proc = zcl_remessa_terceiro=>zif_remessa_terceiro~c_ajustar_ov_dummy
      CHANGING t_return     = t_return[] ).

*--------------------------------
* Commit
*--------------------------------
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFUNCTION.
